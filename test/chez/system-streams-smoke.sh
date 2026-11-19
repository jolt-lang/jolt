#!/bin/sh
# System/in, System/out, System/err — the process's own streams.
#
# System.in is an InputStream the JVM hands every program; jolt had System/out
# and System/err but no System/in at all, so any ported code that read stdin
# through it ("No matching field or method: System/in") failed to load. The
# streams also have to report the classes the JVM's do — System.out is a
# PrintStream, not the PrintWriter *out* is — because libraries branch on that.
#
# The stacking matters as much as the classes. The JVM builds *in* as a Reader
# over System.in and *out* as a PrintWriter over System.out, so a program that
# uses both sees one stream; jolt read standard input through a second port of
# its own, and the two buffered independently (jolt-6wad).

set -e

pass=0
fails=0
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"

jolt="bin/jolt"

# run <expr> [stdin-text]: evaluate and print the LAST line of stdout.
run() {
  if [ "$#" -ge 2 ]; then
    printf '%s' "$2" | JOLT_QUIET=1 "$jolt" -e "$1" 2>/dev/null | tail -1
  else
    JOLT_QUIET=1 "$jolt" -e "$1" </dev/null 2>/dev/null | tail -1
  fi
}

check() {
  clabel="$1"; cgot="$2"; cwant="$3"
  if [ "$cgot" = "$cwant" ]; then
    echo "PASS: ($clabel) $cgot"; pass=$((pass+1))
  else
    echo "FAIL: ($clabel) got '$cgot', want '$cwant'"; fails=$((fails+1))
  fi
}

# --- (a) System/in exists and is an InputStream ------------------------------
check a "$(run '(println (instance? java.io.InputStream System/in))')" "true"
# System.in is a BufferedInputStream on the JVM, and reports its own class
check a2 "$(run '(println (class System/in))')" "java.io.BufferedInputStream"

# stdin is decoded by the same decoder as every other Reader (natives-str.ss
# utf8-bytes->string), not by Chez's codec: a leading BOM is U+FEFF and an
# overlong costs one replacement per byte, both as the JVM answers them.
check b0 "$(run '(println (pr-str [(mapv int (read-line)) (mapv int (read-line))]))' "$(printf '\357\273\277ab\n\300\257c\n')")" \
  "[[65279 97 98] [65533 65533 99]]"

# --- (b) slurp / read / line-seq over piped stdin -----------------------------
check b "$(run '(println (slurp System/in))' 'hello stdin')" "hello stdin"
check c "$(run '(println (pr-str (vec (line-seq (clojure.java.io/reader System/in)))))' 'a
b
c
')" "[\"a\" \"b\" \"c\"]"
# InputStream.read() is the next byte as an unsigned int, -1 at EOF.
check d "$(run '(println [(.read System/in) (.read System/in)])' 'AB')" "[65 66]"
check d2 "$(run '(println (.read System/in))')" "-1"

# *in* over real stdin is a java.io.Reader, as the JVM's is (#1380): io/reader
# takes it, and .read / .readLine share the cursor read-line uses.
check in1 "$(run '(println (.readLine (clojure.java.io/reader *in*)))' 'line1
')" "line1"
check in2 "$(run '(println (pr-str (vec (line-seq (clojure.java.io/reader *in*)))))' 'a
b
')" "[\"a\" \"b\"]"
check in3 "$(run '(println [(.read *in*) (.readLine *in*) (read-line) (.readLine *in*)])' 'xy
z
')" "[120 y z nil]"
# a bulk read and read-line share one cursor: nothing is lost or read twice
check in4 "$(run '(let [b (char-array 64) n (.read *in* b 0 64)] (println (pr-str (str (String. b 0 n) (some-> (read-line) (str \|))))))' 'ab
cd
')" "\"ab\\ncd|\""

# A read of *in* takes what has arrived, so a message with no newline after it
# reaches the program while the pipe is still open (an LSP body is one). The
# writer below holds the pipe open until the program leaves a mark, then sends
# the optional tail and closes; a read that waited for a line end or EOF only
# returns once the writer gives up, after "LATE" lands in the input.
held() {
  hmark="$(mktemp -d)/mark"
  { printf '%s' "$2"
    i=0; while [ ! -e "$hmark" ] && [ "$i" -lt 100 ]; do sleep 0.1; i=$((i+1)); done
    if [ -e "$hmark" ]; then printf '%s' "${3:-}"; else printf 'LATE\n'; fi
  } | JOLT_QUIET=1 HELD_MARK="$hmark" "$jolt" -e "$1" 2>/dev/null | tail -1
  rm -rf "$(dirname "$hmark")"
}
mark='(spit (System/getenv "HELD_MARK") "")'
check held1 "$(held "(let [c (.read *in*)] $mark (println (pr-str [c (read-line)])))" 'x')" "[120 nil]"
check held2 "$(held "(let [b (char-array 64) n (.read *in* b 0 64)] $mark (println (pr-str [n (String. b 0 n) (read-line)])))" 'hello')" "[5 \"hello\" nil]"
check held3 "$(held "(do (Thread/sleep 300) (let [r (.ready *in*)] $mark (println (pr-str [r (read-line)]))))" 'abc')" "[true \"abc\"]"
# read-line after a character read waits for the rest of that line
check held4 "$(held "(let [c (.read *in*)] $mark (println (pr-str [c (read-line) (read-line)])))" 'ab' 'cd
ef
')" "[97 \"bcd\" \"ef\"]"

# --- (e) io/copy System/in -> System/out (the babashka.process wd.clj echo) ---
check e "$(run '(clojure.java.io/copy System/in System/out) (flush)' 'echoed')" "echoed"

# --- (f) the classes the JVM reports -----------------------------------------
# System.out / System.err are PrintStreams (java.io.OutputStream), NOT the
# PrintWriter *out* is. Ported code does (instance? java.io.PrintStream x) and
# (.write System/out bytes).
check f  "$(run '(println (class System/out))')" "java.io.PrintStream"
check f2 "$(run '(println (class System/err))')" "java.io.PrintStream"
check f3 "$(run '(println [(instance? java.io.PrintStream System/out) (instance? java.io.OutputStream System/out) (instance? java.io.Closeable System/out)])')" "[true true true]"
# *out* stays a PrintWriter, as on the JVM — the two are different objects there
# and jolt must not collapse their classes.
check g  "$(run '(println (class *out*))')" "java.io.PrintWriter"
check g2 "$(run '(println [(instance? java.io.Writer *out*) (instance? java.io.PrintStream *out*)])')" "[true false]"

# --- (h) System/out is the PROCESS stream: a with-out-str does not capture it --
# "direct" goes to the terminal, so the captured string is "captured" alone —
# resolving System/out through (current-output-port) like *out* used to swallow
# it into the capture instead, giving "direct\ncaptured".
check h "$(run '(let [s (with-out-str (.println System/out "direct") (print "captured"))] (println (pr-str s)))')" "\"captured\""
# and a (binding [*out* …]) does not move it either
check h2 "$(run '(let [w (java.io.StringWriter.)] (binding [*out* w] (.println System/out "direct")) (println (pr-str (str w))))')" "\"\""

# --- (k) .write on a stream: bytes, chars, an int, and a region ---------------
# (.write System/out (.getBytes s)) is the ordinary way ported code writes raw
# output to a PrintStream; the array used to render as "#object[[B …]" into the
# stream. write(int) is the CHARACTER with that code on both PrintStream and
# PrintWriter, and the 3-arg form is (buf off len), not (buf start end).
check k  "$(run '(.write System/out (.getBytes "bytes-out")) (flush)')" "bytes-out"
check k2 "$(run '(.write System/out (.getBytes "0123456789") 2 3) (flush)')" "234"
check k3 "$(run '(do (.write System/out 65) (.write System/out 66) (flush))')" "AB"
# the same three through a StringWriter, so the writer family agrees with the
# stream family instead of each rendering its own way
check k4 "$(run '(let [w (java.io.StringWriter.)] (.write w (.getBytes "abc")) (.write w 68) (.write w "0123456789" 2 3) (println (str w)))')" "abcD234"
# a char[] is its characters for write AND print (the JVM has an overload for
# both); a byte[] only for write, since print(Object) is all it can reach.
check k5 "$(run '(let [w (java.io.StringWriter.)] (.write w (char-array [\x \y])) (.write w (char-array [\z]) 0 1) (println (str w)))')" "xyz"

# --- (i) setIn replaces the stream -------------------------------------------
# read-line reads System/in, so setIn has to redirect it (clojure.test fixtures
# and REPL harnesses drive input this way).
check i "$(run '(System/setIn (java.io.ByteArrayInputStream. (.getBytes "line1\nline2\n"))) (println (pr-str [(read-line) (read-line) (read-line)]))')" "[\"line1\" \"line2\" nil]"
check i2 "$(run '(System/setIn (java.io.ByteArrayInputStream. (.getBytes "xy"))) (println (slurp System/in))')" "xy"

# --- (j) the default System/in is restored-able and reads real stdin ---------
check j "$(run '(let [orig System/in] (System/setIn (java.io.ByteArrayInputStream. (.getBytes ""))) (System/setIn orig) (println (slurp System/in)))' 'back')" "back"

# --- (m) *in* reads THROUGH System/in: one buffer, not two -------------------
# The JVM stacks them (RT.in is a Reader over System.in) and jolt has the same
# shape, so read-line and the stream agree on where the input is up to. They used
# to be two ports on fd 0 buffering independently, and whichever read first ate
# input the other never saw. jolt goes one better than the JVM here: nothing is
# read past the line returned, where an InputStreamReader decodes into 8K of its
# own and leaves (.read System/in) answering -1 with the rest of stdin inside it.
check m1 "$(run '(println (pr-str [(read-line) (.read System/in) (read-line)]))' 'alpha
beta
')" "[\"alpha\" 98 \"eta\"]"
# and the other way round: a byte taken first, then the rest of that line
check m2 "$(run '(println (pr-str [(.read System/in) (read-line)]))' 'abc
')" "[97 \"bc\"]"
# slurp / readAllBytes pick up exactly where read-line stopped
check m3 "$(run '(println (pr-str [(read-line) (slurp System/in)]))' 'one
two
three
')" "[\"one\" \"two\nthree\n\"]"
check m4 "$(run '(println (pr-str [(read-line) (String. (.readAllBytes System/in))]))' 'head
tail
')" "[\"head\" \"tail\n\"]"
# a replaced stream is read the same way — through its own .read, one byte at a
# time, so setIn does not reintroduce a second buffer either
check m5 "$(run '(do (System/setIn (java.io.ByteArrayInputStream. (.getBytes "redirected\nsecond\n"))) (println (pr-str [(read-line) (.read System/in) (read-line)])))')" "[\"redirected\" 115 \"econd\"]"
# with-in-str still wins over both: it rebinds *in*, which is not System/in
check m6 "$(run '(println (pr-str [(with-in-str "from-string" (read-line)) (read-line)]))' 'from-stdin
')" "[\"from-string\" \"from-stdin\"]"

# --- (n) the three line terminators readLine has -----------------------------
# \n, \r, and \r\n all end a line (JVM: (read-line) over "a\rb\nc\r\nd\n" is
# a, b, c, d). jolt used to end a line on \n alone, so a \r came back inside the
# string and a CR-only file read as one enormous line.
check n1 "$(run '(println (pr-str [(read-line) (read-line) (read-line) (read-line) (read-line)]))' 'a
b
c
d
')" "[\"a\" \"b\" \"c\" \"d\" nil]"
check n2 "$(run '(println (pr-str [(read-line) (read-line) (read-line)]))' "$(printf 'a\rb\r\nc')")" "[\"a\" \"b\" \"c\"]"
# a CRLF is one terminator, so the stream's next byte is the one after it, not a
# stray \n left behind for (.read System/in)
check n3 "$(run '(println (pr-str [(read-line) (.read System/in) (read-line)]))' "$(printf 'a\r\nbc\n')")" "[\"a\" 98 \"c\"]"
# a line longer than the reader's initial 128-byte buffer, and a multi-byte
# character that must not be cut where the buffer grows
n4_long=""
while [ "${#n4_long}" -lt 200 ]; do n4_long="${n4_long}xxxxxxxxxx"; done
check n4 "$(run '(let [l (read-line)] (println (pr-str [(count l) (subs l 200 203)])))' "${n4_long}héllo
")" "[205 \"hél\"]"

# a stream that is not one of jolt's own — a proxy over java.io.InputStream — is
# read through its .read, so the override is what feeds read-line, and the CRLF
# it cannot peek at is carried to the next line by the flag rather than coming
# back as an empty one
check n5 "$(run '(do (def bs (atom (seq (.getBytes "px\r\ny\n")))) (System/setIn (proxy [java.io.InputStream] [] (read [] (if-let [s @bs] (let [b (first s)] (swap! bs next) (bit-and b 255)) -1)))) (println (pr-str [(read-line) (read-line) (read-line)])))')" "[\"px\" \"y\" nil]"

# --- (o) a Reader over System/in does not take the stream away ---------------
# R6RS transcoded-port takes ownership of the port it wraps, so building the
# reader used to CLOSE standard input: the next read of System/in — or the next
# read-line, which is the same port — died on a closed port. The reader pulls
# through the stream's own .read now, which is also what lets a proxy's override
# be seen. It does buffer ahead, exactly as the JVM's InputStreamReader does, so
# read-line after it correctly sees end of input rather than an error.
check o "$(run '(let [r (clojure.java.io/reader System/in)] (println (pr-str [(.readLine r) (read-line)])))' 'one
two
')" "[\"one\" nil]"

# --- (p) io/copy System/in -> System/out is byte-exact ------------------------
# A PrintStream is an OutputStream, so the cat has to come out byte for byte.
# Routing it through the text sink decoded the bytes as UTF-8 first and replaced
# every one that was not text with U+FFFD.
check p "$(printf '\000\377\376A' | JOLT_QUIET=1 "$jolt" -e '(clojure.java.io/copy System/in System/out) (flush)' 2>/dev/null | od -An -tx1 | tr -d ' \n')" "00fffe41"
# raw bytes and printed text reach the descriptor in the order they were written
check p2 "$(run '(do (print "A") (.write System/out (.getBytes "B")) (print "C") (flush))')" "ABC"
# a byte[] written to a captured *out* is still text — a string port has no bytes
check p3 "$(run '(println (pr-str (with-out-str (.write *out* (.getBytes "hi")))))')" "\"hi\""

# --- (q) InputStream.read(buf off len) returns as soon as a byte is there -----
# The JVM blocks "until at least one byte is available", not until the buffer is
# full; filling it first hung on a pipe whose writer was waiting for a response.
# The writer sends five bytes and then just holds the pipe open, so this is a
# question about blocking and not a race against startup: filling the buffer
# first cannot answer until the writer gives up, and returning what arrived
# answers at once.
q_fifo="${TMPDIR:-/tmp}/jolt-sysin-q-$$"
rm -f "$q_fifo" "$q_fifo.pid"
mkfifo "$q_fifo"
# started from a subshell that exits at once, so this shell has no job to report
# when the writer is killed below
( { printf 'abcde'; sleep 30; } > "$q_fifo" & echo $! > "$q_fifo.pid" )
check q "$(JOLT_QUIET=1 "$jolt" -e '(let [b (byte-array 4096) n (.read System/in b 0 4096)] (println (pr-str [n (String. b 0 n)])))' < "$q_fifo" 2>/dev/null | tail -1)" "[5 \"abcde\"]"
kill "$(cat "$q_fifo.pid")" 2>/dev/null || true
rm -f "$q_fifo" "$q_fifo.pid"
# it still reports -1 at end of input, and fills across several calls
check q2 "$(run '(let [b (byte-array 8)] (println (pr-str [(.read System/in b 0 8) (String. b 0 3) (.read System/in b 0 8)])))' 'xyz')" "[3 \"xyz\" -1]"

# --- (r) available() is a real byte count ------------------------------------
# It used to answer 0 for every stream. That is a legal JVM answer ("an
# estimate"), but it leaves (pos? (.available in)) false forever, so the loop
# that drains what has arrived never runs. A seekable source answers its exact
# remainder, as FileInputStream and ByteArrayInputStream do; a pipe answers what
# is really there, by filling the port's buffer with a lookahead that consumes
# nothing. All four of these agree with the JVM value for value.
check r1 "$(run '(let [b (java.io.ByteArrayInputStream. (.getBytes "hello"))] (println (pr-str [(.available b) (do (.read b) (.available b)) (do (.readAllBytes b) (.available b))])))')" "[5 4 0]"
check r2 "$(run '(let [o (java.io.PipedOutputStream.) i (java.io.PipedInputStream. o)] (.write o (.getBytes "abcdef")) (println (pr-str [(.available i) (do (.read i) (.available i)) (do (.write o (.getBytes "gh")) (.available i))])))')" "[6 5 7]"
check r3 "$(run '(println (pr-str [(.available System/in) (do (.read System/in) (.available System/in))]))' 'abcdefgh
')" "[9 8]"
# a file, whether opened directly or redirected onto stdin, knows its remainder
check r4 "$(run '(let [f (java.io.FileInputStream. "README.md")] (println (pr-str [(> (.available f) 1000) (let [a (.available f)] (.read f) (- a (.available f)))])))')" "[true 1]"
check r5 "$(JOLT_QUIET=1 "$jolt" -e '(println (.available System/in))' < README.md 2>/dev/null | tail -1)" "$(wc -c < README.md | tr -d ' ')"
# which makes the drain idiom work — this read one byte, or nothing at all
check r6 "$(run '(let [n (.available System/in) b (byte-array n)] (.read System/in b 0 n) (println (String. b)))' 'drain-me')" "drain-me"
# and it is not capped by any buffer of jolt's. The count for a pipe comes from
# ioctl(FIONREAD) — the syscall the JVM's available() makes — so it is the whole
# amount waiting, not the 4096 a port buffer holds. A target with no ioctl falls
# back to filling the buffer, which is where the cap came from.
#
# The strong form — one reading that sees all 8000 bytes — needs the kernel's
# pipe buffer to hold them, and xnu shrinks new pipes to 512 bytes under
# system-wide pipe-memory pressure (the writer then blocks, so no reading can
# ever exceed 512 no matter who asks). So: pass on a single reading above the
# 4096 port-buffer cap (the kernel's count, the property this guards), or on an
# available-guided drain that accounts for every byte when the pipe is small.
r9_big=""
while [ "${#r9_big}" -lt 8000 ]; do r9_big="${r9_big}${n4_long}"; done
check r9 "$(run '(loop [total 0 tries 0] (let [n (.available System/in)] (cond (> n 4096) (println "OK") (>= (+ total n) 8000) (println "OK") (> tries 400) (println (str "STUCK avail " n " total " total)) (pos? n) (let [b (byte-array n)] (.read System/in b 0 n) (recur (+ total n) (inc tries))) :else (do (Thread/sleep 5) (recur total (inc tries))))))' "$r9_big")" "OK"

# a closed stream raises the JVM IOException rather than a classless host error
check r7 "$(run '(let [b (java.io.ByteArrayInputStream. (.getBytes "hi"))] (.close b) (println (try (.available b) (catch java.io.IOException e (.getMessage e)))))')" "Stream closed"
check r8 "$(run '(let [o (java.io.PipedOutputStream.) i (java.io.PipedInputStream. o)] (.close i) (println (try (.available i) (catch java.io.IOException e (.getMessage e)))))')" "Pipe closed"

# --- (s) PushbackInputStream over the process stream -------------------------
# bencode (babashka.pods, nrepl) reads a byte to classify a token and puts it
# back before reading the netstring. The pushed-back byte lives in the stream's
# own port buffer, so it is the next byte for everything that reads the stream:
# its own read/readAllBytes over real stdin, and read-line through a System/in
# it was setIn as — no site has to know the stream is a pushback one.
check s1 "$(run '(let [p (java.io.PushbackInputStream. System/in) c (.read p)] (.unread p c) (println (pr-str [(class p) (String. (.readAllBytes p))])))' 'hello')" "[java.io.PushbackInputStream \"hello\"]"
check s2 "$(run '(let [p (java.io.PushbackInputStream. (java.io.ByteArrayInputStream. (.getBytes "bc\nd\n")))] (.unread p 97) (System/setIn p) (println (pr-str [(read-line) (.read System/in) (read-line)])))')" "[\"abc\" 100 \"\"]"
# available over a pipe: the pushed-back byte counts, on top of whatever the
# kernel reports (r9 above shows why that part is not a fixed number here)
check s3 "$(run '(let [p (java.io.PushbackInputStream. System/in)] (.unread p 120) (println (pr-str [(>= (.available p) 1) (.read p) (.read p)])))' 'ab')" "[true 120 97]"

# --- (t) System/console: a Console only when stdin and stdout are terminals ---
# The JVM answers nil when either is redirected and a java.io.Console when both
# are a terminal, whatever stderr is (measured on OpenJDK 25.0.1). jolt answered
# nil always, so a CLI could not tell a person from a pipe. The terminal rows
# need a pty, which python3's pty module provides; without python3 they skip.
check t1 "$(run '(println (nil? (System/console)))' 'piped')" "true"
check t2 "$(run '(println (nil? (System/console)))')" "true"
# ptyrun <input> <cmd...>: run cmd on a fresh pty, type <input> (escapes allowed)
# once it prints a '>' prompt, and print what the terminal showed, \r dropped.
pty_dir="$(mktemp -d)"
cat > "$pty_dir/ptyrun.py" <<'PY'
import os, pty, select, sys, time
inp = sys.argv[1].encode().decode('unicode_escape').encode()
pid, fd = pty.fork()
if pid == 0:
    os.execvp(sys.argv[2], sys.argv[2:])
out, sent, deadline = b'', False, time.time() + 120
while time.time() < deadline:
    if select.select([fd], [], [], 0.2)[0]:
        try:
            b = os.read(fd, 4096)
        except OSError:
            break
        if not b:
            break
        out += b
    if not sent and b'>' in out:
        os.write(fd, inp)
        sent = True
os.waitpid(pid, 0)
sys.stdout.write(out.decode(errors='replace').replace('\r', ''))
PY
ptyrun() { JOLT_QUIET=1 python3 -I "$pty_dir/ptyrun.py" "$@" 2>/dev/null; }
if command -v python3 >/dev/null 2>&1 && ptyrun '' sh -c 'test -t 0 && test -t 1 && echo pty-ok' | grep -q pty-ok; then
  check t3 "$(ptyrun '' "$jolt" -e '(let [c (System/console)] (println (pr-str [(class c) (.isTerminal c) (identical? c (System/console)) (class (.writer c))])))' | tail -1)" \
    "[java.io.Console true true java.io.PrintWriter]"
  # stdout piped, stdin still the terminal: nil, as on the JVM
  check t4 "$(ptyrun '' sh -c "$jolt -e '(println (nil? (System/console)))' | cat" | tail -1)" "true"
  # stderr is not consulted
  check t5 "$(ptyrun '' sh -c "$jolt -e '(println (nil? (System/console)))' 2>/dev/null" | tail -1)" "false"
  # readLine prints its prompt and answers the line
  check t6 "$(ptyrun 'alice\n' "$jolt" -e '(let [c (System/console)] (println (pr-str (.readLine c "%s> " (object-array ["name"])))))' | tr '\n' '|')" \
    'name> alice|"alice"|'
  # readPassword turns echo off: the typed text never reaches the terminal, the
  # newline after it is the Console's own, and echo is back on afterwards
  check t7 "$(ptyrun 'hunter2\n' sh -c "$jolt -e '(println (String. (.readPassword (System/console) \"pw> \" (object-array 0))))'; stty -a | tr ' ' '\n' | grep -x -- '-*echo'" | tr '\n' '|')" \
    'pw> |hunter2|echo|'
  # ^C at the prompt still puts echo back, through the shutdown hook. The ^C
  # goes to the whole foreground group, so the shell traps it to live on and
  # run stty (dash, Linux's sh, otherwise dies with jolt). A trapped signal is
  # back to its default in jolt, which still exits 130.
  check t8 "$(ptyrun '\003' sh -c "trap : INT; $jolt -e '(.readPassword (System/console) \"pw> \" (object-array 0))'; echo rc=\$?; stty -a | tr ' ' '\n' | grep -x -- '-*echo'" | tail -2 | tr '\n' '|')" \
    'pw> rc=130|echo|'
else
  echo "SKIP: (t3-t8) no python3 pty to run System/console under a terminal"
fi
rm -rf "$pty_dir"

# --- (io/reader path) streams the file, it does not slurp it (jolt-0nk) -------
# io/reader over a path used to answer a StringReader built from the whole file,
# so the content had to be read before the reader existed and nothing could be
# consumed from a source still arriving: .readLine and line-seq over a FIFO, a
# tailed log or a device blocked until EOF. The JVM hands back a BufferedReader
# over a FileReader; io-streams.ss now builds a char-reader over the file's port.
#
# The witness is a FIFO whose writer holds it open after one line until the
# reader says it has that line: jolt makes an ack file once .readLine returns,
# and the writer then sends "line2" and closes. A streaming reader gets line1
# while the writer still waits, so its next line is line2. A slurping one cannot
# return line1 until the writer closes, which it does only after giving up on the
# ack (20 s) with "gave-up" as its last line. No clock is read, so a loaded
# machine moves nothing that is asserted; the 20 s only bounds the failing case.
if command -v mkfifo >/dev/null 2>&1; then
  fifo_dir="$(mktemp -d)"
  fifo="$fifo_dir/pipe"
  ack="$fifo_dir/ack"
  # first_two_lines <expr-building-the-reader>: "<line> <line>" as read.
  first_two_lines() {
    rm -f "$fifo" "$ack"; mkfifo "$fifo"
    ( exec 3> "$fifo"; printf 'line1\n' >&3
      i=0; while [ ! -e "$ack" ] && [ "$i" -lt 200 ]; do sleep 0.1; i=$((i+1)); done
      if [ -e "$ack" ]; then printf 'line2\n' >&3; else printf 'gave-up\n' >&3; fi ) &
    writer=$!
    JOLT_QUIET=1 "$jolt" -e "(let [r $1
                                   l (.readLine r)]
                               (spit \"$ack\" \"\")
                               (println (str l \" \" (.readLine r))))" \
      </dev/null 2>/dev/null | tail -1
    wait "$writer" 2>/dev/null || true
  }
  check "io/reader path is incremental" \
    "$(first_two_lines "(clojure.java.io/reader \"$fifo\")")" "line1 line2"
  # The same through io/file, which is the other door into the same arm.
  check "io/reader io/file is incremental" \
    "$(first_two_lines "(clojure.java.io/reader (clojure.java.io/file \"$fifo\"))")" "line1 line2"
  rm -rf "$fifo_dir"
fi

# The port-backed reader must decode and split exactly as the slurping one did.
rd_dir="$(mktemp -d)"
printf 'alpha\nbeta\ngamma\n'                > "$rd_dir/plain.txt"
printf 'a\r\nb\r\nc'                         > "$rd_dir/crlf.txt"
printf 'caf\303\251 \346\227\245 \360\237\216\211\n' > "$rd_dir/utf8.txt"
: > "$rd_dir/empty.txt"
check "reader lines" \
  "$(run "(println (pr-str (vec (line-seq (clojure.java.io/reader \"$rd_dir/plain.txt\")))))")" \
  '["alpha" "beta" "gamma"]'
# CRLF: BufferedReader strips the \r with the \n, and a last line with no
# terminator is still a line.
check "reader CRLF lines" \
  "$(run "(println (pr-str (vec (line-seq (clojure.java.io/reader \"$rd_dir/crlf.txt\")))))")" \
  '["a" "b" "c"]'
check "reader utf8 round trip" \
  "$(run "(println (pr-str (slurp (clojure.java.io/reader \"$rd_dir/utf8.txt\"))))")" \
  '"café 日 🎉\n"'
check "reader empty file" \
  "$(run "(println (pr-str (slurp (clojure.java.io/reader \"$rd_dir/empty.txt\"))))")" \
  '""'
# A missing path still fails at io/reader time, with the message it always had.
check "reader missing path" \
  "$(run "(println (try (clojure.java.io/reader \"$rd_dir/nope.txt\") :no-throw (catch java.io.FileNotFoundException e (.getMessage e))))")" \
  "$rd_dir/nope.txt (No such file or directory)"
check "reader on a directory" \
  "$(run "(println (try (clojure.java.io/reader \"$rd_dir\") :no-throw (catch java.io.FileNotFoundException e (.getMessage e))))")" \
  "$rd_dir (Is a directory)"

# slurp of a PATH reached the file without the classification every constructor
# above goes through, so these three came back as a bare java.io.IOException
# carrying Chez's own wording -- uncatchable as the class the JVM raises and the
# class a library branches on (jolt-3ah).
printf 'content\n' > "$rd_dir/read.txt"
check "slurp missing path" \
  "$(run "(println (try (slurp \"$rd_dir/nope.txt\") :no-throw (catch java.io.FileNotFoundException e (.getMessage e))))")" \
  "$rd_dir/nope.txt (No such file or directory)"
# io/input-stream funnels its file arms through one opener, and that one was
# never guarded either -- a missing path answered with Chez's wording inside the
# right class, which no caller can read.
check "input-stream missing path" \
  "$(run "(println (try (clojure.java.io/input-stream \"$rd_dir/nope.txt\") :no-throw (catch java.io.FileNotFoundException e (.getMessage e))))")" \
  "$rd_dir/nope.txt (No such file or directory)"
check "input-stream on a directory" \
  "$(run "(println (try (clojure.java.io/input-stream \"$rd_dir\") :no-throw (catch java.io.FileNotFoundException e (.getMessage e))))")" \
  "$rd_dir (Is a directory)"
check "slurp on a directory" \
  "$(run "(println (try (slurp \"$rd_dir\") :no-throw (catch java.io.FileNotFoundException e (.getMessage e))))")" \
  "$rd_dir (Is a directory)"
# Running as root defeats the mode bits, so only assert this where they hold.
chmod 000 "$rd_dir/read.txt"
if [ -r "$rd_dir/read.txt" ]; then
  echo "SKIP: (slurp unreadable file) mode bits do not apply to this user"
else
  check "slurp unreadable file" \
    "$(run "(println (try (slurp \"$rd_dir/read.txt\") :no-throw (catch java.io.FileNotFoundException e (.getMessage e))))")" \
    "$rd_dir/read.txt (Permission denied)"
fi
chmod 644 "$rd_dir/read.txt"

# A target under a directory the process cannot write is the case a probe after
# the fact gets WRONG: the open says EACCES, and re-looking at the filesystem
# cannot even stat the target, so it would report it missing. The reason is read
# off the condition instead, so all three doors say what the JVM says.
mkdir -p "$rd_dir/ro"
chmod 555 "$rd_dir/ro"
if [ -w "$rd_dir/ro" ]; then
  echo "SKIP: (write into an unwritable directory) mode bits do not apply to this user"
else
  for form in "(spit \"$rd_dir/ro/x.txt\" \"y\")" \
              "(clojure.java.io/output-stream \"$rd_dir/ro/x.txt\")" \
              "(clojure.java.io/writer \"$rd_dir/ro/x.txt\")"; do
    check "write into an unwritable directory: $form" \
      "$(run "(println (try $form :no-throw (catch java.io.FileNotFoundException e (.getMessage e))))")" \
      "$rd_dir/ro/x.txt (Permission denied)"
  done
fi
chmod 755 "$rd_dir/ro"

# The WRITE side had the same gap, and io/writer had no check at all: it handed
# back a Writer over a directory and the failure surfaced at close, or never
# (jolt-g81).
check "spit to a directory" \
  "$(run "(println (try (spit \"$rd_dir\" \"x\") :no-throw (catch java.io.FileNotFoundException e (.getMessage e))))")" \
  "$rd_dir (Is a directory)"
check "output-stream on a directory" \
  "$(run "(println (try (clojure.java.io/output-stream \"$rd_dir\") :no-throw (catch java.io.FileNotFoundException e (.getMessage e))))")" \
  "$rd_dir (Is a directory)"
check "writer on a directory" \
  "$(run "(println (try (clojure.java.io/writer \"$rd_dir\") :no-throw (catch java.io.FileNotFoundException e (.getMessage e))))")" \
  "$rd_dir (Is a directory)"
check "output-stream into a missing directory" \
  "$(run "(println (try (clojure.java.io/output-stream \"$rd_dir/no/x.txt\") :no-throw (catch java.io.FileNotFoundException e (.getMessage e))))")" \
  "$rd_dir/no/x.txt (No such file or directory)"

# A BOM is content, not a signature: Chez's codec ate it and the JVM hands it
# back, so a Reader and a path read both used to lose the first character
# (jolt-qvt). The same file loaded as SOURCE now fails the way it does on the
# JVM and on babashka, which is the point.
printf '\357\273\277ab\n' > "$rd_dir/bom.txt"
check "reader keeps a leading BOM" \
  "$(run "(println (mapv int (slurp (clojure.java.io/reader \"$rd_dir/bom.txt\"))))")" \
  "[65279 97 98 10]"
check "slurp of a path keeps a leading BOM" \
  "$(run "(println (mapv int (slurp \"$rd_dir/bom.txt\")))")" \
  "[65279 97 98 10]"

# Descriptor exhaustion. Nothing is wrong with the path -- the PROCESS is out of
# descriptors -- so re-probing the filesystem cannot tell, and the classification
# has to come from the condition the open raised or the message blames the file's
# mode bits for the program's own leak. The witness holds readers open (io/reader
# keeps a descriptor as of jolt-0nk) until the next open has to fail.
emfile_expr="(let [p \"$rd_dir/read.txt\"
                   held (doall (for [_ (range 2000)] (try (clojure.java.io/reader p) (catch Throwable _ nil))))
                   n (count (remove nil? held))
                   r (try (slurp p) :no-throw (catch java.io.FileNotFoundException e (.getMessage e)))]
               (println (if (< n 8) :too-few-descriptors r)))"
emfile_out="$( ulimit -n 256 2>/dev/null && run "$emfile_expr" )"
case "$emfile_out" in
  ":too-few-descriptors"|"")
    echo "SKIP: (slurp under descriptor exhaustion) could not establish the limit" ;;
  *)
    check "slurp under descriptor exhaustion" \
      "$emfile_out" \
      "$rd_dir/read.txt (Too many open files)" ;;
esac
rm -rf "$rd_dir"

echo ""
echo "system-streams smoke: $pass passed, $fails failed"
[ "$fails" -eq 0 ]

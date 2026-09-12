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
check a2 "$(run '(println (class System/in))')" "java.io.InputStream"

# --- (b) slurp / read / line-seq over piped stdin -----------------------------
check b "$(run '(println (slurp System/in))' 'hello stdin')" "hello stdin"
check c "$(run '(println (pr-str (vec (line-seq (clojure.java.io/reader System/in)))))' 'a
b
c
')" "[\"a\" \"b\" \"c\"]"
# InputStream.read() is the next byte as an unsigned int, -1 at EOF.
check d "$(run '(println [(.read System/in) (.read System/in)])' 'AB')" "[65 66]"
check d2 "$(run '(println (.read System/in))')" "-1"

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

echo ""
echo "system-streams smoke: $pass passed, $fails failed"
[ "$fails" -eq 0 ]

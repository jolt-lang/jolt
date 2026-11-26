;; The interactive REPL's line editor: multi-line entry, parinfer smart mode
;; keeping the parens balanced, docs for the symbol at the cursor, completion
;; and history, drawn through the jolt.host/term-* seams (host/chez/terminal.ss).
;;
;; Portions are adapted from clojure-cli.repl,
;; https://github.com/clojure/clojure-cli.repl
;; Copyright (c) 2026 Jarrod Taylor and contributors.
;; Eclipse Public License 1.0: licenses/EPL-1.0.txt.
;; The adapted pieces are the token scanner (token-char?, token-at), the doc
;; preview capped to a quarter of the screen with the full doc on a second
;; press, and the Enter rule that submits a balanced entry only with the cursor
;; at its end.
(ns jolt.line-editor
  "Line editing for the interactive REPL. `editor` claims the terminal (nil
  when stdin and stdout are not one), `read-entry` reads one entry from it.

  Typing an open paren, bracket or brace gets its closer from parinfer's smart
  mode, which keeps the closers where the indentation puts them. Backspace
  deletes a form's content first and the empty form last; it steps over a
  closer instead of deleting it. Enter submits when the entry is complete and
  the cursor is at its end, and otherwise starts an indented new line; Alt-Enter
  always submits and Ctrl-J always starts a new line. Alt-D shows the doc for the
  symbol at the cursor below the entry, and a second Alt-D prints all of it.
  Tab completes the symbol before the cursor."
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [jolt.parinfer :as parinfer]))

;; -----------------------------------------------------------------------------
;; Lexical context

(def ^:private open->close {\( \) \[ \] \{ \}})
(def ^:private close->open {\) \( \] \[ \} \{})

(defn scan
  "One context keyword per char of text: :code, :comment, :escape (a backslash
  in code) and :escaped (the char it makes a literal), :quote-open and
  :quote-close (a string's quotes), :string, and :str-escape / :str-escaped
  inside a string."
  [^String text]
  (let [n (count text)]
    (loop [i 0 state :code acc (transient [])]
      (if (>= i n)
        (persistent! acc)
        (let [c (.charAt text i)]
          (case state
            :code
            (cond
              (= c \;) (recur (inc i) :comment (conj! acc :comment))
              (= c \") (recur (inc i) :string (conj! acc :quote-open))
              (= c \\) (if (< (inc i) n)
                         (recur (+ i 2) :code (-> acc (conj! :escape) (conj! :escaped)))
                         (recur (inc i) :code (conj! acc :escape)))
              :else (recur (inc i) :code (conj! acc :code)))
            :string
            (cond
              (= c \\) (if (< (inc i) n)
                         (recur (+ i 2) :string (-> acc (conj! :str-escape) (conj! :str-escaped)))
                         (recur (inc i) :string (conj! acc :str-escape)))
              (= c \") (recur (inc i) :code (conj! acc :quote-close))
              :else (recur (inc i) :string (conj! acc :string)))
            :comment
            (if (= c \newline)
              (recur (inc i) :code (conj! acc :code))
              (recur (inc i) :comment (conj! acc :comment)))))))))

(defn- ctx-before [ctx i] (when (pos? i) (nth ctx (dec i))))

(defn- in-string? [ctx i]
  (contains? #{:quote-open :string :str-escape :str-escaped} (ctx-before ctx i)))

(defn- in-comment? [ctx i] (= :comment (ctx-before ctx i)))

(defn- pairs
  "Each matched paren's index mapped to its partner's."
  [^String text ctx]
  (let [n (count text)]
    (loop [i 0 stack () m {}]
      (if (>= i n)
        m
        (let [c (.charAt text i)]
          (if (= :code (nth ctx i))
            (cond
              (open->close c) (recur (inc i) (cons i stack) m)
              (close->open c) (if (and (seq stack) (= (close->open c) (.charAt text (first stack))))
                                (recur (inc i) (rest stack) (assoc m (first stack) i i (first stack)))
                                (recur (inc i) stack m))
              :else (recur (inc i) stack m))
            (recur (inc i) stack m)))))))

(defn- open-stack
  "The openers still open at position i, innermost first."
  [^String text ctx i]
  (loop [j 0 stack ()]
    (if (>= j i)
      stack
      (let [c (.charAt text j)]
        (if (= :code (nth ctx j))
          (cond
            (open->close c) (recur (inc j) (cons j stack))
            (close->open c) (recur (inc j) (if (and (seq stack) (= (close->open c) (.charAt text (first stack))))
                                             (rest stack)
                                             stack))
            :else (recur (inc j) stack))
          (recur (inc j) stack))))))

;; -----------------------------------------------------------------------------
;; Tokens and lines

(defn- token-char? [c]
  (not (#{\space \tab \newline \return \, \( \) \[ \] \{ \} \" \; \' \` \~ \@ \^ \\} c)))

(defn- token-start [^String text i]
  (loop [j i]
    (if (and (pos? j) (token-char? (.charAt text (dec j)))) (recur (dec j)) j)))

(defn- token-end [^String text i]
  (let [n (count text)]
    (loop [j i]
      (if (and (< j n) (token-char? (.charAt text j))) (recur (inc j)) j))))

(defn token-at
  "The token at the cursor or just before it, nil when there is none."
  [^String text cursor]
  (let [n (count text)
        step-back? (or (= cursor n) (#{\space \newline \) \] \}} (.charAt text cursor)))
        pos (if step-back? (dec cursor) cursor)]
    (when (and (<= 0 pos) (< pos n) (token-char? (.charAt text pos)))
      (subs text (token-start text pos) (token-end text pos)))))

(defn- line-start [^String text i]
  (loop [j i]
    (if (and (pos? j) (not= \newline (.charAt text (dec j)))) (recur (dec j)) j)))

(defn- line-end [^String text i]
  (let [n (count text)]
    (loop [j i]
      (if (and (< j n) (not= \newline (.charAt text j))) (recur (inc j)) j))))

(defn- line-col
  "[line x] of index i."
  [^String text i]
  (loop [j 0 line 0 start 0]
    (if (>= j i)
      [line (- i start)]
      (if (= \newline (.charAt text j))
        (recur (inc j) (inc line) (inc j))
        (recur (inc j) line start)))))

(defn- index-at [^String text line x]
  (let [n (count text)]
    (loop [j 0 l 0]
      (cond
        (= l line) (min n (+ j x))
        (>= j n) n
        :else (recur (inc (line-end text j)) (inc l))))))

;; -----------------------------------------------------------------------------
;; Indentation

(defn- digit? [c] (<= (int \0) (int c) (int \9)))

(defn- symbol-head? [c]
  (and (token-char? c) (not (digit? c)) (not= c \:) (not= c \#)))

(defn indent-for
  "Columns to indent a new line started at i: two past a list whose head is a
  symbol, aligned with a non-symbol head's first argument when one follows it
  on the same line, else one past the open paren, bracket or brace."
  [^String text ctx i]
  (if-let [o (first (open-stack text ctx i))]
    (let [ls (line-start text o)
          col (- o ls)
          h (inc o)
          n (count text)]
      (if (and (= \( (.charAt text o)) (< h n))
        (if (symbol-head? (.charAt text h))
          (+ col 2)
          (let [head-end (cond
                           (token-char? (.charAt text h)) (token-end text h)
                           :else (inc (get (pairs text ctx) h h)))
                arg (loop [j head-end]
                      (if (and (< j i) (#{\space \,} (.charAt text j))) (recur (inc j)) j))]
            (if (and (> arg head-end) (< arg i) (< arg (line-end text o))
                     (not= \; (.charAt text arg)))
              (- arg ls)
              (inc col))))
        (inc col)))
    0))

;; -----------------------------------------------------------------------------
;; Editing model
;;
;; A state is {:text :cursor} plus the kill buffer and the history position.
;; A command edits it; edit then lets parinfer reconcile the result, telling it
;; the change made and where the cursor was, as an editor plugin does.

(defn initial-state [] {:text "" :cursor 0})

(defn- move [st i]
  (assoc st :cursor (max 0 (min (count (:text st)) i))))

(defn- splice
  "Replace [start end) of the text with s and put the cursor at cursor."
  [st start end s cursor]
  (let [text (:text st)]
    (assoc st
           :text (str (subs text 0 start) s (subs text end))
           :cursor cursor
           ::change {:start start :end end :new s})))

(defn- insert [st s]
  (let [c (:cursor st)]
    (splice st c c s (+ c (count s)))))

(defn- bell [st] (assoc st ::bell true))

(defn- close-form
  "A typed closer moves past the close of the innermost form it can close,
  dropping the spaces left in front of that close, as paredit does."
  [st ctx c]
  (let [{:keys [^String text cursor]} st
        o (first (filter #(= c (open->close (.charAt text %))) (open-stack text ctx cursor)))
        m (when o (get (pairs text ctx) o))]
    (if m
      (let [start (loop [j m]
                    (if (and (> j (inc o)) (#{\space \tab} (.charAt text (dec j)))) (recur (dec j)) j))]
        (if (< start m)
          (splice st start m "" (inc start))
          (move st (inc m))))
      (bell st))))

(defn- type-char [st c]
  (let [{:keys [^String text cursor]} st
        ctx (scan text)
        before (ctx-before ctx cursor)]
    (cond
      (= :escape before) (insert st (str c))
      (in-comment? ctx cursor) (insert st (str c))
      (in-string? ctx cursor)
      (cond
        (not= c \") (insert st (str c))
        (= :str-escape before) (insert st "\"")
        (= :quote-close (get ctx cursor)) (move st (inc cursor))
        :else (insert st "\\\""))
      (= c \") (splice st cursor cursor "\"\"" (inc cursor))
      (close->open c) (close-form st ctx c)
      :else (insert st (str c)))))

(defn- dispatch-before?
  "Is the opener at i the second char of a #{ #( or #\" dispatch?"
  [^String text ctx i]
  (and (pos? i) (= \# (.charAt text (dec i))) (= :code (nth ctx (dec i)))))

(defn- backspace [st]
  (let [{:keys [^String text cursor]} st]
    (if (zero? cursor)
      st
      (let [ctx (scan text)
            p (dec cursor)
            c (.charAt text p)
            del (fn [start end] (splice st start end "" start))
            whole (fn [] (del (if (dispatch-before? text ctx p) (dec p) p) (inc cursor)))]
        (case (nth ctx p)
          (:escaped :str-escaped) (del (dec p) cursor)
          :quote-open (if (= :quote-close (get ctx cursor)) (whole) (move st p))
          :quote-close (move st p)
          :code (cond
                  (open->close c) (let [m (get (pairs text ctx) p)]
                                    (cond (= m cursor) (whole)
                                          m (move st p)
                                          :else (del p cursor)))
                  (close->open c) (if (get (pairs text ctx) p) (move st p) (del p cursor))
                  :else (del p cursor))
          (del p cursor))))))

(defn- delete-forward [st]
  (let [{:keys [^String text cursor]} st
        n (count text)]
    (if (>= cursor n)
      st
      (let [ctx (scan text)
            c (.charAt text cursor)
            del (fn [start end] (splice st start end "" start))]
        (case (nth ctx cursor)
          (:escape :str-escape) (del cursor (min n (+ cursor 2)))
          :quote-open (if (= :quote-close (get ctx (inc cursor)))
                        (del cursor (+ cursor 2))
                        (move st (inc cursor)))
          :quote-close (if (= :quote-open (get ctx (dec cursor)))
                         (del (dec cursor) (inc cursor))
                         st)
          :code (cond
                  (open->close c) (let [m (get (pairs text ctx) cursor)]
                                    (cond (= m (inc cursor)) (del cursor (inc m))
                                          m (move st (inc cursor))
                                          :else (del cursor (inc cursor))))
                  (close->open c) (let [o (get (pairs text ctx) cursor)]
                                    (cond (= o (dec cursor)) (del o (inc cursor))
                                          o st
                                          :else (del cursor (inc cursor))))
                  :else (del cursor (inc cursor)))
          (del cursor (inc cursor)))))))

(defn- newline-indent [st]
  (let [{:keys [^String text cursor]} st
        ctx (scan text)]
    (if (in-string? ctx cursor)
      (insert st "\n")
      (let [n (count text)
            ls (line-start text cursor)
            start (loop [j cursor]
                    (if (and (> j ls) (= \space (.charAt text (dec j)))) (recur (dec j)) j))
            end (loop [j cursor]
                  (if (and (< j n) (= \space (.charAt text j))) (recur (inc j)) j))
            s (str "\n" (apply str (repeat (indent-for text ctx start) \space)))]
        (splice st start end s (+ start (count s)))))))

(defn- home [st]
  (let [{:keys [^String text cursor]} st
        ls (line-start text cursor)
        n (count text)
        first-char (loop [j ls]
                     (if (and (< j n) (#{\space \tab} (.charAt text j))) (recur (inc j)) j))]
    (move st (if (= cursor first-char) ls first-char))))

(defn- line-up [st]
  (let [{:keys [text cursor]} st
        ls (line-start text cursor)]
    (if (zero? ls)
      (assoc st ::edge :up)
      (let [pls (line-start text (dec ls))]
        (move st (min (+ pls (- cursor ls)) (dec ls)))))))

(defn- line-down [st]
  (let [{:keys [text cursor]} st
        le (line-end text cursor)]
    (if (= le (count text))
      (assoc st ::edge :down)
      (let [nls (inc le)]
        (move st (min (+ nls (- cursor (line-start text cursor))) (line-end text nls)))))))

(defn- word-left-index [^String text cursor]
  (let [j (loop [j cursor]
            (if (and (pos? j) (not (token-char? (.charAt text (dec j))))) (recur (dec j)) j))]
    (token-start text j)))

(defn- word-right-index [^String text cursor]
  (let [n (count text)
        j (loop [j cursor]
            (if (and (< j n) (not (token-char? (.charAt text j)))) (recur (inc j)) j))]
    (token-end text j)))

(defn- kill [st start end]
  (if (< start end)
    (assoc (splice st start end "" start) :kill (subs (:text st) start end))
    st))

(defn- kill-line
  "Kill to the end of the line, stopping at the close of the form the cursor
  is in; at the end of a line, join the next one."
  [st]
  (let [{:keys [^String text cursor]} st
        le (line-end text cursor)
        ctx (scan text)
        o (first (open-stack text ctx cursor))
        m (when o (get (pairs text ctx) o))
        end (if (and m (<= cursor m) (< m le)) m le)]
    (cond
      (< cursor end) (kill st cursor end)
      (and (= cursor le) (< cursor (count text))) (kill st cursor (inc cursor))
      :else st)))

(defn- command [st cmd]
  (let [{:keys [text cursor]} st
        [op arg] (if (vector? cmd) cmd [cmd])]
    (case op
      :char (type-char st arg)
      :insert (insert st arg)
      :paste (insert st (-> arg (str/replace "\r\n" "\n") (str/replace "\r" "\n")))
      :backspace (backspace st)
      :delete (delete-forward st)
      :newline (newline-indent st)
      :left (move st (dec cursor))
      :right (move st (inc cursor))
      :home (home st)
      :end (move st (line-end text cursor))
      :buffer-start (move st 0)
      :buffer-end (move st (count text))
      :up (line-up st)
      :down (line-down st)
      :word-left (move st (word-left-index text cursor))
      :word-right (move st (word-right-index text cursor))
      :kill-line (kill-line st)
      :kill-line-back (kill st (line-start text cursor) cursor)
      :kill-word-back (kill st (word-left-index text cursor) cursor)
      :yank (if-let [k (:kill st)] (insert st k) st)
      st)))

(defn- change-of [old {:keys [start end new]}]
  (let [[line x] (line-col (:text old) start)]
    {:line-no line :x x :old-text (subs (:text old) start end) :new-text new}))

(defn- reconcile
  "Run parinfer over the edited state. A paste goes through paren mode, which
  keeps the pasted parens and fixes indentation; everything else through smart
  mode. A text parinfer cannot process (a string left open) stays as typed."
  [old new paste?]
  (let [text (:text new)
        [cl cx] (line-col text (:cursor new))
        r (if paste?
            (parinfer/paren-mode text {:cursor-line cl :cursor-x cx})
            (let [[pl px] (line-col (:text old) (:cursor old))]
              (parinfer/smart-mode text
                                   (cond-> {:cursor-line cl :cursor-x cx
                                            :prev-cursor-line pl :prev-cursor-x px}
                                     (::change new) (assoc :changes [(change-of old (::change new))])))))]
    (dissoc (if (:success r)
              (assoc new :text (:text r) :cursor (index-at (:text r) (:cursor-line r) (:cursor-x r)))
              new)
            ::change)))

(defn edit
  "Apply one editing command to state and let parinfer reconcile it."
  [st cmd]
  (let [old (dissoc st ::change ::edge ::bell)
        new (command old cmd)]
    (if (and (= (:text old) (:text new)) (= (:cursor old) (:cursor new)))
      new
      (reconcile old new (and (vector? cmd) (= :paste (first cmd)))))))

(defn submit?
  "Does Enter submit? Only a complete entry (no form or string left open) with
  the cursor at its end; elsewhere Enter starts a new line."
  [^String text cursor]
  (let [ctx (scan text)
        n (count text)]
    (and (str/blank? (subs text cursor))
         (not (in-string? ctx n))
         (empty? (open-stack text ctx n)))))

;; -----------------------------------------------------------------------------
;; Keys

(defn- decode-csi [next-char]
  (loop [params ""]
    (let [c (next-char true)]
      (cond
        (not (char? c)) :ignore
        (<= 0x40 (int c) 0x7e)
        (let [[p1 mods] (str/split params #";")]
          (case c
            \A :up
            \B :down
            \C (if mods :word-right :right)
            \D (if mods :word-left :left)
            \H :home
            \F :end
            \~ (case p1
                 ("1" "7") :home
                 ("4" "8") :end
                 "3" :delete
                 :ignore)
            :ignore))
        :else (recur (str params c))))))

(defn- decode-escape [next-char]
  (let [c (next-char true)]
    (cond
      (nil? c) :eof
      (not (char? c)) :ignore
      (= c \[) (decode-csi next-char)
      (= c \O) (case (next-char true)
                 \A :up \B :down \C :right \D :left \H :home \F :end
                 :ignore)
      (= c \return) :submit
      (#{\u007f \backspace} c) :kill-word-back
      :else (case c
              \d :doc
              \b :word-left
              \f :word-right
              \< :buffer-start
              \> :buffer-end
              :ignore))))

(defn decode-key
  "Read one key through next-char (called with true to block) and name it: a
  command keyword, [:char c], :eof, or :resize."
  [next-char]
  (let [c (next-char true)]
    (cond
      (nil? c) :eof
      (not (char? c)) c
      :else
      (case (int c)
        13 :enter
        10 :newline
        (127 8) :backspace
        9 :tab
        27 (decode-escape next-char)
        1 :home
        5 :end
        2 :left
        6 :right
        16 :up
        14 :down
        4 :ctrl-d
        3 :interrupt
        7 :cancel
        11 :kill-line
        21 :kill-line-back
        23 :kill-word-back
        25 :yank
        12 :clear
        26 :suspend
        (if (< (int c) 32) :ignore [:char c])))))

;; -----------------------------------------------------------------------------
;; Layout

(defn- cell
  "How char c shows: [text columns]."
  [c width]
  (let [i (int c)]
    (cond
      (= c \tab) ["  " 2]
      (< i 32) [(str "^" (char (+ i 64))) 2]
      (= i 127) ["^?" 2]
      (< i 128) [(str c) 1]
      :else [(str c) (max 0 (width c))])))

(defn layout
  "The screen rows for an entry: {:rows [[[text style] ...] ...] :cursor [row
  col]}. The first line follows prompt, the rest a blank prompt of the same
  width, so columns line up; lines wrap one column short of the edge, which is
  never written. below, when given, follows the entry dimmed, one row per line,
  cut at the width. width gives a non-ASCII char's columns."
  [{:keys [prompt text cursor below]} cols width]
  (let [wmax (max 2 (dec cols))
        cells (fn [s] (map #(cell % width) s))
        cont (apply str (repeat (reduce + (map second (cells prompt))) \space))
        rows (volatile! [])
        row (volatile! [])
        col (volatile! 0)
        cur (volatile! [0 0])
        new-row! (fn [] (vswap! rows conj @row) (vreset! row []) (vreset! col 0))
        emit! (fn [s w style]
                (when (> (+ @col w) wmax) (new-row!))
                (let [r @row
                      [ls lstyle] (peek r)]
                  (vreset! row (if (and (seq r) (= lstyle style))
                                 (conj (pop r) [(str ls s) style])
                                 (conj r [s style]))))
                (vswap! col + w))
        lines (str/split text #"\n" -1)]
    (loop [li 0 idx 0]
      (when (< li (count lines))
        (when (pos? li) (new-row!))
        (doseq [[s w] (cells (if (zero? li) prompt cont))]
          (emit! s w :prompt))
        (let [^String line (nth lines li)]
          (dotimes [x (count line)]
            (let [[s w] (cell (.charAt line x) width)]
              (when (= (+ idx x) cursor)
                (when (> (+ @col w) wmax) (new-row!))
                (vreset! cur [(count @rows) @col]))
              (emit! s w nil)))
          (when (= (+ idx (count line)) cursor)
            (vreset! cur [(count @rows) @col]))
          (recur (inc li) (+ idx (count line) 1)))))
    (vswap! rows conj @row)
    (doseq [line (when below (str/split-lines below))]
      (let [shown (loop [acc "" w 0 [c & more] (seq line)]
                    (if (nil? c)
                      acc
                      (let [[s cw] (cell c width)]
                        (if (> (+ w cw) wmax) acc (recur (str acc s) (+ w cw) more)))))]
        (vswap! rows conj (if (seq shown) [[shown :dim]] []))))
    {:rows @rows :cursor @cur}))

;; -----------------------------------------------------------------------------
;; History

(def ^:private history-limit 1000)

(defn- default-history-file []
  (or (System/getenv "JOLT_REPL_HISTORY")
      (str (System/getProperty "user.home") "/.jolt_repl_history")))

(defn- load-history
  "Entries one per line, each a string literal, oldest first."
  [file]
  (try
    (let [entries (->> (str/split-lines (slurp file))
                       (keep #(try (let [s (edn/read-string %)] (when (string? s) s))
                                   (catch Exception _ nil)))
                       vec)]
      (if (> (count entries) (* 2 history-limit))
        (let [kept (subvec entries (- (count entries) history-limit))]
          (spit file (apply str (map #(str (pr-str %) "\n") kept)))
          kept)
        entries))
    (catch Exception _ [])))

(defn- remember! [ed entry]
  (let [{:keys [history history-file]} @ed]
    (when-not (or (str/blank? entry) (= entry (peek history)))
      (swap! ed update :history conj entry)
      (try (spit history-file (str (pr-str entry) "\n") :append true)
           (catch Exception _ nil)))))

(defn- recall
  "Step through history from an edge of the entry: :up to older, :down back."
  [ed st dir]
  (let [history (:history @ed)
        n (count history)
        idx (:hist-idx st n)
        to (if (= dir :up) (dec idx) (inc idx))]
    (if (or (neg? to) (> to n))
      (bell st)
      (let [st (if (= idx n) (assoc st :draft (:text st)) st)
            text (if (= to n) (:draft st "") (nth history to))]
        (assoc st :text text :cursor (count text) :hist-idx to)))))

;; -----------------------------------------------------------------------------
;; Docs and completion

(defn- default-doc
  "The doc for the symbol named by token, resolved in *ns*."
  [token]
  (try
    (not-empty (with-out-str (eval (list 'clojure.repl/doc (symbol token)))))
    (catch Throwable _ nil)))

(defn- default-candidates
  "Names visible from *ns* for a completion prefix: its mappings, aliases and
  namespaces, or an alias or namespace's publics after a slash."
  [prefix]
  (let [ns *ns*]
    (if-let [i (str/index-of prefix "/")]
      (let [qual (subs prefix 0 i)
            target (or (get (ns-aliases ns) (symbol qual)) (find-ns (symbol qual)))]
        (when target
          (map #(str qual "/" %) (keys (ns-publics target)))))
      (concat (map str (keys (ns-map ns)))
              (map #(str % "/") (keys (ns-aliases ns)))
              (map #(str (ns-name %)) (all-ns))))))

(defn- doc-preview
  "At most cap lines of the doc, with a pointer to the rest."
  [text cap]
  (let [lines (str/split-lines text)]
    (if (<= (count lines) cap)
      text
      (str (str/join "\n" (take cap lines)) "\n... Alt-D again for the full doc"))))

(defn- common-prefix [strs]
  (reduce (fn [a b]
            (let [n (min (count a) (count b))]
              (subs a 0 (loop [i 0] (if (and (< i n) (= (.charAt ^String a i) (.charAt ^String b i))) (recur (inc i)) i)))))
          strs))

(defn- columns
  "Candidates packed into lines no wider than width, at most cap of them."
  [cands width cap]
  (let [colw (+ 2 (apply max (map count cands)))
        per (max 1 (quot width colw))
        lines (map (fn [row] (str/trimr (apply str (map #(format (str "%-" colw "s") %) row))))
                   (partition-all per cands))]
    (str/join "\n" (if (> (count lines) cap)
                     (concat (take cap lines) [(str "... " (count cands) " candidates")])
                     lines))))

;; -----------------------------------------------------------------------------
;; The terminal

(defn editor
  "Claim the terminal for line editing and return the editor, or nil when
  stdin and stdout are not an interactive terminal (a pipe, a file, or
  TERM=dumb). Options: :history-file, :doc-fn (token -> doc text or nil),
  :complete-fn (prefix -> candidate names)."
  ([] (editor {}))
  ([{:keys [history-file doc-fn complete-fn]}]
   (when (jolt.host/term-open?)
     (let [file (or history-file (default-history-file))]
       (atom {:history (load-history file)
              :history-file file
              :doc-fn (or doc-fn default-doc)
              :complete-fn (or complete-fn default-candidates)
              :drawn-row 0
              :pending []})))))

(defn- term-width [c] (jolt.host/term-char-width c))

(defn- screen-size [] (let [[rows cols] (jolt.host/term-size)] [rows cols]))

(defn- write-row! [row]
  (doseq [[s style] row]
    (if (= style :dim)
      (do (jolt.host/term-color! 8 false)
          (jolt.host/term-write! s)
          (jolt.host/term-color! -1 false))
      (jolt.host/term-write! s))))

(defn- draw!
  "Redraw the entry (and anything below it) over what was drawn last."
  [ed st]
  (let [[rows cols] (screen-size)
        lay (layout st cols term-width)
        out (:rows lay)
        ;; what fits on the screen below the entry
        entry-rows (inc (first (:cursor (layout (dissoc st :below) cols term-width))))
        out (subvec out 0 (min (count out) (max entry-rows (dec rows))))
        [cr cc] (:cursor lay)]
    (jolt.host/term-move! :up (:drawn-row @ed))
    (jolt.host/term-cr!)
    (jolt.host/term-clear! :eos)
    (dotimes [i (count out)]
      (when (pos? i) (jolt.host/term-cr!) (jolt.host/term-lf!))
      (write-row! (nth out i)))
    (jolt.host/term-move! :up (- (dec (count out)) cr))
    (jolt.host/term-cr!)
    (jolt.host/term-move! :right cc)
    (jolt.host/term-flush!)
    (swap! ed assoc :drawn-row cr)))

(defn- write-lines!
  "Write text a line at a time; raw mode does no newline translation."
  [text]
  (doseq [line (str/split-lines text)]
    (jolt.host/term-write! line)
    (jolt.host/term-cr!)
    (jolt.host/term-lf!)))

(defn- print-above!
  "Write text where the entry was, then redraw the entry under it."
  [ed st text]
  (jolt.host/term-move! :up (:drawn-row @ed))
  (jolt.host/term-cr!)
  (jolt.host/term-clear! :eos)
  (write-lines! text)
  (swap! ed assoc :drawn-row 0)
  (draw! ed st))

(defn- finish!
  "Draw the entry as submitted and leave the cursor on the line after it."
  [ed st suffix]
  (draw! ed (assoc st :below nil :cursor (count (:text st))))
  (when suffix (jolt.host/term-write! suffix))
  (jolt.host/term-cr!)
  (jolt.host/term-lf!)
  (jolt.host/term-flush!))

(defn- read-key
  "The next key. Chars that arrive together with it are a paste: they come back
  as one [:paste text], a final newline in it as the Enter that follows."
  [ed]
  (if-let [k (first (:pending @ed))]
    (do (swap! ed update :pending subvec 1) k)
    (let [k (decode-key jolt.host/term-read-char)
          paste-char (if (vector? k)
                       (second k)
                       ({:enter \newline :newline \newline :tab \tab} k))]
      (if-let [more (when paste-char
                      ;; a paste can be long: one buffer, not a string copy per char
                      (let [sb (StringBuilder.)]
                        (loop []
                          (let [c (jolt.host/term-read-char false)]
                            (when (char? c) (.append sb c) (recur))))
                        (when (pos? (.length sb)) (.toString sb))))]
        (let [text (str paste-char more)
              text (str/replace (str/replace text "\r\n" "\n") "\r" "\n")]
          (if (str/ends-with? text "\n")
            (do (swap! ed update :pending conj :enter)
                [:paste (subs text 0 (dec (count text)))])
            [:paste text]))
        k))))

(defn- show-doc [ed st]
  (let [{:keys [text cursor doc-token doc-text]} st
        token (token-at text cursor)]
    (cond
      (and token (= token doc-token))
      (do (print-above! ed (assoc st :below nil) doc-text)
          (assoc st :below nil :doc-token nil))

      token
      (let [d (some-> ((:doc-fn @ed) token) str/trim-newline)
            [rows _] (screen-size)]
        (if (str/blank? d)
          (bell (assoc st :below nil :doc-token nil))
          (assoc st :below (doc-preview d (max 1 (quot rows 4))) :doc-token token :doc-text d)))

      :else (assoc st :below nil :doc-token nil))))

(defn- complete [ed st]
  (let [{:keys [^String text cursor]} st
        prefix (subs text (token-start text cursor) cursor)]
    (if (str/blank? prefix)
      (bell st)
      (let [cands (->> ((:complete-fn @ed) prefix)
                       (filter #(str/starts-with? % prefix))
                       distinct
                       sort)
            [rows cols] (screen-size)]
        (case (count cands)
          0 (bell st)
          1 (assoc (edit st [:insert (subs (first cands) (count prefix))]) :below nil)
          (let [common (common-prefix cands)]
            (if (> (count common) (count prefix))
              (edit st [:insert (subs common (count prefix))])
              (assoc st :below (columns cands (dec cols) (max 1 (quot rows 4)))))))))))

(defn- step
  "Handle one key: {:state st} to go on, {:done text} to return."
  [ed st k]
  (case k
    :eof (if (str/blank? (:text st)) (do (finish! ed st nil) {:done nil}) {:state st})
    :ctrl-d (if (empty? (:text st)) (do (finish! ed st nil) {:done nil}) {:state (edit st :delete)})
    ;; ^C abandons what is typed, and on an empty entry ends the session
    :interrupt (do (finish! ed st "^C") {:done (when-not (empty? (:text st)) "")})
    :enter (if (submit? (:text st) (:cursor st))
             (do (finish! ed st nil) {:done (:text st)})
             {:state (edit st :newline)})
    :submit (do (finish! ed st nil) {:done (:text st)})
    :resize {:state st}
    :clear (do (jolt.host/term-clear! :screen) (swap! ed assoc :drawn-row 0) {:state st})
    :suspend (do (jolt.host/term-cooked!)
                 (jolt.host/term-pause!)
                 (jolt.host/term-raw!)
                 (swap! ed assoc :drawn-row 0)
                 {:state st})
    :cancel {:state (assoc st :below nil :doc-token nil)}
    :doc {:state (show-doc ed st)}
    :tab {:state (complete ed st)}
    :ignore {:state st}
    (let [st' (edit st k)]
      {:state (if-let [dir (::edge st')]
                (recall ed (dissoc st' ::edge) dir)
                st')})))

(defn read-entry
  "Read one entry at the terminal after prompt. Returns its text (\"\" when
  Ctrl-C abandons it), or nil at end of input (Ctrl-D or Ctrl-C on an empty
  entry)."
  [ed prompt]
  ;; whatever jolt printed has to reach the terminal before the editor draws
  (flush)
  (binding [*out* *err*] (flush))
  (jolt.host/term-raw!)
  (try
    (swap! ed assoc :drawn-row 0 :pending [])
    (loop [st (assoc (initial-state) :prompt prompt :hist-idx (count (:history @ed)))]
      (draw! ed st)
      (let [{:keys [state done] :as r} (step ed st (read-key ed))]
        (if (contains? r :done)
          (do (when done (remember! ed done)) done)
          (do (when (::bell state) (jolt.host/term-bell!))
              (recur (dissoc state ::bell))))))
    (finally
      (jolt.host/term-cooked!))))

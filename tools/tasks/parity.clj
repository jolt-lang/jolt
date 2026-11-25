;; taskparity: the Makefile and deps.edn are both written by hand while both
;; exist, so this fails when they stop describing the same build. A gate added
;; to the Makefile's CI-GATES and not to tools/tasks/gate.clj would otherwise
;; never run under `jolt ci`, and `jolt ci` would still say OK: an incomplete
;; gate read as a pass.
;;
;; It checks that every Makefile target has a task (and every task a target,
;; past the few that exist only on one side), and that CI-GATES, TEST-GATES
;; and QUIET-GATES match gate.clj's lists. It reads the Makefile as text and
;; starts no processes, so it runs the same under `make taskparity` and
;; `jolt taskparity`.
(ns tasks.parity
  (:require [clojure.set :as set]
            [clojure.string :as str]))

;; Present on one side only, on purpose.
(def make-only #{"gate-run-test" "gate-run-ci" "gate-run-bionic-ci" "gate-run-ci-shard"
                 ;; `jolt install` runs ./install, so the task is install-local
                 "install"})
(def jolt-only #{"chez-path" "install-local" "info"})

(defn- logical-lines
  "The Makefile's lines with backslash continuations joined."
  [text]
  (str/split-lines (str/replace text #"\\\n" " ")))

(defn makefile-targets [lines]
  (into (sorted-set)
        (keep #(second (re-find #"^([A-Za-z0-9_-]+)::?(?:\s|$)" %)))
        lines))

(defn makefile-list [lines var-name]
  (some (fn [l]
          (when-let [[_ v] (re-find (re-pattern (str "^" var-name " :?= ?(.*)$")) l)]
            (vec (remove str/blank? (str/split (str/trim v) #"\s+")))))
        lines))

(defn task-names [deps-text]
  (into (sorted-set)
        (comp (filter symbol?) (map str))
        (keys (:tasks (read-string deps-text)))))

(defn- report [label a b & [side-a side-b]]
  (let [side-a (or side-a "in the Makefile only:")
        side-b (or side-b "in deps.edn only:    ")
        only-a (set/difference (set a) (set b))
        only-b (set/difference (set b) (set a))]
    (when (or (seq only-a) (seq only-b))
      (str label
           (when (seq only-a) (str "\n  " side-a " " (str/join " " (sort only-a))))
           (when (seq only-b) (str "\n  " side-b " " (str/join " " (sort only-b))))))))

(defn check [root]
  (let [lines (logical-lines (slurp (str root "/Makefile")))
        ;; gate.clj requires tasks.env, which deps.edn's :init loads first
        gate-ns (do (when-not (find-ns 'tasks.env) (load-file (str root "/tools/tasks/env.clj")))
                    (load-file (str root "/tools/tasks/gate.clj"))
                    (find-ns 'tasks.gate))
        gate-var #(map str @(ns-resolve gate-ns %))]
    (keep identity
          [(report "targets differ (add the task to deps.edn, or the target to the Makefile):"
                   (remove make-only (makefile-targets lines))
                   (remove jolt-only (task-names (slurp (str root "/deps.edn")))))
           (report "CI-GATES differs from tools/tasks/gate.clj ci-gates:"
                   (makefile-list lines "CI-GATES") (gate-var 'ci-gates))
           (report "TEST-GATES differs from tools/tasks/gate.clj test-gates:"
                   (makefile-list lines "TEST-GATES") (gate-var 'test-gates))
           (report "QUIET-GATES differs from tools/tasks/gate.clj quiet-gates:"
                   (makefile-list lines "QUIET-GATES") (gate-var 'quiet-gates))
           (let [_ (load-file (str root "/tools/tasks/info.clj"))
                 grouped @(ns-resolve (find-ns 'tasks.info) 'grouped-names)]
             (report "tasks and tools/tasks/info.clj's groups differ (put each task in one group):"
                     (task-names (slurp (str root "/deps.edn")))
                     (grouped)
                     "tasks in no group:" "grouped, no task: "))])))

(defn main! [root]
  (let [problems (check root)]
    (if (seq problems)
      (do (doseq [p problems] (println (str "taskparity: " p)))
          (System/exit 1))
      (println "taskparity: the Makefile and deps.edn name the same targets and gates"))))

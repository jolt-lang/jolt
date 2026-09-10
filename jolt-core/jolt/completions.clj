(ns jolt.completions
  "`jolt completions` — shell completion for jolt's own commands and a project's
  tasks.

    jolt completions zsh    # the function to source, for zsh / bash / fish
    jolt completions tasks  # name<TAB>doc per listable task, for that function

  The split between those two is the whole design, and it is a departure from
  babashka, which calls its binary back on every TAB press to compute the
  candidates for the line so far.

  Jolt cannot afford that. Its floor is the Chez runtime coming up, about 0.22s
  whatever it is asked for, and `jolt version` costs the same as `jolt tasks`.
  A quarter of a second between pressing TAB and seeing anything is the
  difference between completion that feels instant and completion that feels
  broken, and no amount of care inside the process removes it.

  So the two halves are split by how often they change. jolt's own commands and
  options change when the binary does, so they are baked into the snippet at the
  moment it is generated and cost nothing afterwards. A project's tasks change
  when its deps.edn or bb.edn changes, so the snippet caches them against those
  two mtimes and calls back only when one of them moves. A warm press reads a
  small file, and jolt does not run at all."
  (:require [jolt.tasks :as tasks]))

;; --- what jolt itself answers to ---------------------------------------------

;; Baked into a generated snippet, so this list is as current as the binary that
;; generated it and costs a completing shell nothing. Kept beside main's usage
;; text: a command that appears in one and not the other is a bug in whichever
;; was edited alone.
(def ^:private commands
  [["repl"         "start a REPL"]
   ["nrepl-server" "start an nREPL server (default 7888) for editors"]
   ["run"          "load a file, or run -m NS"]
   ["build"        "compile a standalone binary or shared library"]
   ["path"         "print the resolved source roots"]
   ["tasks"        "list the project's bb.edn/deps.edn :tasks"]
   ["completions"  "print a shell completion snippet"]
   ["help"         "print usage"]
   ["version"      "print the jolt version"]])

(def ^:private options
  [["-e"         "evaluate EXPR and print the result"]
   ["-m"         "resolve deps.edn, load NS, call its -main"]
   ["-f"         "load FILE, whose name may be a command or a task"]
   ["-M"         "run an alias's :main-opts"]
   ["-A"         "add an alias's paths and deps"]
   ["-X"         "invoke an alias's :exec-fn"]
   ["-T"         "like -X, with the project's paths replaced"]
   ["-P"         "fetch every dependency, then stop"]
   ["-Spath"     "print the resolved source roots"]
   ["-Stree"     "print the dependency tree"]
   ["-Sgraph"    "print the dependency tree as an indented graph"]
   ["-Soutdated" "print the dependency graph, marking available updates"]
   ["-Strace"    "write the dep expansion to trace.edn"]
   ["-Sdescribe" "print the environment as an edn map"]
   ["-Sdeps"     "merge an extra deps.edn map"]
   ["-Scp"       "run against these roots, expanding no deps"]
   ["-Srepro"    "ignore the user deps.edn"]
   ["-Sverbose"  "print where deps are read from"]])

;; --- the dynamic half --------------------------------------------------------

;; The commands a task can take the name of. jolt.main dispatches a built-in
;; ahead of a task of the same name unless the task sets :override-builtin, and
;; this set has to be the one main checks — a name here that main does not guard
;; would hide a task that actually runs.
(def ^:private overridable
  #{"run" "repl" "nrepl-server" "path" "build" "tasks" "completions"})

(defn task-lines
  "One line per listable task: `name<TAB>doc`, or the bare name when it has no
  :doc. This is the only part a completing shell has to ask jolt for, and it
  goes through jolt.tasks/listable so that TAB and `jolt tasks` agree on which
  tasks exist.

  A task that shares a built-in's name is dropped unless it wins that name,
  because a completion's description is a promise about what the word will do.
  `jolt tasks` is a list of what the project defines and lists such a task
  either way; this is a list of what typing the word gets you, and for a task
  that loses to a built-in the answer is the built-in. The snippet offers what
  comes back here BEFORE its own baked-in commands, so an overriding task's
  description is the one that survives the shell's de-duplication."
  [tasks]
  (for [[k v] (tasks/listable tasks)
        :when (or (not (overridable (str k)))
                  (and (map? v) (:override-builtin v)))]
    (if-let [doc (tasks/doc-line v)]
      (str k \tab doc)
      (str k))))

;; --- the snippets ------------------------------------------------------------

;; clojure.string is not required here on purpose: jolt-core is baked into the
;; seed and jolt.tasks avoids the dependency for the same reason, so two small
;; escapers are cheaper than pulling the namespace in for them.

(defn- esc-sq
  "For inside a single-quoted shell word: a quote closes, escapes and reopens."
  [s]
  (apply str (mapcat #(if (= % \') [\' \\ \' \'] [%]) (str s))))

(defn- esc-fish
  "For inside a single-quoted fish word, where a backslash escapes."
  [s]
  (apply str (mapcat #(if (= % \') [\\ \'] [%]) (str s))))

(defn- pairs->sh
  "A shell array literal of `name<TAB>doc` rows, single-quoted. A description is
  ours and holds no quote today, but a task's :doc is a project's text, so the
  same escaping is applied to both rather than only where it currently matters."
  [pairs]
  (->> pairs
       (map (fn [[n d]] (str "'" (esc-sq n) \tab (esc-sq d) "'")))
       (interpose " ")
       (apply str)))

(defn- zsh [prog cmds opts]
  (str "#compdef " prog "\n"
       "# Generated by `" prog " completions zsh`. jolt's own commands are baked in\n"
       "# below; a project's tasks are fetched once per change to its deps.edn or\n"
       "# bb.edn and cached, because jolt costs ~0.22s to start and a completing\n"
       "# shell must not pay that on every press.\n"
       "\n"
       "_jolt_cached_tasks() {\n"
       "  local dir=$PWD stamp cache key\n"
       "  [[ -f $dir/deps.edn || -f $dir/bb.edn ]] || return 0\n"
       "  # zsh/stat reads an mtime without forking; the fallback is for a zsh\n"
       "  # built without the module.\n"
       "  if zmodload -F zsh/stat b:zstat 2>/dev/null; then\n"
       "    local -a s\n"
       "    zstat -A s +mtime $dir/deps.edn 2>/dev/null && stamp+=$s[1]\n"
       "    zstat -A s +mtime $dir/bb.edn   2>/dev/null && stamp+=:$s[1]\n"
       "  else\n"
       "    # One file at a time, GNU spelling first. `stat -c %Y` is GNU's\n"
       "    # mtime; `-f` there is a FILESYSTEM query, so `stat -f %m file`\n"
       "    # reads %m as a second file, prints a block table on stdout and\n"
       "    # exits 1 -- a stamp built from it moves whenever free space does,\n"
       "    # which is every few seconds on a live disk. BSD stat has no -c and\n"
       "    # spells the mtime -f %m, so it falls through to the second try.\n"
       "    local f\n"
       "    for f in $dir/deps.edn $dir/bb.edn; do\n"
       "      stamp+=:$(stat -c %Y $f 2>/dev/null || stat -f %m $f 2>/dev/null)\n"
       "    done\n"
       "  fi\n"
       "  cache=${XDG_CACHE_HOME:-$HOME/.cache}/jolt/completion\n"
       "  key=$cache/${${dir//\\//%}//:/%%}\n"
       "  if [[ -z $JOLT_COMPLETION_NO_CACHE && -r $key ]]; then\n"
       "    local -a c=(\"${(@f)$(<$key)}\")\n"
       "    if [[ $c[1] == $stamp ]]; then print -l -- \"${(@)c[2,-1]}\"; return 0; fi\n"
       "  fi\n"
       "  local out; out=$(" prog " completions tasks 2>/dev/null) || return 0\n"
       "  print -r -- $out\n"
       "  if [[ -z $JOLT_COMPLETION_NO_CACHE ]] && mkdir -p $cache 2>/dev/null; then\n"
       "    print -r -- $stamp$'\\n'$out > $key 2>/dev/null\n"
       "  fi\n"
       "}\n"
       "\n"
       "_jolt() {\n"
       "  local -a commands=(" (pairs->sh cmds) ")\n"
       "  local -a options=(" (pairs->sh opts) ")\n"
       "  local -a described bare\n"
       "  local l v d\n"
       "  # _describe eats a backslash and splits `value:description` at the first\n"
       "  # colon, so both characters are escaped in both halves. Task names carry\n"
       "  # colons routinely (build:linux, release:macos); unescaped, such a name is\n"
       "  # truncated at the colon and vanishes behind the command it then collides\n"
       "  # with.\n"
       "  _jolt_add() {\n"
       "    v=${1%%$'\\t'*}; d=; [[ $1 == *$'\\t'* ]] && d=${1#*$'\\t'}\n"
       "    v=${v//\\\\/\\\\\\\\}; v=${v//:/\\\\:}\n"
       "    d=${d//\\\\/\\\\\\\\}; d=${d//:/\\\\:}\n"
       "    if [[ -n $d ]]; then described+=(\"$v:$d\"); else bare+=(\"$v\"); fi\n"
       "  }\n"
       "\n"
       "  case ${words[CURRENT-1]} in\n"
       "    -f|--file|-o) _files; return ;;\n"
       "    -e|-m|-Sdeps|-Scp|--target) _message 'value'; return ;;\n"
       "  esac\n"
       "\n"
       "  if (( CURRENT == 2 )); then\n"
       "    if [[ ${words[CURRENT]} == -* ]]; then\n"
       "      for l in $options; do _jolt_add $l; done\n"
       "    else\n"
       ;; Tasks first, then any command a task has NOT taken the name of.
       ;; Dropping the command explicitly rather than letting the two candidates
       ;; collide: _describe does not keep the first of a repeated value, so
       ;; with both present the command's description is what shows, on the one
       ;; name where the task is what actually runs. A task only reaches here
       ;; with a command's name when it wins that name.
       "      local -A taken\n"
       "      for l in ${(f)\"$(_jolt_cached_tasks)\"}; do\n"
       "        [[ -n $l ]] || continue\n"
       "        taken[${l%%$'\\t'*}]=1\n"
       "        _jolt_add $l\n"
       "      done\n"
       "      for l in $commands; do\n"
       "        [[ -n ${taken[${l%%$'\\t'*}]} ]] && continue\n"
       "        _jolt_add $l\n"
       "      done\n"
       "    fi\n"
       "    (( $#described )) && _describe -t commands 'jolt' described\n"
       "    (( $#bare )) && _describe -t commands 'jolt' bare\n"
       "    _files\n"
       "    return\n"
       "  fi\n"
       "\n"
       "  case ${words[2]} in\n"
       "    run)\n"
       "      if (( CURRENT == 3 )); then\n"
       "        for l in ${(f)\"$(_jolt_cached_tasks)\"}; do [[ -n $l ]] && _jolt_add $l; done\n"
       "        (( $#described )) && _describe -t commands 'task' described\n"
       "        (( $#bare )) && _describe -t commands 'task' bare\n"
       "      fi\n"
       "      _files ;;\n"
       "    completions)\n"
       "      (( CURRENT == 3 )) && _describe -t commands 'shell' \\\n"
       "        '(zsh:a\\ zsh\\ completion\\ function bash:a\\ bash\\ completion\\ function"
       " fish:a\\ fish\\ completion tasks:name/doc\\ lines\\ for\\ a\\ snippet)' ;;\n"
       "    build)\n"
       "      _arguments '-m[entry namespace]:namespace:' '-o[output path]:output:_files' \\\n"
       "        '--opt[optimized build]' '--dev[development build]' \\\n"
       "        '--no-direct-link[disable direct linking]' '--dynamic[link dynamically]' \\\n"
       "        '--tree-shake[drop unreachable code]' '--library[build a shared object]' \\\n"
       "        '--boot[boot image: startup vs binary size]:mode:(fast small plain)' \\\n"
       "        '--target[cross-compile for a Chez machine]:machine:' \\\n"
       "        '--target-pack[support directory]:dir:_files -/' ;;\n"
       "    tasks|path|version|help|repl) ;;\n"
       "    nrepl-server) (( CURRENT == 3 )) && _message 'port' ;;\n"
       "    *) _files ;;\n"
       "  esac\n"
       "}\n"
       "\n"
       ;; Both ways of installing this have to work, and they need opposite
       ;; endings. Sourced from .zshrc, the file defines _jolt and then has to
       ;; register it with compdef. Saved as _jolt on fpath, zsh autoloads it at
       ;; the first press and runs the FILE ITSELF as the function body, so a
       ;; file that only defines _jolt completes nothing: the definition lands
       ;; and the press that caused it returns no candidates. zsh_eval_context
       ;; names the case.
       "\n# Sourced, this registers the function. Autoloaded off fpath, the file\n"
       "# IS the function body and has to call it, or the press that triggered\n"
       "# the load completes nothing.\n"
       "if [[ ${zsh_eval_context[-1]} == loadautofunc ]]; then\n"
       "  _jolt \"$@\"\n"
       "else\n"
       "  compdef _jolt " prog "\n"
       "fi\n"))

(defn- bash [prog cmds opts]
  (str "# Generated by `" prog " completions bash`. Source it from ~/.bashrc.\n"
       "# bash shows no description column, so only names are offered here.\n"
       "\n"
       "# GNU spelling first: `stat -c %Y` is GNU's mtime, while `-f` there is a\n"
       "# FILESYSTEM query, so `stat -f %m file` reads %m as a second file, prints\n"
       "# a block table on stdout and exits 1. Trying that first puts free-block\n"
       "# counts in the stamp, and those move every few seconds on a live disk --\n"
       "# a cache that never hits, on every Linux. BSD stat has no -c and spells\n"
       "# the mtime -f %m, so it falls through to the second try.\n"
       "_jolt_mtime() { stat -c %Y \"$1\" 2>/dev/null || stat -f %m \"$1\" 2>/dev/null; }\n"
       "\n"
       "_jolt_cached_tasks() {\n"
       "  [ -f deps.edn ] || [ -f bb.edn ] || return 0\n"
       "  local stamp cache key\n"
       "  stamp=\"$(_jolt_mtime deps.edn)/$(_jolt_mtime bb.edn)\"\n"
       "  cache=\"${XDG_CACHE_HOME:-$HOME/.cache}/jolt/completion\"\n"
       "  key=\"$cache/$(printf '%s' \"$PWD\" | cksum | tr -d ' ')\"\n"
       "  if [ -z \"${JOLT_COMPLETION_NO_CACHE:-}\" ] && [ -r \"$key\" ] &&\n"
       "     [ \"$(head -n 1 \"$key\")\" = \"$stamp\" ]; then\n"
       "    tail -n +2 \"$key\"; return 0\n"
       "  fi\n"
       "  local out; out=$(" prog " completions tasks 2>/dev/null) || return 0\n"
       "  printf '%s\\n' \"$out\"\n"
       "  if [ -z \"${JOLT_COMPLETION_NO_CACHE:-}\" ] && mkdir -p \"$cache\" 2>/dev/null; then\n"
       "    { printf '%s\\n' \"$stamp\"; printf '%s\\n' \"$out\"; } > \"$key\" 2>/dev/null\n"
       "  fi\n"
       "}\n"
       "\n"
       "_jolt_completions() {\n"
       "  local cur prev commands options tasks\n"
       "  COMPREPLY=()\n"
       "  cur=\"${COMP_WORDS[COMP_CWORD]}\"\n"
       "  prev=\"${COMP_WORDS[COMP_CWORD-1]}\"\n"
       "  commands=\"" (->> cmds (map first) (interpose " ") (apply str)) "\"\n"
       "  options=\"" (->> opts (map first) (interpose " ") (apply str)) "\"\n"
       "  case \"$prev\" in\n"
       "    -f|--file|-o|--target-pack) COMPREPLY=( $(compgen -f -- \"$cur\") ); return 0 ;;\n"
       "    -e|-m|-Sdeps|-Scp|--target) return 0 ;;\n"
       "  esac\n"
       "  if [ \"$COMP_CWORD\" -eq 1 ]; then\n"
       "    if [ \"${cur#-}\" != \"$cur\" ]; then\n"
       "      COMPREPLY=( $(compgen -W \"$options\" -- \"$cur\") )\n"
       "    else\n"
       "      tasks=$(_jolt_cached_tasks | cut -f1)\n"
       ;; awk drops a repeat: a task that has taken a command's name is in both
       ;; lists, and bash would offer the word twice.
       "      COMPREPLY=( $(compgen -W \"$tasks $commands\" -- \"$cur\" | awk '!seen[$0]++') )\n"
       "    fi\n"
       "    return 0\n"
       "  fi\n"
       "  case \"${COMP_WORDS[1]}\" in\n"
       "    run) tasks=$(_jolt_cached_tasks | cut -f1)\n"
       "         COMPREPLY=( $(compgen -W \"-m -f --file --parallel $tasks\" -- \"$cur\") ) ;;\n"
       "    completions) COMPREPLY=( $(compgen -W \"zsh bash fish tasks\" -- \"$cur\") ) ;;\n"
       "    build) COMPREPLY=( $(compgen -W \"-m -o --opt --dev --no-direct-link --dynamic\n"
       "                                     --tree-shake --boot --library --target --target-pack\" -- \"$cur\") ) ;;\n"
       "    tasks|path|version|help|repl) ;;\n"
       "    *) COMPREPLY=( $(compgen -f -- \"$cur\") ) ;;\n"
       "  esac\n"
       "  return 0\n"
       "}\n"
       "complete -o default -F _jolt_completions " prog "\n"))

(defn- fish [prog cmds opts]
  (str "# Generated by `" prog " completions fish`. Save under\n"
       "# ~/.config/fish/completions/" prog ".fish\n"
       "\n"
       "# The cache lives in the shell's own variables rather than a file, and is\n"
       "# keyed on the directory it was filled from: fish keeps a completion\n"
       "# function loaded for the whole session, so an unkeyed cache answers the\n"
       "# second project you cd into with the first one's tasks. It does not watch\n"
       "# deps.edn and bb.edn the way the zsh and bash snippets do, so a task added\n"
       "# mid-session wants a new shell, or JOLT_COMPLETION_NO_CACHE=1 to ask jolt\n"
       "# on every press.\n"
       "function __jolt_tasks\n"
       "  # No project here, no callback: a bare directory must not spawn jolt.\n"
       "  if not test -f deps.edn; and not test -f bb.edn\n"
       "    return 0\n"
       "  end\n"
       ;; -n, not `set -q`: an empty-but-set variable is still set to fish, and
       ;; the zsh and bash snippets both read the knob as "non-empty".
       "  if test -n \"$JOLT_COMPLETION_NO_CACHE\"\n"
       "    " prog " completions tasks 2>/dev/null\n"
       "    return 0\n"
       "  end\n"
       "  if not set -q __jolt_tasks_dir; or test \"$__jolt_tasks_dir\" != \"$PWD\"\n"
       "    set -g __jolt_tasks_dir $PWD\n"
       "    set -g __jolt_tasks_cache (" prog " completions tasks 2>/dev/null)\n"
       "  end\n"
       "  for l in $__jolt_tasks_cache\n"
       "    echo $l\n"
       "  end\n"
       "end\n"
       "\n"
       "complete -c " prog " -f -n '__fish_is_first_arg' -a '(__jolt_tasks)'\n"
       (apply str
              (for [[n d] cmds]
                (str "complete -c " prog " -f -n '__fish_is_first_arg' -a '" n "' -d '"
                     (esc-fish d) "'\n")))
       ;; The options as candidates rather than as fish's own -s/-l flags, because
       ;; a name like -Sdeps is neither a short flag nor a long one in fish's
       ;; grammar. Offered only once the word being typed starts with a dash, so
       ;; a bare first TAB stays a list of commands and tasks.
       (apply str
              (for [[n d] opts]
                (str "complete -c " prog " -f -n '__fish_is_first_arg; and string match"
                     " -q -- \"-*\" (commandline -ct)' -a '" n "' -d '"
                     (esc-fish d) "'\n")))))

(defn snippet
  "The completion function for `shell`, as text."
  [shell prog]
  (case shell
    "zsh"  (zsh prog commands options)
    "bash" (bash prog commands options)
    "fish" (fish prog commands options)
    (throw (ex-info (str "unknown shell for completions: " shell
                         " (want zsh, bash or fish)")
                    {:shell shell}))))

#!/usr/bin/env python3
"""Generate docs/spec/coverage.md — the spec status dashboard.

Cross-references three sources:
  1. clojuredocs-export.json   — the clojure.core var inventory (the surface)
  2. jolt's interned clojure.core mappings (via janet)
  3. symbols exercised by test/spec/* + the 3-path conformance suite

Run from the repo root: python3 tools/spec_coverage.py
"""
import json, re, subprocess, glob, datetime
from collections import Counter

# --- 1. the surface --------------------------------------------------------
data = json.load(open('clojuredocs-export.json'))
core = sorted(v['name'] for v in data['vars'] if v['ns'] == 'clojure.core')
examples = {v['name'] for v in data['vars']
            if v['ns'] == 'clojure.core' and v.get('examples')}

# --- 2. what jolt provides ---------------------------------------------------
# Two notions: INTERNED (in clojure.core's mappings — visible to ns
# introspection) and RESOLVABLE (usable in code; some seed fns resolve through
# fallback paths without being interned — itself a conformance finding).
janet_prog = '''(use ./src/jolt/api) (use ./src/jolt/types) (use ./src/jolt/reader)
(def ctx (init))
(def core (ctx-find-ns ctx "clojure.core"))
(each n (sort (keys (core :mappings))) (print "I " n))
(def names (string/split "\\n" (slurp "/tmp/spec-surface-names.txt")))
(each n names
  (when (> (length n) 0)
    # value-position probe: seed fns resolve through the core fallback even
    # when not interned (and jolt resolve can't see them — a finding itself).
    (def r (protect (do (def form (parse-string n))
                        (when (and (struct? form) (= :symbol (form :jolt/type)))
                          (eval-string ctx n) true))))
    (when (and (r 0) (= true (r 1))) (print "R " n))))'''
open('/tmp/spec-surface-names.txt','w').write('\n'.join(core))
out = subprocess.run(['janet', '-e', janet_prog], capture_output=True, text=True)
interned = {l[2:] for l in out.stdout.splitlines() if l.startswith('I ')}
resolvable = {l[2:] for l in out.stdout.splitlines() if l.startswith('R ')}
jolt = interned | resolvable

# --- 3. what the tests exercise --------------------------------------------
tested = set()
test_text = ''
# A var counts as tested when its name appears as a WHOLE TOKEN anywhere in
# the test sources (assertions live inside strings, so call-position-only
# matching missed *1, +', ., .., /, and bare transducer refs like cat).
SYMCHARS = r"\w*+!?<>=_.'/-"
def token_re(name):
    return re.compile('(?<![' + re.escape(SYMCHARS) + '])' + re.escape(name) + '(?![' + re.escape(SYMCHARS) + '])')
for f in glob.glob('test/spec/*.janet') + ['test/integration/conformance-test.janet']:
    test_text += open(f).read()

# --- classification ---------------------------------------------------------
SPECIAL = {'catch','finally','do','def','defmacro','fn','if','let','loop','quote',
           'recur','throw','try','var','new','set!','monitor-enter','monitor-exit',
           # '.' is the interop special form — (resolve '.) is nil on the JVM too
           '.'}
AGENTS = {'agent','send','send-off','send-via','await','await-for','await1',
          'agent-error','agent-errors','clear-agent-errors','error-handler',
          'error-mode','set-agent-send-executor!','set-agent-send-off-executor!',
          'restart-agent','shutdown-agents','release-pending-sends','add-tap',
          'tap>','remove-tap','set-error-handler!','set-error-mode!'}
STM = {'dosync','ref','ref-set','alter','commute','ensure','ref-history-count',
       'ref-max-history','ref-min-history','sync','io!'}
JVM = {'class','class?','cast','bases','supers','compile','add-classpath',
       'definline','bean','accessor','create-struct','defstruct','struct',
       'struct-map','amap','areduce','memfn','enumeration-seq','iterator-seq',
       'resultset-seq','print-ctor','print-dup','print-method','print-simple',
       'primitives-classnames','vector-of','PrintWriter-on',
       'StackTraceElement->vec','Throwable->map','Inst','->ArrayChunk','->Vec',
       '->VecNode','->VecSeq','-cache-protocol-fn','-reset-methods','EMPTY-NODE',
       'method-sig','proxy-name','gen-class','gen-interface','find-protocol-impl',
       'find-protocol-method','with-loading-context','load','load-file',
       'load-reader','loaded-libs','requiring-resolve','default-data-readers',
       '..','.','pcalls','pmap','pvalues','stream-into!','stream-reduce!',
       'stream-seq!','stream-transduce!','mix-collection-hash','iteration',
       'unquote','unquote-splicing'}

def classify(n):
    if n in jolt:
        return 'implemented+tested' if token_re(n).search(test_text) else 'implemented-untested'
    if re.match(r'^\*.*\*$', n): return 'dynamic-var'
    if n in SPECIAL: return 'special-form'
    if n in AGENTS:  return 'agents-taps'
    if n in STM:     return 'stm-refs'
    if n in JVM:     return 'jvm-specific'
    return 'missing-portable'

cls = {n: classify(n) for n in core}
counts = Counter(cls.values())
stamp = datetime.date.today().isoformat()

rows = []
for n in core:
    rows.append(f"| `{n}` | {cls[n]} | {'✓' if n in examples else ''} |")

md = f"""# Appendix A — Coverage Dashboard (generated)

Generated {stamp} by `tools/spec_coverage.py` — do not edit by hand.

Surface: **{len(core)}** clojure.core vars (ClojureDocs export; {len(examples)} with
community examples). jolt interns {len(jolt & set(core))} of them.

| Status | Count | Meaning |
|---|---|---|
| implemented+tested | {counts['implemented+tested']} | in jolt and exercised by spec/conformance |
| implemented-untested | {counts['implemented-untested']} | in jolt, no direct test — spec entries will add them |
| resolvable-not-interned | {len((resolvable - interned) & set(core) - SPECIAL)} | works in code but invisible to ns introspection (conformance finding) |
| missing-portable | {counts['missing-portable']} | portable semantics, jolt lacks it — implementation gap |
| special-form | {counts['special-form']} | specified in §3, not a library var |
| dynamic-var | {counts['dynamic-var']} | classification needed: portable default vs host-dependent |
| agents-taps | {counts['agents-taps']} | out of scope pending concurrency design note |
| stm-refs | {counts['stm-refs']} | out of scope pending concurrency design note |
| jvm-specific | {counts['jvm-specific']} | catalogued, not specified |

Classifications are initial and mechanical — reclassifying is an ordinary
spec change. A var is *Verified* only when its §9 entry exists and carries no
UNVERIFIED field; that column will be added as entries land.

## Per-var status

| Var | Status | ClojureDocs examples |
|---|---|---|
{chr(10).join(rows)}
"""
open('docs/spec/coverage.md', 'w').write(md)
print(f"wrote docs/spec/coverage.md — {len(core)} vars")
for k, v in counts.most_common(): print(f"  {k}: {v}")

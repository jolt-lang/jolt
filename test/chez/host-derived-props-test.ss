;; The derived host properties (jolt-lang/jolt#796): what sa-os-family, sa-arch
;; and sa-endian answer for each Chez machine tag. Run:
;;   chez --script test/chez/host-derived-props-test.ss
;;
;; These three are the ONLY things host logic may ask about the platform — the
;; tag itself is naming-only — so a wrong row here is a wrong SIGCHLD, EAGAIN,
;; O_NONBLOCK, LC_TIME, struct-stat offset, chmod fallback and link library, on
;; every caller at once. #796 was exactly that: portable-bytecode tags carry no
;; OS, so the else-branch called every bytecode build Linux, including one
;; running on macOS, and a Darwin pb build could not open a socket because
;; jolt.nrepl handed Darwin's socket() the Linux SOCK_CLOEXEC.
;;
;; The table is pinned over the tags a run does NOT have, because the row that
;; broke can only be reached from a host we do not build on. The *-for-tag
;; entry points exist for that; the last check ties the table back to reality by
;; requiring the pb probe to agree with what this host's native tag says.

(import (chezscheme))
;; The boot takes the process-global handle before anything asks for an
;; architecture (rt.ss binds _exit through jolt-foreign-proc-safe long before
;; host-static-methods.ss reads sa-arch), and sa-probed-arch relies on that
;; rather than re-taking it. Loading the adapter on its own here has to stand in
;; for that, or the uname probe finds no symbol and the gate measures the
;; degraded answer instead of the real one.
(load-shared-object #f)
(load "host/chez/scheme-adapter-runtime.ss")

(define total 0) (define fails 0)
(define (ok name pred)
  (set! total (+ total 1))
  (unless pred (set! fails (+ fails 1)) (printf "FAIL: ~a\n" name)))
(define (row tag fam arch endian)
  (ok (format "~a -> os ~a" tag fam)     (eq? (sa-os-family-for-tag tag) fam))
  (ok (format "~a -> arch ~a" tag arch)  (eq? (sa-arch-for-tag tag) arch))
  (ok (format "~a -> endian ~a" tag endian) (eq? (sa-endian-for-tag tag) endian)))

;; Native tags: the tag names the OS and every row is decided by the tag alone.
;; endian is #f for the osx/nt tags because their suffix is not le/be — the
;; stat-layout guard reads that as "unverified" and nio-file keys those hosts
;; off sa-os-family instead.
(row "tarm64osx" 'macos   'arm64  #f)
(row "a6osx"     'macos   'x86-64 #f)
(row "ta6nt"     'windows 'x86-64 #f)
(row "i3nt"      'windows 'i386   #f)
(row "ta6le"     'linux   'x86-64 'little)
(row "arm64le"   'linux   'arm64  'little)
(row "i3le"      'linux   'i386   'little)
(row "a6ob"      'linux   'x86-64 #f)   ; unrecognized OS still degrades to linux

;; iOS is Darwin, and its tags say so without saying "osx". Chez has four of
;; them (a6ios, arm64ios, ta6ios, tarm64ios) and BUILDING documents tarm64ios
;; as the iOS cross-target. The OS row is what picks SIGCHLD, EAGAIN,
;; O_NONBLOCK, LC_TIME, the struct-stat offsets and the link libraries, all of
;; which are Darwin's here. endian is #f for the same reason as the osx tags:
;; the suffix is not le/be.
(row "tarm64ios" 'macos   'arm64  #f)
(row "a6ios"     'macos   'x86-64 #f)

;; Android is the other cross-target with no tag of its own, and unlike iOS it
;; needs no branch: it builds as tarm64le (tools/cross-compile/README.md), and
;; Bionic is Linux for every constant this row picks. Pinned so the answer is a
;; decision rather than the else-branch's luck. Where Android does diverge is
;; the link libraries, which this tag cannot express — tarm64le is glibc arm64
;; Linux too — and which the target pack owns.
(row "tarm64le"  'linux   'arm64  'little)

;; Portable-bytecode tags: pb/pb64l/tpb64l name the threading, word size and
;; endianness and deliberately name no OS, and their 64/l fields are not in the
;; shape sa-arch-for-tag/sa-endian-for-tag parse either. So all three tag
;; derivations decline, and all three public entry points probe past them
;; (#796 for the OS, #798 for the other two).
(for-each
  (lambda (tag)
    (ok (format "~a -> os declines to the probe" tag)
        (eq? (sa-os-family-for-tag tag) (sa-probed-os-family)))
    (ok (format "~a -> arch not derivable from the tag" tag)
        (eq? (sa-arch-for-tag tag) 'other))
    (ok (format "~a -> endian not derivable from the tag" tag)
        (eq? (sa-endian-for-tag tag) #f)))
  '("pb" "pb64l" "pb64b" "pb32l" "tpb64l" "tpb64b"))

;; The probes are caches, and none of the three answers can change while the
;; process runs.
(ok "os probe is stable across calls"   (eq? (sa-probed-os-family) (sa-probed-os-family)))
(ok "arch probe is stable across calls" (eq? (sa-probed-arch) (sa-probed-arch)))

;; The probes must produce a real answer, not the degraded one, on any host the
;; gate runs on. sa-arch degrades to 'other when uname cannot be reached, which
;; is a legitimate answer on a statically linked build but not on this one.
(ok "os probe answers a real family"
    (memq (sa-probed-os-family) '(macos windows linux)))
(ok (format "arch probe answers a real architecture (~a)" (sa-probed-arch))
    (memq (sa-probed-arch) '(x86-64 arm64 i386)))
(ok (format "endian answers a real byte order (~a)" (sa-endian))
    (memq (sa-endian) '(little big)))

;; And the probes are RIGHT: this run is a native build, whose tag names the OS
;; and the architecture independently. Each probe has to reach the same verdict
;; as the tag it can be checked against — that is the whole claim a bytecode
;; build then rests on, checked on every host the gate runs on.
(define native-tag (sa-host-tag))
(define native? (not (sa-tag-contains? native-tag "pb")))
(ok (format "os probe agrees with the native tag ~s (~a)" native-tag (sa-os-family))
    (or (not native?)
        (eq? (sa-os-family-for-tag native-tag) (sa-probed-os-family))))
(ok (format "arch probe agrees with the native tag ~s (~a)" native-tag (sa-arch))
    (or (not native?)
        (eq? (sa-arch-for-tag native-tag) 'other)      ; tag names no arch to compare
        (eq? (sa-arch-for-tag native-tag) (sa-probed-arch))))
(ok "endian agrees with the tag when the tag names one"
    (let ((from-tag (sa-endian-for-tag native-tag)))
      (or (not from-tag) (eq? from-tag (sa-endian)))))

;; A pb build resolves all three, which is the end state #796 and #798 are
;; between them for: nothing about a bytecode tag may leave a property unknown
;; on a host that can answer it.
(for-each
  (lambda (tag)
    (ok (format "~a -> os resolves" tag)
        (memq (sa-os-family-for-tag tag) '(macos windows linux)))
    (ok (format "~a -> arch resolves to this host's" tag)
        (eq? (let ((a (sa-arch-for-tag tag))) (if (eq? a 'other) (sa-probed-arch) a))
             (sa-arch)))
    (ok (format "~a -> endian resolves to this host's" tag)
        (eq? (or (sa-endian-for-tag tag) (native-endianness)) (sa-endian))))
  '("pb" "pb64l" "tpb64l"))

(printf "host-derived-props: ~a checks, ~a failures\n" total fails)
(when (> fails 0) (exit 1))

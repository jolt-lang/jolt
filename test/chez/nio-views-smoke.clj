;; The Windows half of java.nio.file's attribute views and directory order
;; (unit.edn's nio-views / nio-order rows are the POSIX half). Run against a
;; built jolt.exe by windows-static:
;;
;;   target/release/jolt.exe test/chez/nio-views-smoke.clj
;;
;; Expected values are JDK 21's (WindowsFileAttributeViews.Dos,
;; WindowsAclFileAttributeView, WindowsUserDefinedFileAttributeView,
;; WindowsUserPrincipals), cross-checked where Windows has its own tool for it:
;; `attrib` for the DOS bits, `dir /r` for the streams, `icacls` for the DACL and
;; `dir /b /a` for the order FindFirstFileW/FindNextFileW hand entries out in.
;; Prints one line per check and exits 1 if any failed.
(ns nio-views-smoke
  (:require [clojure.java.shell :as sh]
            [clojure.string :as str])
  (:import [java.nio.file Files LinkOption FileSystems]
           [java.nio.file.attribute FileAttribute DosFileAttributeView DosFileAttributes
            AclFileAttributeView UserDefinedFileAttributeView PosixFileAttributeView
            AclEntry AclEntryType AclEntryPermission UserPrincipal]))

(def failures (atom 0))
(defn check [label got want]
  (if (= got want)
    (println "ok  " label)
    (do (swap! failures inc)
        (println "FAIL" label)
        (println "     got: " (pr-str got))
        (println "     want:" (pr-str want)))))
(defn caught [f]
  (try (f) :no-exception
       (catch Exception e [(.getSimpleName (class e)) (.getMessage e)])))
(defn cmd [& args] (str/trim (:out (apply sh/sh "cmd" "/c" args))))

(def windows? (.startsWith (System/getProperty "os.name") "Windows"))
(when-not windows?
  (println "skip: the Windows views; this host is" (System/getProperty "os.name"))
  (System/exit 0))

(def lo (into-array LinkOption []))
(def fa (into-array FileAttribute []))
(def d (Files/createTempDirectory "jnv" fa))
(def p (.resolve d "f.txt"))
(spit (str p) "data")

;; ---- the views this provider has ----
(check "supportedFileAttributeViews"
       (set (.supportedFileAttributeViews (FileSystems/getDefault)))
       #{"basic" "owner" "dos" "acl" "user"})
(check "view classes"
       (mapv #(some-> (Files/getFileAttributeView p % lo) class .getName)
             [DosFileAttributeView AclFileAttributeView UserDefinedFileAttributeView PosixFileAttributeView])
       ["sun.nio.fs.WindowsFileAttributeViews$Dos" "sun.nio.fs.WindowsAclFileAttributeView"
        "sun.nio.fs.WindowsUserDefinedFileAttributeView" nil])
(check "posix view is not available"
       (caught #(Files/getAttribute p "posix:permissions" lo))
       ["UnsupportedOperationException" "View 'posix' not available"])

;; ---- dos ----
(def dv (Files/getFileAttributeView p DosFileAttributeView lo))
(defn attrib-letters []
  ;; `attrib F` prints the set letters in fixed columns ahead of the path
  (set (re-seq #"[ASHR]" (first (str/split (cmd "attrib" (str p)) #"[A-Za-z]:\\")))))
(.setArchive dv false)
(check "dos: cleared archive" (let [a (Files/readAttributes p DosFileAttributes lo)]
                                [(.isArchive a) (.isHidden a) (.isReadOnly a) (.isSystem a)])
       [false false false false])
(.setHidden dv true)
(.setSystem dv true)
(check "dos: hidden and system set, through the view" (let [a (.readAttributes dv)]
                                                        [(.isHidden a) (.isSystem a) (.isReadOnly a)])
       [true true false])
(check "dos: attrib agrees" (attrib-letters) #{"S" "H"})
(Files/setAttribute p "dos:readonly" true lo)
(Files/setAttribute p "dos:system" false lo)
(check "dos: readonly through setAttribute"
       (into (sorted-map) (Files/readAttributes p "dos:readonly,hidden,system,archive" lo))
       {"archive" false "hidden" true "readonly" true "system" false})
(check "dos: attrib agrees again" (attrib-letters) #{"H" "R"})
(check "dos: the attribute word" (bit-and (Files/getAttribute p "dos:attributes" lo) 0x27) 0x3)
(check "dos: dos:* names"
       (sort (keys (Files/readAttributes p "dos:*" lo)))
       ["archive" "attributes" "creationTime" "fileKey" "hidden" "isDirectory" "isOther"
        "isRegularFile" "isSymbolicLink" "lastAccessTime" "lastModifiedTime" "readonly" "size" "system"])
(check "dos: DosFileAttributes class" (.getName (class (Files/readAttributes p DosFileAttributes lo)))
       "sun.nio.fs.WindowsFileAttributes")
(check "dos: a name the view has not"
       [(caught #(Files/setAttribute p "dos:attributes" (int 0) lo))
        (caught #(Files/readAttributes p "dos:bogus" lo))]
       [["IllegalArgumentException" "'dos:attributes' not recognized"]
        ["IllegalArgumentException" "'bogus' not recognized"]])
(.setReadOnly dv false)
(.setHidden dv false)
(check "dos: cleared" (attrib-letters) #{})
(check "dos: a missing file" (caught #(.setHidden (Files/getFileAttributeView (.resolve d "gone") DosFileAttributeView lo) true))
       ["NoSuchFileException" (str (.resolve d "gone"))])

;; ---- user: alternate data streams ----
(def uv (Files/getFileAttributeView p UserDefinedFileAttributeView lo))
(check "user: none yet" (vec (.list uv)) [])
(check "user: write answers the count" (.write uv "k1" (java.nio.ByteBuffer/wrap (.getBytes "hello" "UTF-8"))) 5)
(Files/setAttribute p "user:k2" (.getBytes "xy" "UTF-8") lo)
(check "user: list" (sort (.list uv)) ["k1" "k2"])
(check "user: dir /r sees the streams"
       (set (filter #(str/includes? (cmd "dir" "/r" (str p)) %) ["f.txt:k1:$DATA" "f.txt:k2:$DATA"]))
       #{"f.txt:k1:$DATA" "f.txt:k2:$DATA"})
(check "user: size" (.size uv "k1") 5)
(check "user: read" (let [b (java.nio.ByteBuffer/allocate 16) n (.read uv "k1" b)]
                      [n (.position b) (String. (.array b) 0 n "UTF-8")])
       [5 5 "hello"])
(check "user: getAttribute" (vec (Files/getAttribute p "user:k1" lo)) [104 101 108 108 111])
(check "user: readAttributes" (into (sorted-map) (map (fn [[k v]] [k (count v)])) (Files/readAttributes p "user:*" lo))
       {"k1" 5 "k2" 2})
(check "user: the file's own data is untouched" (slurp (str p)) "data")
(check "user: a buffer too small" (caught #(.read uv "k1" (java.nio.ByteBuffer/allocate 2)))
       ["IOException" "Stream too large"])
(check "user: a missing name names the stream"
       [(caught #(.size uv "nope")) (caught #(.delete uv "nope"))]
       [["NoSuchFileException" (str p ":nope")] ["NoSuchFileException" (str p ":nope")]])
(.delete uv "k1")
(check "user: after delete" (vec (.list uv)) ["k2"])
(.delete uv "k2")
(check "user: a missing file is NoSuchFileException, not a file made for the stream"
       [(first (caught #(.write (Files/getFileAttributeView (.resolve d "gone") UserDefinedFileAttributeView lo)
                                "a" (java.nio.ByteBuffer/wrap (.getBytes "a" "UTF-8")))))
        (Files/exists (.resolve d "gone") lo)]
       ["NoSuchFileException" false])
(check "user: a directory's streams" (vec (.list (Files/getFileAttributeView d UserDefinedFileAttributeView lo))) [])

;; ---- acl and the owner ----
(def av (Files/getFileAttributeView p AclFileAttributeView lo))
(def owner (.getOwner av))
(check "acl: the owner is the owner view's" (= owner (Files/getOwner p lo) (Files/getAttribute p "owner:owner" lo)) true)
(check "acl: the owner is a Windows principal named DOMAIN\\NAME"
       [(contains? #{"sun.nio.fs.WindowsUserPrincipals$User" "sun.nio.fs.WindowsUserPrincipals$Group"} (.getName (class owner)))
        (str/includes? (.getName owner) "\\")
        (= (str owner) (str (.getName owner) " (" (second (re-find #"\(([^)]+)\)$" (str owner))) ")"))]
       [true true true])
(def ls (.getUserPrincipalLookupService (FileSystems/getDefault)))
(check "acl: lookup by the owner's name" (= owner (.lookupPrincipalByName ls (.getName owner))) true)
(check "acl: lookup of no account" (first (caught #(.lookupPrincipalByName ls "no-such-user-xyz-123")))
       "UserPrincipalNotFoundException")
(def acl0 (vec (.getAcl av)))
(check "acl: the DACL has entries, each ALLOW or DENY"
       [(pos? (count acl0)) (every? #(#{"ALLOW" "DENY"} (str (.type %))) acl0)]
       [true true])
(def deny (-> (AclEntry/newBuilder) (.setType AclEntryType/DENY) (.setPrincipal owner)
              (.setPermissions (into-array AclEntryPermission [AclEntryPermission/WRITE_DATA AclEntryPermission/APPEND_DATA]))
              .build))
(let [acl (.getAcl av)] (.add acl 0 deny) (.setAcl av acl))
(check "acl: the entry set is the first one read back" (= deny (first (.getAcl av))) true)
(check "acl: icacls shows the DENY" (str/includes? (cmd "icacls" (str p)) "(DENY)") true)
(check "acl: the deny is enforced" (do (caught #(spit (str p) "more")) (slurp (str p))) "data")
(Files/setAttribute p "acl:acl" acl0 lo)
(check "acl: restored" (= acl0 (vec (.getAcl av))) true)
(check "acl: icacls no longer shows a DENY" (str/includes? (cmd "icacls" (str p)) "(DENY)") false)
(.setOwner av owner)
(Files/setOwner p owner)
(check "acl: setOwner to the owner" (= owner (Files/getOwner p lo)) true)
(check "acl: acl:* names" (sort (keys (Files/readAttributes p "acl:*" lo))) ["acl" "owner"])
(check "acl: names the view has not"
       [(caught #(Files/readAttributes p "acl:bogus" lo)) (caught #(Files/setAttribute p "acl:bogus" 1 lo))
        (caught #(Files/readAttributes p "owner:bogus" lo))]
       [["IllegalArgumentException" "'acl:bogus' not recognized"] ["IllegalArgumentException" "'acl:bogus' not recognized"]
        ["IllegalArgumentException" "'owner:bogus' not recognized"]])
(check "acl: a missing file" (first (caught #(.getAcl (Files/getFileAttributeView (.resolve d "gone") AclFileAttributeView lo))))
       "NoSuchFileException")

;; ---- directory order: FindFirstFileW/FindNextFileW's, as `dir /b /a` lists ----
(def od (Files/createTempDirectory "jno" fa))
(def names (mapv (fn [i] (str (char (+ (if (even? i) 65 97) (mod (* i 7) 26))) i)) (range 40)))
(doseq [n names] (spit (str (.resolve od n)) n))
(def dir-order (vec (str/split-lines (cmd "dir" "/b" "/a" (str od)))))
(defn nm [x] (str (.getFileName x)))
(check "order: dir lists what is there" (= (set dir-order) (set names)) true)
(check "order: newDirectoryStream, File.list, listFiles, Files/list, walkFileTree follow it"
       (mapv #(= dir-order %)
             [(with-open [s (Files/newDirectoryStream od)] (mapv nm s))
              (vec (.list (java.io.File. (str od))))
              (mapv #(.getName %) (.listFiles (java.io.File. (str od))))
              (with-open [s (Files/list od)] (mapv nm (iterator-seq (.iterator s))))
              (let [seen (atom [])]
                (Files/walkFileTree od (reify java.nio.file.FileVisitor
                                         (preVisitDirectory [_ x a] java.nio.file.FileVisitResult/CONTINUE)
                                         (visitFile [_ x a] (swap! seen conj (nm x)) java.nio.file.FileVisitResult/CONTINUE)
                                         (visitFileFailed [_ x e] java.nio.file.FileVisitResult/CONTINUE)
                                         (postVisitDirectory [_ x e] java.nio.file.FileVisitResult/CONTINUE)))
                @seen)])
       [true true true true true])
(doseq [n names] (Files/delete (.resolve od n)))
(Files/delete od)

(Files/delete p)
(Files/delete d)
(println (if (zero? @failures) "all passed" (str @failures " failed")))
(System/exit (if (zero? @failures) 0 1))

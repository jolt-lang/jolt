# jolt-at0a (inc X) — #inst / #uuid literals + java.time formatting on Chez.
# #inst lowers (analyzer :inst node) to a jinst value (RFC3339 ms, partial
# defaults + offsets); #uuid to a juuid; the java.time shim (DateTimeFormatter/
# Instant/ZoneId/LocalDateTime/FormatStyle/Locale/Date.
# Reader data-readers and statics emit the runtime value
# (host/chez/inst-time.ss).
#
#
#   janet test/chez/_insttime.janet
(def jolt-bin (or (os/getenv "JOLT_BIN") "bin/joltc"))

(def cases
  [# --- #inst reading + identity ---
   ["(inst? #inst \"2020-01-01T00:00:00Z\")"                          "true"]
   ["(inst-ms #inst \"1970-01-01T00:00:01Z\")"                        "1000"]
   ["(inst-ms #inst \"1970-01-01T00:00:00.123Z\")"                    "123"]
   ["(inst-ms #inst \"2020-01-01T00:00:00Z\")"                        "1577836800000"]
   ["(inst-ms* #inst \"1970-01-01T00:00:00Z\")"                       "0"]
   ["(some? #inst \"2020-01-01T00:00:00Z\")"                          "true"]
   ["(str (type #inst \"2020-01-01T00:00:00Z\"))"                     ":jolt/inst"]
   # --- partial timestamps + offsets (value equality by instant) ---
   ["(= #inst \"2020\" #inst \"2020-01-01T00:00:00.000Z\")"           "true"]
   ["(= #inst \"2020-03\" #inst \"2020-03-01T00:00:00Z\")"            "true"]
   ["(= #inst \"2020-03-15\" #inst \"2020-03-15T00:00:00Z\")"         "true"]
   ["(= #inst \"2020-01-01T01:00:00+01:00\" #inst \"2020-01-01T00:00:00Z\")" "true"]
   ["(= #inst \"2019-12-31T23:00:00-01:00\" #inst \"2020-01-01T00:00:00Z\")" "true"]
   ["(= #inst \"2020-01-01T00:00:00Z\" #inst \"2020-01-01T00:00:01Z\")" "false"]
   # --- map key + pr-str round trip ---
   ["(get {#inst \"2020-01-01T00:00:00Z\" :v} #inst \"2020-01-01T00:00:00.000Z\")" ":v"]
   ["(pr-str #inst \"2020-01-01T00:00:00Z\")"                         "#inst \"2020-01-01T00:00:00.000-00:00\""]
   ["(str #inst \"2020-01-01T00:00:00Z\")"                            "2020-01-01T00:00:00.000-00:00"]
   # --- #uuid ---
   ["(uuid? #uuid \"550e8400-e29b-41d4-a716-446655440000\")"         "true"]
   ["(= #uuid \"550E8400-E29B-41D4-A716-446655440000\" #uuid \"550e8400-e29b-41d4-a716-446655440000\")" "true"]
   ["(pr-str #uuid \"550e8400-e29b-41d4-a716-446655440000\")"        "#uuid \"550e8400-e29b-41d4-a716-446655440000\""]
   # --- java.util.Date / java.time.Instant interop ---
   ["(.getTime #inst \"1970-01-01T00:00:00Z\")"                       "0"]
   ["(instance? java.util.Date #inst \"2020-01-01T00:00:00Z\")"       "true"]
   ["(instance? java.sql.Timestamp #inst \"2020-01-01T00:00:00Z\")"   "false"]
   ["(.toEpochMilli (Instant/ofEpochMilli 1234))"                     "1234"]
   ["(instance? java.time.Instant (Instant/ofEpochMilli 0))"          "true"]
   ["(> (.toEpochMilli (Instant/now)) 1500000000000)"                 "true"]
   ["(instance? LocalDateTime (-> #inst \"2020-03-05T13:45:30Z\" (.toInstant) (.atZone (ZoneId/systemDefault)) (.toLocalDateTime)))" "true"]
   # --- DateTimeFormatter pattern engine ---
   ["(.format (DateTimeFormatter/ofPattern \"yyyy-MM-dd\") #inst \"2020-03-05T13:45:30Z\")" "2020-03-05"]
   ["(boolean (re-matches #\"[A-Z][a-z]{2} \\d{1,2}, 2020 \\d{1,2}:\\d{2} [AP]M\" (.format (DateTimeFormatter/ofPattern \"MMM d, yyyy h:mm a\") #inst \"2020-03-05T13:45:30Z\")))" "true"]
   ["(boolean (re-matches #\"\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}\" (.format DateTimeFormatter/ISO_LOCAL_DATE_TIME #inst \"2020-03-05T13:45:30Z\")))" "true"]
   ["(string? (.format (DateTimeFormatter/ofLocalizedDate FormatStyle/MEDIUM) #inst \"2020-03-05T13:45:30Z\"))" "true"]
   ["(string? (.format (.withLocale (DateTimeFormatter/ofPattern \"yyyy\") (java.util.Locale. \"en\")) #inst \"2020-01-01T00:00:00Z\"))" "true"]])

(defn run-capture [bin expr]
  (def proc (os/spawn [bin "-e" expr] :p {:out :pipe :err :pipe}))
  (def out (ev/read (proc :out) 0x100000))
  (def err (ev/read (proc :err) 0x100000))
  (def code (os/proc-wait proc))
  (def lines (filter (fn [l] (not (empty? l)))
                     (string/split "\n" (string/trim (if out (string out) "")))))
  [code (if (empty? lines) "" (last lines)) (string/trim (if err (string err) ""))])

(var pass 0)
(def fails @[])
(each [expr expected] cases
  (def [code got err] (run-capture jolt-bin expr))
  (cond
    (not= code 0) (array/push fails [expr (string "exit " code "; err: " err)])
    (= got expected) (++ pass)
    (array/push fails [expr (string "want `" expected "`, got `" got "`")])))

(printf "\n_insttime parity [%s]: %d/%d passed" jolt-bin pass (length cases))
(when (> (length fails) 0)
  (printf "%d FAIL(s):" (length fails))
  (each [e m] fails (printf "  FAIL %s\n    %s" e m)))
(flush)
(os/exit (if (empty? fails) 0 1))

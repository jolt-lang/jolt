# Specification: #inst literals — instants as values (spec 02-reader S20).
# An instant is an immutable tagged struct {:jolt/type :jolt/inst :ms <epoch-ms>}:
# value equality by instant (offset-normalized), usable as map keys.
# The reader accepts RFC3339 with Clojure's partial-timestamp defaults
# (#inst "2020" == #inst "2020-01-01T00:00:00.000-00:00").
(use ../support/harness)

(defspec "inst / reading & identity"
  ["reads to inst"      "true"  "(inst? #inst \"2020-01-01T00:00:00Z\")"]
  ["inst? false on string" "false" "(inst? \"2020-01-01\")"]
  ["epoch zero"         "0"     "(inst-ms #inst \"1970-01-01T00:00:00Z\")"]
  ["one second"         "1000"  "(inst-ms #inst \"1970-01-01T00:00:01Z\")"]
  ["millis"             "123"   "(inst-ms #inst \"1970-01-01T00:00:00.123Z\")"]
  ["a real date"        "1577836800000" "(inst-ms #inst \"2020-01-01T00:00:00Z\")"]
  ["inst-ms throws on non-inst" :throws "(inst-ms 42)"])

(defspec "inst / partial timestamps & offsets"
  ["year only"          "true"  "(= #inst \"2020\" #inst \"2020-01-01T00:00:00.000Z\")"]
  ["year-month"         "true"  "(= #inst \"2020-03\" #inst \"2020-03-01T00:00:00Z\")"]
  ["date only"          "true"  "(= #inst \"2020-03-15\" #inst \"2020-03-15T00:00:00Z\")"]
  ["positive offset"    "true"  "(= #inst \"2020-01-01T01:00:00+01:00\" #inst \"2020-01-01T00:00:00Z\")"]
  ["negative offset"    "true"  "(= #inst \"2019-12-31T23:00:00-01:00\" #inst \"2020-01-01T00:00:00Z\")"]
  ["-00:00 offset"      "true"  "(= #inst \"2020-01-01T00:00:00-00:00\" #inst \"2020-01-01T00:00:00Z\")"]
  ["bad timestamp throws" :throws "#inst \"garbage\""])

(defspec "inst / value semantics & printing"
  ["equal by instant"   "true"  "(= #inst \"2020-01-01T00:00:00Z\" #inst \"2020-01-01T00:00:00.000Z\")"]
  ["unequal instants"   "false" "(= #inst \"2020-01-01T00:00:00Z\" #inst \"2020-01-01T00:00:01Z\")"]
  ["works as map key"   ":v"    "(get {#inst \"2020-01-01T00:00:00Z\" :v} #inst \"2020-01-01T00:00:00.000Z\")"]
  ["pr-str round-trips" "\"#inst \\\"2020-01-01T00:00:00.000-00:00\\\"\"" "(pr-str #inst \"2020-01-01T00:00:00Z\")"])

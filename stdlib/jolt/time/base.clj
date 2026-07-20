(ns jolt.time.base
  "Core-owned aggregator for the base java.time API — the portable part that
  computes from epoch arithmetic alone and needs no OS timezone database or
  locale data. Autoloaded on first java.time.* use (see host/chez/java/host-static.ss),
  so date-free programs pay nothing and a program that touches java.time gets the
  base transparently with no dependency.

  The jolt-lang/time library adds the zone/locale layer on top: named-zone offset
  resolution and DST (ZoneId rules), ZonedDateTime/OffsetDateTime, localized
  formatting, and the tick API."
  (:require [jolt.time.enums]
            [jolt.time.local]
            [jolt.time.amount]
            [jolt.time.year]
            [jolt.time.temporal]
            [jolt.time.instant]))

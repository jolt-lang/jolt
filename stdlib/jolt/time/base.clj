(ns jolt.time.base
  "Core-owned aggregator for the base java.time API — the portable value types
  that compute from epoch arithmetic alone and need no OS timezone database or
  locale data: Instant, LocalDate/LocalTime/LocalDateTime, Duration, Period,
  Year/YearMonth/MonthDay, and the Month/DayOfWeek/Chrono* enums. Autoloaded on
  first java.time.* use (see host/chez/java/host-static.ss), so date-free programs
  pay nothing and a program that touches these gets them with no dependency.

  Everything that formats or names a zone lives in the jolt-lang/time library,
  which owns the single implementation of DateTimeFormatter, ZoneOffset/ZoneId,
  ZonedDateTime/OffsetDateTime, localized formatting, java.util.Locale, and the
  tick API. Core does not carry a second copy of any of those (RFC 0008)."
  (:require [jolt.time.enums]
            [jolt.time.local]
            [jolt.time.amount]
            [jolt.time.year]
            [jolt.time.temporal]
            [jolt.time.instant]))

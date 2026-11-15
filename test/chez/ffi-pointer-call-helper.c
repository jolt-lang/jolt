/* C witness for jolt.ffi/pointer-fn: every function a test calls is reached
 * through a pointer this library hands out, never by its own name. The static
 * functions are not exported, so a pointer is the only way to them, which is
 * the libghostty-vt shape (a callback slot in a struct the library owns). */
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

#ifdef _WIN32
#include <windows.h>
#define JOLT_PTR_EXPORT __declspec(dllexport)
#define JOLT_PTR_SET_NATIVE_ERROR(code) SetLastError((DWORD)(code))
#else
#include <errno.h>
#define JOLT_PTR_EXPORT __attribute__((visibility("default")))
#define JOLT_PTR_SET_NATIVE_ERROR(code) (errno = (int)(code))
#endif

/* -- scalars --------------------------------------------------------------- */

static int64_t add3(int64_t a, int64_t b) { return a + b + 3; }
static bool is_even(int32_t x) { return x % 2 == 0; }
static double scale(double x, float f) { return x * (double)f; }
static float halve(float x) { return x / 2.0f; }
static size_t c_strlen(const char *s) { return s ? strlen(s) : 999; }
static int fail_with(int e) { JOLT_PTR_SET_NATIVE_ERROR(e); return -1; }
static int8_t neg8(int8_t x) { return (int8_t)-x; }
static uint16_t twice16(uint16_t x) { return (uint16_t)(x * 2); }
static bool flip(bool b) { return !b; }

JOLT_PTR_EXPORT void *jolt_ptr_get_add(void) { return (void *)add3; }
JOLT_PTR_EXPORT void *jolt_ptr_get_is_even(void) { return (void *)is_even; }
JOLT_PTR_EXPORT void *jolt_ptr_get_scale(void) { return (void *)scale; }
JOLT_PTR_EXPORT void *jolt_ptr_get_halve(void) { return (void *)halve; }
JOLT_PTR_EXPORT void *jolt_ptr_get_strlen(void) { return (void *)c_strlen; }
JOLT_PTR_EXPORT void *jolt_ptr_get_fail_with(void) { return (void *)fail_with; }
JOLT_PTR_EXPORT void *jolt_ptr_get_neg8(void) { return (void *)neg8; }
JOLT_PTR_EXPORT void *jolt_ptr_get_twice16(void) { return (void *)twice16; }
JOLT_PTR_EXPORT void *jolt_ptr_get_flip(void) { return (void *)flip; }

/* A declared variadic tail through a pointer: sum `n` ints after it. */
static int64_t sum_ints(int n, ...) {
  va_list ap;
  int64_t total = 0;
  va_start(ap, n);
  for (int i = 0; i < n; i++) total += va_arg(ap, int);
  va_end(ap);
  return total;
}
JOLT_PTR_EXPORT void *jolt_ptr_get_sum_ints(void) { return (void *)sum_ints; }

/* -- by-value aggregates --------------------------------------------------- */

typedef struct { int32_t year; uint8_t month; uint8_t day; } jolt_ptr_date;
typedef struct { uint64_t a, b, c; } jolt_ptr_large; /* passed in memory */

static int64_t date_score(jolt_ptr_date d) {
  return (int64_t)d.year * 10000 + d.month * 100 + d.day;
}
static jolt_ptr_date make_date(int32_t y, uint8_t m, uint8_t d) {
  jolt_ptr_date out = {y, m, d};
  return out;
}
static uint64_t large_score(jolt_ptr_large v) { return v.a * 100 + v.b * 10 + v.c; }
static jolt_ptr_large add_large(jolt_ptr_large x, jolt_ptr_large y) {
  jolt_ptr_large out = {x.a + y.a, x.b + y.b, x.c + y.c};
  return out;
}

JOLT_PTR_EXPORT void *jolt_ptr_get_date_score(void) { return (void *)date_score; }
JOLT_PTR_EXPORT void *jolt_ptr_get_make_date(void) { return (void *)make_date; }
JOLT_PTR_EXPORT void *jolt_ptr_get_large_score(void) { return (void *)large_score; }
JOLT_PTR_EXPORT void *jolt_ptr_get_add_large(void) { return (void *)add_large; }

/* -- the two libghostty-vt shapes ------------------------------------------ */

/* GhosttyWriter: a struct handed over BY VALUE that carries the function and
 * the userdata to call it with. The writer appends into a caller buffer. */
typedef struct {
  bool (*write)(void *, const uint8_t *, size_t);
  void *userdata;
} jolt_ptr_writer;

typedef struct { char bytes[64]; size_t len; } jolt_ptr_sink;

static bool sink_write(void *userdata, const uint8_t *data, size_t n) {
  jolt_ptr_sink *sink = userdata;
  if (sink->len + n > sizeof sink->bytes - 1) return false;
  memcpy(sink->bytes + sink->len, data, n);
  sink->len += n;
  sink->bytes[sink->len] = '\0';
  return true;
}

/* jolt.ffi callbacks take no by-value aggregate, so a jolt test gets the
 * writer as a by-value RESULT and reads its two fields; a pointer-called
 * function then takes the writer back by value and writes through it. */
static jolt_ptr_sink the_sink;
JOLT_PTR_EXPORT jolt_ptr_writer jolt_ptr_writer_open(void) {
  memset(&the_sink, 0, sizeof the_sink);
  jolt_ptr_writer writer = {sink_write, &the_sink};
  return writer;
}
JOLT_PTR_EXPORT const char *jolt_ptr_writer_contents(void) { return the_sink.bytes; }

static bool writer_emit(jolt_ptr_writer w, const char *s) {
  return w.write(w.userdata, (const uint8_t *)s, strlen(s));
}
JOLT_PTR_EXPORT void *jolt_ptr_get_writer_emit(void) { return (void *)writer_emit; }

/* GhosttyClipboardRead: C hands jolt a pointer to a struct holding a reply
 * function that takes the struct itself; jolt must call read->reply(read, &r). */
typedef struct { const char *text; int32_t code; } jolt_ptr_reply;
typedef struct jolt_ptr_read {
  void (*reply)(const struct jolt_ptr_read *, const jolt_ptr_reply *);
  char got[64];
  int32_t code;
} jolt_ptr_read;

static void read_reply(const jolt_ptr_read *read, const jolt_ptr_reply *r) {
  jolt_ptr_read *self = (jolt_ptr_read *)read;
  strncpy(self->got, r->text ? r->text : "(null)", sizeof self->got - 1);
  self->code = r->code;
}

typedef void (*jolt_ptr_read_cb)(const jolt_ptr_read *);
JOLT_PTR_EXPORT size_t jolt_ptr_reply_offset(void) { return offsetof(jolt_ptr_read, reply); }
JOLT_PTR_EXPORT int32_t jolt_ptr_clipboard(jolt_ptr_read_cb cb, char *out, size_t cap) {
  jolt_ptr_read read;
  memset(&read, 0, sizeof read);
  read.reply = read_reply;
  read.code = -1;
  cb(&read);
  strncpy(out, read.got, cap - 1);
  out[cap - 1] = '\0';
  return read.code;
}

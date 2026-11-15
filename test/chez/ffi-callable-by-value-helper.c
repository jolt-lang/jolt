/* C witness for jolt.ffi callbacks that take and return structs by value.
 * Each jolt_cbv_* driver builds known structs, calls the callback it is handed
 * with them BY VALUE, and answers what the callback returned (or, for an
 * aggregate result, a checksum of the fields it wrote), so the jolt side can
 * check both what it read and what C read back. */
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

/* libghostty-vt's paste reader: a string and a writer, both by value. */
typedef struct { const uint8_t *ptr; size_t len; } GhosttyString;
typedef struct {
  bool (*write)(void *userdata, const uint8_t *data, size_t len);
  void *userdata;
} GhosttyWriter;

typedef struct { int32_t year; uint8_t month; uint8_t day; } Date; /* 8 bytes */
typedef struct { int64_t a; int64_t b; } Pair;                     /* 16 bytes */
typedef struct { uint64_t a; uint64_t b; uint64_t c; } Large;      /* 24 bytes */
typedef struct { float x; float y; } Vec2f;                        /* HFA */
typedef struct { double x; double y; double z; } Vec3d;            /* HFA, 24 bytes */
typedef struct { float f; int32_t i; } Mixed;                      /* not an HFA */

static bool writer_write(void *userdata, const uint8_t *data, size_t len) {
  (void)userdata; (void)data; (void)len;
  return true;
}
static int writer_state;
static const char mime_text[] = "text/plain;charset=utf-8";

void *jolt_cbv_writer_fn(void) { return (void *)writer_write; }
void *jolt_cbv_writer_userdata(void) { return &writer_state; }
void *jolt_cbv_mime_ptr(void) { return (void *)mime_text; }
size_t jolt_cbv_mime_len(void) { return sizeof mime_text - 1; }

/* read_clipboard(userdata, mime, writer). */
int32_t jolt_cbv_paste(bool (*cb)(void *, GhosttyString, GhosttyWriter), void *userdata) {
  GhosttyString mime = { (const uint8_t *)mime_text, sizeof mime_text - 1 };
  GhosttyWriter writer = { writer_write, &writer_state };
  return cb(userdata, mime, writer) ? 1 : 0;
}

int64_t jolt_cbv_date(int64_t (*cb)(Date)) {
  Date d = { -123456789, 250, 251 };
  return cb(d);
}
int64_t jolt_cbv_pair(int64_t (*cb)(Pair)) {
  Pair p = { INT64_MIN + 7, INT64_MAX - 9 };
  return cb(p);
}
uint64_t jolt_cbv_large(uint64_t (*cb)(Large)) {
  Large l = { 1, 2, 3 };
  return cb(l);
}
/* Two large structs: on arm64 both travel as pointers to caller copies. */
uint64_t jolt_cbv_two_large(uint64_t (*cb)(Large, Large)) {
  Large l = { 1, 2, 3 }, m = { 40, 50, 60 };
  return cb(l, m);
}
double jolt_cbv_vec2f(double (*cb)(Vec2f)) {
  Vec2f v = { 1.5f, -2.25f };
  return cb(v);
}
double jolt_cbv_vec3d(double (*cb)(Vec3d)) {
  Vec3d v = { 0.5, 1024.25, -3.0 };
  return cb(v);
}
double jolt_cbv_mixed(double (*cb)(Mixed)) {
  Mixed m = { 0.75f, -42 };
  return cb(m);
}
/* Scalars and structs interleaved, with converted positions among them. */
bool jolt_cbv_interleaved(bool (*cb)(int8_t, Date, float, const char *, Large, double),
                          const char *s) {
  Date d = { 2026, 7, 23 };
  Large l = { 10, 20, 30 };
  return cb(-3, d, 0.5f, s, l, 6.25);
}

/* Aggregate results: C reads back what the callback wrote. */
int64_t jolt_cbv_ret_date(Date (*cb)(int32_t), int32_t year) {
  Date d = cb(year);
  return (int64_t)d.year * 10000 + d.month * 100 + d.day;
}
uint64_t jolt_cbv_ret_large(Large (*cb)(uint64_t, Large), uint64_t k) {
  Large in = { 100, 200, 300 };
  Large out = cb(k, in);
  return out.a * 1000000 + out.b * 1000 + out.c;
}
double jolt_cbv_ret_vec3d(Vec3d (*cb)(double)) {
  Vec3d v = cb(2.0);
  return v.x * 100 + v.y * 10 + v.z;
}

/* :blocking-call target that invokes a :collect-safe callback on the calling
 * thread while it is deactivated. */
int64_t jolt_cbv_blocking_date(int64_t (*cb)(Date)) { return jolt_cbv_date(cb); }
int64_t jolt_cbv_blocking_ret_date(Date (*cb)(int32_t)) { return jolt_cbv_ret_date(cb, 1999); }

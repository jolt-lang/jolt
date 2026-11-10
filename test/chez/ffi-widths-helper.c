/* Exact-width native argument, result, and callback witnesses for jolt.ffi. */
#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdarg.h>

#ifdef _WIN32
#define JOLT_WIDTHS_EXPORT __declspec(dllexport)
#else
#define JOLT_WIDTHS_EXPORT __attribute__((visibility("default")))
#endif

JOLT_WIDTHS_EXPORT int64_t jolt_w_widen_i8(int8_t value) { return value; }
JOLT_WIDTHS_EXPORT int64_t jolt_w_widen_i16(int16_t value) { return value; }
JOLT_WIDTHS_EXPORT uint64_t jolt_w_widen_u16(uint16_t value) { return value; }
JOLT_WIDTHS_EXPORT int64_t jolt_w_widen_i32(int32_t value) { return value; }
JOLT_WIDTHS_EXPORT uint64_t jolt_w_widen_u32(uint32_t value) { return value; }

JOLT_WIDTHS_EXPORT int8_t jolt_w_return_i8(void) { return INT8_MIN; }
JOLT_WIDTHS_EXPORT int16_t jolt_w_return_i16(void) { return INT16_MIN; }
JOLT_WIDTHS_EXPORT uint16_t jolt_w_return_u16(void) { return UINT16_MAX; }
JOLT_WIDTHS_EXPORT int32_t jolt_w_return_i32(void) { return INT32_MIN; }
JOLT_WIDTHS_EXPORT uint32_t jolt_w_return_u32(void) { return UINT32_MAX; }

typedef int8_t (*jolt_w_i8_callback)(int8_t);
typedef int16_t (*jolt_w_i16_callback)(int16_t);
typedef uint16_t (*jolt_w_u16_callback)(uint16_t);
typedef int32_t (*jolt_w_i32_callback)(int32_t);
typedef uint32_t (*jolt_w_u32_callback)(uint32_t);

JOLT_WIDTHS_EXPORT int64_t jolt_w_call_i8(jolt_w_i8_callback callback) {
  return callback(INT8_MIN);
}
JOLT_WIDTHS_EXPORT int64_t jolt_w_call_i16(jolt_w_i16_callback callback) {
  return callback(INT16_MIN);
}
JOLT_WIDTHS_EXPORT uint64_t jolt_w_call_u16(jolt_w_u16_callback callback) {
  return callback(UINT16_MAX);
}
JOLT_WIDTHS_EXPORT int64_t jolt_w_call_i32(jolt_w_i32_callback callback) {
  return callback(INT32_MIN);
}
JOLT_WIDTHS_EXPORT uint64_t jolt_w_call_u32(jolt_w_u32_callback callback) {
  return callback(UINT32_MAX);
}

/* :bool is a ONE-BYTE C boolean, so these are declared with stdbool's `bool`
   (C99 _Bool) and not with int. A binding that used an int-sized carrier passes
   the argument case by luck and fails the RETURN case, where the three bytes
   above the result are whatever the callee left there. */
JOLT_WIDTHS_EXPORT size_t jolt_w_sizeof_bool(void) { return sizeof(bool); }
JOLT_WIDTHS_EXPORT int64_t jolt_w_widen_bool(bool value) { return value ? 42 : -42; }
JOLT_WIDTHS_EXPORT bool jolt_w_return_bool_true(void) { return true; }
JOLT_WIDTHS_EXPORT bool jolt_w_return_bool_false(void) { return false; }

typedef bool (*jolt_w_bool_callback)(bool);
/* 10 * f(true) + f(false), so one number reports both directions. */
JOLT_WIDTHS_EXPORT int64_t jolt_w_call_bool(jolt_w_bool_callback callback) {
  return (callback(true) ? 10 : 0) + (callback(false) ? 1 : 0);
}

/* The ABI only defines the LOW byte of a _Bool in a register (x86-64 SysV:
   %al, AArch64: the low byte of w0), so a callee may leave anything above it.
   A compiler returning a real `bool` zeroes those bits anyway, so these return
   int and are BOUND as :bool: 0x100 is false with bit 8 set, 0x101 is true.
   An int-sized read answers 256 and 257, both truthy. */
JOLT_WIDTHS_EXPORT int jolt_w_return_dirty_false(void) { return 0x100; }
JOLT_WIDTHS_EXPORT int jolt_w_return_dirty_true(void) { return 0x101; }

/* The same garbage in the other direction: C hands a :bool callback a register
   whose low byte says false and whose upper bits do not. */
typedef int (*jolt_w_int_callback)(int);
JOLT_WIDTHS_EXPORT int64_t jolt_w_call_bool_dirty(jolt_w_int_callback callback) {
  return (callback(0x100) & 0xff) ? 1 : 0;
}

/* _Bool fields in a by-value struct: one byte each, packed between the ints. */
typedef struct { int32_t a; bool flag; bool other; int32_t b; } jolt_w_bool_struct;
JOLT_WIDTHS_EXPORT size_t jolt_w_sizeof_bool_struct(void) { return sizeof(jolt_w_bool_struct); }
JOLT_WIDTHS_EXPORT size_t jolt_w_offsetof_bool_other(void) {
  return offsetof(jolt_w_bool_struct, other);
}
JOLT_WIDTHS_EXPORT int64_t jolt_w_bool_struct_flags(jolt_w_bool_struct s) {
  return (s.flag ? 10 : 0) + (s.other ? 1 : 0);
}
JOLT_WIDTHS_EXPORT jolt_w_bool_struct jolt_w_make_bool_struct(bool flag) {
  jolt_w_bool_struct s = {7, flag, !flag, 9};
  return s;
}

/* A _Bool passed through `...` is promoted to int, so va_arg reads an int. */
JOLT_WIDTHS_EXPORT int64_t jolt_w_sum_promoted_bools(int n, ...) {
  va_list ap;
  int64_t sum = 0;
  va_start(ap, n);
  for (int i = 0; i < n; i++) sum = sum * 10 + va_arg(ap, int);
  va_end(ap);
  return sum;
}

/* :float and :double take a java.lang.Float from jolt as well as a double.
   The result is negated, so a value that reached C as 0.0 cannot pass. The
   results are double, so the test does not depend on how a :float result is
   boxed. */
JOLT_WIDTHS_EXPORT double jolt_w_neg_float(float value) { return -(double)value; }
JOLT_WIDTHS_EXPORT double jolt_w_neg_double(double value) { return -value; }

typedef float (*jolt_w_float_callback)(float);
typedef double (*jolt_w_double_callback)(double);
JOLT_WIDTHS_EXPORT double jolt_w_call_float(jolt_w_float_callback callback) {
  return callback(1.5f);
}
JOLT_WIDTHS_EXPORT double jolt_w_call_double(jolt_w_double_callback callback) {
  return callback(1.5);
}

/* A :float result and a :float callback argument reach jolt as a
   java.lang.Float, as a C float reaches Clojure on the JVM. */
JOLT_WIDTHS_EXPORT float jolt_w_half_float(float value) { return value / 2.0f; }

typedef int64_t (*jolt_w_float_arg_callback)(float);
JOLT_WIDTHS_EXPORT int64_t jolt_w_call_float_arg(jolt_w_float_arg_callback callback) {
  return callback(1.5f);
}

/* Exact-width native argument, result, and callback witnesses for jolt.ffi. */
#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

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

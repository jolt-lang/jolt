/* ffi-stack-args-helper.c: C functions taking arguments past the argument
   registers, for test/chez/ffi-stack-args-test.ss (chez-patches/0006).
   Apple's arm64 ABI packs a scalar stack argument at its natural alignment
   but gives any composite that isn't a homogeneous floating-point aggregate
   8-byte-aligned slots; a struct over 16 bytes goes by pointer. Each
   function folds its arguments in order, so one read from the wrong place
   changes the result. */
#include <stdint.h>

#ifdef _WIN32
#define JOLT_STACK_EXPORT __declspec(dllexport)
#else
#define JOLT_STACK_EXPORT __attribute__((visibility("default")))
#endif

typedef struct { float a, b, c; } three_floats;
#define FOLD(r, v) ((r) * 7 + (long long)(v))
#define FOLD8(a, b, c, d, e, f, g, h) FOLD(FOLD(FOLD(FOLD(FOLD(FOLD(FOLD(FOLD(0, a), b), c), d), e), f), g), h)

typedef struct { int x, y; } two_ints;
typedef struct { char a, b, c; } three_chars;
typedef struct { short a; } one_short;
typedef struct { float a; } one_float;
typedef struct { long long a, b, c; } three_longs;

JOLT_STACK_EXPORT long long stack_int_two_ints(int a, int b, int c, int d, int e, int f, int g, int h, int i, two_ints j, int k) {
  return FOLD(FOLD(FOLD(FOLD(FOLD8(a, b, c, d, e, f, g, h), i), j.x), j.y), k);
}

JOLT_STACK_EXPORT long long stack_short_one_short(int a, int b, int c, int d, int e, int f, int g, int h, short i, one_short j, short k) {
  return FOLD(FOLD(FOLD(FOLD8(a, b, c, d, e, f, g, h), i), j.a), k);
}

JOLT_STACK_EXPORT long long stack_char_three_chars(int a, int b, int c, int d, int e, int f, int g, int h, char i, three_chars j, char k) {
  return FOLD(FOLD(FOLD(FOLD(FOLD(FOLD8(a, b, c, d, e, f, g, h), i), j.a), j.b), j.c), k);
}

JOLT_STACK_EXPORT long long stack_three_chars_short(int a, int b, int c, int d, int e, int f, int g, int h, char i, char j, char k, short l, int m) {
  return FOLD(FOLD(FOLD(FOLD(FOLD(FOLD8(a, b, c, d, e, f, g, h), i), j), k), l), m);
}

JOLT_STACK_EXPORT double stack_three_floats_char_three_floats(int a, int b, int c, int d, int e, int f, int g, int h,
                                                   double da, double db, double dc, double dd, double de, double df, double dg, double dh,
                                                   three_floats i, char j, three_floats k) {
  return a + b + c + d + e + f + g + h + da + db + dc + dd + de + df + dg + dh
    + i.a + 2 * i.b + 3 * i.c + 4 * j + 5 * k.a + 6 * k.b + 7 * k.c;
}

JOLT_STACK_EXPORT double stack_one_float_char_one_float(int a, int b, int c, int d, int e, int f, int g, int h,
                                             double da, double db, double dc, double dd, double de, double df, double dg, double dh,
                                             one_float i, char j, one_float k) {
  return a + b + c + d + e + f + g + h + da + db + dc + dd + de + df + dg + dh
    + i.a + 2 * j + 3 * k.a;
}

JOLT_STACK_EXPORT long long stack_three_longs(int a, int b, int c, int d, int e, int f, int g, int h, three_longs i, int j) {
  return FOLD(FOLD(FOLD(FOLD(FOLD8(a, b, c, d, e, f, g, h), i.a), i.b), i.c), j);
}


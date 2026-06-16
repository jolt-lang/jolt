/* Native-C mandelbrot, exposed as a Janet module — the lever-1 ceiling probe for
 * the foundational-runtime epic (jolt-5vsp). Measures:
 *   (1) mandel/run-c        — whole run in C (count_point inlined in C). The pure
 *                             native-codegen ceiling: no Janet in the hot loop.
 *   (2) mandel/count-point-c — just count_point exposed as a Janet cfunction, so a
 *                             Janet `while` loop can call it n^2 times. Measures the
 *                             Janet->C boundary-crossing cost — the incremental
 *                             hybrid (hot fn in C, caller still bytecode) pays this.
 * Build: jpm --local build  (project.janet declares the native module). */
#include <janet.h>

/* Pure C. cr/ci/cap are doubles; cap compared as int iteration count. */
static long count_point(double cr, double ci, long cap) {
    long i = 0;
    double zr = 0.0, zi = 0.0;
    while (i < cap && (zr*zr + zi*zi) <= 4.0) {
        double nzr = zr*zr - zi*zi + cr;
        double nzi = 2.0*zr*zi + ci;
        zr = nzr; zi = nzi;
        i++;
    }
    return i;
}

static long run_c(long n) {
    long cap = 200;
    double nd = (double)n;
    long acc = 0;
    for (long y = 0; y < n; y++) {
        double ci = (2.0*y)/nd - 1.0;
        long a = 0;
        for (long x = 0; x < n; x++) {
            double cr = (2.0*x)/nd - 1.5;
            a += count_point(cr, ci, cap);
        }
        acc += a;
    }
    return acc;
}

static Janet cfun_run_c(int32_t argc, Janet *argv) {
    janet_fixarity(argc, 1);
    long n = (long)janet_getinteger(argv, 0);
    return janet_wrap_number((double)run_c(n));
}

/* count_point exposed for the Janet-loop-calls-C boundary test. */
static Janet cfun_count_point_c(int32_t argc, Janet *argv) {
    janet_fixarity(argc, 3);
    double cr = janet_getnumber(argv, 0);
    double ci = janet_getnumber(argv, 1);
    long cap = (long)janet_getinteger(argv, 2);
    return janet_wrap_number((double)count_point(cr, ci, cap));
}

/* run loop in C, but count_point is a Janet function called back via janet_call
 * n^2 times — the reverse crossing: a C-compiled hot fn invoking a cold bytecode
 * helper. Measures janet_call overhead (the cost the hybrid pays when native code
 * calls back into the bytecode world). */
static Janet cfun_run_callback(int32_t argc, Janet *argv) {
    janet_fixarity(argc, 2);
    long n = (long)janet_getinteger(argv, 0);
    JanetFunction *cp = janet_getfunction(argv, 1);
    long cap = 200;
    double nd = (double)n;
    long acc = 0;
    for (long y = 0; y < n; y++) {
        double ci = (2.0*y)/nd - 1.0;
        long a = 0;
        for (long x = 0; x < n; x++) {
            double cr = (2.0*x)/nd - 1.5;
            Janet args[3] = { janet_wrap_number(cr), janet_wrap_number(ci),
                              janet_wrap_number((double)cap) };
            Janet r = janet_call(cp, 3, args);
            a += (long)janet_unwrap_number(r);
        }
        acc += a;
    }
    return janet_wrap_number((double)acc);
}

static const JanetReg cfuns[] = {
    {"run-c", cfun_run_c, "(mandel/run-c n) whole mandelbrot run in native C."},
    {"count-point-c", cfun_count_point_c, "(mandel/count-point-c cr ci cap) one point, native C."},
    {"run-callback", cfun_run_callback, "(mandel/run-callback n count-point-fn) C loop calling a Janet fn back via janet_call."},
    {NULL, NULL, NULL}
};

JANET_MODULE_ENTRY(JanetTable *env) {
    janet_cfuns(env, "mandel", cfuns);
}

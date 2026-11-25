/* jolt's JIT for native regions (stdlib/jolt/native): a region's C, compiled
   in this process by MIR's C compiler (vendor/mir, c2mir) the first time the
   region's call site loads, so a region runs as native code in the REPL and
   under jolt run as it does in a binary jolt build linked it into.

   The release jolt links this file and MIR in (build-jolt.ss); a source
   checkout loads them as one shared object, target/mir/libjoltmir (make
   mirlib). host/chez/java/natives-array.ss finds jolt_mir_compile either way.

   A region's C reaches the JIT exactly as cc sees it in a build, but c2mir
   predefines __MIRC__, and under it the prelude (stdlib/jolt/native/emit.clj)
   takes three things from here rather than from headers a machine without a
   C toolchain may not carry:
     jolt_mir_status       the status of the region this thread ran last. MIR
                           has no thread-local storage, so every JIT'd region
                           shares this one, as every region a build links
                           shares the translation unit's.
     jolt_mir_stack_limit  the lowest stack address a recursive helper may
                           enter at (the thread's stack less HEADROOM).
     jolt_mir_frame        an address in the caller's frame, for the helper's
                           check against that limit.
   Everything else a region calls (malloc, sqrt, a defextern's function) is
   resolved by the resolver Chez passes in, which is Chez's own foreign-entry. */
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <setjmp.h>
#include <stdarg.h>
#include "mir.h"
#include "mir-gen.h"
#include "c2mir/c2mir.h"

#if defined(_WIN32)
/* GetCurrentThreadStackLimits is Windows 8's; mingw declares it only from there */
#if !defined(_WIN32_WINNT) || _WIN32_WINNT < 0x0602
#undef _WIN32_WINNT
#define _WIN32_WINNT 0x0602
#endif
#include <windows.h>
#define JOLT_MIR_EXPORT __declspec(dllexport)
#else
#include <pthread.h>
#define JOLT_MIR_EXPORT __attribute__((visibility("default")))
#endif

/* the layout of the prelude's jn_out */
typedef struct {
  int64_t code; int64_t a; int64_t b; double d; int64_t len;
  uintptr_t stack_lo; uintptr_t stack_thr;
} jolt_mir_out;

static _Thread_local jolt_mir_out jolt_mir_tls;

JOLT_MIR_EXPORT jolt_mir_out *jolt_mir_status(void) { return &jolt_mir_tls; }

JOLT_MIR_EXPORT __attribute__((noinline)) uintptr_t jolt_mir_frame(void) {
  volatile char here = 0;
  return (uintptr_t)&here;
}

#if defined(__linux__)
int pthread_getattr_np(pthread_t, pthread_attr_t *);
#endif

/* The prelude's jn_stack_limit_slow, for the JIT: read once per thread. */
JOLT_MIR_EXPORT uintptr_t jolt_mir_stack_limit(uintptr_t headroom) {
  if (jolt_mir_tls.stack_lo) return jolt_mir_tls.stack_lo;
  uintptr_t lo = 0, size = 0;
#if defined(__APPLE__)
  uintptr_t hi = (uintptr_t)pthread_get_stackaddr_np(pthread_self());
  size = pthread_get_stacksize_np(pthread_self());
  lo = hi - size;
#elif defined(__linux__)
  pthread_attr_t at; void *addr; size_t sz;
  if (pthread_getattr_np(pthread_self(), &at) == 0) {
    if (pthread_attr_getstack(&at, &addr, &sz) == 0) { lo = (uintptr_t)addr; size = sz; }
    pthread_attr_destroy(&at);
  }
#elif defined(_WIN32)
  ULONG_PTR l = 0, h = 0;
  GetCurrentThreadStackLimits(&l, &h);
  lo = (uintptr_t)l; size = (uintptr_t)(h - l);
#endif
  uintptr_t head = size / 4 < headroom ? size / 4 : headroom;
  jolt_mir_tls.stack_lo = size ? lo + head : 1;
  return jolt_mir_tls.stack_lo;
}

/* --- the compiler ----------------------------------------------------------
   One MIR context for the process, used under the caller's lock (Chez holds a
   mutex around every call here). A failure MIR reports through its error
   function (an unresolved name at link, an internal error) longjmps back to
   jolt_mir_compile, which then abandons the context: the code it generated
   for earlier regions stays where it is, and the next compile starts a fresh
   one. */
static MIR_context_t jolt_mir_ctx;
static long jolt_mir_modules;
static jmp_buf *jolt_mir_jmp;
static char jolt_mir_msg[4096];
static void *(*jolt_mir_resolver)(const char *);

static void MIR_NO_RETURN jolt_mir_on_error(MIR_error_type_t type, const char *fmt, ...) {
  va_list ap;
  (void)type;
  va_start(ap, fmt);
  vsnprintf(jolt_mir_msg, sizeof jolt_mir_msg, fmt, ap);
  va_end(ap);
  if (jolt_mir_jmp) longjmp(*jolt_mir_jmp, 1);
  abort();
}

static void *jolt_mir_resolve(const char *name) {
  void *p = jolt_mir_resolver ? jolt_mir_resolver(name) : NULL;
  /* MIR's link raises on a NULL: name the symbol rather than its message */
  if (p == NULL) {
    snprintf(jolt_mir_msg, sizeof jolt_mir_msg, "undefined symbol %s", name);
    if (jolt_mir_jmp) longjmp(*jolt_mir_jmp, 1);
  }
  return p;
}

/* Where c2mir looks for a header a region includes (a defextern's :header)
   beyond the system's own: each directory JOLT_MIR_INCLUDE lists, and on
   Android Termux's $PREFIX/include, which c2mir's /usr/include misses. Read
   once; the strings live as long as the process. */
#if defined(_WIN32)
#define JOLT_MIR_PATH_SEP ';'
#else
#define JOLT_MIR_PATH_SEP ':'
#endif
static const char *jolt_mir_incs[64];
/* zero-initialized, so .bss: the footprint gate keeps .data to the boot arrays */
static size_t jolt_mir_incs_n;
static int jolt_mir_incs_ready;
static void jolt_mir_add_inc(const char *dir, size_t len) {
  if (len == 0 || jolt_mir_incs_n >= 64) return;
  char *d = malloc(len + 1);
  if (!d) return;
  memcpy(d, dir, len);
  d[len] = 0;
  jolt_mir_incs[jolt_mir_incs_n++] = d;
}
static void jolt_mir_init_incs(void) {
  if (jolt_mir_incs_ready) return;
  jolt_mir_incs_ready = 1;
  const char *env = getenv("JOLT_MIR_INCLUDE");
  while (env && *env) {
    const char *end = strchr(env, JOLT_MIR_PATH_SEP);
    size_t len = end ? (size_t)(end - env) : strlen(env);
    jolt_mir_add_inc(env, len);
    env = end ? end + 1 : NULL;
  }
#if defined(__ANDROID__)
  const char *prefix = getenv("PREFIX");
  if (prefix && *prefix) {
    char buf[1024];
    int n = snprintf(buf, sizeof buf, "%s/include", prefix);
    if (n > 0 && (size_t)n < sizeof buf) jolt_mir_add_inc(buf, (size_t)n);
  }
#endif
}

/* --- the machine the JIT compiles for -------------------------------------------
   A region's (on guard ...) bodies are chosen by the C preprocessor. jolt build
   compiles for the build machine's baseline (no -march), so __AVX2__ and its
   kin are off there; the JIT compiles on the machine that runs the code, so it
   defines the GCC macro of each feature this CPU has, and a guard picks the
   body for it. Bit k of jolt_mir_cpu_features is feature k of
   jolt_mir_feature_macro; JOLT_MIR_CPU=baseline turns them all off. */
#define JOLT_MIR_NFEATURES 9
static const char *jolt_mir_feature_macro(int k) {
  switch (k) {
  case 0: return "__SSE4_1__";
  case 1: return "__SSE4_2__";
  case 2: return "__POPCNT__";
  case 3: return "__AVX__";
  case 4: return "__AVX2__";
  case 5: return "__FMA__";
  case 6: return "__BMI2__";
  case 7: return "__AVX512F__";
  case 8: return "__ARM_NEON";
  default: return NULL;
  }
}

JOLT_MIR_EXPORT const char *jolt_mir_feature_name(int64_t k) {
  return k >= 0 && k < JOLT_MIR_NFEATURES ? jolt_mir_feature_macro((int)k) : NULL;
}

JOLT_MIR_EXPORT int64_t jolt_mir_cpu_features(void) {
  int64_t f = 0;
  const char *env = getenv("JOLT_MIR_CPU");
  if (env && strcmp(env, "baseline") == 0) return 0;
#if (defined(__x86_64__) || defined(_M_X64)) && (defined(__GNUC__) || defined(__clang__))
  __builtin_cpu_init();
  if (__builtin_cpu_supports("sse4.1")) f |= 1 << 0;
  if (__builtin_cpu_supports("sse4.2")) f |= 1 << 1;
  if (__builtin_cpu_supports("popcnt")) f |= 1 << 2;
  if (__builtin_cpu_supports("avx")) f |= 1 << 3;
  if (__builtin_cpu_supports("avx2")) f |= 1 << 4;
  if (__builtin_cpu_supports("fma")) f |= 1 << 5;
  if (__builtin_cpu_supports("bmi2")) f |= 1 << 6;
  if (__builtin_cpu_supports("avx512f")) f |= 1 << 7;
#elif defined(__aarch64__) || defined(_M_ARM64)
  f |= 1 << 8; /* Advanced SIMD is part of every AArch64 core */
#endif
  return f;
}

typedef struct { const char *s; size_t i; } jolt_mir_src;
static int jolt_mir_getc(void *data) {
  jolt_mir_src *src = data;
  unsigned char c = (unsigned char)src->s[src->i];
  if (c == 0) return EOF;
  src->i++;
  return c;
}

/* What the last failed compile reported. */
JOLT_MIR_EXPORT const char *jolt_mir_error(void) { return jolt_mir_msg; }

/* Compile SRC, a region's C, and generate its code. Answers the module, whose
   functions jolt_mir_lookup finds, or NULL with the reason in jolt_mir_error.
   RESOLVE answers the address of a C name the source uses but does not define,
   or NULL. */
JOLT_MIR_EXPORT void *jolt_mir_compile(const char *src, void *(*resolve)(const char *)) {
  jmp_buf here;
  MIR_module_t module = NULL;
  FILE *messages = NULL;
  jolt_mir_msg[0] = 0;
  jolt_mir_resolver = resolve;
  jolt_mir_jmp = &here;
  if (setjmp(here)) {
    /* abandoned, not finished: earlier regions' code lives in it */
    jolt_mir_ctx = NULL;
    jolt_mir_jmp = NULL;
    if (messages) fclose(messages);
    if (!jolt_mir_msg[0]) snprintf(jolt_mir_msg, sizeof jolt_mir_msg, "MIR failed");
    return NULL;
  }
  if (jolt_mir_ctx == NULL) {
    jolt_mir_ctx = MIR_init();
    MIR_set_error_func(jolt_mir_ctx, jolt_mir_on_error);
    c2mir_init(jolt_mir_ctx);
    MIR_gen_init(jolt_mir_ctx);
    MIR_gen_set_optimize_level(jolt_mir_ctx, 2);
  }
  struct c2mir_options opts;
  memset(&opts, 0, sizeof opts);
  messages = tmpfile();
  opts.message_file = messages;
  opts.module_num = ++jolt_mir_modules;
  jolt_mir_init_incs();
  opts.include_dirs = jolt_mir_incs;
  opts.include_dirs_num = jolt_mir_incs_n;
  /* the running CPU's feature macros, and SSE/SSE2, which every x86-64 has */
  struct c2mir_macro_command macros[JOLT_MIR_NFEATURES + 2];
  size_t nmacros = 0;
#if defined(__x86_64__) || defined(_M_X64)
  macros[nmacros].def_p = 1; macros[nmacros].name = "__SSE__"; macros[nmacros].def = "1"; nmacros++;
  macros[nmacros].def_p = 1; macros[nmacros].name = "__SSE2__"; macros[nmacros].def = "1"; nmacros++;
#endif
  int64_t feats = jolt_mir_cpu_features();
  for (int k = 0; k < JOLT_MIR_NFEATURES; k++)
    if (feats & ((int64_t)1 << k)) {
      macros[nmacros].def_p = 1;
      macros[nmacros].name = jolt_mir_feature_macro(k);
      macros[nmacros].def = "1";
      nmacros++;
    }
  opts.macro_commands = macros;
  opts.macro_commands_num = nmacros;
  jolt_mir_src in = {src, 0};
  int ok = c2mir_compile(jolt_mir_ctx, &opts, jolt_mir_getc, &in, "native-region.c", NULL);
  if (!ok) {
    size_t n = 0;
    if (messages) {
      rewind(messages);
      n = fread(jolt_mir_msg, 1, sizeof jolt_mir_msg - 1, messages);
    }
    jolt_mir_msg[n] = 0;
    if (n == 0) snprintf(jolt_mir_msg, sizeof jolt_mir_msg, "c2mir could not compile the region");
    if (messages) fclose(messages);
    jolt_mir_jmp = NULL;
    return NULL;
  }
  if (messages) fclose(messages);
  messages = NULL;
  module = DLIST_TAIL(MIR_module_t, *MIR_get_module_list(jolt_mir_ctx));
  MIR_load_module(jolt_mir_ctx, module);
  MIR_link(jolt_mir_ctx, MIR_set_gen_interface, jolt_mir_resolve);
  jolt_mir_jmp = NULL;
  return module;
}

/* The address of the function NAME that MODULE defines, or NULL. */
JOLT_MIR_EXPORT void *jolt_mir_lookup(void *module, const char *name) {
  MIR_module_t m = module;
  for (MIR_item_t it = DLIST_HEAD(MIR_item_t, m->items); it != NULL; it = DLIST_NEXT(MIR_item_t, it))
    if (it->item_type == MIR_func_item && strcmp(it->u.func->name, name) == 0) return it->addr;
  return NULL;
}

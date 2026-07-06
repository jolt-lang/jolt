/* driver.c — load a jolt --library .so/.dylib and call an exported fn.
 *
 * Deliberately dlopens with RTLD_LOCAL so the test exercises the
 * jolt_library_init + jolt_lookup handoff (no reliance on global symbol
 * export). Prints add(2,3).
 */
#include <stdio.h>
#include <dlfcn.h>

typedef int (*init_fn)(int, char**);
typedef void* (*lookup_fn)(const char*);
typedef int (*add_fn)(int, int);

int main(int argc, char** argv) {
  if (argc < 2) { fprintf(stderr, "usage: driver <libpath>\n"); return 2; }
  void* h = dlopen(argv[1], RTLD_NOW | RTLD_LOCAL);
  if (!h) { fprintf(stderr, "dlopen failed: %s\n", dlerror()); return 1; }
  init_fn init = (init_fn)dlsym(h, "jolt_library_init");
  lookup_fn lookup = (lookup_fn)dlsym(h, "jolt_lookup");
  if (!init || !lookup) { fprintf(stderr, "missing init/lookup: %s\n", dlerror()); return 1; }
  if (init(1, NULL) != 0) { fprintf(stderr, "jolt_library_init failed\n"); return 1; }
  add_fn add = (add_fn)lookup("add");
  if (!add) { fprintf(stderr, "jolt_lookup(\"add\") returned NULL\n"); return 1; }
  printf("%d\n", add(2, 3));
  return 0;
}

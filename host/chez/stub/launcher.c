/* launcher.c — the native stub for self-contained jolt binaries (jolt-eaj).
 *
 * A toolchain-free `jolt build` (and joltc itself) produces an executable by
 * appending a Chez boot image to a copy of this prebuilt stub, framed as:
 *
 *     [stub bytes][boot bytes][boot-length : little-endian u64]["JOLTBOOT"]
 *
 * (see host/chez/java/io.ss jolt-append-payload!). At startup the stub locates
 * its own executable, reads the trailing 16-byte frame to find the boot, and
 * hands the boot to the Chez kernel — no external boot file, no Chez install.
 *
 * Built once at joltc-build time against the Chez kernel (libkernel.a + scheme.h)
 * by host/chez/build-joltc.ss; the resulting binary is embedded into joltc and
 * copied per app build. Inherently per-platform (the boot targets the host
 * machine-type), like a native compiler.
 */
#include "scheme.h"
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined(__APPLE__)
#include <mach-o/dyld.h>
static int self_path(char *buf, uint32_t size) {
  /* _NSGetExecutablePath fills buf and reports the needed size on overflow. */
  return _NSGetExecutablePath(buf, &size);
}
#else
#include <unistd.h>
static int self_path(char *buf, uint32_t size) {
  ssize_t n = readlink("/proc/self/exe", buf, (size_t)size - 1);
  if (n < 0) return -1;
  buf[n] = '\0';
  return 0;
}
#endif

#define JOLT_MAGIC "JOLTBOOT"
#define JOLT_MAGIC_LEN 8
#define JOLT_TRAILER_LEN 16 /* u64 length + 8-byte magic */

int main(int argc, char *argv[]) {
  char path[4096];
  if (self_path(path, (uint32_t)sizeof(path)) != 0) {
    fprintf(stderr, "jolt: cannot resolve own executable path\n");
    return 1;
  }

  FILE *f = fopen(path, "rb");
  if (!f) { fprintf(stderr, "jolt: cannot open self for reading\n"); return 1; }

  if (fseek(f, 0, SEEK_END) != 0) { fclose(f); return 1; }
  long fsize = ftell(f);
  if (fsize < JOLT_TRAILER_LEN) {
    fprintf(stderr, "jolt: no boot payload (run was not produced by jolt build)\n");
    fclose(f);
    return 1;
  }

  unsigned char trailer[JOLT_TRAILER_LEN];
  if (fseek(f, fsize - JOLT_TRAILER_LEN, SEEK_SET) != 0 ||
      fread(trailer, 1, JOLT_TRAILER_LEN, f) != JOLT_TRAILER_LEN) {
    fclose(f);
    return 1;
  }
  if (memcmp(trailer + 8, JOLT_MAGIC, JOLT_MAGIC_LEN) != 0) {
    fprintf(stderr, "jolt: boot payload not found\n");
    fclose(f);
    return 1;
  }

  uint64_t boot_len = 0;
  for (int i = 0; i < 8; i++)
    boot_len |= ((uint64_t)trailer[i]) << (8 * i);

  long boot_off = fsize - JOLT_TRAILER_LEN - (long)boot_len;
  if (boot_off < 0) {
    fprintf(stderr, "jolt: corrupt boot payload\n");
    fclose(f);
    return 1;
  }

  /* The kernel keeps the boot bytes for the life of the process (demand-loaded),
   * so this buffer is freed only after Sscheme_deinit. */
  void *boot = malloc((size_t)boot_len);
  if (!boot) { fclose(f); return 1; }
  if (fseek(f, boot_off, SEEK_SET) != 0 ||
      fread(boot, 1, (size_t)boot_len, f) != (size_t)boot_len) {
    free(boot);
    fclose(f);
    return 1;
  }
  fclose(f);

  Sscheme_init(0);
  Sregister_boot_file_bytes("jolt", boot, (iptr)boot_len);
  Sbuild_heap(0, 0);
  int status = Sscheme_start(argc, (const char **)argv);
  Sscheme_deinit();
  free(boot);
  return status;
}

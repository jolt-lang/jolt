/* launcher.c — the native stub for self-contained jolt binaries (jolt-eaj).
 *
 * A toolchain-free `jolt build` (and jolt itself) produces an executable by
 * appending a Chez boot image to a copy of this prebuilt stub, framed as:
 *
 *     [stub bytes][boot bytes][boot-length : le u64]
 *                             [unpacked-length : le u64]["JOLTBOO2"]
 *
 * (see host/chez/java/io.ss jolt-append-payload!). At startup the stub locates
 * its own executable and reads the trailing 24-byte frame to find the boot.
 *
 * The boot is normally LZ4-framed, because what a first run costs is bytes read
 * off storage and packing removes ~40% of them. Then unpacked-length is nonzero,
 * this streams the frame off its own fd and unpacks it into a buffer that is
 * freed the moment Sbuild_heap returns — Chez copies what it needs into the
 * Scheme heap and frees its boot descriptors before returning, so the process
 * keeps no resident copy. That distinction is the whole point: an EARLIER
 * version of this stub kept a malloc'd payload alive for the life of the
 * process, 7-14 MB per app, which is what registering an fd region fixed.
 *
 * An unpacked-length of 0 means the boot was stored verbatim (bld-pack-boot!
 * reports that when packing did not pay). Then the original path runs unchanged:
 * the boot is registered as a region of the executable itself, and the Chez
 * kernel reads it through the fd during Sbuild_heap and closes it when done.
 * Either way: no external boot file and no Chez install.
 *
 * Built once at jolt-build time against the Chez kernel (libkernel.a + scheme.h)
 * by host/chez/build-jolt.ss; the resulting binary is embedded into jolt and
 * copied per app build. Inherently per-platform (the boot targets the host
 * machine-type), like a native compiler.
 */
#include "scheme.h"
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#if defined(__APPLE__)
#include <mach-o/dyld.h>
#include <fcntl.h>
static int self_path(char *buf, uint32_t size) {
  /* _NSGetExecutablePath fills buf and reports the needed size on overflow. */
  return _NSGetExecutablePath(buf, &size);
}
static int open_self(const char *path) { return open(path, O_RDONLY); }
#elif defined(_WIN32)
#include <windows.h>
#include <io.h>
#include <fcntl.h>
static int self_path(char *buf, uint32_t size) {
  DWORD n = GetModuleFileNameA(NULL, buf, size);
  return (n == 0 || n >= size) ? -1 : 0;
}
/* A CRT fd in binary mode — the kernel reads the region with CRT reads. */
static int open_self(const char *path) { return _open(path, _O_RDONLY | _O_BINARY); }
#else
#include <unistd.h>
#include <fcntl.h>
static int self_path(char *buf, uint32_t size) {
  ssize_t n = readlink("/proc/self/exe", buf, (size_t)size - 1);
  if (n < 0) return -1;
  buf[n] = '\0';
  return 0;
}
static int open_self(const char *path) { return open(path, O_RDONLY); }
#endif

/* Best-effort readahead of the boot region. The Chez kernel reads the boot
   through this fd during Sbuild_heap; on a cold page cache those reads block one
   after another, and nothing has told the kernel that the whole multi-MB region
   is about to be read in order. Issued before Sscheme_init so the I/O overlaps
   kernel init and the runtime image's top levels. Advisory: the result is not
   checked and a platform without an equivalent simply keeps the old timing.
   (The C-array boot sites use madvise instead — see bld-boot-prefetch-defn in
   host/chez/build.ss.) */
static void prefetch_boot_region(int fd, long off, uint64_t len) {
#if defined(__linux__)
  posix_fadvise(fd, (off_t)off, (off_t)len, POSIX_FADV_WILLNEED);
#elif defined(__APPLE__)
  /* Darwin has no posix_fadvise; F_RDADVISE is the read-ahead request, and its
     count is an int, so a boot larger than INT_MAX prefetches its first 2GB. */
  struct radvisory ra;
  ra.ra_offset = (off_t)off;
  ra.ra_count = (int)(len > (uint64_t)INT_MAX ? (uint64_t)INT_MAX : len);
  fcntl(fd, F_RDADVISE, &ra);
#else
  (void)fd;
  (void)off;
  (void)len;
#endif
}

#define JOLT_MAGIC "JOLTBOO2"
#define JOLT_MAGIC_LEN 8
#define JOLT_TRAILER_LEN 24 /* u64 payload length + u64 unpacked length + magic */
#define JOLT_UNPACK_CHUNK (256 * 1024)

#if defined(_WIN32)
#define jolt_read _read
#define jolt_lseek _lseek
#else
#define jolt_read read
#define jolt_lseek lseek
#endif

/* The LZ4 frame API, declared by hand: liblz4 is linked in already (the Chez
   kernel uses it for compressed ports) but lz4frame.h is not bundled, and the
   machine relinking this stub is not required to have one. */
typedef struct LZ4F_dctx_s LZ4F_dctx;
extern unsigned LZ4F_isError(size_t);
extern const char *LZ4F_getErrorName(size_t);
extern size_t LZ4F_createDecompressionContext(LZ4F_dctx **, unsigned);
extern size_t LZ4F_freeDecompressionContext(LZ4F_dctx *);
extern size_t LZ4F_decompress(LZ4F_dctx *, void *, size_t *, const void *,
                              size_t *, const void *);

/* Stream the LZ4 frame at [off, off+packed_len) out of FD and unpack it into a
   fresh buffer of RAW_LEN bytes, which the caller owns. Reading in chunks rather
   than mapping the whole payload keeps the peak at the unpacked image plus one
   256KB window, and makes the read explicitly sequential. NULL on any failure,
   with the reason already reported. */
static unsigned char *unpack_boot(int fd, long off, uint64_t packed_len,
                                  uint64_t raw_len) {
  LZ4F_dctx *dctx = NULL;
  unsigned char *raw = NULL;
  unsigned char *window = NULL;
  size_t dp = 0;
  uint64_t left = packed_len;
  const char *why = NULL;

  if (LZ4F_isError(LZ4F_createDecompressionContext(&dctx, 100))) {
    fprintf(stderr, "jolt: cannot start LZ4 decompression\n");
    return NULL;
  }
  raw = (unsigned char *)malloc((size_t)raw_len);
  window = (unsigned char *)malloc(JOLT_UNPACK_CHUNK);
  if (raw == NULL || window == NULL) { why = "out of memory"; goto done; }
  if (jolt_lseek(fd, off, SEEK_SET) < 0) { why = "cannot seek to the boot"; goto done; }

  while (left > 0 && dp < (size_t)raw_len) {
    size_t want = left < (uint64_t)JOLT_UNPACK_CHUNK ? (size_t)left
                                                     : (size_t)JOLT_UNPACK_CHUNK;
    long got = (long)jolt_read(fd, window, (unsigned int)want);
    size_t sp = 0;
    if (got <= 0) { why = "boot payload is truncated"; goto done; }
    left -= (uint64_t)got;
    /* A Chez compressed port emits one frame per 256KB of input, so the payload
       is a SEQUENCE of frames: a zero return means the current frame ended, not
       that the image did, and dctx is reusable for the next one. Only a lack of
       progress ends the loop. */
    while (sp < (size_t)got && dp < (size_t)raw_len) {
      size_t dn = (size_t)raw_len - dp, sn = (size_t)got - sp;
      size_t r = LZ4F_decompress(dctx, raw + dp, &dn, window + sp, &sn, NULL);
      if (LZ4F_isError(r)) { why = LZ4F_getErrorName(r); goto done; }
      if (dn == 0 && sn == 0) break; /* no progress: the frame is truncated */
      dp += dn;
      sp += sn;
    }
  }
  if (dp != (size_t)raw_len) why = "boot image is truncated";

done:
  LZ4F_freeDecompressionContext(dctx);
  free(window);
  if (why != NULL) {
    fprintf(stderr, "jolt: cannot unpack the boot image (%s)\n", why);
    free(raw);
    return NULL;
  }
  return raw;
}
static double monotonic_ms(void) {
#if defined(_WIN32)
  LARGE_INTEGER frequency;
  LARGE_INTEGER counter;
  QueryPerformanceFrequency(&frequency);
  QueryPerformanceCounter(&counter);
  return (double)counter.QuadPart * 1000.0 / (double)frequency.QuadPart;
#else
  struct timespec now;
  clock_gettime(CLOCK_MONOTONIC, &now);
  return (double)now.tv_sec * 1000.0 + (double)now.tv_nsec / 1000000.0;
#endif
}

static void startup_profile_mark(int enabled, double started, double *last,
                                 const char *label) {
  if (enabled) {
    double now = monotonic_ms();
    fprintf(stderr,
            "jolt startup: [profile] native %-22s %9.3f ms"
            "   (cumulative %9.3f ms)\n",
            label, now - *last, now - started);
    *last = now;
  }
}


int main(int argc, char *argv[]) {
  int startup_profile = getenv("JOLT_STARTUP_PROFILE") != NULL;
  double startup_started = startup_profile ? monotonic_ms() : 0.0;
  double startup_last = startup_started;
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
  if (memcmp(trailer + 16, JOLT_MAGIC, JOLT_MAGIC_LEN) != 0) {
    fprintf(stderr, "jolt: boot payload not found\n");
    fclose(f);
    return 1;
  }

  uint64_t boot_len = 0, boot_raw_len = 0;
  for (int i = 0; i < 8; i++) {
    boot_len |= ((uint64_t)trailer[i]) << (8 * i);
    boot_raw_len |= ((uint64_t)trailer[8 + i]) << (8 * i);
  }

  long boot_off = fsize - JOLT_TRAILER_LEN - (long)boot_len;
  if (boot_off < 0) {
    fprintf(stderr, "jolt: corrupt boot payload\n");
    fclose(f);
    return 1;
  }
  fclose(f);
  startup_profile_mark(startup_profile, startup_started, &startup_last,
                       "locate boot payload");

  int fd = open_self(path);
  if (fd < 0) {
    fprintf(stderr, "jolt: cannot reopen self for boot\n");
    return 1;
  }

  prefetch_boot_region(fd, boot_off, boot_len);
  startup_profile_mark(startup_profile, startup_started, &startup_last,
                       "prefetch boot payload");

  /* A packed boot is unpacked here, before Sscheme_init, and the buffer is
     released as soon as Sbuild_heap has read it. A verbatim one (boot_raw_len 0)
     keeps the zero-copy path: the kernel reads it through the fd. */
  unsigned char *boot_bytes = NULL;
  if (boot_raw_len != 0) {
    if ((boot_bytes = unpack_boot(fd, boot_off, boot_len, boot_raw_len)) == NULL)
      return 1;
    startup_profile_mark(startup_profile, startup_started, &startup_last,
                         "unpack boot payload");
  }

  Sscheme_init(0);
  startup_profile_mark(startup_profile, startup_started, &startup_last,
                       "Sscheme_init");
  if (boot_bytes != NULL) {
    close(fd);
    Sregister_boot_file_bytes("jolt", boot_bytes, (iptr)boot_raw_len);
  } else {
    /* final arg: close the fd when the boot is consumed */
    Sregister_boot_file_fd_region("jolt", fd, (iptr)boot_off, (iptr)boot_len, 1);
  }
  startup_profile_mark(startup_profile, startup_started, &startup_last,
                       "register boot payload");
  Sbuild_heap(0, 0);
  free(boot_bytes);
  startup_profile_mark(startup_profile, startup_started, &startup_last,
                       "Sbuild_heap");
  int status = Sscheme_start(argc, (const char **)argv);
  startup_profile_mark(startup_profile, startup_started, &startup_last,
                       "Sscheme_start");
  Sscheme_deinit();
  startup_profile_mark(startup_profile, startup_started, &startup_last,
                       "Sscheme_deinit");
  return status;
}

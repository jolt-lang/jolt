/* driver-relay: a jolt library's sun.misc.Signal handler and its host's (#1679).
 *
 * The host installs its own SIGUSR2 handler, loads the library, and has it
 * register a USR2 handler. The kernel's signal relay (chez-patches/0008) puts
 * a C handler in front that chains to the host's, so one raise runs both.
 * jolt_library_shutdown must give SIGUSR2 back to the host's handler, and a
 * second init must be able to take it again, on a fresh relay.
 *
 * Prints, and exits 0:
 *   before load: host
 *   after load: host
 *   1 watching: relay
 *   1 seen: 1 host: 1
 *   1 after shutdown: host
 *   2 watching: relay
 *   2 seen: 1 host: 1
 *   2 after shutdown: host
 * alarm(30) turns a hang into a SIGALRM death.
 */
#include <dlfcn.h>
#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

typedef int (*init_fn)(int, char **);
typedef int (*int_fn)(void);
typedef void (*void_fn)(void);

static volatile sig_atomic_t host_ran;
static void host_usr2(int sig) { (void)sig; host_ran += 1; }

static const char *disposition(void) {
  struct sigaction a;
  sigaction(SIGUSR2, NULL, &a);
  if (a.sa_flags & SA_SIGINFO) return "relay";
  if (a.sa_handler == host_usr2) return "host";
  if (a.sa_handler == SIG_DFL) return "dfl";
  if (a.sa_handler == SIG_IGN) return "ign";
  return "relay";
}

int main(int argc, char **argv) {
  if (argc < 2) { fprintf(stderr, "usage: driver-relay <libpath>\n"); return 2; }
  alarm(30);
  struct sigaction act;
  memset(&act, 0, sizeof act);
  sigemptyset(&act.sa_mask);
  act.sa_handler = host_usr2;
  sigaction(SIGUSR2, &act, NULL);
  printf("before load: %s\n", disposition());
  void *h = dlopen(argv[1], RTLD_NOW | RTLD_LOCAL);
  if (!h) { fprintf(stderr, "dlopen failed: %s\n", dlerror()); return 1; }
  init_fn init = (init_fn)dlsym(h, "jolt_library_init");
  void *(*lookup)(const char *) = (void *(*)(const char *))dlsym(h, "jolt_lookup");
  void_fn release = (void_fn)dlsym(h, "jolt_library_release_thread");
  void_fn shutdown = (void_fn)dlsym(h, "jolt_library_shutdown");
  if (!init || !lookup || !release || !shutdown) {
    fprintf(stderr, "missing init/lookup/release/shutdown\n"); return 1;
  }
  for (int round = 1; round <= 2; round += 1) {
    if (init(0, 0) != 0) { fprintf(stderr, "jolt_library_init failed\n"); return 1; }
    release();
    if (round == 1) printf("after load: %s\n", disposition());
    int_fn watch = (int_fn)lookup("watch_usr2");
    int_fn seen = (int_fn)lookup("usr2_seen");
    if (!watch || !seen) { fprintf(stderr, "missing watch_usr2/usr2_seen\n"); return 1; }
    if (watch() != 1) { fprintf(stderr, "watch_usr2 failed\n"); return 1; }
    printf("%d watching: %s\n", round, disposition());
    host_ran = 0;
    raise(SIGUSR2);
    int n = 0;
    for (int i = 0; i < 300 && (n = seen()) < 1; i += 1) usleep(10000);
    printf("%d seen: %d host: %d\n", round, n, (int)host_ran);
    shutdown();
    printf("%d after shutdown: %s\n", round, disposition());
    fflush(stdout);
  }
  return 0;
}

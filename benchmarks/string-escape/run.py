"""Run bench.clj in three fresh processes per runtime and write the samples to stdout."""
import csv
import statistics
import subprocess
import sys
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent
RUNTIMES = [
    ("Jolt", ["jolt"]),
    ("Babashka", ["bb"]),
    ("JVM", ["clojure", "-Srepro", "-M"]),
]


def measure(name, command):
    start = time.perf_counter_ns()
    samples = {}
    with subprocess.Popen(command + ["bench.clj"], cwd=HERE, stdout=subprocess.PIPE,
                          text=True) as process:
        if process.stdout.readline().rstrip() != "ready":
            raise RuntimeError(f"{name} failed to load the benchmark")
        startup_ms = (time.perf_counter_ns() - start) / 1e6
        for line in process.stdout:
            profile, checksum, elapsed = line.split()
            samples.setdefault((profile, int(checksum)), []).append(int(elapsed))
        if process.wait() or len(samples) != 6 or any(len(v) != 7 for v in samples.values()):
            raise RuntimeError(f"{name} returned incomplete results")
    return startup_ms, samples


def main():
    writer = csv.writer(sys.stdout, lineterminator="\n")
    writer.writerow(["runtime", "launch", "profile", "checksum", "startup_ms", "exec_median_ns",
                     *[f"exec_{i}_ns" for i in range(1, 8)]])
    expected = None
    for i in range(3):
        for name, command in RUNTIMES[i:] + RUNTIMES[:i]:
            startup_ms, samples = measure(name, command)
            if expected is None:
                expected = set(samples)
            elif expected != set(samples):
                raise RuntimeError(f"{name} disagreed on a profile or checksum")
            for (profile, checksum), times in samples.items():
                writer.writerow([name, i + 1, profile, checksum, round(startup_ms, 3),
                                 statistics.median(times), *times])
            sys.stdout.flush()


if __name__ == "__main__":
    main()

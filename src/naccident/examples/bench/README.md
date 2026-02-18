# Running Benchmarks for SNAP

Benchmarks should always be done before and after performance tuning. Improvements should work in both cases below.

Download meteorology like

```
wget https://thredds.met.no/thredds/fileServer/metusers/heikok/snap_testdata/bench_meteo20260206_00.nc -O meteo20260206_00.nc
```

## Running then the particle benchmark:

This benchmark reflects a case with many particles, e.g. for a long-running experiment.


```
time OMP_NUM_THREADS=1 ../../bsnap_naccident snap_particles.input
```
runs about 30s on a laptop with SSD. snap.log gives the following metrics:
```
timer: Total: time_loop:             0:00:35.615 (wall)   0:00:35.614 (cpu)
timer: Total: output/accumulation:   0:00:03.346 (wall)   0:00:03.346 (cpu)
timer: Total: Reading MET input:     0:00:07.352 (wall)   0:00:07.351 (cpu)
timer: Total: Calc MET input:        0:00:01.289 (wall)   0:00:01.289 (cpu)
timer: Total: release prep:          0:00:00.508 (wall)   0:00:00.508 (cpu)
timer: Total: Others:                0:00:00.000 (wall)   0:00:00.000 (cpu)
timer: Total: Particle loop:         0:00:26.505 (wall)   0:00:26.504 (cpu)
```
i.e. 26s are used for the particles, 7s for intput and 3s for output.


## Running the IO benchmark

This benchmark reflects a case with few particles, e.g. for operational run during the first 1-2 days.


Running
```
time OMP_NUM_THREADS=1 ../../bsnap_naccident snap_io.input
```
Gives output in snap.log like:
```
timer: Total: time_loop:             0:00:17.842 (wall)   0:00:17.841 (cpu)
timer: Total: output/accumulation:   0:00:01.359 (wall)   0:00:01.359 (cpu)
timer: Total: Reading MET input:     0:00:12.656 (wall)   0:00:12.655 (cpu)
timer: Total: Calc MET input:        0:00:02.508 (wall)   0:00:02.508 (cpu)
timer: Total: release prep:          0:00:00.025 (wall)   0:00:00.025 (cpu)
timer: Total: Others:                0:00:00.000 (wall)   0:00:00.000 (cpu)
timer: Total: Particle loop:         0:00:05.028 (wall)   0:00:05.028 (cpu)
```
this is 12s for io and 5s for the particles. It should be noted that the user-timer from time gives 21s i.e. 4s extra than the total time_loop.

## Useful Analyzation tools

### perf

CPU-usage per function:
```
time OMP_NUM_THREADS=1 perf record ../../bsnap_naccident snap_particles.input
perf report
```

The follwing will give cache misses:
```
time OMP_NUM_THREADS=1 perf stat -e cycles,instructions,cache-references,cache-misses   -e mem_load_retired.l3_miss,mem_load_retired.l3_hit  ../../bsnap_naccident snap_particles.input
```

SNAP has around 55% of cache-misses, which is caused by non-linear access in `posint` and `forward`, making those mostly memory bound.
(sorting of particles to x/y/z does not seem to help.)

## Parallelization

SNAP scales ok to up to 4 CPU on a laptop. Ensure that hyperthreads are not used with OMP_PLACES=cores.
Thread affinity doesn't help (OMP_PROC_BIND=close)

Test with:
```
for t in 1 2 4 8; do
    echo "=== Threads: $t ===";
    time OMP_PLACES=cores OMP_NUM_THREADS=$t ../../bsnap_naccident snap_particles.input 2> /dev/null;
done
```
which gives something like:
```
=== Threads: 1 ===

real    0m37.820s
user    0m36.671s
sys     0m1.147s
=== Threads: 2 ===

real    0m28.703s
user    0m43.855s
sys     0m1.133s
=== Threads: 4 ===

real    0m25.620s
user    0m56.317s
sys     0m1.234s
=== Threads: 8 ===

real    0m25.333s
user    1m23.136s
sys     0m1.208s
```

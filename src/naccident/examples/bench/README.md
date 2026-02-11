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
time OMP_NUM_THREADS=1 ../../bsnap_na
ccident snap_io.input
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


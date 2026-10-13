# Status codes of snapRemoteRunner / snapJob.py

Files are stored on production machine and then copied to the remote machine.

- status-filename: IDENT_MODEL_status
- containing a single line like:
  ```status_number:timestamp:text```

with status_number/text combinations like:
|code| message|type|note|
|--- | --- | --- | --- |
|100 | Getting ARGOS data from server | Processing |
|101 | queued {model} for processing | Processing  | queued |
|102 | Starting run for {model} (timeout: 2h) | Processing | running |
|202 | Finished extracting {model} data for ARGOS | Success | only code ARGOS reacts upon, all others are send to user |
|409 | {model} output data do not exist | Input Error | customer responsibility |
|409 | {model} output data does not exist, snap4rimsterm failed |  Input Error | customers responsibility |
|409 | unknown input model {model} | Input Error | customer responsibility |
|410 | {model} internal error copying data to destination | Internal Error |
|410 | {model} internal error, zip failed | Internal Error |
|410 | {model} internal error, ncatted failed | Internal Error |
|500 | internal error, cannot start job in queue in dir '{rundir}'| Internal Error |
|500 | internal error in status tag | Internal Error |

status codes from status_exporter (alert-manager):
|code| explanation |
|--- | --- |
|501 | input directory missing |
|502 | upload directory missing |
|503 | files hanging in upload (>10 min) |
|504 | strange files in upload |
|505 | uploaded (100), but not queued/started after 10 min |
|506 | queued (101), but not started after 10 min |
|507 | started (102), but no change after 2 hours |
|508 | `*_status` file not readable or malformatted or with unknown status_code |




# snapCombine

Combine several snap.nc output files (e.g. releases from different sites) into one file:

```
snapCombine -o combined_snap.nc snap1.nc snap2.nc snap3.nc ... [--recalc-toa] [-grib TAGNAME]
```

or as module: `python3 -m Snappy.SnapCombine ...` / `Snappy.SnapCombine.snap_combine(infiles, outfile, grib_tag)`.

- All files are put on a common time axis. Time-steps may be extrapolated (0,3,6 + 3,6,9 -> 0,3,6,9),
  but interpolation is an error (0,3,6 + 1,4,7).
- Variables with `_concentration` or `_deposition` are summed. Outside a file's time range they are 0,
  except accumulated (`_acc_`) fields after the end of a file, which keep their last value;
  accumulated depositions are decayed using the decay rate from `isotope_list.txt`.
- Other time-dependent variables are taken from any file with that time step;
  `time_of_arrival` and `_acc_total_deposition` are dropped; use `--recalc-toa` to recalculate
  them (as `snapAddToa`).
- `-grib TAGNAME` converts the result to grib files `TAGNAME_*` in the output directory.

## Caveats

- the tool might write some warnings about lustre-dirs and mapp-services.sh, ignore
- only for EC-files / lat-lon for now, MEPS not tested
- grids need to be identical, i.e. different global runs won't work if they are further than 5° apart
- incident-files are not included, i.e. ARGOS can't show the NPPs on the map


## TODO

- check grib conversion (filenames...)
- zip grib-files
- add status file
- write upload to dsa command as help (don't do it)

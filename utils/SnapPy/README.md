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
|410 | {model} internal error copying data to destination | Internal Error |
|410 | {model} internal error, zip failed | Internal Error |
|410 | {model} internal error, ncatted failed | Internal Error |
|500 | internal error, cannot start job in queue in dir '{rundir}'| Internal Error |

status codes from status_exporter (alert-manager):
|code| explanation |
|--- | --- |
|501 | input directory missing | 
|502 | upload directory missing |
|503 | files hanging in upload (>10in) | 
|504 | strange files in upload |
|505 | uploaded (100), but not queued/started after 10 min |
|506 | queued (101), but not started after 10 min |
|507 | started (102), but no change after 2 hours |
|508 | `*_status` file not readable or malformatted or with unknown status_code |




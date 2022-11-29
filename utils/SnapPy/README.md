# Status codes of snapRemoteRunner / snapJob.py

Files are stored on production machine and then copied to the remote machine.

- status-filename: IDENT_MODEL_status
- containing a single line like:
  ```status_number:timestamp:text```

with status_number/text combinations like:
|code| message|type|
|--- | --- | --- |
|100 | Getting ARGOS data from server | Processing |
|101 | running {model} | Processing |
|202 | Finished extracting {model} data for ARGOS | Success |
|409 | {model} output data do not exist | Error, customers responsibility |
|409 | {model} output data does not exist, snap4rimsterm failed |  Error, customers responsibility |
|410 | {model} internal error copying data to destination | Internal Error |
|410 | {model} internal error, zip failed | Internal Error |
|410 | {model} internal error, ncatted failed | Internal Error |
|500 | internal error, cannot start job in queue in dir '{rundir}'| Internal Error |


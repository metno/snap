
/* version.c */

/* Keeps track of ODB-version */

#include "alloc.h"
#include "magicwords.h"

#define VERSION_MAJOR 31     /* A positive integer; Number(s) after possible decimal point are ignored */
#define VERSION_MINOR 2.032  /* Four digits precision (e.q. 2.068) here, please */
			     /* Please make also sure the last digit is between 1 and 9 i.e. not 0 */

const char *
codb_versions_(double *major, double *minor)
{
  static char *cycle_str = NULL;
  if (!cycle_str) { /* first time */
    ALLOC(cycle_str, 20);
    sprintf(cycle_str, "CY%.0fR%.4g", (double)VERSION_MAJOR, (double)VERSION_MINOR);
  }
  if (major) *major = (int)VERSION_MAJOR;
  if (minor) *minor = VERSION_MINOR;
  return (const char *)cycle_str;
}

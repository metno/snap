
/* env.c */

/* Implement Fortran-callable ec_getenv and ec_putenv,
   since not all environments have getenv & putenv,
   but Unix/C library always have them */

/* Author: Sami Saarinen, ECMWF, 15-Mar-2006 */


#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <signal.h>
#include "raise.h"

extern char **environ; /* Global Unix var */
static int numenv = 0;

void
ec_numenv_(int *n)
{ /* Returns the number of environment variables currently active */
  int j=0;
  if (environ) {
    for (; environ[j]; j++) { }
  }
  if (n) *n = j;
  numenv = j; /* Not thread-safe */
}


void
ec_numenv(int *n)
{
  ec_numenv_(n);
}


void
ec_strenv_(const int *i,
	   char *value,
	   /* Hidden arguments */
	   const int valuelen)
{ /* Returns (*i)'th environment number; 
     Note: "Fortran", not "C" range between [1..numenv] */
  int j = (i && environ) ? (*i) : 0;
  memset(value, ' ', valuelen);
  if (j >= 1 && j <= numenv) {
    char *p = environ[--j];
    if (p) {
      int len = strlen(p);
      if (valuelen < len) len = valuelen;
      memcpy(value,p,len);
    }
  }
}


void
ec_strenv(const int *i,
	  char *value,
	  /* Hidden arguments */
	  const int valuelen)
{
  ec_strenv_(i, value, valuelen);
}


void
ec_getenv_(const char *s,
	   char *value,
	   /* Hidden arguments */
	   int slen,
	   const int valuelen)
{
  char *env = NULL;
  char *p = malloc(slen+1);
  if (!p) {
    fprintf(stderr,"ec_getenv_(): Unable to allocate %d bytes of memory\n", slen+1);
    ABOR1("ec_getenv_(): Unable to allocate memory");
  }
  memcpy(p,s,slen);
  p[slen]='\0';
  memset(value, ' ', valuelen);
  env = getenv(p);
  if (env) {
    int len = strlen(env);
    if (valuelen < len) len = valuelen;
    memcpy(value,env,len);
  }
  free(p);
}


void
ec_getenv(const char *s,
	   char *value,
	   /* Hidden arguments */
	   int slen,
	   const int valuelen)
{
  ec_getenv_(s, value, slen, valuelen);
}


void
ec_putenv_(const char *s,
	   /* Hidden argument */
	   int slen)
{
  const char *x = &s[slen-1];
  /* strip trailing blanks first */
  while (slen > 0 && *x-- == ' ') { slen--; }
  /* now go ahead */
  {
    char *p = malloc(slen+1);
    if (!p) {
      fprintf(stderr,"ec_putenv_(): Unable to allocate %d bytes of memory\n", slen+1);
      ABOR1("ec_putenv_(): Unable to allocate memory");
    }
    memcpy(p,s,slen);
    p[slen]='\0';
    putenv(p);
    /* Cannot free(p); , since putenv() uses this memory area for good ;-( */
  }
}


void
ec_putenv(const char *s,
	  /* Hidden argument */
	  int slen)
{
  ec_putenv_(s,slen);
}

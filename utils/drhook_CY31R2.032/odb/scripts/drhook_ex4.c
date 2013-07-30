#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <ctype.h>

#include "drhook.h"
#include "cargs.h"

static int trig_sigfpe = 0;

double mysin(double x)
{
  DRHOOK_START(mysin);
  {
    double pi = 4*atan(1);
    double div = 180;
    static int jcnt = 0;
    if (trig_sigfpe && (++jcnt%15000 == 0)) div = 0; /* Trigger divide-by-zero  i.e. "My sin" ;-) */
    x = sin(x/div);
  }
  DRHOOK_END(0); /* mysin */
  return x;
}

double mycos(double x)
{
  DRHOOK_START(mycos);
  x = 1 - mysin(x);
  DRHOOK_END(0); /* mycos */
  return x;
}

double mysqrt(double x)
{
  DRHOOK_START(mysqrt);
  x = sqrt(x);
  DRHOOK_END(0); /* mysqrt */
  return x;
}

double sub(int j)
{
  double ans = 0;
  DRHOOK_START(sub);
  {
    int i, i1 = (j-1)*100, i2 = j*100;
    for (i=i1; i<i2; i++) {
      ans += mysin(i) + mysqrt(fabs(mycos(i)));
    }
  }
  DRHOOK_END(0); /* sub */
  return ans;
}

void chop(char *s, int slen)
{
  if (s && slen > 0) {
    char *p = s + slen - 1;
    while (p >= s && isspace(*p)) --p;
    ++p;
    if (p >= s && isspace(*p)) *p = '\0';
  }
}

int real_main(int argc, char *argv[])
{
  int rc = 0;
  DRHOOK_START(real_main);

  { /* Arglist test */
    int j, numargs;
    DRHOOK_START(ARGLIST_TEST);
    numargs = ec_NumArgs();
    fprintf(stderr,
	    "%s: no. of C-args = %d and F90-args (numargs) = %d, cmpl_iargc_() = %d)\n",
	    argv[0], argc, numargs, cmpl_iargc_());
    for (j=0; j<argc; j++) {
      const char *arg = ec_GetArgs(j);
      char farg[1024];
      int jj = j;
      cmpl_getarg_(&jj, farg, sizeof(farg));
      chop(farg,sizeof(farg));
      fprintf(stderr,
	      "argv[%d] = '%s' (%s) : getarg(%d, farg) --> farg = '%s'\n",
	      j, argv[j], arg ? arg : "<nil>", j, farg);
      if (j == 1 && strcmp(argv[1],"SIGFPE") == 0) trig_sigfpe = 1;
    }
    fprintf(stderr,"trig_sigfpe = %d\n",trig_sigfpe);
    DRHOOK_END(0); /* ARGLIST_TEST */
  }
  
  {
    int j, n = 1000;
    DRHOOK_START(LOOP_BLOCK);
    for (j=0; j<n; j++) {
      (void) sub(j);
    }
    DRHOOK_END(0); /* LOOP_BLOCK */
  }

  DRHOOK_END(0); /* real_main */
  return rc;
}

int main(int argc, char *argv[])
{
  ec_PutArgs(argc, argv); /* Pass args for use by mpl_arg_mod.F90 (mpl_getarc, mpl_iargc() etc.) */
  return real_main(argc, argv);
}

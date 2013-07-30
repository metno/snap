#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

typedef  long long int  ll_t;

static ll_t maxstack = 0;

#ifdef RS6K

#if defined(__64BIT__)
/* Assume AIX >= 5.1 with 64-bit addressing */

#include <fcntl.h>
#include <sys/procfs.h>
#include <pthread.h> /* requires xlc_r or cc_r */

static pthread_t master_tid;

ll_t
getstk_()
{
  static int fd = -9999;
  static ll_t stackbase = 0;
  ll_t stackused = 0;

  if (fd == -9999) {
    pstatus_t pstatus;
    char procfile[80];
    int pid = getpid();
    master_tid = pthread_self();
    sprintf(procfile,"/proc/%d/status",pid);
    fd = open(procfile, O_RDONLY);
    if (read(fd, &pstatus, sizeof(pstatus)) == sizeof(pstatus)) {
      stackbase = (ll_t)pstatus.pr_stkbase;
      close(fd);
      fd = 0;
    }
  }

  if (fd == 0 && stackbase > 0) {
    int tid = pthread_self();
    if (pthread_equal(tid, master_tid)) { /* the master thread */
      char addr[1];
      stackused = stackbase - (ll_t)addr;
      if (stackused > maxstack) maxstack = stackused;
    }
    else {
      stackused = 0; /* at the moment we cannot figure out 
			stack base address for slave thread(s) */
    }
  }

  return stackused;
}

#else

ll_t
getstk_() 
{ 
  extern ll_t getstackusage_();
  static ll_t init_stack = -1;
  ll_t stackused = 0;
  if (init_stack == -1) init_stack = getstackusage_();
  stackused = getstackusage_() - init_stack;
  if (stackused > maxstack) maxstack = stackused;
  return stackused;
}

#endif /* defined(__64BIT__) */

#else  /* non-RS6K */

ll_t
getstk_() 
{ 
#if defined(CRAY)
  return 0;
#else
  extern ll_t getstackusage_();
  static ll_t init_stack = -1;
  ll_t stackused = 0;
  if (init_stack == -1) init_stack = getstackusage_();
  stackused = getstackusage_() - init_stack;
  if (stackused > maxstack) maxstack = stackused;
  return stackused;
#endif
}

#endif

/* Maximum stacksize encountered */

ll_t
getmaxstk_()
{
  return maxstack;
}

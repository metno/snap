#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "getstatm.h"

typedef  long long int  ll_t;

#if defined(CRAY) && !defined(SV2)
#define getrss GETRSS
#else
#define getrss getrss_
#endif

#if defined(RS6K) || defined(SGI)
#include <sys/resource.h>
ll_t
getrss()
{
  const ll_t scaler = 1024; /* in kilobytes */
#if defined(__64BIT__)
  struct rusage64 r;
  ll_t rc = getrusage64(RUSAGE_SELF, &r);
#else
  struct rusage r;
  ll_t rc = getrusage(RUSAGE_SELF, &r);
#endif
  rc = (rc == 0) ? (ll_t) r.ru_maxrss * scaler : 0;
  return rc;
}

#else

#if defined(LINUX)
static ll_t basesize = -1;
static size_t pagesize = 4096;
ll_t getrss()
{
  struct statm sm;
  ll_t rc = 0;
  if (getstatm(&sm) == 0) {
    if (basesize < 0) { /* the very first time */
      basesize = sm.resident;
      pagesize = getpagesize();
      if (pagesize <= 0) pagesize = 4096;
    }
    rc = (sm.resident - basesize) * pagesize;
  }
  return rc;
}
#else
ll_t getrss()
{
  ll_t rc = (ll_t)((char *)sbrk(0) - (char *)0);
  return rc;
}
#endif

#endif


#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <signal.h>
#include <unistd.h>
#include "drhook.h"
#include "raise.h"

typedef  long long int  ll_t;

#define NTHRDS 32   /* ***Note: A hardcoded max number of threads !!! */
#define TIMES  0    /* 0=Normal, 1=timing locks v threads */

extern int get_thread_id_(void);   /* ***Note: Returns YOMOML-value [1..max_threads] */
extern int get_max_threads_(void);

static ll_t maxcurheap = 0;
static ll_t maxcurheapa[NTHRDS];
static ll_t maxloc = 0;
static ll_t begloc = 0;

#if defined(CRAY) && !defined(SV2)
#define getcurheap           GETCURHEAP
#define getmaxcurheap        GETMAXCURHEAP
#define getcurheap_thread    GETCURHEAP_THREAD
#define getmaxcurheap_thread GETMAXCURHEAP_THREAD
#define getmaxloc            GETMAXLOC
#define resetmaxloc          RESETMAXLOC
#else
#define getcurheap           getcurheap_
#define getmaxcurheap        getmaxcurheap_
#define getcurheap_thread    getcurheap_thread_
#define getmaxcurheap_thread getmaxcurheap_thread_
#define getmaxloc            getmaxloc_
#define resetmaxloc          resetmaxloc_
#endif

#ifdef RS6K

extern void xl__trbk_();
#define NANS_FILL ((ll_t)0x7FF7FFFF7FF7FFFF)

#if defined(__64BIT__)
/* Assume AIX >= 5.1 with 64-bit addressing */

#if defined(INTERCEPT_ALLOC)
#include <pthread.h>
pthread_mutex_t getcurheap_lock = PTHREAD_MUTEX_INITIALIZER;

static ll_t curalloc = 0;
static ll_t curalloca[NTHRDS];

static int profile_heap = -1; /* Profiling:  -1 = UNDEF, 0 = OFF, 1 = ON */

static int nans_fill = -1; /* NaNS fill:  -1 = First, 1 = ON */

static int free_error     = 0;
static int max_free_error = 10;

#define NPROFILE 9 /* byte ranges: 10**1 .. 10^9 */
static ll_t malloc_hits[NPROFILE+1]; /* +1 for ranges >= 10^9 */
static ll_t free_hits[NPROFILE+1];
static ll_t alloc_amount[NPROFILE+1];
static ll_t malloc_hits_thrd[NTHRDS][NPROFILE+1]; 
static ll_t free_hits_thrd[NTHRDS][NPROFILE+1];
static ll_t alloc_amount_thrd[NTHRDS][NPROFILE+1];

#define WORDLEN   ((ll_t)sizeof(ll_t))
#define RNDUP(i,n) (( ( (i) + (n) - 1 ) / (n) ) * (n))
#define TRUE_BYTES(x) ((x) + 3*WORDLEN) /* the size, keyptr at start & padding at end */

static void
Check_curalloc() /* Normally not called */
{
  const ll_t big = (ll_t) 1000000000000L; /* 1,000,000 million bytes */
  if (curalloc < 0 || curalloc > big) {
    fprintf(stderr,"Check_curalloc(): curalloc has gone crazy => %lld\n",curalloc);
    xl__trbk_();
    RAISE(SIGABRT);
    _exit(1);  /* Just in case, but shouldn't end up here at all */
  }
}

static void
Profile_heap_put(ll_t size, int is_malloc)
{
  if (profile_heap == -1) { /* First time */
    char *env = getenv("EC_PROFILE_HEAP");
    if (env) profile_heap = atoi(env);
    if (profile_heap != 0) profile_heap = 1; /* OFF by export EC_PROFILE_HEAP=0 */
  }
  if (profile_heap == 1) {
    int j;
    ll_t n = 1; /* initial byte range */
    ll_t *p = is_malloc ? malloc_hits : free_hits;
    for (j=0; j<NPROFILE; j++) { 
      n *= 10; /* increment byte range by 10X */
      /* BTW: Don't want log10() overhead here !! */
      if (size < n) { /* i.e. size < pow(10,j+1) */
	alloc_amount[j] += is_malloc ? size : -size;
	p[j]++;
	return;
      }
    }
    j = NPROFILE;
    alloc_amount[j] += is_malloc ? size : -size;
    p[j]++;
  } /* if (profile_heap == 1) */
}

static void
Profile_heap_put_thrd(ll_t size, int is_malloc, int it)
{
  if (profile_heap == -1) { /* First time */
    char *env = getenv("EC_PROFILE_HEAP");
    if (env) profile_heap = atoi(env);
    if (profile_heap != 0) profile_heap = 1; /* OFF by export EC_PROFILE_HEAP=0 */
  }
  if (profile_heap == 1) {
    int j;
    ll_t n = 1; /* initial byte range */
    ll_t *p = is_malloc ? malloc_hits_thrd[it] : free_hits_thrd[it];
    for (j=0; j<NPROFILE; j++) { 
      n *= 10; /* increment byte range by 10X */
      /* BTW: Don't want log10() overhead here !! */
      if (size < n) { /* i.e. size < pow(10,j+1) */
	alloc_amount_thrd[it][j] += is_malloc ? size : -size;
	p[j]++;
	return;
      }
    }
    j = NPROFILE;
    alloc_amount_thrd[it][j] += is_malloc ? size : -size;
    p[j]++;
  } /* if (profile_heap == 1) */
}

void
profile_heap_get_(ll_t val[], 
		  const int *Nval, 
		  const int *Icase,
		  int *nret)
     /* Fortran callable */
{
  int icase = *Icase;
  int nval = *Nval;
  int j, it, nt;
  if (nval < 0) nval = 0;
  if (nval > NPROFILE+1) nval = NPROFILE+1;
  nt = get_max_threads_();
  for (j=0; j<nval; j++) {
    free_hits[j] = 0;
    malloc_hits[j] = 0;
    alloc_amount[j] = 0;
  }
  for (it=0; it<nt; it++) {
    for (j=0; j<nval; j++) {
      free_hits[j] += free_hits_thrd[it][j];
      malloc_hits[j] += malloc_hits_thrd[it][j];
      alloc_amount[j] += alloc_amount_thrd[it][j];
    }
  }
  if (icase == 0) { /* free() hits */
    for (j=0; j<nval; j++) val[j] = free_hits[j];
  }
  else if (icase == 1) { /* malloc() hits */
    for (j=0; j<nval; j++) val[j] = malloc_hits[j];
  }
  else if (icase == 2) { /* outstanding allocs (malloc minus free) */
    for (j=0; j<nval; j++) val[j] = malloc_hits[j] - free_hits[j];
  }
  else if (icase == 3) { /* allocation amount left per range; in bytes */
    for (j=0; j<nval; j++) val[j] = alloc_amount[j];
  }
  else if (icase == 4) { /* average allocation chunk left per range; in bytes */
    for (j=0; j<nval; j++) {
      ll_t tmp = malloc_hits[j] - free_hits[j];
      val[j] = (tmp > 0) ? RNDUP(alloc_amount[j],tmp)/tmp : 0;
    }
  }
  else {
    nval = 0;
  }
  *nret = nval;
}

void __free(void *vptr)
{
  if (vptr) {
    ll_t *p = vptr;
    ll_t adjsize = *--p;
    ll_t keyptr = *--p;
    ll_t true_bytes;
    int it;
    if (nans_fill == 1) {
      ll_t *q = vptr;
      ll_t nans  = NANS_FILL;
      ll_t j = adjsize/WORDLEN;
      if (q[j] != nans) {
        fprintf(stderr,"WARNING: NaNS at end of array overwritten with %e\n",q[j]);
        xl__trbk_();
        free_error++;
        if (free_error > max_free_error) {
	  fprintf(stderr,"ERROR: Too many NaNS overwrites at end of arrays\n");
	  xl__trbk_(); /* Oops !! */
	  RAISE(SIGABRT);
	  _exit(1); /* Just in case, but shouldn't end up here at all */
	}
      }
    }
    it=get_thread_id_();
    true_bytes = -TRUE_BYTES(adjsize);
    if (drhook_lhook) c_drhook_memcounter_(&it, &true_bytes, &keyptr);
    free(p);
    pthread_mutex_lock(&getcurheap_lock);
    curalloca[--it] += true_bytes; /* += since true_bytes is negative */
    pthread_mutex_unlock(&getcurheap_lock);
    if (profile_heap != 0) Profile_heap_put_thrd(true_bytes, 0, it);
  }
}

void *__malloc(ll_t size)
{
  double *d = NULL;
  void *vptr = NULL;
  ll_t keyptr = 0;
  ll_t adjsize = size;
  ll_t true_bytes;
  int it;
  if (nans_fill == -1) { /* First time */
    char *env = getenv("EC_FILL_NANS");
    nans_fill=0;
    if (env) {
      if (strcmp(env,"true") == 0 || 
	  strcmp(env,"TRUE") == 0 ||
	  strcmp(env,"1"   ) == 0) nans_fill = 1;
      /* fprintf(stderr,"EC_FILL_NANS ==> env,env,nans_fill= 0x%x %s %d\n",env,env,nans_fill); */
    }
  }
  if (adjsize < 0) adjsize = 0;
  adjsize = RNDUP(adjsize,WORDLEN);
  true_bytes = TRUE_BYTES(adjsize);
  it=get_thread_id_();
  if (drhook_lhook) c_drhook_memcounter_(&it, &true_bytes, &keyptr);
  it--;
  if (TIMES == 1) {
    DRHOOK_START(dummy);
    DRHOOK_END(0);
  }
  if (TIMES == 1) {
    DRHOOK_START(malloc);
    d = (double *)malloc(true_bytes); 
    DRHOOK_END(0);
  }
  else {
    d = (double *)malloc(true_bytes);
  }
  vptr = d;
  if (vptr) {
    if (TIMES == 1) {
      {
        ll_t *p = vptr;
        extern ll_t getstk_();
        DRHOOK_START(lock);
        (void) getstk_(); /* to gather near up to date stack statistics */
	*p++ = keyptr;
        *p++ = adjsize;
        pthread_mutex_lock(&getcurheap_lock);
        curalloc += true_bytes;
        if (curalloc > maxcurheap) maxcurheap = curalloc;
        /* Check_curalloc(); */
        Profile_heap_put(true_bytes, 1);
        pthread_mutex_unlock(&getcurheap_lock);
        vptr = p;
        DRHOOK_END(0);
      }
      { 
        ll_t *p = vptr;
        extern ll_t getstk_();
        DRHOOK_START(thread);
        (void) getstk_(); /* to gather near up to date stack statistics */
        pthread_mutex_lock(&getcurheap_lock);
        curalloca[it] += true_bytes;
        pthread_mutex_unlock(&getcurheap_lock);
        if (curalloca[it] > maxcurheapa[it]) maxcurheapa[it] = curalloca[it];
        if (profile_heap != 0) Profile_heap_put_thrd(true_bytes, 1, it);
        DRHOOK_END(0);
      }
    }
    else {
      ll_t *p = vptr;
      ll_t q;
      extern ll_t getstk_();
      (void) getstk_(); /* to gather near up to date stack statistics */
      *p++ = keyptr;
      *p++ = adjsize;
      pthread_mutex_lock(&getcurheap_lock);
      curalloca[it] += true_bytes;
      pthread_mutex_unlock(&getcurheap_lock);
      if (curalloca[it] > maxcurheapa[it]) maxcurheapa[it] = curalloca[it];
      if (profile_heap != 0) Profile_heap_put_thrd(true_bytes, 1, it);
      if (nans_fill == 1) {
        int j;
        ll_t nans  = NANS_FILL;
        for (j=0; j<(WORDLEN+adjsize)/WORDLEN; j++) {
          p[j]=nans;
        }
      }
      q=(ll_t)p+true_bytes;
      if (q > maxloc) maxloc=q;
      if (begloc == 0) begloc=(ll_t)p;
/*
      fprintf(stderr,"JJJ pntr= %ld %ld %ld %ld\n",true_bytes,p,q,maxloc);
*/
      vptr=p;
    }
  }
  else {
    pthread_mutex_lock(&getcurheap_lock);
    fprintf(stderr,
	    "__malloc(size=%lld => adjsize=%lld, true_bytes=%lld bytes) failed in file=%s, line=%d\n",
	    size, adjsize, true_bytes, __FILE__, __LINE__);
    xl__trbk_(); /* Oops !! */
    RAISE(SIGABRT);
    pthread_mutex_unlock(&getcurheap_lock);
    _exit(1); /* Just in case, but shouldn't end up here at all */
  }
  return vptr;
}

void *__calloc(ll_t nelem, ll_t elsize)
{
  ll_t totbytes = nelem * elsize;
  void *p = __malloc(totbytes);
  if (p) memset(p, 0, totbytes);
  return p;
}

void *__realloc(void *vptr, ll_t size)
{
  ll_t *pnew = NULL;
  if (vptr) {
    ll_t *p = vptr;
    ll_t oldsize = p[-1];
    if (oldsize < size) {
      pnew = __malloc(size);
      if (pnew) {
	memcpy(pnew, p, oldsize);
	__free(p);
      }
    }
    else { /* the old allocation size was already sufficient */
      pnew = p;
    }
  }
  else { /* Revert to malloc() */
    pnew = __malloc(size);
  }
  return pnew;
}

char *__strdup(const char *s)
{
  ll_t totbytes = sizeof(*s) * (strlen(s) + 1);
  char *p = __malloc(totbytes);
  if (p) memcpy(p,s,totbytes);
  return p;
}

#else

void
profile_heap_get_(ll_t val[], 
		  const int *Nval, 
		  const int *Is_malloc,
		  int *nret)
{
  *nret = 0;
}

#endif /* defined(INTERCEPT_ALLOC) */

ll_t
getcurheap()
{
#if defined(INTERCEPT_ALLOC)
  ll_t curvalue = 0;
  int it = get_thread_id_();
  if (it == 1) { /* Only thread#1 sums up */
    int i;
    int nt=get_max_threads_();
    pthread_mutex_lock(&getcurheap_lock);
    for (i=0; i<nt; i++) {
      curvalue += curalloca[i];
    }
    pthread_mutex_unlock(&getcurheap_lock);
  }
  else {
    curvalue = curalloca[--it];
  }
  return curvalue;
#else
  extern ll_t gethwm_();
  ll_t rc = gethwm_();
  if (rc > maxcurheap) maxcurheap = rc;
  return rc;
#endif
}

ll_t
getcurheap_thread(const int *thread_id)
{
#if defined(INTERCEPT_ALLOC)
  int it = (thread_id && (*thread_id > 0)) ? *thread_id : get_thread_id_();
  return curalloca[--it];
#else
  return getcurheap();
#endif
}


#else /* non-defined(__64BIT__) [but still RS6K] */

ll_t
getcurheap() 
{ 
  extern ll_t gethwm_();
  ll_t rc = gethwm_();
  if (rc > maxcurheap) maxcurheap = rc;
  return rc;
}

ll_t 
getcurheap_thread(const int *thread_id)
{
  return getcurheap();
}

void
profile_heap_get_(ll_t val[], 
		  const int *Nval, 
		  const int *Is_malloc,
		  int *nret)
{
  *nret = 0;
}

#endif /* defined(__64BIT__) */

#else  /* non-RS6K */

ll_t 
getcurheap()
{
  extern ll_t gethwm_();
  ll_t rc = gethwm_();
  if (rc > maxcurheap) maxcurheap = rc;
  return rc;
}

ll_t 
getcurheap_thread(const int *thread_id)
{
  return getcurheap();
}

void
profile_heap_get_(ll_t val[], 
		  const int *Nval, 
		  const int *Is_malloc,
		  int *nret)
{
  *nret = 0;
}

#endif

/* Maximum (total) current (virtual mem) allocation encountered */

ll_t
getmaxcurheap()
{
  ll_t maxcurheap_local=0;
  int it, nt;
  nt=get_max_threads_();
  for (it=0; it<nt; it++) {
    maxcurheap_local += maxcurheapa[it];
  }
  maxcurheap = maxcurheap_local;
  return maxcurheap_local;
}

/* Maximum (total) current (virtual mem) allocation encountered per thread */

ll_t
getmaxcurheap_thread(const int *thread_id) /* ***Note: YOMOML thread id */
{
  int it = (thread_id && (*thread_id > 0)) ? *thread_id : get_thread_id_();
  return maxcurheapa[--it];
}

ll_t
getmaxloc()
{
  ll_t z=maxloc-begloc;
  return z;
}

void
resetmaxloc()
{
  maxloc=0;
}

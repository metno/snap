
#define _DRHOOK_C_   1

/* 
   drhook.c

   Author: Sami Saarinen, ECMWF, 14..24-Nov-2003

   Thanks to Bob Walkup & John Hague for IBM Power4 version
   Thanks to Bob Carruthers for Cray X1 (SV2), XD1 and XT3 versions,
   as well as David Tanqueray for the flop routines

   Also thanks to Roland Richter for suggesting the use
   of "call tracebackqq()" function.
   In our environment this is accomplished by calling fortran
   routine intel_trbk() from ifsaux/utilities/gentrbk.F90.
   This source must be compiled with -DINTEL flag, too.

*/

/*
If intending to run on IBM P4+ or newer systems the following definition
should be activated to use pm_initialize() instead of pm_init() of PMAPI-lib ($LIBHPM)
#define PMAPI_POST_P4
*/

/*
If *ALSO* intending to run on IBM P5+ systems, then set also BOTH
#define PMAPI_POST_P4
#define PMAPI_P5_PLUS
*/

#if defined(PMAPI_P5_PLUS)
#define ENTRY_4 5
#define ENTRY_6 4
#else
#define ENTRY_4 4
#define ENTRY_6 6
#endif

#if defined(SV2) || defined(XD1) || defined(XT3)
#define DT_FLOP
#define HPM
#define MAX_COUNTERS 6
#endif

#ifdef RS6K
#pragma options opt=3 halt=e
#endif

/* === This doesn't handle recursive calls correctly (yet) === */

#include "drhook.h"
#include "crc.h"
#include <time.h>

static char *start_stamp = NULL;
static char *end_stamp = NULL;
#ifdef VPP
#include <ucontext.h>
#endif

#if defined(LINUX) && !defined(XT3) && !defined(XD1) && !defined(CYGWIN)

#if defined(__GNUC__)

#define _GNU_SOURCE 1
#if defined(CYGWIN)
#include <mingw/fenv.h>
#else
#include <fenv.h>
#endif

static void trapfpe(void)
{
  /* Enable some exceptions. At startup all exceptions are masked. */
  (void) feenableexcept(FE_INVALID|FE_DIVBYZERO|FE_OVERFLOW);
}

static void untrapfpe(void)
{
  /* Disable some exceptions. At startup all exceptions are masked. */
  (void)fedisableexcept(FE_INVALID|FE_DIVBYZERO|FE_OVERFLOW);
}

#endif /* defined(__GNUC__) */

#endif /* defined(LINUX) && !defined(XT3) && !defined(XD1) */

#if (!defined(LINUX) || defined(CYGWIN)) && defined(__GNUC__)
/* For example Solaris with gcc */
#define trapfpe()
#define untrapfpe()
#endif

static int any_memstat = 0;
static int opt_gethwm = 0;
static int opt_getstk = 0;
static int opt_getrss = 0;
static int opt_getpag = 0;
static int opt_walltime = 0;
static int opt_cputime = 0;
static int opt_wallprof = 0;
static int opt_cpuprof = 0;
static int opt_hpmprof = 0;
static int opt_memprof = 0;
static int opt_trim = 0;
static int opt_calls = 0;
static int opt_self = 1; /* 0=exclude drhook altogether, 
			    1=include, but don't print, 
			    2=also print */
static int opt_propagate_signals = 1;
static int opt_sizeinfo = 1;
static int opt_clusterinfo = 0;
static int opt_callpath = 0;
#define callpath_indent_default 2
static int callpath_indent = callpath_indent_default;
#define callpath_depth_default 50
static int callpath_depth = callpath_depth_default;

static int opt_calltrace = 0;

static int opt_timeline = 0; /* myproc or -1 [or 0 for --> timeline feature off] */
static int opt_timeline_thread = 1; /* thread-id control : 
				       0 for all threads; thread-wise values, 
				       1 -> #1 only [but curheap still global] (default), 
				       n -> print for increasing number of threads separately : [1..n] */
static int opt_timeline_format = 1; /* if 1, print only {wall,hwm,rss,curheap} w/o labels "wall=" etc.; else fully expanded fmt */
static int opt_timeline_unitno = 6; /* Fortran unit number : default = 6 i.e. stdout */
static int opt_timeline_freq = 100; /* How often to print : every n-th call : default = every 100th call */
static unsigned long long int *timeline_calls = NULL;

/* HPM-specific */

static long long int opt_hpmstop_threshold = -1;
static        double opt_hpmstop_mflops    =  1000000.0; /* Yes, 1 PetaFlop/s !! */


#define DRHOOK_STRBUF 1000

#ifndef SA_SIGINFO
#define SA_SIGINFO 0
#define SIG_EXTRA_ARGS       /* empty */
#define SIG_PASS_EXTRA_ARGS  /* empty */
#else 
#define SIG_EXTRA_ARGS       , siginfo_t *sigcode, void *sigcontextptr
#define SIG_PASS_EXTRA_ARGS  , sigcode, sigcontextptr
#endif

#undef MIN
#define MIN(a,b) ( (a) < (b) ? (a) :  (b) )

#undef MAX
#define MAX(a,b) ( (a) > (b) ? (a) :  (b) )

#undef ABS
#define ABS(x)   ( (x) >= 0  ? (x) : -(x) )

#define strequ(s1,s2)     ((void *)s1 && (void *)s2 && strcmp(s1,s2) == 0)
#define strnequ(s1,s2,n)  ((void *)s1 && (void *)s2 && memcmp(s1,s2,n) == 0)

extern long long int getstk_();
extern long long int getmaxstk_();
extern long long int gethwm_();
extern long long int getrss_();
extern long long int getcurheap_();
extern long long int getmaxcurheap_();
extern long long int getcurheap_thread_(const int *tidnum);    /* *tidnum >= 1 && <= max_threads */
extern long long int getmaxcurheap_thread_(const int *tidnum); /* *tidnum >= 1 && <= max_threads */
extern long long int getpag_();

extern void ec_set_umask_();

#if defined(DT_FLOP)
extern double flop_();
#endif

extern double util_cputime_();
extern double util_walltime_();

#ifdef RS6K
static long long int irtc_start = 0;
extern long long int irtc();
#define WALLTIME() ((double)(irtc() - irtc_start)*1.0e-9)
#define CPUTIME() util_cputime_()
#else
#if defined(SV2)
#include <intrinsics.h>
#endif
#if defined(XD1) || defined(XT3)
extern long long int irtc_();             /* integer*8 irtc() */
extern long long int irtc_rate_();        /* integer*8 irtc_rate() */
#endif
#if defined(SV2) || defined(XD1) || defined(XT3)
static long long int irtc_start = 0;
static double my_irtc_rate = 0;
static double my_inv_irtc_rate = 0;
#if defined(SV2)
#define WALLTIME() ((double)(_rtc() - irtc_start)*my_inv_irtc_rate)
#else
#define WALLTIME() ((double)(irtc_() - irtc_start)*my_inv_irtc_rate)
#endif
#define CPUTIME() util_cputime_()
#else
#define WALLTIME() util_walltime_()
#define CPUTIME() util_cputime_()
#endif
#endif

/* #define RAISE(x) { int tmp = x; c_drhook_raise_(&tmp); } */
#include "raise.h"
#include "cargs.h"

extern int get_thread_id_();
extern void LinuxTraceBack(void *sigcontextptr);

/*** typedefs ***/

typedef union {
  struct drhook_key_t *keyptr;
  double d;
  unsigned long long int ull;
} equivalence_t;

typedef struct drhook_key_t {
  char *name;
  unsigned short name_len;
  const equivalence_t *callpath; /* parent's tree down to callpath_depth */
  int callpath_len;
  unsigned int callpath_fullhash;
  unsigned short status; /* 0=inactive, >1 active */
  unsigned long long int calls;
  long long int hwm, maxrss, rssnow, stack, maxstack, paging;
  double wall_in, delta_wall_all, delta_wall_child;
  double cpu_in, delta_cpu_all, delta_cpu_child;
#ifdef HPM
  unsigned char hpm_stopped, counter_stopped;
  double this_delta_wall_child;
  double avg_mipsrate, avg_mflops;
  unsigned long long int hpm_calls;
  double mip_count_in, mflop_count_in;
  long long int *counter_in, *counter_sum;
#endif
  char *filename;         /* the filename where the 1st call (on this routine-name) 
			     to dr_hook() occurred */
  long long int sizeinfo; /* # of data elements, bytes, etc. */
  /* memprof specific */
  long long int mem_seenmax;
  long long int mem_child, mem_curdelta;
  long long int maxmem_selfdelta, maxmem_alldelta;
  long long int mem_maxhwm, mem_maxrss, mem_maxstk, mem_maxpagdelta;
  long long int paging_in;
  unsigned long long int alloc_count, free_count;
  struct drhook_key_t *next;
} drhook_key_t;

typedef struct drhook_calltree_t {
  int active;
  drhook_key_t *keyptr;
  struct drhook_calltree_t *next;
  struct drhook_calltree_t *prev;
} drhook_calltree_t;

typedef struct drhook_sig_t {
  int active;
  char name[32];
  struct sigaction new;
  struct sigaction old;
  int ignore_atexit;
} drhook_sig_t;

typedef union {
  void (*func1args)(int sig);
  void (*func3args)(int sig SIG_EXTRA_ARGS);
} drhook_sigfunc_t;

typedef struct drhook_prof_t {
  double pc;
  double total;
  double self;
  unsigned long long int calls;
  double percall_ms_self;
  double percall_ms_total;
  double mipsrate, mflops, divpc;
  int index;
  int tid;
  int cluster;
  double *maxval;
  unsigned char is_max;
  char *name;
  char *filename;
  unsigned long long int sizeinfo;
  double sizespeed, sizeavg;
  const equivalence_t *callpath; /* parent's tree down to callpath_depth */
  int callpath_len;
} drhook_prof_t;

typedef struct drhook_memprof_t {
  double pc;
  long long int self;
  long long int children;
  long long int hwm, rss, stk, pag, leaked;
  unsigned long long int calls, alloc_count, free_count;
  int index;
  int tid;
  int cluster;
  long long int *maxval;
  unsigned char is_max;
  char *name;
  char *filename;
  const equivalence_t *callpath; /* parent's tree down to callpath_depth */
  int callpath_len;
} drhook_memprof_t;

#define MAX_WATCH_FIRST_NBYTES 8

typedef struct drhook_watch_t {
  char *name;
  int tid;
  int active;
  int abort_if_changed;
  const char *ptr;
  int nbytes;
  int watch_first_nbytes;
  char first_nbytes[MAX_WATCH_FIRST_NBYTES];
  unsigned int crc32;
  struct drhook_watch_t *next;
} drhook_watch_t;

/*** static (local) variables ***/

static o_lock_t DRHOOK_lock = 0;
static int numthreads = 0;
static int myproc = 1;
static int nproc = -1;
static int max_threads = 1;
static pid_t pid = -1;
static drhook_key_t      **keydata  = NULL;
static drhook_calltree_t **calltree = NULL;
static drhook_calltree_t **thiscall = NULL;
static int signals_set = 0;
static int signal_handler_called = 0;
static int signal_handler_ignore_atexit = 0;
static drhook_sig_t siglist[1+NSIG] = { 0 };
static char *a_out = NULL;
static char *mon_out = NULL;
static int mon_out_procs = -1;
static double percent_limit = -10; /* Lowest percentage accepted into the printouts */
static drhook_key_t **keyself = NULL; /* pointers to itself (per thread) */
static double *overhead; /* Total Dr.Hook-overhead for every thread in either WALL or CPU secs */
static drhook_key_t **curkeyptr = NULL; /* pointers to current keyptr (per thread) */
static drhook_watch_t *watch = NULL;
static drhook_watch_t *last_watch = NULL;
static int watch_count = 0; /* No. of *active* watch points */

#define HASHSIZE(n) ((unsigned int)1<<(n))
#define HASHMASK(n) (HASHSIZE(n)-1)

#define NHASH    15
#define NHASHMAX 24
static int nhash = NHASH;
unsigned int hashsize = HASHSIZE(NHASH);
unsigned int hashmask = HASHMASK(NHASH);

#ifdef HPM
/* HPM-specific (static) protos */

static void stopstart_hpm(int tid, drhook_key_t *pstop, drhook_key_t *pstart);
static void stop_only_hpm(int tid, drhook_key_t *pstop);
static void init_hpm(int tid);
static double mflops_hpm(const drhook_key_t *keyptr);
static double mips_hpm(const drhook_key_t *keyptr);
static double divpc_hpm(const drhook_key_t *keyptr);
static double mflop_count(const drhook_key_t *keyptr);
static double mip_count(const drhook_key_t *keyptr);

#else
/* Dummies for HPM as macros that do nothing */

#define stopstart_hpm(tid, pstop, pstart)
#define stop_only_hpm(tid, pstop)
#define init_hpm(tid)
#define mflops_hpm(keyptr)  0
#define mips_hpm(keyptr)    0
#define divpc_hpm(keyptr)   0
#define mflop_count(keyptr) 0
#define mip_count(keyptr)   0

#endif

/*--- malloc_drhook ---*/

static void *
malloc_drhook(size_t size)
{
  size_t size1 = MAX(1,size);
  void *p = malloc(size1);
  if (!p) {
    fprintf(stderr,"***Error in malloc_drhook(): Unable to allocate space for %d bytes\n", size1);
    RAISE(SIGABRT);
  }
  return p;
}

/*--- calloc_drhook ---*/

static void *
calloc_drhook(size_t nmemb, size_t size)
{
  size_t n = nmemb * size;
  void *p = malloc_drhook(n);
  memset(p,0,n);
  return p;
}

/*--- free_drhook ---*/

#define free_drhook(x) { if (x) { free(x); x = NULL; } }

/*--- strdup_drhook ---*/

static char *
strdup_drhook(const char *s)
{
  int n = strlen(s);
  char *p = malloc_drhook(n+1);
  memcpy(p,s,n);
  p[n] = 0;
  return p;
}

/*--- strdup2_drhook ---*/

static char *
strdup2_drhook(const char *s, int s_len)
{
  int n = s_len;
  char *p = malloc_drhook(n+1);
  memcpy(p,s,n);
  p[n] = 0;
  return p;
}

/*--- timestamp ---*/

static char *
timestamp()
{
  time_t tp;
  const int bufsize = 80;
  char *buf = malloc_drhook(bufsize+1);
  time(&tp);
  strftime(buf, bufsize, "%Y%m%d %H%M%S", localtime(&tp));
  return buf;
}

/*--- hashfunc ---*/

unsigned int
hashfunc(const char *s, int s_len)
{
  unsigned int hashval;
  if (opt_trim) {
    for (hashval = 0; s_len>0 ; s++, s_len--) {
      unsigned char c = islower(*s) ? toupper(*s) : *s;
      hashval = (hashval<<4)^(hashval>>28)^(c);
    }
  }
  else {
    for (hashval = s_len; s_len>0 ; s_len--) {
      hashval = (hashval<<4)^(hashval>>28)^(*s++);
    }
  }
  hashval = (hashval ^ (hashval>>10) ^ (hashval>>20)) & hashmask;
  return hashval;
}

/*--- callpath_hashfunc ---*/

unsigned int
callpath_hashfunc(unsigned int inithash, /* from hashfunc() */
		  const equivalence_t *callpath, int callpath_len,
		  unsigned int *fullhash)
{
  unsigned int hashval;
  for (hashval = inithash; callpath_len>0 ; callpath++, callpath_len--) {
    hashval = (hashval<<4)^(hashval>>28)^(callpath->ull);
  }
  if (fullhash) *fullhash = hashval;
  hashval = (hashval ^ (hashval>>10) ^ (hashval>>20)) & hashmask;
  return hashval;
}

/*--- insert_calltree ---*/

static void
insert_calltree(int tid, drhook_key_t *keyptr)
{
  if (tid >= 1 && tid <= numthreads) {
    drhook_calltree_t *treeptr = thiscall[--tid];
    while (treeptr->active) {
      if (!treeptr->next) {
	treeptr->next = calloc_drhook(1,sizeof(drhook_calltree_t));
	treeptr->next->prev = treeptr;
      }
      treeptr = treeptr->next;
    }
    treeptr->keyptr = keyptr;
    treeptr->active = 1;
    thiscall[tid] = treeptr;
#ifdef HPM
    if (opt_hpmprof) {
      drhook_key_t *kptr = treeptr->keyptr;
      if (!kptr->hpm_stopped) {
	stopstart_hpm(tid+1,
		      treeptr->prev ? treeptr->prev->keyptr : NULL, /* stop current (i.e. my parent) */
		      kptr);                             /* start to gather for me */
	kptr->this_delta_wall_child = 0;
	kptr->mip_count_in = mip_count(kptr);
	kptr->mflop_count_in = mflop_count(kptr);
#ifdef DEBUG
	fprintf(stderr,"insert[%.*s@%d]: this_delta_wall_child=%.15g, mip#%.15g, mflop#%.15g\n",
		kptr->name_len,kptr->name,
		tid+1,kptr->this_delta_wall_child,
		kptr->mip_count_in,kptr->mflop_count_in);
#endif
      }
      else {
	stop_only_hpm(tid+1,
		      treeptr->prev ? treeptr->prev->keyptr : NULL /* stop current (i.e. my parent) */);
      } /* if (!kptr->hpm_stopped) else */
    } /* if (opt_hpmprof) */
#endif
  }
}

/*--- remove_calltree ---*/

static void 
remove_calltree(int tid, drhook_key_t *keyptr, 
		const double *delta_wall, const double *delta_cpu)
{
  if (tid >= 1 && tid <= numthreads) {
    drhook_calltree_t *treeptr = thiscall[--tid];
    if (treeptr->active && treeptr->keyptr == keyptr) {
      treeptr->active = 0;
      if (treeptr->prev) {
	drhook_key_t *parent_keyptr = treeptr->prev->keyptr;
	if (parent_keyptr) { /* extra security */
	  if (opt_walltime) {
	    parent_keyptr->delta_wall_child += (*delta_wall);
#ifdef HPM
	    if (opt_hpmprof) parent_keyptr->this_delta_wall_child += (*delta_wall);
#endif
	  }
	  if (opt_cputime)  {
	    parent_keyptr->delta_cpu_child  += (*delta_cpu);
	  }
	  if (opt_memprof) {
	    /*
	    int tmp_tid = tid+1;
	    const long long int size = 0;
	    c_drhook_memcounter_(&tmp_tid, &size, NULL);
	    fprintf(stderr,
		    ">parent(%.*s)->mem_child = %lld ; this(%.*s)->alldelta = %lld, mem_child = %lld\n",
		    parent_keyptr->name_len, parent_keyptr->name, parent_keyptr->mem_child,
		    keyptr->name_len, keyptr->name, keyptr->maxmem_alldelta, keyptr->mem_child);
	    */
	    parent_keyptr->mem_child = MAX(parent_keyptr->mem_child, keyptr->maxmem_alldelta);
	    /*
	    fprintf(stderr,
		    "<parent(%.*s)->mem_child = %lld ; this(%.*s)->alldelta = %lld, mem_child = %lld\n",
		    parent_keyptr->name_len, parent_keyptr->name, parent_keyptr->mem_child,
		    keyptr->name_len, keyptr->name, keyptr->maxmem_alldelta, keyptr->mem_child);
	    */
	  }
	} /* if (parent_keyptr) */
	thiscall[tid] = treeptr->prev;
      }
      else {

	thiscall[tid] = calltree[tid];
      }
#ifdef HPM
      if (opt_hpmprof) {
	drhook_key_t *kptr = treeptr->keyptr;
	if (!kptr->hpm_stopped) {
	  double this_delta_wall_self = *delta_wall - kptr->this_delta_wall_child;
	  stopstart_hpm(tid+1, 
			kptr, 
			thiscall[tid]->keyptr); /* stop current, (re-)start previous */
	  /* Calculate moving average of mipsrate & mflops ; divpc we don't bother */
#ifdef DEBUG
	  fprintf(stderr,"remove[%.*s@%d]: this_delta_wall_self=%.15g i.e. %.15g - %.15g",
		  kptr->name_len,kptr->name,
		  tid+1,this_delta_wall_self,
		  *delta_wall,kptr->this_delta_wall_child);
#endif
	  if (this_delta_wall_self > 0) {
	    long long int hpm_calls = ++kptr->hpm_calls;
	    double mipsrate, mflops;
	    kptr->mip_count_in = mip_count(kptr) - kptr->mip_count_in;
	    kptr->mflop_count_in = mflop_count(kptr) - kptr->mflop_count_in;
	    mipsrate = kptr->mip_count_in/this_delta_wall_self;
	    kptr->avg_mipsrate = ((hpm_calls-1)*kptr->avg_mipsrate + mipsrate)/hpm_calls;
	    mflops = kptr->mflop_count_in/this_delta_wall_self;
	    kptr->avg_mflops = ((hpm_calls-1)*kptr->avg_mflops + mflops)/hpm_calls;
#ifdef DEBUG
	    fprintf(stderr,
		    ", mip#%.15g, mflop#%.15g : mipsrate=%.15g, avg=%.15g; mflops=%.15g, avg=%.15g",
		    kptr->mip_count_in,kptr->mflop_count_in,
		    mipsrate, kptr->avg_mipsrate,
		    mflops, kptr->avg_mflops);
#endif
	  }
#ifdef DEBUG
	  fprintf(stderr,"\n");
#endif
	  if (opt_hpmstop_threshold > 0 && kptr->calls == opt_hpmstop_threshold) {
	    /* check whether hpm should anymore be called for this routine */
	    if (kptr->avg_mflops < opt_hpmstop_mflops) kptr->hpm_stopped = 1;
	  }
	}
	else {
	  stop_only_hpm(tid+1,kptr);
	} /* if (!kptr->hpm_stopped) else ... */
      } /* if (opt_hpmprof) */
#endif
      curkeyptr[tid] = thiscall[tid]->keyptr;
    }
    else {
      curkeyptr[tid] = NULL;
    } /* if (treeptr->active && treeptr->keyptr == keyptr) else ... */
  }
}

/*--- memstat ---*/

static void
memstat(drhook_key_t *keyptr, const int *thread_id, int in_getkey)
{
  if (any_memstat && keyptr) {
    if (opt_gethwm) keyptr->hwm = gethwm_();
    if (opt_getrss) {
      keyptr->maxrss = getrss_();
      keyptr->rssnow = getcurheap_thread_(thread_id);
    }
    if (opt_getstk) {
      long long int stk = getstk_();
      keyptr->stack = stk;
      keyptr->maxstack = MAX(keyptr->maxstack,stk);
    }
    if (opt_getpag) keyptr->paging = getpag_();
    if (opt_memprof) {
      keyptr->mem_seenmax = getmaxcurheap_thread_(thread_id);
      if (in_getkey) { /* Upon enter of a Dr.Hook'ed routine */
	/* A note for "keyptr->mem_curdelta": 
	   1) do not reset to 0
	   2) initially calloc'ed to 0 while initializing the keydata[] ~ alias keyptr
	   3) remember the previous value --> catches memory leaks, too !! */
	/* keyptr->mem_curdelta = 0; */
	/* Nearly the same holds for "keyptr->mem_child"; 
	   we need to capture the maximum/hwm for child */
	/* keyptr->mem_child = 0; */
	keyptr->paging_in = keyptr->paging;
      }
      else { /* Upon exit of a Dr.Hook'ed routine */
	long long int alldelta = keyptr->mem_curdelta + keyptr->mem_child;
	if (alldelta > keyptr->maxmem_alldelta) keyptr->maxmem_alldelta = alldelta;
	if (keyptr->paging - keyptr->paging_in > keyptr->mem_maxpagdelta)
	  keyptr->mem_maxpagdelta = keyptr->paging - keyptr->paging_in;
      }
      if (keyptr->hwm      > keyptr->mem_maxhwm) keyptr->mem_maxhwm = keyptr->hwm;
      if (keyptr->maxrss   > keyptr->mem_maxrss) keyptr->mem_maxrss = keyptr->maxrss;
      if (keyptr->maxstack > keyptr->mem_maxstk) keyptr->mem_maxstk = keyptr->maxstack;
    }
  }
}

/*--- flptrap ---*/

/*
  -----------------------------------------------------------------------
  If we are trapping Floating-Point Error, then set the processor in SYNC
  modes and enable TRP_INVALID, TRP_DIV_BY_ZERO and TRP_OVERFLOW.
  -----------------------------------------------------------------------
*/

#ifdef RS6K
static void
flptrap(int sig)
{
  if (sig == SIGFPE) {
    /* From John Hague, IBM, UK (--> thanks a lot, John !!)*/
    int ret = fp_trap(FP_TRAP_FASTMODE);
    if ((ret == FP_TRAP_UNIMPL) || (ret == FP_TRAP_ERROR)) {
      char errmsg[4096];
      sprintf(errmsg, 
      "flptrap(): Call to 'fp_trap' in signal_trap failed (return code = %d)\n (line %d in file %s)\n",
      ret, __LINE__, __FILE__);
      perror(errmsg);
      RAISE(SIGABRT);
    }
    fp_enable(TRP_INVALID | TRP_DIV_BY_ZERO | TRP_OVERFLOW);
  }
}
#elif defined(__GNUC__)
static void
flptrap(int sig)
{
  if (sig == SIGFPE) {
    /* Adapted from www.twinkle.ws/arnaud/CompilerTricks.html#Glibc_FP */
    trapfpe(); /* No need for pgf90's -Ktrap=fp  now ? */
  }
}
#else
static void
flptrap(int sig)
{
  return; /* A dummy */
}
#endif

/*--- catch_signals ---*/

static void signal_drhook(int sig SIG_EXTRA_ARGS);

#define CATCHSIG(x) {\
  drhook_sig_t *sl = &siglist[x];\
  if (sl->active == 0) {\
    drhook_sigfunc_t u;\
    u.func3args = signal_drhook;\
    sl->active = 1;\
    sigemptyset(&sl->new.sa_mask);\
    sl->new.sa_handler = u.func1args;\
    sl->new.sa_flags = SA_SIGINFO;\
    sigaction(x,&sl->new,&sl->old);\
    flptrap(x);\
    if (!silent && myproc == 1) {\
      fprintf(stderr,\
	      ">>%s(): DR_HOOK also catches signal#%d; new handler installed at 0x%x; old preserved at 0x%x\n",\
              "catch_signals", x, sl->new.sa_handler, sl->old.sa_handler);\
    }\
  }\
}

static void
catch_signals(int silent)
{
  char *env = getenv("DR_HOOK_CATCH_SIGNALS");
  if (env) {
    const char delim[] = ", \t/";
    char *p, *s = strdup_drhook(env);
    p = strtok(s,delim);
    while (p) {
      int sig = atoi(p);
      if (sig >= 1 && sig <= NSIG) {
	CATCHSIG(sig);
      }
      else if (sig == -1) { /* Makes ALL (catchable) signals available to DR_HOOK */
	int j;
	for (j=1; j<=NSIG; j++) {
	  CATCHSIG(j);
	} /* for (j=1; j<=NSIG; j++) */
	break;
      }
      p = strtok(NULL,delim);
    }
    free_drhook(s);
  }
}

/*--- ignore_signals ---*/

static void
ignore_signals(int silent)
{
  char *env = getenv("DR_HOOK_IGNORE_SIGNALS");
  if (env) {
    const char delim[] = ", \t/";
    char *p, *s = strdup_drhook(env);
    p = strtok(s,delim);
    while (p) {
      int sig = atoi(p);
      if (sig >= 1 && sig <= NSIG) {
	drhook_sig_t *sl = &siglist[sig];
	if (!silent && myproc == 1) {
	  fprintf(stderr,
		  ">>>ignore_signals(): DR_HOOK will ignore signal#%d altogether\n", sig);
	}
#if defined(__GNUC__)
	if (sig == SIGFPE) untrapfpe(); /* Turns off a possible -Ktrap=fp from pgf90 */
#endif
	sl->active = -1;
      }
      else if (sig == -1) { /* Switches off ALL signals from DR_HOOK */
	int j;
	for (j=1; j<=NSIG; j++) {
	  drhook_sig_t *sl = &siglist[j];
	  if (!silent && myproc == 1) {
	    fprintf(stderr,
		    ">>>ignore_signals(): DR_HOOK will ignore signal#%d altogether\n", j);
	  }
#if defined(__GNUC__)
	  if (sig == SIGFPE) untrapfpe(); /* Turns off a possible -Ktrap=fp from pgf90 */
#endif
	  sl->active = -1;
	} /* for (j=1; j<=NSIG; j++) */
	break;
      }
      p = strtok(NULL,delim);
    }
    free_drhook(s);
  }
}

/*--- gdb__sigdump ---*/

#if (defined(LINUX) || defined(SUN4)) && !defined(XT3) && !defined(XD1)
static void gdb__sigdump(int sig SIG_EXTRA_ARGS)
{
  static int who = 0; /* Current owner of the lock, if > 0 */
  int is_set = 0;
  int it = get_thread_id_(); 
  drhook_sig_t *sl = &siglist[sig];

  coml_test_lockid_(&is_set, &DRHOOK_lock);
  if (is_set && who == it) {
    fprintf(stderr,"[gdb__sigdump] : Received (another) signal#%d(%s), pid=%d\n",sig,sl->name,pid);
    fprintf(stderr,"[gdb__sigdump] : Recursive calls by the same thread#%d not allowed. Bailing out\n",it);
    return;
  }
  if (!is_set) coml_set_lockid_(&DRHOOK_lock);
  who = it;
  fprintf(stderr,"[gdb__sigdump] : Received signal#%d(%s), pid=%d\n",sig,sl->name,pid);
  LinuxTraceBack(sigcontextptr);
  who = 0;
  coml_unset_lockid_(&DRHOOK_lock);
}
#endif

/*--- signal_drhook ---*/

#define SETSIG(x,ignore_flag) {\
  drhook_sig_t *sl = &siglist[x];\
  if (sl->active == 0) {\
    drhook_sigfunc_t u;\
    u.func3args = signal_drhook;\
    sl->active = 1;\
    strcpy(sl->name,#x);\
    sigemptyset(&sl->new.sa_mask);\
    sl->new.sa_handler = u.func1args;\
    sl->new.sa_flags = SA_SIGINFO;\
    sigaction(x,&sl->new,&sl->old);\
    sl->ignore_atexit = ignore_flag;\
    flptrap(x);\
    if (!silent && myproc == 1) {\
      fprintf(stderr,"%s(%s=%d): New handler installed at 0x%x; old preserved at 0x%x\n",\
              "signal_drhook", sl->name, x, sl->new.sa_handler, sl->old.sa_handler);\
    }\
  }\
}

#define JSETSIG(x,ignore_flag) {\
  drhook_sig_t *sl = &siglist[x];\
  drhook_sigfunc_t u;\
  fprintf(stderr,"JSETSIG: sl->active = %d\n",sl->active);\
  u.func3args = signal_drhook;\
  sl->active = 1;\
  strcpy(sl->name,#x);\
  sigemptyset(&sl->new.sa_mask);\
  sl->new.sa_handler = u.func1args;\
  sl->new.sa_flags = SA_SIGINFO;\
  sigaction(x,&sl->new,&sl->old);\
  sl->ignore_atexit = ignore_flag;\
  flptrap(x);\
  fprintf(stderr,"%s(%s=%d): New handler installed at 0x%x; old preserved at 0x%x\n",\
            "signal_drhook", sl->name, x, sl->new.sa_handler, sl->old.sa_handler);\
}

static void 
signal_drhook(int sig SIG_EXTRA_ARGS)
{
  /* signal(sig, SIG_IGN); */
  if (signals_set && sig >= 1 && sig <= NSIG) { 
    /* Signal catching */
    drhook_sig_t *sl = &siglist[sig];
    drhook_sigfunc_t u;
    sigset_t newmask, oldmask;
    int tid, nsigs;
    long long int hwm = gethwm_();
    long long int rss = getrss_();
    long long int maxstack = getmaxstk_();
    long long int pag = getpag_();
    rss /= 1048576;
    hwm /= 1048576;
    maxstack /= 1048576;
    tid = get_thread_id_();

    /*------------------------------------------------------------ 
      Strategy:
      - drhook intercepts most interupts.
      - 1st interupt will 
        - call alarm(10) to try to make sure 2nd interrupt received
        - try to call tracebacks and exit (which includes atexits)
      - 2nd (and subsequent) interupts will 
        - spin for 20 sec (to give 1st interrupt time to complete tracebacks) 
        - and then call _exit (bypassing atexit)
    ------------------------------------------------------------*/
      
    /* if (sig != SIGTERM) signal(SIGTERM, SIG_DFL); */  /* Let the default SIGTERM to occur */

    nsigs=1+signal_handler_called++;

    if (nsigs > 3 * max_threads) {
      /* Note: Cannot even print & flush the msg below */
      /*
      fprintf(stderr,
	      "***Error: Too deep recursion in signal handling. Issuing 'kill -9 %d' now !!\n",
	      pid);
      fflush(NULL);
      */
      raise(SIGKILL); /* Use raise, not RAISE here */
      _exit(1); /* Should never reach here, bu' in case it does, then ... */
    }

    fprintf(stderr,
	    "[myproc#%d,tid#%d,pid#%d,signal#%d(%s)]: Received signal :: %lldMB (heap),"
	    " %lldMB (rss), %lldMB (stack), %lld (paging), nsigs %d, time %8.2f\n",
	    myproc,tid,pid,sig,sl->name, hwm, rss, maxstack, pag, nsigs, WALLTIME());
    fflush(NULL);

    /*----- 2nd (and subsequent) calls to signal handler: spin 20 sec,  _exit ---------*/
    if (nsigs > 1) {
      if (nsigs < max_threads) {
	double tt, ttt=0;
	int is;
	tt=WALLTIME();
	while ( ttt < 20.0 ) {
	  for ( is=0; is<100000000; is++) {
	    tt=tt+0.01; 
	    tt=tt-0.01;
	  }
	  ttt=WALLTIME()-tt;
	}
	fprintf(stderr,"tid#%d calling _exit with sig=%d, time =%8.2f\n",tid,sig,WALLTIME());
	fflush(NULL);
      }
      raise(SIGKILL); /* Use raise, not RAISE here */
      _exit(1); /* Should never reach here, bu' in case it does, then ... */
    }

    /*---- First call to signal handler: call alarm(10), tracebacks,  exit ------*/

    fprintf(stderr,"Activating SIGALRM=%d and calling alarm(10), time =%8.2f\n",SIGALRM,WALLTIME());
    fflush(NULL);
    JSETSIG(SIGALRM,1);
    alarm(10);

#ifdef RS6K
    /*-- llcancel attempted but sometimes hangs ---
    {
      char *env = getenv("LOADL_STEP_ID");
      if (env) {
        char *cancel = "delayed_llcancel ";
	char cmd[80];
	sprintf(cmd,"%s %s &",cancel,env);
	fprintf(stderr,"tid#%d issuing command: %s, time =%8.2f\n",tid,cmd,WALLTIME());
	fflush(NULL);
	system(cmd);
      }
    }
    ------------------------------------*/
#endif

    u.func3args = signal_drhook;

    sigfillset(&newmask);
    /*
    sigemptyset(&newmask);
    sigaddset(&newmask, sig);
    */

    /* Start critical region (we don't want any signals to interfere while doing this) */
    /* sigprocmask(SIG_BLOCK, &newmask, &oldmask); */

    if (sl->ignore_atexit) signal_handler_ignore_atexit++;

    { /* Print Dr.Hook traceback */
      const int ftnunitno = 0; /* stderr */
      const int print_option = 2; /* calling tree */
      int level = 0;
      int is;
      fprintf(stderr,"tid#%d starting drhook traceback, time =%8.2f\n",tid,WALLTIME());
      fflush(NULL);
      c_drhook_print_(&ftnunitno, &tid, &print_option, &level);
      fflush(NULL);
      fprintf(stderr,"tid#%d starting sigdump traceback, time =%8.2f\n",tid,WALLTIME());
      fflush(NULL);
#ifdef RS6K
      xl__sigdump(sig SIG_PASS_EXTRA_ARGS); /* Can't use xl__trce(...), since it also stops */
#endif
#ifdef INTEL
      intel_trbk_(); /* from ../utilities/gentrbk.F90 */
#endif
#ifdef VPP
#if defined(SA_SIGINFO) && SA_SIGINFO > 0
      _TraceCalls(sigcontextptr); /* Need VPP's libmp.a by Pierre Lagier */
#endif
#endif
#if (defined(LINUX) || defined(SUN4)) && !defined(XT3) && !defined(XD1)
      gdb__sigdump(sig SIG_PASS_EXTRA_ARGS);
#endif
      fflush(NULL);
      fprintf(stderr,"Done tracebacks, calling exit with sig=%d, time =%8.2f\n",sig,WALLTIME());
      fflush(NULL);
      if (sig != SIGABRT && sig != SIGTERM) ABOR1("Dr.Hook calls ABOR1 ...");
      _exit(1);
    }
    /* sigprocmask(SIG_SETMASK, &oldmask, 0); */
    /* End critical region : the original signal state restored */

#if 1
    /*-------------- Following code currently redundant---------------*/
    if (opt_propagate_signals &&
	sl->old.sa_handler != SIG_DFL && 
	sl->old.sa_handler != SIG_IGN && 
	sl->old.sa_handler != u.func1args) {
      /*
      fprintf(stderr,
	      ">>%s(at 0x%x): Calling previous signal handler in chain at 0x%x (if possible)\n",
	      "signal_drhook",signal_drhook,sl->old.sa_handler); 
      u.func1args = sl->old.sa_handler;
      u.func3args(sig SIG_PASS_EXTRA_ARGS);
      */
      fprintf(stderr,
              ">>%s(at 0x%x): Do not call previous signal handler in chain at 0x%x\n",
              "signal_drhook",signal_drhook,sl->old.sa_handler);
    }
    /* Make sure that the process really exits now */
    fprintf(stderr,
	    "[myproc#%d,tid#%d,pid#%d,signal#%d(%s)]: Error exit due to this signal\n",
	    myproc,tid,pid,sig,sl->name);
    fflush(NULL);
    _exit(1);
    /*---------------- End of redundant code---------------------*/
#endif

  }
  else {
    fprintf(stderr,
	    "%s(at 0x%x): Invalid signal#%d or signals/this signal not set (%d)\n",
	    "signal_drhook",signal_drhook,sig,signals_set);
#ifdef RS6K
    xl__sigdump(sig SIG_PASS_EXTRA_ARGS);
#endif
#ifdef INTEL
    intel_trbk_(); /* from ../utilities/gentrbk.F90 */
#endif
#ifdef VPP
#if defined(SA_SIGINFO) && SA_SIGINFO > 0
    _TraceCalls(sigcontextptr); /* Need VPP's libmp.a by Pierre Lagier */
#endif
#endif
#if (defined(LINUX) || defined(SUN4)) && !defined(XT3) && !defined(XD1)
    gdb__sigdump(sig SIG_PASS_EXTRA_ARGS);
#endif
    fflush(NULL);
    _exit(1);
  }
}

void
c_drhook_set_mpi_()
{
  dr_hook_procinfo_(&myproc, &nproc);
}


/*--- signal_drhook_init ---*/

static void 
signal_drhook_init(int enforce)
{
  char *env = getenv("DR_HOOK_SILENT");
  int silent = env ? atoi(env) : 0;
  int j;
  dr_hook_procinfo_(&myproc, &nproc);
  if (myproc < 1) myproc = 1; /* Just to enable output as if myproc was == 1 */
  /* Signals may not yet been set, since MPI not initialized 
     Only enforce-parameter can enforce to set these => no output on myproc=1 */
  if (!enforce && (myproc < 1 || nproc < 0)) return; 
  if (signals_set) return; /* Extra safety */
  for (j=1; j<=NSIG; j++) { /* Initialize */
    drhook_sig_t *sl = &siglist[j];
    sl->active = 0;
    sprintf(sl->name, "DR_HOOK_SIG#%d", j);
    sl->ignore_atexit = 0;
  }
  ignore_signals(silent); /* These signals will not be seen by DR_HOOK */
  SETSIG(SIGABRT,0); /* Good to be first */
  SETSIG(SIGBUS,0);
  SETSIG(SIGSEGV,0);
  SETSIG(SIGILL,0);
#if defined(SIGEMT)
  SETSIG(SIGEMT,0);
#endif
#if defined(SIGSTKFLT)
  SETSIG(SIGSTKFLT,0); /* Stack fault */
#endif
  SETSIG(SIGFPE,0);
  SETSIG(SIGTRAP,0); /* should be switched off when used with debuggers */
  SETSIG(SIGINT,0);
  SETSIG(SIGQUIT,0);
  SETSIG(SIGTERM,0);
#if defined(SIGIOT)
  SETSIG(SIGIOT,0);  /* Same as SIGABRT; Used to be a typo SIGIO ;-( */
#endif
  SETSIG(SIGXCPU,1); /* ignore_atexit == 1 i.e. no profile info via atexit() */
#if defined(SIGDANGER)
  SETSIG(SIGDANGER,1); /* To catch the place where paging space gets dangerously low */
#endif
  SETSIG(SIGSYS,0);
  /* SETSIG(SIGCHLD); we may not want to catch this either; may interfere parallel processing */
  /* -- not active
  SETSIG(SIGCHLD);
  SETSIG(SIGHUP);
  SETSIG(SIGCONT);
  */
  catch_signals(silent); /* Additional signals to be seen by DR_HOOK */
  signals_set = 1; /* Signals are set now */
}

/*--- get_mon_out ---*/

static char *
get_mon_out(int me)
{
  char *s = mon_out;
  if (mon_out_procs == me || (mon_out_procs == -1 && me >= 1 && me <= nproc)) {
    if (!mon_out) mon_out = strdup_drhook("drhook.prof.%d");
    s = malloc_drhook((strlen(mon_out) + 20) * sizeof(*s));
    sprintf(s,mon_out,me);
  }
  if (!s) s = strdup_drhook("drhook.prof.0");
  return s;
}

/*--- get_memmon_out ---*/

static char *
get_memmon_out(int me)
{
  char *s = NULL;
  char *p = get_mon_out(me);
  if (p) {
    s = malloc_drhook((strlen(p) + 5) * sizeof(*s));
    sprintf(s,"%s-mem",p);
  }
  if (!s) s = strdup_drhook("drhook.prof.0-mem");
  return s;
}

/*--- process_options ---*/

static void do_prof();

static void
process_options()
{
  char *env;
  FILE *fp = stderr;

  env = getenv("DR_HOOK_SHOW_PROCESS_OPTIONS");
  if (env) {
    int ienv = atoi(env);
    if (ienv == 0) fp = NULL;
  }

  env = getenv("DR_HOOK_PROFILE");
  if (env) {
    char *s = calloc_drhook(strlen(env) + 10, sizeof(*s));
    strcpy(s,env);
    if (!strchr(env,'%')) strcat(s,".%d");
    mon_out = strdup_drhook(s);
    if (fp) fprintf(fp,">>>process_options(): DR_HOOK_PROFILE=%s\n",mon_out);
    free(s);
  }

  env = getenv("DR_HOOK_PROFILE_PROC");
  if (env) {
    mon_out_procs = atoi(env);
    if (fp) fprintf(fp,">>>process_options(): DR_HOOK_PROFILE_PROC=%d\n",mon_out_procs);
  }

  env = getenv("DR_HOOK_PROFILE_LIMIT");
  if (env) {
    percent_limit = atof(env);
    if (fp) fprintf(fp,">>>process_options(): DR_HOOK_PROFILE_LIMIT=%.3f\n",percent_limit);
  }

  env = getenv("DR_HOOK_CALLPATH_INDENT");
  if (env) {
    callpath_indent = atoi(env);
    if (callpath_indent < 1 || callpath_indent > 8) callpath_indent = callpath_indent_default;
    if (fp) fprintf(fp,">>>process_options(): DR_HOOK_CALLPATH_INDENT=%d\n",callpath_indent);
  }

  env = getenv("DR_HOOK_CALLPATH_DEPTH");
  if (env) {
    callpath_depth = atoi(env);
    if (callpath_depth < 0) callpath_depth = callpath_depth_default;
    if (fp) fprintf(fp,">>>process_options(): DR_HOOK_CALLPATH_DEPTH=%d\n",callpath_depth);
  }

  env = getenv("DR_HOOK_CALLTRACE");
  if (env) {
    opt_calltrace = atoi(env);
    if (fp) fprintf(fp,">>>process_options(): DR_HOOK_CALLTRACE=%d\n",opt_calltrace);
  }

  env = getenv("DR_HOOK_TIMELINE");
  if (env) {
    opt_timeline = atoi(env);
    if (fp) fprintf(fp,">>>process_options(): DR_HOOK_TIMELINE=%d\n",opt_timeline);
  }

  env = getenv("DR_HOOK_TIMELINE_THREAD");
  if (env) {
    opt_timeline_thread = atoi(env);
    if (fp) fprintf(fp,">>>process_options(): DR_HOOK_TIMELINE_THREAD=%d\n",opt_timeline_thread);
  }

  env = getenv("DR_HOOK_TIMELINE_FORMAT");
  if (env) {
    opt_timeline_format = atoi(env);
    if (fp) fprintf(fp,">>>process_options(): DR_HOOK_TIMELINE_FORMAT=%d\n",opt_timeline_format);
  }

  env = getenv("DR_HOOK_TIMELINE_UNITNO");
  if (env) {
    opt_timeline_unitno = atoi(env);
    if (fp) fprintf(fp,">>>process_options(): DR_HOOK_TIMELINE_UNITNO=%d\n",opt_timeline_unitno);
  }

  env = getenv("DR_HOOK_TIMELINE_FREQ");
  if (env) {
    opt_timeline_freq = atoi(env);
    if (fp) fprintf(fp,">>>process_options(): DR_HOOK_TIMELINE_FREQ=%d\n",opt_timeline_freq);
  }

  env = getenv("DR_HOOK_HASHBITS");
  if (env) {
    int value = atoi(env);
    if (value < 1) value = 1;
    else if (value > NHASHMAX) value = NHASHMAX;
    nhash = value;
    hashsize = HASHSIZE(nhash);
    hashmask = HASHMASK(nhash);
    if (fp) fprintf(fp,">>>process_options(): DR_HOOK_HASHBITS=%d\n",nhash);
  }

  env = getenv("DR_HOOK_HPMSTOP");
  if (env) {
    char *s = strdup_drhook(env);
    long long int a;
    double b;
    int n = 0;
    env = s;
    while (*env) {
      if (isspace(*env) || *env == ',') *env = ' ';
      env++;
    }
    n = sscanf(s,"%lld %lf",&a,&b);
    if (n >= 1) opt_hpmstop_threshold = a;
    if (n >= 2) opt_hpmstop_mflops = b;
    if (fp) fprintf(fp,">>>process_options(): DR_HOOK_HPMSTOP=%lld,%.15g\n",
	    opt_hpmstop_threshold,opt_hpmstop_mflops);
    free_drhook(s);
  }

  env = getenv("DR_HOOK_OPT");
  if (env) {
    const char delim[] = ", \t/";
    char *comma = ">>>process_options(): DR_HOOK_OPT=\"";
    char *s = strdup_drhook(env);
    char *p = s;
    while (*p) {
      if (islower(*p)) *p = toupper(*p);
      p++;
    } 
    p = strtok(s,delim);
    /* if (p) if (fp) fprintf(fp,">>>process_options(): DR_HOOK_OPT=\""); */
    while (p) {
      /* Assume that everything is OFF by default */
      if (strequ(p,"ALL")) { /* all except profiler data */
	opt_gethwm = opt_getstk = opt_getrss = opt_getpag = opt_walltime = opt_cputime = 1;
	opt_calls = 1;
	any_memstat++;
	if (fp) fprintf(fp,"%s%s",comma,"ALL"); comma = ",";
      }
      else if (strequ(p,"MEM") || strequ(p,"MEMORY")) {
	opt_gethwm = opt_getstk = opt_getrss = 1;
	opt_calls = 1;
	any_memstat++;
	if (fp) fprintf(fp,"%s%s",comma,"MEMORY"); comma = ",";
      }
      else if (strequ(p,"TIME") || strequ(p,"TIMES")) {
	opt_walltime = opt_cputime = 1;
	opt_calls = 1;
	if (fp) fprintf(fp,"%s%s",comma,"TIMES"); comma = ",";
      }
      else if (strequ(p,"HWM") || strequ(p,"HEAP")) {
	opt_gethwm = 1;
	opt_calls = 1;
	any_memstat++;
	if (fp) fprintf(fp,"%s%s",comma,"HEAP"); comma = ",";
      }
      else if (strequ(p,"STK") || strequ(p,"STACK")) {
	opt_getstk = 1;
	opt_calls = 1;
	any_memstat++;
	if (fp) fprintf(fp,"%s%s",comma,"STACK"); comma = ",";
      }
      else if (strequ(p,"RSS")) {
	opt_getrss = 1;
	opt_calls = 1;
	any_memstat++;
	if (fp) fprintf(fp,"%s%s",comma,"RSS"); comma = ",";
      }
      else if (strequ(p,"PAG") || strequ(p,"PAGING")) {
	opt_getpag = 1;
	opt_calls = 1;
	any_memstat++;
	if (fp) fprintf(fp,"%s%s",comma,"PAGING"); comma = ",";
      }
      else if (strequ(p,"WALL") || strequ(p,"WALLTIME")) {
	opt_walltime = 1;
	opt_calls = 1;
	if (fp) fprintf(fp,"%s%s",comma,"WALLTIME"); comma = ",";
      }
      else if (strequ(p,"CPU") || strequ(p,"CPUTIME")) {
	opt_cputime = 1;
	opt_calls = 1;
	if (fp) fprintf(fp,"%s%s",comma,"CPUTIME"); comma = ",";
      }
      else if (strequ(p,"CALLS") || strequ(p,"COUNT")) {
	opt_calls = 1;
	if (fp) fprintf(fp,"%s%s",comma,"CALLS"); comma = ",";
      }
      else if (strequ(p,"MEMPROF")) {
	opt_memprof = 1;
	opt_gethwm = opt_getstk = opt_getrss = 1;
	opt_getpag = 1;
	opt_calls = 1;
	any_memstat++;
	if (fp) fprintf(fp,"%s%s",comma,"MEMPROF"); comma = ",";
      }
      else if (strequ(p,"PROF") || strequ(p,"WALLPROF")) {
	opt_wallprof = 1;
	opt_walltime = 1;
	opt_cpuprof = 0; /* Note: Switches cpuprof OFF */
	opt_calls = 1;
	if (fp) fprintf(fp,"%s%s",comma,"WALLPROF"); comma = ",";
      }
      else if (strequ(p,"CPUPROF")) {
	opt_cpuprof = 1;
	opt_cputime = 1;
	opt_wallprof = 0; /* Note: Switches walprof OFF */
	opt_calls = 1;
	if (fp) fprintf(fp,"%s%s",comma,"CPUPROF"); comma = ",";
      }
      else if (strequ(p,"HPM") || strequ(p,"HPMPROF") || strequ(p,"MFLOPS")) {
	opt_hpmprof = 1;
	opt_wallprof = 1; /* Note: Implies wallprof (or prof), not cpuprof */
	opt_walltime = 1;
	opt_cpuprof = 0;  /* Note: Switches cpuprof OFF */
	opt_calls = 1;
	if (fp) fprintf(fp,"%s%s",comma,"HPMPROF"); comma = ",";
      }
      else if (strequ(p,"TRIM")) {
	opt_trim = 1;
	if (fp) fprintf(fp,"%s%s",comma,"TRIM"); comma = ",";
      }
      else if (strequ(p,"SELF")) {
	opt_self = 2;
	if (fp) fprintf(fp,"%s%s",comma,"SELF"); comma = ",";
      }
      else if (strequ(p,"NOSELF")) {
	opt_self = 0;
	if (fp) fprintf(fp,"%s%s",comma,"NOSELF"); comma = ",";
      }
      else if (strequ(p,"NOPROP") || strequ(p,"NOPROPAGATE") ||
	       strequ(p,"NOPROPAGATE_SIGNALS")) {
	opt_propagate_signals = 0;
	if (fp) fprintf(fp,"%s%s",comma,"NOPROPAGATE_SIGNALS"); comma = ",";
      }
      else if (strequ(p,"NOSIZE") || strequ(p,"NOSIZEINFO")) {
	opt_sizeinfo = 0;
	if (fp) fprintf(fp,"%s%s",comma,"NOSIZEINFO"); comma = ",";
      }
      else if (strequ(p,"CLUSTER") || strequ(p,"CLUSTERINFO")) {
	opt_clusterinfo = 1;
	if (fp) fprintf(fp,"%s%s",comma,"CLUSTERINFO"); comma = ",";
      }
      else if (strequ(p,"CALLPATH")) {
	opt_callpath = 1;
	if (fp) fprintf(fp,"%s%s",comma,"CALLPATH"); comma = ",";
      }
      p = strtok(NULL,delim);
    }
    free_drhook(s);
    if (*comma == ',') if (fp) fprintf(fp,"\"\n");
    if (opt_wallprof || opt_cpuprof || opt_memprof) {
      atexit(do_prof);
    }
  }
}

/*--- trim ---*/

static const char *
trim(const char *name, int *n)
{
  const char *from;
  int len;
  int name_len = *n;
  while (*name && isspace(*name) && name_len > 0) {
    /* skip leading blanks */
    name++;
    name_len--;
  }
  len = 0;
  from = name;
  while (*from && !isspace(*from) && name_len > 0) {
    /* find first space point, if any */
    from++;
    len++;
    name_len--;
  }
  *n = len;
  if (name) return name;
  else {
    /* Never actually called (unless a true fatality) */
    ABOR1("***Fatal error in drhook.c:trim()-function");
    return NULL;
  }
}

/*--- insertkey ---*/

static drhook_key_t *
insertkey(int tid, const drhook_key_t *keyptr_in)
{
  drhook_key_t *keyptr = NULL;
  if (tid >= 1 && tid <= numthreads) {
    /* no trimming available for this; just raw eval & insert */
    unsigned int hash = hashfunc(keyptr_in->name, keyptr_in->name_len);
    keyptr = &keydata[tid-1][hash];
    for (;;) {
      if (!keyptr->name) { /* A free slot */
	memcpy(keyptr,keyptr_in,sizeof(*keyptr));
	keyptr->next = NULL;
	break;
      }
      else {
	if (!keyptr->next) {
	  keyptr->next = calloc_drhook(1, sizeof(drhook_key_t)); /* chaining */
	}
	keyptr = keyptr->next;
      }  /* if (!keyptr->name) ... else ... */
    } /* for (;;) */
  } /* if (tid >= 0 && tid < numthreads) */
  return keyptr;
}

/*--- getkey ---*/

static drhook_key_t *
getkey(int tid, const char *name, int name_len,
       const char *filename, int filename_len,
       const double *walltime, const double *cputime,
       const equivalence_t *callpath, int callpath_len,
       int *free_callpath)
{
  drhook_key_t *keyptr = NULL;
  if (tid >= 1 && tid <= numthreads) {
    unsigned int hash, fullhash;
    if (opt_trim) name = trim(name, &name_len);
    hash = hashfunc(name, name_len);
    if (callpath) {
      callpath_hashfunc(hash, callpath, callpath_len, &fullhash);
#ifdef DEBUG
      fprintf(stderr,
	      "getkey: name='%.*s', name_len=%d, callpath_len=%d, fullhash=%u\n",
	      name_len, name, name_len, callpath_len, fullhash);
#endif
    }
    keyptr = &keydata[tid-1][hash];
    for (;;) {
      int found = 0;
      if (!keyptr->name) { /* A free slot */
	keyptr->name = malloc_drhook((name_len+1)*sizeof(*name));
	keyptr->name_len = name_len;
	if (opt_trim) {
	  const char *from = name;
	  char *to = keyptr->name;
	  int len = name_len;
	  for (; len>0; from++, len--) {
	    *to++ = islower(*from) ? toupper(*from) : *from;
	  }
	  *to = 0;
	}
	else {
	  memcpy(keyptr->name, name, name_len);
	  keyptr->name[name_len] = 0;
	}
	if (filename && *filename && filename_len > 0) {
	  char *psave = NULL;
	  char *p = psave = malloc_drhook((filename_len+1)*sizeof(*filename));
	  memcpy(p, filename, filename_len);
	  p[filename_len] = 0;
	  { /* Strip out dirname */
	    char *s = strrchr(p,'/');
	    if (s) p = s+1;
	  }
	  keyptr->filename = strdup_drhook(p);
	  free_drhook(psave);
	}
	if (callpath) {
	  if (free_callpath) *free_callpath = 0;
	  keyptr->callpath = callpath;
	  keyptr->callpath_len = callpath_len;
	  keyptr->callpath_fullhash = fullhash;
	}
	found = 1;
      }
      if (found || 
	  (keyptr->name_len == name_len &&
	   (!callpath || (callpath && keyptr->callpath && 
			  keyptr->callpath_len == callpath_len &&
			  keyptr->callpath_fullhash == fullhash)) &&
	   ((!opt_trim && *keyptr->name == *name && strnequ(keyptr->name, name, name_len)) ||
	    (opt_trim && strncasecmp(keyptr->name, name, name_len) == 0)))) {
	if (opt_walltime) keyptr->wall_in = walltime ? *walltime : WALLTIME();
	if (opt_cputime) keyptr->cpu_in  = cputime ? *cputime : CPUTIME();
	if (any_memstat) memstat(keyptr,&tid,1);
	if (opt_calls) {
	  keyptr->calls++;
	  keyptr->status++;
	}
	insert_calltree(tid, keyptr);
	break; /* for (;;) */
      }
      else {
	if (!keyptr->next) {
	  keyptr->next = calloc_drhook(1, sizeof(drhook_key_t)); /* chaining */
	}
	keyptr = keyptr->next;
      }  /* if (found ...) else ... */
    } /* for (;;) */
    curkeyptr[tid-1] = keyptr;
  } /* if (tid >= 1 && tid <= numthreads) */
  return keyptr;
}

/*--- putkey ---*/

static void
putkey(int tid, drhook_key_t *keyptr, const char *name, int name_len,
       int sizeinfo,
       double *walltime, double *cputime)
{
  drhook_calltree_t *treeptr = (tid >= 1 && tid <= numthreads) ? thiscall[tid-1] : NULL;
  if (!treeptr || !treeptr->active || !(treeptr->keyptr == keyptr)) {
    const int sig = SIGABRT;
    const char sl_name[] = "SIGABRT";
    char *s;
    if (opt_trim) name = trim(name,&name_len);
    s = strdup2_drhook(name,name_len);
    if (opt_trim) {
      char *p = s;
      while (*p) {
	if (islower(*p)) *p = toupper(*p);
	p++;
      }
    }
    fprintf(stderr,
	    "[myproc#%d,tid#%d,pid#%d,signal#%d(%s)]: Dr.Hook has detected an invalid"
	    " key-pointer/handle while leaving routine '%s'\n",
	    myproc,tid,pid,sig,sl_name,s);
    if (treeptr) {
      equivalence_t u;
      u.keyptr = treeptr->keyptr;
      fprintf(stderr,
	      "[myproc#%d,tid#%d,pid#%d,signal#%d(%s)]: Expecting key-pointer=0x%x (%.20g)"
	      " and treeptr->active-flag == 1\n",
	      myproc,tid,pid,sig,sl_name,u.keyptr,u.d);
      u.keyptr = keyptr;
      fprintf(stderr,
	      "[myproc#%d,tid#%d,pid#%d,signal#%d(%s)]: Got a key-pointer=0x%x (%.20g)"
	      " and treeptr->active-flag = %d\n",
	      myproc,tid,pid,sig,sl_name,u.keyptr,u.d,treeptr->active);
    }
    fprintf(stderr,"[myproc#%d,tid#%d,pid#%d,signal#%d(%s)]: Aborting...\n",myproc,tid,pid,sig,sl_name);
    free_drhook(s);
    RAISE(SIGABRT);
  }
  else if (tid >= 1 && tid <= numthreads) {
    double delta_wall = 0;
    double delta_cpu  = 0;
    if (any_memstat) memstat(keyptr,&tid,0);
    if (opt_calls)   keyptr->status--;
    if (opt_cputime && cputime) {
      *cputime = CPUTIME();
      delta_cpu = *cputime - keyptr->cpu_in;
    }
    if (opt_walltime && walltime) {
      *walltime = WALLTIME();
      delta_wall = *walltime - keyptr->wall_in;
    }
    if (opt_walltime) keyptr->delta_wall_all += delta_wall;
    if (opt_cputime)  keyptr->delta_cpu_all  += delta_cpu;
    if (opt_sizeinfo && sizeinfo > 0) keyptr->sizeinfo += sizeinfo;
    remove_calltree(tid, keyptr, &delta_wall, &delta_cpu);
  }
}
    
/*--- init_drhook ---*/

static void
init_drhook(int ntids)
{
  if (numthreads == 0 || !keydata || !calltree || !keyself || !overhead || !curkeyptr) {
    int j;
    if (pid == -1) { /* Ensure that just called once */
      {
	/* Invoke once : timers, memory counters etc. to "wake them up" */
	(void) WALLTIME();
	(void) CPUTIME();
	(void) gethwm_();
	(void) getrss_();
	(void) getstk_();
	(void) getmaxstk_();
	(void) getpag_();
      }
#ifdef RS6K
      irtc_start = irtc();
#endif
#if defined(SV2) || defined(XD1) || defined(XT3)
#if defined(SV2)
      irtc_start = _rtc();
#else
      irtc_start = irtc_();
#endif
      my_irtc_rate = irtc_rate_();
      my_inv_irtc_rate = 1.0/my_irtc_rate;
#endif
      start_stamp = timestamp();
      {
	char *env = getenv("DR_HOOK_SHOW_LOCK"); /* export DR_HOOK_SHOW_LOCK=1 to show the lock-info */
	int konoff = env ? atoi(env) : 0;
	int kret = 0;
	if (konoff == 1) coml_set_debug_(&konoff, &kret);
	INIT_LOCKID_WITH_NAME(&DRHOOK_lock,"drhook.c:DRHOOK_lock");
	if (kret != 0) {
	  konoff = 0;
	  coml_set_debug_(&konoff, &kret);
	}
      }
#ifdef NEC
      { /* If C-programs compiled with -traceback, then NEC/F90 
	   MESPUT-call will also includes C-routines in the traceback if 
	   in addition 'export C_TRACEBACK=YES' */
	char *env = getenv("C_TRACEBACK");
	if (!env) {
	  /* Override only if C_TRACEBACK hadn't already been defined */
	  static char s[] = "C_TRACEBACK=YES"; /* note: must be static */
	  putenv(s);
	}
      }
#endif
      ec_set_umask_();
      pid = getpid();
      process_options();
      drhook_lhook = 1;
    }
    if (!keydata) {
      keydata = malloc_drhook(sizeof(**keydata) * ntids);
      for (j=0; j<ntids; j++) {
	keydata[j] = calloc_drhook(hashsize, sizeof(drhook_key_t));
      }
    }
    if (!calltree) {
      calltree = malloc_drhook(sizeof(**calltree) * ntids);
      thiscall = malloc_drhook(sizeof(**thiscall) * ntids);
      for (j=0; j<ntids; j++) {
	thiscall[j] = calltree[j] = calloc_drhook(1,sizeof(drhook_calltree_t));
      }
    }
    if (!keyself && opt_self && (opt_wallprof || opt_cpuprof || opt_hpmprof)) {
      const char *name = "$drhook";
      int name_len = strlen(name);
      keyself = malloc_drhook(sizeof(**keyself) * ntids);
      for (j=0; j<ntids; j++) {
	drhook_key_t *keyptr = keyself[j] = calloc_drhook(1,sizeof(drhook_key_t));
	keyptr->name = strdup_drhook(name);
	keyptr->name_len = name_len;
      }
    }
    if (!overhead) {
      overhead = calloc_drhook(ntids,sizeof(*overhead));
    }
    if (!curkeyptr) {
      curkeyptr = malloc_drhook(sizeof(**curkeyptr) * ntids);
      for (j=0; j<ntids; j++) {
	curkeyptr[j] = NULL;
      }
    }
    numthreads = ntids;
    signal_drhook_init(1);
    if (!timeline_calls) {
      if (opt_timeline_unitno >= 0 && opt_timeline_freq >= 1 &&
	  (opt_timeline == myproc || opt_timeline == -1)) {
	timeline_calls = calloc_drhook(ntids * 2, sizeof(*timeline_calls));
      }
    }
    init_hpm(1); /* First thread */
  }
}

/*-- overhead-macro --*/

#define OVERHEAD(tid,walltime_in,cputime_in,delta,calc_delta) \
if (overhead && tid >= 1 && tid <= numthreads) { \
  if (calc_delta) { \
    if      (opt_walltime) delta = WALLTIME() - walltime_in; \
    else if (opt_cputime)  delta = CPUTIME()  - cputime_in; \
    else                   delta = 0; \
  } \
  overhead[tid-1] += delta; \
}
/*--- itself ---*/

#define ITSELF_0 \
  double delta = 0; \
  drhook_key_t *keyptr_self = keyself ? itself(NULL,*thread_id,0,NULL,&walltime,&cputime) : NULL;

#define ITSELF_1 \
  if (keyptr_self) { \
    (void) itself(keyptr_self,*thread_id,1,&delta,&walltime,&cputime); \
    if (opt_wallprof) u.keyptr->delta_wall_child += delta; \
    else              u.keyptr->delta_cpu_child  += delta; \
    OVERHEAD(*thread_id,walltime,cputime,delta,0); \
  } \
  else { \
    OVERHEAD(*thread_id,walltime,cputime,delta,1); \
  }

static drhook_key_t *
itself(drhook_key_t *keyptr_self, 
       int tid, int opt, double *delta_time, 
       const double *walltime, const double *cputime) 
{
  drhook_key_t *keyptr = NULL;
  if (keyself) {
    keyptr = keyptr_self ? keyptr_self : keyself[--tid];
    if (opt == 0) {
      if (opt_wallprof) keyptr->wall_in = walltime ? *walltime : WALLTIME();
      else              keyptr->cpu_in = cputime ? *cputime : CPUTIME();
      keyptr->calls++;
    }
    else if (opt == 1) {
      double delta = 0;
      if (opt_wallprof) {
	delta = walltime ? (*walltime - keyptr->wall_in) : (WALLTIME() - keyptr->wall_in);
	keyptr->delta_wall_all += delta;
      }
      else {
	delta = cputime ? (*cputime - keyptr->cpu_in) : (CPUTIME() - keyptr->cpu_in);
	keyptr->delta_cpu_all += delta;
      }
      if (delta_time) *delta_time = delta;
    }
  }
  return keyptr;
}

/*--- commie -routines : adds "," i.e. comma after each 3 digit, e.g.:
  1234567890 becomes more readable 1,234,567,890 */

static void 
lld_commie(long long int n, char sd[])
{
  const char comma = ',';
  char s[DRHOOK_STRBUF];
  char *p;
  int len, ncommas;
  sprintf(s,"%lld",n);
  len = strlen(s);
  ncommas = (len-1)/3;
  if (ncommas > 0) {
    char *pd = sd + len + ncommas;
    *pd-- = 0;
    p = s + len - 1;
    len = 0;
    while (p-s >= 0) {
      *pd-- = *p--;
      len++;
      if (p-s >= 0 && len%3 == 0) *pd-- = comma;
    }
  }
  else {
    strcpy(sd,s);
  }
}

static void 
dbl_commie(double n, char sd[])
{
  const char comma = ',';
  char s[DRHOOK_STRBUF];
  char *p;
  int len, ncommas;
  sprintf(s,"%.0f",n);
  len = strlen(s);
  ncommas = (len-1)/3;
  if (ncommas > 0) {
    char *pd = sd + len + ncommas;
    *pd-- = 0;
    p = s + len - 1;
    len = 0;
    while (p-s >= 0) {
      *pd-- = *p--;
      len++;
      if (p-s >= 0 && len%3 == 0) *pd-- = comma;
    }
  }
  else {
    strcpy(sd,s);
  }
}

/*--- callpath as a "pathname" ---*/

static void
unroll_callpath(FILE *fp, int len, 
		const equivalence_t *callpath, int callpath_len)
{
  if (fp && callpath && callpath_len > 0) {
    int j;
    for (j=0; j<callpath_len; callpath++, j++) {
      if (callpath && callpath->keyptr && callpath->keyptr->name) {
	const char *name = callpath->keyptr->name;
	int name_len = callpath->keyptr->name_len;
	len -= callpath_indent;
	if (len < 0) len = 0;
	fprintf(fp,"\n%*s%.*s",len," ",name_len,name);
      }
#ifdef DEBUG
      else {
	fprintf(fp,
		"\n????callpath=0x%x, callpath->keyptr=0x%x,  callpath->keyptr->name='%s'",
		callpath, callpath ? callpath->keyptr : 0,
		(callpath && callpath->keyptr && callpath->keyptr->name) ?
		callpath->keyptr->name : "(nil)");
      }
#endif
    }
  } /* if (fp) */
}


static equivalence_t *
get_callpath(int tid, int *callpath_len)
{
  int depth = 0;
  equivalence_t *callpath = NULL;
  if (tid >= 1 && tid <= numthreads) {
    const drhook_calltree_t *treeptr = thiscall[--tid];
    while (treeptr && treeptr->active && depth < callpath_depth) {
      depth++;
      treeptr = treeptr->prev;
    }
    if (depth > 0) {
      int j = 0;
      callpath = malloc_drhook(sizeof(*callpath) * depth);
      treeptr = thiscall[tid];
      while (treeptr && treeptr->active && j < callpath_depth) {
	callpath[j].keyptr = treeptr->keyptr;
	j++;
	treeptr = treeptr->prev;
      }
    } /* if (depth > 0) */
  } /* if (tid >= 1 && tid <= numthreads) */
  if (callpath_len) *callpath_len = depth;
  return callpath;
}

/*--- profiler output ---*/

static int do_prof_off = 0;

static void
do_prof()
{

  /* to avoid recursive signals while atexit() (e.g. SIGXCPU) */
  if (signal_handler_ignore_atexit) return; 

  if (!do_prof_off && (opt_wallprof || opt_cpuprof)) {
    /* CPU, wall-clock and/or MFlop/s profiling */
    const int ftnunitno = 0;
    const int master = 1;
    const int print_option = 3;
    int initlev = 0;
    c_drhook_print_(&ftnunitno, &master, &print_option, &initlev);
  }

  if (!do_prof_off && opt_memprof) {
    /* Memory profiling */
    const int ftnunitno = 0;
    const int master = 1;
    const int print_option = 4;
    int initlev = 0;
    c_drhook_print_(&ftnunitno, &master, &print_option, &initlev);
  }
}

/*--- Check watch points ---*/

static void 
check_watch(const char *label,
	    const char *name,
	    int name_len)
{
  if (watch) {
    drhook_watch_t *p;
    coml_set_lockid_(&DRHOOK_lock);
    p = watch;
    while (p) {
      if (p->active) {
	unsigned int crc32 = 0;
	int calc_crc = 0;
	const char *first_nbytes = p->ptr;
	int changed = memcmp(first_nbytes,p->ptr,p->watch_first_nbytes);
	if (!changed) {
	  /* The first nbytes the same; checking if crc has changed ... */
	  crc32_(p->ptr, &p->nbytes, &crc32);
	  changed = (crc32 != p->crc32);
	  calc_crc = 1;
	}
	if (changed) {
	  int tid = get_thread_id_();
	  if (!calc_crc) crc32_(p->ptr, &p->nbytes, &crc32);
	  fprintf(stderr,
		  "***%s: Watch point '%s' at address 0x%x on myproc#%d has changed"
		  " (detected in tid#%d when %s routine %.*s) : new crc32=%u\n",
		  p->abort_if_changed ? "Error" : "Warning",
		  p->name, p->ptr, myproc, tid,
		  label, name_len, name, crc32);
	  if (p->abort_if_changed) {
	    coml_unset_lockid_(&DRHOOK_lock); /* An important unlocking on Linux; otherwise hangs (until time-out) */
	    RAISE(SIGABRT);
	  }
	  p->active = 0; /* No more these messages for this array */
	  watch_count--;
	}
      }
      p = p->next;
    } /* while (p) */
    coml_unset_lockid_(&DRHOOK_lock);
  }
}

/*** PUBLIC ***/

#define TIMERS \
double walltime = opt_walltime ? WALLTIME() : 0; \
double cputime  = opt_cputime ? CPUTIME()  : 0


/*=== c_drhook_set_lhook_ ===*/

void
c_drhook_set_lhook_(const int *lhook)
{
  if (lhook) drhook_lhook = *lhook;
}

/*=== c_drhook_getenv_ ===*/

void 
c_drhook_getenv_(const char *s, 
		 char *value,
		 /* Hidden arguments */
		 int slen,
		 const int valuelen) 
{
  char *env = NULL;
  char *p = malloc_drhook(slen+1);
  if (!p) {
    fprintf(stderr,"c_drhook_getenv_(): Unable to allocate %d bytes of memory\n", slen+1);
    RAISE(SIGABRT);
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


/*=== c_drhook_init_ ===*/

void 
c_drhook_init_(const char *progname,
	       const int *num_threads
	       /* Hidden length */
	       ,int progname_len)
{
  init_drhook(*num_threads);
  max_threads = MAX(1,*num_threads);
  if (a_out) free_drhook(a_out);
  progname = trim(progname, &progname_len);  
  if (progname_len > 0) {
    a_out = calloc_drhook(progname_len+1,sizeof(*progname));
    memcpy(a_out, progname, progname_len);
  }
  else {
    /* progname is a blank string;
       this is most likely due to a Fortran-call to getarg
       from program that has a C-main program, thus Fortran getarg
       may return a blank string */

    const char *arg0 = ec_GetArgs(0);
    if (arg0) {
      const char *pc = arg0;
      progname_len = strlen(pc);
      pc = trim(pc, &progname_len);
      a_out = strdup_drhook(pc);
    }
  }
  if (!a_out) {
    a_out = strdup_drhook("a.out"); /* Failed to obtain the name of the executing program */
  }
}


/*=== c_drhook_watch_ ===*/

void
c_drhook_watch_(const int *onoff,
		const char *array_name,
		const void *array_ptr,
		const int *nbytes,
		const int *abort_if_changed
		/* Hidden length */
		,int array_name_len)
{
  int tid = get_thread_id_();
  drhook_watch_t *p = NULL;
  if (!drhook_lhook) return; 

  coml_set_lockid_(&DRHOOK_lock);

  /* check whether this array_ptr is already registered, but maybe inactive */
  p = watch;
  while (p) {
    if (p->ptr == array_ptr) {
      if (p->active) watch_count--;
      free_drhook(p->name);
      break;
    }
    p = p->next;
  }

  if (!p) {
    /* create new branch */
    p = calloc_drhook(1, sizeof(*p)); /* Implies p->next = NULL */
    if (!last_watch) {
      last_watch = watch = p;
    }
    else {
      last_watch->next = p;
      last_watch = p;
    }
  }

  p->name = strdup2_drhook(array_name,array_name_len);
  p->tid = tid;
  p->active = *onoff;
  if (p->active) watch_count++;
  p->abort_if_changed = *abort_if_changed;
  p->ptr = array_ptr;
  p->nbytes = *nbytes;
  p->watch_first_nbytes = MIN(p->nbytes, MAX_WATCH_FIRST_NBYTES);
  memcpy(p->first_nbytes,p->ptr,p->watch_first_nbytes);
  p->crc32 = 0;
  crc32_(p->ptr, &p->nbytes, &p->crc32);
  fprintf(stderr,
  "***Warning: Watch point '%s' was created for address 0x%x (%d bytes, on myproc#%d, tid#%d) : crc32=%u\n",
          p->name, p->ptr, p->nbytes, myproc, p->tid, p->crc32);

  coml_unset_lockid_(&DRHOOK_lock);
}

/*=== c_drhook_start_ ===*/

void 
c_drhook_start_(const char *name, 
		const int *thread_id, 
		double *key,
		const char *filename,
		const int *sizeinfo
		/* Hidden length */
		,int name_len, int filename_len)
{
  TIMERS;
  equivalence_t u;
  ITSELF_0;
  if (!signals_set) signal_drhook_init(1);
  if (watch && watch_count > 0) check_watch("entering", name, name_len);
  if (!opt_callpath) {
    u.keyptr = getkey(*thread_id, name, name_len, 
		      filename, filename_len,
		      &walltime, &cputime,
		      NULL, 0, NULL);
  }
  else { /* (Much) more overhead */
    int free_callpath = 1;
    int callpath_len = 0;
    equivalence_t *callpath = get_callpath(*thread_id, &callpath_len);
    u.keyptr = getkey(*thread_id, name, name_len, 
		      filename, filename_len,
		      &walltime, &cputime,
		      callpath, callpath_len, &free_callpath);
    if (free_callpath) free_drhook(callpath);
  }
  *key = u.d;
  ITSELF_1;
  if (opt_calltrace) {      
    coml_set_lockid_(&DRHOOK_lock);
    {
      const int ftnunitno = 0; /* stderr */
      const int print_option = 2; /* calling tree */
      int level = 0;
      c_drhook_print_(&ftnunitno, thread_id, &print_option, &level);
      /* fprintf(stderr,"%d#%d> %*.*s [%llu]\n",myproc,*thread_id,name_len,name_len,name,u.ull); */
    }
    coml_unset_lockid_(&DRHOOK_lock);
  }
  if (timeline_calls) {
    int tid = *thread_id;
    if (opt_timeline_thread <= 0 || tid <= opt_timeline_thread) {
      int id = 2 * (tid - 1);
      if ((timeline_calls[id]++)%opt_timeline_freq == 0) {
	coml_set_lockid_(&DRHOOK_lock);
	{
	  const int ftnunitno = opt_timeline_unitno;
	  const int print_option = 5; /* calling "tree" with just the current entry */
	  int level = 0;
	  c_drhook_print_(&ftnunitno, thread_id, &print_option, &level);
	}
	coml_unset_lockid_(&DRHOOK_lock);
      }
    } /* if (opt_timeline_thread <= 0 || tid <= opt_timeline_thread) */
  }
}

/*=== c_drhook_end_ ===*/

void 
c_drhook_end_(const char *name,
	      const int *thread_id,
	      const double *key,
	      const char *filename,
	      const int *sizeinfo
	      /* Hidden length */
	      ,int name_len, int filename_len)
{
  TIMERS;
  equivalence_t u;
  ITSELF_0;
  u.d = *key;
  /*
  if (opt_calltrace) {
    coml_set_lockid_(&DRHOOK_lock);
    fprintf(stderr,"%d#%d< %*.*s [%llu]\n",myproc,*thread_id,name_len,name_len,name,u.ull);
    coml_unset_lockid_(&DRHOOK_lock);
  }
  */
  if (timeline_calls) {
    int tid = *thread_id;
    if (opt_timeline_thread <= 0 || tid <= opt_timeline_thread) {
      int id = 2 * (tid - 1);
      if ((timeline_calls[id]++)%opt_timeline_freq == 0) {
	coml_set_lockid_(&DRHOOK_lock);
	{
	  const int ftnunitno = opt_timeline_unitno;
	  const int print_option = -5; /* calling "tree" with just the current entry */
	  int level = 0;
	  c_drhook_print_(&ftnunitno, thread_id, &print_option, &level);
	}
	coml_unset_lockid_(&DRHOOK_lock);
      }
    } /* if (opt_timeline_thread <= 0 || tid <= opt_timeline_thread) */
  }
  if (watch && watch_count > 0) check_watch("leaving", name, name_len);
  putkey(*thread_id, u.keyptr, name, name_len, 
	 *sizeinfo,
	 &walltime, &cputime);
  ITSELF_1;
}

/*=== c_drhook_memcounter_ ===*/

void
c_drhook_memcounter_(const int *thread_id,
		     const long long int *size,
		     long long int *keyptr_addr)
{
  if (opt_memprof) {
    if (size) {
      union {
	long long int keyptr_addr;
	drhook_key_t *keyptr;
      } u;
      long long int alldelta;
      int tid = (thread_id && (*thread_id >= 1) && (*thread_id <= numthreads))
	? *thread_id : get_thread_id_();
      tid--;
      if (*size > 0) { /* Memory is being allocated */
	if (curkeyptr[tid]) {
	  drhook_key_t *keyptr = curkeyptr[tid];
	  keyptr->mem_curdelta += *size;
	  alldelta = keyptr->mem_curdelta + keyptr->mem_child;
	  if (alldelta > keyptr->maxmem_alldelta) keyptr->maxmem_alldelta = alldelta;
	  if (keyptr->mem_curdelta > keyptr->maxmem_selfdelta) 
	    keyptr->maxmem_selfdelta = keyptr->mem_curdelta;
	  if (keyptr_addr) {
	    u.keyptr = keyptr;
	    *keyptr_addr = u.keyptr_addr;
	  }
	  keyptr->alloc_count++;
	}
	else {
	  if (keyptr_addr) *keyptr_addr = 0;
	} /* if (curkeyptr[tid]) */
	/*
	fprintf(stderr,
		"memcounter: allocated %lld bytes ; *keyptr_addr = %lld\n",
		*size, *keyptr_addr);
	 */
      }
      else { /* Memory is being freed */
	drhook_key_t *keyptr;
	if (keyptr_addr && (*keyptr_addr)) {
	  u.keyptr_addr = *keyptr_addr;
	  keyptr = u.keyptr;
	}
	else 
	  keyptr = curkeyptr[tid];
	/*
	fprintf(stderr,
		"memcounter: DE-allocated %lld bytes ; *keyptr_addr = %lld\n",
		*size, *keyptr_addr);
	 */
	if (keyptr) {
	  long long int prev_curdelta = keyptr->mem_curdelta;
	  keyptr->mem_curdelta += *size;
	  alldelta = prev_curdelta + keyptr->mem_child;
	  if (alldelta > keyptr->maxmem_alldelta) keyptr->maxmem_alldelta = alldelta;
	  if (*size < 0) keyptr->free_count++;
	} /* if (keyptr) */
      }
    } /* if (size) */
  }
}

/*=== c_drhook_print_ ===*/

#define PRINT_HWM() \
if (opt_gethwm) { sprintf(s,",hwm=%lldK",keyptr->hwm/1024); s += strlen(s); }

#define PRINT_RSS() \
if (opt_getrss) { \
  sprintf(s,",rss/max=%lldK/%lldK",keyptr->rssnow/1024, keyptr->maxrss/1024); \
  s += strlen(s); \
}

#define PRINT_STK() \
if (opt_getstk) { \
  sprintf(s,",stack/max=%lldK/%lldK",keyptr->stack/1024, keyptr->maxstack/1024); \
  s += strlen(s); \
}

#define PRINT_PAG() \
if (opt_getpag) { \
  sprintf(s,",pag=%lld",keyptr->paging); \
  s += strlen(s); \
}

#define PRINT_WALL() \
if (opt_walltime) { \
  double self = keyptr->delta_wall_all-keyptr->delta_wall_child; \
  if (self < 0) self = 0; \
  sprintf(s,",wall=%.3fs/%.3fs", \
	  keyptr->delta_wall_all, self); \
  s += strlen(s); \
}

#define PRINT_CPU() \
if (opt_cputime) { \
  double self = keyptr->delta_cpu_all-keyptr->delta_cpu_child; \
  if (self < 0) self = 0; \
  sprintf(s,",cpu=%.3fs/%.3fs", \
	  keyptr->delta_cpu_all, self); \
  s += strlen(s); \
}

#define PRINT_CALLS() \
if (opt_calls) { \
  sprintf(s,",#%llu,st=%d",keyptr->calls,keyptr->status); \
  s += strlen(s); \
}

static int
prof_name_comp(const void *v1, const void *v2)
{
  const drhook_prof_t *p1 = v1;
  const drhook_prof_t *p2 = v2;
  return strcmp(p1->name,p2->name);
}

static int
memprof_name_comp(const void *v1, const void *v2)
{
  const drhook_memprof_t *p1 = v1;
  const drhook_memprof_t *p2 = v2;
  return strcmp(p1->name,p2->name);
}

static int
prof_pc_comp_desc(const void *v1, const void *v2)
{
  const drhook_prof_t *p1 = v1;
  const drhook_prof_t *p2 = v2;
  if (p1->pc < p2->pc) return 1;
  else if (p1->pc > p2->pc) return -1;
  else return 0;
}

static int
memprof_pc_comp_desc(const void *v1, const void *v2)
{
  const drhook_memprof_t *p1 = v1;
  const drhook_memprof_t *p2 = v2;
  if (p1->pc < p2->pc) return 1;
  else if (p1->pc > p2->pc) return -1;
  else return 0;
}

static const char *
trim_and_adjust_left(const char *p, int *name_len)
{
  int len = strlen(p);
  if (len > 0) {
    const char *back = &p[len-1];
    while (len > 0 && *back-- == ' ') len--;
    while (len > 0 && *p == ' ') { p++; len--; }
  }
  if (name_len) *name_len = len;
  return p;
}

#define print_routine_name(fp, p, len, cluster_size) \
  if (fp && p) { \
    int name_len = 0; \
    const char *name = trim_and_adjust_left(p->name,&name_len); \
    fprintf(fp,"%.*s@%d%s%s", \
	    name_len, name, \
	    p->tid, \
	    p->filename ? ":" : "", \
	    p->filename ? p->filename : ""); \
    \
    if (opt_clusterinfo) { \
      fprintf(fp," [%d,%d]", \
	      p->cluster, ABS(cluster_size)); \
    } \
    \
    unroll_callpath(fp, len, p->callpath, p->callpath_len); \
  } /* if (fp && p) */


void 
c_drhook_print_(const int *ftnunitno,
		const int *thread_id,
		const int *print_option, /* 
					    1=raw call counts 
					    2=calling tree
					    3=profiling info
					    4=memory profiling
					    5=timeline upon entering the routine
					   -5=timeline upon leaving the routine
					 */
		int *level
		)
{
  int tid = (thread_id && (*thread_id > 0)) ? *thread_id : get_thread_id_();
  if (keydata && calltree && tid >= 1 && tid <= numthreads) {
    char line[4096];
    int j;

    if (*print_option == 1) { /* raw call counts */
      for (j=0; j<hashsize; j++) {
	int nestlevel = 0;
	drhook_key_t *keyptr = &keydata[tid-1][j];
	while (keyptr) {
	  if (keyptr->name) {
	    char *s = line;
	    sprintf(s,
		    "[myproc#%d,tid#%d,pid#%d,hash#%d,nest=%d]: '%s'",
		    myproc,tid,pid,j,nestlevel,keyptr->name);
	    s += strlen(s);
	    PRINT_CALLS();
	    PRINT_HWM();
	    PRINT_RSS();
	    PRINT_STK();
	    PRINT_PAG();
	    PRINT_WALL();
	    PRINT_CPU();
	    *s = 0;
	    dr_hook_prt_(ftnunitno, line, strlen(line));
	  }
	  keyptr = keyptr->next;
	  nestlevel++;
	} /* while (keyptr) */
      } /* for (j=0; j<hashsize; j++) */
    }

    else if (*print_option == 2 || ABS(*print_option) == 5) { /* current calling tree */
      drhook_calltree_t *treeptr = calltree[tid-1];
      if (tid > 1 && *print_option == 2) { 
	/* I'm not master thread, but my master has the beginning of the calltree */
	int initlev = 0;
	const int master = 1;
	c_drhook_print_(ftnunitno, &master, print_option, &initlev);
	*level += initlev;
      }
      while (treeptr && treeptr->active) {
	int do_print = (*print_option == 2 || 
			(ABS(*print_option) == 5 && 
			((!treeptr->next) || (treeptr->next && !treeptr->next->active))));
	if (do_print) {
	  drhook_key_t *keyptr = treeptr->keyptr;
	  char *s = line;
	  char kind = (*print_option == 2) ? ':' :
	    ((*print_option == -5) ? '<' : '>');
	  sprintf(s,"[myproc#%d,tid#%d,pid#%d]%c ",myproc,tid,pid,kind);
	  s += strlen(s);
	  (*level)++;
	  for (j=0; j<(*level); j++) *s++ = ' ';
	  if (*print_option == 2) {
	    sprintf(s,"%s ",keyptr->name);
	    s += strlen(s);
	  }
	  if (ABS(*print_option) == 5) {
	    if (opt_timeline_format == 1) {
	      sprintf(s,
		      "%.4g %.4g %.4g %.4g %.4g",
		      WALLTIME(), 
		      (double)(gethwm_()/1048576.0), (double)(getrss_()/1048576.0), 
		      (opt_timeline_thread == 1 && tid == 1) ? 
		      (double)(getcurheap_()/1048576.0) : (double)(getcurheap_thread_(&tid)/1048576.0),
		      (double)(getstk_()/1048576.0));
	    }
	    else {
	      sprintf(s,
		      "wall=%.4g cpu=%.4g hwm=%.4g rss=%.4g curheap=%.4g stack=%.4g pag=%lld",
		      WALLTIME(), CPUTIME(),
		      (double)(gethwm_()/1048576.0), (double)(getrss_()/1048576.0), 
		      (opt_timeline_thread == 1 && tid == 1) ? 
		      (double)(getcurheap_()/1048576.0) : (double)(getcurheap_thread_(&tid)/1048576.0),
		      (double)(getstk_()/1048576.0),
		      getpag_());
	    }
	    s += strlen(s);
	    *s++ = ' ';
	    sprintf(s,"'%s'",keyptr->name);
	    s += strlen(s);
	  }
	  else {
	    PRINT_CALLS();
	    PRINT_HWM();
	    PRINT_RSS();
	    PRINT_STK();
	    PRINT_PAG();
	    PRINT_WALL();
	    PRINT_CPU();
	  }
	  *s = 0;
	  dr_hook_prt_(ftnunitno, line, strlen(line));
	}
	treeptr = treeptr->next;
      } /* while (treeptr && treeptr->active) */
    }

    else if (*print_option == 3) { /* profiling (CPU, wall-clock and/or MFlop/s) */
      int len;
      int t;
      double cumul;
      double tottime = 0, max_overhead_pc = 0;
      double *tot = NULL;
      int nprof = 0;
      drhook_prof_t *prof = NULL;
      drhook_prof_t *p;
      double flop_tot = 0, instr_tot = 0;
      double *flop = NULL, *instr = NULL;

      if (!opt_wallprof && !opt_cpuprof) return; /* no profiling info available */
      if (tid > 1) return; /* just master thread allowed ; takes care of siblings, too */
      if (numthreads<=0) return;
      if (do_prof_off) return;
      do_prof_off = 1;

      /* Insert "$drhook" */
      if (keyself && opt_self > 1) {
	for (t=0; t<numthreads; t++) (void) insertkey(t+1,keyself[t]);
      }

      flop = calloc_drhook(numthreads, sizeof(*flop));
      instr = calloc_drhook(numthreads, sizeof(*instr));
      tot = calloc_drhook(numthreads, sizeof(*tot));

      for (t=0; t<numthreads; t++) {
	for (j=0; j<hashsize; j++) {
	  drhook_key_t *keyptr = &keydata[t][j];
	  while (keyptr) {
	    if (keyptr->name && (keyptr->status == 0 || signal_handler_called)) {
	      double self;
	      if (opt_wallprof) {
		self = keyptr->delta_wall_all - keyptr->delta_wall_child;
	      }
	      else {
		self = keyptr->delta_cpu_all - keyptr->delta_cpu_child;
	      }
	      /* if (self < 0) self = 0; */
	      tot[t] += self;
#ifdef HPM
	      flop[t] += keyptr->avg_mflops * self; /* mflop_count(keyptr); */
	      instr[t] += keyptr->avg_mipsrate * self; /* mip_count(keyptr); */
#endif
	      nprof++;
	    }
	    keyptr = keyptr->next;
	  } /* while (keyptr && keyptr->status == 0) */
	} /* for (t=0; t<numthreads; t++) */
      } /* for (j=0; j<hashsize; j++) */

      if (opt_wallprof) { /* a bit unreliable; had not taken max. value of threads wall yet; will be recalculated */
	tottime = tot[0] + ((keyself && opt_self > 1) ? keyself[0]->delta_wall_all : 0);
	for (t=1; t<numthreads; t++) {
	  double tmp = tot[t] + ((keyself && opt_self > 1) ? keyself[t]->delta_wall_all : 0);
	  tottime = MAX(tottime,tmp);
	}
      }
      else { /* ok & reliable (for cpuprof) */
	tottime = 0;
	for (t=0; t<numthreads; t++) tottime += (tot[t] + ((keyself && opt_self > 1) ? keyself[t]->delta_cpu_all : 0));
      }

      if (tottime <= 0) tottime = 1e-10;

      p = prof = calloc_drhook(nprof + 1, sizeof(*prof)); /* Make sure there is at least one entry */

      for (t=0; t<numthreads; t++) {
	for (j=0; j<hashsize; j++) {
	  drhook_key_t *keyptr = &keydata[t][j];
	  while (keyptr) {
	    if (keyptr->name && (keyptr->status == 0 || signal_handler_called)) {
	      p->self = opt_wallprof ?
		keyptr->delta_wall_all - keyptr->delta_wall_child :
		keyptr->delta_cpu_all - keyptr->delta_cpu_child;
	      p->total = opt_wallprof ?
		keyptr->delta_wall_all :
		keyptr->delta_cpu_all;
	      p->calls = keyptr->calls;
	      p->name = keyptr->name;
	      p->pc = (p->self/tottime) * 100.0;
	      if (p->calls > 0) {
		p->percall_ms_self = (p->self/p->calls) * 1000.0;
		p->percall_ms_total = (p->total/p->calls) * 1000.0;
	      }
	      p->tid = t+1;
	      p->index = p - prof;
#ifdef HPM
	      if (opt_hpmprof) {
		p->mflops = keyptr->avg_mflops; /* mflops_hpm(keyptr); */
		p->mipsrate = keyptr->avg_mipsrate; /* mips_hpm(keyptr); */
		p->divpc = divpc_hpm(keyptr);
	      }
#endif
	      p->filename = keyptr->filename;
	      p->sizeinfo = keyptr->sizeinfo;
	      p->sizespeed = (p->self > 0 && p->sizeinfo > 0) ? p->sizeinfo/p->self : 0;
	      p->sizeavg = (p->calls > 0 && p->sizeinfo > 0) ? p->sizeinfo/p->calls : 0;
	      p->callpath = keyptr->callpath;
	      p->callpath_len = keyptr->callpath_len;
	      p++;
	    }
	    keyptr = keyptr->next;
	  } /* while (keyptr && keyptr->status == 0) */
	} /* for (t=0; t<numthreads; t++) */
      } /* for (j=0; j<hashsize; j++) */

      do {
	double mflop_rate = 0;
	double mip_rate = 0;
	int numroutines = 0;
	int cluster;
	double *maxval = calloc_drhook(nprof+1, sizeof(*maxval)); /* make sure at least 1 element */
	int *clusize = calloc_drhook(nprof+1, sizeof(*clusize)); /* make sure at least 1 element */
	char *prevname = NULL;
	const char *fmt1 = "%5d %8.2f %12.3f %12.3f %12.3f %14llu %11.2f %11.2f   %s";
	const char *fmt2 = "%5d %8.2f %12.3f %12.3f %12.3f %14llu %7.0f %7.0f %7.1f   %s";
	const char *fmt = opt_hpmprof ? fmt2 : fmt1;
	char *filename = get_mon_out(myproc);
	FILE *fp = NULL;

	if (!filename) break;

	if ((myproc == 1 && mon_out_procs == -1) || mon_out_procs == myproc) {
	  fprintf(stderr,"Writing profiling information of proc#%d into file '%s'\n",myproc,filename);
	}

	fp = fopen(filename,"w");
	if (!fp) goto finish_3;
	
	/* alphanumerical sorting to find out clusters of the same routine but on different threads */
	/* also find out total wall clock time */
	/* calculate percentage values */
	
	p = prof;
	qsort(p, nprof, sizeof(*p), prof_name_comp);

	cluster = 0;
	maxval[cluster] = p->self;
	p->maxval = &maxval[cluster];
	clusize[cluster] = 1;
	prevname = p->name;
	p++;
	for (j=1; j<nprof; j++) {
	  if (!strequ(prevname,p->name)) {
	    (p-1)->cluster = cluster;
	    (p-1)->maxval = &maxval[cluster];
	    prevname = p->name;
	    cluster++;
	  }
	  if (p->self > maxval[cluster]) maxval[cluster] = p->self;
	  p->cluster = cluster;
	  p->maxval = &maxval[cluster];
	  clusize[cluster]++;
	  p++;
	} /* for (j=1; j<nprof; j++) */

	numroutines = (nprof > 0) ? (cluster + 1) : 0; /* Active no. of routines */

	if (opt_wallprof) tottime = 0;
	p = prof;
	for (j=0; j<nprof; j++) {
	  int use_this = 0;
	  cluster = p->cluster;
	  if (clusize[cluster] > 1) { /* multiple threads <= numthreads indeed called this routine */
	    p->is_max = (p->self == *p->maxval);
	    if (p->is_max) { /* first max found will be used for total time */
	      clusize[cluster] = -clusize[cluster]; /* ensures that max has been found for this cluster */
	      use_this = opt_wallprof;
	    }
	  }
	  else if (clusize[cluster] == 1) {
	    use_this = opt_wallprof;
	  }
	  if (use_this && opt_wallprof) tottime += p->self;
	  p++;
	}

        if (tottime <= 0) tottime = 1e-10;

	if (opt_wallprof) { /* use re-calculated tottime to define percentages */
	  p = prof;
	  for (j=0; j<nprof; j++) {
	    p->pc = (p->self/tottime) * 100.0;
	    p++;
	  }
	}

	/* sorting with respect to percentage value */

	p = prof;
	qsort(p, nprof, sizeof(*p), prof_pc_comp_desc);

	flop_tot = 0;
	instr_tot = 0;
	max_overhead_pc = 0;
	for (t=0; t<numthreads; t++) {
	  flop_tot += flop[t];
	  instr_tot += instr[t];
	  if (overhead) {
	    max_overhead_pc = MAX(max_overhead_pc,overhead[t]);
#ifdef DEBUG
	    fprintf(fp,"tid#%d: overhead = %.15g s\n",t+1,overhead[t]);
#endif
	  }
	}
#ifdef DEBUG
	fprintf(fp,"max overhead = %.15g s, tottime = %.15g s\n",
		max_overhead_pc, tottime);
#endif
	if (tottime - max_overhead_pc > 0) {
	  max_overhead_pc = 100.0*(max_overhead_pc/(tottime - max_overhead_pc));
	}
	else {
	  max_overhead_pc = 100;
	}

	fprintf(fp,
		"Profiling information for program='%s', proc#%d:\n",a_out, myproc);
	fprintf(fp,"\tNo. of instrumented routines called : %d\n", numroutines);
	fprintf(fp,"\tInstrumentation started : %s\n",start_stamp ? start_stamp : "N/A");
	end_stamp = timestamp();
	fprintf(fp,"\tInstrumentation   ended : %s\n",end_stamp ? end_stamp : "N/A");
	fprintf(fp,"\tInstrumentation overhead: %.2f%%\n",max_overhead_pc);
	{
	  long long int hwm = gethwm_()/1048576;
	  long long int rss = getrss_()/1048576;
	  long long int maxstack = getmaxstk_()/1048576;
	  long long int pag = getpag_();
	  fprintf(fp,
	  "\tMemory usage : %lld MBytes (heap), %lld MBytes (rss), %lld MBytes (stack), %lld (paging)\n",
		  hwm,rss,maxstack,pag);
	}
	if (opt_hpmprof) {
	  mflop_rate = flop_tot / tottime;
	  mip_rate = instr_tot / tottime;
	  fprintf(fp,
		  "\t%s-time is %.2f sec on proc#%d, %.0f MFlops (ops#%.0f*10^6), %.0f MIPS (ops#%.0f*10^6) (%d procs, %d threads)\n",
		  opt_wallprof ? "Wall" : "Total CPU", tottime, myproc,
		  mflop_rate, flop_tot, mip_rate, instr_tot,
		  nproc, numthreads);
	}
	else {
	  fprintf(fp,
		  "\t%s-time is %.2f sec on proc#%d (%d procs, %d threads)\n",
		  opt_wallprof ? "Wall" : "Total CPU", tottime, myproc,
		  nproc, numthreads);
	}

	if (myproc == 1) {
	  fprintf(stderr,
		  "Profiling information for program='%s', proc#%d:\n",a_out, myproc);
	  fprintf(stderr,"\tNo. of instrumented routines called : %d\n", numroutines);
	  fprintf(stderr,"\tInstrumentation started : %s\n",start_stamp ? start_stamp : "N/A");
	  fprintf(stderr,"\tInstrumentation   ended : %s\n",end_stamp ? end_stamp : "N/A");
	  fprintf(stderr,"\tInstrumentation overhead: %.2f%%\n",max_overhead_pc);
	  if (opt_hpmprof) {
	    fprintf(stderr,
		  "\t%s-time is %.2f sec on proc#%d, %.0f MFlops (ops#%.0f*10^6), %.0f MIPS (ops#%.0f*10^6) (%d procs, %d threads)\n",
		  opt_wallprof ? "Wall" : "Total CPU", tottime, myproc,
		  mflop_rate, flop_tot, mip_rate, instr_tot,
		  nproc, numthreads);
	  }
	  else {
	    fprintf(stderr,
		    "\t%s-time is %.2f sec on proc#%d (%d procs, %d threads)\n",
		    opt_wallprof ? "Wall" : "Total CPU", tottime, myproc,
		    nproc, numthreads);
	  }
	} /* if (myproc == 1) */

	free_drhook(end_stamp);

	for (t=0; t<numthreads; t++) {
	  double tmp = 100.0*(tot[t]/tottime);
	  if (opt_hpmprof && tot[t] > 0) {
	    mflop_rate = flop[t]/tot[t];
	    mip_rate = instr[t]/tot[t];
	  }
	  else {
	    mflop_rate = 0;
	    mip_rate = 0;
	  }
	  fprintf(    fp,"\tThread#%d: %11.2f sec (%.2f%%)",t+1,tot[t],tmp);
	  if (opt_hpmprof) fprintf(    fp,", %.0f MFlops (ops#%.0f*10^6), %.0f MIPS (ops#%.0f*10^6)", mflop_rate, flop[t], mip_rate, instr[t]);
	  fprintf(    fp,"\n");
	  if (myproc == 1) {
	    fprintf(stderr,"\tThread#%d: %11.2f sec (%.2f%%)",t+1,tot[t],tmp);
	    if (opt_hpmprof) fprintf(stderr,", %.0f MFlops (ops#%.0f*10^6), %.0f MIPS (ops#%.0f*10^6)", mflop_rate, flop[t], mip_rate, instr[t]);
	    fprintf(stderr,"\n");
	  }
	}

	fprintf(fp,"\n");
	if (opt_hpmprof) {
	  len = 
	    fprintf(fp,"    #  %% Time         Cumul         Self        Total     # of calls    MIPS  MFlops   Div-%%    ");
	}
	else {
	  len = 
	    fprintf(fp,"    #  %% Time         Cumul         Self        Total     # of calls        Self       Total    ");
	}
	fprintf(fp,"Routine@<thread-id>");
	if (opt_clusterinfo) fprintf(fp," [Cluster:(id,size)]");
	fprintf(fp,"\n");
	if (opt_sizeinfo) fprintf(fp,"%*s %s\n",len," ","(Size; Size/sec; AvgSize/call)");
	if (opt_hpmprof) {
	  fprintf(fp,  "        (self)        (sec)        (sec)        (sec)                                       \n");
	}
	else {
	  fprintf(fp,  "        (self)        (sec)        (sec)        (sec)                    ms/call     ms/call\n");
	}
	fprintf(fp,"\n");

	cumul = 0;
	for (j=0; j<nprof; ) {
	  int cluster_size = clusize[p->cluster];
	  if (p->pc < percent_limit) break;
	  if (opt_cputime) {
	    cumul += p->self;
	  }
	  else {
	    if (p->is_max || cluster_size == 1) cumul += p->self;
	  }
	  if (opt_hpmprof) {
	    fprintf(fp, fmt,
		    ++j, p->pc, cumul, p->self, p->total, p->calls,
		    p->mipsrate, p->mflops, p->divpc,
		    p->is_max ? "*" : " ");
	  }
	  else {
	    fprintf(fp, fmt,
		    ++j, p->pc, cumul, p->self, p->total, p->calls,
		    p->percall_ms_self, p->percall_ms_total, 
		    p->is_max ? "*" : " ");
	  }

	  print_routine_name(fp, p, len, cluster_size);
	    
	  if (opt_sizeinfo && p->sizeinfo > 0) {
	    char s1[DRHOOK_STRBUF], s2[DRHOOK_STRBUF], s3[DRHOOK_STRBUF];
	    lld_commie(p->sizeinfo,s1);
	    dbl_commie(p->sizespeed,s2);
	    dbl_commie(p->sizeavg,s3);
	    fprintf(fp,"\n%*s (%s  %s  %s)",len-10," ",s1,s2,s3);
	  }
	  fprintf(fp,"\n");
	  p++;
	} /* for (j=0; j<nprof; ) */
	
	fclose(fp);
      finish_3:
	free_drhook(filename);
	free_drhook(maxval);
	free_drhook(clusize);
      } while (0);

      free_drhook(instr);
      free_drhook(flop);
      free_drhook(tot);
      free_drhook(prof);
      do_prof_off = 0;
    }

    else if (*print_option == 4) { /* Memory profiling */
      int t, len;
      int nprof = 0;
      drhook_memprof_t *prof = NULL;
      drhook_memprof_t *p;
      long long int *tot;
      long long int *maxseen_tot;
      double totmaxmem_delta;

      if (!opt_memprof) return; /* no profiling info available */
      if (tid > 1) return; /* just master thread allowed ; takes care of siblings, too */
      if (numthreads<=0) return;
      if (do_prof_off) return;
      do_prof_off = 1;

      tot = calloc_drhook(numthreads, sizeof(*tot));
      maxseen_tot = calloc_drhook(numthreads, sizeof(*maxseen_tot));

      for (t=0; t<numthreads; t++) {
	for (j=0; j<hashsize; j++) {
	  drhook_key_t *keyptr = &keydata[t][j];
	  while (keyptr) {
	    if (keyptr->name && (keyptr->status == 0 || signal_handler_called)) {

	      long long int self;
	      self = keyptr->maxmem_selfdelta;
	      if (self < 0) self = 0;
	      tot[t] += self;
	      maxseen_tot[t] = MAX(maxseen_tot[t], keyptr->mem_seenmax);
	      nprof++;
	    }
	    keyptr = keyptr->next;
	  } /* while (keyptr && keyptr->status == 0) */
	} /* for (t=0; t<numthreads; t++) */
      } /* for (j=0; j<hashsize; j++) */

      totmaxmem_delta = tot[0];
      for (t=1; t<numthreads; t++) {
	long long int tmp = tot[t];
	totmaxmem_delta = MAX(totmaxmem_delta,tmp);
      }

      if (totmaxmem_delta <= 0) totmaxmem_delta = 1e-10; /* To avoid divide-by-zero */

      p = prof = calloc_drhook(nprof + 1, sizeof(*prof)); /* Make sure there is at least one entry */

      for (t=0; t<numthreads; t++) {
	for (j=0; j<hashsize; j++) {
	  drhook_key_t *keyptr = &keydata[t][j];
	  while (keyptr) {
	    if (keyptr->name && (keyptr->status == 0 || signal_handler_called)) {
	      p->self = keyptr->maxmem_selfdelta;
	      p->children = keyptr->mem_child;
	      p->hwm = keyptr->mem_maxhwm;
	      p->rss = keyptr->mem_maxrss;
	      p->stk = keyptr->mem_maxstk;
	      p->pag = keyptr->mem_maxpagdelta;
	      p->leaked = keyptr->mem_curdelta;
	      p->calls = keyptr->calls;
	      p->alloc_count += keyptr->alloc_count;
	      p->free_count += keyptr->free_count;
	      p->name = keyptr->name;
	      p->pc = (p->self/totmaxmem_delta) * 100.0;
	      p->tid = t+1;
	      p->index = p - prof;
	      p->filename = keyptr->filename;
	      p->callpath = keyptr->callpath;
	      p->callpath_len = keyptr->callpath_len;
	      p++;
	    }
	    keyptr = keyptr->next;
	  } /* while (keyptr && keyptr->status == 0) */
	} /* for (t=0; t<numthreads; t++) */
      } /* for (j=0; j<hashsize; j++) */

      do {
	int numroutines = 0;
	int cluster;
	long long int *maxval = calloc_drhook(nprof+1, sizeof(*maxval)); /* make sure at least 1 element */
	int *clusize = calloc_drhook(nprof+1, sizeof(*clusize)); /* make sure at least 1 element */
	char *prevname = NULL;
	const char *fmt1 = "%5d %9.2f  %14lld %14lld %14lld %14lld %14lld %10lld %10llu %10llu%s%10llu   %s";
	const char *fmt = fmt1;
	char *filename = get_memmon_out(myproc);
	FILE *fp = NULL;

	if (!filename) break;

	if ((myproc == 1 && mon_out_procs == -1) || mon_out_procs == myproc) {
	  fprintf(stderr,"Writing memory-profiling information of proc#%d into file '%s'\n",myproc,filename);
	}

	fp = fopen(filename,"w");
	if (!fp) goto finish_4;
	
	/* alphanumerical sorting to find out clusters of the same routine but on different threads */
	
	p = prof;
	qsort(p, nprof, sizeof(*p), memprof_name_comp);

	cluster = 0;
	maxval[cluster] = p->self;
	p->maxval = &maxval[cluster];
	clusize[cluster] = 1;
	prevname = p->name;
	p++;
	for (j=1; j<nprof; j++) {
	  if (!strequ(prevname,p->name)) {
	    (p-1)->cluster = cluster;
	    (p-1)->maxval = &maxval[cluster];
	    prevname = p->name;
	    cluster++;
	  }
	  if (p->self > maxval[cluster]) maxval[cluster] = p->self;
	  p->cluster = cluster;
	  p->maxval = &maxval[cluster];
	  clusize[cluster]++;
	  p++;
	} /* for (j=1; j<nprof; j++) */

	numroutines = (nprof > 0) ? (cluster + 1) : 0; /* Active no. of routines */

	totmaxmem_delta = 0;
	p = prof;
	for (j=0; j<nprof; j++) {
	  int use_this = 0;
	  cluster = p->cluster;
	  if (clusize[cluster] > 1) { /* multiple threads <= numthreads indeed called this routine */
	    p->is_max = (p->self == *p->maxval);
	    if (p->is_max) { /* first max found will be used for total time */
	      clusize[cluster] = -clusize[cluster]; /* ensures that max has been found for this cluster */
	      use_this = 1;
	    }
	  }
	  else if (clusize[cluster] == 1) {
	    use_this = 1;
	  }
	  if (use_this) totmaxmem_delta += p->self;
	  p++;
	}

        if (totmaxmem_delta <= 0) totmaxmem_delta = 1e-10; /* To avoid divide-by-zero */

	/* use re-calculated totmaxmem_delta to define percentages */
	p = prof;
	for (j=0; j<nprof; j++) {
	  p->pc = (p->self/totmaxmem_delta) * 100.0;
	  p++;
	}

	/* sorting with respect to percentage value */

	p = prof;
	qsort(p, nprof, sizeof(*p), memprof_pc_comp_desc);

	fprintf(fp,
		"Memory-profiling information for program='%s', proc#%d:\n",a_out, myproc);
	fprintf(fp,"\tNo. of instrumented routines called : %d\n", numroutines);
	fprintf(fp,"\tInstrumentation started : %s\n",start_stamp ? start_stamp : "N/A");
	end_stamp = timestamp();
	fprintf(fp,"\tInstrumentation   ended : %s\n",end_stamp ? end_stamp : "N/A");
	{
	  long long int hwm = gethwm_()/1048576;
	  long long int rss = getrss_()/1048576;
	  long long int maxstack = getmaxstk_()/1048576;
	  long long int pag = getpag_();
	  long long int maxseen = 0;
	  long long int leaked = 0;
	  p = prof;
	  for (j=0; j<nprof; j++) {
	    if (p->leaked > 0) leaked += p->leaked;
	    p++;
	  }
	  for (t=0; t<numthreads; t++) {
	    maxseen += maxseen_tot[t];
	  }
	  maxseen /= 1048576;
	  leaked /= 1048576;
	  fprintf(fp,
	  "\tMemory usage : %lld MBytes (max.seen), %lld MBytes (leaked), %lld MBytes (heap), %lld MBytes (max.rss), %lld MBytes (max.stack), %lld (paging)\n",
		  maxseen,leaked,hwm,rss,maxstack,pag);
	  fprintf(fp,"\tNo. of procs/threads: %d procs, %d threads\n",nproc,numthreads);
	}

	if (myproc == 1) {
	  fprintf(stderr,
		  "Memory-profiling information for program='%s', proc#%d:\n",a_out, myproc);
	  fprintf(stderr,"\tNo. of instrumented routines called : %d\n", numroutines);
	  fprintf(stderr,"\tInstrumentation started : %s\n",start_stamp ? start_stamp : "N/A");
	  fprintf(stderr,"\tInstrumentation   ended : %s\n",end_stamp ? end_stamp : "N/A");
	} /* if (myproc == 1) */

	free_drhook(end_stamp);

	fprintf(fp,"\n");
	len = 
	  fprintf(fp,"    #  Memory-%%      Self-alloc     + Children    Self-Leaked          Heap       Max.Stack     Paging     #Calls    #Allocs     #Frees   ");
                   /*"12345-1234567899-12345678901234-12345678901234-12345678901234-12345678901234-12345678901234-12345678901234-12345678901234-123456789012-123456789012"*/
	fprintf(fp,"Routine@<thread-id>");
	if (opt_clusterinfo) fprintf(fp," [Cluster:(id,size)]");
	fprintf(fp,"\n");
	fprintf(fp,  "         (self)         (bytes)        (bytes)        (bytes)        (bytes)        (bytes)    (delta)");
                   /*"12345-1234567899-12345678901234-12345678901234-12345678901234-12345678901234-12345678901234-12345678901234-12345678901234-123456789012-123456789012"*/
	fprintf(fp,"\n");

	p = prof;
	for (j=0; j<nprof; ) {
	  int cluster_size = clusize[p->cluster];
	  if (p->pc < percent_limit) break;
	  t = p->tid - 1;
	  if (p->children > maxseen_tot[t]) p->children = maxseen_tot[t]; /* adjust */
	  fprintf(fp, fmt,
		  ++j, p->pc, 
		  p->self, p->children, p->leaked,
		  p->hwm, p->stk, p->pag,
		  p->calls, p->alloc_count, 
		  (p->alloc_count - p->free_count != 0) ? "*" : " ", p->free_count,
		  p->is_max ? "*" : " ");

	  print_routine_name(fp, p, len, cluster_size);

	  fprintf(fp,"\n");
	  p++;
	} /* for (j=0; j<nprof; ) */
	
	fclose(fp);
      finish_4:
	free_drhook(filename);
	free_drhook(maxval);
	free_drhook(clusize);
      } while (0);

      free_drhook(tot);
      free_drhook(maxseen_tot);
      free_drhook(prof);
      do_prof_off = 0;
    }
  }
}

/*=== c_drhook_init_signals_ ===*/

void
c_drhook_init_signals_(const int *enforce)
{
  signal_drhook_init(*enforce);
}

/*=== c_drhook_raise_ ===*/

/* 
   Just a convenience function for Fortran90 which may not have raise()-signal function
   CALL c_drhook_raise(10)  ! Raise signal#10
*/

void 
c_drhook_raise_(const int *sig) 
{ 
  fflush(NULL);
  raise(*sig);
} 

/**** C-interface to Dr.Hook ****/

void
Dr_Hook(const char *name, int option, double *handle, 
	const char *filename, int sizeinfo,
	int name_len, int filename_len)
{
  static int first_time = 1;
  static int value = 1; /* ON by default */
  if (first_time) { /* Not thread safe */
    extern void *cdrhookinit_(int *value); /* from ifsaux/support/cdrhookinit.F90 */
    cdrhookinit_(&value);
    first_time = 0;
  }
  if (value == 0) return; /* Immediate return if OFF */
  if (value != 0) {
    int tid = get_thread_id_();
    if (option == 0) {
      c_drhook_start_(name, &tid, handle, 
		      filename, &sizeinfo,
		      name_len > 0 ? name_len : strlen(name),
		      filename_len > 0 ? filename_len : strlen(filename));
    }
    else if (option == 1) {
      c_drhook_end_(name, &tid, handle, 
		    filename, &sizeinfo,
		    name_len > 0 ? name_len : strlen(name),
		    filename_len > 0 ? filename_len : strlen(filename));
    }
  }
}


/**** Interface to HPM ****/

/*<<< experimental >>>*/

#ifdef HPM

#ifdef RS6K
/**** Interface to HPM (RS6K) ****/

#include <pmapi.h>
#include <pthread.h>

static pthread_mutex_t hpm_lock = PTHREAD_MUTEX_INITIALIZER;

static int *hpm_tid_init = NULL;
static double cycles = 1300000000.0; /* 1.3GHz ; changed via pm_cycles() in init_hpm() */

#define MCYCLES (cycles * 1e-6)

#define TEST_PM_ERROR(name, rc) \
  if (rc != 0) { \
    fprintf(stderr,"PM_ERROR(tid#%d, pthread_self()=%d): rc=%d at %s(), line=%d, file=%s\n",\
	    tid+1,pthread_self(),rc,name,__LINE__,__FILE__); \
    pm_error((char *)name, rc); \
    sleep(tid+1); \
    RAISE(SIGABRT); \
  }

static void
init_hpm(int tid)
{
  const char *name = "init_hpm";
  int rc;

  --tid;

  if (!hpm_tid_init) {
    hpm_tid_init = calloc_drhook(numthreads, sizeof(*hpm_tid_init));
    cycles = pm_cycles();
  }

  if (!hpm_tid_init[tid]) {
#ifdef PMAPI_POST_P4
    pm_info2_t pminfo;
#else
    pm_info_t pminfo;
#endif
    pm_groups_info_t pmgroupsinfo;
    
    /*------------------------------------*/
    /* initialize the performance monitor */
    /*------------------------------------*/
#ifdef PMAPI_POST_P4
    rc = pm_initialize(PM_VERIFIED | PM_UNVERIFIED | PM_CAVEAT | PM_GET_GROUPS, 
		 &pminfo, &pmgroupsinfo, PM_CURRENT);
#else
    rc = pm_init(PM_VERIFIED | PM_UNVERIFIED | PM_CAVEAT | PM_GET_GROUPS, 
		 &pminfo, &pmgroupsinfo);
#endif
    TEST_PM_ERROR((char *)name, rc);

    if (myproc <= 1) fprintf(stderr,
			     ">>>pm_init() for ECMWF/OpenMP-tid#%d, pthread_self()=%d\n",
			     tid+1,pthread_self());
  }

  if (!hpm_tid_init[tid]) {
#if defined(PMAPI_P5_PLUS)
    /* IBM Power 5+ specific */
    const int group = 150; /* pm_hpmcount2 */
    /*-- counters -- (from John Hague, IBM/UK, 22-Aug-2006 : Thanx!!)
     case 150:
       strcpy(group_label, "pm_flop, Floating point operations");
       strcpy(label[0], "FPU executed FDIV instruction (PM_FPU_FDIV)");
       strcpy(label[1], "FPU executed multiply-add instruction (PM_FPU_FMA)");
       strcpy(label[2], "FPU executed FSQRT instruction (PM_FPU_SQRT)");
       strcpy(label[3], "FPU executed one flop instruction (PM_FPU_1FLOP)");
       strcpy(label[4], "Run instructions completed(PM_RUN_INST_CMPL)");
       strcpy(label[5], "Run cycles (PM_RUN_CYC)");
       strcpy(label[6], "Nothing");
       strcpy(label[7], "Nothing");
    */
#else
    const int group = 60; /* pm_hpmcount2 */
    /*-- counters --
     case 60:
       strcpy(group_label, "pm_hpmcount2, Hpmcount group for computation intensity analysis");
       strcpy(label[0], "FPU executed FDIV instruction (PM_FPU_FDIV)");
       strcpy(label[1], "FPU executed multiply-add instruction (PM_FPU_FMA)");
       strcpy(label[2], "FPU0 produced a result (PM_FPU0_FIN)");
       strcpy(label[3], "FPU1 produced a result (PM_FPU1_FIN)");
       strcpy(label[4], "Processor cycles (PM_CYC)");
       strcpy(label[5], "FPU executed store instruction (PM_FPU_STF)");
       strcpy(label[6], "Instructions completed (PM_INST_CMPL)");
       strcpy(label[7], "LSU executed Floating Point load instruction (PM_LSU_LDF)");
    */
#endif

    pm_prog_t pmprog;
    pm_data_t pmdata;
    int i;

    /*---------------------*/
    /* set a default group */
    /*---------------------*/
    for (i=0; i<MAX_COUNTERS; i++) {
      pmprog.events[i] = COUNT_NOTHING;
    }
    pmprog.events[0] = group;
    
    /*-------------------------------------------------------------*/
    /* set the mode for user (not kernel) and thread (not process) */
    /*-------------------------------------------------------------*/
    pmprog.mode.w = 0;
    pmprog.mode.b.user = 1;
    pmprog.mode.b.process = 0;
    /* pmprog.mode.b.process = 1; */
    
    /*------------------------------------------*/
    /* for power-4 you have to use event groups */
    /*------------------------------------------*/
    pmprog.mode.b.is_group = 1;
    
    /*---------------------------------------------------*/
    /* set the mode to not to start counting immediately */
    /*---------------------------------------------------*/
    /* pmprog.mode.b.count = 1; */
    pmprog.mode.b.count = 0;
    
    /*-----------------------------------------*/
    /* initialize the group and start counting */
    /*-----------------------------------------*/
    hpm_tid_init[tid] = pthread_self(); /* Always > 0 */

    rc = pm_set_program_mythread(&pmprog); 
    TEST_PM_ERROR((char *)name, rc);

    rc = pm_start_mythread();
    TEST_PM_ERROR((char *)name, rc);
  }
}

static void
stop_only_hpm(int tid, drhook_key_t *pstop) 
{
  const char *name = "stop_only_hpm";
  pm_data_t pmdata;
  int i, rc;

  /* if (numthreads > 1) pthread_mutex_lock(&hpm_lock); */

  if (!hpm_tid_init || !hpm_tid_init[tid-1]) init_hpm(tid);
  --tid;

  /*
  rc = pm_stop_mythread();
  TEST_PM_ERROR((char *)name, rc);
  */

  if (pstop && !pstop->counter_stopped) {
    rc = pm_get_data_mythread(&pmdata);
    TEST_PM_ERROR((char *)name, rc);
    
    if (pstop && pstop->counter_in && !pstop->counter_stopped) {
      for (i=0; i<MAX_COUNTERS; i++) {
	pstop->counter_sum[i] += (pmdata.accu[i] - pstop->counter_in[i]);
      }
      pstop->counter_stopped = 1;
    }
  }

  /*
  rc = pm_start_mythread();
  TEST_PM_ERROR((char *)name, rc);
  */

  /* if (numthreads > 1) pthread_mutex_unlock(&hpm_lock); */
}

static void
stopstart_hpm(int tid, drhook_key_t *pstop, drhook_key_t *pstart)
{
  const char *name = "stopstart_hpm";
  pm_data_t pmdata;
  int i, rc;

  /* if (numthreads > 1) pthread_mutex_lock(&hpm_lock); */

  if (!hpm_tid_init || !hpm_tid_init[tid-1]) init_hpm(tid);
  --tid;

  /*
  rc = pm_stop_mythread();
  TEST_PM_ERROR((char *)name, rc);
  */

  rc = pm_get_data_mythread(&pmdata);
  TEST_PM_ERROR((char *)name, rc);

  if (pstop && pstop->counter_in && !pstop->counter_stopped) {
    for (i=0; i<MAX_COUNTERS; i++) {
      pstop->counter_sum[i] += (pmdata.accu[i] - pstop->counter_in[i]);
    }
    pstop->counter_stopped = 1;
  }

  if (pstart) {
    if (!pstart->counter_in ) pstart->counter_in  = calloc_drhook(MAX_COUNTERS, sizeof(*pstart->counter_in ));
    if (!pstart->counter_sum) pstart->counter_sum = calloc_drhook(MAX_COUNTERS, sizeof(*pstart->counter_sum));
     for (i=0; i<MAX_COUNTERS; i++) {
       pstart->counter_in[i] = pmdata.accu[i];
     }
     pstart->counter_stopped = 0;
  }

  /*
  rc = pm_start_mythread();
  TEST_PM_ERROR((char *)name, rc);
  */

  /* if (numthreads > 1) pthread_mutex_unlock(&hpm_lock); */
}

#else

/**** Interface to HPM (CRAY SV2, XD1 and XT3) ****/

static int *hpm_tid_init = NULL;
static double cycles = 0;

#define MCYCLES (cycles * 1e-6)

#define TEST_PM_ERROR(name, rc) \
  if (rc != 0) { \
    fprintf(stderr,"PM_ERROR(tid#%d, pthread_self()=%d): rc=%d at %s(), line=%d, file=%s\n",\
            tid+1,pthread_self(),rc,name,__LINE__,__FILE__); \
    pm_error((char *)name, rc); \
    sleep(tid+1); \
    RAISE(SIGABRT); \
  }

static void
init_hpm(int tid)
{
  const char *name = "init_hpm";
  int rc;

  cycles = irtc_rate_();
}

static void
stop_only_hpm(int tid, drhook_key_t *pstop)
{
  const char *name = "stop_only_hpm";
  int i, rc;

  if (!hpm_tid_init || !hpm_tid_init[tid-1]) init_hpm(tid);
  --tid;


  if (pstop && !pstop->counter_stopped) {

    if (pstop && pstop->counter_in && !pstop->counter_stopped) {
#if defined(DT_FLOP)
      pstop->counter_sum[0] += ((long long int) flop_() - pstop->counter_in[0]);
#if defined(SV2)
      pstop->counter_sum[ENTRY_4] += (_rtc() - pstop->counter_in[ENTRY_4]);
#else
      pstop->counter_sum[ENTRY_4] += (irtc_() - pstop->counter_in[ENTRY_4]);
#endif
#endif
      pstop->counter_stopped = 1;
    }
  }
}


static void
stopstart_hpm(int tid, drhook_key_t *pstop, drhook_key_t *pstart)
{
  const char *name = "stopstart_hpm";
  int i, rc;

  if (!hpm_tid_init || !hpm_tid_init[tid-1]) init_hpm(tid);
  --tid;

  if (pstop && pstop->counter_in && !pstop->counter_stopped) {
#if defined(DT_FLOP)
      pstop->counter_sum[0] += ((long long int) flop_() - pstop->counter_in[0]);
#if defined(SV2)
      pstop->counter_sum[ENTRY_4] += (_rtc() - pstop->counter_in[ENTRY_4]);
#else
      pstop->counter_sum[ENTRY_4] += (irtc_() - pstop->counter_in[ENTRY_4]);
#endif
#endif
    pstop->counter_stopped = 1;
  }

  if (pstart) {
    if (!pstart->counter_in ) pstart->counter_in  = calloc_drhook(MAX_COUNTERS, sizeof(*pstart->counter_in ));
    if (!pstart->counter_sum) pstart->counter_sum = calloc_drhook(MAX_COUNTERS, sizeof(*pstart->counter_sum));
#if defined(DT_FLOP)
      pstart->counter_in[0] = (long long int) flop_();
#if defined(SV2)
      pstart->counter_in[ENTRY_4] = _rtc();
#else
      pstart->counter_in[ENTRY_4] = irtc_();
#endif
#endif
     pstart->counter_stopped = 0;
  }
}

#endif /*Interface to RS6K and SV2, XD1, XT3 */

static double
mflops_hpm(const drhook_key_t *keyptr)
{
  double mflops = 0;
  if (keyptr && keyptr->counter_sum && keyptr->counter_sum[ENTRY_4] > 0) {
    long long int sum = 0;
#if defined(DT_FLOP)
    sum = keyptr->counter_sum[0];
#elif defined(PMAPI_P5_PLUS)
    /* IBM Power 5+ specific */
    sum = 2 * keyptr->counter_sum[1] + keyptr->counter_sum[3];
#else
    sum = keyptr->counter_sum[1] + keyptr->counter_sum[2] + keyptr->counter_sum[3] - keyptr->counter_sum[5];
#endif
    if (sum > 0)
      mflops = (sum * MCYCLES)/keyptr->counter_sum[ENTRY_4];
  }
  return mflops;
}

static double
mips_hpm(const drhook_key_t *keyptr)
{
  double mipsrate = 0;
#if defined(DT_FLOP)
  mipsrate = 0;
#else
  if (keyptr && keyptr->counter_sum && keyptr->counter_sum[ENTRY_4] > 0) {
    mipsrate = (keyptr->counter_sum[ENTRY_6] * MCYCLES)/keyptr->counter_sum[ENTRY_4];
  }
#endif
  return mipsrate;
}

static double
divpc_hpm(const drhook_key_t *keyptr)
{
  double divpc = 0;
#if defined(DT_FLOP)
  divpc = 0;
#else
  if (keyptr && keyptr->counter_sum) {
    long long int sum = 0;
#if defined(PMAPI_P5_PLUS)
    /* IBM Power 5+ specific */
    sum = 2 * keyptr->counter_sum[1] + keyptr->counter_sum[3];
#else
    sum = keyptr->counter_sum[1] + keyptr->counter_sum[2] + keyptr->counter_sum[3] - keyptr->counter_sum[5];
#endif
    if (sum > 0) divpc = (keyptr->counter_sum[0]*100.0)/sum;
  }
#endif
  return divpc;
}

static double
mflop_count(const drhook_key_t *keyptr)
{
  double sum = 0;
  if (keyptr && keyptr->counter_sum && keyptr->counter_sum[ENTRY_4] > 0) {
#if defined(DT_FLOP)
    sum = (keyptr->counter_sum[0]) * 1e-6;
#elif defined(PMAPI_P5_PLUS)
    /* IBM Power 5+ specific */
    sum = (2 * keyptr->counter_sum[1] + keyptr->counter_sum[3]) * 1e-6;
#else
    sum = (keyptr->counter_sum[1] + keyptr->counter_sum[2] + keyptr->counter_sum[3] - keyptr->counter_sum[5]) * 1e-6;
#endif
    if (sum < 0) sum = 0;
  }
  return sum;
}

static double
mip_count(const drhook_key_t *keyptr)
{
  double sum = 0;
#if defined(DT_FLOP)
  sum = 0;
#else
  if (keyptr && keyptr->counter_sum && keyptr->counter_sum[ENTRY_4] > 0) {
    sum = keyptr->counter_sum[ENTRY_6] * 1e-6;
  }
#endif
  return sum;
}

#endif /* HPM */


/* 
   this is result of moving some code from libodb.a
   (odb/aux/util_ccode.c) for use by libifsaux.a
   directly ; simplifies linking sequences.
*/

#include <stdio.h>
#include <string.h>
/* #include <malloc.h> */
#include <stdlib.h>
#include <signal.h>

#define FORTRAN_CALL

#if defined(CRAY) && !defined(SV2)
#define util_cputime_  UTIL_CPUTIME
#define util_walltime_ UTIL_WALLTIME
#endif

/* Portable CPU-timer (User + Sys) ; also WALL CLOCK-timer */

#include <unistd.h>
#include <sys/types.h>
#include <sys/times.h>
#undef MIN
#undef MAX
#include <sys/param.h>

extern clock_t times (struct tms *buffer);

FORTRAN_CALL
double util_cputime_()
{
  struct tms tbuf;
  static int first_time = 1;
  static double clock_ticks = 0;

  (void) times(&tbuf);

  if (first_time) {
    clock_ticks = (double) sysconf(_SC_CLK_TCK);
    first_time = 0;
  }

  return (tbuf.tms_utime + tbuf.tms_stime +
          tbuf.tms_cutime + tbuf.tms_cstime) / clock_ticks; 
}

#include <sys/time.h>

#ifndef VPP
FORTRAN_CALL
double util_walltime_()
{
  static double time_init = 0;
  double time_in_secs;
  struct timeval tbuf;
  if (gettimeofday(&tbuf,NULL) == -1) perror("UTIL_WALLTIME");

  if (time_init == 0) time_init = 
    (double) tbuf.tv_sec + (tbuf.tv_usec / 1000000.0);

  time_in_secs = 
  (double) tbuf.tv_sec + (tbuf.tv_usec / 1000000.0) - time_init;

  return time_in_secs;
}
#else 
/* VPP */
FORTRAN_CALL
double util_walltime_() 
{
  double w, time_in_secs;
  static double wallref = 0;
  extern FORTRAN_CALL gettod_(double *);
  if (wallref == 0) gettod_(&wallref);
  gettod_(&w);
  time_in_secs = (w - wallref) * 0.000001;
  return time_in_secs;
}
#endif

#include <sys/time.h>
#include <sys/resource.h>

#ifdef VPP

#include <sys/types.h>
#include <sys/param.h>
#include <sys/signal.h>
#include <sys/fault.h>
#include <sys/syscall.h>
#include <sys/procfs.h>
#include <sys/proc.h>
#include <fcntl.h>

static int fujitsu_getrusage(int who, struct rusage *rusage)
{
  int rc = -1;

  if (rusage) rusage->ru_maxrss = 0;

  if (who == RUSAGE_SELF && rusage) {
    static int maxrss =  0;
    static int oldpid = -1;
    static char procfile[20] = "";
    static char *pf = NULL;
    /* static prpsinfo_t ps; */
    static proc_t proc;
    int pid = getpid();
    static int fildes = -1;
    unsigned int size;

    if (oldpid != pid) {
      oldpid = pid;
      maxrss = 0;
      pf = NULL;
    }

    if (!pf) {
      sprintf(procfile,"/proc/%d",pid);
      pf = procfile;
      fildes = open(procfile, O_RDONLY);
    }

    if (fildes == -1) return rc;

    /*
    if (ioctl(fildes, PIOCPSINFO, &ps) == -1) {
      perror("ioctl@fujitsu_getrusage(PIOCPSINFO)");
      return rc;
    }
    */

    if (ioctl(fildes, PIOCGETPR, &proc) == -1) {
      perror("ioctl@fujitsu_getrusage(PIOCGETPR)");
      return rc;
    }

    size  = /* ps.pr_usevpmem + */ proc.p_brksize + proc.p_stksize;
    if (size > maxrss) maxrss = size;
    rusage->ru_maxrss = maxrss;

    /* close(fildes); */
    rc = 0;
  }
  return rc;
}
#endif /* VPP */

FORTRAN_CALL
int util_ihpstat_(int *option)
{
  int ret_value = 0;

#if defined(SGI) || defined(VPP)
  if (*option == 1) {
    struct rusage rusage;
#ifdef SGI
    int pagesize = 1024;
    getrusage(0, &rusage);
#endif
#ifdef VPP
    int pagesize = 1; /* getpagesize() */
    fujitsu_getrusage(0, &rusage);
#endif
#if defined(SV2)
    int pagesize = getpagesize();
    getrusage(0, &rusage);
#endif
#if defined(XT3)
    int pagesize = getpagesize();
    getrusage(0, &rusage);
#endif
#if defined(XD1)
    int pagesize = getpagesize();
    getrusage(0, &rusage);
#endif
    ret_value = (rusage.ru_maxrss * pagesize + 7) / 8; /* In 8 byte words */
  }
#endif /* SGI or VPP */

  return ret_value;
}

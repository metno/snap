
/* linuxtrbk.c : Print traceback on linux */

/* 
   Author: Sami Saarinen, ECMWF, 28-Apr-2006
   The code "nicked" from ifsaux/support/drhook.c

*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>
#include <signal.h>
#include "cargs.h"

#define PRETOSTR(x) #x
#define TOSTR(x) PRETOSTR(x)

#if defined(SUN4) && !defined(PSTACKTRACE)
#define PSTACKTRACE /bin/pstack
#endif

#define strequ(s1,s2)     ((void *)s1 && (void *)s2 && strcmp(s1,s2) == 0)
#define strnequ(s1,s2,n)  ((void *)s1 && (void *)s2 && memcmp(s1,s2,n) == 0)


#if (defined(LINUX) || defined(SUN4)) && !defined(XT3) && !defined(XD1) 

#ifndef LINELEN
#define LINELEN 1024
#endif

#if defined(__GNUC__)

#if defined(LINUX) && !defined(CYGWIN)
#include <execinfo.h>
#endif

#define GNUC_BTRACE 1024
#define __USE_GNU
#if !defined(CYGWIN)
#include <ucontext.h>
#endif

#endif /* defined(__GNUC__) */

#if !defined(ADDR2LINE)
#define ADDR2LINE /usr/bin/addr2line
#endif


void
LinuxTraceBack(void *sigcontextptr)
{
  extern void gdb_trbk_();
  extern void dbx_trbk_();
#if defined(__GNUC__) && defined(LINUX) && !defined(CYGWIN)
  ucontext_t ctx;
#endif
  static int recur = 0;
  pid_t pid = getpid();
  const char *a_out = ec_GetArgs(0);
  fprintf(stderr,"[LinuxTraceBack]: Backtrace(s) for program '%s' :\n", a_out ? a_out : ec_GetArgs(0));
  if (++recur > 1) {
    fprintf(stderr,
	    "[LinuxTraceBack]: I don't handle recursive calls very well (recursion level = %d)\n",
	    recur);
    if (recur > 10) {
      fprintf(stderr,"[LinuxTraceBack]: Recursion too deep. Exiting immediately with _exit(%d)\n",
	      recur);
      _exit(recur); /* Exit immediately */
    }
  }

#if defined(__GNUC__) && defined(LINUX) && !defined(CYGWIN)
  fflush(NULL);

  if (!sigcontextptr) {
    sigcontextptr = getcontext(&ctx) ? NULL : &ctx;
  }
  
  if (sigcontextptr) {
    /* To have a desired effect, 
       compile with -g (and maybe -O1 or greater to get some optimization)
       and link with -g -Wl,-export-dynamic */
    void *trace[GNUC_BTRACE];
    int trace_size = 0;
    ucontext_t *uc = (ucontext_t *)sigcontextptr;
    int fd = fileno(stderr);
    trace_size = backtrace(trace, GNUC_BTRACE);
#if defined(REG_EIP)
    /* overwrite sigaction with caller's address */
    trace[1] = (void *) uc->uc_mcontext.gregs[REG_EIP]; /* Help!! REG_EIP only available in 32-bit mode ? */
#endif
    if (trace_size > 0 && (access(TOSTR(ADDR2LINE),X_OK) == 0)) {
      /* Use ADDR2LINE to obtain source file & line numbers for each trace-address */
      int i;
      FILE *fp = NULL;
      /* char addr2linecmd[strlen(TOSTR(ADDR2LINE)) + 10 + strlen(a_out) + trace_size * 30]; */
      int len_addr2linecmd = sizeof(TOSTR(ADDR2LINE)) + 10 + strlen(a_out) + trace_size * 30;
      char *addr2linecmd = malloc(len_addr2linecmd);
      snprintf(addr2linecmd, len_addr2linecmd, "%s -e '%s'", TOSTR(ADDR2LINE), a_out);
      for (i = 0; i < trace_size; i++) {
	char s[30];
	snprintf(s,sizeof(s),(sizeof(void *) == 8) ? " %llx" : " %x",trace[i]);
	strcat(addr2linecmd,s);
      }
      fp = popen(addr2linecmd,"r");
      free(addr2linecmd);
      if (fp) {
	char **strings = backtrace_symbols(trace, trace_size);
	if (strings) {
	  int len = 20;
	  for (i = 0; i < trace_size; i++) {
	    char line[LINELEN];
	    if (!feof(fp) && fgets(line, LINELEN, fp)) {
	      const char *last_slash = strrchr(strings[i],'/');
	      if (last_slash) last_slash++; else last_slash = strings[i];
	      if (*line != '?') {
		int newlen;
		char *nl = strchr(line,'\n');
		if (nl) *nl = '\0';
		newlen = strlen(line);
		if (newlen > len) len = newlen;
		fprintf(stderr, "%*.*s  :  %s\n", len, len, line, last_slash);
	      }
	      else {
		fprintf(stderr, "%*.*s  :  %s\n", len, len, "<Unknown>", last_slash);
	      }
	    }
	    else {
	      fprintf(stderr, "%s\n", strings[i]);
	    }
	  } /* for (i = 0; i < trace_size; i++) */
	} /* if (strings) */
	/* free(strings) */
	fflush(stderr);
	pclose(fp);
      } /* if (fp) */
    }
    else {
      /* Print traceback directly to fd=2 (stderr) */
      backtrace_symbols_fd(trace, trace_size, fd);
    } /* if (addr2linecmd) else ... */
  }
#endif /* __GNUC__ */

#if defined(PSTACKTRACE)
  /* This is normally available on Sun/Solaris ("SUN4") platforms */
  if (access(TOSTR(PSTACKTRACE),X_OK) == 0) {
    char cmd[sizeof(TOSTR(PSTACKTRACE)) + 20];
    snprintf(cmd,sizeof(cmd),"%s %d", TOSTR(PSTACKTRACE), pid);
    fflush(NULL);
    system(cmd);
    fflush(NULL);
  }
#endif /* defined(PSTACKTRACE) */

  gdb_trbk_();
  dbx_trbk_();

 finish:
  fprintf(stderr,"[LinuxTraceBack] : End of backtrace(s)\n");
  recur--;
}
 
void linux_trbk_(void)
{
  LinuxTraceBack(NULL);
}

#else

/* Non-Linux: A dummy call which does nothing */

void LinuxTraceBack(void *sigcontextptr) { }

void linux_trbk_(void) { }

#endif

void linux_trbk(void)
{
  linux_trbk_();
}

/* GNU-debugger traceback */

#if !defined(GNUDEBUGGER)
#define GNUDEBUGGER /usr/bin/gdb
#endif

void gdb_trbk_()
{
  char *gdb = getenv("GNUDEBUGGER");
  if (gdb && 
      (access(TOSTR(GNUDEBUGGER),X_OK) == 0) && /* GNUDEBUGGER was set */
      (strequ(gdb,"1")    || 
       strequ(gdb,"true") || 
       strequ(gdb,"TRUE"))) {
    char gdbcmd[65536];
    pid_t pid = getpid();
    const char *a_out = ec_GetArgs(0);
    fprintf(stderr,
	    "[gdb_trbk] : Invoking %s ...\n",
	    TOSTR(GNUDEBUGGER));
    snprintf(gdbcmd,sizeof(gdbcmd),
	     "set +e; /bin/echo '"
	     "set watchdog 1\n"
	     "set confirm off\n"
	     "set pagination off\n"
	     "set print elements 16\n"
	     "set print repeats 3\n"
	     "set print sevenbit-strings on\n"
	     "where\n"
	     "quit\n' > ./gdb_drhook.%d ; "
	     "%s -x ./gdb_drhook.%d -q -n -f -batch %s %d < /dev/null ; "
	     "/bin/rm -f ./gdb_drhook.%d"
	     , pid
	     , TOSTR(GNUDEBUGGER), pid, a_out, pid
	     , pid);
    
    /* fprintf(stderr,"%s\n",gdbcmd); */
    fflush(NULL);
    system(gdbcmd);
    fflush(NULL);
  }
}

void gdb_trbk() { gdb_trbk_(); }


/* DBX-debugger traceback */

#if !defined(DBXDEBUGGER)
#define DBXDEBUGGER /usr/bin/dbx
#endif

void dbx_trbk_()
{
  char *dbx = getenv("DBXDEBUGGER");
  if (dbx && 
      (access(TOSTR(DBXDEBUGGER),X_OK) == 0) && /* DBXDEBUGGER was set */
      (strequ(dbx,"1")    || 
       strequ(dbx,"true") || 
       strequ(dbx,"TRUE"))) {
    pid_t pid = getpid();
    const char *a_out = ec_GetArgs(0);
    char dbxcmd[65536];
#if defined(SUN4)
    const char *qopt = " -q";
#else
    const char *qopt = "";
#endif
    fprintf(stderr,
	    "[dbx_trbk] : Invoking %s ...\n",
	    TOSTR(DBXDEBUGGER));
    if (a_out && (access(a_out,X_OK|R_OK) == 0)) {
      snprintf(dbxcmd,sizeof(dbxcmd),
	       "set +e; /bin/echo 'where; quit; '"
	       " | %s%s %s %d ",
	       TOSTR(DBXDEBUGGER), qopt, a_out, pid);
    }
    else {
      snprintf(dbxcmd,sizeof(dbxcmd),
	       "set +e; /bin/echo 'where; quit; '"
	       " | %s%s - %d ",
	       TOSTR(DBXDEBUGGER), qopt, pid);
    }
    
    /* fprintf(stderr,"%s\n",dbxcmd); */
    fflush(NULL);
    system(dbxcmd);
    fflush(NULL);
  }
}

void dbx_trbk() { dbx_trbk_(); }

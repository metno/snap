#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

#include "cargs.h"

typedef struct {
  char *name;
  int len;
} arg_t;

static arg_t *args = NULL;
static int numargs = -1;
static char *cl_terminate = NULL;
static char *a_out = NULL;

#if !defined(PSCMD)
#define PSCMD "/bin/ps"
#endif /* !defined(PSCMD) */

static const char *get_a_out()
{
  /* progname is a blank string;
     this is most likely due to a Fortran-call to getarg
     from program that has a C-main program, thus Fortran getarg
     may return a blank string */
  
  /* Using an alternative method of getting a.out :
     
  ps -p<pid> | tail -1 | awk '{print $NF}' 
  
  Naturally this cannot be the nicest method around ;-(
  So, a competition is launched: make this better and 
  you may win a week in Bahamas!!

  */

  if (!a_out && (access(PSCMD,X_OK) == 0)) {
    char cmd[sizeof(PSCMD) + 100];
    FILE *fp = NULL;
    pid_t pid = getpid();
    sprintf(cmd,"%s -p%d | tail -1 | awk '{print $NF}'", PSCMD, (int)pid);
    fp = popen(cmd, "r");
    if (fp) {
      char c[65536];
      if (fscanf(fp,"%s",c) == 1) {
	if (!strchr(c,'/')) { 
	  /* The file path was NOT embedded in the name 
	     ==> Must search from $PATH f.ex. /bin:/usr/bin:/some/thing/else:/etc/bin */
	  char *path = getenv("PATH");
	  if (path) {
	    int lenc = strlen(c);
	    char *saved = strdup(path);
	    char *start = saved;
	    char *token = strtok(saved,":");
	    do {
	      /* char fullpath[strlen(start) + 1 + lenc + 1]; */
	      char *fullpath = malloc(strlen(start) + 1 + lenc + 1);
	      snprintf(fullpath,sizeof(fullpath),"%s/%s",start,c);
	      if (access(fullpath,X_OK) == 0) { /* It's this one!! */
		a_out = strdup(fullpath);
		free(fullpath);
		break;
	      }
	      free(fullpath);
	      start = token;
	      token = strtok(NULL,":");
	    } while (token);
	    free(saved);
	  }
	}
	if (!a_out) a_out = strdup(c);
      }
      pclose(fp);
    }
  }
  if (!a_out) a_out = strdup("/unknown/executable");
  return a_out;
}

  
void ec_PutArgs(int argc, char *argv[])
{
  if (numargs == -1 && !args) {
    if (argc > 0) {
      int j;
      args = calloc(argc, sizeof(arg_t));
      /* cl_terminate: see ifsaux/module/mpl_arg_mod.F90 */
      if (!cl_terminate) {
	char *env = getenv("MPL_CL_TERMINATE");
	cl_terminate = env ? strdup(env) : strdup("-^");
      }
      numargs = 0;
      for (j=0; j<argc; j++) {
	if (!argv[j] || strcmp(argv[j],cl_terminate) == 0) break;
	args[j].name = strdup(argv[j]);
	args[j].len = strlen(argv[j]);
	numargs++;
      }
      if (numargs == 0) {
	const char *arg0 = get_a_out();
	args[0].name = strdup(arg0);
	args[0].len = strlen(arg0);
      }
      else {
	if (a_out) free(a_out);
	a_out = strdup(args[0].name);
	numargs--; /* Fortran # of args == C # of args - 1 */
      }
    } /* if (argc > 0) */
  } /* if (numargs == -1 && !args) */
}


const char *ec_GetArgs(int argno)
{
  const char *arg = NULL;
  if (argno == 0 && !args) {
    arg = get_a_out();
  }
  else if (argno >= 0 && argno <= numargs && args) {
    arg = args[argno].name;
  }
  return arg;
}


int ec_NumArgs(void) { return numargs; }

int ec_argc(void) { return 1 + ec_NumArgs(); }

char **ec_argv(void)
{
  int j, argc = ec_argc();
  char **argv = NULL;
  argv = calloc((argc + 1), sizeof(*argv));
  for (j=0; j<argc; j++) {
    argv[j] = (char *)ec_GetArgs(j);
  }
  argv[argc] = NULL;
  return argv;
}


/* Fortran interface */

int iargc_c_(void) { return numargs; }
int iargc_c (void) { return numargs; }


void getarg_c_(const int *argno, char *arg
	       /* Hidden argument */
	       , const int arg_len)
{
  int Argno = argno ? *argno : -1;
  if (arg && arg_len > 0 && 
      Argno >= 0 && Argno <= numargs && args && args[Argno].name) {
    const char *s = args[Argno].name;
    int len = args[Argno].len;
    if (arg_len < len) len = arg_len;
    strncpy(arg,s,len);
    if (arg_len > len) memset(&arg[len],' ',arg_len-len);
  }
}


void getarg_c (const int *argno, char *arg
	       /* Hidden argument */
	       , const int arg_len)
{
  getarg_c_(argno, arg, arg_len);
}


void putarg_c_(const int *argno, const char *arg
	       /* Hidden argument */
	       , int arg_len)
{
  int Argno = argno ? *argno : -1;
  if (arg && arg_len >= 0 && 
      Argno >= 0 && Argno <= numargs && args) {
    char *s = calloc(arg_len+1,sizeof(*s));
    strncpy(s,arg,arg_len);
    s[arg_len] = '\0';
    if (args[Argno].name) free(args[Argno].name);
    args[Argno].name = s;
    args[Argno].len = arg_len;
  }
}


void putarg_c (const int *argno, const char *arg
	       /* Hidden argument */
	       , int arg_len)
{
  putarg_c_(argno, arg, arg_len);
}


void putarg_info_(const int *argc, const char *cterm
		  /* Hidden argument */
		  , int cterm_len)
{
  int Argc = argc ? *argc : 0;
  if (cterm && cterm_len >= 0) {
    if (cl_terminate) free(cl_terminate);
    cl_terminate = calloc(cterm_len+1,sizeof(*cl_terminate));
    strncpy(cl_terminate,cterm,cterm_len);
    cl_terminate[cterm_len] = '\0';
  }
  if (numargs >= 0 || args) {
    if (args) {
      int j;
      for (j=0; j<=numargs; j++) { /* note:  "j<=", not "j<" */
	if (args[j].name) free(args[j].name);
      }
      free(args);
      args = NULL;
    } /* if (args) */
    numargs = -1;
  }
  /* Re-initialize args & numargs */
  if (Argc < 0) Argc = 0;
  numargs = Argc;
  args = calloc(1 + numargs, sizeof(arg_t));
}


void putarg_info (const int *argc, const char *cterm
		  /* Hidden argument */
		  , int cterm_len)
{
  putarg_info_(argc, cterm, cterm_len);
}

/* raise.h */

#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <unistd.h>

extern void abor1fl_(const char *filename, const int *linenum, 
		     const char *s, 
		     int filenamelen, int slen);
extern void abor1_(const char *s, int slen);

#define ABOR1(txt) abor1_(txt ? txt : "",txt ? strlen(txt) : 0)

#define ABOR1FL(txt) { int linenum=__LINE__; \
                       abor1fl_(__FILE__, &linenum, \
				txt ? txt : "", \
				strlen(__FILE__), \
				txt ? strlen(txt) : 0); \
                        _exit(1); /* Should never end up here */ }

#define RAISE(x) { \
  if ((x) == SIGABRT) { \
    ABOR1FL("*** Fatal error; aborting (SIGABRT) ..."); \
    _exit(1); /* Should never end up here */ \
  } \
  else raise(x); \
}

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/times.h>

int main(int argc, char *argv[])
{
  if (argc >= 4 && strcmp(argv[1],"__NO_TIMESTAMPS__") != 0) {
    struct tms dummy;
    double dtime;
    int ticks;
    ticks=sysconf(_SC_CLK_TCK);
    dtime=((double)times(&dummy)/ticks);
    printf("Timestamp %s : %s : %s : %.6f\n",argv[1],argv[2],argv[3],dtime);
  }
  return 0;
}

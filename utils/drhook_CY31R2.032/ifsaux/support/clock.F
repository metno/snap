      INTEGER FUNCTION CLOCK()
C
C     clock() returns the amount of CPU time (in microseconds) used since
C     the first call to clock().  The time reported is the sum of the user
C     and system times of the calling process and its terminated child
C     processes for which it has executed wait() or system() (see wait(2)
C     and system(3S)).  To determine the time in seconds, the value returned
C     by clock() should be divided by the value of the macro CLOCKS_PER_SEC.
C
C     INTRODUCE A PROPER CLCOK ROUTINE HERE
C
      IMPLICIT LOGICAL (L)
C
      call system_clock(ic,icr,icm)
      if(icm.eq.0) then
        print *,' no system clock'
        clock=-99.
      else
#ifdef VPP
        clock=ic
#else
        clock=1000.*ic/icr
#endif
      endif
      RETURN
      END

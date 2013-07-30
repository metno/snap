#if defined (VPP)
      SUBROUTINE CPTIME(PVCP,PTCP)
C     Fujitsu function
      call clockv(PVCP,PTCP,0,2)
      return
      END
#endif
#if defined (RS6K) || defined (SX4) || defined(ALPHA) || defined(HPPA) || defined(SUN4)
      SUBROUTINE CPTIME(PVCP,PTCP)
      data ifirst/0/
      save ifirst,zfirst
      if(ifirst.eq.0) then
         ifirst=1
         zfirst=second()
         pvcp=0.0
         ptcp=pvcp
      else
         ZSEC=SECOND()
         PVCP=ZSEC-ZFIRST
         PTCP=PVCP
      endif
      return
      END
#endif
#if defined (LINUX)
      SUBROUTINE CPTIME(PVCP,PTCP)
      integer clock
      external clock
      data ifirst/0/
      save ifirst,zfirst
      if(ifirst.eq.0) then
         ifirst=1
         zfirst=clock()/1000.
         pvcp=0.0
         ptcp=pvcp
      else
         ZSEC=clock()/1000.
         PVCP=ZSEC-ZFIRST
         PTCP=PVCP
      endif
      return
      END
#endif

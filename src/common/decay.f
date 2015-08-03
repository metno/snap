      subroutine decay(n)
c
c  Purpose:  Decrease radioactive contents due to decay
c    WARNING:   make sure decayDeps is run once before running decay
c
#if defined(DRHOOK)
      USE PARKIND1  ,ONLY : JPIM     ,JPRB
      USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
#endif
      use particleML
      implicit none
#if defined(DRHOOK)
      REAL(KIND=JPRB) :: ZHOOK_HANDLE ! Stack variable i.e. do not use SAVE
#endif
c
      include 'snapdim.inc'
ccc   include 'snapgrd.inc'
ccc      include 'snapfld.inc'
      include 'snappar.inc'
c
c
      integer, INTENT(IN) :: n
      integer m
c
c
c
#if defined(DRHOOK)
      ! Before the very first statement
      IF (LHOOK) CALL DR_HOOK('DECAY',0,ZHOOK_HANDLE)
#endif
c
c
c      do n=1,npart loop outside this function
       m= icomp(n)
       if(kdecay(m).eq.1) then
         pdata(n)%rad= pdata(n)%rad * decayrate(m)
       end if
c      end do
c
c
#if defined(DRHOOK)
c     before the return statement
      IF (LHOOK) CALL DR_HOOK('DECAY',1,ZHOOK_HANDLE)
#endif
      return
      end

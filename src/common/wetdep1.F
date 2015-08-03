      subroutine wetdep1(n)
      use particleML
c
c  Purpose:  Compute wet deposition for each particle and each component
c            and store depositions in nearest gridpoint in a field
c  Method:   J.Saltbones 1994
c
c
#if defined(DRHOOK)
      USE PARKIND1  ,ONLY : JPIM     ,JPRB
      USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
#endif
      implicit none
#if defined(DRHOOK)
      REAL(KIND=JPRB) :: ZHOOK_HANDLE ! Stack variable i.e. do not use SAVE
#endif
c
      include 'snapdim.inc'
      include 'snapgrd.inc'
      include 'snapfld.inc'
      include 'snappar.inc'
      include 'snaptab.inc'
c
c particle loop index, n = 0 means init
      INTEGER, INTENT(IN) :: n
      integer m,itab,i,j,mm
      real    precint,probab,prand,dep
c
c
#if defined(DRHOOK)
      ! Before the very first statement
      IF (LHOOK) CALL DR_HOOK('WETDEP1',0,ZHOOK_HANDLE)
#endif
c
c
c      do n=1,npart // particle loop moved outside subroutine
       m= icomp(n)
       if(kwetdep(m).eq.1 .and. pdata(n)%prc.gt.0.0) then
c..find particles with wet deposition and
c..reset precipitation to zero if not wet deposition
          precint=pdata(n)%prc
          itab=nint(precint*premult)
          itab=min(itab,mpretab)
          probab=pretab(itab)
c..the rand function returns random real numbers between 0.0 and 1.0
          call random_number(prand)
          if(prand.gt.probab) then
           pdata(n)%prc=0.0
         else
            dep=wetdeprat(m)*pdata(n)%rad
            pdata(n)%rad=pdata(n)%rad-dep
           i=nint(pdata(n)%x)
           j=nint(pdata(n)%y)
           mm=iruncomp(m)
!$omp atomic
            depwet(i,j,mm)=depwet(i,j,mm)+dble(dep)
         end if
       end if
c      end do
c
#if defined(DRHOOK)
c     before the return statement
      IF (LHOOK) CALL DR_HOOK('WETDEP1',1,ZHOOK_HANDLE)
#endif
      return
      end

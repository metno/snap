      subroutine decayDeps(tstep)
c
c  Purpose:  Decrease radioactive contents of deposition fields
c            due to decay
c     NEEDS TO BE RUN BEFORE 1 decay
c
#if defined(DRHOOK)
      USE PARKIND1  ,ONLY : JPIM     ,JPRB
      USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
#endif
      implicit none
      include 'snapdim.inc'
ccc   include 'snapgrd.inc'
      include 'snapfld.inc'
      include 'snappar.inc'
c
      real tstep
c
      integer i,j,m
c
      logical, save :: prepare = .true.
c
c
#if defined(DRHOOK)
      ! Before the very first statement
      IF (LHOOK) CALL DR_HOOK('DECAYDEPS',0,ZHOOK_HANDLE)
#endif
c
      if(prepare) then
c
c..radioactive decay rate
        do m=1,ndefcomp
         if (kdecay(m).eq.1) then
           decayrate(m)= exp(-log(2.0)*tstep/(halftime(m)*3600.))
         else
           decayrate(m)=1.0
         end if
        end do
c
       prepare=.false.
      end if

      do m=1,ndefcomp
        if(kdecay(m).eq.1) then
          do j=1,ny
            do i=1,nx
              depdry(i,j,m)=depdry(i,j,m)*decayrate(m)
              depwet(i,j,m)=depwet(i,j,m)*decayrate(m)
              accdry(i,j,m)=accdry(i,j,m)*decayrate(m)
              accwet(i,j,m)=accwet(i,j,m)*decayrate(m)
            enddo
          enddo
        endif
      enddo
#if defined(DRHOOK)
c     before the return statement
      IF (LHOOK) CALL DR_HOOK('DECAYDEPS',1,ZHOOK_HANDLE)
#endif
      return
      end subroutine decayDeps

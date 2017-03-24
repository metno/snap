      subroutine forwrd(tf1,tf2,tnow,tstep,np,pextra)
      use particleML
c
c  Purpose:  Move all particles one timestep forward
c
c  Notes:
c    - sigma levels (norlam) or eta levels (hirlam,...)
c      defined by alevel and blevel
c    - horizontal wind components in unit m/s
c    - vertical   wind component  in unit vlevel/second
c      (vlevel = sigma or eta(alevel,blevel))
c    - all wind components in non-staggered horizontal grid
c      and in the same levels
c    - lower model level is level 2
c
c  Input:
c       tf1:   time in seconds for field set 1 (e.g. 0.)
c       tf2:   time in seconds for field set 2 (e.g. 21600, if 6 hours)
c       tnow:  time in seconds for current paricle positions
c       tstep: timestep in seconds
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
      real, INTENT(IN) :: tf1,tf2,tnow,tstep
c
c particle loop index, np = 0 is initalization
      INTEGER, INTENT(IN) :: np
      TYPE(extraParticle), INTENT(INOUT) :: pextra
      include 'snapgrd.inc'

      TYPE(particle) :: nparticle
      REAL :: dx1, dx2, dy1, dy2, dz1, dz2, u, v
#if defined(TRAJ)
c... calculation of distance and speed
      real speed
      common /speed/ speed
#endif

#if defined(DRHOOK)
      ! Before the very first statement
      IF (LHOOK) CALL DR_HOOK('FORWRD',0,ZHOOK_HANDLE)
#endif


      if (np .eq. 0) then
c init
        call forwrd_dx(tf1,tf2,tnow,tstep,np,nparticle,dx1,dy1,dz1,u,v)
#if defined(DRHOOK)
c     before the return statement
        IF (LHOOK) CALL DR_HOOK('FORWRD',1,ZHOOK_HANDLE)
#endif

        return
      endif

      nparticle = pdata(np)
      call forwrd_dx(tf1,tf2,tnow,tstep,np,nparticle, dx1, dy1, dz1,
     +                  u, v)
c..store u,v for rwalk
      pextra%u=u
      pextra%v=v
#if defined(TRAJ)
      speed=sqrt(u*u+v*v)
#endif
#if defined(PETTERSEN)
      nparticle%x=nparticle%x+dx1*pextra%rmx
      nparticle%y=nparticle%y+dy1*pextra%rmy
      nparticle%z=nparticle%z+dz1
      if(nparticle%z .gt. vlevel(1)) nparticle%z=vlevel( 1)
      call forwrd_dx(tf1,tf2,tnow+tstep,tstep,
     +                np,nparticle, dx2, dy2, dz2, u, v)
      pdata(np)%x=pdata(np)%x+.5*(dx1 + dx2)*pextra%rmx
      pdata(np)%y=pdata(np)%y+.5*(dy1 + dy2)*pextra%rmy
      pdata(np)%z=pdata(np)%z+.5*(dz1 + dz2)
#else
      pdata(np)%x=pdata(np)%x+dx1*pextra%rmx
      pdata(np)%y=pdata(np)%y+dy1*pextra%rmy
      pdata(np)%z=pdata(np)%z+dz1
#endif
      if(pdata(np)%z .gt. vlevel(1)) pdata(np)%z=vlevel( 1)

#if defined(DRHOOK)
c     before the return statement
      IF (LHOOK) CALL DR_HOOK('FORWRD',1,ZHOOK_HANDLE)
#endif
      return
      end subroutine forwrd

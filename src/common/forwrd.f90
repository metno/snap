! SNAP: Servere Nuclear Accident Programme
! Copyright (C) 1992-2017   Norwegian Meteorological Institute

! This file is part of SNAP. SNAP is free software: you can
! redistribute it and/or modify it under the terms of the
! GNU General Public License as published by the
! Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.

! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.

! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https://www.gnu.org/licenses/>.

subroutine forwrd(tf1,tf2,tnow,tstep,np,pextra)

!  Purpose:  Move all particles one timestep forward

!  Notes:
!    - sigma levels (norlam) or eta levels (hirlam,...)
!      defined by alevel and blevel
!    - horizontal wind components in unit m/s
!    - vertical   wind component  in unit vlevel/second
!      (vlevel = sigma or eta(alevel,blevel))
!    - all wind components in non-staggered horizontal grid
!      and in the same levels
!    - lower model level is level 2

!  Input:
!       tf1:   time in seconds for field set 1 (e.g. 0.)
!       tf2:   time in seconds for field set 2 (e.g. 21600, if 6 hours)
!       tnow:  time in seconds for current paricle positions
!       tstep: timestep in seconds


  USE particleML
  USE snapgrdML
#if defined(DRHOOK)
  USE PARKIND1  ,ONLY : JPIM     ,JPRB
  USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
#endif
  implicit none
#if defined(DRHOOK)
  REAL(KIND=JPRB) :: ZHOOK_HANDLE ! Stack variable i.e. do not use SAVE
#endif

  real, INTENT(IN) :: tf1,tf2,tnow,tstep

! particle loop index, np = 0 is initalization
  INTEGER, INTENT(IN) :: np
  TYPE(extraParticle), INTENT(INOUT) :: pextra

  TYPE(particle) :: nparticle
  REAL*8 :: dx1, dx2, dy1, dy2, dz1, dz2, u, v
  REAL :: vmin, vmax
#if defined(TRAJ)
!... calculation of distance and speed
  real :: speed
  common /speed/ speed
#endif

#if defined(DRHOOK)
! Before the very first statement
  IF (LHOOK) CALL DR_HOOK('FORWRD',0,ZHOOK_HANDLE)
#endif


  if (np == 0) then
  ! init
    call forwrd_dx(tf1,tf2,tnow,tstep,np,nparticle,dx1,dy1,dz1,u,v)
#if defined(DRHOOK)
  !     before the return statement
    IF (LHOOK) CALL DR_HOOK('FORWRD',1,ZHOOK_HANDLE)
#endif

    return
  endif

  nparticle = pdata(np)
  call forwrd_dx(tf1,tf2,tnow,tstep,np,nparticle, dx1, dy1, dz1, &
  u, v)
!..store u,v for rwalk
  pextra%u=u
  pextra%v=v
#if defined(TRAJ)
  speed=sqrt(u*u+v*v)
#endif
#if defined(PETTERSEN)
  nparticle%x=nparticle%x+dx1*pextra%rmx
  nparticle%y=nparticle%y+dy1*pextra%rmy
  nparticle%z=nparticle%z+dz1
  if (nparticle%x < 1. .OR. nparticle%y < 1. .OR. &
  nparticle%x > nx .OR. nparticle%y > ny) then
    nparticle%active = .FALSE. 
    pdata(np) = nparticle
  else
    vmin=vlevel(nk)
    vmax=vlevel( 1)
    if (nparticle%z > vmax) then
      nparticle%z = vmax
    else if (nparticle%z < vmin) then
      nparticle%z = vmin
    end if

    call forwrd_dx(tf1,tf2,tnow+tstep,tstep, &
    np,nparticle, dx2, dy2, dz2, u, v)
    pdata(np)%x=pdata(np)%x+.5*(dx1 + dx2)*pextra%rmx
    pdata(np)%y=pdata(np)%y+.5*(dy1 + dy2)*pextra%rmy
    pdata(np)%z=pdata(np)%z+.5*(dz1 + dz2)
  endif
#else
  pdata(np)%x=pdata(np)%x+dx1*pextra%rmx
  pdata(np)%y=pdata(np)%y+dy1*pextra%rmy
  pdata(np)%z=pdata(np)%z+dz1
#endif
  if(pdata(np)%z > vlevel(1)) pdata(np)%z=vlevel( 1)

#if defined(DRHOOK)
!     before the return statement
  IF (LHOOK) CALL DR_HOOK('FORWRD',1,ZHOOK_HANDLE)
#endif
  return
end subroutine forwrd

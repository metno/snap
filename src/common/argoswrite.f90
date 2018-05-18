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

subroutine argoswrite(iunit,name,iparam,itimeargos,nx,ny,dblfield)

  USE snapdebugML

#if defined(DRHOOK)
  USE PARKIND1  ,ONLY : JPIM     ,JPRB
  USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
#endif
  implicit none
#if defined(DRHOOK)
  REAL(KIND=JPRB) :: ZHOOK_HANDLE ! Stack variable i.e. do not use SAVE
#endif

!..input
  integer :: iunit,iparam,nx,ny,itimeargos(5)
  character(4) :: name
  double precision :: dblfield(nx*ny)

!..local
  integer :: nxy,ij,ij1,ij2,i
  double precision :: dblmin,dblmax

#if defined(DRHOOK)
! Before the very first statement
  IF (LHOOK) CALL DR_HOOK('ARGOSWRITE',0,ZHOOK_HANDLE)
#endif

  nxy=nx*ny

  if(name == 'depo') write(iunit,1001,err=900) iparam
  if(name == 'conc') write(iunit,1002,err=900) iparam
  if(name == 'dose') write(iunit,1003,err=900) iparam

  write(iunit,1004,err=900) 1.0
  write(iunit,1005,err=900) (itimeargos(i),i=1,5)

  do ij1=1,nxy,10
    ij2= min(ij1+9,nxy)
    write(iunit,1006,err=900) (dblfield(ij),ij=ij1,ij2)
  end do
!c
  1001 format('Isotope ',i3,' deposition (Unit/m2)')
  1002 format('Isotope ',i3,' concentration (Unit/m3)')
  1003 format('Isotope ',i3,' dose (Unit*hr/m3)')
  1004 format(1pe8.2e2)
  1005 format(5i2.2)
  1006 format(10(1pe14.6e2))

  if(idebug == 1) then
    dblmin=+1.0d+100
    dblmax=-1.0d+100
    do ij=1,nxy
      if(dblfield(ij) > 0.0d0) then
        if(dblmin > dblfield(ij)) dblmin=dblfield(ij)
        if(dblmax < dblfield(ij)) dblmax=dblfield(ij)
      end if
    end do
    if(dblmin > dblmax) then
      dblmin=0.0d0
      dblmax=0.0d0
    end if
    write(9,*) 'ARGOS ',name,iparam,dblmin,dblmax
  end if

#if defined(DRHOOK)
!     before the return statement
  IF (LHOOK) CALL DR_HOOK('ARGOSWRITE',1,ZHOOK_HANDLE)
#endif
  return

  900 continue
  stop 'ARGOSWRITE'

end subroutine argoswrite

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

module drydep
  use snapdimML, only: mdefcomp
  implicit none
  private

  public drydep1, drydep2

!> dry deposition rate
  real, save, public :: drydeprat(mdefcomp)
!> max height above ground for dry deposition
  real, save, public :: drydephgt(mdefcomp)
!> for each component: 0=dry deposition off  1=dry dep. on
  integer, save, public :: kdrydep(mdefcomp)

  contains

!> Purpose:  Compute dry deposition for each particle and each component
!>           and store depositions in nearest gridpoint in a field
!>
!> Method:   J.Saltbones 1994
subroutine drydep1(n)
  USE particleML, only: pdata
  USE snapfldML, only: depdry
  USE snapparML, only: icomp, iruncomp

!> particle loop index, n=0 means init
  integer, intent(in) :: n

  integer :: m, i, j, mm
  real :: h, dep

  m = icomp(n)
  if(kdrydep(m) == 1) then
  !..very rough eastimate of height,
  !..using boundary layer height, then just linear in sigma/eta !!! ????
    h=pdata(n)%hbl*(1.-pdata(n)%z)/(1.-pdata(n)%tbl)
    if(h < drydephgt(m)) then
      dep=drydeprat(m)*pdata(n)%rad
      pdata(n)%rad=pdata(n)%rad-dep
      i=nint(pdata(n)%x)
      j=nint(pdata(n)%y)
      mm=iruncomp(m)
    ! omp atomic
      depdry(i,j,mm)=depdry(i,j,mm)+dble(dep)
    end if
  end if

  return
end subroutine drydep1


!> Purpose:  Compute dry deposition for each particle and each component
!>           and store depositions in nearest gridpoint in a field
!>
!> Method:   J.Bartnicki 2003
subroutine drydep2(tstep, n)
  USE particleML, only: pdata
  USE snapfldML, only: depdry
  USE snapparML, only: icomp, iruncomp
  USE snapgrdML, only: vlevel
  USE vgravtablesML, only: radiusmym

! ... 23.04.12 - gas, particle 0.1<d<10, particle d>10 - J. Bartnicki|
  real, intent(in) :: tstep
!> particle loop index, n = 0 means init
  integer, intent(in) :: n

  integer :: m,i,j,mm
  real ::    h,deprate,dep

  m= icomp(n)
!#### 30m = surface-layer (deposition-layer); sigma(hybrid)=0.996 ~ 30m
  if(kdrydep(m) == 1 .AND. pdata(n)%z > 0.996) then
    h=30.0
  ! b...23.04.12... difference between particle and gas

    if(radiusmym(m) <= 0.05) then
    ! gas
      deprate= 1.0-exp(-tstep*(0.008)/h)
    else if (radiusmym(m) <= 10.0) then
    ! particle 0.05<r<10
      deprate= 1.0-exp(-tstep*(0.002)/h)
    else
    ! particle r>=10
      deprate= 1.0-exp(-tstep*(0.002+pdata(n)%grv)/h)
    ! complete deposition when particle hits ground
      if (pdata(n)%z == vlevel(1)) deprate = 1.
    endif
    dep=deprate*pdata(n)%rad
    pdata(n)%rad=pdata(n)%rad-dep
    i=nint(pdata(n)%x)
    j=nint(pdata(n)%y)
    mm=iruncomp(m)
  ! omp atomic
    depdry(i,j,mm)=depdry(i,j,mm)+dble(dep)
  end if

  return
end subroutine drydep2
end module drydep

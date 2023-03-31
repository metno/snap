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
  implicit none
  private

  public drydep1, drydep2

  contains

!> Purpose:  Compute dry deposition for each particle and each component
!>           and store depositions in nearest gridpoint in a field
!>
!> Method:   J.Saltbones 1994
subroutine drydep1(part)
  USE particleML, only: Particle
  USE snapfldML, only: depdry
  USE snapparML, only: def_comp
  USE snapdimML, only: hres_pos

!> particle
  type(Particle), intent(inout) :: part

  integer :: m, i, j, mm
  real :: h, dep

  m = part%icomp
  if (def_comp(m)%kdrydep == 1) then
  !..very rough eastimate of height,
  !..using boundary layer height, then just linear in sigma/eta !!! ????
    h = part%hbl*(1.-part%z)/(1.-part%tbl)
    if (h < def_comp(m)%drydephgt) then
      dep = part%scale_rad(1.0 - def_comp(m)%drydeprat)
      i = hres_pos(part%x)
      j = hres_pos(part%y)
      mm = def_comp(m)%to_running
    !$OMP atomic
      depdry(i,j,mm) = depdry(i,j,mm) + dble(dep)
    end if
  end if
end subroutine drydep1


!> Purpose:  Compute dry deposition for each particle and each component
!>           and store depositions in nearest gridpoint in a field
!>
!> Method:   J.Bartnicki 2003
subroutine drydep2(tstep, part)
  USE particleML, only: Particle
  USE snapfldML, only: depdry
  USE snapdimML, only: hres_pos
  USE snapparML, only: def_comp
  USE snapgrdML, only: vlevel
  USE snaptabML, only: surface_height_sigma

! ... 23.04.12 - gas, particle 0.1<d<10, particle d>10 - J. Bartnicki|
!> timestep of the simulation, affects the deposition rate
  real, intent(in) :: tstep
!> particle
  type(Particle), intent(inout) :: part

  integer :: m,i,j,mm
  real :: deprate, dep
  real, parameter :: h = 30.0

  m = part%icomp
!#### 30m = surface-layer (deposition-layer); sigma(hybrid)=0.996 ~ 30m
  if (def_comp(m)%kdrydep == 1 .AND. part%z > surface_height_sigma) then
  ! b...23.04.12... difference between particle and gas

    if (def_comp(m)%radiusmym <= 0.05) then
    ! gas
      deprate = 1.0 - exp(-tstep*(0.008)/h)
    else if (def_comp(m)%radiusmym <= 10.0) then
    ! particle 0.05<r<10
      deprate = 1.0 - exp(-tstep*(0.002+part%grv)/h)
    else
    ! particle r>=10.0
      deprate = 1.0 - exp(-tstep*(0.002+part%grv)/h)
    ! complete deposition when particle hits ground
      if (part%z == vlevel(1)) deprate = 1.
    endif
    dep = part%scale_rad(1.0 - deprate)
    i = hres_pos(part%x)
    j = hres_pos(part%y)
    mm = def_comp(m)%to_running
  !$OMP atomic
    depdry(i,j,mm) = depdry(i,j,mm) + dble(dep)
  end if
end subroutine drydep2
end module drydep

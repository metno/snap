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

  public drydep1, drydep2, drydep_emep, drydep_emep_vd

  integer, parameter, public :: DRYDEP_SCHEME_UNDEFINED = 0
  integer, parameter, public :: DRYDEP_SCHEME_OLD = 1
  integer, parameter, public :: DRYDEP_SCHEME_NEW = 2
  integer, parameter, public :: DRYDEP_SCHEME_EMEP = 3

  !> The active dry deposition scheme
  integer, save, public :: drydep_scheme = DRYDEP_SCHEME_UNDEFINED


  contains

!> Purpose:  Compute dry deposition for each particle and each component
!>           and store depositions in nearest gridpoint in a field
!>
!> Method:   J.Saltbones 1994
subroutine drydep1(part)
  USE particleML, only: Particle
  USE snapfldML, only: depdry
  USE snapparML, only: def_comp

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
      dep = def_comp(m)%drydeprat*part%rad
      part%rad = part%rad - dep
      i = nint(part%x)
      j = nint(part%y)
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
  USE snapparML, only: def_comp
  USE snapgrdML, only: vlevel

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
  if (def_comp(m)%kdrydep == 1 .AND. part%z > 0.996) then
  ! b...23.04.12... difference between particle and gas

    if (def_comp(m)%radiusmym <= 0.05) then
    ! gas
      deprate = 1.0 - exp(-tstep*(0.008)/h)
    else if (def_comp(m)%radiusmym <= 10.0) then
    ! particle 0.05<r<10
      deprate = 1.0 - exp(-tstep*(0.002)/h)
    else
    ! particle r>=10
      deprate = 1.0 - exp(-tstep*(0.002+part%grv)/h)
    ! complete deposition when particle hits ground
      if (part%z == vlevel(1)) deprate = 1.
    endif
    dep = deprate*part%rad
    part%rad = part%rad - dep
    i = nint(part%x)
    j = nint(part%y)
    mm = def_comp(m)%to_running
  !$OMP atomic
    depdry(i,j,mm) = depdry(i,j,mm) + dble(dep)
  end if
end subroutine drydep2

pure subroutine aerodynres(L, ustar, z0, raero)
  real, intent(in) :: L, ustar, z0
  real, intent(out) :: raero

  real, parameter :: ka = 0.4
  real, parameter :: z = 30 ! Assumed height of surface/constant flux layer

  real :: fac, fi

  if (L < 0) then
    fac = 0.598 + 0.390 * log(-z / L) - 0.09 * (log(-z / L)) ** 2
    fi = exp(fac)
  else if (L > 0) then
    fi = -5 * (z/L)
  else
    fi = 0
  endif

  raero = (1 / (ka * ustar)) * (log(z/z0) - fi)


end subroutine

pure elemental real function gravitational_settling(roa, diam) result(vs)
    real, intent(in) :: roa
    !> TODO: Check unit!!!
    real, intent(in) :: diam

    real, parameter :: ro_part = 2300 ! kg m-3, corresponds to Cs 2.3 g/cm3
    real, parameter :: grav = 9.8
    real, parameter :: ny = 1.5e-5 ! Kinematic viscosity of air, m2 s-1 at +15 C
    real, parameter :: lambda = 0.065e-6 ! Mean free path of air molecules [m]
    real :: my ! Dynamic visocity of air, kg m-1 s-1

    real :: fac1, cslip

    my = ny * roa
    fac1 = -0.55 * diam / lambda
    cslip = 1 + 2 * lambda / diam * ( 1.257 + 0.4 * exp(fac1) )

    vs = ro_part * diam * grav * cslip / (18*my)
end function

impure elemental subroutine drydep_emep_vd(surface_pressure, t2m, yflux, xflux, z0, hflux, leaf_area_index, diam, vd_dep)
  !> In hPa
  real, intent(in) :: surface_pressure
  real, intent(in) :: t2m
  real, intent(in) :: yflux, xflux
  real, intent(in) :: z0, hflux
  real, intent(in) :: leaf_area_index
  real, intent(in) :: diam
  real, intent(out) :: vd_dep

  real :: roa
  real :: ustar
  real :: monin_obukhov_length
  real :: SAI

  real, parameter :: R = 287.05
  real, parameter :: CP = 1005.0
  real, parameter :: k = 0.4
  real, parameter :: g = 9.8
  ! real, parameter :: a1 = 0.002
  real :: a1
  real, parameter :: a2 = 300
  real :: a1sai
  real :: vs
  real :: rsemep
  real :: raero
  real :: fac


  roa = surface_pressure / (t2m * R)
  vs = gravitational_settling(roa, diam)

  ustar = hypot(yflux, xflux) / sqrt(roa)
  monin_obukhov_length = - roa * CP * t2m * (ustar**3) / (k * g * hflux)

  SAI = leaf_area_index + 1
  a1sai = 0.008 * SAI / 10

  call aerodynres(monin_obukhov_length, ustar, z0, raero)

  if (leaf_area_index > 0.75 .and. a1sai > 0.002) then
    a1 = a1sai
  else
    a1 = 0.002
  endif

  monin_obukhov_length = max(-25.0, monin_obukhov_length)
  if (monin_obukhov_length > 0) then
    rsemep = 1.0 / (ustar * a1)
  else
    fac = (-a2 / monin_obukhov_length) ** ( 2.0 / 3.0 )
    rsemep = 1.0 / (ustar * a1 * (1 + fac))
  endif

  vd_dep = 1.0 / (rsemep + raero) + vs

  write(*,*) "ps:     ", surface_pressure
  write(*,*) "t2m:    ", t2m
  write(*,*) "xflux:  ", xflux
  write(*,*) "yflux:  ", yflux
  write(*,*) "hflux:  ", hflux
  write(*,*) "z0:     ", z0
  write(*,*) "LAI:    ", leaf_area_index
  write(*,*) "diam:   ", diam
  write(*,*) "vs:     ", vs
  write(*,*) "ustar:  ", ustar
  write(*,*) "SAI:    ", SAI
  write(*,*) "L:      ", monin_obukhov_length
  write(*,*) "a1sai:  ", a1sai
  write(*,*) "raero:  ", raero
  write(*,*) "rsemep: ", rsemep
  write(*,*) "vd:     ", vd_dep

  error stop "ONLY CHECK ONE ITERATION"

end subroutine

subroutine drydep_emep(tstep, vd, part)
  use particleML, only: Particle
  use snapfldML, only: depdry
  use snapparML, only: def_comp

  !> Timestep of the simulation in seconds
  real, intent(in) :: tstep
  !> Particle which observes deposition
  type(Particle), intent(inout) :: part
  !> Deposition velocity
  real, intent(in) :: vd(:,:, :)

  integer :: m, mm, i, j
  real :: dep


  m = part%icomp
  if (def_comp(m)%kdrydep == 1) then

    mm = def_comp(m)%to_running

    i = nint(part%x)
    j = nint(part%y)
    dep = part%rad*vd(i,j,mm)*tstep

    part%rad = part%rad - dep
    !$OMP atomic
    depdry(i,j,mm) = depdry(i,j,mm) + dble(dep)
  end if
end subroutine

end module drydep

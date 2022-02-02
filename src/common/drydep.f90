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
  use ISO_FORTRAN_ENV, only: real64
  implicit none
  private

  public :: drydep1, drydep2, drydep_emep_vd, drydep_zhang_emerson_vd, &
    drydep_nonconstant_vd, gravitational_settling

  integer, parameter, public :: DRYDEP_SCHEME_UNDEFINED = 0
  integer, parameter, public :: DRYDEP_SCHEME_OLD = 1
  integer, parameter, public :: DRYDEP_SCHEME_NEW = 2
  integer, parameter, public :: DRYDEP_SCHEME_EMEP = 3
  integer, parameter, public :: DRYDEP_SCHEME_ZHANG = 4
  integer, parameter, public :: DRYDEP_SCHEME_EMERSON = 5

  !> The active dry deposition scheme
  integer, save, public :: drydep_scheme = DRYDEP_SCHEME_UNDEFINED


  real(real64), parameter :: R = 287.05
  real(real64), parameter :: grav = 9.8
  real(real64), parameter :: CP = 1005.0
  real(real64), parameter :: pi = 2.0*asin(1.0)

  !> Kinematic viscosity of air, m2 s-1 at +15 C
  real(real64), parameter :: ny = 1.5e-5
  !> Mean free path of air molecules [m]
  real(real64), parameter :: lambda = 0.065e-6

  !> Boltzmanns constant, J K-1
  real(real64), parameter :: bolzc = 1.380649e-23


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
      deprate = 1.0 - exp(-tstep*(0.002)/h)
    else
    ! particle r>=10
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

pure function aerodynres(L, ustar, z0) result(raero)
  real(real64), intent(in) :: L, ustar, z0
  real(real64) :: raero

  real(real64), parameter :: ka = 0.4
  real(real64), parameter :: z = 30 ! Assumed height of surface/constant flux layer

  real(real64) :: fac, fi

  if (L < 0) then
    fac = 0.598 + 0.390 * log(-z / L) - 0.09 * (log(-z / L)) ** 2
    fi = exp(fac)
  else if (L > 0) then
    fi = -5 * (z/L)
  else
    fi = 0
  endif

  raero = (1 / (ka * ustar)) * (log(z/z0) - fi)
end function

pure elemental function gravitational_settling(roa, diam, ro_part) result(vs)
    real(real64), intent(in) :: roa
    !> Diameter in m
    real(real64), intent(in) :: diam
    !> Density in kg m-3
    real(real64), intent(in) :: ro_part

    real(real64) :: vs

    real(real64) :: my ! Dynamic visocity of air, kg m-1 s-1

    real(real64) :: fac1, cslip

    my = ny * roa
    fac1 = -0.55 * diam / lambda
    cslip = 1 + 2 * lambda / diam * ( 1.257 + 0.4 * exp(fac1) )

    vs = ro_part * diam * diam * grav * cslip / (18*my)
end function

pure elemental subroutine drydep_emep_vd(surface_pressure, t2m, yflux, xflux, z0, &
    hflux, leaf_area_index, diam, density, vd_dep, &
    roa, ustar, monin_obukhov_length, raero, vs)
  !> In hPa
  real, intent(in) :: surface_pressure
  real, intent(in) :: t2m
  real, intent(in) :: yflux, xflux
  real, intent(in) :: z0, hflux
  real, intent(in) :: leaf_area_index
  real, intent(in) :: diam
  real, intent(in) :: density
  real, intent(out) :: vd_dep

  real(real64), intent(out) :: roa
  real(real64), intent(out) :: ustar
  real(real64), intent(out) :: monin_obukhov_length
  real(real64) :: SAI

  real(real64), parameter :: k = 0.4
  ! real, parameter :: a1 = 0.002
  real(real64) :: a1
  real(real64), parameter :: a2 = 300
  real(real64) :: a1sai
  real(real64), intent(out) :: vs
  real(real64) :: rsemep
  real(real64), intent(out) :: raero
  real(real64) :: fac


  roa = surface_pressure / (t2m * R)
  vs = real(gravitational_settling(real(roa, real64), real(diam, real64), real(density, real64)))

  ustar = hypot(yflux, xflux) / sqrt(roa)
  monin_obukhov_length = - roa * CP * t2m * (ustar**3) / (k * grav * hflux)

  SAI = leaf_area_index + 1
  a1sai = 0.008 * SAI / 10

  raero = aerodynres(real(monin_obukhov_length, real64), real(ustar, real64), real(z0, real64))

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
end subroutine

pure elemental subroutine drydep_zhang_emerson_vd(surface_pressure, t2m, yflux, xflux, z0, &
    hflux, leaf_area_index, diam, density, vd_dep, emerson_mode, &
    roa, ustar, monin_obukhov_length, raero, vs)
  use ieee_arithmetic, only: ieee_value, IEEE_QUIET_NAN
  !> In hPa
  real, intent(in) :: surface_pressure
  real, intent(in) :: t2m
  real, intent(in) :: yflux, xflux
  real, intent(in) :: z0, hflux
  real, intent(in) :: leaf_area_index
  real(real64), intent(in) :: diam
  real(real64), intent(in) :: density
  real, intent(out) :: vd_dep
  logical, intent(in) :: emerson_mode
  real(real64), intent(out) :: roa, monin_obukhov_length, raero, vs, ustar

  !> Aerial factor for interception (table 3 Zhang et al. (2001)), corresponding to evergreen
  !>  needleleaf trees (i.e. close to maximum deposition)
  real(real64) :: A
  real(real64), parameter :: k = 0.4
  !> Below this we ignore A
  real(real64), parameter :: lai_A_cutoff = 0.3

  real(real64) :: fac1, cslip, bdiff, my, sc, EB, EIM, EIN, rs, stokes

  roa = surface_pressure / (t2m * R)
  vs = gravitational_settling(roa, diam, density)

  ustar = hypot(yflux, xflux) / sqrt(roa)
  monin_obukhov_length = - roa * CP * t2m * (ustar**3) / (k * grav * hflux)
  raero = aerodynres(monin_obukhov_length, ustar, real(z0, real64))

  my = ny * roa

  fac1 = -0.55 * diam / lambda
  ! Cunningham slip factor
  cslip = 1 + 2 * lambda / diam * (1.257 + 0.4*exp(fac1))

  ! Brownian diffusion
  ! Brownian diffusivity of air (see equation 19 of Giardiana and Buffa, 2018)
  ! bdiff=2.83e-11 # Browian diffusion coefficient for 1 um particle (see Brereton, 2014)
   bdiff = bolzc * t2m * cslip / (3 * pi * my * diam)

  sc = ny / bdiff
  if (.not.emerson_mode) then
    ! A range og 0.5-0.58 dependening on the surface is given, 0.54=grass
    EB = sc ** (-0.54)
  else
    ! Revised Emerson et al. (2020)
    EB = 0.2 * sc ** (-2.0 / 3.0)
  endif

  if (leaf_area_index < lai_A_cutoff) then
    A = ieee_value(A, IEEE_QUIET_NAN)
  else if (leaf_area_index < 1) then
    A = 10 * 1e-3
  else if (leaf_area_index < 3) then
    A = 5 * 1e-3
  else
    A = 2 * 1e-3
  endif
  ! Impaction
  ! Stokes number for vegetated surfaces (Zhang (2001)
  if (leaf_area_index >= 1) then
    stokes = vs * ustar / (grav * A)
  else
    ! ???????
    stokes = vs * ustar * ustar / (grav * ny)
  endif

  if (.not.emerson_mode) then
    ! Zhang et al. (2001)
    EIM = (stokes / (0.8 + stokes)) ** 2
  else
    ! Revised Emerson et al. (2020)
    EIM = 0.4 * (stokes / (0.8 + stokes)) ** 1.7
  endif

  ! Interception
  ! if (leaf_area_index == 0) then
  if (leaf_area_index < lai_A_cutoff) then
    ! No interception over water surfaces
    EIN = 0.0
  else
    if (.not.emerson_mode) then
      ! Zhang et al. (2001)
      EIN = 0.5 * (diam / A) ** 2
    else
      ! Revised Emerson et al. (2020)
      EIN = 2.5 * (diam / A) ** 0.8
    endif
  endif

  rs = 1.0 / (3.0 * ustar * (EB + EIM + EIN))

  vd_dep = 1.0 / (raero + rs) + vs
end subroutine

subroutine drydep_nonconstant_vd(tstep, vd, part)
  use particleML, only: Particle
  use snapfldML, only: depdry
  use snapparML, only: def_comp

  !> Timestep of the simulation in seconds
  real, intent(in) :: tstep
  !> Particle which observes deposition
  type(Particle), intent(inout) :: part
  !> Deposition velocity
  real, intent(in) :: vd(:,:, :)

  real, parameter :: h = 30.0

  integer :: m, mm, i, j
  real :: dep, deprate


  m = part%icomp
  if (def_comp(m)%kdrydep == 1 .and. part%z > 0.996) then

    mm = def_comp(m)%to_running

    i = nint(part%x)
    j = nint(part%y)

    deprate = 1.0 - exp(-tstep*vd(i,j,mm)/h)
    dep = part%scale_rad(deprate)
    !$OMP atomic
    depdry(i,j,mm) = depdry(i,j,mm) + dble(dep)
  end if
end subroutine

end module drydep

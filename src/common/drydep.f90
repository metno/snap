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

module drydepml
  use ISO_FORTRAN_ENV, only: real64, int8, int16

  implicit none
  private

  public :: drydep, preprocess_landfraction, unload, &
    requires_extra_fields_to_be_read, drydep_precompute_meteo, drydep_precompute_particle, &
    requires_landfraction_file

  integer, parameter, public :: DRYDEP_SCHEME_UNDEFINED = 0
  integer, parameter, public :: DRYDEP_SCHEME_OLD = 1
  integer, parameter, public :: DRYDEP_SCHEME_NEW = 2
  integer, parameter, public :: DRYDEP_SCHEME_EMERSON = 5

  !> The active dry deposition scheme
  integer, save, public :: drydep_scheme = DRYDEP_SCHEME_UNDEFINED


  real(real64), parameter :: R = 287.05
  real(real64), parameter :: grav = 9.8
  real(real64), parameter :: CP = 1005.0
  real(real64), parameter :: pi = 2.0*asin(1.0)
  integer(int16), parameter :: LOOKUP_NAN = -32768 ! NaN value in zhang table, just something out of bounds

  !> Kinematic viscosity of air, m2 s-1 at +15 C
  real(real64), parameter :: ny = 1.5e-5
  !> Mean free path of air molecules [m]
  real(real64), parameter :: lambda = 0.065e-6

  !> Boltzmanns constant, J K-1
  real(real64), parameter :: bolzc = 1.380649e-23

  character(len=256), save, public :: largest_landfraction_file = "not set"

  integer(int8), save, public, allocatable :: classnr(:, :)

  contains

subroutine drydep(tstep, part)
  use snapfldML, only: vd_dep
  use particleML, only: Particle
  real, intent(in) :: tstep
  type(particle), intent(inout) :: part


  if (drydep_scheme == DRYDEP_SCHEME_OLD) call drydep1(part)
  if (drydep_scheme == DRYDEP_SCHEME_NEW) call drydep2(tstep, part)
  if (drydep_scheme == DRYDEP_SCHEME_EMERSON) call drydep_nonconstant_vd(tstep, vd_dep, part)
end subroutine

pure logical function requires_landfraction_file()
    requires_landfraction_file = (drydep_scheme == DRYDEP_SCHEME_EMERSON)
end function

pure logical function requires_extra_fields_to_be_read()
  requires_extra_fields_to_be_read = (drydep_scheme == DRYDEP_SCHEME_EMERSON)
end function


!> Precompute dry deposition meteorology
!> returns roa, ustar, monin_obukhov_length, raero, my
elemental subroutine drydep_precompute_meteo(surface_pressure, t2m, yflux, xflux, z0, hflux, &
                                             ustar, raero, my)
  use iso_fortran_env, only: real64
  use datetime, only: datetime_t
  real, intent(in) :: surface_pressure !> [Pa]
  real, intent(in) :: t2m !> [K]
  real, intent(in) :: yflux !> [N/m^2]
  real, intent(in) :: xflux !> [N/m^2]
  real, intent(in) :: z0 !> [m]
  real, intent(in) :: hflux !> [W s/m^2]
  real(real64), intent(out) :: raero, ustar, my

  real(real64), parameter :: k = 0.4

  real(real64) :: roa, monin_obukhov_length

  roa = surface_pressure / (t2m * R)
  ustar = hypot(yflux, xflux) / sqrt(roa)
  monin_obukhov_length = - roa * CP * t2m * (ustar**3) / (k * grav * hflux)
  raero = aerodynres(monin_obukhov_length, ustar, real(z0, real64))
  my = ny * roa
end subroutine


elemental subroutine drydep_precompute_particle(surface_pressure, t2m, &
    ustar, raero, my, date, &
    component, classnr, vd_dep)
  use iso_fortran_env, only: real64, int8
  use datetime, only: datetime_t
  use snapparML, only: defined_component
  real, intent(in) :: surface_pressure !> [Pa]
  real, intent(in) :: t2m !> [K]
  real(real64), intent(in) :: raero, ustar, my
  type(datetime_t), intent(in) :: date
  type(defined_component), intent(in) :: component
  integer(int8), intent(in) :: classnr !> Speficic mapping to land use type, see subroutine `lookup_A`
  real, intent(out) :: vd_dep ! m/s

  select case(drydep_scheme)
    case (DRYDEP_SCHEME_EMERSON)
      call drydep_emerson_vd(surface_pressure, t2m, &
            ustar, raero, my, date, &
            component, classnr, vd_dep)
    case default
      error stop "Precomputation should not be called for this dry deposition scheme"
  end select
end subroutine


!> Store landfraction values
subroutine preprocess_landfraction(values)
  use iso_fortran_env, only: real32, error_unit
  real(real32), intent(in) :: values(:,:)

  if (allocated(classnr)) then
    error stop "preprocess_landfraction is to be called once only"
  endif
  allocate(classnr(size(values,dim=1),size(values,dim=2)))
  classnr(:,:) = nint(values)
end subroutine

subroutine unload()
  if (allocated(classnr)) deallocate(classnr)
end subroutine

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

  integer :: m, i, j, mo
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
      mo = def_comp(m)%to_output
    !$OMP atomic
      depdry(i,j,mo) = depdry(i,j,mo) + dble(dep)
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

  integer :: m,i,j,mo
  real :: deprate, dep
  real, parameter :: h = 30.0  ! [m]
  real, parameter :: vd_gas = 0.008  ! [m/s]
  real, parameter :: vd_particles = 0.002  ! [m/s]

  m = part%icomp
!#### 30m = surface-layer (deposition-layer); sigma(hybrid)=0.996 ~ 30m
  if (def_comp(m)%kdrydep == 1 .AND. part%z > surface_height_sigma) then
  ! b...23.04.12... difference between particle and gas

    if (def_comp(m)%radiusmym <= 0.05) then
    ! gas
      deprate = 1.0 - exp(-tstep*(vd_gas)/h)
    else if (def_comp(m)%radiusmym <= 10.0) then
    ! particle 0.05<r<10
      deprate = 1.0 - exp(-tstep*(vd_particles + part%grv)/h)
    else
    ! particle r>=10.0
      deprate = 1.0 - exp(-tstep*(vd_particles + part%grv)/h)
    ! complete deposition when particle hits ground
      if (part%z == vlevel(1)) deprate = 1.
    endif
    dep = part%scale_rad(1.0 - deprate)
    i = hres_pos(part%x)
    j = hres_pos(part%y)
    mo = def_comp(m)%to_output
    !$OMP atomic
    depdry(i,j,mo) = depdry(i,j,mo) + dble(dep)
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


!> Table 3 for Zhang et. al 2001 https://doi.org/10.1016/S1352-2310(00)00326-5
elemental integer(int16) function lookup_A(classnr, seasonal_category)
  integer(int8), intent(in) :: classnr
  integer, intent(in) :: seasonal_category
  lookup_A = LOOKUP_NAN
  select case(classnr)
  case (11) ! Sea -> Z14
    lookup_A = LOOKUP_NAN
  case (12) ! Inland water -> Z13
    lookup_A = LOOKUP_NAN
  case (13) ! Tundra/desert -> Z8,Z9
    lookup_A = LOOKUP_NAN
  case (14) ! Ice and ice sheets -> Z12
    lookup_A = LOOKUP_NAN
  case (15) ! Urban -> Z15
    lookup_A = 10
  case (16) ! Crops -> Z7
    select case(seasonal_category)
    case (1,2,5)
      lookup_A = 2
    case (3,4)
      lookup_A = 5
    end select
  case (17) ! Grass -> Z6
    select case(seasonal_category)
    case (1,2,5)
      lookup_A = 2
    case (3,4)
      lookup_A = 5
    end select
  case (18) ! Wetlands -> Z11
    lookup_A = 10
  case (19) ! Evergreen needleleaf -> Z1
    lookup_A = 2
  case (20) ! Deciduous needleleaf -> Z3
    select case(seasonal_category)
    case (1,2,5)
      lookup_A = 2
    case (3,4)
      lookup_A = 5
    end select
  case (21) ! Mixed forest -> Z5
    lookup_A = 5
  case (22) ! Shrubs and interrupted woodlands -> Z10
    lookup_A = 10
  case default
    lookup_A = LOOKUP_NAN
  end select

end function

!> Dry deposition velocites based on
!> Emerson et al. 2020, Revisiting particle dry deposition and its role in radiative effect estimates
!> https://doi.org/10.1073/pnas.2014761117
pure elemental subroutine drydep_emerson_vd(surface_pressure, t2m, ustar, raero, my,&
    date, component, classnr, &
    vd_dep)
  use datetime, only: datetime_t
  use snapparML, only: defined_component
  use vgravtablesML, only: vgrav
  !> In hPa
  real, intent(in) :: surface_pressure
  real, intent(in) :: t2m
  real(real64), intent(in) :: raero, ustar, my
  type(datetime_t), intent(in) :: date
  type(defined_component), intent(in) :: component
  integer(int8), intent(in) :: classnr
  real, intent(out) :: vd_dep

  !> Aerial factor for interception (table 3 Zhang et al. (2001)), corresponding to evergreen
  !>  needleleaf trees (i.e. close to maximum deposition)
  real(real64) :: A, rs
  real :: diam, vs

  real(real64) :: fac1, cslip, bdiff, sc, EB, EIM, EIN, stokes
  integer(int16) :: Apar

  if (component%radiusmym <= 0.05) then ! gas
    vd_dep = 0.008 ! [m/s] see drydep2
    return
  end if

  diam = 2*component%radiusmym*1e-6
  vs = vgrav(component%to_running, surface_pressure/100., t2m)

  fac1 = -0.55 * diam / lambda
  ! Cunningham slip factor
  cslip = 1 + 2 * lambda / diam * (1.257 + 0.4*exp(fac1))

  ! Brownian diffusion
  ! Brownian diffusivity of air (see equation 19 of Giardiana and Buffa, 2018)
  ! bdiff=2.83e-11 # Browian diffusion coefficient for 1 um particle (see Brereton, 2014)
   bdiff = bolzc * t2m * cslip / (3 * pi * my * diam)

  sc = ny / bdiff
  EB = 0.2 * sc ** (-2.0 / 3.0)


  Apar = lookup_A(classnr, date_to_seasonal_category(date))
  if (Apar .ne. LOOKUP_NAN) then
    A = Apar * 1e-3
    ! Stokes number for vegetated surfaces (Zhang (2001) needed for impaction
    stokes = vs * ustar / (grav * A)
    ! Interception
    EIN = 2.5 * (diam / A) ** 0.8
  else
    stokes = vs * ustar * ustar / (grav * ny)
    ! No interception over water surfaces
    EIN = 0.0
  endif
  ! Impaction
  EIM = 0.4 * (stokes / (0.8 + stokes)) ** 1.7

  rs = 1.0 / (3.0 * ustar * (EB + EIM + EIN))
  vd_dep = 1.0 / (raero + rs) + vs
end subroutine

!> Apply precomputed dry deposition velocities
subroutine drydep_nonconstant_vd(tstep, vd, part)
  use particleML, only: Particle
  use snapfldML, only: depdry
  use snapparML, only: def_comp
  use snapdimML, only: hres_pos

  !> Timestep of the simulation in seconds
  real, intent(in) :: tstep
  !> Particle which observes deposition
  type(Particle), intent(inout) :: part
  !> Deposition velocity
  real, intent(in) :: vd(:,:, :)

  real, parameter :: h = 30.0

  integer :: m, mm, i, j, mo
  real :: dep, deprate_m1

  m = part%icomp
  if (def_comp(m)%kdrydep == 1 .and. part%z > 0.996) then

    mm = def_comp(m)%to_running

    i = nint(part%x)
    j = nint(part%y)

    deprate_m1 = exp(-tstep*vd(i,j,mm)/h)

    i = hres_pos(part%x)
    j = hres_pos(part%y)
    dep = part%scale_rad(deprate_m1)
    mo = def_comp(m)%to_output
    !$OMP atomic
    depdry(i,j,mo) = depdry(i,j,mo) + dble(dep)
  end if
end subroutine


!> Very simplified, should be adapted for latitude, but
!> good enough for MEPS in Norway for now.
!> Calibrated for lowland South Norway
pure integer function date_to_seasonal_category(date)
  use datetime, only: datetime_t
  type(datetime_t), value :: date
  integer, parameter :: dummy_year = 2024 ! Leap year

  date%year = dummy_year

  if (date <= datetime_t(dummy_year, 1, 11, 0)) then
    date_to_seasonal_category = 3
  elseif (date <= datetime_t(dummy_year, 3, 31, 0)) then
    date_to_seasonal_category = 4
  elseif (date <= datetime_t(dummy_year, 5, 15, 0)) then
    date_to_seasonal_category = 5
  elseif (date <= datetime_t(dummy_year, 8, 15, 0)) then
    date_to_seasonal_category = 1
  elseif (date <= datetime_t(dummy_year, 9, 15, 0)) then
    date_to_seasonal_category = 2
  else
    date_to_seasonal_category = 3
  endif
end function

end module drydepml

! SNAP: Servere Nuclear Accident Programme
! Copyright (C) 1992-2017   Norwegian Meteorological Institute
!
! This file is part of SNAP. SNAP is free software: you can
! redistribute it and/or modify it under the terms of the
! GNU General Public License as published by the
! Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https://www.gnu.org/licenses/>.

!> meteorology parameter definitions
!>
!> these settings can be changed by specifying 'grid.nctype'
!> in the input file, which calls ::init_meteo_params
module snapmetML
  use iso_fortran_env, only: error_unit
  implicit none
  private

  ! netcdf names for the various fields
  type, public :: met_params_t
    character(len=80) :: xwindv = ''
    character(len=80) :: ywindv = ''
    character(len=80) :: xwind10mv = ''
    character(len=80) :: ywind10mv = ''
    character(len=80) :: pottempv = ''
    character(len=80) :: ptopv = ''
    character(len=80) :: sigmadotv = ''
    character(len=80) :: apv = ''
    character(len=80) :: bv = ''
    character(len=80) :: sigmav = ''
    character(len=80) :: psv = ''
    character(len=80) :: mslpv = ''
    character(len=80) :: precaccumv = ''
    character(len=80) :: precstratiaccumv = ''
    character(len=80) :: precconaccumv = ''
    character(len=80) :: precstrativrt = ''
    character(len=80) :: precconvrt = ''
    character(len=80) :: total_column_rain = ''

    character(len=80) :: t2m = ''
    character(len=80) :: yflux = ''
    character(len=80) :: xflux = ''
    character(len=80) :: hflux = ''
    character(len=80) :: z0 = ''
    character(len=80) :: leaf_area_index = ''

    ! flags when reading the data
    logical :: temp_is_abs = .false.
    logical :: has_dummy_dim = .false.
    logical :: manual_level_selection = .false.
    logical :: sigmadot_is_omega = .false.
    logical :: need_precipitation = .true.
    !> Use lowest level in #xwindv/#ywindv in place of
    !> #xwind10mv/#ywind10mv
    logical :: use_model_wind_for_10m = .false.
  end type

  type(met_params_t), save, public :: met_params = met_params_t()
  ! units as used in SNAP for the different variables
 ! these should not be changed! Needed when reading fimex.
  character(len=*), parameter, public :: xy_wind_units = 'm/s'
  character(len=*), parameter, public :: pressure_units= 'hPa'
  character(len=*), parameter, public :: omega_units = 'hPa/s'
  character(len=*), parameter, public :: sigmadot_units = '1/s'
  character(len=*), parameter, public :: precip_rate_units = 'mm/hr' ! kg/m2/s
  character(len=*), parameter, public :: precip_units = 'kg/m2'
  !> Equivalency to `precip_units`, and scaling between the two =>
  !> precip_units / density of water = (kg / m^2) / (1000 kg/m^3) = 1 / 1000 m = mm
  character(len=*), parameter, public :: precip_units_fallback = 'mm'
  character(len=*), parameter, public :: temp_units = 'K'

  character(len=*), parameter, public :: downward_momentum_flux_units = 'N/m^2'
  character(len=*), parameter, public :: surface_roughness_length_units = 'm'
  character(len=*), parameter, public :: surface_heat_flux_units = 'W s/m^2'
  character(len=*), parameter, public :: leaf_area_index_units = '1'

  public init_meteo_params, requires_precip_deaccumulation

  contains

  subroutine init_meteo_params(nctype, ierr)
    character(len=*), intent(in) :: nctype
    integer, intent(out) :: ierr

    ierr = 0
    select case (nctype)
    case('h12')
      met_params%xwindv = 'x_wind_ml'
      met_params%ywindv = 'y_wind_ml'
      met_params%xwind10mv = 'x_wind_10m'
      met_params%ywind10mv = 'y_wind_10m'
      met_params%pottempv = 'air_potential_temperature_ml'
      met_params%sigmav = ''
      met_params%ptopv = ''
      met_params%apv = 'ap'
      met_params%bv = 'b'
      met_params%sigmadotv = 'omega_ml'
      met_params%sigmadot_is_omega = .true.
      met_params%psv = 'surface_air_pressure'
      met_params%mslpv = 'air_pressure_at_sea_level'
      met_params%precaccumv = 'precipitation_amount_acc'
      met_params%precstrativrt = ''
      met_params%precconvrt = ''
!..get grid parameters from field identification
    case('h12_grib')
!..h12 results converted directly from grib-files via fimex
      met_params%manual_level_selection = .true.
      met_params%has_dummy_dim = .true.
      met_params%xwindv = 'x_wind_ml'
      met_params%ywindv = 'y_wind_ml'
      met_params%xwind10mv = 'x_wind_10m'
      met_params%ywind10mv = 'y_wind_10m'
!       !! real temperature, convert to pot-temp later
! semipalatinsk uses air_temp
      met_params%pottempv = 'air_temperature_ml'
      met_params%temp_is_abs = .true.
! chernobyl uses pot-temp
!        pottempv = 'air_potential_temperature_ml'
      met_params%sigmav = ''
      met_params%ptopv = ''
      met_params%apv = 'ap'
      met_params%bv = 'b'
! upward_air_velocity_ml, not used yet?
! semipalatinsk
      met_params%sigmadotv = ''
! chernobyl
!        sigmadotv = 'omega_ml'
      met_params%psv = 'surface_air_pressure'
      met_params%mslpv = ''
      met_params%precaccumv = ''
      met_params%precstrativrt = ''
      met_params%precconvrt = ''
      met_params%precstratiaccumv = 'lwe_thickness_of_stratiform_precipitation_amount'
      met_params%precconaccumv = 'lwe_thickness_of_convective_precipitation_amount'
!..get grid parameters from field identification

    case('ec_det')
      met_params%manual_level_selection = .true.
      met_params%has_dummy_dim = .true.
      met_params%xwindv = 'x_wind_ml'
      met_params%ywindv = 'y_wind_ml'
      met_params%xwind10mv = 'x_wind_10m'
      met_params%ywind10mv = 'y_wind_10m'
!       !! real temperature, convert to pot-temp later
      met_params%pottempv = 'air_temperature_ml'
      met_params%temp_is_abs = .true.
      met_params%sigmav = ''
      met_params%ptopv = ''
      met_params%apv = 'ap'
      met_params%bv = 'b'
! upward_air_velocity_ml, not used yet?
      met_params%sigmadotv = ''
      met_params%psv = 'surface_air_pressure'
      met_params%mslpv = ''
      met_params%precaccumv = ''
      met_params%precstrativrt = ''
      met_params%precconvrt = ''
      met_params%precstratiaccumv = 'lwe_thickness_of_stratiform_precipitation_amount_acc'
      met_params%precconaccumv = 'lwe_thickness_of_convective_precipitation_amount_acc'
!..get grid parameters from field identification
    case('era5')
      met_params%manual_level_selection = .true.
      met_params%has_dummy_dim = .false.
      met_params%xwindv = 'x_wind_ml'
      met_params%ywindv = 'y_wind_ml'
      met_params%xwind10mv = 'x_wind_10m'
      met_params%ywind10mv = 'y_wind_10m'
!       !! real temperature, convert to pot-temp later
      met_params%pottempv = 'air_temperature_ml'
      met_params%temp_is_abs = .true.
      met_params%sigmav = ''
      met_params%ptopv = 'p0'
      met_params%apv = 'a' ! need ptopv to multiply ap=a*ptopv
      met_params%bv = 'b'
      met_params%sigmadotv = 'omega_ml' !'lagrangian_tendency_of_atmosphere_sigma_coordinate_ml'
      met_params%sigmadot_is_omega = .true.
      met_params%psv = 'surface_air_pressure'
      met_params%mslpv = 'air_pressure_at_sea_level'
      met_params%precaccumv = ''
      met_params%precstrativrt = 'precipitation_rate'
      met_params%precconvrt = ''
      met_params%precstratiaccumv = ''
      met_params%precconaccumv = ''
!..get grid parameters from field identification
    case('arome')
      met_params%manual_level_selection = .true.
      met_params%has_dummy_dim = .true.
      met_params%xwindv = 'x_wind_ml'
      met_params%ywindv = 'y_wind_ml'
      met_params%xwind10mv = 'x_wind_10m'
      met_params%ywind10mv = 'y_wind_10m'
!       !! real temperature, convert to pot-temp later
      met_params%pottempv = 'air_temperature_ml'
      met_params%temp_is_abs = .true.
      met_params%sigmav = ''
      met_params%ptopv = ''
      met_params%apv = 'ap'
      met_params%bv = 'b'
! upward_air_velocity_ml, not used yet?
      met_params%sigmadotv = ''
      met_params%psv = 'surface_air_pressure'
      met_params%mslpv = 'air_pressure_at_sea_level'
      met_params%precaccumv = 'precipitation_amount_acc'
      met_params%precstrativrt = ''
      met_params%precconvrt = ''

      met_params%t2m = 'air_temperature_2m'
      met_params%xflux = 'downward_northward_momentum_flux_in_air'
      met_params%yflux = 'downward_eastward_momentum_flux_in_air'
      met_params%z0 = 'surface_roughness_length'
      met_params%hflux = 'integral_of_surface_downward_sensible_heat_flux_wrt_time'
      met_params%leaf_area_index = 'leaf_area_index'
!..get grid parameters from field identification
    case('dmi_eps')
      met_params%has_dummy_dim = .true.
      met_params%xwindv = 'x_wind_ml'
      met_params%ywindv = 'y_wind_ml'
      met_params%xwind10mv = 'x_wind_10m'
      met_params%ywind10mv = 'y_wind_10m'
!       !! real temperature, convert to pot-temp later
      met_params%pottempv = 'air_temperature_ml'
      met_params%temp_is_abs = .true.
      met_params%sigmav = ''
      met_params%ptopv = ''
      met_params%apv = 'ap'
      met_params%bv = 'b'
! upward_air_velocity_ml, not used yet?
      met_params%sigmadotv = ''
      met_params%psv = 'surface_air_pressure'
!        mslpv = 'air_pressure_at_sea_level'
      met_params%precaccumv = ''
      met_params%precstratiaccumv = 'stratiform_precipitation_amount_acc'
      met_params%precconaccumv = 'convective_precipitation_amount_acc'
      met_params%precstrativrt = ''
      met_params%precconvrt = ''

    case('emep')
      met_params%manual_level_selection = .true.
      met_params%xwindv = 'u_wind'
      met_params%ywindv = 'v_wind'
      met_params%xwind10mv = 'u10'
      met_params%ywind10mv = 'v10'
      met_params%pottempv = 'potential_temperature'
      met_params%sigmav = 'k'
      met_params%ptopv = ''
      met_params%apv = ''
      met_params%bv = ''
      met_params%sigmadotv = ''
      met_params%psv = 'surface_pressure'
      met_params%mslpv = 'air_pressure_at_sea_level'
      met_params%precaccumv = ''
      met_params%precstratiaccumv = ''
!.. non accumulated precipitation rates in m/s
      met_params%precstrativrt = 'large_scale_precipitations'
      met_params%precconvrt = 'convective_precipitations'
    case('ecemep')
      met_params%manual_level_selection = .true.
      met_params%xwindv = 'u_wind'
      met_params%ywindv = 'v_wind'
      met_params%xwind10mv = 'u10'
      met_params%ywind10mv = 'v10'
      met_params%pottempv = 'potential_temperature'
      met_params%sigmav = ''
      met_params%ptopv = 'P0'
      met_params%apv = 'hyai'
      met_params%bv = 'hybi'
      met_params%sigmadotv = ''
      met_params%psv = 'surface_pressure'
!        mslpv = 'air_pressure_at_sea_level'
      met_params%precaccumv = ''
      met_params%precstratiaccumv = ''
!.. non accumulated precipitation rates in m/s
      met_params%precstrativrt = 'large_scale_precipitations'
      met_params%precconvrt = 'convective_precipitations'
!..get grid parameters from field identification
! set as long as sortfield still is called
    case('gfs_grib_filter_fimex')
      met_params%manual_level_selection = .true.
      met_params%xwindv = 'x_wind_pl'
      met_params%ywindv = 'y_wind_pl'
      met_params%xwind10mv = 'x_wind_10m'
      met_params%ywind10mv = 'y_wind_10m'
      met_params%pottempv = 'air_temperature_pl'
      met_params%temp_is_abs = .true.
      met_params%sigmav = ''
      met_params%ptopv = ''
      met_params%apv = 'ap'
      met_params%bv = 'b'
      met_params%sigmadotv = ''
      met_params%psv = 'surface_air_pressure'
!        mslpv = 'air_pressure_at_sea_level'
      met_params%precaccumv = ''
      met_params%precstratiaccumv = ''
!.. non accumulated precipitation rates in m/s
      met_params%precstrativrt = 'precipitation_flux'
      met_params%precconvrt = ''
!..get grid parameters from field identification
! set as long as sortfield still is called
!..get grid parameters from field identification
    case('SLIM')
      met_params%manual_level_selection = .true.
      met_params%has_dummy_dim = .true.
      met_params%xwindv = 'x_wind_ml'
      met_params%ywindv = 'y_wind_ml'
      met_params%xwind10mv = 'x_wind_10m'
      met_params%ywind10mv = 'y_wind_10m'
      met_params%pottempv = 'air_temperature_ml'
      met_params%temp_is_abs = .true.
      met_params%sigmadotv = 'ga_etadot_105'
      met_params%psv = 'surface_air_pressure'
      met_params%apv = 'ap'
      met_params%bv = 'b'
      met_params%need_precipitation = .false.
    case default
      write(error_unit,*) "undefined grid.nctype: ", nctype
      ierr = 1
    end select
  end subroutine init_meteo_params

  pure logical function requires_precip_deaccumulation() result(req)
    req = (met_params%precaccumv /= '') .or. (met_params%precstratiaccumv /= '')
  end function

end module snapmetML

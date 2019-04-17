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
!> many of these definitions can be changed in
!> from the setup-file with a call to init_meteo_params()
module snapmetML
  IMPLICIT NONE
  private

  character(len=80), save, public :: xwindv,ywindv,xwind10mv,ywind10mv,pottempv,ptopv
  character(len=80), save, public :: sigmadotv,apv,bv,sigmav,psv,mslpv,precaccumv
  character(len=80), save, public :: precstratiaccumv, precconaccumv
  character(len=80), save, public :: precstrativrt,precconvrt
  logical, save, public :: temp_is_abs, has_dummy_dim, manual_level_selection
  logical, save, public :: sigmadot_is_omega
!> Use model wind (bottom layer) in place of 10m wind
  logical, save, public :: use_model_wind_for_10m = .false.

  public init_meteo_params

  CONTAINS

  subroutine init_meteo_params()
      USE snapfilML, only: nctype
      IMPLICIT NONE

      temp_is_abs = .false.
      has_dummy_dim = .false.
      manual_level_selection = .false.
      sigmadot_is_omega = .false.
      if (nctype.eq.'h12') then
        xwindv = 'x_wind_ml'
        ywindv = 'y_wind_ml'
        xwind10mv = 'x_wind_10m'
        ywind10mv = 'y_wind_10m'
        pottempv = 'air_potential_temperature_ml'
        sigmav = ''
        ptopv = ''
        apv = 'ap'
        bv = 'b'
        sigmadotv = 'omega_ml'
        sigmadot_is_omega = .true.
        psv = 'surface_air_pressure'
        mslpv = 'air_pressure_at_sea_level'
        precaccumv = 'precipitation_amount_acc'
        precstrativrt = ''
        precconvrt = ''
!..get grid parameters from field identification
      else if (nctype.eq.'h12_grib') then
!..h12 results converted directly from grib-files via fimex
        manual_level_selection = .true.
        has_dummy_dim = .true.
        xwindv = 'x_wind_ml'
        ywindv = 'y_wind_ml'
        xwind10mv = 'x_wind_10m'
        ywind10mv = 'y_wind_10m'
!       !! real temperature, convert to pot-temp later
! semipalatinsk uses air_temp
        pottempv = 'air_temperature_ml'
        temp_is_abs = .true.
! chernobyl uses pot-temp
!        pottempv = 'air_potential_temperature_ml'
        sigmav = ''
        ptopv = ''
        apv = 'ap'
        bv = 'b'
! upward_air_velocity_ml, not used yet?
! semipalatinsk
        sigmadotv = ''
! chernobyl
!        sigmadotv = 'omega_ml'
        psv = 'surface_air_pressure'
        mslpv = ''
        precaccumv = ''
        precstrativrt = ''
        precconvrt = ''
        precstratiaccumv = 'lwe_thickness_of_stratiform_precipitation_amount'
        precconaccumv = 'lwe_thickness_of_convective_precipitation_amount'
!..get grid parameters from field identification

      else if (nctype.eq.'ec_det') then
        manual_level_selection = .true.
        has_dummy_dim = .true.
        xwindv = 'x_wind_ml'
        ywindv = 'y_wind_ml'
        xwind10mv = 'x_wind_10m'
        ywind10mv = 'y_wind_10m'
!       !! real temperature, convert to pot-temp later
        pottempv = 'air_temperature_ml'
        temp_is_abs = .true.
        sigmav = ''
        ptopv = ''
        apv = 'ap'
        bv = 'b'
! upward_air_velocity_ml, not used yet?
        sigmadotv = ''
        psv = 'surface_air_pressure'
        mslpv = ''
        precaccumv = ''
        precstrativrt = ''
        precconvrt = ''
        precstratiaccumv = 'lwe_thickness_of_stratiform_precipitation_amount'
        precconaccumv = 'lwe_thickness_of_convective_precipitation_amount'
!..get grid parameters from field identification
      else if (nctype.eq.'arome') then
        manual_level_selection = .true.
        has_dummy_dim = .true.
        xwindv = 'x_wind_ml'
        ywindv = 'y_wind_ml'
        xwind10mv = 'x_wind_10m'
        ywind10mv = 'y_wind_10m'
!       !! real temperature, convert to pot-temp later
        pottempv = 'air_temperature_ml'
        temp_is_abs = .true.
        sigmav = ''
        ptopv = ''
        apv = 'ap'
        bv = 'b'
! upward_air_velocity_ml, not used yet?
        sigmadotv = ''
        psv = 'surface_air_pressure'
        mslpv = 'air_pressure_at_sea_level'
        precaccumv = 'precipitation_amount_acc'
        precstrativrt = ''
        precconvrt = ''
!..get grid parameters from field identification
      else if (nctype.eq.'dmi_eps') then
        has_dummy_dim = .true.
        xwindv = 'x_wind_ml'
        ywindv = 'y_wind_ml'
        xwind10mv = 'x_wind_10m'
        ywind10mv = 'y_wind_10m'
!       !! real temperature, convert to pot-temp later
        pottempv = 'air_temperature_ml'
        temp_is_abs = .true.
        sigmav = ''
        ptopv = ''
        apv = 'ap'
        bv = 'b'
! upward_air_velocity_ml, not used yet?
        sigmadotv = ''
        psv = 'surface_air_pressure'
!        mslpv = 'air_pressure_at_sea_level'
        precaccumv = ''
        precstratiaccumv = 'stratiform_precipitation_amount_acc'
        precconaccumv = 'convective_precipitation_amount_acc'
        precstrativrt = ''
        precconvrt = ''

      else if (nctype.eq.'emep') then
        manual_level_selection = .true.
        xwindv = 'u_wind'
        ywindv = 'v_wind'
        xwind10mv = 'u10'
        ywind10mv = 'v10'
        pottempv = 'potential_temperature'
        sigmav = 'k'
        ptopv = ''
        apv = ''
        bv = ''
        sigmadotv = ''
        psv = 'surface_pressure'
        mslpv = 'air_pressure_at_sea_level'
        precaccumv = ''
        precstratiaccumv = ''
!.. non accumulated precipitation rates in m/s
        precstrativrt = 'large_scale_precipitations'
        precconvrt = 'convective_precipitations'
      else if (nctype.eq.'ecemep') then
        manual_level_selection = .true.
        xwindv = 'u_wind'
        ywindv = 'v_wind'
        xwind10mv = 'u10'
        ywind10mv = 'v10'
        pottempv = 'potential_temperature'
        sigmav = ''
        ptopv = 'P0'
        apv = 'hyai'
        bv = 'hybi'
        sigmadotv = ''
        psv = 'surface_pressure'
!        mslpv = 'air_pressure_at_sea_level'
        precaccumv = ''
        precstratiaccumv = ''
!.. non accumulated precipitation rates in m/s
        precstrativrt = 'large_scale_precipitations'
        precconvrt = 'convective_precipitations'
!..get grid parameters from field identification
! set as long as sortfield still is called
      else if (nctype.eq.'ec_n1s') then
        manual_level_selection = .true.
        has_dummy_dim = .true.
        xwindv = 'x_wind_ml'
        ywindv = 'y_wind_ml'
        xwind10mv = 'x_wind_10m'
        ywind10mv = 'y_wind_10m'
        pottempv = 'air_temperature_ml'
        temp_is_abs = .true.
        sigmav = ''
        ptopv = ''
        apv = 'ap'
        bv = 'b'
        sigmadotv = ''
        psv = 'surface_air_pressure'
        precaccumv = ''
        precstratiaccumv = 'lwe_thickness_of_stratiform_precipitation_amount_acc'
        precconaccumv = 'lwe_thickness_of_convective_precipitation_amount_acc'
        precstrativrt = ''
        precconvrt = ''
      else
        write(*,*) "undefined grid.nctype: ", nctype
        error stop 1
      end if
  end subroutine init_meteo_params



end module snapmetML

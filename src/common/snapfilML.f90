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

!> Common for input felt files
module snapfilML
    use iso_fortran_env, only: int16
    use fileInfoML, only: fileInfo
    use snapdimML, only: mavail, mfilef
    implicit none

    private

!> min. step in hours between field input
    integer, save, public :: nhfmin
!> max. step in hours between field input
    integer, save, public :: nhfmax
!> no. of input FELT files
    integer, save, public :: nfilef
!> no. of available timesteps with data, plus 'other' files
    integer, save, public :: navail
!> unsorted list of timesteps with data as defined in fileInfo
!>       plus files with 'other' data
    type(fileInfo), save, public :: iavail(mavail)
!> pointers etc. to lists in iavail:
!>
!>         kavail(1): pointer to first forward  sorted timestep
!>
!>         kavail(2): pointer to first backward sorted timestep
    integer, save, public :: kavail(2)
!> data used for model level fieldset 1 (u1,v1,t1,...)
    integer, save, public :: navailt1
!> data used for model level fieldset 2 (u2,v2,t2,...)
    integer, save, public :: navailt2
!> time range, first and last possible input time
    integer, save, public :: itimer(5,2)
!> limitation of used forecast lengths in each file
!>
!>            limfcf(1,n): min forecast length in hours
!>
!>            limfcf(2,n): max forecast length in hours
    integer, save, public :: limfcf(2,mfilef)
!>  file names
    character(len=1024), save, public :: filef(mfilef)
!> type of meteorology, used for different nc-inputs
!>
!> Accepted values (see also snapmetML::init_meteo_params()):
!> * h12
!> * h12_grib
!> * ec_det
!> * dmi_eps
!> * ecemep
      character(len=72), save, public :: nctype

      real, allocatable, save, public ::  fdata(:)
      integer(int16), allocatable, save, public :: idata(:)
      character(len=80), save, public :: nctitle
      character(len=1024), save, public :: ncsummary
      CHARACTER(LEN=19), save, public :: simulation_start

end module snapfilML

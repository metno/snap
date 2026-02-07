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
    use iso_fortran_env, only: int16, int32
    use snapdimML, only: mavail
    use datetime, only: datetime_t
    implicit none

    private

!> max. no. of input FELT files
    integer, parameter :: mfilef = 1024

!> min. step in hours between field input
    integer, save, public :: nhfmin = 6
!> max. step in hours between field input
    integer, save, public :: nhfmax = 12
!> no. of input FELT files
    integer, save, public :: nfilef = 0
!> no. of available timesteps with data, plus 'other' files
    integer, save, public :: navail
!> no. of steps to skip in from the meteorology
    integer, save, public :: spinup_steps = 1

!> analysis/forecast reference time
!> \see iavail
    type, public :: fileInfo
        integer(kind=int16) :: AYEAR
        integer(kind=int16) :: AMONTH
        integer(kind=int16) :: ADAY
        integer(kind=int16) :: AHOUR
        !> forecast hour, distance from reference time
        integer(kind=int16) :: FCHOUR
        !> file number in filename array
        integer(kind=int16) :: FILENO
        !> filetype 1=model 2=surface 3=both
        integer(kind=int16) :: FILETYPE
        !> timePos position of #fchour in current time
        integer(kind=int16) :: TIMEPOS
        !> offset in hours since 1970-01-01 00:00
        integer(kind=int32) :: OHOUR
        !> pointer to next forward time data
        integer(kind=int16) :: NAVAIL
        !> pointer to next backward (=previous) time data
        integer(kind=int16) :: PAVAIL
        !> pointer to previous time data in same file (needed for deaccumulation)
        integer(kind=int16) :: pavail_same_file
    end type fileInfo
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
    type(datetime_t), save, public :: itimer(2)
!>  file names
    character(len=1024), save, public :: filef(mfilef)
!> type of meteorology, used for different nc-inputs
!>
!> Accepted values
!> * h12
!> * h12_grib
!> * ec_det
!> * dmi_eps
!> * ecemep
!> \see snapmetML::init_meteo_params()
    character(len=72), save, public :: nctype = "*"

    real, allocatable, save, public ::  fdata(:)
    integer(int16), allocatable, save, public :: idata(:)
    character(len=:), save, public, allocatable :: nctitle
    character(len=1024), save, public :: ncsummary = ""
    character(len=19), save, public :: simulation_start

end module snapfilML

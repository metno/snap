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
!
module snapfilML
    use iso_fortran_env, only: int16
    use fileInfoML
    use snapdimML
    implicit none

    private
!
!..include file  -  common for input felt files
!
!
!..nhfmin: min. step in hours between field input
!..nhfmax: max. step in hours between field input
!..nfilef: no. of input FELT files
!..navail: no. of available timesteps with data, plus 'other' files
!..iavail: unsorted list of timesteps with data as defined in fileInfo
!        plus files with 'other' data
!..kavail: pointers etc. to lists in iavail:
!          kavail(1): pointer to first forward  sorted timestep
!          kavail(2): pointer to first backward sorted timestep
!  navailt1 : data used for model level fieldset 1 (u1,v1,t1,...)
!  navailt2 : data used for model level fieldset 2 (u2,v2,t2,...)
!..itimer: time range, first and last possible input time
!..limfcf: limitation of used forecast lengths in each file
!             limfcf(1,n): min forecast length in hours
!             limfcf(2,n): max forecast length in hours
!..filef:  file names
!..nctype: type of meteorology, used for different nc-inputs (emep,hirlam12)
!
      integer, save, public :: nhfmin,nhfmax,nfilef,navail,navailt1,navailt2
      integer, save, public :: kavail(2)
      TYPE(fileInfo), save, public ::  iavail(mavail)
      integer, save, public :: itimer(5,2),limfcf(2,mfilef)
! fdata(maxsiz), idata(ldata)
      real, pointer, save, public ::  fdata(:)
      integer(int16), pointer, save, public :: idata(:)
      character(len=1024), save, public :: filef(mfilef)
      character(len=72), save, public :: nctype
      character(len=80), save, public :: nctitle
      character(len=1024), save, public :: ncsummary
      CHARACTER(LEN=19), save, public :: simulation_start

end module snapfilML

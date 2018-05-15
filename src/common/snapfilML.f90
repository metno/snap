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
    use fileInfoML
    use snapdimML
    implicit none
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
      integer      nhfmin,nhfmax,nfilef,navail,navailt1,navailt2
      integer      kavail(2)
      TYPE(fileInfo) ::  iavail(mavail)
      integer      itimer(5,2),limfcf(2,mfilef)
! fdata(maxsiz), idata(ldata)
      real, pointer ::  fdata(:)
      integer*2, pointer :: idata(:)
      character*1024 filef(mfilef)
      character*72 nctype
      character*80 nctitle
      character*1024 ncsummary
      CHARACTER(LEN=19)  :: simulation_start
!
      common/cfiles/nhfmin,nhfmax,nfilef,navail &
                  ,iavail,kavail,navailt1,navailt2 &
                  ,itimer,limfcf &
                  ,fdata,idata &
                  ,filef,nctype,nctitle, ncsummary, simulation_start
end module snapfilML

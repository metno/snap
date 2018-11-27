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
module fileInfoML
    implicit none
    private
    ! information about file and file-order
    !          previous implementation
    !          iavail( 1,n): year    )
    !          iavail( 2,n): month   ) Time of analysis
    !          iavail( 3,n): day     ) (not valid time of forecast)
    !          iavail( 4,n): hour    )
    !          iavail( 5,n): forecast hour
    !          iavail( 6,n): file no. (in filename array)
    !          iavail( 7,n): 1=model levels 2=surface data 3=both
    !          iavail( 8,n): offset in hours from first (sorted) timestep
    !          iavail( 9,n): pointer to next forward  (time) data
    !          iavail(10,n): pointer to next backward (time) data
    !                    n=1,navail

    TYPE, PUBLIC :: fileInfo
        ! analysis/forecast reference time
        SEQUENCE
        INTEGER(KIND=2) :: AYEAR
        INTEGER(KIND=2) :: AMONTH
        INTEGER(KIND=2) :: ADAY
        INTEGER(KIND=2) :: AHOUR
        ! forecast our, distance from reference time
        INTEGER(KIND=2) :: FCHOUR
        ! file number in filename array
        INTEGER(KIND=2) :: FILENO
        ! filetype 1=model 2=surface 3=both
        INTEGER(KIND=2) :: FILETYPE
        ! timePos position of fchour in current time
        INTEGER(KIND=2) :: TIMEPOS
        ! offset in hours from first sorted timestep
        INTEGER(KIND=4) :: OHOUR
        ! pointer to next forward time data
        INTEGER(KIND=2) :: NAVAIL
        ! pointer to next backward (=previous) time data
        INTEGER(KIND=2) :: PAVAIL
    END TYPE fileInfo

end module fileInfoML

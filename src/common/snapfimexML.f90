! SNAP: Servere Nuclear Accident Programme
! Copyright (C) 1992-2021   Norwegian Meteorological Institute
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

!> @brief SNAP parameters for fimex
!!
!! The parameters in this file are usually set from the snap.input file
!! and used in the different *_fi.f90 files and only used when ftype = fimex
!!
module snapfimexML
  implicit none
  private
  !> fimex filetype, e.g. netcdf, grib, ncml for all files. All files have same type
  character(len=1024), public :: file_type = ""
  !> config file, applied to all files
  character(len=1024), public :: conf_file = ""

end module snapfimexML

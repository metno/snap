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

  !> parse the interpolation string, return 0 on success, 1 for  parse-errors
  public parse_interpolator

  !> fimex filetype, e.g. netcdf, grib, ncml for all files. All files have same type
  character(len=256), save, public :: file_type = ""
  !> config file, applied to all files
  character(len=256), save, public :: conf_file = ""

  !> string separated by | giving the new fimex-interpolator setup
  !> The string consist of method|proj_input|out_x_axis|out_y_axis|out_x_axis_unit|out_y_axis_unit
  !> method: none, nearest, bilinear, bicubic
  !> proj_input: a proj4 input string, e.g. +proj=stere +R=6310000 +no_defs
  !> out_x_axis,out_y_axis: axis description in m or degree, e.g. '1000,2000,...,50000'
  !> out_x_axis_unit, out_y_axis_unit: cf-units for the axes, e.g. degrees_north, degrees_east, m
  character(len=256), save, public :: interpolation = ""

  !> settings for fimex-interpolation (grid-interpolation)
  type, public :: fimex_interpolation
    !> interpolator method: -2= not-set, -1 = none, 0=nearest, 1=bilinear, 2=bicubic
    integer :: method = -2
    !> proj4 string
    character(len=1024) :: proj
    !> fimex description of x_axis, e.g. 0,1000,...,5000
    character(len=1024) :: x_axis
    !> fimex description of y_axis, e.g. 0,1000,...,5000
    character(len=1024) :: y_axis
    !> unit of x-axis, e.g. true=degree, false=m
    logical :: unit_is_degree
  end type fimex_interpolation
  type(fimex_interpolation), save, public :: fint

contains
  integer function parse_interpolator()
    USE iso_fortran_env, only: error_unit
    character(len=256), dimension(5) :: parts
    integer :: i = 1, prev_pos = 1, pos = 1
    integer method

    parse_interpolator = 0

    if (interpolation == "") then
      fint%method = -1
      return
    end if

    !write(error_unit,*) "interpolation: ", trim(interpolation)
    do while (pos /= 0 .and. pos < (1+len_trim(interpolation)) .and. i < 5)
      pos = index(interpolation(prev_pos:), '|')
      if (pos > 0) then
        pos = pos + prev_pos
        ! write(error_unit,*) "i,pos,prev_pos=", i, prev_pos, pos
        if (pos > prev_pos) then
          parts(i) = interpolation(prev_pos:pos-2)
          prev_pos = pos
        end if
        ! write(error_unit,*) "parts(i)=", parts(i)
        i = i+1
      endif
    end do
    parts(i) = interpolation(prev_pos:)
    select case (trim(parts(1)))
    case ('none')
      fint%method = -1
      return
    case ('nearest')
      method = 0
    case ('bilinear')
      method = 1
    case ('bicubic')
      method = 2
    case default
      parse_interpolator = 1
    end select

    do i = 2,4
      if (parts(i) == "") then
        parse_interpolator = 1
      endif
    end do

    if (parts(5)(1:1) == 'm') then
      fint%unit_is_degree = .false.
    else
      fint%unit_is_degree = .true.
    end if

    if (parse_interpolator == 0) fint%method = method
    fint%proj = parts(2)
    fint%x_axis = parts(3)
    fint%y_axis = parts(4)
    return
  end function parse_interpolator

end module snapfimexML

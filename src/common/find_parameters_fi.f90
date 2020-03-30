! SNAP: Servere Nuclear Accident Programme
! Copyright (C) 1992-2020  Norwegian Meteorological Institute
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
!> Tries to detect parameters from the given
!> fimex input-source
module find_parameters_fi
  use iso_fortran_env, only: error_unit, int32, real32, real64
  use netcdf ! should be removed
  use snapmetML
  use Fimex, only: FimexIO, AXIS_GeoX, AXIS_GeoY, AXIS_Lon, AXIS_Lat, AXIS_GeoZ
  use readfield_fiML, only: check, fi_checkload
  use utils, only: atof
  implicit none
  private

  ! Grid types
  integer, parameter :: POLARSTEREOGRAPHIC_60N = 1
  integer, parameter :: GEOGRAPHIC = 2
  integer, parameter :: SPHERICAL_ROTATED = 3
  integer, parameter :: POLARSTEREOGRAPHIC = 4
  integer, parameter :: MERCATOR = 5
  integer, parameter :: LAMBERT = 6

  integer, parameter :: ERROR_THIS_MODULE = -451123435

  public detect_gridparams_fi

contains

  !> Tries to detect grid parameters given by the
  !> netcdf file, taking the projection from
  !> the varname
  subroutine detect_gridparams_fi(file, config, type, varname, nx, ny, igtype, gparam, klevel, stat)
    !> Path to the netcdf file
    character(len=*), intent(in) :: file
    character(len=*), intent(in) :: config
    character(len=*), intent(in) :: type
    character(len=*), intent(in) :: varname
    !> Number of longitudes
    integer, intent(out) :: nx
    !> Number of latitudes
    integer, intent(out) :: ny
    !> Grid type
    !> * 1 : polarstereographic grid (true at 60N)
    !> * 2 : geographic
    !> * 3 : spherical rotated grid
    !> * 4 : polarstereographic grid
    !> * 5 : mercator grid (unrotated)
    !> * 6 : lambert (tangent, non-oblique) grid
    integer, intent(out) :: igtype
    !> grid parameters (for use in ::xyconvert)
    real, intent(out) :: gparam(6)
    !> Forms the k-levels to search, starting from max
    !> of the hybrid dimension, to 1, skipping 00
    !> nk, nk-1, ..., 1
    integer, allocatable, intent(out) :: klevel(:)
    !> Error code, 0 for success
    integer, intent(out) :: stat

    TYPE(FimexIO) :: fio
    integer(int32), dimension(:), allocatable :: start, length, atypes
    integer :: i, xpos, ypos, kpos, ndims, nk
    character(LEN=1024) :: proj4, xdim, ydim, kdim

    stat = 0
    stat = fio%open (file, config, type)
    if (stat /= 0) then
      write (error_unit, *) "Can't make io-object with file:"//trim(file)//" config: "//config
      return
    endif

    ! Initialize the slicebuilder and get dimensions
    ndims = fio%get_dimensions(varname)
    if (ndims <= 0) &
      call check(ndims, "can't make slicebuilder for "//TRIM(varname))

    ALLOCATE (start(ndims))
    ALLOCATE (length(ndims))
    ALLOCATE (atypes(ndims))

    call check(fio%get_dimension_start_size(start, length), "reading dim-sizes for "//TRIM(varname))
    call check(fio%get_axistypes(atypes), "reading dim-types for "//TRIM(varname))

    xpos = 0
    ypos = 0
    DO i = 1, ndims
      SELECT CASE (atypes(i))
      CASE (AXIS_GeoX, AXIS_Lon) ! full x-range
        xpos = i
      CASE (AXIS_GeoY, AXIS_Lat) ! full y-range
        ypos = i
      CASE (AXIS_GeoZ)
        kpos = i
      END SELECT
    END DO
    if (xpos == 0 .or. ypos == 0 .or. kpos == 0) then
      write (error_unit, *) "Could not determine the size of the grids"
      stat = ERROR_THIS_MODULE
      return
    end if
    nx = length(xpos)
    ny = length(ypos)
    nk = length(kpos)
    xdim = fio%get_dimname(xpos)
    ydim = fio%get_dimname(ypos)
    kdim = fio%get_dimname(kpos)

    proj4 = fio%get_proj4()
    select case (proj_arg(proj4, 'proj'))

    case ("latlon", "lonlat")
      call geographic_grid(fio, proj4, nx, ny, xdim, ydim, igtype, gparam, rotated=.false., stat=stat)

    case ("lcc")
      call lambert_grid(fio, varname, proj4, nx, ny, xdim, ydim, igtype, gparam, stat)

    case default
      write (error_unit, *) "This projection type is lacking a grid mapping: ", TRIM(proj_arg(proj4, 'proj'))
      stat = ERROR_THIS_MODULE
    end select
    if (stat /= 0) then
      return
    endif

    ! set the klevels = 0, nk-1, nk-2, ..., 1
    ! skipping the lowest level, which usually is much below 80m and as such not suitable for dry-dep-velocities
    allocate (klevel(nk))
    klevel(1) = 0
    do i = 2, nk
      klevel(i) = nk - i + 1
    enddo

  end subroutine detect_gridparams_fi

  !> Get the value of a key in the proj4 string
  !! @return string, or "" on error
  FUNCTION proj_arg(proj4, parg)
    !> proj4 string
    CHARACTER(len=*), INTENT(IN) :: proj4
    !> proj4 argument, without + and =, e.g. proj, lon_0
    CHARACTER(len=*), INTENT(IN) :: parg
    CHARACTER(len=1024) :: proj_arg
    CHARACTER(len=1024) :: substr

    INTEGER :: i

    i = INDEX(proj4, TRIM("+"//TRIM(parg)//"="))
    if (i == 0) then
      proj_arg = ""
      return
    end if

    i = i + len("+"//TRIM(parg)//"=")
    substr = proj4(i:)

    i = INDEX(substr, " ")
    if (i == 0) then
      proj_arg = substr
    else
      proj_arg = substr(:i)
    end if
    return
  END FUNCTION proj_arg

  !> determine gparam for latitude longitude grids
  subroutine geographic_grid(fio, proj4, nx, ny, xdim, ydim, igtype, gparam, rotated, stat)
    !> open fimex file
    TYPE(FimexIO), intent(inout) :: fio
    !> proj4 string
    character(1024), intent(in) :: proj4
    !> xaxis-size
    integer, intent(in) :: nx
    !> yaxis-name
    integer, intent(in) :: ny
    !> xaxis-name
    character(1024), intent(in) :: xdim
    !> yaxis-name
    character(1024), intent(in) :: ydim
    !> grid type
    integer, intent(out) :: igtype
    !> grid parameters
    real, intent(out) :: gparam(6)
    !> whether the grid is rotated or standard
    logical, intent(in) :: rotated
    !> error code (0 for success)
    integer, intent(out) :: stat

    real(real32), allocatable :: dims(:), latlons(:, :)
    character(1024) :: pval

    stat = 0
    if (.not. rotated) then
      igtype = GEOGRAPHIC
      gparam(5:6) = 0
    else
      igtype = SPHERICAL_ROTATED
      stat = ERROR_THIS_MODULE
      if (stat /= 0) return
      gparam(5) = 180 + gparam(5) ! need equator projection
      gparam(6) = 0 + 90 - gparam(6) ! need equator projection
    endif

    ! Getting the longitude oriented gparams
    allocate (dims(nx))
    call fi_checkload(fio, xdim, "degree", dims)
    gparam(1) = dims(1)
    gparam(3) = dims(2) - dims(1)

    ! Getting the latitude oriented gparams
    allocate (dims(ny))
    call fi_checkload(fio, ydim, "degree", dims)
    gparam(2) = dims(1)
    gparam(4) = dims(2) - dims(1)

  end subroutine

  !> determine gparam for lamber grids
  subroutine lambert_grid(fio, varname, proj4, nx, ny, xdim, ydim, igtype, gparam, stat)
    !> open fimex file
    TYPE(FimexIO), intent(inout) :: fio
    !> proj4 string
    character(len=*), intent(in) :: varname
    !> proj4 string
    character(len=*), intent(in) :: proj4
    !> xaxis-size
    integer, intent(in) :: nx
    !> yaxis-name
    integer, intent(in) :: ny
    !> xaxis-name
    character(len=*), intent(in) :: xdim
    !> yaxis-name
    character(len=*), intent(in) :: ydim
    !> grid type
    integer, intent(out) :: igtype
    !> grid parameters
    real, intent(out) :: gparam(6)
    !> error code (0 for success)
    integer, intent(out) :: stat

    real(kind=real64), allocatable, target :: dims(:), latlons(:)
    character(1024) :: pval, longname, latname
    integer :: ndims

    igtype = LAMBERT

    stat = 0
    ! Get first values of longitude/latitude - need to guess that coordinates are named longitude, latitude
    longname = fio%get_var_longitude(varname)
    ndims = fio%get_dimensions(longname)
    allocate (latlons(ny*nx))
    stat = fio%read (longname, latlons, "degree")
    gparam(1) = latlons(1)
    if (stat /= 0) return
    latname = fio%get_var_latitude(varname)
    ndims = fio%get_dimensions(latname)
    stat = fio%read (latname, latlons, "degree")
    if (stat /= 0) return
    gparam(2) = latlons(1)

    ! get increment in km
    allocate (dims(nx))
    ndims = fio%get_dimensions(xdim)
    stat = fio%read (xdim, dims, "km")
    if (stat /= 0) return
    gparam(3) = dims(2) - dims(1)
    deallocate (dims)
    allocate (dims(ny))
    ndims = fio%get_dimensions(ydim)
    stat = fio%read (ydim, dims, "km")
    if (stat /= 0) return
    gparam(4) = dims(2) - dims(1)

    ! Get reference longitude
    pval = proj_arg(proj4, "lon_0")
    if (pval == "") then
      gparam(5) = 0
    else
      gparam(5) = atof(pval)
    end if

    ! Get reference longitude
    pval = proj_arg(proj4, "lat_0")
    if (pval == "") then
      gparam(6) = 90
    else
      gparam(6) = atof(pval)
    end if

  end subroutine lambert_grid

end module find_parameters_fi

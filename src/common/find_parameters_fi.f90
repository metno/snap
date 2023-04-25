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
  use snapmetML
  use Fimex, only: FimexIO, AXIS_GeoX, AXIS_GeoY, AXIS_Lon, AXIS_Lat, AXIS_GeoZ
  use readfield_fiML, only: check, fi_checkload, fimex_open
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
  subroutine detect_gridparams_fi(file, varname, nx, ny, igtype, gparam, klevel, stat)
    !> Path to the netcdf file
    character(len=*), intent(in) :: file
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

    call fimex_open(file, fio)

    stat = 0
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

    case ("latlon", "lonlat", "latlong", "longlat")
      call geographic_grid(fio, proj4, nx, ny, xdim, ydim, igtype, gparam, rotated=.false., stat=stat)

    case ("lcc")
      call lambert_grid(fio, varname, proj4, nx, ny, xdim, ydim, igtype, gparam, stat)

    case ("stere")
      call polar_stereographic_grid(fio, proj4, nx, ny, xdim, ydim, igtype, gparam, stat)


    case ("ob_tran")
      call geographic_grid(fio, proj4, nx, ny, xdim, ydim, igtype, gparam, rotated=.true., stat=stat)

    case default
      write (error_unit, *) "This projection type is lacking a grid mapping: ", TRIM(proj_arg(proj4, 'proj'))
      stat = ERROR_THIS_MODULE
    end select
    if (stat /= 0) then
      return
    endif

    ! set the klevels = 0, nk, nk-1, nk-2, ..., 1
    if (allocated(klevel)) then
      write(error_unit,*) "Manual klevel selection is not implmented when fimex reading"
      deallocate(klevel)
    endif

    allocate (klevel(nk+1))
    klevel(1) = 0
    do i = 2, nk+1
      klevel(i) = (nk+1) - i + 1
    enddo

    stat = fio%close()
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

    real(real64), allocatable, target :: dims(:)
    character(1024) :: pval
    integer :: n

    write (error_unit, *) "geographic ", TRIM(xdim), nx, TRIM(ydim), ny

    stat = 0
    if (.not. rotated) then
      igtype = GEOGRAPHIC
      gparam(5:6) = 0
    else
      pval = proj_arg(proj4, "o_proj")
      if (pval /= "longlat") then
        stat = ERROR_THIS_MODULE
        return
      endif

      pval = proj_arg(proj4, "lon_0")
      if (pval == "") then
        stat = ERROR_THIS_MODULE
        return
      endif
      ! No projection of longitude is required
      gparam(5) = atof(pval)

      pval = proj_arg(proj4, "o_lat_p")
      if (pval == "") then
        stat = ERROR_THIS_MODULE
        return
      endif
      gparam(6) = atof(pval)

      igtype = SPHERICAL_ROTATED
      ! Projection from pole to equator
      gparam(6) = 0 + 90 - gparam(6)
    endif

    ! Getting the longitude oriented gparams
    n = fio%get_dimensions(xdim)
    allocate (dims(nx))
    stat = fio%read (xdim, dims, "degree")
    if (stat /= 0) return
    gparam(1) = dims(1)
    gparam(3) = dims(2) - dims(1)
    deallocate(dims)

    ! Getting the latitude oriented gparams
    n = fio%get_dimensions(ydim)
    allocate (dims(ny))
    stat = fio%read (ydim, dims, "degree")
    if (stat /= 0) return
    gparam(2) = dims(1)
    gparam(4) = dims(2) - dims(1)

  end subroutine

  !> determine gparam for lambert conformal conic grids
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

  !> determine gparam for polar_stereographic grids
  subroutine polar_stereographic_grid(fio, proj4, nx, ny, xdim, ydim, igtype, gparam, stat)
    !> open fimex file
    TYPE(FimexIO), intent(inout) :: fio
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

    real(kind=real64) :: pi
    real(kind=real64) :: lat0, incr, startX, startY
    real(kind=real64), allocatable, target :: dims(:)
    character(1024) :: pval
    integer :: ndims

    PI = 4.D0*DATAN(1.D0)

    igtype = POLARSTEREOGRAPHIC

    ! Check polar stereographic
    pval = proj_arg(proj4, "lat_0")
    if (pval == "") then
      lat0 = 0
    else
      lat0 = atof(pval)
    end if
    if (lat0 /= 90.) then
      stat = 1
      write (error_unit, *) "SNAP requires polar_stereographic: got lat_0= ", lat0
      return
    endif

    stat = 0
    ! get increment in m
    allocate (dims(nx))
    ndims = fio%get_dimensions(xdim)
    stat = fio%read (xdim, dims, "m")
    if (stat /= 0) return
    startX = dims(1)
    incr = dims(2) - dims(1)
    deallocate (dims)

    allocate (dims(ny))
    ndims = fio%get_dimensions(ydim)
    stat = fio%read (ydim, dims, "m")
    if (stat /= 0) return
    startY = dims(1)

    if ((dims(2) - dims(1)) /= incr) then
      write (error_unit, *) "SNAP requires equal distance polar_stereographic: xdelta ", &
        incr, " != ydelta ", (dims(2)-dims(1))
      stat = 1
      return
    end if

    ! Get reference longitude
    pval = proj_arg(proj4, "lon_0")
    if (pval == "") then
      gparam(4) = 0
    else
      gparam(4) = atof(pval)
    end if

    ! Get true scale latitude
    pval = proj_arg(proj4, "lat_ts")
    if (pval == "") then
      gparam(5) = 60
    else
      gparam(5) = atof(pval)
    end if

    ! gparam1/2 is distance in cells of start to northpole in fortran counting (northpole = 1)
    gparam(1) = 1 - startX/incr
    gparam(2) = 1 - startY/incr

    ! gparam(3) is number of grid-cells between equator and northpole
    gparam(3) = 6371000 * (1 + sin(PI/180.*gparam(5)))/incr

  end subroutine polar_stereographic_grid

end module find_parameters_fi

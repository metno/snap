!> Tries to detect parameters from the given
!> netcdf file
module find_parameter
  use iso_fortran_env, only: error_unit
  use snapmetML, only: met_params
  use netcdf
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

  public :: detect_gridparams
  public :: get_klevel

contains

  !> Tries to detect grid parameters given by the
  !> netcdf file, taking the projection from
  !> the #met_params%xwindv variable
  subroutine detect_gridparams(ncfile, nx, ny, igtype, gparam, stat)
    !> Path to the netcdf file
    character(len=*), intent(in) :: ncfile
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
    !> Error code, 0 for success
    integer, intent(out) :: stat

    integer :: ncid
    integer :: int_dummy
    integer :: projection_varid

    stat = 0
    stat = nf90_open(ncfile, NF90_NOWRITE, ncid)
    if (stat /= 0) then
      write (error_unit, *) trim(nf90_strerror(stat))
      return
    endif

    call get_nx_ny(ncid, nx, ny, stat)
    if (stat /= 0) then
      write (error_unit, *) "Could not determine the size of the grids"
      if (stat /= ERROR_THIS_MODULE) then
        write (error_unit, *) trim(nf90_strerror(stat))
      endif
      int_dummy = nf90_close(ncid)
      return
    endif

    call detect_type(ncid, projection_varid, igtype, stat)
    if (stat /= 0) then
      if (stat == NF90_ENOTATT) then
        write (error_unit, *) "Could not detect grid mapping type from attribute'grid_mapping', assuming lat/lon grid"
        igtype = GEOGRAPHIC
      else
        if (stat /= ERROR_THIS_MODULE) then
          write (error_unit, *) trim(nf90_strerror(stat))
        endif
        int_dummy = nf90_close(ncid)
        return
      endif
    endif

    select case (igtype)

    case (GEOGRAPHIC)
      call geographic_grid(ncid, projection_varid, gparam, rotated=.false., stat=stat)

    case (SPHERICAL_ROTATED)
      call geographic_grid(ncid, projection_varid, gparam, rotated=.true., stat=stat)

    case (LAMBERT)
      call lambert_grid(ncid, projection_varid, gparam, stat)

    case default
      write (error_unit, *) "This projection type is lacking a grid mapping"
      stat = ERROR_THIS_MODULE
    end select

    if (stat /= 0) then
      if (stat /= ERROR_THIS_MODULE) then
        write (error_unit, *) trim(nf90_strerror(stat))
      endif
      int_dummy = nf90_close(ncid)
      return
    endif

    stat = nf90_close(ncid)
    if (stat /= 0) then
      write (error_unit, *) trim(nf90_strerror(stat))
      return
    endif

  end subroutine

  !> Getting the sizes of nx and ny from #met_params%xwindv,
  !> with them set by the two first dimension sizes
  subroutine get_nx_ny(ncid, nx, ny, stat)
    integer, intent(in) :: ncid
    integer, intent(out) :: nx
    integer, intent(out) :: ny
    integer, intent(out) :: stat

    integer :: varid
    integer :: dimids(NF90_MAX_DIMS)
    stat = 0

    stat = nf90_inq_varid(ncid, met_params%xwindv, varid)
    if (stat /= 0) then
      return
    endif

    stat = nf90_inquire_variable(ncid, varid, dimids=dimids)
    if (stat /= 0) return

    stat = nf90_inquire_dimension(ncid, dimids(1), len=nx)
    if (stat /= 0) return

    stat = nf90_inquire_dimension(ncid, dimids(2), len=ny)
    if (stat /= 0) return
  end subroutine

  !> Take varid of #met_params%xwindv, lookup grid_mapping, check this variables
  !> name and set igtype
  subroutine detect_type(ncid, projection_varid, igtype, stat)
    integer, intent(in) :: ncid
    integer, intent(out) :: projection_varid
    integer, intent(out) :: igtype
    integer, intent(out) :: stat

    integer :: varid
    integer :: len_str
    character(len=:), allocatable :: grid_mapping_name

    stat = 0
    stat = nf90_inq_varid(ncid, met_params%xwindv, varid)
    if (stat /= 0) then
      return
    endif

    stat = nf90_inquire_attribute(ncid, varid, "grid_mapping", len=len_str)
    if (stat /= 0) then
      ! CF defines geographic if no grid_mapping
      igtype = GEOGRAPHIC
      return
    endif
    allocate (character(len=len_str) :: grid_mapping_name)
    stat = nf90_get_att(ncid, varid, "grid_mapping", grid_mapping_name)
    if (stat /= 0) return

    stat = nf90_inq_varid(ncid, grid_mapping_name, projection_varid)
    deallocate (grid_mapping_name)
    if (stat /= 0) return

    stat = nf90_inquire_attribute(ncid, projection_varid, "grid_mapping_name", &
                                  len=len_str)
    if (stat /= 0) return
    allocate (character(len=len_str) :: grid_mapping_name)
    stat = nf90_get_att(ncid, projection_varid, "grid_mapping_name", grid_mapping_name)
    if (stat /= 0) return

    if (grid_mapping_name == "latitude_longitude") then
      igtype = GEOGRAPHIC
      return
    endif

    if (grid_mapping_name == "lambert_conformal_conic") then
      igtype = LAMBERT
      return
    endif

    if (grid_mapping_name == "rotated_latitude_longitude") then
      igtype = SPHERICAL_ROTATED
      return
    endif

    stat = ERROR_THIS_MODULE
  end subroutine

  !> determine gparam for latitude longitude grids
  subroutine geographic_grid(ncid, projection_varid, gparam, rotated, stat)
    !> Open nc file
    integer, intent(in) :: ncid
    !> varid of the projection variable
    integer, intent(in) :: projection_varid
    !> grid parameters
    real, intent(out) :: gparam(6)
    !> whether the grid is rotated or standard
    logical, intent(in) :: rotated
    !> error code (0 for success)
    integer, intent(out) :: stat

    integer :: standard_varid
    integer :: dimids(NF90_MAX_DIMS)
    character(len=NF90_MAX_NAME) :: dimname
    integer :: latlon_varid
    real :: latlon_itudes(2)
    integer :: i

    stat = 0
    if (.not. rotated) then
      gparam(5:6) = 0
    else
      stat = nf90_get_att(ncid, projection_varid, "grid_north_pole_longitude", gparam(5))
      if (stat /= 0) return
      gparam(5) = 180 + gparam(5) ! need equator projection
      stat = nf90_get_att(ncid, projection_varid, "grid_north_pole_latitude", gparam(6))
      if (stat /= 0) return
      gparam(6) = 0 + 90 - gparam(6) ! need equator projection
    endif

    stat = nf90_inq_varid(ncid, met_params%xwindv, standard_varid)
    if (stat /= 0) return

    stat = nf90_inquire_variable(ncid, standard_varid, dimids=dimids)
    if (stat /= 0) return

    ! Getting the longitude oriented gparams
    do i = 1, 2
      stat = nf90_inquire_dimension(ncid, dimids(i), name=dimname)
      if (stat /= 0) return
      stat = nf90_inq_varid(ncid, dimname, latlon_varid)
      if (stat /= 0) return

      stat = nf90_get_var(ncid, latlon_varid, start=[1], count=[2], values=latlon_itudes)
      if (stat /= 0) return
      gparam(i) = latlon_itudes(1)
      gparam(2 + i) = latlon_itudes(2) - latlon_itudes(1)
    enddo

  end subroutine

  !> determine gparam for lamber grids
  subroutine lambert_grid(ncid, projection_varid, gparam, stat)
    !> open nc file
    integer, intent(in) :: ncid
    !> varid of the projection variable
    integer, intent(in) :: projection_varid
    !> grid parameters
    real, intent(out) :: gparam(6)
    !> error code (0 for success)
    integer, intent(out) :: stat

    integer :: latlon_varid
    real :: latlons(1, 1)

    integer :: dimids(NF90_MAX_DIMS)
    character(len=NF90_MAX_NAME) :: dimname
    integer :: i
    integer :: xy_varid
    real :: xy_vals(2)
    character(len=NF90_MAX_NAME) :: units

    integer :: standard_varid

    stat = 0

    ! Get first values of longitude/latitude
    stat = nf90_inq_varid(ncid, "longitude", latlon_varid)
    if (stat /= 0) return
    stat = nf90_get_var(ncid, latlon_varid, &
                        start=[1, 1], count=[1, 1], values=latlons)
    if (stat /= 0) return
    gparam(1) = latlons(1, 1)

    stat = nf90_inq_varid(ncid, "latitude", latlon_varid)
    if (stat /= 0) return
    stat = nf90_get_var(ncid, latlon_varid, &
                        start=[1, 1], count=[1, 1], values=latlons)
    if (stat /= 0) return
    gparam(2) = latlons(1, 1)

    ! get increment in km, requires the two first dimension of
    ! a standard variable
    stat = nf90_inq_varid(ncid, met_params%xwindv, standard_varid)
    if (stat /= 0) return
    stat = nf90_inquire_variable(ncid, standard_varid, dimids=dimids)
    if (stat /= 0) return

    do i = 1, 2
      ! The variable got the same name as the dimension
      stat = nf90_inquire_dimension(ncid, dimids(i), name=dimname)
      if (stat /= 0) return
      stat = nf90_inq_varid(ncid, dimname, xy_varid)
      if (stat /= 0) return
      ! Get two consecutive values
      stat = nf90_get_var(ncid, xy_varid, start=[1], count=[2], &
                          values=xy_vals)
      if (stat /= 0) return
      gparam(2 + i) = xy_vals(2) - xy_vals(1)
      ! Checking units, converting to km
      stat = nf90_get_att(ncid, xy_varid, "units", units)
      if (stat /= 0) return
      if (units == "m") then
        gparam(2 + i) = gparam(2 + i)/1000.0
      else
        write (error_unit, *) "Do not know how to convert ", units, " to km"
        write (error_unit, *) "please change the conversion here"
        stat = 1
        return
      endif
    enddo

    ! Get reference longitude and latitude
    stat = nf90_get_att(ncid, projection_varid, "longitude_of_central_meridian", gparam(5))
    if (stat /= 0) return
    stat = nf90_get_att(ncid, projection_varid, "latitude_of_projection_origin", gparam(6))
    if (stat /= 0) return

  end subroutine

  !> Forms the k-levels to search, starting from max
  !> of the hybrid dimension, to 1, skipping 0
  subroutine get_klevel(ncfile, klevel, stat)
    !> path to nc file
    character(len=*), intent(in) :: ncfile
    !> resulting klevels, ranging from 0 to len(hybrid)
    !> with the levels like
    !>
    !> 0, nk, nk-1, ..., 1
    integer, allocatable, intent(out) :: klevel(:)
    !> error code (0 for success)
    integer, intent(out) :: stat

    integer :: dummy_int
    integer :: ncid
    integer :: hybrid_dimid, hybrid_len
    integer :: i
    integer :: nk
    stat = 0

    stat = nf90_open(ncfile, NF90_NOWRITE, ncid)
    if (stat /= 0) return
    stat = nf90_inq_dimid(ncid, "hybrid", hybrid_dimid)
    if (stat == NF90_EBADDIM) then
      stat = nf90_inq_dimid(ncid, "lev", hybrid_dimid)
    endif
    if (stat /= 0) then
      dummy_int = nf90_close(ncid)
      return
    endif
    stat = nf90_inquire_dimension(ncid, hybrid_dimid, len=hybrid_len)
    if (stat /= 0) then
      dummy_int = nf90_close(ncid)
      return
    endif
    nk = hybrid_len + 1
    allocate(klevel(nk))
    klevel(1) = 0
    do i = 2, nk
      klevel(i) = nk - i + 1
    enddo
  end subroutine
end module

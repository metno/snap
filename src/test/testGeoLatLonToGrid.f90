program testGeoLatLonToGrid
  use milibML, only: xyconvert, GEO_PARAMS
  implicit none

  real, parameter :: tol = 1.0e-3
  integer, parameter :: nx = 1251, ny = 601
  real :: gparam(6)
  real :: x(3), y(3), expected_x(3), expected_y(3)
  real :: lon_in(3), lat_in(3)
  real, parameter :: tol_grid_lcc = 5.0e-2
  real, parameter :: tol_geo_lcc = 1.0e-3
  real, parameter :: tol_xy_lcc_m = 1.0
  integer, parameter :: nx_lcc = 949, ny_lcc = 1069
  real, parameter :: x0_lcc_m = -1060084.0, y0_lcc_m = -1332518.0
  real, parameter :: dx_lcc_m = 2500.0, dy_lcc_m = 2500.0
  real :: lambert_gparam(6)
  real :: lon_lcc(4), lat_lcc(4), expected_x_lcc(4), expected_y_lcc(4)
  real :: x_lcc(4), y_lcc(4), grid_x_lcc(4), grid_y_lcc(4)
  real :: lon_back_lcc(4), lat_back_lcc(4)
  integer :: ierror, i

  ! Unrotated lat/lon output grid:
  ! lon = 1251, lat = 601, start lon/lat = -50/25, increment = 0.1 deg
  ! x = (lon - gparam(1)) / gparam(3) + 1
  ! y = (lat - gparam(2)) / gparam(4) + 1
  gparam = [-50.0, 25.0, 0.1, 0.1, 0.0, 0.0]

  x = [-50.0, 0.0, 75.0]
  y = [25.0, 50.0, 85.0]
  lon_in = x
  lat_in = y

  expected_x = [1.0, 501.0, real(nx)]
  expected_y = [1.0, 251.0, real(ny)]

  call xyconvert(3, x, y, 2, GEO_PARAMS, 2, gparam, ierror)
  if (ierror /= 0) then
    print *, "FAIL: xyconvert returned ierror=", ierror
    stop 1
  end if

  do i = 1, 3
    if (abs(x(i) - expected_x(i)) > tol) then
      print *, "FAIL: grid_x mismatch at point", i, "got", x(i), "expected", expected_x(i)
      stop 1
    end if
    if (abs(y(i) - expected_y(i)) > tol) then
      print *, "FAIL: grid_y mismatch at point", i, "got", y(i), "expected", expected_y(i)
      stop 1
    end if
  end do

  call xyconvert(3, x, y, 2, gparam, 2, GEO_PARAMS, ierror)
  if (ierror /= 0) then
    print *, "FAIL: reverse xyconvert returned ierror=", ierror
    stop 1
  end if

  do i = 1, 3
    if (abs(x(i) - lon_in(i)) > tol) then
      print *, "FAIL: longitude round-trip mismatch at point", i, "got", x(i), "expected", lon_in(i)
      stop 1
    end if
    if (abs(y(i) - lat_in(i)) > tol) then
      print *, "FAIL: latitude round-trip mismatch at point", i, "got", y(i), "expected", lat_in(i)
      stop 1
    end if
  end do

  print *, "PASS: geo_latitude/geo_longitude <-> grid_x/grid_y conversion"

  ! Lambert conformal grid metadata:
  ! grid_mapping_name = lambert_conformal_conic
  ! standard_parallel = 63.3, 63.3
  ! longitude_of_central_meridian = 15
  ! latitude_of_projection_origin = 63.3
  ! x/y increment = 2500 m = 2.5 km
  !
  ! lambert_gparam(1:2) are lon/lat at grid point (1,1), obtained from
  ! independent PROJ conversion of x=-1060084, y=-1332518.
  !lambert_gparam = [0.27828106, 50.319614, 2.5, 2.5, 15.0, 63.3]
  ! params from snap.log for meps
  lambert_gparam = [0.278280646, 50.3196182, 2.5, 2.5, 15.0, 63.2999992]
  ! Reference lon/lat values from latitude/longitude of MEPS-grid:
  ! (x,y)=(1,1), (2,1), (1,2), (949,1069)
  lon_lcc = [0.278280657208962, 0.311796259577913, 0.270446145155287, 54.2412616280741]
  lat_lcc = [50.3196182324731, 50.3246105707101, 50.3410140792226, 71.5760088846991]

  x_lcc = [-1060084, -1057584, -1060084, 1309916]
  y_lcc = [-1332518, -1332518, -1330018, 1337482]

  ! these are the values of xyconvert! SNAP uses internally shifted grid coordinates with lower left = (1,1)
  ! and not center = 1,1
  expected_x_lcc = [1.0, 2.0, 1.0, real(nx_lcc)]
  expected_y_lcc = [1.0, 1.0, 2.0, real(ny_lcc)]

  grid_x_lcc = lon_lcc
  grid_y_lcc = lat_lcc
  call xyconvert(4, grid_x_lcc, grid_y_lcc, 2, GEO_PARAMS, 6, lambert_gparam, ierror)
  if (ierror /= 0) then
    print *, "FAIL: geo->lambert xyconvert returned ierror=", ierror
    stop 1
  end if

  do i = 1, 4
    if (abs(grid_x_lcc(i) - expected_x_lcc(i)) > tol_grid_lcc) then
      print *, "FAIL: lambert grid_x mismatch at point", i, "got", grid_x_lcc(i), "expected", expected_x_lcc(i)
      stop 1
    end if
    if (abs(grid_y_lcc(i) - expected_y_lcc(i)) > tol_grid_lcc) then
      print *, "FAIL: lambert grid_y mismatch at point", i, "got", grid_y_lcc(i), "expected", expected_y_lcc(i)
      stop 1
    end if
  end do

  ! Convert computed grid indices to Lambert x/y axis coordinates in meters.
  do i = 1, 4
    if (abs((x0_lcc_m + (grid_x_lcc(i) - 1.0)*dx_lcc_m) - x_lcc(i)) > tol_xy_lcc_m) then
      print *, "FAIL: x_lcc meter mismatch at point", i, "got", x0_lcc_m + (grid_x_lcc(i) - 1.0)*dx_lcc_m, "expected", x_lcc(i)
      stop 1
    end if
    if (abs((y0_lcc_m + (grid_y_lcc(i) - 1.0)*dy_lcc_m) - y_lcc(i)) > tol_xy_lcc_m) then
      print *, "FAIL: y_lcc meter mismatch at point", i, "got", y0_lcc_m + (grid_y_lcc(i) - 1.0)*dy_lcc_m, "expected", y_lcc(i)
      stop 1
    end if
  end do

  lon_back_lcc = grid_x_lcc
  lat_back_lcc = grid_y_lcc
  call xyconvert(4, lon_back_lcc, lat_back_lcc, 6, lambert_gparam, 2, GEO_PARAMS, ierror)
  if (ierror /= 0) then
    print *, "FAIL: lambert->geo xyconvert returned ierror=", ierror
    stop 1
  end if

  do i = 1, 4
    if (abs(lon_back_lcc(i) - lon_lcc(i)) > tol_geo_lcc) then
      print *, "FAIL: longitude reverse mismatch at point", i, "got", lon_back_lcc(i), "expected", lon_lcc(i)
      stop 1
    end if
    if (abs(lat_back_lcc(i) - lat_lcc(i)) > tol_geo_lcc) then
      print *, "FAIL: latitude reverse mismatch at point", i, "got", lat_back_lcc(i), "expected", lat_lcc(i)
      stop 1
    end if
  end do

  call xyconvert(4, lon_back_lcc, lat_back_lcc, 2, GEO_PARAMS, 6, lambert_gparam, ierror)
  if (ierror /= 0) then
    print *, "FAIL: final geo->lambert xyconvert returned ierror=", ierror
    stop 1
  end if

  do i = 1, 4
    if (abs(lon_back_lcc(i) - grid_x_lcc(i)) > tol_grid_lcc .or. abs(lat_back_lcc(i) - grid_y_lcc(i)) > tol_grid_lcc) then
      print *, "FAIL: lambert round-trip grid mismatch at point", i
      stop 1
    end if
  end do

  print *, "PASS: lambert conformal geo <-> grid conversion"





end program testGeoLatLonToGrid

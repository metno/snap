program find_parameters_fi_test
  use iso_fortran_env, only: error_unit
  use find_parameters_fi, only: detect_gridparams_fi
  implicit none

  integer :: n, len
  integer :: stat
  character(len=:), allocatable :: ncfile
  character(len=1024) :: config, type, varname

  integer :: nx, ny
  integer :: igtype
  real :: gparam(6)

  integer, allocatable :: klevel(:)

  config = ""
  type = "netcdf"

  n = command_argument_count()
  if (n < 2) then
    write(*,*) "Usage: find_parameters_fi_test file.nc x_wind_ml [config] [type]"
    write(error_unit,*) "Expected 2-4 command argument:"
    write(error_unit,*) "got ", n
    error stop 1
  endif

  call get_command_argument(1, length=len, status=stat)
  if (stat /= 0) error stop "Could not get command argument"
  allocate(character(len=len) :: ncfile)
  call get_command_argument(1, value=ncfile, status=stat)
  if (stat /= 0) error stop "Could not get file argument"
  call get_command_argument(2, value=varname, status=stat)
  if (stat /= 0) error stop "Could not get varname argument"
  if (n > 2) then
    call get_command_argument(3, value=config, status=stat)
    if (stat /= 0) error stop "Could not get config argument"
  end if
  if (n > 3) then
    call get_command_argument(4, value=type, status=stat)
    if (stat /= 0) error stop "Could not get type argument"
  end if


  call detect_gridparams_fi(ncfile, config, type, varname, nx, ny, igtype, gparam, klevel, stat)
  if (stat /= 0) error stop "Could not detect gridparams"

  write(*,*) "GRID.SIZE= ", nx, ny
  write(*,*) "IGTYPE= ", igtype
  write(*,*) "GRID PARAMS :", gparam
  write(*,*) "KLEVEL :", klevel

  ! call get_klevel_fi(ncfile, klevel, stat)
  ! if (stat /= 0) error stop "Could not determine klevel"

  ! write(*,*) "KLEVEL: ", size(klevel)
  ! write(*,'(I3)') klevel

end program find_parameters_fi_test

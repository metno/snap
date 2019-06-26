program autodetect
  use iso_fortran_env, only: error_unit
  use find_parameter, only: detect_gridparams, get_klevel
  implicit none

  integer :: n
  integer :: stat
  character(len=:), allocatable :: ncfile

  integer :: nx, ny
  integer :: igtype
  real :: gparam(6)

  integer, allocatable :: klevel(:)


  n = command_argument_count()
  if (n /= 1) then
    write(error_unit,*) "Expected one command argument"
    write(error_unit,*) "got ", n
    error stop 1
  endif

  call get_command_argument(1, length=n, status=stat)
  if (stat /= 0) error stop "Could not get command argument"
  allocate(character(n) :: ncfile)
  call get_command_argument(1, value=ncfile, status=stat)
  if (stat /= 0) error stop "Could not get command argument"

  call detect_gridparams(ncfile, nx, ny, igtype, gparam, stat)
  if (stat /= 0) error stop 1

  write(*,*) "GRID.SIZE= ", nx, ny
  write(*,*) "IGTYPE= ", igtype
  write(*,*) "GRID PARAMS :", gparam

  call get_klevel(ncfile, klevel, stat)
  if (stat /= 0) error stop 1

  write(*,*) "KLEVEL: ", size(klevel)
  write(*,'(I3)') klevel

end program autodetect

program test
use array_utils, only : is_in_array
  use iso_fortran_env, only: int16
  integer(int16) :: i16 = 0
  integer, allocatable, target, dimension(:) :: ary
  integer(int16), allocatable, target, dimension(:) :: ary16
  integer, pointer, dimension(:) :: ptr
  integer(int16), pointer, dimension(:) :: ptr16

  allocate(ary(5))
  ary = 1
  if (is_in_array(ary, 0)) error stop "0 found in 1-array"
  if (.not. is_in_array(ary, 1)) error stop "1 not found in 1-array"

  ptr => ary
  if (is_in_array(ptr, 0)) error stop "0 found in 1-ptr-array"
  if (.not. is_in_array(ptr, 1)) error stop "1 not found in 1-ptr-array"

  ! int16
  allocate(ary16(5))
  ary16 = 1
  i16 = 0
  if (is_in_array(ary16, i16)) error stop "0 found in 1-array16"
  i16 = 1
  if (.not. is_in_array(ary16, i16)) error stop "1 not found in 1-array16"

  ptr16 => ary16
  i16 = 0
  if (is_in_array(ptr16, i16)) error stop "0 found in 1-ptr16-array"
  i16 = 1
  if (.not. is_in_array(ptr16, i16)) error stop "1 not found in 1-ptr16-array"


end program

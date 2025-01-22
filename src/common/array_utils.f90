module array_utils
  implicit none
  public :: is_in_array

  interface is_in_array
    module procedure is_in_array_i16
    module procedure is_in_array_int
    module procedure is_in_array_char
  end interface

  private

  contains
  pure function is_in_array_int(array, target) result(found)
    implicit none
    integer, dimension(:), intent(in) :: array
    integer, intent(in) :: target
    ! output
    logical :: found
    integer :: i

    found = .false.
    do i = 1, size(array)
      if (array(i) == target) then
        found = .true.
        exit
      end if
    end do
  end function is_in_array_int

  pure function is_in_array_i16(array, target) result(found)
    USE iso_fortran_env, only: int16
    implicit none
    integer(int16), dimension(:), intent(in) :: array
    integer(int16), intent(in) :: target
    ! output
    logical :: found
    integer :: i

    found = .false.
    do i = 1, size(array)
      if (array(i) == target) then
        found = .true.
        exit
      end if
    end do
  end function is_in_array_i16


  pure function is_in_array_char(array, target) result(found)
    implicit none
    character(len=*), dimension(:), intent(in) :: array
    character(len=*), intent(in) :: target
    ! output
    logical :: found
    integer :: i

    found = .false.
    do i = 1, size(array)
      if (array(i) == target) then
        found = .true.
        exit
      end if
    end do
  end function is_in_array_char

end module array_utils

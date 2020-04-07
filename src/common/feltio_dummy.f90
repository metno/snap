! SNAP: Servere Nuclear Accident Programme
! Copyright (C) 1992-2017   Norwegian Meteorological Institute

! This file is part of SNAP. SNAP is free software: you can
! redistribute it and/or modify it under the terms of the
! GNU General Public License as published by the
! Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.

! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.

! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https://www.gnu.org/licenses/>.

module feltio_dummy
  USE iso_fortran_env, only: error_unit
  implicit none
  private

  public readfd, readfield, filesort, fldout

  contains

subroutine readfd()
  error stop "readfd not implemented"
end subroutine readfd

subroutine readfield(x0, x1, x2, x3, x4, x5, x6, x7)
  integer :: x0, x1, x2, x3(:), x4, x5, x6(:), x7
  if (.false.) then ! Silence the sompiler
    write (error_unit,*) x0, x1, x2, x3, x4, x5, x6, x7
  endif
  error stop "readfield not implemented"
end subroutine

subroutine filesort(x, y)
  integer :: x, y
  if (.false.) then
    write (error_unit, *) x, y ! Silence the compiler
  endif
  error stop "filesort not implemented"
end subroutine

subroutine fldout(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x11)
  integer :: x0, x1, x8, x9, x10, x11
  character(len=*) :: x2
  integer :: x3(:)
  real :: x4, x5, x6, x7
  if (.false.) then ! Silence the compiler
    write (error_unit,*) x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11
  endif
  error stop "fldout not implemented"
end subroutine
end module feltio_dummy

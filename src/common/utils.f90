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

module utils
  public itoa, ftoa, atof, to_uppercase, to_lowercase

  character(len=26), parameter :: alphabet_upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  character(len=26), parameter :: alphabet_lower = "abcdefghijklmnopqrstuvwxyz"
contains
  function itoa(i) result(res)
    character(:),allocatable :: res
    integer,intent(in) :: i
    character(range(i)+2) :: tmp
    write(tmp,'(i0)') i
    res = trim(adjustl(tmp))
  end function itoa

  function ftoa(f) result(res)
    character(:),allocatable :: res
    real,intent(in) :: f
    character(8) :: tmp
    write(tmp,'(f8.2)') f
    res = trim(adjustl(tmp))
  end function ftoa


  function atof(str) result(res)
    real :: res
    character(len=*), intent(in) :: str
    read(str,*) res
  end function atof

  function find_in_str(needle, haystack) result(res)
    character, intent(in) :: needle
    character(len=*), intent(in) :: haystack
    integer :: res, n

    res = -1
    do n=1,len(haystack)
      if (needle == haystack(n:n)) then
        res = n
      endif
    enddo
  end function

  subroutine to_uppercase(str)
    character(len=*), intent(inout) :: str
    integer :: i, n

    do n=1,len_trim(str)
      i = find_in_str(str(n:n), alphabet_lower)
      if (i > 0) then
        str(n:n) = alphabet_upper(i:i)
      endif
    enddo
  end subroutine

  subroutine to_lowercase(str)
    character(len=*), intent(inout) :: str
    integer :: i, n

    do n=1,len_trim(str)
      i = find_in_str(str(n:n), alphabet_upper)
      if (i > 0) then
        str(n:n) = alphabet_lower(i:i)
      endif
    enddo
  end subroutine
end module utils

! SNAP: Servere Nuclear Accident Programme
! Copyright (C) 1992-2017   Norwegian Meteorological Institute
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

!> Common for debugging
module snapdebug
    implicit none
    private
!> * 0 = debug off
!> * 1 = debug on, note that mrfturbo/mrfelt only
!>       prints to standard out (not the log file)
    integer, public, save :: idebug = 0
!> output unit for log file
    integer, public :: iulog

    type, public :: timer_t
      real :: innertime
      contains
      procedure :: reset => timer_reset
      procedure :: now => timer_now
      procedure :: log => timer_log
    end type

    interface timer_t
      procedure :: timer_new
    end interface

    contains

    subroutine timer_reset(this)
      use iso_fortran_env, only: error_unit
      class(timer_t), intent(out) :: this

      call cpu_time(this%innertime)
      if (this%innertime == -1.0) then
        write(error_unit,*) "No clock available, reported times are not reliable"
      endif
    end subroutine

    function timer_new()
      type(timer_t) :: timer_new
      call timer_reset(timer_new)
    end function

    !> Returns the time in seconds since initializing this clock
    function timer_now(this)
      class(timer_t), intent(in) :: this
      real :: timer_now
      call cpu_time(timer_now)

      timer_now = timer_now - this%innertime
    end function

    !> Gets current time since start and
    !> outputs elapsed time into the log
    subroutine timer_log(this, prefix)
      class(timer_t), intent(in) :: this
      character(len=*), intent(in), optional :: prefix
      real :: now
      integer :: hours, minutes, seconds, milliseconds

      now = this%now()

      hours = floor(now / 3600)
      now = now - hours * 3600

      minutes = floor(now / 60)
      now = now - minutes * 60

      seconds = floor(now)
      now = now - seconds

      milliseconds = floor(now*1000)
      if (present(prefix)) then
        write(iulog,"(a,I3,a,I2,a,I2,a,I4)") prefix, hours, ":", minutes, ":", seconds, ".", milliseconds
      else
        write(iulog,"(a,I3,a,I2,a,I2,a,I3)") "ctime: ", hours, ":", minutes, ":", seconds, ".", milliseconds
      endif
    end subroutine
end module snapdebug

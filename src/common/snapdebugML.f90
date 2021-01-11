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

    character(len=*), parameter :: global_timer_prefix = "timer: "
    character(len=*), parameter :: global_timer_total_prefix = "Total: "

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

    type, public :: prefixed_accumulating_timer
      type(timer_t) :: timer
      logical :: running = .false.
      real :: total = 0.0
      character(len=1024) :: prefix
      contains
        procedure :: start => prefixed_timer_start
        procedure :: stop => prefixed_timer_stop
        procedure :: stop_and_log => prefixed_timer_stop_and_log
        procedure :: print_accumulated => prefixed_timer_print_accumulated
    end type

    interface prefixed_accumulating_timer
      procedure :: prefixed_accumulating_timer_new
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
    subroutine timer_log(this, prefix, unit)
      class(timer_t), intent(in) :: this
      character(len=*), intent(in), optional :: prefix
      integer, intent(in), optional :: unit

      real :: now

      now = this%now()
      call print_time(now, prefix, unit)
    end subroutine

    subroutine print_time(t, prefix, unit)
      real, intent(in) :: t
      character(len=*), intent(in), optional :: prefix
      integer, intent(in), optional :: unit

      integer :: current_unit
      integer :: hours, minutes, seconds, milliseconds

      if (present(unit)) then
        current_unit = unit
      else
        current_unit = iulog
      endif

      call real_to_hms_ms(t, hours, minutes, seconds, milliseconds)

      if (present(prefix)) then
        write(current_unit,"(a,I3,a,I2,a,I2,a,I4)") prefix, hours, ":", minutes, ":", seconds, ".", milliseconds
      else
        write(current_unit,"(a,I3,a,I2,a,I2,a,I3)") "ctime: ", hours, ":", minutes, ":", seconds, ".", milliseconds
      endif
    end subroutine

    pure subroutine real_to_hms_ms(t, hours, minutes, seconds, milliseconds)
      real, value :: t
      integer, intent(out) :: hours, minutes, seconds, milliseconds

      hours = floor(t / 3600)
      t = t - hours * 3600

      minutes = floor(t / 60)
      t = t - minutes * 60

      seconds = floor(t)
      t = t - seconds

      milliseconds = floor(t*1000)
    end subroutine

    function prefixed_accumulating_timer_new(prefix) result(timer)
      type(prefixed_accumulating_timer) :: timer
      character(len=*), intent(in) :: prefix

      timer%prefix(:) = trim(prefix)
    end function

    subroutine prefixed_timer_start(this)
      class(prefixed_accumulating_timer), intent(inout) :: this
      this%timer = timer_t()
    end subroutine

    subroutine prefixed_timer_stop(this)
      class(prefixed_accumulating_timer), intent(inout) :: this
      real :: duration

      duration = this%timer%now()
      this%total = this%total + duration
    end subroutine

    subroutine prefixed_timer_stop_and_log(this, unit)
      class(prefixed_accumulating_timer), intent(inout) :: this
      integer, intent(in), optional :: unit

      character(len=1024) :: prefix
      real :: duration

      duration = this%timer%now()
      this%total = this%total + duration

      write(prefix,*) GLOBAL_TIMER_PREFIX, trim(this%prefix), " "
      call print_time(duration, trim(prefix), unit)
    end subroutine

    subroutine prefixed_timer_print_accumulated(this, unit)
      class(prefixed_accumulating_timer), intent(in) :: this
      integer, intent(in), optional :: unit

      character(len=1024) :: prefix

      write(prefix,*) GLOBAL_TIMER_PREFIX, GLOBAL_TIMER_TOTAL_PREFIX, trim(this%prefix), " "
      call print_time(this%total, trim(prefix), unit)
    end subroutine
end module snapdebug

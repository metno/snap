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
    use iso_fortran_env, only: error_unit
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

    type, public :: timer_cpu_t
      real :: innertime
      contains
        procedure :: now => timer_cpu_now
        procedure :: log => timer_cpu_log
    end type

    interface timer_cpu_t
      procedure :: timer_cpu_new
    end interface

    type, public :: timer_sys_t
      integer :: count
      integer :: countrate
      contains
        procedure :: now => timer_sys_now
        procedure :: log => timer_sys_log
    end type

    interface timer_sys_t
      procedure :: timer_sys_new
    end interface

    type, public :: prefixed_accumulating_timer
      type(timer_cpu_t) :: timer_cpu
      type(timer_sys_t) :: timer_sys
      real :: total_cpu = 0.0
      real :: total_sys = 0.0
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

    function timer_cpu_new()
      type(timer_cpu_t) :: timer_cpu_new

      call cpu_time(timer_cpu_new%innertime)
      if (timer_cpu_new%innertime == -1.0) then
        write(error_unit,*) "No cpu time available, reported times are not reliable"
      endif
    end function

    !> Returns the time in seconds since initializing this clock
    function timer_cpu_now(this)
      class(timer_cpu_t), intent(in) :: this
      real :: timer_cpu_now
      call cpu_time(timer_cpu_now)

      timer_cpu_now = timer_cpu_now - this%innertime
    end function

    !> Gets current time since start and
    !> outputs elapsed time into the log
    subroutine timer_cpu_log(this, prefix, unit)
      class(timer_cpu_t), intent(in) :: this
      character(len=*), intent(in), optional :: prefix
      integer, intent(in), optional :: unit
      real :: now

      now = this%now()
      call print_time(now, prefix, unit)
    end subroutine

    function timer_sys_new()
      type(timer_sys_t) :: timer_sys_new

      call system_clock(count=timer_sys_new%count, count_rate=timer_sys_new%countrate)
    end function

    function timer_sys_now(this)
      class(timer_sys_t), intent(in) :: this
      real :: timer_sys_now
      integer :: now_count

      call system_clock(count=now_count)
      now_count = now_count - this%count
      timer_sys_now = real(now_count) / real(this%countrate)
    end function

    subroutine timer_sys_log(this, prefix, unit)
      class(timer_sys_t), intent(in) :: this
      character(len=*), intent(in), optional :: prefix
      integer, intent(in), optional :: unit
      real :: now

      now = this%now()
      call print_time(now, prefix, unit)
    end subroutine

    function to_str(t) result(str)
      real, intent(in) :: t
      character(len=64) :: str

      integer :: h, m, s, ms
      call real_to_hms_ms(t, h, m, s, ms)

      write(str, *) "(I3, a, I2, a, I2, a, I4)", h, ":", m, ":", s, ".", ms

    end function

    subroutine print_time(t, prefix, unit)
      real, intent(in) :: t
      character(len=*), intent(in), optional :: prefix
      integer, intent(in), optional :: unit

      integer :: current_unit

      if (present(unit)) then
        current_unit = unit
      else
        current_unit = iulog
      endif

      if (present(prefix)) then
        write(current_unit,*) prefix, trim(to_str(t))
      else
        write(current_unit,*) "ctime: ", trim(to_str(t))
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
      this%timer_cpu = timer_cpu_t()
      this%timer_sys = timer_sys_t()
    end subroutine

    subroutine prefixed_timer_stop(this)
      class(prefixed_accumulating_timer), intent(inout) :: this
      real :: duration

      duration = this%timer_cpu%now()
      this%total_cpu = this%total_cpu + duration

      duration = this%timer_sys%now()
      this%total_sys = this%total_sys + duration
    end subroutine

    subroutine prefixed_timer_stop_and_log(this, unit)
      class(prefixed_accumulating_timer), intent(inout) :: this
      integer, intent(in), optional :: unit

      real :: duration_sys, duration_cpu
      integer :: current_unit

      duration_sys = this%timer_sys%now()
      this%total_sys = this%total_sys + duration_sys

      duration_cpu = this%timer_cpu%now()
      this%total_cpu = this%total_cpu + duration_cpu

      if (present(unit)) then
        current_unit = unit
      else
        current_unit = iulog
      endif

      write(current_unit,*) GLOBAL_TIMER_PREFIX, trim(this%prefix), " sys: ", &
        trim(to_str(duration_sys)), " cpu: ", trim(to_str(duration_cpu))
    end subroutine

    subroutine prefixed_timer_print_accumulated(this, unit)
      class(prefixed_accumulating_timer), intent(in) :: this
      integer, intent(in), optional :: unit

      integer :: current_unit

      if (present(unit)) then
        current_unit = unit
      else
        current_unit = iulog
      endif

      write(current_unit,*) GLOBAL_TIMER_PREFIX, GLOBAL_TIMER_TOTAL_PREFIX, trim(this%prefix), &
        " sys: ", trim(to_str(this%total_sys)), " cpu: ", trim(to_str(this%total_cpu))
    end subroutine
end module snapdebug

! SNAP: Servere Nuclear Accident Programme
! Copyright (C) 1992-2026   Norwegian Meteorological Institute

! License: GNU General Public License Version 3 (GNU GPL-3.0)

!> Module to compute timelike items
module datetime
  use iso_fortran_env, only: int64, error_unit
  implicit none
  private

  public :: timeGM, epochToDate, parseIsoDate, timeUnitScale, timeUnitOffset

  !> Represents a datetime with granularity of
  !> integer seconds
  !> Datetimes must be in the Zulu timezone and assumes
  !> the datetimes must be in the Gregorian calendar
  !> No validation of dates are performed
  type, public :: datetime_t
    !> Year
    integer :: year
    !> Month, Jan: 1, Feb: 2, ...
    integer :: month
    !> Day in month, 1, 2, 3, ...
    integer :: day
    !> Hour in day, 0, 1, 2, ..., 23
    integer :: hour
    integer :: minute = 0
    integer :: second = 0

    contains
      procedure :: add_duration
      generic, public :: operator(+) => add_duration

      procedure :: sub_duration
      procedure :: datetime_diff
      generic, public :: operator(-) => sub_duration, datetime_diff

      procedure :: equality
      generic, public :: operator(==) => equality

      procedure :: greater
      generic, public :: operator(>) => greater

      procedure :: greater_equal
      generic, public :: operator(>=) => greater_equal

      procedure :: lesser
      generic, public :: operator(<) => lesser

      procedure :: lesser_equal
      generic, public :: operator(<=) => lesser_equal
  end type

  !> Duration between two datetimes
  type, public :: duration_t
    !> Signed difference in seconds
    integer(int64) :: seconds

    contains
      procedure :: duration_equality
      generic, public :: operator(==) => duration_equality
  end type

  interface duration_t
    module procedure duration_from_units, duration_from_seconds64
  end interface

  integer, parameter :: JANUARY = 1, FEBRUARY = 2, DECEMBER = 12

  contains

  !> Scale of a time unit in seconds.
  function timeUnitScale(unit)
    character(len=*), intent(in) :: unit
    integer(int64) :: timeUnitScale
    integer :: ind
    character(len=25) :: tmp

    ind = index(unit, " since ")
    if (ind == 0) then
      write(error_unit,*) "' since ' not found in time-units: ", unit
      stop 1
    end if
    tmp = trim(unit(:ind-1))
    timeUnitScale = 0
    if (tmp == "seconds") timeUnitScale = 1
    if (tmp == "minutes") timeUnitScale = 60
    if (tmp == "hours") timeUnitScale = 60*60
    if (tmp == "days") timeUnitScale = 60*60*24
    if (timeUnitScale == 0) then
      write(error_unit,*) "' since ' not found in time-units: ", unit
      stop 1
    end if
  end function timeUnitScale

  !> Offset in seconds from 1970-01-01T00:00:00.
  function timeUnitOffset(unit)
    character(len=*), intent(in) :: unit
    integer(int64) :: timeUnitOffset
    integer :: ind
    character(len=25) :: date

    ind = index(unit, " since ")
    if (ind == 0) then
      write(error_unit,*) "' since ' not found in time-units: ", unit
      stop 1
    end if
    date = trim(unit(ind+7:))
    timeUnitOffset = timeGM(parseIsoDate(date))
  end function timeUnitOffset

  pure function epochToDate(epochSeconds) result(date)
    integer(int64), intent(in) :: epochSeconds
    type(datetime_t) :: date

    date = datetime_t(1970, 1, 1, 0) + duration_t(epochSeconds)
  end function epochToDate

  pure function timeGM(date)
    type(datetime_t), intent(in) :: date
    integer(int64) :: timeGM
    type(duration_t) :: elapsed

    elapsed = date - datetime_t(1970, 1, 1, 0)
    timeGM = elapsed%seconds
  end function timeGM

  !> Parse an ISO date at integer-second precision, without timezone conversion.
  function parseIsoDate(str) result(date)
    use iso_c_binding, only: C_NULL_CHAR
    character(len=*), intent(in) :: str
    type(datetime_t) :: date
    integer :: ind, ind2
    character(len=80) :: substr

    date = datetime_t(0, 0, 0, 0, 0, 0)
    ind = index(str, "-")
    if (ind > 0) read(str(:ind-1),*) date%year
    substr = str(ind+1:)

    ind = index(substr, "-")
    if (ind > 0) read(substr(:ind-1),*) date%month
    substr = substr(ind+1:)

    ind = index(substr, " ")
    ind2 = index(substr, "T")
    if (ind == 0) ind = len(substr)
    if (ind2 == 0) ind2 = len(substr)
    ind = min(ind, ind2)
    if (ind > 0) read(substr(:ind-1),*) date%day
    substr = substr(ind+1:)

    ind = index(substr, ":")
    if (ind > 0) read(substr(:ind-1),*) date%hour
    substr = substr(ind+1:)

    ind = index(substr, ":")
    if (ind > 0) read(substr(:ind-1),*) date%minute
    substr = substr(ind+1:)

    ! Seconds end at a suffix or fractional part, which are not interpreted.
    ind = len(substr)
    ind2 = index(substr, ".")
    if (ind2 /= 0) ind = min(ind, ind2)
    ind2 = index(substr, " ")
    if (ind2 /= 0) ind = min(ind, ind2)
    ind2 = index(substr, "Z")
    if (ind2 /= 0) ind = min(ind, ind2)
    ind2 = index(substr, "z")
    if (ind2 /= 0) ind = min(ind, ind2)
    ind2 = index(substr, "+")
    if (ind2 /= 0) ind = min(ind, ind2)
    ind2 = index(substr, C_NULL_CHAR)
    if (ind2 /= 0) ind = min(ind, ind2)
    if (ind > 0) read(substr(:ind-1),*) date%second
  end function parseIsoDate

  !> Positional arguments are seconds; named hours may be combined with seconds.
  pure type(duration_t) function duration_from_units(seconds, hours) result(duration)
    integer, optional, intent(in) :: seconds, hours

    duration%seconds = 0
    if (present(seconds)) duration%seconds = int(seconds, int64)
    if (present(hours)) duration%seconds = duration%seconds + hours*3600_int64
  end function

  pure type(duration_t) function duration_from_seconds64(seconds, hours) result(duration)
    integer(int64), intent(in) :: seconds
    integer, optional, intent(in) :: hours

    duration%seconds = seconds
    if (present(hours)) duration%seconds = duration%seconds + hours*3600_int64
  end function

  !> Add a duration to the current time
  pure type(datetime_t) function add_duration(this, duration) result(newtime)
    class(datetime_t), intent(in) :: this
    type(duration_t), intent(in) :: duration

    integer(int64) :: seconds, days
    integer :: current_month, current_year

    ! Split first to avoid overflowing when adding the time of day.
    days = duration%seconds / 86400_int64
    seconds = mod(duration%seconds, 86400_int64) + &
              this%hour*3600_int64 + this%minute*60_int64 + this%second
    days = this%day + days + seconds/86400_int64
    seconds = mod(seconds, 86400_int64)
    if (seconds < 0) then
      seconds = seconds + 86400_int64
      days = days - 1
    endif
    newtime%hour = int(seconds/3600_int64)
    newtime%minute = int(mod(seconds, 3600_int64)/60_int64)
    newtime%second = int(mod(seconds, 60_int64))

    current_month = this%month
    current_year = this%year
    do while (days > monthdays(current_month, current_year))
      days = days - monthdays(current_month, current_year)
      current_month = current_month + 1
      if (current_month == DECEMBER + 1) then
        current_year = current_year + 1
        current_month = JANUARY
      endif
    enddo
    do while (days < 1)
      current_month = current_month - 1
      if (current_month == JANUARY - 1) then
        current_month = DECEMBER
        current_year = current_year - 1
      endif
      days = days + monthdays(current_month, current_year)
    enddo
    newtime%day = int(days)
    newtime%month = current_month
    newtime%year = current_year
  end function

  !> Subtract a duration from a datetime
  pure type(datetime_t) function sub_duration(this, duration) result(newtime)
    class(datetime_t), intent(in) :: this
    type(duration_t), intent(in) :: duration

    newtime = this + duration_t(-duration%seconds)
  end function

  !> Compare two datetimes for equality
  pure logical function equality(this, other)
    class(datetime_t), intent(in) :: this
    type(datetime_t), intent(in) :: other

    equality = (this%year == other%year) .and. &
               (this%month == other%month) .and. &
               (this%day == other%day) .and. &
               (this%hour == other%hour) .and. &
               (this%minute == other%minute) .and. &
               (this%second == other%second)
  end function

  !> Compares this > other
  pure logical function greater(this, other)
    class(datetime_t), intent(in) :: this
    type(datetime_t), intent(in) :: other

    if (this == other) then
      greater = .false.
      return
    endif
    greater = .false.

    if (this%year > other%year) then
      greater = .true.
    else if (this%year == other%year) then
      if (this%month > other%month) then
        greater = .true.
      else if (this%month == other%month) then
        if (this%day > other%day) then
          greater = .true.
        else if (this%day == other%day) then
          if (this%hour > other%hour) then
            greater = .true.
          else if (this%hour == other%hour) then
            greater = this%minute > other%minute .or. &
                      (this%minute == other%minute .and. this%second > other%second)
          endif
        endif
      endif
    endif
  end function

  !> Compares this >= other
  pure logical function greater_equal(this, other)
    class(datetime_t), intent(in) :: this
    type(datetime_t), intent(in) :: other

    greater_equal = (this > other).or.(this == other)
  end function

  !> Compares this < other
  pure logical function lesser(this, other)
    class(datetime_t), intent(in) :: this
    type(datetime_t), intent(in) :: other

    lesser = (.not.this == other) .and. (.not.this > other)
  end function

  !> Compares this <= other
  pure logical function lesser_equal(this, other)
    class(datetime_t), intent(in) :: this
    type(datetime_t), intent(in) :: other

    lesser_equal = (this < other) .or. (this == other)
  end function

  !> Gives the number of days in a month, accounting for leap years
  pure function monthdays(month, year)
    integer, intent(in) :: month, year
    integer :: monthdays

    integer, parameter :: MONTHDAYS_NORMAL(12) = [31,-1000,31,30,31,30,31,31,30,31,30,31]

    if (month == FEBRUARY) then
      if (mod(year, 400) == 0) then
        monthdays = 29
      else if (mod(year, 100) == 0) then
        monthdays = 28
      else if (mod(year, 4) == 0) then
        monthdays = 29
      else
        monthdays = 28
      end if
    else
      monthdays = MONTHDAYS_NORMAL(month)
    endif
  end function

  !> The difference between two timesteps
  pure function datetime_diff(this, other) result(diff)
    class(datetime_t), intent(in) :: this
    type(datetime_t), intent(in) :: other
    type(duration_t) :: diff

    logical :: swap
    type(datetime_t) :: f, s

    swap = this < other
    if (swap) then
      s = this
      f = other
    else
      f = this
      s = other
    endif

    diff%seconds = 0
    do while (f%year > s%year)
      diff%seconds = diff%seconds + seconds_to_end_of_year(s)
      s%year = s%year + 1
      s%month = JANUARY
      s%day = 1
      s%hour = 0
      s%minute = 0
      s%second = 0
    end do

    ! The remainder in f is accounted for
    diff%seconds = diff%seconds + (seconds_to_end_of_year(s) - seconds_to_end_of_year(f))
    if (swap) diff%seconds = -diff%seconds
  end function

  !> Counts the number of seconds until the end of the year
  pure function seconds_to_end_of_year(d) result(h)
    type(datetime_t), intent(in) :: d
    integer(int64) :: h
    integer :: current_month

    ! Add to end of month
    h = (monthdays(d%month, d%year) - d%day) * 86400_int64 + &
        (24 - d%hour)*3600_int64 - d%minute*60_int64 - d%second
    current_month = d%month + 1

    do while (current_month < DECEMBER + 1)
      h = h + monthdays(current_month, d%year)*86400_int64
      current_month = current_month + 1
    enddo
  end function

  !> Checks for equality between two durations
  pure function duration_equality(this, other)
    class(duration_t), intent(in) :: this
    type(duration_t), intent(in) :: other
    logical :: duration_equality

    duration_equality = this%seconds == other%seconds
  end function
end module

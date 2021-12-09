!> Module to compute timelike items
module datetime
  implicit none
  private

  !> Represents a datetime with granularity of
  !> integer hours (no minutes or seconds)
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

      procedure :: lesser
      generic, public :: operator(<) => lesser
  end type

  !> Duration between two datetimes
  type, public :: duration_t
    !> The difference is given only computed in hours for simplicity
    integer :: hours

    contains
      procedure :: duration_equality
      generic, public :: operator(==) => duration_equality
  end type

  integer, parameter :: JANUARY = 1, FEBRUARY = 2, DECEMBER = 12

  contains

  !> Add a duration to the current time
  pure function add_duration(this, duration) result(newtime)
    class(datetime_t), intent(in) :: this
    type(duration_t), intent(in) :: duration
    type(datetime_t) :: newtime


    integer :: hours, days
    integer :: current_month, current_year

    if (duration%hours > 0) then
      hours = this%hour + duration%hours
      newtime%hour = mod(hours, 24)

      days = this%day + hours / 24

      current_month = this%month
      current_year = this%year
      !> Naively subracting per month as this is easy...
      do while (days > monthdays(current_month, current_year))
        days = days - monthdays(current_month, current_year)
        current_month = current_month + 1
        if (current_month == DECEMBER + 1) then
          current_year = current_year + 1
          current_month = JANUARY
        endif
      enddo

      newtime%day = days
      newtime%month = current_month
      newtime%year = current_year
    else
      hours = this%hour + duration%hours
      if (hours >= 0) then
        newtime%hour = mod(hours, 24)
        days = this%day
      else
        ! Need the lest non-negative remainder
        ! (Euclidean division remainder)
        newtime%hour = mod(hours, 24)
        days = this%day + hours/24
        if (newtime%hour < 0) then
          newtime%hour = 24 + newtime%hour
          days = days - 1
        endif
      endif

      current_month = this%month
      current_year = this%year
      do while (days < 1)
        current_month = current_month - 1
        if (current_month == JANUARY - 1) then
          current_month = DECEMBER
          current_year = current_year - 1
        endif
        days = days + monthdays(current_month, current_year)
      enddo

      newtime%day = days
      newtime%month = current_month
      newtime%year = current_year
    endif
  end function

  !> Subtract a duration from a datetime
  pure function sub_duration(this, duration) result(newtime)
    class(datetime_t), intent(in) :: this
    type(duration_t), intent(in) :: duration
    type(datetime_t) :: newtime

    newtime = this + duration_t(-duration%hours)
  end function

  !> Compare two datetimes for equality
  pure function equality(this, other)
    class(datetime_t), intent(in) :: this
    type(datetime_t), intent(in) :: other
    logical :: equality

    equality = (this%year == other%year) .and. &
               (this%month == other%month) .and. &
               (this%day == other%day) .and. &
               (this%hour == other%hour)
  end function

  !> Compares this > other
  pure function greater(this, other)
    class(datetime_t), intent(in) :: this
    type(datetime_t), intent(in) :: other
    logical :: greater

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
          greater = this%hour > other%hour
        endif
      endif
    endif
  end function

  !> Compares this < other
  pure function lesser(this, other)
    class(datetime_t), intent(in) :: this
    type(datetime_t), intent(in) :: other
    logical :: lesser

    lesser = (.not.this == other) .and. (.not.this > other)
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

    diff%hours = 0
    do while (f%year > s%year)
      diff%hours = diff%hours + hours_to_end_of_year(s)
      s%year = s%year + 1
      s%month = JANUARY
      s%day = 1
      s%hour = 0
    end do

    ! The remainder in f is accounted for
    diff%hours = diff%hours + (hours_to_end_of_year(s) - hours_to_end_of_year(f))
    if (swap) diff%hours = -diff%hours
  end function

  !> Counts the number of hours until the end of the year
  pure function hours_to_end_of_year(d) result(h)
    type(datetime_t), intent(in) :: d
    integer :: h
    integer :: current_month

    ! Add to end of month
    h = (monthdays(d%month, d%year) - d%day) * 24 + (24 - d%hour)
    current_month = d%month + 1

    do while (current_month < DECEMBER + 1)
      h = h + monthdays(current_month, d%year)*24
      current_month = current_month + 1
    enddo
  end function

  !> Checks for equality between two durations
  pure function duration_equality(this, other)
    class(duration_t), intent(in) :: this
    type(duration_t), intent(in) :: other
    logical :: duration_equality

    duration_equality = this%hours == other% hours
  end function
end module

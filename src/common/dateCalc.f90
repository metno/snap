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
!
module DateCalc
    USE iso_fortran_env, only: int64, error_unit
    use datetime, only: datetime_t, duration_t
    implicit none
    private

    public timeGM
! this function is similar to gmtime, except that values are month: 1-12, mday 1-31 and year in YYYY
    public epochToDate
    public parseIsoDate
    public timeUnitScale
    public timeUnitOffset

contains
! scale of timeUnit in seconds
  function timeUnitScale(unit)
    character(len=*), intent(in) :: unit
    integer(int64) timeUnitScale
    integer :: ind=0
    character(len=25) :: tmp
    ind=index(unit," since ")
    if (ind .eq. 0) then
      write (error_unit,*) "' since ' not found in time-units: ", unit
      stop 1
    end if
    tmp = trim(unit(:ind-1))
    timeUnitScale = 0
    if (tmp .eq. "seconds") timeUnitScale = 1
    if (tmp .eq. "minutes") timeUnitScale = 60
    if (tmp .eq. "hours") timeUnitScale = 60*60
    if (tmp .eq. "days") timeUnitScale = 60*60*24
    if (timeUnitScale .eq. 0) then
      write (error_unit,*) "' since ' not found in time-units: ", unit
      stop 1
    end if
    return
  end function timeUnitScale

! offset in seconds to 1.1.1970
  function timeUnitOffset(unit)
    character(len=*), intent(in) :: unit
    integer(int64) timeUnitOffset
    integer :: ind=0
    character(len=25) :: date
    ind=index(unit," since ")
    if (ind .eq. 0) then
      write (error_unit,*) "' since ' not found in time-units: ", unit
      stop 1
    end if
    date = trim(unit(ind+7:))
    timeUnitOffset = timeGM(parseIsoDate(date))
  end function timeUnitOffset

  function epochToDate(epochSeconds) result(values)
    integer(int64), intent(in) :: epochSeconds
    integer :: values(6)
    type(datetime_t) :: date

    date = datetime_t(1970, 1, 1, 0) + duration_t(epochSeconds)
    values = [date%second, date%minute, date%hour, date%day, date%month, date%year]

  end function epochToDate

! calculate from values(6)=(/secs,min,hours,mday(1-31),month(1-12),year(since 0)
  function timeGM(values)
    integer, intent(in) :: values(6)
    integer(int64) :: timeGM
    type(duration_t) :: elapsed
    elapsed = datetime_t(values(6), values(5), values(4), values(3), values(2), values(1)) - &
              datetime_t(1970, 1, 1, 0)
    timeGM = elapsed%seconds
  end function timeGM

! convert an ISO8601 date string to a list of values(6) = secs,min,hours, days,month,year
  function parseIsoDate(str) result(values)
    USE iso_c_binding, ONLY: C_NULL_CHAR
    character(len=*), intent(in)       :: str
    integer                         :: values(6), ind, ind2
    character(len=80)                   :: substr

    values = 0
    ! year
    ind = index(str, "-")
    if (ind > 0) read(str(:ind-1),*) values(6)
    substr = str(ind+1:)

    !month
    ind = index(substr, "-")
    if (ind > 0) read(substr(:ind-1),*) values(5)
    substr = substr(ind+1:)

    !day
    ind = index(substr, " ")
    ind2 = index(substr, "T")
    if (ind == 0) ind = len(substr)
    if (ind2 == 0) ind2 = len(substr)
    ind = min(ind, ind2)
    ! write (error_unit,*) ind, ind2, substr
    if (ind > 0) read(substr(:ind-1),*) values(4)
    substr = substr(ind+1:)

    ! hour
    ind = index(substr, ":")
    if (ind > 0) read(substr(:ind-1),*) values(3)
    substr = substr(ind+1:)

    ! minute
    ind = index(substr, ":")
    if (ind > 0) read(substr(:ind-1),*) values(2)
    substr = substr(ind+1:)

    ! seconds, end at end, ., space, C_NULL_CHAR, Z, or +
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

    if (ind > 0) read(substr(:ind-1),*) values(1)
    substr = substr(ind+1:)

  end function parseIsoDate


end module DateCalc

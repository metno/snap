! gfortran -I. testDateCalc.f90 datetime.o -o testDateCalc
program testDateCalc
  use iso_fortran_env, only: int64
  use iso_c_binding, only: C_NULL_CHAR
  use datetime, only: datetime_t, timeGM, epochToDate, parseIsoDate, timeUnitScale, timeUnitOffset
  implicit none

  type(datetime_t) :: date, round_trip
  integer :: year, month

  call check_epoch(datetime_t(1970, 1, 1, 0), 0_int64)
  call check_epoch(datetime_t(1970, 6, 1, 0), 13046400_int64)
  call check_epoch(datetime_t(1970, 1, 6, 0), 5_int64*24*3600)
  call check_epoch(datetime_t(1980, 1, 1, 0), 315532800_int64)
  call check_epoch(datetime_t(1969, 1, 1, 0), -31536000_int64)
  call check_epoch(datetime_t(1960, 1, 1, 0), -315619200_int64)
  call check_epoch(datetime_t(1973, 6, 26, 9, 51, 21), 109936281_int64)
  call check_epoch(datetime_t(1969, 12, 31, 23, 59, 59), -1_int64)
  call check_epoch(datetime_t(2038, 1, 19, 3, 14, 7), 2147483647_int64)
  call check_epoch(datetime_t(2038, 1, 19, 3, 14, 8), 2147483648_int64)
  call check_epoch(datetime_t(1900, 1, 1, 0), -2208988800_int64)
  call check_epoch(datetime_t(2000, 2, 29, 0), 951782400_int64)
  call check_epoch(datetime_t(2000, 3, 1, 0, 0, 1), 951868801_int64)
  call check_epoch(datetime_t(2100, 3, 1, 0), 4107542400_int64)

  date = parseIsoDate("1973-06-26T09:51:21Z")
  call check_epoch(date, 109936281_int64)
  round_trip = epochToDate(timeGM(date))
  if (.not.round_trip == date) error stop "Parsed date round trip"
  if (round_trip%year /= 1973 .or. round_trip%month /= 6 .or. round_trip%day /= 26 .or. &
      round_trip%hour /= 9 .or. round_trip%minute /= 51 .or. round_trip%second /= 21) &
      error stop "Named date components"
  call check_epoch(parseIsoDate("1973-06-26 09:51:21"), 109936281_int64)
  call check_epoch(parseIsoDate("1973-6-26 9:51:1"), 109936261_int64)
  call check_epoch(parseIsoDate("1973-06-26T09:51:21z"), 109936281_int64)
  call check_epoch(parseIsoDate("1973-06-26T09:51:21.5 +00:00"), 109936281_int64)
  call check_epoch(parseIsoDate("1973-06-26T09:51:21+00:00"), 109936281_int64)
  call check_epoch(parseIsoDate("1973-06-26T09:51:21"//C_NULL_CHAR), 109936281_int64)
  date = parseIsoDate("1970-01-01T00:00:00Z")
  if (.not.date == datetime_t(1970, 1, 1, 0)) error stop "Default parsed time"

  if (timeUnitScale("seconds since 1970-01-01 00:00:00") /= 1_int64) &
      error stop "Seconds scale"
  if (timeUnitScale("minutes since 1970-01-01 00:00:00") /= 60_int64) &
      error stop "Minutes scale"
  if (timeUnitScale("hours since 1970-01-01 00:00:00") /= 3600_int64) &
      error stop "Hours scale"
  if (timeUnitScale("days since 1900-1-1 00:00:00.0 +00:00") /= 86400_int64) &
      error stop "Days scale"
  if (timeUnitOffset("seconds since 1970-01-01 00:00:00") /= 0_int64) &
      error stop "Epoch time-unit offset"
  if (timeUnitOffset("days since 1900-01-01T00:00:00+00:00") /= -2208988800_int64) &
      error stop "Pre-epoch time-unit offset"
  if (timeUnitOffset("seconds since 2000-03-01 00:00:01") /= 951868801_int64) &
      error stop "Leap year time-unit offset"

  do year = 1896, 2104, 4
    do month = 1, 12
      date = datetime_t(year, month, 28, 13, 41, 37)
      if (.not.epochToDate(timeGM(date)) == date) error stop "Datetime round trip"
    enddo
  enddo

contains

  subroutine check_epoch(date, epoch)
    type(datetime_t), intent(in) :: date
    integer(int64), intent(in) :: epoch
    if (timeGM(date) /= epoch) error stop "Incorrect epoch seconds"
    if (.not.epochToDate(epoch) == date) error stop "Incorrect epoch date"
  end subroutine check_epoch

end program testDateCalc

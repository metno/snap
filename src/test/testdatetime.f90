program test_time
  use iso_fortran_env, only: int64
  use datetime, only: datetime_t, duration_t
  implicit none

  type(datetime_t) :: start, fin, fin2
  type(duration_t) :: d
  integer :: year, month, day, hour, minute, second

  start = datetime_t(2021, 10, 1, 0)
  d = duration_t(hours=24)
  fin = start + d
  if (.not.fin == datetime_t(2021, 10, 2, 0)) error stop "Test failure"
  fin2 = fin - d
  if (.not.fin2 == start) error stop "Test failure"
  fin2 = start - d
  if (.not.fin2 == datetime_t(2021, 9, 30, 0)) error stop "Test failure"

  start = datetime_t(2021, 10, 1, 0)
  d = duration_t(hours=24000)
  fin = start + d
  if (.not.fin == datetime_t(2024, 6, 27, 0)) error stop "Test failure"
  fin2 = fin - d
  if (.not.fin2 == start) error stop "Test failure"
  fin2 = start - d
  if (.not.fin2 == datetime_t(2019, 1, 5, 0)) error stop "Test failure"

  start = datetime_t(2021, 10, 1, 0)
  d = duration_t(hours=240000)
  fin = start + d
  if (.not.fin == datetime_t(2049, 2, 16, 0)) error stop "Test failure"
  fin2 = fin - d
  if (.not.fin2 == start) error stop "Test failure"
  fin2 = start - d
  if (.not.fin2 == datetime_t(1994, 5, 16, 0)) error stop "Test failure"

  start = datetime_t(1983, 10, 9, 17)
  d = duration_t(hours=591237)
  fin = start + d
  if (.not.fin == datetime_t(2051, 3, 21, 14)) error stop "Test failure"
  fin2 = fin - d
  if (.not.fin2 == start) error stop "Test failure"
  fin2 = start - d
  if (.not.fin2 == datetime_t(1916, 4, 28, 20)) error stop "Test failure"

  fin = datetime_t(1994, 2, 28, 23) + duration_t(hours=1)
  if (.not.fin == datetime_t(1994, 3, 1, 0)) error stop "Test failure"
  fin = datetime_t(1994, 3, 1, 0) - duration_t(hours=1)
  if (.not.fin == datetime_t(1994, 2, 28, 23)) error stop "Test failure"

  fin = datetime_t(1996, 2, 28, 23) + duration_t(hours=1)
  if (.not.fin == datetime_t(1996, 2, 29, 0)) error stop "Test failure"
  fin = datetime_t(1996, 2, 29, 0) - duration_t(hours=1)
  if (.not.fin == datetime_t(1996, 2, 28, 23)) error stop "Test failure"

  if (.not.datetime_t(2022, 1, 1, 0) - datetime_t(2021, 9, 10, 23) == &
      duration_t(hours=2689)) error stop "Test failure"
  if (.not.datetime_t(2011, 1, 1, 0) - datetime_t(2010, 3, 1, 4) == &
      duration_t(hours=7340)) error stop "Test failure"

  d = datetime_t(2021, 9, 10, 23) - datetime_t(2010, 3, 1, 4)
  if (.not.d == duration_t(hours=101083)) error stop "Test failure"
  d = datetime_t(2010, 3, 1, 4) - datetime_t(2021, 9, 10, 23)
  if (.not.d == duration_t(hours=-101083)) error stop "Test failure"

  d = duration_t(hours=2, seconds=3)
  if (d%seconds /= 7203_int64) error stop "Named units"
  if (.not.duration_t(seconds=90) == duration_t(90_int64)) error stop "Named seconds"
  if (.not.duration_t(seconds=3155673722_int64) == &
      duration_t(seconds=3155670122_int64, hours=1)) error stop "Named int64 seconds"
  if (.not.duration_t() == duration_t(seconds=0)) error stop "Default duration"

  start = datetime_t(2024, 12, 31, 23, 59, 59)
  fin = start + duration_t(1_int64)
  if (fin%year /= 2025 .or. fin%month /= 1 .or. fin%day /= 1 .or. &
      fin%hour /= 0 .or. fin%minute /= 0 .or. fin%second /= 0) error stop "Second carry"
  if (.not.fin - duration_t(1_int64) == start) error stop "Second borrow"
  if (.not.start + duration_t(-86400_int64) == datetime_t(2024, 12, 30, 23, 59, 59)) &
      error stop "Negative whole day"
  if (.not.start + duration_t(-86401_int64) == datetime_t(2024, 12, 30, 23, 59, 58)) &
      error stop "Negative day and second"
  if (.not.start + duration_t(0_int64) == start) error stop "Zero duration"
  if (.not.start - start == duration_t(0_int64)) error stop "Zero difference"

  start = datetime_t(2000, 2, 28, 23, 59, 59)
  if (.not.start + duration_t(1_int64) == datetime_t(2000, 2, 29, 0)) error stop "Leap century"
  start = datetime_t(1900, 2, 28, 23, 59, 59)
  if (.not.start + duration_t(1_int64) == datetime_t(1900, 3, 1, 0)) error stop "Common century"
  start = datetime_t(2100, 3, 1, 0)
  if (.not.start - duration_t(1_int64) == datetime_t(2100, 2, 28, 23, 59, 59)) error stop "Century borrow"
  start = datetime_t(2024, 3, 1, 0)
  if (.not.start - duration_t(1_int64) == datetime_t(2024, 2, 29, 23, 59, 59)) error stop "Leap borrow"
  if (start%minute /= 0 .or. start%second /= 0) error stop "Default components"

  start = datetime_t(1900, 1, 1, 0, 1, 2)
  fin = datetime_t(2000, 1, 1, 0, 3, 4)
  d = fin - start
  if (d%seconds /= 3155673722_int64) error stop "Large difference"
  if (.not.start + d == fin) error stop "Large forward round trip"
  if (.not.fin - d == start) error stop "Large backward round trip"
  if (.not.start - fin == duration_t(-d%seconds)) error stop "Negative difference"
  if (duration_t(1_int64) == duration_t(2_int64)) error stop "Duration inequality"

  do year = 1999, 2001
    do month = 1, 12
      do day = 1, 28, 27
        do hour = 0, 23, 23
          do minute = 0, 59, 59
            do second = 0, 59
              start = datetime_t(year, month, day, hour, minute, second)
              fin = start + duration_t(1_int64)
              call check_order(start, fin)
              if (.not.fin - start == duration_t(1_int64)) error stop "Second difference"
              if (.not.fin - duration_t(1_int64) == start) error stop "Second round trip"
              fin = start + duration_t(60_int64)
              call check_order(start, fin)
              if (.not.fin - start == duration_t(60_int64)) error stop "Minute difference"
              if (.not.fin - duration_t(60_int64) == start) error stop "Minute round trip"
            enddo
          enddo
        enddo
      enddo
    enddo
  enddo

contains

  subroutine check_order(earlier, later)
    type(datetime_t), intent(in) :: earlier, later
    if (earlier == later) error stop "Datetime inequality"
    if (.not.earlier == earlier) error stop "Datetime equality"
    if (.not.earlier < later .or. earlier > later) error stop "Strict ordering"
    if (.not.later > earlier .or. later < earlier) error stop "Reverse ordering"
    if (.not.earlier <= later .or. earlier >= later) error stop "Inclusive ordering"
    if (.not.later >= earlier .or. later <= earlier) error stop "Reverse inclusive ordering"
    if (.not.earlier <= earlier .or. .not.earlier >= earlier) error stop "Equal inclusive ordering"
    if (earlier < earlier .or. earlier > earlier) error stop "Equal strict ordering"
  end subroutine check_order

end program

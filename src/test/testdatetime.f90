program test_time
  use datetime, only: datetime_t, duration_t
  implicit none

  type(datetime_t) :: start, fin, fin2
  type(duration_t) :: d

  start = datetime_t(2021, 10, 1, 0)
  d = duration_t(24)
  fin = start + d
  if (.not.fin == datetime_t(2021, 10, 2, 0)) error stop "Test failure"
  fin2 = fin - d
  if (.not.fin2 == start) error stop "Test failure"
  fin2 = start - d
  if (.not.fin2 == datetime_t(2021, 9, 30, 0)) error stop "Test failure"

  start = datetime_t(2021, 10, 1, 0)
  d = duration_t(24000)
  fin = start + d
  if (.not.fin == datetime_t(2024, 6, 27, 0)) error stop "Test failure"
  fin2 = fin - d
  if (.not.fin2 == start) error stop "Test failure"
  fin2 = start - d
  if (.not.fin2 == datetime_t(2019, 1, 5, 0)) error stop "Test failure"

  start = datetime_t(2021, 10, 1, 0)
  d = duration_t(240000)
  fin = start + d
  if (.not.fin == datetime_t(2049, 2, 16, 0)) error stop "Test failure"
  fin2 = fin - d
  if (.not.fin2 == start) error stop "Test failure"
  fin2 = start - d
  if (.not.fin2 == datetime_t(1994, 5, 16, 0)) error stop "Test failure"

  start = datetime_t(1983, 10, 9, 17)
  d = duration_t(591237)
  fin = start + d
  if (.not.fin == datetime_t(2051, 3, 21, 14)) error stop "Test failure"
  fin2 = fin - d
  if (.not.fin2 == start) error stop "Test failure"
  fin2 = start - d
  if (.not.fin2 == datetime_t(1916, 4, 28, 20)) error stop "Test failure"

  fin = datetime_t(1994, 2, 28, 23) + duration_t(1)
  if (.not.fin == datetime_t(1994, 3, 1, 0)) error stop "Test failure"
  fin = datetime_t(1994, 3, 1, 0) - duration_t(1)
  if (.not.fin == datetime_t(1994, 2, 28, 23)) error stop "Test failure"

  fin = datetime_t(1996, 2, 28, 23) + duration_t(1)
  if (.not.fin == datetime_t(1996, 2, 29, 0)) error stop "Test failure"
  fin = datetime_t(1996, 2, 29, 0) - duration_t(1)
  if (.not.fin == datetime_t(1996, 2, 28, 23)) error stop "Test failure"

  if (.not.datetime_t(2022, 1, 1, 0) - datetime_t(2021, 9, 10, 23) == duration_t(2689)) error stop "Test failure"
  if (.not.datetime_t(2011, 1, 1, 0) - datetime_t(2010, 3, 1, 4) == duration_t(7340)) error stop "Test failure"

  d = datetime_t(2021, 9, 10, 23) - datetime_t(2010, 3, 1, 4)
  if (.not.d == duration_t(101083)) error stop "Test failure"
  d = datetime_t(2010, 3, 1, 4) - datetime_t(2021, 9, 10, 23)
  if (.not.d == duration_t(-101083)) error stop "Test failure"

end program

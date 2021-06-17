! gfortran testDateCalc.f90 -I../naccident/ ../naccident/dateCalc.o -o testDateCalc
program testDateCalc
  use DateCalc
  implicit none

  integer :: values(6), values2(6)
  integer(kind=8) :: epochSeconds


  values = (/0,0,0, 1, 1, 1970 /)
  if (timegm(values) .ne. 0) then
    write(*,*) "wrong epoch: ", timegm(values)
    call exit(1)
  end if

  values = (/0,0,0, 1, 6, 1970 /)
  if (timegm(values) .ne. 13046400) then
    write(*,*) "wrong ", values, ": ", timegm(values)
    call exit(1)
  end if

  values = (/0,0,0, 6, 1, 1970 /)
  if (timegm(values) .ne. 5*24*3600) then
    write(*,*) "wrong ", values, ": ", timegm(values)
    call exit(1)
  end if

  values = (/0,0,0, 1, 1, 1980 /)
  if (timegm(values) .ne. 315532800) then
    write(*,*) "wrong ", values, ": ", timegm(values)
    write(*,*) "wrong epoch: ", timegm(values)
    call exit(1)
  end if

  values = (/0,0,0, 1, 1, 1969 /)
  if (timegm(values) .ne. -31536000) then
    write(*,*) "wrong ", values, ": ", timegm(values)
    write(*,*) "wrong epoch: ", timegm(values)
    call exit(1)
  end if

  values = (/0,0,0, 1, 1, 1960 /)
  if (timegm(values) .ne. -315619200) then
    write(*,*) "wrong ", values, ": ", timegm(values)
    write(*,*) "wrong epoch: ", timegm(values)
    call exit(1)
  end if


  values = (/21,51,9, 26, 6, 1973 /)
  if (timegm(values) .ne. 109936281) then
    write(*,*) "wrong ", values, ": ", timegm(values)
    call exit(1)
  end if

  values = parseIsoDate("1973-06-26T09:51:21Z")
  epochSeconds = timegm(values)
  values2 = epochToDate(epochSeconds)
  if (.not. all(values.eq.values2)) then
    write(*,*) "wrong epochToDate: ", values, " != ", values2
    call exit(1)
  end if



  values = parseIsoDate("1973-06-26 09:51:21")
  if (timegm(values) .ne. 109936281) then
    write(*,*) "wrong 1973-06-26 09:51:21", parseIsoDate("1973-06-26 09:51:21")
    call exit(1)
  end if
  values = parseIsoDate("1973-06-26T09:51:21Z")
  if (timegm(values) .ne. 109936281) then
    write(*,*) "wrong 1973-06-26 09:51:21", parseIsoDate("1973-06-26T09:51:21Z")
    call exit(1)
  end if

  values = parseIsoDate("1973-6-26 9:51:1")
  if (timegm(values) .ne. 109936261) then
    write(*,*) "wrong 1973-6-26 9:51:1", parseIsoDate("1973-6-26 9:51:1")
    call exit(1)
  end if


  if (timeUnitScale("seconds since 1970-01-01 00:00:00") .ne. 1) then
    write(*,*) "error reading timeUnitScale('seconds since 1970-01-01 00:00:00'): ", &
                timeUnitScale("seconds since 1970-01-01 00:00:00")
    call exit(1)
  end if
  if (timeUnitOffset("seconds since 1970-01-01 00:00:00") .ne. 0) then
    write(*,*) "error reading timeUnitOffset('seconds since 1970-01-01 00:00:00'): ", &
                 timeUnitOffset("seconds since 1970-01-01 00:00:00")
    call exit(1)
  end if

  if (timeUnitOffset("days since 1900-1-1 00:00:00.0 +00:00") .ne. -613608*3600) then
    error stop "Error reading timeunitoffset"
  end if

  if (timeUnitOffset("days since 1900-01-01T00:00:00+00:00") .ne. -613608*3600) then
    error stop "Error reading timeunitoffset"
  end if

end program testDateCalc

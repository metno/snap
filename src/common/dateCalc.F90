module DateCalc
    implicit none
    public timeGM
    public parseDate

    integer, private, parameter :: MONTHDAYS(12) = (/ 0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30 /)
    integer, private, parameter :: SECS_PER_HOUR = 3600

contains
! scale of timeUnit in seconds
  function timeUnitScale(unit)
    character(*), intent(in) :: unit
    integer(kind=8) timeUnitScale
    integer :: ind=0
    character(25) :: tmp
    ind=index(unit," since ")
    if (ind .eq. 0) then
      write(*,*) "' since ' not found in time-units: ", unit
      call exit(1)
    end if
    tmp = trim(unit(:ind-1))
    timeUnitScale = 0
    if (tmp .eq. "seconds") timeUnitScale = 1
    if (tmp .eq. "minutes") timeUnitScale = 60
    if (tmp .eq. "hours") timeUnitScale = 60*60
    if (tmp .eq. "days") timeUnitScale = 60*60*24
    if (timeUnitScale .eq. 0) then
      write(*,*) "' since ' not found in time-units: ", unit
      call exit(1)
    end if
    return
  end function timeUnitScale

! offset in seconds to 1.1.1970
  function timeUnitOffset(unit)
    character(*), intent(in) :: unit
    integer(kind=8) timeUnitOffset
    integer :: ind=0
    character(25) :: date
    ind=index(unit," since ")
    if (ind .eq. 0) then
      write(*,*) "' since ' not found in time-units: ", unit
      call exit(1)
    end if
    date = trim(unit(ind+7:))
    timeUnitOffset = timeGM(parseDate(date, "YYYY-MM-DD hh:mm:ss"))
  end function timeUnitOffset

! calculate from values(6)=(/secs,min,hours,mday(1-31),month(1-12),year(since 0)
  function timeGM(values)
    integer, intent(in) :: values(6)
    integer(kind=8) :: timeGM
    timeGM = values(1) + 60 * (values(2) + 60 * values(3)) + daygm(values)
  end function timeGM

  function daygm(values)
    integer(kind=8) :: daygm
    integer, intent(in) :: values(6)
    integer i, ydays
    daygm = 0
    if (values(6) .ge. 1970) then
      do i = 1970, values(6)-1, 1
        daygm = daygm + (365 + leapyear(i)) * SECS_PER_HOUR*24
      end do
    else
      do i = 1969, values(6), -1
        daygm = daygm - (365 + leapyear(i)) * SECS_PER_HOUR*24
      end do
    end if

    ! the days of this month
    ydays = values(4)
    ! the days of the previous month
    do i = 1, values(5)
      ydays = ydays + MONTHDAYS(i)
    end do
    daygm = daygm + (ydays - 1) * SECS_PER_HOUR*24
    return
  end function daygm

! year since 0
  function leapyear(year)
    integer :: leapyear
    integer, intent(IN) :: year

    if (modulo(year, 4) > 0) then
      leapyear = 0
      return
    end if

    if (modulo(year, 100) > 0) then
      leapyear = 1
      return
    end if

    if (modulo(year, 400) > 0) then
      leapyear = 0
      return
    end if

    leapyear = 1
    return
  end function leapyear

  subroutine str2detail(str,fmt,year,month,day,hour,seconds,minute,second,days,&
                       fstep,ntme,nlev,nlat,nlon,debug)
    implicit none
    character(len=*), intent(in) :: str,fmt
    integer,intent(out),optional :: year,month,day,hour,seconds,&
                                    minute,second,days,&
                                    fstep,ntme,nlev,nlat,nlon
    logical, intent(in),optional :: debug
    if(present(second ))second =str2int(str,fmt,'ss'  )
    if(present(minute ))minute =str2int(str,fmt,'mm'  )
    if(present(hour   ))hour   =str2int(str,fmt,'hh'  )
    if(present(day    ))day    =str2int(str,fmt,'DD'  )
    if(present(month  ))month  =str2int(str,fmt,'MM'  )
    if(present(year   ))year   =str2int(str,fmt,'YYYY')
!   if(present(year   ))year   =str2int(str,fmt,'YY'  )+1900
    if(present(days   ))days   =str2int(str,fmt,'JJJ' ) ! day of the year
    if(present(nlon   ))nlon   =str2int(str,fmt,'LON' )
    if(present(nlat   ))nlat   =str2int(str,fmt,'LAT' )
    if(present(nlev   ))nlev   =str2int(str,fmt,'LL'  )
    if(present(ntme   ))ntme   =str2int(str,fmt,'TTT' )
    if(present(fstep  ))fstep  =str2int(str,fmt,'FFF' )
    if(present(debug))then
      if(debug) write(*,*)'string2date: ',trim(str),'/',trim(fmt)
    endif
  end subroutine str2detail

! convert a string to a list of values(6) = secs,min,hours, days,month,year
  function parseDate(str,fmt,debug) result(values)
    implicit none
    character(len=*), intent(in)   :: str,fmt
    integer                         :: values(6)
    logical, intent(in),optional  :: debug
    call str2detail(str,fmt,year=values(6),month=values(5),day=values(4),&
                    hour=values(3),minute=values(2),second=values(1),debug=debug)
  end function parseDate


! extract an integer from string in a certain format
  function str2int(str,fmt,key) result(val)
    implicit none
    character(len=*), intent(in) :: str,fmt,key
    integer :: val
    integer :: ind=0
    val=0
    ind=index(fmt,trim(key))
!    write(*,*) ind, key, fmt, str(ind:ind+len_trim(key)-1)
    if(ind>0)read(str(ind:ind+len_trim(key)-1),*)val
  end function str2int



end module DateCalc

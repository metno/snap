      subroutine filesort_nc
c
c       check and sort felt file contents
c
c       unsorted list of files and timesteps with data:
c         iavail( 1,n): year    )
c         iavail( 2,n): month   ) Time of analysis
c         iavail( 3,n): day     ) (not valid time of forecast)
c         iavail( 4,n): hour    )
c         iavail( 5,n): forecast hour
c         iavail( 6,n): file no. (in filename array)
c         iavail( 7,n): 1=model level  2=surface  3=both
c         iavail( 8,n): offset in hours from first (sorted) timestep
c         iavail( 9,n): pointer to next forward  (time) data
c         iavail(10,n): pointer to next backward (time) data
c                   n=1,navail
c
c       pointers to lists in iavail:
c         kavail(1): pointer to first forward  sorted timestep
c         kavail(2): pointer to first backward sorted timestep
c
      USE DateCalc
#if defined(DRHOOK)
      USE PARKIND1  ,ONLY : JPIM     ,JPRB
      USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
#endif
      implicit none
#if defined(DRHOOK)
      REAL(KIND=JPRB) :: ZHOOK_HANDLE ! Stack variable i.e. do not use SAVE
#endif
      include 'snapdim.inc'
      include 'snapfil.inc'
      include 'snapgrd.inc'
      include 'snapfld.inc'
      include 'snapdebug.inc'
! netcdf
      include 'netcdf.inc'

      integer i, j, ncid, nf, varid, dimid, tsize, ierror
      real(kind=8) times(mavail)
      integer zeroHour, tunitLen
      integer(kind=8) eTimes(mavail)
      integer(kind=8) :: add_offset, scalef
      integer, dimension(6) :: dateTime
      character(80) :: tunits

#if defined(DRHOOK)
! Before the very first statement
      IF (LHOOK) CALL DR_HOOK('FILESORT_NC',0,ZHOOK_HANDLE)
#endif

! position in iavail
      navail = 0
! loop over all file-names
      do nf = 1,nfilef
! get the time steps from the files "time" variable
        call check(nf_open(filef(nf), NF_NOWRITE, ncid), filef(nf))
        call check(nf_inq_varid(ncid, "time", varid), "time")
        call check(nf_inq_dimid(ncid, "time", dimid), "tdim-id")
        call check(nf_inq_dimlen(ncid, dimid, tsize), "tdim-len")
        if (tsize > size(times)) then
          write(*,*) "to many time-steps in ", filef(nf), ": ", tsize
          call exit(1)
        end if
        call check(nf_get_vara_double(ncid, varid, (/1/), (/tsize/),
     +             times), "time")
        call check(nf_inq_attlen(ncid, varid, "units", tunitLen))
        call check(nf_get_att_text(ncid, varid, "units", tunits),
     +             "time units")
! shrink units-string to actual size
        tunits = tunits(:tunitLen)
        add_offset = timeUnitOffset(tunits)
        scalef = timeUnitScale(tunits)
        do i = 1, tsize
          navail = navail + 1
          if(navail.gt.mavail) then
            if (navail.eq.mavail) then
              write(9,*) 'WARNING : TOO MANY AVAILABLE TIME STEPS'
              write(9,*) '          no.,max(MAVAIL): ',navail,mavail
              write(9,*) '    CONTINUING WITH RECORDED DATA'
              write(6,*) 'WARNING : TOO MANY AVAILABLE TIME STEPS'
              write(6,*) '          max (MAVAIL): ',mavail
              write(6,*) '    CONTINUING WITH RECORDED DATA'
            end if
            navail=mavail
          end if
          eTimes(i) = times(i)*scalef + add_offset
          dateTime = epochToDate(eTimes(i))
!          write(*,*) dateTime
          iavail( 1,navail) = dateTime(6)
          iavail( 2,navail) = dateTime(5)
          iavail( 3,navail) = dateTime(4)
          iavail( 4,navail) = dateTime(3)
c         iavail( 5,n): forecast hour
          iavail( 5,navail) = 0
          iavail( 6,navail) = nf
c         iavail( 7,n): 1=model level  2=surface  3=both
c in nc-mode: time-postion in file
          iavail( 7,navail) = i
c         iavail( 8,n): offset in hours from first (sorted) timestep
c         but currently used to store the hours since 1970-01-01
          iavail( 8,navail) = int(eTimes(i)/3600)
c         iavail( 9,n): pointer to next forward  (time) data
c         iavail(10,n): pointer to next backward (time) data
c still to be set
          iavail( 9,navail) = 0
          iavail(10,navail) = 0
        end do
      end do

c sorting time-steps, setting iavail 9, 10, kavail(1) and kavail(2)
c drop double occurances of time, using latest
      kavail(1) = 1
      kavail(2) = 1
      iavail(10, 1) = 0
      do i = 2, navail
c       run back until time is > existing time
        j = kavail(2)
        do while (j > 0 .and. iavail(8, i) < iavail(8, j))
          j = iavail(10, j)
        end do
c        write(*,*) i, j, kavail(1), kavail(2)
        if (j .eq. 0) kavail(1) = i
        if (j .eq. kavail(2)) kavail(2) = i

        if (j == 0) then
c         insert at beginning
          iavail(10, kavail(1)) = i
          iavail( 9, i) = kavail(1)
          iavail(10, i) = 0
        else
          if (iavail(8, i) == iavail(8, j)) then
c replace position j with i
            iavail( 9, i) = iavail( 9, j)
            iavail(10, i) = iavail(10, j)
c           set next of previous if previous exists
            if (iavail(10,j) .ne. 0) iavail( 9, iavail(10, j)) = i
c           set previous of next if next exists
            if (iavail( 9,j) .ne. 0) iavail(10, iavail(9, j)) = i
          else
c insert i as successor to j
            iavail( 9, i) = iavail( 9, j)
            iavail(10, i) = j
            iavail( 9, j) = i
          end if
        end if
      end do


c..time range
      do i=1,5
        itimer(i,1)=iavail(i,kavail(1))
        itimer(i,2)=iavail(i,kavail(2))
      end do
c..get valid time (with forecast=0)
      call vtime(itimer(1,1),ierror)
      call vtime(itimer(1,2),ierror)

c..adjust hours to hours since first available time
      zeroHour = iavail(8,kavail(1))
      do i=1,navail
        iavail(8, i) = iavail(8,i) - zeroHour
      end do



#if defined(DRHOOK)
! Before the return statement
      IF (LHOOK) CALL DR_HOOK('FILESORT_NC',1,ZHOOK_HANDLE)
#endif
      RETURN
      end subroutine filesort_nc

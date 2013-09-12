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

      integer i, ncid, nf, varid, dimid, tsize
      real(kind=8) times(mavail)
      integer(kind=8) eTimes(mavail)
      integer(kind=8) :: add_offset, scalef
      character(80) :: tunits

#if defined(DRHOOK)
! Before the very first statement
      IF (LHOOK) CALL DR_HOOK('FILESORT_NC',0,ZHOOK_HANDLE)
#endif

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
        call check(nf_get_att_text(ncid, varid, "units", tunits),
     +             "time units")
        add_offset = timeUnitOffset(tunits)
        scalef = timeUnitScale(tunits)
        do i = 1, tsize
          eTimes(i) = times(i)*scalef + add_offset
          write(*,*) epochToDate(eTimes(i))
        end do


      end do

#if defined(DRHOOK)
! Before the return statement
      IF (LHOOK) CALL DR_HOOK('FILESORT_NC',1,ZHOOK_HANDLE)
#endif
      RETURN
      end subroutine filesort_nc

! SNAP: Servere Nuclear Accident Programme
! Copyright (C) 1992-2017   Norwegian Meteorological Institute

! This file is part of SNAP. SNAP is free software: you can
! redistribute it and/or modify it under the terms of the
! GNU General Public License as published by the
! Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.

! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.

! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https://www.gnu.org/licenses/>.

module releasefileML
  implicit none
  private

  public releasefile

  contains

function should_skip_line(line)
  character(len=*), intent(in) :: line
  logical :: should_skip_line

  if (line == "") then
    should_skip_line = .true.
  else if (line(1:1) == "*") then
    should_skip_line = .true.
  else
    should_skip_line = .false.
  endif
end function

!> reading of input-files with hourly data (hours since run-start)
!> comment-rows start with #
!> hour height[upper_in_m] component release[kg/s]
!>
!> data is read from the filename and modifies the fields of
!> - releases
subroutine  releasefile(filename, release1)
  USE iso_fortran_env, only: error_unit, IOSTAT_END
  USE snapparML, only: ncomp, component
  USE releaseML, only: releases, nrelheight, release_t

  character(72), intent(in) :: filename
!> Structure to copy rellower, relupper, and relradius from
  type(release_t), intent(in) :: release1

  character(256) :: cinput
  integer :: ifd, ios, iexit, nlines
  integer :: i
  real :: hour, lasthour
  integer :: height
  integer :: ihour, iheight, icmp
  real ::    rel_s
  character(32) :: comp
  integer :: ntprof
  type(release_t), allocatable :: tmp_release(:)

  logical, parameter :: debugrelfile = .FALSE.
  iexit = 0

  write (error_unit,*) 'reading release from: ', filename
  if (debugrelfile) then
    write (error_unit,*) 'ncomp, nrelheight', ncomp, nrelheight
  end if

  open(newunit=ifd,file=filename, &
      access='sequential',form='formatted', &
      status='old',iostat=ios)
  if(ios /= 0) then
    write (error_unit,*) 'Open Error: ', trim(filename)
    error stop 1
  endif

  nlines = 0
  lasthour = -1
  ihour = 0
  inputlines: do
    nlines=nlines+1
    read(ifd, fmt='(a)', iostat=ios) cinput
    if (ios == IOSTAT_END) then
      exit inputlines
    else if (ios /= 0) then
      goto 11
    endif
    if (debugrelfile) write (error_unit,*) 'cinput (',nlines,'):',cinput
    if (should_skip_line(cinput)) cycle inputlines
    if (cinput == "end") exit inputlines

    read(cinput, *, err=12) hour, height, comp, rel_s

    if (hour > lasthour) then
      ! add new release timestep
      lasthour = hour
      if (.not.allocated(releases)) then
        allocate(releases(1))
      else
        call move_alloc(from=releases, to=tmp_release)
        allocate(releases(size(tmp_release)+1))
        releases(1:size(tmp_release)) = tmp_release
        deallocate(tmp_release)
      endif
      ihour = size(releases)
      releases(ihour)%frelhour = hour
      ! make sure all initial release are undefined
      releases(ihour)%relbqsec(:,:) = -1
    else
      ihour = 0
      do i=1,size(releases)
        if (releases(i)%frelhour == hour) then
          ihour = i
          exit
        endif
      enddo
      if (ihour == 0) then
        write(error_unit,*) "Unknown release hour: ", hour
        goto 12
      endif
    endif


    ! find the component
    icmp = 0
    do i=1,ncomp
      if(comp == component(i)) icmp=i
    end do
    if (icmp == 0) then
      write (error_unit,*) 'unknown component: ',comp
      goto 12
    endif
    ! find the height
    iheight = 0
    do i=1,nrelheight
      if(height == release1%rellower(i)) iheight = i
    end do
    if (iheight == 0) then
      write (error_unit,*) 'unkown lower height: ', height
      goto 12
    end if
    ! save the release
    releases(ihour)%relbqsec(icmp, iheight) = rel_s
    ! end ifnot comment '*'
  end do inputlines
  goto 18

  11 write (error_unit,*) 'ERROR reading file: ',filename(1:len(filename,1))
  write (error_unit,*) 'At line no. ',nlines
  iexit=2
  goto 18

  12 write(error_unit,*) 'ERROR reading file: ',filename(1:len(filename,1))
  write(error_unit,*) 'At line no. ',nlines,' :'
  write(error_unit,*)  cinput
  iexit=2
  goto 18

  18 close(ifd)
  ntprof = ihour
  write (error_unit,*) 'finished reading: ', ntprof, ' timesteps'
! theoretically possible to add time-varying heights/radiuses
! but not supported by input file format yet
  do ihour=1,ntprof
    releases(ihour)%rellower(1:nrelheight) = release1%rellower(1:nrelheight)
    releases(ihour)%relupper(1:nrelheight) = release1%relupper(1:nrelheight)
    releases(ihour)%relradius(1:nrelheight) = release1%relradius(1:nrelheight)
  end do

! sanity check of relbqsec
  do ihour=1,ntprof
    do icmp=1,ncomp
      do iheight=1,nrelheight
        if (releases(ihour)%relbqsec(icmp,iheight) < 0) then
          releases(ihour)%relbqsec(icmp,iheight) = 0
          write (error_unit,*) 'no release for (',component(icmp),',', &
          releases(ihour)%rellower(iheight),'m,',releases(ihour)%frelhour,'h)'
        end if
      end do
    end do
  end do

  if (iexit /= 0) then
    error stop 'aborted in releasefile.f90'
  end if

end subroutine  releasefile
end module releasefileML

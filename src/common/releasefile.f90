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
      subroutine  releasefile(filename)
c reading of input-files with hourly data (hours since run-start)
c comment-rows start with #
c hour height[upper_in_m] component release[kg/s]

c data is read to
c  frelhour
c  relbqsec(time, comp, height)
c
c for each release-step, rellower/relupper/relradius are copied from (1,x)
      USE snapparML
      implicit none

c input
      character*72 filename
      character*256 cinput
c
      logical debugrelfile
      integer ifd, ios, iend, iexit, nlines
      integer i,j
      real hour, lasthour
      integer height
      integer ihour, iheight, icmp
      real    rel_s
      character*32 comp
      debugrelfile = .false.
      iexit = 0

      write(*,*) 'reading release from: ', filename
      if (debugrelfile) then
        write(*,*) 'ncomp, nrelheight', ncomp, nrelheight
      end if

      ifd=8
      open(ifd,file=filename,
     +           access='sequential',form='formatted',
     +           status='old',iostat=ios)
      if(ios.ne.0) then
        write(6,*) 'Open Error: ',filename(1:len(filename,1))
        stop 1
      endif

c header row
      nlines=0
      iend=0
      lasthour = -1
      ihour = 0
      do while (iend.eq.0)
        nlines=nlines+1
        read(ifd,fmt='(a)',err=11) cinput
        if (debugrelfile) write(*,*) 'cinput (',nlines,'):',cinput
        if (cinput .eq. "end") goto 18
        if (cinput(1:1) .ne. '*') then
          read(cinput, *, err=12) hour, height, comp, rel_s
          if (lasthour.eq.-1 .and. hour.ne.0) then
            write(*,*) 'first hour must be 0'
            goto 12
          end if
          if (hour .lt. lasthour) then
            write(*,*) 'hour must increase monotonic: ',
     +                   hour, ' < ', lasthour
            goto 12
          end if
          if (hour .gt. lasthour) then
c add new release timestep
            lasthour = hour
            ihour = ihour + 1
            if (ihour .gt. mtprof) then
              write(*,*) 'to many release timesteps, increase mtprof'
              goto 12
            end if
            frelhour(ihour) = hour
c make sure all initial release are undefined
            do i=1,mcomp
              do j=1,mrelheight
                relbqsec(ihour,i,j)= -1.
              end do
            end do
          end if
c find the component
          icmp = 0
          do i=1,ncomp
            if(comp.eq.component(i)) icmp=i
          end do
          if (icmp .eq. 0) then
            write(*,*) 'unknown component: ',comp
            goto 12
          endif
c find the height
          iheight = 0
          do i=1,nrelheight
            if(height .eq. rellower(1,i)) iheight = i
          end do
          if (iheight .eq. 0) then
            write(*,*) 'unkown lower height: ', height
            goto 12
          end if
c save the release
          relbqsec(ihour, icmp, iheight) = rel_s
c end ifnot comment '*'
        end if
      end do
      goto 18
c
   11 write(6,*) 'ERROR reading file: ',filename(1:len(filename,1))
      write(6,*) 'At line no. ',nlines
      iexit=2
      goto 18
c
   12 write(6,*) 'ERROR reading file: ',filename(1:len(filename,1))
      write(6,*) 'At line no. ',nlines,' :'
      write(6,*)  cinput
      iexit=2
      goto 18

   18 close(ifd)
      ntprof = ihour
      write (*,*) 'finished reading: ', ntprof, ' timesteps'
c theoretically possible to add time-varying heights/radiuses
c but not supported by input file format yet
      do ihour=2,ntprof
        do iheight=1,nrelheight
          rellower(ihour,iheight) = rellower(1,iheight)
          relupper(ihour,iheight) = relupper(1,iheight)
          relradius(ihour,iheight) = relradius(1,iheight)
        end do
      end do

c sanity check of relbqsec
      do ihour=1,ntprof
        do icmp=1,ncomp
          do iheight=1,nrelheight
            if (relbqsec(ihour,icmp,iheight).lt.0) then
              relbqsec(ihour,icmp,iheight) = 0
              write(*,*) 'no release for (',component(icmp),',',
     c               rellower(ihour,iheight),'m,',frelhour(ihour),'h)'
            end if
          end do
        end do
      end do

      if (iexit.ne.0) then
        write(*,*) 'aborted in releasefile.f'
        call exit(1)
      end if


      end subroutine  releasefile

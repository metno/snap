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
      subroutine tabcon
c
c  Purpose:  Define fixed tables and constans
c            (independant of input data)
c
      implicit none
c
      include 'snapdim.inc'
      include 'snaptab.inc'
c
      integer i,j,n
      real    rcp,p,prestep,precint,probab
c
      rcp=r/cp
c
c..Exner function, pitab(0:130) for p=0,10,20,...1300 hPa
c
c      pmult=0.1
      do i=0,130
        p=i/pmult
        pitab(i)=1004.*(p/1000.)**rcp
      end do
c
c..precipitation probability for wet depositions
c
      if(nprepro.gt.0) then
        prestep=prepro(1,nprepro)/float(mpretab)
        premult=1./prestep
        i=2
        pretab(0)=0.0
        do n=1,mpretab
          precint=prestep*n
          do while (precint.gt.prepro(1,i) .and. i.lt.nprepro)
            i=i+1
          end do
          probab=( prepro(2,i-1)*(prepro(1,i)-precint)
     *            +prepro(2,i)  *(precint-prepro(1,i-1)))
     *           /(prepro(1,i)-prepro(1,i-1))
          pretab(n)=probab
        end do
        pretab(mpretab)=prepro(2,nprepro)
c######################################################################
        do n=1,nprepro
          write(9,*) '...n,prepro: ',n,prepro(1,n),prepro(2,n)
        end do
        write(9,*) '...premult: ',premult
        do n=0,mpretab,20
          precint=prestep*n
          write(9,*) '...n,pretab: ',n,precint,pretab(n)
        end do
c######################################################################
      else
        premult=0.
        do n=1,mpretab
          pretab(n)=0.
        end do
      end if
c
      return
      end

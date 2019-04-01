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

module tabconML
  implicit none
  private

  public tabcon

  contains

subroutine tabcon

!  Purpose:  Define fixed tables and constans
!            (independant of input data)

  USE snaptabML
  USE snapdimML, only: mpretab
  USE snapdebug, only: iulog
  implicit none

  integer :: i,n
  real :: p,prestep,precint,probab
  real, parameter :: rcp = r/cp

!..Exner function, pitab(0:130) for p=0,10,20,...1300 hPa

!      pmult=0.1
  do i=0,130
    p=i/pmult
    pitab(i)=1004.*(p/1000.)**rcp
  end do

!..precipitation probability for wet depositions

  if(nprepro > 0) then
    prestep=prepro(1,nprepro)/float(mpretab)
    premult=1./prestep
    i=2
    pretab(0)=0.0
    do n=1,mpretab
      precint=prestep*n
      do while (precint > prepro(1,i) .AND. i < nprepro)
        i=i+1
      end do
      probab=( prepro(2,i-1)*(prepro(1,i)-precint) &
      +prepro(2,i)  *(precint-prepro(1,i-1))) &
      /(prepro(1,i)-prepro(1,i-1))
      pretab(n)=probab
    end do
    pretab(mpretab)=prepro(2,nprepro)
  !######################################################################
    do n=1,nprepro
      write(iulog,*) '...n,prepro: ',n,prepro(1,n),prepro(2,n)
    end do
    write(iulog,*) '...premult: ',premult
    do n=0,mpretab,20
      precint=prestep*n
      write(iulog,*) '...n,pretab: ',n,precint,pretab(n)
    end do
  !######################################################################
  else
    premult=0.
    do n=1,mpretab
      pretab(n)=0.
    end do
  end if

  return
end subroutine tabcon
end module tabconML

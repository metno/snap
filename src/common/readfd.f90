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

subroutine readfd(iunit,nav,ivcord,iparam,ilevel,ihdisp, &
  field,ierror)

!  Purpose:  Read one field.

!  Method:   Call MRFTURBO (MRFELT fast version) to read the field,
!	     and possibly select a subarea.

!  Input:    iunit  - file unit no. (used for all files)
!            nav    - no. in the available timestep list
!		      nav=0: close file
!            ivcord - vertical coordinate
!            iparam - parameter
!            ilevel - level or level no.
!            ihdisp - time displacement in hours (forecast length)


  USE fileInfoML
  USE snapfilML
  USE snapgrdML
  USE snapdebugML
  implicit none


!..input
  integer ::   iunit,nav,ivcord,iparam,ilevel,ihdisp

!..output
  integer ::   ierror
  real ::      field(nx*ny)

!..local
  integer ::   iopen,itotal,ix1,ix2,iy1,iy2
  integer ::   ierr(3),ihelp(6),in9(2)
  integer*2 :: in(16)
  integer*2 :: i2dum

  integer :: imo,imr,imc,nf,ipack,i,ix,iy,no,j,ni

  data iopen,itotal/0,0/
  data ix1,ix2,iy1,iy2/4*0/


  if(idebug == 0) then
  !..silent (open,read,close modes)
    imo=11
    imr=12
    imc=13
  else
  !..print error messages (open,read,close modes)
    imo=1
    imr=2
    imc=3
  end if

  if(nav <= 0) then
  
  !..close last used file
    if(iopen > 0) then
    ! c 	  call mrfelt  (imc,filef(iopen),iunit,i2dum,0,0,0.,0.,
    ! c  +			0,i2dum,ierror)
      call mrfturbo(imc,filef(iopen),iunit,i2dum,0,0,0.,0., &
      & 			0,i2dum,ierror)
    end if
    iopen=0
    ierror=0
    return
  
  elseif(nav > navail) then
  
    write(9,*) 'PROGRAM ERROR IN READFD. nav,navail: ',nav,navail
    write(6,*) 'PROGRAM ERROR IN READFD. nav,navail: ',nav,navail
    ierror=1
    return
  
  end if

  nf=iavail(nav)%fileNo

  if(iopen /= nf) then
  
    if(iopen > 0) then
    ! c 	  call mrfelt  (imc,filef(iopen),iunit,i2dum,0,0,0.,0.,
    ! c  +			0,i2dum,ierror)
      call mrfturbo(imc,filef(iopen),iunit,i2dum,0,0,0.,0., &
      & 			0,i2dum,ierror)
    end if
    iopen=nf
  ! c	call mrfelt  (imo,filef(nf),iunit,i2dum,0,0,0.,0.,
  ! c  +		      0,i2dum,ierror)
    call mrfturbo(imo,filef(nf),iunit,i2dum,0,0,0.,0., &
    & 		      0,i2dum,ierror)
    if(ierror /= 0) then
      iopen=0
      return
    end if
  
  end if

!----------------------------------------------
!           in( 1) : producer
!           in( 2) : grid
!           in( 3) : year
!           in( 4) : month*100+day
!           in( 5) : hour*100+min
!           in( 9) : data type
!           in(10) : forecast time in hours
!           in(11) : vertical coordinate
!           in(12) : parameter
!           in(13) : level_1
!           in(14) : level_2
!----------------------------------------------

  in( 1)=iprod
  in( 2)=igrid
  in( 3)=iavail(nav)%aYear
  in( 4)=iavail(nav)%aMonth*100+iavail(nav)%aDay
  in( 5)=iavail(nav)%aHour*100
  in(10)=iavail(nav)%fcHour+ihdisp
  in(11)=ivcord
  in(12)=iparam
  in(13)=ilevel
  in(14)=-32767

  in(9)=-32767
  if(in(1) == 88 .AND. in(10) <= 0) in(9)=3
  if(in(1) == 88 .AND. in(10) > 0) in(9)=2

!..assuming no undefined values in the fields (ipack=1)
  ipack=1

  if(itotal == 1) then
  
  !..computation area equal input field area
  ! c	call mrfelt  (imr,filef(nf),iunit,in,ipack,nx*ny,field,
  ! c  +                1.0,ldata,idata,ierror)
    call mrfturbo(imr,filef(nf),iunit,in,ipack,nx*ny,field, &
    &                 1.0,ldata,idata,ierror)
  !###################################################################
    if(ierror == 0) write(9,fmt='(1x,11i6)') (idata(i),i=1,2), &
    (idata(i),i=12,14), &
    (idata(i),i=3,8)
  !###################################################################
  
    return
  
  end if

! c   call mrfelt  (imr,filef(nf),iunit,in,ipack,maxsiz,fdata,
! c  +              1.0,ldata,idata,ierror)
  call mrfturbo(imr,filef(nf),iunit,in,ipack,maxsiz,fdata, &
  &               1.0,ldata,idata,ierror)
!###################################################################
  if(ierror == 0) write(9,fmt='(1x,11i6)') (idata(i),i=1,2), &
  (idata(i),i=12,14), &
  (idata(i),i=3,8)
!###################################################################
  if(ierror /= 0) return

  ix=idata(10)
  iy=idata(11)

  if(itotal == 0) then
  
    if(ixbase < 1) ixbase=1
    if(iybase < 1) iybase=1
    if(ixystp < 1) ixystp=1
  
    ix1=ixbase
    ix2=ixbase+(nx-1)*ixystp
    iy1=iybase
    iy2=iybase+(ny-1)*ixystp
  !..move compute area if necessary
    if(ix2 > ix) then
      ix2=ix
      ix1=ix-(nx-1)*ixystp
    end if
    if(iy2 > iy) then
      iy2=iy
      iy1=iy-(ny-1)*ixystp
    end if
    if(ix1 < 1 .OR. iy1 < 1) then
      write(9,*) '*READFD* Field dimension problem.'
      write(9,*) '         Input x,y base:  ',ixbase,iybase
      write(9,*) '         Input x/y step:  ',ixystp
      write(9,*) '         Program x,y dim: ',nx,ny
      write(9,*) '         Field       dim: ',ix,iy
      close(iunit)
      stop 1
    end if
    if(ix1 /= ixbase .OR. iy1 /= iybase) then
      write(9,*) '*READFD* Field area moved.'
      write(9,*) '         Input x,y base:  ',ixbase,iybase
      write(9,*) '         Input x/y step:  ',ixystp
      write(9,*) '         Program x,y dim: ',nx,ny
      write(9,*) '         Field       dim: ',ix,iy
      write(9,*) '         Used  x,y base:  ',ix1,iy1
      ixbase=ix1
      iybase=iy1
    end if
  
    if(ix1 == 1 .AND. ix2 == nx .AND. &
    iy1 == 1 .AND. iy2 == ny .AND. ixystp == 1) then
      itotal=1
    else
      itotal=-1
    end if
  
  end if

  if(itotal /= 1) then
  !..computation field not equal input field
    no=0
    do j=iy1,iy2,ixystp
      ni=(j-1)*ix
      do i=ix1,ix2,ixystp
        no=no+1
        field(no)=fdata(ni+i)
      end do
    end do
  else
  !..computation field equal input field (get here only the first time)
    do i=1,nx*ny
      field(i)=fdata(i)
    end do
  end if

  ierror=0

  return
end subroutine readfd

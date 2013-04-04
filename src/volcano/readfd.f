      subroutine readfd(iunit,nav,ivcord,iparam,ilevel,ihdisp,
     +			field,ierror)
c
c  Purpose:  Read one field.
c
c  Method:   Call MRFTURBO (MRFELT fast version) to read the field,
c	     and possibly select a subarea.
c
c  Input:    iunit  - file unit no. (used for all files)
c            nav    - no. in the available timestep list
c		      nav=0: close file
c            ivcord - vertical coordinate
c            iparam - parameter
c            ilevel - level or level no.
c            ihdisp - time displacement in hours (forecast length)
c
c
      implicit none
c
      include 'snapdim.inc'
      include 'snapfil.inc'
      include 'snapgrd.inc'
      include 'snapdebug.inc'
c
c..input
      integer   iunit,nav,ivcord,iparam,ilevel,ihdisp
c
c..output
      integer   ierror
      real      field(nx*ny)
c
c..local
      integer   iopen,itotal,ix1,ix2,iy1,iy2
      integer   ierr(3),ihelp(6),in9(2)
      integer*2 in(16)
      integer*2 i2dum
c
      integer imo,imr,imc,nf,ipack,i,ix,iy,no,j,ni
c
      data iopen,itotal/0,0/
      data ix1,ix2,iy1,iy2/4*0/
c
      if(idebug.eq.0) then
c..silent (open,read,close modes)
	imo=11
	imr=12
	imc=13
      else
c..print error messages (open,read,close modes)
	imo=1
	imr=2
	imc=3
      end if
c
      if(nav.le.0) then
c
c..close last used file
        if(iopen.gt.0) then
ccc 	  call mrfelt  (imc,filef(iopen),iunit,i2dum,0,0,0.,0.,
ccc  +			0,i2dum,ierror)
	  call mrfturbo(imc,filef(iopen),iunit,i2dum,0,0,0.,0.,
     +			0,i2dum,ierror)
	end if
        iopen=0
        ierror=0
        return
c
      elseif(nav.gt.navail) then
c
	write(9,*) 'PROGRAM ERROR IN READFD. nav,navail: ',nav,navail
	write(6,*) 'PROGRAM ERROR IN READFD. nav,navail: ',nav,navail
	ierror=1
	return
c
      end if
c
      nf=iavail(6,nav)
c
      if(iopen.ne.nf) then
c
        if(iopen.gt.0) then
ccc 	  call mrfelt  (imc,filef(iopen),iunit,i2dum,0,0,0.,0.,
ccc  +			0,i2dum,ierror)
	  call mrfturbo(imc,filef(iopen),iunit,i2dum,0,0,0.,0.,
     +			0,i2dum,ierror)
	end if
	iopen=nf
ccc	call mrfelt  (imo,filef(nf),iunit,i2dum,0,0,0.,0.,
ccc  +		      0,i2dum,ierror)
	call mrfturbo(imo,filef(nf),iunit,i2dum,0,0,0.,0.,
     +		      0,i2dum,ierror)
	if(ierror.ne.0) then
	  iopen=0
	  return
	end if
c
      end if
c
c----------------------------------------------
c           in( 1) : producer
c           in( 2) : grid
c           in( 3) : year
c           in( 4) : month*100+day
c           in( 5) : hour*100+min
c           in( 9) : data type
c           in(10) : forecast time in hours
c           in(11) : vertical coordinate
c           in(12) : parameter
c           in(13) : level_1
c           in(14) : level_2
c----------------------------------------------
c
      in( 1)=iprod
      in( 2)=igrid
      in( 3)=iavail(1,nav)
      in( 4)=iavail(2,nav)*100+iavail(3,nav)
      in( 5)=iavail(4,nav)*100
      in(10)=iavail(5,nav)+ihdisp
      in(11)=ivcord
      in(12)=iparam
      in(13)=ilevel
      in(14)=-32767
c
      in(9)=-32767
      if(in(1).eq.88 .and. in(10).le.0) in(9)=3
      if(in(1).eq.88 .and. in(10).gt.0) in(9)=2
c
c..assuming no undefined values in the fields (ipack=1)
      ipack=1
c
      if(itotal.eq.1) then
c
c..computation area equal input field area
ccc	call mrfelt  (imr,filef(nf),iunit,in,ipack,nx*ny,field,
ccc  +                1.0,ldata,idata,ierror)
	call mrfturbo(imr,filef(nf),iunit,in,ipack,nx*ny,field,
     +                1.0,ldata,idata,ierror)
c###################################################################
	if(ierror.eq.0) write(9,fmt='(1x,11i6)') (idata(i),i=1,2),
     +						 (idata(i),i=12,14),
     +						 (idata(i),i=3,8)
c###################################################################
c
	return
c
      end if
c
ccc   call mrfelt  (imr,filef(nf),iunit,in,ipack,maxsiz,fdata,
ccc  +              1.0,ldata,idata,ierror)
      call mrfturbo(imr,filef(nf),iunit,in,ipack,maxsiz,fdata,
     +              1.0,ldata,idata,ierror)
c###################################################################
      if(ierror.eq.0) write(9,fmt='(1x,11i6)') (idata(i),i=1,2),
     +					       (idata(i),i=12,14),
     +					       (idata(i),i=3,8)
c###################################################################
      if(ierror.ne.0) return
c
      ix=idata(10)
      iy=idata(11)
c
      if(itotal.eq.0) then
c
	if(ixbase.lt.1) ixbase=1
	if(iybase.lt.1) iybase=1
	if(ixystp.lt.1) ixystp=1
c
	ix1=ixbase
	ix2=ixbase+(nx-1)*ixystp
	iy1=iybase
	iy2=iybase+(ny-1)*ixystp
c..move compute area if necessary
	if(ix2.gt.ix) then
	  ix2=ix
	  ix1=ix-(nx-1)*ixystp
	end if
	if(iy2.gt.iy) then
	  iy2=iy
	  iy1=iy-(ny-1)*ixystp
	end if
	if(ix1.lt.1 .or. iy1.lt.1) then
	  write(9,*) '*READFD* Field dimension problem.'
	  write(9,*) '         Input x,y base:  ',ixbase,iybase
	  write(9,*) '         Input x/y step:  ',ixystp
	  write(9,*) '         Program x,y dim: ',nx,ny
	  write(9,*) '         Field       dim: ',ix,iy
	  close(iunit)
	  stop 1
	end if
	if(ix1.ne.ixbase .or. iy1.ne.iybase) then
	  write(9,*) '*READFD* Field area moved.'
	  write(9,*) '         Input x,y base:  ',ixbase,iybase
	  write(9,*) '         Input x/y step:  ',ixystp
	  write(9,*) '         Program x,y dim: ',nx,ny
	  write(9,*) '         Field       dim: ',ix,iy
	  write(9,*) '         Used  x,y base:  ',ix1,iy1
	  ixbase=ix1
	  iybase=iy1
	end if
c
	if(ix1.eq.1 .and. ix2.eq.nx .and.
     +     iy1.eq.1 .and. iy2.eq.ny .and. ixystp.eq.1) then
	  itotal=1
	else
	  itotal=-1
	end if
c
      end if
c
      if(itotal.ne.1) then
c..computation field not equal input field
        no=0
        do j=iy1,iy2,ixystp
          ni=(j-1)*ix
          do i=ix1,ix2,ixystp
            no=no+1
            field(no)=fdata(ni+i)
          end do
        end do
      else
c..computation field equal input field (get here only the first time)
        do i=1,nx*ny
          field(i)=fdata(i)
        end do
      end if
c
      ierror=0
c
      return
      end

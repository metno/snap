      subroutine decay(tstep)
c
c  Purpose:  Decrease radioactive contents due to decay
c
      implicit none
c
      include 'snapdim.inc'
ccc   include 'snapgrd.inc'
      include 'snapfld.inc'
      include 'snappar.inc'
c
      real tstep
c
      integer i,j,m,n
c
      logical prepare
c
      data prepare/.true./
c
c
      if(prepare) then
c
c..radioactive decay rate
        do m=1,ndefcomp
	  if (kdecay(m).eq.1) then
	    decayrate(m)= exp(-log(2.0)*tstep/(halftime(m)*3600.))
	  else
	    decayrate(m)=1.0
	  end if
        end do
c
	prepare=.false.
      end if
c
      do n=1,npart
	m= icomp(n)
	if(kdecay(m).eq.1) then
	  pdata(9,n)= pdata(9,n) * decayrate(m)
	end if
      end do
c

	do m=1,ndefcomp
        if(kdecay(m).eq.1) then
	do i=1,nx
	do j=1,ny
	  depdry(i,j,m)=depdry(i,j,m)*decayrate(m)
	  depwet(i,j,m)=depwet(i,j,m)*decayrate(m)
	  accdry(i,j,m)=accdry(i,j,m)*decayrate(m)
	  accwet(i,j,m)=accwet(i,j,m)*decayrate(m)
	enddo
	enddo
	endif
	enddo
c
      return
      end

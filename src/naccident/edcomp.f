      subroutine edcomp(nx,ny,nz,u,v,edot,ps,xmd2h,ymd2h,
     +			ahalf,bhalf,vhalf,
     +			uu,vv,dpsdt,edoth)
c
c	  compute etadot (in full levels) from u,v and ps
c
      implicit none
c
      integer nx,ny,nz
      real    u(nx,ny,nz),v(nx,ny,nz),edot(nx,ny,nz)
      real    ps(nx,ny),xmd2h(nx,ny),ymd2h(nx,ny)
      real    ahalf(nz+1),bhalf(nz+1),vhalf(nz+1)
      real    uu(nx,ny),vv(nx,ny),dpsdt(nx,ny),edoth(nx,ny)
c
      integer i,j,k
      real    da,db,dp,deta,div,edothu,etadot
c
c..computation of surface pressure tendency.............................
c
	do j=1,ny
	  do i=1,nx
	    uu(i,j)=0.
	    vv(i,j)=0.
	  end do
	end do
c
	do k=1,nz
c
	  da=ahalf(k)-ahalf(k+1)
	  db=bhalf(k)-bhalf(k+1)
c
	  do j=1,ny
	    do i=1,nx
	      dp=da+db*ps(i,j)
	      uu(i,j) = uu(i,j) - u(i,j,k)*dp
	      vv(i,j) = vv(i,j) - v(i,j,k)*dp
	    end do
	  end do
c
c.......end do k=1,nz
	end do
c
	do j=2,ny-1
	  do i=2,nx-1
	    dpsdt(i,j) = (uu(i+1,j)-uu(i-1,j))*xmd2h(i,j)
     +			+(vv(i,j+1)-vv(i,j-1))*ymd2h(i,j)
	  end do
	end do
c
c..vertical integration of the continuity equation......................
c
	do j=1,ny
	  do i=1,nx
	    edoth(i,j)=0.
	  end do
	end do
c
	do k=1,nz-1
c
	  da=ahalf(k)-ahalf(k+1)
	  db=bhalf(k)-bhalf(k+1)
	  deta=vhalf(k)-vhalf(k+1)
c
	  do j=1,ny
	    do i=1,nx
	      dp=da+db*ps(i,j)
	      uu(i,j)=dp*u(i,j,k)
	      vv(i,j)=dp*v(i,j,k)
	    end do
	  end do
c
	  do j=2,ny-1
	    do i=2,nx-1
c..divergence
	      div= (uu(i+1,j)-uu(i-1,j))*xmd2h(i,j)
     +		  +(vv(i,j+1)-vv(i,j-1))*ymd2h(i,j)
c..etadot in half level below full level k
	      edothu=edoth(i,j)
c..etadot in half level above full level k
	      edoth(i,j)= edoth(i,j) + db*dpsdt(i,j) + div
c..etadot in full level k (mean of the two previous)
	      etadot=(edothu+edoth(i,j))*0.5
c..etadot * dp/deta -> etadot
	      dp=da+db*ps(i,j)
	      etadot=etadot*deta/dp
c..and then mean with the input value
	      edot(i,j,k)=(edot(i,j,k)+etadot)*0.5
	    end do
	  end do
c
c.......end do k=nz,2,-1
	end do
c
c..etadot in upper full level
	da=ahalf(nz)-ahalf(nz+1)
	db=bhalf(nz)-bhalf(nz+1)
	deta=vhalf(nz)-vhalf(nz+1)
	do j=2,ny-1
	  do i=2,nx-1
	    dp=da+db*ps(i,j)
	    edot(i,j,nz)=(edot(i,j,nz)+0.5*edoth(i,j)*deta/dp)*0.5
	  end do
	end do
c
	do k=1,nz
c
	  do i=1,nx
	    edot(i, 1,k)=edot(i, 1,k)*0.5
	    edot(i,ny,k)=edot(i,ny,k)*0.5
	  end do
c
	  do j=2,ny-1
	    edot( 1,j,k)=edot( 1,j,k)*0.5
	    edot(nx,j,k)=edot(nx,j,k)*0.5
	  end do
c
c.......end do k=1,nz
	end do
c
	return
	end

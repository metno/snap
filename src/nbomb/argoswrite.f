      subroutine argoswrite(iunit,name,iparam,itimeargos,nx,ny,dblfield)
c-----------------------------------------------------------------------
c
c 25.05.12 - new output format for remote NRPA - Jerzy Bartnicki
c 02.06.12 - dblfield changed from one-dimensional to two-dimensional
c
      implicit none
c
      include 'snapdebug.inc'
c
c..input
      integer iunit,iparam,nx,ny,itimeargos(5)
      character*4 name
cjb      double precision dblfield(nx*ny)
      double precision dblfield(nx,ny)
c
c..local
      integer nxy,ij,ij1,ij2,i,j,k
      double precision dblmin,dblmax
c
      nxy=nx*ny
c
      if(name.eq.'depo') write(iunit,1001,err=900) iparam
      if(name.eq.'conc') write(iunit,1002,err=900) iparam
      if(name.eq.'dose') write(iunit,1003,err=900) iparam
c
cjb      write(iunit,1004,err=900) 1.0
      write(iunit,1005,err=900) (itimeargos(i),i=1,5)
c
	k=0
cjb	do ij=1,nxy
	do i=1,nx
	do j=1,ny
	  if(dblfield(i,j).gt.0.0d0) k=k+1
	enddo
	enddo
c
	write(iunit,1007,err=900) k
	do j=1,ny
	do i=1,nx
	  if(dblfield(i,j).gt.0.0d0) then
	  write(iunit,1008,err=900) i,j,dblfield(i,j)
	  endif
	enddo
	enddo
	
cjb      do ij1=1,nxy,10
cjb	ij2= min(ij1+9,nxy)
cjb	write(iunit,1006,err=900) (dblfield(ij),ij=ij1,ij2)
cjb      end do
cc
 1001 format('Isotope ',i3,' deposition (Unit/m2)')
 1002 format('Isotope ',i3,' concentration (Unit/m3)')
 1003 format('Isotope ',i3,' dose (Unit*hr/m3)')
 1004 format(1pe8.2e2)
 1005 format(5i2.2)
 1006 format(10(1pe14.6e2))
 1007 format(i4)
 1008 format(2i5,1pe14.6e2)
c
      return
c
  900 continue
      stop 'ARGOSWRITE'
c
      end

c gdata.f
c
c Program gdata.f generates data in the EMEP grid for the maps of deposition, time integrated concentration 
c and inst. concentration from the bomb simulation.
c Input files:
c   griddef.gdf   - geographical coordinates of the SNAP grid
c   ../snap_depo  - deposition data from the SNAP run
c   ../snap_dose  - time integrated concentration data from the SNAP run
c   ../snap_conc  - inst. concentration data from the SNAP run
c Output files:
c   grid_depo  - deposition data for the maps
c   grid_dose  - time integrated concentration  for the maps
c   grid_conc  - inst. concentration data  for the maps    
c------------------------------------------------------------------------------------------------------------------
c 03.11.2011 - first version (m.f) - Jerzy Bartnicki
c 01.06.2012 - second version for bomb - Jerzy Bartnicki
c------------------------------------------------------------------------------------------------------------------
c
	implicit none
	character*80 lname
	integer imax, jmax	! dimension of the SNAP grid   864  698
	parameter(imax=864,jmax=698)
	integer i,j,k		! indexes
	real lon(imax,imax)	! longitude matrix
	real lat(imax,imax)	! lattitude matrix
	real depo(imax,jmax)	! deposition
	real conc(imax,jmax)	! inst concentration
	real dose(imax,jmax)	! time integrated concentration matrix
	integer ngrid		! number of lines
        character*10 idate	! dateid
	integer np		! number of periods
	integer ip		! current period
	integer nc		! number of particle classes
	integer ic		! current particle class
        real value	! input value
c
c------------------------------------------------------------------------------------------------------------------
c
c... initialization
c
	do i=1,imax
	do j=1,jmax
	   depo(i,j)=0.0
	   dose(i,j)=0.0
	   conc(i,j)=0.0	
	enddo
	enddo
c
	nc=10	! 10 particle classes for the bomb case
c
c... read longitude and latitude of the SNAP grid
c
	open(1,file='griddef.gdf')
	rewind 1
	write(*,*) 'griddef.gdf - opened'
c
	do k=1,7
	  read(1,*) lname
c	  write(*,*) lname
	enddo
c
	read(1,"(10f10.4)") ((lon(i,j),i=1,imax),j=1,jmax)
c
	write(*,*)
	write(*,*) 'Longitude sample values'
	write(*,*)
	write(*,"(5x,6i10)") (i,i=45,50)
	do j=32,27,-1
	   write(*,"(i3,2x,6f10.3)") j,(lon(i,j),i=45,50)
	enddo
	write(*,*)
c
	do k=1,2
	  read(1,*) lname
c	  write(*,*) lname
	enddo
c
	read(1,"(10f10.4)") ((lat(i,j),i=1,imax),j=1,jmax)
c
	write(*,*)
	write(*,*) 'Latitude sample values'
	write(*,*)
	write(*,"(5x,6i10)") (i,i=45,50)
	do j=32,27,-1
cjb	do j=205,200,-1
	   write(*,"(i3,2x,6f10.3)") j,(lat(i,j),i=45,50)
	enddo
	write(*,*)
c
	close (1)
c
c... read inst.conc
c
	open(1,file='../snap_conc')
	rewind 1
	write(*,*) 'file snap_conc - opened'
c
	do k=1,4
	  read(1,*) lname
	  write(*,*) lname
	enddo
	read(1,*) np,idate
	write(*,*) 'np=',np
	write(*,*) 'idate=',idate		
c
c... period and class loop
c
	write(*,*)
	do ip=1,np
	do ic=1,nc
	  write(*,*) 'ip=',ip	
	  write(*,*) 'ic=',ic	
	  do k=1,2
	    read(1,*) lname
	    write(*,*) lname
	  enddo
	  read(1,*) ngrid
	  write(*,*) 'ngrid=',ngrid
	  do k=1,ngrid
	    read(1,"(2(i4,1x),e12.4)") i,j,value
	    write(*,"(2(i4,1x),e12.4)") i,j,value
	    write(*,"(2(f8.2,1x),e12.4)") lon(i,j),lat(i,j),value
	    conc(i,j)=conc(i,j)+value
c	    write(2,"(2(f8.2,1x),e12.4)") lon(i,j),lat(i,j),conc
	  enddo	
	enddo
	enddo
c
c... write inst. conc to the file
c
	open(2,file='grid_conc')
	rewind 2
	write(*,*) 'grid_conc - opened'
c
c... calculate number of non-zero points
c
	ngrid=0
	do i=1,imax
	do j=1,jmax
	  if(conc(i,j) .gt. 0.0) then
	     ngrid=ngrid+1
	  endif	
	enddo
	enddo
c
c... write non-zero concentrations to the output file
c
	write(2,*) ngrid
	do i=1,imax
	do j=1,jmax
	  if(conc(i,j) .gt. 0.0) then
	    write(2,"(2(f8.2,1x),e12.4)") lon(i,j),lat(i,j),conc(i,j) 
	  endif	
	enddo
	enddo
c
	close (1)
	close (2)
c
c... read and write deposition
c
	open(1,file='../snap_depo')
	rewind 1
	write(*,*) 'snap_depo - opened'
c
c
	do k=1,4
	  read(1,*) lname
	  write(*,*) lname
	enddo
	read(1,*) np,idate
	write(*,*) 'np=',np
	write(*,*) 'idate=',idate		
c
c... period and class loop
c
	write(*,*)
	do ip=1,np
	do ic=1,nc
	  write(*,*) 'ip=',ip	
	  write(*,*) 'ic=',ic	
	  do k=1,2
	    read(1,*) lname
	    write(*,*) lname
	  enddo
	  read(1,*) ngrid
	  write(*,*) 'ngrid=',ngrid
	  do k=1,ngrid
	    read(1,"(2(i4,1x),e12.4)") i,j,value
	    write(*,"(2(i4,1x),e12.4)") i,j,value
	    write(*,"(2(f8.2,1x),e12.4)") lon(i,j),lat(i,j),value
	    depo(i,j)=depo(i,j)+value
c	    write(2,"(2(f8.2,1x),e12.4)") lon(i,j),lat(i,j),conc
	  enddo	
	enddo
	enddo
c
c... write deposition to the file
c
	open(2,file='grid_depo')
	rewind 2
	write(*,*) 'grid_depo - opened'
c
c... calculate number of non-zero points
c
	ngrid=0
	do i=1,imax
	do j=1,jmax
	  if(depo(i,j) .gt. 0.0) then
	     ngrid=ngrid+1
	  endif	
	enddo
	enddo
c
c... write non-zero concentrations to the output file
c
	write(2,*) ngrid
	do i=1,imax
	do j=1,jmax
	  if(depo(i,j) .gt. 0.0) then
	    write(2,"(2(f8.2,1x),e12.4)") lon(i,j),lat(i,j),depo(i,j) 
	  endif	
	enddo
	enddo
c
	close (1)
	close (2)
c
c... read and write time integrated concntration
c
	open(1,file='../snap_dose')
	rewind 1
	write(*,*) 'snap_dose - opened'
c
	do k=1,4
	  read(1,*) lname
	  write(*,*) lname
	enddo
	read(1,*) np,idate
	write(*,*) 'np=',np
	write(*,*) 'idate=',idate		
c
c... period and class loop
c
	write(*,*)
	do ip=1,np
	do ic=1,nc
	  write(*,*) 'ip=',ip	
	  write(*,*) 'ic=',ic	
	  do k=1,2
	    read(1,*) lname
	    write(*,*) lname
	  enddo
	  read(1,*) ngrid
	  write(*,*) 'ngrid=',ngrid
	  do k=1,ngrid
	    read(1,"(2(i4,1x),e12.4)") i,j,value
	    write(*,"(2(i4,1x),e12.4)") i,j,value
	    write(*,"(2(f8.2,1x),e12.4)") lon(i,j),lat(i,j),value
	    dose(i,j)=dose(i,j)+value
c	    write(2,"(2(f8.2,1x),e12.4)") lon(i,j),lat(i,j),conc
	  enddo	
	enddo
	enddo
c
c... write dosesition to the file
c
	open(2,file='grid_dose')
	rewind 2
	write(*,*) 'grid_dose - opened'
c
c... calculate number of non-zero points
c
	ngrid=0
	do i=1,imax
	do j=1,jmax
	  if(dose(i,j) .gt. 0.0) then
	     ngrid=ngrid+1
	  endif	
	enddo
	enddo
c
c... write non-zero concentrations to the output file
c
	write(2,*) ngrid
	do i=1,imax
	do j=1,jmax
	  if(dose(i,j) .gt. 0.0) then
	    write(2,"(2(f8.2,1x),e12.4)") lon(i,j),lat(i,j),dose(i,j) 
	  endif	
	enddo
	enddo
c
	close (1)
	close (2)
c
	stop
	end	

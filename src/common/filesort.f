      subroutine filesort(iunit,ierror)
c
c       check and sort felt file contents
c
c       unsorted list of files and timesteps with data:
c         iavail( 1,n): year    )
c         iavail( 2,n): month   ) Time of analysis
c         iavail( 3,n): day     ) (not valid time of forecast)
c         iavail( 4,n): hour    )
c         iavail( 5,n): forecast hour
c         iavail( 6,n): file no. (in filename array)
c         iavail( 7,n): 1=model level  2=surface  3=both
c         iavail( 8,n): offset in hours from first (sorted) timestep
c         iavail( 9,n): pointer to next forward  (time) data
c         iavail(10,n): pointer to next backward (time) data
c                   n=1,navail
c
c       pointers to lists in iavail:
c         kavail(1): pointer to first forward  sorted timestep
c         kavail(2): pointer to first backward sorted timestep
c
      implicit none
c
      include 'snapdim.inc'
      include 'snapfil.inc'
      include 'snapgrd.inc'
      include 'snapfld.inc'
      include 'snapdebug.inc'
c
c..input/output
      integer   iunit,ierror
c
c..local
      integer   maxinh
      parameter (maxinh=64*8)
c
      integer*2 inh(16,maxinh),idfile(32)
      integer   ifound(maxinh),itime(5),itimev(5),itimeref(5)
      integer*2 i2dum
c
      integer lenstr
      integer nf,i,mhdiff,minhfc,maxhfc,nbegin,iend,nfound,ioerr
      integer n,ihdiff,ierr1,ierr2,iaforw,iaback,laforw,laback
      integer k,ihrmin,iprmin,nmin,ihrmax,iprmax,nmax,ktest,modsurf,j
c
      if(idebug.eq.1) then
        write(9,*)
        write(9,*) 'FILESORT------------------------------------------'
        write(9,*) 'iprod,igrid:   ',iprod,igrid
        write(9,*) 'FILES.    nfilef= ',nfilef
        do nf=1,nfilef
          write(9,*) nf,'  ',filef(nf)(1:lenstr(filef(nf),1))
          write(9,*) '      min,max fc:',(limfcf(i,nf),i=1,2)
        end do
      end if
c
      navailt1=0
      navailt2=0
c
      navail=0
      mhdiff=0
c
      do nf = 1,nfilef
c
c..open the (standard/archive/cyclic_archive) felt file
c
ccc     call mrfelt  (1,filef(nf),iunit,i2dum,0,0,0.,0.,
ccc  +		      32,idfile,ierror)
        call mrfturbo(1,filef(nf),iunit,i2dum,0,0,0.,0.,
     +		      32,idfile,ierror)
c
        if(ierror.eq.0) then
c
c..min,max forecast length in hours for model level data
          minhfc=limfcf(1,nf)
          maxhfc=limfcf(2,nf)
c
	  ktest=klevel(nk-kadd)
c
          nbegin=navail
          iend=0
c
          do while (iend.eq.0)
c
            do i=1,16
              inh(i,1)=-32767
            end do
c..search for u component of wind in upper input model level
            inh( 1,1)=iprod
            inh( 2,1)=igrid
c
            call qfelt(iunit,1,1,maxinh,inh(1,1),ifound(1),nfound,
     +                 iend,ierror,ioerr)
c
            if(nfound.gt.0 .and. ierror.eq.0) then
c
              do n = 1,nfound
c
		if(inh(10,n).ge.minhfc .and. inh(10,n).le.maxhfc) then
		  modsurf=0
		  if(inh(11,n).eq.ivcoor .and. inh(12,n).eq.2
     +					 .and. inh(13,n).eq.ktest) then
		    modsurf=1
		  elseif(inh(11,n).eq.2 .and. inh(12,n).eq.33
     +					.and. inh(13,n).eq.1000) then
		    modsurf=2
		  end if
		  if(modsurf.gt.0) then
c..year,month,day,hour,forecast_hour
                    itime(1)=inh(3,n)
                    itime(2)=inh(4,n)/100
                    itime(3)=inh(4,n)-(inh(4,n)/100)*100
                    itime(4)=inh(5,n)/100
                    itime(5)=inh(10,n)
	            do i=1,5
		      itimev(i)=itime(i)
		    end do
                    call vtime(itimev(1),ierror)
	            if(ierror.eq.0) then
		      k=0
		      j=0
		      do while (k.eq.0 .and. j.lt.navail)
		        j=j+1
			if(iavail(1,j).eq.itime(1) .and.
     +			   iavail(2,j).eq.itime(2) .and.
     +			   iavail(3,j).eq.itime(3) .and.
     +			   iavail(4,j).eq.itime(4) .and.
     +			   iavail(5,j).eq.itime(5) .and.
     +			   iavail(6,j).eq.nf)  k=j
     		      end do
		      if(k.eq.0) then
                        navail=navail+1
                        if(navail.le.mavail) then
		          if(navail.eq.1) then
	                    do i=1,5
		              itimeref(i)=itimev(i)
		            end do
		            ihdiff=0
		          else
		            call hrdiff(0,0,itimeref(1),itimev(1),
     +				        ihdiff,ierr1,ierr2)
		            mhdiff=min(mhdiff,ihdiff)
		          end if
		          iavail( 1,navail)=itime(1)
		          iavail( 2,navail)=itime(2)
		          iavail( 3,navail)=itime(3)
		          iavail( 4,navail)=itime(4)
		          iavail( 5,navail)=itime(5)
                          iavail( 6,navail)=nf
                          iavail( 7,navail)=modsurf
                          iavail( 8,navail)=ihdiff
                          iavail( 9,navail)=0
                          iavail(10,navail)=0
		        end if
		      else
		        if(iavail(7,k).eq.1 .and. modsurf.eq.2) iavail(7,k)=3
		        if(iavail(7,k).eq.2 .and. modsurf.eq.1) iavail(7,k)=3
		      end if
                    end if
		  end if
		end if
c
c.............end do n = 1,nfound
              end do
c
            end if
c
c.........end do while (iend.eq.0)
          end do
c
c..qfelt cleanup
          call qfelt(0,0,1,maxinh,inh(1,1),ifound(1),nfound,
     +               iend,ierror,ioerr)
c
c..close the (standard/archive/cyclic_archive) felt file
c
ccc       call mrfelt  (3,filef(nf),iunit,i2dum,0,0,0.,0.,
ccc  +			1,idfile,ierror)
          call mrfturbo(3,filef(nf),iunit,i2dum,0,0,0.,0.,
     +			1,idfile,ierror)
c
        end if
c
c.....end do nf = 1,nfilef
      end do
c
      do n=1,navail
	iavail(8,n)=iavail(8,n)+mhdiff
      end do
c
      if(navail.gt.mavail) then
        write(9,*) 'WARNING : TOO MANY AVAILABLE TIME STEPS'
        write(9,*) '          no.,max(MAVAIL): ',navail,mavail
        write(9,*) '    CONTINUING WITH RECORDED DATA'
        write(6,*) 'WARNING : TOO MANY AVAILABLE TIME STEPS'
        write(6,*) '          max (MAVAIL): ',mavail
        write(6,*) '    CONTINUING WITH RECORDED DATA'
        navail=mavail
      end if
c
c#############################################################################
c      if(idebug.eq.1) then
c        write(9,*) 'ALL STEPS UNSORTED AVAILABLE    navail=',navail
c        do n=1,navail
c          write(9,fmt='(i4,'':'',7(1x,i4),1x,i6,2i5)')
c     +				n,(iavail(i,n),i=1,10)
c        end do
c      end if
c#############################################################################
      n=0
      do while (n.lt.navail)
        n=n+1
	if(iavail(7,n).eq.2) then
	  k=0
	  j=0
	  do while (k.eq.0 .and. j.lt.navail)
	    j=j+1
	    if(iavail(1,j).eq.iavail(1,n) .and.
     +	       iavail(2,j).eq.iavail(2,n) .and.
     +	       iavail(3,j).eq.iavail(3,n) .and.
     +	       iavail(4,j).eq.iavail(4,n) .and.
     +	       iavail(5,j).eq.iavail(5,n) .and.
     +	       iavail(7,j).ne.2) k=j
	  end do
	  if(k.eq.0) then
	    navail=navail-1
	    do j=n,navail
	      do i=1,10
	        iavail(i,j)=iavail(i,j+1)
	      end do
	    end do
	    n=n-1
	  end if
	end if
      end do
c
      if(idebug.eq.1) then
        write(9,*) 'UNSORTED AVAILABLE    navail=',navail
        do n=1,navail
          write(9,fmt='(i4,'':'',7(1x,i4),1x,i6,2i5)')
     +				n,(iavail(i,n),i=1,10)
        end do
      end if
c
      if(navail.lt.2) then
        write(9,*) 'NOT ENOUGH AVAILABLE DATA'
        write(9,*) '  Total no. of recorded timesteps: ',navail
        write(6,*) 'NOT ENOUGH AVAILABLE DATA'
        write(6,*) '  Total no. of recorded timesteps: ',navail
        ierror=1
        return
      end if
c
c..make the forward and backward (time) pointers,
c..always shortest forecast length first if equal time difference,
c..which means that one of the pointer lists is not the other reversed
c..(if everything equal, the file input sequence is kept)
c
      iaforw=0
      iaback=0
      laforw=0
      laback=0
c
      do k=1,navail
c
        ihrmin=+999999999
        iprmin=+999999999
        nmin=0
        ihrmax=-999999999
        iprmax=+999999999
        nmax=0
c
        do n=1,navail
c..forward
          if(iavail(9,n).eq.0 .and. (iavail(8,n).lt.ihrmin .or.
     +       (iavail(8,n).eq.ihrmin .and. iavail(5,n).lt.iprmin))) then
            ihrmin=iavail(8,n)
            iprmin=iavail(5,n)
            nmin  =n
          end if
c..backward (note that shortest forecast length is recorded first)
          if(iavail(10,n).eq.0 .and. (iavail(8,n).gt.ihrmax .or.
     +       (iavail(8,n).eq.ihrmax .and. iavail(5,n).lt.iprmax))) then
            ihrmax=iavail(8,n)
            iprmax=iavail(5,n)
            nmax  =n
          end if
c
        end do
c
c..start of forward list
        if(iaforw.eq.0) iaforw=nmin
c..forward pointer from previous
        if(laforw.gt.0) iavail(9,laforw)=nmin
        laforw=nmin
	iavail(9,laforw)=-1
c
c..start of backward list
        if(iaback.eq.0) iaback=nmax
c..backward pointer from previous
        if(laback.gt.0) iavail(10,laback)=nmax
        laback=nmax
	iavail(10,laback)=-1
c
c.....end do k=1,navail
      end do
c
      iavail( 9,laforw)=0
      iavail(10,laback)=0
c
      kavail(1)=iaforw
      kavail(2)=iaback
c
c..set time difference in hours equal zero for first timestep
      ihdiff=-iavail(8,iaforw)
      do n = 1,navail
        iavail(8,n)=iavail(8,n)+ihdiff
      end do
c
      if(idebug.eq.1) then
        write(9,*) 'FORWARD SORTED AVAILABLE    navail=',navail
        n=iaforw
        do while (n.gt.0)
          write(9,fmt='(i4,'':'',7(1x,i4),1x,i6,2i5)')
     +				n,(iavail(i,n),i=1,10)
          n=iavail(9,n)
        end do
        write(9,*) 'BACKWARD SORTED AVAILABLE    navail=',navail
        n=iaback
        do while (n.gt.0)
          write(9,fmt='(i4,'':'',7(1x,i4),1x,i6,2i5)')
     +				n,(iavail(i,n),i=1,10)
          n=iavail(10,n)
        end do
        write(9,*) '--------------------------------------------------'
        write(9,*)
      end if
c
c..time range
      do i=1,5
        itimer(i,1)=iavail(i,iaforw)
        itimer(i,2)=iavail(i,iaback)
      end do
c..get valid time (with forecast=0)
      call vtime(itimer(1,1),ierror)
      call vtime(itimer(1,2),ierror)
c
      ierror=0
c
      return
      end

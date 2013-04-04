      subroutine pselect(igrspec,idrydep,iwetdep)
c
c..select particles to be displayed (graphics) or saved in video files.
c
c..making 4 types:  1 - not precipitation, in    boundary layer
c..                 2 - not precipitation, above boundary layer
c..                 3 -     precipitation, in    boundary layer
c..                 4 -     precipitation, above boundary layer
c..          types  5,6,7,8 as 1,2,3,4 above but without enough
c..                         mass to be displayed
c
c..     type stored in the array ipwork
c
c..     mass is treated separately for the 4 classes.
c
      implicit none
c
      include 'snapdim.inc'
      include 'snappar.inc'
c
      integer igrspec(11),idrydep,iwetdep
c
      real    pgtot(4)
c
      integer igcomp,idep,n,k,m,iunit,i,ierror,minute,ltext
      real    bqlim
c
c..component or total
      igcomp=igrspec(10)
      if(igcomp.le.0 .and. ncomp.eq.1) igcomp=idefcomp(1)
      igrspec(10)=igcomp
c
      idep=0
      if(idrydep.eq.1) idep=1
      if(iwetdep.eq.1) idep=1
c
      if(igrspec(11).eq.0 .or. idep.eq.0) then
c
c..keep (all) positions
c
        do n=1,npart
          k=1
          if(pdata(8,n).gt.0.0) k=3
          if(pdata(3,n).lt.pdata(4,n)) k=k+1
          ipwork(n)=k
        end do
c
      else
c
c..keep mass
c
        do k=1,4
          pgtot(k)=0.
        end do
c
        if(igcomp.gt.0 .and. numtotal(igcomp).gt.0) then
c
c..single component
          bqlim=totalbq(igcomp)/numtotal(igcomp)
c
          do n=1,npart
	    if(icomp(n).eq.igcomp) then
              k=1
              if(pdata(8,n).gt.0.0) k=3
              if(pdata(3,n).lt.pdata(4,n)) k=k+1
              pgtot(k)=pgtot(k)+pdata(9,n)
              if(pgtot(k).ge.bqlim) then
                pgtot(k)=pgtot(k)-bqlim
                ipwork(n)=k
              else
                ipwork(n)=k+4
              end if
            end if
          end do
          if(pgtot(1)+pgtot(2)+pgtot(3)+pgtot(4).ge.bqlim
     +       .and. ipwork(npart).gt.4) ipwork(npart)=ipwork(npart)-4
c
        else
c
c..using the total mass of all components
          bqlim=0.
c..............................................................
c..the following does not take care of short release time for
c..i.e. noble gas, but how can that be done ??????
c..............................................................
          do n=1,ncomp
	    m= idefcomp(n)
            if(numtotal(m).gt.0) bqlim=bqlim+totalbq(m)/numtotal(m)
          end do
c
          do n=1,npart
            k=1
            if(pdata(8,n).gt.0.0) k=3
            if(pdata(3,n).lt.pdata(4,n)) k=k+1
            pgtot(k)=pgtot(k)+pdata(9,n)
            if(pgtot(k).gt.bqlim) then
              pgtot(k)=pgtot(k)-bqlim
              ipwork(n)=k
            else
              ipwork(n)=k+4
            end if
          end do
          if(pgtot(1)+pgtot(2)+pgtot(3)+pgtot(4).gt.bqlim
     +       .and. ipwork(npart).gt.4) ipwork(npart)=ipwork(npart)-4
c
        end if
c
      end if
c
ccc   do n=1,npart
ccc     write(6,*) 'npart,n,ipwork(n): ',npart,n,ipwork(n)
ccc   end do
c
      return
      end

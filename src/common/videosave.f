      subroutine videosave(iunit,istep,nstep,itime,isteph,nsteph,itime1,
     +                     igrspec)
c
c       store data for 'video replay'
c
      implicit none
c
      include 'snapdim.inc'
      include 'snapgrd.inc'
      include 'snapfld.inc'
      include 'snappar.inc'
c
      integer iunit,istep,nstep,isteph,nsteph
      integer itime(5),itime1(5),igrspec(11)
c
      integer lenstr
      integer i,ierror,minute,igcomp,ltext,np,n
      real    xsave,ysave
c
      integer   nsave
      integer   itimev(5),ksave(8)
      integer*2 ixysave(2,mpart)
      character*80 text
      character*64 savefile
c
      equivalence (ixysave,pwork)
c
      data nsave/0/
c
      if(nsave.eq.0) then
        do i=1,5
          itimev(i)=itime1(i)
        end do
        call vtime(itimev,ierror)
        minute=0
        itimev(5)=minute
        write(savefile,fmt='(''snap.video.'',i5.5)') nsave
        open(iunit,file=savefile,
     +             access='sequential',form='unformatted')
        write(iunit) nstep,(itimev(i),i=1,5),
     +               nx,ny,igtype,(gparam(i),i=1,6)
        igcomp=igrspec(10)
        text=' '
        if(igcomp.gt.0) then
          text=compname(igcomp)
        else
          text='Total'
        end if
        ltext=lenstr(text,2)
        write(iunit) ltext
        write(iunit) text(1:ltext)
        close(iunit)
      end if
c
      do i=1,5
        itimev(i)=itime(i)
      end do
      call vtime(itimev,ierror)
      minute=60*isteph/nsteph
      itimev(5)=minute
c
      np=0
c
      do n=1,npart
        if(ipwork(n).le.4) then
          xsave=pdata(1,n)
          ysave=pdata(2,n)
c..mark particle above boundary layer
          if(ipwork(n).eq.2 .or. ipwork(n).eq.4) xsave=-xsave
c..mark particle without precipitation
          if(ipwork(n).eq.1 .or. ipwork(n).eq.2) ysave=-ysave
          np=np+1
          ixysave(1,np)=nint(xsave*100.)
          ixysave(2,np)=nint(ysave*100.)
        end if
      end do
c
      nsave=nsave+1
      write(savefile,fmt='(''snap.video.'',i5.5)') nsave
      open(iunit,file=savefile,
     +           access='sequential',form='unformatted')
      write(iunit) istep,(itimev(i),i=1,5),np,npart,nparnum
      if(np.gt.0) write(iunit) ((ixysave(i,n),i=1,2),n=1,np)
      close(iunit)
c
      return
      end

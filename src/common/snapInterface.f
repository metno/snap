c
c***********************************************************************
c
      subroutine getmapspec(gridtype,gridspec,gridpart)
c
      implicit none
c
      include 'snapdim.inc'
      include 'snapgrd.inc'
      include 'snapgrp.inc'
c
      integer gridtype
      real gridspec(6),gridpart(4)
c
      integer i,i1,i2,j1,j2
c
      gridtype=igtype
c
      do i=1,6
	gridspec(i)=gparam(i)
      end do
c
      i1=nint(rgrspec(1)*0.01*float(nx-1))+1
      i2=nint(rgrspec(2)*0.01*float(nx-1))+1
      j1=nint(rgrspec(3)*0.01*float(ny-1))+1
      j2=nint(rgrspec(4)*0.01*float(ny-1))+1
      if(i1.lt.1 .or. i1.ge.i2 .or. i2.gt.nx .or.
     +   j1.lt.1 .or. j1.ge.j2 .or. j2.gt.ny) then
        i1=1
        i2=nx
        j1=1
        j2=ny
      end if
c
      gridpart(1)=i1
      gridpart(2)=i2
      gridpart(3)=j1
      gridpart(4)=j2
c
      return
      end
c
c***********************************************************************
c
      subroutine getdrawspec(nfields,nnx,nny,mpos,iistep,nnstep,
     +			     year,month,day,hour,minute,
     +			     numreleased,numrunning,
     +			     savePNG,saveXPM,saveBMP)
c
      implicit none
c
      include 'snapdim.inc'
      include 'snappar.inc'
      include 'snapgrp.inc'
c
      integer nfields,nnx,nny,mpos,iistep,nnstep
      integer year,month,day,hour,minute,numreleased,numrunning
      integer savePNG,saveXPM,saveBMP
c
      integer i,ierror
      integer itime(5)
c
      nfields=0
      nnx    =nx
      nny    =ny
      mpos   =npart
      iistep =istepgr
      nnstep =nstepgr
c
      do i=1,5
	itime(i)=itimegr(i)
      end do
      call vtime(itime,ierror)
      year=  itime(1)
      month= itime(2)
      day=   itime(3)
      hour=  itime(4)
      minute= 60*istephgr/nstephgr
c
      numreleased= nparnum
      numrunning=  npart
c
      savePNG=0
      saveXPM=0
      saveBMP=0
      if(savePNGstep.gt.0) then
	if(mod(istepgr,savePNGstep).eq.0) savePNG=1
      end if
      if(saveXPMstep.gt.0) then
	if(mod(istepgr,saveXPMstep).eq.0) saveXPM=1
      end if
      if(saveBMPstep.gt.0) then
	if(mod(istepgr,saveBMPstep).eq.0) saveBMP=1
      end if
c
      return
      end
c
c***********************************************************************
c
      subroutine getfield(nfield,fieldid,field)
c
      implicit none
c
      include 'snapdim.inc'
      include 'snapfld.inc'
      include 'snapgrp.inc'
      include 'snappar.inc'
c
      integer nfield,fieldid
      real    field(nx,ny)
c
      integer i,j,n
      real    rt1,rt2
c
      if (fieldid.eq.-1) then
c
        do j=1,ny
	  do i=1,nx
	    field(i,j)=0.
	  end do
        end do
c
        do n=1,ncomp
          do j=1,ny
	    do i=1,nx
	      field(i,j)= field(i,j)
     +			  + sngl( accdry(i,j,n)+accwet(i,j,n)
     +				 +depdry(i,j,n)+depwet(i,j,n))
	    end do
	  end do
        end do
c
      elseif (fieldid.eq.58) then
c
c..mslp (hPa)
c
        rt1=(tf2gr-tnowgr)/(tf2gr-tf1gr)
        rt2=(tnowgr-tf1gr)/(tf2gr-tf1gr)
c
        do j=1,ny
	  do i=1,nx
            field(i,j)= rt1*pmsl1(i,j) + rt2*pmsl2(i,j)
	  end do
        end do
c
      elseif (fieldid.eq.17) then
c
c..precip (mm/hour)
c
        do j=1,ny
	  do i=1,nx
            field(i,j)= precip(i,j,iprecip)
	  end do
        end do
c
      else
c
	write(6,*) 'ERROR getfield: fieldid= ',fieldid
c
        do j=1,ny
	  do i=1,nx
	    field(i,j)=0.
	  end do
        end do
c
      end if
c
      return
      end
c
c***********************************************************************
c
      subroutine getposis(mpos,npos,xpos,ypos,ipos)
c
      implicit none
c
      include 'snapdim.inc'
      include 'snappar.inc'
      include 'snapgrp.inc'
c
      integer mpos,npos
      real    xpos(mpos),ypos(mpos)
      integer ipos(mpos)
c
      integer npdisp,k,n,kshow(8)
c
c
c..draw points at particle positions...........................
c
      npdisp=0
c
      if(igrspec(5).eq.1 .or. igrspec(6).eq.1) then
c
c..show particles in and/or above boundary layer
c
c..ipwork computed in subr. pselect (for show and videosave)
c  ipwork(n) = 1 - no.precip, in ABL
c            = 2 - no.precip, above ABL
c            = 3 -    precip, in ABL
c            = 4 -    precip, above ABL
c            = 5,6,7,8 as 1,2,3,4 but without enough mass
c
	do k=1,8
	  kshow(k)=-1
	end do
	if(igrspec(5).eq.1) then
	  kshow(1)=0
	  kshow(3)=1
	end if
	if(igrspec(6).eq.1) then
	  kshow(2)=0
	  kshow(4)=1
	end if
c
	do n=1,npart
c
	  if(kshow(ipwork(n)).ne.-1) then
c
	    npdisp=npdisp+1
	    xpos(npdisp)=pdata(1,n)
	    ypos(npdisp)=pdata(2,n)
	    ipos(npdisp)=kshow(ipwork(n))
c
	  end if
c
	end do
c
      end if
c
      npos= npdisp
c
      return
      end
c
c***********************************************************************
c
      subroutine c2fgetarg(iarg,carg)
c
c	SET arguments from C/C++ (main) program (iarg=-1,-2,-3,...)
c	GET arguments from Fortran subroutines  (iarg=+1,+2,+3,...)
c
c
      implicit none
c
      integer iarg
      character*(*) carg
c
      integer maxargs
      parameter (maxargs=40)
c
      integer nargs,lallargs,iallargs(2,maxargs)
c
      character*512 allargs
c
      integer lenstr
      integer la,lc,k1,k2
c
      save iallargs,allargs
c
      data nargs,lallargs/0,0/
c
      if(iarg.lt.0) then
c
c..set (from C/C++)
c
	la=len(allargs)
	lc=lenstr(carg,0)
	if(lallargs+lc.le.la .and. nargs.lt.maxargs) then
	  nargs=nargs+1
	  k1=lallargs+1
	  k2=lallargs+lc
	  iallargs(1,nargs)=k1
	  iallargs(2,nargs)=k2
	  allargs(k1:k2)=carg(1:lc)
	  lallargs=k2
	end if
c
      elseif(iarg.eq.0) then
c
c..only called from iargc below !!!!!
c..return no. of arguments set
c
	iarg=nargs
c
      elseif(iarg.gt.0) then
c
c..get (from fortran subroutine)
c
	if(iarg.le.nargs) then
	  k1=iallargs(1,iarg)
	  k2=iallargs(2,iarg)
	  lc=len(carg)
	  lc=min(k2-k1+1,lc)
	  carg=' '
	  carg(1:lc)=allargs(k1:k1+lc-1)
	else
	  carg=' '
	end if
c
      end if
c
      return
      end
c
c***********************************************************************
c
      integer function c2fiargc()
c
c..only called from fortran subroutine
c
      implicit none
c
      integer nargs
      character*2 cdummy
c
      nargs=0
      call c2fgetarg(nargs,cdummy)
c
      c2fiargc=nargs
c
      return
      end
c
c***********************************************************************
c
      subroutine c2fgetvar(nvar,var,nilarg,ilarg,iprint,ierror)
c
c  PURPOSE: Search for Environment variables and Command line arguments
c           in text strings and replace them with actual values.
c           The character ~ is interpreted as $HOME.
c
c  INPUT:   nvar          - integer       - no. of character strings
c           var(nvar)     - character*(*) - the character strings
c           nilarg        - integer       - no. of illegal command line
c                                           arguments
c           ilarg(nilarg) - integer       - the illegal command line
c                                           arguments (none illegal:
c                                           nilarg=1 and ilarg(1)=-1)
c           iprint        - integer       - 0 = do not print error
c                                               messages
c                                           1 = print error messages
c
c  OUTPUT:  var(nvar) - character*(*) - the character strings
c           ierror    - integer       - 0 = no error
c                                       1 = error
c
c  DESCRIPTION:
c    The character ~ is replaced with the value of $HOME.
c    Environment variables  starts with a $ character
c    followed by the variable name.
c    Command line arguments starts with a # character
c    followed by the argument number.
c    Both types are terminated by one these characters:
c    <space> / . $ # ? , ; \
c    If \ is used, it will be removed from the text string.
c
c    One text string can contain any combinations of fixed text,
c    environment variables and command line arguments.
c    The ~ is interpreted as $HOME any place in the text string.
c
c  LIMITATIONS:
c    Maximum length of one 'variable name' and it's actual value,
c    is 256 characters.
c
c-----------------------------------------------------------------------
c  DNMI/FoU  09.10.1992  Anstein Foss
c  DNMI/FoU  04.10.1993  Anstein Foss
c  DNMI/FoU  02.09.1994  Anstein Foss .... added ~ as $HOME
c  DNMI/FoU  27.01.1998  Anstein Foss .... length of "#N" incl. spaces
c  DNMI/FoU  17.02.2001  Anstein Foss .... c2f version (C/C++ -> Fortran)
c-----------------------------------------------------------------------
c
c..input/output:
      integer       nvar,iprint,ierror,nilarg
      integer       ilarg(nilarg)
      character*(*) var(nvar)
c
c..local:
      integer       lvarx,nbeg,nend
      parameter     (lvarx=256,nbeg=3,nend=9)
      character*256 var2,var3
      character*1   cbeg(nbeg),cend(nend)
      logical       search
c
ccccc integer       iargc
      integer       c2fiargc
c
c.................1.....2.....3..
      data cbeg/ '$' , '#' , '~'/
c.................1.....2.....3.....4.....5.....6.....7.....8.....9...
cccc  data cend/ ' ' , '.' , '/' , '$' , '#' , '?' , ',' , ';' , '\' /
      data cend/ ' ' , '.' , '/' , '$' , '#' , '?' , ',' , ';' , '\\' /
c..'\\' = '\' when read by the SGI (MIPS) f77 compiler
c
      ierror=0
      narg=-1
      lenvar=len(var(1))
c
      do nv=1,nvar
c
        lvar=0
c
        search=.true.
c
        do while (search)
c
c..search for 'variable'
          ibeg=0
          k1=lenvar+1
          do n=1,nbeg
            k=index(var(nv),cbeg(n))
            if(k.gt.0 .and. k.lt.k1) then
              k1=k
              ibeg=n
            end if
          end do
c
          if(ibeg.gt.0 .and. lvar.eq.0) then
            do k=1,lenvar
              if(var(nv)(k:k).ne.' ') lvar=k
            end do
          end if
          k2=lvar
c
          if(ibeg.eq.0) then
c
            search=.false.
c
          else
c
c..search for end of 'variable' unless ~
c
            iend=0
            if(ibeg.eq.3) then
              ibeg=1
              iend=1
              k2=k1
              var2='HOME'
            else
              kv=min0(lvar-k1,lvarx)
              var2=var(nv)(k1+1:k1+kv)
              k2=lvarx
              do n=1,nend
                k=index(var2,cend(n))
                if(k.gt.0 .and. k.le.k2) then
                  k2=k
                  iend=n
                end if
              end do
              k2=k1+k2
              if(iend.eq.0 .or. k2.eq.k1+1) goto 210
c..keep all 'end' characters unless it is \
              if(iend.ne.nend) k2=k2-1
              var2=var(nv)(k1+1:k2)
            end if
	    var3=' '
            if(ibeg.eq.1) then
c..$name (environment variable)
              call getenv(var2,var3)
              if(var3(1:1).eq.' ') goto 220
            elseif(ibeg.eq.2) then
c..#n (command line argument no. n)
ccccc         if(narg.lt.0) narg=iargc()
              if(narg.lt.0) narg=c2fiargc()
              read(var2,*,err=230,end=230) iarg
              if(iarg.lt.1 .or. iarg.gt.narg) goto 230
              do i=1,nilarg
                if(iarg.eq.ilarg(i)) goto 240
              end do
ccccc         call getarg(iarg,var3)
              call c2fgetarg(iarg,var3)
            end if
c
	    kv=0
	    do k=1,lvarx
	      if(var3(k:k).ne.' ') kv=k
            end do
            km=kv-(k2-k1+1)
            kr=lvar-k2
            if(km.gt.0) then
              lvar=lvar+km
              if(lvar.gt.lenvar) goto 250
              do k=kr,1,-1
                var(nv)(k2+km+k:k2+km+k)=var(nv)(k2+k:k2+k)
              end do
            elseif(km.lt.0) then
              do k=1,kr
                var(nv)(k2+km+k:k2+km+k)=var(nv)(k2+k:k2+k)
              end do
              do k=km+1,0
                var(nv)(k2+kr+k:k2+kr+k)=' '
              end do
            end if
            var(nv)(k1:k1+kv-1)=var3(1:kv)
c
          end if
c
        end do
c
        goto 290
c
  210   ierror=ierror+1
        if(iprint.gt.0) then
          if(k2.lt.k1)     k2=k1
          if(k2.gt.lenvar) k2=lenvar
          write(6,*) '** Error in string:'
          write(6,*) '** ',var(nv)(1:lvar)
          write(6,*) '** Error found in the substring:'
          write(6,*) '** ',var(nv)(k1:k2)
          write(6,*) '**----------------------------------------*'
        end if
        goto 290
c
  220   ierror=ierror+1
        if(iprint.gt.0) then
          write(6,*) '** Undefined environment variable in string:'
          write(6,*) '** ',var(nv)(1:lvar)
          write(6,*) '** Environment variable: ',var(nv)(k1+1:k2)
          write(6,*) '**----------------------------------------*'
        end if
        goto 290
c
  230   ierror=ierror+1
        if(iprint.gt.0) then
          write(6,*) '** Not existing command line argument no.',
     +               ' in string:'
          write(6,*) '** ',var(nv)(1:lvar)
          write(6,*) '** Command line argument no: ',var(nv)(k1+1:k2)
          write(6,*) '** No. of command line arguments: ',narg
          write(6,*) '**----------------------------------------*'
        end if
        goto 290
c
  240   ierror=ierror+1
        if(iprint.gt.0) then
          write(6,*) '** Illegal command line argument no. in string:'
          write(6,*) '** ',var(nv)(1:lvar)
          write(6,*) '** Command line argument no: ',var(nv)(k1+1:k2)
          write(6,*) '** Illegal command line argument numbers:'
          write(6,*) '** ',(ilarg(i),i=1,nilarg)
          write(6,*) '**----------------------------------------*'
        end if
        goto 290
c
  250   ierror=ierror+1
        if(iprint.gt.0) then
          write(6,*) '** String will become too long:'
          write(6,*) '** ',var(nv)
          write(6,*) '** Variable:         ',var(nv)(k1+1:k2)
          write(6,*) '** The actual value: ',var3(1:kv)
          write(6,*) '**----------------------------------------*'
        end if
        goto 290
c
  290   continue
c
      end do
c
      if(ierror.ne.0) ierror=1
c
      return
      end

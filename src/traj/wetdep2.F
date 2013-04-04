      subroutine wetdep2(tstep)
c
c  Purpose:  Compute wet deposition for each particle and each component
c            and store depositions in nearest gridpoint in a field
c  Method:   J.Bartnicki 2003
c
c
      implicit none
c
      include 'snapdim.inc'
      include 'snapgrd.inc'
      include 'snapfld.inc'
      include 'snappar.inc'
      include 'snaptab.inc'
c
      real    tstep
c
      real    a0,a1,a2,b0,b1,b2,b3
c
      parameter (a0=8.4e-5)
      parameter (a1=2.7e-4)
      parameter (a2=-3.618e-6)
      parameter (b0=-0.1483)
      parameter (b1=0.3220133)
      parameter (b2=-3.0062e-2)
      parameter (b3=9.34458e-4)
c
      integer m,n,itab,i,j,mm
      real    precint,probab,prand,deprate,dep,q,rkw
      real    depconst(mdefcomp)
c
c..for random number functions
      integer irand,iseed
c
#if defined(linux)
      real rand
#else
      double precision rand
#endif
c
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      real ratdep(mdefcomp)
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      logical prepare
c################################################################
      integer numdep
      real depmin,depmax,ratmin,ratmax,premin,premax
      double precision totinp,depsum,totsum
c################################################################
c
      save depconst
c
      data prepare/.true./
c
c
      if(prepare) then
c
	do m=1,ndefcomp
	  r=radiusmym(m)
	  depconst(m)=b0 + b1*r + b2*r*r + b3*r*r*r
c################################################################
	  write(9,*) 'WETDEP2 m,r,depconst(m): ',m,r,depconst(m)
c################################################################
        end do
c
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	write(9,*) '-------------------------------------------------'
	write(9,*) 'WETDEP2 PREPARE .... q,deprate(1:ndefcomp):'
c##############################################
ccc	dep=radiusmym(1)
ccc	radiusmym(1)=1.0
c##############################################
	do n=1,200
	  q=float(n)*0.1
	  do m=1,ndefcomp	
	    if(radiusmym(m).le.1.4) then
	      rkw= a0 * q**0.79
	    elseif(radiusmym(m).le.10.0) then
	      rkw= depconst(m) * (a1*q + a2*q*q)
	    else
	      rkw= a1*q + a2*q*q
	    end if
	    deprate= 1.0 - exp(-tstep*rkw)	
	    ratdep(m)=deprate
	  end do
	  write(9,1010) q,(ratdep(m),m=1,ndefcomp)
 1010	  format(1x,f5.1,':',12f7.4)
	end do
c##############################################
ccc	radiusmym(1)=dep
c##############################################
	write(9,*) '-------------------------------------------------'
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	prepare=.false.
      end if
c
c..for random number generation
      iseed=irand()
      call srand(iseed)
c################################################################
      numdep=0
      premin=+1.e+38
      premax=-1.e+38
      ratmin=+1.e+38
      ratmax=-1.e+38
      depmin=+1.e+38
      depmax=-1.e+38
      totinp=0.0d0
      depsum=0.0d0
      totsum=0.0d0
c################################################################
c
      do n=1,npart
c################################################################
        totinp=totinp+dble(pdata(9,n))
c################################################################
	m= icomp(n)
c	if(kwetdep(m).eq.1 .and. pdata(8,n).gt.0.0) then
	if(kwetdep(m).eq.1 .and. pdata(8,n).gt.0.0
     & .and. pdata(3,n) .gt. 0.67) then
c..find particles with wet deposition and
c..reset precipitation to zero if not wet deposition
          precint=pdata(8,n)
	    q=precint
	    if(radiusmym(m).le.1.4) then
	      rkw= a0 * q**0.79
	    elseif(radiusmym(m).le.10.0) then
	      rkw= depconst(m) * (a1*q + a2*q*q)
	    else
	      rkw= a1*q + a2*q*q
	    end if
	    deprate= 1.0 - exp(-tstep*rkw)	
            dep=deprate*pdata(9,n)
            if(dep.gt.pdata(9,n)) dep=pdata(9,n)
            pdata(9,n)=pdata(9,n)-dep
	    i=nint(pdata(1,n))
	    j=nint(pdata(2,n))
	    mm=iruncomp(m)
            depwet(i,j,mm)=depwet(i,j,mm)+dble(dep)
c################################################################
	    if(premin.gt.precint) premin=precint
	    if(premax.lt.precint) premax=precint
	    if(ratmin.gt.deprate) ratmin=deprate
	    if(ratmax.lt.deprate) ratmax=deprate
	    if(depmin.gt.dep) depmin=dep
	    if(depmax.lt.dep) depmax=dep
	    depsum=depsum+dble(dep)
	    numdep=numdep+1
c################################################################
	end if
c################################################################
        totsum=totsum+dble(pdata(9,n))
c################################################################
      end do
c
c################################################################
c      write(88,*) 'WETDEP2 numdep,npart:  ',numdep,npart
c      write(88,*) 'WETDEP2 totinp:        ',totinp
c      write(88,*) 'WETDEP2 totsum,depsum: ',totsum,depsum
c      if(premin.le.premax)
c     +   write(88,*) 'WETDEP2 premin,premax: ',premin,premax
c      if(ratmin.le.ratmax)
c     +   write(88,*) 'WETDEP2 ratmin,ratmax: ',ratmin,ratmax
c      if(depmin.le.depmax)
c     +   write(88,*) 'WETDEP2 depmin,depmax: ',depmin,depmax
c      write(88,*) '---------------------------------------'
c################################################################
      return
      end

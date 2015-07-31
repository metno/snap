      subroutine drydep2(tstep)
      use particleML
c
c  Purpose:  Compute dry deposition for each particle and each component
c            and store depositions in nearest gridpoint in a field
c  Method:   J.Bartnicki 2003
c
c ... 23.04.12 - gas, particle 0.1<d<10, particle d>10 - J. Bartnicki|
c
c
#if defined(DRHOOK)
      USE PARKIND1  ,ONLY : JPIM     ,JPRB
      USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
#endif
      implicit none
#if defined(DRHOOK)
      REAL(KIND=JPRB) :: ZHOOK_HANDLE ! Stack variable i.e. do not use SAVE
#endif
c
      include 'snapdim.inc'
      include 'snapgrd.inc'
      include 'snapfld.inc'
      include 'snappar.inc'
c
      real    tstep
c
      integer m,n,i,j,mm
      real    h,deprate,dep
c################################################################
      integer numdep
      real depmin,depmax,ratmin,ratmax,hblmin,hblmax
      double precision totinp,depsum,totsum
c
#if defined(DRHOOK)
      ! Before the very first statement
      IF (LHOOK) CALL DR_HOOK('DRYDEP2',0,ZHOOK_HANDLE)
#endif
c
      numdep=0
      hblmin=+1.e+38
      hblmax=-1.e+38
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
        totinp=totinp+dble(pdata(n)%rad)
c################################################################
       m= icomp(n)
c######	if(kdrydep(m).eq.1 .and. pdata(n)%z.lt.pdata(n)%tbl) then
cjb	if(kdrydep(m).eq.1 .and. pdata(n)%z.gt.pdata(n)%tbl) then
       if(kdrydep(m).eq.1 .and. pdata(n)%z.gt. 0.996) then
cjb	  h=pdata(n)%hbl
         h=30.0
c..gravityms=pdata(n)%grv
cjb...23.04.12... difference between particle and gas
c
         if(radiusmym(m) .le. 0.05) then
           deprate= 1.0-exp(-tstep*(0.008)/h)	!gas
         endif
         if(radiusmym(m) .gt. 0.05 .and. radiusmym(m) .le. 10.0) then
           deprate= 1.0-exp(-tstep*(0.002)/h)	!particle 0.05<r<10
         endif
         if(radiusmym(m) .gt. 10.0) then
           deprate= 1.0-exp(-tstep*(0.002+pdata(n)%grv)/h)	!particle r>10
           if (pdata(n)%z .eq. vlevel(1)) deprate = 1.  ! complete deposition when particle hits ground
         endif
          dep=deprate*pdata(n)%rad
          pdata(n)%rad=pdata(n)%rad-dep
         i=nint(pdata(n)%x)
         j=nint(pdata(n)%y)
         mm=iruncomp(m)
          depdry(i,j,mm)=depdry(i,j,mm)+dble(dep)
c################################################################
         if(hblmin.gt.h) hblmin=h
         if(hblmax.lt.h) hblmax=h
         if(ratmin.gt.deprate) ratmin=deprate
         if(ratmax.lt.deprate) ratmax=deprate
         if(depmin.gt.dep) depmin=dep
         if(depmax.lt.dep) depmax=dep
         depsum=depsum+dble(dep)
         numdep=numdep+1
c################################################################
       end if
c################################################################
        totsum=totsum+dble(pdata(n)%rad)
c################################################################
      end do
c
c################################################################
c      write(88,*) 'DRYDEP2 numdep,npart:  ',numdep,npart
c      write(88,*) 'DRYDEP2 totinp:        ',totinp
c      write(88,*) 'DRYDEP2 totsum,depsum: ',totsum,depsum
c      if(hblmin.le.hblmax)
c     +   write(88,*) 'DRYDEP2 hblmin,hblmax: ',hblmin,hblmax
c      if(ratmin.le.ratmax)
c     +   write(88,*) 'DRYDEP2 ratmin,ratmax: ',ratmin,ratmax
c      if(depmin.le.depmax)
c     +   write(88,*) 'DRYDEP2 depmin,depmax: ',depmin,depmax
c      write(88,*) '--------'
c################################################################
#if defined(DRHOOK)
c     before the return statement
      IF (LHOOK) CALL DR_HOOK('DRYDEP2',1,ZHOOK_HANDLE)
#endif
      return
      end

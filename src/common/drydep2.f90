! SNAP: Servere Nuclear Accident Programme
! Copyright (C) 1992-2017   Norwegian Meteorological Institute
! 
! This file is part of SNAP. SNAP is free software: you can 
! redistribute it and/or modify it under the terms of the 
! GNU General Public License as published by the 
! Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
! 
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
! 
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https://www.gnu.org/licenses/>.
!
      subroutine drydep2(tstep,n)
        USE particleML
        USE snapfldML
        USE snapparML
        USE snapgrdML, only: vlevel
        USE snapdimML, only: nx,ny,nk
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
      real, INTENT(IN) ::    tstep
c
c particle loop index, n = 0 means init
      INTEGER, INTENT(IN) :: n
      integer m,i,j,mm
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
c     do n=1,npart // particle loop outside
c################################################################
        totinp=totinp+dble(pdata(n)%rad)
c################################################################
       m= icomp(n)
c#### 30m = surface-layer (deposition-layer); sigma(hybrid)=0.996 ~ 30m
       if(kdrydep(m).eq.1 .and. pdata(n)%z.gt. 0.996) then
         h=30.0
cjb...23.04.12... difference between particle and gas
c
         if(radiusmym(m) .le. 0.05) then
           ! gas
           deprate= 1.0-exp(-tstep*(0.008)/h)	
         else if (radiusmym(m) .le. 10.0) then
           ! particle 0.05<r<10
           deprate= 1.0-exp(-tstep*(0.002)/h)	
         else
           ! particle r>=10                                   
           deprate= 1.0-exp(-tstep*(0.002+pdata(n)%grv)/h)	
           ! complete deposition when particle hits ground
           if (pdata(n)%z .eq. vlevel(1)) deprate = 1.
         endif
         dep=deprate*pdata(n)%rad
         pdata(n)%rad=pdata(n)%rad-dep
         i=nint(pdata(n)%x)
         j=nint(pdata(n)%y)
         mm=iruncomp(m)
!$omp atomic
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
c      end do
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

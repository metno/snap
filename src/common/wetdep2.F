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
      subroutine wetdep2(tstep,np,pextra)
      use particleML
c
c  Purpose:  Compute wet deposition for each particle and each component
c            and store depositions in nearest gridpoint in a field
c  Method:   J.Bartnicki 2003
c
c ... 23.04.12 - gas, particle 0.1<d<10, particle d>10 - J. Bartnicki|
c ... 12.12.13 - gas 'particle size' changed to 0.05um - H. Klein
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
      include 'snaptab.inc'
c
      real, INTENT(IN) ::    tstep
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
c particle loop index, np = 0 means init
      INTEGER, INTENT(IN) :: np
      TYPE(extraParticle), INTENT(IN) :: pextra
      integer m,n,itab,i,j,mm
      real    precint,probab,prand,deprate,dep,q,rkw
      real    depconst(mdefcomp)
c
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      real ratdep(mdefcomp)
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c################################################################
      integer numdep
      real depmin,depmax,ratmin,ratmax,premin,premax,rm
      double precision totinp,depsum,totsum
c################################################################
c
      save depconst
c
#if defined(DRHOOK)
      ! Before the very first statement
      IF (LHOOK) CALL DR_HOOK('WETDEP2',0,ZHOOK_HANDLE)
#endif
c initalization
      if(np .eq. 0) then
c
       do m=1,ndefcomp
         rm=radiusmym(m)
         depconst(m)=b0 + b1*rm + b2*rm*rm + b3*rm*rm*rm
c################################################################
         write(9,*) 'WETDEP2 m,r,depconst(m): ',m,rm,depconst(m)
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
c
cjb... 25.04.12 wet deposition for convective and gases
c
           if(radiusmym(m).gt.0.05.and.radiusmym(m).le.1.4) then
             rkw= a0*q**0.79
           endif
           if(radiusmym(m).gt.1.4.and.radiusmym(m).le.10.0) then
             rkw= depconst(m)*(a1*q + a2*q*q)
           endif
           if(radiusmym(m).gt.10.0) then
             rkw= a1*q + a2*q*q
           endif
           if(q .gt. 7.0) then	! convective
              rkw=3.36e-4*q**0.79
           endif
           if(radiusmym(m).le. 0.05) then	! gas
              rkw=1.12e-4*q**0.79
           endif
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
c end init
#if defined(DRHOOK)
c     before the return statement
      IF (LHOOK) CALL DR_HOOK('WETDEP1',1,ZHOOK_HANDLE)
#endif
      return
      end if
c
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
c      do np=1,npart // particle loop moved out
c################################################################
        totinp=totinp+dble(pdata(np)%rad)
c################################################################
       m= icomp(np)
c	if(kwetdep(m).eq.1 .and. pextra%prc.gt.0.0) then
       if(kwetdep(m).eq.1 .and. pextra%prc.gt.0.0
     & .and. pdata(np)%z .gt. 0.67) then
c..find particles with wet deposition and
c..reset precipitation to zero if not wet deposition
          precint=pextra%prc
           q=precint
c
cjb... 25.04.12 wet deposition for convective and gases
c
           if(radiusmym(m).gt.0.05.and.radiusmym(m).le.1.4) then
             rkw= a0*q**0.79
           endif
           if(radiusmym(m).gt.1.4.and.radiusmym(m).le.10.0) then
             rkw= depconst(m)*(a1*q + a2*q*q)
           endif
           if(radiusmym(m).gt.10.0) then
             rkw= a1*q + a2*q*q
           endif
           if(q .gt. 7.0) then	! convective
              rkw=3.36e-4*q**0.79
           endif
           if(radiusmym(m).le. 0.1) then	! gas
              rkw=1.12e-4*q**0.79
           endif
           deprate= 1.0 - exp(-tstep*rkw)
            dep=deprate*pdata(np)%rad
            if(dep.gt.pdata(np)%rad) dep=pdata(np)%rad
            pdata(np)%rad=pdata(np)%rad-dep
           i=nint(pdata(np)%x)
           j=nint(pdata(np)%y)
           mm=iruncomp(m)
!$omp atomic
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
        totsum=totsum+dble(pdata(np)%rad)
c################################################################
c      end do
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
#if defined(DRHOOK)
c     before the return statement
      IF (LHOOK) CALL DR_HOOK('WETDEP1',1,ZHOOK_HANDLE)
#endif
      return
      end

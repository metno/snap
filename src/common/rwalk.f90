! SNAP: Servere Nuclear Accident Programme
! Copyright (C) 1992-2017   Norwegian Meteorological Institute

! This file is part of SNAP. SNAP is free software: you can
! redistribute it and/or modify it under the terms of the
! GNU General Public License as published by the
! Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.

! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.

! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https://www.gnu.org/licenses/>.

module rwalkML
  implicit none
  private

  public rwalk

  contains

subroutine rwalk(tstep,blfullmix,np,pextra)

!  Purpose:  Diffusion, in and above boudary layer.

!  Method:   Random walk.

!  Input:
!     tstep     - time step in seconds (trajectory calculations)
!     blfullmix - full mixing in boundarylayer (true=old,false=new)

!---------------------------------------------------------------------
!   24.04.2009 Jerzy Bartnicki: Model particle which goes below the
!   ground or above the top boundary in the random walk is reflected
!   26.03.2011 Jerzy Bartnicki: New parameterization of vertical diffusion in the
!   mixing layer. l-eta proportional to mixing height and the time step.
!   For mixing height = 2500 m and time step = 15 min:
!   In ABL: l-eta=0.28
!   Above ABL: l-eta=0.003
!   For 200< mixing height<2500 and arbitrary time step:
!   In ABL: l-eta=0.28*(mh/2500m)*(tstep/tstep-mix)
!   Above ABL: l-eta=0.003*(tstep/tstep-mix)
!   Entrainment zone = 10%*h

  USE particleML
  USE snapgrdML
  USE snapparML

#if defined(DRHOOK)
  USE PARKIND1  ,ONLY : JPIM     ,JPRB
  USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
#endif
  implicit none
#if defined(DRHOOK)
  REAL(KIND=JPRB) :: ZHOOK_HANDLE ! Stack variable i.e. do not use SAVE
#endif

  REAL, INTENT(IN) ::    tstep
  LOGICAL, INTENT(IN) :: blfullmix
! particle-loop index, np = 0 means init
  INTEGER, INTENT(IN)  :: np
  TYPE(extraParticle), INTENT(IN) :: pextra


  integer :: i
  real*8, save ::    a,cona,conb,vrange,vrdbla,vrdblb,vrqrt
  real*8, save :: hmax ! maximum mixing height = 2500m
  real*8, save :: tmix ! Characteristic mixing time = 15 min
  real*8, save :: lmax ! Maximum l-eta in the mixing layer = 0.28
  real*8, save :: labove ! Standard l-eta above the mixing layer
  real*8, save :: tfactor ! tfactor=tstep/tmix

  real*8 :: rnd(3), xrand, yrand, zrand, u, v, rl, vabs
  real*8 :: hfactor, rv, rvmax


#if defined(DRHOOK)
! Before the very first statement
  IF (LHOOK) CALL DR_HOOK('RWALK',0,ZHOOK_HANDLE)
#endif
! initialization
  if (np == 0) then
    hmax = 2500.0
    tmix = 15.0*60.0
    lmax = 0.28
    labove=0.03
    tfactor=tstep/tmix
  !	write(*,*) 'hmax, tmix,lmax, labove, tfactor',
  !     &hmax, tmix,lmax, labove, tfactor
  
    a=0.5
    conb=2.*tstep*0.5*(tstep**0.75)*(a**2)
    a=0.25
    cona=2.*tstep*0.5*(tstep**0.75)*(a**2)
  
    vrange=0.02
  !       vrdbla=vrange*0.5
  ! l-eta above mixing height
    vrdbla=labove*tfactor
  
  ! l-eta below mixing height
    vrdblb=lmax*tfactor
    vrqrt=vrange*0.25
#if defined(DRHOOK)
  !     before the return statement
    IF (LHOOK) CALL DR_HOOK('RWALK',1,ZHOOK_HANDLE)
#endif
    return
  end if

!--------------------------------------
  if (blfullmix) then
  !--------------------------------------
  
  !      do np=1,npart // moved particle loop out
  
  !..the rand function returns random real numbers between 0.0 and 1.0
  
    call random_number(rnd)
    xrand = rnd(1) - 0.5
    yrand = rnd(2) - 0.5
    zrand = rnd(3) - 0.5

  
    if(pdata(np)%z > pdata(np)%tbl) then
    
    !..particle in boundary layer.....................................
    
    !..horizontal diffusion
      u=pextra%u
      v=pextra%v
      vabs=sqrt(u*u+v*v)
      rl=sqrt(conb*vabs**1.75)*2.
      pdata(np)%x=pdata(np)%x+rl*xrand*pextra%rmx
      pdata(np)%y=pdata(np)%y+rl*yrand*pextra%rmy
    
    !..vertical diffusion
    
      pdata(np)%z=1.-(1.-pdata(np)%tbl)*1.1*(zrand+0.5)
    
    else
    
    !..particle above boundary layer..................................
    
    !..horizontal diffusion
      u=pextra%u
      v=pextra%v
      vabs=sqrt(u*u+v*v)
      rl=sqrt(cona*vabs**1.75)*2.
      pdata(np)%x=pdata(np)%x+rl*xrand*pextra%rmx
      pdata(np)%y=pdata(np)%y+rl*yrand*pextra%rmy
    
    !..vertical diffusion
      pdata(np)%z=pdata(np)%z+vrdbla*zrand
    
    end if

  !      end do
  
  !--------------------------------------
  else
  !--------------------------------------
  
  !... no full mixing
  
  !      do np=1,npart  // moved particle loop out
  !	write(*,*) np,(pdata(i,np),i=1,5)
  
  !..the rand function returns random real numbers between 0.0 and 1.0
  
    call random_number(rnd)
    xrand = rnd(1) - 0.5
    yrand = rnd(2) - 0.5
    zrand = rnd(3) - 0.5
  
    if(pdata(np)%z > pdata(np)%tbl) then
    
    !..particle in boundary layer.....................................
    
    !..horizontal diffusion
      u=pextra%u
      v=pextra%v
      vabs=sqrt(u*u+v*v)
      rl=sqrt(conb*vabs**1.75)*2.
      pdata(np)%x=pdata(np)%x+rl*xrand*pextra%rmx
      pdata(np)%y=pdata(np)%y+rl*yrand*pextra%rmy
    
    !..vertical diffusion
      hfactor=pdata(np)%hbl/hmax
      rv=lmax*hfactor*tfactor
      rvmax=1.0-pdata(np)%tbl
    !	write(*,*) 'hfactor,rv,rvmax,pdata(np)%tbl,pdata(np)%hbl',
    !     &hfactor,rv,rvmax,pdata(np)%tbl,pdata(np)%hbl
    !	if(rv .gt. rvmax) then
    !	write(*,*) 'rv,rvmax,pdata(np)%z,pdata(np)%tbl',
    !     &  rv,rvmax,pdata(np)%z,pdata(np)%tbl
    !	stop
    !	endif
    
      if(rv > rvmax) rv=rvmax
      pdata(np)%z=pdata(np)%z+rv*zrand
    
    !... reflection from the ABL top
    
      if(pdata(np)%z < pdata(np)%tbl) then
      !	  write(*,*) 'gora-przed, p4=',pdata(np)%tbl
      !	  write(*,*) 'gora-przed, p3=',pdata(np)%z
        pdata(np)%z= 2.0*pdata(np)%tbl-pdata(np)%z
      !	  write(*,*) 'gora-po   , p3=',pdata(np)%z
      endif
    
    !... reflection from the bottom
    
      if(pdata(np)%z > 1.0) then
      !	  write(*,*) 'dol-przed, p=',pdata(np)%z
        pdata(np)%z= 2.0-pdata(np)%z
      !	  write(*,*) 'dol-po   , p=',pdata(np)%z
      endif
    
    !..vertical limits
      if(pdata(np)%z > 1.0) pdata(np)%z=1.0
      if(pdata(np)%z < pdata(np)%tbl) &
      pdata(np)%z=pdata(np)%tbl
    
    else
    
    !..particle above boundary layer..................................
    
    !..horizontal diffusion
      u=pextra%u
      v=pextra%v
      vabs=sqrt(u*u+v*v)
      rl=sqrt(cona*vabs**1.75)*2.

      pdata(np)%x=pdata(np)%x+rl*xrand*pextra%rmx
      pdata(np)%y=pdata(np)%y+rl*yrand*pextra%rmy
    
    !..vertical diffusion
      pdata(np)%z=pdata(np)%z+vrdbla*zrand
    
    end if

  !      end do
  
  !--------------------------------------
  end if
!--------------------------------------

#if defined(DRHOOK)
!     before the return statement
  IF (LHOOK) CALL DR_HOOK('RWALK',1,ZHOOK_HANDLE)
#endif
  return
end subroutine rwalk
end module rwalkML

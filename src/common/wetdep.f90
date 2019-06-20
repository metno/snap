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

module wetdep
  use snapdimML, only: mdefcomp
  implicit none
  private

  public wetdep1, wetdep2

!> wet deposition rate
  real, save, public :: wetdeprat(mdefcomp)

!> for each component: 0=wet deposition off  1=wet dep. on
  integer, save, public :: kwetdep(mdefcomp)

  contains

!> Purpose:  Compute wet deposition for each particle and each component
!>           and store depositions in nearest gridpoint in a field
!>
!> Method:   J.Saltbones 1994
subroutine wetdep1(n,pextra)
  USE particleML, only: pdata, extraParticle
  USE snapfldML, only: depwet
  USE snapparML, only: icomp, iruncomp
  USE snaptabML, only: mpretab, premult, pretab

!> particle loop index, n = 0 means init
  integer, intent(in) :: n
  Type(extraParticle), intent(inout) :: pextra

  integer :: m,itab,i,j,mm
  real :: precint,probab,prand,dep

  m= icomp(n)
  if(kwetdep(m) == 1 .AND. pextra%prc > 0.0) then
  !..find particles with wet deposition and
  !..reset precipitation to zero if not wet deposition
    precint=pextra%prc
    itab=nint(precint*premult)
    itab=min(itab,mpretab)
    probab=pretab(itab)
  !..the rand function returns random real numbers between 0.0 and 1.0
    call random_number(prand)
    if(prand > probab) then
      pextra%prc=0.0
    else
      dep=wetdeprat(m)*pdata(n)%rad
      pdata(n)%rad=pdata(n)%rad-dep
      i=nint(pdata(n)%x)
      j=nint(pdata(n)%y)
      mm=iruncomp(m)
    ! omp atomic
      depwet(i,j,mm)=depwet(i,j,mm)+dble(dep)
    end if
  end if

  return
end subroutine wetdep1


!> Purpose:  Compute wet deposition for each particle and each component
!>           and store depositions in nearest gridpoint in a field
!>
!> Method:   J.Bartnicki 2003
subroutine wetdep2(tstep,np,pextra)
! ... 23.04.12 - gas, particle 0.1<d<10, particle d>10 - J. Bartnicki|
! ... 12.12.13 - gas 'particle size' changed to 0.05um - H. Klein
  USE particleML, only: pdata, extraParticle
  USE snapfldML, only: depwet
  USE snapparML, only: ncomp, icomp, iruncomp
  USE snapdimML, only: mdefcomp
  USE snapdebug, only: iulog
  USE vgravtablesML, only: radiusmym

  real, intent(in) ::    tstep
!> particle loop index, np = 0 means init
  integer, intent(in) :: np
  type(extraParticle), intent(in) :: pextra

  real, parameter :: a0=8.4e-5
  real, parameter :: a1=2.7e-4
  real, parameter :: a2=-3.618e-6
  real, parameter :: b0=-0.1483
  real, parameter :: b1=0.3220133
  real, parameter :: b2=-3.0062e-2
  real, parameter :: b3=9.34458e-4

  integer :: m,n,i,j,mm
  real :: precint,deprate,dep,q,rkw
  real, save :: depconst(mdefcomp)

  real :: ratdep(mdefcomp)
  real :: rm

! initalization
  if(np == 0) then

    do m=1,ncomp
      rm=radiusmym(m)
      depconst(m)=b0 + b1*rm + b2*rm*rm + b3*rm*rm*rm
      write(iulog,*) 'WETDEP2 m,r,depconst(m): ',m,rm,depconst(m)
    end do

    write(iulog,*) '-------------------------------------------------'
    write(iulog,*) 'WETDEP2 PREPARE .... q,deprate(1:ncomp):'

    do n=1,200
      q=float(n)*0.1
      do m=1,ncomp

      ! b... 25.04.12 wet deposition for convective and gases
        if(radiusmym(m) > 0.05 .AND. radiusmym(m) <= 1.4) then
          rkw= a0*q**0.79
        endif
        if(radiusmym(m) > 1.4 .AND. radiusmym(m) <= 10.0) then
          rkw= depconst(m)*(a1*q + a2*q*q)
        endif
        if(radiusmym(m) > 10.0) then
          rkw= a1*q + a2*q*q
        endif
        if(q > 7.0) then ! convective
          rkw=3.36e-4*q**0.79
        endif
        if(radiusmym(m) <= 0.05) then ! gas
          rkw=1.12e-4*q**0.79
        endif
        deprate= 1.0 - exp(-tstep*rkw)
        ratdep(m)=deprate
      end do
      write(iulog,1010) q,(ratdep(m),m=1,ncomp)
      1010 format(1x,f5.1,':',12f7.4)
    end do
    write(iulog,*) '-------------------------------------------------'

    return
  end if

  m= icomp(np)
  if(kwetdep(m) == 1 .AND. pextra%prc > 0.0 &
      .AND. pdata(np)%z > 0.67) then
  !..find particles with wet deposition and
  !..reset precipitation to zero if not wet deposition
    precint=pextra%prc
    q=precint

  ! b... 25.04.12 wet deposition for convective and gases
    if(radiusmym(m) > 0.05 .AND. radiusmym(m) <= 1.4) then
      rkw= a0*q**0.79
    endif
    if(radiusmym(m) > 1.4 .AND. radiusmym(m) <= 10.0) then
      rkw= depconst(m)*(a1*q + a2*q*q)
    endif
    if(radiusmym(m) > 10.0) then
      rkw= a1*q + a2*q*q
    endif
    if(q > 7.0) then ! convective
      rkw=3.36e-4*q**0.79
    endif
    if(radiusmym(m) <= 0.1) then ! gas
      rkw=1.12e-4*q**0.79
    endif
    deprate= 1.0 - exp(-tstep*rkw)
    dep=deprate*pdata(np)%rad
    if(dep > pdata(np)%rad) dep=pdata(np)%rad
    pdata(np)%rad=pdata(np)%rad-dep
    i=nint(pdata(np)%x)
    j=nint(pdata(np)%y)
    mm=iruncomp(m)
  ! omp atomic
    depwet(i,j,mm)=depwet(i,j,mm)+dble(dep)
  end if
  return
end subroutine wetdep2
end module wetdep

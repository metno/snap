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

module rmpartML
  implicit none
  private

  public rmpart

  contains

!> Remove particles which are inactive
!> or have lost (almost) all mass, in the last case the
!> remaining mass is transferred to to the other particles
!> in the same plume (or to the next plume if none left).
subroutine rmpart(rmlimit)
  USE particleML, only: pdata
  USE snapparML, only: ncomp, run_comp, totalbq, icomp, iparnum
  USE snapdimML, only: mdefcomp
  USE releaseML, only: iplume, nplume, npart, numtotal
  USE drydep, only: kdrydep
  USE wetdep, only: kwetdep
  USE decayML, only: kdecay

  real, intent(in) :: rmlimit

  integer :: idep,m,n,npl,i,i1,i2,iredist

  integer :: npkeep(mdefcomp)
  real :: pbqmin(mdefcomp), pbqtotal(mdefcomp), pbqlost(mdefcomp)
  real :: pbqdist(mdefcomp)

!..rmlimit is now input, used to be 0.02 (2%)
  idep=0
  do n=1,ncomp
    m = run_comp(n)%to_defined
    pbqmin(m)=0.
    if(kdrydep(m) == 1 .OR. kwetdep(m) == 1 &
     .OR. kdecay(m) == 1) then
      if(numtotal(m) > 0) &
      pbqmin(m)=(totalbq(m)/numtotal(m))*rmlimit
      idep=1
    end if
    pbqlost(m)=0.
  end do

  n=0

  do npl=1,nplume

    i1=iplume(1,npl)
    i2=iplume(2,npl)

  !..redistribution of lost mass (within one plume)
    if(idep == 1 .AND. i1 > 0) then
      do m=1,ncomp
        pbqtotal(m)=0.
        npkeep(m)=0
      end do
      do i=i1,i2
        m=icomp(i)
        if(pdata(i)%rad > pbqmin(m)) then
          pbqtotal(m)=pbqtotal(m)+pdata(i)%rad
          npkeep(m)=npkeep(m)+1
        else
          pbqlost(m)=pbqlost(m)+pdata(i)%rad
          pdata(i)%rad=0.
          pdata(i)%active = .FALSE.
          pdata(i)%x=0.
          pdata(i)%y=0.
        end if
      end do
      iredist=0
      do m=1,ncomp
        pbqdist(m)=0.
        if(pbqlost(m) > 0. .AND. npkeep(m) > 0) then
          pbqdist(m)=pbqlost(m)/float(npkeep(m))
          pbqlost(m)=0.
          iredist=1
        end if
      end do
      if(iredist == 1) then
        do i=i1,i2
          if(pdata(i)%rad > 0.0) then
            m=icomp(i)
            pdata(i)%rad= pdata(i)%rad+pbqdist(m)
          end if
        end do
      end if
    end if

  ! removal of particles outside of the domain
  ! by reordering of plumes
    iplume(1,npl)=n+1
    do i=i1,i2
    ! reorder all particles, only keep active
      if(pdata(i)%active) then
        n=n+1
        if(n /= i) then
        !             moving paricle to new position in pdata (n < i)
          pdata(n) = pdata(i)
          icomp(n)=  icomp(i)
          iparnum(n)=iparnum(i)
        end if
      end if
    end do

  ! updating plume-particle relation, or making plume empty
  ! (plumes are never removed!)
    iplume(2,npl)=n
    if(iplume(1,npl) > iplume(2,npl)) then
      iplume(1,npl)=0
      iplume(2,npl)=-1
    end if

  end do

! updating the used number of particles
  npart=n

!..note: if pmlost>0 we lost mass inside the grid area
!..      (no later plumes to take the mass).

  return
end subroutine rmpart
end module rmpartML

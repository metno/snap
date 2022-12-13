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
    integer, save :: has_reduction = -1
  public rmpart

  contains

!> Remove particles which are inactive
!> or have lost (almost) all mass, in the last case the
!> remaining mass is transferred to to the other particles
!> in the same plume (or to the next plume if none left).
subroutine rmpart(rmlimit)
  USE particleML, only: pdata
  USE snapparML, only: ncomp, run_comp, iparnum, def_comp
  USE releaseML, only: iplume, plume_release, nplume, npart
  use snapfldMl, only: total_activity_lost_other

  !> if particles content drop to rmlimit*initial-content
  !> the particle will be removed and it's content redistributed
  real, intent(in) :: rmlimit

  integer :: m,n,npl,i,i1,i2, mm
  logical :: redistribute

  integer, allocatable, save :: npkeep(:)
  real, allocatable, save :: pbqlost(:)
  real, allocatable, save :: pbqdist(:)

  if (.not.allocated(npkeep)) allocate(npkeep(ncomp))
  if (.not.allocated(pbqlost)) allocate(pbqlost(ncomp))
  if (.not.allocated(pbqdist)) allocate(pbqdist(ncomp))

  !..initialize
  if (has_reduction .eq. -1) then 
    has_reduction = 0
    do n=1,ncomp
      m = run_comp(n)%to_defined
      if(def_comp(m)%kdrydep == 1 .or. def_comp(m)%kwetdep == 1 &
          .or. def_comp(m)%kdecay == 1) then
      has_reduction = 1
     end if
    end do
  end if

  pbqlost = 0.

  n=0

  plume_loop: do npl=1,nplume

    i1 = iplume(npl)%start
    i2 = iplume(npl)%end

  !..redistribution of lost mass (within one plume)
    if(has_reduction == 1 .AND. i1 > 0 .and. rmlimit > 0) then
      npkeep = 0
      do i=i1,i2
        m=pdata(i)%icomp
        mm = def_comp(m)%to_running
        if (pdata(i)%is_active()) then
          if(pdata(i)%rad() <= (plume_release(npl, mm)*rmlimit)) then
            ! Reset to avoid double count
            pbqlost(mm) = pbqlost(mm) + pdata(i)%get_set_rad(0.0)
          else
            npkeep(mm) = npkeep(mm) + 1
          endif
        end if
      end do
      redistribute = .false.
      do m=1,ncomp
        pbqdist(m) = 0.0
        if(pbqlost(m) > 0. .AND. npkeep(m) > 0) then
          pbqdist(m) = pbqlost(m)/float(npkeep(m))
          pbqlost(m) = 0.0
          redistribute = .true.
        end if
      end do
      if(redistribute) then
        do i=i1,i2
          if(pdata(i)%is_active()) then
            m = pdata(i)%icomp
            call pdata(i)%add_rad(pbqdist(m))
          end if
        end do
      end if
    end if

  ! removal of particles outside of the domain
  ! by reordering of plumes
    iplume(npl)%start = n + 1
    do i=i1,i2
    ! reorder all particles, only keep active
      if(pdata(i)%is_active()) then
        n=n+1
        if(n /= i) then
        !             moving paricle to new position in pdata (n < i)
          pdata(n) = pdata(i)
          iparnum(n)=iparnum(i)
        end if
      else
        m = def_comp(pdata(i)%icomp)%to_running
        total_activity_lost_other(m) = total_activity_lost_other(m) + (-pdata(i)%rad())
      end if
    end do

  ! updating plume-particle relation, or making plume empty
  ! (plumes are never removed!)
    iplume(npl)%end = n
    if(iplume(npl)%start > iplume(npl)%end) then
      iplume(npl)%start = 0
      iplume(npl)%end = -1
    end if

  end do plume_loop

! updating the used number of particles
  npart=n

!..note: if pbqlost>0 we lost mass inside the grid area
!..      (no later plumes to take the mass).
  total_activity_lost_other(:) = total_activity_lost_other + pbqlost

end subroutine rmpart

end module rmpartML

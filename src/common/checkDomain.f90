! SNAP: Servere Nuclear Accident Programme
! Copyright (C) 1992-2026   Norwegian Meteorological Institute
! License: GNU General Public License v3.0 or later

module checkDomainML
  implicit none
  private

  public :: check_in_domain

  contains

!> Purpose:
!>   check if particle is inside domain (set active = .false. if outside)
!>   move particle to lowest (highest) level if below lowest (highest) level
subroutine check_in_domain(part, out_of_domain)
  USE snapdimML, only: nx,ny,nk
  use snapgrdML, only: vlevel
  use particleML, only: Particle

  type(Particle), intent(inout) :: part
  logical, intent(out) :: out_of_domain
  real :: vmin, vmax

  out_of_domain = .false.

  ! Cell centers are at integers; valid range is [0.5, nx+0.5).
  ! Require x in [1, nx) and y in [1, ny) so bilinear interpolation
  ! always has both a left and right (lower and upper) neighbour.
  if (part%x < 1 .OR. part%y < 1 .OR. &
      part%x >= nx .OR. part%y >= ny) then
    out_of_domain = .true.
  else
    vmin = vlevel(nk)
    vmax = vlevel( 1)
    if (part%z > vmax) then
      part%z = vmax
    else if (part%z < vmin) then
      part%z = vmin
    end if
  end if
end subroutine
end module checkDomainML

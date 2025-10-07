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

module compheightML
  implicit none
  private

  public compheight

  contains


!> Purpose:  Compute height of model levels and thickness of model layers
!>
!> Notes:
!>   - sigma levels (norlam) or eta levels (hirlam,...)
!>     defined by alevel and blevel
!>   - lower model level is level 2
subroutine compheight()
  USE snapgrdML, only: alevel, blevel
  USE snapfldML, only: ps2, hlayer2, hlevel2, t2_abs
  USE snapfldML, only: hlayer => field3d1
  USE snaptabML, only: g, exner
  USE snapdimML, only: nx,ny,nk,hres_field
  USE ftestML, only: ftest

  real, parameter :: r=287

  integer :: k

  real :: pe(nx, ny)
  real :: pe2(nx, ny)

!..compute height of model levels (in the model grid)
  hlayer(:,:,nk) = 9999.0
  hlevel2(:,:,1) = 0.0

  ! Calculate thickness of levels in metres
  do k = 2, nk
    pe = alevel(k-1) + blevel(k-1) * ps2
    pe2 = alevel(k) + blevel(k) * ps2
    hlayer(:, :, k-1) = (r * t2_abs(:, :, k) / g) * log(pe/pe2)
  end do

  ! Calculate cumulative height of levels in metres
  do k = 2, nk
    hlevel2(:, :, k) = hlevel2(:, :, k-1) + hlayer(:, :, k-1)
  end do

  call ftest('hlayer', hlayer, contains_undef=.true., reverse_third_dim=.true.)
  call ftest('hlevel', hlevel2, contains_undef=.true., reverse_third_dim=.true.)
  do k=1,nk
    call hres_field(hlayer(:,:,k), hlayer2(:,:,k))
  end do
end subroutine compheight
end module compheightML

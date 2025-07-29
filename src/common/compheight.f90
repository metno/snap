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
  USE snapgrdML, only: ahalf, bhalf, alevel, blevel
  USE snapfldML, only: ps2, hlayer2, hlevel2, t2
  USE snapfldML, only: hlayer => field3d1
  USE snaptabML, only: g, exner
  USE snapdimML, only: nx,ny,nk,hres_field
  USE ftestML, only: ftest

  real, parameter :: ginv = 1.0/g

  integer :: i,j,k
  real :: p,pih,pif,h1,h2
  real :: pihl(nx,ny),hlev(nx,ny)

!..compute height of model levels (in the model grid)
  hlev(:,:) = 0.0
  hlayer(:,:,nk) = 9999.0
  hlevel2(:,:,1) = 0.0

  pihl(:,:) = exner(ps2)

  do k=2,nk
    do j=1,ny
      do i=1,nx
        p = ahalf(k) + bhalf(k)*ps2(i,j)
        pih = exner(p)

        p = alevel(k) + blevel(k)*ps2(i,j)
        pif = exner(p)

        h1 = hlev(i,j)
        h2 = h1 + t2(i,j,k)*(pihl(i,j)-pih)*ginv

        hlayer(i,j,k-1) = h2-h1
        hlevel2(i,j,k) = h1 + (h2-h1)*(pihl(i,j)-pif) &
            /(pihl(i,j)-pih)

        hlev(i,j) = h2
        pihl(i,j) = pih
      end do
    end do
  end do

  call ftest('hlayer', hlayer, contains_undef=.true., reverse_third_dim=.true.)
  call ftest('hlevel', hlevel2, contains_undef=.true., reverse_third_dim=.true.)
  do k=1,nk
    call hres_field(hlayer(:,:,k), hlayer2(:,:,k))
  end do
end subroutine compheight
end module compheightML

! SNAP: Servere Nuclear Accident Programme
! Copyright (C) 1992-2026   Norwegian Meteorological Institute
! License: GNU General Public License v3.0 or later

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
  USE snapfldML, only: ps_io, hlayer_io, hlevel_io, t_io
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
  hlayer_io(:,:,nk) = 9999.0
  hlevel_io(:,:,1) = 0.0

  pihl(:,:) = exner(ps_io)

  do k=2,nk
    do j=1,ny
      do i=1,nx
        p = ahalf(k) + bhalf(k)*ps_io(i,j)
        pih = exner(p)

        p = alevel(k) + blevel(k)*ps_io(i,j)
        pif = exner(p)

        h1 = hlev(i,j)
        h2 = h1 + t_io(i,j,k)*(pihl(i,j)-pih)*ginv

        hlayer_io(i,j,k-1) = h2-h1
        hlevel_io(i,j,k) = h1 + (h2-h1)*(pihl(i,j)-pif) &
            /(pihl(i,j)-pih)

        hlev(i,j) = h2
        pihl(i,j) = pih
      end do
    end do
  end do

  call ftest('hlayer', hlayer, contains_undef=.true., reverse_third_dim=.true.)
  call ftest('hlevel', hlevel_io, contains_undef=.true., reverse_third_dim=.true.)
  do k=1,nk
    call hres_field(hlayer(:,:,k), hlayer_io(:,:,k))
  end do
end subroutine compheight
end module compheightML

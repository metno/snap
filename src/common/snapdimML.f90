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

!> all parameter statements (model dimensions etc.)
module snapdimML
  implicit none
  private

!> default size of ::nx (when input.gridsize not given)
  integer, parameter :: nxpre = 864
!> default size of ::ny (when input.gridsize not given)
  integer, parameter :: nypre = 698
!> default size of ::nk (when input.levels not given)
  integer, parameter :: nkpre = 61

!> horizontal field dimensions (for computations)
  integer, save, public :: nx = nxpre
!> horizontal field dimensions (for computations)
  integer, save, public :: ny = nypre
!> number of levels
  integer, save, public :: nk = nkpre

!> max. input field size (possibly larger than nx*ny)
  integer, save, public :: maxsiz
!> length of buffer for field input/output
  integer, save, public :: ldata

!> allow output grids to have finer resolution than input by this factor
  integer, save, public :: output_resolution_factor = 1

!> max no. of components used in one run
!>
!> (keep as small as "possible", it dimensions 2d/3d output fields)
  integer, parameter, public :: mcomp=51

!> max. no. of available timesteps with data
  integer, parameter, public :: mavail = 8192

!> translate a field in normal resolution to high output resolution
  public :: hres_field

  contains

!> translate a field in normal resolution to high output resolution
  subroutine hres_field(field, field_hres, bilinear)
    USE iso_fortran_env, only: real32
    real(kind=real32), intent(in) :: field(:,:)
    real(kind=real32), intent(inout) :: field_hres(:,:)
    logical, optional, intent(in) :: bilinear
    logical :: is_bilinear
    integer :: i, j, k, l, or_2
    real dd, dx, dy, c1, c2, c3, c4

    is_bilinear = .false.
    if (present(bilinear)) is_bilinear = bilinear
    if (output_resolution_factor == 1) is_bilinear = .false.

    ! nearest neighbor, even if bilinear, to fix the borders
    do j = 1, ny
      do l = 1, output_resolution_factor
        do i = 1, nx
          do k = 1, output_resolution_factor
            field_hres(output_resolution_factor*(i-1)+k, output_resolution_factor*(j-1)+l) = field(i,j)
          end do
        end do
      end do
    end do

    if (is_bilinear) then
      or_2 = output_resolution_factor - int(output_resolution_factor / 2.)
      dd = 1./output_resolution_factor
      do j = 1, ny-1
        do l = 1, output_resolution_factor
          do i = 1, nx-1
            do k = 1, output_resolution_factor
              dx=(k-1)*dd
              dy=(l-1)*dd
              c1=(1.-dy)*(1.-dx)
              c2=(1.-dy)*dx
              c3=dy*(1.-dx)
              c4=dy*dx
              field_hres(or_2+output_resolution_factor*(i-1)+k, or_2+output_resolution_factor*(j-1)+l) = &
                c1 * field(i, j)   + c2 * field(i+1, j) + &
                c3 * field(i, j+1) + c4 * field(i+1, j+1)
            end do
          end do
        end do
      end do
    end if
  end subroutine hres_field

end module snapdimML

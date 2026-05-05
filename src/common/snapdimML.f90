! SNAP: Servere Nuclear Accident Programme
! Copyright (C) 1992-2026   Norwegian Meteorological Institute
! License: GNU General Public License v3.0 or later

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
!> Index of the surface level of the meteorology
  integer, save, public :: surface_index = -1

!> max. input field size (possibly larger than nx*ny)
  integer, save, public :: maxsiz
!> length of buffer for field input/output
  integer, save, public :: ldata

!> allow output grids to have finer resolution than input by this factor
  integer, save, public :: output_resolution_factor = 1

!> max no. of components used in one run
!>   use only for simple arrays, for large arrays, use ncomp (dynamic allocation)
  integer, parameter, public :: mcomp=1024

!> max. no. of available timesteps with data
  integer, parameter, public :: mavail = 8192

!> translate a field in normal resolution to high output resolution
  public :: hres_field
!> translate a x or y position in the input-grid to the
!> high_resolution output grid position
  public :: hres_pos
!> translate a x or y position in the output-grid to the
!> low_resolution input grid position
  public :: lres_pos

!> translate a field in normal resolution to high output resolution
  interface hres_field
    module procedure :: hres_field_real32, hres_field_int8, hres_field_real64_real32
  end interface

  contains

!> translate a field in normal resolution to high output resolution
  subroutine hres_field_real32(field, field_hres, bilinear)
    USE iso_fortran_env, only: real32
    real(kind=real32), intent(in) :: field(:,:)
    real(kind=real32), intent(inout) :: field_hres(:,:)
    logical, optional, intent(in) :: bilinear
    logical :: is_bilinear
    integer :: i, j, k, l, or_2, i0, j0, k_rel, l_rel
    real dd, dx, dy, c1, c2, c3, c4

    is_bilinear = .false.
    if (present(bilinear)) is_bilinear = bilinear
    if (output_resolution_factor == 1) is_bilinear = .false.
    or_2 = int(output_resolution_factor / 2.)

    ! nearest neighbor, even if bilinear, to fix the borders
    do j = 1, ny
      do l_rel = -or_2, output_resolution_factor-or_2-1
        l = l_rel + or_2 + 1
        do i = 1, nx
          do k_rel = -or_2, output_resolution_factor-or_2-1
            k = k_rel + or_2 + 1
            field_hres(output_resolution_factor*(i-1)+k, output_resolution_factor*(j-1)+l) = field(i,j)
          end do
        end do
      end do
    end do

    if (is_bilinear) then
      dd = 1./output_resolution_factor
      do j = 1, ny-1
        do l_rel = -or_2, output_resolution_factor-or_2-1
          if (l_rel < 0) then
            if (j == 1) cycle
            j0 = j - 1
          else
            j0 = j
          end if
          l = l_rel + or_2 + 1
          do i = 1, nx-1
            do k_rel = -or_2, output_resolution_factor-or_2-1
              if (k_rel < 0) then
                if (i == 1) cycle
                i0 = i - 1
              else
                i0 = i
              end if
              k = k_rel + or_2 + 1
              dx = (k_rel + 0.5_real32) * dd
              dy = (l_rel + 0.5_real32) * dd
              c1 = (1._real32 - dy) * (1._real32 - dx)
              c2 = (1._real32 - dy) * dx
              c3 = dy * (1._real32 - dx)
              c4 = dy * dx
              field_hres(output_resolution_factor*(i-1)+k, output_resolution_factor*(j-1)+l) = &
                c1 * field(i0, j0) + c2 * field(i0+1, j0) + &
                c3 * field(i0, j0+1) + c4 * field(i0+1, j0+1)
            end do
          end do
        end do
      end do
    end if
  end subroutine

!> translate a field in normal resolution to high output resolution
  subroutine hres_field_real64_real32(field, field_hres, bilinear)
    USE iso_fortran_env, only: real32, real64
    real(kind=real64), intent(in) :: field(:,:)
    real(kind=real32), intent(inout) :: field_hres(:,:)
    logical, optional, intent(in) :: bilinear
    logical :: is_bilinear
    integer :: i, j, k, l, or_2, i0, j0, k_rel, l_rel
    real dd, dx, dy, c1, c2, c3, c4

    is_bilinear = .false.
    if (present(bilinear)) is_bilinear = bilinear
    if (output_resolution_factor == 1) is_bilinear = .false.
    or_2 = int(output_resolution_factor / 2.)

    ! nearest neighbor, even if bilinear, to fix the borders
    do j = 1, ny
      do l_rel = -or_2, output_resolution_factor-or_2-1
        l = l_rel + or_2 + 1
        do i = 1, nx
          do k_rel = -or_2, output_resolution_factor-or_2-1
            k = k_rel + or_2 + 1
            field_hres(output_resolution_factor*(i-1)+k, output_resolution_factor*(j-1)+l) = field(i,j)
          end do
        end do
      end do
    end do

    if (is_bilinear) then
      dd = 1./output_resolution_factor
      do j = 1, ny-1
        do l_rel = -or_2, output_resolution_factor-or_2-1
          if (l_rel < 0) then
            if (j == 1) cycle
            j0 = j - 1
          else
            j0 = j
          end if
          l = l_rel + or_2 + 1
          do i = 1, nx-1
            do k_rel = -or_2, output_resolution_factor-or_2-1
              if (k_rel < 0) then
                if (i == 1) cycle
                i0 = i - 1
              else
                i0 = i
              end if
              k = k_rel + or_2 + 1
              dx = (k_rel + 0.5_real32) * dd
              dy = (l_rel + 0.5_real32) * dd
              c1 = (1._real32 - dy) * (1._real32 - dx)
              c2 = (1._real32 - dy) * dx
              c3 = dy * (1._real32 - dx)
              c4 = dy * dx
              field_hres(output_resolution_factor*(i-1)+k, output_resolution_factor*(j-1)+l) = &
                c1 * field(i0, j0) + c2 * field(i0+1, j0) + &
                c3 * field(i0, j0+1) + c4 * field(i0+1, j0+1)
            end do
          end do
        end do
      end do
    end if
  end subroutine

  subroutine hres_field_int8(field, field_hres)
    USE iso_fortran_env, only: int8
    integer(kind=int8), intent(in) :: field(:,:)
    integer(kind=int8), allocatable, intent(out) :: field_hres(:,:)

    integer :: newshape(2)

    integer :: i, j, k, l, or_2, k_rel, l_rel

    newshape(:) = shape(field)*output_resolution_factor
    allocate(field_hres(newshape(1), newshape(2)))
    or_2 = int(output_resolution_factor / 2.)

    do j = 1, ny
      do l_rel = -or_2, output_resolution_factor-or_2-1
        l = l_rel + or_2 + 1
        do i = 1, nx
          do k_rel = -or_2, output_resolution_factor-or_2-1
            k = k_rel + or_2 + 1
            field_hres(output_resolution_factor*(i-1)+k, output_resolution_factor*(j-1)+l) = field(i,j)
          end do
        end do
      end do
    end do
  end subroutine

!> translate a x or y position in the input-grid to the
!> high_resolution output grid position
  pure integer function hres_pos(lres_pos)
    USE iso_fortran_env, only: real64
    real(kind=real64), intent(in) :: lres_pos
    ! Low- and high-resolution cells are centered on integer positions,
    ! so cell 1 spans [0.5, 1.5[ on both grids.
    hres_pos = int((lres_pos - 0.5_real64) * output_resolution_factor) + 1
  end function hres_pos

!> translate a x or y position in the output-grid to the
!> low_resolution input grid position
  pure integer function lres_pos(hres_pos)
    integer, intent(in) :: hres_pos
    ! Map the high-resolution cell center back to the containing
    ! low-resolution cell using the same half-integer cell edges.
    lres_pos = int((hres_pos - 0.5)/output_resolution_factor) + 1
  end function lres_pos


end module snapdimML

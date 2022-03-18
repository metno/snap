module milibML
  implicit none
  public

  interface
    subroutine xyconvert(npos, x, y, igtypa, ga, igtypr, gr, ierror)
      integer, intent(in) :: npos
      real, intent(inout) :: x(npos), y(npos)
      integer, intent(in) :: igtypa, igtypr
      real, intent(in) :: ga(6), gr(6)
      integer, intent(out) :: ierror
    end subroutine xyconvert

    subroutine gridpar(icall, ldata, idata, igtype, nx, ny, grid, ierror)
      USE iso_fortran_env, only: int16
      integer, intent(in) :: icall, ldata
      integer, intent(out) :: igtype, nx, ny, ierror
      integer(int16), intent(inout) :: idata(ldata)
      real, intent(out) :: grid(6)
    end subroutine gridpar

    subroutine mapfield(imapr, icori, igtype, grid, nx, ny, xm, ym, fc, hx, hy, ierror)
      integer, intent(in) :: imapr, icori, igtype, nx, ny
      integer, intent(out) :: ierror
      real, intent(in) :: grid(6)
      real, intent(out), dimension(nx, ny) :: xm, ym, fc
      real, intent(out) :: hx, hy
    end subroutine mapfield

    subroutine rmfile(filnam, iprint, ierror)
      character*(*) :: filnam
      integer, intent(in) :: iprint
      integer, intent(out) :: ierror
    end subroutine rmfile
  end interface
end module milibML

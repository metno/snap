module milibML
  implicit none
  public

  interface
    function lenstr(text, minlen)
      character*(*), intent(in) :: text
      integer, intent(in) :: minlen
      integer :: lenstr
    end function lenstr

    subroutine xyconvert(npos, x, y, igtypa, ga, igtypr, gr, ierror)
      integer, intent(in) :: npos
      real, intent(inout) :: x(npos), y(npos)
      integer, intent(in) :: igtypa, igtypr
      real, intent(in) :: ga(6), gr(6)
      integer, intent(out) :: ierror
    end subroutine xyconvert

    subroutine keywrd(ntext, text, cend, csep, mkey, kwhere, nkey, ierror)
      integer, intent(in) :: ntext, mkey
      integer, intent(out) :: nkey, ierror
      character*(*), intent(in) :: text(ntext)
      character(len=1) :: cend, csep
      integer, intent(out) :: kwhere(5, mkey)
    end subroutine keywrd

    subroutine chcase(mode, ntext, text)
      integer, intent(in) :: mode
      integer, intent(in) :: ntext
      character*(*), intent(inout) :: text(ntext)
    end subroutine chcase

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

    subroutine rlunit(lrunit)
      integer, intent(out) :: lrunit
    end subroutine rlunit

    subroutine termchar(chr)
      character(len=1), intent(out) :: chr
    end subroutine termchar

    subroutine getvar(nvar, var, nilarg, ilarg, iprint, ierror)
      integer, intent(in) :: nvar, nilarg, iprint
      integer, intent(out) :: ierror
      integer, intent(in) :: ilarg(nilarg)
      character*(*), intent(inout) :: var(nvar)
    end subroutine getvar

    subroutine hrdiff(iup1, iup2, itime1, itime2, ihours, ierr1, ierr2)
      integer, intent(in) :: iup1, iup2
      integer, intent(inout) :: itime1(5), itime2(5), ierr1, ierr2
      integer, intent(out) :: ihours
    end subroutine hrdiff

    subroutine prhelp(iunit, chelp)
      integer, intent(in) :: iunit
      character*(*), intent(in) :: chelp
    end subroutine prhelp

    subroutine rmfile(filnam, iprint, ierror)
      character*(*) :: filnam
      integer, intent(in) :: iprint
      integer, intent(out) :: ierror
    end subroutine rmfile

    subroutine vtime(itime, ierror)
      integer, intent(inout) :: itime(5)
      integer, intent(out) :: ierror
    end subroutine vtime
  end interface
end module milibML

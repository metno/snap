module input
  USE iso_fortran_env, only: error_unit
  implicit none
  private

  public read_input

  integer, parameter :: MAX_LINE_LEN = 256

  contains

  subroutine read_input(filename, stat)
    character(len=*), intent(in) :: filename
    integer, intent(out) :: stat

    integer :: iuinp
    logical :: still_content
    character(len=MAX_LINE_LEN) :: line
    character(len=MAX_LINE_LEN) :: key, val
    logical :: has_val

    stat = 0
    line = ''

    open(newunit=iuinp, file=filename, access='sequential', form='formatted', &
         status='old', iostat=stat, action="READ")
    if (stat /= 0) then
      write(error_unit,*) 'Could not open ', filename
      return
    endif

    still_content = .true.

    do while (still_content)
      read(iuinp,iostat=stat,fmt='(A)') line
      if (stat < 0) then
        stat = 0
        still_content = .false.
        cycle
      endif

      if (stat /= 0) return

      line = adjustl(line)
      if (line(1:1) == '*') then
        cycle ! Comment
      endif

      call split_on_equal(line, key, val, has_val)

      if (has_val) then
        write(*,*) 'KEY: ', trim(key), ' VALUE: ', trim(val)
      else
       !write(*,*) 'KEY: ', trim(key)
      endif
    end do

    close(iuinp)

  end subroutine read_input

  subroutine split_on_equal(line, key, val, has_val)
    character(len=*), intent(in) :: line
    character(len=*), intent(out) :: key, val
    logical, intent(out) :: has_val

    character(len=len_trim(line)) :: copy_key
    integer :: equal_pos

    has_val = .false.
    key = ''
    val = ''

    equal_pos = index(line, "=")
    if (equal_pos == 0) then
      key = trim(line)
      copy_key = key
      call lower_case(copy_key, key)
      return
    endif

    has_val = .true.
    key = trim(line(1:equal_pos-1))
    copy_key = key
    call lower_case(copy_key, key)

    val = trim(adjustl(line(equal_pos+1:)))
  end subroutine split_on_equal

  subroutine lower_case(line, outline)
    character(len=*), intent(in) :: line
    character(len=*), intent(out) :: outline

    character(len=26), parameter :: lower_alphabet = "abcdefghijklmnopqrstuvwxyz"
    character(len=26), parameter :: upper_alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    integer :: i, ind

    outline = line

    do i=1,len_trim(line)
      ind = index(upper_alphabet, line(i:i))
      if (ind > 0) then
        outline(i:i) = lower_alphabet(ind:ind)
      endif
    enddo
  end subroutine lower_case
end module input

program test
  use input, only: read_input

  character(len=256) :: filename
  integer :: stat
  integer :: nargs

  nargs =  command_argument_count()
  if (nargs /= 1) then
    error stop "Could not determine argument in"
  endif

  call get_command_argument(1, filename)

  call read_input(filename, stat)
  if (stat /= 0) then
    error stop stat
  endif

end program test

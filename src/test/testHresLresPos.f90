program testHresLresPos
  use snapdimML, only: output_resolution_factor, hres_pos, lres_pos
  use iso_fortran_env, only: real64
  implicit none

  call test_factor_one_identity()
  call test_hres_pos_factor_five()
  call test_hres_pos_factor_ten()
  call test_lres_pos_factor_five()
  call test_even_factor_behavior()
  call test_containment_roundtrip()

  print *, "PASS: hres_pos/lres_pos tests"

contains

  subroutine assert_int_eq(got, expected, msg)
    integer, intent(in) :: got, expected
    character(len=*), intent(in) :: msg
    if (got /= expected) then
      print *, "FAIL:", trim(msg), " got", got, "expected", expected
      stop 1
    end if
  end subroutine assert_int_eq

  subroutine test_factor_one_identity()
    output_resolution_factor = 1

    call assert_int_eq(hres_pos(0.51_real64), 1, "hres_pos factor=1 at 0.51")
    call assert_int_eq(hres_pos(1.49_real64), 1, "hres_pos factor=1 at 1.49")
    call assert_int_eq(hres_pos(1.51_real64), 2, "hres_pos factor=1 at 1.51")
    call assert_int_eq(hres_pos(2.49_real64), 2, "hres_pos factor=1 at 2.49")

    call assert_int_eq(lres_pos(1), 1, "lres_pos factor=1 at 1")
    call assert_int_eq(lres_pos(2), 2, "lres_pos factor=1 at 2")
    call assert_int_eq(lres_pos(10), 10, "lres_pos factor=1 at 10")
  end subroutine test_factor_one_identity

  subroutine test_hres_pos_factor_five()
    output_resolution_factor = 5

    call assert_int_eq(hres_pos(0.54_real64), 1, "hres_pos factor=5 at 0.54")
    call assert_int_eq(hres_pos(0.74_real64), 2, "hres_pos factor=5 at 0.74")
    call assert_int_eq(hres_pos(0.94_real64), 3, "hres_pos factor=5 at 0.94")
    call assert_int_eq(hres_pos(1.14_real64), 4, "hres_pos factor=5 at 1.14")
    call assert_int_eq(hres_pos(1.34_real64), 5, "hres_pos factor=5 at 1.34")
    call assert_int_eq(hres_pos(1.54_real64), 6, "hres_pos factor=5 at 1.54")
  end subroutine test_hres_pos_factor_five

  subroutine test_hres_pos_factor_ten()
    output_resolution_factor = 10

    call assert_int_eq(hres_pos(0.54_real64), 1, "hres_pos factor=10 at 0.54")
    call assert_int_eq(hres_pos(0.66_real64), 2, "hres_pos factor=10 at 0.66")
    call assert_int_eq(hres_pos(1.04_real64), 6, "hres_pos factor=10 at 1.04")
    call assert_int_eq(hres_pos(1.44_real64), 10, "hres_pos factor=10 at 1.44")
    call assert_int_eq(hres_pos(1.56_real64), 11, "hres_pos factor=10 at 1.56")
    call assert_int_eq(hres_pos(2.04_real64), 16, "hres_pos factor=10 at 2.04")
  end subroutine test_hres_pos_factor_ten

  subroutine test_lres_pos_factor_five()
    integer :: h

    output_resolution_factor = 5

    do h = 1, 5
      call assert_int_eq(lres_pos(h), 1, "lres_pos factor=5 bin 1")
    end do
    do h = 6, 10
      call assert_int_eq(lres_pos(h), 2, "lres_pos factor=5 bin 2")
    end do
    do h = 11, 15
      call assert_int_eq(lres_pos(h), 3, "lres_pos factor=5 bin 3")
    end do
  end subroutine test_lres_pos_factor_five

  subroutine test_even_factor_behavior()
    output_resolution_factor = 4

    call assert_int_eq(hres_pos(0.56_real64), 1, "hres_pos factor=4 at 0.56")
    call assert_int_eq(hres_pos(0.81_real64), 2, "hres_pos factor=4 at 0.81")
    call assert_int_eq(hres_pos(1.06_real64), 3, "hres_pos factor=4 at 1.06")
    call assert_int_eq(hres_pos(1.31_real64), 4, "hres_pos factor=4 at 1.31")
    call assert_int_eq(hres_pos(1.56_real64), 5, "hres_pos factor=4 at 1.56")

    call assert_int_eq(lres_pos(1), 1, "lres_pos factor=4 at 1")
    call assert_int_eq(lres_pos(4), 1, "lres_pos factor=4 at 4")
    call assert_int_eq(lres_pos(5), 2, "lres_pos factor=4 at 5")
    call assert_int_eq(lres_pos(8), 2, "lres_pos factor=4 at 8")
  end subroutine test_even_factor_behavior

  subroutine test_containment_roundtrip()
    integer :: f, k, h, lr
    real(kind=real64) :: x

    do f = 2, 6
      output_resolution_factor = f
      do k = 1, 4
        x = real(k, kind=real64) + 0.13_real64
        h = hres_pos(x)
        lr = lres_pos(h)

        ! lres_pos(hres_pos(x)) returns the containing low-res cell index.
        call assert_int_eq(lr, k, "containment roundtrip low->high->low")
      end do
    end do
  end subroutine test_containment_roundtrip

end program testHresLresPos

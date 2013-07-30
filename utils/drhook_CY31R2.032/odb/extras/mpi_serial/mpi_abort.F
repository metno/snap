      subroutine mpi_abort(icomm,iret,ierr)
      INTEGER, parameter :: SIGABRT = 6 ! Hardcoded
      CALL ec_raise(SIGABRT)
      ierr=0
      return
      end

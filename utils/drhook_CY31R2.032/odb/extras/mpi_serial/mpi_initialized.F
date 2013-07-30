      subroutine mpi_initialized(ldflag,ierr)
      logical ldflag
      integer ierr
      ldflag=.FALSE. ! doesn't matter, 1 proc anyway since mpi_serial
      ierr=0
      return
      end

!-- the following dummies that are never called, but cause linking to fail with GNU/g95
!   and produce complaints with Intel ifort.
!   origin of these "externals" is coming from generic & dummy odb/scripts/mpif.h

      FUNCTION MPI_WTIME()
      DOUBLE PRECISION MPI_WTIME
      MPI_WTIME = 1d-10
      END
      FUNCTION MPI_WTICK()
      DOUBLE PRECISION MPI_WTICK
      MPI_WTICK = 1
      END
      FUNCTION PMPI_WTIME()
      DOUBLE PRECISION PMPI_WTIME
      PMPI_WTIME = 1d-10
      END
      FUNCTION PMPI_WTICK()
      DOUBLE PRECISION PMPI_WTICK
      PMPI_WTICK = 1
      END
      SUBROUTINE MPI_NULL_COPY_FN
      END
      SUBROUTINE MPI_NULL_DELETE_FN
      END
      SUBROUTINE MPI_DUP_FN
      END

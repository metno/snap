      subroutine mpi_comm_create(MPI_COMM_WORLD,IGROUP,KCOMM,IER)
!      call abor1('mpi_comm_create should never be called')
      KCOMM = MPI_COMM_WORLD
      IER=0
      return
      end

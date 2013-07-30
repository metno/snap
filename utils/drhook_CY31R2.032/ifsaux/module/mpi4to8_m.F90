Module MPI4TO8_M

Use PARKIND1, Only : JPIM, JPRM, JPRB
USE MPL_MPIF

Implicit None

Private :: STATUS8
  Integer(Kind=8), Dimension(MPI_STATUS_SIZE) :: STATUS8

Interface MPI_ALLGATHERV8
  Module Procedure MPI_ALLGATHERV8_R4, MPI_ALLGATHERV8_R8, &
                   MPI_ALLGATHERV8_I4, MPI_ALLGATHERV8_I1
End Interface MPI_ALLGATHERV8

Interface MPI_ALLREDUCE8
  Module Procedure MPI_ALLREDUCE8_R4, MPI_ALLREDUCE8_R8, &
                   MPI_ALLREDUCE8_I4
End Interface MPI_ALLREDUCE8

Interface MPI_ALLTOALLV8
  Module Procedure MPI_ALLTOALLV8_R8, MPI_ALLTOALLV8_I4
End Interface MPI_ALLTOALLV8

Interface MPI_BCAST8
  Module Procedure MPI_BCAST8_R4, MPI_BCAST8_R8, &
                   MPI_BCAST8_I4, MPI_BCAST8_I1, &
                   MPI_BCAST8_CH
End Interface MPI_BCAST8

Interface MPI_BSEND8
  Module Procedure MPI_BSEND8_R4, MPI_BSEND8_R8, &
                   MPI_BSEND8_R42, MPI_BSEND8_R82, &
                   MPI_BSEND8_I4, MPI_BSEND8_I1, &
                   MPI_BSEND8_CH, MPI_BSEND8_I42, &
                   MPI_BSEND8_R4_1, MPI_BSEND8_R8_1
End Interface MPI_BSEND8

Interface MPI_ISEND8
  Module Procedure MPI_ISEND8_R4, MPI_ISEND8_R8, &
                   MPI_ISEND8_R42, MPI_ISEND8_R82, &
                   MPI_ISEND8_I4, MPI_ISEND8_I1, &
                   MPI_ISEND8_CH, MPI_ISEND8_I42, &
                   MPI_ISEND8_R4_1, MPI_ISEND8_R8_1
End Interface MPI_ISEND8

Interface MPI_SEND8
  Module Procedure MPI_SEND8_R4, MPI_SEND8_R8, &
                   MPI_SEND8_R42, MPI_SEND8_R82, &
                   MPI_SEND8_I4, MPI_SEND8_I1, &
                   MPI_SEND8_CH, MPI_SEND8_I42, &
                   MPI_SEND8_R4_1, MPI_SEND8_R8_1
End Interface MPI_SEND8

Interface MPI_FILE_IREAD_SHARED8
  Module Procedure MPI_FILE_IREAD_SHARED8_R8, MPI_FILE_IREAD_SHARED8_I4
End Interface MPI_FILE_IREAD_SHARED8

Interface MPI_FILE_IWRITE_SHARED8
  Module Procedure MPI_FILE_IWRITE_SHARED8_R8, MPI_FILE_IWRITE_SHARED8_I4
End Interface MPI_FILE_IWRITE_SHARED8

Interface MPI_FILE_READ_ORDERED8
  Module Procedure MPI_FILE_READ_ORDERED8_R8, MPI_FILE_READ_ORDERED8_I4
End Interface MPI_FILE_READ_ORDERED8

Interface MPI_FILE_READ_ORDERED_BEGIN8
  Module Procedure MPI_FREAD_ORDERED_BEGIN8_R8, MPI_FREAD_ORDERED_BEGIN8_I4
End Interface MPI_FILE_READ_ORDERED_BEGIN8

Interface MPI_FILE_READ_ORDERED_END8
  Module Procedure MPI_FREAD_ORDERED_END8_R8, MPI_FREAD_ORDERED_END8_I4
End Interface MPI_FILE_READ_ORDERED_END8

Interface MPI_FILE_READ_SHARED8
  Module Procedure MPI_FILE_READ_SHARED8_R8, MPI_FILE_READ_SHARED8_I4
End Interface MPI_FILE_READ_SHARED8

Interface MPI_FILE_WRITE_ORDERED8
  Module Procedure MPI_FILE_WRITE_ORDERED8_R8, MPI_FILE_WRITE_ORDERED8_I4
End Interface MPI_FILE_WRITE_ORDERED8

Interface MPI_FILE_WRITE_ORDERED_BEGIN8
  Module Procedure MPI_FWRITE_ORDERED_BEGIN8_R8, MPI_FWRITE_ORDERED_BEGIN8_I4
End Interface MPI_FILE_WRITE_ORDERED_BEGIN8

Interface MPI_FILE_WRITE_ORDERED_END8
  Module Procedure MPI_FWRITE_ORDERED_END8_R8, MPI_FWRITE_ORDERED_END8_I4
End Interface MPI_FILE_WRITE_ORDERED_END8

Interface MPI_FILE_WRITE_SHARED8
  Module Procedure MPI_FILE_WRITE_SHARED8_R8, MPI_FILE_WRITE_SHARED8_I4
End Interface MPI_FILE_WRITE_SHARED8

Interface MPI_GATHER8
  Module Procedure MPI_GATHER8_I1, MPI_GATHER8_R8_1
End Interface MPI_GATHER8

Interface MPI_GATHERV8
  Module Procedure MPI_GATHERV8_R4, MPI_GATHERV8_R8, &
                   MPI_GATHERV8_R4S, MPI_GATHERV8_R8S, &
                   MPI_GATHERV8_I4, MPI_GATHERV8_I1, &
                   MPI_GATHERV8_I4S, MPI_GATHERV8_I4S_1, &
                   MPI_GATHERV8_R8_1, MPI_GATHERV8_R8S_1
End Interface MPI_GATHERV8

Interface MPI_RECV8
  Module Procedure MPI_RECV8_R4, MPI_RECV8_R8, &
                   MPI_RECV8_R42, MPI_RECV8_R82, &
                   MPI_RECV8_I4, MPI_RECV8_I1, &
                   MPI_RECV8_R4_1, MPI_RECV8_R8_1, &
                   MPI_RECV8_CH, MPI_RECV8_I42
End Interface MPI_RECV8

Interface MPI_IRECV8
  Module Procedure MPI_IRECV8_R4, MPI_IRECV8_R8, &
                   MPI_IRECV8_R42, MPI_IRECV8_R82, &
                   MPI_IRECV8_I4, MPI_IRECV8_I1, &
                   MPI_IRECV8_I42, &
                   MPI_IRECV8_R4_1, MPI_IRECV8_R8_1, &
                   MPI_IRECV8_CH
End Interface MPI_IRECV8

Interface MPI_SCATTERV8
  Module Procedure MPI_SCATTERV8_R8, MPI_SCATTERV8_I4, &
                   MPI_SCATTERV8_R8S, MPI_SCATTERV8_I4S
End Interface MPI_SCATTERV8

Public

Contains

! =========================================================
! ---------------------------------------------------------
Subroutine MPI_ALLREDUCE8_R4(SENDDATA, RECVDATA, COUNT, DATATYPE, OP, &
                             COMM, IERROR)

  Real(Kind=JPRM), Dimension(:), Intent(IN) :: &
    SENDDATA(:)
  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, OP, COMM
  Real(Kind=JPRM), Dimension(:), Intent(OUT) :: &
    RECVDATA(:)
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Real(Kind=8), Dimension(:), Allocatable :: &
    SENDDATA8, RECVDATA8
  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, OP8, COMM8, IERROR8

  Allocate(SENDDATA8(SIZE(SENDDATA)))
  Allocate(RECVDATA8(SIZE(RECVDATA)))

  SENDDATA8 = SENDDATA
  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  OP8 = OP
  COMM8 = COMM

  Call MPI_ALLREDUCE(SENDDATA8, RECVDATA8, COUNT8, DATATYPE8, OP8, COMM8, IERROR8)

  RECVDATA = RECVDATA8
  IERROR = IERROR8

  Deallocate(SENDDATA8)
  Deallocate(RECVDATA8)

End Subroutine MPI_ALLREDUCE8_R4

! ---------------------------------------------------------
Subroutine MPI_ALLREDUCE8_R8(SENDDATA, RECVDATA, COUNT, DATATYPE, OP, &
                             COMM, IERROR)

  Real(Kind=JPRB), Dimension(:), Intent(IN) :: &
    SENDDATA(:)
  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, OP, COMM
  Real(Kind=JPRB), Dimension(:), Intent(OUT) :: &
    RECVDATA(:)
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, OP8, COMM8, IERROR8

  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  OP8 = OP
  COMM8 = COMM

  Call MPI_ALLREDUCE(SENDDATA, RECVDATA, COUNT8, DATATYPE8, OP8, COMM8, IERROR8)

  IERROR = IERROR8

End Subroutine MPI_ALLREDUCE8_R8

! ---------------------------------------------------------
Subroutine MPI_ALLREDUCE8_I4(SENDDATA, RECVDATA, COUNT, DATATYPE, OP, &
                              COMM, IERROR)

  Integer(Kind=JPIM), Dimension(:), Intent(IN) :: &
    SENDDATA(:)
  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, OP, COMM
  Integer(Kind=JPIM), Dimension(:), Intent(OUT) :: &
    RECVDATA(:)
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Integer(Kind=8), Dimension(:), Allocatable :: &
    SENDDATA8, RECVDATA8
  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, OP8, COMM8, IERROR8

  Allocate(SENDDATA8(SIZE(SENDDATA)))
  Allocate(RECVDATA8(SIZE(RECVDATA)))

  SENDDATA8 = SENDDATA
  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  OP8 = OP
  COMM8 = COMM

  Call MPI_ALLREDUCE(SENDDATA8, RECVDATA8, COUNT8, DATATYPE8, OP8, COMM8, IERROR8)

  RECVDATA = RECVDATA8
  IERROR = IERROR8

  Deallocate(SENDDATA8)
  Deallocate(RECVDATA8)

End Subroutine MPI_ALLREDUCE8_I4

! =========================================================
! ---------------------------------------------------------
Subroutine MPI_ALLGATHERV8_R4(SENDAREA, SENDCOUNT, SENDTYPE, RECVAREA, &
                              RECVCOUNTS, DISPLS, RECVTYPE, COMM, IERROR)

  Real(Kind=JPRM), Dimension(:), Intent(IN) :: &
    SENDAREA
  Integer(Kind=JPIM), Intent(IN) :: &
    SENDCOUNT, SENDTYPE, RECVTYPE, COMM
  Real(Kind=JPRM), Dimension(:), Intent(OUT) :: &
    RECVAREA
  Integer(Kind=JPIM), Dimension(:), Intent(IN) :: &
    RECVCOUNTS, DISPLS
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Real(Kind=8), Dimension(:), Allocatable :: &
    SENDAREA8, RECVAREA8
  Integer(Kind=8), Dimension(:), Allocatable :: &
    RECVCOUNTS8, DISPLS8
  Integer(Kind=8) :: &
    SENDCOUNT8, SENDTYPE8, RECVTYPE8, COMM8, IERROR8

  Allocate(SENDAREA8(SIZE(SENDAREA)))
  Allocate(RECVAREA8(SIZE(RECVAREA)))
  Allocate(RECVCOUNTS8(SIZE(RECVCOUNTS)))
  Allocate(DISPLS8(SIZE(DISPLS)))

  SENDAREA8 = SENDAREA
  SENDCOUNT8 = SENDCOUNT
  SENDTYPE8 = SENDTYPE
  RECVCOUNTS8 = RECVCOUNTS
  DISPLS8 = DISPLS
  RECVTYPE8 = RECVTYPE
  COMM8 = COMM

  Call MPI_ALLGATHERV(SENDAREA8, SENDCOUNT8, SENDTYPE8, RECVAREA8, &
                      RECVCOUNTS8, DISPLS8, RECVTYPE8, COMM8, IERROR8)

  RECVAREA = RECVAREA8
  IERROR = IERROR8

  Deallocate(SENDAREA8)
  Deallocate(RECVAREA8)
  Deallocate(RECVCOUNTS8)
  Deallocate(DISPLS8)

End Subroutine MPI_ALLGATHERV8_R4

! ---------------------------------------------------------
Subroutine MPI_ALLGATHERV8_R8(SENDAREA, SENDCOUNT, SENDTYPE, RECVAREA, &
                              RECVCOUNTS, DISPLS, RECVTYPE, COMM, IERROR)

  Real(Kind=JPRB), Dimension(:), Intent(IN) :: &
    SENDAREA
  Integer(Kind=JPIM), Intent(IN) :: &
    SENDCOUNT, SENDTYPE, RECVTYPE, COMM
  Real(Kind=JPRB), Dimension(:), Intent(OUT) :: &
    RECVAREA
  Integer(Kind=JPIM), Dimension(:), Intent(IN) :: &
    RECVCOUNTS, DISPLS
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Integer(Kind=8), Dimension(:), Allocatable :: &
    RECVCOUNTS8, DISPLS8
  Integer(Kind=8) :: &
    SENDCOUNT8, SENDTYPE8, RECVTYPE8, COMM8, IERROR8

  Allocate(RECVCOUNTS8(SIZE(RECVCOUNTS)))
  Allocate(DISPLS8(SIZE(DISPLS)))

  SENDCOUNT8 = SENDCOUNT
  SENDTYPE8 = SENDTYPE
  RECVCOUNTS8 = RECVCOUNTS
  DISPLS8 = DISPLS
  RECVTYPE8 = RECVTYPE
  COMM8 = COMM

  Call MPI_ALLGATHERV(SENDAREA, SENDCOUNT8, SENDTYPE8, RECVAREA, &
                      RECVCOUNTS8, DISPLS8, RECVTYPE8, COMM8, IERROR8)

  IERROR = IERROR8

  Deallocate(RECVCOUNTS8)
  Deallocate(DISPLS8)

End Subroutine MPI_ALLGATHERV8_R8

! ---------------------------------------------------------
Subroutine MPI_ALLGATHERV8_I4(SENDAREA, SENDCOUNT, SENDTYPE, RECVAREA, &
                               RECVCOUNTS, DISPLS, RECVTYPE, COMM, IERROR)

  Integer(Kind=JPIM), Dimension(:), Intent(IN) :: &
    SENDAREA, RECVCOUNTS, DISPLS
  Integer(Kind=JPIM), Intent(IN) :: &
    SENDCOUNT, SENDTYPE, RECVTYPE, COMM
  Integer(Kind=JPIM), Dimension(:), Intent(OUT) :: &
    RECVAREA
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Integer(Kind=8), Dimension(:), Allocatable :: &
    SENDAREA8, RECVAREA8, RECVCOUNTS8, DISPLS8
  Integer(Kind=8) :: &
    SENDCOUNT8, SENDTYPE8, RECVTYPE8, COMM8, IERROR8

  Allocate(SENDAREA8(SIZE(SENDAREA)))
  Allocate(RECVAREA8(SIZE(RECVAREA)))
  Allocate(RECVCOUNTS8(SIZE(RECVCOUNTS)))
  Allocate(DISPLS8(SIZE(DISPLS)))

  SENDAREA8 = SENDAREA
  SENDCOUNT8 = SENDCOUNT
  SENDTYPE8 = SENDTYPE
  RECVCOUNTS8 = RECVCOUNTS
  DISPLS8 = DISPLS
  RECVTYPE8 = RECVTYPE
  COMM8 = COMM

  Call MPI_ALLGATHERV(SENDAREA8, SENDCOUNT8, SENDTYPE8, RECVAREA8, &
                      RECVCOUNTS8, DISPLS8, RECVTYPE8, COMM8, IERROR8)

  RECVAREA = RECVAREA8
  IERROR = IERROR8

  Deallocate(SENDAREA8)
  Deallocate(RECVAREA8)
  Deallocate(RECVCOUNTS8)
  Deallocate(DISPLS8)

End Subroutine MPI_ALLGATHERV8_I4

! ---------------------------------------------------------
Subroutine MPI_ALLGATHERV8_I1(SENDAREA, SENDCOUNT, SENDTYPE, RECVAREA, &
                               RECVCOUNTS, DISPLS, RECVTYPE, COMM, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    SENDAREA
  Integer(Kind=JPIM), Intent(IN) :: &
    SENDCOUNT, SENDTYPE, RECVTYPE, COMM
  Integer(Kind=JPIM), Dimension(:), Intent(IN) :: &
    RECVCOUNTS, DISPLS
  Integer(Kind=JPIM), Dimension(:), Intent(OUT) :: &
    RECVAREA
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Integer(Kind=8), Dimension(:), Allocatable :: &
    RECVAREA8, RECVCOUNTS8, DISPLS8
  Integer(Kind=8) :: &
    SENDAREA8, SENDCOUNT8, SENDTYPE8, RECVTYPE8, COMM8, IERROR8

  Allocate(RECVAREA8(SIZE(RECVAREA)))
  Allocate(RECVCOUNTS8(SIZE(RECVCOUNTS)))
  Allocate(DISPLS8(SIZE(DISPLS)))

  SENDAREA8 = SENDAREA
  SENDCOUNT8 = SENDCOUNT
  SENDTYPE8 = SENDTYPE
  RECVCOUNTS8 = RECVCOUNTS
  DISPLS8 = DISPLS
  RECVTYPE8 = RECVTYPE
  COMM8 = COMM

  Call MPI_ALLGATHERV(SENDAREA8, SENDCOUNT8, SENDTYPE8, RECVAREA8, &
                      RECVCOUNTS8, DISPLS8, RECVTYPE8, COMM8, IERROR8)

  RECVAREA = RECVAREA8
  IERROR = IERROR8

  Deallocate(RECVAREA8)
  Deallocate(RECVCOUNTS8)
  Deallocate(DISPLS8)

End Subroutine MPI_ALLGATHERV8_I1

! =========================================================
! ---------------------------------------------------------
Subroutine MPI_ALLTOALLV8_R8(SENDAREA, SENDCOUNTS, SDISPLS, SENDTYPE, &
                             RECVAREA, RECVCOUNTS, RDISPLS, RECVTYPE, &
                             COMM, IERROR)

  Real(Kind=JPRB), Dimension(:), Intent(IN) :: &
    SENDAREA
  Integer(Kind=JPIM), Dimension(:), Intent(IN) :: &
    SENDCOUNTS, SDISPLS, RECVCOUNTS, RDISPLS
  Integer(Kind=JPIM), Intent(IN) :: &
    SENDTYPE, RECVTYPE, COMM
  Real(Kind=JPRB), Dimension(:), Intent(OUT) :: &
    RECVAREA
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Integer(Kind=8), Dimension(:), Allocatable :: &
    SENDCOUNTS8, SDISPLS8, RECVCOUNTS8, RDISPLS8
  Integer(Kind=8) :: &
    SENDTYPE8, RECVTYPE8, COMM8, IERROR8

  Allocate(SENDCOUNTS8(SIZE(SENDCOUNTS)))
  Allocate(SDISPLS8(SIZE(SDISPLS)))
  Allocate(RECVCOUNTS8(SIZE(RECVCOUNTS)))
  Allocate(RDISPLS8(SIZE(RDISPLS)))

  SENDCOUNTS8 = SENDCOUNTS
  SDISPLS8 = SDISPLS
  SENDTYPE8 = SENDTYPE
  RECVCOUNTS8 = RECVCOUNTS
  RDISPLS8 = RDISPLS
  RECVTYPE8 = RECVTYPE
  COMM8 = COMM

  Call MPI_ALLTOALLV(SENDAREA, SENDCOUNTS8, SDISPLS8, SENDTYPE8, RECVAREA, &
                     RECVCOUNTS8, RDISPLS8, RECVTYPE8, COMM8, IERROR8)

  IERROR = IERROR8

  Deallocate(SENDCOUNTS8)
  Deallocate(SDISPLS8)
  Deallocate(RECVCOUNTS8)
  Deallocate(RDISPLS8)

End Subroutine MPI_ALLTOALLV8_R8

! ---------------------------------------------------------
Subroutine MPI_ALLTOALLV8_I4(SENDAREA, SENDCOUNTS, SDISPLS, SENDTYPE, &
                              RECVAREA, RECVCOUNTS, RDISPLS, RECVTYPE, &
                              COMM, IERROR)

  Integer(Kind=JPIM), Dimension(:), Intent(IN) :: &
    SENDAREA, SENDCOUNTS, SDISPLS, RECVCOUNTS, RDISPLS
  Integer(Kind=JPIM), Intent(IN) :: &
    SENDTYPE, RECVTYPE, COMM
  Integer(Kind=JPIM), Dimension(:), Intent(OUT) :: &
    RECVAREA
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Integer(Kind=8), Dimension(:), Allocatable :: &
    SENDAREA8, SENDCOUNTS8, SDISPLS8, RECVAREA8, RECVCOUNTS8, RDISPLS8
  Integer(Kind=8) :: &
    SENDTYPE8, RECVTYPE8, COMM8, IERROR8

  Allocate(SENDAREA8(SIZE(SENDAREA)))
  Allocate(SENDCOUNTS8(SIZE(SENDCOUNTS)))
  Allocate(SDISPLS8(SIZE(SDISPLS)))
  Allocate(RECVAREA8(SIZE(RECVAREA)))
  Allocate(RECVCOUNTS8(SIZE(RECVCOUNTS)))
  Allocate(RDISPLS8(SIZE(RDISPLS)))

  SENDAREA8 = SENDAREA
  SENDCOUNTS8 = SENDCOUNTS
  SDISPLS8 = SDISPLS
  SENDTYPE8 = SENDTYPE
  RECVCOUNTS8 = RECVCOUNTS
  RDISPLS8 = RDISPLS
  RECVTYPE8 = RECVTYPE
  COMM8 = COMM

  Call MPI_ALLTOALLV(SENDAREA8, SENDCOUNTS8, SDISPLS8, SENDTYPE8, RECVAREA8, &
                     RECVCOUNTS8, RDISPLS8, RECVTYPE8, COMM8, IERROR8)

  RECVAREA = RECVAREA8
  IERROR = IERROR8

  Deallocate(SENDAREA8)
  Deallocate(SENDCOUNTS8)
  Deallocate(SDISPLS8)
  Deallocate(RECVAREA8)
  Deallocate(RECVCOUNTS8)
  Deallocate(RDISPLS8)

End Subroutine MPI_ALLTOALLV8_I4

! =========================================================
! ---------------------------------------------------------
Subroutine MPI_BCAST8_R4(DATA, COUNT, DATATYPE, ROOT, COMM, IERROR)

  Real(Kind=JPRM), Dimension(:), Intent(INOUT) :: &
    DATA
  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, ROOT, COMM
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Real(Kind=8), Dimension(:), Allocatable :: &
    DATA8
  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, ROOT8, COMM8, IERROR8

  Allocate(DATA8(SIZE(DATA)))

  DATA8 = DATA
  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  ROOT8 = ROOT
  COMM8 = COMM

  Call MPI_BCAST(DATA8, COUNT8, DATATYPE8, ROOT8, COMM8, IERROR8)

  DATA = DATA8
  IERROR = IERROR8

  Deallocate(DATA8)

End Subroutine MPI_BCAST8_R4

! ---------------------------------------------------------
Subroutine MPI_BCAST8_R8(DATA, COUNT, DATATYPE, ROOT, COMM, IERROR)

  Real(Kind=JPRB), Dimension(:), Intent(INOUT) :: &
    DATA
  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, ROOT, COMM
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, ROOT8, COMM8, IERROR8

  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  ROOT8 = ROOT
  COMM8 = COMM

  Call MPI_BCAST(DATA, COUNT8, DATATYPE8, ROOT8, COMM8, IERROR8)

  IERROR = IERROR8

End Subroutine MPI_BCAST8_R8

! ---------------------------------------------------------
Subroutine MPI_BCAST8_I4(DATA, COUNT, DATATYPE, ROOT, COMM, IERROR)

  Integer(Kind=JPIM), Dimension(:), Intent(INOUT) :: &
    DATA
  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, ROOT, COMM
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Integer(Kind=8), Dimension(:), Allocatable :: &
    DATA8
  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, ROOT8, COMM8, IERROR8

  Allocate(DATA8(SIZE(DATA)))

  DATA8 = DATA
  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  ROOT8 = ROOT
  COMM8 = COMM

  Call MPI_BCAST(DATA8, COUNT8, DATATYPE8, ROOT8, COMM8, IERROR8)

  DATA = DATA8
  IERROR = IERROR8

  Deallocate(DATA8)

End Subroutine MPI_BCAST8_I4

! ---------------------------------------------------------
Subroutine MPI_BCAST8_I1(DATA, COUNT, DATATYPE, ROOT, COMM, IERROR)

  Integer(Kind=JPIM), Intent(INOUT) :: &
    DATA
  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, ROOT, COMM
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Integer(Kind=8) :: &
    DATA8, COUNT8, DATATYPE8, ROOT8, COMM8, IERROR8

  DATA8 = DATA
  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  ROOT8 = ROOT
  COMM8 = COMM

  Call MPI_BCAST(DATA8, COUNT8, DATATYPE8, ROOT8, COMM8, IERROR8)

  DATA = DATA8
  IERROR = IERROR8

End Subroutine MPI_BCAST8_I1

! ---------------------------------------------------------
Subroutine MPI_BCAST8_CH(DATA, COUNT, DATATYPE, ROOT, COMM, IERROR)

  Character(Len=*), Intent(INOUT) :: &
    DATA
  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, ROOT, COMM
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, ROOT8, COMM8, IERROR8

  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  ROOT8 = ROOT
  COMM8 = COMM

  Call MPI_BCAST(DATA, COUNT8, DATATYPE8, ROOT8, COMM8, IERROR8)

  IERROR = IERROR8

End Subroutine MPI_BCAST8_CH

! =========================================================
! ---------------------------------------------------------
Subroutine MPI_BSEND8_R4(DATA, COUNT, DATATYPE, DEST, TAG, COMM, IERROR)

  Real(Kind=JPRM), Dimension(:), Intent(IN) :: &
    DATA
  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, DEST, TAG, COMM
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Real(Kind=8), Dimension(:), Allocatable :: &
    DATA8
  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, DEST8, TAG8, COMM8, IERROR8

  Allocate(DATA8(SIZE(DATA)))

  DATA8 = DATA
  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  DEST8 = DEST
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_BSEND(DATA8, COUNT8, DATATYPE8, DEST8, TAG8, COMM8, IERROR8)

  IERROR = IERROR8

  Deallocate(DATA8)

End Subroutine MPI_BSEND8_R4

! =========================================================
! ---------------------------------------------------------
Subroutine MPI_BSEND8_R8(DATA, COUNT, DATATYPE, DEST, TAG, COMM, IERROR)

  Real(Kind=JPRB), Dimension(:), Intent(IN) :: &
    DATA
  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, DEST, TAG, COMM
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, DEST8, TAG8, COMM8, IERROR8

  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  DEST8 = DEST
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_BSEND(DATA, COUNT8, DATATYPE8, DEST8, TAG8, COMM8, IERROR8)

  IERROR = IERROR8

End Subroutine MPI_BSEND8_R8

! ---------------------------------------------------------
Subroutine MPI_BSEND8_R42(DATA, COUNT, DATATYPE, DEST, TAG, COMM, IERROR)

  Real(Kind=JPRM), Dimension(:,:), Intent(IN) :: &
    DATA
  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, DEST, TAG, COMM
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Real(Kind=8), Dimension(:,:), Allocatable :: &
    DATA8
  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, DEST8, TAG8, COMM8, IERROR8

  Allocate(DATA8(SIZE(DATA,Dim=1),SIZE(DATA,Dim=2)))

  DATA8 = DATA
  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  DEST8 = DEST
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_BSEND(DATA8, COUNT8, DATATYPE8, DEST8, TAG8, COMM8, IERROR8)

  IERROR = IERROR8

  Deallocate(DATA8)

End Subroutine MPI_BSEND8_R42

! =========================================================
Subroutine MPI_BSEND8_I42(DATA, COUNT, DATATYPE, DEST, TAG, COMM, IERROR)

  Integer(Kind=JPRM), Dimension(:,:), Intent(IN) :: &
    DATA
  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, DEST, TAG, COMM
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Integer(Kind=8), Dimension(:,:), Allocatable :: &
    DATA8
  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, DEST8, TAG8, COMM8, IERROR8

  Allocate(DATA8(SIZE(DATA,Dim=1),SIZE(DATA,Dim=2)))

  DATA8 = DATA
  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  DEST8 = DEST
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_BSEND(DATA8, COUNT8, DATATYPE8, DEST8, TAG8, COMM8, IERROR8)

  IERROR = IERROR8

  Deallocate(DATA8)

End Subroutine MPI_BSEND8_I42

! =========================================================
! ---------------------------------------------------------
Subroutine MPI_BSEND8_R82(DATA, COUNT, DATATYPE, DEST, TAG, COMM, IERROR)

  Real(Kind=JPRB), Dimension(:,:), Intent(IN) :: &
    DATA
  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, DEST, TAG, COMM
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, DEST8, TAG8, COMM8, IERROR8

  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  DEST8 = DEST
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_BSEND(DATA, COUNT8, DATATYPE8, DEST8, TAG8, COMM8, IERROR8)

  IERROR = IERROR8

End Subroutine MPI_BSEND8_R82

! =========================================================
! ---------------------------------------------------------
Subroutine MPI_BSEND8_I4(DATA, COUNT, DATATYPE, DEST, TAG, COMM, IERROR)

  Integer(Kind=JPIM), Dimension(:), Intent(IN) :: &
    DATA
  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, DEST, TAG, COMM
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Integer(Kind=8), Dimension(:), Allocatable :: &
    DATA8
  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, DEST8, TAG8, COMM8, IERROR8

  Allocate(DATA8(SIZE(DATA)))

  DATA8 = DATA
  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  DEST8 = DEST
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_BSEND(DATA8, COUNT8, DATATYPE8, DEST8, TAG8, COMM8, IERROR8)

  IERROR = IERROR8

  Deallocate(DATA8)

End Subroutine MPI_BSEND8_I4

! =========================================================
! ---------------------------------------------------------
Subroutine MPI_BSEND8_I1(DATA, COUNT, DATATYPE, DEST, TAG, COMM, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    DATA
  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, DEST, TAG, COMM
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Integer(Kind=8) :: &
    DATA8, COUNT8, DATATYPE8, DEST8, TAG8, COMM8, IERROR8

  DATA8 = DATA
  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  DEST8 = DEST
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_BSEND(DATA8, COUNT8, DATATYPE8, DEST8, TAG8, COMM8, IERROR8)

  IERROR = IERROR8

End Subroutine MPI_BSEND8_I1

! =========================================================
Subroutine MPI_BSEND8_R4_1(DATA, COUNT, DATATYPE, DEST, TAG, COMM, IERROR)

  Real(Kind=JPRM), Intent(IN) :: &
    DATA
  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, DEST, TAG, COMM
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Integer(Kind=8) :: DATA8
  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, DEST8, TAG8, COMM8, IERROR8

  DATA8 = DATA
  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  DEST8 = DEST
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_BSEND(DATA8, COUNT8, DATATYPE8, DEST8, TAG8, COMM8, IERROR8)

  IERROR = IERROR8

End Subroutine MPI_BSEND8_R4_1

! =========================================================
Subroutine MPI_BSEND8_R8_1(DATA, COUNT, DATATYPE, DEST, TAG, COMM, IERROR)

  Real(Kind=JPRB), Intent(IN) :: &
    DATA
  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, DEST, TAG, COMM
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Integer(Kind=8) :: DATA8
  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, DEST8, TAG8, COMM8, IERROR8

  DATA8 = DATA
  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  DEST8 = DEST
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_BSEND(DATA8, COUNT8, DATATYPE8, DEST8, TAG8, COMM8, IERROR8)

  IERROR = IERROR8

End Subroutine MPI_BSEND8_R8_1

! =========================================================
! ---------------------------------------------------------
Subroutine MPI_BSEND8_CH(DATA, COUNT, DATATYPE, DEST, TAG, COMM, IERROR)

  Character(Len=*), Intent(IN) :: &
    DATA
  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, DEST, TAG, COMM
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, DEST8, TAG8, COMM8, IERROR8

  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  DEST8 = DEST
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_BSEND(DATA, COUNT8, DATATYPE8, DEST8, TAG8, COMM8, IERROR8)

  IERROR = IERROR8

End Subroutine MPI_BSEND8_CH

! =========================================================
! ---------------------------------------------------------
Subroutine MPI_ISEND8_R4(DATA, COUNT, DATATYPE, DEST, TAG, COMM, REQUEST, IERROR)

  Real(Kind=JPRM), Dimension(:), Intent(IN) :: &
    DATA
  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, DEST, TAG, COMM
  Integer(Kind=JPIM), Intent(OUT) :: &
    REQUEST, IERROR

  Real(Kind=8), Dimension(:), Allocatable :: &
    DATA8
  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, DEST8, TAG8, COMM8, REQUEST8, IERROR8

  Allocate(DATA8(SIZE(DATA)))

  DATA8 = DATA
  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  DEST8 = DEST
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_ISEND(DATA8, COUNT8, DATATYPE8, DEST8, TAG8, COMM8, REQUEST8, IERROR8)

  REQUEST = REQUEST8
  IERROR = IERROR8

  Deallocate(DATA8)

End Subroutine MPI_ISEND8_R4

! =========================================================
! ---------------------------------------------------------
Subroutine MPI_ISEND8_R8(DATA, COUNT, DATATYPE, DEST, TAG, COMM, REQUEST, IERROR)

  Real(Kind=JPRB), Dimension(:), Intent(IN) :: &
    DATA
  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, DEST, TAG, COMM
  Integer(Kind=JPIM), Intent(OUT) :: &
    REQUEST, IERROR

  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, DEST8, TAG8, COMM8, REQUEST8, IERROR8

  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  DEST8 = DEST
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_ISEND(DATA, COUNT8, DATATYPE8, DEST8, TAG8, COMM8, REQUEST8, IERROR8)

  REQUEST = REQUEST8
  IERROR = IERROR8

End Subroutine MPI_ISEND8_R8

! ---------------------------------------------------------
Subroutine MPI_ISEND8_R42(DATA, COUNT, DATATYPE, DEST, TAG, COMM, REQUEST, IERROR)

  Real(Kind=JPRM), Dimension(:,:), Intent(IN) :: &
    DATA
  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, DEST, TAG, COMM
  Integer(Kind=JPIM), Intent(OUT) :: &
    REQUEST, IERROR

  Real(Kind=8), Dimension(:,:), Allocatable :: &
    DATA8
  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, DEST8, TAG8, COMM8, REQUEST8, IERROR8

  Allocate(DATA8(SIZE(DATA,Dim=1),SIZE(DATA,Dim=2)))

  DATA8 = DATA
  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  DEST8 = DEST
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_ISEND(DATA8, COUNT8, DATATYPE8, DEST8, TAG8, COMM8, REQUEST8, IERROR8)

  REQUEST = REQUEST8
  IERROR = IERROR8

  Deallocate(DATA8)

End Subroutine MPI_ISEND8_R42

! =========================================================
Subroutine MPI_ISEND8_I42(DATA, COUNT, DATATYPE, DEST, TAG, COMM, REQUEST, IERROR)

  Integer(Kind=JPRM), Dimension(:,:), Intent(IN) :: &
    DATA
  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, DEST, TAG, COMM
  Integer(Kind=JPIM), Intent(OUT) :: &
    REQUEST, IERROR

  Integer(Kind=8), Dimension(:,:), Allocatable :: &
    DATA8
  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, DEST8, TAG8, COMM8, REQUEST8, IERROR8

  Allocate(DATA8(SIZE(DATA,Dim=1),SIZE(DATA,Dim=2)))

  DATA8 = DATA
  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  DEST8 = DEST
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_ISEND(DATA8, COUNT8, DATATYPE8, DEST8, TAG8, COMM8, REQUEST8, IERROR8)

  REQUEST = REQUEST8
  IERROR = IERROR8

  Deallocate(DATA8)

End Subroutine MPI_ISEND8_I42

! =========================================================
! ---------------------------------------------------------
Subroutine MPI_ISEND8_R82(DATA, COUNT, DATATYPE, DEST, TAG, COMM, REQUEST, IERROR)

  Real(Kind=JPRB), Dimension(:,:), Intent(IN) :: &
    DATA
  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, DEST, TAG, COMM
  Integer(Kind=JPIM), Intent(OUT) :: &
    REQUEST, IERROR

  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, DEST8, TAG8, COMM8, REQUEST8, IERROR8

  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  DEST8 = DEST
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_ISEND(DATA, COUNT8, DATATYPE8, DEST8, TAG8, COMM8, REQUEST8, IERROR8)

  REQUEST = REQUEST8
  IERROR = IERROR8

End Subroutine MPI_ISEND8_R82

! =========================================================
! ---------------------------------------------------------
Subroutine MPI_ISEND8_I4(DATA, COUNT, DATATYPE, DEST, TAG, COMM, REQUEST, IERROR)

  Integer(Kind=JPIM), Dimension(:), Intent(IN) :: &
    DATA
  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, DEST, TAG, COMM
  Integer(Kind=JPIM), Intent(OUT) :: &
    REQUEST, IERROR

  Integer(Kind=8), Dimension(:), Allocatable :: &
    DATA8
  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, DEST8, TAG8, COMM8, REQUEST8, IERROR8

  Allocate(DATA8(SIZE(DATA)))

  DATA8 = DATA
  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  DEST8 = DEST
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_ISEND(DATA8, COUNT8, DATATYPE8, DEST8, TAG8, COMM8, REQUEST8, IERROR8)

  REQUEST = REQUEST8
  IERROR = IERROR8

  Deallocate(DATA8)

End Subroutine MPI_ISEND8_I4

! =========================================================
Subroutine MPI_ISEND8_R4_1(DATA, COUNT, DATATYPE, DEST, TAG, COMM, REQUEST, IERROR)

  Real(Kind=JPRM), Intent(IN) :: &
    DATA
  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, DEST, TAG, COMM
  Integer(Kind=JPIM), Intent(OUT) :: &
    REQUEST, IERROR

  Real(Kind=8):: &
    DATA8
  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, DEST8, TAG8, COMM8, REQUEST8, IERROR8

  DATA8 = DATA
  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  DEST8 = DEST
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_ISEND(DATA8, COUNT8, DATATYPE8, DEST8, TAG8, COMM8, REQUEST8, IERROR8)

  REQUEST = REQUEST8
  IERROR = IERROR8

End Subroutine MPI_ISEND8_R4_1

! =========================================================
Subroutine MPI_ISEND8_R8_1(DATA, COUNT, DATATYPE, DEST, TAG, COMM, REQUEST, IERROR)

  Real(Kind=JPRB), Intent(IN) :: &
    DATA
  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, DEST, TAG, COMM
  Integer(Kind=JPIM), Intent(OUT) :: &
    REQUEST, IERROR

  Real(Kind=8) :: &
    DATA8
  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, DEST8, TAG8, COMM8, REQUEST8, IERROR8

  DATA8 = DATA
  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  DEST8 = DEST
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_ISEND(DATA8, COUNT8, DATATYPE8, DEST8, TAG8, COMM8, REQUEST8, IERROR8)

  REQUEST = REQUEST8
  IERROR = IERROR8

End Subroutine MPI_ISEND8_R8_1

! =========================================================
! ---------------------------------------------------------
Subroutine MPI_ISEND8_I1(DATA, COUNT, DATATYPE, DEST, TAG, COMM, REQUEST, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    DATA
  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, DEST, TAG, COMM
  Integer(Kind=JPIM), Intent(OUT) :: &
    REQUEST, IERROR

  Integer(Kind=8) :: &
    DATA8, COUNT8, DATATYPE8, DEST8, TAG8, COMM8, REQUEST8, IERROR8

  DATA8 = DATA
  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  DEST8 = DEST
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_ISEND(DATA8, COUNT8, DATATYPE8, DEST8, TAG8, COMM8, REQUEST8, IERROR8)

  REQUEST = REQUEST8
  IERROR = IERROR8

End Subroutine MPI_ISEND8_I1

! =========================================================
! ---------------------------------------------------------
Subroutine MPI_ISEND8_CH(DATA, COUNT, DATATYPE, DEST, TAG, COMM, REQUEST, IERROR)

  Character(Len=*), Intent(IN) :: &
    DATA
  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, DEST, TAG, COMM
  Integer(Kind=JPIM), Intent(OUT) :: &
    REQUEST, IERROR

  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, DEST8, TAG8, COMM8, REQUEST8, IERROR8

  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  DEST8 = DEST
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_ISEND(DATA, COUNT8, DATATYPE8, DEST8, TAG8, COMM8, REQUEST8, IERROR8)

  REQUEST = REQUEST8
  IERROR = IERROR8

End Subroutine MPI_ISEND8_CH

! =========================================================
! ---------------------------------------------------------
Subroutine MPI_SEND8_R4(DATA, COUNT, DATATYPE, DEST, TAG, COMM, IERROR)

  Real(Kind=JPRM), Dimension(:), Intent(IN) :: &
    DATA
  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, DEST, TAG, COMM
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Real(Kind=8), Dimension(:), Allocatable :: &
    DATA8
  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, DEST8, TAG8, COMM8, IERROR8

  Allocate(DATA8(SIZE(DATA)))

  DATA8 = DATA
  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  DEST8 = DEST
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_SEND(DATA8, COUNT8, DATATYPE8, DEST8, TAG8, COMM8, IERROR8)

  IERROR = IERROR8

  Deallocate(DATA8)

End Subroutine MPI_SEND8_R4

! =========================================================
! ---------------------------------------------------------
Subroutine MPI_SEND8_R8(DATA, COUNT, DATATYPE, DEST, TAG, COMM, IERROR)

  Real(Kind=JPRB), Dimension(:), Intent(IN) :: &
    DATA
  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, DEST, TAG, COMM
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, DEST8, TAG8, COMM8, IERROR8

  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  DEST8 = DEST
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_SEND(DATA, COUNT8, DATATYPE8, DEST8, TAG8, COMM8, IERROR8)

  IERROR = IERROR8

End Subroutine MPI_SEND8_R8

! ---------------------------------------------------------
Subroutine MPI_SEND8_R42(DATA, COUNT, DATATYPE, DEST, TAG, COMM, IERROR)

  Real(Kind=JPRM), Dimension(:,:), Intent(IN) :: &
    DATA
  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, DEST, TAG, COMM
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Real(Kind=8), Dimension(:,:), Allocatable :: &
    DATA8
  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, DEST8, TAG8, COMM8, IERROR8

  Allocate(DATA8(SIZE(DATA,Dim=1),SIZE(DATA,Dim=2)))

  DATA8 = DATA
  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  DEST8 = DEST
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_SEND(DATA8, COUNT8, DATATYPE8, DEST8, TAG8, COMM8, IERROR8)

  IERROR = IERROR8

  Deallocate(DATA8)

End Subroutine MPI_SEND8_R42

! ---------------------------------------------------------
Subroutine MPI_SEND8_I42(DATA, COUNT, DATATYPE, DEST, TAG, COMM, IERROR)

  Integer(Kind=JPIM), Dimension(:,:), Intent(IN) :: &
    DATA
  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, DEST, TAG, COMM
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Integer(Kind=8), Dimension(:,:), Allocatable :: &
    DATA8
  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, DEST8, TAG8, COMM8, IERROR8

  Allocate(DATA8(SIZE(DATA,Dim=1),SIZE(DATA,Dim=2)))

  DATA8 = DATA
  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  DEST8 = DEST
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_SEND(DATA8, COUNT8, DATATYPE8, DEST8, TAG8, COMM8, IERROR8)

  IERROR = IERROR8

  Deallocate(DATA8)

End Subroutine MPI_SEND8_I42

! =========================================================
! ---------------------------------------------------------
Subroutine MPI_SEND8_R82(DATA, COUNT, DATATYPE, DEST, TAG, COMM, IERROR)

  Real(Kind=JPRB), Dimension(:,:), Intent(IN) :: &
    DATA
  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, DEST, TAG, COMM
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, DEST8, TAG8, COMM8, IERROR8

  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  DEST8 = DEST
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_SEND(DATA, COUNT8, DATATYPE8, DEST8, TAG8, COMM8, IERROR8)

  IERROR = IERROR8

End Subroutine MPI_SEND8_R82

! =========================================================
! ---------------------------------------------------------
Subroutine MPI_SEND8_I4(DATA, COUNT, DATATYPE, DEST, TAG, COMM, IERROR)

  Integer(Kind=JPIM), Dimension(:), Intent(IN) :: &
    DATA
  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, DEST, TAG, COMM
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Integer(Kind=8), Dimension(:), Allocatable :: &
    DATA8
  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, DEST8, TAG8, COMM8, IERROR8

  Allocate(DATA8(SIZE(DATA)))

  DATA8 = DATA
  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  DEST8 = DEST
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_SEND(DATA8, COUNT8, DATATYPE8, DEST8, TAG8, COMM8, IERROR8)

  IERROR = IERROR8

  Deallocate(DATA8)

End Subroutine MPI_SEND8_I4

! =========================================================
! ---------------------------------------------------------
Subroutine MPI_SEND8_I1(DATA, COUNT, DATATYPE, DEST, TAG, COMM, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    DATA
  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, DEST, TAG, COMM
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Integer(Kind=8) :: &
    DATA8, COUNT8, DATATYPE8, DEST8, TAG8, COMM8, IERROR8

  DATA8 = DATA
  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  DEST8 = DEST
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_SEND(DATA8, COUNT8, DATATYPE8, DEST8, TAG8, COMM8, IERROR8)

  IERROR = IERROR8

End Subroutine MPI_SEND8_I1

! =========================================================
Subroutine MPI_SEND8_R4_1(DATA, COUNT, DATATYPE, DEST, TAG, COMM, IERROR)

  Real(Kind=JPRM), Intent(IN) :: &
    DATA
  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, DEST, TAG, COMM
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Real(Kind=8) :: DATA8
  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, DEST8, TAG8, COMM8, IERROR8

  DATA8 = DATA
  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  DEST8 = DEST
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_SEND(DATA8, COUNT8, DATATYPE8, DEST8, TAG8, COMM8, IERROR8)

  IERROR = IERROR8

End Subroutine MPI_SEND8_R4_1

! =========================================================
Subroutine MPI_SEND8_R8_1(DATA, COUNT, DATATYPE, DEST, TAG, COMM, IERROR)

  Real(Kind=JPRB), Intent(IN) :: &
    DATA
  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, DEST, TAG, COMM
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Real(Kind=8) :: DATA8
  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, DEST8, TAG8, COMM8, IERROR8

  DATA8 = DATA
  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  DEST8 = DEST
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_SEND(DATA8, COUNT8, DATATYPE8, DEST8, TAG8, COMM8, IERROR8)

  IERROR = IERROR8

End Subroutine MPI_SEND8_R8_1

! =========================================================
! ---------------------------------------------------------
Subroutine MPI_SEND8_CH(DATA, COUNT, DATATYPE, DEST, TAG, COMM, IERROR)

  Character(Len=*), Intent(IN) :: &
    DATA
  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, DEST, TAG, COMM
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, DEST8, TAG8, COMM8, IERROR8

  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  DEST8 = DEST
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_SEND(DATA, COUNT8, DATATYPE8, DEST8, TAG8, COMM8, IERROR8)

  IERROR = IERROR8

End Subroutine MPI_SEND8_CH

! =========================================================
! ---------------------------------------------------------
Subroutine MPI_FILE_IREAD_SHARED8_R8(FH, BUF, COUNT, DATATYPE, REQUEST, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    FH, COUNT, DATATYPE
  Real(Kind=JPRB), Dimension(:), Intent(OUT) :: &
    BUF
  Integer(Kind=JPIM), Intent(OUT) :: &
    REQUEST, IERROR

  Integer(Kind=8) :: &
    FH8, COUNT8, DATATYPE8, REQUEST8, IERROR8

  FH8 = FH
  COUNT8 = COUNT
  DATATYPE8 = DATATYPE

  Call MPI_FILE_IREAD_SHARED(FH8, BUF, COUNT8, DATATYPE8, REQUEST8, IERROR8)

  REQUEST = REQUEST8
  IERROR = IERROR8

End Subroutine MPI_FILE_IREAD_SHARED8_R8

! ---------------------------------------------------------
Subroutine MPI_FILE_IREAD_SHARED8_I4(FH, BUF, COUNT, DATATYPE, REQUEST, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    FH, COUNT, DATATYPE
  Integer(Kind=JPIM), Intent(OUT) :: &
    BUF(:), REQUEST, IERROR

  Integer(Kind=8), Dimension(:), Allocatable :: &
    BUF8
  Integer(Kind=8) :: &
    FH8, COUNT8, DATATYPE8, REQUEST8, IERROR8

  Allocate(BUF8(SIZE(BUF)))

  FH8 = FH
  COUNT8 = COUNT
  DATATYPE8 = DATATYPE

  Call MPI_FILE_IREAD_SHARED(FH8, BUF8, COUNT8, DATATYPE8, REQUEST8, IERROR8)

  BUF = BUF8
  REQUEST = REQUEST8
  IERROR = IERROR8

  Deallocate(BUF8)

End Subroutine MPI_FILE_IREAD_SHARED8_I4

! =========================================================
! ---------------------------------------------------------
Subroutine MPI_FILE_IWRITE_SHARED8_R8(FH, BUF, COUNT, DATATYPE, REQUEST, IERROR)

  Real(Kind=JPRB), Dimension(:), Intent(IN) :: &
    BUF
  Integer(Kind=JPIM), Intent(IN) :: &
    FH, COUNT, DATATYPE
  Integer(Kind=JPIM), Intent(OUT) :: &
    REQUEST, IERROR

  Integer(Kind=8) :: &
    FH8, COUNT8, DATATYPE8, REQUEST8, IERROR8

  FH8 = FH
  COUNT8 = COUNT
  DATATYPE8 = DATATYPE

  Call MPI_FILE_IWRITE_SHARED(FH8, BUF, COUNT8, DATATYPE8, REQUEST8, IERROR8)

  REQUEST = REQUEST8
  IERROR = IERROR8

End Subroutine MPI_FILE_IWRITE_SHARED8_R8

! ---------------------------------------------------------
Subroutine MPI_FILE_IWRITE_SHARED8_I4(FH, BUF, COUNT, DATATYPE, REQUEST, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    FH, BUF(:), COUNT, DATATYPE
  Integer(Kind=JPIM), Intent(OUT) :: &
    REQUEST, IERROR

  Integer(Kind=8), Dimension(:), Allocatable :: &
    BUF8
  Integer(Kind=8) :: &
    FH8, COUNT8, DATATYPE8, REQUEST8, IERROR8

  Allocate(BUF8(SIZE(BUF)))

  FH8 = FH
  BUF8 = BUF
  COUNT8 = COUNT
  DATATYPE8 = DATATYPE

  Call MPI_FILE_IWRITE_SHARED(FH8, BUF8, COUNT8, DATATYPE8, REQUEST8, IERROR8)

  REQUEST = REQUEST8
  IERROR = IERROR8

  Deallocate(BUF8)

End Subroutine MPI_FILE_IWRITE_SHARED8_I4

! =========================================================
! ---------------------------------------------------------
Subroutine MPI_FILE_READ_ORDERED8_R8(FH, BUF, COUNT, DATATYPE, STATUS, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    FH, COUNT, DATATYPE
  Real(Kind=JPRB), Dimension(:), Intent(OUT) :: &
    BUF
  Integer(Kind=JPIM), Intent(OUT) :: &
    STATUS(:), IERROR

  Integer(Kind=8) :: &
    FH8, COUNT8, DATATYPE8, IERROR8

  FH8 = FH
  COUNT8 = COUNT
  DATATYPE8 = DATATYPE

  Call MPI_FILE_READ_ORDERED(FH8, BUF, COUNT8, DATATYPE8, STATUS8, IERROR8)

  STATUS = STATUS8
  IERROR = IERROR8

End Subroutine MPI_FILE_READ_ORDERED8_R8

! ---------------------------------------------------------
Subroutine MPI_FILE_READ_ORDERED8_I4(FH, BUF, COUNT, DATATYPE, STATUS, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    FH, COUNT, DATATYPE
  Integer(Kind=JPIM), Intent(OUT) :: &
    BUF(:), STATUS(:), IERROR

  Integer(Kind=8), Dimension(:), Allocatable :: &
    BUF8
  Integer(Kind=8) :: &
    FH8, COUNT8, DATATYPE8, IERROR8

  Allocate(BUF8(SIZE(BUF)))

  FH8 = FH
  COUNT8 = COUNT
  DATATYPE8 = DATATYPE

  Call MPI_FILE_READ_ORDERED(FH8, BUF8, COUNT8, DATATYPE8, STATUS8, IERROR8)

  BUF = BUF8
  STATUS = STATUS8
  IERROR = IERROR8

  Deallocate(BUF8)

End Subroutine MPI_FILE_READ_ORDERED8_I4

! =========================================================
! ---------------------------------------------------------
Subroutine MPI_FREAD_ORDERED_BEGIN8_R8(FH, BUF, COUNT, DATATYPE, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    FH, COUNT, DATATYPE
  Real(Kind=JPRB), Dimension(:), Intent(OUT) :: &
    BUF
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Integer(Kind=8) :: &
    FH8, COUNT8, DATATYPE8, IERROR8

  FH8 = FH
  COUNT8 = COUNT
  DATATYPE8 = DATATYPE

  Call MPI_FILE_READ_ORDERED_BEGIN(FH8, BUF, COUNT8, DATATYPE8, IERROR8)

  IERROR = IERROR8

End Subroutine MPI_FREAD_ORDERED_BEGIN8_R8

! ---------------------------------------------------------
Subroutine MPI_FREAD_ORDERED_BEGIN8_I4(FH, BUF, COUNT, DATATYPE, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    FH, COUNT, DATATYPE
  Integer(Kind=JPIM), Intent(OUT) :: &
    BUF(:), IERROR

  Integer(Kind=8), Dimension(:), Allocatable :: &
    BUF8
  Integer(Kind=8) :: &
    FH8, COUNT8, DATATYPE8, IERROR8

  Allocate(BUF8(SIZE(BUF)))

  FH8 = FH
  COUNT8 = COUNT
  DATATYPE8 = DATATYPE

  Call MPI_FILE_READ_ORDERED_BEGIN(FH8, BUF8, COUNT8, DATATYPE8, IERROR8)

  BUF = BUF8
  IERROR = IERROR8

  Deallocate(BUF8)

End Subroutine MPI_FREAD_ORDERED_BEGIN8_I4

! =========================================================
! ---------------------------------------------------------
Subroutine MPI_FREAD_ORDERED_END8_R8(FH, BUF, STATUS, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    FH
  Real(Kind=JPRB), Dimension(:), Intent(OUT) :: &
    BUF
  Integer(Kind=JPIM), Intent(OUT) :: &
    STATUS(:), IERROR

  Integer(Kind=8) :: &
    FH8, IERROR8

  FH8 = FH

  Call MPI_FILE_READ_ORDERED_END(FH8, BUF, STATUS8, IERROR8)

  STATUS = STATUS8
  IERROR = IERROR8

End Subroutine MPI_FREAD_ORDERED_END8_R8

! ---------------------------------------------------------
Subroutine MPI_FREAD_ORDERED_END8_I4(FH, BUF, STATUS, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    FH
  Integer(Kind=JPIM), Intent(OUT) :: &
    BUF(:), STATUS(:), IERROR

  Integer(Kind=8), Dimension(:), Allocatable :: &
    BUF8
  Integer(Kind=8) :: &
    FH8, IERROR8

  Allocate(BUF8(SIZE(BUF)))

  FH8 = FH

  Call MPI_FILE_READ_ORDERED_END(FH8, BUF8, STATUS8, IERROR8)

  BUF = BUF8
  STATUS = STATUS8
  IERROR = IERROR8

  Deallocate(BUF8)

End Subroutine MPI_FREAD_ORDERED_END8_I4

! =========================================================
! ---------------------------------------------------------
Subroutine MPI_FILE_READ_SHARED8_R8(FH, BUF, COUNT, DATATYPE, STATUS, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    FH, COUNT, DATATYPE
  Real(Kind=JPRB), Dimension(:), Intent(OUT) :: &
    BUF
  Integer(Kind=JPIM), Intent(OUT) :: &
    STATUS(:), IERROR

  Integer(Kind=8) :: &
    FH8, COUNT8, DATATYPE8, IERROR8

  FH8 = FH
  COUNT8 = COUNT
  DATATYPE8 = DATATYPE

  Call MPI_FILE_READ_SHARED(FH8, BUF, COUNT8, DATATYPE8, STATUS8, IERROR8)

  STATUS = STATUS8
  IERROR = IERROR8

End Subroutine MPI_FILE_READ_SHARED8_R8

! ---------------------------------------------------------
Subroutine MPI_FILE_READ_SHARED8_I4(FH, BUF, COUNT, DATATYPE, STATUS, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    FH, COUNT, DATATYPE
  Integer(Kind=JPIM), Intent(OUT) :: &
    BUF(:), STATUS(:), IERROR

  Integer(Kind=8), Dimension(:), Allocatable :: &
    BUF8
  Integer(Kind=8) :: &
    FH8, COUNT8, DATATYPE8, IERROR8

  Allocate(BUF8(SIZE(BUF)))

  FH8 = FH
  COUNT8 = COUNT
  DATATYPE8 = DATATYPE

  Call MPI_FILE_READ_SHARED(FH8, BUF8, COUNT8, DATATYPE8, STATUS8, IERROR8)

  BUF = BUF8
  STATUS = STATUS8
  IERROR = IERROR8

  Deallocate(BUF8)

End Subroutine MPI_FILE_READ_SHARED8_I4

! =========================================================
! ---------------------------------------------------------
Subroutine MPI_FILE_WRITE_ORDERED8_R8(FH, BUF, COUNT, DATATYPE, STATUS, IERROR)

  Real(Kind=JPRB), Dimension(:), Intent(IN) :: &
    BUF
  Integer(Kind=JPIM), Intent(IN) :: &
    FH, COUNT, DATATYPE
  Integer(Kind=JPIM), Intent(OUT) :: &
    STATUS(:), IERROR

  Integer(Kind=8) :: &
    FH8, COUNT8, DATATYPE8, IERROR8

  FH8 = FH
  COUNT8 = COUNT
  DATATYPE8 = DATATYPE

  Call MPI_FILE_WRITE_ORDERED(FH8, BUF, COUNT8, DATATYPE8, STATUS8, IERROR8)

  STATUS = STATUS8
  IERROR = IERROR8

End Subroutine MPI_FILE_WRITE_ORDERED8_R8

! ---------------------------------------------------------
Subroutine MPI_FILE_WRITE_ORDERED8_I4(FH, BUF, COUNT, DATATYPE, STATUS, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    FH, BUF(:), COUNT, DATATYPE
  Integer(Kind=JPIM), Intent(OUT) :: &
    STATUS(:), IERROR

  Integer(Kind=8), Dimension(:), Allocatable :: &
    BUF8
  Integer(Kind=8) :: &
    FH8, COUNT8, DATATYPE8, IERROR8

  Allocate(BUF8(SIZE(BUF)))

  FH8 = FH
  BUF8 = BUF
  COUNT8 = COUNT
  DATATYPE8 = DATATYPE

  Call MPI_FILE_WRITE_ORDERED(FH8, BUF8, COUNT8, DATATYPE8, STATUS8, IERROR8)

  STATUS = STATUS8
  IERROR = IERROR8

  Deallocate(BUF8)

End Subroutine MPI_FILE_WRITE_ORDERED8_I4

! =========================================================
! ---------------------------------------------------------
Subroutine MPI_FWRITE_ORDERED_BEGIN8_R8(FH, BUF, COUNT, DATATYPE, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    FH, COUNT, DATATYPE
  Real(Kind=JPRB), Dimension(:), Intent(OUT) :: &
    BUF
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Integer(Kind=8) :: &
    FH8, COUNT8, DATATYPE8, IERROR8

  FH8 = FH
  COUNT8 = COUNT
  DATATYPE8 = DATATYPE

  Call MPI_FILE_WRITE_ORDERED_BEGIN(FH8, BUF, COUNT8, DATATYPE8, IERROR8)

  IERROR = IERROR8

End Subroutine MPI_FWRITE_ORDERED_BEGIN8_R8

! ---------------------------------------------------------
Subroutine MPI_FWRITE_ORDERED_BEGIN8_I4(FH, BUF, COUNT, DATATYPE, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    FH, COUNT, DATATYPE
  Integer(Kind=JPIM), Intent(OUT) :: &
    BUF(:), IERROR

  Integer(Kind=8), Dimension(:), Allocatable :: &
    BUF8
  Integer(Kind=8) :: &
    FH8, COUNT8, DATATYPE8, IERROR8

  Allocate(BUF8(SIZE(BUF)))

  FH8 = FH
  COUNT8 = COUNT
  DATATYPE8 = DATATYPE

  Call MPI_FILE_WRITE_ORDERED_BEGIN(FH8, BUF8, COUNT8, DATATYPE8, IERROR8)

  BUF = BUF8
  IERROR = IERROR8

  Deallocate(BUF8)

End Subroutine MPI_FWRITE_ORDERED_BEGIN8_I4

! =========================================================
! ---------------------------------------------------------
Subroutine MPI_FWRITE_ORDERED_END8_R8(FH, BUF, STATUS, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    FH, STATUS(:)
  Real(Kind=JPRB), Dimension(:), Intent(OUT) :: &
    BUF
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Integer(Kind=8) :: &
    FH8, IERROR8

  FH8 = FH
  STATUS8 = STATUS

  Call MPI_FILE_WRITE_ORDERED_END(FH8, BUF, STATUS8, IERROR8)

  IERROR = IERROR8

End Subroutine MPI_FWRITE_ORDERED_END8_R8

! ---------------------------------------------------------
Subroutine MPI_FWRITE_ORDERED_END8_I4(FH, BUF, STATUS, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    FH, STATUS(:)
  Integer(Kind=JPIM), Intent(OUT) :: &
    BUF(:), IERROR

  Integer(Kind=8), Dimension(:), Allocatable :: &
    BUF8
  Integer(Kind=8) :: &
    FH8, IERROR8

  Allocate(BUF8(SIZE(BUF)))

  FH8 = FH
  STATUS8 = STATUS

  Call MPI_FILE_WRITE_ORDERED_END(FH8, BUF8, STATUS8, IERROR8)

  BUF = BUF8
  IERROR = IERROR8

  Deallocate(BUF8)

End Subroutine MPI_FWRITE_ORDERED_END8_I4

! =========================================================
! ---------------------------------------------------------
Subroutine MPI_FILE_WRITE_SHARED8_R8(FH, BUF, COUNT, DATATYPE, STATUS, IERROR)

  Real(Kind=JPRB), Dimension(:), Intent(IN) :: &
    BUF
  Integer(Kind=JPIM), Intent(IN) :: &
    FH, COUNT, DATATYPE
  Integer(Kind=JPIM), Intent(OUT) :: &
    STATUS(:), IERROR

  Integer(Kind=8) :: &
    FH8, COUNT8, DATATYPE8, IERROR8

  FH8 = FH
  COUNT8 = COUNT
  DATATYPE8 = DATATYPE

  Call MPI_FILE_WRITE_SHARED(FH8, BUF, COUNT8, DATATYPE8, STATUS8, IERROR8)

  STATUS = STATUS8
  IERROR = IERROR8

End Subroutine MPI_FILE_WRITE_SHARED8_R8

! ---------------------------------------------------------
Subroutine MPI_FILE_WRITE_SHARED8_I4(FH, BUF, COUNT, DATATYPE, STATUS, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    FH, BUF(:), COUNT, DATATYPE
  Integer(Kind=JPIM), Intent(OUT) :: &
    STATUS(:), IERROR

  Integer(Kind=8), Dimension(:), Allocatable :: &
    BUF8
  Integer(Kind=8) :: &
    FH8, COUNT8, DATATYPE8, IERROR8

  Allocate(BUF8(SIZE(BUF)))

  FH8 = FH
  BUF8 = BUF
  COUNT8 = COUNT
  DATATYPE8 = DATATYPE

  Call MPI_FILE_WRITE_SHARED(FH8, BUF8, COUNT8, DATATYPE8, STATUS8, IERROR8)

  STATUS = STATUS8
  IERROR = IERROR8

  Deallocate(BUF8)

End Subroutine MPI_FILE_WRITE_SHARED8_I4

! =========================================================
! ---------------------------------------------------------
Subroutine MPI_GATHER8_R8_1(SENDDATA, SENDCOUNT, SENDTYPE, RECVAREA, &
                            RECVCOUNT, RECVTYPE, ROOT, COMM, IERROR)

  Real(Kind=JPRB), Intent(IN) :: &
    SENDDATA
  Integer(Kind=JPIM), Intent(IN) :: &
    SENDCOUNT, SENDTYPE, RECVCOUNT, RECVTYPE, ROOT, COMM
  Real(Kind=JPRB), Intent(OUT) :: &
    RECVAREA
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Integer(Kind=8) :: &
    SENDCOUNT8, SENDTYPE8, RECVCOUNT8, RECVTYPE8, ROOT8, COMM8, IERROR8

  SENDCOUNT8 = SENDCOUNT
  SENDTYPE8 = SENDTYPE
  RECVCOUNT8 = RECVCOUNT
  RECVTYPE8 = RECVTYPE
  ROOT8 = ROOT
  COMM8 = COMM

  Call MPI_GATHER(SENDDATA, SENDCOUNT8, SENDTYPE8, RECVAREA, &
                  RECVCOUNT8, RECVTYPE8, ROOT8, COMM8, IERROR8)

  IERROR = IERROR8

End Subroutine MPI_GATHER8_R8_1

! ---------------------------------------------------------
Subroutine MPI_GATHER8_I1(SENDDATA, SENDCOUNT, SENDTYPE, RECVAREA, &
                          RECVCOUNT, RECVTYPE, ROOT, COMM, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    SENDDATA, SENDCOUNT, SENDTYPE, RECVCOUNT, RECVTYPE, ROOT, COMM
  Integer(Kind=JPIM), Intent(OUT) :: &
    RECVAREA, IERROR

  Integer(Kind=8) :: &
    SENDDATA8, RECVAREA8
  Integer(Kind=8) :: &
    SENDCOUNT8, SENDTYPE8, RECVCOUNT8, RECVTYPE8, ROOT8, COMM8, IERROR8

  SENDDATA8 = SENDDATA
  SENDCOUNT8 = SENDCOUNT
  SENDTYPE8 = SENDTYPE
  RECVCOUNT8 = RECVCOUNT
  RECVTYPE8 = RECVTYPE
  ROOT8 = ROOT
  COMM8 = COMM

  Call MPI_GATHER(SENDDATA8, SENDCOUNT8, SENDTYPE8, RECVAREA8, &
                  RECVCOUNT8, RECVTYPE8, ROOT8, COMM8, IERROR8)

  RECVAREA = RECVAREA8
  IERROR = IERROR8

End Subroutine MPI_GATHER8_I1

! =========================================================
! ---------------------------------------------------------
Subroutine MPI_GATHERV8_R8(SENDDATA, SENDCOUNT, SENDTYPE, RECVAREA, &
                           RECVCOUNTS, DISPLS, RECVTYPE, ROOT, COMM, IERROR)

  Real(Kind=JPRB), Dimension(:), Intent(IN) :: &
    SENDDATA
  Integer(Kind=JPIM), Dimension(:), Intent(IN) :: &
    RECVCOUNTS, DISPLS
  Integer(Kind=JPIM), Intent(IN) :: &
    SENDCOUNT, SENDTYPE, RECVTYPE, ROOT, COMM
  Real(Kind=JPRB), Dimension(:), Intent(OUT) :: &
    RECVAREA
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Integer(Kind=8), Dimension(:), Allocatable :: &
    RECVCOUNTS8, DISPLS8
  Integer(Kind=8) :: &
    SENDCOUNT8, SENDTYPE8, RECVTYPE8, ROOT8, COMM8, IERROR8

  Allocate(RECVCOUNTS8(SIZE(RECVCOUNTS)))
  Allocate(DISPLS8(SIZE(DISPLS)))

  SENDCOUNT8 = SENDCOUNT
  SENDTYPE8 = SENDTYPE
  RECVCOUNTS8 = RECVCOUNTS
  DISPLS8 = DISPLS
  RECVTYPE8 = RECVTYPE
  ROOT8 = ROOT
  COMM8 = COMM

  Call MPI_GATHERV(SENDDATA, SENDCOUNT8, SENDTYPE8, RECVAREA, &
                   RECVCOUNTS8, DISPLS8, RECVTYPE8, ROOT8, COMM8, IERROR8)

  IERROR = IERROR8

  Deallocate(RECVCOUNTS8)
  Deallocate(DISPLS8)

End Subroutine MPI_GATHERV8_R8

! ---------------------------------------------------------
Subroutine MPI_GATHERV8_R8S(SENDDATA, SENDCOUNT, SENDTYPE, RECVAREA, &
                           RECVCOUNTS, DISPLS, RECVTYPE, ROOT, COMM, IERROR)

  Real(Kind=JPRB), Dimension(:), Intent(IN) :: &
    SENDDATA
  Integer(Kind=JPIM), Intent(IN) :: &
    RECVCOUNTS, DISPLS
  Integer(Kind=JPIM), Intent(IN) :: &
    SENDCOUNT, SENDTYPE, RECVTYPE, ROOT, COMM
  Real(Kind=JPRB), Intent(OUT) :: &
    RECVAREA
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Integer(Kind=8) :: RECVCOUNTS8, DISPLS8
  Integer(Kind=8) :: &
    SENDCOUNT8, SENDTYPE8, RECVTYPE8, ROOT8, COMM8, IERROR8

  SENDCOUNT8 = SENDCOUNT
  SENDTYPE8 = SENDTYPE
  RECVCOUNTS8 = RECVCOUNTS
  DISPLS8 = DISPLS
  RECVTYPE8 = RECVTYPE
  ROOT8 = ROOT
  COMM8 = COMM

  Call MPI_GATHERV(SENDDATA, SENDCOUNT8, SENDTYPE8, RECVAREA, &
                   RECVCOUNTS8, DISPLS8, RECVTYPE8, ROOT8, COMM8, IERROR8)

  IERROR = IERROR8

End Subroutine MPI_GATHERV8_R8S

! ---------------------------------------------------------
Subroutine MPI_GATHERV8_R4(SENDDATA, SENDCOUNT, SENDTYPE, RECVAREA, &
                           RECVCOUNTS, DISPLS, RECVTYPE, ROOT, COMM, IERROR)

  Real(Kind=JPRM), Dimension(:), Intent(IN) :: &
    SENDDATA
  Integer(Kind=JPIM), Dimension(:), Intent(IN) :: &
    RECVCOUNTS, DISPLS
  Integer(Kind=JPIM), Intent(IN) :: &
    SENDCOUNT, SENDTYPE, RECVTYPE, ROOT, COMM
  Real(Kind=JPRM), Dimension(:), Intent(OUT) :: &
    RECVAREA
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Real(Kind=8), Dimension(:), Allocatable :: &
    SENDDATA8, RECVAREA8
  Integer(Kind=8), Dimension(:), Allocatable :: &
    RECVCOUNTS8, DISPLS8
  Integer(Kind=8) :: &
    SENDCOUNT8, SENDTYPE8, RECVTYPE8, ROOT8, COMM8, IERROR8

  Allocate(SENDDATA8(SIZE(SENDDATA)))
  Allocate(RECVAREA8(SIZE(RECVAREA)))
  Allocate(RECVCOUNTS8(SIZE(RECVCOUNTS)))
  Allocate(DISPLS8(SIZE(DISPLS)))

  SENDDATA8 = SENDDATA
  SENDCOUNT8 = SENDCOUNT
  SENDTYPE8 = SENDTYPE
  RECVCOUNTS8 = RECVCOUNTS
  DISPLS8 = DISPLS
  RECVTYPE8 = RECVTYPE
  ROOT8 = ROOT
  COMM8 = COMM

  Call MPI_GATHERV(SENDDATA8, SENDCOUNT8, SENDTYPE8, RECVAREA8, &
                   RECVCOUNTS8, DISPLS8, RECVTYPE8, ROOT8, COMM8, IERROR8)

  RECVAREA = RECVAREA8
  IERROR = IERROR8

  Deallocate(SENDDATA8)
  Deallocate(RECVAREA8)
  Deallocate(RECVCOUNTS8)
  Deallocate(DISPLS8)

End Subroutine MPI_GATHERV8_R4

! ---------------------------------------------------------
Subroutine MPI_GATHERV8_R4S(SENDDATA, SENDCOUNT, SENDTYPE, RECVAREA, &
                           RECVCOUNTS, DISPLS, RECVTYPE, ROOT, COMM, IERROR)

  Real(Kind=JPRM), Dimension(:), Intent(IN) :: &
    SENDDATA
  Integer(Kind=JPIM), Intent(IN) :: &
    RECVCOUNTS, DISPLS
  Integer(Kind=JPIM), Intent(IN) :: &
    SENDCOUNT, SENDTYPE, RECVTYPE, ROOT, COMM
  Real(Kind=JPRM), Intent(OUT) :: &
    RECVAREA
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Real(Kind=8), Dimension(:), Allocatable :: &
    SENDDATA8
  Real(Kind=8) :: RECVAREA8
  Integer(Kind=8) :: RECVCOUNTS8, DISPLS8
  Integer(Kind=8) :: &
    SENDCOUNT8, SENDTYPE8, RECVTYPE8, ROOT8, COMM8, IERROR8

  Allocate(SENDDATA8(SIZE(SENDDATA)))

  SENDDATA8 = SENDDATA
  SENDCOUNT8 = SENDCOUNT
  SENDTYPE8 = SENDTYPE
  RECVCOUNTS8 = RECVCOUNTS
  DISPLS8 = DISPLS
  RECVTYPE8 = RECVTYPE
  ROOT8 = ROOT
  COMM8 = COMM

  Call MPI_GATHERV(SENDDATA8, SENDCOUNT8, SENDTYPE8, RECVAREA8, &
                   RECVCOUNTS8, DISPLS8, RECVTYPE8, ROOT8, COMM8, IERROR8)

  RECVAREA = RECVAREA8
  IERROR = IERROR8

  Deallocate(SENDDATA8)

End Subroutine MPI_GATHERV8_R4S

! ---------------------------------------------------------
Subroutine MPI_GATHERV8_I4(SENDDATA, SENDCOUNT, SENDTYPE, RECVAREA, &
                           RECVCOUNTS, DISPLS, RECVTYPE, ROOT, COMM, IERROR)

  Integer(Kind=JPIM), Dimension(:), Intent(IN) :: &
    SENDDATA, RECVCOUNTS, DISPLS
  Integer(Kind=JPIM), Intent(IN) :: &
    SENDCOUNT, SENDTYPE, RECVTYPE, ROOT, COMM
  Integer(Kind=JPIM), Intent(OUT) :: &
    RECVAREA(:), IERROR

  Integer(Kind=8), Dimension(:), Allocatable :: &
    SENDDATA8, RECVAREA8, RECVCOUNTS8, DISPLS8
  Integer(Kind=8) :: &
    SENDCOUNT8, SENDTYPE8, RECVTYPE8, ROOT8, COMM8, IERROR8

  Allocate(SENDDATA8(SIZE(SENDDATA)))
  Allocate(RECVAREA8(SIZE(RECVAREA)))
  Allocate(RECVCOUNTS8(SIZE(RECVCOUNTS)))
  Allocate(DISPLS8(SIZE(DISPLS)))

  SENDDATA8 = SENDDATA
  SENDCOUNT8 = SENDCOUNT
  SENDTYPE8 = SENDTYPE
  RECVCOUNTS8 = RECVCOUNTS
  DISPLS8 = DISPLS
  RECVTYPE8 = RECVTYPE
  ROOT8 = ROOT
  COMM8 = COMM

  Call MPI_GATHERV(SENDDATA8, SENDCOUNT8, SENDTYPE8, RECVAREA8, &
                   RECVCOUNTS8, DISPLS8, RECVTYPE8, ROOT8, COMM8, IERROR8)

  RECVAREA = RECVAREA8
  IERROR = IERROR8

  Deallocate(SENDDATA8)
  Deallocate(RECVAREA8)
  Deallocate(RECVCOUNTS8)
  Deallocate(DISPLS8)

End Subroutine MPI_GATHERV8_I4

! ---------------------------------------------------------
Subroutine MPI_GATHERV8_I4S(SENDDATA, SENDCOUNT, SENDTYPE, RECVAREA, &
                           RECVCOUNTS, DISPLS, RECVTYPE, ROOT, COMM, IERROR)

  Integer(Kind=JPIM), Dimension(:), Intent(IN) :: &
    SENDDATA
  Integer(Kind=JPIM), Intent(IN) :: &
    SENDCOUNT, SENDTYPE, RECVTYPE, ROOT, COMM, RECVCOUNTS, DISPLS
  Integer(Kind=JPIM), Intent(OUT) :: &
    RECVAREA, IERROR

  Integer(Kind=8), Dimension(:), Allocatable :: &
    SENDDATA8
  Integer(Kind=8) :: &
    RECVAREA8, RECVCOUNTS8, DISPLS8
  Integer(Kind=8) :: &
    SENDCOUNT8, SENDTYPE8, RECVTYPE8, ROOT8, COMM8, IERROR8

  Allocate(SENDDATA8(SIZE(SENDDATA)))

  SENDDATA8 = SENDDATA
  SENDCOUNT8 = SENDCOUNT
  SENDTYPE8 = SENDTYPE
  RECVCOUNTS8 = RECVCOUNTS
  DISPLS8 = DISPLS
  RECVTYPE8 = RECVTYPE
  ROOT8 = ROOT
  COMM8 = COMM

  Call MPI_GATHERV(SENDDATA8, SENDCOUNT8, SENDTYPE8, RECVAREA8, &
                   RECVCOUNTS8, DISPLS8, RECVTYPE8, ROOT8, COMM8, IERROR8)

  RECVAREA = RECVAREA8
  IERROR = IERROR8

  Deallocate(SENDDATA8)

End Subroutine MPI_GATHERV8_I4S

! ---------------------------------------------------------
Subroutine MPI_GATHERV8_I4S_1(SENDDATA, SENDCOUNT, SENDTYPE, RECVAREA, &
                           RECVCOUNTS, DISPLS, RECVTYPE, ROOT, COMM, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    SENDDATA
  Integer(Kind=JPIM), Intent(IN) :: &
    SENDCOUNT, SENDTYPE, RECVTYPE, ROOT, COMM, RECVCOUNTS, DISPLS
  Integer(Kind=JPIM), Intent(OUT) :: &
    RECVAREA, IERROR

  Integer(Kind=8) :: &
    SENDDATA8
  Integer(Kind=8) :: &
    RECVAREA8, RECVCOUNTS8, DISPLS8
  Integer(Kind=8) :: &
    SENDCOUNT8, SENDTYPE8, RECVTYPE8, ROOT8, COMM8, IERROR8

  SENDDATA8 = SENDDATA
  SENDCOUNT8 = SENDCOUNT
  SENDTYPE8 = SENDTYPE
  RECVCOUNTS8 = RECVCOUNTS
  DISPLS8 = DISPLS
  RECVTYPE8 = RECVTYPE
  ROOT8 = ROOT
  COMM8 = COMM

  Call MPI_GATHERV(SENDDATA8, SENDCOUNT8, SENDTYPE8, RECVAREA8, &
                   RECVCOUNTS8, DISPLS8, RECVTYPE8, ROOT8, COMM8, IERROR8)

  RECVAREA = RECVAREA8
  IERROR = IERROR8

End Subroutine MPI_GATHERV8_I4S_1

! ---------------------------------------------------------
Subroutine MPI_GATHERV8_I1(SENDDATA, SENDCOUNT, SENDTYPE, RECVAREA, &
                           RECVCOUNTS, DISPLS, RECVTYPE, ROOT, COMM, IERROR)

  Integer(Kind=JPIM), Dimension(:), Intent(IN) :: &
    RECVCOUNTS, DISPLS
  Integer(Kind=JPIM), Intent(IN) :: &
    SENDDATA, SENDCOUNT, SENDTYPE, RECVTYPE, ROOT, COMM
  Integer(Kind=JPIM), Intent(OUT) :: &
    RECVAREA, IERROR

  Integer(Kind=8) :: &
    SENDDATA8, RECVAREA8
  Integer(Kind=8), Dimension(:), Allocatable :: &
    RECVCOUNTS8, DISPLS8
  Integer(Kind=8) :: &
    SENDCOUNT8, SENDTYPE8, RECVTYPE8, ROOT8, COMM8, IERROR8

  Allocate(RECVCOUNTS8(SIZE(RECVCOUNTS)))
  Allocate(DISPLS8(SIZE(DISPLS)))

  SENDDATA8 = SENDDATA
  SENDCOUNT8 = SENDCOUNT
  SENDTYPE8 = SENDTYPE
  RECVCOUNTS8 = RECVCOUNTS
  DISPLS8 = DISPLS
  RECVTYPE8 = RECVTYPE
  ROOT8 = ROOT
  COMM8 = COMM

  Call MPI_GATHERV(SENDDATA8, SENDCOUNT8, SENDTYPE8, RECVAREA8, &
                   RECVCOUNTS8, DISPLS8, RECVTYPE8, ROOT8, COMM8, IERROR8)

  RECVAREA = RECVAREA8
  IERROR = IERROR8

  Deallocate(RECVCOUNTS8)
  Deallocate(DISPLS8)

End Subroutine MPI_GATHERV8_I1

! ---------------------------------------------------------
Subroutine MPI_GATHERV8_R8_1(SENDDATA, SENDCOUNT, SENDTYPE, RECVAREA, &
                             RECVCOUNTS, DISPLS, RECVTYPE, ROOT, COMM, IERROR)

  Real(Kind=JPRB), Intent(IN) :: &
    SENDDATA
  Integer(Kind=JPIM), Dimension(:), Intent(IN) :: &
    RECVCOUNTS, DISPLS
  Integer(Kind=JPIM), Intent(IN) :: &
    SENDCOUNT, SENDTYPE, RECVTYPE, ROOT, COMM
  Real(Kind=JPRB), Intent(OUT) :: &
    RECVAREA
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Integer(Kind=8), Dimension(:), Allocatable :: &
    RECVCOUNTS8, DISPLS8
  Integer(Kind=8) :: &
    SENDCOUNT8, SENDTYPE8, RECVTYPE8, ROOT8, COMM8, IERROR8

  Allocate(RECVCOUNTS8(SIZE(RECVCOUNTS)))
  Allocate(DISPLS8(SIZE(DISPLS)))

  SENDCOUNT8 = SENDCOUNT
  SENDTYPE8 = SENDTYPE
  RECVCOUNTS8 = RECVCOUNTS
  DISPLS8 = DISPLS
  RECVTYPE8 = RECVTYPE
  ROOT8 = ROOT
  COMM8 = COMM

  Call MPI_GATHERV(SENDDATA, SENDCOUNT8, SENDTYPE8, RECVAREA, &
                   RECVCOUNTS8, DISPLS8, RECVTYPE8, ROOT8, COMM8, IERROR8)

  IERROR = IERROR8

  Deallocate(RECVCOUNTS8)
  Deallocate(DISPLS8)

End Subroutine MPI_GATHERV8_R8_1

! =========================================================
Subroutine MPI_GATHERV8_R8S_1(SENDDATA, SENDCOUNT, SENDTYPE, RECVAREA, &
                             RECVCOUNTS, DISPLS, RECVTYPE, ROOT, COMM, IERROR)

  Real(Kind=JPRB), Intent(IN) :: &
    SENDDATA
  Integer(Kind=JPIM), Intent(IN) :: &
    RECVCOUNTS, DISPLS
  Integer(Kind=JPIM), Intent(IN) :: &
    SENDCOUNT, SENDTYPE, RECVTYPE, ROOT, COMM
  Real(Kind=JPRB), Intent(OUT) :: &
    RECVAREA
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Integer(Kind=8) :: &
    RECVCOUNTS8, DISPLS8
  Integer(Kind=8) :: &
    SENDCOUNT8, SENDTYPE8, RECVTYPE8, ROOT8, COMM8, IERROR8

  SENDCOUNT8 = SENDCOUNT
  SENDTYPE8 = SENDTYPE
  RECVCOUNTS8 = RECVCOUNTS
  DISPLS8 = DISPLS
  RECVTYPE8 = RECVTYPE
  ROOT8 = ROOT
  COMM8 = COMM

  Call MPI_GATHERV(SENDDATA, SENDCOUNT8, SENDTYPE8, RECVAREA, &
                   RECVCOUNTS8, DISPLS8, RECVTYPE8, ROOT8, COMM8, IERROR8)

  IERROR = IERROR8

End Subroutine MPI_GATHERV8_R8S_1

! =========================================================
! ---------------------------------------------------------
Subroutine MPI_RECV8_R4(DATA, COUNT, DATATYPE, SOURCE, TAG, COMM, STATUS, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, SOURCE, TAG, COMM
  Real(Kind=JPRM), Dimension(:), Intent(OUT) :: &
    DATA
  Integer(Kind=JPIM), Dimension(:), Intent(OUT) :: &
    STATUS
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Real(Kind=8), Dimension(:), Allocatable :: &
    DATA8
  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, SOURCE8, TAG8, COMM8, IERROR8

  Allocate(DATA8(SIZE(DATA)))

  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  SOURCE8 = SOURCE
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_RECV(DATA8, COUNT8, DATATYPE8, SOURCE8, TAG8, COMM8, STATUS8, IERROR8)

  DATA = DATA8
  STATUS = STATUS8
  IERROR = IERROR8

  Deallocate(DATA8)

End Subroutine MPI_RECV8_R4

! ---------------------------------------------------------
Subroutine MPI_RECV8_R8(DATA, COUNT, DATATYPE, SOURCE, TAG, COMM, STATUS, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, SOURCE, TAG, COMM
  Real(Kind=JPRB), Dimension(:), Intent(OUT) :: &
    DATA
  Integer(Kind=JPIM), Dimension(:), Intent(OUT) :: &
    STATUS
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, SOURCE8, TAG8, COMM8, IERROR8

  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  SOURCE8 = SOURCE
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_RECV(DATA, COUNT8, DATATYPE8, SOURCE8, TAG8, COMM8, STATUS8, IERROR8)

  STATUS = STATUS8
  IERROR = IERROR8

End Subroutine MPI_RECV8_R8

! ---------------------------------------------------------
Subroutine MPI_RECV8_R42(DATA, COUNT, DATATYPE, SOURCE, TAG, COMM, STATUS, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, SOURCE, TAG, COMM
  Real(Kind=JPRM), Dimension(:,:), Intent(OUT) :: &
    DATA
  Integer(Kind=JPIM), Dimension(:), Intent(OUT) :: &
    STATUS
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Real(Kind=8), Dimension(:,:), Allocatable :: &
    DATA8
  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, SOURCE8, TAG8, COMM8, IERROR8

  Allocate(DATA8(SIZE(DATA,Dim=1),SIZE(DATA,Dim=2)))

  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  SOURCE8 = SOURCE
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_RECV(DATA8, COUNT8, DATATYPE8, SOURCE8, TAG8, COMM8, STATUS8, IERROR8)

  DATA = DATA8
  STATUS = STATUS8
  IERROR = IERROR8

  Deallocate(DATA8)

End Subroutine MPI_RECV8_R42

! ---------------------------------------------------------
Subroutine MPI_RECV8_I42(DATA, COUNT, DATATYPE, SOURCE, TAG, COMM, STATUS, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, SOURCE, TAG, COMM
  Integer(Kind=JPIM), Dimension(:,:), Intent(OUT) :: &
    DATA
  Integer(Kind=JPIM), Dimension(:), Intent(OUT) :: &
    STATUS
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Integer(Kind=8), Dimension(:,:), Allocatable :: &
    DATA8
  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, SOURCE8, TAG8, COMM8, IERROR8

  Allocate(DATA8(SIZE(DATA,Dim=1),SIZE(DATA,Dim=2)))

  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  SOURCE8 = SOURCE
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_RECV(DATA8, COUNT8, DATATYPE8, SOURCE8, TAG8, COMM8, STATUS8, IERROR8)

  DATA = DATA8
  STATUS = STATUS8
  IERROR = IERROR8

  Deallocate(DATA8)

End Subroutine MPI_RECV8_I42

! ---------------------------------------------------------
Subroutine MPI_RECV8_R82(DATA, COUNT, DATATYPE, SOURCE, TAG, COMM, STATUS, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, SOURCE, TAG, COMM
  Real(Kind=JPRB), Dimension(:,:), Intent(OUT) :: &
    DATA
  Integer(Kind=JPIM), Dimension(:), Intent(OUT) :: &
    STATUS
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, SOURCE8, TAG8, COMM8, IERROR8

  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  SOURCE8 = SOURCE
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_RECV(DATA, COUNT8, DATATYPE8, SOURCE8, TAG8, COMM8, STATUS8, IERROR8)

  STATUS = STATUS8
  IERROR = IERROR8

End Subroutine MPI_RECV8_R82

! ---------------------------------------------------------
Subroutine MPI_RECV8_I4(DATA, COUNT, DATATYPE, SOURCE, TAG, COMM, STATUS, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, SOURCE, TAG, COMM
  Integer(Kind=JPIM), Dimension(:), Intent(OUT) :: &
    DATA, STATUS
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Integer(Kind=8), Dimension(:), Allocatable :: &
    DATA8
  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, SOURCE8, TAG8, COMM8, IERROR8

  Allocate(DATA8(SIZE(DATA)))

  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  SOURCE8 = SOURCE
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_RECV(DATA8, COUNT8, DATATYPE8, SOURCE8, TAG8, COMM8, STATUS8, IERROR8)

  DATA = DATA8
  STATUS = STATUS8
  IERROR = IERROR8

  Deallocate(DATA8)

End Subroutine MPI_RECV8_I4

! ---------------------------------------------------------
Subroutine MPI_RECV8_I1(DATA, COUNT, DATATYPE, SOURCE, TAG, COMM, STATUS, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, SOURCE, TAG, COMM
  Integer(Kind=JPIM), Dimension(:), Intent(OUT) :: &
    STATUS
  Integer(Kind=JPIM), Intent(OUT) :: &
    DATA, IERROR

  Integer(Kind=8) :: &
    DATA8, COUNT8, DATATYPE8, SOURCE8, TAG8, COMM8, IERROR8

  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  SOURCE8 = SOURCE
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_RECV(DATA8, COUNT8, DATATYPE8, SOURCE8, TAG8, COMM8, STATUS8, IERROR8)

  DATA = DATA8
  STATUS = STATUS8
  IERROR = IERROR8

End Subroutine MPI_RECV8_I1

! ---------------------------------------------------------
Subroutine MPI_RECV8_R4_1(DATA, COUNT, DATATYPE, SOURCE, TAG, COMM, STATUS, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, SOURCE, TAG, COMM
  Integer(Kind=JPIM), Dimension(:), Intent(OUT) :: &
    STATUS
  Real(Kind=JPRM), Intent(OUT) :: &
    DATA
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Real(Kind=8) :: &
    DATA8
  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, SOURCE8, TAG8, COMM8, IERROR8

  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  SOURCE8 = SOURCE
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_RECV(DATA8, COUNT8, DATATYPE8, SOURCE8, TAG8, COMM8, STATUS8, IERROR8)

  DATA = DATA8
  STATUS = STATUS8
  IERROR = IERROR8

End Subroutine MPI_RECV8_R4_1

! ---------------------------------------------------------
Subroutine MPI_RECV8_R8_1(DATA, COUNT, DATATYPE, SOURCE, TAG, COMM, STATUS, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, SOURCE, TAG, COMM
  Integer(Kind=JPIM), Dimension(:), Intent(OUT) :: &
    STATUS
  Real(Kind=JPRB), Intent(OUT) :: &
    DATA
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, SOURCE8, TAG8, COMM8, IERROR8

  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  SOURCE8 = SOURCE
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_RECV(DATA, COUNT8, DATATYPE8, SOURCE8, TAG8, COMM8, STATUS8, IERROR8)

  STATUS = STATUS8
  IERROR = IERROR8

End Subroutine MPI_RECV8_R8_1

! ---------------------------------------------------------
Subroutine MPI_RECV8_CH(DATA, COUNT, DATATYPE, SOURCE, TAG, COMM, STATUS, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, SOURCE, TAG, COMM
  Integer(Kind=JPIM), Dimension(:), Intent(OUT) :: &
    STATUS
  Character(Len=*), Intent(OUT) :: &
    DATA
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, SOURCE8, TAG8, COMM8, IERROR8

  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  SOURCE8 = SOURCE
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_RECV(DATA, COUNT8, DATATYPE8, SOURCE8, TAG8, COMM8, STATUS8, IERROR8)

  STATUS = STATUS8
  IERROR = IERROR8

End Subroutine MPI_RECV8_CH

! ---------------------------------------------------------
Subroutine MPI_IRECV8_R4(DATA, COUNT, DATATYPE, SOURCE, TAG, COMM, REQUEST, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, SOURCE, TAG, COMM
  Real(Kind=JPRM), Dimension(:), Intent(OUT) :: &
    DATA
  Integer(Kind=JPIM), Intent(OUT) :: &
    REQUEST, IERROR

  Real(Kind=8), Dimension(:), Allocatable :: &
    DATA8
  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, SOURCE8, TAG8, COMM8, REQUEST8, IERROR8

  Allocate(DATA8(SIZE(DATA)))

  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  SOURCE8 = SOURCE
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_IRECV(DATA8, COUNT8, DATATYPE8, SOURCE8, TAG8, COMM8, REQUEST8, IERROR8)

  DATA = DATA8
  REQUEST = REQUEST8
  IERROR = IERROR8

  Deallocate(DATA8)

End Subroutine MPI_IRECV8_R4

! ---------------------------------------------------------
Subroutine MPI_IRECV8_R8(DATA, COUNT, DATATYPE, SOURCE, TAG, COMM, REQUEST, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, SOURCE, TAG, COMM
  Real(Kind=JPRB), Dimension(:), Intent(OUT) :: &
    DATA
  Integer(Kind=JPIM), Intent(OUT) :: &
    REQUEST, IERROR

  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, SOURCE8, TAG8, COMM8, REQUEST8, IERROR8

  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  SOURCE8 = SOURCE
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_IRECV(DATA, COUNT8, DATATYPE8, SOURCE8, TAG8, COMM8, REQUEST8, IERROR8)

  REQUEST = REQUEST8
  IERROR = IERROR8

End Subroutine MPI_IRECV8_R8

! ---------------------------------------------------------
Subroutine MPI_IRECV8_R42(DATA, COUNT, DATATYPE, SOURCE, TAG, COMM, REQUEST, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, SOURCE, TAG, COMM
  Real(Kind=JPRM), Dimension(:,:), Intent(OUT) :: &
    DATA
  Integer(Kind=JPIM), Intent(OUT) :: &
    REQUEST, IERROR

  Real(Kind=8), Dimension(:,:), Allocatable :: &
    DATA8
  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, SOURCE8, TAG8, COMM8, REQUEST8, IERROR8

  Allocate(DATA8(SIZE(DATA,Dim=1),SIZE(DATA,Dim=2)))

  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  SOURCE8 = SOURCE
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_IRECV(DATA8, COUNT8, DATATYPE8, SOURCE8, TAG8, COMM8, REQUEST8, IERROR8)

  DATA = DATA8
  REQUEST = REQUEST8
  IERROR = IERROR8

  Deallocate(DATA8)

End Subroutine MPI_IRECV8_R42

! ---------------------------------------------------------
Subroutine MPI_IRECV8_R82(DATA, COUNT, DATATYPE, SOURCE, TAG, COMM, REQUEST, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, SOURCE, TAG, COMM
  Real(Kind=JPRB), Dimension(:,:), Intent(OUT) :: &
    DATA
  Integer(Kind=JPIM), Intent(OUT) :: &
    REQUEST, IERROR

  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, SOURCE8, TAG8, COMM8, REQUEST8, IERROR8

  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  SOURCE8 = SOURCE
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_IRECV(DATA, COUNT8, DATATYPE8, SOURCE8, TAG8, COMM8, REQUEST8, IERROR8)

  REQUEST = REQUEST8
  IERROR = IERROR8

End Subroutine MPI_IRECV8_R82

! ---------------------------------------------------------
Subroutine MPI_IRECV8_I4(DATA, COUNT, DATATYPE, SOURCE, TAG, COMM, REQUEST, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, SOURCE, TAG, COMM
  Integer(Kind=JPIM), Intent(OUT) :: &
    DATA(:), REQUEST, IERROR

  Integer(Kind=8), Dimension(:), Allocatable :: &
    DATA8
  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, SOURCE8, TAG8, COMM8, REQUEST8, IERROR8

  Allocate(DATA8(SIZE(DATA)))

  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  SOURCE8 = SOURCE
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_IRECV(DATA8, COUNT8, DATATYPE8, SOURCE8, TAG8, COMM8, REQUEST8, IERROR8)

  DATA = DATA8
  REQUEST = REQUEST8
  IERROR = IERROR8

  Deallocate(DATA8)

End Subroutine MPI_IRECV8_I4

! ---------------------------------------------------------
Subroutine MPI_IRECV8_I42(DATA, COUNT, DATATYPE, SOURCE, TAG, COMM, REQUEST, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, SOURCE, TAG, COMM
  Integer(Kind=JPIM), Dimension(:,:), Intent(OUT) :: &
    DATA
  Integer(Kind=JPIM), Intent(OUT) :: &
    REQUEST, IERROR

  Integer(Kind=8), Dimension(:,:), Allocatable :: &
    DATA8
  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, SOURCE8, TAG8, COMM8, REQUEST8, IERROR8

  Allocate(DATA8(SIZE(DATA,Dim=1),SIZE(DATA,Dim=2)))

  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  SOURCE8 = SOURCE
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_IRECV(DATA8, COUNT8, DATATYPE8, SOURCE8, TAG8, COMM8, REQUEST8, IERROR8)

  DATA = DATA8
  REQUEST = REQUEST8
  IERROR = IERROR8

  Deallocate(DATA8)

End Subroutine MPI_IRECV8_I42

! ---------------------------------------------------------
Subroutine MPI_IRECV8_I1(DATA, COUNT, DATATYPE, SOURCE, TAG, COMM, REQUEST, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, SOURCE, TAG, COMM
  Integer(Kind=JPIM), Intent(OUT) :: &
    DATA, REQUEST, IERROR

  Integer(Kind=8) :: &
    DATA8, COUNT8, DATATYPE8, SOURCE8, TAG8, COMM8, REQUEST8, IERROR8

  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  SOURCE8 = SOURCE
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_IRECV(DATA8, COUNT8, DATATYPE8, SOURCE8, TAG8, COMM8, REQUEST8, IERROR8)

  DATA = DATA8
  REQUEST = REQUEST8
  IERROR = IERROR8

End Subroutine MPI_IRECV8_I1

! ---------------------------------------------------------
Subroutine MPI_IRECV8_R4_1(DATA, COUNT, DATATYPE, SOURCE, TAG, COMM, REQUEST, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, SOURCE, TAG, COMM
  Real(Kind=JPRM), Intent(OUT) :: &
    DATA
  Integer(Kind=JPIM), Intent(OUT) :: &
    REQUEST, IERROR

  Real(Kind=8) :: &
    DATA8
  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, SOURCE8, TAG8, COMM8, REQUEST8, IERROR8

  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  SOURCE8 = SOURCE
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_IRECV(DATA8, COUNT8, DATATYPE8, SOURCE8, TAG8, COMM8, REQUEST8, IERROR8)

  DATA = DATA8
  REQUEST = REQUEST8
  IERROR = IERROR8

End Subroutine MPI_IRECV8_R4_1

! ---------------------------------------------------------
Subroutine MPI_IRECV8_R8_1(DATA, COUNT, DATATYPE, SOURCE, TAG, COMM, REQUEST, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, SOURCE, TAG, COMM
  Real(Kind=JPRB), Intent(OUT) :: &
    DATA
  Integer(Kind=JPIM), Intent(OUT) :: &
    REQUEST, IERROR

  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, SOURCE8, TAG8, COMM8, REQUEST8, IERROR8

  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  SOURCE8 = SOURCE
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_IRECV(DATA, COUNT8, DATATYPE8, SOURCE8, TAG8, COMM8, REQUEST8, IERROR8)

  REQUEST = REQUEST8
  IERROR = IERROR8

End Subroutine MPI_IRECV8_R8_1

! ---------------------------------------------------------
Subroutine MPI_IRECV8_CH(DATA, COUNT, DATATYPE, SOURCE, TAG, COMM, REQUEST, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT, DATATYPE, SOURCE, TAG, COMM
  Character(Len=*), Intent(OUT) :: &
    DATA
  Integer(Kind=JPIM), Intent(OUT) :: &
    REQUEST, IERROR

  Integer(Kind=8) :: &
    COUNT8, DATATYPE8, SOURCE8, TAG8, COMM8, REQUEST8, IERROR8

  COUNT8 = COUNT
  DATATYPE8 = DATATYPE
  SOURCE8 = SOURCE
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_IRECV(DATA, COUNT8, DATATYPE8, SOURCE8, TAG8, COMM8, REQUEST8, IERROR8)

  REQUEST = REQUEST8
  IERROR = IERROR8

End Subroutine MPI_IRECV8_CH

! =========================================================
! ---------------------------------------------------------
Subroutine MPI_SCATTERV8_R8(SENDAREA, SENDCOUNTS, DISPLS, SENDTYPE, &
                            RECVDATA, RECVCOUNT, RECVTYPE, ROOT, COMM, IERROR)
  
  Real(Kind=JPRB), Dimension(:), Intent(IN) :: &
    SENDAREA
  Integer(Kind=JPIM), Dimension(:), Intent(IN) :: &
    SENDCOUNTS, DISPLS
  Integer(Kind=JPIM), Intent(IN) :: &
    SENDTYPE, RECVCOUNT, RECVTYPE, ROOT, COMM
  Real(Kind=JPRB), Dimension(:), Intent(OUT) :: &
    RECVDATA
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Real(Kind=8), Dimension(:), Allocatable :: &
    SENDAREA8, RECVDATA8
  Integer(Kind=8), Dimension(:), Allocatable :: &
    SENDCOUNTS8, DISPLS8
  Integer(Kind=8) :: &
    SENDTYPE8, RECVCOUNT8, RECVTYPE8, ROOT8, COMM8, IERROR8

  Allocate(SENDAREA8(SIZE(SENDAREA)))
  Allocate(SENDCOUNTS8(SIZE(SENDCOUNTS)))
  Allocate(DISPLS8(SIZE(DISPLS)))
  Allocate(RECVDATA8(SIZE(RECVDATA)))

  SENDAREA8 = SENDAREA
  SENDCOUNTS8 = SENDCOUNTS
  DISPLS8 = DISPLS
  SENDTYPE8 = SENDTYPE
  RECVCOUNT8 = RECVCOUNT
  RECVTYPE8 = RECVTYPE
  ROOT8 = ROOT
  COMM8 = COMM

  Call MPI_SCATTERV(SENDAREA8, SENDCOUNTS8, DISPLS8, SENDTYPE8, &
                    RECVDATA8, RECVCOUNT8, RECVTYPE8, ROOT8, COMM8, IERROR8)

  RECVDATA = RECVDATA8
  IERROR = IERROR8

  Deallocate(SENDAREA8)
  Deallocate(SENDCOUNTS8)
  Deallocate(DISPLS8)
  Deallocate(RECVDATA8)

End Subroutine MPI_SCATTERV8_R8

! ---------------------------------------------------------
Subroutine MPI_SCATTERV8_R8S(SENDAREA, SENDCOUNTS, DISPLS, SENDTYPE, &
                            RECVDATA, RECVCOUNT, RECVTYPE, ROOT, COMM, IERROR)
  
  Real(Kind=JPRB), Intent(IN) :: &
    SENDAREA
  Integer(Kind=JPIM), Intent(IN) :: &
    SENDTYPE, RECVCOUNT, RECVTYPE, ROOT, COMM, SENDCOUNTS, DISPLS
  Real(Kind=JPRB), Dimension(:), Intent(OUT) :: &
    RECVDATA
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Real(Kind=8), Dimension(:), Allocatable :: &
    RECVDATA8
  Real(Kind=8) :: &
    SENDAREA8
  Integer(Kind=8) :: &
    SENDCOUNTS8, DISPLS8
  Integer(Kind=8) :: &
    SENDTYPE8, RECVCOUNT8, RECVTYPE8, ROOT8, COMM8, IERROR8

  Allocate(RECVDATA8(SIZE(RECVDATA)))

  SENDAREA8 = SENDAREA
  SENDCOUNTS8 = SENDCOUNTS
  DISPLS8 = DISPLS
  SENDTYPE8 = SENDTYPE
  RECVCOUNT8 = RECVCOUNT
  RECVTYPE8 = RECVTYPE
  ROOT8 = ROOT
  COMM8 = COMM

  Call MPI_SCATTERV(SENDAREA8, SENDCOUNTS8, DISPLS8, SENDTYPE8, &
                    RECVDATA8, RECVCOUNT8, RECVTYPE8, ROOT8, COMM8, IERROR8)

  RECVDATA = RECVDATA8
  IERROR = IERROR8

  Deallocate(RECVDATA8)

End Subroutine MPI_SCATTERV8_R8S

! ---------------------------------------------------------
Subroutine MPI_SCATTERV8_I4(SENDAREA, SENDCOUNTS, DISPLS, SENDTYPE, &
                            RECVDATA, RECVCOUNT, RECVTYPE, ROOT, COMM, IERROR)
  
  Integer(Kind=JPIM), Dimension(:), Intent(IN) :: &
    SENDAREA, SENDCOUNTS, DISPLS
  Integer(Kind=JPIM), Intent(IN) :: &
    SENDTYPE, RECVCOUNT, RECVTYPE, ROOT, COMM
  Integer(Kind=JPIM), Dimension(:), Intent(OUT) :: &
    RECVDATA
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Integer(Kind=8), Dimension(:), Allocatable :: &
    SENDAREA8, SENDCOUNTS8, DISPLS8, RECVDATA8
  Integer(Kind=8) :: &
    SENDTYPE8, RECVCOUNT8, RECVTYPE8, ROOT8, COMM8, IERROR8

  Allocate(SENDAREA8(SIZE(SENDAREA)))
  Allocate(SENDCOUNTS8(SIZE(SENDCOUNTS)))
  Allocate(DISPLS8(SIZE(DISPLS)))
  Allocate(RECVDATA8(SIZE(RECVDATA)))

  SENDAREA8 = SENDAREA
  SENDCOUNTS8 = SENDCOUNTS
  DISPLS8 = DISPLS
  SENDTYPE8 = SENDTYPE
  RECVCOUNT8 = RECVCOUNT
  RECVTYPE8 = RECVTYPE
  ROOT8 = ROOT
  COMM8 = COMM

  Call MPI_SCATTERV(SENDAREA8, SENDCOUNTS8, DISPLS8, SENDTYPE8, &
                    RECVDATA8, RECVCOUNT8, RECVTYPE8, ROOT8, COMM8, IERROR8)

  RECVDATA = RECVDATA8
  IERROR = IERROR8

  Deallocate(SENDAREA8)
  Deallocate(SENDCOUNTS8)
  Deallocate(DISPLS8)
  Deallocate(RECVDATA8)

End Subroutine MPI_SCATTERV8_I4

! =========================================================
Subroutine MPI_SCATTERV8_I4S(SENDAREA, SENDCOUNTS, DISPLS, SENDTYPE, &
                            RECVDATA, RECVCOUNT, RECVTYPE, ROOT, COMM, IERROR)
  
  Integer(Kind=JPIM), Intent(IN) :: &
    SENDAREA, SENDCOUNTS, DISPLS
  Integer(Kind=JPIM), Intent(IN) :: &
    SENDTYPE, RECVCOUNT, RECVTYPE, ROOT, COMM
  Integer(Kind=JPIM), Dimension(:), Intent(OUT) :: &
    RECVDATA
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Integer(Kind=8) :: &
    SENDAREA8, SENDCOUNTS8, DISPLS8
  Integer(Kind=8), Dimension(:), Allocatable :: &
    RECVDATA8
  Integer(Kind=8) :: &
    SENDTYPE8, RECVCOUNT8, RECVTYPE8, ROOT8, COMM8, IERROR8

  Allocate(RECVDATA8(SIZE(RECVDATA)))

  SENDAREA8 = SENDAREA
  SENDCOUNTS8 = SENDCOUNTS
  DISPLS8 = DISPLS
  SENDTYPE8 = SENDTYPE
  RECVCOUNT8 = RECVCOUNT
  RECVTYPE8 = RECVTYPE
  ROOT8 = ROOT
  COMM8 = COMM

  Call MPI_SCATTERV(SENDAREA8, SENDCOUNTS8, DISPLS8, SENDTYPE8, &
                    RECVDATA8, RECVCOUNT8, RECVTYPE8, ROOT8, COMM8, IERROR8)

  RECVDATA = RECVDATA8
  IERROR = IERROR8

  Deallocate(RECVDATA8)

End Subroutine MPI_SCATTERV8_I4S

! =========================================================
! =========================================================
! =========================================================

End Module MPI4TO8_M

Module MPI4TO8_S

Use PARKIND1, Only : JPIM
USE MPL_MPIF

Implicit None

Private :: STATUS8
  Integer(Kind=8), Dimension(MPI_STATUS_SIZE) :: STATUS8

Interface MPI_GET_COUNT8
  Module Procedure MPI_GET_COUNT8_I4, MPI_GET_COUNT8_I4_1
End Interface MPI_GET_COUNT8

Interface MPI_WAITALL8
  Module Procedure MPI_WAITALL8_I4, MPI_WAITALL8_I4_1
End Interface MPI_WAITALL8

Interface MPI_WAIT8
  Module Procedure MPI_WAIT8_I4, MPI_WAIT8_I4_1
End Interface MPI_WAIT8

Public

Contains

! ---------------------------------------------------------
Subroutine MPI_ABORT8(COMM, ERRORCODE, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    COMM, ERRORCODE
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Integer(Kind=8) :: &
    COMM8, ERRORCODE8, IERROR8

  COMM8 = COMM
  ERRORCODE8 = ERRORCODE

  Call MPI_ABORT(COMM8, ERRORCODE8, IERROR8)

  IERROR = IERROR8

End Subroutine MPI_ABORT8

! ---------------------------------------------------------
Subroutine MPI_BARRIER8(COMM, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    COMM
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Integer(Kind=8) :: &
    COMM8, IERROR8

  COMM8 = COMM

  Call MPI_BARRIER(COMM8, IERROR8)

  IERROR = IERROR8

End Subroutine MPI_BARRIER8

! ---------------------------------------------------------
Subroutine MPI_BUFFER_DETACH8(BUFFER_ADDR, SZ, IERROR)

  Integer(Kind=JPIM), Dimension(:), Intent(OUT) :: &
    BUFFER_ADDR
  Integer(Kind=JPIM), Intent(OUT) :: &
    SZ, IERROR

  Integer(Kind=8), Dimension(:), Allocatable :: &
    BUFFER_ADDR8
  Integer(Kind=8) :: &
    SZ8, IERROR8

  Allocate(BUFFER_ADDR8(SIZE(BUFFER_ADDR)))

  Call MPI_BUFFER_DETACH(BUFFER_ADDR8, SZ8, IERROR8)

  BUFFER_ADDR = BUFFER_ADDR8
  SZ = SZ8
  IERROR = IERROR8

  Deallocate(BUFFER_ADDR8)

End Subroutine MPI_BUFFER_DETACH8

! ---------------------------------------------------------
Subroutine MPI_BUFFER_ATTACH8(BUFFER_ADDR, SZ, IERROR)

  Integer(Kind=JPIM), Dimension(:), Intent(IN) :: &
    BUFFER_ADDR
  Integer(Kind=JPIM), Intent(IN) :: &
    SZ
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Integer(Kind=8), Dimension(:), Allocatable :: &
    BUFFER_ADDR8
  Integer(Kind=8) :: &
    SZ8, IERROR8

  Allocate(BUFFER_ADDR8(SIZE(BUFFER_ADDR)))

  BUFFER_ADDR8 = BUFFER_ADDR
  SZ8 = SZ

  Call MPI_BUFFER_ATTACH(BUFFER_ADDR8, SZ8, IERROR8)

  IERROR = IERROR8

  Deallocate(BUFFER_ADDR8)

End Subroutine MPI_BUFFER_ATTACH8

! ---------------------------------------------------------
Subroutine MPI_CART_COORDS8(COMM, RANK, MAXDIMS, COORDS, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    COMM, RANK, MAXDIMS
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR
  Integer(Kind=JPIM), Dimension(:), Intent(OUT) :: COORDS

  Integer(Kind=8), Dimension(:), Allocatable :: COORDS8
  Integer(Kind=8) :: &
    COMM8, RANK8, MAXDIMS8, IERROR8

  Allocate(COORDS8(Size(COORDS)))

  COMM8 = COMM
  RANK8 = RANK
  MAXDIMS8 = MAXDIMS

  Call MPI_CART_COORDS(COMM8, RANK8, MAXDIMS8, COORDS8, IERROR8)

  COORDS = COORDS8
  IERROR = IERROR8

  Deallocate(COORDS8)

End Subroutine MPI_CART_COORDS8

! ---------------------------------------------------------
Subroutine MPI_CART_CREATE8(COMM_OLD, NDIMS, DIMS, PERIODS, REORDER, COMM_CART, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    COMM_OLD, NDIMS, DIMS(:)
  Logical(Kind=JPIM), Intent(IN) :: &
    PERIODS(:), REORDER
  Integer(Kind=JPIM), Intent(OUT) :: &
    COMM_CART, IERROR

  Integer(Kind=8), Dimension(:), Allocatable :: &
    DIMS8
  Integer(Kind=8) :: &
    COMM_OLD8, NDIMS8, COMM_CART8, IERROR8
  Logical(Kind=8), Dimension(:), Allocatable :: &
    PERIODS8
  Logical(Kind=8) :: &
    REORDER8

  Allocate(DIMS8(SIZE(DIMS)))
  Allocate(PERIODS8(SIZE(PERIODS)))

  COMM_OLD8 = COMM_OLD
  NDIMS8 = NDIMS
  DIMS8 = DIMS
  PERIODS8 = PERIODS
  REORDER8 = REORDER

  Call MPI_CART_CREATE(COMM_OLD8, NDIMS8, DIMS8, PERIODS8, REORDER8, COMM_CART8, IERROR8)

  COMM_CART =  COMM_CART8
  IERROR  = IERROR8

  Deallocate(DIMS8)
  Deallocate(PERIODS8)

End Subroutine MPI_CART_CREATE8

! ---------------------------------------------------------
Subroutine MPI_CART_RANK8(COMM, COORDS, RANK, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    COMM
  Integer(Kind=JPIM), Intent(IN), Dimension(:) :: COORDS
  Integer(Kind=JPIM), Intent(OUT) :: &
    RANK, IERROR

  Integer(Kind=8), Dimension(:), Allocatable :: COORDS8
  Integer(Kind=8) :: &
    COMM8, RANK8, IERROR8

  Allocate(COORDS8(Size(COORDS)))

  COMM8 = COMM
  COORDS8 = COORDS

  Call MPI_CART_RANK(COMM8, COORDS8, RANK8, IERROR8)

  RANK = RANK8
  IERROR = IERROR8

  Deallocate(COORDS8)

End Subroutine MPI_CART_RANK8

! ---------------------------------------------------------
Subroutine MPI_CART_SUB8(COMM, REMAIN_DIMS, NEWCOMM, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    COMM
  Logical(Kind=JPIM), Intent(IN), Dimension(:) :: &
    REMAIN_DIMS
  Integer(Kind=JPIM), Intent(OUT) :: &
    NEWCOMM, IERROR

  Integer(Kind=8) :: &
    COMM8, NEWCOMM8, IERROR8
  Logical(Kind=8), Dimension(:), Allocatable :: &
    REMAIN_DIMS8

  Allocate(REMAIN_DIMS8(Size(REMAIN_DIMS)))

  COMM8 = COMM
  REMAIN_DIMS8 = REMAIN_DIMS

  Call MPI_CART_SUB(COMM8, REMAIN_DIMS8, NEWCOMM8, IERROR8)

  NEWCOMM = NEWCOMM8
  IERROR = IERROR8

  Deallocate(REMAIN_DIMS8)

End Subroutine MPI_CART_SUB8

! ---------------------------------------------------------
Subroutine MPI_COMM_CREATE8(COMM, GROUP, NEWCOMM, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    COMM, GROUP
  Integer(Kind=JPIM), Intent(OUT) :: &
    NEWCOMM, IERROR

  Integer(Kind=8) :: &
    COMM8, GROUP8, NEWCOMM8, IERROR8

  COMM8 = COMM
  GROUP8 = GROUP

  Call MPI_COMM_CREATE(COMM8, GROUP8, NEWCOMM8, IERROR8)

  NEWCOMM = NEWCOMM8
  IERROR = IERROR8

End Subroutine MPI_COMM_CREATE8

! ---------------------------------------------------------
Subroutine MPI_COMM_GROUP8(COMM, GROUP, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    COMM
  Integer(Kind=JPIM), Intent(OUT) :: &
    GROUP, IERROR

  Integer(Kind=8) :: &
    COMM8, GROUP8, IERROR8

  COMM8 = COMM

  Call MPI_COMM_GROUP(COMM8, GROUP8, IERROR8)

  GROUP = GROUP8
  IERROR = IERROR8

End Subroutine MPI_COMM_GROUP8

! ---------------------------------------------------------
Subroutine MPI_COMM_RANK8(COMM, RANK, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    COMM
  Integer(Kind=JPIM), Intent(OUT) :: &
    RANK, IERROR

  Integer(Kind=8) :: &
    COMM8, RANK8, IERROR8

  COMM8 = COMM

  Call MPI_COMM_RANK(COMM8, RANK8, IERROR8)

  RANK = RANK8
  IERROR = IERROR8

End Subroutine MPI_COMM_RANK8

! ---------------------------------------------------------
Subroutine MPI_COMM_SIZE8(COMM, SIZE, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    COMM
  Integer(Kind=JPIM), Intent(OUT) :: &
    SIZE, IERROR

  Integer(Kind=8) :: &
    COMM8, SIZE8, IERROR8

  COMM8 = COMM

  Call MPI_COMM_SIZE(COMM8, SIZE8, IERROR8)

  SIZE = SIZE8
  IERROR = IERROR8

End Subroutine MPI_COMM_SIZE8

! ---------------------------------------------------------
Subroutine MPI_COMM_SPLIT8(COMM, COLOR, KEY, NEWCOMM, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    COMM, COLOR, KEY
  Integer(Kind=JPIM), Intent(OUT) :: &
    NEWCOMM, IERROR

  Integer(Kind=8) :: &
    COMM8, COLOR8, KEY8, NEWCOMM8, IERROR8

  COMM8 = COMM
  COLOR8 = COLOR
  KEY8 = KEY

  Call MPI_COMM_SPLIT(COMM8, COLOR8, KEY8, NEWCOMM8, IERROR8)

  NEWCOMM = NEWCOMM8
  IERROR = IERROR8

End Subroutine MPI_COMM_SPLIT8

! ---------------------------------------------------------
Subroutine MPI_ERROR_STRING8(ERRORCODE, STRING, RESULTLEN, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    ERRORCODE
  Character(Len=*), Intent(OUT) :: &
    STRING
  Integer(Kind=JPIM), Intent(OUT) :: &
    RESULTLEN, IERROR

  Integer(Kind=8) :: &
    ERRORCODE8, RESULTLEN8, IERROR8

  ERRORCODE8 = ERRORCODE

  Call MPI_ERROR_STRING(ERRORCODE8, STRING, RESULTLEN8, IERROR8)

  RESULTLEN = RESULTLEN8
  IERROR = IERROR8

End Subroutine MPI_ERROR_STRING8

! ---------------------------------------------------------
Subroutine MPI_FILE_CLOSE8(FH, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    FH
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Integer(Kind=8) :: &
    FH8, IERROR8

  FH8 = FH

  Call MPI_FILE_CLOSE(FH8, IERROR8)

  IERROR = IERROR8

End Subroutine MPI_FILE_CLOSE8

! ---------------------------------------------------------
Subroutine MPI_FILE_OPEN8(COMM, FILENAME, AMODE, INFO, FH, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    COMM, AMODE, INFO
  Character(Len=*), Intent(IN) :: &
    FILENAME
  Integer(Kind=JPIM), Intent(OUT) :: &
    FH, IERROR

  Integer(Kind=8) :: &
    COMM8, AMODE8, INFO8, FH8, IERROR8

  COMM8 = COMM
  AMODE8 = AMODE
  INFO8 = INFO

  Call MPI_FILE_OPEN(COMM8, FILENAME, AMODE8, INFO8, FH8, IERROR8)

  FH = FH8
  IERROR = IERROR8

End Subroutine MPI_FILE_OPEN8

! ---------------------------------------------------------
Subroutine MPI_FINALIZE8(IERROR)

  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Integer(Kind=8) :: &
    IERROR8

  Call MPI_FINALIZE(IERROR8)

  IERROR = IERROR8

End Subroutine MPI_FINALIZE8

! ---------------------------------------------------------
Subroutine MPI_GET_COUNT8_I4(STATUS, DATATYPE, COUNT, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    STATUS(:), DATATYPE
  Integer(Kind=JPIM), Intent(OUT) :: &
    COUNT, IERROR

  Integer(Kind=8) :: &
    DATATYPE8, COUNT8, IERROR8

  STATUS8 = STATUS
  DATATYPE8 = DATATYPE

  Call MPI_GET_COUNT(STATUS8, DATATYPE8, COUNT8, IERROR8)

  COUNT = COUNT8
  IERROR = IERROR8

End Subroutine MPI_GET_COUNT8_I4

! ---------------------------------------------------------
Subroutine MPI_GET_COUNT8_I4_1(STATUS, DATATYPE, COUNT, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    STATUS, DATATYPE
  Integer(Kind=JPIM), Intent(OUT) :: &
    COUNT, IERROR

  Integer(Kind=8) :: &
    DATATYPE8, COUNT8, IERROR8, STATUS8

  STATUS8 = STATUS
  DATATYPE8 = DATATYPE

  Call MPI_GET_COUNT(STATUS8, DATATYPE8, COUNT8, IERROR8)

  COUNT = COUNT8
  IERROR = IERROR8

End Subroutine MPI_GET_COUNT8_I4_1

! ---------------------------------------------------------
Subroutine MPI_GROUP_INCL8(GROUP1, N, RANKS, NEWGROUP, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    GROUP1, N, RANKS(:)
  Integer(Kind=JPIM), Intent(OUT) :: &
    NEWGROUP, IERROR

  Integer(Kind=8), Dimension(:), Allocatable :: &
    RANKS8
  Integer(Kind=8) :: &
    GROUP18, N8, NEWGROUP8, IERROR8

  Allocate(RANKS8(SIZE(RANKS)))

  GROUP18 = GROUP1
  N8 = N
  RANKS8 = RANKS

  Call MPI_GROUP_INCL(GROUP18, N8, RANKS8, NEWGROUP8, IERROR8)

  NEWGROUP = NEWGROUP8
  IERROR = IERROR8

  Deallocate(RANKS8)

End Subroutine MPI_GROUP_INCL8

! ---------------------------------------------------------
Subroutine MPI_INIT8(IERROR)

  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Integer(Kind=8) :: &
    IERROR8

  Call MPI_INIT(IERROR8)

  IERROR = IERROR8

End Subroutine MPI_INIT8

! ---------------------------------------------------------
Subroutine MPI_INITIALIZED8(FLAG, IERROR)

  Logical(Kind=JPIM), Intent(OUT) :: &
    FLAG
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Logical(Kind=8) :: &
    FLAG8
  Integer(Kind=8) :: &
    IERROR8


  Call MPI_INITIALIZED(FLAG8, IERROR8)

  FLAG = FLAG8
  IERROR = IERROR8

End Subroutine MPI_INITIALIZED8

! ---------------------------------------------------------
Subroutine MPI_IPROBE8(SOURCE, TAG, COMM, FLAG, STATUS, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    SOURCE, TAG, COMM
  Logical(Kind=JPIM), Intent(OUT) :: &
    FLAG
  Integer(Kind=JPIM), Intent(OUT) :: &
    STATUS(:), IERROR

  Integer(Kind=8) :: &
    SOURCE8, TAG8, COMM8, IERROR8
  Logical(Kind=8) :: &
    FLAG8

  SOURCE8 = SOURCE
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_IPROBE(SOURCE8, TAG8, COMM8, FLAG8, STATUS8, IERROR8)

  FLAG = FLAG8
  STATUS = STATUS8
  IERROR = IERROR8

End Subroutine MPI_IPROBE8

! ---------------------------------------------------------
Subroutine MPI_PROBE8(SOURCE, TAG, COMM, STATUS, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    SOURCE, TAG, COMM
  Integer(Kind=JPIM), Intent(OUT) :: &
    STATUS(:), IERROR

  Integer(Kind=8) :: &
    SOURCE8, TAG8, COMM8, IERROR8

  SOURCE8 = SOURCE
  TAG8 = TAG
  COMM8 = COMM

  Call MPI_PROBE(SOURCE8, TAG8, COMM8, STATUS8, IERROR8)

  STATUS = STATUS8
  IERROR = IERROR8

End Subroutine MPI_PROBE8

! ---------------------------------------------------------
Subroutine MPI_WAIT8_I4(REQUEST, STATUS, IERROR)

  Integer(Kind=JPIM), Intent(INOUT) :: &
    REQUEST
  Integer(Kind=JPIM), Intent(OUT) :: &
    STATUS(:), IERROR

  Integer(Kind=8) :: &
    REQUEST8, IERROR8
  Integer(Kind=8), Dimension(:), Allocatable :: STATUS8

  Allocate(STATUS8(Size(STATUS)))

  REQUEST8 = REQUEST

  Call MPI_WAIT(REQUEST8, STATUS8, IERROR8)

  REQUEST = REQUEST8
  STATUS = STATUS8
  IERROR = IERROR8

  Deallocate(STATUS8)

End Subroutine MPI_WAIT8_I4

! ---------------------------------------------------------
Subroutine MPI_WAIT8_I4_1(REQUEST, STATUS, IERROR)

  Integer(Kind=JPIM) :: &
    REQUEST
  Integer(Kind=JPIM), Intent(OUT) :: &
    STATUS, IERROR

  Integer(Kind=8) :: &
    REQUEST8, IERROR8, STATUS8

  REQUEST8 = REQUEST

  Call MPI_WAIT(REQUEST8, STATUS8, IERROR8)

  REQUEST = REQUEST8
  STATUS = STATUS8
  IERROR = IERROR8

End Subroutine MPI_WAIT8_I4_1

! ---------------------------------------------------------
Subroutine MPI_WAITALL8_I4(COUNT, ARRAY_OF_REQUESTS, ARRAY_OF_STATUSES, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT
  Integer(Kind=JPIM), Dimension(:), Intent(INOUT) :: &
    ARRAY_OF_REQUESTS
  Integer(Kind=JPIM), Dimension(:,:), Intent(OUT) :: &
    ARRAY_OF_STATUSES
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Integer(Kind=8), Dimension(:), Allocatable :: &
    ARRAY_OF_REQUESTS8
  Integer(Kind=8), Dimension(:,:), Allocatable :: &
    ARRAY_OF_STATUSES8
  Integer(Kind=8) :: &
    COUNT8, IERROR8

  Integer :: N

  COUNT8 = COUNT

  Allocate(ARRAY_OF_REQUESTS8(SIZE(ARRAY_OF_REQUESTS)))
  N = SIZE(ARRAY_OF_STATUSES) / MPI_STATUS_SIZE
  Allocate(ARRAY_OF_STATUSES8(MPI_STATUS_SIZE,N))

  ARRAY_OF_REQUESTS8 = ARRAY_OF_REQUESTS

  Call MPI_WAITALL(COUNT8, ARRAY_OF_REQUESTS8, ARRAY_OF_STATUSES8, IERROR8)

  ARRAY_OF_REQUESTS = ARRAY_OF_REQUESTS8
  ARRAY_OF_STATUSES = ARRAY_OF_STATUSES8

  Deallocate(ARRAY_OF_REQUESTS8)
  Deallocate(ARRAY_OF_STATUSES8)

  IERROR = IERROR8

End Subroutine MPI_WAITALL8_I4

! ---------------------------------------------------------
Subroutine MPI_WAITALL8_I4_1(COUNT, ARRAY_OF_REQUESTS, ARRAY_OF_STATUSES, IERROR)

  Integer(Kind=JPIM), Intent(IN) :: &
    COUNT
  Integer(Kind=JPIM) :: &
    ARRAY_OF_REQUESTS
  Integer(Kind=JPIM), Dimension(:,:), Intent(OUT) :: &
    ARRAY_OF_STATUSES
  Integer(Kind=JPIM), Intent(OUT) :: &
    IERROR

  Integer(Kind=8) :: &
    ARRAY_OF_REQUESTS8
  Integer(Kind=8), Dimension(:,:), Allocatable :: &
    ARRAY_OF_STATUSES8
  Integer(Kind=8) :: &
    COUNT8, IERROR8

  Integer :: N

  COUNT8 = COUNT

  N = SIZE(ARRAY_OF_STATUSES) / MPI_STATUS_SIZE
  Allocate(ARRAY_OF_STATUSES8(MPI_STATUS_SIZE,N))

  ARRAY_OF_REQUESTS8 = ARRAY_OF_REQUESTS

  Call MPI_WAITALL(COUNT8, ARRAY_OF_REQUESTS8, ARRAY_OF_STATUSES8, IERROR8)

  ARRAY_OF_REQUESTS = ARRAY_OF_REQUESTS8
  ARRAY_OF_STATUSES = ARRAY_OF_STATUSES8

  Deallocate(ARRAY_OF_STATUSES8)

  IERROR = IERROR8

End Subroutine MPI_WAITALL8_I4_1

End Module MPI4TO8_S

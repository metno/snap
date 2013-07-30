MODULE MPL_INIT_MOD

!**** MPL_INIT - Initialises the Message passing environment

!     Purpose.
!     --------
!     Must be called before any other MPL routine.

!**   Interface.
!     ----------
!        CALL MPL_INIT

!        Input required arguments :
!        -------------------------
!           none

!        Input optional arguments :
!        -------------------------
!           KOUTPUT  -  Level of printing for MPL routines
!                       =0: none
!                       =1: intermediate (default)
!                       =2: full trace
!           KUNIT    -  Fortran Unit to receive printed trace
!           LDINFO   -  = .TRUE.  : Print informative msgs from MPL_INIT (default) 
!                       = .FALSE. : Do not print

!        Output required arguments :
!        -------------------------
!           none

!        Output optional arguments :
!        -------------------------
!           KERROR   -  return error code.     If not supplied, 
!                       MPL_INIT aborts when an error is detected.
!           KPROCS   -  Number of processes which have been initialised
!                       in the MPI_COMM_WORLD communicator
!     Author.
!     -------
!        D.Dent, M.Hamrud     ECMWF

!     Modifications.
!     --------------
!        Original: 2000-09-01

!     ------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM     ,JPRB

USE MPL_MPIF
USE MPL_DATA_MODULE
USE MPL_MESSAGE_MOD
USE MPL_BUFFER_METHOD_MOD
USE MPL_TOUR_TABLE_MOD
USE MPL_LOCOMM_CREATE_MOD
USE MPL_ARG_MOD

IMPLICIT NONE

PUBLIC MPL_INIT

PRIVATE

CONTAINS 

SUBROUTINE MPL_INIT(KOUTPUT,KUNIT,KERROR,KPROCS,LDINFO)


#ifdef USE_8_BYTE_WORDS
  Use mpi4to8, Only : &
    MPI_INITIALIZED => MPI_INITIALIZED8, MPI_INIT => MPI_INIT8, &
    MPI_COMM_SIZE => MPI_COMM_SIZE8,     MPI_COMM_RANK => MPI_COMM_RANK8, &
    MPI_BCAST => MPI_BCAST8
#endif



INTEGER(KIND=JPIM),INTENT(IN),OPTIONAL :: KOUTPUT,KUNIT
INTEGER(KIND=JPIM),INTENT(OUT),OPTIONAL :: KERROR,KPROCS
LOGICAL,INTENT(IN),OPTIONAL :: LDINFO
INTEGER(KIND=JPIM) :: IERROR,IP,ICOMM,IRANK
INTEGER(KIND=JPIM) :: IMAX_THREADS, IRET, IROOT, INUM, ICOUNT
LOGICAL            :: LLABORT=.TRUE., LLINIT, LLINFO
CHARACTER(LEN=12)  :: CL_MBX_SIZE
CHARACTER(LEN=1024) :: CLENV
CHARACTER(LEN=20)  :: CL_METHOD

IF(PRESENT(KOUTPUT)) THEN
  MPL_OUTPUT=MAX(0,KOUTPUT)
ELSE
  MPL_OUTPUT=1
ENDIF

IF(PRESENT(KUNIT)) THEN
  MPL_UNIT=MAX(0,KUNIT)
ELSE
  MPL_UNIT=6
ENDIF

IF(PRESENT(LDINFO)) THEN
  LLINFO = LDINFO
ELSE
  LLINFO = .TRUE.
ENDIF

IF(MPL_NUMPROC /= -1) THEN
!!  We do not want this extra message
!!  CALL MPL_MESSAGE(CDMESSAGE=' MPL_INIT CALLED MULTIPLE TIMES ')
  IF(PRESENT(KERROR)) THEN
    KERROR=0
  ENDIF
  IF(PRESENT(KPROCS)) THEN
    KPROCS=MPL_NUMPROC
  ENDIF
  RETURN
ENDIF

CALL MPI_INITIALIZED(LLINIT, IRET)

IF (.NOT.LLINIT) THEN
  CALL MPI_INIT(IERROR)
  LINITMPI_VIA_MPL = .TRUE.
  CALL ec_mpi_atexit() ! ifsaux/support/endian.c: to make sure MPI_FINALIZE gets called
ELSE
  IERROR = 0
ENDIF

IF(PRESENT(KERROR)) THEN
  KERROR=IERROR
ELSE
  IF(IERROR /= 0) THEN
    CALL MPL_MESSAGE(IERROR,CDMESSAGE=' MPL_INIT ERROR ',LDABORT=LLABORT)
  ENDIF
ENDIF

CALL MPI_COMM_SIZE(INT(MPI_COMM_WORLD),MPL_NUMPROC,IERROR)

IF(PRESENT(KPROCS)) THEN
  KPROCS=MPL_NUMPROC
ENDIF

ALLOCATE (MPL_IDS(MPL_NUMPROC))
DO IP=1,MPL_NUMPROC
  MPL_IDS(IP)=IP
ENDDO

CALL MPI_COMM_RANK(INT(MPI_COMM_WORLD), IRANK, IERROR)
MPL_RANK=IRANK+1

!-- Propagate environment variables & argument lists
!   Here we have to be careful and use MPI_BCAST directly (not MPL_BROADCAST) since
!   1) MPL_BUFFER_METHOD has not been called
!   2) MPL_COMM_OML has not been initialized since it is possible that only the 
!      master proc knows the # of threads (i.e. OMP_NUM_THREADS may be set only for master)

IF (MPL_NUMPROC > 1) THEN
  IROOT = 0
  !-- Progate environment variables
  INUM = 0
  IF (MPL_RANK == 1) CALL EC_NUMENV(INUM) ! Master proc
  ! The following broadcast does not use "mailbox" nor attached buffer, both potentially yet to be allocated
  CALL MPI_BCAST(INUM,1,INT(MPI_INTEGER),IROOT,INT(MPI_COMM_WORLD),IERROR)
  ICOUNT = LEN(CLENV)
  DO IP=1,INUM
    IF (MPL_RANK == 1) CALL EC_STRENV(IP,CLENV)
    ! The following broadcast does not use "mailbox" nor attached buffer, both potentially yet to be allocated
    CALL MPI_BCAST(CLENV,ICOUNT,INT(MPI_BYTE),IROOT,INT(MPI_COMM_WORLD),IERROR)
    IF (MPL_RANK > 1) CALL EC_PUTENV(CLENV)
  ENDDO
  !-- Propagate argument list (all under the bonnet using MPL_ARG_MOD-module)
  INUM = MPL_IARGC()
ENDIF

IMAX_THREADS = OML_MAX_THREADS()
ALLOCATE(MPL_COMM_OML(IMAX_THREADS))
MPL_COMM_OML(1) = MPI_COMM_WORLD
DO IP=2,IMAX_THREADS
  CALL MPL_LOCOMM_CREATE(MPL_NUMPROC,MPL_COMM_OML(IP))
ENDDO

#ifdef VPP
MPL_METHOD=JP_BLOCKING_STANDARD
MPL_MBX_SIZE=4000000
CL_MBX_SIZE=' '
CALL EC_GETENV('VPP_MBX_SIZE',CL_MBX_SIZE)
IF(CL_MBX_SIZE == ' ') THEN
  CALL EC_GETENV('MPL_MBX_SIZE',CL_MBX_SIZE)
ENDIF
IF(CL_MBX_SIZE /= ' ') THEN
  READ(CL_MBX_SIZE,*) MPL_MBX_SIZE
ENDIF
IF (LLINFO) WRITE(MPL_UNIT,'(A)')'MPL_INIT : MPL_METHOD=JP_BLOCKING_STANDARD'
IF (LLINFO) WRITE(MPL_UNIT,'(A,I12)')'MPL_INIT : MAILBOX SIZE=',MPL_MBX_SIZE
LUSEHLMPI = .FALSE.

!#elif defined (LINUX)
!MPL_METHOD=JP_BLOCKING_STANDARD
!MPL_MBX_SIZE=4000000
!CL_MBX_SIZE=' '
!CALL EC_GETENV('VPP_MBX_SIZE',CL_MBX_SIZE)
!IF(CL_MBX_SIZE == ' ') THEN
!  CALL EC_GETENV('MPL_MBX_SIZE',CL_MBX_SIZE)
!ENDIF
!IF(CL_MBX_SIZE /= ' ') THEN
!  READ(CL_MBX_SIZE,*) MPL_MBX_SIZE
!ENDIF
!IF (LLINFO) WRITE(MPL_UNIT,'(A)')'MPL_INIT : MPL_METHOD=JP_BLOCKING_STANDARD'
!IF (LLINFO) WRITE(MPL_UNIT,'(A,I12)')'MPL_INIT : MAILBOX SIZE=',MPL_MBX_SIZE
!LUSEHLMPI = .FALSE.

#else
CL_METHOD=' '
CALL EC_GETENV('MPL_METHOD',CL_METHOD)
IF (CL_METHOD == 'JP_BLOCKING_STANDARD' ) THEN
  MPL_METHOD=JP_BLOCKING_STANDARD
ELSE
  MPL_METHOD=JP_BLOCKING_BUFFERED
ENDIF
MPL_MBX_SIZE=1000000
CL_MBX_SIZE=' '
CALL EC_GETENV('MPL_MBX_SIZE',CL_MBX_SIZE)
IF (CL_MBX_SIZE /= ' ') THEN
  READ(CL_MBX_SIZE,*) MPL_MBX_SIZE
ENDIF
IF (CL_METHOD == 'JP_BLOCKING_STANDARD' ) THEN
  IF (LLINFO) WRITE(MPL_UNIT,'(A)')'MPL_INIT : MPL_METHOD=JP_BLOCKING_STANDARD'
ELSE
  IF (LLINFO) WRITE(MPL_UNIT,'(A)')'MPL_INIT : MPL_METHOD=JP_BLOCKING_BUFFERED'
ENDIF
IF (LLINFO) WRITE(MPL_UNIT,'(A,I12)')'MPL_INIT : MAILBOX SIZE=',MPL_MBX_SIZE

CALL MPL_BUFFER_METHOD(kmp_type=MPL_METHOD,kmbx_size=MPL_MBX_SIZE,LDINFO=LLINFO)
LUSEHLMPI = .TRUE.
#endif

ALLOCATE(MPL_OPPONENT(MPL_NUMPROC+1))
CALL MPL_TOUR_TABLE(MPL_OPPONENT)

RETURN
END SUBROUTINE MPL_INIT

END MODULE MPL_INIT_MOD

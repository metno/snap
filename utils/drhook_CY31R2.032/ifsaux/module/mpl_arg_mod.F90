MODULE mpl_arg_mod

!**** MPL_GETARG : A substitute for GETARG for MPL applications
!     MPL_IARGC  : A substitute for function IARGC for MPL applications

!     Purpose.
!     --------
!     MPL-task#1 calls getarg until iargc() arguments read
!     or until the argument is a terminating argument
!     Then arguments are passed on to other processors
!     If MPL has not been initialized, it will be done now.

!**   Interface.
!     ----------
!        CALL MPL_GETARG(KARG, CDARG)

!        Input required arguments :
!        -------------------------
!           KARG     -  The argument number requested (INTEGER(4))
!                       Range : [ 0 .. MPL_IARGC() ]

!        Output required arguments :
!        ---------------------------
!           CDARG    -  Return argument value (CHARACTER(LEN=*))
!
!**   Interface.
!     ----------
!        INUM_ARGS = MPL_IARGC()
!
!     where INUM_ARGS is INTEGER(4)

!     Author.
!     -------
!        S.Saarinen, G.Mozdzynski  ECMWF

!     Modifications.
!     --------------
!        Original: 2006-03-15

USE PARKIND1  ,ONLY : JPIM
USE MPL_MPIF
USE MPL_DATA_MODULE, ONLY : MPL_NUMPROC, LINITMPI_VIA_MPL

#ifdef NAG
use f90_unix_env, only: getarg, iargc
#endif

IMPLICIT NONE

PRIVATE

CHARACTER(LEN=10), SAVE :: CL_TERMINATE = '-^' ! terminating argument

INTEGER(KIND=JPIM), PARAMETER :: JP_ARGLEN = 1024
CHARACTER(LEN=JP_ARGLEN), ALLOCATABLE :: CL_ARGS(:)
INTEGER(KIND=JPIM), SAVE :: N_ARGS = -1

PUBLIC :: MPL_GETARG
PUBLIC :: MPL_IARGC
PUBLIC :: MPL_ARG_SET_CL_TERMINATE
PUBLIC :: MPL_ARG_GET_CL_TERMINATE

CONTAINS

SUBROUTINE MPL_ARG_SET_CL_TERMINATE(CDTERM)
CHARACTER(LEN=*), INTENT(IN) :: CDTERM
CL_TERMINATE = CDTERM
END SUBROUTINE MPL_ARG_SET_CL_TERMINATE

SUBROUTINE MPL_ARG_GET_CL_TERMINATE(CDTERM)
CHARACTER(LEN=*), INTENT(OUT) :: CDTERM
CDTERM = CL_TERMINATE
END SUBROUTINE MPL_ARG_GET_CL_TERMINATE

SUBROUTINE INIT_ARGS()

#ifdef USE_8_BYTE_WORDS
  Use mpi4to8, Only : &
    MPI_INITIALIZED => MPI_INITIALIZED8, MPI_COMM_SIZE => MPI_COMM_SIZE8, &
    MPI_COMM_RANK => MPI_COMM_RANK8, MPI_BCAST => MPI_BCAST8, &
    MPI_INIT => MPI_INIT8
#endif

INTEGER(KIND=JPIM) :: IARGS
INTEGER(KIND=JPIM) :: IERROR, IROOT, ICOUNT
INTEGER(KIND=JPIM) :: IRANK, INUMPROC, IRET, J
INTEGER(KIND=JPIM) :: IARGC
INTEGER(KIND=JPIM) :: IARGC_C
CHARACTER(LEN=LEN(CL_TERMINATE)) :: ENV_CL_TERMINATE
CHARACTER(LEN=JP_ARGLEN) :: CLARG0
LOGICAL LLINIT, LLCARGS

IF (N_ARGS == -1) THEN
  IF (MPL_NUMPROC == -1) THEN
    ! This is complicated, but I hope it works:
    ! MPI has not yet been initialized, when this routines was called.
    ! Initialize MPI, but NOT via MPL_INIT to avoid recursion in MPL_IARGC()
    ! However, must pretend that MPL_INIT has actually initialized it, but 
    ! MPL_NUMPROC will not be set
    CALL MPI_INITIALIZED(LLINIT,IRET)
    IF (.not.LLINIT) THEN
      CALL MPI_INIT(IERROR)
      LINITMPI_VIA_MPL = .TRUE.
      CALL ec_mpi_atexit() ! ifsaux/support/endian.c: to make sure MPI_FINALIZE gets called
    ENDIF
  ENDIF

  CALL MPI_COMM_SIZE(INT(MPI_COMM_WORLD),INUMPROC,IERROR)
  CALL MPI_COMM_RANK(INT(MPI_COMM_WORLD),IRANK,IERROR)
  IRANK=IRANK+1

  IF (IRANK == 1 .OR. INUMPROC == 1) THEN
    CALL EC_GETENV('MPL_CL_TERMINATE',ENV_CL_TERMINATE)
    IF (ENV_CL_TERMINATE /= ' ') CL_TERMINATE = ENV_CL_TERMINATE
    IARGS = IARGC()
    LLCARGS = (IARGS < 0) ! Should be true for non-F90 main programs
    IF (LLCARGS) THEN
      IARGS = IARGC_C()
      LLCARGS = (IARGS >= 0)
      CALL GETARG_C(0,CLARG0) ! The executable name (see ifsaux/support/cargs.c)
    ELSE
      CALL PUTARG_INFO(IARGS, TRIM(CL_TERMINATE)) ! (see ifsaux/support/cargs.c)
      CALL GETARG(0,CLARG0)                       ! The executable name (normal F90 way)
      CALL PUTARG_C(0,TRIM(CLARG0))               ! (see ifsaux/support/cargs.c)
    ENDIF
    IF (IARGS < 0) IARGS = 0
    ALLOCATE(CL_ARGS(0:IARGS))
    N_ARGS = 0
    CL_ARGS(0) = CLARG0
    DO J=1,IARGS ! Other args (repeat until end of loop or terminating argument found)
      IF (LLCARGS) THEN
        CALL GETARG_C(J,CL_ARGS(J))
      ELSE
        CALL GETARG(J,CL_ARGS(J))
        CALL PUTARG_C(J,TRIM(CL_ARGS(J)))
      ENDIF
      IF (CL_ARGS(J) == CL_TERMINATE) EXIT
      N_ARGS = N_ARGS + 1
    ENDDO
  ENDIF

  IF (INUMPROC > 1) THEN
    IROOT = 0
    IARGS = 0
    IF (IRANK == 1) IARGS = N_ARGS
    ! The following broadcast does not use "mailbox" nor attached buffer, both potentially yet to be allocated
    CALL MPI_BCAST(IARGS,1,INT(MPI_INTEGER),IROOT,INT(MPI_COMM_WORLD),IERROR)
    ICOUNT = JP_ARGLEN
    IF (IRANK > 1) ALLOCATE(CL_ARGS(0:IARGS))
    IF (IRANK > 1) CALL PUTARG_INFO(IARGS, TRIM(CL_TERMINATE))
    DO J=0,IARGS
     ! The following broadcast does not use "mailbox" nor attached buffer, both potentially yet to be allocated
      CALL MPI_BCAST(CL_ARGS(J),ICOUNT,INT(MPI_BYTE),IROOT,INT(MPI_COMM_WORLD),IERROR)
      IF (IRANK > 1) CALL PUTARG_C(J,TRIM(CL_ARGS(J)))
    ENDDO
    IF (IRANK > 1) N_ARGS = IARGS
  ENDIF
ENDIF
END SUBROUTINE INIT_ARGS

SUBROUTINE MPL_GETARG(KARG, CDARG)
INTEGER(KIND=JPIM), INTENT(IN) :: KARG
CHARACTER(LEN=*), INTENT(OUT)  :: CDARG
INTEGER(KIND=JPIM) :: IARGC
IF (N_ARGS == -1) CALL INIT_ARGS()
IF (KARG >= 0 .AND. KARG <= N_ARGS) THEN
  CDARG = CL_ARGS(KARG)
ELSE
  CDARG = ' '
ENDIF
END SUBROUTINE MPL_GETARG

FUNCTION MPL_IARGC() RESULT(IRET)
INTEGER(KIND=JPIM) :: IRET
IF (N_ARGS == -1) CALL INIT_ARGS()
IRET = N_ARGS
END FUNCTION MPL_IARGC

END MODULE mpl_arg_mod

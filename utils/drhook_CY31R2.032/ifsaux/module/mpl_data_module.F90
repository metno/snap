MODULE MPL_DATA_MODULE

!     Author.
!     -------
!        D.Dent, M.Hamrud     ECMWF

!     Modifications.
!     --------------
!        Original: 2000-09-01

!     ------------------------------------------------------------------

!      variables controlling the execution of MPL

!  MPL_METHOD   : buffering type
!  MPL_MBX_SIZE : size of application mailbox, (bytes)
!                 used when MPL_METHOD=JP_BLOCKING_BUFFERED
! (MPL_COMM     : default communicator in use; removed; corresponds now to MPL_COMM_OML(1))
!  MPL_COMM_OML : communicators for messages between corresponding OML-threads
!  MPL_UNIT     : Fortran I/O unit for messages (default=6)
!  MPL_ERRUNIT  : Fortran I/O unit for error messages (default=0)
!  MPL_OUTPUT   : controls contents of Output (see mpl_init_mod.F90 for values/default)
!  MPL_RANK     : rank of the process within MPL_COMM_OML(1)
!  MPL_NUMPROC  : number of processes in MPL_COMM_OML(1)
!  MPL_IDS      : array of processor numbers
!  LUSEHLMPI    : always use high level MPI calls (collective comm.)
!  LINITMPI_VIA_MPL : true if MPI has been initialized from within MPL_INIT()

USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE YOMOML    ,ONLY : OML_MAX_THREADS, OML_MY_THREAD

IMPLICIT NONE

SAVE

PUBLIC 

INTEGER(KIND=JPIM) :: MPL_METHOD, MPL_MBX_SIZE, MPL_UNIT=6, MPL_OUTPUT=1
INTEGER(KIND=JPIM) :: MPL_RANK=0,MPL_NUMPROC = -1,MPL_ERRUNIT=0
INTEGER(KIND=JPIM),ALLOCATABLE :: MPL_IDS(:)
INTEGER(KIND=JPIM),ALLOCATABLE :: MPL_COMM_OML(:)
INTEGER(KIND=JPIM),ALLOCATABLE :: MPL_OPPONENT(:)
!INTEGER_M,ALLOCATABLE :: MPL_ATTACHED_BUFFER(:)
!   needs to ge a TARGET for coexistence with MPE
INTEGER(KIND=JPIM),ALLOCATABLE,TARGET     :: MPL_ATTACHED_BUFFER(:)
LOGICAL :: LUSEHLMPI
LOGICAL :: LINITMPI_VIA_MPL = .FALSE.
INTEGER(KIND=JPIM),PARAMETER :: JP_ATTACHED_BUFFER_BYTES = 4
INTEGER(KIND=JPIM),PARAMETER :: JP_BLOCKING_STANDARD        = 1
INTEGER(KIND=JPIM),PARAMETER :: JP_BLOCKING_BUFFERED        = 2
INTEGER(KIND=JPIM),PARAMETER :: JP_NON_BLOCKING_STANDARD    = 3
INTEGER(KIND=JPIM),PARAMETER :: JP_NON_BLOCKING_READY       = 4
INTEGER(KIND=JPIM),PARAMETER :: JP_NON_BLOCKING_SYNCHRONOUS = 5

END MODULE MPL_DATA_MODULE

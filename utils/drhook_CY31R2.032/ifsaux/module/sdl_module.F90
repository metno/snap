MODULE SDL_MODULE

!    Interface between user applications and system-dependent intrinsic
!    routines, provided by the computer vendors.

!    All routines which wish to call these routines must contain:
!    USE SDL_MODULE

! Author :
! ------
!   11-Apr-2005 R. El Khatib  *METEO-FRANCE*
!   26-Apr-2006 S.T.Saarinen  Dr.Hook trace, calls to EC_RAISE, Intel/ifort traceback

USE PARKIND1  ,ONLY : JPIM  ,JPRB
USE YOMHOOK   ,ONLY : LHOOK ,DR_HOOK
USE YOMOML, ONLY : OML_MY_THREAD

IMPLICIT NONE

SAVE

PRIVATE

INTEGER, parameter :: SIGABRT = 6 ! Hardcoded

PUBLIC :: SDL_SRLABORT, SDL_DISABORT, SDL_TRACEBACK

CONTAINS

!-----------------------------------------------------------------------------
SUBROUTINE SDL_TRACEBACK(KTID)

! Purpose :
! -------
!   Traceback

!   KTID : thread 

INTEGER(KIND=JPIM), INTENT(IN), OPTIONAL :: KTID
INTEGER(KIND=JPIM) ITID, IPRINT_OPTION, ILEVEL
#ifdef NEC
CHARACTER(LEN=*), PARAMETER :: necmsg = '*** Calling NEC traceback ***'
#endif

IF (PRESENT(KTID)) THEN
  ITID = KTID
ELSE
  ITID = OML_MY_THREAD()
ENDIF

IF (LHOOK) THEN
  IPRINT_OPTION = 2
  ILEVEL = 0
  CALL C_DRHOOK_PRINT(0, ITID, IPRINT_OPTION, ILEVEL) ! from drhook.c
ENDIF

#ifdef VPP
  CALL ERRTRA
  IF (PRESENT(KTID)) CALL SLEEP(28)
#elif RS6K
  WRITE(0,*)'SDL_TRACEBACK: Calling XL_TRBK, THRD = ',ITID
  CALL XL__TRBK()
  WRITE(0,*)'SDL_TRACEBACK: Done XL_TRBK, THRD = ',ITID
#elif __INTEL_COMPILER
  WRITE(0,*)'SDL_TRACEBACK: Calling INTEL_TRBK, THRD = ',ITID
  CALL INTEL_TRBK() ! See ifsaux/utilities/gentrbk.F90
  WRITE(0,*)'SDL_TRACEBACK: Done INTEL_TRBK, THRD = ',ITID
#elif defined(LINUX) || defined(SUN4)
  WRITE(0,*)'SDL_TRACEBACK: Calling LINUX_TRBK, THRD = ',ITID
  CALL LINUX_TRBK() ! See ifsaux/utilities/linuxtrbk.c
  WRITE(0,*)'SDL_TRACEBACK: Done LINUX_TRBK, THRD = ',ITID
#elif defined(NEC)
  WRITE(0,*)'SDL_TRACEBACK: Calling NEC/MESPUT, THRD = ',ITID
  CALL MESPUT(necmsg, len(necmsg), 1)
  WRITE(0,*)'SDL_TRACEBACK: Done NEC/MESPUT, THRD = ',ITID
#else
  WRITE(0,*)'SDL_TRACEBACK: No proper traceback implemented.'
  ! A traceback using dbx-debugger, if available AND 
  ! activated via 'export DBXDEBUGGER=1'
  WRITE(0,*)'SDL_TRACEBACK: Calling DBX_TRBK, THRD = ',ITID
  CALL DBX_TRBK() ! See ifsaux/utilities/linuxtrbk.c
  WRITE(0,*)'SDL_TRACEBACK: Done DBX_TRBK, THRD = ',ITID
  ! A traceback using gdb-debugger, if available AND 
  ! activated via 'export GDBDEBUGGER=1'
  WRITE(0,*)'SDL_TRACEBACK: Calling GDB_TRBK, THRD = ',ITID
  CALL GDB_TRBK() ! See ifsaux/utilities/linuxtrbk.c
  WRITE(0,*)'SDL_TRACEBACK: Done GDB_TRBK, THRD = ',ITID
#endif

END SUBROUTINE SDL_TRACEBACK
!-----------------------------------------------------------------------------
SUBROUTINE SDL_SRLABORT

! Purpose :
! -------
!   To abort in serial environment

CALL EC_RAISE(SIGABRT)
STOP 'SDL_SRLABORT'

END SUBROUTINE SDL_SRLABORT
!-----------------------------------------------------------------------------
SUBROUTINE SDL_DISABORT(KCOMM)

! Purpose :
! -------
!   To abort in distributed environment

!   KCOMM : communicator

INTEGER(KIND=JPIM), INTENT(IN) :: KCOMM

INTEGER(KIND=JPIM) :: IRETURN_CODE,IERROR

#ifdef VPP

CALL VPP_ABORT()

#else

IRETURN_CODE=1
CALL MPI_ABORT(KCOMM,IRETURN_CODE,IERROR)

#endif

CALL EC_RAISE(SIGABRT) ! In case ever ends up here
STOP 'SDL_DISABORT'

END SUBROUTINE SDL_DISABORT
!-----------------------------------------------------------------------------

END MODULE SDL_MODULE

MODULE MPL_OPEN_MOD
!
!     Purpose.  open an MPIIO file 
!     --------
!
!
!     Interface.
!     ----------
!        call mpl_open(...)
!
!        Explicit arguments :
!        --------------------
!
!        input arguments:
!        ktype   - 1 = open for reading , 2 = writing
!        kname   - Name of the file
!        output arguments:
!        kfptr   - handle for file pointer
!        kerror  - error code
!
!        Implicit arguments :
!        --------------------
!
!     Method.
!     -------
!     MPL supports 4 styles of MPIIO
!
!     kop = 1    -  Blocking, non collective, shared file pointer
!                   using MPI_FILE_WRITE_SHARED,
!                         MPI_FILE_READ_SHARED
!     kop = 2    -  Blocking, collective, ordered, shared file pointer
!                   using MPI_FILE_WRITE_ORDERED,
!                         MPI_FILE_READ_ORDERED
!     kop = 3    -  Non Blocking, non collective, shared file pointer
!                   using MPI_FILE_IWRITE_SHARED,
!                         MPI_FILE_IREAD_SHARED
!                   and MPI_WAIT
!     kop = 4    -  Non Blocking, collective, ordered, shared file pointer
!                   using MPI_FILE_WRITE_ORDERED_BEGIN/END,
!                         MPI_FILE_READ_ORDERED_BEGIN/END
!    
!
!     Externals.
!     ----------
!
!     Reference.
!     ----------
!        none yet
!
!     Author.
!     -------
!        G.Mozdzynski
!
!     Modifications.
!     --------------
!        Original : 2000-12-08 (Based on MPE_OPEN)
!
!     -----------------------------------------------------------------
!
USE PARKIND1  ,ONLY : JPIM     ,JPRB

USE MPL_MPIF
USE MPL_DATA_MODULE
USE MPL_MESSAGE_MOD
USE MPL_IOINIT_MOD

IMPLICIT NONE

PUBLIC MPL_OPEN

CONTAINS

SUBROUTINE MPL_OPEN(KFPTR,KTYPE,KNAME,KERROR)


#ifdef USE_8_BYTE_WORDS
  Use mpi4to8, Only : &
    MPI_FILE_OPEN => MPI_FILE_OPEN8
#endif

INTEGER(KIND=JPIM),INTENT(IN) :: ktype
INTEGER(KIND=JPIM),INTENT(OUT) :: kfptr,kerror
CHARACTER*(*) KNAME
INTEGER(KIND=JPIM) :: mode,info

#ifdef MPI2

!
!     -----------------------------------------------------------------
!
!     1.    Preamble
!           --------
!
IF( MPL_RANK > MPL_NUMIO ) then
  KERROR = -1
  RETURN
ENDIF

IF( KTYPE == 1 ) THEN
   MODE = MPI_MODE_RDONLY
ELSEIF( KTYPE == 2 ) THEN
   MODE = MPI_MODE_WRONLY + MPI_MODE_CREATE
ELSE
   KERROR = -1
   RETURN
ENDIF

INFO = MPI_INFO_NULL
    
!     -----------------------------------------------------------------
!
!     2.    open the file
!           ----------------------

CALL MPI_FILE_OPEN(MPL_COMM_IO,KNAME,MODE,INFO,KFPTR,KERROR)
!
!
!     -----------------------------------------------------------------
#else

CALL ABOR1('MPL_OPEN not built with MPI2')

#endif

RETURN
END SUBROUTINE MPL_OPEN

END MODULE MPL_OPEN_MOD

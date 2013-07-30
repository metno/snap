MODULE MPL_ALLGATHER_MOD

!**** MPL_ALLGATHER Send data to all processes

!     Purpose.
!     --------
!     Send a message to all processes from a buffer.
!     The data may be REAL*4, REAL*8,or INTEGER, one dimensional array
!                     REAL*4,or REAL*8, two dimensional array
!                  or INTEGER scalar

!**   Interface.
!     ----------
!        CALL MPL_ALLGATHER

!        Input required arguments :
!        -------------------------
!           PSENDBUF -  buffer containing message
!                       (can be type REAL*4, REAL*8 or INTEGER)
!           PRECVBUF -  buffer containing message
!                       (can be type REAL*4, REAL*8 or INTEGER)
!           KRECVCOUNTS-number of elements received from each process

!        Input optional arguments :
!        -------------------------
!           KCOMM    -  Communicator number if different from MPI_COMM_WORLD 
!                       or from that established as the default 
!                       by an MPL communicator routine
!           KRECVDISPL -displacements in PRECVBUF at which to place 
!                       the incoming data
!           CDSTRING -  Character string for ABORT messages
!                       used when KERROR is not provided

!        Output required arguments :
!        -------------------------
!           none

!        Output optional arguments :
!        -------------------------
!           KERROR   -  return error code.     If not supplied, 
!                       MPL_ALLGATHER aborts when an error is detected.
!     Author.
!     -------
!        D.Dent, M.Hamrud     ECMWF

!     Modifications.
!     --------------
!        Original: 2000-11-23

!     ------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM     ,JPRB

USE MPL_MPIF
USE MPL_DATA_MODULE
USE MPL_MESSAGE_MOD
USE MPL_SEND_MOD
USE MPL_RECV_MOD
USE MPL_BARRIER_MOD

IMPLICIT NONE

PRIVATE

INTEGER(KIND=JPIM) :: IR,ISENDCOUNT,IRECVCOUNT,ICOMM,IERROR
LOGICAL :: LLABORT=.TRUE.
INTEGER(KIND=JPIM) :: ITAG
LOGICAL :: LLBARRIER
INTEGER(KIND=JPIM) :: IMAXMSG,JK,IMYPAIR,ICHUNKS,ISTS,ISTR,JMESS,ILENS,IENS,IOUNT
INTEGER(KIND=JPIM) :: ILIMIT,IBARRFREQ,IDUM

PUBLIC MPL_ALLGATHER

CONTAINS

SUBROUTINE MPL_ALLGATHER()
return
end SUBROUTINE MPL_ALLGATHER
end MODULE MPL_ALLGATHER_MOD

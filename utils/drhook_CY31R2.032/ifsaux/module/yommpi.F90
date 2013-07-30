MODULE YOMMPI


USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------
!*    Contains identifiers used by MPI (Message Passing Interface)


INTEGER(KIND=JPIM) :: MINTET = 1
INTEGER(KIND=JPIM) :: MREALT = 2
INTEGER(KIND=JPIM) :: MLOGIT = 3
INTEGER(KIND=JPIM) :: MCHART = 4

END MODULE YOMMPI

FUNCTION get_max_threads() RESULT(imaxt)
USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE yomoml
implicit none
INTEGER(KIND=JPIM) :: imaxt
imaxt = OML_MAX_THREADS()
END FUNCTION get_max_threads

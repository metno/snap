FUNCTION get_thread_id() RESULT(tid)
USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE yomoml
implicit none
INTEGER(KIND=JPIM) :: tid
tid = OML_MY_THREAD()
END FUNCTION get_thread_id

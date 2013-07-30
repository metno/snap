subroutine dr_hook_procinfo(kmyproc, knproc)
USE PARKIND1  ,ONLY : JPIM     ,JPRB
use mpl_data_module, only : MPL_RANK,MPL_NUMPROC
implicit none
INTEGER(KIND=JPIM),intent(out) :: kmyproc, knproc
kmyproc = mpl_rank
knproc = mpl_numproc
end subroutine dr_hook_procinfo

subroutine coml_set_debug(konoff, kret)
USE YOMOML, ONLY : OML_DEBUG
USE PARKIND1, ONLY : JPIM
implicit none
INTEGER(KIND=JPIM), intent(in) :: konoff
INTEGER(KIND=JPIM), intent(out) :: kret
kret = 0
if (OML_DEBUG) kret = 1
if (konoff == 0) then 
  OML_DEBUG = .FALSE.
else
  OML_DEBUG = .TRUE.
endif
end subroutine coml_set_debug

subroutine coml_init_lockid_with_name(kmylock,cdlockname)
USE YOMOML, ONLY : OML_INIT_LOCK, OML_LOCK_KIND, OML_DEBUG
USE PARKIND1, ONLY : JPIB
implicit none
INTEGER(KIND=OML_LOCK_KIND), intent(inout) :: kmylock
CHARACTER(LEN=*), intent(in) :: cdlockname
INTEGER(KIND=JPIB), external :: loc_addr
CALL OML_INIT_LOCK(kmylock)
IF (OML_DEBUG) write(0,'(1x,a,2i20)') &
     & 'coml_init_lockid_with_name "'//cdlockname//'" :',kmylock,loc_addr(kmylock)
end subroutine coml_init_lockid_with_name

subroutine coml_init_lockid(kmylock)
USE YOMOML, ONLY : OML_INIT_LOCK, OML_LOCK_KIND, OML_DEBUG
USE PARKIND1, ONLY : JPIB
implicit none
INTEGER(KIND=OML_LOCK_KIND), intent(inout) :: kmylock
INTEGER(KIND=JPIB), external :: loc_addr
CALL OML_INIT_LOCK(kmylock)
IF (OML_DEBUG) write(0,'(1x,2i20)') &
     & 'coml_init_lockid :',kmylock,loc_addr(kmylock)
end subroutine coml_init_lockid

subroutine coml_init_lock()
USE YOMOML, ONLY : OML_INIT_LOCK
implicit none
!$ CALL OML_INIT_LOCK()
end subroutine coml_init_lock

subroutine coml_test_lockid(kisset,kmylock)
USE PARKIND1, ONLY : JPIM
USE YOMOML, ONLY : OML_TEST_LOCK, OML_LOCK_KIND
implicit none
INTEGER(KIND=JPIM), intent(out) :: kisset
INTEGER(KIND=OML_LOCK_KIND), intent(inout) :: kmylock
kisset = 1
IF (.not.OML_TEST_LOCK(kmylock)) kisset = 0
end subroutine coml_test_lockid

subroutine coml_test_lock(kisset)
USE PARKIND1, ONLY : JPIM
USE YOMOML, ONLY : OML_TEST_LOCK
implicit none
INTEGER(KIND=JPIM), intent(out) :: kisset
kisset = 1
IF (.not.OML_TEST_LOCK()) kisset = 0
end subroutine coml_test_lock

subroutine coml_set_lockid(kmylock)
USE YOMOML, ONLY : OML_SET_LOCK, OML_LOCK_KIND, OML_MY_THREAD, OML_DEBUG
USE PARKIND1, ONLY : JPIB, JPRB
implicit none
INTEGER(KIND=OML_LOCK_KIND), intent(inout) :: kmylock
INTEGER(KIND=JPIB), external :: loc_addr
REAL(KIND=JPRB), external :: util_walltime
IF (OML_DEBUG) write(0,'(1x,f20.6,1x,i3,a,2i20)') &
     & util_walltime(),OML_MY_THREAD(),': coml_SET_lockid >>',kmylock,loc_addr(kmylock)
CALL OML_SET_LOCK(kmylock)
IF (OML_DEBUG) write(0,'(1x,f20.6,1x,i3,a,2i20)') &
     & util_walltime(),OML_MY_THREAD(),': coml_SET_lockid <<',kmylock,loc_addr(kmylock)
end subroutine coml_set_lockid

subroutine coml_set_lock()
USE YOMOML, ONLY : OML_SET_LOCK
implicit none
CALL OML_SET_LOCK()
end subroutine coml_set_lock

subroutine coml_unset_lockid(kmylock)
USE YOMOML, ONLY : OML_UNSET_LOCK, OML_LOCK_KIND, OML_MY_THREAD, OML_DEBUG
USE PARKIND1, ONLY : JPIB, JPRB
implicit none
INTEGER(KIND=OML_LOCK_KIND), intent(inout) :: kmylock
INTEGER(KIND=JPIB), external :: loc_addr
REAL(KIND=JPRB), external :: util_walltime
IF (OML_DEBUG) write(0,'(1x,f20.6,1x,i3,a,2i20)') &
     & util_walltime(),OML_MY_THREAD(),': coml_UNSET_lockid >>',kmylock,loc_addr(kmylock)
CALL OML_UNSET_LOCK(kmylock)
IF (OML_DEBUG) write(0,'(1x,f20.6,1x,i3,a,2i20)') &
     & util_walltime(),OML_MY_THREAD(),': coml_UNSET_lockid <<',kmylock,loc_addr(kmylock)
end subroutine coml_unset_lockid

subroutine coml_unset_lock()
USE YOMOML, ONLY : OML_UNSET_LOCK
implicit none
CALL OML_UNSET_LOCK()
end subroutine coml_unset_lock

subroutine coml_in_parallel(kispar_region)
USE PARKIND1, ONLY : JPIM
USE YOMOML, ONLY : OML_IN_PARALLEL
implicit none
INTEGER(KIND=JPIM), intent(out) :: kispar_region
kispar_region = 0
if (OML_IN_PARALLEL()) kispar_region = 1
end subroutine coml_in_parallel

      subroutine allocateFields
      implicit none
      logical, save :: FirstCall = .TRUE.
      integer :: AllocateStatus
      include 'snapdim.inc'
      include 'snapfld.inc'

      if (.not. FirstCall) return
      FirstCall = .FALSE.

      ALLOCATE ( u1(nx,ny,nk), STAT = AllocateStatus)
      IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
      ALLOCATE ( v1(nx,ny,nk), STAT = AllocateStatus)
      IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
      ALLOCATE ( w1(nx,ny,nk), STAT = AllocateStatus)
      IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
      ALLOCATE ( t1(nx,ny,nk), STAT = AllocateStatus)
      IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
      ALLOCATE ( ps1(nx,ny), STAT = AllocateStatus)
      IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
      ALLOCATE ( bl1(nx,ny), STAT = AllocateStatus)
      IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
      ALLOCATE ( hbl1(nx,ny), STAT = AllocateStatus)
      IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
      ALLOCATE ( hlevel1(nx,ny,nk), STAT = AllocateStatus)
      IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
      ALLOCATE ( hlayer1(nx,ny,nk), STAT = AllocateStatus)

      ALLOCATE ( u2(nx,ny,nk), STAT = AllocateStatus)
      IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
      ALLOCATE ( v2(nx,ny,nk), STAT = AllocateStatus)
      IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
      ALLOCATE ( w2(nx,ny,nk), STAT = AllocateStatus)
      IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
      ALLOCATE ( t2(nx,ny,nk), STAT = AllocateStatus)
      IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
      ALLOCATE ( ps2(nx,ny), STAT = AllocateStatus)
      IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
      ALLOCATE ( bl2(nx,ny), STAT = AllocateStatus)
      IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
      ALLOCATE ( hbl2(nx,ny), STAT = AllocateStatus)
      IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
      ALLOCATE ( hlevel2(nx,ny,nk), STAT = AllocateStatus)
      IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
      ALLOCATE ( hlayer2(nx,ny,nk), STAT = AllocateStatus)

      ALLOCATE ( xm(nx,ny), STAT = AllocateStatus)
      IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
      ALLOCATE ( ym(nx,ny), STAT = AllocateStatus)
      IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
      ALLOCATE ( garea(nx,ny), STAT = AllocateStatus)
      IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
      ALLOCATE ( field1(nx,ny), STAT = AllocateStatus)
      IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
      ALLOCATE ( field2(nx,ny), STAT = AllocateStatus)
      IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
      ALLOCATE ( field3(nx,ny), STAT = AllocateStatus)
      IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
      ALLOCATE ( field4(nx,ny), STAT = AllocateStatus)
      IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"

      ALLOCATE ( pmsl1(nxad,nyad), STAT = AllocateStatus)
      IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
      ALLOCATE ( pmsl2(nxad,nyad), STAT = AllocateStatus)
      IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"

      ALLOCATE ( precip(nx,ny,mprecip), STAT = AllocateStatus)
      IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"

      end subroutine allocateFields


      subroutine deAllocateFields
      implicit none
      include 'snapdim.inc'
      include 'snapfld.inc'

      DEALLOCATE ( u1)
      DEALLOCATE ( v1)
      DEALLOCATE ( w1)
      DEALLOCATE ( t1)
      DEALLOCATE ( ps1)
      DEALLOCATE ( bl1)
      DEALLOCATE ( hbl1)
      DEALLOCATE ( hlevel1)
      DEALLOCATE ( hlayer1)

      DEALLOCATE ( u2)
      DEALLOCATE ( v2)
      DEALLOCATE ( w2)
      DEALLOCATE ( t2)
      DEALLOCATE ( ps2)
      DEALLOCATE ( bl2)
      DEALLOCATE ( hbl2)
      DEALLOCATE ( hlevel2)
      DEALLOCATE ( hlayer2)

      DEALLOCATE ( xm)
      DEALLOCATE ( ym)
      DEALLOCATE ( garea)
      DEALLOCATE ( field1)
      DEALLOCATE ( field2)
      DEALLOCATE ( field3)
      DEALLOCATE ( field4)

      DEALLOCATE ( pmsl1)
      DEALLOCATE ( pmsl2)

      DEALLOCATE ( precip)

      end subroutine deAllocateFields

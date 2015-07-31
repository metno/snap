module particleML
    implicit none
    ! a simple particle to be stored
    TYPE particle
! old definition
!..pdata:   pdata(1,n) - x position in grid
!           pdata(2,n) - y position in grid
!           pdata(3,n) - sigma/eta position (vertical)
!           pdata(4,n) - sigma/eta at top of boundary layer
!           pdata(5,n) - height of boundary layer
!           pdata(6,n) - map ratio in x direction
!           pdata(7,n) - map ratio in y direction
!           pdata(8,n) - precipitation intensity (mm/hour)
!           pdata(9,n) - radioactive content (Bq)
!           pdata(10,n) - gravity in m/s (fixed or computed)
       REAL    :: x     !- x position in grid
       REAL    :: y     !- y position in grid
       REAL    :: z     !- sigma/eta position (vertical)
       REAL    :: tbl   !- sigma/eta at top of boundary layer
       REAL    :: rad   !- radioactive content (Bq)
       ! to be removed
       REAL    :: hbl
       REAL    :: rmx
       REAL    :: rmy ! map ration in y direction
       REAL    :: prc ! precipition intensity (mm/hour)
       REAL    :: grv ! gravity in m/s (fixed or computed)
    END TYPE particle

! the actual particle storage, will be allocated in allocateFields.F
    TYPE(particle), DIMENSION(:), POINTER :: pdata


    TYPE complexParticle
       TYPE(particle) :: par ! the original particle
       REAL    :: u ! u-speed
       REAL    :: v ! v-speed
       REAL    :: hbl ! height of boundary layer
       REAL    :: rmx ! map ratio in x direction
       REAL    :: rmy ! map ration in y direction
       REAL    :: precip ! precipition intensity (mm/hour)
       REAL    :: grav ! gravity in m/s (fixed or computed)
    END TYPE complexParticle

end module particleML

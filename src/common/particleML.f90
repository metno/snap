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
       REAL    :: hbl
       REAL    :: grv ! gravity in m/s (fixed or computed)
       INTEGER*2 :: ageInSteps ! age of particle since construction
       LOGICAL :: active ! inside/outside domain
    END TYPE particle

! storage for extra particle data
    TYPE extraParticle
       REAL    :: u ! u-speed
       REAL    :: v ! v-speed
       REAL    :: rmx ! map ratio in x direction
       REAL    :: rmy ! map ration in y direction
       REAL    :: prc ! precipition intensity (mm/hour)
    END TYPE extraParticle

! the actual particle storage, will be allocated in allocateFields.F
    TYPE(particle), DIMENSION(:), POINTER :: pdata

end module particleML

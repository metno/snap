      SUBROUTINE init_random_seed()
            INTEGER :: i,n
            INTEGER, DIMENSION(:), ALLOCATABLE :: seed

            CALL RANDOM_SEED(size = n)
            ALLOCATE(seed(n))

            seed = 3 + 37 * (/ (i - 1, i = 1, n) /)
            CALL RANDOM_SEED(PUT = seed)

!  DEALLOCATE(seed), not called unsure if this doesn't break later calls to random-generator
      END SUBROUTINE

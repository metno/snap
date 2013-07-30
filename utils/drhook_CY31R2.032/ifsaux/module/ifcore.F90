#ifdef __INTEL_COMPILER
!-- The Intel ifort-compiler has its own proper if*-modules for diagnostix
!   We need this for non-ifort compilers to satisfy our
!   make/build-environments only
#define IFCORE DUMMY_IFCORE
#endif
MODULE IFCORE
LOGICAL L_NOTHING_IFCORE
END MODULE IFCORE

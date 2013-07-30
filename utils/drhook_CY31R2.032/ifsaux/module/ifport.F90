#ifdef __INTEL_COMPILER
!-- The Intel ifort-compiler has its own proper if*-modules for diagnostix
!   We need this for non-ifort compilers to satisfy our
!   make/build-environments only
#define IFPORT DUMMY_IFPORT
#endif
MODULE IFPORT
LOGICAL L_NOTHING_IFPORT
END MODULE IFPORT

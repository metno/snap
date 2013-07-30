program kind_test

!!NOT use parkind1

implicit none

!----------------------------------------------------------------------------
!-- The following are taken from ifsaux/module/parkind1.F90

!
!     Integer Kinds
!     -------------
!

INTEGER, PARAMETER :: JPIT = SELECTED_INT_KIND(2)
INTEGER, PARAMETER :: JPIS = SELECTED_INT_KIND(4)
INTEGER, PARAMETER :: JPIM = SELECTED_INT_KIND(9)
INTEGER, PARAMETER :: JPIB = SELECTED_INT_KIND(12)

!Special integer type to be used for sensitive address calculations
!should be *8 for a machine with 8 byte addressing for optimum performance
#ifdef ADDRESS64
INTEGER, PARAMETER :: JPIA = JPIB
#else
INTEGER, PARAMETER :: JPIA = JPIM
#endif

!
!     Real Kinds
!     ----------
!

INTEGER, PARAMETER :: JPRT = SELECTED_REAL_KIND(2,1)
INTEGER, PARAMETER :: JPRS = SELECTED_REAL_KIND(4,2)
INTEGER, PARAMETER :: JPRM = SELECTED_REAL_KIND(6,37)
INTEGER, PARAMETER :: JPRB = SELECTED_REAL_KIND(13,300)

!----------------------------------------------------------------------------

integer i
integer(kind=JPIT) i_JPIT
integer(kind=JPIS) i_JPIS
integer(kind=JPIM) i_JPIM
integer(kind=JPIB) i_JPIB

real r
real(kind=JPRT) r_JPRT
real(kind=JPRS) r_JPRS
real(kind=JPRM) r_JPRM
real(kind=JPRB) r_JPRB
double precision d

print 1001,'bit_size(i     )=',bit_size(i)     , bit_size(i)/8,' (the default INTEGER)'
print 1000,'bit_size(i_JPIT)=',bit_size(i_JPIT), bit_size(i_JPIT)/8, JPIT
print 1000,'bit_size(i_JPIS)=',bit_size(i_JPIS), bit_size(i_JPIS)/8, JPIS
print 1000,'bit_size(i_JPIM)=',bit_size(i_JPIM), bit_size(i_JPIM)/8, JPIM
print 1000,'bit_size(i_JPIB)=',bit_size(i_JPIB), bit_size(i_JPIB)/8, JPIB
print 1002,'eps/huge(r     )=',epsilon(r),huge(r),' (the default REAL)'
print 1003,'eps/huge(r_JPRT)=',epsilon(r_JPRT),huge(r_JPRT),JPRT
print 1003,'eps/huge(r_JPRS)=',epsilon(r_JPRS),huge(r_JPRS),JPRS
print 1003,'eps/huge(r_JPRM)=',epsilon(r_JPRM),huge(r_JPRM),JPRM
print 1003,'eps/huge(r_JPRB)=',epsilon(r_JPRB),huge(r_JPRB),JPRB
print 1002,'eps/huge(d     )=',epsilon(d),huge(d),' (the default DOUBLE PRECISION)'

1000 format(a,3i5)
1001 format(a,2i5,a)
1002 format(a,2g30.20,a)
1003 format(a,2g30.20,i5)

end program kind_test

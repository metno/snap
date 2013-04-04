c-----------------------------------------------------------------------
c snapargos.inc
c
c..include file  -  common for SSV ARGOS OUTPUT
c
      integer margos
      parameter (margos=500)
c
      integer iargos,nargos
      integer argoshourstep,argoshoursrelease,argoshoursrun
      integer argostime(5,margos)
      character*100 argosdepofile,argosconcfile,argosdosefile
c
      common/argoscom/iargos,nargos,argoshourstep,
     +		      argoshoursrelease,argoshoursrun,
     +		      argostime,
     +                argosdepofile,argosconcfile,argosdosefile
c
c-----------------------------------------------------------------------

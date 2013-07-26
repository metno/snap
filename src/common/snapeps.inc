c-----------------------------------------------------------------------
c snapeps.inc
c
c..include file  -  common for ENSEMBLE PROJECT OUTPUT
c
c
c..ensemblefile
c..ensembleStepHours
c..ensembleparticipant
c..ensembleRandomKey
c
      integer iensemble
      integer ensembleStepHours
      integer ensembleparticipant
      character*128 ensemblefile
      character*7   ensembleRandomKey
c
      common/ensemb/iensemble,
     +              ensembleStepHours,
     +              ensembleparticipant,
     +              ensemblefile,ensembleRandomKey
c
c-----------------------------------------------------------------------

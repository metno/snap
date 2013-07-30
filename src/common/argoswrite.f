      subroutine argoswrite(iunit,name,iparam,itimeargos,nx,ny,dblfield)
c
#if defined(DRHOOK)
      USE PARKIND1  ,ONLY : JPIM     ,JPRB
      USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
#endif
      implicit none
#if defined(DRHOOK)
      REAL(KIND=JPRB) :: ZHOOK_HANDLE ! Stack variable i.e. do not use SAVE
#endif
c
      include 'snapdebug.inc'
c
c..input
      integer iunit,iparam,nx,ny,itimeargos(5)
      character*4 name
      double precision dblfield(nx*ny)
c
c..local
      integer nxy,ij,ij1,ij2,i
      double precision dblmin,dblmax
c
#if defined(DRHOOK)
      ! Before the very first statement
      IF (LHOOK) CALL DR_HOOK('ARGOSWRITE',0,ZHOOK_HANDLE)
#endif
c
      nxy=nx*ny
c
      if(name.eq.'depo') write(iunit,1001,err=900) iparam
      if(name.eq.'conc') write(iunit,1002,err=900) iparam
      if(name.eq.'dose') write(iunit,1003,err=900) iparam
c
      write(iunit,1004,err=900) 1.0
      write(iunit,1005,err=900) (itimeargos(i),i=1,5)
c
      do ij1=1,nxy,10
       ij2= min(ij1+9,nxy)
       write(iunit,1006,err=900) (dblfield(ij),ij=ij1,ij2)
      end do
cc
 1001 format('Isotope ',i3,' deposition (Unit/m2)')
 1002 format('Isotope ',i3,' concentration (Unit/m3)')
 1003 format('Isotope ',i3,' dose (Unit*hr/m3)')
 1004 format(1pe8.2e2)
 1005 format(5i2.2)
 1006 format(10(1pe14.6e2))
c
      if(idebug.eq.1) then
       dblmin=+1.0d+100
       dblmax=-1.0d+100
       do ij=1,nxy
         if(dblfield(ij).gt.0.0d0) then
           if(dblmin.gt.dblfield(ij)) dblmin=dblfield(ij)
           if(dblmax.lt.dblfield(ij)) dblmax=dblfield(ij)
         end if
       end do
       if(dblmin.gt.dblmax) then
         dblmin=0.0d0
         dblmax=0.0d0
        end if
        write(9,*) 'ARGOS ',name,iparam,dblmin,dblmax
      end if
c
#if defined(DRHOOK)
c     before the return statement
      IF (LHOOK) CALL DR_HOOK('ARGOSWRITE',1,ZHOOK_HANDLE)
#endif
      return
c
  900 continue
      stop 'ARGOSWRITE'
c
      end

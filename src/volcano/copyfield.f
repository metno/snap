      subroutine copyfield(fromfield,tofield,nx,ny,nz)
c
      implicit none
c
c..input/output
      integer nx,ny,nz
      real    fromfield(nx*ny*nz),tofield(nx*ny*nz)
c
c..local
      integer n,i
c
      n=nx*ny*nz
c
      do i=1,n
        tofield(i)=fromfield(i)
      end do
c
      return
      end

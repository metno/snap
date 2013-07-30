program drhook_ex5
use mpl_module
use sdl_module, only : SDL_TRACEBACK
implicit none
integer j, numargs
integer jpe, npes, mype
character(len=256) arg, env
CALL MPL_INIT(LDINFO=.FALSE.)
npes = mpl_nproc()
mype = mpl_myrank()
do jpe=1,npes
   if (jpe == mype) then
      write(0,1000) mype,&
           & ': Basic MPL/MPI implementation works : # of MPI-tasks = ',npes
      numargs = mpl_iargc()
      write(0,1000) mype,&
           & ': Number of args = ',numargs
      do j=0,numargs
         call mpl_getarg(j,arg)
         write(0,1001) mype, ': arg#', j, ' "'//trim(arg)//'"'
      enddo
      call ec_getenv('MPICH_ROOT',env)
      write(0,1002) mype, ': env MPICH_ROOT="'//trim(env)//'"'
      call flush(0)
   endif
   call mpl_barrier()
enddo
call mpl_barrier()
CALL SDL_TRACEBACK ! Testing traceback, too
CALL MPL_END()
1000  format(i5,a,i5)
1001  format(i5,a,i2,a)
1002  format(i5,a)
end program drhook_ex5

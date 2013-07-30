make[1] : on entre dans le répertoire « /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032 »
set -e; \cd /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/odb; \
/usr/bin/gmake ROOTDIR=/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032 ARCH=LXpgi_nompi TASK=mpi_serial _mpi_serial_
gmake[2] : on entre dans le répertoire « /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/odb »
================================================================================
Directory: /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/odb
Objects for TASK=mpi_serial, ODBASE=__notdef__, ARCH=LXpgi_nompi:
================================================================================
dirs=`/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/make_depend -D -a none -g -l mpi_serial 2>/dev/null || :`; \
for d in $dirs; do \
  set -e; \cd /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/odb/$d; \
  /usr/bin/gmake ROOTDIR=/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032 TASK=mpi_serial  /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/libmpi_serial.so -j 2; \
done
gmake[3] : on entre dans le répertoire « /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/odb/extras/mpi_serial »
pgf90  -c   -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpi_abort.F
pgf90  -c   -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpi_allgatherv.F
pgf90  -c   -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpi_allreduce.F
pgf90  -c   -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpi_alltoallv.F
pgf90  -c   -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpi_barrier.F
pgf90  -c   -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpi_bcast.F
pgf90  -c   -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpi_bsend.F
pgf90  -c   -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpi_buffer_attach.F
pgf90  -c   -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpi_buffer_detach.F
pgf90  -c   -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpi_cart_coords.F
pgf90  -c   -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpi_cart_create.F
pgf90  -c   -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpi_cart_rank.F
pgf90  -c   -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpi_cart_sub.F
pgf90  -c   -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpi_comm_create.F
pgf90  -c   -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpi_comm_group.F
pgf90  -c   -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpi_comm_rank.F
pgf90  -c   -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpi_comm_size.F
pgf90  -c   -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpi_comm_split.F
pgf90  -c   -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpi_end.F
pgf90  -c   -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpi_error_string.F
pgf90  -c   -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpi_finalize.F
pgf90  -c   -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpi_gather.F
pgf90  -c   -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpi_gatherv.F
pgf90  -c   -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpi_get_count.F
pgf90  -c   -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpi_group_incl.F
pgf90  -c   -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpi_init.F
pgf90  -c   -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpi_initialized.F
pgf90  -c   -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpi_iprobe.F
pgf90  -c   -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpi_irecv.F
pgf90  -c   -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpi_isend.F
pgf90  -c   -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpi_probe.F
pgf90  -c   -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpi_recv.F
pgf90  -c   -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpi_scatterv.F
pgf90  -c   -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpi_send.F
pgf90  -c   -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpi_waitall.F
pgf90  -c   -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpi_wait.F
pgf90  -c   -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  vpp_abort.F
gcc -g -c  -Werror -DLINUX -DLITTLE -DLXpgi_nompi -DHAS_XMOTIF -I/usr/X11/include -DINTEGER_IS_INT tracecalls.c
pgf90  -c   -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  vpp_barrier.F
gcc -g -c  -Werror -DLINUX -DLITTLE -DLXpgi_nompi -DHAS_XMOTIF -I/usr/X11/include -DINTEGER_IS_INT vpp_sempost.c
gcc -g -c  -Werror -DLINUX -DLITTLE -DLXpgi_nompi -DHAS_XMOTIF -I/usr/X11/include -DINTEGER_IS_INT vpp_semwait.c
/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/make_lib -c ar -q -u ar -ruv -t ar -t -l /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/libmpi_serial.a -f _foolib.15539 -p __notdef__
ar -q /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/libmpi_serial.a
ar: creating /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/libmpi_serial.a
ar -ruv /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/libmpi_serial.a  mpi_abort.o mpi_allgatherv.o mpi_allreduce.o mpi_alltoallv.o mpi_barrier.o mpi_bcast.o mpi_bsend.o mpi_buffer_attach.o mpi_buffer_detach.o mpi_cart_coords.o mpi_cart_create.o mpi_cart_rank.o mpi_cart_sub.o mpi_comm_create.o mpi_comm_group.o mpi_comm_rank.o mpi_comm_size.o mpi_comm_split.o mpi_end.o mpi_error_string.o mpi_finalize.o mpi_gather.o mpi_gatherv.o mpi_get_count.o mpi_group_incl.o mpi_initialized.o mpi_init.o mpi_iprobe.o mpi_irecv.o mpi_isend.o
a - mpi_abort.o
a - mpi_allgatherv.o
a - mpi_allreduce.o
a - mpi_alltoallv.o
a - mpi_barrier.o
a - mpi_bcast.o
a - mpi_bsend.o
a - mpi_buffer_attach.o
a - mpi_buffer_detach.o
a - mpi_cart_coords.o
a - mpi_cart_create.o
a - mpi_cart_rank.o
a - mpi_cart_sub.o
a - mpi_comm_create.o
a - mpi_comm_group.o
a - mpi_comm_rank.o
a - mpi_comm_size.o
a - mpi_comm_split.o
a - mpi_end.o
a - mpi_error_string.o
a - mpi_finalize.o
a - mpi_gather.o
a - mpi_gatherv.o
a - mpi_get_count.o
a - mpi_group_incl.o
a - mpi_initialized.o
a - mpi_init.o
a - mpi_iprobe.o
a - mpi_irecv.o
a - mpi_isend.o
ar -ruv /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/libmpi_serial.a  mpi_probe.o mpi_recv.o mpi_scatterv.o mpi_send.o mpi_waitall.o mpi_wait.o tracecalls.o vpp_abort.o vpp_barrier.o vpp_sempost.o vpp_semwait.o
a - mpi_probe.o
a - mpi_recv.o
a - mpi_scatterv.o
a - mpi_send.o
a - mpi_waitall.o
a - mpi_wait.o
a - tracecalls.o
a - vpp_abort.o
a - vpp_barrier.o
a - vpp_sempost.o
a - vpp_semwait.o
/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/make_shlib -i "none " -c "none" -s so -l /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/libmpi_serial.a 
gmake[3] : on quitte le répertoire « /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/odb/extras/mpi_serial »
gmake[2] : on quitte le répertoire « /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/odb »
set -e; \cd /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux; \
/usr/bin/gmake ROOTDIR=/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032 ARCH=LXpgi_nompi TASK=drhook _drhook_
gmake[2] : on entre dans le répertoire « /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux »
================================================================================
Directory: /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux
Objects for TASK=drhook, ODBASE=__notdef__, ARCH=LXpgi_nompi:
================================================================================
dirs=`/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/make_depend -D -a none -g -l drhook 2>/dev/null || :`; \
for d in $dirs; do \
  set -e; \cd /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/$d; \
  /usr/bin/gmake ROOTDIR=/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032 TASK=drhook  /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/libdrhook.so -j 2; \
done
gmake[3] : on entre dans le répertoire « /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module »
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  f90_unix_env.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  f90_unix_io.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  f90_unix_proc.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  ifcore.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  ifport.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpl_mpif.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  parkind1.F90
touch ../include/dr_hook_util_multi.ok
touch ../include/dr_hook_util.ok
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  parkind2.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpi4to8_m.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpi4to8_s.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  yomoml.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  yomhook.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  yomabrt.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  yomlun_ifsaux.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  yommpi.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  yomwatch.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpi4to8.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpl_data_module.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  sdl_module.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpl_arg_mod.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpl_abort_mod.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpl_message_mod.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpl_barrier_mod.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpl_recv_mod.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpl_send_mod.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpl_myrank_mod.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpl_alltoallv_mod.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpl_buffer_method_mod.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpl_ioinit_mod.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpl_comm_create_mod.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpl_end_mod.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpl_gatherv_mod.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpl_groups.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpl_locomm_create_mod.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpl_tour_table_mod.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpl_nproc_mod.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpl_open_mod.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpl_probe_mod.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpl_read_mod.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpl_scatterv_mod.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpl_wait_mod.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpl_write_mod.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpl_mygatherv_mod.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpl_setdflt_comm_mod.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpl_allgather_mod.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpl_allgatherv_mod.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpl_broadcast_mod.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpl_close_mod.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpl_init_mod.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpl_allreduce_mod.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  mpl_module.F90
/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/make_lib -c ar -q -u ar -ruv -t ar -t -l /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/libdrhook.a -f _foolib.16218 -p __notdef__
ar -q /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/libdrhook.a
ar: creating /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/libdrhook.a
ar -ruv /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/libdrhook.a  f90_unix_env.o f90_unix_io.o f90_unix_proc.o ifcore.o ifport.o mpi4to8_m.o mpi4to8.o mpi4to8_s.o mpl_abort_mod.o mpl_allgather_mod.o mpl_allgatherv_mod.o mpl_allreduce_mod.o mpl_alltoallv_mod.o mpl_arg_mod.o mpl_barrier_mod.o mpl_broadcast_mod.o mpl_buffer_method_mod.o mpl_close_mod.o mpl_comm_create_mod.o mpl_data_module.o mpl_end_mod.o mpl_gatherv_mod.o mpl_groups.o mpl_init_mod.o mpl_ioinit_mod.o mpl_locomm_create_mod.o mpl_message_mod.o mpl_module.o mpl_mpif.o mpl_mygatherv_mod.o
a - f90_unix_env.o
a - f90_unix_io.o
a - f90_unix_proc.o
a - ifcore.o
a - ifport.o
a - mpi4to8_m.o
a - mpi4to8.o
a - mpi4to8_s.o
a - mpl_abort_mod.o
a - mpl_allgather_mod.o
a - mpl_allgatherv_mod.o
a - mpl_allreduce_mod.o
a - mpl_alltoallv_mod.o
a - mpl_arg_mod.o
a - mpl_barrier_mod.o
a - mpl_broadcast_mod.o
a - mpl_buffer_method_mod.o
a - mpl_close_mod.o
a - mpl_comm_create_mod.o
a - mpl_data_module.o
a - mpl_end_mod.o
a - mpl_gatherv_mod.o
a - mpl_groups.o
a - mpl_init_mod.o
a - mpl_ioinit_mod.o
a - mpl_locomm_create_mod.o
a - mpl_message_mod.o
a - mpl_module.o
a - mpl_mpif.o
a - mpl_mygatherv_mod.o
ar -ruv /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/libdrhook.a  mpl_myrank_mod.o mpl_nproc_mod.o mpl_open_mod.o mpl_probe_mod.o mpl_read_mod.o mpl_recv_mod.o mpl_scatterv_mod.o mpl_send_mod.o mpl_setdflt_comm_mod.o mpl_tour_table_mod.o mpl_wait_mod.o mpl_write_mod.o parkind1.o parkind2.o sdl_module.o yomabrt.o yomhook.o yomlun_ifsaux.o yommpi.o yomoml.o yomwatch.o
a - mpl_myrank_mod.o
a - mpl_nproc_mod.o
a - mpl_open_mod.o
a - mpl_probe_mod.o
a - mpl_read_mod.o
a - mpl_recv_mod.o
a - mpl_scatterv_mod.o
a - mpl_send_mod.o
a - mpl_setdflt_comm_mod.o
a - mpl_tour_table_mod.o
a - mpl_wait_mod.o
a - mpl_write_mod.o
a - parkind1.o
a - parkind2.o
a - sdl_module.o
a - yomabrt.o
a - yomhook.o
a - yomlun_ifsaux.o
a - yommpi.o
a - yomoml.o
a - yomwatch.o
/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/make_shlib -i "none " -c "none" -s so -l /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/libdrhook.a 
gmake[3] : on quitte le répertoire « /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module »
gmake[3] : on entre dans le répertoire « /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/support »
touch ../include/cargs.ok
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  abor1.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  cdrhookinit.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  cmpl_binding.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  coml_binding.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  convin.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  convout.F90
touch ../include/crc.ok
touch ../include/drhook.ok
touch ../include/raise.ok
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  dr_hook_procinfo.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  dr_hook_prt.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  dr_hook_util.F90
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  dr_hook_util_multi.F90
gcc -g -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module -Werror -DLINUX -DLITTLE -DLXpgi_nompi -DHAS_XMOTIF -I/usr/X11/include -DINTEGER_IS_INT endian.c
gcc -g -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module -Werror -DLINUX -DLITTLE -DLXpgi_nompi -DHAS_XMOTIF -I/usr/X11/include -DINTEGER_IS_INT env.c
gcc -g -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module -Werror -DLINUX -DLITTLE -DLXpgi_nompi -DHAS_XMOTIF -I/usr/X11/include -DINTEGER_IS_INT mpe_locking.c
gcc -g -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module -Werror -DLINUX -DLITTLE -DLXpgi_nompi -DHAS_XMOTIF -I/usr/X11/include -DINTEGER_IS_INT cargs.c
gcc -g -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module -Werror -DLINUX -DLITTLE -DLXpgi_nompi -DHAS_XMOTIF -I/usr/X11/include -DINTEGER_IS_INT crc.c
gcc -g -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module -Werror -DLINUX -DLITTLE -DLXpgi_nompi -DHAS_XMOTIF -I/usr/X11/include -DINTEGER_IS_INT drhook.c
/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/make_lib -c ar -q -u ar -ruv -t ar -t -l /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/libdrhook.a -f _foolib.16443 -p __notdef__
ar -ruv /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/libdrhook.a  abor1.o cargs.o cdrhookinit.o cmpl_binding.o coml_binding.o convin.o convout.o crc.o drhook.o dr_hook_procinfo.o dr_hook_prt.o dr_hook_util_multi.o dr_hook_util.o endian.o env.o mpe_locking.o
a - abor1.o
a - cargs.o
a - cdrhookinit.o
a - cmpl_binding.o
a - coml_binding.o
a - convin.o
a - convout.o
a - crc.o
a - drhook.o
a - dr_hook_procinfo.o
a - dr_hook_prt.o
a - dr_hook_util_multi.o
a - dr_hook_util.o
a - endian.o
a - env.o
a - mpe_locking.o
/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/make_shlib -i "none " -c "none" -s so -l /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/libdrhook.a 
gmake[3] : on quitte le répertoire « /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/support »
gmake[3] : on entre dans le répertoire « /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/utilities »
gcc -g -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module -Werror -DLINUX -DLITTLE -DLXpgi_nompi -DHAS_XMOTIF -I/usr/X11/include -DINTEGER_IS_INT addrdiff.c
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  gentrbk.F90
gcc -g -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module -Werror -DLINUX -DLITTLE -DLXpgi_nompi -DHAS_XMOTIF -I/usr/X11/include -DINTEGER_IS_INT getcurheap.c
touch ../include/getstatm.ok
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  get_max_threads.F90
gcc -g -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module -Werror -DLINUX -DLITTLE -DLXpgi_nompi -DHAS_XMOTIF -I/usr/X11/include -DINTEGER_IS_INT getpag.c
gcc -g -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module -Werror -DLINUX -DLITTLE -DLXpgi_nompi -DHAS_XMOTIF -I/usr/X11/include -DINTEGER_IS_INT getrss.c
gcc -g -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module -Werror -DLINUX -DLITTLE -DLXpgi_nompi -DHAS_XMOTIF -I/usr/X11/include -DINTEGER_IS_INT getstackusage.c
gcc -g -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module -Werror -DLINUX -DLITTLE -DLXpgi_nompi -DHAS_XMOTIF -I/usr/X11/include -DINTEGER_IS_INT getstatm.c
gcc -g -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module -Werror -DLINUX -DLITTLE -DLXpgi_nompi -DHAS_XMOTIF -I/usr/X11/include -DINTEGER_IS_INT getstk.c
pgf90  -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module  -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  get_thread_id.F90
gcc -g -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module -Werror -DLINUX -DLITTLE -DLXpgi_nompi -DHAS_XMOTIF -I/usr/X11/include -DINTEGER_IS_INT linuxtrbk.c
gcc -g -c -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include -I../include -I/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module -I../module -Werror -DLINUX -DLITTLE -DLXpgi_nompi -DHAS_XMOTIF -I/usr/X11/include -DINTEGER_IS_INT gethwm.c
/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/make_lib -c ar -q -u ar -ruv -t ar -t -l /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/libdrhook.a -f _foolib.16588 -p __notdef__
ar -ruv /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/libdrhook.a  addrdiff.o gentrbk.o getcurheap.o gethwm.o get_max_threads.o getpag.o getrss.o getstackusage.o getstatm.o getstk.o get_thread_id.o linuxtrbk.o
a - addrdiff.o
a - gentrbk.o
a - getcurheap.o
a - gethwm.o
a - get_max_threads.o
a - getpag.o
a - getrss.o
a - getstackusage.o
a - getstatm.o
a - getstk.o
a - get_thread_id.o
a - linuxtrbk.o
/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/make_shlib -i "none " -c "none" -s so -l /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/libdrhook.a 
gmake[3] : on quitte le répertoire « /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/utilities »
gmake[2] : on quitte le répertoire « /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux »
set -e; \cd /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/odb; \
/usr/bin/gmake ROOTDIR=/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032 ARCH=LXpgi_nompi TASK=odbmain _odbmain_
gmake[2] : on entre dans le répertoire « /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/odb »
================================================================================
Directory: /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/odb
Objects for TASK=odbmain, ODBASE=__notdef__, ARCH=LXpgi_nompi:
================================================================================
dirs=`/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/make_depend -D -a none -g -l odbmain 2>/dev/null || :`; \
for d in $dirs; do \
  set -e; \cd /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/odb/$d; \
  /usr/bin/gmake ROOTDIR=/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032 TASK=odbmain  /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/libodbmain.so -j 2; \
done
gmake[3] : on entre dans le répertoire « /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/odb/tools »
gcc -g -c  -Werror -DLINUX -DLITTLE -DLXpgi_nompi -DHAS_XMOTIF -I/usr/X11/include -DINTEGER_IS_INT sizeof.c
pgf90  -c   -r8 -g -Ktrap=fp -fpic -Mbackslash -Munixlogical  Kind.F90
/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/make_lib -c ar -q -u ar -ruv -t ar -t -l /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/libodbmain.a -f _foolib.16660 -p __notdef__
ar -q /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/libodbmain.a
ar: creating /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/libodbmain.a
ar -ruv /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/libodbmain.a  Kind.o sizeof.o
a - Kind.o
a - sizeof.o
/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/make_shlib -i "none " -c "none" -s so -l /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/libodbmain.a 
gmake[3] : on quitte le répertoire « /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/odb/tools »
gmake[2] : on quitte le répertoire « /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/odb »
set -e; \cd /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/odb; \
/usr/bin/gmake ROOTDIR=/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032 ARCH=LXpgi_nompi TASK=odbdummy _odbdummy_
gmake[2] : on entre dans le répertoire « /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/odb »
================================================================================
Directory: /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/odb
Objects for TASK=odbdummy, ODBASE=__notdef__, ARCH=LXpgi_nompi:
================================================================================
dirs=`/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/make_depend -D -a none -g -l odbdummy 2>/dev/null || :`; \
for d in $dirs; do \
  set -e; \cd /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/odb/$d; \
  /usr/bin/gmake ROOTDIR=/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032 TASK=odbdummy  /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/libodbdummy.so -j 2; \
done
gmake[3] : on entre dans le répertoire « /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/odb/lib »
gcc -g -c  -Werror -DLINUX -DLITTLE -DLXpgi_nompi -DHAS_XMOTIF -I/usr/X11/include -DINTEGER_IS_INT Dummies.c
/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/make_lib -c ar -q -u ar -ruv -t ar -t -l /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/libodbdummy.a -f _foolib.16721 -p __notdef__
ar -q /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/libodbdummy.a
ar: creating /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/libodbdummy.a
ar -ruv /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/libodbdummy.a  Dummies.o
a - Dummies.o
/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/make_shlib -i "none " -c "none" -s so -l /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/libodbdummy.a 
gmake[3] : on quitte le répertoire « /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/odb/lib »
gmake[2] : on quitte le répertoire « /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/odb »
\cp -p /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/module/*.mod .
\cp -p /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include/drhook.h .
\cp -p /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032/ifsaux/include/cargs.h .
chmod a+r,u+w *.mod *.h
flist="insert_drhook insert_drhook.pl Fortran90_stuff.pm mpirun* run*fe"; \
for f in $flist; do \
  \rm -f $f; \
  \ln -s odb/scripts/$f .; \
  chmod a=r,u+w $f; \
done; \
chmod a+x insert_drhook; \
chmod a+x mpirun*; \
chmod a+x run*fe; \
pwd; \
\ls -ltr $flist *.mod libdrhook.* libmpi_serial.* libodbmain.* libodbdummy.*
/home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032
-rw-r--r-- 1 faroux mosayc       8 nov.  20 17:41 libmpi_serial.a.save
-rw-r--r-- 1 faroux mosayc  158832 nov.  20 17:41 libmpi_serial.a
-rw-r--r-- 1 faroux mosayc     400 nov.  20 17:41 f90_unix_io.mod
-rw-r--r-- 1 faroux mosayc     404 nov.  20 17:41 f90_unix_env.mod
-rw-r--r-- 1 faroux mosayc     386 nov.  20 17:41 ifcore.mod
-rw-r--r-- 1 faroux mosayc     408 nov.  20 17:41 f90_unix_proc.mod
-rw-r--r-- 1 faroux mosayc     386 nov.  20 17:41 ifport.mod
-rw-r--r-- 1 faroux mosayc   39580 nov.  20 17:41 mpl_mpif.mod
-rw-r--r-- 1 faroux mosayc    1549 nov.  20 17:41 parkind1.mod
-rw-r--r-- 1 faroux mosayc     516 nov.  20 17:41 parkind2.mod
-rw-r--r-- 1 faroux mosayc  224965 nov.  20 17:41 mpi4to8_m.mod
-rw-r--r-- 1 faroux mosayc   30517 nov.  20 17:41 mpi4to8_s.mod
-rw-r--r-- 1 faroux mosayc    7986 nov.  20 17:41 yomoml.mod
-rw-r--r-- 1 faroux mosayc   11259 nov.  20 17:41 yomhook.mod
-rw-r--r-- 1 faroux mosayc    1410 nov.  20 17:41 yomabrt.mod
-rw-r--r-- 1 faroux mosayc     908 nov.  20 17:41 yomlun_ifsaux.mod
-rw-r--r-- 1 faroux mosayc    1574 nov.  20 17:41 yommpi.mod
-rw-r--r-- 1 faroux mosayc   22931 nov.  20 17:41 yomwatch.mod
-rw-r--r-- 1 faroux mosayc     253 nov.  20 17:41 mpi4to8.mod
-rw-r--r-- 1 faroux mosayc    9680 nov.  20 17:41 mpl_data_module.mod
-rw-r--r-- 1 faroux mosayc    2708 nov.  20 17:41 sdl_module.mod
-rw-r--r-- 1 faroux mosayc    5953 nov.  20 17:41 mpl_arg_mod.mod
-rw-r--r-- 1 faroux mosayc    1516 nov.  20 17:41 mpl_abort_mod.mod
-rw-r--r-- 1 faroux mosayc    1416 nov.  20 17:41 mpl_message_mod.mod
-rw-r--r-- 1 faroux mosayc    1379 nov.  20 17:41 mpl_barrier_mod.mod
-rw-r--r-- 1 faroux mosayc   24708 nov.  20 17:41 mpl_recv_mod.mod
-rw-r--r-- 1 faroux mosayc   17779 nov.  20 17:41 mpl_send_mod.mod
-rw-r--r-- 1 faroux mosayc    1023 nov.  20 17:41 mpl_myrank_mod.mod
-rw-r--r-- 1 faroux mosayc    1841 nov.  20 17:41 mpl_buffer_method_mod.mod
-rw-r--r-- 1 faroux mosayc   18092 nov.  20 17:41 mpl_alltoallv_mod.mod
-rw-r--r-- 1 faroux mosayc    1679 nov.  20 17:41 mpl_ioinit_mod.mod
-rw-r--r-- 1 faroux mosayc    1054 nov.  20 17:41 mpl_comm_create_mod.mod
-rw-r--r-- 1 faroux mosayc    1121 nov.  20 17:41 mpl_end_mod.mod
-rw-r--r-- 1 faroux mosayc   24944 nov.  20 17:41 mpl_gatherv_mod.mod
-rw-r--r-- 1 faroux mosayc    3315 nov.  20 17:41 mpl_groups.mod
-rw-r--r-- 1 faroux mosayc    1164 nov.  20 17:41 mpl_locomm_create_mod.mod
-rw-r--r-- 1 faroux mosayc    2201 nov.  20 17:41 mpl_tour_table_mod.mod
-rw-r--r-- 1 faroux mosayc    1016 nov.  20 17:41 mpl_nproc_mod.mod
-rw-r--r-- 1 faroux mosayc    1357 nov.  20 17:41 mpl_open_mod.mod
-rw-r--r-- 1 faroux mosayc    1601 nov.  20 17:41 mpl_probe_mod.mod
-rw-r--r-- 1 faroux mosayc    4222 nov.  20 17:41 mpl_read_mod.mod
-rw-r--r-- 1 faroux mosayc   13618 nov.  20 17:41 mpl_scatterv_mod.mod
-rw-r--r-- 1 faroux mosayc   18744 nov.  20 17:41 mpl_wait_mod.mod
-rw-r--r-- 1 faroux mosayc    4232 nov.  20 17:41 mpl_write_mod.mod
-rw-r--r-- 1 faroux mosayc    1161 nov.  20 17:41 mpl_setdflt_comm_mod.mod
-rw-r--r-- 1 faroux mosayc    4550 nov.  20 17:41 mpl_mygatherv_mod.mod
-rw-r--r-- 1 faroux mosayc    3364 nov.  20 17:41 mpl_allgather_mod.mod
-rw-r--r-- 1 faroux mosayc   18424 nov.  20 17:41 mpl_allgatherv_mod.mod
-rw-r--r-- 1 faroux mosayc   15645 nov.  20 17:41 mpl_broadcast_mod.mod
-rw-r--r-- 1 faroux mosayc    1153 nov.  20 17:41 mpl_close_mod.mod
-rw-r--r-- 1 faroux mosayc    1372 nov.  20 17:41 mpl_init_mod.mod
-rw-r--r-- 1 faroux mosayc   12553 nov.  20 17:41 mpl_allreduce_mod.mod
-rw-r--r-- 1 faroux mosayc    1290 nov.  20 17:41 mpl_module.mod
-rw-r--r-- 1 faroux mosayc 2964118 nov.  20 17:41 libdrhook.a.save
-rw-r--r-- 1 faroux mosayc 3033884 nov.  20 17:41 libdrhook.a
-rw-r--r-- 1 faroux mosayc       8 nov.  20 17:41 libodbmain.a.save
-rw-r--r-- 1 faroux mosayc   18556 nov.  20 17:41 libodbmain.a
-rw-r--r-- 1 faroux mosayc       8 nov.  20 17:41 libodbdummy.a.save
-rw-r--r-- 1 faroux mosayc   27064 nov.  20 17:41 libodbdummy.a
lrwxrwxrwx 1 faroux mosayc      28 nov.  20 17:41 insert_drhook.pl -> odb/scripts/insert_drhook.pl
lrwxrwxrwx 1 faroux mosayc      25 nov.  20 17:41 insert_drhook -> odb/scripts/insert_drhook
lrwxrwxrwx 1 faroux mosayc      30 nov.  20 17:41 Fortran90_stuff.pm -> odb/scripts/Fortran90_stuff.pm
lrwxrwxrwx 1 faroux mosayc      18 nov.  20 17:41 run_fe -> odb/scripts/run_fe
lrwxrwxrwx 1 faroux mosayc      17 nov.  20 17:41 runfe -> odb/scripts/runfe
lrwxrwxrwx 1 faroux mosayc      24 nov.  20 17:41 mpirun.linux -> odb/scripts/mpirun.linux
lrwxrwxrwx 1 faroux mosayc      22 nov.  20 17:41 mpirun.ibm -> odb/scripts/mpirun.ibm
make[1] : on quitte le répertoire « /home/faroux/SVN/surfex/trunk/src/LIB/drhook_CY31R2.032 »

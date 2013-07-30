#-- A makefile to manage standalone ODB-builds, tarball creations etc.

#-- The following parameters *must* appear before 'include make.rootdir'

ARCH=none
MAKE=make
INSTALL_DIR=
MOD_EXT=mod
MODUPCASE=
#MODUPCASE=-u
SUBPRODUCT=
#SUBPRODUCT=_drhook
ODB_FCLIBS=
NODB=
# DX=. on Windows/CYGWIN to avoid .exe files being created
DX=
#-- C-preprocessor
#CPP=/lib/cpp -P -C -DECMWF
CPP=/lib/cpp -P -C
# In MeteoFrance, use:
#CPP=/lib/cpp -P -C
#CPP=cc -E -C
# Windows/CYGWIN (gets auto-included into the make.rootdir during ./configure -a cygwin):
#CPP=/usr/bin/cpp -P -C

# Using parallel (g)make with option -j #ntasks when compiling certain libs
# gmake -j #ntasks can be activated and #ntasks specified via configure :
# ./configure -j 4 [...other options...] # this will use 4 processes to compile the libs
PARALLEL_GMAKE=

# Use -t option in configure to *ENABLE* timestamps : ./configure -t [...other options...]
TIMESTAMPER_OPTS=__NO_TIMESTAMPS__

# If compilation/linking of timestamp.x fails, then $(TIMESTAMP_DIR)/timestamp.x
# will automatically refer to $(TIMESTAMP_DIR)/notimestamp.x
TIMESTAMP_EXE=$(TIMESTAMP_DIR)/timestamp.x
TIMESTAMPER=$(TIMESTAMP_EXE) $(TIMESTAMPER_OPTS)

#-- Curses library is by default set to blank (i.e. as if -DNO_CURSES had been applied)
#  The ./configure tries to find the location of libncurses or libcurses and will
#  then re-set the $(LIBCURSES) accordingly; or if not found or if -DNO_CURSES found 
#  in the platform specific make.$(ARCH), will it leave the $(LIBCURSES) blank
#was: LIBCURSES=-lncurses
LIBCURSES=

include make.rootdir

#-- Current cycle consistently from odb/lib/version.c
#CYCLE=CY31R1
CYCLE=`/bin/ksh -e $(ROOTDIR)/odb/scripts/get_cycle $(ROOTDIR)/odb/lib/version.c`

#-- Library creation procedures
MAKE_LIB=$(ROOTDIR)/make_lib
MAKE_SHLIB=$(ROOTDIR)/make_shlib
SHLIB_LIST=none # don't leave this empty; some make's fail in make_shlib -i "<empty>" ;-(
SHLIB_CMD=none
SHLIB_SFX=so
SHLIB_RLDPATH=
#-- put the following to "-r" if ranlib is required
RANLIB=
#RANLIB=-r

LIB=$(LIBDIR)/lib$(TASK).a
SHLIB=$(LIBDIR)/lib$(TASK).$(SHLIB_SFX)
OBJS=
TASK=none

#-- Library path
LIBDIR=$(ROOTDIR)
LIBSTD=$(LIBODB) $(LIBIFSAUX) $(LIBODB) $(LIBIFSAUX)
LIBCSTD=$(LIBODB) $(LIBIFSAUX)
STD_LIB=$(ODB_LIB) $(IFSAUX_LIB) $(ODB_LIB) $(IFSAUX_LIB)
CSTD_LIB=$(ODB_LIB) $(IFSAUX_LIB)
XLDUM=xldummy.o abor1.o

#-- Executable path
EXEDIR=$(ROOTDIR)

#-- Fortran90
FC=f90
FCINC=$(CCINC)
FCFLAGS=
FCFREEFMT=
FCFIXEDFMT=

#-- C
CC=cc
CCFLAGS=
CCINC=

#-- C++
CCPLUS=ansi_cplusplus_compiler
CCPLUSFLAGS=
CCPLUSINC=$(CCINC)

#-- C-linker
LDCC=$(CC)
#LDCCFLAGS=
LDCCLIBS=-lm

#-- Fortran90-linker
LDFC=$(FC)
LDFC_SERIAL=$(LDFC)
#LDFCFLAGS=
LDFCLIBS=$(AUXLIBS)

#-- Magics
LDMAG=$(LDFC_SERIAL)
MAGLIB=\
$(LIBODBDUMMY) \
$(LIBIFSAUX) $(MPILIB_SERIAL) \
$(LIBODB)

#-- Lex & Yacc
YACC=bison --yacc -d
#YACC=yacc -d
LEX=flex -lv
#LEX=lex -v
YACCLEX_LIBS=-ll -ly -lm

#-- cc & Lex & Yacc for front-end compilation/build of $(ODBSQL) only
FE_YACC=$(YACC)
FE_LEX=$(LEX)
FE_CC=$(CC)
FE_CCFLAGS=$(CCFLAGS)
FE_LDCC=$(LDCC)
FE_LDCCFLAGS=$(LDCCFLAGS)
FE_YACCLEX_LIBS=$(YACCLEX_LIBS)
FE_SYSLIB=$(SYSLIB)
FE_AR=$(AR)
FE_AR_CREATE=$(FE_AR) -q$(VERBOSE)
FE_AR_UPDATE=$(FE_AR) -ruv$(VERBOSE)
FE_AR_EXTRACT=$(FE_AR) -x$(VERBOSE)
FE_AR_TBLCONT=$(FE_AR) -t$(VERBOSE)
FE_RANLIB=

BACKENDTOO=no

#-- GNU-zip
MYGZIP=gzip -1v -f

#-- (binary) diff
DIFF=cmp -s

#-- Unix cksum (could be md5sum)
CKSUM=cksum

#-- Create ODB-glue code
CREATE_ODBGLUE=env ODB_CC="$(CC)" $(ROOTDIR)/create_odbglue
ODBGLUE_OBJ=_odb_glue.o
ODBGLUE_ALL=_odb_glue.*

#-- ODB-compiler
#   *** Note: ODBASE must be defined for a particular database (like ECMA)
ODBASE=__notdef__
ODBSQL=$(EXEDIR)/odb98.x
ODBSQLBE=$(EXEDIR)/odb98be.x
#ODBSQLEXTRAFLAGS=-V -v
ODBSQLEXTRAFLAGS=
#-- If in MeteoFrance, comment out the following line #define/#undef statement
#ODBDEF=-DCANARI -UECMWF
ODBDEF=-UCANARI -DECMWF
ODBSQLFLAGS=-O3 -C $(ODBDEF) $(ODBSQLEXTRAFLAGS)
ODB_COMPILE_DDL=\
env ODB_COMPILER_FLAGS=odb98.flags $(SHLIB_RLDPATH) \
$(ODBSQL) $(ODBSQLFLAGS) -c -I . -l $(ODBASE)
#ODB_COMPILE_SQL=$(ODB_COMPILE_DDL) -i -w -S -s
ODB_COMPILE_SQL=$(ODB_COMPILE_DDL) -i -w
ODB_FILTER=no

#-- Static stubb (now -S -s removed from $(ODB_COMPILE_SQL) and we compile static only once
CREATE_STATIC_STUBB=env ODB_CC="$(CC)" $(ROOTDIR)/create_static_stubb

#-- Known (recognized) ODB databases (add/remove as necessary)
#   and their libraries
KNOWN_DBS=PREODB CCMA ECMA ECMASCR
LIBPREODB=$(LIBDIR)/libPREODB.a
LIBCCMA=$(LIBDIR)/libCCMA.a
LIBECMA=$(LIBDIR)/libECMA.a
LIBECMASCR=$(LIBDIR)/libECMASCR.a

#-- ... and their -l syntax :
PREODB_LIB=-lPREODB
CCMA_LIB=-lCCMA
ECMA_LIB=-lECMA
ECMASCR_LIB=-lECMASCR

#-- Libraries created
LIBDRHOOK=$(LIBDIR)/libdrhook.a
LIBIFSAUX=$(LIBDIR)/libifsaux.a
LIBIFS=$(LIBDIR)/libifs.a
LIBMPI_SERIAL=$(LIBDIR)/libmpi_serial.a
LIBODBSQLCOMPILER=$(LIBDIR)/libodbsqlcompiler.a
LIBODBSQLBECOMPILER=$(LIBDIR)/libodbsqlbecompiler.a
LIBODBCLIENT=$(LIBDIR)/libodbclient.a
LIBODB=$(LIBDIR)/libodb.a
LIBODBDUMMY=$(LIBDIR)/libodbdummy.a
LIBODBPORT=$(LIBDIR)/libodbport.a
LIBODBMAIN=$(LIBDIR)/libodbmain.a
LIBOBSTAT=$(LIBDIR)/libobstat.a
LIBSSA=$(LIBDIR)/libssa.a
LIBEC=$(LIBDIR)/libodbec.a
LIBEMOS=$(LIBDIR)/libodbemos.a

#-- Library referencing : The -l syntax
DRHOOK_LIB=-ldrhook
IFSAUX_LIB=-lifsaux
IFS_LIB=-lifs
MPI_SERIAL_LIB=-lmpi_serial
ODBSQLCOMPILER_LIB=-lodbsqlcompiler
ODBSQLBECOMPILER_LIB=-lodbsqlbecompiler
ODBCLIENT_LIB=-lodbclient
ODB_LIB=-lodb
ODBDUMMY_LIB=-lodbdummy
ODBPORT_LIB=-lodbport
ODBMAIN_LIB=-lodbmain
OBSTAT_LIB=-lobstat
SSA_LIB=-lssa
EC_LIB=-lodbec
EMOS_LIB=-lodbemos

#-- All libs
ALL_LIBS=\
$(LIBIFSAUX) \
$(LIBIFS) \
$(LIBMPI_SERIAL) \
$(LIBODB) \
$(LIBODBDUMMY) \
$(LIBODBPORT) \
$(LIBODBMAIN) \
$(LIBPREODB) \
$(LIBCCMA) \
$(LIBECMA) \
$(LIBECMASCR) \
$(LIBODBSQLCOMPILER) \
$(LIBODBSQLBECOMPILER) \
$(LIBOBSTAT) \
$(LIBSSA) \
$(LIBEMOS) \
$(LIBEC)

#-- Install shared libs
INSTALL_SHLIBS=\
$(LIBIFSAUX) \
$(LIBIFS) \
$(LIBMPI_SERIAL) \
$(LIBODB) \
$(LIBODBDUMMY) \
$(LIBODBPORT) \
$(LIBODBMAIN) \
$(LIBPREODB) \
$(LIBCCMA) \
$(LIBECMA) \
$(LIBECMASCR) \
$(LIBODBSQLCOMPILER) \
$(LIBODBSQLBECOMPILER) \
$(LIBOBSTAT) \
$(LIBSSA) \
$(LIBEMOS) \
$(LIBEC)

#-- Libraries to be installed (note: database libs can be re-created i.e. not installed)
#   You can always change this behaviour by setting INSTALL_LIBS=$(ALL_LIBS)
#                                                or INSTALL_LIBS=$(INSTALL_SHLIBS)
INSTALL_LIBS=\
$(LIBIFSAUX) \
$(LIBIFS) \
$(LIBMPI_SERIAL) \
$(LIBODBCLIENT) \
$(LIBODB) \
$(LIBODBDUMMY) \
$(LIBODBPORT) \
$(LIBODBMAIN) \
$(LIBOBSTAT) \
$(LIBEMOS) \
$(LIBEC)

#-- Client objects (hardcoded; the list is supposed to change very rarely)
CLIOBJS=\
odbi_client.o \
odbcs_error.o \
odbcs_signal.o \
odbcs_wrappers.o \
memory.o \
swapbytes.o \
dtnum.o \
pcma_mdis.o

#-- Auxiliary libraries (system dependent)
#   You can override these in make.$(MKEXTRA) or make.$(ARCH)
AUXLIBS=$(EC_LIB) $(EMOS_LIB)

#-- You can override these in your ARCH-dependent make.$(ARCH)
MPILIB=-L/some/path -lmpi
MPILIB_SERIAL=-L$(ROOTDIR) -lmpi_serial

#-- Some system dependent libs needed ?
SYSLIB=

#-- C-library (in order to pick up C-versions of popen() & pclose() before Magics does it!)
CLIB=-lc

#-- X-windows libraries (for b4.x, when b4.c compiled with -DHAS_XMOTIF)
XWLIBS=-lXm -lXt -lX11

#-- Utility executables
UTILITY_EXES=\
$(EXEDIR)/odb_version \
$(EXEDIR)/odbversion \
$(EXEDIR)/hcat \
$(EXEDIR)/qtar \
$(EXEDIR)/numproducts \
$(EXEDIR)/conv_le2be \
$(EXEDIR)/conv_be2le \
$(EXEDIR)/dd2ddl.x \
$(EXEDIR)/ioassign \
$(EXEDIR)/mr2d_create \
$(EXEDIR)/mr2d_split \
$(EXEDIR)/odbcksum \
$(EXEDIR)/pcma \
$(EXEDIR)/upcma \
$(EXEDIR)/dcagen.x \
$(EXEDIR)/odbcalc.x \
$(EXEDIR)/odbsql.x \
$(EXEDIR)/dtfilt.x \
$(EXEDIR)/fscheduler.x \
$(EXEDIR)/mpi_scheduler.x \
$(EXEDIR)/b4.x \
$(EXEDIR)/odbi_direct.x \
$(EXEDIR)/odbi_server.x \
$(EXEDIR)/odbi_client.x \
$(EXEDIR)/sizeof.x \
$(EXEDIR)/kind.x

#-- The actual (possibly MPI-parallel) application executables
APPLICATION_EXES=\
$(EXEDIR)/shuffle.x.gz \
$(EXEDIR)/mrfs2disk \
$(EXEDIR)/disk2mrfs \
$(EXEDIR)/split_bufr_data.x \
$(EXEDIR)/split_timeslot_bufr_data.x \
$(EXEDIR)/fbnew2old.x \
$(EXEDIR)/bufr2odb.x.gz \
$(EXEDIR)/odb2bufr.x.gz \
$(EXEDIR)/Fetchdupl.x \
$(EXEDIR)/Excludebufr.x \
$(EXEDIR)/Sourcelist.x \
$(EXEDIR)/viewer.x \
$(EXEDIR)/hex2bufr.x \
$(EXEDIR)/plotobs.x \
$(EXEDIR)/simulobs2odb.x \
$(EXEDIR)/obstat_calc.x \
$(EXEDIR)/obstat_merge.x \
$(EXEDIR)/obstat_plot.x \
$(EXEDIR)/ssa.x \
$(EXEDIR)/buta.x \
$(EXEDIR)/bufr_add_bias.x \
$(EXEDIR)/bufr_check.x \
$(EXEDIR)/bufr_compress.x \
$(EXEDIR)/bufr_decode.x \
$(EXEDIR)/bufr_filter.x \
$(EXEDIR)/bufr_key.x \
$(EXEDIR)/bufr_merge_tovs.x \
$(EXEDIR)/bufr_nt1.x \
$(EXEDIR)/bufr_ntm.x \
$(EXEDIR)/bufr_obs_filter.x \
$(EXEDIR)/bufr_repack.x \
$(EXEDIR)/bufr_ship_anmh.x \
$(EXEDIR)/bufr_split.x

#-- All exes
ALL_EXES=\
$(UTILITY_EXES) \
$(APPLICATION_EXES) \
$(ODBSQL) \
$(ODBSQLBE)

#-- Benchmark executables only
BM_EXES=\
$(EXEDIR)/numproducts \
$(EXEDIR)/ioassign \
$(EXEDIR)/dcagen.x \
$(EXEDIR)/shuffle.x.gz \
$(EXEDIR)/split_bufr_data.x \
$(EXEDIR)/split_timeslot_bufr_data.x \
$(EXEDIR)/bufr2odb.x.gz \
$(EXEDIR)/bufr_add_bias.x \
$(EXEDIR)/bufr_check.x \
$(EXEDIR)/bufr_compress.x \
$(EXEDIR)/bufr_decode.x \
$(EXEDIR)/bufr_filter.x \
$(EXEDIR)/bufr_key.x \
$(EXEDIR)/bufr_merge_tovs.x \
$(EXEDIR)/bufr_nt1.x \
$(EXEDIR)/bufr_ntm.x \
$(EXEDIR)/bufr_obs_filter.x \
$(EXEDIR)/bufr_repack.x \
$(EXEDIR)/bufr_ship_anmh.x \
$(EXEDIR)/bufr_split.x

#--Dr.Hook exes
DRHOOK_EXES=\
$(EXEDIR)/sizeof.x \
$(EXEDIR)/kind.x

#-- Installation executables; as with libraries, not all are copied to $(INSTALL_DIR)
#   You can change this behavious by setting INSTALL_EXES=$(ALL_EXES)
INSTALL_EXES=\
odb_version \
odbversion \
hcat \
qtar \
numproducts \
conv_le2be \
conv_be2le \
dd2ddl.x \
ioassign \
mr2d_create \
mr2d_split \
pcma \
upcma \
dcagen.x \
odbcalc.x \
odbsql.x \
dtfilt.x \
fscheduler.x \
mpi_scheduler.x \
b4.x \
buta.x \
bufr_check.x \
bufr_decode.x \
bufr_filter.x \
bufr_key.x \
bufr_nt1.x \
bufr_obs_filter.x \
bufr_repack.x \
bufr_split.x \
bufr2odb.x.gz \
mrfs2disk \
disk2mrfs \
split_bufr_data.x \
split_timeslot_bufr_data.x \
Excludebufr.x \
Sourcelist.x \
hex2bufr.x \
plotobs.x \
shuffle.x.gz \
odbcksum \
sizeof.x \
kind.x \
odbi_server.x \
odbi_client.x \
$(ODBSQL) \
$(ODBSQLBE)

#-- Various scripts (needed for 'make install')
#   These scripts are taken from $(ROOTDIR)/odb/scripts/
#   If you need some other scripts, then you must help yourself from
#   $(ROOTDIR)/scripts/gen/ directory!

INSTALL_SCRIPTS=\
odb/scripts/askodb \
odb/scripts/odb_banner \
odb/scripts/odbcalc \
odb/scripts/odbsql \
odb/scripts/odbclean \
odb/scripts/odbgzip \
odb/scripts/odbgunzip \
odb/scripts/odb4to1 \
odb/scripts/odb_4to1 \
odb/scripts/odb1to4 \
odb/scripts/odb_1to4 \
odb/scripts/bufr2odb \
odb/scripts/fbufr2odb \
odb/scripts/fbbufr2odb \
odb/scripts/fb_bufr2odb \
odb/scripts/fb2odb \
odb/scripts/Fbdecode.ksh \
odb/scripts/F90hppa \
odb/scripts/check_links \
odb/scripts/create_audit \
odb/scripts/create_ccma \
odb/scripts/create_flist \
odb/scripts/create_index \
odb/scripts/create_ioassign \
odb/scripts/create_odbglue \
odb/scripts/create_static_stubb \
odb/scripts/dcafix \
odb/scripts/dcafix.pl \
odb/scripts/dcagen \
odb/scripts/dcaquick \
odb/scripts/dd2ddl \
odb/scripts/ddl2flags.pl \
odb/scripts/fetchERA40obs.linux \
odb/scripts/Fortran90_stuff.pm \
odb/scripts/gen_static \
odb/scripts/hex2bufr \
odb/scripts/insert_drhook \
odb/scripts/insert_drhook.pl \
odb/scripts/latlon_rad \
odb/scripts/latlonrad \
odb/scripts/makegp.ksh \
odb/scripts/make_fclibs \
odb/scripts/mandalay \
odb/scripts/mgrep \
odb/scripts/mkvdb \
odb/scripts/mpirun.ibm \
odb/scripts/mpirun.linux \
odb/scripts/newodb \
odb/scripts/obstat_odb \
odb/scripts/odb2grads \
odb/scripts/odb2mysql \
odb/scripts/odb2mysql.pl \
odb/scripts/odb2mysql_api \
odb/scripts/odb2netcdf \
odb/scripts/odbcc \
odb/scripts/odbc++ \
odb/scripts/odbcomp \
odb/scripts/odbdiff \
odb/scripts/odb_diff \
odb/scripts/odbdup \
odb/scripts/odb_dup \
odb/scripts/odbmerge \
odb/scripts/odb_merge \
odb/scripts/odbf90 \
odb/scripts/odbf90mp \
odb/scripts/odbless \
odb/scripts/odbmore \
odb/scripts/odbcat \
odb/scripts/odb_less \
odb/scripts/odb_more \
odb/scripts/odb_cat \
odb/scripts/odbgnuplot \
odb/scripts/odb_gnuplot \
odb/scripts/odbshuffle \
odb/scripts/odb_shuffle \
odb/scripts/odbviewer \
odb/scripts/odb_viewer \
odb/scripts/odbxyplot \
odb/scripts/odb_xyplot \
odb/scripts/odb_compress \
odb/scripts/odbcompress \
odb/scripts/rpt2html.pl \
odb/scripts/rpt2simul.pl \
odb/scripts/run_fe \
odb/scripts/runfe \
odb/scripts/simulobs2odb \
odb/scripts/simulobs.pl \
odb/scripts/start_server \
odb/scripts/tablecols.pl \
odb/scripts/test_arch \
odb/scripts/time.awk \
odb/scripts/ungzipify \
odb/scripts/uniquepath.pl \
odb/scripts/use_odb \
odb/scripts/use_odb.sh \
odb/scripts/where_fiddle.pl \
odb/scripts/xlf_omp \
odb/scripts/xvi

#-- Archive commands
#VERBOSE=v
VERBOSE=
AR=ar
AR_CREATE=$(AR) -q$(VERBOSE)
AR_UPDATE=$(AR) -ruv$(VERBOSE)
AR_EXTRACT=$(AR) -x$(VERBOSE)
AR_TBLCONT=$(AR) -t$(VERBOSE)

SHELL=/bin/ksh

#-- Note: ARCH must be present via environment or supplied via ARCH=zzzz
MKARCH=make.$(ARCH)
MKDEP=make.dep.$(TASK)
MKEXTRA=make.extra
MKALL=make.all

#-- Architecture specific flags (override some of the defs above)
#   *** Must be supplied ***
include $(MKARCH)

.SUFFIXES:
.SUFFIXES: \
.s .y .l .ddl .sql .ddl_ .h .hh .c .cc .F .F90 .$(MOD_EXT) .M .o .ok .$(SHLIB_SFX) .a .u .d .so .x

#-- The following includes different file depending on configure/build (tarball/ODB/Dr.Hook)
include $(MKALL)

#-- benchmark executables : assumes that all the necessary libs already reside in $(ROOTDIR)
bm:
	@$(TIMESTAMPER) "Start" "bm" ""
	$(MAKE) ROOTDIR=$(ROOTDIR) TASK=bm $(OPTS)
	@$(TIMESTAMPER) "End" "bm" ""

.PHONY: all _foolib _fooshlib install

#-- The bulk of the objects & their dependencies
#   *** Must be supplied ***
include $(MKDEP)

$(LIB): $(TIMESTAMP_EXE) $(OBJS) _foolib
	@$(TIMESTAMPER) "Start" "LIB" "`basename $@`"
	@set -e; if [[ "$(LIB)" = "$(LIBODBSQLCOMPILER)" ]] ; then \
	  echo $(MAKE_LIB) -$(VERBOSE)c "$(FE_AR_CREATE)" -u "$(FE_AR_UPDATE)" -t "$(FE_AR_TBLCONT)" -l $(LIB) $(FE_RANLIB) $(OBJS) ; \
	       $(MAKE_LIB) -$(VERBOSE)c "$(FE_AR_CREATE)" -u "$(FE_AR_UPDATE)" -t "$(FE_AR_TBLCONT)" -l $(LIB) $(FE_RANLIB) $(OBJS) ; \
	else \
	  echo "$(OBJS)" | perl -pe 's/\s+/\n/g' > _foolib.$$$$ ; \
	  echo $(MAKE_LIB) -$(VERBOSE)c "$(AR_CREATE)" -u "$(AR_UPDATE)" -t "$(AR_TBLCONT)" -l $(LIB) -f _foolib.$$$$ -p $(ODBASE) $(RANLIB) ; \
	       $(MAKE_LIB) -$(VERBOSE)c "$(AR_CREATE)" -u "$(AR_UPDATE)" -t "$(AR_TBLCONT)" -l $(LIB) -f _foolib.$$$$ -p $(ODBASE) $(RANLIB) ; \
	  \rm -f _foolib.$$$$ ; \
	fi
	@$(TIMESTAMPER) "End" "LIB" "`basename $@`"

$(SHLIB): $(TIMESTAMP_EXE) $(LIB) _fooshlib
	@$(TIMESTAMPER) "Start" "SHLIB" "`basename $@`"
	$(MAKE_SHLIB) -i "$(SHLIB_LIST)" -c "$(SHLIB_CMD)" -s $(SHLIB_SFX) -l $(LIB) $(RANLIB)
	@$(TIMESTAMPER) "End" "SHLIB" "`basename $@`"

_foolib:
	@if [[ "$(VERBOSE)" = v ]] ; then \
	  echo "_foolib: TASK=$(TASK), LIB=$(LIB)" ;\
	fi

_fooshlib:
	@if [[ "$(VERBOSE)" = v ]] ; then \
	  echo "_fooshlib: TASK=$(TASK), SHLIB=$(SHLIB)" ;\
	fi

_print_objs:
	@echo "================================================================================"
	@echo "Directory: `pwd`"
	@if [[ "$(VERBOSE)" = v ]] ; then \
	  echo "Building the following objects for TASK=$(TASK), ODBASE=$(ODBASE), ARCH=$(ARCH):" ;\
	  echo $(OBJS) | xargs -n3 echo ;\
	else \
	  echo "Objects for TASK=$(TASK), ODBASE=$(ODBASE), ARCH=$(ARCH):" ;\
        fi
	@echo "================================================================================"

_drhook_: $(TIMESTAMP_EXE) _print_objs
	@$(TIMESTAMPER) "Start" "_drhook_" ""
	dirs=`$(ROOTDIR)/make_depend $(NODB) -a none -g -l drhook 2>/dev/null || :`; \
	for d in $$dirs; do \
	  set -e; \cd $(ROOTDIR)/ifsaux/$$d; \
	  $(MAKE) ROOTDIR=$(ROOTDIR) TASK=drhook $(OPTS) $(SHLIB) $(PARALLEL_GMAKE); \
	done
	@$(TIMESTAMPER) "End" "_drhook_" ""

_ifsaux_: $(TIMESTAMP_EXE) _print_objs
	@$(TIMESTAMPER) "Start" "_ifsaux_" ""
	dirs=`$(ROOTDIR)/make_depend $(NODB) -a none -g -l ifsaux 2>/dev/null || :`; \
	for d in $$dirs; do \
	  set -e; \cd $(ROOTDIR)/ifsaux/$$d; \
	  $(MAKE) ROOTDIR=$(ROOTDIR) TASK=ifsaux $(OPTS) $(SHLIB) $(PARALLEL_GMAKE); \
	done
	@$(TIMESTAMPER) "End" "_ifsaux_" ""

_ifs_: $(TIMESTAMP_EXE) _print_objs
	@$(TIMESTAMPER) "Start" "_ifs_" ""
	dirs=`$(ROOTDIR)/make_depend $(NODB) -a none -g -l ifs 2>/dev/null || :`; \
	for d in $$dirs; do \
	  set -e; \cd $(ROOTDIR)/ifs/$$d; \
	  $(MAKE) ROOTDIR=$(ROOTDIR) TASK=ifs $(OPTS) $(SHLIB) $(PARALLEL_GMAKE); \
	done
	@$(TIMESTAMPER) "End" "_ifs_" ""

_mpi_serial_:$(TIMESTAMP_EXE)  _print_objs
	@$(TIMESTAMPER) "Start" "_mpi_serial_" ""
	dirs=`$(ROOTDIR)/make_depend $(NODB) -a none -g -l mpi_serial 2>/dev/null || :`; \
	for d in $$dirs; do \
	  set -e; \cd $(ROOTDIR)/odb/$$d; \
	  $(MAKE) ROOTDIR=$(ROOTDIR) TASK=mpi_serial $(OPTS) $(SHLIB) $(PARALLEL_GMAKE); \
	done
	@$(TIMESTAMPER) "End" "_mpi_serial_" ""

_odbec_:$(TIMESTAMP_EXE)  _print_objs
	@$(TIMESTAMPER) "Start" "_odbec_" ""
	dirs=`$(ROOTDIR)/make_depend $(NODB) -a none -g -l odbec 2>/dev/null || :`; \
	for d in $$dirs; do \
	  set -e; \cd $(ROOTDIR)/odb/$$d; \
	  $(MAKE) ROOTDIR=$(ROOTDIR) TASK=odbec $(OPTS) $(SHLIB) $(PARALLEL_GMAKE); \
	done
	@$(TIMESTAMPER) "End" "_odbec_" ""

#-- Note the "-i" in the $(MAKE) below : we temporarely ignore errors in libodbemos build
#   (to be removed)
_odbemos_:$(TIMESTAMP_EXE)  _print_objs
	@$(TIMESTAMPER) "Start" "_odbemos_" ""
	dirs=`$(ROOTDIR)/make_depend $(NODB) -a none -g -l odbemos 2>/dev/null || :`; \
	for d in $$dirs; do \
	  set -e; \cd $(ROOTDIR)/odb/$$d; \
	  $(MAKE) ROOTDIR=$(ROOTDIR) TASK=odbemos $(OPTS) -i $(SHLIB) $(PARALLEL_GMAKE); \
	done
	@$(TIMESTAMPER) "End" "_odbemos_" ""

_odbsqlcompiler_:$(TIMESTAMP_EXE)  _print_objs
	@$(TIMESTAMPER) "Start" "_odbsqlcompiler_" ""
	dirs=`$(ROOTDIR)/make_depend $(NODB) -a none -g -l odbsqlcompiler 2>/dev/null || :`; \
	for d in $$dirs; do \
	  set -e; \cd $(ROOTDIR)/odb/$$d; \
	  $(MAKE) ROOTDIR=$(ROOTDIR) TASK=odbsqlcompiler \
		FE_AR="$(FE_AR)" FE_RANLIB="$(FE_RANLIB)" \
		FE_YACC="$(FE_YACC)" FE_LEX="$(FE_LEX)" \
		FE_CC="$(FE_CC)" FE_CCFLAGS="$(FE_CCFLAGS)" \
		FE_LDCC="$(FE_LDCC)" FE_LDCCFLAGS="$(FE_LDCCFLAGS)" \
		FE_YACCLEX_LIBS="$(FE_YACCLEX_LIBS)" FE_SYSLIB="$(FE_SYSLIB)" \
		$(OPTS) $(SHLIB) $(PARALLEL_GMAKE); \
	done
	@$(TIMESTAMPER) "End" "_odbsqlcompiler_" ""

$(ODBSQL):$(TIMESTAMP_EXE)  _print_objs
	@$(TIMESTAMPER) "Start" "ODBSQL" "`basename $@`"
	set -e; \cd $(ROOTDIR)/odb; \
	  $(MAKE) ROOTDIR=$(ROOTDIR) ARCH=$(ARCH) TASK=odbsqlcompiler \
		FE_YACC="$(FE_YACC)" FE_LEX="$(FE_LEX)" \
		FE_AR="$(FE_AR)" FE_RANLIB="$(FE_RANLIB)" \
		FE_CC="$(FE_CC)" FE_CCFLAGS="$(FE_CCFLAGS)" \
		FE_LDCC="$(FE_LDCC)" FE_LDCCFLAGS="$(FE_LDCCFLAGS)" \
		FE_YACCLEX_LIBS="$(FE_YACCLEX_LIBS)" FE_SYSLIB="$(FE_SYSLIB)"
	@if [[ ! -f $(ODBSQL) ]] || [[ $(LIBODBSQLCOMPILER) -nt $(ODBSQL) ]] ; then \
	  [[ "$(SHLIB_CMD)" = none ]] || touch $(LIBODBSQLCOMPILER); \
	  echo "$(FE_AR_EXTRACT) $(LIBODBSQLCOMPILER) odb98.o"; \
	        $(FE_AR_EXTRACT) $(LIBODBSQLCOMPILER) odb98.o ; \
	  ls -ltr $(LIBODBSQLCOMPILER) $(ODBSQL) 2>/dev/null || : ; \
	  sleep 2; \
	  echo   "$(FE_LDCC) -L$(ROOTDIR) $(FE_LDCCFLAGS) odb98.o $(ODBSQLCOMPILER_LIB) $(FE_YACCLEX_LIBS) $(FE_SYSLIB) -o $(ODBSQL)"; \
	  set -e; $(FE_LDCC) -L$(ROOTDIR) $(FE_LDCCFLAGS) odb98.o $(ODBSQLCOMPILER_LIB) $(FE_YACCLEX_LIBS) $(FE_SYSLIB) -o $(ODBSQL) ; \
	  \rm -f odb98.o ; \
	  ls -ltr $(LIBODBSQLCOMPILER) $(ODBSQL) 2>/dev/null || : ; \
	fi
	@$(TIMESTAMPER) "End" "ODBSQL" "`basename $@`"

_odbsqlbecompiler_ :$(TIMESTAMP_EXE)  _print_objs
	@$(TIMESTAMPER) "Start" "_odbsqlbecompiler_" ""
	@set -e; \
	if [[ $(BACKENDTOO) = yes ]] ; then \
	  dirs=`$(ROOTDIR)/make_depend $(NODB) -a none -g -l odbsqlbecompiler 2>/dev/null || :`; \
	  for d in $$dirs; do \
	    \cd $(ROOTDIR)/odb/$$d; \
	    $(MAKE) ROOTDIR=$(ROOTDIR) TASK=odbsqlbecompiler $(OPTS) $(SHLIB) $(PARALLEL_GMAKE); \
	  done \
	fi
	@$(TIMESTAMPER) "End" "_odbsqlbecompiler_" ""

$(ODBSQLBE):$(TIMESTAMP_EXE)  _print_objs
	@$(TIMESTAMPER) "Start" "ODBSQLBE" "`basename $@`"
	@set -e; \
	 if [[ $(BACKENDTOO) = yes ]] ; then \
	  \cd $(ROOTDIR)/odb; \
	  $(MAKE) ROOTDIR=$(ROOTDIR) ARCH=$(ARCH) TASK=odbsqlbecompiler ODBSQL=$(ODBSQLBE) ; \
	  if [[ ! -f $(ODBSQLBE) ]] || [[ $(LIBODBSQLBECOMPILER) -nt $(ODBSQLBE) ]] ; then \
	     [[ "$(SHLIB_CMD)" = none ]] || touch $(LIBODBSQLBECOMPILER); \
	    echo "$(AR_EXTRACT) $(LIBODBSQLBECOMPILER) odb98.o"; \
	          $(AR_EXTRACT) $(LIBODBSQLBECOMPILER) odb98.o ; \
	    ls -ltr $(LIBODBSQLBECOMPILER) $(ODBSQL) 2>/dev/null || : ; \
	    sleep 2; \
	    echo "$(LDCC) -L$(ROOTDIR) $(LDCCFLAGS) odb98.o $(ODBSQLBECOMPILER_LIB) $(YACCLEX_LIBS) $(SYSLIB) -o $(ODBSQLBE)"; \
	          $(LDCC) -L$(ROOTDIR) $(LDCCFLAGS) odb98.o $(ODBSQLBECOMPILER_LIB) $(YACCLEX_LIBS) $(SYSLIB) -o $(ODBSQLBE); \
	    \rm -f odb98.o ; \
	    ls -ltr $(LIBODBSQLBECOMPILER) $(ODBSQL) 2>/dev/null || : ; \
	  fi \
	 fi
	@$(TIMESTAMPER) "End" "ODBSQLBE" "`basename $@`"

_odb_ :$(TIMESTAMP_EXE)  _print_objs
	@$(TIMESTAMPER) "Start" "_odb_" ""
	dirs=`$(ROOTDIR)/make_depend $(NODB) -a none -g -l odb 2>/dev/null || :`; \
	for d in $$dirs; do \
	  set -e; \cd $(ROOTDIR)/odb/$$d; \
	  $(MAKE) ROOTDIR=$(ROOTDIR) TASK=odb $(OPTS) $(SHLIB) $(PARALLEL_GMAKE); \
	done
	@$(TIMESTAMPER) "End" "_odb_" ""

_odbdummy_: $(TIMESTAMP_EXE) 	_print_objs
	@$(TIMESTAMPER) "Start" "_odbdummy_" ""
	dirs=`$(ROOTDIR)/make_depend $(NODB) -a none -g -l odbdummy 2>/dev/null || :`; \
	for d in $$dirs; do \
	  set -e; \cd $(ROOTDIR)/odb/$$d; \
	  $(MAKE) ROOTDIR=$(ROOTDIR) TASK=odbdummy $(OPTS) $(SHLIB) $(PARALLEL_GMAKE); \
	done
	@$(TIMESTAMPER) "End" "_odbdummy_" ""

_odbport_: $(TIMESTAMP_EXE) 	_print_objs
	@$(TIMESTAMPER) "Start" "_odbport_" ""
	dirs=`$(ROOTDIR)/make_depend $(NODB) -a none -g -l odbport 2>/dev/null || :`; \
	for d in $$dirs; do \
	  set -e; \cd $(ROOTDIR)/odb/$$d; \
	  $(MAKE) ROOTDIR=$(ROOTDIR) TASK=odbport $(OPTS) $(SHLIB) $(PARALLEL_GMAKE); \
	done
	@$(TIMESTAMPER) "End" "_odbport_" ""

_odbmain_: $(TIMESTAMP_EXE) 	_print_objs
	@$(TIMESTAMPER) "Start" "_odbmain_" ""
	dirs=`$(ROOTDIR)/make_depend $(NODB) -a none -g -l odbmain 2>/dev/null || :`; \
	for d in $$dirs; do \
	  set -e; \cd $(ROOTDIR)/odb/$$d; \
	  $(MAKE) ROOTDIR=$(ROOTDIR) TASK=odbmain $(OPTS) $(SHLIB) $(PARALLEL_GMAKE); \
	done
	@$(TIMESTAMPER) "End" "_odbmain_" ""

_obstat_: $(TIMESTAMP_EXE) 	_print_objs
	@$(TIMESTAMPER) "Start" "_obstat_" ""
	dirs=`$(ROOTDIR)/make_depend $(NODB) -a none -g -l obstat 2>/dev/null || :`; \
	for d in $$dirs; do \
	  set -e; \cd $(ROOTDIR)/obstat/$$d; \
	  $(MAKE) ROOTDIR=$(ROOTDIR) TASK=obstat $(OPTS) $(SHLIB) $(PARALLEL_GMAKE); \
	done
	@$(TIMESTAMPER) "End" "_obstat_" ""

_ssa_:	$(TIMESTAMP_EXE) 	_print_objs
	@$(TIMESTAMPER) "Start" "_ssa_" ""
	dirs=`$(ROOTDIR)/make_depend $(NODB) -a none -g -l ssa 2>/dev/null || :`; \
	for d in $$dirs; do \
	  set -e; \cd $(ROOTDIR)/ssa/$$d; \
	  $(MAKE) ROOTDIR=$(ROOTDIR) TASK=ssa $(OPTS) $(SHLIB) $(PARALLEL_GMAKE); \
	done
	@$(TIMESTAMPER) "End" "_ssa_" ""

_$(ODBASE)_: $(TIMESTAMP_EXE) 	_print_objs
	@$(TIMESTAMPER) "Start" "ODBASE" "`basename $@`"
	dirs=`$(ROOTDIR)/make_depend -a none -g -d $(ODBASE) 2>/dev/null || :`; \
	for d in $$dirs; do \
	  set -e; \cd $(ROOTDIR)/odb/$$d; \
	  $(MAKE) ROOTDIR=$(ROOTDIR) TASK=$(ODBASE) ODBASE=$(ODBASE) $(OPTS) $(SHLIB) $(PARALLEL_GMAKE); \
	  $(CREATE_STATIC_STUBB) $(ODBASE) *.sql; \
	  if [[ ! -f $(ODBASE)_Sstatic.o ]] || \
	     [[ $(ODBASE)_Sstatic.c -nt $(ODBASE)_Sstatic.o ]] ; then \
	    $(CC) -c $(CCINC) $(CCFLAGS) $(ODBASE)_Sstatic.c ; \
	  fi; \
	  if [[ -f $(ODBASE).o ]] ; then \
	    $(MAKE_LIB) -$(VERBOSE)c "$(AR_CREATE)" -u "$(AR_UPDATE)" -t "$(AR_TBLCONT)" -l $(LIB) $(RANLIB) \
		$(ODBASE).o $(ODBASE)_Sstatic.o $(ODBASE)_T_*.o  ; \
	  fi; \
	  $(MAKE_SHLIB) -i "$(SHLIB_LIST)" -c "$(SHLIB_CMD)" -s $(SHLIB_SFX) -l $(LIB) $(RANLIB) ; \
	done
	@$(TIMESTAMPER) "End" "ODBASE" "`basename $@`"

tarball tarfile distrib distribution package:
	chmod u+rx ./make_tarball$(SUBPRODUCT)
	./make_tarball$(SUBPRODUCT) -c $(CYCLE) -v $(OPTS) -r $(ROOTDIR)
	@\rm -f $(TIMESTAMP_EXE)

drhook_tarball drhook_tarfile drhook_distrib drhook_distribution drhook_package \
tarball_drhook tarfile_drhook distrib_drhook distribution_drhook package_drhook:
	chmod u+rx ./make_tarball_drhook
	./make_tarball_drhook -c $(CYCLE) -v $(OPTS) -r $(ROOTDIR)
	@\rm -f $(TIMESTAMP_EXE)

fclibs: $(TIMESTAMP_EXE) 
	@$(TIMESTAMPER) "Start" "fclibs" ""
	chmod u+rx ./make_fclibs
	./make_fclibs -a $(ARCH) >$(ROOTDIR)/ODB_FCLIBS 2>/dev/null || :
	@$(TIMESTAMPER) "End" "fclibs" ""

depend: $(TIMESTAMP_EXE) 
	@$(TIMESTAMPER) "Start" "depend" ""
	chmod u+rx ./make_depend
	./make_depend $(NODB) -r `pwd` -a $(ARCH) $(MODUPCASE) -m $(MOD_EXT) $(OPTS)
	@$(TIMESTAMPER) "End" "depend" ""

libs: $(TIMESTAMP_EXE) 
	@$(TIMESTAMPER) "Start" "libs" ""
	set -e; \cd $(ROOTDIR)/odb; \
	$(MAKE) ROOTDIR=$(ROOTDIR) ARCH=$(ARCH) TASK=odbsqlcompiler \
		FE_YACC="$(FE_YACC)" FE_LEX="$(FE_LEX)" \
		FE_CC="$(FE_CC)" FE_CCFLAGS="$(FE_CCFLAGS)" \
		FE_AR="$(FE_AR)" FE_RANLIB="$(FE_RANLIB)" \
		FE_LDCC="$(FE_LDCC)" FE_LDCCFLAGS="$(FE_LDCCFLAGS)" \
		FE_YACCLEX_LIBS="$(FE_YACCLEX_LIBS)" FE_SYSLIB="$(FE_SYSLIB)" \
		$(ODBSQL)
	if [[ $(BACKENDTOO) = yes ]] ; then \
	  set -e; \cd $(ROOTDIR)/odb; \
	  $(MAKE) ROOTDIR=$(ROOTDIR) ARCH=$(ARCH) TASK=odbsqlbecompiler ; \
	else \
	  set -e; \cd $(ROOTDIR); \
	  \rm -f $(ODBSQLBE) $(LIBODBSQLBECOMPILER) ; \
	  \ln -s `basename $(ODBSQL)` `basename $(ODBSQLBE)` ; \
	  \ln -s `basename $(LIBODBSQLCOMPILER)` `basename $(LIBODBSQLBECOMPILER)` ; \
	  \ls -ltr $(ODBSQL) $(LIBODBSQLCOMPILER) $(ODBSQLBE) $(LIBODBSQLBECOMPILER) || : ; \
	fi
	set -e; \cd $(ROOTDIR)/ifsaux; \
	$(MAKE) ROOTDIR=$(ROOTDIR) ARCH=$(ARCH) TASK=ifsaux
	set -e; \cd $(ROOTDIR)/odb; \
	$(MAKE) ROOTDIR=$(ROOTDIR) ARCH=$(ARCH) TASK=mpi_serial
	set -e; \cd $(ROOTDIR)/odb; \
	$(MAKE) ROOTDIR=$(ROOTDIR) ARCH=$(ARCH) TASK=odbec
	set -e; \cd $(ROOTDIR)/odb; \
	$(MAKE) ROOTDIR=$(ROOTDIR) ARCH=$(ARCH) TASK=odbemos
	set -e; \cd $(ROOTDIR)/ifs; \
	$(MAKE) ROOTDIR=$(ROOTDIR) ARCH=$(ARCH) TASK=ifs
	tlist="odb odbdummy odbport odbmain"; \
	for t in $$tlist; do \
	  echo "Processing $$t of odb"; \
	  set -e; \cd $(ROOTDIR)/odb; \
	  $(MAKE) ROOTDIR=$(ROOTDIR) ARCH=$(ARCH) TASK=$$t; \
	done
	set -e; \cd $(ROOTDIR)/obstat; \
	$(MAKE) ROOTDIR=$(ROOTDIR) ARCH=$(ARCH) TASK=obstat
	set -e; \cd $(ROOTDIR)/ssa; \
	$(MAKE) ROOTDIR=$(ROOTDIR) ARCH=$(ARCH) TASK=ssa
	tlist="$(KNOWN_DBS)"; \
	for t in $$tlist; do \
	  echo "Processing database $$t"; \
	  set -e; \cd $(ROOTDIR)/odb; \
	  $(MAKE) ROOTDIR=$(ROOTDIR) ARCH=$(ARCH) TASK=$$t ODBASE=$$t; \
	done
	chmod ug+rx $(ROOTDIR)/make_install
	@$(TIMESTAMPER) "End" "libs" ""

drhook: $(TIMESTAMP_EXE) 
	@$(TIMESTAMPER) "Start" "drhook" ""
	set -e; \cd $(ROOTDIR)/odb; \
	$(MAKE) ROOTDIR=$(ROOTDIR) ARCH=$(ARCH) TASK=mpi_serial _mpi_serial_
	set -e; \cd $(ROOTDIR)/ifsaux; \
	$(MAKE) ROOTDIR=$(ROOTDIR) ARCH=$(ARCH) TASK=drhook _drhook_
	set -e; \cd $(ROOTDIR)/odb; \
	$(MAKE) ROOTDIR=$(ROOTDIR) ARCH=$(ARCH) TASK=odbmain _odbmain_
	set -e; \cd $(ROOTDIR)/odb; \
	$(MAKE) ROOTDIR=$(ROOTDIR) ARCH=$(ARCH) TASK=odbdummy _odbdummy_
	\cp -p $(ROOTDIR)/ifsaux/module/*.$(MOD_EXT) .
	\cp -p $(ROOTDIR)/ifsaux/include/drhook.h .
	\cp -p $(ROOTDIR)/ifsaux/include/cargs.h .
	chmod a+r,u+w *.$(MOD_EXT) *.h
	flist="insert_drhook insert_drhook.pl Fortran90_stuff.pm mpirun* run*fe"; \
	for f in $$flist; do \
	  \rm -f $$f; \
	  \ln -s odb/scripts/$$f .; \
	  chmod a=r,u+w $$f; \
	done; \
	chmod a+x insert_drhook; \
	chmod a+x mpirun*; \
	chmod a+x run*fe; \
	pwd; \
	\ls -ltr $$flist *.$(MOD_EXT) libdrhook.* libmpi_serial.* libodbmain.* libodbdummy.*
	@$(TIMESTAMPER) "End" "drhook" ""

odbclient: $(TIMESTAMP_EXE) 	
	@$(TIMESTAMPER) "Start" "odbclient" ""
	libs="$(LIBODBCLIENT)"; \
	for x in $$libs; do \
	  set -e; $(MAKE) ROOTDIR=$(ROOTDIR) ARCH=$(ARCH) $(OPTS) $$x; \
	done
	-@\ls -ltr $(LIBODBCLIENT)
	@$(TIMESTAMPER) "End" "odbclient" ""

exes: $(TIMESTAMP_EXE) 	
	@$(TIMESTAMPER) "Start" "exes" ""
	exes="$(UTILITY_EXES) $(APPLICATION_EXES)"; \
	for x in $$exes; do \
	  set -e; $(MAKE) ROOTDIR=$(ROOTDIR) ARCH=$(ARCH) $(OPTS) $$x; \
	done
	-@\ls -ltr $(UTILITY_EXES) $(APPLICATION_EXES)
	@$(TIMESTAMPER) "End" "exes" ""

drhook_exes: $(TIMESTAMP_EXE) 
	@$(TIMESTAMPER) "Start" "drhook_exes" ""
	exes="$(DRHOOK_EXES)"; \
	for x in $$exes; do \
	  set -e; $(MAKE) ROOTDIR=$(ROOTDIR) ARCH=$(ARCH) $(OPTS) $$x; \
	done
	-@\ls -ltr $(DRHOOK_EXES)
	@$(TIMESTAMPER) "End" "drhook_exes" ""

_none_:	libs odbclient exes

_bm_: $(TIMESTAMP_EXE) 
	@$(TIMESTAMPER) "Start" "_bm_" ""
	exes="$(BM_EXES)"; \
	for x in $$exes; do \
	  set -e; $(MAKE) ROOTDIR=$(ROOTDIR) ARCH=$(ARCH) $(OPTS) $$x; \
	done
	-@\ls -ltr $(BM_EXES)
	@$(TIMESTAMPER) "End" "_bm_" ""

install: $(TIMESTAMP_EXE) 
	@$(TIMESTAMPER) "Start" "install" ""
	./make_install \
          -L "$(CPP)" \
	  -d $(INSTALL_DIR) \
	  -r $(ROOTDIR) \
	  -s "$(INSTALL_SCRIPTS)" \
	  -e "$(INSTALL_EXES)" \
	  -l "$(INSTALL_LIBS)" \
	  -S "$(SHLIB_SFX)" \
	  -i "ODB_FCLIBS" \
	  -i "SHLIB" \
	  -i "odb/ddl/odb98.flags" \
	  -i "odb/include/*.h" \
	  -i "odb/interface/*.h" \
	  -I "odb/ddl.CCMA/CCMA.ddl" \
          -I "odb/ddl.CCMA/*.sql" \
	  -I "odb/ddl.ECMA/ECMA.ddl" \
          -I "odb/ddl.ECMA/*.sql" \
	  -I "odb/ddl.ECMASCR/ECMASCR.ddl" \
          -I "odb/ddl.ECMASCR/*.sql" \
	  -m "odb/module/*.$(MOD_EXT)" \
	  -m "ifsaux/module/*.$(MOD_EXT)" \
	  -i "ifsaux/include/*.h" \
	  -i "ifs/common/*.h" \
	  -i "AUTO_INTFB/*.h" \
	  -m "ifs/module/*.$(MOD_EXT)" \
	  -m "obstat/module/*.$(MOD_EXT)" \
	  -c "odb/scripts/*.cmap" \
	  -c "odb/scripts/general.cfg" \
	  -c "odb/scripts/bufrodbcodes.cfg" \
	  -c "odb/scripts/stat.ref" \
	  -c "odb/scripts/airs_channels" \
	  -g
	@$(TIMESTAMPER) "End" "install" ""

test:
	@echo "***Error: Test not implemented yet"
	@exit 1 

#
#$(ODBASE).ddl_:  $(ODBSQL)
#
#.ddl.ddl_:
#	@if [[ ! -f $*.ddl_ ]] || [[ $(ODBSQL) -nt $*.ddl_ ]] || \
#	    [[ ! -f $*.c    ]] || [[ ! -f $*.h    ]] ; then \
#	  echo "$(ODB_COMPILE_DDL) $<"; \
#	        $(ODB_COMPILE_DDL) $< ; \
#	fi
#

.ddl_.o: $(TIMESTAMP_EXE) 
	@$(TIMESTAMPER) "Start" ".ddl_.o" "$*"
	@set -e; flist=`\ls -C1 $*.c $*_T_*.c`; \
	objs=""; \
	for f in $$flist; do \
	  obj=`basename $$f .c`.o; \
	  if [[ ! -f $$obj ]] || [[ $$f -nt $$obj ]] ; then \
	    echo "$(CC) -c $(CCINC) $(CCFLAGS) $$f"; \
	          $(CC) -c $(CCINC) $(CCFLAGS) $$f ; \
	    objs="$$objs $$obj"; \
	  fi; \
	done ; \
	echo "$(AR_UPDATE) $(LIB) $$objs" ; \
	      $(AR_UPDATE) $(LIB) $$objs  ; \
	$(MAKE_SHLIB) -i "$(SHLIB_LIST)" -c "$(SHLIB_CMD)" -s $(SHLIB_SFX) -u "$(AR_UPDATE)" -l $(LIB) $(RANLIB) $$objs
	@$(TIMESTAMPER) "End" ".ddl_.o" "$*"

#-- The following seems have to appear before the ".c.o:" -rule
#   or otherwise that rule will take the precedence!
.sql.o: $(TIMESTAMP_EXE) 
	@$(TIMESTAMPER) "Start" ".sql.o" "$(ODBASE)_$*"
	@set -e; obj=$(ODBASE)_$*.o; \
	if [[ ! -f $$obj ]] || [[ $(ODBSQL) -nt $$obj ]] || \
	   [[ $*.sql -nt $$obj ]] || \
	   [[ $(ODBASE).ddl_ -nt $$obj ]] ; then \
	  echo "$(ODB_COMPILE_SQL) $<" ; \
	        $(ODB_COMPILE_SQL) $< ; \
	  echo "$(CC) -c $(CCINC) $(CCFLAGS) $(ODBASE)_$*.c" ; \
	        $(CC) -c $(CCINC) $(CCFLAGS) $(ODBASE)_$*.c ; \
	  \rm -f $*.o ; \
	  \ln -s $(ODBASE)_$*.o $*.o ; \
	fi
	@$(TIMESTAMPER) "End" ".sql.o" "$(ODBASE)_$*"

.y.o: $(TIMESTAMP_EXE) 
	@$(TIMESTAMPER) "Start" ".y.o" "$*"
	@set -e; \
	if [[ -f yacc.y && ! -L yacc.y ]] ; then \
	  echo "$(FE_YACC) $<" ; \
	        $(FE_YACC) $<  ; \
	  echo "mv y.tab.c $*.c" ; \
	       \mv y.tab.c $*.c  ; \
	  echo "$(FE_CC) -c $(CCINC) $(FE_CCFLAGS) $*.c" ; \
	        $(FE_CC) -c $(CCINC) $(FE_CCFLAGS) $*.c  ; \
	else \
	  echo "$(YACC) $<" ; \
	        $(YACC) $<  ; \
	  echo "mv y.tab.c $*.c" ; \
	       \mv y.tab.c $*.c  ; \
	  echo "$(CC) -c $(CCINC) $(CCFLAGS) $*.c" ; \
	        $(CC) -c $(CCINC) $(CCFLAGS) $*.c  ; \
	fi
	@$(TIMESTAMPER) "End" ".y.o" "$*"

.l.o: $(TIMESTAMP_EXE) 
	@$(TIMESTAMPER) "Start" ".l.o" "$*"
	@set -e; \
	if [[ -f yacc.y && ! -L yacc.y ]] ; then \
	  echo "$(FE_LEX) $<" ; \
	        $(FE_LEX) $<  ; \
	  echo "mv lex.yy.c $*.c" ; \
	       \mv lex.yy.c $*.c  ; \
	  echo "$(FE_CC) -c $(CCINC) $(FE_CCFLAGS) $*.c" ; \
	        $(FE_CC) -c $(CCINC) $(FE_CCFLAGS) $*.c  ; \
	else \
	  echo "$(LEX) $<" ; \
	        $(LEX) $<  ; \
	  echo "mv lex.yy.c $*.c" ; \
	       \mv lex.yy.c $*.c  ; \
	  echo "$(CC) -c $(CCINC) $(CCFLAGS) $*.c" ; \
	        $(CC) -c $(CCINC) $(CCFLAGS) $*.c  ; \
	fi
	@$(TIMESTAMPER) "End" ".l.o" "$*"

.h.ok: $(TIMESTAMP_EXE) 
	@$(TIMESTAMPER) "Start" ".h.ok" "$*"
	touch $*.ok
	@$(TIMESTAMPER) "End" ".h.ok" "$*"

.hh.ok: $(TIMESTAMP_EXE) 
	@$(TIMESTAMPER) "Start" ".hh.ok" "$*"
	touch $*.ok
	@$(TIMESTAMPER) "End" ".hh.ok" "$*"

.c.o: $(TIMESTAMP_EXE) 
	@$(TIMESTAMPER) "Start" ".c.o" "$*"
	@set -e; \
	if [[ -f yacc.y && ! -L yacc.y ]] ; then \
	  echo "$(FE_CC) -c $(CCINC) $(FE_CCFLAGS) $<" ;\
	        $(FE_CC) -c $(CCINC) $(FE_CCFLAGS) $<  ;\
	else \
	  echo "$(CC) -c $(CCINC) $(CCFLAGS) $<" ;\
	        $(CC) -c $(CCINC) $(CCFLAGS) $<  ;\
	fi
	@$(TIMESTAMPER) "End" ".c.o" "$*"

.cc.o: $(TIMESTAMP_EXE) 
	@$(TIMESTAMPER) "Start" ".cc.o" "$*"
	$(CCPLUS) -c $(CCPLUSINC) $(CCPLUSFLAGS) $<
	@$(TIMESTAMPER) "End" ".cc.o" "$*"

#.F90.o .F90.$(MOD_EXT):
.F90.o: $(TIMESTAMP_EXE) 
	@$(TIMESTAMPER) "Start" ".F90.o" "$*"
	@set -e; dn=`dirname $<`; bn=`basename $<`; \
	if [[ "$$dn" = "." ]] ; then \
	  echo   "$(FC) -c $(FCINC) $(FCFLAGS) $(FCFREEFMT) $$bn"; \
	          $(FC) -c $(FCINC) $(FCFLAGS) $(FCFREEFMT) $$bn ; \
	else \
	  \cd $$dn ; \
	  echo   "$(FC) -c $(FCINC) $(FCFLAGS) $(FCFREEFMT) $$bn"; \
	          $(FC) -c $(FCINC) $(FCFLAGS) $(FCFREEFMT) $$bn ; \
	fi
	@$(TIMESTAMPER) "End" ".F90.o" "$*"

.F.o: $(TIMESTAMP_EXE) 
	@$(TIMESTAMPER) "Start" ".F.o" "$*"
	$(FC) -c $(FCINC) $(FCFLAGS) $(FCFIXEDFMT) $<
	@$(TIMESTAMPER) "End" ".F.o" "$*"

#-- Prints ODB-version (as in odb/lib/version.c)
$(EXEDIR)/odb_version $(EXEDIR)/odbversion:  $(TIMESTAMP_EXE) $(LIBODBMAIN) $(LIBCSTD) $(LIBODBDUMMY)
	@$(TIMESTAMPER) "Start" "odbversion" ""
	$(AR_EXTRACT) $(LIBODBMAIN) odb_version.o $(XLDUM)
	$(LDCC) -L$(ROOTDIR) $(LDCCFLAGS) odb_version.o $(XLDUM) $(CSTD_LIB) $(LDCCLIBS) $(ODBDUMMY_LIB) \
		$(SYSLIB) -o $(EXEDIR)/odb_version$(DX)
	\cd $(EXEDIR); \ln -f -s odb_version$(DX) odbversion
	@\rm -f odb_version.o $(XLDUM)
	@$(TIMESTAMPER) "End" "odbversion" ""

#-- Translates I/O-method#4 database files back to I/O-method#1 format (and vice versa)
$(EXEDIR)/hcat:  $(TIMESTAMP_EXE) $(LIBODBMAIN) $(LIBCSTD) $(LIBODBDUMMY)
	@$(TIMESTAMPER) "Start" "hcat" ""
	$(AR_EXTRACT) $(LIBODBMAIN) hcat.o $(XLDUM)
	$(LDCC) -L$(ROOTDIR) $(LDCCFLAGS) hcat.o $(XLDUM) $(CSTD_LIB) $(LDCCLIBS) $(ODBDUMMY_LIB) \
		$(SYSLIB) -o $(EXEDIR)/hcat$(DX)
	@\rm -f hcat.o $(XLDUM)
	@$(TIMESTAMPER) "End" "hcat" ""

#-- QTAR-file (guitar) access utility
$(EXEDIR)/qtar:  $(TIMESTAMP_EXE) $(LIBODBMAIN) $(LIBCSTD) $(LIBODBDUMMY)
	@$(TIMESTAMPER) "Start" "qtar" ""
	$(AR_EXTRACT) $(LIBODBMAIN) qtar.o $(XLDUM)
	$(LDCC) -L$(ROOTDIR) $(LDCCFLAGS) qtar.o $(XLDUM) $(CSTD_LIB) $(LDCCLIBS) $(ODBDUMMY_LIB) \
		$(SYSLIB) -o $(EXEDIR)/qtar$(DX)
	@\rm -f qtar.o $(XLDUM)
	@$(TIMESTAMPER) "End" "qtar" ""

#-- Calculates the number of (say) BUFR-messages in a file
$(EXEDIR)/numproducts:  $(TIMESTAMP_EXE) $(LIBODBMAIN) $(LIBCSTD) $(LIBODBDUMMY)
	@$(TIMESTAMPER) "Start" "numproducts" ""
	$(AR_EXTRACT) $(LIBODBMAIN) numproducts.o $(XLDUM)
	$(LDCC) -L$(ROOTDIR) $(LDCCFLAGS) numproducts.o $(XLDUM) $(CSTD_LIB) $(LDCCLIBS) $(ODBDUMMY_LIB) \
		$(SYSLIB) -o $(EXEDIR)/numproducts$(DX)
	@\rm -f numproducts.o $(XLDUM)
	@$(TIMESTAMPER) "End" "numproducts" ""

#-- Little/endian and vice versa test program
$(EXEDIR)/conv_le2be $(EXEDIR)/conv_be2le:  $(TIMESTAMP_EXE) $(LIBODBMAIN) $(LIBCSTD) $(LIBODBDUMMY)
	@$(TIMESTAMPER) "Start" "conv_le2be" ""
	$(AR_EXTRACT) $(LIBODBMAIN) conv_le2be.o $(XLDUM)
	$(LDCC) -L$(ROOTDIR) $(LDCCFLAGS) conv_le2be.o $(XLDUM) $(CSTD_LIB) $(LDCCLIBS) $(ODBDUMMY_LIB) \
	    $(SYSLIB) -o $(EXEDIR)/conv_le2be$(DX)
	@\rm -f $(EXEDIR)/conv_be2le
	\cd $(EXEDIR); \ln -f -s conv_le2be$(DX) conv_be2le
	@\rm -f conv_le2be.o $(XLDUM)
	@$(TIMESTAMPER) "End" "conv_le2be" ""

#-- Re-creates DDL-file from DD-metadata file
$(EXEDIR)/dd2ddl.x:  $(TIMESTAMP_EXE) $(LIBODBMAIN) $(LIBCSTD) $(LIBODBDUMMY)
	@$(TIMESTAMPER) "Start" "dd2ddl.x" ""
	$(AR_EXTRACT) $(LIBODBMAIN) dd2ddl.o $(XLDUM)
	$(LDCC) -L$(ROOTDIR) $(LDCCFLAGS) dd2ddl.o $(XLDUM) $(CSTD_LIB) $(LDCCLIBS) $(ODBDUMMY_LIB) $(SYSLIB) -o $@
	@\rm -f dd2ddl.o $(XLDUM)
	@$(TIMESTAMPER) "End" "dd2ddl.x" ""

#-- Provides mapping and I/O-parameters between logical and physical files (invoked by cma_open)
$(EXEDIR)/ioassign:  $(TIMESTAMP_EXE) $(LIBODBMAIN) $(LIBCSTD) $(LIBODBDUMMY)
	@$(TIMESTAMPER) "Start" "ioassign" ""
	$(AR_EXTRACT) $(LIBODBMAIN) ioassign.o $(XLDUM)
	$(LDCC) -L$(ROOTDIR) $(LDCCFLAGS) ioassign.o $(XLDUM) $(CSTD_LIB) $(LDCCLIBS) $(ODBDUMMY_LIB) \
		$(SYSLIB) -o $(EXEDIR)/ioassign$(DX)
	@\rm -f ioassign.o $(XLDUM)
	@$(TIMESTAMPER) "End" "ioassign" ""

#-- Create MR2D (memory resident to disk) -formatted file from a number of component files
$(EXEDIR)/mr2d_create:  $(TIMESTAMP_EXE) $(LIBODBMAIN) $(LIBCSTD) $(LIBODBDUMMY)
	@$(TIMESTAMPER) "Start" "mr2d_create" ""
	$(AR_EXTRACT) $(LIBODBMAIN) mr2d_create.o $(XLDUM)
	$(LDCC) -L$(ROOTDIR) $(LDCCFLAGS) mr2d_create.o $(XLDUM) $(CSTD_LIB) $(LDCCLIBS) $(ODBDUMMY_LIB) \
		$(SYSLIB) -o $(EXEDIR)/mr2d_create$(DX)
	@\rm -f mr2d_create.o $(XLDUM)
	@$(TIMESTAMPER) "End" "mr2d_create" ""

#-- Split MR2D (memory resident to disk) -formatted file back to its component files
$(EXEDIR)/mr2d_split:  $(TIMESTAMP_EXE) $(LIBODBMAIN) $(LIBCSTD) $(LIBODBDUMMY)
	@$(TIMESTAMPER) "Start" "mr2d_split" ""
	$(AR_EXTRACT) $(LIBODBMAIN) mr2d_split.o $(XLDUM)
	$(LDCC) -L$(ROOTDIR) $(LDCCFLAGS) mr2d_split.o $(XLDUM) $(CSTD_LIB) $(LDCCLIBS) $(ODBDUMMY_LIB) \
		$(SYSLIB) -o $(EXEDIR)/mr2d_split$(DX)
	@\rm -f mr2d_split.o $(XLDUM)
	@$(TIMESTAMPER) "End" "mr2d_split" ""

#-- CMA-packing/unpacking; per CMA-report
$(EXEDIR)/pcma $(EXEDIR)/upcma:  $(TIMESTAMP_EXE) $(LIBODBMAIN) $(LIBCSTD) $(LIBODBDUMMY)
	@$(TIMESTAMPER) "Start" "upcma" ""
	$(AR_EXTRACT) $(LIBODBMAIN) pcma_main.o $(XLDUM)
	$(LDCC) -L$(ROOTDIR) $(LDCCFLAGS) pcma_main.o $(XLDUM) $(CSTD_LIB) $(LDCCLIBS) $(ODBDUMMY_LIB) \
	    $(SYSLIB) -o $(EXEDIR)/pcma$(DX)
	@\rm -f $(EXEDIR)/upcma
	\cd $(EXEDIR); \ln -f -s pcma$(DX) upcma
	@\rm -f pcma_main.o $(XLDUM)
	@$(TIMESTAMPER) "End" "upcma" ""

#-- Standalone program to create Direct Column Access -metadata file for ODB-databases
$(EXEDIR)/dcagen.x:  $(TIMESTAMP_EXE) $(LIBODBMAIN) $(LIBCSTD) $(LIBODBDUMMY)
	@$(TIMESTAMPER) "Start" "dcagen.x" ""
	$(AR_EXTRACT) $(LIBODBMAIN) dcagen.o $(XLDUM)
	$(LDCC) -L$(ROOTDIR) $(LDCCFLAGS) dcagen.o $(XLDUM) $(CSTD_LIB) $(LDCCLIBS) $(ODBDUMMY_LIB) $(SYSLIB) -o $@
	@\rm -f dcagen.o $(XLDUM)
	@$(TIMESTAMPER) "End" "dcagen.x" ""

#-- ODB-calculator or Unix-command Schnautzer
$(EXEDIR)/odbcalc.x:  $(TIMESTAMP_EXE) $(LIBODBMAIN) $(LIBSTD) $(LIBODBDUMMY) $(LIBEC)
	@$(TIMESTAMPER) "Start" "odbcalc.x" ""
	$(AR_EXTRACT) $(LIBODBMAIN) Fodbcalc.o
	$(LDFC_SERIAL) -L$(ROOTDIR) $(LDFCFLAGS) Fodbcalc.o \
	  $(ODB_LIB) $(IFSAUX_LIB) $(ODB_LIB) $(LDFCLIBS) $(IFSAUX_LIB) \
	  $(LIBEC) $(LIBCURSES) $(MPILIB_SERIAL) \
	  $(ODBDUMMY_LIB) $(SYSLIB) -o $@
	@\rm -f Fodbcalc.o
	@$(TIMESTAMPER) "End" "odbcalc.x" ""

#-- Generic ODB/SQL interpreter & data accesser w/o need to create and/or link C-code of a specific database
$(EXEDIR)/odbsql.x:  $(TIMESTAMP_EXE) $(LIBODBMAIN) $(LIBSTD) $(LIBODBDUMMY) $(LIBEC)
	@$(TIMESTAMPER) "Start" "odbsql.x" ""
	$(AR_EXTRACT) $(LIBODBMAIN) Fodbsql.o
	$(LDFC_SERIAL) -L$(ROOTDIR) $(LDFCFLAGS) Fodbsql.o \
	  $(ODB_LIB) $(IFSAUX_LIB) $(ODB_LIB) $(LDFCLIBS) $(IFSAUX_LIB) \
	  $(LIBEC) $(LIBCURSES) $(MPILIB_SERIAL) \
	  $(ODBDUMMY_LIB) $(SYSLIB) -o $@
	@\rm -f Fodbsql.o
	@$(TIMESTAMPER) "End" "odbsql.x" ""

#-- ODB-cksum : should produce the same results as Unix cksum
#   Has got I/O read-error handling and tests ifsaux/support/crc.c
$(EXEDIR)/odbcksum:  $(TIMESTAMP_EXE) $(LIBODBMAIN) $(LIBCSTD) $(LIBODBDUMMY)
	@$(TIMESTAMPER) "Start" "odbcksum" ""
	$(AR_EXTRACT) $(LIBODBMAIN) odbcksum.o $(XLDUM)
	$(LDCC) -L$(ROOTDIR) $(LDCCFLAGS) odbcksum.o $(XLDUM) $(CSTD_LIB) $(LDCCLIBS) $(ODBDUMMY_LIB) \
		$(SYSLIB) -o $(EXEDIR)/odbcksum$(DX)
	@\rm -f odbcksum.o $(XLDUM)
	@$(TIMESTAMPER) "End" "odbcksum" ""

#-- Various ODB tools bundled into a single executable
#   to_odb: Create ODB/ECMA or CCMA from ECMA or CCMA-files (obsolescent => removed)
#   to_ecma: Re-create ECMA-file from ODB/ECMA (obsolescent => removed)
#   shuffle: Create load-balanced CCMA database from ECMA, or
#            create load-balanced ECMA database from ECMASCR, or
#            reverse matchup-mode, where final traj ECMA database updates are passed to CCMA, or
#            matchup-mode, where CCMA database updates are passed back to ECMA

$(EXEDIR)/shuffle.x.gz: \
 $(TIMESTAMP_EXE) $(LIBODBMAIN) $(LIBODBPORT) $(LIBSTD) $(LIBODBDUMMY) $(LIBCCMA) $(LIBECMA) $(LIBECMASCR) $(LIBIFS)
	@$(TIMESTAMPER) "Start" "shuffle.x.gz" ""
	$(CREATE_ODBGLUE) CCMA ECMA ECMASCR
	$(AR_EXTRACT) $(LIBODBMAIN) Odbtools.o 
	$(LDFC) -L$(ROOTDIR) $(LDFCFLAGS) Odbtools.o $(ODBGLUE_OBJ) \
	  $(ODBPORT_LIB) $(IFS_LIB) $(ODB_LIB) $(CCMA_LIB) $(ECMA_LIB) $(ECMASCR_LIB) \
	  $(IFSAUX_LIB) $(ODB_LIB) $(LDFCLIBS) $(IFSAUX_LIB) $(MPILIB) \
	  $(ODBDUMMY_LIB) $(SYSLIB) -o $(EXEDIR)/shuffle.x
	@\rm -f Odbtools.o $(ODBGLUE_ALL)
	$(MYGZIP) $(EXEDIR)/shuffle.x
	@$(TIMESTAMPER) "End" "shuffle.x.gz" ""

#-- Copy files from memory resident file system back to disk and vice versa
#   Fujitsu VPP used to have per CPU a local memory resident filesystem
$(EXEDIR)/mrfs2disk $(EXEDIR)/disk2mrfs:  $(TIMESTAMP_EXE) $(LIBODBMAIN) $(LIBSTD) $(LIBODBDUMMY)
	@$(TIMESTAMPER) "Start" "mrfs2disk" ""
	$(AR_EXTRACT) $(LIBODBMAIN) Mrfscopy.o
	$(LDFC) -L$(ROOTDIR) $(LDFCFLAGS) Mrfscopy.o \
	  $(ODB_LIB) \
	  $(IFSAUX_LIB) $(LDFCLIBS) $(IFSAUX_LIB) $(MPILIB) \
	  $(ODBDUMMY_LIB) $(SYSLIB) -o $(EXEDIR)/mrfs2disk$(DX)
	@\rm -f $(EXEDIR)/disk2mrfs
	\cd $(EXEDIR); \ln -f -s mrfs2disk$(DX) disk2mrfs
	@\rm -f Mrfscopy.o
	@$(TIMESTAMPER) "End" "mrfs2disk" ""

#-- Split & separate BUFR-messages based on BUFR-subtype
$(EXEDIR)/split_bufr_data.x:  $(TIMESTAMP_EXE) $(LIBODBMAIN) $(LIBSTD) $(LIBODBDUMMY)
	@$(TIMESTAMPER) "Start" "split_bufr_data.x" ""
	$(AR_EXTRACT) $(LIBODBMAIN) Split_bufr_data.o
	$(LDFC_SERIAL) -L$(ROOTDIR) $(LDFCFLAGS) Split_bufr_data.o \
	  $(ODB_LIB) \
	  $(IFSAUX_LIB) $(LDFCLIBS) $(IFSAUX_LIB) $(MPILIB_SERIAL) \
	  $(ODBDUMMY_LIB) $(SYSLIB) -o $@
	@\rm -f Split_bufr_data.o
	@$(TIMESTAMPER) "End" "split_bufr_data.x" ""

#-- Split & separate BUFR-messages according to their 4dvar-timeslots
$(EXEDIR)/split_timeslot_bufr_data.x:  $(TIMESTAMP_EXE) $(LIBODBMAIN) $(LIBSTD) $(LIBODBDUMMY)
	@$(TIMESTAMPER) "Start" "split_timeslot_bufr_data.x" ""
	$(AR_EXTRACT) $(LIBODBMAIN) Split_timeslot_bufr_data.o
	$(LDFC_SERIAL) -L$(ROOTDIR) $(LDFCFLAGS) Split_timeslot_bufr_data.o \
	  $(ODB_LIB) \
	  $(IFSAUX_LIB) $(LDFCLIBS) $(IFSAUX_LIB) $(MPILIB_SERIAL) \
	  $(ODBDUMMY_LIB) $(SYSLIB) -o $@
	@\rm -f Split_timeslot_bufr_data.o
	@$(TIMESTAMPER) "End" "split_timeslot_bufr_data.x" ""

#-- Translate new FEEDBACK-BUFR format into old, MARS-compatible format
$(EXEDIR)/fbnew2old.x:  $(TIMESTAMP_EXE) $(LIBODBMAIN) $(LIBODBPORT) $(LIBSTD) $(LIBODBDUMMY) $(LIBIFS)
	@$(TIMESTAMPER) "Start" "fbnew2old.x" ""
	$(AR_EXTRACT) $(LIBODBMAIN) Fbnew2old.o
	$(LDFC) -L$(ROOTDIR) $(LDFCFLAGS) Fbnew2old.o \
	  $(ODBPORT_LIB) $(IFS_LIB) $(ODB_LIB) \
	  $(IFSAUX_LIB) $(LDFCLIBS) $(IFSAUX_LIB) $(MPILIB) \
	  $(ODBDUMMY_LIB) $(SYSLIB) -o $@
	@\rm -f Fbnew2old.o
	@$(TIMESTAMPER) "End" "fbnew2old.x" ""

#-- TXT-bufr_tables => BIN-bufr_tables conversion
$(EXEDIR)/buta.x:  $(TIMESTAMP_EXE) $(LIBODBMAIN) $(LIBSTD) $(LIBEMOS) $(LIBMPI_SERIAL) $(LIBODBDUMMY)
	@$(TIMESTAMPER) "Start" "buta.x" ""
	$(AR_EXTRACT) $(LIBODBMAIN) Buta.o
	$(LDFC_SERIAL) -L$(ROOTDIR) $(LDFCFLAGS) Buta.o \
	  $(STD_LIB) $(EMOS_LIB) $(IFSAUX_LIB) $(MPI_SERIAL_LIB) \
	  $(ODBDUMMY_LIB) $(SYSLIB) -o $@
	@\rm -f Buta.o
	@$(TIMESTAMPER) "End" "buta.x" ""

#-- BUFR-tools
$(EXEDIR)/bufr_add_bias.x \
$(EXEDIR)/bufr_check.x \
$(EXEDIR)/bufr_compress.x \
$(EXEDIR)/bufr_decode.x \
$(EXEDIR)/bufr_filter.x \
$(EXEDIR)/bufr_key.x \
$(EXEDIR)/bufr_merge_tovs.x \
$(EXEDIR)/bufr_nt1.x \
$(EXEDIR)/bufr_ntm.x \
$(EXEDIR)/bufr_obs_filter.x \
$(EXEDIR)/bufr_repack.x \
$(EXEDIR)/bufr_ship_anmh.x \
$(EXEDIR)/bufr_split.x \
	:  $(TIMESTAMP_EXE) $(LIBODBMAIN) $(LIBSTD) $(LIBEMOS) $(LIBMPI_SERIAL) $(LIBODBDUMMY)
	@$(TIMESTAMPER) "Start" "BUFR tools" "`basename $@`"
	$(AR_EXTRACT) $(LIBODBMAIN) `basename $@ .x`.o
	$(LDFC_SERIAL) -L$(ROOTDIR) $(LDFCFLAGS) `basename $@ .x`.o \
	  $(STD_LIB) $(EMOS_LIB) $(IFSAUX_LIB) $(MPI_SERIAL_LIB) \
	  $(ODBDUMMY_LIB) $(SYSLIB) -o $@
	@\rm -f `basename $@ .x`.o
	@$(TIMESTAMPER) "End" "BUFR tools" "`basename $@`"

#-- Input-BUFR to ODB/ECMA conversion
$(EXEDIR)/bufr2odb.x.gz:  $(TIMESTAMP_EXE) $(LIBODBMAIN) $(LIBODBPORT) $(LIBSTD) $(LIBODBDUMMY) \
$(LIBECMA) $(LIBECMASCR) $(LIBIFS)
	@$(TIMESTAMPER) "Start" "bufr2odb.x.gz" ""
	$(CREATE_ODBGLUE) ECMA ECMASCR
	$(AR_EXTRACT) $(LIBODBMAIN) Bufr2odb.o
	$(LDFC) -L$(ROOTDIR) $(LDFCFLAGS) Bufr2odb.o $(ODBGLUE_OBJ) \
	  $(ODBPORT_LIB) $(IFS_LIB) $(ODB_LIB) $(ECMA_LIB) $(ECMASCR_LIB) \
	  $(IFSAUX_LIB) $(LDFCLIBS) $(IFSAUX_LIB) $(ODB_LIB) $(IFSAUX_LIB) $(MPILIB) \
	  $(ODBDUMMY_LIB) $(SYSLIB) -o $(EXEDIR)/bufr2odb.x
	@\rm -f Bufr2odb.o $(ODBGLUE_ALL)
	$(MYGZIP) $(EXEDIR)/bufr2odb.x
	@$(TIMESTAMPER) "End" "bufr2odb.x.gz" ""

#-- ODB/ECMA to FEEDBACK-BUFR -conversion
$(EXEDIR)/odb2bufr.x.gz:  $(TIMESTAMP_EXE) $(LIBODBMAIN) $(LIBODBPORT) $(LIBSTD) $(LIBODBDUMMY) \
$(LIBECMA) $(LIBECMASCR) $(LIBIFS)
	@$(TIMESTAMPER) "Start" "odb2bufr.x.gz" ""
	$(CREATE_ODBGLUE) ECMA ECMASCR
	$(AR_EXTRACT) $(LIBODBMAIN) Odb2bufr.o
	$(LDFC) -L$(ROOTDIR) $(LDFCFLAGS) Odb2bufr.o $(ODBGLUE_OBJ) \
	  $(ODBPORT_LIB) $(IFS_LIB) $(ODB_LIB) $(ECMA_LIB) $(ECMASCR_LIB) \
	  $(IFSAUX_LIB) $(LDFCLIBS) $(IFSAUX_LIB) $(ODB_LIB) $(IFSAUX_LIB) $(MPILIB) \
	  $(ODBDUMMY_LIB) $(SYSLIB) -o $(EXEDIR)/odb2bufr.x
	@\rm -f Odb2bufr.o $(ODBGLUE_ALL)
	$(MYGZIP) $(EXEDIR)/odb2bufr.x
	@$(TIMESTAMPER) "End" "odb2bufr.x.gz" ""

#-- Simulated observations to ECMA (just a test link to see if any unsatisfied externals)
$(EXEDIR)/simulobs2odb.x:  $(TIMESTAMP_EXE) $(LIBODBMAIN) $(LIBODBPORT) $(LIBSTD) $(LIBODBDUMMY) $(LIBECMA)
	@$(TIMESTAMPER) "Start" "simulobs2odb.x" ""
	$(CREATE_ODBGLUE) ECMA
	$(AR_EXTRACT) $(LIBODBMAIN) Simulobs2odb.o
	$(LDFC) -L$(ROOTDIR) $(LDFCFLAGS) Simulobs2odb.o $(ODBGLUE_OBJ) \
	  $(ODB_LIB) $(ECMA_LIB) \
	  $(IFSAUX_LIB) $(ODB_LIB) $(LDFCLIBS) $(IFSAUX_LIB) $(MPILIB) \
	  $(ODBDUMMY_LIB) $(SYSLIB) -o $@
	@\rm -f Simulobs2odb.o $(ODBGLUE_ALL)
	@$(TIMESTAMPER) "End" "simulobs2odb.x" ""

#-- Run a list of Unix-commands under different OpenMP-threads
$(EXEDIR)/fscheduler.x:  $(TIMESTAMP_EXE) $(LIBODBMAIN) $(LIBCSTD) $(LIBODBDUMMY)
	@$(TIMESTAMPER) "Start" "fscheduler.x" ""
	$(AR_EXTRACT) $(LIBODBMAIN) Fscheduler.o
	$(LDFC_SERIAL) -L$(ROOTDIR) $(LDFCFLAGS) Fscheduler.o \
	  $(ODB_LIB) \
	  $(IFSAUX_LIB) $(LDFCLIBS) $(IFSAUX_LIB) $(MPILIB_SERIAL) \
	  $(ODBDUMMY_LIB) $(SYSLIB) -o $@
	@\rm -f Fscheduler.o
	@$(TIMESTAMPER) "End" "fscheduler.x" ""

#-- Run a list of Unix-commands under different MPI-tasks
$(EXEDIR)/mpi_scheduler.x:  $(TIMESTAMP_EXE) $(LIBODBMAIN) $(LIBCSTD) $(LIBODBDUMMY)
	@$(TIMESTAMPER) "Start" "mpi_scheduler.x" ""
	$(AR_EXTRACT) $(LIBODBMAIN) Mpi_scheduler.o
	$(LDFC) -L$(ROOTDIR) $(LDFCFLAGS) Mpi_scheduler.o \
	  $(ODB_LIB) \
	  $(IFSAUX_LIB) $(LDFCLIBS) $(IFSAUX_LIB) $(MPILIB) \
	  $(ODBDUMMY_LIB) $(SYSLIB) -o $@
	@\rm -f Mpi_scheduler.o
	@$(TIMESTAMPER) "End" "mpi_scheduler.x" ""

#-- ERA40: Duplicate checking
$(EXEDIR)/Fetchdupl.x:  $(TIMESTAMP_EXE) $(LIBODBMAIN) $(LIBSTD) $(LIBODBDUMMY) $(LIBPREODB) $(LIBIFS)
	@$(TIMESTAMPER) "Start" "Fetchdupl.x" ""
	$(CREATE_ODBGLUE) PREODB
	$(AR_EXTRACT) $(LIBODBMAIN) Fetchdupl.o
	$(LDFC) -L$(ROOTDIR) $(LDFCFLAGS) Fetchdupl.o $(ODBGLUE_OBJ) \
	  $(ODBPORT_LIB) $(IFS_LIB) $(ODB_LIB) $(PREODB_LIB) \
	  $(IFSAUX_LIB) $(ODB_LIB) $(LDFCLIBS) $(IFSAUX_LIB) $(MPILIB) \
	  $(ODBDUMMY_LIB) $(SYSLIB) -o $@
	@\rm -f Fetchdupl.o $(ODBGLUE_ALL)
	@$(TIMESTAMPER) "End" "Fetchdupl.x" ""

#-- ERA40: BUFR-messages to be excluded
$(EXEDIR)/Excludebufr.x:  $(TIMESTAMP_EXE) $(LIBODBMAIN) $(LIBODBPORT) $(LIBSTD) $(LIBODBDUMMY) $(LIBIFS)
	@$(TIMESTAMPER) "Start" "Excludebuft.x" ""
	$(AR_EXTRACT) $(LIBODBMAIN) Excludebufr.o
	$(LDFC_SERIAL) -L$(ROOTDIR) $(LDFCFLAGS) Excludebufr.o \
	  $(ODBPORT_LIB) $(IFS_LIB) $(ODB_LIB) \
	  $(IFSAUX_LIB) $(LDFCLIBS) $(IFSAUX_LIB) $(MPILIB_SERIAL) \
	  $(ODBDUMMY_LIB) $(SYSLIB) -o $@
	@\rm -f Excludebufr.o
	@$(TIMESTAMPER) "End" "Excludebuft.x" ""

#-- ERA40: Data sources from BUFR-messages
$(EXEDIR)/Sourcelist.x:  $(TIMESTAMP_EXE) $(LIBODBMAIN) $(LIBODBPORT) $(LIBSTD) $(LIBODBDUMMY) $(LIBIFS)
	@$(TIMESTAMPER) "Start" "Sourcelist.x" ""
	$(AR_EXTRACT) $(LIBODBMAIN) Sourcelist.o
	$(LDFC_SERIAL) -L$(ROOTDIR) $(LDFCFLAGS) Sourcelist.o \
	  $(ODBPORT_LIB) $(IFS_LIB) $(ODB_LIB) \
	  $(IFSAUX_LIB) $(LDFCLIBS) $(IFSAUX_LIB) $(MPILIB_SERIAL) \
	  $(ODBDUMMY_LIB) $(SYSLIB) -o $@
	@\rm -f Sourcelist.o
	@$(TIMESTAMPER) "End" "Sourcelist.x" ""

#-- odbviewer executable 
#   Note: When ODB_STATIC_LINKING=1, re-created as user_viewer.x by the odbviewer-script
$(EXEDIR)/viewer.x:  $(TIMESTAMP_EXE) $(LIBODBMAIN) $(LIBSTD) $(LIBODBDUMMY)
	@$(TIMESTAMPER) "Start" "viewer.x" ""
	$(AR_EXTRACT) $(LIBODBMAIN) Viewer.o
	$(LDFC) -L$(ROOTDIR) $(LDFCFLAGS) Viewer.o \
	  $(ODB_LIB) \
	  $(IFSAUX_LIB) $(LDFCLIBS) $(IFSAUX_LIB) $(MPILIB) \
	  $(ODBDUMMY_LIB) $(SYSLIB) -o $@
	@\rm -f Viewer.o
	@$(TIMESTAMPER) "End" "viewer.x" ""

#-- Converts hexadecimal report output of odbviewer into a BUFR-file
$(EXEDIR)/hex2bufr.x:  $(TIMESTAMP_EXE) $(LIBODBMAIN) $(LIBSTD) $(LIBODBDUMMY) $(LIBEMOS)
	@$(TIMESTAMPER) "Start" "hex2bufr.x" ""
	$(AR_EXTRACT) $(LIBODBMAIN) Hex2bufr.o
	$(LDFC_SERIAL) -L$(ROOTDIR) $(LDFCFLAGS) Hex2bufr.o $(STD_LIB) $(EMOS_LIB) \
	  $(MPILIB_SERIAL) $(ODBDUMMY_LIB) $(SYSLIB) -o $@
	@\rm -f Hex2bufr.o
	@$(TIMESTAMPER) "End" "hex2bufr.x" ""

#-- Magics plotting
$(EXEDIR)/plotobs.x:  $(TIMESTAMP_EXE) $(LIBODBMAIN) $(LIBSTD) $(LIBODBDUMMY)
	@$(TIMESTAMPER) "Start" "plotobs.x" ""
	$(AR_EXTRACT) $(LIBODBMAIN) Plotobs.o
	-$(LDMAG) -L$(ROOTDIR) $(LDFCFLAGS) Plotobs.o $(ODB_LIB) $(CLIB) $(MAGLIB) \
	  $(IFSAUX_LIB) $(MPILIB_SERIAL) $(SYSLIB) -o $@
	@\rm -f Plotobs.o
	@$(TIMESTAMPER) "End" "plotobs.x" ""

#-- Date & time filter
$(EXEDIR)/dtfilt.x:  $(TIMESTAMP_EXE) $(LIBODBMAIN) $(LIBCSTD) $(LIBODBDUMMY)
	@$(TIMESTAMPER) "Start" "dtfilt..x" ""
	$(AR_EXTRACT) $(LIBODBMAIN) dtfilt.o $(XLDUM)
	$(LDCC) -L$(ROOTDIR) $(LDCCFLAGS) dtfilt.o $(XLDUM) $(CSTD_LIB) $(LDCCLIBS) $(ODBDUMMY_LIB) $(SYSLIB) -o $@
	@\rm -f dtfilt.o $(XLDUM)
	@$(TIMESTAMPER) "End" "dtfilt..x" ""

#-- Report viewer in X-windows
$(EXEDIR)/b4.x:  $(TIMESTAMP_EXE) $(LIBODBMAIN) $(LIBCSTD) $(LIBODBDUMMY)
	@$(TIMESTAMPER) "Start" "b4.x" ""
	$(AR_EXTRACT) $(LIBODBMAIN) b4.o $(XLDUM)
	$(LDCC) -L$(ROOTDIR) $(LDCCFLAGS) b4.o $(XLDUM) $(CSTD_LIB) \
	  $(LDCCLIBS) $(ODBDUMMY_LIB) $(XWLIBS) $(SYSLIB) -o $@
	@\rm -f b4.o $(XLDUM)
	@$(TIMESTAMPER) "End" "b4.x" ""

#-- Prints information about machine dependent sizeof() in C (see also kind.x)
$(EXEDIR)/sizeof.x:  $(TIMESTAMP_EXE) $(LIBODBMAIN)
	@$(TIMESTAMPER) "Start" "sizeof.x" ""
	$(AR_EXTRACT) $(LIBODBMAIN) sizeof.o
	$(LDCC) -L$(ROOTDIR) $(LDCCFLAGS) sizeof.o $(LDCCLIBS) $(SYSLIB) -o $@
	@\rm -f sizeof.o
	@$(TIMESTAMPER) "End" "sizeof.x" ""

#-- Prints information about machine dependent bit_size()/epsilon()/huge() in F90 (see also sizeof.x)
$(EXEDIR)/kind.x:  $(TIMESTAMP_EXE) $(LIBODBMAIN)
	@$(TIMESTAMPER) "Start" "kind.x" ""
	$(AR_EXTRACT) $(LIBODBMAIN) Kind.o
	$(LDFC_SERIAL) -L$(ROOTDIR) $(LDFCFLAGS) Kind.o $(SYSLIB) -o $@
	@\rm -f Kind.o
	@$(TIMESTAMPER) "End" "kind.x" ""

#-- Obstat executables (do NOT abort if linking fails i.e. these are NOT show-stoppers)
$(EXEDIR)/obstat_calc.x $(EXEDIR)/obstat_merge.x $(EXEDIR)/obstat_plot.x: \
 $(TIMESTAMP_EXE) $(LIBODBMAIN) $(LIBOBSTAT) $(LIBSTD) $(LIBODBDUMMY) $(LIBECMA) $(LIBCCMA)
	@$(TIMESTAMPER) "Start" "obstat_calc.x" ""
	$(CREATE_ODBGLUE) CCMA ECMA
	$(AR_EXTRACT) $(LIBODBMAIN) obstat.o
	-$(LDFC) -L$(ROOTDIR) $(LDFCFLAGS) obstat.o $(ODBGLUE_OBJ) \
	  $(OBSTAT_LIB) $(ODB_LIB) $(OBSTAT_LIB) \
	  $(CCMA_LIB) $(ECMA_LIB) \
	  $(IFSAUX_LIB) $(ODB_LIB) $(LDFCLIBS) $(IFSAUX_LIB) $(MPILIB) \
	  $(ODBDUMMY_LIB) $(SYSLIB) -o $(EXEDIR)/obstat_calc.x
	-$(LDFC_SERIAL) -L$(ROOTDIR) $(LDFCFLAGS) obstat.o \
	  $(OBSTAT_LIB) $(ODB_LIB) $(OBSTAT_LIB) \
	  $(IFSAUX_LIB) $(ODB_LIB) $(LDFCLIBS) $(IFSAUX_LIB) $(MPILIB_SERIAL) \
	  $(ODBDUMMY_LIB) $(SYSLIB) -o $(EXEDIR)/obstat_merge.x
	-$(LDMAG) -L$(ROOTDIR) $(LDFCFLAGS) obstat.o \
	  $(OBSTAT_LIB) $(ODB_LIB) $(OBSTAT_LIB) \
	  $(IFSAUX_LIB) $(ODB_LIB) $(LDFCLIBS) $(IFSAUX_LIB) $(MPILIB_SERIAL) \
	  $(MAGLIB) \
	  $(ODBDUMMY_LIB) $(SYSLIB) -o $(EXEDIR)/obstat_plot.x
	@\rm -f obstat.o $(ODBGLUE_ALL)
	@$(TIMESTAMPER) "End" "obstat_calc.x" ""

#-- SSA test executable to check that linkages are ok (do NOT abort if linking fails i.e. these are NOT show-stoppers)
$(EXEDIR)/ssa.x: $(TIMESTAMP_EXE) $(LIBODBMAIN) $(LIBSSA) $(LIBSTD) $(LIBODBDUMMY) $(LIBECMA) $(LIBIFS) $(LIBODBPORT)
	@$(TIMESTAMPER) "Start" "ssa.x" ""
	$(CREATE_ODBGLUE) ECMA
	$(AR_EXTRACT) $(LIBODBMAIN) ssa.o
	-$(LDFC_SERIAL) -L$(ROOTDIR) $(LDFCFLAGS) ssa.o \
	  $(SSA_LIB) $(ODBPORT_LIB) $(ODB_LIB) $(IFS_LIB) \
	  $(IFSAUX_LIB) $(ODB_LIB) $(LDFCLIBS) $(IFSAUX_LIB) $(MPILIB_SERIAL) \
	  $(ODBDUMMY_LIB) $(SYSLIB) -o $@
	@\rm -f ssa.o $(ODBGLUE_ALL)
	@$(TIMESTAMPER) "End" "ssa.x" ""

#-- Create ODB client library from a subset of objects in libodb.a
$(LIBODBCLIENT):  $(TIMESTAMP_EXE) $(LIBODB)
	@$(TIMESTAMPER) "Start" "LIBODBCLIENT" ""
	@\rm -f $(CLIOBJS) $(LIBODBCLIENT)
	$(AR_EXTRACT) $(LIBODB) $(CLIOBJS)
	$(MAKE_SHLIB) -i "$(SHLIB_LIST)" -c "$(SHLIB_CMD)" -s $(SHLIB_SFX) -u "$(AR_CREATE)" -l $(LIBODBCLIENT) $(CLIOBJS)
	@\rm -f $(CLIOBJS)
	@$(TIMESTAMPER) "End" "LIBODBCLIENT" ""

#-- Client/Server/Direct -test executables to check for unsatisfied externals
$(EXEDIR)/odbi_direct.x $(EXEDIR)/odbi_server.x $(EXEDIR)/odbi_client.x: \
 $(TIMESTAMP_EXE) $(LIBODBMAIN) $(LIBCSTD) $(LIBSTD) $(LIBODBDUMMY)
	@$(TIMESTAMPER) "Start" "client/server stuff" ""
	$(AR_EXTRACT) $(LIBODBMAIN) odbi_direct_main.o odbi_server_main.o odbi_client_main.o
	$(LDCC) -L$(ROOTDIR) odbi_direct_main.o \
		$(LDCCFLAGS) $(STD_LIB) $(LDFCLIBS) $(MPILIB_SERIAL) $(ODBDUMMY_LIB) \
		$(ODB_FCLIBS) $(SYSLIB) -o $(EXEDIR)/odbi_direct.x || \
		echo "***Warning: Could not create $(EXEDIR)/odbi_direct.x"
	$(LDCC) -L$(ROOTDIR) odbi_server_main.o \
		$(LDCCFLAGS) $(STD_LIB) $(LDFCLIBS) $(MPILIB_SERIAL) $(ODBDUMMY_LIB) \
		$(ODB_FCLIBS)  $(SYSLIB) -o $(EXEDIR)/odbi_server.x || \
		echo "***Warning: Could not create $(EXEDIR)/odbi_server.x"
	$(LDCC) -L$(ROOTDIR) odbi_client_main.o \
		$(LDCCFLAGS) $(CSTD_LIB) $(LDFCLIBS) $(MPILIB_SERIAL) $(ODBDUMMY_LIB) \
		$(ODB_FCLIBS) $(SYSLIB) -o $(EXEDIR)/odbi_client.x || \
		echo "***Warning: Could not create $(EXEDIR)/odbi_client.x"
	@\rm -f odbi_direct_main.o odbi_server_main.o odbi_client_main.o
	@$(TIMESTAMPER) "End" "client/server stuff" ""

$(TIMESTAMP_EXE): $(TIMESTAMP_DIR)/timestamp.c
	@rm -f $@
	$(FE_CC) $(FE_CCFLAGS) $(TIMESTAMP_DIR)/timestamp.c -o $@ || \
		(rm -f $@ ; ln -s $(TIMESTAMP_DIR)/notimestamp.x $@)
	@chmod u+rx $@

tidy: $(TIMESTAMP_EXE) 
	@$(TIMESTAMPER) "Start" "tidy" ""
	find . -name '*.o'          -print | xargs \rm -f
	find . -name '*.ok'         -print | xargs \rm -f
	find . -name '*.'$(MOD_EXT) -print | xargs \rm -f
	find . -name '*~'           -print | xargs \rm -f
	find . -name '*.ddl_'       -print | xargs \rm -f
	@$(TIMESTAMPER) "End" "tidy" ""

clean:	$(TIMESTAMP_EXE) tidy
	@$(TIMESTAMPER) "Start" "clean" ""
	\rm -f core
	\rm -f a.out $(UTILITY_EXES) $(APPLICATION_EXES)
	\rm -f *.a
	\rm -f *.$(SHLIB_SFX)
	\rm -f $(LIB) $(SHLIB)
	-\cd odb/compiler; \rm -f yacc.c lex.c y.tab.h
	-\rm -rf odb/backend_compiler
	\rm -f $(ODBSQL) $(ODBSQLBE)
	\rm -f odb/ddl.*/*.c
	\rm -f odb/ddl.*/[A-Z]*.h
	\rm -f odb/ddl.*/*.old
	\rm -rf _lib*.dir
	@$(TIMESTAMPER) "End" "clean" ""

#-- Extra rules etc.
include $(MKEXTRA)

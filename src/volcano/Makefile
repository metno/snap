# includefile contains Compiler definitions etc.
include ./make.ubuntu

INCLUDES = -I. $(PUINC)  $(FTGLINC) $(FTINC) $(QQQ) $(GLINC) $(GLTTINC)

LIBS= $(PULIB) $(FTGLLIB) $(FTLIB) $(QTLIB) $(GLLIB) $(MILIB) $(EXLIB)
BLIBS= $(MILIB)

.SUFFIXES: .cc .c .f .F
.f.o:
	$(F77) -c $(F77FLAGS) -I. $<
.F.o:
	$(F77) -c $(F77FLAGS) -I. $<
.c.o:
	$(CC)  -c $(CCFLAGS) -I. $<
.cc.o:
	$(CXX) -c $(CXXFLAGS) $(DEFS) $(INCLUDES) $<

MODELOBJ= argoswrite.o bldp.o compheight.o copyfield.o decay.o drydep1.o drydep2.o edcomp.o \
ensemble.o epinterp.o filesort.o fldout.o forwrd.o ftest.o om2edot.o posint.o \
pselect.o readfd.o readfield.o release.o rmpart.o rwalk.o tabcon.o \
vgravtables.o videosave.o wetdep1.o wetdep2.o

CCOBJ = glSnapPlot.o glMapPlot.o glFieldPlot.o \
diFontManager.o diColour.o diLinetype.o diContouring.o diTesselation.o \
diArea.o diProjection.o diRectangle.o

OBJ = snap.o snapInterface.o

BOBJ = snap_batch_copy.o

INCFILES = snapdim.inc snapfil.inc snapfld.inc snapgrd.inc snappar.inc \
snappos.inc snaptab.inc snapgrp.inc snapeps.inc snapargos.inc snapdebug.inc

HFILES = diColour.h diLinetype.h diContouring.h diTesselation.h diFontManager.h diPlotOptions.h \
glFieldPlot.h glMapPlot.h snapInterface.h \
diArea.h diProjection.h diRectangle.h

all: $(BINDIR)/snap $(BINDIR)/bsnap

$(BINDIR)/snap: $(CCOBJ) $(OBJ) $(MODELOBJ)
	$(CXX) $(CXXFLAGS) $(CCOBJ) $(OBJ) $(MODELOBJ) $(LIBS) -o $(BINDIR)/snap

$(BINDIR)/bsnap: $(BOBJ) $(MODELOBJ)
	$(F77) $(F77FLAGS) $(BOBJ) $(MODELOBJ) $(BLIBS) -o $(BINDIR)/bsnap

#--------------------------------

snap_batch_copy.o: snap.F $(INCFILES)
	cp -p snap.F snap_batch_copy.F
	$(F77) -c $(F77FLAGS) -I. -DBATCH snap_batch_copy.F

snap.o:        $(INCFILES)
argoswrite.o:  $(INCFILES)
bldp.o:        $(INCFILES)
compheight.o:  $(INCFILES)
copyfield.o:   $(INCFILES)
decay.o:       $(INCFILES)
drydep1.o:     $(INCFILES)
drydep2.o:     $(INCFILES)
edcomp.o:      $(INCFILES)
ensemble.o:    $(INCFILES)
epinterp.o:    $(INCFILES)
filesort.o:    $(INCFILES)
fldout.o:      $(INCFILES)
forwrd.o:      $(INCFILES)
ftest.o:       $(INCFILES)
om2edot.o:     $(INCFILES)
posint.o:      $(INCFILES)
pselect.o:     $(INCFILES)
readfd.o:      $(INCFILES)
readfield.o:   $(INCFILES)
release.o:     $(INCFILES)
rmpart.o:      $(INCFILES)
rwalk.o:       $(INCFILES)
tabcon.o:      $(INCFILES)
vgravtables.o: $(INCFILES)
videosave.o:   $(INCFILES)
wetdep1.o:     $(INCFILES)
wetdep2.o:     $(INCFILES)

snapInterface.o: $(INCFILES)

diColour.o: $(HFILES)
diLinetype.o: $(HFILES)
diContouring.o: $(HFILES)
diFontManager.o: $(HFILES)
glFieldPlot.o: $(HFILES)
glMapPlot.o: $(HFILES)
glSnapPlot.o: $(HFILES)

#---------------------------------------------------------

clean:
	rm -f *.o
	rm -f *~
	rm -f snap_batch_copy.*

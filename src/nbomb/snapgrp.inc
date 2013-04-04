c-----------------------------------------------------------------------
c snapgrp.inc
c
c..include file  -  specifications for graphics
c
c
      integer   igraphics,istephgr,nstephgr,istepgr,nstepgr
      integer   iunitgr,nlandfile
      integer   itimegr(5),igrspec(11)
      integer   savePNGstep,saveXPMstep,saveBMPstep
      real      tf1gr,tf2gr,tnowgr
      real      rgrspec(8)
      character*72 landfile(mlandfile)
      character*32 cgrspec
c
      common/graphics/igraphics,istephgr,nstephgr,istepgr,nstepgr
     -               ,iunitgr,nlandfile
     -               ,itimegr,igrspec
     -               ,savePNGstep,saveXPMstep,saveBMPstep
     -               ,tf1gr,tf2gr,tnowgr,rgrspec
     -               ,landfile,cgrspec
c-----------------------------------------------------------------------

! SNAP: Servere Nuclear Accident Programme
! Copyright (C) 1992-2017   Norwegian Meteorological Institute
! 
! This file is part of SNAP. SNAP is free software: you can 
! redistribute it and/or modify it under the terms of the 
! GNU General Public License as published by the 
! Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
! 
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
! 
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https://www.gnu.org/licenses/>.
!
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

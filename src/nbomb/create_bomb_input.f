c create_snap_input.f
c
c Program create_snap_input.f reads the input file for SNAP run with nuclear bomb detonation specified by NRPA.
c The input file from NRPA is: bomb_test_SNAP-BOMB_input
c The output from the program is: snap.input - standard input file for the snap model
c
c------------------------------------------------------------------------------------------------------------------
c 15.06.2011 - first version - Jerzy Bartnicki
c 22.09.2011 - mdification - Jerzy Bartnicki
c 04.11.2011 - version for BOMB - Jerzy Bartnicki
c 24.05.2012 - all explosive yield (8 classes)
c------------------------------------------------------------------------------------------------------------------
c
       implicit none
       character*80 lname
       real lat, long              ! position decimal
       integer idate_start(5)       ! date and time of the start of simulation (UTC)
       integer idate_end(5)       ! date and time of the end of simulation (UTC)
       integer iforecast       ! forecast in hours
       integer tout              ! frequency of output in hrs
       integer yield              ! yield of source in kt
       real fispro              ! fission proportion (not used at present)
       integer i,j,j1,k       ! indexes
       integer npart              ! Maximum number of particles released
        character*64 fmt       ! Alvaro's variable necessary for a speciall format
       real stem              ! stem radius in m
       real radius              ! mashroom (or cylinder) radius in m
       real lower              ! base of the mashroom (or cylinder) in m
       real upper              ! top of the mashroom (or cylinder) in m
       real activity              ! total radioactivity released in Bq
       integer activities  ! number of activity-classes to divide the activity into
c
c------------------------------------------------------------------------------------------------------------------
c
c... coordinates and start time
c
       write(*,*)
       write(*,*) 'File bomb-input.txt:'

       open(1,file='bomb-input.txt')
       rewind 1
       read(1,*) lat
       write(*,*) lat,' LATTITUDE'
       read(1,*) long
       write(*,*) long,' LONGITUDE'
       read(1,'(i4,4i2)') (idate_start(i),i=1,4)
       write(*,'(i4,3i2,''     Start Time (UTC)'')') (idate_start(i),
     &     i=1,4)
       read(1,*) iforecast
       write(*,'(i4,12x,'' Forecast time (hrs)'')') iforecast
       read(1,*) yield
       if(yield .eq. 1 .or. yield .eq. 3 .or. yield .eq. 10 .or.
     &  yield .eq. 30 . or. yield .eq. 100 .or. yield .eq. 300 .or.
     &  yield .eq. 1000 . or. yield .eq. 3000) then
          write(*,*) yield,'    Explosive Yield (kt)'
       else
         write(*,*) '*** ERROR ***'
         write(*,*) ' Explosive yield =',yield
         write(*,*) ' ONLY these values are allowed:'
         write(*,*) ' 1,3,10,30,100,300,1000,3000'
         write(*,*) ' STOP with ERROR in explosive yield'
          stop
       endif
       read(1,*) npart
       if(npart .le. 800000) then
          write(*,*) npart,'  Number of model particles'
          write(*,*) ' Bomb input OK!'
          write(*,*)
       else
         write(*,*) '*** ERROR ***'
         write(*,*) ' Number of model particles =',npart
         write(*,*) ' Maximum is: 800000:'
         write(*,*) ' STOP with ERROR in the number of model particles'
          stop
       endif
c
c... Cylinder parameters
c
       if(yield .eq. 1) then
          lower=500.0
          upper=1500.0
          radius=600.0
          activity=2.0e19
          go to 888
       endif
       if(yield .eq. 3) then
          lower=1400.0
          upper=3100.0
          radius=1000.0
          activity=6.0e19
          go to 888
       endif
       if(yield .eq. 10) then
          lower=2250.0
          upper=4750.0
          radius=1400.0
          activity=2.0e20
          go to 888
       endif
       if(yield .eq. 30) then
          lower=4100.0
          upper=8400.0
          radius=2300.0
          activity=6.0e20
          go to 888
       endif
       if(yield .eq. 100) then
          lower=5950.0
          upper=12050.0
          radius=3200.0
          activity=2.0e21
          go to 888
       endif
       if(yield .eq. 300) then
          lower=8000.0
          upper=18500.0
          radius=5800.0
          activity=6.0e21
       endif
       if(yield .eq. 1000) then
          lower=10000.0
          upper=25000.0
          radius=8500.0
          activity=2.0e22
          go to 888
       endif
       if(yield .eq. 3000) then
          lower=12000.0
          upper=32000.0
          radius=11100.0
          activity=6.0e22
          go to 888
       endif
       write(*,*) '*** ERROR ***'
       write(*,*) ' Explosive yield =',yield
       write(*,*) ' ONLY these values are allowed:'
       write(*,*) ' 1,3,10,30,100,300,1000,3000'
       write(*,*) ' STOP with ERROR in explosive yield'
       stop       
888       continue
c
c... maximum number of particles released
c
c       npart=200000
       write(*,*)
       write(*,'(''Number of particles released ='',i10)') npart
       write(*,*)
c
c... file: snap.input
c
       open(2,file='snap.input')
       rewind 2
c
c       write(2,*) 'TIME.START=   2010 10 14 06'
       write(2,"('** EXPLOSIVE YIELD = ',i5,' kt')") yield
       write(2,"('TIME.START=  ',i5,3i3)")
     &(idate_start(i),i=1,4)
       write(2,"('SET_RELEASE.POS= P=',f10.4,',',f10.4)")
     &lat,long
       write(2,"('TIME.RUN  = ',i3,'h')") iforecast
       write(2,"(1(A))") 'GRAPHICS.OFF',
     & 'VIDEO.SAVE.OFF','RANDOM.WALK.ON',
     & 'BOUNDARY.LAYER.FULL.MIX.OFF','DRY.DEPOSITION.NEW',
     & 'WET.DEPOSITION.NEW',
     & 'TIME.RELEASE.PROFILE.BOMB',
     & 'RELEASE.SECOND=    0'
       write(2,"('RELEASE.RADIUS.M=',f10.4)")
     &radius
       write(2,"('RELEASE.LOWER.M=',f10.4)")
     &lower
       write(2,"('RELEASE.UPPER.M=',f10.4)")
     &upper

c       fmt="(A,??(e10.2,','),e10.2,1x,'''test''')"
c       nrel=1
       write(2,"('*')")
c        fmt="(A,T21,10(i10,:,','))"
c       write(2,fmt) 'RELEASE.HOUR=',(ihour(i),i=1,nrel+1)
c       write(2,fmt) 'RELEASE.RADIUS.M=',(iradius(i),i=1,nrel+1)
c       write(2,fmt) 'RELEASE.LOWER.M=',(lowrel(i),i=1,nrel+1)
c       write(2,fmt) 'RELEASE.UPPER.M=',(toprel(i),i=1,nrel+1)
c        fmt="(A,??(e10.2,','),e10.2,1x,'''C',I2.2,'''')"
c        write(fmt(4:5),"(I2.2)"),nrel!+1
c       do i=1,niso
c          write(2,fmt) "RELEASE.BQ/SEC.COMP=",(emi(i,j),j=1,nrel+1),i
c       enddo

cTIME.RELEASE.PROFILE.BOMB
cRELEASE.SECOND=    0
cRELEASE.MUSHROOM.STEM.RADIUS.M=  2648.
cRELEASE.RADIUS.M=    17651.
cRELEASE.LOWER.M= 13347.
cRELEASE.UPPER.M= 21635
cRELEASE.BQ/STEP.COMP= 1.0e+16 'Aerosol_2.2mym'
cRELEASE.BQ/STEP.COMP= 1.0e+16 'Aerosol_4.4mym'
cRELEASE.BQ/STEP.COMP= 1.0e+16 'Aerosol_8.6mym'
cRELEASE.BQ/STEP.COMP= 1.0e+16 'Aerosol_14.6mym'
cRELEASE.BQ/STEP.COMP= 1.0e+16 'Aerosol_22.8mym'
cRELEASE.BQ/STEP.COMP= 1.0e+16 'Aerosol_36.1mym'
cRELEASE.BQ/STEP.COMP= 1.0e+16 'Aerosol_56.5mym'
cRELEASE.BQ/STEP.COMP= 1.0e+16 'Aerosol_92.3mym'
cRELEASE.BQ/STEP.COMP= 1.0e+16 'Aerosol_173.2mym'
cRELEASE.BQ/STEP.COMP= 1.0e+16 'Aerosol_250mym'
c        fmt="(A,e10.1,1x,'''Aerosol_2.2mym''')"
c       write(2,fmt) "RELEASE.BQ/STEP.COMP=",activity
       activities = 10 ! 10 aerosol classes
       write(2,"(A,e10.1,1x,'''Aerosol_2.2mym''')") 
     &"RELEASE.BQ/STEP.COMP=",activity/activities
       write(2,"(A,e10.1,1x,'''Aerosol_4.4mym''')") 
     &"RELEASE.BQ/STEP.COMP=",activity/activities
       write(2,"(A,e10.1,1x,'''Aerosol_8.6mym''')") 
     &"RELEASE.BQ/STEP.COMP=",activity/activities
       write(2,"(A,e10.1,1x,'''Aerosol_14.6mym''')") 
     &"RELEASE.BQ/STEP.COMP=",activity/activities
       write(2,"(A,e10.1,1x,'''Aerosol_22.8mym''')") 
     &"RELEASE.BQ/STEP.COMP=",activity/activities
       write(2,"(A,e10.1,1x,'''Aerosol_36.1mym''')") 
     &"RELEASE.BQ/STEP.COMP=",activity/activities
       write(2,"(A,e10.1,1x,'''Aerosol_56.5mym''')") 
     &"RELEASE.BQ/STEP.COMP=",activity/activities
       write(2,"(A,e10.1,1x,'''Aerosol_92.3mym''')") 
     &"RELEASE.BQ/STEP.COMP=",activity/activities
       write(2,"(A,e10.1,1x,'''Aerosol_173.2mym''')") 
     &"RELEASE.BQ/STEP.COMP=",activity/activities
       write(2,"(A,e10.1,1x,'''Aerosol_250.0mym''')") 
     &"RELEASE.BQ/STEP.COMP=",activity/activities
       write(2,"('* PARTICLE CLASSES')")
       write(2,"('COMPONENT= Aerosol_2.2mym')")
       write(2,"('DRY.DEP.ON')")
       write(2,"('WET.DEP.ON')")
       write(2,"('RADIOACTIVE.DECAY.OFF')")
       write(2,"('RADIUS.MICROMETER= 2.2')")
       write(2,"('*DENSITY.G/CM3=2.95')")
       write(2,"('GRAVITY.FIXED.CM/S= 0.2')")
       write(2,"('FIELD.IDENTIFICATION=01')")
       write(2,"('*')")
       write(2,"('COMPONENT= Aerosol_4.4mym')")
       write(2,"('DRY.DEP.ON')")
       write(2,"('WET.DEP.ON')")
       write(2,"('RADIOACTIVE.DECAY.OFF')")
       write(2,"('RADIUS.MICROMETER= 4.4')")
       write(2,"('GRAVITY.FIXED.CM/S= 0.7')")
       write(2,"('FIELD.IDENTIFICATION=2')")
       write(2,"('*')")
       write(2,"('COMPONENT= Aerosol_8.6mym')")
       write(2,"('DRY.DEP.ON')")
       write(2,"('WET.DEP.ON')")
       write(2,"('RADIOACTIVE.DECAY.OFF')")
       write(2,"('RADIUS.MICROMETER= 8.6')")
       write(2,"('GRAVITY.FIXED.CM/S= 2.5')")
       write(2,"('FIELD.IDENTIFICATION=3')")
       write(2,"('*')")
       write(2,"('COMPONENT= Aerosol_14.6mym')")
       write(2,"('DRY.DEP.ON')")
       write(2,"('WET.DEP.ON')")
       write(2,"('RADIOACTIVE.DECAY.OFF')")
       write(2,"('RADIUS.MICROMETER= 14.6')")
       write(2,"('GRAVITY.FIXED.CM/S= 6.9')")
       write(2,"('FIELD.IDENTIFICATION=4')")
       write(2,"('*')")
       write(2,"('COMPONENT= Aerosol_22.8mym')")
       write(2,"('DRY.DEP.ON')")
       write(2,"('WET.DEP.ON')")
       write(2,"('RADIOACTIVE.DECAY.OFF')")
       write(2,"('RADIUS.MICROMETER= 22.8')")
       write(2,"('GRAVITY.FIXED.CM/S= 15.9')")
       write(2,"('FIELD.IDENTIFICATION=5')")
       write(2,"('*')")
       write(2,"('COMPONENT= Aerosol_36.1mym')")
       write(2,"('DRY.DEP.ON')")
       write(2,"('WET.DEP.ON')")
       write(2,"('RADIOACTIVE.DECAY.OFF')")
       write(2,"('RADIUS.MICROMETER= 36.1')")
       write(2,"('GRAVITY.FIXED.CM/S= 35.6')")
       write(2,"('FIELD.IDENTIFICATION=6')")
       write(2,"('*')")
       write(2,"('COMPONENT= Aerosol_56.5mym')")
       write(2,"('DRY.DEP.ON')")
       write(2,"('WET.DEP.ON')")
       write(2,"('RADIOACTIVE.DECAY.OFF')")
       write(2,"('RADIUS.MICROMETER= 56.5')")
       write(2,"('GRAVITY.FIXED.CM/S= 71.2')")
       write(2,"('FIELD.IDENTIFICATION=7')")
       write(2,"('*')")
       write(2,"('COMPONENT= Aerosol_92.3mym')")
       write(2,"('DRY.DEP.ON')")
       write(2,"('WET.DEP.ON')")
       write(2,"('RADIOACTIVE.DECAY.OFF')")
       write(2,"('RADIUS.MICROMETER= 92.3')")
       write(2,"('GRAVITY.FIXED.CM/S= 137.0')")
       write(2,"('FIELD.IDENTIFICATION=8')")
       write(2,"('*')")
       write(2,"('COMPONENT= Aerosol_173.2mym')")
       write(2,"('DRY.DEP.ON')")
       write(2,"('WET.DEP.ON')")
       write(2,"('RADIOACTIVE.DECAY.OFF')")
       write(2,"('RADIUS.MICROMETER=173.2')")
       write(2,"('GRAVITY.FIXED.CM/S= 277.3')")
       write(2,"('FIELD.IDENTIFICATION=9')")
       write(2,"('*')")
       write(2,"('COMPONENT= Aerosol_250.0mym')")
       write(2,"('DRY.DEP.ON')")
       write(2,"('WET.DEP.ON')")
       write(2,"('RADIOACTIVE.DECAY.OFF')")
       write(2,"('RADIUS.MICROMETER= 250.0')")
       write(2,"('GRAVITY.FIXED.CM/S= 10000.0')")
       write(2,"('FIELD.IDENTIFICATION=10')")
       write(2,"('*')")
c
       write(2,"('MAX.PARTICLES.PER.RELEASE=',i10)") npart
       write(2,*) '*'
       write(2,"('*')")
c
       write(2,"('PRECIP(MM/H).PROBAB= 0.0,0.00, 0.5,0.31, 1.0,0.48,'
     &' 1.5,0.60, 2.0,0.66, ',
     &'3.3,0.72, 8.3,0.80, 15.,0.85, 25.,0.91')")
c
          write(2,"('*')")
          write(2,"('REMOVE.RELATIVE.MASS.LIMIT= 0.01')")
          write(2,"('*')")
          write(2,"('TIME.STEP= 300.')")
          write(2,"('STEP.HOUR.INPUT.MIN=  3')")
          write(2,"('STEP.HOUR.INPUT.MAX= 12')")
          write(2,"('STEP.HOUR.OUTPUT.FIELDS= 240',i2)") iforecast
          write(2,"('ASYNOPTIC.OUTPUT')")
c
          write(2,"('*')")
          write(2,"('TOTAL.COMPONENTS.OFF')")
          write(2,"('MSLP.ON')")
          write(2,"('PRECIPITATION.ON')")
          write(2,"('MODEL.LEVEL.FIELDS.OFF')")
c
          write(2,"('*')")
          write(2,"('POSITIONS.DECIMAL')")
c
          write(2,"('*')")
          write(2,"('** Hirlam.12km')")
          write(2,"('GRID.INPUT= 88,12')")
          write(2,"('GRID.RUN=   88,12, 1,1,1')")
          write(2,"('DATA.ETA.LEVELS')")
          write(2,"('LEVELS.INPUT= 61,',
     &'0,60,59,58,57,56,55,54,53,52,51,50,'
     &'49,48,47,46,45,44,43,42,41,40,',
     &'39,38,37,36,35,34,33,32,31,30,',
     &'29,28,27,26,25,24,23,22,21,20,',
     &'19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1')") 
          write(2,"('*')")
          write(2,"('** INPUT FIELD FILES')")
          write(2,"('FORECAST.HOUR.MIN= +3')")
          write(2,"('FORECAST.HOUR.MAX= +9999')")
c
          write(2,"('*')")
       write(2,"('FIELD.INPUT= /opdata/hirlam12/h12snap00.dat-1')")
          write(2,"('FIELD.INPUT= /opdata/hirlam12/h12sf00.dat-1')")
          write(2,"('FIELD.INPUT= /opdata/hirlam12/h12snap06.dat-1')")
          write(2,"('FIELD.INPUT= /opdata/hirlam12/h12sf06.dat-1')")
          write(2,"('FIELD.INPUT= /opdata/hirlam12/h12snap12.dat-1')")
          write(2,"('FIELD.INPUT= /opdata/hirlam12/h12sf12.dat-1')")
          write(2,"('FIELD.INPUT= /opdata/hirlam12/h12snap18.dat-1')")
          write(2,"('FIELD.INPUT= /opdata/hirlam12/h12sf18.dat-1')")
          write(2,"('FIELD.INPUT= /opdata/hirlam12/h12snap00.dat')")
          write(2,"('FIELD.INPUT= /opdata/hirlam12/h12sf00.dat')")
          write(2,"('FIELD.INPUT= /opdata/hirlam12/h12snap06.dat')")
          write(2,"('FIELD.INPUT= /opdata/hirlam12/h12sf06.dat')")
          write(2,"('FIELD.INPUT= /opdata/hirlam12/h12snap12.dat')")
          write(2,"('FIELD.INPUT= /opdata/hirlam12/h12sf12.dat')")
          write(2,"('FIELD.INPUT= /opdata/hirlam12/h12snap18.dat')")
          write(2,"('FIELD.INPUT= /opdata/hirlam12/h12sf18.dat')")
c
          write(2,"('*')")
          write(2,"('FIELD_TIME.FORECAST')")
c
          write(2,"('*')")
       write(2,"('** OUTPUT FILES')")
       write(2,"('FIELD.OUTPUT= snap.felt')")
       write(2,"('LOG.FILE=     snap.log')")
c
          write(2,"('*')")
       write(2,"('DEBUG.ON')")
c
          write(2,"('*')")
       write(2,"('ARGOS.OUTPUT.ON')")
       write(2,"('ARGOS.OUTPUT.DEPOSITION.FILE=    snap_depo')")
       write(2,"('ARGOS.OUTPUT.CONCENTRATION.FILE= snap_conc')")
       write(2,"('ARGOS.OUTPUT.TOTALDOSE.FILE=     snap_dose')")
       write(2,"('ARGOS.OUTPUT.TIMESTEP.HOUR=',i2)") iforecast
       write(2,"('END')")

       close (2)
c
       write(*,*)
       write(*,*) '************************************************'
       write(*,*)
c
       stop
       end       

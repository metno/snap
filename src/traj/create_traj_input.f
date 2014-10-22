c input.f
c
c Program input.f generates the input file for the SNAP model to calculate forward trajectories specified by NRPA.
c The input file from NRPA is: nrpa.input
c The output from the program are 2 files: snap.input - standard input file for the snap model
c
c------------------------------------------------------------------------------------------------------------------
c 27.02.2011 - first version - Jerzy Bartnicki
c 06.09.2012 - corrections - Jerzy Bartnicki
c 22.03.2013 - read added runident from input file - Heiko Klein
c------------------------------------------------------------------------------------------------------------------
c
        implicit none
        character*80 runident  ! identifier for run
        character*1 empty	! character for empty space
        real lat, long		! position decimal
        integer idate(4)	! date and time of start
        character*20 lname1 ! mode forward/backward
        integer iforecast	! forecast in hours
        integer ntraj		! No. of trajectories
        real level(100)		!  Start level (in m) for each trajectory
        integer i,j,k		!indexes
        character*6 comp	! compnent name 'traj'
        character*64 fmt	! Alvaro's variable
        character*80 tname(10)	! name for the trajectory
c------------------------------------------------------------------------------------------------------------------
c
        comp='traj'
        open(1,file='nrpa.input')
        rewind 1
c
        write(*,*)
        write(*,*) 'info from nrpa.input'
        write(*,*)
c
        read(1,*) runident
        write(*,*) trim(runident)
        fmt="('Trajectory_',i3.3,'_',A??,'.DAT')"
        write(fmt(26:27),"(I2.2)"), len_trim(runident)
        write(*,*)'fmt=', fmt
c
        write(*,*)
        read(1,*) lat
        write(*,*) lat
        read(1,*) long
        write(*,*) long
        read(1,'(i4,3i2.2)') (idate(i),i=1,4)
        write(*,'(i4,3i2.2)') (idate(i),i=1,4)
        read(1,*) lname1
        if (lname1(1:7).eq.'forward'
     &    .or.lname1(1:8).eq.'backward') then
c           new input format format with mode and duration (iforecast)
            read(1,*) iforecast
            read(1,*) ntraj
            if (lname1(1:8).eq.'backward') then
                iforecast = - iforecast
            end if
        else
c           old format
            read (lname1, '(I10)') ntraj
            lname1 = "forward"
            iforecast = 48
        end if
        write(*,*) lname1
        write(*,*) iforecast
        write(*,*) ntraj
        do i=1,ntraj
           read(1,*) level(i)
           write(*,*) i,level(i)
        enddo
c
c... trajectory names
c
        do i=1,10
            write(tname(i),fmt) i,runident
        enddo
        write(*,*)
        do i=1,ntraj
           write(*,'(i4,1x,a80)') i,tname(i)
        enddo
        write(*,*)'----------------'
        close (1)
        write(*,*)
c
        open(2,file='snap.input')
        rewind 2
c	write(2,*) 'TIME.START=   2010 10 14 06'
        write(2,'('' TIME.START=  '',i5,3i3)')
     &(idate(i),i=1,4)
        write(2,'('' SET_RELEASE.POS= P='',f4.1,1h,,f4.1)')
     &lat,long
c	write(2,*) 'TIME.RUN  = 48h'
        write(2,'('' TIME.RUN  = '',i3,''h'')') iforecast
        write(2,*) 'GRAPHICS.OFF'
        write(2,*) 'VIDEO.SAVE.OFF'
        write(2,*) 'RANDOM.WALK.OFF'
        write(2,*) 'BOUNDARY.LAYER.FULL.MIX.OFF'
        write(2,*) 'DRY.DEPOSITION.NEW'
        write(2,*) 'WET.DEPOSITION.NEW'
        write(2,*) 'TIME.RELEASE.PROFILE.BOMB'
        write(2,*) 'RELEASE.SECOND=    0'
        write(2,*) 'RELEASE.RADIUS.M=    0.'
        write(2,*) 'RELEASE.LOWER.M= 500.'
        write(2,*) 'RELEASE.UPPER.M= 500.'
        write(2,'('' RELEASE.BQ/STEP.COMP= 1.0e+20 '',
     &a6)') comp
        write(2,*) 'MAX.PARTICLES.PER.RELEASE=1'
        write(2,*) 'COMPONENT= traj'
        write(2,*) 'DRY.DEP.OFF'
        write(2,*) 'WET.DEP.OFF'
        write(2,*) 'GRAVITY.OFF'
        write(2,*) 'FIELD.IDENTIFICATION=1'
        write(2,*) 'PRECIP(MM/H).PROBAB= 0.0,0.00, 0.5,0.31, 1.0,0.48'
        write(2,*) 'PRECIP(MM/H).PROBAB= 1.5,0.60, 2.0,0.66'
        write(2,*) 'PRECIP(MM/H).PROBAB= 3.3,0.72, 8.3,0.80'
        write(2,*) 'PRECIP(MM/H).PROBAB= 15.,0.85, 25.,0.91'
        write(2,*) 'REMOVE.RELATIVE.MASS.LIMIT= 0.00001'
        write(2,*) 'TIME.STEP= 300.'
        write(2,*) 'STEP.HOUR.INPUT.MIN=  3'
        write(2,*) 'STEP.HOUR.INPUT.MAX= 12'
        write(2,*) 'STEP.HOUR.OUTPUT.FIELDS= 240'
        write(2,*) 'ASYNOPTIC.OUTPUT'
        write(2,*) 'TOTAL.COMPONENTS.OFF'
        write(2,*) '*MSLP.ON'
        write(2,*) '*PRECIPITATION.ON'
        write(2,*) 'MODEL.LEVEL.FIELDS.OFF'
        write(2,*) 'POSITIONS.DECIMAL'
        write(2,*) 'GRID.INPUT= 88,12'
        write(2,*) 'GRID.RUN=   88,12, 1,1,1'
        write(2,*) 'DATA.ETA.LEVELS'
        write(2,'(''LEVELS.INPUT= 61,0,60,59,58,57,56,55,54,53,'',
     &''52,51,50,49,48,47,46,45,44,43,42,41,'',
     &''40,39,38,37,36,35,34,33,'',
     &''32,31,30,29,28,27,26,25,24,23,22,21,20,19,18,17,16,15,'',
     &''14,13,12,11,10,9,8,7,6,5,4,3,2,1'')')
        write(2,*) 'FORECAST.HOUR.MIN= +3'
        write(2,*) 'FORECAST.HOUR.MAX= +9999'
        write(2,'(''FIELD.INPUT= /opdata/hirlam12/h12snap00.dat-1'')')
        write(2,'(''FIELD.INPUT= /opdata/hirlam12/h12sf00.dat-1'')')
        write(2,'(''FIELD.INPUT= /opdata/hirlam12/h12snap06.dat-1'')')
        write(2,'(''FIELD.INPUT= /opdata/hirlam12/h12sf06.dat-1'')')
        write(2,'(''FIELD.INPUT= /opdata/hirlam12/h12snap12.dat-1'')')
        write(2,'(''FIELD.INPUT= /opdata/hirlam12/h12sf12.dat-1'')')
        write(2,'(''FIELD.INPUT= /opdata/hirlam12/h12snap18.dat-1'')')
        write(2,'(''FIELD.INPUT= /opdata/hirlam12/h12sf18.dat-1'')')
        write(2,'(''FIELD.INPUT= /opdata/hirlam12/h12snap00.dat'')')
        write(2,'(''FIELD.INPUT= /opdata/hirlam12/h12sf00.dat'')')
        write(2,'(''FIELD.INPUT= /opdata/hirlam12/h12snap06.dat'')')
        write(2,'(''FIELD.INPUT= /opdata/hirlam12/h12sf06.dat'')')
        write(2,'(''FIELD.INPUT= /opdata/hirlam12/h12snap12.dat'')')
        write(2,'(''FIELD.INPUT= /opdata/hirlam12/h12sf12.dat'')')
        write(2,'(''FIELD.INPUT= /opdata/hirlam12/h12snap18.dat'')')
        write(2,'(''FIELD.INPUT= /opdata/hirlam12/h12sf18.dat'')')
        write(2,*) 'FIELD_TIME.FORECAST'
        write(2,*) 'FIELD.OUTPUT= snap.felt'
        write(2,*) 'LOG.FILE=     snap.log'
        write(2,*) 'DEBUG.ON'
        write(2,*) 'END'
c
c... trajectory levels
c
        write(2,'(i3)') ntraj
        do i=1,ntraj
           write(2,'(f7.1)') level(i)
        enddo
c
c... trajectory names
c
        do i=1,ntraj
           write(2,'(a)') trim(tname(i))
        enddo
        write(2,*) '**----'
        close (2)
c
        stop
        end

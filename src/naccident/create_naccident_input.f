c create_naccident_input.f
c
c Program reads the input file for SNAP run with nuclear accidents specified by NRPA.
c The input file from NRPA is: nrpa_input.txt.txt
c Than, program reads a list of isotopes with the identification number,
c name and decay rate from the file isotope_list.txt
c The output from the program is: snap.input - standard input file for the snap model
c
c------------------------------------------------------------------------------------------------------------------
c 15.06.2011 - first version - Jerzy Bartnicki
c 22.09.2011 - mdification - Jerzy Bartnicki
c 22.03.2013 - fixing decay-rates, fix order - Heiko Klein
c------------------------------------------------------------------------------------------------------------------
c
       implicit none
       character*80 lname
       real lat, long		! position decimal
       integer idate(6)	! date and time of start
       integer iforecast	! forecast in hours
       integer nlist		! Number of isotopes on the list
       integer maxrel ! max number of releases
       parameter(nlist=382)
       parameter(maxrel=100)
       integer isoid(nlist)	! Isotope id on the list
       integer isoin(nlist)	! Isotope number on the linput file
       character*7 isoname(nlist)	! Isotope name on the list
       integer isotype(nlist)	! Isotope type (0 noble gas, 1 gas, 2 aerosol)
       real drate(nlist)	! Decay rate on the list
       real eratein(nlist)	! Emission rates in the nrpa.input file
       integer i,j,j1,k
       character*40 comp
       integer isoid0		! current isotope id number
       integer hour,minute	! release time in hours and minutes
       integer hmin,hmax	! minimum and maximum height of release interval
       integer nrel		! number of release intervals
       integer niso		! number of isotopes released
       integer ihour(maxrel)	! hours for emissin steps (in hours)
       integer iradius(maxrel)	! release radius for emissin steps (in m)
       integer lowrel(maxrel)	! bottom of release (in m)
       integer toprel(maxrel)	! top of release (in m)
       real emi(nlist,maxrel)		! emission rates for each isotope and emission step emi(iso,step)
       character*3 cname(nlist)	! component name for snap.input file
        character*64 fmt	! Alvaro's variable
       integer npart		! Maximum number of particles released
c
c------------------------------------------------------------------------------------------------------------------
c
c... releas radius for emission steps
c
       do i=1,maxrel
          iradius(i)=50
       enddo
c
c... read id, name and decay rate for each isotope from the list
c
       do i=1, nlist
          isoin(i) = 0
       enddo
       open(1,file='isotope_list.txt')
       rewind 1
       do i=1,nlist
          read(1,'(i4,1x,a7,i2,1x,e10.3)') isoid(i),
     &     isoname(i),isotype(i),drate(i)
c	   write(*,'(2i4,1x,a7,i2,1x,e10.3)') i,isoid(i),
c     &     isoname(i),isotype(i),drate(i)
       enddo
       close (1)
c
       open(1,file='nrpa_input.txt')
c	open(1,file='nrpa_input.txt')
       rewind 1
c
c... coordinates and start time
c
       write(*,*)
       write(*,*) 'info from nrpa_input.txt'
       write(*,*)
       read(1,*) lat
       write(*,*) lat,' LATTITUDE'
       read(1,*) long
       write(*,*) long,' LONGITUDE'
       read(1,'(i4,5(1x,i2))') (idate(i),i=1,6)
       write(*,'(i4,1h-,i2.2,1h-,i2.2,1hT,i2.2,1h:,i2.2,1h:i2.2,1hZ)')
     &(idate(i),i=1,6)
c
c... isotepes involved
c
       read(1,*) niso
       write(*,*) niso,' ISOTOPES'
       do i=1,niso
          read(1,*) isoid0
          do j=1,nlist
             if(isoid0 .eq. isoid(j)) isoin(i)=j
          enddo
          j=isoin(i)
          write(*,'(i4,1x,a7,i3,e10.3)')
     &     isoid(j),isoname(j),isotype(j),drate(j)
       enddo
c
c... maximum number of particles released
c
       npart=200*niso
       write(*,*)
       write(*,'(''Number of particles released ='',i10)') npart
       write(*,*)
c
c.. release intervals
c
       read(1,*) nrel
       write(*,*) nrel,' RELEASE INTERVALS'
       j=0
       ihour(1)=0
       write(*,'('' step='',i3,'' ihour='',i3)') j+1,ihour(j+1)
       do j=1,nrel
         write(*,*) j,' INTERVAL'
         read(1,*)
         read(1,*) hour, minute
         write(*,'(i3,'' hours'',i3,'' minutes'')') hour,minute
         ihour(j+1)= nint(real(hour)+real(minute)/60.0)
c	  ihour(j+)=ihour(j)+hour
         write(*,'('' step='',i3,'' ihour='',i3)') j+1,ihour(j+1)
         read(1,*) hmin, hmax
         write(*,'(i4,'' Hmin'',i4,'' Hmax'')') hmin,hmax
         lowrel(j+1)=hmin
         toprel(j+1)=hmax
         do i=1,niso
           read(1,*) isoid0,eratein(i)
           do j1=1,niso
c release order and isotope order might change
c check by isoid0
             if(isoid0.eq.isoid(isoin(j1))) then
                emi(j1,j+1)=eratein(i)
             end if
           enddo
           j1=isoin(i)
           write(*,'(i4,1x,a7,1x,i1,2e10.3,2e10.3)') isoid0,
     &      isoname(j1),isotype(j1),drate(j1),eratein(i),emi(i,j+1)
         enddo
       enddo
c snap expects start of period, not end of period
c move emissions and height to a time-slot earlier
       do j=2,nrel+1
         lowrel(j-1)=lowrel(j)
         toprel(j-1)=toprel(j)
         do i=1,niso
           emi(i,j-1)=emi(i,j)
         enddo
       end do
c
c... file: snap.input
c
       open(2,file='snap.input')
       rewind 2
c
c	write(2,*) 'TIME.START=   2010 10 14 06'
       write(2,"('TIME.START=  ',i5,3i3)")
     &(idate(i),i=1,4)
       write(2,"('SET_RELEASE.POS= P=',f10.4,',',f10.4)")
     &lat,long
       write(2,"(A)") 'TIME.RUN  = 66h','GRAPHICS.OFF',
c	write(2,"(1(A)") 'TIME.RUN  = 66h','GRAPHICS.OFF',
     & 'VIDEO.SAVE.OFF','RANDOM.WALK.ON',
     & 'BOUNDARY.LAYER.FULL.MIX.OFF','DRY.DEPOSITION.NEW',
     & 'WET.DEPOSITION.NEW',
     & 'TIME.RELEASE.PROFILE.STEPS'
        fmt="(A,T21,??(i10,:,','))"
        write(fmt(8:9),"(I2.2)"),nrel+1
       write(2,fmt) 'RELEASE.HOUR=',(ihour(i),i=1,nrel+1)
       write(2,fmt) 'RELEASE.RADIUS.M=',(iradius(i),i=1,nrel+1)
       write(2,fmt) 'RELEASE.LOWER.M=',(lowrel(i),i=1,nrel+1)
       write(2,fmt) 'RELEASE.UPPER.M=',(toprel(i),i=1,nrel+1)
        fmt="(A,??(1pe10.3,','),1pe10.3,1x,'''C',I2.2,'''')"
        write(fmt(4:5),"(I2.2)"),nrel
       do i=1,niso
          write(2,fmt) "RELEASE.BQ/SEC.COMP=",(emi(i,j),j=1,nrel+1),i
       enddo
c
       write(2,"('*** particles distributed on components',
     &' according to mass')")
       write(2,"('MAX.PARTICLES.PER.RELEASE=',i10)") npart
       do i=1,niso
         j=isoin(i)
         write(2,"('***')")
         write(2,"('Component= C',i2.2)") i
c	  write(2,"('Type=',i2)") isotype(j)
         write(2,"('RADIOACTIVE.DECAY.ON')")
         write(2,"('HALF.LIFETIME.DAYS= ',f10.4)")
     &(log(2.)/(drate(j)*60.*60.*24.))
         select case (isotype(j))
           case(0)
c	    write(2,*) 'Noble gas'
             write(2,"('DRY.DEP.OFF')")
                write(2,"('WET.DEP.OFF')")
             write(2,"('GRAVITY.OFF')")
           case(1)
c	    write(2,*) 'Gas'
             write(2,"('DRY.DEP.ON')")
              write(2,"('WET.DEP.ON')")
             write(2,"('RADIUS.MICROMETER=0.05')")
             write(2,"('DENSITY.G/CM3=0.001')")
             write(2,"('GRAVITY.FIXED.M/S=0.00001')")
           case(2)
c	    write(2,*) 'Aerosol'
             write(2,"('DRY.DEP.ON')")
              write(2,"('WET.DEP.ON')")
             write(2,"('RADIUS.MICROMETER=0.55')")
             write(2,"('DENSITY.G/CM3=2.3')")
             write(2,"('GRAVITY.FIXED.M/S=0.0002')")
           case default
             write(2,"('Error, unknown type ',i2,' for isotope ',i3)")
     &isotype(j), j
             call exit(1)
         end select
c
         write(2,"('FIELD.IDENTIFICATION= ',i2)") i
       enddo
c
       write(2,"('PRECIP(MM/H).PROBAB= 0.0,0.00, 0.5,0.31, 1.0,0.48,'
     &' 1.5,0.60, 2.0,0.66, ',
     &'3.3,0.72, 8.3,0.80, 15.,0.85, 25.,0.91')")
c
       write(2,"('*')")
       write(2,"('REMOVE.RELATIVE.MASS.LIMIT= 0.')")
       write(2,"('*')")
       write(2,"('TIME.STEP= 300.')")
       write(2,"('STEP.HOUR.INPUT.MIN=  3')")
       write(2,"('STEP.HOUR.INPUT.MAX= 12')")
       write(2,"('STEP.HOUR.OUTPUT.FIELDS= 240')")
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
       write(2,"('ARGOS.OUTPUT.DEPOSITION.FILE=    argos_dep')")
       write(2,"('ARGOS.OUTPUT.CONCENTRATION.FILE= argos_conc_inst')")
       write(2,"('ARGOS.OUTPUT.TOTALDOSE.FILE=     argos_conc')")
       write(2,"('ARGOS.OUTPUT.TIMESTEP.HOUR=3')")
       write(2,"('END')")

       close (2)
c
       write(*,*)
       write(*,*) '************************************************'
       write(*,*)
c
100    stop
       end

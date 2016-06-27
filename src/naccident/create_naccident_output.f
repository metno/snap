c create_naccident_output.f
c
c Program out.f generates final output files from the accident run for the ARGOS system in NRPA.
c The input files are:
c   nrpa.input - info about the run
c   argos_dep - SNAP results with calculated deposition
c   argos_conc - SNAP results with calculated time integrated concntration
c   argos_ conc_inst - SNAP results with calculated instanteanous concntration
c The program reads a list of isotopes with the identification numbers
c from the file nrpa.input.
c The output from the program is included in the following files:
c   SNAP_depo - deposition results
c   SNAP_conc - inst. concentration results
c   SNAP_dose - time integrated concentration results
c   griddef.gdf - definition of the SNAP grid for ARGOS
c Only values greater than zero are included in the files
c
c------------------------------------------------------------------------------------------------------------------
c 24.09.2011 - first version - Jerzy Bartnicki
c------------------------------------------------------------------------------------------------------------------
c
       implicit none
       character*70 lname
       integer imax, jmax    ! dimension of the SNAP grid   864  698
       parameter(imax=864,jmax=698)
       integer nlist        ! Number of isotopes on the list
       parameter(nlist=382)
       integer niso        ! number of isotopes released
       integer isoid(nlist)    ! identification numbers for simulated isotopes
       integer i,j,k        ! indexes
       integer ip,is        ! current period and isotope
       integer nperiod        ! number of periods
       real, dimension(:,:), allocatable :: lon    ! longitude matrix
       real, dimension(:,:), allocatable :: lat    ! lattitude matrix
       real, dimension(:,:), allocatable :: dep    ! deposition matrix
       real, dimension(:,:), allocatable :: conc   ! concentration matrix
       real, dimension(:,:), allocatable :: dose   ! time integration matrix
       character*10 period(30)    ! definition of 3-hour periods
c
c------------------------------------------------------------------------------------------------------------------
c
c... open output files: naccident_out.input, naccident_out_depo, naccident_out_conc, naccident_out_dose
c

       allocate(lon(imax,jmax))
       allocate(lat(imax,jmax))
       allocate(dep(imax,jmax))
       allocate(conc(imax,jmax))
       allocate(dose(imax,jmax))

       open(1,file='nrpa_input.txt')
       rewind 1
c
       open(21,file='naccident_SNAP_depo')    ! deposition
       rewind 21
       open(22,file='naccident_SNAP_dose')    ! time integrated concentration
       rewind 22
       open(23,file='naccident_SNAP_conc')    ! Instanteanous concentration
       rewind 23
c
c... read identification numbers from the file: naccident_out.input
c
       write(*,*)
       write(*,*) 'Initial data'
       write(*,*)
       write(*,*) 'naccident_out.txt - opened'
       do i=1,3
         read(1,'(a70)') lname
         write(*,'(i2,1x,a70)') i, lname
       enddo
c
       read(1,*) niso
       write(*,*) 'niso=',niso
c
       do i=1,niso
          read(1,*) isoid(i)
          write(*,*) i,isoid(i)
       enddo
       close (1)
c
c... read files argos_* and create the output file with grid definition
c
       write(*,*)
       write(*,*) 'Header for grid definition and sample values'
       write(*,*)
c
       open(11,file='argos_dep')
       rewind 11
       open(12,file='argos_conc')
       rewind 12
       open(13,file='argos_conc_inst')
       rewind 13
c
       open(2,file='griddef.gdf')
       rewind 2
       do i=1,4
         read(11,'(a70)') lname
         write(*,'(a70)') lname
         write(21,'(a70)') lname
c
         read(12,'(a70)') lname
         write(*,'(a70)') lname
         write(22,'(a70)') lname
c
         read(13,'(a70)') lname
         write(*,'(a70)') lname
         write(23,'(a70)') lname
c
       enddo
c
c... read number of periods for output and specifications
c
       read(11,*) nperiod
       write(*,*) 'nperiod=',nperiod
       rewind 11
       do i=1,4
         read(11,'(a70)') lname
       enddo
       read(11,*) j,(period(i),i=1,nperiod)
       read(12,*) j,(period(i),i=1,nperiod)
       read(13,*) j,(period(i),i=1,nperiod)
       write(*,"(i3,30(1x,a10))") j,(period(i),i=1,nperiod)
       write(21,"(i3,30(1x,a10))") j,(period(i),i=1,nperiod)
       write(22,"(i3,30(1x,a10))") j,(period(i),i=1,nperiod)
       write(23,"(i3,30(1x,a10))") j,(period(i),i=1,nperiod)
c
       do i=6,10
         read(11,'(a70)') lname
         read(12,'(a70)') lname
         read(13,'(a70)') lname
c      write(*,'(a70)') lname
       enddo
c
       write(*,"('HEADER')")
       write(*,"('864  698')")
       write(*,"('1 centered')")
       write(*,"('-25.00 0.00')")
       write(*,"('SINGLE-LEVEL FIELDS')")
       write(*,"('longitude (decimal deg.)')")
       write(*,"('1.00e+000')")
c
       write(2,"('HEADER')")
       write(2,"('864  698')")
       write(2,"('1 centered')")
       write(2,"('-25.00 0.00')")
       write(2,"('SINGLE-LEVEL FIELDS')")
       write(2,"('longitude (decimal deg.)')")
       write(2,"('1.00e+000')")
       read(11,*) lon
       read(12,*) lon
       read(13,*) lon
       write(2,"(10f10.4)") ((lon(i,j),i=1,imax),j=1,jmax)
c
       write(*,*)
       write(*,*) 'Longitude sample values'
       write(*,*)
       write(*,"(5x,6i10)") (i,i=200,205)
       do j=205,200,-1
          write(*,"(i3,2x,6f10.3)") j,(lon(i,j),i=200,205)
       enddo
       write(*,*)
c
       read(11,'(a70)') lname
c    write(21,'(a70)') lname
       read(12,'(a70)') lname
c    write(22,'(a70)') lname
       read(13,'(a70)') lname
c    write(23,'(a70)') lname
       write(*,'(a70)') lname
c
       read(11,'(a70)') lname
c    write(21,'(a70)') lname
       read(12,'(a70)') lname
c    write(22,'(a70)') lname
       read(13,'(a70)') lname
c    write(23,'(a70)') lname
       write(*,'(a70)') lname
c
       write(2,"('latitude (decimal deg.)')")
       write(2,"('1.00e+000')")
       read(11,*) lat
       read(12,*) lat
       read(13,*) lat
       write(2,"(10f10.4)") ((lat(i,j),i=1,imax),j=1,jmax)
c
       write(*,*)
       write(*,*) 'Latitude sample values'
       write(*,*)
       write(*,"(5x,6i10)") (i,i=200,205)
       do j=205,200,-1
          write(*,"(i3,2x,6f10.3)") j,(lat(i,j),i=200,205)
       enddo
       write(*,*)
       close (2)
c
c... read deposition results from the argos files and create the output files with non-zero values
c
       write(*,*)
       write(*,*) 'Headers and sample values of the deposition'
       write(*,*)
       read(11,'(a70)') lname
       write(*,'(a70)') lname
       write(21,'(a70)') lname
c
       read(12,'(a70)') lname
       write(22,'(a70)') lname
c
       read(13,'(a70)') lname
       write(23,'(a70)') lname
c
       do ip=1,nperiod
       do is=1,niso
         read(11,'(a70)') lname
         write(*,"('Isotope ',i3,
     &' deposition (Unit/m2)',' ip=',i2)") isoid(is),ip
         write(21,"('Isotope ',i3,
     &' deposition (Unit/m2)')") isoid(is)
         read(12,'(a70)') lname
         write(*,"('Isotope ',i3,
     &' dose (Unit*hr/m3)',' ip=',i2)") isoid(is),ip
         write(22,"('Isotope ',i3,
     &' dose (Unit*hr/m3)')") isoid(is)
         read(13,'(a70)') lname
         write(*,"('Isotope ',i3,
     &' concentration (Unit/m3)',' ip=',i2)") isoid(is),ip
         write(23,"('Isotope ',i3,
     &' concentration (Unit/m3)')") isoid(is)
c
         read(11,'(a70)') lname
c      write(*,'(a70)') lname
         write(21,'(a70)') lname
         read(12,'(a70)') lname
         write(22,'(a70)') lname
         read(13,'(a70)') lname
         write(23,'(a70)') lname
c
         read(11,'(a70)') lname
c      write(*,'(a70)') lname
         write(21,'(a70)') lname
         read(12,'(a70)') lname
         write(22,'(a70)') lname
         read(13,'(a70)') lname
         write(23,'(a70)') lname
c
         read(11,*) dep
         read(12,*) dose
         read(13,*) conc
c
         do i=1,imax
         do j=1,jmax
           if(dep(i,j) .gt. 0.0) then
c          write(*,"(2i4,e12.4)") i,j,dep(i,j)
             write(21,"(2(i4,1h,),1pe14.6e2)") i,j,dep(i,j)
           endif
           if(dose(i,j) .gt. 0.0) then
c          write(*,"(2i4,4x,e12.4)") i,j,dose(i,j)
             write(22,"(2(i4,1h,),1pe14.6e2)") i,j,dose(i,j)
           endif
           if(conc(i,j) .gt. 0.0) then
c          write(*,"(2i4,8x,e12.4)") i,j,conc(i,j)
             write(23,"(2(i4,1h,),1pe14.6e2)") i,j,conc(i,j)
           endif
         enddo
         enddo
         write(*,*)
       enddo
       enddo
c
       close (21)
       close (22)
       close (23)
       close (11)
       close (12)
       close (13)
c
       stop
       end

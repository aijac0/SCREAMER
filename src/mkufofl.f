      subroutine ufovals
c
c-------Description-----------------------------------------------------
c
c  Source File : ufovals.for
c
c  Author/Date : Mark Kiefer, 1265  (original version)
c                Major rewrite by Kelley Fugelso, 1265 (SEA)  01/90
c                Mac mods December 10, 1992;     hnw
c                  
c  Purpose     : Creates file which contains the data points for the ufo
c                output requests.
c
c  Modifications:
c    03/07/95, MLK: Change include filenames to be 8 characters or less
c                   added "no_text" parameter
c    1997-08013 MLK: Create filename based on 'base_filename'
c    2013-12-09 RBS: Fixed the extra filename text, added two character
c                    lengths*80 like mkcsvfl
c    2013-12-10 RBS: Added two spaces in front of time
c    2013-12-10 RBS: Removed the format calls for consistency
c    2014-02-08 RBS: Changed the output format to 1pe13.5
c    2014-03-12 RBS: Changed the time label to Time(s)
c 2015-06-23 RBS: Placed newfile, oldfile, fflag declarations internal
c                 removed them from zdemout to get rid of compiler
c                 warnings.
c 2015-06-23 RBS: Declared time_flag, half_step, whole_step internal to
c                 function to eliminate compiler warnings
c 2015-06-23 RBS: Explicit integer conversion line 78
c 2017-02-08 RBS: Added a space in the time label in line 95/96 between
c                 Time and (s), removed the leading space.
c 2017-02-08 RBS: Increased the length of the lblout to 12 from 10
c 2017-02-08 RBS: Removed the leading spaces from the real write.
c 2017-12-16 RBS: Increased the length of the lblout to 16 from 12
c 2017-12-16 RBS: Increased the number of data col from 100 to 120
c 2018-01-31 RBS: Increase the number of data col from 120 to 150
c 2019-01-16 RBS: Increase the number of data col from 150 to 200
c 2019-01-27 RBS: Added a scale_out multiplier to the print outdata loop
c 2019-01-28 RBS: Increased the length of the lblout to 22 from 16
c 2019-04-05 RBS: Changed output file name
c 2019-12-01 RBS: Increased the length of the lblout to 25 from 22
c
c-------Include Files---------------------------------------------------
c
      use zdemmax
      include 'zdemparm.h'
      include 'zdempprm.h'
      include 'zdemout.h'
      include 'zdemcomm.h'
      include 'zdemenv.h'
c
c-------Input Parameters------------------------------------------------
c     NONE
c-------Output Parameters-----------------------------------------------
c     NONE
c-------Constants-------------------------------------------------------
c
      integer    ufo_unit
      parameter (ufo_unit    =  24)

      integer   newfile, oldfile, fflag
      parameter (newfile=1, oldfile=2)

c
c ***** Time flag parameters ******
c

      integer time_flag, half_step,     whole_step
      parameter         (half_step = 1, whole_step = 2)

c
c-------Local Variables-------------------------------------------------
c
      character  ufofile*80, tempfile*80, filename*80
c
c-------Subroutine Body-------------------------------------------------
c
c Clear the output buffers, calculate the record size of the output
c   parameter file, and "gather" all of the UFO output requests together.
c
      call clear_outbuf
      ibufsize = numout*2+2
      call gather (iouttype, oufo, maxout, indices, numufo)
c
c Find start time and stop time, skip factor and nptsufo (these will
c    be the same for all UFO output requests
c
      tstart = tbegout(indices(1))
      tstop = tendout(indices(1))
      nskip = ifsteps (maxfpts, ht, tstart, tstop)
      nptsufo = int(((tstop - tstart) / ht) / nskip) + 1
c
c  Create the UFO filename based on the input file name
c
      ufofile = base_filename
      call strip (ufofile, i_1st, i_last)
c
c     strip off the text file extension
c
      call strip_name (ufofile(i_1st:i_last),tempfile,lentemp)
      filename = tempfile(1:lentemp)//'_d.txt'
      open (unit=ufo_unit, file=filename, status='unknown')
c
c
C   write out the observer names out as data column labels
C
      write (ufo_unit, '(A10,1x,200(A25))')
     &      'Time (s)  ',(lblout(indices(j))(1:25), j=1,numufo)
C
c      
c  Process UFO output requests
c
      time_flag = half_step
      ncycle = 0
      ipntcnt = 0
      fflag = oldfile
      iunit   = outunit
      call open_outfile (iunit, fflag, ierr)
c
c  Get the value of time t1/2 and first data value at time 0.0
c
      newrec = 1
      ipntcnt = 1
      call read_outfile (iunit, newrec, time_flag, indices(1), 
     +                   ibufsize, tmptime, tmpval, ierr)
      timeout(ipntcnt,1) = tmptime
      outdata(ipntcnt,1) = tmpval
      newrec = 0
c
c  Reads the rest of the first row containing the outputted values
c
      do i = 2, numufo
         call read_outfile (iunit, newrec, time_flag, indices(i),
     +                      ibufsize, tmptime, tmpval, ierr)
         outdata(ipntcnt,i) = tmpval
      enddo
c
c Get values for the rest of the simulation
c
c first skip unwanted data
c
      newrec = 1
      call read_outfile (iunit, newrec, time_flag, indices(1),
     +                    ibufsize, tmptime, tmpval, ierr)
c
      do while (ierr .eq. 0)
         ncycle = ncycle + 1
         if (ncycle .ge. nskip) then
c
c  Reads and stores the data that is needed for the output file.
c
            ipntcnt = ipntcnt + 1
            timeout(ipntcnt,1) = tmptime
            outdata(ipntcnt,1) = tmpval
            newrec = 0
c
c change i here to j for clarity
c
            do i = 2, numufo
               call read_outfile(iunit, newrec, time_flag, indices(i),
     +                           ibufsize, tmptime, tmpval, ierr)
               outdata(ipntcnt,i) = tmpval
            enddo
            ncycle = 0
         endif
c
c  Skips through the data that you do not want in the output file.
c
         newrec = 1
         call read_outfile (iunit, newrec, time_flag, indices(1), 
     +                      ibufsize, tmptime, tmpval, ierr)
      enddo
c
c  Close the temp file
c
      call close_outfile (iunit,ierr)
c
c Now write it all out.
c Write out all values at each time step: first line has time and first
c   100 variables.
c
      do i = 1, nptsufo
c
c Set the number of points to be printed in the first line of the group
c of values. Writes the output data array.
c
        write (ufo_unit, '(201(1pe12.5,2x))')
     &        timeout(i,1),(outdata(i,j)*scale_out(j), j=1,numufo)
c
      end do        !end of loop over time points
c
c
c Now close the file
c
      close (unit=ufo_unit)
c
c-------End of Subroutine-----------------------------------------------
c
      return
      end

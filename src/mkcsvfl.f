      subroutine csvvals
c
c-------Description-----------------------------------------------------
c
c  Source File : csvvals.f
c
c  Author/Date : Mark Kiefer, based on ufovals routine
c                  
c  Purpose     : Creates file which contains the output data points for
c                the csv file type requests.
c
c  Modifications:
c    1997-08-13 MLK: Create filename based on 'base_filename'
c    2012-04-10 RBS: Got rid of dangling commas finally
c    2012-04-10 RBS: Fixed the extra filename text
c    2013-12-10 RBS: Removed the format statements
c    2013-12-10 RBS: Increased the size of the output data to 1PE11.4
c    2014-02-08 RBS: Changed the output formation to 1pe12.5
c 2015-06-23 RBS: Placed newfile, oldfile, fflag declarations internal
c                 removed them from zdemout to get rid of compiler
c                 warnings.
c 2015-06-23 RBS: Declared time_flag, half_step, whole_step internal to
c                 function to eliminate compiler warnings
c 2015-03-30 RBS: Changed line 74 to explicit integer conversion
c 2019-01-28 RBS: Increased label length from 12 to 22
c                 Added output scaling to the final write statement
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
      integer    csv_unit
      parameter (csv_unit    =  24)

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
      character  csvfile*80, tempfile*80, filename*80
c
c-------Subroutine Body-------------------------------------------------
c
c Clear the output buffers, calculate the record size of the output
c   parameter file, and "gather" all of the CSV output requests together.
c
      call clear_outbuf
      ibufsize = numout*2+2
      call gather (iouttype, ocsv, maxout, indices, numcsv)
c
c Find start time and stop time, skip factor and nptscsv (these will
c    be the same for all CSV output requests
c
      tstart  = tbegout(indices(1))
      tstop   = tendout(indices(1))
      nskip   = ifsteps (maxfpts, ht, tstart, tstop)
      nptscsv = int(((tstop - tstart) / ht) / nskip) + 1
c
c  Create the CSV filename based on the input file name
c
      csvfile = base_filename
      call strip (csvfile, i_1st, i_last)
c
c     strip off the text file extension
c
      call strip_name (csvfile(i_1st:i_last),tempfile,lentmp)
      filename = tempfile(1:lentmp)//'.csv'
      open (unit=csv_unit, file=filename, status='unknown')
c
c   write out the observer names out as titles
c
      write (csv_unit, '(A,$)') 'Time (s)'
      do j = 1,numcsv
        write (csv_unit, '(A, A22,$)') ',',lblout(indices(j))
      end do
      write (csv_unit,'( )')
c
c doing this gets rid of the dangling comma!!! Tricky
c
c      
c  Process CSV output requests
c
      time_flag = half_step
      ncycle    = 0
      ipntcnt   = 0
      fflag     = oldfile
      iunit     = outunit
      call open_outfile (iunit, fflag, ierr)
c
c  Get the value at time 0.0
c
      newrec  = 1
      ipntcnt = 1
      call read_outfile (iunit, newrec, time_flag, indices(1), 
     +                   ibufsize, tmptime, tmpval, ierr)
      timeout(ipntcnt,1) = tmptime
      outdata(ipntcnt,1) = tmpval
      newrec = 0
      do i = 2, numcsv
         call read_outfile (iunit, newrec, time_flag, indices(i),
     +                      ibufsize, tmptime, tmpval, ierr)
         outdata(ipntcnt,i) = tmpval
      enddo
c
c Get values for the rest of the simulation
c
      newrec = 1
      call read_outfile (iunit, newrec, time_flag, indices(1),
     +                    ibufsize, tmptime, tmpval, ierr)
c
      do while (ierr .eq. 0)
         ncycle = ncycle + 1
         if (ncycle .ge. nskip) then
            ipntcnt = ipntcnt + 1
            timeout(ipntcnt,1) = tmptime
            outdata(ipntcnt,1) = tmpval
            newrec = 0
            do i = 2, numcsv
               call read_outfile(iunit, newrec, time_flag, indices(i),
     +                           ibufsize, tmptime, tmpval, ierr)
               outdata(ipntcnt,i) = tmpval
            enddo
            ncycle = 0
         endif
         newrec = 1
         call read_outfile (iunit, newrec, time_flag, indices(1), 
     +                      ibufsize, tmptime, tmpval, ierr)
      enddo
      call close_outfile (iunit,ierr)
c
c Now write it all out.
c Write out all values at each time step: first line has time and
c   then the 100 variables.
c
      do i = 1, nptscsv
c
c Set the number of points to be printed in the first line of the group
c of values.
c
        write (csv_unit, '(1pe12.5,$)') timeout(i,1)

        do j = 1, numcsv
          write (csv_unit, '(A,1pe12.5,$)') ',',
     +                    outdata(i,j)*scale_out(j)
        end do

        write (csv_unit,'( )')
c
c doing this gets rid of the dangling comma!!! Tricky
c
c
      end do        !end of loop over time points
c
c Now close the file
c
      close (unit=csv_unit)
c
c-------End of Subroutine-----------------------------------------------
c
      return
      end

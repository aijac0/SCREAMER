      subroutine filvals
c
c-------Description-----------------------------------------------------
c
c  Source File : mkfilfl.f
c
c  Author/Date : Mark Kiefer, 1265  (original version)
c                Major rewrite by Kelley Fugelso, 1265 (SEA)    01/90
c
c  Purpose     : Creates storage files which contain the recorded points
c                 for each file request.
c                The naming algorithm is:
c                 NAME.F##
c                 where NAME is the data file name and ## is the file index.
c
c                The format of the file is:
c                 line 1:  I4,X,A75
c                 where I4 is the number of points in the file and A75 is 
c                 the branch/block label with the variable name.
c                 lines 2 to npoints+1:  1p2e12.3
c
c  Called by   : Program ZDEM$MAIN
c
c  Calls       : Subroutine STRIP, Subroutine CLEAR_OUTBUF, 
c                Subroutine GATHER, Subroutine OPEN_OUTFILE,
c                Subroutine READ_OUTFILE, Subroutine CLOSE_OUTFILE,
c                Subroutine INT_TO_TEXT
c
c  Modifications:
c    03/07/95, MLK: Change include filenames to be 8 characters or less
c                   added "no_text" parameter
c    08/13/97, MLK: Create filename based on 'base_filename'
c 2015-06-23 RBS: Placed newfile, oldfile, fflag declarations internal
c                 removed them from zdemout to get rid of compiler
c                 warnings.
c 2015-06-23 RBS: Declared time_flag, half_step, whole_step internal to
c                 function to eliminate compiler warnings
c 2019-01-28 RBS: Removed write format statements
c                 Added a scale_out multiplier to the print outdata loop
c
c-------Include Files---------------------------------------------------
c
      include 'zdemparm.h'
      include 'zdempprm.h'
      include 'zdemmax.h'
      include 'zdemout.h'
      include 'zdemcomm.h'
      include 'zdemenv.h'
c
c-------Input Parameters------------------------------------------------
c
c     NONE
c
c-------Output Parameters-----------------------------------------------
c
c     NONE
c
c-----Constants---------------------------------------------------------
c

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
      integer    no_text
      parameter (no_text = 0)
      character  ti*2, t1*2, t2*2
      character  header*73, filfile*80
c
c-------Subroutine Body-------------------------------------------------
c
c Create a template for the file name from the input data file name
c
      filfile = base_filename
      call strip (filfile, i_1st, i_last)
      iend1 = i_last - i_1st + 3
      iend2 = iend1 + 1
      filfile(1:iend1) = filfile(i_1st:i_last)//'.f'
      filfile(iend2:80) = ' '
c
c Clear the output buffers, calculate the record size of the output
c   parameter file, and "gather" all of the FILE output requests together.
c
      call clear_outbuf
      ibufsize = numout*2+2
      call gather (iouttype, ofile, maxout, indices, numfil)
c
c Loop over all of the file requests, creating one output file for each
c
      do i = 1, numfil
c
         time_flag   = itimeflg(indices(i))
         ncycle  = 0
         tstart  = tbegout(indices(i))
         tstop   = tendout(indices(i))
         nskip   = ifsteps (maxfpts, ht, tstart, tstop)
         ipntcnt = 0
         fflag   = oldfile
         iunit   = outunit
         call open_outfile (iunit, fflag, ierr)
c
c       Get the value at time 0.0
c
         newrec = 1
         call read_outfile (iunit, newrec, time_flag, indices(i),
     +                      ibufsize, tmptime, tmpval, ierr)
         if (tstart .eq. 0.0) then
            ipntcnt = 1
            timeout(ipntcnt,1) = tmptime
            outdata(ipntcnt,1) = tmpval
            call read_outfile (iunit, newrec, time_flag, indices(i), 
     +                         ibufsize, tmptime, tmpval, ierr)
         endif
c
c  Read the values for this parameter, saving only those which fall
c    on a "save cycle"
c    
         do while (ierr .eq. 0)
            if ((tmptime.ge.tstart).and.(tmptime.le.tstop)) then
               ncycle = ncycle + 1
               if (ncycle .ge. nskip) then
                  ipntcnt = ipntcnt + 1
                  timeout(ipntcnt,1) = tmptime
                  outdata(ipntcnt,1) = tmpval
                  ncycle = 0
               endif
            endif
            call read_outfile (iunit, newrec, time_flag, indices(i),
     +                         ibufsize, tmptime, tmpval, ierr)
         enddo
         call close_outfile(iunit,ierr)
c
c Translate ifile to text and append it to the filename to the the full
c file specification (less version number).
c
         call int_to_text (i, ti)
         call strip (ti, i1, i2)
         iend = iend2 + i2 - i1
         filfile(iend2:iend) = ti(i1:i2)
c
c Now open the output request file.
c
         open (unit=22, file=filfile(1:iend), status='NEW')
c
c Set up the header with block and branch indicies, block type, and plot type
c if no comment line entered, else use the comment line.
c
         call strip (lblout(indices(i)), istart, iend)
         if (istart .eq. no_text) then
            call int_to_text (ixbrnout(indices(i)), t1)
            call int_to_text (ixblkout(indices(i)), t2)
            itype = iblkout(indices(i))
            if (itype .eq. transline) then
              header(1:24) = 'Brn '//t1//', Blk '//t2//': trline {'
            else if (itype .eq. pisection) then
              header(1:24) = 'Brn '//t1//', Blk '//t2//': pisect {'
            else if (itype .eq. rcground) then
              header(1:24) = 'Brn '//t1//', Blk '//t2//': RCgrnd {'
            else if (itype .eq. voltsource) then
              header(1:24) = 'Brn '//t1//', Blk '//t2//': Vsourc {'
            else if (itype .eq. vendsource) then
              header(1:24) = 'Brn '//t1//', Blk '//t2//': EVsrce {'
            else if (itype .eq. currsource) then
              header(1:24) = 'Brn '//t1//', Blk '//t2//': Isourc {'
            else if (itype .eq. cendsource) then
              header(1:24) = 'Brn '//t1//', Blk '//t2//': Isourc {'
            else if (itype .eq. csclsource) then
              header(1:24) = 'Brn '//t1//', Blk '//t2//': SCLsrc {'
            else if ((itype .eq. mitline) .or.
     &               (itype .eq. pmitline)) then
              header(1:24) = 'Brn '//t1//', Blk '//t2//': MITL   {'
            else if (itype .eq. adder) then
              header(1:24) = 'Brn '//t1//', Blk '//t2//': Adder  {'
            else if (itype .eq. rlseries) then
              header(1:24) = 'Brn '//t1//', Blk '//t2//': RLsers {'
            end if
c
c Set the label for the plot type. (ylblfil is 11 characters)
c
            header(25:37) = ylblout(indices(i))//'} '
            header(38:73) = ' '
c
c Use the comment line as the header
c
         else
            header(1:73) = lblout(indices(i))(1:73)
         end if
c
c Write the number of points and the header to the file.
c
         write (22, '(i4,3x,a73)') ipntcnt, header
c
c Now write the points to this file
c
         do j = 1, ipntcnt
            write (22, '(1p2e12.3)')
     +                timeout(j,1), outdata(j,1)*scale_out(i)
         end do
c
c Now close the file
c
         close (unit=22)
c
      end do
c
c-------End of Subroutine-----------------------------------------------
c
      return
      end

      subroutine tabvals
c
c-------Description-----------------------------------------------------
c
c  Source File : WAS ud5:[klfugel.screamer.scrref]tabvals.for
c
c  Author/Date : Mark Kiefer, 1265  (original version)
c                Major rewrite by Kelley Fugelso, 1265 (SEA)  01/90
c                Mac mods  December 10, 1992;     hnw
c
c  Purpose     : Creates files which contain the points for each table request.
c                Puts them in correct SCREAMER table format.
c                The naming algorithm is:
c                   NAME.T##
c                where NAME is the data file name and ## is the plot index.
c
c                The format of the file is:
c                  line 1:  ! title
c                  line 2:  ! user comment or circuit location
c                  line 3:  ! number of points
c                  line 4:  1pe10.3, ' 0.0'
c                    where 1pe10.3 is the scale and 0.0 is the delay.
c                  lines 5 to npoints+4:  4x, 1p2e12.3
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
c
c-------Include Files---------------------------------------------------
c
      use zdemmax
      use zdemparm
      use zdemcomm
      include 'zdempprm.h'
      include 'zdemout.h'
      include 'zdemenv.h'
c
c-------Input Parameters------------------------------------------------
c     NONE
c-------Output Parameters-----------------------------------------------
c     NONE
c------Constants-------------------------------------------------------
c
      integer     table_unit
      parameter  (table_unit = 22)

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
      character  header*79, tabfile*80 
c
c-------Subroutine Body-------------------------------------------------
c
c Create a "template" for naming files based on the input file name
c
      tabfile = base_filename
      call strip (tabfile, i_1st, i_last)
        iend1 = i_last - i_1st + 3
        iend2 = iend1 + 1
      tabfile(1:iend1) = tabfile(i_1st:i_last)//'.t'
      tabfile(iend2:80) = ' '
c
c Clear the output buffers, calculate the record size of the output
c   parameter file, and "gather" all of the TABLE output requests together.
c
      call clear_outbuf
      ibufsize = numout*2+2
      call gather (iouttype, otable, maxout, indices, numtab)
c
c Loop over all of the TABLE requests, creating one output file for each
c
      do i = 1, numtab
c
         time_flag = itimeflg(indices(i))
         ncycle  = 0
         tstart  = tbegout(indices(i))
         tstop   = tendout(indices(i))
         nskip   = ifsteps (max_table_points, ht, tstart, tstop)
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
         call close_outfile (iunit,ierr)
c
c Translate i to text and append it to the filename to the the full
c file specification 
c
         call int_to_text (i, ti)
         call strip (ti, i1, i2)
         iend = iend2 + i2 - i1
         tabfile(iend2:iend) = ti(i1:i2)
c
c Now open the file.
c
         open (unit=table_unit, file=tabfile(1:iend), status='NEW')
c
c
c Now print the title as the first line, but comment it out by using a
c '!' as the first character.
c
         write (table_unit, '(1x,a,a79)' ) '!', title(1:79)
c
c Set up the header with block and branch indicies, block type, and plot type
c if no comment line entered, else use the comment line.  Print this as the
c second line, again using a '!' as the first character.
c
         call strip (lblout(indices(i)), istart, iend)
         if (istart .eq. no_text) then
            call int_to_text (ixbrnout(indices(i)), t1)
            call int_to_text (ixblkout(indices(i)), t2)
            itype = iblkout(indices(i))
            header(1:22) = 'Branch '//t1//', Block '//t2//':  '
            if (itype .eq. transline) then
              header(23:44) = 'transmission line     '
            else if (itype .eq. pisection) then
              header(23:44) = 'pisection             '
            else if (itype .eq. rcground) then
              header(23:44) = 'rcground              '
            else if (itype .eq. voltsource) then
              header(23:44) = 'voltage source        '
            else if (itype .eq. vendsource) then
              header(23:44) = 'end-brn voltage source'
            else if (itype .eq. currsource) then
              header(23:44) = 'current source        '
            else if (itype .eq. cendsource) then
              header(23:44) = 'end-brn current source'
            else if (itype .eq. csclsource) then
              header(23:44) = 'SCL current source    '
            else if ((itype .eq. mitline) .or.
     &               (itype .eq. pmitline)) then
              header(23:44) = 'MITL                  '
            else if (itype .eq. adder) then
              header(23:44) = 'adder                 '
            else if (itype .eq. rlseries) then
              header(23:44) = 'rlseries              '
            end if
c
c Set the label for the plot type. (ylbltab is 11 characters)
c
            header(45:57) = '{'//ylblout(indices(i))//'}'
            header(58:79) = ' '
c
c Use the comment line as the header.
c
         else
            header(1:79) = lblout(indices(i))(1:79)
         end if
c
         write (table_unit, 1000) header(1:79)
c
c Write the third line which shows the number of points in the table.
c
         write (table_unit, 2000) ipntcnt
c
c Find the scale factor so that all points lie between -1 and 1.
c
         scale = abs (outdata(1,1))
         do j = 2, ipntcnt
            aval  = abs (outdata(j,1))
            scale = amax1 (aval, scale)
         end do
         if (scale .gt. 0.0) then
            rscale = 1.0 / scale
         else
            rscale = 1.0
         endif
c
c Write the scale and delay to the file.
c
         write (table_unit, 3000) scale
c
c Now write the points to this file, dividing by the scale.
c
         do j = 1, ipntcnt
            write (table_unit, 4000) timeout(j,1), outdata(j,1)*rscale
         end do
c
c Write the last line to signal end of table.
c
         write (table_unit, 5000)
c
c Now close the file
c
         close (unit=table_unit)
c
      end do
c
c-------FORMAT Statements-----------------------------------------------
c
 1000 format (' ', '!', a79)
 2000 format (' ', '!', i4, ' points in the table')
 3000 format (' ', 1pe10.3, '  0.0')
 4000 format (' ', 4x, 1p2e12.3)
 5000 format (' ', '  Last-entry')
c
c-------End of Subroutine-----------------------------------------------
c
      return
      end

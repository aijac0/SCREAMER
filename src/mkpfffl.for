      subroutine pffvals
c
c----Subroutine Summary-------------------------------------------------
c
c Purpose: Creates PFF file which contains the points for the 
c          pff requests.
c
c Author:  Kelley L. Fugelso, 1265 (SEA) 26-mar-1991
c
c Source File: mkpfffl.f
c
c  Modifications:
c 1997-08-13 MLK: Create filename based on 'base_filename'
c 2014-02-06 RBS: Changed real*4 to real
c 2014-05-02 RBS: Changed integer*4 to integer
c 2015-03-28 RBS: pff_file character length changed to *80 to match
c                 base_filename
c 2015-06-23 RBS: Placed newfile, oldfile, fflag declarations internal
c                 removed them from zdemout to get rid of compiler
c                 warnings.
c 2015-06-23 RBS: scrname, pff_array, format statements removed
c                 - unused,
c
c----Include Files------------------------------------------------------
c
      use zdemmax
      use zdemparm
      use zdemcomm
      use zdempprm
      use zdemout
      use zdemenv
c
c-----Constants---------------------------------------------------------
c

      integer   newfile, oldfile, fflag
      parameter (newfile=1, oldfile=2)

c
c-----Local Variables---------------------------------------------------
c
      character*80 desc         !* Comment for PFF dataset            */
      character*80 pff_file     !* PFF file name                      */
c     +             scrname     !* Contains symbls set in SCREAMER.COM*/
      character*15 st
      character*5  t1,          !* Branch # of current variable       */
     +             t2           !* Block # of current variable        */
      integer      whstep(maxout), hfstep(maxout)
      integer      fid, ispare(5)
      integer      no_text
      parameter   (no_text = 0)
      integer      tflag(maxout)
      integer      pfuopn, idlist(3)
c
c----FORMAT Statements--------------------------------------------------
c
c1000  format (' ',a)
c1001  format (' ',a28,5x,a)
c1002  format (' ',a,10x,a)
c1003  format (' ',a8,5x,a)
c
c----Subroutine Body----------------------------------------------------
c
c  Create the name for the PFF file
c
      pff_file = base_filename
      call strip(pff_file,is1,ie1)
      iend1 = ie1 - is1 + 5
      iend2 = iend1 + 1
      pff_file(1:iend1) = pff_file(is1:ie1)//'.pff'
      pff_file(iend2:60) = ' '
c
c Clear the output buffers, calculate the record size of the output
c  parameter file, and "gather" all of the PFF output requests together.
c
      call clear_outbuf
      ibufsize = numout*2+2
      call gather (iouttype, opff, maxout, indices, numpff)
      do i = 1, numpff
         tflag(i) = itimeflg(indices(i))
      enddo
      tstart = tbegout(indices(1))
      tstop = tendout(indices(1))
      nskip = ifsteps (maxfpts, ht, tstart, tstop)
c
      ncycle = 0
      ipntcnt_wh = 0
      ipntcnt_hf = 0
      fflag = oldfile
      iunit   = outunit
      call open_outfile (iunit, fflag, ierr)
c
c       Gather all PFF requests on the whole time step. Then gather
c         all PFF requests of the half time step.
c
      call gather (tflag, whole_step, numpff, whstep, numwh)
      call gather (tflag, half_step,  numpff, hfstep, numhf)
c
c   Process all requests on the whole time step
c
      if (numwh .gt. 0) then
         newrec = 1
         ipntcnt_wh = 1
         call read_outfile (iunit, newrec, whole_step, 
     +                      indices(whstep(1)),
     +                      ibufsize, tmptime, tmpval, ierr)
         timeout(ipntcnt_wh,whstep(1)) = tmptime
         outdata(ipntcnt_wh,whstep(1)) = tmpval
         newrec = 0
         do i = 2, numwh
            call read_outfile (iunit, newrec, whole_step, 
     +                         indices(whstep(i)),
     +                         ibufsize, tmptime, tmpval, ierr)
            timeout(ipntcnt_wh,whstep(i)) = tmptime
            outdata(ipntcnt_wh,whstep(i)) = tmpval
         enddo
         newrec = 1
         call read_outfile (iunit, newrec, whole_step, 
     +                      indices(whstep(1)),
     +                      ibufsize, tmptime, tmpval, ierr)
         do while (ierr .eq. 0)
            ncycle = ncycle + 1
            if (ncycle .ge. nskip) then
               ipntcnt_wh = ipntcnt_wh + 1
               timeout(ipntcnt_wh,whstep(1)) = tmptime
               outdata(ipntcnt_wh,whstep(1)) = tmpval
               newrec = 0
               do i = 2, numwh
                  call read_outfile (iunit, newrec, whole_step, 
     +                               indices(whstep(i)),
     +                               ibufsize, tmptime, tmpval, ierr)
                  timeout(ipntcnt_wh,whstep(i)) = tmptime
                  outdata(ipntcnt_wh,whstep(i)) = tmpval
               enddo
               ncycle = 0
            endif
            newrec = 1
            call read_outfile (iunit, newrec, whole_step, 
     +                         indices(whstep(1)),
     +                         ibufsize, tmptime, tmpval, ierr)
         enddo
         call close_outfile (iunit, ierr)
         call open_outfile (iunit, fflag, ierr)
      endif
c
c   Process all requests on the half time step
c
      if (numhf .gt. 0) then
         ipntcnt_hf = 1
c       *Read first record to get it out of the way
         newrec = 1
         call read_outfile (iunit, newrec, half_step, 1, ibufsize,
     +                      tmptime, tmpval, ierr)
c       *Read first needed record
         call read_outfile (iunit, newrec, half_step, 
     +                      indices(hfstep(1)),
     +                      ibufsize, tmptime, tmpval, ierr)
         timeout(ipntcnt_hf,hfstep(1)) = tmptime
         outdata(ipntcnt_hf,hfstep(1)) = tmpval
         newrec = 0
         do i = 2, numhf
            call read_outfile (iunit, newrec, half_step, 
     +                         indices(hfstep(i)),
     +                         ibufsize, tmptime, tmpval, ierr)
            timeout(ipntcnt_hf,hfstep(i)) = tmptime
            outdata(ipntcnt_hf,hfstep(i)) = tmpval
         enddo
         newrec = 1
         call read_outfile (iunit, newrec, half_step, 
     +                      indices(hfstep(1)),
     +                      ibufsize, tmptime, tmpval, ierr)
         do while (ierr .eq. 0)
            ncycle = ncycle + 1
            if (ncycle .ge. nskip) then
               ipntcnt_hf = ipntcnt_hf + 1
               timeout(ipntcnt_hf,hfstep(1)) = tmptime
               outdata(ipntcnt_hf,hfstep(1)) = tmpval
               newrec = 0
               do i = 2, numhf
                  call read_outfile (iunit, newrec, half_step, 
     +                               indices(hfstep(i)),
     +                               ibufsize, tmptime, tmpval, ierr)
                  timeout(ipntcnt_hf,hfstep(i)) = tmptime
                  outdata(ipntcnt_hf,hfstep(i)) = tmpval
               enddo
               ncycle = 0
            endif
            newrec = 1
            call read_outfile (iunit, newrec, half_step, 
     +                         indices(hfstep(1)),
     +                         ibufsize, tmptime, tmpval, ierr)
         enddo
         call close_outfile(iunit,ierr)
      else
         call close_outfile(iunit,ierr)
      endif
c
c  Now write information stored in output arrays to PFF files
c
c    Open PFF file
c
      irwflag = 1
      ierr = 0
      idummy = 0
      call pfsvrb (6,ierr)
      ierr = 0
      fid = pfuopn (pff_file, irwflag, ierr, idummy) 
      if (ierr. ne. 0) goto 999
c
      do i = 1, numpff
c
         low = 1
         if (tflag(i) .eq. whole_step) then
            imax = ipntcnt_wh
            ipntcnt = ipntcnt_wh
         else
            imax = ipntcnt_hf
            ipntcnt = ipntcnt_hf
         endif
         dx = ht*nskip
         x0 = timeout(1,i)
c
c       *If user did not enter description, create pff comment
c
         call strip(lblout(indices(i)), istart, iend)
         if (istart .eq. no_text) then
c
            call int_to_text(ixbrnout(indices(i)), t1)
            call strip      (t1, it11, it12)
            call int_to_text(ixblkout(indices(i)), t2)
            call strip      (t2, it21, it22)
            iblock_type = iblkout(i)
c
            if (iblock_type .eq. transline) then
              desc=lblout_temp(indices(i))(1:23)//':Branch '//
     +            t1(it11:it12)//
     +            ', Block '//t2(it21:it22)//':Transmission line'
            elseif (iblock_type .eq. pisection) then
              desc=lblout_temp(indices(i))(1:23)//':Branch '//
     +            t1(it11:it12)//
     +            ', Block '//t2(it21:it22)//':Pi Section'
            elseif (iblock_type .eq. rcground) then
              desc=lblout_temp(indices(i))(1:23)//':Branch '//
     +            t1(it11:it12)//
     +            ', Block '//t2(it21:it22)//':RC to Ground'
            elseif (iblock_type .eq. voltsource) then
              desc=lblout_temp(indices(i))(1:23)//':Branch '//
     +            t1(it11:it12)//
     +            ', Block '//t2(it21:it22)//':Voltage Source'
            elseif (iblock_type .eq. vendsource) then
              desc=lblout_temp(indices(i))(1:23)//':Branch '//
     +            t1(it11:it12)//
     +            ', Block '//t2(it21:it22)//':EOB Voltage Source'
            elseif (iblock_type .eq. currsource) then
              desc=lblout_temp(indices(i))(1:23)//':Branch '//
     +            t1(it11:it12)//
     +            ', Block '//t2(it21:it22)//':Current Source'
            elseif (iblock_type .eq. cendsource) then
              desc=lblout_temp(indices(i))(1:23)//':Branch '//
     +            t1(it11:it12)//
     +            ', Block '//t2(it21:it22)//':EOB Current Source'
            elseif (iblock_type .eq. csclsource) then
              desc=lblout_temp(indices(i))(1:23)//':Branch '//
     +            t1(it11:it12)//
     +            ', Block '//t2(it21:it22)//':EOB SCL Curr.Source'
            elseif ((iblock_type .eq. mitline) .or.
     +                (iblock_type .eq. pmitline)) then
              desc=lblout_temp(indices(i))(1:23)//':Branch '//
     +            t1(it11:it12)//
     +            ', Block '//t2(it21:it22)//':MIT Line'
            elseif (iblock_type .eq. adder) then
              desc=lblout_temp(indices(i))(1:23)//':Branch '//
     +            t1(it11:it12)//
     +            ', Block '//t2(it21:it22)//':Adder Block'
            elseif (iblock_type .eq. rlseries) then
              desc=lblout_temp(indices(i))(1:23)//':Branch '//
     +            t1(it11:it12)//
     +            ', Block '//t2(it21:it22)//':RL in Series'
            endif
c
c       *If user entered a description, use it for the PFF comment
c
         else
            call strip(lblout(indices(i)),istart,iend)
            desc=lblout(indices(i))(istart:iend)
         endif
c
c
c       *Write data for this variable to the PFF file
c         *Set dummy variables for PFF library call
c
         itap=-3
         st='SCREAMER Data'
         nblks=1
         locb=1
         idlist(1)=1
         idlist(2)=ipntcnt
         idlist(3)=1
         call pfwuf1 (fid,itap,st,desc,nblks,ispare,x0,dx,
     +                xlblout,ylblout(indices(i)),locb,low,imax,idlist,
     +                outdata(1,i),ierr)
         if (ierr. ne. 0) goto 999
c
      enddo
c
c Now close the PFF file 
c
      call pfucls (fid, ierr)
c
      goto 5000
c
999   print*,' '
      print*,' '
      print*,'>>>>>Error while attempting to open or write to ',
     +        'PFF file...error=',ierr
c
c-----End of Subroutine-------------------------------------------------
c
5000  return
      end

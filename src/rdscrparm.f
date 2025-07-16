c----------------------------------------------------------------------
c    @(#)rdscrparm.f   version 1.0   created 06/15/2005 by Mathias Bavay
c----------------------------------------------------------------------
c---------------------------------------------------------------------
c
c  This subroutine does the processing of the general parameters
c  at the begining of an input deck.
c
c  Modifications:
c 2014-05-01 RBS: Initialize switch_time to zero
c---------------------------------------------------------------------

      subroutine readscreamerparameters(status)
      
      include 'zdemmax.h'
      include 'zdemcomm.h'
      include 'zdemout.h'
c
c Include the files with the keywords and the integer flags as parameters.
c
      include 'zdemparm.h'
      include 'zdempprm.h'
      include 'zdemenv.h'

      include 'rdscrdat.h'
c
c Define passed variables
c
      integer status
c
      status=0

c Get the initial parameters.
c First, set the flags which tell if a particular parameter has been
c  entered,
c set the TITLE to blanks in case one is not entered,
c then zero the counters for the number of lines with errors.
c In this section, any line starting with diaglabel_key or a
c diaglabel_altkey is considered a comment line.
c
      tstepflg      = err
      endtflg       = err
      nprtflg       = err
      ncyclflg      = err
      restflg       = err
      plot_grid     = no_grid
      maxfpts       = max_plot_points
      echoset       = no_echo
      detail_prints = detail_prints_min
      title         = ' '
      numerr        = 0

      do i = 1, max_switch_points
         switch_time(i) = 0.0
         enddo

c
c Get the title, assumed to be everything on line 1,
c and set the counter for the number of lines read.
c Write a banner page using the title and then start a new page writing
c  the time.
c
      read (lunit, 100) title
 100  format (a80)
      nlines = 1
      call banner (title)
c
c Get the initial parameters, checking for comments.
c
  200 continue
c
      call get_next_line
     &     (currline, currline_lc, field, nlines, eofflg, max_fields)
      if (eofflg .eq. err) then
         status=1000
         return
      endif
      keyword = field(1)(1:keyword_len)
c
c Plot grids?
c
      if (keyword .eq. k_plot_grid) then
        if (field(2)(1:1) .eq. k_yes_grid) then
          plot_grid = yes_grid
        end if
c
c Echo the setup parameters and indicies?
c
      else if (keyword .eq. k_echo) then
        if (field(2)(1:1) .eq. k_yes_echo) then
          echoset = yes_echo
        end if
c
c Set level of detail on printing to log file?
c
      else if (keyword .eq. k_detail_prints) then
        if (field(2)(1:keyword_len) .eq. k_detail_prints_full) then
          detail_prints = detail_prints_full
        end if
c
c Time step
c
      else if (keyword .eq. k_time_step) then
        call text_to_real (field(2), time_step, flag)
        if (flag .eq. noerr) then
          ht       = time_step
          tstepflg = noerr
        else
          call print_bad_line (currline, nlines, numerr)
        end if
c
c Stop time (end simulation)
c
      else if (keyword .eq. k_end_time) then
        call text_to_real (field(2), end_time, flag)
        if (flag .eq. noerr) then
          tmax    = end_time
          endtflg = noerr
        else
          call print_bad_line (currline, nlines, numerr)
        end if
c
c Number of print outs during the execution.
c
      else if (keyword .eq. k_num_prints) then
        call text_to_int (field(2), num_prints, flag)
        if (flag .eq. noerr) then
          nprint = num_prints
          nprtflg = noerr
        else
          call print_bad_line (currline, nlines, numerr)
        end if
c
c Number of cycles to execute (one or all).
c
      else if (keyword .eq. k_num_cycles) then
        keychar           = field(2)(1:1)
        if (keychar .eq. k_all_cycles) then
          iset     = all_cycles
          ncyclflg = noerr
        else if (keychar .eq. k_one_cycle) then
          iset     = one_cycle
          ncyclflg = noerr
        else
          call print_bad_line (currline, nlines, numerr)
        end if
c
c Resolution time step (transmission lines only).
c
      else if (keyword .eq. k_res_time) then
        call text_to_real (field(2), rtime, flag)
        if (flag .eq. noerr) then
          res_time = rtime
          restflg  = noerr
        else
          call print_bad_line (currline, nlines, numerr)
        end if
c
c Maximum number of points to store for plots, prints, and files.
c
      else if (keyword .eq. k_maxfpts) then
        call text_to_int (field(2), jmax, flag)
        if (flag .eq. noerr) then
          if (jmax .le. max_plot_points) then
            maxfpts = jmax
          end if
        else
          call print_bad_line (currline, nlines, numerr)
        end if
c
c Look for switch table input, input vector values
c
      else if (keyword .eq. k_switch_time) then
         call read_vector (switch_time, num_pts, eofflg, nlines, numerr)
c
c Is this the first branch after the set up parameters?
c
      else if (keyword .eq. k_branch) then
        goto 300
c
c Comment line.
c
      else if ((keyword(1:1) .eq. comment_key) .or.
     +         (keyword(1:1) .eq. diaglabel_key) .or.
     +         (keyword(1:1) .eq. diaglabel_altkey) )then
        continue
c
c Something unknown, so signal error.
c
      else
        call print_bad_line (currline, nlines, numerr)
      end if
c
c Go and get another line.
c
      go to 200
      
c      if (0 .eq. 1) then
c 1000   status=1000
c      end if
 
  300 continue
      return
      end

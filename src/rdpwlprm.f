      subroutine read_pwl_parms (parms, nparams, eofflg, nlines,
     &                           numerr)

c Modification log
c
c 2008-07-17 RBS: Changed the length of currline to 120 from 80
c 2014-02-06 RBS: Changed real*4 to real
c 2016-03-16 RBS: Cleaned up a single write format statement
c 2017-02-24 RBS: Added the new passed variable currline_lc in the
c                 get_next_line call. Also added the necessary character
c                 declaration
c
c-----------------------------------------------------------------------
c
c This subroutine reads in a set of (X,Y) values to be used in a
c piecewise linear model.  Up to 201 (X,Y) pairs
c may be entered, two per line.
c The first coefficient on each line is X and the second is Y.
c The subroutine quits reading when an error is detected (there were
c not two numbers on the line).  If the error is because there was
c the keyword 'LAS*T' is on the line, then it returns normally with the
c X,Y array entered properly.  If the error is caused by something
c besides the keyword as the first data on the line, an error is
c signalled and the subroutine exits and not all the X,Y may have been
c read.
c If more than 201 pairs are read in,
c only the first 201 area kept and an error is signalled.
c X1 is parms(1), Y1 is parms(2), X2 is parms(3), Y2 is parms(4), ...
c num_parms is twice the number of pairs read in.

c
c Include the modules containing the keywords.
c
      use zdemparm
      include  'zdempprm.h'
      
c
c Define passed variables
c
      real       parms(*)
      integer    nparams, eofflg, nlines, numerr

c
c Various parameters and variable types.
c
      character  currline*120, currline_lc*120
      integer    flag1, flag2
      integer    noerr,          error,     notext
      parameter (noerr      = 0, error = 1, notext = 0)
      parameter (max_pwl_pairs = 401)
      parameter (max_pwl_parms = max_pwl_pairs*2)
      parameter (max_fields    = 10)
      character  field(max_fields)*80, keyword*(keyword_len)
c
c Set the counter for the number of elements read and the limit check to
c no error
c
      nparams = 0
      limit     = noerr
c
c Get the line.
c
  200 continue
      call get_next_line
     &     (currline, currline_lc, field, nlines, eofflg, max_fields)
      if (eofflg .eq. error) then
      return
      end if
c
      if (nparams .lt. max_pwl_parms) then
c
c Attempt to convert each field into a real number.
c
        call text_to_real (field(1), parms(nparams+1), flag1)
        call text_to_real (field(2), parms(nparams+2), flag2)
c
c If we failed to convert a number, see if we had the last entry keyword.
c If so, then set the number of parameters entered, else signal an error.
c If we did fail, this will cause the subroutine to exit.
c
        if ((flag1+flag2) .eq. noerr) then
          nparams = nparams +2
        else
          keyword   = field(1)(1:keyword_len)
          if (keyword .ne. k_last_entry) then
            call print_bad_line (currline, nlines, numerr)
          end if
          go to 1000
        end if
c
c If we have read in the maximum number of pairs, check this line
c for LAST keyword.  If not LAST do not use anything on the line.
c
      else
        keyword      = field(1)(1:keyword_len)
        if (keyword .ne. k_last_entry) then
          numerr = numerr + 1
          limit      = error
        else
          go to 1000
        end if
c
      end if
      go to 200
c
c If we were over the limit, send a message and tell what the current
c limit is.
c
 1000 continue
      eofflg = noerr
      if (limit .eq. error) then
        write(9,'(2A,I3,A)')
     &               '0', 'Error, too many table values entered, only ',
     &               max_pwl_pairs, ' points allowed!'
      end if
c
c      return
      end

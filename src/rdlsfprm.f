      subroutine read_lsf_parms
     &           (parms, num_parms, eofflg, nlines, numerr)
c
c This subroutine reads in a set of coefficients to be used in a least
c squares polynomial model.  Up to 7 coefficients
c may be entered (6th order polynomial), all on one line.
c The first coefficient (parms(1)) is for the constant term,
c the second (parms(2)) for the linear term, ...
c num_parms is the number of coefficients read in on the line.
c This subroutine does not print any of the data read in, but does
c print an error message if a user error is encountered.
c
c Modification log
c
c 2008-07-17 RBS: Changed the length of currline to 120 from 80
c 2014-02-06 RBS: Changed real*4 to real
c 2014-05-01 RBS: Changed integer*4 to integer
c 2017-02-24 RBS: Fixed the lack of declaration of the passed vaiables
c                 in get_next_line
c 2017-02-24 RBS: Added the new passed variable currline_lc in
c                 get_next_line, declared it *120
c
c Define passed variables
c
      real       parms(*)
      integer    num_parms, eofflg, nlines, numerr
c
c Define internal variables
c
      character  currline*120, currline_lc*120
c
c Various parameters and variable types.
c
      integer    flag, max_fields
      integer    noerr,          error,     notext
      parameter (noerr      = 0, error = 1, notext = 0)
      parameter (max_fields    = 10)
      parameter (max_lsf_parms = 10)
      character  field(max_fields)*80
c
c Get the line.
c
      call get_next_line
     &     (currline, currline_lc, field, nlines, eofflg, max_fields)
      if (eofflg .eq. error) return
c
c Attempt to convert each field into a real number.
c
      flag = noerr
      i    = 0
      do while ((flag .eq. noerr) .and. (i .lt. max_lsf_parms))
        i = i + 1
        call text_to_real (field(i), parms(i), flag)
      end do
c
c If we failed to convert a number, then see if we ran out of fields.
c If so, then set the number of parameters entered, else signal an error.
c
      if (flag .eq. error) then
        call strip (field(i), istart, iend)
        if (istart .eq. notext) then
          num_parms = i - 1
        else
          call print_bad_line (currline, nlines, numerr)
        end if
c
c Did not fail to convert all numbers (the maximum amount were entered).
c So set the number of parameters entered.
c
      else
        num_parms = i
      end if
c
      return
      end

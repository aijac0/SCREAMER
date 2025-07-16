      subroutine text_to_real (text, rvalue, flag)
c
c ---------------------------------------------------------------------
c
c Convert text in E- OR F-format to its decimal equivalent, labeled
c RVALUE.  TEXT must be of the form:
c '     (sign)x---x.y----yE(signe)z---z      ',
c with no embedded blanks.  Where 'x,y,z' is a decimal digit.
c (sign) and (signe) are '+' (optional) or '-'.
c Other valid forms: (decimal point is optional)
c (sign)x---x.y---y
c (sign)x---x.E(signe)z---z
c (sign)x---x.
c (sign)x---x
c (sign)x---xE(signe)z---z
c (sign).y---yE(signe)z---z
c (sign).y---y
c (sign)E(signe)z---z   which is (sign)*10**((signe)*z---z)
c An illegal form gives RVALUE=0.0 and FLAG=1.
c FLAG=0 if form is allowed.
c TEXT has from 1 t0 80 characters.
c z---z must be < 28.
c -10**9 < x---x < 10**9.
c -10**9 < y---y < 10**9 and y---y must be less than 10 digits.
c
c ---------------------------------------------------------------------
c
c Modifications:
c   MLK, 04/28/95, Comment out all write(9,--) error messages
c  2014-02-06 RBS: Changed real*4 to real
c  2014-05-06 RBS: Changed integer*4 to integer
c
c ---------------------------------------------------------------------
c
c Define passed variables
c
      character  text*(*)
      real       rvalue
      integer    flag
c
c Define internal variables
c
      real*8     rvalue8, frac8, exp8, ten8
      integer    start, end
      integer    error, no_error, no_text
      integer    max_exp, max_digits
      character  decpt*1,    e*1,    plus*1,    minus*1,   t*1
c
c Initialize variables
c
      parameter (decpt='.',  e='E',  plus='+',  minus='-')
      parameter (ten8=10.0)
      parameter (error = 1, no_error = 0, no_text = 0)
      parameter (max_exp = 27, max_digits = 9)
c
c
c
      flag = no_error
c
c Strip blanks and return if all blanks
c
      call strip (text, start, end)
      if (start .eq. no_text) then
c          write(9,*)'Error 5'
        go to 999
      end if
c
c Set decimal point and 'E' position counters less than zero
c
      idecpt = -2
      iexp   = -1
c
c Look for decimal point
c
      i = start
      do while (( i .le. end) .and. (text(i:i) .ne. decpt))
        i = i + 1
      end do
      if (i .le. end) then
        idecpt = i
      end if
c
c Look for 'E' from exponent. Error if it is the last character.
c
      i = start
      do while ((i .le. end) .and. (text(i:i) .ne. e))
        i = i + 1
      end do
      if (i .lt. end) then
        iexp = i
      else if (i .eq. end) then
c          write(9,*)'Error 6'
        go to 999
      end if
c
c Make sure decimal point is left of 'E' if 'E' exists
c
      if ((iexp .gt. 0) .and. (idecpt .ge. iexp)) then
c          write(9,*)'Error 7'
        go to 999
      end if
c
c
c Evaluate exponent ---
c
c
      exp8 = 1.0
      if (iexp .gt. 0) then
c
c There is an exponent.
c Check to see if there are
c any blanks to the left or right in the exponent field.
c
        call strip (text((iexp+1):end), istart_exp, iend_exp)
        if ((end-iexp-1) .ne. (iend_exp-istart_exp)) then
c          write(9,*)'Error 8'
          go to 999
        end if
c
c Convert the exponent
c
        call conv_to_int (text((iexp+1):end), ivalue, isign, iflag)
        if ((iflag .eq. error) .or. (ivalue .gt. max_exp)) then
c          write(9,*)'Error 9'
          go to 999
        end if
        exp8 = ten8 ** (ivalue*isign)
c
      end if
c
c
c Evaluate fraction ---
c
c
      frac8 = 0.0
      if (idecpt .gt. 0) then
c
c There is a decimal point.
c
        t = text((idecpt+1):(idecpt+1))
        if (iexp .gt. 0) then
c
c There is also an exponent.
c
          if (idecpt .lt. (iexp-1)) then
c
c The fraction contains more than just a decimal point.
c check for + or - as the first fraction character.
c Check for leading or trailing blanks, then convert.
c
            if ((t .eq. plus) .or. (t .eq. minus)) then
c          write(9,*)'Error 10'
              go to 999
            end if
c
            call strip (text((idecpt+1):(iexp-1)), 
     &                  istart_frac, iend_frac)
            if ((iexp-idecpt-2) .ne. (iend_frac-istart_frac)) then
c          write(9,*)'Error 11'
              go to 999
            end if
c
            call conv_to_int (text((idecpt+1):(iexp-1)), 
     &                        ifrac, isign, iflag)
            if (iflag .eq. error) then
c          write(9,*)'Error 12'
              go to 999
            end if
c
            num_digits = (iexp - idecpt - 1)
            if (num_digits .gt. max_digits) then
c          write(9,*)'Error 13'
              go to 999
            end if
            frac8 = dfloat(ifrac) * (ten8 ** (-1*num_digits))
          end if
c
        else
c
c or there is no exponent.
c
          if (idecpt .lt. end) then
c
c There is more than just a decimal point. Check for leading
c + or -, leading or trailing blanks, then convert.
c
            if ((t .eq. plus) .or. (t .eq. minus)) then
c          write(9,*)'Error 14'
              go to 999
            end if
c
            call strip (text((idecpt+1):end),
     &                  istart_frac, iend_frac)
            if ((end-idecpt-1) .ne. (iend_frac-istart_frac)) then
c          write(9,*)'Error 15'
              go to 999
            end if
c
            call conv_to_int (text((idecpt+1):end), ifrac, isign,
     &                        iflag)
            if (iflag .eq. error) then
c          write(9,*)'Error 16'
              go to 999
            end if
c
            num_digits = end - idecpt
            if (num_digits .gt. max_digits) then
c          write(9,*)'Error 17'
              go to 999
            end if
            frac8 = dfloat(ifrac) * (ten8 ** (-1*num_digits))
          end if
c
        end if
c
      end if
c
c
c Evaluate integer part ---
c
c
      int = 0
      intsign = +1
      t = text(start:start)
      if (idecpt .gt. start) then
c
c There is a decimal point and an integer field.
c
        if (idecpt .gt. (start+1)) then
c
c The integer length is > 1. check for leading or trailing blanks
c and convert.
c
          idecptm=idecpt - 1
          call strip (text(start:idecptm),
     &                istart_int, iend_int)
          if ((idecpt-1-start) .ne. (iend_int-istart_int)) then
c          write(9,*)'Error 18'
            go to 999
          end if
c
          call conv_to_int (text(start:idecptm), int, intsign,
     &                      iflag)
          if (iflag .eq. error) then
c          write(9,*)'Error 19'
            go to 999
          end if
c
        else
c
c The integer length = 1. So check for sign only.
c
          if (t .eq. plus) then
            intsign = +1
            int = 0
          else if (t .eq. minus) then
            intsign = -1
            int = 0
          else 
            call conv_to_int (t, int, intsign, iflag)
            if  (iflag .eq. error) then
c          write(9,*)'Error 20'
              go to 999
            end if
          end if
c
        end if
c
      else if (idecpt .eq. start) then
c
c Number starts with a decimal point.
c
        int = 0
        intsign = +1
c
      else if (iexp .gt. start) then
c
c No decimal point, but there is an exponent and an integer length.
c
        if (iexp .gt. (start+1)) then
c
c Also, the integer length is > 1.
c Check for leading or trailing blanks and convert.
c
          call strip (text(start:(iexp-1)), istart_int, iend_int)
          if ((iexp-1-start) .ne. (iend_int-istart_int)) then
c          write(9,*)'Error 1'
            go to 999
          end if
c
          call conv_to_int (text(start:(iexp-1)), int, intsign, iflag)
          if (iflag .eq. error) then
c          write(9,*)'Error 2'
            go to 999
          end if
c
        else
c
c or, the integer length = 1 so check for sign only.
c
          if (t .eq. plus) then
            intsign = +1
            int = 1
          else if (t .eq. minus) then
            intsign = -1
            int = 1
          else 
            call conv_to_int (t, int, intsign, iflag)
            if (iflag .eq. error) then
c          write(9,*)'Error 3'
              go to 999
            end if
          end if
c
        end if
c
      else if (iexp .eq. start) then
c
c There is only an exponent, so integer part = 1.
c
        int = 1
        intsign = +1
c
      else
c
c Otherwise there is only an integer part. No exp or dpoint.
c Leading and trailing blanks have already been stripped.
c
        call conv_to_int (text(start:end), int, intsign, iflag)
        if (iflag .eq. error) then
c          write(9,*)'Error 4'
          go to 999
        end if
c
      end if
c
c Find RVALUE
c
      rvalue8 = (dfloat(int) + frac8) * exp8 
      if (intsign .lt. 0) then
        rvalue = -sngl (rvalue8)
      else
        rvalue = sngl (rvalue8)
      end if
      return
c
c Error
c
  999 continue
      rvalue = 0.0
      flag = error
c
      return
      end

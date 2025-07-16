      subroutine conv_to_int (text, intmag, intsign, flag)
c
c Define passed variables
c
      character  text*(*)
      integer    intmag, intsign, flag
c
c Define internal variables
c
      parameter (limit = 80)
      dimension  int_text(limit)
      integer    start, end, char_value
c
      integer    error
      parameter (error = 1, no_error = 0)
      integer    plus, minus
      parameter (plus = -5, minus = -3)
      integer    ascii_factor
      parameter (ascii_factor = 48)
      parameter (max_digits = 9)
c
c Convert TEXT (up to 80 characters) to its integer equivalent,
c labeled INTMAG.
c INTMAG is always >= 0, INTSIGN is its sign (+1 or -1).
c Routine only recognizes integers in the form
c '(sign)xxxxxx' with no blanks.
c x is a decimal digit and sign is
c '+' (optional) or '-'.  Anything else causes INTMAG=0.
c If TEXT was a legal form, FLAG=0; otherwise, FLAG=1.
c TEXT has from 1 to 80 characters.
c Integers must be in the range -10**9 < I < 10**9 , otherwise an
c error is flagged.
c
      flag = no_error
      start = 1
      end = len(text)
      if (end .gt. limit) then
        go to 999
      end if
c
c Fill ITEXT with ASCII decimal values - 48 of TEXT's characters.
c 'X'=X for a digit X. Return if not in above form.
c
c Check first character for sign or digit or something else.
c
      char_value = ichar (text(start:start))  -  ascii_factor
      int_text(start) = char_value
      if  (char_value .eq. plus) then
        if (start .ge. end) then
          go to 999
        else
          intsign = +1
          start = start + 1
        end if
      else if  (char_value .eq. minus) then
        if  (start .ge. end) then
          go to 999
        else
          intsign = -1
          start = start + 1
        end if
      else if ((char_value .ge. 0) .and. (char_value .le. 9)) then
        intsign = +1
      else
        go to 999
      end if
c
c Check characters to see if they are digits
c
      do i = start, end
        char_value = ichar (text(i:i))  -  ascii_factor
        if ((char_value .lt. 0) .or. (char_value .gt. 9)) then
          go to 999
        else
          int_text(i) = char_value
        end if
      end do
c
c Add the digits, multiplied by the appropriate power of 10,
c using nested multiplication.
c
c First digit.
c
      intmag = int_text(start)
      num_digits = end - start + 1
      if ((num_digits .gt. max_digits) .and.
     &    (intmag .gt. 0))             then
        go to 999
      end if
c
c Rest of the digits.
c
      do i = start + 1, end
        num_digits = num_digits - 1
        if ((num_digits .gt. max_digits) .and.
     &      (int_text(i) .gt. 0))        then
          go to 999
        else
          intmag = 10*intmag +  int_text(i)
        end if
      end do
c
      return
c
c Error.
c
  999 continue
      intmag = 0
      flag = error
c
      return
      end

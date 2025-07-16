      subroutine text_to_int (text, int, flag)
c
c Uses CONV_TO_INT to find the integer equivalent of TEXT.
c This just performs the multiplication:
c INT = INT*INTSIGN.
c Also, it strips off outer blanks.
c If there was an error, INT=0 and FLAG=1.
c Otherwise, FLAG=0 if everything is ok.
c
c Passed variables
c
      character  text*(*)
      integer    int, flag
c
c Define internal variables
c
      integer  start, end, intsign, intmag
      integer    error,     no_error,     no_text
      parameter (error = 1, no_error = 0, no_text = 0)
c
c Strip blanks and return if all blanks, setting FLAG=ERROR.
c
      call strip (text, start, end)
      if  (start .eq. no_text) then
        int = 0
        flag = error
        return
      end if
c
c Convert to integer.
c
      call conv_to_int (text(start:end), intmag, intsign, flag)
      if (flag .eq. error) then
        int = 0
      else 
        int = intsign * intmag
      end if
c
      return
      end

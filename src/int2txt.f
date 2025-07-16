      subroutine int_to_text (int ,text)
c
c     Converts an integer to its character representation (up to
c       10 digits). If the character string is longer than 
c       required, it will be padded with blanks on the right.
c
c Define passed variables
c
      integer       int
      character*(*) text
c
c Define internal variables
c
      integer       num, ichar_cnt, idig, icode
      logical       leading_digit
c
c  initialize variables
c
      text = ' '
      ichar_cnt = 0
      num = int
      leading_digit = .true.
c
c  Loop 10 times, once for each possible digit
c
      do i = 9, 0, -1
         idig = num / (10**i)
c
c        If this is the first significant digit, or if this is
c          a zero embedded within the number, convert it to
c          a character and append it to the character string
c
         if ((idig .ne. 0) .or. (.not.leading_digit)) then
            ichar_cnt = ichar_cnt + 1
            icode = idig + 48
            text(ichar_cnt:ichar_cnt) = char(icode)
            num = num - (idig*10**i)
            leading_digit = .false.
         endif
      enddo
c
      return
      end

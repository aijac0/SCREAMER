      subroutine conv_to_ucase (text)
c
c Converts all lowercase letters in TEXT to uppercase.
c
c
c Modifications:
c 2014-05-01 RBS: integer*4 to integer
c
c Define passed variables
c
      character  text*(*)
c
c Define internal variables
c
      integer    a, z, ascii_factor, char_value
      parameter (a = 97, z = 122, ascii_factor = 32)
c
c
c
      do i = 1, len (text)
        char_value = ichar (text(i:i))
        if (( char_value .ge. a) .and. (char_value .le. z)) then
          text(i:i) = char (char_value - ascii_factor)
        end if
      end do
c
      return
      end      

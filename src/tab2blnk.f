       subroutine tab2blnk (text)
c
c Converts all tab characters in text to blanks
c
c Define passed variables
c
      character  text*(*)
c
c Define internal variables
c
      character*1  tab, ablank
      parameter (tab = char(9), ablank = char(0))
c      parameter (tab = '\t',    ablank = ' ')
c
      do i = 1, len (text)
         if (text(i:i) .eq. tab) then
            text(i:i) = ablank
           end if
        end do
c
       return
       end      

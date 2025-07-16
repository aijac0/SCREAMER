      subroutine gather (inarray, intarget, maxin, outarray, numout)
c
c-------Description-----------------------------------------------------
c
c  Author/Date : Kelley Fugelso, 1265 (SEA)    01/90
c
c  Purpose     : This subroutine "gathers" all elements from INARRAY
c                which match the value of INTARGET. The indices of
c                the matched elements are stored in array OUTARRAY.
c
c  Called by   : Subroutine FILVALS, Subroutine IDRVALS,
c                Subroutine PLTVALS, Subroutine PRTVALS,
c                Subroutine TABVLAS, Subroutine UFOVALS
c
c  Calls       : None
c
c-------Include Files---------------------------------------------------
c
c     NONE
c
c-------Input Parameters------------------------------------------------
c
      integer maxin, inarray(maxin), intarget
c
c-------Output Parameters-----------------------------------------------
c
      integer outarray(maxin), numout
c
c Define internal variables
c
c     NONE
c
c-------Subroutine Body-------------------------------------------------
c      
      numout = 0
      do i = 1, maxin
         if (inarray(i) .eq. intarget) then
            numout = numout + 1
            outarray(numout) = i
         endif
      enddo
c
c-------End of Subroutine-----------------------------------------------
c
      return
      end      

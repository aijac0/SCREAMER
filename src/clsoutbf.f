      subroutine clear_outbuf
c
c-------Description-----------------------------------------------------
c
c  Source File : clsoutbf.f
c
c  Author/Date : Kelley Fugelso, 1265 (SEA)    01/90
c
c  Purpose     : This subroutine the three arrays used to process
c                output requests.
c
c  Called by   : subroutine csvvals, subroutine filvals,
c                subroutine pffvals, subroutine sfcvals,
c                subroutine tabvals, subroutine ufovals
c
c  Calls       : None
c
c-------Include Files---------------------------------------------------
c
      use zdemmax
      include 'zdemout.h'
c
c-------Input Parameters------------------------------------------------
c
c     NONE
c
c-------Output Parameters-----------------------------------------------
c
c     NONE
c
c-------Subroutine Body-------------------------------------------------
c
c  Clear the INDICES array and the OUTDATA array
c
      do i = 1, maxout
         indices(i) = 0
         do j = 1, max_plot_points
            outdata(j,i) = 0.0
         enddo
      enddo
c
c  Clear the TIMEOUT array
c
      do i = 1, max_plot_points
         timeout(i,1) = 0.0
         timeout(i,2) = 0.0
      enddo
c
c-------End of Subroutine-----------------------------------------------
c
      return
      end

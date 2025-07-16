      subroutine set_current (time, index, current)
c
c  Change log
c
c  2014-02-06 RBS: Changed real*4 to real
c
c current source as a function of time.
c
c
c Include files
c
      use zdemmax
      include 'zdemparm.h'
      include 'zdemcomm.h'
      include 'zdemwork.h'
c
c Define passed variables
c
      real       time, current
      integer    index
c
c Sort the functions describing this source.
c
      ifunction = icurrf(index)
      if (ifunction .eq. sinsquared) then
        current = fsinsquared (time, currf_parms(1,index),
     &                               currf_parms(2,index),
     &                               currf_parms(3,index))
      else if (ifunction .eq. sinfun) then
        current = fsinfun (time, currf_parms(1,index),
     &                           currf_parms(2,index),
     &                           currf_parms(3,index))
      else if (ifunction .eq. leastsquares) then
        current = fleastsquares (time, num_currf_parms(index),
     &                           currf_parms(1,index))
      else if (ifunction .eq. piecewiselinear) then
        current = fpiecewiselinear (time, num_currf_parms(index),
     &               currf_parms(1,index), lastcurrf_time(index))
      else if (ifunction .eq. table) then
        scale   = currf_parms(1,index)
        delay   = currf_parms(2,index)
        num_tpoints = num_currf_parms(index) - 2
        current = fpiecewiselinear (time-delay, num_tpoints,
     &               currf_parms(3,index), lastcurrf_time(index))
        current = current * scale
      end if
c
      return
      end

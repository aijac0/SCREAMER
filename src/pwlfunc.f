      function  fpiecewiselinear (time, npts, pts, ltime)
c
c Define passed variables
c
      integer   npts, ltime
      real      time, pts(npts)
c
c Define internal variables
c
      real almost0, ralmost0, time1, tdel, slope
      parameter (almost0  = 1.0e-12)
      parameter (ralmost0 = 1.0 / almost0)
c
c Change log
c
c 2014-02-06 RBS: Changed real*4 to real
c 2014-05-02 RBS: Changed integer*4 to integer
c 2015-06-23 RBS: Removed unused variable time2tdiff
c
c Interpolates with a piecewise linear function using time as the independent
c variable.  pts(i) i=1,3,5,... are the independent variable parameters
c and pts(j) j=2,4,6,... are the dependent variable parameters.
c npts is at least 4 and is an even integer.
c pts(1) < pts(3) < pts(5) ...  .
c ltime is an integer that points to the last independent variable in
c points which the previous time was less than (speeds the lookup)
c (note that we are assuming that each time this is called for a particular
c circuit element section, the time has increased).
c Set ltime=0 if this is the first call.
c
c Find the endpoints for the independent variable.
c
      if ((time .lt. pts(1)) .or. (time .gt. pts(npts-1))) then
        fpiecewiselinear = 0.0
        ltime = 0
        return
      end if
c
c Use lasttime to set the looping.
c
      if (ltime .lt. 3) then
        i   = 3
      else
        i   = ltime
      end if
      time1 = pts(i-2)
      time2 = pts(i)
c
c Set the index to the point in the table which brackets the current time.
c
      do while ((time .lt. time1) .or. (time .gt. time2))
        i     = i + 2
        time1 = time2
        time2 = pts(i)
      end do
c
c Set the time difference of the two table points and set the time incremented
c since the first table time point.
c Then interpolate to find the function.
c
      tdiff = amax1 ((time2-time1), almost0)
      tdel  = time  - time1
      slope = (pts(i+1) - pts(i-1)) / tdiff
      fpiecewiselinear = pts(i-1) + tdel*slope
c
      ltime = i
c
      return
      end

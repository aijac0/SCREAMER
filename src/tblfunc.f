      function  ftabcond (time, cond, npts, pts, ltime)
c
c  Change log
c
c  2014-02-06 RBS: Changed real*4 to real
c
c Interpolates with a piecewise linear function using time as the
c independent variable.  pts(i) i=1,4,7,... are the independent variable
c parameters (time) and pts(j) j=2,5,8,... are the dependent variable
c parameters f(time).
c
c Also, this checks against a function (piece-wise linear) fcond(time)
c residing in pts(k), k=3,6,9,... such that
c ftabcond = f(time) if cond .ge. fcond(time)
c          = 0       if cond .lt. fcond(time)
c npts is at least 6 and is an even integer.
c pts(1) < pts(4) < pts(7) ...  .
c ltime is an integer that points to the last independent variable in
c pts which the previous time was less than (speeds the lookup).
c (note that we are assuming that each time this is called for a particular
c circuit element section, the time has increased).
c Set ltime=0 if this is the first call.
c
c Find the endpoints for the independent variable.
c Set value to zero if outside of independent variable range given.
c
c Define passed variables
c
      integer   npts, ltime
      real      time, cond, pts(npts)
c
c Define internal variables
c
      real ftabcond, time1, time2, tdiff, tdel, slope, fcond
c
      real       almost0, ralmost0
      parameter (almost0  = 1.0e-12)
      parameter (ralmost0 = 1.0 / almost0)
c
c
c
      if ((time .lt. pts(1)) .or. (time .gt. pts(npts-2))) then
        ftabcond = 0.0
        ltime    = 0
        return
      end if
c
c Use ltime to set the looping.
c And find the two time points which bracket the specified time.
c
      if (ltime .lt. 4) then
        i = 4
      else
        i = ltime
      end if
      time1 = pts(i-3)
      time2 = pts(i)
c
      do while ((time .lt. time1) .or. (time .gt. time2))
        i     = i + 3
        time1 = time2
        time2 = pts(i)
      end do
c
c Find the conditional value to check against the specified cond
c   First find the inverse table time difference,
c   then the time increment from the first table time point.
c
      tdiff = amax1 ((time2-time1), almost0)
      tdiff = 1.0 / tdiff
      tdel  = time - time1
c
      slope = (pts(i+2) - pts(i-1)) * tdiff
      fcond = pts(i-1) + tdel*slope
c
c If satisfy condition, find f(time), else set it to zero.
c
      if (cond .ge. fcond) then
        slope    = (pts(i+1) - pts(i-2)) * tdiff
        ftabcond = pts(i-2) + tdel*slope
      else
        ftabcond = 0.0
      end if
c
      ltime = i
c
      return
      end

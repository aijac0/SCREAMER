      function  calc_resistance (time, npts, pts, ltime)
c
c-------Description-----------------------------------------------------
c
c Source File: calcres.f
c
c Author/Date:  Kelley Fugelso, 1265 (SEA)   04/89
c
c 2014-02-06 RBS: Changed real*4 to real
c 2014-03-24 RBS: Made it explicit that we generate variable L as well
c 2014-05-06 RBS: Defined all internal variables explicitly
c
c Purpose: This subroutine determines the resistance (inductance) by
c          interpolating using time as the indepdent variable.
c          pts(i) i=1,3,5,... are the independent variable parameters
c          and pts(i) i=2,4,6,... are the dependent variable parameters.
c          ltime points to the last independent variable in pts which
c          the previous time was less than (speeds up the lookup).
c
c Called by: Subroutine GET_TABLEM_VALUE
c
c Calls:  none
c
c-------Define passed variables-----------------------------------------
c
      integer   npts,     ! Number of points in array pts             */
     +          ltime     ! Pointer to last time used in lookup       */
      real      time,     ! Current problem time                      */
     +          pts(npts) ! Array of independent & dependent vals     */
c
c-------Define internal variables---------------------------------------
c
      real almost0, ralmost0, slope, time1, time2, tdiff, tdel
c
c-------Constants-------------------------------------------------------
c
      parameter (almost0  = 1.0e-12)
      parameter (ralmost0 = 1.0 / almost0)
c
c-------Function Body---------------------------------------------------
c
c Find the endpoints for the independent variable.
c
      if ((time .lt. pts(1)) .or. (time .gt. pts(npts-1))) then
        calc_resistance = 0.0
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
c Set the index to the point in the table that brackets the current time
c
      do while ((time .lt. time1) .or. (time .gt. time2))
         i     = i + 2
         time1 = time2
         time2 = pts(i)
       end do
c
c Set the time difference of the two table points and set the time
c incremented since the first table time point.
c Then interpolate to find the function.
c
      tdiff = amax1 ((time2-time1), almost0)
      tdel  = time  - time1
      slope = (pts(i+1) - pts(i-1)) / tdiff
      calc_resistance = pts(i-1) + tdel*slope
c
      ltime = i
c
c-------Function End----------------------------------------------------

      return
      end

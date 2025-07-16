      function fsinsquared (time, amplitude, halfperiod, delay)
c
c  Change log
c
c  2014-02-06 RBS: Changed real*4 to real
c  2014-05-05 RBS: Define pi, alpha, sin_alpha as real
c
c Calulates the function:
c                               2
c    f(time) = amplitude * ( sin (alpha) ) ,
c       where:  alpha = ((time-delay)/halfperiod)*pi  .
c
c    Note: f is zero if time < delay and if time > delay + halfperiod
c
c Define passed variables
c
      real       time, amplitude, halfperiod, delay
c
c Define internal variables
c
      real       pi, deltime, alpha, sin_alpha
      parameter (pi = 3.1415927)
c
c
c
      deltime = time - delay
      if ((deltime .ge. 0.0) .and. (deltime .le. halfperiod)) then
        alpha       = (deltime/halfperiod) * pi
        sin_alpha   = sin (alpha)
        fsinsquared = amplitude * sin_alpha * sin_alpha
      else
        fsinsquared = 0.0
      end if
c
      return
      end

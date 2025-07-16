      function   fsinfun (time, amplitude, period, delay)
c
c  Changed log
c
c  2014-02-06 RBS: Changed real*4 to real
c
c Calulates the function:
c
c    f(time) = amplitude * ( sin (alpha) ) ,
c       where:  alpha = ((time-delay)/period)*twopi  .
c
c Define passed variables
c
      real       time, amplitude, period, delay
c
c Define internal variables
c
      real       alpha, pi, twopi
      parameter (pi    = 3.1415927)
      parameter (twopi = 2.0*pi)
c
c
c
      alpha   = ( (time-delay) / period )  *  twopi
      fsinfun = amplitude * sin (alpha)
c
      return
      end

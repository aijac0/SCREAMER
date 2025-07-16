      subroutine sexp_model (time, var_parms, resistance)
c
c  December 10, 1992;     hnw
c  2014-02-06 RBS: Change real*4 to real
c
c     Calculates the value for the resistance of a  variable resistor
c     at time (the half time step).  The model used here is the
c     exponential model of a variable resistive switch.
c
c     r = ropen       time < timsw
c
c     r = rclose +    zsw*exp(-a)/(1-exp(-a)+zsw*1.0e-6)    time > timsw
c
c       where a = (time-timsw)/tau    (rtau = 1/tau)
c
c Define passed variables
c
      real  time, var_parms(*), resistance
c
c Define internal variables
c
      real ropen, timsw, rclose, rtau, zsw, argument, expa
c
c
c
      ropen = var_parms(1)
      timsw = var_parms(3)
CMB      write(*,*) 'time=', time, ' var_parms=', var_parms(1), 
CMB     &' resistance=', resistance
     
c
c Set the resistance.
c
      if (time .le. timsw) then
        resistance = ropen
      else
        rclose     = var_parms(2)
        rtau       = var_parms(4)
        zsw        = var_parms(5)
        argument   = (time - timsw) * rtau
        if (argument .ge. 50.0) then
          expa     = 0.0
        else
          expa     = exp (-1.0 * argument)
        endif
        resistance = zsw*expa / (1.0 - expa + zsw*1.0e-6)  +  rclose
      end if
c
      return
      end

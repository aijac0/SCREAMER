      subroutine sdecay_model (time, var_parms, resistance)
c
c-----Description-------------------------------------------------------
c
c  File: sdecay_model.for
c
c Author/Date: Kelley Fugelso, 1265 (SEA)  05/89
c  December 10, 1992;     hnw
c  2014-02-06 RBS: Changed real*4 to real
c  2014-04-11 RBS: Added argument to the local variables real definition
c
c Purpose: Calculates the value for the resistance of a variable 
c          resistor at the current time (half-step). The model used
c          is the decay model of the variable resistive switch
c
c     r = ropen       time <= timsw
c
c     r = rclose +    (ropen - rclose) * exp(-a)  time > timsw
c
c       where a = (time-timsw)/tau    (rtau = 1/tau)
c
c Called by: Program ZDEM
c
c Calls: None
c
c-----Define passed variables-------------------------------------------
c 
      real   time, var_parms(*), resistance
c
c-----Define Local Variables--------------------------------------------
c
      real   ropen, rclose, timsw, rtau, expa, argument
c
c-----Subroutine Body---------------------------------------------------

      ropen = var_parms(1)
      timsw = var_parms(3)
c
c Set the resistance.
c
      if (time .le. timsw) then
        resistance = ropen
      else
        rclose     = var_parms(2)
        rtau       = var_parms(4)
        argument   = (time-timsw) * rtau
        if (argument .ge. 50.0) then
          expa     = 0.0
        else
          expa     = exp (-1.0 * argument)
        end if
        resistance = rclose + ( (ropen - rclose) * expa )
      end if
c
c-----Return to Program ZDEM--------------------------------------------
c
      return
      end

      subroutine srise_model (time, var_parms, resistance)
c
c-----Description-------------------------------------------------------
c
c Author/Date: Kelley Fugelso, 1265 (SEA)  05/89
c
c  December 10, 1992;     hnw
c  2014-02-06 RBS: Changed real*4 to real
c
c Purpose: Calculates the value for the resistance of a variable 
c          resistor at the current time (half-step). The model used
c          is the rise model of the variable resistive switch
c
c     r = rclose      time <= timsw
c
c     r = rclose +    (ropen - rclose) * (1 - exp(-a))  time > timsw
c
c       where a = (time-timsw)/tau    (rtau = 1/tau)
c
c Called by: Program ZDEM
c
c Calls: None
c
c-----Input Parameters--------------------------------------------------
c 
      real   time, var_parms(*), resistance
c
c-----Local Variables---------------------------------------------------
c
      real   ropen, timsw, rclose, rtau, expa
c
c-----Subroutine Body---------------------------------------------------

      rclose = var_parms(2)
      timsw = var_parms(3)
c
c Set the resistance.
c
      if (time .le. timsw) then
        resistance = rclose
      else
        ropen      = var_parms(1)
        rtau       = var_parms(4)
        argument   = (time - timsw) * rtau
        if (argument .ge. 50.0) then
          expa     = 0.0
        else
          expa     = exp (-1.0 * argument)
        endif
        resistance = rclose + ( (ropen - rclose) * (1-expa) )
      end if
c
c-----Return to Program ZDEM--------------------------------------------
c
      return
      end

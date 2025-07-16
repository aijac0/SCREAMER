      subroutine sps2_model (timestep, current, voltage, parms, gvar)
c
c  December 10, 1992;     hnw
c 2014-02-06 RBS: Changed real*4 to real
c 2014-05-07 RBS: Defined all reals explictly
c
c Calculates value of variable shunt conductance based on a PEOS model
c from a SCEPTRE model of DHM and LXS.
c   Switches by charge pointer.
c   parms is an array containing parameters of the specific switch,
c   gvar is the returned conductance.
c   voltage is the voltage across the switch,
c   current is the current flowing into the switch.
c
c Define passed variables
c
      real timestep, current, voltage, parms(*), gvar
c
      real     ig, vg
      real qswitch, constant, gmin, gmax, charge, vold, chargesw, vnew
c
      qswitch     = parms(1)
      constant    = parms(2)
      gmin        = parms(3)
      gmax        = parms(4)
      charge      = parms(5)
      vold        = parms(6)
      chargesw    = parms(7)
      vnew        = voltage
      parms(6)    = vnew
c
c Accumulate charge and see if we have switched.
c
      ig = abs (current)
      chargesw = chargesw + ig*timestep
      parms(7) = chargesw
c
c If already switched, then integrate the current to get the charge to
c find the conductance.
c
      if (chargesw .gt. qswitch) then
        charge   = charge + ig*timestep
        parms(5) = charge
      end if
c
c Now find the conductance and limit if necessary.
c
      if (charge .lt. 1.0e-10) then
        gvar = gmax
      else
c
c Use the voltage at the previous half time step for stability.
c
        vg = 0.5  *  abs (vnew + vold)
        gvar = (constant * sqrt (vg)) / (charge*charge)
        if (gvar .lt. gmin) then
          gvar = gmin
        else if (gvar .gt. gmax) then
          gvar = gmax
        end if
      end if
c
      return
      end

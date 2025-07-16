      subroutine sps1_model (timestep, timehalf, current, voltage,
     &       parms, gvar)
c
c  December 10, 1992;     hnw
c 2014-02-06 RBS: Changed real*4 to real
c
c Calculates value of variable shunt conductance based on a PEOS model
c from a SCEPTRE model of DHM and LXS.
c   Switches by time pointer.
c   parms is an array containing parameters of the specific switch,
c   gvar is the returned conductance.
c   voltage is the voltage across the switch,
c   current is the current flowing into the switch.
c
c Define passed variables
c
      real timestep, timehalf, current, voltage, parms(*), gvar
c
c Define internal variables
c
      real ig, vg
      real tswitch, constant, gmin, gmax, charge, vold, vnew
c
      tswitch     = parms(1)
      constant    = parms(2)
      gmin        = parms(3)
      gmax        = parms(4)
      charge      = parms(5)
      vold        = parms(6)
      vnew        = voltage
      parms(6)    = vnew
c
c If already switched, then integrate the current to get the charge to
c find the conductance.
c
      if (timehalf .gt. tswitch) then
        ig = abs (current)
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

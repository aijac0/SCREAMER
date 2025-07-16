      subroutine dyhohlraumparm
c read and write arguments
     & (parms, nparms)
c
c Sets up the parameters needed for the dynamic hohlraum implosion model
c and returns them in parms(i).
c nparms is the number of parameters in parms.
c
c parms is input with the basic parameters needed to rearrange and fill
c the actual parms array.
c
c ------------------------------------------------------------------------------
c
c Modifications:
c  2014-10-11 RBS: created from a copy of gas puff parm
c  2014-10-11 RBS: Dimension vector temp to 10, # inputs, defined the
c                  number of inputs needed for the block
c  2014-10-23 RBS: Added implosion flags to parameter definition
c  2014-10-23 RBS: Comment block fully updated
c  2014-10-23 RBS: Added peak current init, nparms to 24
c
c ------------------------------------------------------------------------------
c
c Define passed variables
c
      real       parms(*)
      integer    nparms
c
c Include the common block for plotting so that we can set some
c initial plotting values.
c
      include   'zdemmax.h'
      include   'zdemout.h'
c
c Define internal variables
c
      parameter (ninputs = 10)
      real       temp(ninputs)
      real       mu
      parameter (mu = 2.0e-7)
      real       pi
      parameter (pi = 3.14159)
c
c Fill temp with original values from parms, then rearrange parms and
c fill it in with the information needed for dyhohlmdl.f.
c
      do i = 1, ninputs
        temp(i) = parms(i)
      end do
c      print '(10F11.8)', (temp(i), i=1,10)
c
c In output parms we want:
c  1: initrad       (does not change)
c       = initial radius of outer liner
c  2: rinner        (does not change)
c       = foam inner radius at which point we turn off drag term
c  3: rmin          (does not change)
c       = minimum allowed radius of stagnation
c  4: massri        (does not change)
c       = total foam mass when radius=innerrad
c  5: masst3
c       = incremented foam mass for drag term from last half time step
c       = starts with the initial liner mass
c  6: density       (does not change)
c       = foam density
c  7: aconst        (does not change)
c       = -(mu * length) / 2
c  8: dconst        (does not change)
c       = 2 * pi * length * density 
c  9: lconst        (does not change)
c       = mu * length
c 10: mconst        (does not change)
c       = pi * length * density
c 11: lminrad       (does not change)
c       = lconst * ln(initrad/minrad)
c 12: lt3           (initially = 0)
c       = inductance from last half time step
c 13: radt3         (initially = initrad)
c       = radius from last half time step
c 14: velt3         (initially = 0)
c       = velocity from last half time step
c 15: testimpl      (initially = 0, which is imploding, set to 1 when
c                    radius shrinks to minrad)
c       = value to test to see if foil is still imploding or has stagnated at
c         the minimum radius
c 16: mliner1     (does not change)
c       = initial mass term
c 17: rliner2     (does not change)
c       = second liner mass
c 18: mliner2     (does not change)
c       = second liner mass
c 19: router      (does not change)
c       = third liner radius and foam outer radius
c 20: mliner3     (does not change)
c       = third liner mass
c 21: flagl2
c       = changes when implosion radius equals second liner radius
c 22: flagl3
c       = changes when implosion radius equals third liner radius
c 23: flag3m
c       = changes when imploding mass accretes third liner
c 24: peakcur
c       = init peakcur
c
c In temp we have the dyhohl input parameters:
c 1: length
c 2: rliner1
c 3: mliner1
c 4: rliner2
c 5. mliner2
c 6. router
c 7. mliner3
c 8. density
c 9. rinner
c 10. rmin
c
      nparms    = 24
      parms(1)  = temp(2)
      parms(2)  = temp(9)
      parms(3)  = temp(10)
      parms(4)  = pi * temp(1) * temp(8)
     &               * (temp(6)*temp(6) - temp(9)*temp(9))
      parms(5)  = temp(3)
      parms(6)  = temp(8)
      parms(7)  = - (mu * temp(1) * 0.5)
      parms(8)  = 2.0 * pi * temp(1) * temp(8)
      parms(9)  = mu * temp(1)
      parms(10) = pi * temp(1) * temp(8)
      parms(11) = mu * temp(1) * log(temp(2)/temp(10))
      parms(12) = 0.0      !lt3=initial inductance
      parms(13) = temp(2)  !radt3=initrad
      parms(14) = 0.0      !initial velocity
      parms(15) = 0.0      !set status as "imploding"
      parms(16) = temp(3)
      parms(17) = temp(4)
      parms(18) = temp(5)
      parms(19) = temp(6)
      parms(20) = temp(7)
      parms(21) = 0.0      !implosion flags
      parms(22) = 0.0      !implosion flags
      parms(23) = 0.0      !implosion flags
      parms(24) = 0.0      !init peakcur
c      print '(10F11.7)', (parms(i), i=1,10)
c      print '(10F11.7)', (parms(i), i=11,20)
c      print '(3F11.7)',  (parms(i), i=21,23)
c
c Set initial plotting values for GAS PUFF radius, velocity,
c acceleration, kinetic energy.
c
      gasrad   = parms(1)
      gasvel   = 0.0
      gasacc   = 0.0
      gaske    = 0.0
c
      return
      end

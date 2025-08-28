      subroutine dpfparm
c read and write parameters
     & (parms, nparms)
c
c Sets up the parameters needed for the DPF implosion model and
c returns them in the parms vector. This will be loaded in the nvarl
c column of var_model.
c nparms is passed back with the new the number of parameters in parms.
c
c  2016-04-01 RBS: Created from gaspfprm.f
c  2016-07-01 RBS: Included the sheath angle in the inputs
c
c Include the common block for plotting so that we can set some
c initial plotting values.
c
      use zdemmax
      use zdemout
c
c Declare the passed variables
c
      real       parms(*)
      integer    nparms
c
c Define internal variables, dimension to 16 for dpf
c
      real parmsin(nparms)
c
c Note carefully the value of mu
c
      real       mu
      parameter (mu = 2.0e-7)
      real       pi
      parameter (pi = 3.14159)
c
c Fill parmsin with original values from parms. We now have a dummy
c array that never changes herein and can be used to generate new values
c of parms that will be passed back to rdscrelem.f
c
      do i = 1, nparms
        parmsin(i) = parms(i)
      end do
c
c      print '(10F11.8)', (parmsin(i), i=1,nparms)
c
c In the parmsin array we have the following values:
c 1: router
c 2: rinner
c 3: zlen
c 4: density
c 5. rimass
c 6. rmin
c 7. initmass
c 8. theta
c
c
c The parms array is used in the model in every time step we have:
c  1: outerrad       (does not change)
c       = DPF outer radius cathode
c  2: innerrad       (does not change)
c       = DPF radius of the anode
c  3: zlenmax        (does not change)
c       = length of the anode
c  4: density        (does not change)
c       = DPF fill density
c  5: rimass         (does not change)
c       = initial radial sheath mass fraction
c  6: minrad         (does not change)
c       = minimum DPF radius - radius of stagnation
c  7: initmass       (does not change)
c       = initial sheath mass
c  8: theta          (does not change)
c       = fixed angle of the sheath
c
c  9: azconst        (does not change) NOT negative like radial
c       = (mu * ln(outerrad/innerrad)) / 2
c  10: dzconst        (does not change) Negative not positive like radial
c       = -pi * density * (outerrad**2 - innerrad**2)
c  11: lzconst        (does not change)
c       = mu * ln(outerrad/innerrad)
c  12: mzconst        (does not change)
c       = pi * density * (outerrad**2 - innerrad**2)
c  13: masszt3        (initial value is ximass)
c       = DPF mass for accel and drag terms from last half time step
c  14: zlent3         (initially = 0)
c       = axial position from last half time step
c  15: velzt3         (initially = 0)
c       = axial velocity from last half time step
c  16: masszlen       (initially 0)
c       = mass in the axial sheath when z = zlenmax
c  17: arconst        (does not change)
c       = -(mu * length) / 2
c  18: drconst        (does not change)
c       = 2 * pi * length * density
c  19: lrconst        (does not change)
c       = mu * length
c  20: mrconst        (does not change)
c       = pi * length * density
c  21: lrmin        (does not change)
c       = lconst * ln(initrad/minrad)
c  22: massrt3        (initial value is zero)
c       = mass in radial sheath for accel and drag terms from last 1/2 time step
c  23: radt3         (initially = initrad)
c       = radius from last half time step
c  24: velrt3         (initially = 0)
c       = velocity from last half time step
c  25: lt3            (initially = 0)
c       = total inductance from last half time step
c  26: testimpl       (initially = 0, which is imploding, set to 1 when radius
c                                   shrinks to minrad)
c       = value to test to see if foil is still imploding or has stagnated at
c         the minimum radius
c
      nparms    = 26
      parms(1)  = parmsin(1)   !router
      parms(2)  = parmsin(2)   !rinner
      parms(3)  = parmsin(3)   !anode length
      parms(4)  = parmsin(4)   !gas density
      parms(5)  = parmsin(5)   !fractional mass of radial sheath to axial
      parms(6)  = parmsin(6)   !minimum implosion radius
      parms(7)  = parmsin(7)   !initial mass
      parms(8)  = parmsin(8)   !sheath angle

      parms(9)  = ( mu * .5 * log(parmsin(1)/parmsin(2)) )
      parms(10) = -pi * parmsin(4) * (parmsin(1)**2 - parmsin(2)**2)
      parms(11) = mu * log(parmsin(1)/parmsin(2))
      parms(12) = pi * parmsin(4) * (parmsin(1)**2 - parmsin(2)**2)
      parms(13) = parmsin(7)   !masszt3=initial sheath mass
      parms(14) = 0.0          !zlent3=initial axial position
      parms(15) = 0.0          !velzt3=initial axial velocity
      parms(16) = 0.0          !mass axial sheath at z=zlenmax

      parms(17) = - (mu * 1.0e-2 * 0.5) !initial radial length = 1e-2 m
      parms(18) = 2.0 * pi * 1.0e-2 * parmsin(4)
      parms(19) = mu * 1.0e-2
      parms(20) = pi * 2.0e-2 * parmsin(4)
      parms(21) = mu * 2.0e-2 * log(parmsin(2)/parmsin(6))
      parms(22) = 0.0         !massrt3=initial radial sheath mass
      parms(23) = parmsin(2)  !radt3=initrad
      parms(24) = 0.0         !initial velocity

      parms(25) = 0.0         !initial total inductance
      parms(26) = 0.0         !set status as "imploding"
c
c      print '(24E12.4)', (parms(i), i=1,nparms)
c
c
c Set initial plotting values for DPF radius, velocity,
c acceleration, kinetic energy.
c
      gasrad   = 0.0
      gasvel   = 0.0
      gasacc   = 0.0
      gaske    = 0.0
c
      return
      end

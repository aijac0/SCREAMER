      subroutine ediode
c Read-only arguments
     & (time, voltage,
c Read and write arguments
     &  var_parms,
c Write-only arguments
     &  conductance)
c
c ----------------------------------------------------------------------
c
c 2016-02-17 Original version written by R. B. Spielman
c Modifications:
c 2016-04-25 RBS: Continuing to develop the subroutine
c 2016-05-29 RBS: Finished subroutine
c 2017-08-05 RBS: Added limit check on the diode gap, modified the
c                 diode perveance to agree with Roy et al., corrected
c                 cathode emissivity test, added a test to vt
c 2017-11-29 RBS: Removed the enhancement of uhalf, uhalf should not be
c                 enhanced
c 2018-02-22 RBS: Changed the definition of vt to include the motion of
c                 both the anode and cathode plasmas
c 2019-01-21 RBS: Gap_1 removed from the real declaration. Not used.
c
c  Core routine based on sdiode_model
c
c  Electron beam diode model with gap closure. No emission for fields
c   less than 300 kV/cm.
c
c  At non-relativistic energies the current is the CL limited current
c   for that particular geometry
c
c  Model calculates the 1-D planar CL limited current.
c
c  Model calculates the axial ring diode CL limited current.
c
c  Use PhysPlasmas.16.053103.2009-Roy et al. for CL formulae
c  Use PhysPlasma.7.1514.2000 Swanekamp for magnetized CL current
c  R. B. Miller, Intro. to the Physics of Intense Charged Particle
c      Beams - general information
c
c We assume that the beam is either non-relativistic or slightly
c relativistic. We use Swanekamp for the relativistic formulation.
c
c We will calculate the 1-D planar Perveance
c
c  time is the problem time at the half-step,
c
c  voltage is the voltage across the diode at the prior time step.
c    (the voltage actually used the the voltage at the last half-step).
c
c This subroutine returns the diode impedance (conductance) for each
c    time step.
c
c This subroutine calculates the diode gap for each time step.
c
c  We will assume that the anode and cathode expansion velocity are
c  equal to the input velocity. Typical values are 5-10 cm/µs.
c
c The Alfven current is 16.9 kA.
c
c We assume ion neutralization of the beam that allows 2X more electron
c    current than the simple CL limit.
c
c  We assume that it takes 10 ns to start the expansion of the anode
c    plasma.
c
c ----------------------------------------------------------------------
c
c Define passed variables
c
      real       time
      real       voltage
      real       var_parms(*)
      real       conductance
c
c Define internal variables
c
      real       area, beta, circ, circ_i, e_cathode, efield,
     &           gamma, gap, gap_d, g_log, gap_time, perveance,
     &           tanode, tcathode, uhalf,
     &           velocity, voltlast, volthalf

      integer    idiode, i_on

      real        pi
      parameter ( pi = 3.14159265 )

      real        e_mass
      parameter ( e_mass = 511.06e3 )

      real        condmin
      parameter ( condmin = 1.0e-06 )
c
c ----------------------------------------------------------------------
c
c Passed parameters
c
c var_parms(1) = diode type
c var_parms(2) = A/K gap
c var_parms(3) = cathode field enhancement
c var_parms(4) = velocity
c var_parms(5) = router
c var_parms(6) = rinner
c
c Modified parameters
c var_parms(7) = voltage
c
c Parameters used in this diode model:
c   area is the effective diode area
c   gap is the initial diode gap (before closure).
c   velocity is the gap closure velocity.
c   voltlast is the voltage at the last time step.
c   gap_d is the calculated gap for this time step
c
c Determine the 1-D field and apply the electric field enhancement
c  factors by multiplying the 1-D field by the enhancement.
c
c ----------------------------------------------------------------------
c
c Populate subroutine variables with passed values.
c
      idiode   = int(var_parms(1))
      gap      = var_parms(2)
      enhance  = var_parms(3)
      velocity = var_parms(4)
      router   = var_parms(5)
      rinner   = var_parms(6)

      voltlast = var_parms(7)
      gap_time = var_parms(8)
      i_on     = int(var_parms(9))

c
c Initialize variables
c

c Calculate the half-step voltage to be used this time step
c The solution assumes that the voltage is always positive.
c Always saves the voltage from the last time step.
c Uhalf is the voltage scaled to the rest mass
c
      volthalf = 0.5 * (voltlast + voltage)
c
c Check for voltage below normal diode operation
c
      if (abs(volthalf) .le. 1.0) then
        volthalf = 1.0
      end if

      uhalf = abs( volthalf / e_mass )
      efield = abs( volthalf / gap )
c
c If the cathode electric field is less than 300 kV/cm (30 MV/m),
c  set the conductance for an open circuit (so it really is a diode).
c  Use the enhancement factor
c  Will increment time until emission starts
c Once emission starts it never ends
c uhalf should NOT be enhanced.
c
      e_cathode = enhance * efield
c      uhalf     = enhance * uhalf

      if (e_cathode .lt. 3.0e+07 .and. i_on .eq. 0) then
        conductance = condmin
        gap_time = time
c
c The field is above the 300 kV/cm threshold emission is allowed.
c  Set the gap and find conductance.
c
      else
c
c Calculate the A/K gap based on inputted velocity
c   Sets the i_on switch to emissive, once on always on
c   Anode plasma starts 10 ns later
c
        i_on = 1
        tcathode = time - gap_time

        if ( tcathode .lt. 1.0e-8 ) then
          tanode = 0
         else
          tanode = tcathode - 1.0e-8
        end if

c
c Calculate the modified AK gap anew for each time step
c
        gap_d = gap - (velocity * tcathode) -
     &                (velocity * tanode)

c      print '(a,1pe12.4,a,1pe12.4)',
c     &      'velocity = ', velocity,'  tcathode =', tcathode


c Gap limit - gap_d can never totally close

        if (gap_d .lt. 0.01 * gap ) then
          gap_d = 0.01 * gap
        end if
c        print '(a,1pe12.4,a,1pe12.4)', 'gap = ', gap,'  gap_d = ', gap_d

c
c We calculate the conductance by dividing the current by volthalf.
c   Note: the current equations use the voltage normalized to the
c   electron rest mass.
c
c Planar diode (area only)
c   No enhancement in area term
c

        if ( idiode .eq. 1 ) then
          area = pi * router * router
          perveance = 1691.1 * area / ( gap_d * gap_d )
          gamma = ( uhalf + 1.0 )
c
c Check if relativistic
c
          if (uhalf .le. 1.0) then
            conductance = perveance * uhalf**1.5 / abs(volthalf)
          else
            conductance = 3.18 * perveance *
     &                    ((sqrt(gamma) - 0.8471)**2) / abs(volthalf)
          end if

c
c Planar diode (area + one edge)
c   No enhancement in area term included - contradicts the SNL report
c

        else if ( idiode .eq. 2 ) then
          area = pi * router * router
          perveance = 1691.1 * area / ( gap_d * gap_d )
          gamma  = ( uhalf + 1.0 )
          circ   = 2 * pi * router
          vt     = ( velocity * tcathode ) + ( velocity * tanode )


          if ( vt .lt. 0.001 * gap ) then
            vt = 0.001 * gap
c           this is early in time when gap closure is low
c           this limits max g_log to 6.9
          end if

          if ( vt .gt. 0.99 * gap ) then
            vt = 0.99 * gap
c           this is late in time when gap closure is maximum
c           this limits min g_log to 0.01
          end if

          g_log = log( gap / vt )
          beta = exp(-g_log/2.0)*(g_log + 0.1 * g_log**2 +
     &           0.01667 * g_log**3 + 0.00242 * g_log**4 +
     &           0.000287 * g_log**5 + 0.0000266 * g_log**6)
c    This series only converges for values of g-log less than 2.72
c
c Check if relativistic
c
          if (uhalf .le. 1.0) then
            perveance = perveance +
     &                  1328.2 * circ / (beta * beta * gap)
            conductance = perveance * uhalf**1.5 / abs(volthalf)
          else
            conductance = 3.18 * perveance *
     &                    ((sqrt(gamma) - 0.8471)**2) / abs(volthalf)
            conductance = conductance +
     &                    (4129.7 * circ * (sqrt(gamma) - 0.8471)**2 /
     &                    (beta * beta * gap * abs(volthalf)))
          end if
c          print '(A, 1pe10.3)', ' R-Conductance P+ring= ', conductance

c
c Ring diode (area + edges)
c   No enhancement in area term included - contradicts the SNL report
c

        else
          area = pi * ( router * router - rinner * rinner )
          perveance = 1691.1 * area / ( gap_d * gap_d )
          gamma = ( uhalf + 1.0 )
          circ   = 2 * pi * router
          circ_i = 2 * pi * rinner
c
c Is this def of vt correct? Should anode plasma motion be included?
c gap_d is the remaining open gap used to calculated current
c vt is how far the cathode plasma has moved, they sum to 1.0
c gap_d and vt are correctly calculated numerically
c
          vt     = ( velocity * tcathode ) + ( velocity * tanode )

          if ( vt .lt. 0.001 * gap ) then
            vt = 0.001 * gap
          end if
          if ( vt .gt. 0.99 * gap ) then
            vt = 0.99 * gap
          end if

c          print '(a,1pe12.4,a,1pe12.4,a,1pe12.4)',
c     &          'time=', time, '  gap_d=', gap_d, '  vt=', vt

          g_log = log( gap / vt )
          beta = exp(-g_log/2)*(g_log + 0.1 * g_log**2 +
     &           0.01667 * g_log**3 + 0.00242 * g_log**4 +
     &           0.000287 * g_log**5 + 0.0000266 * g_log**6)
c
c          print '(a,1pe12.4,a,1pe12.4)',
c     &          'g_log =', g_log, '  beta =', beta
c
c Check if relativistic or not
c
          if (uhalf .le. 1.0) then
            perveance = perveance +
     &                  1328.2 * circ   / (beta * beta * gap) +
     &                  1328.2 * circ_i / (beta * beta * gap)
            conductance = perveance * uhalf**1.5 / abs(volthalf)
          else
            conductance = 3.18 * perveance *
     &                    ((sqrt(gamma) - 0.8471)**2) / abs(volthalf)
            conductance = conductance +
     &                    (4129.7 * circ * (sqrt(gamma) - 0.8471)**2 /
     &                    (beta * beta * gap * abs(volthalf))) +
     &                    (4129.7 * circ_i * (sqrt(gamma) - 0.8471)**2 /
     &                    (beta * beta * gap * abs(volthalf)))
          end if

        end if

      end if
c
c Update common variables for next time step.
c
      var_parms(7) = voltage
      var_parms(8) = gap_time
      var_parms(9) = float(i_on)
c
c -----------------------------------------------------------------------------
c
      return
      end

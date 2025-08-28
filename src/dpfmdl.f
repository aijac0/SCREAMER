      subroutine dpf_model
c read only arguments
     &  (time, dt, rdt, ilt2,
c read and write arguments
     &  parms,
c write only arguments
     &  ldott2, lt1)
c ----------------------------------------------------------------------
c
c Summary:
c
c Screamer dpf implosion subroutine.
c
c   Define:
c     the next               full time step as t=i
c                                                   (variable suffix t0)
c     the current            half time step as t=i-1/2
c                                                   (variable suffix t1)
c     the previous           full time step as t=i-1
c                                                   (variable suffix t2)
c     the previous           half time step as t=i-3/2
c                                                   (variable suffix t3)
c     the previous, previous full time step as t=i-2
c                                                   (variable suffix t4)
c     the previous, previous half time step as t=i-5/2
c                                                   (variable suffix t5)
c
c Calculates the following values for an imploding dpf:
c   Passed via argument list:
c     inductance (=lt1) at t=i-1/2
c     time rate of change of inductance (=ldott2) at t=i-1
c   Passed via 'zdemout.h' common blocks:
c    (used gas puff variables for now)
c     gas puff kinetic energy (=gaske) at t=i-1/2
c     gas puff radius (=gasrad)        at t=i-1/2
c     gas puff velocity (=gasvel)      at t=i-1/2
c     gas puff acceleration (=gasacc)  at t=i-1
c
c When the calculated dpf radius becomes less than or equal to the
c minimum radius, the implosion stops and this routine sets:
c   radt1    = minrad
c   lt1      = mu * length * ln (initrad/minrad) (=lminrad)
c   ldott2   = 0
c   gaske    = 0
c   gasvel   = 0
c   gasacc   = 0
c
c ----------------------------------------------------------------------
c
c Modifications:
c 2016-03-22 RBS: Created from gaspfmdl.f
c 2016-05-02 RBS: Included radial dynamics
c 2016-07-01 RBS: Included and angled sheath and shear
c
c ----------------------------------------------------------------------
c
c Include the file which has a common block for storing some unusual
c parameters from this model
c
      use zdemmax             !parameters
      use zdemout             !common blocks
c
c ----------------------------------------------------------------------
c
c Read only passed arguments
c
      real       time     !simulation time, t=i
      real       dt       !time-step
      real       rdt      !1/dt
      real       ilt2     !current through this inductor at t=i-1
c
c Read and write passed arguments
c
      real       parms(*) !model parameters for this circuit element
c
c Write only passed arguments
c
      real       ldott2  !time rate of change of inductance at t=i-1
      real       lt1     !total inductance at t=i-1/2
c
c ----------------------------------------------------------------------
c
c Set some internal parameters
c
      real       implode,       end_implode
      parameter (implode = 0.0, end_implode = 1.0)

      real       pi
      parameter (pi = 3.14159)
c
c Declare parms variables
c
      real       router    !outer radius DPF
      real       rinner    !inner radius DPF
      real       zlenmax   !length of anode
      real       density   !density
      real       rimass    !fractional mass of radial sheath vs. axial
      real       rmin      !minimum radius
      real       initmass  !initial mass term
      real       theta     !sheath angle

      real       azconst   !(mu*ln(router/rinner))/2
      real       dzconst   !(-pi*density*(router^2-rinner^2))
      real       lzconst   !(mu*ln(router/rinner)))
      real       mzconst   !(pi*length*density)
      real       masszt3   !axial mass at t=i-3/2
      real       zlent3    !axial position t=i-3/2
      real       velzt3    !axial velocity from t=i-3/2
      real       masszlen  !axial mass when z=zlenmax

      real       arconst   !-(mu*length)/2
      real       drconst   !(2*pi*length*density)
      real       lrconst   !(mu*length)
      real       mrconst   !(pi*length*density)
      real       lrmin     !inductance at minimum radius
      real       massrt3   !radial mass at t=i-3/2
      real       radt3     !radius from t=i-3/2
      real       velrt3    !radial velocity from t=i-3/2

      real       lt3       !total inductance at t=i-3/2

      real       testimpl  !calculated current state of DPF implosion
c                          ! (imploding 0 or stagnated 1)
c
c Declare internal variables
c
      real       accelzt2  !calculated acceleration term due to current,
c                           t=i-1
      real       dragzt2   !calculated acceleration term due to drag,
c                           t=i-1
      real       acczt2    !calculated total acceleration term, t=i-1
      real       velzt1    !calculated velocity, t=i-1/2
      real       zlent1    !position t=i-1/2
      real       lzt1      !axial inductance at t=i-1/2
      real       masszt1   !calculated axial mass at t=i-1/2
c
      real       accelrt2  !calculated acceleration term due to current,
c                           t=i-1
      real       dragrt2   !calculated acceleration term due to drag,
c                           t=i-1
      real       accrt2    !calculated total acceleration term, t=i-1
      real       velrt1    !calculated velocity, t=i-1/2
      real       radt1     !calculated radius,   t=i-1/2
      real       lrt1      !radial inductance at t=i-1/2
      real       mrinit    !Initial radial sheath mass
      real       massrt1   !calculated radial mass at t=i-1/2
c
c ----------------------------------------------------------------------
c
c Set the model parameters to the understandable names.
c
      router    = parms(1)
      rinner    = parms(2)
      zlenmax   = parms(3)
      density   = parms(4)
      rimass    = parms(5)
      rmin      = parms(6)
      initmass  = parms(7)
      theta     = parms(8)

      azconst   = parms(9)
      dzconst   = parms(10)
      lzconst   = parms(11)
      mzconst   = parms(12)
      masszt3   = parms(13)
      zlent3    = parms(14)
      velzt3    = parms(15)
      masszlen  = parms(16)

      arconst   = parms(17)
      drconst   = parms(18)
      lrconst   = parms(19)
      mrconst   = parms(20)
      lrmin     = parms(21)
      massrt3   = parms(22)
      radt3     = parms(23)
      velrt3    = parms(24)

      lt3       = parms(25)

      testimpl  = parms(26)

c
c Initialize
c
      radt1 = 0.0
c
c ----------------------------------------------------------------------
c
c We will calculate the acceleration in the axial direction of the DPF
c
c This simple model assumes a stiff interface that is normal to the axis
c and mass accretes as a total mass. The total accelerating force is the
c integral of the pressure across the sheath from rinner to router.
c
c Calculate the acceleration, velocity and radius of the gas puff.
c From these, calculate L, dL/dt, d(LI)/dI.
c  Note: in this calculation mu = 2x10^-7 NOT 4pix10^-7 ****************
c
c   The acceleration at t=i-1 is defined by the sum of:
c       accilt2 = P*A/masst3
c
c   for an axial snow plow the axial acceleration is
c
c     d(accilzt2) = - P•dA/masst3
c                 = -P 2 pi rdr / masst3
c                 = - [mu0I**2 / 4 pi r] dr
c
c     accilzt2 = - (mu0 * ilt2**2) ln(ro/ri) / (4 pi * masst3)
c              = - (mu  * ilt2**2) ln(ro/ri) / (2 * masst3)
c              =   (aconst * ilt2**2) / masst3
c     masst3 is calculated at the end of this time step
c
c Drag can be thought of as the force of gas moving a velocity velt3
c striking the sheath per time step.
c
c     dragzt2 = F / m = [(Area * velt3 * density)] * velt3 / masst3
c             = [(pi * density) * (ro**2 - ri**2) * velt3]
c                                                       * velt3 / masst3
c     dragzt2 = (pi * density) * (ro**2 - ri**2) * velt3**2 / masst3
c             =  dconst * velt3**2 / masst3
c
c
c   The time centered axial velocity is defined by:
c       (velzt1 - velzt3) / dt  = acczt2

c   The time centered radial velocity is defined by:
c       (velt1 - velt3) / dt  = acct2
c
c   The time centered radius is defined by:
c       (radt1 - radt3) / dt = 0.5 * (velt1 + velt3)
c
c   The time centered axial position is defined by:
c       (zlent1 - zlent3) / dt = 0.5 * (velzt1 + velzt3)
c
c   Axial inductance Lz (lzt1) is defined by:
c       (mu0 * zlent1) * ln(ro/ri) / 4 pi
c
c   Axial dL/dt (ldotzt2) is defined by:
c       (ltz1 - ltz3) / dt
c
c   dL/dt (ldott2) is defined by:
c       (lt1 - lt3) / dt
c
c The height of the sheath above a plane that touches the outer radius
c   of the sheath or router is:
c
c  -the ith split radius = zlent1/tan(theta) - i*delta_r + 0.5*delta_r
c
c The height of the sheath at the ith radius is:
c
c  - ith height = ri * tan(theta)
c               = zlent1 - i*delta_r*tan(theta) + 0.5*delta_r*tan(theta)
c
c This formulation is used to calculate the height for the iterated
c   calculation of the inductance of the angled current sheath and
c   the mass swept out by an angled sheath.
c
c ----------------------------------------------------------------------
c
c We will calculate the acceleration in the radial direction
c
c Calculate the radial acceleration, velocity and radius of the dpf.
c From these, calculate radial L, dL/dt, d(LI)/dI.
c  Note: in this calculation mu = 2x10^-7 NOT 4pix10-7
c
c   The acceleration at t=i-1 is defined by the sum of:
c       accelrt2 = P*A/masst3
c       accelrt2 = - ( mu * ilt2**2 * length) / (2 * massrt3 * radt3)
c       massrt3 is calculated at the end of this time step
c Drag can be thought of as the force of gas moving a velocity velrt3
c striking the sheath per time step.
c    dragrt2 = F / m = [(Area * velt3 * density)] * velt3 / masst3
c            = [(2 * pi * length * density) * radt3 * velt3]
c                                                      * velt3 / massrt3
c    dragrt2 = (2 * pi * length * density) * radt3 * velt3**2  / massrt3
c
c   The time centered velocity is defined by:
c       (velrt1 + velrt3) / dt  = accrt2
c
c   The time centered radius is defined by:
c       (radt1 - radt3) / dt = 0.5 * (velrt1 + velrt3)
c
c   Radial L (lt1) is defined by:
c       (mu * length) * ln(rinner/radt1)
c
c   Total dL/dt (ldott2) is defined by:
c       (lt1 - lt3) / dt
c
c ----------------------------------------------------------------------
c
c For a dense plasma focus:
c
c Maximum axial length is zlenmax for start of radial motion
c Stop of radial phase reaches minrad
c
      if (testimpl .eq. implode) then
c
c   a. DPF axial acceleration terms
c
c       print '(/A,1pE10.3)', 'Time step= ', time
c       print '(A,1pE10.3,A,E10.3)',
c     &       ' Current= ',ilt2,' Old radius= ',radt3

        if (zlent3 .le. zlenmax) then
          accelzt2 = ( azconst * ilt2   *  ilt2  ) / (masszt3)
          dragzt2  = ( dzconst * velzt3 * velzt3 ) / (masszt3)
          acczt2   =   accelzt2 + dragzt2
          masszlen = masszt3
          accelrt2 = 0.0
          dragrt2  = 0.0
          accrt2   = 0.0
          velrt3   = 0.0
        else
c
c    b. DPF axial and radial acceleration terms
c       We are past the end of the anode and can start radial implosion
c       set the radial mass to be a fraction of the old masszt3
c       First, we keep axial acceleration and, second, start radial acc
c
          accelzt2 = ( azconst * ilt2   * ilt2    ) / (masszt3)
          dragzt2  = ( dzconst * velzt3 * velzt3  ) / (masszt3)
          acczt2   =   accelzt2 + dragzt2

          accelrt2 = ( arconst * ilt2   * ilt2    ) / (radt3 * massrt3)
          dragrt2  = ( drconst * radt3  * velrt3 * velrt3 ) / (massrt3)
          accrt2   =  accelrt2 + dragrt2
        end if

c      print '(4E10.3)', masszt3, accelzt2, dragzt2, acczt2
c      print '(4E10.3)', massrt3, accelrt2, dragrt2, accrt2
c
c   c. New DPF axial velocity
c
        velzt1 = velzt3 + acczt2 * dt

c     print '(A,E10.3,A,E10.3)','Old zvel= ',velzt3,' New zvel= ',velzt1
c
c   d. New DPF radial velocity
c
        velrt1 = velrt3 + accrt2 * dt

c      print '(A,E10.3,A,E10.3)','Old rvel= ',velrt3,' New rvel= ',velrt1
c
c   e. New DPF axial position
c      Use the time centered velocity to calculate the axial motion in
c      the time step
c
        dzlen  = ((velzt1 + velzt3) * dt * 0.5)
        zlent1 = zlent3 + dzlen

c      print '(A,E11.4,A,E11.4)','Old zlen= ',zlent3,' New zlen= ',zlent1
c
c   f. New DPF radial position
c
        radt1 = radt3 + ((velrt1 + velrt3) * dt * 0.5)

c      print '(A,E10.3,A,E10.3)','Old rad=  ',radt3,' New rad=  ',radt1
c
c      Test for end of implosion
c
        if (radt1 .lt. rmin) then
          radt1 = rmin
          testimpl  = end_implode
        end if
c
c      Do loop here to test for lower sheath radius .ge. router
c      Calculate the inductance base on the angle and zlent1
c
        rad_sh = zlent3 / tan(theta)
        if( rad_sh .ge. (router - rinner)) rad_sh = (router - rinner)
        rad_tot = rad_sh + rinner
        sheath = ( rad_sh ) / cos(theta)

        z0 = ( router - rinner ) * tan(theta)
        print '(A,e11.4,A,e11.4,A,e10.3)', 'Sheath radial extent= ',
     &  rad_sh, ' Sheath Router= ', rad_tot, ' DPF Router= ', router
c
c  Sheath resolution
c
        j = 50
c
c   g. New DPF axial inductance based on new axial position
c
        lzt1 = 0.0

        if ( rad_tot .lt. router ) then
          delta_r = ( rad_sh ) / float ( j )

          do i = 1, j
            lzt1 = lzt1 + 2.0e-7 *
     &             ( zlent1 - float(i) * delta_r * tan(theta) + 0.5 *
     &             delta_r * tan(theta)) *
     &             log(( rinner + float( i ) * delta_r ) /
     &             ( rinner + float( i - 1 ) * delta_r ) )
            end do

        else
          delta_r = ( router - rinner ) / float ( j )
          lzt1 = 2.0e-07 * ( zlent1 - z0 ) * log ( router / rinner )

          do i = 1, j
            lzt1 = lzt1 + 2.0e-7 *
     &             ( z0 - float( i ) * delta_r * tan(theta) + 0.5 *
     &             delta_r * tan(theta)) *
     &             log(( rinner + float( i ) * delta_r ) /
     &             ( rinner + float( i - 1 ) * delta_r ) )
             end do

        end if

c        print '(A,1pE10.3)', 'New axial inductance= ', lzt1
c
c   h. New DPF radial inductance based on new radial position
c
        lrt1 = lrconst * log(rinner/radt1)
c
c   i. New total inductance
c
        lt1 = lzt1 + lrt1
c
c   j. New time rate of change of inductance time new - old
c
        ldott2 = (lt1 - lt3) * rdt
c
c   k. Calculate new DPF mass with new axial position for next time step
c      acceleration terms.
c
c      Axial
c      Calculate the new mass including the mass lost by shear
c      Include the fact that the mass of the last time step is
c      calculated.
c
        if (time .lt. dt) masszt1 = initmass
        masszt1 = masszt3
c
c      In one time step the sheath has moved dzlen and accumulated the
c      swept up mass of that tiny volume. The volume is about the sheath
c      length times the height perpendicular to the sheat
c      (dzlen * cos (theta))
c

        masszt1 = masszt1 + ( sheath * dzlen * cos(theta) * density )

c
c Shear Velocity
c
        v_shear = velzt1 * sin(theta)
c
c Sheath sheared radial distance in dt
c
        r_shear = v_shear * dt
c
c calculate the fractional mass lost
c

        frac_m = r_shear / sheath
        masszt1 = masszt1 * (1.0 - frac_m)
c
c      Radial
c
        mrinit  = rimass * masszlen
c        print '(A,1pe10.3,A,1pe10.3,A,1pe10.3)',
c     &         'Initial radial mass= ', mrinit,
c     &         ' Sheath mass at end of anode= ', masszlen,
c     &         ' New zmass= ', masszt1

        massrt1 = mrconst * (rinner * rinner - radt1 * radt1)
     &            + mrinit

c        print '(A,1pe10.3)', 'New rmass= ', massrt1
c
c   l. Put some values into the common block so that they will be
c      available for plotting.
c
        gaske     = 0.5 * massrt1 * velrt1 * velrt1
        gasrad    = radt1
        gasvel    = velrt1
        gasacc    = accrt2
c
c Print out an informational message about reaching the min-radius
c
c Write to log file
c
        if (testimpl .eq. end_implode) then
          write(9,
     &  '(/A/,A/,A,1pe10.3/,A,1pe10.3/,A,1pe10.3/,A,1pe10.3/,A/)')
     &  ' ------------------------------------------------------------',
     &  ' DPF radius has reached the minimum value.',
     &  '   time:                   ',time,
     &  '   minimum radius:         ',radt1,
     &  '   KE at min-radius:       ',gaske,
     &  '   velocity at min-radius: ',velrt1,
     &  ' ------------------------------------------------------------'

c write to screen

        write(6,'(A)')
     &  ' ------------------------------------------------'
        write(6,'(A)')
     &  ' DPF radius has reached the minimum value.'
        write(6,'(A,1pe10.3)') '  time:                   ',time
        write(6,'(A,1pe10.3)') '  minimum radius:         ',radt1
        write(6,'(A,1pe10.3)') '  KE at min-radius:       ',gaske
        write(6,'(A,1pe10.3)') '  velocity at min-radius: ',velrt1
        write(6,'(A)')
     &  ' ------------------------------------------------'
      end if
c
c   h. Save all values required for next time step. All of the t1
c      parameters are placed in the parms array where they will be used
c      as the t3 values in the next time step.
c
        parms(13)  = masszt1
        parms(14)  = zlent1
        parms(15)  = velzt1
        parms(16)  = masszlen

        parms(22)  = massrt1
        parms(23)  = radt1
        parms(24)  = velrt1

        parms(25)  = lt1

        parms(26)  = testimpl
c
c For stagnant DPF (constant radius and inductance):
c 
      else
c
c   d. DPF total inductance
c
        lt1 = lt3
c
c   e. Time rate of change of axial inductance
c
        ldott2 = 0.0
c
c   f. Put some values into a common block so that they will be
c      available for plotting.
c
        gaske     = 0.0
        gasrad    = radt1
        gasvel    = 0.0
        gasacc    = 0.0
      end if
c
c ----------------------------------------------------------------------
c
      return
      end

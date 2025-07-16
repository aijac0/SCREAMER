      subroutine nshell_model
c read only arguments
     & (time, dt, rdt, ilt2, vlt2,
c read and write arguments
     &  parms,
c write only arguments
     &  ldott2, lt1)
c ----------------------------------------------------------------------
c  June 6, 1997   Modified from the Cylindrical foil model by Ken Struve
c 2014-02-06 RBS: Changed real*4 to real
c 2015-06-22 RBS: Error in line 262, line was too long.
c 2015-06-23 RBS: Initialized radt1 and velt1 - gave a compiler error.
c
c Summary:
c
c   This model adapted from the Cylindrical foil model.  It will handle
c   two collapsing shells, with no other mass acretion.
c   This routine CANNOT be used with the cylindrical foil model since
c   it uses the same output variable names.
c   Treats r2 and l2 as variable elements (r2 is dl2/dt).
c
c Define:
c
c the next               full time step as t=i      (variable suffix t0)
c the current            half time step as t=i-1/2  (variable suffix t1)
c the previous           full time step as t=i-1    (variable suffix t2)
c the previous           half time step as t=i-3/2  (variable suffix t3)
c the previous, previous full time step as t=i-2    (variable suffix t4)
c the previous, previous half time step as t=i-5/2  (variable suffix t5)
c
c Calculates the following values for an imploding shell:
c   Passed via argument list:
c     inductance (=lt1) at t=i-1/2
c     time rate of change of inductance (=ldott2) at t=i-1
c   Passed via 'zdemout.h' common blocks:
c     foil kinetic energy (=foilke) at t=i-1/2
c     foil radius (=foilrad)        at t=i-1/2
c     foil velocity (=foilvel)      at t=i-1/2
c     foil acceleration (=foilacc)  at t=i-1
c When the calculated foil radius becomes less than or equal to the
c minimum radius, the implosion stops and this routine sets:
c   radt1   = (foilrad =) minrad 
c   lt1     = mu * length * ln (initrad/minrad) (=lminrad)
c   ldott2  = 0
c   foilke  = 0
c   foilvel = 0
c   foilacc = 0
c
c ----------------------------------------------------------------------
c
c Include the file which has a common block for storing some unusual
c parameters from this model
c
      use zdemmax             !parameters
      include   'zdemout.h'   !common blocks
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
      real       ldott2  !time rate of change of inductance at t=i-1/2
      real       lt1     !inductance at t=i-1/2
c
c ----------------------------------------------------------------------
c
c Set some internal parameters
c

      real       implode,       end_implode
      character  nameforit*20
      logical    debug
      parameter (debug = .false.)
      parameter (implode = 0.0, end_implode = 1.0)
      parameter (deltar = 50.0e-6)
c
c Set internal variables
c
      real       newsvelocity(max_shells)
      real       Lequiv, newLequiv  !equivalent inductance of all shells
      real       wallrad   !return current wall radius = akgap + initrad
      real       initrad   !initial radius
      real       minrad    !minimum radius
      real       mass      !mass in kilograms
c    real       reachedr2 !flag indicating whether second shell reached
      real       lconst    !(mu*length)
      real       lminrad   !inductance at minimum radius
      real       lt3       !inductance from t=i-3/2
      real       radt1     !calculated radius,   t=i-1/2
      real       radt3     !radius from t=i-3/2
      real       velt1     !calculated velocity, t=i-1/2
      real       velt3     !velocity from t=i-3/2
c      real       acct2     !calculated acceleration, t=i-1
      real       testimpl  !calculated current state of foil
                           ! (imploding or stagnated)
      real       length    !foil length in meters
c
c ----------------------------------------------------------------------
c
c Set the model parameters to understandable names.
c
      initrad   = parms(1)
      minrad    = parms(2)
      mass      = parms(3)
      length    = parms(4)
      lconst    = parms(5)
      lminrad   = parms(6)
      lt3       = parms(7)
      radt3     = parms(8)
      velt3     = parms(9)
      testimpl  = parms(10)
      Eload     = parms(11)
      peakcur   = parms(12)
      wallrad   = parms(13)
      Lequiv    = parms(14)
      ttrap     = parms(15)

c
c Initialize
c

      radt1 = 0.0
      velt1 = 0.0

c
c ----------------------------------------------------------------------
c
c Calculate the acceleration, velocity and radius of the foil.
c From these, calculate L and dL/dt (=R)
c
c   The acceleration at t=i-1 is defined by:
c       acct2 = - ( mu * ilt2**2 *length) / (2 * mass * radt3)
c
c   The time centered velocity is defined by:
c       (velt1 - velt3) / dt  = acct2
c
c   The time centered radius is defined by:
c       (radt1 - radt3) / dt = 0.5 * (velt1 + velt3)
c
c   L (lt1) is defined by:
c       (mu * length) * ln(initrad/radt1)
c
c   dL/dt (ldott2) is defined by:
c       (lt1 - lt3) / dt
c
c ----------------------------------------------------------------------
c
      if (abs(ilt2) .gt. peakcur) then
         peakcur = abs(ilt2)
      endif
      Eload = Eload + ilt2*vlt2*dt
c For an imploding foil:
c
      if (testimpl .eq. implode) then
c
c
c  0. Trap currents in inner shells when time of magnetic cutoff reached
c
        factor = 1.0
        if (time.gt.ttrap) then
          factor = 0.0
          if (.not.trapped) then
            do i=1,numshells
            rtrap(i)=shellradius(i)
            itrap(i)=shellcurr(i)
            end do
            trapped=.true.
          endif
        endif
c
c   a. Check if ith shell radius reached and adjust mass and velocity
c
c
        do i=1,numshells
          do j=i+1,numshells
            if (((abs(shellradius(i)-shellradius(j)).lt.deltar).or.
     &          (shellradius(i).le.shellradius(j))).and.
     1         (shell(j).eq.j)) then
            shell(j)=i
            mass=shellmass(i)+shellmass(j)
            svelocity(i)=svelocity(i)*shellmass(i)/mass
            shellmass(i)=mass
            shellind(j)=1.0e30
            shellradius(j)=shellradius(i)
            shellmass(j)=0.0
            shellcurr(j)=0.0
            endif
          end do
        end do
c
c ----------------------------------------------------------------------
c
c   b. Calculate inductance, equivalent inductance, and currents in each
c      shell
c
        newLequiv=0.0
        shellind(1)=lconst*log(wallrad/shellradius(1))
        newLequiv=newLequiv+1.0/shellind(1)
        do i=2,numshells
          if (shell(i).eq.i) then
            shellind(i)=lconst*log(wallrad/shellradius(i))
            newLequiv=newLequiv+factor/shellind(i)
          endif
        end do
        newLequiv=1.0/newLequiv
c
c
c       Calculate currents before and after the first shell becomes
c       magnetically opague and currents are trapped.  Also skip 
c       current calculation for shells that have collided with another.
c
c
        if (trapped) then
          shellcurr(1)=ilt2
          do i=2,numshells
            if (shell(i).eq.i) then
            shellcurr(i)=itrap(i)*log(rtrap(i-1)/rtrap(i))/
     1                   log(shellradius(i-1)/shellradius(i))
            endif
          end do
        else
          do i=1,numshells
            if (shell(i).eq.i) then
            shellcurr(i)=ilt2*newLequiv/shellind(i)
            endif
          end do
        endif
c
c ----------------------------------------------------------------------
c
c
c   c. Foil acceleration
c
        do i=1,numshells
        if (shell(i).eq.i) then
        acceleration(i)=lconst*(shellcurr(i+1)**2-shellcurr(i)**2)/
     1                  (2.0*shellmass(i)*shellradius(i))
        endif
        acceleration(i)=acceleration(shell(i))
        end do
c
c ----------------------------------------------------------------------
c
c
c   d. Foil velocity
c
        do i=1,numshells
        newsvelocity(i)=svelocity(i)+acceleration(i)*dt
        newsvelocity(i)=newsvelocity(shell(i))
        end do
c
c ----------------------------------------------------------------------
c
c   e. Foil radius, check to see if less than minimum radius, if
c        so, print a message that this has occurred and set values
c        appropriately.  But keep going with the calculation since
c        the foil radius has changed this time step.
c
        Ekinetic=0.0
        do i=1,numshells
          shellradius(i)=shellradius(i)+(newsvelocity(i)+
     &                   svelocity(i))*dt/2.
          Ekinetic=Ekinetic+0.5*shellmass(i)*newsvelocity(i)**2
        end do
        do i=1,numshells
        shellradius(i)=shellradius(shell(i))
        enddo
        if (shellradius(numshells) .lt. minrad) then
          testimpl = end_implode
          write(9,999) time, Ekinetic, Eload, peakcur,
     &                shellradius(1)
c     1                 (shellradius(i),i=1,numshells)
          write(9,998)
c          write(6,999) time, Ekinetic, Eload, peakcur,
          write(6,999) time, Ekinetic, Eload, peakcur,
     &                shellradius(1)
c     1                 (shellradius(i),i=1,numshells)
          write(6,998)
        end if
c
c
c ----------------------------------------------------------------------
c
c   f. Foil inductance
c
        lt1 = newLequiv
c
c
c ----------------------------------------------------------------------
c
c   g. Time rate of change of inductance
c
        ldott2 = (newLequiv - Lequiv) * rdt
c
c
c ----------------------------------------------------------------------
c
c   h. Put some values into a common block so that they will be
c      available for plotting.
c
        shellke  = Ekinetic
        shellrad = shellradius(1)
        shellvel = newsvelocity(1)
        shellacc = acceleration(1)
        shellm   = shellmass(1)  
c   i. Save all values required for next time step.
c
        do i=1,numshells
        svelocity(i)=newsvelocity(i)
        end do
        parms(3)  = shellmass(1)
        parms(7)  = lt1
        parms(8)  = radt1
        parms(9)  = velt1
        parms(10) = testimpl
        parms(11) = Eload
        parms(12) = peakcur
        parms(14) = newLequiv
c
c
c For stagnant foil (constant radius and inductance):
c 
      else
c
c   j. Foil inductance
c
        lt1 = Lequiv
c
c   k. Time rate of change of inductance
c
        ldott2 = 0.0
c
c   l. Put some values into a common block so that they will be
c      available for plotting.
c
        shellke  = 0.0
        shellrad = minrad
        shellvel = 0.0
        shellacc = 0.0
        shellm   = shellmass(1)
c
      end if
c
c ----------------------------------------------------------------------
c
  999 format
     & (//
     & ' ------------------------------------------------------------'/
     & ' Foil radius has reached the minimum value.'/
     & '   time:                ', 1pe10.3/
     & '   KE at implosion:     ', 1pe10.3/
     & '   load energy:         ', 1pe10.3/
     & '   peak current:        ', 1pe10.3/
     & '   outer shell radius:  ', 1pe10.3)
c     & '   shell radii:      '/
c     & 10(15x,1pe10.3))
  998 format(/
     & ' ------------------------------------------------------------'/
     & //)
c
c ----------------------------------------------------------------------
c
      if (debug) then
      write(9,*)'T = ',time,'   L = ',newLequiv,'   KE = ',Ekinetic,
     &          '   Trapped Current = ',trapped
      nameforit='Shell Number'
      write(9,997)nameforit,(shell(i),i=1,numshells)
      nameforit='Shell Radius'
      write(9,996)nameforit,(shellradius(i),i=1,numshells)
      nameforit='Shell Velocity'
      write(9,996)nameforit,(svelocity(i),i=1,numshells)
      nameforit='Shell Acceleration'
      write(9,996)nameforit,(acceleration(i),i=1,numshells)
      nameforit='Shell Mass'
      write(9,996)nameforit,(shellmass(i),i=1,numshells)
      nameforit='Shell Current'
      write(9,996)nameforit,(shellcurr(i),i=1,numshells)
      write(9,*)
      endif
 997  format(a20,13x,10(i2,12x))
 996  format(a20,5x,10e14.4)
      return
      end

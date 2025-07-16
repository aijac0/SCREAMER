      subroutine cylfoil_model
c read only arguments
     & (time, dt, rdt, ilt2,
c read and write arguments
     &  parms,
c write only arguments
     &  ldott2, lt1)
c
c ----------------------------------------------------------------------
c
c  Modification:
c    01/03/94, KWS, add Mosher/Krisnan/Guiliani K-line radiation yield
c                   calculations
c    1995-04-29 MLK: write(5,999) changed to write (6,999)
c                    unit 5 is stdin, unit 6 is stdout
c    1997-08-12 KWS; added peak-current diagnostic
c    2014-02-06 RBS: Real*4 to real
c
c Summary:
c
c Screamer CYLINDRICAL foil implosion subroutine.
c   Treats r2 and l2 as variable elements (r2 is dl2/dt).
c
c   Define:
c     the next           full time step as t=i      (variable suffix t0)
c     the current        half time step as t=i-1/2  (variable suffix t1)
c     the previous       full time step as t=i-1    (variable suffix t2)
c     the previous       half time step as t=i-3/2  (variable suffix t3)
c     the 2nd previous   full time step as t=i-2    (variable suffix t4)
c     the 2nd previous   half time step as t=i-5/2  (variable suffix t5)
c
c Calculates the following values for an imploding foil:
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
c
c ----------------------------------------------------------------------
c
c Declare passed variables
c
      real       time     !simulation time, t=i
      real       dt       !time-step
      real       rdt      !1/dt
      real       ilt2     !current through this inductor at t=i-1
      real       parms(*) !model parameters for this circuit element
      real       ldott2   !time rate of change of inductance at t=i-1/2
      real       lt1      !inductance at t=i-1/2
c ----------------------------------------------------------------------
c
c Include files
c
      include   'zdemmax.h'   !parameters
      include   'zdemout.h'   !common blocks
c
c ----------------------------------------------------------------------
c
c Set some internal parameters
c
      real       implode,       end_implode
      parameter (implode = 0.0, end_implode = 1.0)
c
c Declare internal variables
c
      real       initrad   !initial radius
      real       minrad    !minimum radius
      real       mass      !mass in kilograms
      real       aconst    !-(mu*length)/(2*mass)
      real       lconst    !(mu*length)
      real       lminrad   !inductance at minimum radius
      real       lt3       !inductance from t=i-3/2
      real       radt1     !calculated radius,   t=i-1/2
      real       radt3     !radius from t=i-3/2
      real       velt1     !calculated velocity, t=i-1/2
      real       velt3     !velocity from t=i-3/2
      real       acct2     !calculated acceleration, t=i-1
      real       testimpl  !calculated current state of foil
                           ! (imploding or stagnated)
      real       length    !foil length in meters
      real       massg     !foil mass in grams
      real       peakcur   !peak liner current
c
c ----------------------------------------------------------------------
c
c Set the model parameters to understandable names.
c
      initrad   = parms(1)
      minrad    = parms(2)
      mass      = parms(3)
      aconst    = parms(4)
      lconst    = parms(5)
      lminrad   = parms(6)
      lt3       = parms(7)
      radt3     = parms(8)
      velt3     = parms(9)
      testimpl  = parms(10)
      length    = parms(11)
      massg     = parms(12)
      peakcur   = parms(13)
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
c
c For an imploding foil:
c
      if (testimpl .eq. implode) then
c
c   a. Foil acceleration
c
        acct2 = (ilt2 * ilt2 * aconst) / (radt3)
c
c   b. Foil velocity
c
        velt1 = velt3 + acct2*dt
c
c   c. Foil radius, check to see if less than minimum radius, if
c        so, print a message that this has occurred and set values
c        appropriately.  But keep going with the calculation since
c        the foil radius has changed this time step.
c
      radt1 = radt3  +  ((velt1+velt3) * dt * 0.5)
      if (radt1 .lt. minrad) then
        radt1    = radt3
         testimpl = end_implode
c         velt1 = abs(velt1/1.4142)
c         velt1 = 0.8944 * abs(velt1)
        foilke  = 0.5 * mass * velt1 * velt1
c
c   Write implosion status to the log file at device 9
c
      write(9,
     &  '(/A/,A/,A,1pe10.3/,A,1pe10.3/,A,1pe10.3/,A,1pe10.3/,A,
     &  1pe10.3/,A/)')
     &  ' ------------------------------------------------------------',
     &  ' Foil radius has reached the minimum value.',
     &  '   time:             ',time,
     &  '   minimum radius:   ',minrad,
     &  '   maximum KE:       ',foilke,
     &  '   peak velocity:    ',velt1,
     &  '   peak current:     ',peakcur,
     &  ' ------------------------------------------------------------'

      write(6,'(A)')
     &  ' ------------------------------------------------'
      write(6,'(A)')
     &  ' Foil radius has reached the minimum value.'
      write(6,'(A,1pe10.3)') '   time:             ',time
      write(6,'(A,1pe10.3)') '   minimum radius:   ',minrad
      write(6,'(A,1pe10.3)') '   maximum KE:       ',foilke
      write(6,'(A,1pe10.3)') '   peak velocity:    ',velt1
      write(6,'(A,1pe10.3)') '   peak current:     ',peakcur
      write(6,'(A)')
     &  ' ------------------------------------------------'
        end if
c
c   d. Foil inductance
c
        lt1 = lconst * log(initrad/radt1)
c
c   e. Time rate of change of inductance
c
        ldott2 = (lt1 - lt3) * rdt
c
c   f. Put some values into a common block so that they will be
c      available for plotting.
c
        foilke  = 0.5 * mass * velt1 * velt1
        foilrad = radt1
        foilvel = velt1
        foilacc = acct2
c
c   g. Save all values required for next time step.
c
        parms(7)  = lt1
        parms(8)  = radt1
        parms(9)  = velt1
        parms(10) = testimpl
        parms(13) = peakcur
c
c Calculate the radiation yield for aluminum, argon, copper, krypton,
C  and xenon
c
      if (radyields) then
         call yield(13.,mass,radt1,length,foilke,eta,T,yw_al,ym_al)
         call yield(18.,mass,radt1,length,foilke,eta,T,yw_ar,ym_ar)
         call yield(29.,mass,radt1,length,foilke,eta,T,yw_cu,ym_cu)
         call yield(36.,mass,radt1,length,foilke,eta,T,yw_kr,ym_kr)
         call yield(54.,mass,radt1,length,foilke,eta,T,yw_xe,ym_xe)
      endif
c
c For stagnant foil (constant radius and inductance):
c 
      else
c
c   d. Foil inductance
c
        lt1 = lminrad
c
c   e. Time rate of change of inductance
c
        ldott2 = 0.0
c
c   f. Put some values into a common block so that they will be
c      available for plotting.
c
        foilke  = 0.0
        foilrad = minrad
        foilvel = 0.0
        foilacc = 0.0
c
      end if
c
c ----------------------------------------------------------------------
c
      return
      end

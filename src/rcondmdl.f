      Subroutine rcond (ht, time, curr, parms, rvar)
c
c     Resistive Conductor Model
c
c     Date/Author: 2019-12-30 Rick Spielman
c
c     Modifications:
c
c     2020-02-12 RBS: Fixed minor error in the resistance math.
c
c     This routine calculates the resistive losses in a conductor based on the
c     energy delivered. Time step by time step the conductor temperature
c     increases and the the resistivity increases. This changes the resistance
c     used for that time step.
c
c     This model is valid only as long as the conductor remains in a solid,
c     solid/liquid, liquid, or liquid/gas phase. Once a plasma is formed then
c     the resistivity collapses.
c
c     This is a simplied model in which the inputted action changes the total
c     Action delivered to the load. The resulting resistance is only a
c     function of added action.
c
c Declare passed parameters
c
      real ht, time, curr, parms(*), rvar
c
c Declare internal inputted variables
c
      real R1, ER1, ER2, alpha1, alpha2, alpha3, ER
c
c     Declare remaining internal variables
c
      real Delta_E, R, R2, R3

      R1          = parms(1)
      ER1         = parms(2)
      ER2         = parms(3)

      alpha1      = parms(4)
      alpha2      = parms(5)
      alpha3      = parms(6)

      ER          = parms(7)
c
c     Define R2 and R3
c
      R2 = R1 + alpha1*ER1
      R3 = R2 + alpha2*(ER2-ER1)

c      write(6,*) 'R1= ',R1, ' R2= ', R2, ' R3= ',R3
c      write(6,'(a,1pe10.3,a,1pe10.3,a,1pe10.3)') 'alpha1= ',alpha1,
c     &           ' alpha2= ',alpha2,
c     &           ' alpha3= ',alpha3

c
c     Set initial value of rvar before first use
c
      if ( time .le. ht ) then
         rvar = R1
      end if
c
c     Calculate the input energy (action) from the past time step
c
c      write(6,*) 'curr =', curr, 'rvar =', rvar, 'ht =', ht

      Delta_E = curr * curr * rvar * ht

c      write(6,*) 'Delta_E =', Delta_E
c
c     Update Total Action in the problem for next time step
c
      ER = ER + Delta_E
c      write(6,*) 'ER =', ER
c
c     Store the new total action
c
      parms(7) = ER
c
c     Calculate the new resistance based on the new total action
c

      if ((ER .ge. 0.0 ) .and. (ER .LE. ER1)) then
         R = R1  + alpha1*ER
      else if ((ER .gt. ER1) .and. (ER .le. ER2)) then
         R = R2 + alpha2*(ER - ER1)
      else if (ER .gt. ER2 ) then
         R = R3 + alpha3*(ER - ER2)
      end if

c      write(6,*) 'R= ', R
c
c     Pass back the new value of R
c
      rvar = R

      return
      end

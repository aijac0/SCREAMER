      SUBROUTINE  tsw_model (ht, time, parms, rvar, currentr2)
c
c  January 26, 1993;     hnw
c
c  Modifications:
c 1994-03-07 KWS:  Put values that are needed from one use of
c  the routine to the next in variable arrays.  These arrays are also available
c  for output.  Deleted versions 2, 3, 4, so that only this routine is used when
c  the model is called.  Note that the arrays are specific to each call of the
c  model, so that there is no cross-talk between different uses.
c 2014-05-02 RBS: Change real*4 to real
c 2015-06-23 RBS: diel1 explicit integer math in line 54. dcm1n unused
c
      use zdemmax           ! parameters
      include 'zdemout.h'   ! common blocks
c
c Declare passed variables
c
      REAL  ht, time, parms(*), rvar, currentr2
c
c Declare internal variables in parms
c
      INTEGER  diel1
      real  tbd1, dm1, atm1, xsw1, xch1, irta1, rchn1
c
c Declare internal variables
c
      REAL  IAMP1, MHOG1, IGAUS1, CONA1, IRTB1, RESC1, pie
      REAL  ZETA1, MHOS1, RHO1, RHOB1, sf, psia1, dcm1
c
      PARAMETER (pie=3.14159265)
c  
c     Switch loss model subroutine for screamer.  This model uses the Braginskii
c     formulations as modified by T H Martin to determine the switch loss  in
c     H2O (1),OIL (2),SF6 (3),AIR (4), HE (5), OR H2 (6).  The model calculates 
c     the channel radius and then provides the channel resistance by assuming the
c     appropriate constant plasma conductivity between  150 and 600 mhos-cm.  
c     The input of switch current is taken to the 2/3 power then integrated
c     and multiplied by the appropriate constant to provide the channel radius
c     squared.   
c
c
c     This model is based on the screamer module of RLS and modifies the
c     resistor R2.
c   
c     The input data consists of the type of dielectric,
c      diel1 =    H2O (1), OIL(2), SF6 (3), AIR (4), HE (5), or H2 (6);
c      TBD1  = the breakdown time in seconds;
c      DM1   = the total gap length d in m;
c      ATM1  = the gas pressure in bars.  Use 1.0 for H2O or oil.
c      xsw1  = no of sw modules
c      xch1  = no of channels per sw
c      irta1 = in parms
c      rchn1 = radius of channel (m)
c     The switch is active for time greater than or equal to TBD1.
c
c     The density RHOB1 is in units of g/cm3 (at what temp and pressure)
c Clearly we input pressure in bars but 1 bar is not one atmosphere at NPT
c 20 °C, 101.325 kPa
c  
c     This model was developed  by T H Martin.
c
      diel1    = int(parms(1))
      tbd1     = parms(2)
      dm1      = parms(3)
      atm1     = parms(4)
      xsw1     = parms(5)
      xch1     = parms(6)
      irta1    = parms(7)
c Passed back value for rchn1 not used just here for clarity
      rchn1    = parms(8)
         
c
c Convert to cm and psia
c
      dcm1  = dm1 * 100.0
      psia1 = atm1 * 14.70
c
      GO TO (10,20,30,40,50,60), DIEL1
c      SETTING UP FOR THE VARIOUS DIELECTRICS
c
c bad fortran use if then else
c
c
c Note: the standard atmosphere for air is 1.225e-3 g/cm3 at 1 atm
c 1 atm = 1.01325 bar = 1.01325e5 Pa, 1 bar is defined as 1e5 Pa
c
  10    CONTINUE    !  H2O
        ZETA1=4.5
        MHOS1=600.
        RHOB1=1.
        GO TO 100
c
  20      CONTINUE  !  OIL
        ZETA1=4.5
        MHOS1=600.
        RHOB1=.9
        GO TO 100
c
  30     CONTINUE   !  SF6
        ZETA1=4.5
        MHOS1=160.
        RHOB1=6.16E-3
        GO TO 100
c
  40    CONTINUE    !  AIR
        ZETA1=4.5
        MHOS1=200.
        RHOB1=1.2929E-3
        GO TO 100
c
  50      CONTINUE   !  HE
        ZETA1=4.5
        MHOS1=140.
        RHOB1=1.78E-4
        GO TO 100
c
  60      CONTINUE   !  H2
        ZETA1=4.5
        MHOS1=300.
        RHOB1=8.99E-5
        GO TO 100
c
c      CALCULATE VALUES FOR USE
c
c Formula actually scales the density at 1 bar by the number of bars.
c calculates RHO1 = (RHOB1/14.7) * PSIA1
c This is stupid as atm1 is already in bars
c
  100   RHO1  = RHOB1/14.7*PSIA1
        MHOG1 = MHOS1 * 0.8985E12
        CONA1 = (4./MHOG1/RHO1/(pie*pie)/ZETA1)**.1667
        sf = (xsw1*xch1)**.3333333
c   
c       PUTTING IN THE BREAKDOWN TIME FOR THIS SWITCH

         IF(TIME.LT.TBD1) THEN
           rvar   = 1.0E6
           rchn1  = 0.0
           goto 200
         ELSE
           IAMP1 = ABS(CURRENTR2)
           IGAUS1=  IAMP1*3.0E9
           IRTB1 = (ht*(IGAUS1**(.66667)))+IRTA1
           IRTA1 = IRTB1
           rchn1 = CONA1*IRTB1**(.5)+1.0E-5
c           write(6,'(A,1pe10.3)') 'thmswmdl: channel radius= ', rchn1
c Are the units right here? below dcm1 in cm, rchn1 in m input but input
c value is not used. calc for rchn1 is in cm here but converted to m for
c output to parms

           resc1 = dcm1/rchn1**2/mhos1/pie/sf
         end if
c
c       PROVIDING OUTPUTS FOR SCREAMER
c
         rvar   = resc1
c
c       Save values for next time step
c
 200     continue
         parms(7) = irta1
c
c       Save switch channel radius in meters
c
         parms(8) = rchn1/100.0
         radch1 = rchn1/100.0
c
         RETURN
         END

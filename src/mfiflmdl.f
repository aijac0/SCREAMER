      Subroutine zmfi_model (time, cin, cout, vin, parms, gvar)
c
c     Subroutine MFI
c
c      Written by Ken Struve, Sept. 30, 1993.
c
c      Modified Mar. 7, 1994.  Put all variables that are needed from
c      one time-step to the next in variable element arrays that are
c      specific to each call of the routine.  Therefore, this model may
c      be called more than once in an input.
c
c      Modifications:
c 1997-08-12 KWS: Added a bmin parameter to test bfld against,
c                 rather than testing against 0.0
c 2014-02-06 RBS: Changed real*4 to real
c 2014-05-05 RBS: Added internal variable definition
c 2014-06-23 RBS: Used cokuttmp = cout to avoid a compiler warning
c
c      Flash criterion supplied by Dillon McDaniel.  See also  J. P.
c      VanDevender, "Power Flow for Vacuum Insulated Inductive Loads,"
c      Proceedings of the 1981 Pulsed Power Conference, p. 248.
c
c      This routine calculates the criterion for flashover of the MFI
c      insulator, and then causes the insulator to crowbar once that
c      criterion is exceeded. Specifically, the insulator will flash
c      when the quantity  Eparallel/(cB) exceeds 0.07, or Estack/(cB)
c      exceeds 0.09 for a 45 deg insulator. Once the insulator flashes
c      rest of the run.  The routine also checks to see if the E field
c      is high enough to flash before the test is applied.  Once the
c      insulator flashes, the resistance to ground is identical to the
c      exponential decay model described in the SCREAMER manual.  tsw is
c      determined at the switch time, and tau is internally set
c      (see parameter list).
c
c
c      Input parameters are:
c
c      rad  = inner radius of the MFI insulator (m)
c      d    = insulator thickness (m)
c      gmin = minimum (switch open) conductance (mhos)
c      gmax = maximum (switch closed conductance (mhos)
c      ni   = number of insulators in parallel
c      icbflag = 1 for switch to crowbar, otherwise it will not crowbar
c
c      This allows calculating the MFI criteria without causing the
c      insulator to flash
c
c      Output parameters are:
c
c      efld  =  E field across the insulator (V/m)  (Not E parallel)
c      bfld  =  B field at the insulator  (wb/m2)
c      xmfi  =  the mfi switch criterion
c      gvar  =  the conductance of the insulator
c
c Define included variables
c
      use zdemmax             !parameters
      include   'zdemout.h'   !common blocks
c
c
c Define passed variables
c
      real   time, cin, cout, vin, parms(*), gvar
c
c
c Define internal variables
c
      real c, critvalu, tau, unot, pi, emin, bmin
      real rad, d, gmin, gmax, xni, cbflag, swclosed, tsw
      integer icbflag
      real  cperunit, rmin, rmax, rsistanc
c
c Set variable parameters
c
      parameter (c=2.997925e8,critvalu=0.09,tau=0.5e-9)
      parameter (unot=1.256637e-6,pi=3.141593,emin=5.0e6)
      parameter (bmin=1.0e-6)
c
c Use couttmp to prevent a compiler warning
c
      couttmp = cout
c
c
      rad      = parms(1)
      d        = parms(2)
      gmin     = parms(3)
      gmax     = parms(4)
      xni      = parms(5)
      cbflag   = parms(6)
      swclosed = parms(7)
      tsw      = parms(8)
c
      icbflag = INT(cbflag)
c
c
c     Find E, B, and the flash condition
c
c
      efld = vin/d
      cperunit=cin/xni
      bfld = unot*cperunit/(2.0*pi*rad)
      if (abs(bfld).le. bmin) then
         xmfi=0.0
        else
         xmfi=abs(efld/(c*bfld*critvalu))
        endif
c
c      Skip tests if you don't want insulator to crowbar
c
      if (icbflag .eq. 1) then
         continue
        else
         goto 400
        endif
c
c      Test to determine whether insulator has already crowbar'd
c
      if (swclosed .eq. 1.0) then
         goto 500
        else
         continue
        endif
c
c      Test: determine whether voltage is high enough to flash insulator
c
      if (abs(efld).ge.emin) then
         continue
        else
         goto 400
        endif
c
c     Determine whether insulator is magnetically insulated or not
c
      if (xmfi .gt. 1.0) then
         continue
        else
         goto 400
        endif
c
c     Set switch time and switch closed flag
c
      swclosed = 1.0
      tsw = time
      goto 500
c
c
c     Switch open condition
c
c
400   gvar=gmin
      goto 600
c
c
c     Switch closed condition
c
c
500   rmin=1.0/gmax
      rmax=1.0/gmin
      rsistanc=rmin+(rmax-rmin)*exp(-(time-tsw)/tau)
      gvar=1.0/rsistanc
      goto 600
c
c
600   continue
c
c     Save values for the next time step
c
      parms(7) = swclosed
      parms(8) = tsw
c
c     Store output parameters
c
      parms(9)  = efld
      parms(10) = bfld
      parms(11) = xmfi
c
      return
      end
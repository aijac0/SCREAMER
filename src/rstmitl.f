      subroutine reset_mitl (index)
c
c ---------------------------------------------------------------------
c
c Resets the conductances in an MITL.
c This model uses circumference of feed and gap, where gap is assumed
c constant.
c
c ---------------------------------------------------------------------
c
c Modifications:
c KWS, 08/12/97, Changed 'esat' parameter, definitions of v1 and i2
c KWS, 03/02/94, Based on a simple LC network.  Checked physics, Looks OK.
c KWS, 05/30/95, Added loss current density to the anode for Zflow Plasma
c                loss model
c 2014-05-02 RBS: Changed integer*4 to integer
c 2014-05-07 RBS: All internal variables explicitly defined
c 2015-06-23 RBS: Removed unused variables edenomv1, gg, fixed a typo on
c                 the real declaration of zmuo (was zmu0)
c 2015-06-23 RBS: Initialized area to 0 to stop a compile warning
c                 Explicit real definition of area
c 2018-02-22 RBS: Changed the length of the element section from Tau to
c                 tresline, we need the conductivity per resolution
c                 element per node not the entire line - As per Pace
c                 VanDevender
c 2018-02-22 RBS: rstmitl.f gives neg loss current. Not possible.
c                 created a new definition of the loss current
c 2018-03-13 RBS: Inititalized anodeloss.
c 2018-03-14 RBS: Outputing the anode loss current and the loss current
c                 density.
c 2018-12-28 RBS: Corrected the average current calculations in lines
c                 141-143 so that they are divided by 2.
c 2019-01-18 RBS: Found and removed the extra 1/16 that replaced the two
c                 corrected current average 0.5X's in parameter con1.
c                 Went back to esat = 4e7 to get the losses back up.
c 2019-01-21 RBS: Removed cup and cdn that were part of the removed
c                 Zflow routine inputted by Ken Struve.
c
c ---------------------------------------------------------------------
c
c
c Include files
c
      use zdemmax
      include 'zdemcomm.h'
      include 'zdemwork.h'
c
c Define passed variables 
c
      integer  index
c
c Define internal variables
c
      real pi, zmuo, epso, cspeed, esat, con1, bm, xy
c
      parameter (pi      = 3.1415927)
      parameter (zmuo    = 4.0e-7 * pi)
      parameter (epso    = 8.8541878e-12)
      parameter (cspeed  = 2.99792458e+8)
      parameter (esat    = 4.0e+7)
c      parameter (esat    = 6.0e+7)
c      parameter (con1    = 0.0625 * zmuo * zmuo)
      parameter (con1    = zmuo * zmuo)
c Transport coefficients - default Screamer
      parameter (bm      = 0.5)
      parameter (barg    = 1.21)
      parameter (xy      = 0.9 * bm)
c
      real       circum, gap, tau, rcircum2, rgap, rgap2, eturnon
      real       elec_len, area, elec_area, e1, emp1, v1mv, a1,
     &           zir1, zir2, argp, con2, b1s, bc1s, rbc1s, arg1, g1x,
     &           fung1, xx, ftail, g1y, anodeloss, alossden

      ibranch   = indexmitl(1,index)
      iblock    = indexmitl(2,index)
c starting node for MITL element
      node1     = indexmitl(3,index)
c ending node for MITL element depends on # resolution elements
      node2     = indexmitl(4,index)
c
c Input parameters
c
      circum    = pin(1,iblock,ibranch)
      gap       = pin(2,iblock,ibranch)
      tau       = pin(3,iblock,ibranch)
c     z         = pin(4,iblock, ibranch) is not used in this subroutine
      tresline  = pin(5,iblock,ibranch)
      eturnon   = pin(9,iblock,ibranch)
c
c Output parameters
c rcircum2 = 1/circum**2
      rcircum2  = pin(6,iblock,ibranch)
c rgap = 1/gap
      rgap      = pin(7,iblock,ibranch)
c rgap2 = 1/gap*gap
      rgap2     = pin(8,iblock,ibranch)
      area = 0.0
      anodeloss = 0.0
      
      elec_len  = tresline * cspeed
      elec_area = elec_len * circum

      if (eturnon .lt. esat) then
         edenom  = 1.0 / (esat - eturnon)
      else
         edenom  = 1.0e-12
      endif
c
c Reset the conductances from the beginning node node1
c to the final node node2.
c Create an average of the voltages of present and past timestep.
c Remove abs in def of voltage, if v<0 then no emission?
c
      do node = node1, node2
        area = elec_area
        v1   = 0.5 * abs ( v(node,ibranch) + vold(node,ibranch) )
c        v1   = 0.5 * ( v(node,ibranch) + vold(node,ibranch) )
c ? polarity of e1
        e1   = v1 * rgap
c
c
c Changing the value of eturnon or esat changes emp1
c increasing esat from 4e7 to 6e7 HALVES edenom, doubles emp1
c this doubles the conductance g
c RBS 2019-01-15 the above statement does not make sense. HALVING
c edenom will HALVE emp1 NOT double it.
c
        if ( e1 .ge. eturnon ) then
          if ( e1 .gt. esat ) then
            emp1 = 1.0
          else
            emp1 = (e1-eturnon) * edenom
          end if
          v1mv = 0.00001 + 1.0e-6*v1
c as v1mv increases above 0.09855 the value of a1 slowly decreases from
c 1.5. Even at v1m1 = 2, a1 is only reduced to 1.35.
c over the range of v1mv= 0.09855 to 10 a1=1.5 to 1.27
c a very weak function of v1mv
c
          if ( v1mv .gt. 0.09855 ) then
            a1 = 1.38 * ( v1mv + 0.0001 )**(-0.036)
          else
            a1 = 1.5
          end if
c
c zir1 = node averaged old current
c zir2 = node averaged new current
c zir1 + zir2 = node averaged old & new current
c Should the zir and zirn be divided by 2 to provide a real average
c Also should the final version of zir1 be divided by 2 as an average
c Doing so gives non realistic losses
c 1/4 zir1 in line 157 is 1/16 b1s, 1/16 arg1, and 1/1024 g1x
c
c         zir1  = ( zir(node-1,ibranch) + zir(node,ibranch) )
c         zir2  = ( zirn(node-1,ibranch) + zirn(node,ibranch) )
c         zir1  = ( zir1 + zir2 )
          zir1  = 0.5 * ( zir(node-1,ibranch) + zir(node,ibranch) )
          zir2  = 0.5 * ( zirn(node-1,ibranch) + zirn(node,ibranch) )
          zir1  = 0.5 * ( zir1 + zir2 )
c the physical meaning of argp is unclear
          argp  = area * rgap2
          con2  = 1.113e-5 * rgap2
c
c b1s is 0.0625 times mu0**2 times the linear current density squared
c this is a constant times Bfield**2 , con1 = 0.0625 * mu0**2
c 1/0.0625 = 16, this divides zir**2 by 16. For the original case with
c zir averaged incorrectly without the 0.5 in each line,
c this corrects the value of b1s.
c
          b1s   = con1 * zir1 * zir1 * rcircum2 + 1.0e-6
c
c what about the polarity of v1mv?
c if v1mv is negative then bc1s and rbc1s can be negative
c bc1s is effectively a constant times E**2 in (MV/m)**2 if the units
c of 1.022 are MV, (1.113e-5? very close to 1/299.8**2, speed of light)
c The v1mv**2 term becomes E/c in mks with some algebra.
c I don't know about the 1.022*v1mv term as it is added to bc1s
c
          bc1s  = con2 * (1.022*v1mv + v1mv*v1mv + 0.0001)
c
c     ref:  R. V. Lovelace and E. Ott, Pys. Fluids, 17, 1263 (1974)
c           (Gaussian);
c           also, J. M. Creedon, J. Appl. Phys, 48, 1072 (1977) (MKS).
c
          rbc1s = 1.0 / bc1s
c
c     exp with no attenuated tail - nominal parameters
c if b1s is 1/16 then arg1 will be 1/16 and g1x 1/1024
c barg = 1.21
c arg1 is EXACTLY B pressure/ E pressure = (cB)**2/E**2
c
          arg1 = barg * b1s * rbc1s
          g1x  = arg1**2.5
c
c     super exponential g model with attenuated tail
c
c In the case where g1x is 1/1024 can fung1 ever reach 0.0?
c When the voltage is small v1mv = 0, rbc1s = 1000, arg1=1210*b1s
c When can g1x be negative? Since arg1 is pressure balance the answer
c is no.
c lowering g1x by 1024 will dramitically impact the value of fung1
c
          if (g1x .gt. 40.0) then
            fung1 = 0.0
          else if (g1x .lt. -40.0) then
            fung1 = 2.35e17
          else
            fung1 = exp(-g1x)
          endif
c
c If b1s is 1/16 then xx is 1/16, bm = 0.5
c here xy is a constant = .45, this means that changes in the definition
c of zir and b1s dramatically changes the ftail calculation.
c
          xx = bm * (b1s * rbc1s)
          if (xx .gt. xy) then
              if ((xx-xy).gt. 4.0) then
                ftail = 0.0
              else
                ftail = exp(-10.0*(xx-xy))
              endif
          else
            ftail = 1.0
          end if
          fung1 = fung1 * ftail
c
c (v1mv**(a1-1)) can only vary v1mv = .1 ()=.31 to v1mv=10 ()= 1.86
c really g1y depends strongly only on fung1
c
          g1y = 1.99e-3 * (v1mv**(a1-1.0)) * argp * fung1
c
c emp1 varies linearly with (e1-eturnon)/(esat-eturnon)
c increasing e1 above eturnon always increases emp1 and increases g
c increasing esat decreases emp1 amd decreases g
c
          g(node,ibranch) = g1y * emp1
        else
          g(node,ibranch) = 0.0
        end if
c
c Develop a running total of loss current from each node, use the g at
c that node and the average voltage v1 at that node to calculate the
c loss current at that node. Sum this current up for anodeloss inside
c the do loop.
c
        anodeloss = anodeloss + v1 * g(node,ibranch)
c
      end do

      pin(11,iblock,ibranch) = anodeloss
      alossden               = anodeloss/( area*float(node2-node1+1) )
      pin(10,iblock,ibranch) = alossden
c
c ------------------------------------
c
      return
      end

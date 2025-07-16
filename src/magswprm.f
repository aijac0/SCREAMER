      subroutine magparm (parms, nparms)
c
c  December 10, 1992;     hnw
c 2014-02-06 RBS: Changed real*4 to real
c
c Sets up the parameters needed for the magnetic switch model and
c returns them in parms(i).
c nparms is the number of parameters in parms.
c
c parms is sent with the basic parameters needed to rearrange and fill
c the actual parms array.
c
c Define passed variables
c
      real       parms(*)
      integer    nparms
c
c Define internal variables
c
      real       temp(9), pi, tpi, zmu0, bh1, zmu
c
c Basic EM parameters:
c
      parameter (pi   = 3.1415927)
      parameter (tpi  = 2.0*pi)
      parameter (zmu0 = 4.0e-7*pi)
c
c Fill temp with parms, then rearrange parms and fill it in.
c
      do i = 1, nparms
        temp(i) = parms(i)
      end do
c
c In temp:
c 1: pf
c 2: ri
c 3: ro
c 4: w
c 5: h1
c 6: hsat
c 7: hrev
c 8: bsat
c 9: lold
c
c In parms we want:
c  1: ziaold
c  2: ziamax
c  3: iold
c  4: lold
c  5: ri
c  6: ro
c  7: 1/ri
c  8: 1/ro
c  9: (ro-ri)
c 10: alog(ro/ri)
c 11: ziatest = ri * tpi* hsat
c 12: tpih1 = tpi* h1
c 13: tpihsat = tpi * hsat
c 14: tpihrev = tpi * hrev
c 15: rtpih1 = 1.0 / (tpi * h1)
c 16: rtpihsat = 1.0 / (tpi * hsat)
c 17: rtpihrev = 1.0 / (tpi * hrev)
c 18: zmue = 1.0 + (pf*(zmu-1.0))
c 19: zmuelog = zmue * alog(ro/ri)
c 20: zmuem1  = zmue - 1.0
c 21: zmue1m  = 1.0 - zmue
c 22: wmu0 = w * zmu0 / tpi
c
      nparms    = 22
      parms(1)  = 1.0e-20
      parms(2)  = 1.0e-20
      parms(3)  = 0.0
      parms(4)  = temp(9)
      parms(5)  = temp(2)
      parms(6)  = temp(3)
      parms(7)  = 1.0 / temp(2)
      parms(8)  = 1.0 / temp(3)
      parms(9)  = temp(3) - temp(2)
      parms(10) = alog (temp(3)/temp(2))
      parms(11) = temp(2) * tpi * temp(6)
      parms(12) = tpi * temp(5)
      parms(13) = tpi * temp(6)
      parms(14) = tpi * temp(7)
      parms(15) = 1.0 / parms(12)
      parms(16) = 1.0 / parms(13)
      parms(17) = 1.0 / parms(14)
      bh1       = temp(5) * zmu0
      zmu       = (temp(8) - bh1) / (zmu0*(temp(6)-temp(5)))
      parms(18) = 1.0 + temp(1)*(zmu-1.0)
      parms(19) = parms(18) * parms(10)
      parms(20) = parms(18) - 1.0
      parms(21) = 1.0 - parms(18)
      parms(22) = temp(4) * zmu0 / tpi
c
      return
      end

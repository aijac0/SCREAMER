      subroutine smagsw_model (rdt, inew, parms, dldt, dlidi, lvar)
c
c 1992-12-10 HNW: Created the subroutine
c 2014-02-06 RBS: Changed real*4 to real
c 2014-05-07 RBS: Defined reals explicitly
c
c Magnetic switch model of a saturable core inductor.
c Note: We have just solved for the current at the last time step (Tnew).
c       The timestep previous to that is Told.
c       T1/2 is the last half-timestep:  0.5 * (Tnew + Told)
c       T-1/2 is the half timestep previous to that:  T1/2 - timestep
c Input:
c   rdt = 1 / timestep
c   inew = current through inductor at last time step.
c   parms = array of parameters for each inductor.
c     1: old switch current
c     2: max switch current
c     3: old problem current
c     4: old inductance
c     5: inner radius (m)
c     6: outer radius (m)
c     7: 1/ri
c     8: 1/ro
c     9: ro-ri
c    10: ln (ro/ri)
c    11: test current for hysteresis
c    12: tpi*h1
c    13: tpi*hsat
c    14: tpi*hrev
c    15: 1/tpi*h1
c    16: 1/tpi*hsat
c    17: 1/tpi*hrev
c    18: rel. permeability of core
c    19: mu*ln(ro/ri)
c    20: mu-1
c    21: 1-mu
c    22: width*mu0/tpi
c
c Output:
c   Inductance at T1/2, lvar.
c   Time rate of change of inductance, dldt = d(inductance)/d(time)).
c   Change in flux with respect to change in current,
c     dlidi = d(flux)/d(current)
c
c
c ***** Valid for I > 0 only (we will take the absolute value of the
c       current and set it to some small positive value if it is zero.
c       For dI/dt < 0  &  I = 0, L has singularity.
c
c Declare passed variables
c
      real       rdt, inew, parms(*), dldt, dlidi, lvar
c
c Declare internal variables
c
      real       lvarold, iold, ihalf, zia, ziamax, ziaold, ziatest,
     &           rtpih1, rtpihsat, rc1, rc2, rc3, alogrori, rri, tpih1,
     &           zmue, zmue1m, rzia, rrc1, rrc2, rrc3, tpihsat, rori,
     &           zmuelog, zmuem1, rtpihrev, wmu0
c
c Set the switch parameters to constants, always need these,
c only set the rest if we need them in the if block.
c
      ziaold   = parms(1)
      ziamax   = parms(2)
      iold     = parms(3)
      lvarold  = parms(4)
      ri       = parms(5)
      ro       = parms(6)
      ziatest  = parms(11)
c
c Find the current at T1/2 and make sure it is positive.
c
      ihalf = 0.5 * (inew + iold)
      zia  = amax1 (1.0e-20, abs (ihalf))
c
c Reset the maximum current so far, if necessary.
c
      ziamax = amax1 (ziamax, ziaold)
c
c
c ***** Max current not to point where L will come down different path
c       for dI/dt < 0 (no hysteresis effect)  or  max current is past this
c       point but current is still increasing.
c
      if ((ziamax .le. ziatest) .or.
     &     ((ziamax .gt. ziatest) .and. (zia .ge. ziamax))) then
        rtpih1   = parms(15)
        rtpihsat = parms(16)
        rc1 = zia  * rtpih1
        rc2 = zia  * rtpihsat
c ----------------------------------------
        if (rc1 .le. ri) then
c
c ---  rc2 < rc1 <= ri < ro
c
          alogrori = parms(10)
          dlidi    = alogrori
          lvar     = dlidi
c ----------------------------------------
        else if (rc1 .le. ro) then
          if (rc2 .le. ri) then
c
c ---  rc2 < ri < rc1 <= ro
c
            rri    = parms(7)
            tpih1  = parms(12)
            zmue   = parms(18)
            zmue1m = parms(21)
            rzia   = 1.0 / zia
            rrc1   = rzia * tpih1
            dlidi  = zmue*alog(rc1*rri) + alog(ro*rrc1)
            lvar   = dlidi + zmue1m*(1.0-ri*rrc1)
c ----------------------------------------
          else
c
c ---  ri < rc2 < rc1 <= ro
c
            rri     = parms(7)
            tpih1   = parms(12)
            tpihsat = parms(13)
            zmue    = parms(18)
            zmue1m  = parms(21)
            rzia    = 1.0 / zia
            rrc1    = rzia * tpih1
            rrc2    = rzia * tpihsat
            dlidi   = alog(ro*rrc1*rc2*rri) + zmue*alog(rc1*rrc2)
            lvar    = dlidi + zmue1m*ri*(rrc2-rrc1)
          end if
c ----------------------------------------
        else
          if (rc2 .le. ri) then
c
c ---  rc2 < ri < ro < rc1
c
            rori    = parms(9)
            tpih1   = parms(12)
            zmuelog = parms(19)
            zmue1m  = parms(21)
            rzia    = 1.0 / zia
            rrc1    = rzia * tpih1
            dlidi   = zmuelog
            lvar    = dlidi + zmue1m*rori*rrc1
c ----------------------------------------
          else if (rc2 .le. ro) then
c
c ---  ri <= rc2 <= ro < rc1
c
            rri     = parms(7)
            tpih1   = parms(12)
            tpihsat = parms(13)
            zmue    = parms(18)
            zmuem1  = parms(20)
            rzia    = 1.0 / zia
            rrc1    = rzia * tpih1
            rrc2    = rzia * tpihsat
            dlidi   = alog(rc2*rri) + zmue*alog(ro*rrc2)
            lvar    = dlidi + zmuem1*(ri*(rrc1-rrc2) + (1.0-rrc1*ro))
c in this else and the next the variable zmuem1 is used instead of the
c zmue1m in the prior loops. It does not matter as these variables
c are used only in the loop but I think it is an error
c ----------------------------------------
          else
c
c ---  ri < ro < rc2 < rc1
c
            rori     = parms(9)
            alogrori = parms(10)
            tpih1    = parms(12)
            tpihsat  = parms(13)
            zmuem1   = parms(20)
            rzia     = 1.0 / zia
            rrc1     = rzia * tpih1
            rrc2     = rzia * tpihsat
            dlidi    = alogrori
            lvar     = dlidi + zmuem1*(rrc2-rrc1)*rori
          end if
c ----------------------------------------
        end if
c ----------------------------------------
c
c ***** Max current past point where L will come down different path
c       for dI/dt < 0  and  current is decreasing.  This is the
c       hysteresis effect section.
c
      else
        rtpihrev = parms(17)
        rc3   = zia  * rtpihrev
c ----------------------------------------
        if (rc3 .le. ri) then
c
c ---  rc3 < ri < ro
c
          rori    = parms(9)
          tpih1   = parms(12)
          tpihsat = parms(13)
          tpihrev = parms(14)
          zmuelog = parms(19)
          zmue1m  = parms(21)
          rzia    = 1.0 / zia
          rrc1    = rzia * tpih1
          rrc2    = rzia * tpihsat
          rrc3    = rzia * tpihrev
          dlidi   = zmuelog
          lvar    = dlidi + (rrc1-rrc2+rrc3)*zmue1m*rori
c ----------------------------------------
        else if (rc3 .le. ro) then
c
c ---  ri <= rc3 < ro
c
          rri     = parms(7)
          rori    = parms(9)
          tpih1   = parms(12)
          tpihsat = parms(13)
          tpihrev = parms(14)
          zmue    = parms(18)
          zmuem1  = parms(20)
          rzia    = 1.0 / zia
          rrc1    = rzia * tpih1
          rrc2    = rzia * tpihsat
          rrc3    = rzia * tpihrev
          dlidi   = alog(rc3*rri) + zmue*alog(ro*rrc3)
          lvar    = dlidi + zmuem1*((rrc2-rrc1)*rori-(ro-rc3)*rrc3)
c ----------------------------------------
        else
c
c ---  rc3 >= ro
c
          rori     = parms(9)
          alogrori = parms(10)
          tpih1    = parms(12)
          tpihsat  = parms(13)
          zmuem1   = parms(20)
          rzia     = 1.0 / zia
          rrc1     = rzia * tpih1
          rrc2     = rzia * tpihsat
          dlidi    = alogrori
          lvar     = dlidi + zmuem1*(rrc2-rrc1)*rori
c ----------------------------------------
        end if
c ----------------------------------------
      end if
c
c Now multiply by wmu0 to get actual values.
c
      wmu0  = parms(22)
      lvar  = lvar  * wmu0
      dlidi = dlidi * wmu0
c
c Reset parameters for the next call.
c
      parms(1) = zia
      parms(2) = ziamax
      parms(3) = inew
      parms(4) = lvar
c
c Now find dL/dt
c
      dldt  = (lvar - lvarold) * rdt
c
      return
      end

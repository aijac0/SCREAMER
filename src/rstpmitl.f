      subroutine reset_pmitl (index)
c
c  Change log
c
c  2014-02-06 RBS: Changed real*4 to real
c
c Resets the conductances in an MITL.  Uses the perveance value.
c Per LXS and DHM SCEPTRE model.
c
c Define passed variables
c
      integer  index
c
c Include files
c
      include 'zdemmax.h'
      include 'zdemcomm.h'
      include 'zdemwork.h'
c
c Define internal variables
c
c
      real vcap, efield, iave, vcapmv, res2, cond2, epi, epi2, u,
     &     hrelfu, gc, epi2res2, shuntg
c
      real  delperv, delt, zline, gap
c
      real alpha1, alpha2, alpha3, alpha4,
     &     beta1, beta2, beta3, beta4,
     &     emass, remass, v2mv,
     &     vcapmv1, vcapmv2, vcapmv3,
     &     dep2, rdep2, qqq, rqqq, shuntgmin, eturnon, esat
c
      parameter (alpha1    = 0.6026)
      parameter (alpha2    = 0.537)
      parameter (alpha3    = 0.537)
      parameter (alpha4    = 0.5755)
      parameter (beta1     = 0.392)
      parameter (beta2     = 0.297)
      parameter (beta3     = 0.203)
      parameter (beta4     = 0.14)
      parameter (emass     = 5.11e+5)
      parameter (remass    = 1.0 / emass)
      parameter (v2mv      = 1.0e-6)
      parameter (vcapmv1   = 0.3)
      parameter (vcapmv2   = 1.0)
      parameter (vcapmv3   = 3.0)
      parameter (dep2      = 0.3)
      parameter (rdep2     = 1.0 / dep2)
      parameter (qqq       = 0.96432601)
      parameter (rqqq      = 1.0 / qqq)
      parameter (shuntgmin = 1.0e-6)
      parameter (eturnon   = 2.0e7)
      parameter (esat      = 4.0e7)
c
      ibranch   = indexmitl(1,index)
      iblock    = indexmitl(2,index)
      node1     = indexmitl(3,index)
      node2     = indexmitl(4,index)
c
      delperv   = pin(1,iblock,ibranch)
      delt      = pin(2,iblock,ibranch)
      zline     = pin(3,iblock,ibranch)
      gap       = pin(5,iblock,ibranch)
c
c
c Reset the conductances.
c
c Find the voltage across the capacitor and the average current
c  in the line.
c
      do i = node1, node2
        vcap   = abs (v(i,ibranch)) + 1.0
        efield = vcap/gap
        iave   = abs (zir(i-1,ibranch))
        vcapmv = vcap * v2mv
c
        if (vcapmv .lt. vcapmv1) then
          res = alpha1 * vcapmv**beta1
        else if (vcapmv .lt. vcapmv2) then
          res = alpha2 * vcapmv**beta2
        else if (vcapmv .lt. vcapmv3) then
          res = alpha3 * vcapmv**beta3
        else
          res = alpha4 * vcapmv**beta4
        end if
        res2   = res * res
        cond2  = 1.0 / res2
c
        epi    = zline * iave / vcap
        epi2   = epi * epi
        if (epi2 .lt. cond2) then
          u        = vcap * remass
          hrelfu   = hrelf (u)
          gc       = delperv * sqrt (vcap) * hrelfu
          epi2res2 = epi2 * res2
          shuntg   = gc * (1.0 - 0.15*epi2res2)
          shuntg   = shuntg * (1.0 - exp((epi2res2 - 1.0) * rdep2))
          shuntg   = shuntg * rqqq
          shuntg   = amax1 (shuntg, shuntgmin)
          if (efield .lt. eturnon) then
             shuntg = shuntgmin
          else if (efield .lt. esat) then
             shuntg = shuntg*(efield-eturnon)/(esat-eturnon)
          endif
        else
          shuntg   = shuntgmin
        end if
c
        g(i,ibranch) = shuntg
c
      end do
c
      return
      end

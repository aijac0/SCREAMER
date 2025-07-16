      subroutine setup_cendsource (ib, ibk, ics_counter, na, iflgs,
     &                             ic_type, ic_counter, itypcs)
c
      use zdemmax
      include 'zdemparm.h'
      include 'zdemcomm.h'
      include 'zdemwork.h'
c
c Define passed variables
c
      integer  ib, ibk, ics_counter, na, iflgs, ic_type, ic_counter
      integer  itypcs
c
c Setup for end-of-branch current source or end-of-branch SCL current source.
c
c Set for 1 node.
c
      n1 = nr(ib) + 1
      na = 1
c
c Set the resistor and capacitor values.
c
      r1          = pin(1,ibk,ib)
      c1          = pin(2,ibk,ib)
c
c Set R1 and C1.
c
      g(n1,ib)       = 1.0 / (r1 + 1.0e-20)
      c(n1,ib)       = c1
      rr(n1,ib)      = 0.0
      zlr(n1,ib)     = 0.0
      iflg(n1,ib)    = iflgs
      cechk(n1,ib)   = c1
      zlrechk(n1,ib) = 0.0
c
c Set the index for this source.
c
      indexcs(1,ics_counter) = n1
      indexcs(2,ics_counter) = ib
c
c Set initial voltage if necessary and the corresponding energy.
c
      if (ic_type .eq. vcapacitor1) then
        vinitial   = value_init(ic_counter)
        v(n1,ib)   = vinitial
        vn(n1,ib)  = vinitial
        ecapsource = ecapsource  +  0.5*c(n1,ib)*vinitial*vinitial
      end if
c
c Set the current at time zero, depending on the type.
c
      if (itypcs .eq. cendsource) then
        time        = 0.0
        call set_current (time, ics_counter, czero)
        zir(n1,ib)  = czero
        zirn(n1,ib) = czero
      else if (itypcs .eq. csclsource) then
        time        = 0.0
        call set_sclcurr (time, v(n1,ib), ics_counter, czero)
        zir(n1,ib)  = czero
        zirn(n1,ib) = czero
      end if
c
      return
      end

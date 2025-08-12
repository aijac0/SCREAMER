      subroutine setup_currsource (ib, ibk, ics_counter, na, iflgs,
     &                             ic_type, ic_counter)
c
      use zdemmax
      use zdemwork
      use zdemparm
      use zdemcomm
c
c Define passed variables
c
      integer  ib, ibk, ics_counter, na, iflgs, ic_type, ic_counter
c
c Setup for current source
c
c Set for 2 nodes.
c
      n1 = nr(ib) + 1
      n2 = n1 + 1
      na = 2
c
c Set the current at time zero.
c
      time        = 0.0
      call set_current (time, ics_counter, czero)
      zir(n1,ib)  = czero
      zirn(n1,ib) = czero
      r3          = pin(1,ibk,ib)
      c3          = pin(2,ibk,ib)
c
c Set R3 and C3.
c
      g(n1,ib)       = 0.0
      c(n1,ib)       = 0.0
      rr(n1,ib)      = 0.0
      zlr(n1,ib)     = 0.0
      iflg(n1,ib)    = iflgs
      cechk(n1,ib)   = 0.0
      zlrechk(n1,ib) = 0.0
c
c Set the index for this source.
c
      indexcs(1,ics_counter) = n1
      indexcs(2,ics_counter) = ib
c
      g(n2,ib)       = 1.0 / (r3 + 1.0e-20)
      c(n2,ib)       = c3
      rr(n2,ib)      = 0.0
      zlr(n2,ib)     = 0.0
      iflg(n2,ib)    = iflgs
      cechk(n2,ib)   = c3
      zlrechk(n2,ib) = 0.0
c
c Set initial voltage if necessary and the corresponding energy.
c
      if (ic_type .eq. vcapacitor3) then
        vinitial   = value_init(ic_counter)
        v(n2,ib)   = vinitial
        vn(n2,ib)  = vinitial
        ecapsource = ecapsource  +  0.5*c(n2,ib)*vinitial*vinitial
      end if
c
      return
      end

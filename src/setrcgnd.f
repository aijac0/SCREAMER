      subroutine setup_rcground (ib, ibk, na, iflgs,
     &                           ic_type, ic_counter)
c
c
      use zdemmax
      use zdemwork
      include 'zdemparm.h'
      include 'zdemcomm.h'

c
c Define passed variables
c
      integer  ib, ibk, na, iflgs, ic_type, ic_counter
c
c  December 10, 1992;     hnw
c Setup for RC to ground.
c
c Set for one node.
c
      n1 = nr(ib)+1
      na = 1
      r1 = pin(1,ibk,ib)
      c1 = pin(2,ibk,ib)
c
c Set R1 and C1
c
      g(n1,ib)       = 1.0 / (r1 + 1.0e-20)
      c(n1,ib)       = c1
      rr(n1,ib)      = 0.0
      zlr(n1,ib)     = 0.0
      iflg(n1,ib)    = iflgs
      cechk(n1,ib)   = c1
      zlrechk(n1,ib) = 0.0
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
      return
      end

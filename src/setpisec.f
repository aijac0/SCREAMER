      subroutine setup_pisection (ib, ibk, na, iflgs,
     &                            ic_type, ic_counter)
c
c
      use zdemmax
      use zdemwork
      use zdemparm
      use zdemcomm

c
c Define passed variables
c
      integer  ib, ibk, na, iflgs, ic_type, ic_counter
c
c Setup for pi section.
c
c Set for 2 nodes.
c
      n1  = nr(ib) + 1
      n2  = n1 + 1
      na  = 2
c
c Set R1, C1, R2, L2, R3, C3
c
      r1  = pin(1,ibk,ib)
      c1  = pin(2,ibk,ib)
      r2  = pin(3,ibk,ib)
      zl2 = pin(4,ibk,ib)
      r3  = pin(5,ibk,ib)
      c3  = pin(6,ibk,ib)
c      
      g(n1,ib)       = 1.0 / (r1+1.0e-20)
      c(n1,ib)       = c1
      rr(n1,ib)      = r2
      zlr(n1,ib)     = zl2
      iflg(n1,ib)    = iflgs
      cechk(n1,ib)   = c1
      zlrechk(n1,ib) = zl2
c
      g(n2,ib)       = 1.0 / (r3+1.0e-20)
      c(n2,ib)       = c3
      rr(n2,ib)      = 0.0
      zlr(n2,ib)     = 0.0
      iflg(n2,ib)    = iflgs
      cechk(n2,ib)   = c3
      zlrechk(n2,ib) = 0.0
c
c Set initial voltage or current if necessary and the corresponding energy.
c
      if (ic_type .eq. vcapacitor1) then
        vinitial    = value_init(ic_counter)
        v(n1,ib)    = vinitial
        vn(n1,ib)   = vinitial
        ecapsource  = ecapsource  +  0.5*c(n1,ib)*vinitial*vinitial
      else if (ic_type .eq. cinductor) then
        cinitial    = value_init(ic_counter)
        zir(n1,ib)  = cinitial
        zirn(n1,ib) = cinitial
        eindsource  = eindsource  +  0.5*zlr(n1,ib)*cinitial*cinitial
      else if (ic_type .eq. vcapacitor3) then
        vinitial    = value_init(ic_counter)
        v(n2,ib)    = vinitial
        vn(n2,ib)   = vinitial
        ecapsource  = ecapsource  +  0.5*c(n2,ib)*vinitial*vinitial
      end if
c
      return
      end

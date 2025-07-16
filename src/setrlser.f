      subroutine setup_rlseries (ib, ibk, na, iflgs,
     &                          ic_type, ic_counter)
c
c  December 10, 1992;     hnw
c Setup for RL in series.
c
c
c Include files
c
      use zdemmax
      include 'zdemparm.h'
      include 'zdemcomm.h'
      include 'zdemwork.h'
c
c
c Define passed variables
c
      integer  ib, ibk, na, iflgs, ic_type, ic_counter
c Set for 2 nodes.
c
      n1  = nr(ib) + 1
      n2  = n1 + 1
      na  = 2
c
c Set R2 and L2
c
      r2  = pin(1,ibk,ib)
      zl2 = pin(2,ibk,ib)
c      
      g(n1,ib)       = 0.0
      c(n1,ib)       = 0.0
      rr(n1,ib)      = r2
      zlr(n1,ib)     = zl2
      iflg(n1,ib)    = iflgs
      cechk(n1,ib)   = 0.0
      zlrechk(n1,ib) = zl2
c
      g(n2,ib)       = 0.0
      c(n2,ib)       = 0.0
      rr(n2,ib)      = 0.0
      zlr(n2,ib)     = 0.0
      iflg(n2,ib)    = iflgs
      cechk(n2,ib)   = 0.0
      zlrechk(n2,ib) = 0.0
c
c Set initial current if necessary and the corresponding energy.
c
      if (ic_type .eq. cinductor) then
        cinitial    = value_init(ic_counter)
        zir(n1,ib)  = cinitial
        zirn(n1,ib) = cinitial
        eindsource  = eindsource  +  0.5*zlr(n1,ib)*cinitial*cinitial
      end if
c
      return
      end

      subroutine setup_adder (ib, ibk, na, iflgs)
c
c Modifications
c 2015-06-23 RBS: ibktmp added to eliminate compiler warning
c
c Setup for adder block.
c
      use zdemmax
      include 'zdemcomm.h'
      include 'zdemwork.h'
c
c Define passed variables
c
      integer  ib, ibk, na, iflgs
c
c
c ibk used to prevent a compiler warning
c
      ibktmp = ibk
c
c Set for 2 nodes.
c
      n1 = nr(ib) + 1
      n2 = n1 + 1
      na = 2
c
c Shunt is open, series is open.
c
      g(n1,ib)       = 0.0
      c(n1,ib)       = 0.0
      rr(n1,ib)      = 1.0e+6
      zlr(n1,ib)     = 0.0
      iflg(n1,ib)    = iflgs
      cechk(n1,ib)   = 0.0
      zlrechk(n1,ib) = 0.0
c
      g(n2,ib)       = 0.0
      c(n2,ib)       = 0.0
      rr(n2,ib)      = 0.0
      zlr(n2,ib)     = 0.0
      iflg(n2,ib)    = iflgs
      cechk(n2,ib)   = 0.0
      zlrechk(n2,ib) = 0.0
c
      return
      end

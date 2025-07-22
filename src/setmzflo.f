      subroutine setup_mzflow (ib, ibk, na, iflgs)
c
c     KWS  10/31/95
c Modified
c 2015-06-23 RBS: ibktmp used to eliminate a compiler error, ibk unused
c
c Setup for Measure Zflow Block.
c
c Include files
c
      use zdemmax
      use zdemwork
      include 'zdemcomm.h'

c
c Define passed variables
c
      integer  ib, ibk, na, iflgs
c
c ibk used to prevent a compiler warning
c
      ibktmp = ibk
c
c Set for 1 node
c
      n1 = nr(ib) + 1
      na = 1
c
c Shunt is open, series is open.
c
      g(n1,ib)       = 0.0
      c(n1,ib)       = 0.0
      rr(n1,ib)      = 0.0
      zlr(n1,ib)     = 0.0
      iflg(n1,ib)    = iflgs
      cechk(n1,ib)   = 0.0
      zlrechk(n1,ib) = 0.0
c
      return
      end

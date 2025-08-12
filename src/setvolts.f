      subroutine setup_voltsource (ib, ibk, ivs_counter, na, iflgs,
     &           ic_type, ic_counter)
c
      use zdemmax
      use zdemwork
      use zdemparm
      use zdemcomm

c
c Define passed variables
c
      integer  ib, ibk, ivs_counter, na, iflgs, ic_type, ic_counter
c
c Setup for voltage source
c
c Set for 2 nodes.
c
      n1 = nr(ib) + 1
      n2 = n1 + 1
      na = 2
c
c Set the voltage at time zero.
c
      time      = 0.0
      call set_voltage (time, ivs_counter, vzero)
      v(n1,ib)  = vzero
      vn(n1,ib) = vzero
      r2        = pin(1,ibk,ib)
      zl2       = pin(2,ibk,ib)
c
c Set R2 and L2.
c
      g(n1,ib)       = 0.0
      c(n1,ib)       = 0.0
      rr(n1,ib)      = r2
      zlr(n1,ib)     = zl2
      iflg(n1,ib)    = iflgs
      cechk(n1,ib)   = 0.0
      zlrechk(n1,ib) = zl2
c
c Set the index for the source.
c
      indexvs(1,ivs_counter) = n1
      indexvs(2,ivs_counter) = ib
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

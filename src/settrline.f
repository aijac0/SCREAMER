      subroutine setup_trline (ib, ibk, icounter, na, iflgs, switch,
     &                         ic_type, ic_counter)
c
c ----------------------------------------------------------------------
c Modifications:
c   02/08/94, KWS, Modified to eliminate last capacitor element and
c                  set Z for first/last segments to be Zin/Zout exactly.
c   08/12/97, MLK, Corrected formula for exponential line segments
c 2014-05-02 RBS: Changed integer*4 to integer
c 2014-12-12 RBS: Added lossy line inputs
c 2014-12-12 RBS: Added lossyline switch in the TRLIN setup, ln 112
c 2015-01-09 RBS: Added lossy line parameters at ln 68.
c 2015-03-30 RBS: Defined all internal variables.
c                 Initialized all internal variables.
c 2017-09-11 RBS: Changed routine name to settrline.f
c ----------------------------------------------------------------------
c
c Set up for a transmission line or MITL.
c
c
c Include files
c
      use zdemmax
      use zdemwork
      use zdemparm
      use zdemcomm

c
c Define passed variables
c
      integer  ib, ibk, icounter, na, iflgs, switch,
     &         ic_type, ic_counter
c
c Define internal variables
c
      real       tau, zin, zout, tres, gline, rline
      integer    itype
      real       factor
c
c Initialize internal variables
c
      tau = 0.0
      zin = 1.0
      zout = 1.0
      tres = 1.00e-06
      gline = 0.0
      rline = 0.0
      factor = 4.38e3
      itype = 1
c
c Set up for lossless line, MITL, or lossy line
c
      if (switch .eq. transline) then
        tau   = pin(1,ibk,ib)
        zin   = pin(2,ibk,ib)
        zout  = pin(3,ibk,ib)
        tres  = pin(4,ibk,ib)
        itype = itrl_type(icounter)
      else if (switch .eq. mitline) then
        tau   = pin(3,ibk,ib)
        zin   = pin(4,ibk,ib)
        zout  = zin
        tres  = pin(5,ibk,ib)
        itype = linearz
      else if (switch .eq. pmitline) then
        tau   = pin(2,ibk,ib)
        zin   = pin(3,ibk,ib)
        zout  = zin
        tres  = pin(4,ibk,ib)
        itype = linearz
      else if (switch .eq. lossyline) then
        tau   = pin(1,ibk,ib)
        zin   = pin(2,ibk,ib)
        gline = (1.0/(pin(3,ibk,ib)+1.0e-12))
        rline = pin(4,ibk,ib)
        zout  = pin(5,ibk,ib)
        tres  = pin(6,ibk,ib)
        itype = linearz

      end if
c
c Determine the number of segments given the resolution time.
c na is the nearest integer to (tau/tres)
c
      na    = int ( (tau / tres) + 0.5 )
      if (na .lt. 2) then
        na = 2
      else if (na .ge. max_trline_nodes) then
        na = max_trline_nodes
      end if
      nseg  = na
      n1    = nr(ib) + 1
      n2    = nr(ib) + nseg
c
c Set the blocks up for linear or exponential variation in z
c
      delt     = tau / nseg
c
c Do a linear variation.
c
      if (itype .eq. linearz) then
        alpha = (zout-zin) / float(nseg-1)
c
c Fills in the various elements of a generic block consisting of 2
c RCGs and 2 RLSs. Compare with setrlser.f
c Note: g and rr for lossy terms are scaled by nseg
c
        do ie = n1, n2
          zele           = zin + alpha * float(ie-n1)
          shuntc         = delt / zele
          seriesl        = delt * zele
          c(ie,ib)       = shuntc
          zlr(ie,ib)     = seriesl
          g(ie,ib)       = 0.0
          rr(ie,ib)      = 0.0
          if (switch .eq. lossyline) then
            g(ie,ib)     = gline * nseg
            rr(ie,ib)    = rline / nseg
            endif
          iflg(ie,ib)    = iflgs
          cechk(ie,ib)   = shuntc
          zlrechk(ie,ib) = seriesl
        end do
c
c Exponential variation.
c
      else if (itype .eq. exponentialz) then
c
        alpha = (log(zout/zin))/(n2-n1)
        do ie = n1, n2
          zele           = zin * exp((ie-n1)*alpha)
          shuntc         = delt / zele
          seriesl        = delt * zele
          c(ie,ib)       = shuntc
          zlr(ie,ib)     = seriesl
          g(ie,ib)       = 0.0
          rr(ie,ib)      = 0.0
          iflg(ie,ib)    = iflgs
          cechk(ie,ib)   = shuntc
          zlrechk(ie,ib) = seriesl
        end do

      end if
c
c Set up some MITL indicies for resetting the conductances later.
c
      if (switch .eq. mitline) then
        indexmitl(1,icounter) = ib
        indexmitl(2,icounter) = ibk
        indexmitl(3,icounter) = n1
        indexmitl(4,icounter) = n2
        indexmitl(5,icounter) = switch
        pin(3,ibk,ib) = delt
      else if (switch .eq. pmitline) then
        indexmitl(1,icounter) = ib
        indexmitl(2,icounter) = ibk
        indexmitl(3,icounter) = n1
        indexmitl(4,icounter) = n2
        indexmitl(5,icounter) = switch
        delperv=pin(1,ibk,ib) / nseg
        pin(1,ibk,ib) = delperv
        pin(2,ibk,ib) = delt
        gap = factor*delt/(delperv*(exp(zin/60.)-1.0))
        pin(5,ibk,ib) = gap
      end if
c
c Setup for any initial voltage or current on the line.
c Set the voltage at all the nodes and
c add the energy resulting from the initial voltage to the initial
c energy total for capacitors.
c OR set the current at all but the last node and add the energy from
c the initial current to the energy total for all inductors.
c

      if (ic_type .eq. vtrline) then
        vinitial    = value_init(ic_counter)
        vinitial2   = 0.5 * vinitial * vinitial
        ctot        = 0.0

        do ie = n1, n2
          v(ie,ib)  = vinitial
          vn(ie,ib) = vinitial
          ctot      = ctot + c(ie,ib)
        end do

        ecapsource  = ecapsource  +  ctot*vinitial2
      else if (ic_type .eq. ctrline) then
        cinitial      = value_init(ic_counter)
        cinitial2     = 0.5 * cinitial * cinitial
        zlrtot        = 0.0

        do ie = n1, n2
          zir(ie,ib)  = cinitial
          zirn(ie,ib) = cinitial
          zlrtot      = zlrtot + zlr(ie,ib)
        end do

        eindsource    = eindsource  +  zlrtot*cinitial2
      end if

      return
      end

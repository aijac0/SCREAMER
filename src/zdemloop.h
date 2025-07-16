c----------------------------------------------------------------------
c    @(#)zdemcomm.h   version 1.2.1   created 11/23/04 11:41:07
c    Last modified:   23-Nov-2004 11:41:07   MB 
C_Groups @(#)
c----------------------------------------------------------------------
c **********************************************************************
c  This is the common block used for sharing data between zdem.f,
c  main_loop.f,start_run.f, and solver_screamer_metrix.f 
c **********************************************************************
c  file zdemloop.h    
c
c ----------------------------------------------------------------------
c  Modified:
c 2014-02-06 RBS: Explicitly defined reals and integers
c                 Reordered common block with 64 bit first
c 2014-04-11 RBS: Explict integer definition in /files/ common
c 2015-06-17 RBS: Removed any3, any4, ctime, vcond, vtime from real
c                 definition and common and put in solver_screamer_
c                 matrix.f
c 2015-06-17 RBS: Removed i1km, i5, i51, i52, i53, i54, i55, i56, i58
c                 from integer definition and common
c
c 2015-06-17 RBS: Removed iax, iq, j1, j2, j3, j4, j5
c                 from integer definition and common
c
c 2015-06-17 RBS: Removed ja, jb, jc, jd, je, jes, jf, jg, jh, ji, jis,
c                 jk, jl from integer definition and common
c
c 2015-06-17 RBS: Removed k2p1, k2p2, km, km1, kmm
c                 from integer definition and common
c
c 2015-06-17 RBS: Removed l1, l10, l11, l12, l13, l14, l3, l3m, l3p, l4,
c                 l5, l6, l7, l8, mm1, mp1from integer definition and
c                 common
c
c 2015-06-17 RBS: Removed nkm, nkp1, nm1 from integer definition and
c                 common. These and previous variables were unused.
c
c 2015-06-18 RBS: Removed ia, ic, id, ie, ig, ih, ii, iib, ij, ik, il,
c                 if, ip, l2, nelmt from integer definition and
c                 common. These and previous variables were unused.
c
c 2015-06-18 RBS: Removed iptold, iptime, jdiv, nk
c                 and declared in main_loop.f .
c
c 2015-06-18 RBS: Removed iblock1, icbp, icx, icy, iexit_type,
c                 if_cendsource, if_vendsource, imitl, nele,
c                 node_num, nr2, nrm, nrow, ny
c                 and declared in solver_screamer_matrix.f .
c
c 2015-06-18 RBS: nzz is only used in main_loop.f and solver_screamer_
c                 matrix AND it is defined in the first incidence in
c                 both routines. Remove from common and declared
c                 separately in both routines.
c
c ----------------------------------------------------------------------
c     
 
c
c variables needed for main_loop, solver_screamer_matrix, start_run,
c and zdem
c
      real tim

      integer
     & i2, icb, icycle, itypcs,
     & j, k, l, m, n, ncycle, nbm, ntot

      character  cyclln*80

      common /for_main_loop/
     & tim,
     & i2, icb, icycle, itypcs,
     & j, k, l, m, n, ncycle, nbm, ntot,
     & cyclln

c variables needed for file outputs

      integer        iunit, ierr
      common /files/ iunit, ierr

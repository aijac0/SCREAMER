!----------------------------------------------------------------------
!    @(#)zdemloop.h   version 1.2.1   created 11/23/04 11:41:07
!    Last modified:   23-Nov-2004 11:41:07   MB 
!_Groups @(#)
!----------------------------------------------------------------------
! **********************************************************************
!  This is the common block used for sharing data between zdem.f,
!  main_loop.f,start_run.f, and solver_screamer_metrix.f 
! **********************************************************************
!  file zdemloop.f90    
!
! ----------------------------------------------------------------------
!  Modified:
! 2014-02-06 RBS: Explicitly defined reals and integers
!                 Reordered common block with 64 bit first
! 2014-04-11 RBS: Explict integer definition in /files/ common
! 2015-06-17 RBS: Removed any3, any4, ctime, vcond, vtime from real
!                 definition and common and put in solver_screamer_
!                 matrix.f
! 2015-06-17 RBS: Removed i1km, i5, i51, i52, i53, i54, i55, i56, i58
!                 from integer definition and common
!
! 2015-06-17 RBS: Removed iax, iq, j1, j2, j3, j4, j5
!                 from integer definition and common
!
! 2015-06-17 RBS: Removed ja, jb, jc, jd, je, jes, jf, jg, jh, ji, jis,
!                 jk, jl from integer definition and common
!
! 2015-06-17 RBS: Removed k2p1, k2p2, km, km1, kmm
!                 from integer definition and common
!
! 2015-06-17 RBS: Removed l1, l10, l11, l12, l13, l14, l3, l3m, l3p, l4,
!                 l5, l6, l7, l8, mm1, mp1from integer definition and
!                 common
!
! 2015-06-17 RBS: Removed nkm, nkp1, nm1 from integer definition and
!                 common. These and previous variables were unused.
!
! 2015-06-18 RBS: Removed ia, ic, id, ie, ig, ih, ii, iib, ij, ik, il,
!                 if, ip, l2, nelmt from integer definition and
!                 common. These and previous variables were unused.
!
! 2015-06-18 RBS: Removed iptold, iptime, jdiv, nk
!                 and declared in main_loop.f .
!
! 2015-06-18 RBS: Removed iblock1, icbp, icx, icy, iexit_type,
!                 if_cendsource, if_vendsource, imitl, nele,
!                 node_num, nr2, nrm, nrow, ny
!                 and declared in solver_screamer_matrix.f .
!
! 2015-06-18 RBS: nzz is only used in main_loop.f and solver_screamer_
!                 matrix AND it is defined in the first incidence in
!                 both routines. Remove from common and declared
!                 separately in both routines.
!
! 2025-08-11 AJC: Moved file into a module
!
! ----------------------------------------------------------------------
!

module zdemloop
      implicit none
 
! variables needed for main_loop, solver_screamer_matrix, start_run, zdem
      real :: tim

      integer :: i2, icb, icycle, itypcs
      integer :: j, k, l, m, n
      integer :: ncycle, nbm, ntot

      character :: cyclln*80

! variables needed for file outputs

      integer :: iunit, ierr

end module zdemloop
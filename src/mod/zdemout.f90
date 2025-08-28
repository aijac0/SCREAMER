! **********************************************************************
!  Common blocks for SCREAMER output - plotting, printing, filing, and
!                                      tabling.
!  To include: use 'zdemmax.f90'
! **********************************************************************
! 
!  Modifications:
!    10/14/93, KWS:  added MFI CB common block setup
!    06/07/95, MLK:  added counter for CSV output types
!    08/12/97, KWS:  added Cathode Current Diagnostic common block
!    06/09/97, KWS:  Added multiple shell parameters
!    12/23/97, MLK:  added counter for SFC output types
! 2014-02-06 RBS: Changed real*4 to real
!                 Explicit real definition in common/shellparm/
!                 Reordered variables in common/shellparm/ 64 bit first
!                 Fixed a bug in which not all of the reals defined for
!                 the common/shellparm/ were in the common block
! 2014-02-07 RBS: Fixed a bug in which not all of the reals and integers
!                 defined for the common/outstuff/ were in that
!                 common block
!                 Reordered the variables in common/outstuff/ 64 bit
! 2014-05-02 RBS: changed integer*4 to integer
! 2015-06-23 RBS: Removed newfil, oldfile, fflag declaration. Placed in
!                 the actual subroutines to remove compiler warnings
! 2015-06-23 RBS: lblout_temp length increased to 26 to agree with
!                 labelout
! 2016-04-01 RBS: xlblout parameter changed to 'Time (s)'
! 2019-01-27 RBS: Add real variable scale_out declared to
!                 the same array size as the output data variables
! 2025-08-28 AJC: Moved file into a module
!
! file zdemout.f90
! ***** The common block for plotting *****
! ***** Stacked for 64 bit *****
!
! Note: outdata and timeout are reals that are written to disk
!       these should be single precision real*4 to prevent odd
!       things from happening with the 1PE12.4 write format
!

module zdemout
      use zdemmax

      real :: saveout1(maxout), saveout2(maxout), saveout3(maxout) 
      real :: saveout4(maxout), scale_out(maxout), tbegout(maxout)
      real :: tendout(maxout), val1(maxout), val2(maxout)
      real :: yminout(maxout), ymaxout(maxout)

      real*4 :: outdata(max_plot_points,maxout)
      real*4 :: timeout(max_plot_points,maxout)

      integer :: iblkout(maxout), indices(maxout), iouttype(maxout)
      integer :: itimeflg(maxout), itypout(maxout), ixblkout(maxout)
      integer :: ixbrnout(maxout), ixlstnodout(maxout), ixnodout(maxout)
      integer :: numout, numplt, numprt, numfil, numtab
      integer :: numufo, numidr, numpff, numcsv, numsfc

      character :: lblout(maxout)*80
      character :: lblout_temp(maxout)*26
      character :: ylblout(maxout)*11
      character :: tagout(maxout)*8

      logical radyields

!
      integer, parameter :: outunit = 12

      character(len=8), parameter :: xlblout = 'Time (s)'
!
! ***** The common block for foil or gas puff parameters *****
! ***** Stacked for 64 bit *****
!
      real :: foilrad, foilvel, foilacc, foilke, gasrad, gasvel, gasacc, gaske
      real :: yw_al,ym_al,yw_ar,ym_ar,yw_cu,ym_cu,yw_kr,ym_kr,yw_xe,ym_xe

!
! ***** Toms sw parameters *****
!
      real :: radch1         

!
! ***** The common block for MFI CB parameters *****
!
      real :: efld, bfld, xmfi         

!
! ***** The common block for the Zflow Measurement model
! ***** Stacked for 64 bit *****
!
      real :: ccathode(max_mzflowblocks), cplasma(max_mzflowblocks)
      real :: measdzflow(max_mzflowblocks), zofmzflow(max_mzflowblocks)

      integer :: mzflowblock

!
! ***** Variables for multiple shell model *****
! ***** Stacked for 64 bit *****
!
      integer, parameter :: max_pwl_pairs = 401

      real :: shellmass(max_shells), shellradius(max_shells)
      real :: shellind(max_shells), shellcurr(max_shells+1)
      real :: acceleration(max_shells), svelocity(max_shells)
      real :: shellparms(2*max_pwl_pairs)
      real :: shellrad, shellvel, shellacc, shellke, shellm
      real :: rtrap(max_shells), itrap(max_shells)

      integer :: numshells, shell(max_shells)
      logical :: trapped

end module zdemout

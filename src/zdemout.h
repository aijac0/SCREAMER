c **********************************************************************
c  Common blocks for SCREAMER output - plotting, printing, filing, and
c                                      tabling.
c  Use: with 'zdemout.h'  include 'zdemmax.h'
c **********************************************************************
c 
c  Modifications:
c    10/14/93, KWS:  added MFI CB common block setup
c    06/07/95, MLK:  added counter for CSV output types
c    08/12/97, KWS:  added Cathode Current Diagnostic common block
c    06/09/97, KWS:  Added multiple shell parameters
c    12/23/97, MLK:  added counter for SFC output types
c 2014-02-06 RBS: Changed real*4 to real
c                 Explicit real definition in common/shellparm/
c                 Reordered variables in common/shellparm/ 64 bit first
c                 Fixed a bug in which not all of the reals defined for
c                 the common/shellparm/ were in the common block
c 2014-02-07 RBS: Fixed a bug in which not all of the reals and integers
c                 defined for the common/outstuff/ were in that
c                 common block
c                 Reordered the variables in common/outstuff/ 64 bit
c 2014-05-02 RBS: changed integer*4 to integer
c 2015-06-23 RBS: Removed newfil, oldfile, fflag declaration. Placed in
c                 the actual subroutines to remove compiler warnings
c 2015-06-23 RBS: lblout_temp length increased to 26 to agree with
c                 labelout
c 2016-04-01 RBS: xlblout parameter changed to 'Time (s)'
c 2019-01-27 RBS: Add real variable scale_out declared to
c                 the same array size as the output data variables
c
c file zdemout.h
c ***** The common block for plotting *****
c ***** Stacked for 64 bit *****
c
c Note: outdata and timeout are reals that are written to disk
c       these should be single precision real*4 to prevent odd
c       things from happening with the 1PE12.4 write format
c

      real
     & saveout1(maxout), saveout2(maxout),
     & saveout3(maxout), saveout4(maxout),
     & scale_out(maxout),
     & tbegout(maxout), tendout(maxout),
     & val1(maxout), val2(maxout),
     & yminout(maxout), ymaxout(maxout)

      real*4
     & outdata(max_plot_points,maxout), timeout(max_plot_points,maxout)

      integer
     & iblkout(maxout), indices(maxout), iouttype(maxout),
     & itimeflg(maxout), itypout(maxout),
     & ixblkout(maxout), ixbrnout(maxout),
     & ixlstnodout(maxout), ixnodout(maxout),
     & numout, numplt, numprt, numfil, numtab,
     & numufo, numidr, numpff, numcsv, numsfc

      character
     & lblout(maxout)*80,
     & lblout_temp(maxout)*26,
     & ylblout(maxout)*11,
     & tagout(maxout)*8

      logical radyields
c
      common /outstuff/
     & saveout1, saveout2,
     & saveout3, saveout4,
     & scale_out,
     & tbegout, tendout,
     & val1, val2,
     & yminout, ymaxout,
     & outdata, timeout,
     & iblkout, indices, iouttype,
     & itimeflg, itypout,
     & ixblkout, ixbrnout,
     & ixlstnodout, ixnodout,
     & numout, numplt, numprt, numfil, numtab,
     & numufo, numidr, numpff, numcsv, numsfc,
     & lblout,
     & lblout_temp,
     & ylblout,
     & tagout,
     & radyields
c
      integer    outunit
      parameter (outunit=12)

      character  xlblout*8
      parameter (xlblout = 'Time (s)')
c
c ***** The common block for foil or gas puff parameters *****
c ***** Stacked for 64 bit *****
c
      real  
     & foilrad, foilvel, foilacc, foilke,
     & gasrad, gasvel, gasacc, gaske,
     & yw_al,ym_al,yw_ar,ym_ar,yw_cu,ym_cu,
     & yw_kr,ym_kr,yw_xe,ym_xe
c
      common /fiparm/
     & foilrad, foilvel, foilacc, foilke, 
     & gasrad, gasvel, gasacc, gaske,
     & yw_al,ym_al,yw_ar,ym_ar,yw_cu,ym_cu,
     & yw_kr,ym_kr,yw_xe,ym_xe
c
c ***** The common block for Toms sw parameters *****
c
      real             radch1         
      common /tswparm/ radch1 
c
c ***** The common block for MFI CB parameters *****
c
      real             efld, bfld, xmfi         
      common /mfiparm/ efld, bfld, xmfi 
c
c ***** The common block for the Zflow Measurement model
c ***** Stacked for 64 bit *****
c
      real
     & ccathode(max_mzflowblocks), cplasma(max_mzflowblocks),
     & measdzflow(max_mzflowblocks), zofmzflow(max_mzflowblocks)

      integer
     & mzflowblock

      common /mzflowparam/
     & ccathode, cplasma, measdzflow, zofmzflow,
     & mzflowblock

c
c ***** Common block for multiple shell model *****
c ***** Stacked for 64 bit *****
c
      parameter (max_pwl_pairs        = 401)

      real
     & shellmass(max_shells), shellradius(max_shells),
     & shellind(max_shells), shellcurr(max_shells+1),
     & acceleration(max_shells), svelocity(max_shells),
     & shellparms(2*max_pwl_pairs),
     & shellrad, shellvel, shellacc, shellke, shellm,
     & rtrap(max_shells), itrap(max_shells)

      integer
     & numshells, shell(max_shells)
      logical
     & trapped

      common /shellparm/
     & shellmass, shellradius,
     & shellind, shellcurr,
     & acceleration, svelocity,
     & shellparms,
     & shellrad, shellvel, shellacc, shellke, shellm,
     & rtrap, itrap,
     & numshells, shell,
     & trapped


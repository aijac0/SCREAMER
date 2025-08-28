      subroutine readscreamerdata(status)
c ************************************************************************
c     This subroutine reads in a properly formatted SCREAMER data file
c     See the SCREAMER documentation for the proper form of the data file.
c        hnw  February 26, 1993.
c
c     Modified March 7, 1994, KWS.  Added RWALL model, added MFI model, 
c     changed the switch model to point to just one routine, initialized 
c     variables in several of the routines to be used later for output, 
c     added an indexing array which saved the variable element number for
c     an output request, plus other minor changes.
c
c  Modifications:
c     MLK, 03/07/95, change include filenames to be 8 characters or less 
c                    and split line 1387 so it is two lines, each less
c                    than 72 characters
c     MLK, 03/15/95  correct bug in MITL with constant impedance to
c                    set eturnon if not entered in input file
c     KWS, 05/30/95  Added input processing for Zflow Plasma Loss Model
c     MLK, 06/09/95  Skip over printing out user subroutine names
c     MLK, 07/19/95  Add handling of DETAIL_PRINTS option
c     MLK, 07/11/95, simplify output to log file
c     MLK, 08/10/95, clean-up opening of input file (inpfile) and added
c                    writing error message to stdout if open failed
c     MLK, 08/18/95, moved opening of input file (inpfile) to get_env_var
c     KWS, 10/25/95, Added flag to input to Zloss and Zpos models to
c                    allow a backward line application.  Also modified
c                    to Zloss and Zpos to handel negative currents.
c     KWS, 08/13/97, Added recognition of output requests for Measure
c                    Zflow and Cathode Current Block
c     MLK, 01/05/98, Added changes to handle SFC output requests
c     KWS, 07/27/98, Changed RWALL model to Stygar formulation
c     KLS, 01/27/99, Added code so that SFC output requests with half_step
c                    time_flag reports that whole time step will be used
c     KWS, 02/09/99, Added code for N Shell implosion model and measure
c                    ZFLOW block
c 2006-06-01 MB: Split the code into subroutines. Several files have
c                     been created, both .f and .h
c 2008/11/07 RBS: Removed ref to echoln - moved to rdscreleminit
c 2008-12-08 RBS: Included write formats, removed ref to zdenfmt.h
c 2017-02-24 RBS: Modified the get_next_line call to include the new
c                 passed variable currline_lc
c **********************************************************************
c
c Include the common blocks which are to be filled by the
c this subroutine.
c
      use zdemmax
      use zdemcomm
      use zdemparm
      use zdemout
      use zdempprm
      use zdemenv
      include 'version.h'
      include 'rdscrdat.h'

c
c Define passed variables
c
      integer    status
c
c Define internal variables
c 
      character  page1*80
      page1    = screamer_version // ' Circuit Analysis Code:  '

c ======================================================
c Initializations
c Write a banner page with SCREAMER title and name in log file.
      call banner (page1)

c Read the general run parameters of the simulation
c initialize a few parameters (in common blocs)
      call readscreamerparameters(status)
      if (status .eq. 1000) then
          goto 1000
      endif
c printing general parameters
      call printscreamerparameters
      
c initialize counters for number of branches, blocks ...
      call readscreleminit

c ======================================================
c Read and process circuit elements
c  
  305 continue

c Read in one line of the input deck and strip the blanks and commas.

      call get_next_line
     &     (currline, currline_lc, field, nlines, eofflg, max_fields)
        if (eofflg .eq. err) go to 1000
        keyword = field(1)(1:keyword_len)

c Read if the keyword is a branch or an electrical element
c If neither status remains 0 else status = 10 = k_found
c
      call readscreamerelements(status)
        if (status .eq. k_found) goto 305
c
c status 305 indicates a bad command
c
        if (status .eq. 305) goto 305
c
c Status 1000 indicates that end of file reached,
c
        if (status .eq. 1000) goto 1000
c
c status was neither k_found, 305, or 1000. Could be a blank line or
c a comment, an output requeset, etc.
c
c Get the output information if keyword was for an output request
c numout initialized to 0 in readscrinit.f. Every good output request
c increments numout in rdscrelem.f
c      print '(i8)', numout

      call readscreameroutputs(status)
c Even if there was a bad line get next line
        if (status .eq.  305) goto 305
c Here status equals 1000 only if the numouts has exceed maxout
        if (status .eq. 1000) goto 1000
c If you got here
      goto 305

c ======================================================
 1000 continue
c
c Set some variables to values determined in this subroutine for future
c use with a different name.
c
      nbk(nbrns) = nblks
      nb         = nbrns
      nvar       = nvarl
      
c Check that no limits have been exceeded      
c      print '(i8,i5)', numout,maxout
      call checkscreamerlimits

c
c If no errors encountered, tell the status is OK, set switch and branch
c counters and the block counter array, and return to calling program.
c Otherwise, tell read errors encountered and stop.
c
      if (numerr .eq. 0) then
        write(6,'(A/)') '   Input file read with no errors.'
        status=0
      else
        write(6,'(A)')
     &   ' ### Errors found in input file, execution halted.'
        status=802
      end if
      close(unit=lunit)
      
      return
      end

      subroutine write_outs
c
c Modifications
c
c 2015-06-23 RBS: Placed newfile, oldfile, fflag declarations internal
c                 removed them from zdemout to get rid of compiler
c                 warnings.
c 2016-03-16 RBS: Cleaned up a single write format statement
c ----------------------------------------------------------------------
c
c
c Include the files specifying the array dimensions and the common
c blocks.
c
      use zdemmax
      include 'zdemcomm.h'
      include 'zdemwork.h'
      include 'zdemout.h'
      include 'zdemenv.h'
      include 'zdemvars.h'
c
c Include the files with the various keywords and integer flags.
c
      include 'zdemparm.h'
      include 'zdempprm.h'
c
c Constants
c

      integer    newfile,   oldfile, fflag
      parameter (newfile=1, oldfile=2)

c
c Include file with version string
c
      include 'version.h'

      
c ----------------------------------------------------------------------
       write(6,'(/A)') 'SCREAMER is now writing to the output file'
 
       if (iset .eq. all_cycles) then
c
c Create files for future processing, one file for each request.
c
        if (numfil .gt. 0) then
          call filvals
        end if
c
c Create SCREAMER compatible tables, one file for each request.
c
        if (numtab .gt. 0) then
          call tabvals
        end if
c
c Create UFO txt file.
c
        if (numufo .gt. 0) then
          call ufovals
        end if
c
c ------------------------- Section if PFF is Used ---------------------
c
C#if defined(USE_PFF)
c
c Create PFF file.
c
C        if (numpff .gt. 0) then
C          call pffvals
C        end if
Cc
C#endif
c
c ------------------------- End PFF Section ----------------------------
c
c
c Create Comma Separated Variable (CSV) file
c
        if (numcsv .gt. 0) then
          call csvvals
        end if
c
c Create Standard de Fichiers Communs (SFC) file
c
        if (numsfc .gt. 0) then
          call sfcvals
        end if
c
c Now delete the SCREAMER output parameter file
c
      fflag = oldfile
      iunit = outunit
      call open_outfile (iunit, fflag, ierr)
      close (iunit,status='delete')
c
      end if
      return
      end
      

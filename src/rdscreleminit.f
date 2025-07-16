c----------------------------------------------------------------------
c rdscreleminit.f   version 1.0  created 06/15/2005 by Mathias Bavay
c----------------------------------------------------------------------
c---------------------------------------------------------------------
c
c  This subroutine initializes  electrical elements called in the input
c  deck.
c
c  Modifications:
c 2008-07-16 RBS Add include zdemout.h as it passes numout&others
c                numout&others  initialized here but needed in 
c                readscreameroutputs
c 2008-11-07 RBS Added definition of echoln to fix log file problem
c                in banner call, Added return in subroutine
c
c-----------------------------------------------------------------------

      subroutine readscreleminit
      
      use zdemmax
      include 'zdemcomm.h'
      include 'zdemparm.h'
      include 'zdemout.h'
      include 'zdempprm.h'
      include 'rdscrdat.h'

      character  echoln*80

c At this point, we are done with the parameters because the branch
c card has just been read.  So print branch message and
c initialize counters for number of branches, number of blocks in the
c current branch including output requests,
c the number of blocks the user has entered (not counting output requests),
c number of variable elements,
c number of initial conditions (initial voltages and currents),
c number of secondary branches (which branch has just
c exitted the current block),
c the last non-output request block type,
c the last block type (including output requests),
c the last type of output request (plot,print,table,file,etc.),
c number of each type of output request,
c number of tranmission lines entered,
c number of MITLs entered,
c number of voltage sources entered,
c number of current sources entered.
c number of measure zflow blocks entered.
c
c
c
      nbrns       = 1
      nblks       = 0
      nublks      = 0
      nvarl       = 0
      ninit_cond  = 0
      nsecbrn     = 1
      lcirblk     = 999
      lastblk     = 999
      numout      = 0
      numplt      = 0
      numprt      = 0
      numfil      = 0
      numtab      = 0
      numufo      = 0
      numidr      = 0
      numpff      = 0
      numcsv      = 0
      numsfc      = 0
      ntransline  = 0
      nmitline    = 0
      nvoltsource = 0
      ncurrsource = 0
      mzflowblock = 0

      echoln   = 'Listing of the User Circuit'
      call banner (echoln)
c      call print_title
      write(9,'(A/A,i2,A)')
     & ' ','************ Branch ',nbrns,' ************'
      
      return
      end

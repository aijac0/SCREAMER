c-----------------------------------------------------------------------
c  rdscrelem.f   version 1.0   created 06/15/2005 by Mathias Bavay
c-----------------------------------------------------------------------
c-----------------------------------------------------------------------
c
c  This subroutine determines if a branch abd electrical elements are
c  called in the input deck.
c  If elements are found then element counters are incremented. It
c  defines the number of branches in the input deck and defines the
c  number of element blocks in each branch. This number of
c  blocks will define the number of nodes in the problem.
c
c  Modifications:
c 2008-07-16 RBS: itabnum does not seem to be initialized
c                 it is only called in this routine
c 2008-12-08 RBS: Many formats from zdemfmt.h included herein
c 2012-04-03 RBS: Added a new variable element, R2Wall
c 2014-03-11 RBS: Added L2 as an allowed variable element in RLS
c 2014-03-11 RBS: Fixed error in block print nublk changed to nublks in
c                 several locations in variable resistors
c 2014-03-11 RBS: Change the log output to show inductance as well as
c                 resistance in the variable resistor/ind table model
c 2014-10-23 RBS: Added a dynamic hohlraum model with three shells
c                 based on the gas puff model with added input
c                 parameters.
c 2015-01-09 RBS: Adding lossyline option by duplicating TRLINE and
c                 allowing only a linear line and adding 6 parameters.
c                 This will immediately follow the TRLINE section.
c 2015-03-29 RBS: Character lengths of material and field changed in
c                 rdscrdata.h to *80 and the material write is now
c                 to (1:3) in line 2004.
c 2015-06-23 RBS: Finally, itabnum is initialized.
c 2016-04-03 RBS: Added DPF model by duplicating the gas puff model and
c                 changing the inputs and order. The new model follows
c                 the gas puff model.
c 2016-05-02 RBS: Added the radial input for the DPF model, rimass.
c 2016-12-29 RBS: Added a check on the value of theta in the DPF model
c 2017-01-02 RBS: Added the RSKin variable resistor model
c 2017-02-24 RBS: Added the currline_lc passed variable to all of the
c                 instances of get_next_line.
c                 currline_lc declared in common.
c 2017-08-04 RBS: Initialized several variables in var_model in the
c                 electron beam diode model call
c 2018-07-20 RBS: Removed itabnum from integer declaration and init
c 2019-01-21 RBS: Corrected a typo in lines 1605 and 1606 where nvarl
c                 was written nvar1.
c 2019-12-22 RBS: Added RCOND model by duplicating the R2wall model.
c 2020-01-27 RBS: Clean up of if/then/else statements for clarity
c                 IF indents are cleaned up and IFs are commented.
c 2020-09-02 RBS: Added the diode variable resistor option
c 2021-02-01 RBS: Typo on line 2355 flag1 should be flag
c-----------------------------------------------------------------------
c  readscreamerelements calls the following subroutines:
c
c       print_bad_line
c       text_to_real
c       strip
c       nshellparm
c       sphfoilparm
c       gaspuffparm
c       dpfparm
c       get_next_line
c       show_end_of_file
c       read_lsf_parms
c       read_pwl_parms
c       readctab
c       readtablem
c       abdiodeparm
c       magparm
c
c-----------------------------------------------------------------------

      subroutine readscreamerelements(status)
      
c
c Include the common blocks which are to be filled by the
c this subroutine.
c
      use zdemmax
      include 'zdemcomm.h'
      include 'zdemout.h'
c
c Include the files with the keywords and the integer flags as
c parameters.
c
      include 'zdemparm.h'
      include 'zdempprm.h'
      include 'zdemenv.h'
      include 'zdemfmt.h'
      include 'rdscrdat.h'
c
c Declare passed variables
c
      integer status

c
c Declare internal variables
c
c Diode Model
c

      real V1, I1, V2, I2, V3, I3, V4, I4, V5, I5, V6, I6

c
c Initialize variables
c
      status  = 0
c
c the first IF statement below only looks at matches in the first field.
c
c
c First check if the first field contains a branch. Increment branch counter
c
c      write(*,*) 'rdscrelem: keyword = ', keyword     
      if (keyword .eq. k_branch) then 
c
c Fill block counter array (NBK) for last branch. Then reset block
c counter and increment branch counter.
c
c status updated to k_found = 10 indicating a good element was found
         status     = k_found
c If a new branch is seen then the number of blocks in the prior branch
c is stored in nbk(nbrns). The branch counter is incremented.
         nbk(nbrns) = nblks
         nblks      = 0
         nublks     = 0
         nbrns      = nbrns + 1
         lcirblk    = 999
         lastblk    = 999
c       Print branch call in log file
         write(9,'(A/A,i2,A)')
     &   ' ','************ Branch ',nbrns,' ************'
c
c-----------------------------------------------------------------------
c Transmission line with linearly or exponentially varying impedance.
c Two, three, or four  parameters may be entered
c  ( Tau, Zin, Zout, Tres   or   Tau, Zin, Zout  or  Tau, Zin).
c If Zout is not entered, it is assumed equal to Zin and the impedance
c is constant over the line.  If Zout is entered, the impedance is
c assumed to vary over the line.  If Tres is not entered, the default
c resolution time is used.
c
      else if (keyword .eq. k_transline) then
         status             = k_found
         nblks              = nblks + 1
         nublks             = nublks + 1
         lcirblk            = transline
         lastblk            = transline
         ntransline         = ntransline + 1
         iin(1,nblks,nbrns) = transline
c
c Get the keyword for linear or exponential
c
         keyword = field(2)(1:keyword_len)

         if (keyword .eq. k_linearz) then
            itrl_type(ntransline) = linearz
            trllabel              = 'linearly     '
         else if (keyword .eq. k_exponentialz) then
            itrl_type(ntransline) = exponentialz
            trllabel              = 'exponentially'
         else
            call print_bad_line (currline, nlines, numerr)
            status=305
            return
         end if
c
c Get the parameters
c
         call text_to_real (field(3), tau, flag)
         call text_to_real (field(4), zin, flag2)
         call strip (field(6), istart6, iend6)

         if (istart6 .ne. notext) then
            call text_to_real (field(6), tresline, flag4)
         else
            tresline = res_time
            flag4    = noerr
         end if

        call strip (field(5), istart5, iend5)

        if (istart5 .ne. notext) then
           call text_to_real (field(5), zout, flag3)
        else
           zout  = zin
           flag3 = noerr
        end if

        if ((flag+flag2+flag3+flag4) .eq. noerr) then
           pin(1,nblks,nbrns) = tau
           pin(2,nblks,nbrns) = zin
           pin(3,nblks,nbrns) = zout
           pin(4,nblks,nbrns) = tresline
           write
     &     (9,'(A,i3,A,a13/A,1pe10.3,A,1pe10.3,A,1pe10.3,A,1pe10.3)')
     &     ' Block ',nublks,
     &     ' : Transmission line with impedance varying ',
     &     trllabel,'   Tau=',tau,'   Zin=',zin,
     &     '   Zout=',zout,'   Tres=',tresline
        else
           call print_bad_line (currline, nlines, numerr)
        end if

c
c-----------------------------------------------------------------------
c Lossy Transmission line with linearly varying impedance.
c Four, five, or six  parameters may be entered.
c (Tau, Zin, R1, R2, Zout, Tres).
c If Zout is not entered, it is assumed equal to Zin and the impedance
c is constant over the line.  If Zout is entered, the impedance is
c assumed to vary linearly over the length line.  If Tres is not
c entered, the default resolution time is used.
c
c lossyline here is the circuit ID that is set in zdemparm.h and is
c never changed
c
      else if (keyword .eq. k_lossyline) then
        status             = k_found
        nblks              = nblks + 1
        nublks             = nublks + 1
        lcirblk            = lossyline
        lastblk            = lossyline
c we increment ntransline here because lossy line is a TL and zdemmax.h
c limits the number of TL in the problem to 6000
        ntransline         = ntransline + 1
        iin(1,nblks,nbrns) = lossyline
c
c Linear transmission line specified
c
        itrl_type(ntransline) = linearz
        trllabel              = 'linearly     '
c
c Get the parameters
c
        call text_to_real (field(2), tau, flag )
        call text_to_real (field(3), zin, flag2)
        call text_to_real (field(4),  r1, flag3)
        call text_to_real (field(5),  r2, flag4)

        call strip (field(7), istart7, iend7)

        if (istart7 .ne. notext) then
          call text_to_real (field(7), tresline, flag6)
        else
          tresline = res_time
          flag6    = noerr
        end if

        call strip (field(6), istart6, iend6)

        if (istart6 .ne. notext) then
           call text_to_real (field(6), zout, flag5)
        else
           zout  = zin
           flag5 = noerr
        end if

        if ((flag+flag2+flag3+flag4+flag5+flag6) .eq. noerr) then
           pin(1,nblks,nbrns) = tau
           pin(2,nblks,nbrns) = zin
           pin(3,nblks,nbrns) = r1
           pin(4,nblks,nbrns) = r2
           pin(5,nblks,nbrns) = zout
           pin(6,nblks,nbrns) = tresline
           write (9,
     &    '(A,i3,A,a13/A,1pe10.3,A,1pe10.3,A,1pe10.3,A,1pe10.3,A,1pe10.3
     &     ,A,1pe10.3)')
     &    ' Block ',nublks,
     &    ' : Lossy line with impedance varying ',trllabel,
     &    '   Tau=',tau,'   Zin=',zin, '   Rloss=',r1, '   Rseries=',r2,
     &    '   Zout=',zout,'   Tres=',tresline
        else
           call print_bad_line (currline, nlines, numerr)
        end if
c
c-----------------------------------------------------------------------
c MITL with constant impedance.
c 4,5, or 6 parameters may be entered
c If Tres is not entered, it is set to the default.
c
      else if (keyword .eq. k_mitline) then
         status             = k_found
         nblks              = nblks + 1
         nublks             = nublks + 1
         lcirblk            = mitline
         lastblk            = mitline
         nmitline           = nmitline + 1
         iin(1,nblks,nbrns) = mitline
c
c Get the parameters
c
         call text_to_real (field(2), cir, flag)
         call text_to_real (field(3), gap, flag2)
         call text_to_real (field(4), tau, flag3)
         call text_to_real (field(5),   z, flag4)
         call strip (field(6), istart6, iend6)

         if (istart6 .ne. notext) then
            call text_to_real (field(6), tresline, flag5)
            call strip (field(7), istart7, iend7)

            if (istart7 .ne. notext) then
               call text_to_real (field(7), eturnon, flag6)
            else
               eturnon = 2.0e7
               flag6   = noerr
            end if

         else
            tresline = res_time
            eturnon  = 2.0e7
            flag5    = noerr
            flag6    = noerr
         end if

         if ((flag+flag2+flag3+flag4+flag5+flag6) .eq. noerr) then
            pin(1,nblks,nbrns) = cir
            pin(2,nblks,nbrns) = gap
            pin(3,nblks,nbrns) = tau
            pin(4,nblks,nbrns) = z
            pin(5,nblks,nbrns) = tresline
            pin(6,nblks,nbrns) = 1.0 / (cir * cir)
            pin(7,nblks,nbrns) = 1.0 / gap
            pin(8,nblks,nbrns) = 1.0 / (gap * gap)
            pin(9,nblks,nbrns) = eturnon
            write(9,
     &      '(A,i3,A/A,1pe10.3,A,1pe10.3,A,1pe10.3,A,1pe10.3,
     &        A,1pe10.3,A,1pe10.3)')
     &      ' Block ',nublks, ' : MITL (geometric setup)',
     &      '   Circumference=',cir,'   Gap=',gap,
     &      '   Tau=',tau,'   Z=',z,
     &      '   Tres=',tresline,'   Eturnon=',eturnon
         else
            call print_bad_line (currline, nlines, numerr)
         end if

c
c-----------------------------------------------------------------------
c MITL with perveance.
c 4 parameters entered ( Perv, Tau, Zline, tres-optional )
c
      else if (keyword .eq. k_pmitline) then
         status             = k_found
         nblks              = nblks + 1
         nublks             = nublks + 1
         lcirblk            = pmitline
         lastblk            = pmitline
         nmitline           = nmitline + 1
         iin(1,nblks,nbrns) = pmitline
c
c Get the parameters
c
         call text_to_real (field(2), perv, flag)
         call text_to_real (field(3), tau, flag2)
         call text_to_real (field(4), zline, flag3)
         call strip (field(5), istart5, iend5)

         if (istart5 .ne. notext) then
            call text_to_real (field(5), tresline, flag4)
         else
            tresline = res_time
            flag4    = noerr
         end if

         if ((flag+flag2+flag3+flag4) .eq. noerr) then
            pin(1,nblks,nbrns) = perv
            pin(2,nblks,nbrns) = tau
            pin(3,nblks,nbrns) = zline
            pin(4,nblks,nbrns) = tresline
            write(9,'(A,i3,A/A,1pe10.3,A,1pe10.3,A,1pe10.3,A,1pe10.3)')
     &      ' Block ',nublks,' : MITL (perveance setup)',
     &      '   Perveance=',perv,'   Tau=',tau,
     &      '   Z=',zline,'   Tres=',tresline
         else
            call print_bad_line (currline, nlines, numerr)
         end if
c
c-----------------------------------------------------------------------
c Resistor and capacitor to ground.
c If no value for the capacitance is entered, it is set to zero.
c
      else if (keyword .eq. k_rcground) then
         status             = k_found
         nblks              = nblks + 1
         nublks             = nublks + 1
         lcirblk            = rcground
         lastblk            = rcground
         iin(1,nblks,nbrns) = rcground
         call text_to_real (field(2), r1, flag)
c          write(6,*)'field(2) = ',field(2)
c          write(6,*)'r1 = ',r1
c          write(6,*)'flag = ',flag
         call strip (field(3), istart3, iend3)

         if (istart3 .ne. notext) then
            call text_to_real (field(3), c1, flag2)
c             write(6,*)'field(3) = ',field(3)
c             write(6,*)'c1 = ',c1
c             write(6,*)'flag2 = ',flag2
c             write(6,*)
c             write(6,*)
         else
            c1    = 0.0
            flag2 = noerr
         end if

         if ((flag+flag2) .eq. noerr) then
            pin(1,nblks,nbrns) = r1
            pin(2,nblks,nbrns) = c1
            write(9,'(A,i3,A/A,1pe10.3,A,1pe10.3)')
     &       ' Block ',nublks,' : resistor and capacitor to ground.',
     &       '   R1=',r1,'   C1=',c1
         else
            call print_bad_line (currline, nlines, numerr)
         end if
c
c-----------------------------------------------------------------------
c Resistor and inductor in series.
c If no value for the inductance is entered, it is set to zero.
c
      else if (keyword .eq. k_rlseries) then
         status             = k_found
         nblks              = nblks + 1
         nublks             = nublks + 1
         lcirblk            = rlseries
         lastblk            = rlseries
         iin(1,nblks,nbrns) = rlseries
         call text_to_real (field(2), r2, flag)
c          write(6,*)'field(2) = ',field(2)
c          write(6,*)'r2 = ',r2
c          write(6,*)'flag = ',flag
         call strip (field(3), istart3, iend3)

         if (istart3 .ne. notext) then
            call text_to_real (field(3), l2, flag2)
c             write(6,*)'field(3) = ',field(3)
c             write(6,*)'l2 = ',l2
c             write(6,*)'flag2 = ',flag2
c             write(6,*)
c             write(6,*)
         else
c
c L2 left blank in the RLSeries call
c
            l2    = 0.0
            flag2 = noerr
         end if

         if ((flag+flag2) .eq. noerr) then
            pin(1,nblks,nbrns) = r2
            pin(2,nblks,nbrns) = l2
            write(9,'(A,i3,A/A,1pe10.3,A,1pe10.3)')
     &       ' Block ',nublks,' : resistor and inductor in series.',
     &       '   R2=',r2,'   L2=',l2
         else
            call print_bad_line (currline, nlines, numerr)
         end if

c
c-----------------------------------------------------------------------
c Pi section.
c
      else if (keyword .eq. k_pisection) then
         status             = k_found
         nblks              = nblks + 1
         nublks             = nublks + 1
         lcirblk            = pisection
         lastblk            = pisection
         iin(1,nblks,nbrns) = pisection
         call text_to_real (field(2), r1, flag)
         call text_to_real (field(3), c1, flag2)
         call text_to_real (field(4), r2, flag3)
         call text_to_real (field(5), l2, flag4)
         call text_to_real (field(6), r3, flag5)
         call text_to_real (field(7), c3, flag6)

         if ((flag+flag2+flag3+flag4+flag5+flag6) .eq. noerr) then
            pin(1,nblks,nbrns) = r1
            pin(2,nblks,nbrns) = c1
            pin(3,nblks,nbrns) = r2
            pin(4,nblks,nbrns) = l2
            pin(5,nblks,nbrns) = r3
            pin(6,nblks,nbrns) = c3
            write(9,
     &      '(A,i3,A/A,1pe10.3,A,1pe10.3,A,1pe10.3,A,1pe10.3,
     &        A,1pe10.3,A,1pe10.3)')
     &      ' Block ',nublks,' : pi section. ',
     &      '   R1=',r1,'   C1=',c1,
     &      '   R2=',r2,'   L2=',l2,
     &      '   R3=',r3,'   C3=',c3
         else
            call print_bad_line (currline, nlines, numerr)
         end if

c
c-----------------------------------------------------------------------
c Adder block.
c
      else if (keyword .eq. k_adder) then
         status             = k_found
         nblks              = nblks + 1
         nublks             = nublks + 1
         lcirblk            = adder
         lastblk            = adder
         iin(1,nblks,nbrns) = adder
         write(9,'(A,i3,A)')
     &   ' Block ',nublks,' : adder block.'
c
c-----------------------------------------------------------------------
c Transformer.
c
      else if (keyword .eq. k_transformer) then
         status  = k_found
         nblks   = nblks + 1
         nublks  = nublks + 1
         lcirblk = transformer
         lastblk = transformer
         iin(1,nblks,nbrns) = transformer
         call text_to_real (field(2), zlp, flag)
         call text_to_real (field(3), zls, flag2)
         call text_to_real (field(4), zm,  flag3)

         if (flag+flag2+flag3 .eq. noerr) then
            pin(1,nblks,nbrns) = zlp
            pin(2,nblks,nbrns) = zls
            pin(3,nblks,nbrns) = zm
            write(9,'(A,i3,A/A,1pe10.3,A,1pe10.3,A,1pe10.3)')
     &       ' Block ',nublks,' : transformer block.',
     &       '   Lp=',zlp,'   Ls=',zls,
     &       '   M=',zm
         else
            call print_bad_line (currline, nlines, numerr)
         end if

c
c-----------------------------------------------------------------------
c Measure Zflow and Cathode current at a null element
c
      else if (keyword .eq. k_measurezflow) then
         status                = k_found
         nblks                 = nblks + 1
         nublks                = nublks + 1
         lcirblk               = measurezflow
         lastblk               = measurezflow
         iin(1,nblks,nbrns)    = measurezflow
         nvarl                 = nvarl + 1
         iin(2,nblks,nbrns)    = measurezflow
         ivar_block(nvarl)     = measurezflow
         ivar_block_num(nvarl) = nublks
         ivar_type(nvarl)      = measurezflow
         mzflowblock = mzflowblock + 1
         call text_to_real (field(2), Zimpedance, flag)

         if (flag .eq. noerr) then
            zofmzflow(mzflowblock) = Zimpedance
            write(9,'(A,i3,A/A,1pe10.3,A,i2)')
     &     ' Block ', nublks, ' : Measure Zflow Block',
     &     '   Vac Impedance = ', Zimpedance,
     &     '   Measure Zflow Block Number = ', mzflowblock
         else
            call print_bad_line (currline, nlines, numerr)
         end if
c
c
c-----------------------------------------------------------------------
c CYLINDICAL Foil implosion model using resistor and inductor in series
c (both variable). Parameters are: initial-radius, length, mass,
c minimum-radius
c
      else if (keyword .eq. k_cylfoilblock) then
         status             = k_found
         nblks              = nblks + 1
         nublks             = nublks + 1
         lcirblk            = cylfoilblock
         lastblk            = cylfoilblock
         iin(1,nblks,nbrns) = cylfoilblock
         nvarl              = nvarl + 1
         ivar_block(nvarl)  = cylfoilblock
         ivar_block_num(nvarl) = nublks
         ivar_type(nvarl)   = cylfoilblock
         iin(2,nblks,nbrns) = l2_var
         pin(1,nblks,nbrns) = 0.0
         pin(2,nblks,nbrns) = 0.0
         call text_to_real (field(2), rinit, flag)
         call text_to_real (field(3), xlen,  flag2)
         call text_to_real (field(4), xmass, flag3)
         call text_to_real (field(5), rmin,  flag4)

         if ((flag+flag2+flag3+flag4) .eq. noerr) then
            write(9,'(A,i3,A/A,1pe10.3,A,1pe10.3,A,1pe10.3,A,1pe10.3)')
     &       ' Block ',nublks,' : imploding CYLfoil model.',
     &       '   Initrad=',rinit,'   Length=',xlen,
     &       '   Mass=',xmass,'   Minrad=',rmin
            num_var_parms(nvarl) = 4
            var_model(1,nvarl)   = rinit
            var_model(2,nvarl)   = xlen
            var_model(3,nvarl)   = xmass
            var_model(4,nvarl)   = rmin
            radyields=.false.
c
c Now call a subroutine which will set the many parameters needed to
c do this foil model.
c
            call cylfoilparm (var_model(1,nvarl), num_var_parms(nvarl))
         else
            call print_bad_line (currline, nlines, numerr)
         end if

c
c-----------------------------------------------------------------------
c N SHELL implosion model using resistor and inductor in series
c (both variable). Parameters are: radius1, radius2, mass1, mass2,
c length, and final radius
c
      else if (keyword .eq. k_nshellblock) then
         status             = k_found
         nblks              = nblks + 1
         nublks             = nublks + 1
         lcirblk            = nshellblock
         lastblk            = nshellblock
         iin(1,nblks,nbrns) = nshellblock
         nvarl              = nvarl + 1
         ivar_block(nvarl)  = nshellblock
         ivar_block_num(nvarl) = nublks
         ivar_type(nvarl)   = nshellblock
         iin(2,nblks,nbrns) = l2_var
         pin(1,nblks,nbrns) = 0.0
         pin(2,nblks,nbrns) = 0.0
         call text_to_real (field(2), xlength, flag)
         call text_to_real (field(3), rmin,  flag2)
         call text_to_real (field(4), akgap, flag3)
         call strip (field(5), istart5, iend5)

         if (istart5 .ne. notext) then
            call text_to_real (field(5), ttrap, flag4)
         else
            ttrap  = 0.0
            flag4 = noerr
         end if

         if ((flag+flag2+flag3+flag4) .eq. noerr) then
            write(9,'(A,i3,A/A,1pe10.3,A,1pe10.3,A,1pe10.3,A,1pe10.3)')
     &      ' Block ',nublks,' : imploding NShell model.',
     &      '  Shell length =',xlength,'   min radius =',rmin,
     &      '  A-K gap = ',akgap,'   trapped field time = ',ttrap
            num_var_parms(nvarl) = 4
            var_model(1,nvarl)   = xlength
            var_model(2,nvarl)   = rmin
            var_model(3,nvarl)   = akgap
            var_model(4,nvarl)   = ttrap
            nsaverr = numerr
c
c Now read the table of shell radii and masses
c
            call read_pwl_parms (shellparms,nparams, eofflg,
     &                         nlines, numerr)

            if (eofflg .eq. noerr) then

               if (nparams/2 .gt. max_shells) then
                  write(9,'(A,i2,A,i2)')
     &            'Too many shells.  Number of shells is ',
     &            nparams/2,'   Max allowed is ',max_shells
                  status=1000
                  return
               endif

               if (numerr .eq. nsaverr) then
                  write(9,48) 'Radius      ', 'Mass   ',
     &                  ((j+1)/2, shellparms(j),
     &                  shellparms(j+1),
     &                  j=1,nparams-1,2)
               end if

            else
               call show_end_of_file (nlines, numerr)
               status=1000
               return
            end if

            numshells = nparams/2

            do nx=1,nparams-1,2
               shellradius((nx+1)/2)=shellparms(nx)
               shellmass((nx+1)/2)=shellparms(nx+1)
            end do

            do nx=1,numshells-1

               if (shellradius(nx).lt.shellradius(nx+1)) then
                  write(9,'(A)') 'Shell radii not in correct sequence'
                  status=1000
                  return
               endif

            end do
c
c
c Now call a subroutine which will set the parameters needed to
c do this shell model.
c
            call nshellparm (var_model(1,nvarl), num_var_parms(nvarl))
         else
            call print_bad_line (currline, nlines, numerr)
         end if

c
c-----------------------------------------------------------------------
c SPHERICAL Foil implosion model using resistor and inductor in series
c (both variable).
c Parameters are: initial-radius, angle, mass, min-radius 
c
      else if (keyword .eq. k_sphfoilblock) then
         status             = k_found
         nblks              = nblks + 1
         nublks             = nublks + 1
         lcirblk            = sphfoilblock
         lastblk            = sphfoilblock
         iin(1,nblks,nbrns) = sphfoilblock
         nvarl              = nvarl + 1
         ivar_block(nvarl)  = sphfoilblock
         ivar_block_num(nvarl) = nublks
         ivar_type(nvarl)   = sphfoilblock
         iin(2,nblks,nbrns) = l2_var
         pin(1,nblks,nbrns) = 0.0
         pin(2,nblks,nbrns) = 0.0
         call text_to_real (field(2), rinit, flag)
         call text_to_real (field(3), xangl, flag2)
         call text_to_real (field(4), xmass, flag3)
         call text_to_real (field(5), rmin,  flag4)

        if ((flag+flag2+flag3+flag4) .eq. noerr) then
            write(9,'(A,i3,A/A,1pe10.3,A,1pe10.3,A,1pe10.3,A,1pe10.3)')
     &       ' Block ',nublks,' : imploding SPHfoil model.',
     &       '   Initrad=',rinit,'   ANGLE=',xangl,
     &       '   Mass=',xmass,'   Minrad=',rmin
           num_var_parms(nvarl) = 4
           var_model(1,nvarl)   = rinit
           var_model(2,nvarl)   = xangl
           var_model(3,nvarl)   = xmass
           var_model(4,nvarl)   = rmin
c
c Now call a subroutine which will set the many parameters needed to
c do this foil model.
c
           call sphfoilparm (var_model(1,nvarl), num_var_parms(nvarl))
        else
           call print_bad_line (currline, nlines, numerr)
        end if

c
c-----------------------------------------------------------------------
c Dynamic hohlraum implosion model using resistor and inductor in series
c (both variable).
c Parameters are: length, rinit, mass liner 1, radius liner 2, mass
c                 liner2, radius liner 3 & outer foam, mass liner 3,
c                 density, inner radius of foam, minimum radius of
c                 foam
c
      else if (keyword .eq. k_dyhohlblock) then
         status             = k_found
         nblks              = nblks + 1
         nublks             = nublks + 1
         lcirblk            = dyhohlblock
         lastblk            = dyhohlblock
         iin(1,nblks,nbrns) = dyhohlblock
         nvarl              = nvarl + 1
         ivar_block(nvarl)  = dyhohlblock
         ivar_block_num(nvarl) = nublks
         ivar_type(nvarl)   = dyhohlblock
         iin(2,nblks,nbrns) = l2_var
         pin(1,nblks,nbrns) = 0.0
         pin(2,nblks,nbrns) = 0.0
         call text_to_real (field(2), xlen,    flag)
         call text_to_real (field(3), rinit,   flag2)
         call text_to_real (field(4), xmliner1,flag3)
         call text_to_real (field(5), rliner2, flag4)
         call text_to_real (field(6), xmliner2,flag5)
         call text_to_real (field(7), router,  flag6)
         call text_to_real (field(8), xmliner3,flag7)
         call text_to_real (field(9), density, flag8)
         call text_to_real (field(10), rinner, flag9)
         call text_to_real (field(11), rmin,   flag10)

         if
     &    ((flag+flag2+flag3+flag4+flag5+flag6+flag7+flag8+flag9+flag10)
     &      .eq. noerr) then
            write(9,'(A,i3,A/A,1pe10.3,A,1pe10.3,A,1pe10.3,A,1pe10.3,A,
     &                1pe10.3,A,1pe10.3,A,1pe10.3,A,1pe10.3,A,1pe10.3,
     &                A,1pe10.3)')
     &       ' Block ',nublks,' : dynamic hohlraum model.',
     &       '   Length=',xlen,
     &       '   Rad liner1=',rinit,'   Mass liner 1=',xmliner1,
     &       '   Rad liner2=',rliner2,'   Mass liner 2=',xmliner2,
     &       '   Rad liner3=',router,'   Mass liner 3=',xmliner3,
     &       '   Density=',density,
     &       '   Rad inner=',rinner,'   Rad min=',rmin
            num_var_parms(nvarl) = 10

            if (xmliner1 .le. 0.0) xmliner1 = 1.0e-12
            if (xmliner2 .le. 0.0) xmliner2 = 1.0e-12
            if (xmliner3 .le. 0.0) xmliner3 = 1.0e-12
            if (rinner .lt. rmin) rinner = rmin

            var_model(1,nvarl)    = xlen
            var_model(2,nvarl)    = rinit
            var_model(3,nvarl)    = xmliner1
            var_model(4,nvarl)    = rliner2
            var_model(5,nvarl)    = xmliner2
            var_model(6,nvarl)    = router
            var_model(7,nvarl)    = xmliner3
            var_model(8,nvarl)    = density
            var_model(9,nvarl)    = rinner
            var_model(10,nvarl)   = rmin
c
c Now call a subroutine which will set the many parameters needed to
c do this dynamic hohlraum model.
c
            call dyhohlraumparm
     &                        (var_model(1,nvarl), num_var_parms(nvarl))
         else
            call print_bad_line (currline, nlines, numerr)
         end if

c
c-----------------------------------------------------------------------
c GAS PUFF implosion model using resistor and inductor in series
c (both variable).
c Parameters are: initial-radius, length, density, minimum-radius,
c inner-radius, initial-mass
c
      else if (keyword .eq. k_gaspuffblock) then
         status             = k_found
         nblks              = nblks + 1
         nublks             = nublks + 1
         lcirblk            = gaspuffblock
         lastblk            = gaspuffblock
         iin(1,nblks,nbrns) = gaspuffblock
         nvarl              = nvarl + 1
         ivar_block(nvarl)  = gaspuffblock
         ivar_block_num(nvarl) = nublks
         ivar_type(nvarl)   = gaspuffblock
         iin(2,nblks,nbrns) = l2_var
         pin(1,nblks,nbrns) = 0.0
         pin(2,nblks,nbrns) = 0.0
         call text_to_real (field(2), rinit,   flag)
         call text_to_real (field(3), xlen,    flag2)
         call text_to_real (field(4), density, flag3)
         call text_to_real (field(5), rmin,    flag4)
         call text_to_real (field(6), rinner,  flag5)
         call text_to_real (field(7), ximass,  flag6)

         if ((flag+flag2+flag3+flag4+flag5+flag6) .eq. noerr) then
            write(9,'(A,i3,A/A,1pe10.3,A,1pe10.3,A,1pe10.3,A,
     &       1pe10.3,A,1pe10.3,A,1pe10.3)')
     &       ' Block ',nublks,' : imploding gas puff model.',
     &       '   Initrad=',rinit,'   Length=',xlen,
     &       '   Density=',density,'   Minrad=',rmin,
     &       '   Innerrad=',rinner,'   InitMass=',ximass
            num_var_parms(nvarl) = 6

            if (ximass .le. 0.0) ximass = 1.0e-12

            var_model(1,nvarl)   = rinit
            var_model(2,nvarl)   = xlen
            var_model(3,nvarl)   = density
            var_model(4,nvarl)   = rmin
            var_model(5,nvarl)   = rinner
            var_model(6,nvarl)   = ximass
c
c Now call a subroutine which will set the many parameters needed to
c do this gaspuff model.
c
            call gaspuffparm (var_model(1,nvarl), num_var_parms(nvarl))
         else
            call print_bad_line (currline, nlines, numerr)
         end if

c
c-----------------------------------------------------------------------
c DPF implosion model using resistor and inductor in series
c (both variable).
c Parameters are: outer-radius, inner-radius, length, fill density,
c fraction of radial mass, minimum-radius, initial-mass, sheath angle
c
      else if (keyword .eq. k_dpfblock) then
         status             = k_found
         nblks              = nblks + 1
         nublks             = nublks + 1
         lcirblk            = dpfblock
         lastblk            = dpfblock
         iin(1,nblks,nbrns) = dpfblock
         nvarl              = nvarl + 1
         ivar_block(nvarl)  = dpfblock
         ivar_block_num(nvarl) = nublks
         ivar_type(nvarl)   = dpfblock
         iin(2,nblks,nbrns) = l2_var
         pin(1,nblks,nbrns) = 0.0
         pin(2,nblks,nbrns) = 0.0
         call text_to_real (field(2), router ,  flag)
         call text_to_real (field(3), rinner , flag2)
         call text_to_real (field(4), zlen   , flag3)
         call text_to_real (field(5), density, flag4)
         call text_to_real (field(6), rimass , flag5)
         call text_to_real (field(7), rmin   , flag6)
         call text_to_real (field(8), ximass , flag7)
         call text_to_real (field(9), theta  , flag8)

         if ((flag+flag2+flag3+flag4+flag5+flag6+flag7+flag8)
     &      .eq. noerr) then
            write(9,'(A,i3,A/A,1pe10.3,A,1pe10.3,A,1pe10.3,A,
     &       1pe10.3,A,1pe10.3,A,1pe10.3,A,1pe10.3,A,1pe10.3)')
     &       ' Block ',nublks,' : dpf model',
     &       '   Router=', router,'   Rinner=', rinner,
     &       '   Length=', zlen    ,'   Density=' , density,
     &       '   Radial mass fraction=', rimass,
     &       '   Minrad=', rmin    ,
     &       '   InitMass=', ximass, '  Theta=',theta
            num_var_parms(nvarl) = 8

            if (ximass .le. 0.0) ximass = 1.0e-12
            if (theta .le. 0.10) theta = 0.10
            if (theta .ge. 85.0) theta = 85.0

c
c Convert theta to radians
c
            theta = theta * ( 3.14159 / 180.0 )

            var_model(1,nvarl)   = router
            var_model(2,nvarl)   = rinner
            var_model(3,nvarl)   = zlen
            var_model(4,nvarl)   = density
            var_model(5,nvarl)   = rimass
            var_model(6,nvarl)   = rmin
            var_model(7,nvarl)   = ximass
            var_model(8,nvarl)   = theta
c
c Now call a subroutine which will set the many parameters needed to
c do this dpf model.
c
            call dpfparm (var_model(1,nvarl), num_var_parms(nvarl))
         else
            call print_bad_line (currline, nlines, numerr)
         end if
c
c-----------------------------------------------------------------------
c Voltage source described by a function in time.
c A voltsource occurs at the beginning of the main branch,
c a vendsource occurs at the end of a secondary branch.
c
      else if ((keyword .eq. k_voltsource) .or.
     &         (keyword .eq. k_vendsource)) then
         status             = k_found
         nblks              = nblks + 1
         nublks             = nublks + 1
         nvoltsource        = nvoltsource + 1

         if (keyword .eq. k_vendsource) then
            lcirblk             = vendsource
            lastblk             = vendsource
            iin(1,nblks,nbrns)  = vendsource
            ivbranch_end(nbrns) = nvoltsource
         else
            lcirblk             = voltsource
            lastblk             = voltsource
            iin(1,nblks,nbrns)  = voltsource
         end if

c
c Get R and L (3rd and 4th fields)
c
         call text_to_real (field(3), r2, flag)
         call text_to_real (field(4), l2, flag2)

         if ((flag+flag2) .eq. noerr) then
            pin(1,nblks,nbrns) = r2
            pin(2,nblks,nbrns) = l2

            if (lcirblk .eq. vendsource) then
              write(9,'(A,i3,A,A/A,1pe10.3,A,1pe10.3)')
     &        ' Block ',nublks,' : end-of-branch voltage source ',
     &        'described as a function of time.',
     &        '   R2=',r2,'   L2=',l2
            else
               write(9,'(A,i3,A,A/A,1pe10.3,A,1pe10.3)')
     &        ' Block ',nublks,' : voltage source described as a ',
     &        'function of time.','   R2=',r2,'   L2=',l2
            end if

         else
            call print_bad_line (currline, nlines, numerr)
            status=305
            return
         end if

c
c Get the function type from the second keyword
c and read in the next line with the function parameters in it.
c
         keyword = field(2)(1:keyword_len)
c
         if (keyword .eq. k_sinsquared) then
            status              = k_found
            ivoltf(nvoltsource) = sinsquared
            call get_next_line
     &        (currline, currline_lc, field, nlines, eofflg, max_fields)
            if (eofflg .eq. noerr) then
               call text_to_real (field(1), vmax, flag)
               call text_to_real (field(2), tp,   flag2)
               call strip (field(3), istart3, iend3)

               if (istart3 .ne. notext) then
                  call text_to_real (field(3), delay, flag3)
               else
                  delay = 0.0
                  flag3 = noerr
               end if

               if (flag+flag2+flag3 .eq. noerr) then
                  num_voltf_parms(nvoltsource) = 3
                  voltf_parms(1,nvoltsource)   = vmax
                  voltf_parms(2,nvoltsource)   = tp
                  voltf_parms(3,nvoltsource)   = delay
                  write(9,'(A/A,1pe10.3,A,1pe10.3,A,1pe10.3)')
     &            '    Sin-squared function:',
     &            '      Magnitude=',vmax,'   Tpulse=',tp,
     &            '   Tdelay=',delay
               else
                  call print_bad_line (currline, nlines, numerr)
               end if

            else
               call show_end_of_file (nlines, numerr)
               status=1000
               return
            end if

         else if (keyword .eq. k_sinfun) then
            status              = k_found
            ivoltf(nvoltsource) = sinfun
            call get_next_line
     &        (currline, currline_lc, field, nlines, eofflg, max_fields)

            if (eofflg .eq. noerr) then
               call text_to_real (field(1), vmax, flag)
               call text_to_real (field(2), tp,   flag2)
               call strip (field(3), istart3, iend3)

               if (istart3 .ne. notext) then
                  call text_to_real (field(3), delay, flag3)
               else
                  delay = 0.0
                  flag3 = noerr
               end if

               if (flag+flag2+flag3 .eq. noerr) then
                  num_voltf_parms(nvoltsource) = 3
                  voltf_parms(1,nvoltsource)   = vmax
                  voltf_parms(2,nvoltsource)   = tp
                  voltf_parms(3,nvoltsource)   = delay
                  write(9,'(A/A,1pe10.3,A,1pe10.3,A,1pe10.3)')
     &            '    Sin function:',
     &            '      Magnitude=',vmax,'   Period=',tp,
     &            '   Tdelay=',delay
               else
                  call print_bad_line (currline, nlines, numerr)
               end if

            else
               call show_end_of_file (nlines, numerr)
               status=1000
               return
            end if

         else if (keyword .eq. k_leastsquares) then
            status              = k_found
            ivoltf(nvoltsource) = leastsquares
            nsaverr             = numerr
            call read_lsf_parms (voltf_parms(1,nvoltsource),
     &                         num_voltf_parms(nvoltsource), eofflg,
     &                         nlines, numerr)

            if (eofflg .eq. noerr) then

               if (numerr .eq. nsaverr) then
                  write(9,47) (j-1, voltf_parms(j,nvoltsource),
     &                   j=1,num_voltf_parms(nvoltsource))
               end if

            else
               call show_end_of_file (nlines, numerr)
               status=1000
               return
            end if

         else if (keyword .eq. k_piecewiselinear) then
            status              = k_found
            ivoltf(nvoltsource) = piecewiselinear
            nsaverr             = numerr
            call read_pwl_parms (voltf_parms(1,nvoltsource),
     &                         num_voltf_parms(nvoltsource), eofflg,
     &                         nlines, numerr)

            if (eofflg .eq. noerr) then

               if (numerr .eq. nsaverr) then
                  write(9,48) 'Time      ', 'Voltage   ',
     &                  ((j+1)/2, voltf_parms(j,nvoltsource),
     &                  voltf_parms(j+1,nvoltsource),
     &                  j=1,num_voltf_parms(nvoltsource)-1,2)
               end if

            else
               call show_end_of_file (nlines, numerr)
               status=1000
               return
            end if

         else if (keyword .eq. k_table) then
            status               = k_found
            ivoltf (nvoltsource) = table
            nsaverr              = numerr
            call read_pwl_parms (voltf_parms(1,nvoltsource),
     &      num_voltf_parms(nvoltsource), eofflg,
     &      nlines, numerr)

            if (eofflg .eq. noerr) then

               if (numerr .eq. nsaverr) then
                  write(9,49) (voltf_parms(j,nvoltsource),j=1,2)
                  write(9,48) 'Time      ', 'Voltage   ',
     &                  ((j-1)/2, voltf_parms(j,nvoltsource),
     &                  voltf_parms(j+1,nvoltsource),
     &                  j=3,num_voltf_parms(nvoltsource)-1,2)
               end if
            else
               call show_end_of_file (nlines, numerr)
               status=1000
               return
            end if

         else
            call print_bad_line (currline, nlines, numerr)
         end if

c
c-----------------------------------------------------------------------
c Current source described by a function in time.
c A currsource occurs at the beginning of the main branch,
c a cendsource occurs at the end of a secondary branch.
c
      else if ((keyword .eq. k_currsource) .or.
     &         (keyword .eq. k_cendsource)) then
         status      = k_found
         nblks       = nblks + 1
         nublks      = nublks + 1
         ncurrsource = ncurrsource + 1

         if (keyword .eq. k_cendsource) then
            itypcend(ncurrsource) = cendsource
            lcirblk               = cendsource
            lastblk               = cendsource
            iin(1,nblks,nbrns)    = cendsource
            icbranch_end(nbrns)   = ncurrsource
         else if (keyword .eq. k_currsource) then
            lcirblk               = currsource
            lastblk               = currsource
            iin(1,nblks,nbrns)    = currsource
         end if

c
c Get R and C (3rd and 4th fields)
c
         call text_to_real (field(3), r, flag)
         call text_to_real (field(4), c, flag2)

         if ((flag+flag2) .eq. noerr) then
            pin(1,nblks,nbrns) = r
            pin(2,nblks,nbrns) = c

            if (lcirblk .eq. currsource) then
               write(9,'(A,i3,A,A/A,1pe10.3,A,1pe10.3)')
     &         ' Block ',nublks,' : current source described as a ',
     &         'function of time.',
     &         '   R3=',r,'   C3=',c
            else if (lcirblk .eq. cendsource) then
               write(9,'(A,i3,A,A/A,1pe10.3,A,1pe10.3)')
     &         ' Block ',nublks,' : end-of-branch current source ',
     &         'described as a function of time.',
     &         '   R1=',r,'   C1=',c
            end if

         else
            call print_bad_line (currline, nlines, numerr)
            status=305
            return
         end if
c
c-----------------------------------------------------------------------------
c NOTE: We are using a new IF statement to look at the second field
c in the command string for voltage and current inputs

c
c Get the function type from the second keyword
c and read in the next line with the function parameters in it.
c
         keyword = field(2)(1:keyword_len)

         if (keyword .eq. k_sinsquared) then
            status              = k_found
            icurrf(ncurrsource) = sinsquared
            call get_next_line
     &        (currline, currline_lc, field, nlines, eofflg, max_fields)

            if (eofflg .eq. noerr) then
               call text_to_real (field(1), cmax, flag)
               call text_to_real (field(2), tp,   flag2)
               call strip (field(3), istart3, iend3)

               if (istart3 .ne. notext) then
                  call text_to_real (field(3), delay, flag3)
               else
                  delay = 0.0
                  flag3 = noerr
               end if

               if (flag+flag2+flag3 .eq. noerr) then
                  num_currf_parms(ncurrsource) = 3
                  currf_parms(1,ncurrsource)   = cmax
                  currf_parms(2,ncurrsource)   = tp
                  currf_parms(3,ncurrsource)   = delay
                  write(9,'(A/A,1pe10.3,A,1pe10.3,A,1pe10.3)')
     &            '    Sin-squared function:',
     &            '      Magnitude=',cmax,'   Tpulse=',tp,
     &            '   Tdelay=',delay
               else
                  call print_bad_line (currline, nlines, numerr)
               end if

            else
               call show_end_of_file (nlines, numerr)
               status=1000
               return
            end if

         else if (keyword .eq. k_sinfun) then
            status              = k_found
            icurrf(ncurrsource) = sinfun
            call get_next_line
     &        (currline, currline_lc, field, nlines, eofflg, max_fields)

            if (eofflg .eq. noerr) then
               call text_to_real (field(1), cmax, flag)
               call text_to_real (field(2), tp,   flag2)
               call strip (field(3), istart3, iend3)

               if (istart3 .ne. notext) then
                  call text_to_real (field(3), delay, flag3)
               else
                  delay = 0.0
                  flag3 = noerr
               end if

               if (flag+flag2+flag3 .eq. noerr) then
                  num_currf_parms(ncurrsource) = 3
                  currf_parms(1,ncurrsource)   = cmax
                  currf_parms(2,ncurrsource)   = tp
                  currf_parms(3,ncurrsource)   = delay
                  write(9,'(A/A,1pe10.3,A,1pe10.3,A,1pe10.3)')
     &            '    Sin function:',
     &            '      Magnitude=',cmax,'   Period=',tp,
     &            '   Tdelay=',delay
               else
                  call print_bad_line (currline, nlines, numerr)
               end if

            else
               call show_end_of_file (nlines, numerr)
               status=1000
               return
            end if

         else if (keyword .eq. k_leastsquares) then
            status              = k_found
            icurrf(ncurrsource) = leastsquares
            nsaverr             = numerr
            call read_lsf_parms (currf_parms(1,ncurrsource),
     &                         num_currf_parms(ncurrsource), eofflg,
     &                         nlines, numerr)

            if (eofflg .eq. noerr) then

               if (numerr .eq. nsaverr) then
                  write(9,47) (j-1, currf_parms(j,ncurrsource),
     &                   j=1,num_currf_parms(ncurrsource))
               end if

            else
               call show_end_of_file (nlines, numerr)
               status=1000
               return
            end if

         else if (keyword .eq. k_piecewiselinear) then
            status              = k_found
            icurrf(ncurrsource) = piecewiselinear
            nsaverr             = numerr
            call read_pwl_parms (currf_parms(1,ncurrsource),
     &                         num_currf_parms(ncurrsource), eofflg,
     &                         nlines, numerr)

            if (eofflg .eq. noerr) then

               if (numerr .eq. nsaverr) then
                  write(9,48) 'Time      ', 'Current    ',
     &                  ((j+1)/2, currf_parms(j,ncurrsource),
     &                  currf_parms(j+1,ncurrsource),
     &                  j=1,num_currf_parms(ncurrsource)-1,2)
               end if

            else
               call show_end_of_file (nlines, numerr)
               status=1000
               return
            end if

         else if (keyword .eq. k_table) then
            status               = k_found
            icurrf (ncurrsource) = table
            nsaverr              = numerr
            call read_pwl_parms (currf_parms(1,ncurrsource),
     &                         num_currf_parms(ncurrsource), eofflg,
     &                         nlines, numerr)

            if (eofflg .eq. noerr) then

               if (numerr .eq. nsaverr) then
                  write(9,49) (currf_parms(j,ncurrsource),j=1,2)
                  write(9,48) 'Time      ', 'Current   ',
     &                  ((j-1)/2, currf_parms(j,ncurrsource),
     &                  currf_parms(j+1,ncurrsource),
     &                  j=3,num_currf_parms(ncurrsource)-1,2)
               end if

            else
               call show_end_of_file (nlines, numerr)
               status=1000
               return
            end if

         else
            call print_bad_line (currline, nlines, numerr)
         end if
c
c-----------------------------------------------------------------------------
c Ends the IF statement looking for the second field in voltage and current
c sources
c
c-----------------------------------------------------------------------------
c SCL Current source described by a table
c
      else if (keyword .eq. k_csclsource) then
         status                = k_found
         nblks                 = nblks + 1
         nublks                = nublks + 1
         ncurrsource           = ncurrsource + 1
         itypcend(ncurrsource) = csclsource
         lcirblk               = csclsource
         lastblk               = csclsource
         iin(1,nblks,nbrns)    = csclsource
         icbranch_end(nbrns)   = ncurrsource
c
c Get R and C (2rd and 3th fields)
c
         call text_to_real (field(2), r, flag)
         call text_to_real (field(3), c, flag2)

         if ((flag+flag2) .eq. noerr) then
            pin(1,nblks,nbrns) = r
            pin(2,nblks,nbrns) = c
            write(9,'(A,i3,A,A/A,1pe10.3,A,1pe10.3)')
     &      ' Block ',nublks,' : end-of-branch SCL current source ',
     &      'described as a function of time.',
     &      '   R1=',r,'   C1=',c
         else
           call print_bad_line (currline, nlines, numerr)
           status=305
           return
         end if

c
c Read in the conditional table.
c
         nsaverr = numerr
         call readctab (currf_parms(1,ncurrsource),
     &                 num_currf_parms(ncurrsource), eofflg,
     &                 nlines, numerr)

         if (eofflg .eq. noerr) then

            if (numerr .eq. nsaverr) then
               write(9,86) (currf_parms(j,ncurrsource),j=1,3)
               write(9,85) 'Time      ', 'Current   ', 'Min-voltg ',
     &                ((j-1)/3, currf_parms(j,ncurrsource),
     &                currf_parms(j+1,ncurrsource),
     &                currf_parms(j+2,ncurrsource),
     &                j=4,num_currf_parms(ncurrsource)-2,3)
               currf_parms(2,ncurrsource) =
     &                                 1.0 / currf_parms(2,ncurrsource)
            end if

            status=305
            return
         else
            call show_end_of_file (nlines, numerr)
            status=1000
            return
         end if

c
c-----------------------------------------------------------------------
c Variable element in the last block type in field 1
c
      else if (keyword .eq. k_variable) then
         status            = k_found
         last_block        = iin(1,nblks,nbrns)
         nvarl             = nvarl + 1
         ivar_block(nvarl) = last_block
         ivar_block_num(nvarl) = nublks
c
c Second keyword to specify which element is the variable element.
c Check against prior block type.
c
         keyword = field(2)(1:keyword_len)

         if (last_block .eq. pisection) then

            if      (keyword .eq. k_r1_var) then
               iin(2,nblks,nbrns) = r1_var
            else if (keyword .eq. k_c1_var) then
               iin(2,nblks,nbrns) = c1_var
            else if (keyword .eq. k_r2_var) then
               iin(2,nblks,nbrns) = r2_var
            else if (keyword .eq. k_l2_var) then
               iin(2,nblks,nbrns) = l2_var
            else if (keyword .eq. k_r3_var) then
               iin(2,nblks,nbrns) = r3_var
            else if (keyword .eq. k_c3_var) then
               iin(2,nblks,nbrns) = c3_var
            else
               call print_bad_line (currline, nlines, numerr)
               status=305
               return
            end if

         else if (last_block .eq. rcground) then

            if      (keyword .eq. k_r1_var) then
               iin(2,nblks,nbrns) = r1_var
            else if (keyword .eq. k_c1_var) then
               iin(2,nblks,nbrns) = c1_var
            else
               call print_bad_line (currline, nlines, numerr)
               status=305
               return
            end if

         else if (last_block .eq. rlseries) then

            if      (keyword .eq. k_r2_var) then
               iin(2,nblks,nbrns) = r2_var
            else if (keyword .eq. k_l2_var) then
               iin(2,nblks,nbrns) = l2_var
            else
               call print_bad_line (currline, nlines, numerr)
               status=305
               return
            end if

         else if (last_block .eq. voltsource) then

            if      (keyword .eq. k_r2_var) then
               iin(2,nblks,nbrns) = r2_var
            else if (keyword .eq. k_l2_var) then
               iin(2,nblks,nbrns) = l2_var
            else
               call print_bad_line (currline, nlines, numerr)
               status=305
               return
            end if
c
         else
            call print_bad_line (currline, nlines, numerr)
            status=305
            return
         end if

         kelement = iin(2,nblks,nbrns)
         label2   = keyword(1:2)
c
c Third keyword for model type (lsf and pwl models not active yet) for the
c variable element model
c
         keyword  = field(3)(1:keyword_len)

c
c-----------------------------------------------------------------------
c User supplied model - all parameters are internal to the user's function.
c The function is compiled by the user before executing SCREAMER.
c
         if (keyword .eq. k_user_model) then
            status           = k_found
            ivar_type(nvarl) = user_model
            write(9,'(A,a2,A,i3)')
     &      '     ',label2,' is a variable element in block ',nublks
            write(9,'(A)') '      User supplied model.'

         else if (keyword .eq. k_user1_model) then
            status           = k_found
            ivar_type(nvarl) = user1_model
            write(9,'(A,a2,A,i3)')
     &      '     ',label2,' is a variable element in block ',nublks
            write(9,'(A)') '      User supplied model.'

         else if (keyword .eq. k_user2_model) then
            status           = k_found
            ivar_type(nvarl) = user2_model
            write(9,'(A,a2,A,i3)')
     &      '     ',label2,' is a variable element in block ',nublks
            write(9,'(A)') '      User supplied model.'
c
         else if (keyword .eq. k_user3_model) then
            status           = k_found
            ivar_type(nvarl) = user3_model
            write(9,'(A,a2,A,i3)')
     &     '     ',label2,' is a variable element in block ',nublks
           write(9,'(A)') '      User supplied model.'
c
         else if (keyword .eq. k_user4_model) then
            status           = k_found
            ivar_type(nvarl) = user4_model
            write(9,'(A,a2,A,i3)')
     &      '     ',label2,' is a variable element in block ',nublks
            write(9,'(A)') '      User supplied model.'
c
c-----------------------------------------------------------------------
c Table model of variable resistor.
c Add an inductor as a variable element
c
         else if (keyword .eq. k_tab_model) then
            status           = k_found
            itabnum          = itabnum + 1
            ivar_type(nvarl) = tab_model
            write(9,'(A,a2,A,i3)')
     &      '     ',label2,' is a variable element in block ',nublks

            if ((kelement .ne. r1_var) .and.
     &         (kelement .ne. r2_var) .and.
     &         (kelement .ne. r3_var) .and.
     &         (kelement .ne. l2_var)) then
c  L2 added above this line
               write(9,'(A,a2,A)')
     &         '      ### ERROR ###  element ',label2,
     &         ' may not be used with this model.'
               numerr = numerr + 1
            end if

            nsaverr = numerr
            call read_tablem(tablem_vals(1,itabnum),
     &                     num_tablem_vals(itabnum),
     &                     eofflg, nlines, numerr)

            if (eofflg .eq. noerr) then

               if (numerr .eq. nsaverr) then
                  write(9,49) (tablem_vals(j,itabnum),j=1,2)
                  if (kelement .eq. l2_var) then
                     write(9,48) 'Time      ', 'Inductance   ',
     &                  ((j-1)/2, tablem_vals(j,itabnum),
     &                  tablem_vals(j+1,itabnum),
     &                  j=3,num_tablem_vals(itabnum)-1,2)
                  else
                     write(9,48) 'Time      ', 'Resistance   ',
     &                  ((j-1)/2, tablem_vals(j,itabnum),
     &                  tablem_vals(j+1,itabnum),
     &                  j=3,num_tablem_vals(itabnum)-1,2)
                  end if

               end if

            else
               call show_end_of_file (nlines, numerr)
               status=1000
               return
            end if

c
c-----------------------------------------------------------------------
c Exponential model of variable resistor.
c
         else if (keyword .eq. k_exp_model) then
            status           = k_found
            ivar_type(nvarl) = exp_model
            write(9,'(A,a2,A,i3)')
     &      '     ',label2,' is a variable element in block ',nublks

            if ((kelement .ne. r1_var) .and.
     &         (kelement .ne. r2_var) .and.
     &         (kelement .ne. r3_var)) then
               write(9,'(A,a2,A)')
     &         '      ### ERROR ###  element ',label2,
     &         ' may not be used with this model.'
               numerr = numerr + 1
            end if

            call get_next_line
     &        (currline, currline_lc, field, nlines, eofflg, max_fields)

            if (eofflg .eq. noerr) then
               call text_to_real (field(1), ropen,  flag)
               call text_to_real (field(2), rclose, flag2)
               call text_to_real (field(3), timsw,  flag3)
               call text_to_real (field(4), tau,    flag4)
               call text_to_real (field(5), zsw,    flag5)

               if (flag+flag2+flag3+flag4+flag5 .eq. noerr) then
                  num_var_parms(nvarl) = 5
                  var_model(1,nvarl)   = ropen
                  var_model(2,nvarl)   = rclose
                  var_model(3,nvarl)   = timsw
                  var_model(4,nvarl)   = 1.0 / tau
                  var_model(5,nvarl)   = zsw
                  write(9,
     &          '(A/A,1pe10.3,A,1pe10.3,A,1pe10.3,A,1pe10.3,A,1pe10.3)')
     &          '      Exponential model of a resistive switch:',
     &          '        Ropen=',ropen,'   Rclose=',rclose,
     &          '   Tswitch=',timsw,'   Tau=',tau,
     &          '   Zswitch=',zsw
               else
                   call print_bad_line (currline, nlines, numerr)
               end if
c
            else
               call show_end_of_file (nlines, numerr)
               status=1000
               return
            end if

c
c-----------------------------------------------------------------------
c Decay or Rise model of variable resistor.
c
         else if ((keyword .eq. k_decay_model) .or.
     &           (keyword .eq. k_rise_model))     then
            status           = k_found

            if (keyword .eq. k_decay_model) then
               ivar_type(nvarl) = decay_model
            else
               ivar_type(nvarl) = rise_model
            endif
c
            write(9,'(A,a2,A,i3)')
     &      '     ',label2,' is a variable element in block ',nublks

            if ((kelement .ne. r1_var) .and.
     &         (kelement .ne. r2_var) .and.
     &         (kelement .ne. r3_var)) then
               write(9,'(A,a2,A)')
     &         '      ### ERROR ###  element ',label2,
     &         ' may not be used with this model.'
               numerr = numerr + 1
            end if

            call get_next_line
     &        (currline, currline_lc, field, nlines, eofflg, max_fields)
c
            if (eofflg .eq. noerr) then
               call text_to_real (field(1), ropen,  flag)
               call text_to_real (field(2), rclose, flag2)
               call text_to_real (field(3), timsw,  flag3)
               call text_to_real (field(4), tau,    flag4)

               if (flag+flag2+flag3+flag4 .eq. noerr) then
                  num_var_parms(nvarl) = 4
                  var_model(1,nvarl)   = ropen
                  var_model(2,nvarl)   = rclose
                  var_model(3,nvarl)   = timsw
                  var_model(4,nvarl)   = 1.0 / tau

                  if (keyword .eq. k_decay_model) then
                     write
     &               (9,'(A/A,1pe10.3,A,1pe10.3,A,1pe10.3,A,1pe10.3)')
     &               '      Decay model of a resistive switch:',
     &               '        Ropen=',ropen,'   Rclose=',rclose,
     &               '   Tswitch=',timsw,'   Tau=',tau
                  else
                     write
     &               (9,'(A/A,1pe10.3,A,1pe10.3,A,1pe10.3,A,1pe10.3)')
     &               '      Rise model of a resistive switch:',
     &               '        Ropen=',ropen,'   Rclose=',rclose,
     &               '   Tswitch=',timsw,'   Tau=',tau
                  end if
               else
                  call print_bad_line (currline, nlines, numerr)
               end if
c
            else
               call show_end_of_file (nlines, numerr)
               status=1000
               return
            end if
c
c-----------------------------------------------------------------------
c E-beam diode model of variable resistor.
c
         else if (keyword .eq. k_ediode_model) then
            status           = k_found
            ivar_type(nvarl) = ediode_model
            write(9,'(A,a2,A,i3)')
     &      '     ',label2,' is a variable element in block ',nublks

            if (kelement .ne.  r1_var)  then
               write(9,'(A,a2,A)') '      ### ERROR ###  element ',
     &         label2, ' may not be used with this model.'
               numerr = numerr + 1
            end if

            call get_next_line
     &        (currline, currline_lc, field, nlines, eofflg, max_fields)

            if (eofflg .eq. noerr) then
               call text_to_int  (field(1),   idiode,  flag )
               call text_to_real (field(2),      gap,  flag2)
               call text_to_real (field(3),  enhance,  flag3)
               call text_to_real (field(4), velocity,  flag4)
               call text_to_real (field(5),   router,  flag5)
               call strip (field(6), istart6, iend6)

               if (istart6 .ne. notext) then
                  call text_to_real (field(6), rinner,  flag6)
               else
                  rinner = router
                  flag6    = noerr
               end if

               if (flag+flag2+flag3+flag4+flag5+flag6 .eq. noerr) then
                  num_var_parms(nvarl) = 6
                  var_model(1,nvarl)  = idiode
                  var_model(2,nvarl)  = gap
                  var_model(3,nvarl)  = enhance
                  var_model(4,nvarl)  = velocity
                  var_model(5,nvarl)  = router
                  var_model(6,nvarl)  = rinner

                  var_model(7,nvarl)  = 0.0
                  var_model(8,nvarl)  = 0.0
                  var_model(9,nvarl)  = 0.0
                  write(9,
     &            '(A/A,i2,A,1pe10.3,A,1pe10.3,A,1pe10.3,A,1pe10.3,A,
     &              1pe10.3)')
     &            '  E beam diode model of a resistor:',
     &            '  Diode Type=', idiode,' A/K Gap (m)=', gap,
     &            ' E-enhance=', enhance,' Velocity (m/s)=', velocity,
     &            ' Router=', router, ' Rinner=', rinner
               else
                  print '(A)', ' Ediode: Bad input parameters'
                  call print_bad_line (currline, nlines, numerr)
               end if

            else
               call show_end_of_file (nlines, numerr)
               status=1000
               return
            end if

c
c-----------------------------------------------------------------------
c Plasma opening switch models.
c PS1 is time switching, PS2 is for charge switching.
c
         else if ((keyword .eq. k_ps1_model) .or.
     &            (keyword .eq. k_ps2_model)) then
            status           = k_found

            if (keyword .eq. k_ps1_model) then
               ivar_type(nvarl) = ps1_model
            else if (keyword .eq. k_ps2_model) then
               ivar_type(nvarl) = ps2_model
            end if

            write(9,'(A,a2,A,i3)')
     &      '     ',label2,' is a variable element in block ',nublks

            if ((kelement .ne. r1_var) .and.
     &          (kelement .ne. r3_var)) then
               write(9,'(A,a2,A)')
     &         '      ### ERROR ###  element ',label2,
     &         ' may not be used with this model.'
               numerr = numerr + 1
            end if

            call get_next_line
     &        (currline, currline_lc, field, nlines, eofflg, max_fields)

            if (eofflg .eq. noerr) then
               call text_to_real (field(1), timsw, flag )
               call text_to_real (field(2), const, flag2)
               call text_to_real (field(3), rmax,  flag3)
               call text_to_real (field(4), rmin,  flag4)

               if (flag+flag2+flag3+flag4 .eq. noerr) then

                  if (keyword .eq. k_ps1_model) then
                     num_var_parms(nvarl) = 6
                     var_model(1,nvarl)   = timsw
                     var_model(2,nvarl)   = const
                     var_model(3,nvarl)   = 1.0 / (rmax + 1.0e-20)
                     var_model(4,nvarl)   = 1.0 / (rmin + 1.0e-20)
                     var_model(5,nvarl)   = 0.0
                     var_model(6,nvarl)   = 0.0
                     write(9,
     &               '(A/A,1pe10.3,A,1pe10.3,A,1pe10.3,A,1pe10.3)')
     &               '      PEOS model 1:',
     &               '        Tswitch=',timsw,'   Constant=',const,
     &               '   Rmax=',rmax,'   Rmin=',rmin
                  else if (keyword .eq. k_ps2_model) then
                     num_var_parms(nvarl) = 7
                     qsw                  = timsw
                     var_model(1,nvarl)   = qsw
                     var_model(2,nvarl)   = const
                     var_model(3,nvarl)   = 1.0 / (rmax + 1.0e-20)
                     var_model(4,nvarl)   = 1.0 / (rmin + 1.0e-20)
                     var_model(5,nvarl)   = 0.0
                     var_model(6,nvarl)   = 0.0
                     var_model(7,nvarl)   = 0.0
                     write(9,
     &               '(A/A,1pe10.3,A,1pe10.3,A,1pe10.3,A,1pe10.3)')
     &               '      PEOS model 2:',
     &               '        Qswitch=',qsw,'   Constant=',const,
     &               '   Rmax=',rmax,'   Rmin=',rmin
                  end if

               else
                 call print_bad_line (currline, nlines, numerr)
               end if
c
            else
               call show_end_of_file (nlines, numerr)
               status=1000
               return
            end if
c
c-----------------------------------------------------------------------
c
c Plasma opening switch model.
c   POS is a Z-flow pos model,   January 11, 1995  kws.
c     pos to switch on time or current level.
c
         else if (keyword .eq. k_pos_model) then
            status           = k_found
            ivar_type(nvarl) = pos_model
            write(9,'(A,a2,A,i3)')
     &      '     ',label2,' is a variable element in block ',nublks

            if ((kelement .ne. r1_var) .and.
     &         (kelement .ne. r3_var)) then
               write(9,'(A,a2,A)')
     &         '      ### ERROR ###  element ',label2,
     &         ' may not be used with this model.'
              numerr = numerr + 1
            end if

            call get_next_line
     &        (currline, currline_lc, field, nlines, eofflg, max_fields)

            if (eofflg .eq. noerr) then
               call text_to_real (field(1), tsw,     flag )
               call text_to_real (field(2), cursw,   flag2)
               call text_to_real (field(3), topen,   flag3)
               call text_to_real (field(4), zflow,   flag4)
               call text_to_real (field(5), gswmin,  flag5)
               call text_to_real (field(6), gswmax,  flag6)
               call text_to_real (field(7), CBswitch,flag7)
               call strip (field(8), istart8, iend8)

               if (istart8 .ne. notext) then
                  call text_to_real (field(8), forward, flag8)
               else
                  forward  = 1.0
                  flag8    = noerr
               end if
c
               if (flag+flag2+flag3+flag4+flag5+flag6+flag7+flag8
     &            .eq. noerr) then
                  num_var_parms(nvarl) = 7
                  var_model(1,nvarl)   = tsw
                  var_model(2,nvarl)   = cursw
                  var_model(3,nvarl)   = topen
                  var_model(4,nvarl)   = zflow
                  var_model(5,nvarl)   = gswmin
                  var_model(6,nvarl)   = gswmax
                  var_model(7,nvarl)   = CBswitch
                  var_model(8,nvarl)   = 0.0  ! parameter skip
                  var_model(9,nvarl)   = 0.0  ! parameter tsw
                  var_model(10,nvarl)  = 0.0  ! parameter ctest
                  var_model(11,nvarl)  = 0.0  ! parameter CBflag
                  var_model(12,nvarl)  = 0.0  ! parameter calczflow
                  var_model(13,nvarl)  = 0.0  ! parameter gvar
                  var_model(14,nvarl)  = forward ! flag for fwrd or bkwrd
                  write(9,'(A/A,1pe10.3,A,1pe10.3,A,1pe10.3,A,1pe10.3,A,
     &             1pe10.3,A,1pe10.3,A,1pe10.3,A,1pe10.3)')
     &            '      Z FLOW POS model :','   tsw =',tsw,
     &            ' cursw =',cursw,' topen=',topen,' zflow=',zflow,
     &            ' gswmax=',gswmax,' gswmin=',gswmin,
     &            ' CBflag=',CBflag,' forward = ',forward
               else
                  call print_bad_line (currline, nlines, numerr)
               end if

            else
               call show_end_of_file (nlines, numerr)
               status=1000
               return
            end if
c
c-----------------------------------------------------------------------
c
c   KWS Zflow Plasma Loss Current Model
c
c      Forces a zflow current loss at the insertion point when Child-Langmuir
c      emission turns off (after it has first been turned on).
c
         else if (keyword .eq. k_zflow_model) then
            status           = k_found
            ivar_type(nvarl) = zflow_model
            write(9,'(A,a2,A,i3)')
     &      '     ',label2,' is a variable element in block ',nublks

            if (kelement .ne. r1_var) then
               write(9,'(A,a2,A)')
     &         '      ### ERROR ###  element ',label2,
     &         ' may not be used with this model.'
               numerr = numerr + 1
            end if

            call get_next_line
     &        (currline, currline_lc, field, nlines, eofflg, max_fields)
            if (eofflg .eq. noerr) then
               call text_to_real (field(1), zflow ,     flag )
               call text_to_real (field(2), gap   ,     flag2)
               call text_to_real (field(3), radius,     flag3)
               call text_to_real (field(4), gmin  ,     flag4)
               call text_to_real (field(5), gmax  ,     flag5)
               call text_to_real (field(6), xni   ,     flag6)
               call strip (field(7), istart7, iend7)

               if (istart7 .ne. notext) then
                  call text_to_real (field(7), forward, flag7)
               else
                  forward  = 1.0
                  flag7    = noerr
               end if

               if (flag+flag2+flag3+flag4+flag5+flag6+flag7.eq. noerr)
     &                                                              then
                  num_var_parms(nvarl) = 6
                  var_model(1,nvarl)   = zflow
                  var_model(2,nvarl)   = gap
                  var_model(3,nvarl)   = radius
                  var_model(4,nvarl)   = gmin
                  var_model(5,nvarl)   = gmax
                  var_model(6,nvarl)   = 0.0  ! parameter calczflow
                  var_model(7,nvarl)   = gmin ! parameter gvar
                  var_model(8,nvarl)   = 0.0  ! parameter emissionflag
                  var_model(9,nvarl)   = 0.0  ! parameter tintegral
                  var_model(10,nvarl)  = xni  ! number of parallel lines
                  var_model(11,nvarl)  = forward ! flag for fwd or bkwd
                  write(9,'(A/A,1pe10.3,A,1pe10.3,A,1pe10.3,A,1pe10.3
     &             ,A,1pe10.3,A,1pe10.3,A,1pe10.3)')
     &           '       Z FLOW Plasma Loss Current model  :',
     &           '  zflow =',zflow,' gap =',gap,' radius=',radius,
     &           ' gmin=',gmin,' gmax=',gmax,' xni=',xni,
     &           ' forward = ',forward
               else
                  call print_bad_line (currline, nlines, numerr)
               end if

               if ((gap.le.0.0).or.(radius.le.0.0).or.(xni.le.0.0)) then
                  write(9,'(A,A)')
     &            'gap, radius, or number of parallel lines ',
     &            'less than or equal zero <<<<<<<<<<<<<<<<<<<<<<<<<<<'
                  call print_bad_line (currline, nlines,numerr)
               endif

            else
               call show_end_of_file (nlines, numerr)
               status=1000
               return
            end if

c
c-----------------------------------------------------------------------
c
c Magnetic Flashover Inhibition (MFI) Insulator Crowbar Model.
c     October 14, 1993,  KWS.
c     Causes Insulator to crowbar if MFI criterion exceeded.
c
         else if (keyword .eq. k_mfi_model) then
            status           = k_found
            ivar_type(nvarl) = mfi_model
            write(9,'(A,a2,A,i3)')
     &      '     ',label2,' is a variable element in block ',nublks

            if ((kelement .ne. r1_var) .and.
     &         (kelement .ne. r3_var)) then
               write(9,'(A,a2,A)')
     &         '      ### ERROR ###  element ',label2,
     &         ' may not be used with this model.'
               numerr = numerr + 1
            end if

            call get_next_line
     &        (currline, currline_lc, field, nlines, eofflg, max_fields)

            if (eofflg .eq. noerr) then
               call text_to_real (field(1), radius, flag )
               call text_to_real (field(2), dgap,   flag2)
               call text_to_real (field(3), gmin,   flag3)
               call text_to_real (field(4), gmax,   flag4)
               call text_to_real (field(5), xni,    flag5)
               call text_to_real (field(6), flash,  flag6)

               if (flag+flag2+flag3+flag4+flag5+flag6 .eq. noerr) then
                  num_var_parms(nvarl) = 6
                  var_model(1,nvarl)   = radius
                  var_model(2,nvarl)   = dgap
                  var_model(3,nvarl)   = gmin
                  var_model(4,nvarl)   = gmax
                  var_model(5,nvarl)   = xni
                  var_model(6,nvarl)   = flash
                  var_model(7,nvarl)   = 0.0    ! Initial value of swclosed
                  var_model(8,nvarl)   = 0.0    ! Initial value of tsw
                  var_model(9,nvarl)   = 0.0    ! Initial value of efld
                  var_model(10,nvarl)  = 0.0    ! Initial value of bfld
                  var_model(11,nvarl)  = 0.0    ! Initial value of xmfi
                  write(9,'(A/A,1pe10.3,A,1pe10.3,A,1pe10.3,A,1pe10.3,
     &              A,1pe10.3,A,1pe10.3)')
     &            '       MFI Insulator CB model  :',
     &            '  radius =',radius,' dgap =',dgap,
     &            ' gmin =',gmin,' gmax =',gmax,
     &            ' xni =',xni,' flash =',flash
               else
                  call print_bad_line (currline, nlines, numerr)
               end if

            else
               call show_end_of_file (nlines, numerr)
               status=1000
               return
            end if

c
c-----------------------------------------------------------------------
c
c Resistive Wall Resistance Model.
c     March 7, 1994,  KWS.
c Calculates resistance of the wall for very short, high current pulses.
c
         else if (keyword .eq. k_rwall_model) then
            status           = k_found
            ivar_type(nvarl) = rwall_model
            write(9,'(A,a2,A,i4)')
     &      '     ',label2,' is a variable element in block ',nublks

            if (kelement .ne. r2_var) then
               write(9,'(A,a2,A)')
     &         '      ### ERROR ###  element ',label2,
     &         ' may not be used with this model.'
               numerr = numerr + 1
            end if

            call get_next_line
     &        (currline, currline_lc, field, nlines, eofflg, max_fields)

            if (eofflg .eq. noerr) then
               call text_to_real (field(1), t_in, flag )
               call text_to_real (field(2), gvalue,flag2)
               call text_to_real (field(3), disk1in,flag3)
               call text_to_real (field(4), disk1out,flag4)
               call text_to_real (field(5), disk2in,flag5)
               call text_to_real (field(6), disk2out,flag6)

               call get_next_line
     &        (currline, currline_lc, field, nlines, eofflg, max_fields)

               if (eofflg .eq. noerr) then
                  call text_to_real (field(1), cyl_r_1,flag7)
                  call text_to_real (field(2), cyl_l_1,flag8)
                  call text_to_real (field(3), cyl_r_2,flag9)
                  call text_to_real (field(4), cyl_l_2,flag10)
               else
                  call show_end_of_file (nlines, numerr)
                  status=1000
                  return
               end if

               if (flag+flag2+flag3+flag4+flag5+flag6+flag7+flag8+flag9+
     &            flag10 .eq. noerr)then
                  num_var_parms(nvarl) = 8
                  gvalue=abs(gvalue)

                  if(disk1in.le.0.0)disk1in=1000.
c                   if(disk1out.le.disk1in)disk1out=disk1in+1000.
                  if(disk1out.lt.disk1in)disk1out=disk1in
                  if(disk2in.le.0.0)disk2in=1000.
c                   if(disk2out.le.disk2in)disk2out=disk2in+1000.
                  if(disk2out.lt.disk2in)disk2out=disk2in

                  if (cyl_r_1.le.0.0) then
                     cyl_r_1=1000.0
                     cyl_l_1=0.0
                  endif

                  if (cyl_r_2 .le. 0.0) then
                     cyl_r_2=1000.0
                     cyl_l_1=0.0
                  endif

                  if (cyl_l_1 .lt. 0.0) cyl_l_1=0.0
                  if (cyl_l_2 .lt. 0.0) cyl_l_2=0.0

                  diskfact=log(disk1out/disk1in)+log(disk2out/disk2in)
                  cylfact=cyl_l_1/cyl_r_1+cyl_l_2/cyl_r_2
                  geomfact=diskfact+cylfact
                  var_model(1,nvarl)   = t_in
                  var_model(2,nvarl)   = gvalue
                  var_model(3,nvarl)   = diskfact
                  var_model(4,nvarl)   = cylfact
                  var_model(5,nvarl)   = geomfact
                  write(9,
     &            '(A/A,1pe10.3,A,1pe10.3/A,1pe10.3,A,1pe10.3/A,1pe10.3,
     &              A,1pe10.3/A,1pe10.3,A,1pe10.3/A,1pe10.3,A,1pe10.3)')
     &            '       RWALL resistive wall model  :',
     &            '  t_in =',t_in,'  gvalue =',gvalue,
     &            '  disk1inner =',disk1in,'  disk1outer =',disk1out,
     &            '  disk2inner =',disk2in,'  disk2outer =',disk2out,
     &            '  cyl_radius_1 =',cyl_r_1,'  cyl_length_1 =',cyl_l_1,
     &           '  cyl_radius_2 =',cyl_r_2,'  cyl_length_2 =',cyl_l_2
               else
                  call print_bad_line (currline, nlines, numerr)
               end if

            else
               call show_end_of_file (nlines, numerr)
               status=1000
               return
            end if

c
c-----------------------------------------------------------------------
c
c Version 2 Modified Wall Resistance Model.
c     2012-03-08,  RBS
c     2013-12-07,  RBS arb. disk and cyl
c
c     Calculates resistance of a conducting wall for  short, high
c     current pulses using the NEW Stygar model.
c
c     Read in the conductor parameters (cylindrical or disk)
c     and check the parameters for validity.
c
         else if (keyword .eq. k_r2wall_model) then
            status = k_found
            ivar_type(nvarl) = r2wall_model
            write(9,'(A,a2,A,i3)')
     &      '     ',label2,' is a variable element in block ',nublks

            if (kelement .ne. r2_var) then
               write(9,'(A,a2,A)')
     &         '      ### ERROR ###  element ',label2,
     &         ' may not be used with this model.'
               numerr = numerr + 1
            end if

c      Read in the lines following the resistor command line
            call get_next_line
     &        (currline, currline_lc, field, nlines, eofflg, max_fields)

            if (eofflg .eq. noerr) then
               call text_to_real (field(1), t_in,    flag)
               call text_to_real (field(2), cyl_lin, flag2)
               call text_to_real (field(3), cyl_lo,  flag3)
               call text_to_real (field(4), cyl_rin, flag4)
               call text_to_real (field(5), cyl_ro,  flag5)

               call get_next_line
     &        (currline, currline_lc, field, nlines, eofflg, max_fields)

               if (eofflg .eq. noerr) then
                  call text_to_real (field(1), d_urin,  flag6)
                  call text_to_real (field(2), d_urout, flag7)
                  call text_to_real (field(3), d_lrin,  flag8)
                  call text_to_real (field(4), d_lrout, flag9)
               else
                  call show_end_of_file (nlines, numerr)
                  status=1000
                  return
               end if
c
c Place the inputted parameters for R2Wall into the passed vector
c
               if (flag+flag2+flag3+flag4+flag5+flag6+flag7+flag8+flag9
     &            .eq.noerr) then
                  var_model(1,nvarl)   = t_in
                  var_model(2,nvarl)   = cyl_lin
                  var_model(3,nvarl)   = cyl_lo
                  var_model(4,nvarl)   = cyl_rin
                  var_model(5,nvarl)   = cyl_ro
                  var_model(6,nvarl)   = d_urin
                  var_model(7,nvarl)   = d_urout
                  var_model(8,nvarl)   = d_lrin
                  var_model(9,nvarl)   = d_lrout
                  write(9,
     &            '(A/A,1pe10.3,/
     &              A,1pe10.3,A,1pe10.3,A,1pe10.3,A,1pe10.3,/
     &              A,1pe10.3,A,1pe10.3,A,1pe10.3,A,1pe10.3/)')
     &            '       R2WALL resistive wall model  :',
     &            '  t_in =',t_in,
     &            '  cyl l inner =',cyl_lin,'  cyl l outer r =',cyl_lo,
     &            '  cyl r inner =',cyl_rin,'  cyl r outer =',cyl_ro,
     &            '  disk up r inner =',d_urin,
     &            '  disk up r outer =',d_urout,
     &            '  disk low r inner =',d_lrin,
     &            '  disk low r outer =',d_lrout
               else
                  call print_bad_line (currline, nlines, numerr)
               end if

            else
               call show_end_of_file (nlines, numerr)
               status=1000
               return
            end if
c
c-----------------------------------------------------------------------
c
c Wall Cold Conductor Resistance Model.
c     2019-12-22 - RBS: Adding a cold conductor model for R with variable rho
c
c     Calculates resistance of a conducting wall of length L and cross
c     Sectional area A, Cp, rhom, intial temperature T0, rhoe = A+BT+CT^2
c
c     Read in the conductor parameters and check the parameters for validity.
c
         else if (keyword .eq. k_rcond_model) then
            status = k_found
            ivar_type(nvarl) = rcond_model
            write(9,'(A,a2,A,i3)')
     &      '     ',label2,' is a variable element in block ',nublks

            if ((kelement .ne. r1_var)  .and.
     &          (kelement .ne. r2_var)) then
               write(9,'(A,a2,A)')
     &         '      ### ERROR ###  element ',label2,
     &         ' may not be used with this model.'
               numerr = numerr + 1
            end if

c      Read in the lines following the resistor command line
            call get_next_line
     &        (currline, currline_lc, field, nlines, eofflg, max_fields)

            if (eofflg .eq. noerr) then
               call text_to_real (field(1), R1,     flag)
               call text_to_real (field(2), ER1,   flag2)
               call text_to_real (field(3), ER2,   flag3)

               call get_next_line
     &        (currline, currline_lc, field, nlines, eofflg, max_fields)

               if (eofflg .eq. noerr) then
                  call text_to_real (field(1), alpha1, flag4)
                  call text_to_real (field(2), alpha2, flag5)
                  call text_to_real (field(3), alpha3, flag6)
               else
                  call show_end_of_file (nlines, numerr)
                  status=1000
                  return
               end if

c
c Place the inputted parameters for Rcond into the passed vector
c
               if (flag+flag2+flag3+flag4+flag5+flag6 .eq. noerr ) then
                  var_model(1,nvarl)   = R1
                  var_model(2,nvarl)   = ER1
                  var_model(3,nvarl)   = ER2
                  var_model(4,nvarl)   = alpha1
                  var_model(5,nvarl)   = alpha2
                  var_model(6,nvarl)   = alpha3
                  var_model(7,nvarl)   = 0.0

                  write( 9,
     &            '(A/A,1pe10.3,A,1pe10.3,A,1pe10.3,/
     &              A,1pe10.3,A,1pe10.3,A,1pe10.3/)' )
     &            '      RCOND resistive wall model  :',
     &            '   R1 =', R1,
     &            '   ER1 =', ER1,
     &            '   ER2 =', ER2,
     &            '   alpha1 =', alpha1,
     &            '   alpha2 =', alpha2,
     &            '   alpha3 =', alpha3

               else
                  call print_bad_line (currline, nlines, numerr)
               end if

            else
               call show_end_of_file (nlines, numerr)
               status=1000
               return
            end if
c
c-----------------------------------------------------------------------
c
c Skin Depth Wall Resistance Model.
c     2017-01-02,  RBS
c
c     Calculates resistance of a conducting wall for arbitrary current
c     pulses using the erf solution to the diffusion model/.
c
c     Read in the conductor parameters (cylinders or disk)
c     and check the parameters for validity.
c
         else if (keyword .eq. k_rskin_model) then
            status = k_found
            ivar_type(nvarl) = rskin_model
            write(9,'(A,a2,A,i3)')
     &      '     ',label2,' is a variable element in block ',nublks

            if (kelement .ne. r2_var) then
               write(9,'(A,a2,A)')
     &         '      ### ERROR ###  element ',label2,
     &         ' may not be used with this model.'
               numerr = numerr + 1
            end if

c     Read in the lines following the resistor command line
            call get_next_line
     &        (currline, currline_lc, field, nlines, eofflg, max_fields)

            if (eofflg .eq. noerr) then
               call text_to_real (field(1), sigma,    flag)
               call text_to_real (field(2), depth,    flag2)
               call text_to_real (field(3), cyl_len,  flag3)
               call text_to_real (field(4), cyl_rout, flag4)
               call text_to_real (field(5), cyl_rin,  flag5)

c     Read in the second line of parameters
               call get_next_line
     &        (currline, currline_lc, field, nlines, eofflg, max_fields)

              if (eofflg .eq. noerr) then
                 call text_to_real (field(1), disk_len,  flag6)
                 call text_to_real (field(2), disk_rout, flag7)
              else
                 call show_end_of_file (nlines, numerr)
                 status=1000
                 return
              end if

c
c Place the inputted parameters for RSKin into the passed array
c
              if (flag+flag2+flag3+flag4+flag5+flag6+flag7
     &           .eq. noerr ) then
                 var_model(1,nvarl)   = sigma
                 var_model(2,nvarl)   = depth
                 var_model(3,nvarl)   = cyl_len
                 var_model(4,nvarl)   = cyl_rout
                 var_model(5,nvarl)   = cyl_rin
                 var_model(6,nvarl)   = disk_len
                 var_model(7,nvarl)   = disk_rout

                 write(9,
     &           '(A/
     &             A,1pe10.3,/
     &             A,1pe10.3,A,1pe10.3,/
     &             A,1pe10.3,A,1pe10.3,A,1pe10.3,/
     &             A,1pe10.3,A,1pe10.3/)')
     &           '       RSKin resistive wall model:',
     &           '  conductivity =', sigma, ' max depth =', depth,
     &           '  cyl len =',cyl_len,
     &           '  cyl r out =',cyl_rout,'  cyl r in =',  cyl_rin,
     &           '  disk len =', disk_len,'  disk r outer =',disk_rout
              else
                 call print_bad_line (currline, nlines, numerr)
              end if

            else
               call show_end_of_file (nlines, numerr)
               status=1000
               return
            end if
c
c-----------------------------------------------------------------------
c
c MIP POS model,         February 26, 1993  hnw.
c     zmip ( CTOPS ) pos to switch on errosion and/or magnetic push back.
c
         else if (keyword .eq. k_zmip_model) then
            status           = k_found
            ivar_type(nvarl) = zmip_model
            write(9,'(A,a2,A,i3)')
     &      '     ',label2,' is a variable element in block ',nublks

            if ((kelement .ne. r1_var) .and.
     &         (kelement .ne. r3_var)) then
               write(9,'(A,a2,A)')
     &         '      ### ERROR ###  element ',label2,
     &         ' may not be used with this model.'
               numerr = numerr + 1
            end if

            call get_next_line
     &        (currline, currline_lc, field, nlines, eofflg, max_fields)

            if (eofflg .eq. noerr) then
               call text_to_real (field(1), plength, flag )
               call text_to_real (field(2), gap0,    flag2)
               call text_to_real (field(3), gapmin,  flag3)
               call text_to_real (field(4), radius,  flag4)
               call text_to_real (field(5), gmax,    flag5)
               call text_to_real (field(6), pitch,   flag6)
               call text_to_real (field(7), massnum, flag7)
               call text_to_real (field(8), numden,  flag8)
               call text_to_real (field(9), econst,  flag9)
               call text_to_real (field(10), ibigpo, flag10)
c
               if (flag+flag2+flag3+flag4+flag5+flag6+flag7+flag8+
     &             flag9+flag10 .eq. noerr) then
                  num_var_parms(nvarl) = 10
                  var_model(1,nvarl)   = plength
                  var_model(2,nvarl)   = gap0
                  var_model(3,nvarl)   = gapmin
                  var_model(4,nvarl)   = radius
                  var_model(5,nvarl)   = gmax
                  var_model(6,nvarl)   = pitch
                  var_model(7,nvarl)   = massnum
                  var_model(8,nvarl)   = numden
                  var_model(9,nvarl)   = econst
                  var_model(10,nvarl)  = ibigpo
                  write(9,'(A/A,1pe10.3,A,1pe10.3,A,1pe10.3/
     &              A,1pe10.3,A,1pe10.3,A,1pe10.3/
     &              A,1pe10.3,A,1pe10.3,A,1pe10.3/A,1pe10.3)')
     &            '      MIP POS ( CTOPS ) model:',
     &            '  length  =',plength,' gap0    =',gap0,
     &            ' gapmin =',gapmin,
     &            '  radius  =',radius,' gmax    =',gmax,
     &            ' pitch  =',pitch,
     &            '  massnum =',massnum,' numden =',numden,
     &            ' econst =',econst,'  ibigpo  =',ibigpo
               else
                  call print_bad_line (currline, nlines, numerr)
               end if

            else
               call show_end_of_file (nlines, numerr)
               status=1000
               return
            end if

c
c-----------------------------------------------------------------------
c  Lossy Switch model, set up with a R2 in a Pi section or
c  a RLSeries block.    hnw  January 14, 1993
c
c   Tom Martins lossy switch model, sw_model
c
         else if ( (keyword .eq. k_sw_model )
     &     .or.    (keyword .eq. k_sw1_model)
     &     .or.    (keyword .eq. k_sw2_model)
     &     .or.    (keyword .eq. k_sw3_model)
     &     .or.    (keyword .eq. k_sw4_model) ) then
c
            status           = k_found
            ivar_type(nvarl) = sw_model
c
            write(9,'(A,a2,A,i3)')
     &      '     ',label2,' is a variable element in block ',nublks

            if (kelement .ne. r2_var) then
               write(9,'(A,a2,A)')
     &         '      ### ERROR ###  element ',label2,
     &         ' may not be used with this model.'
              numerr = numerr + 1
            end if

            call get_next_line
     &        (currline, currline_lc, field, nlines, eofflg, max_fields)

            if (eofflg .eq. noerr) then
               call decodematerial (field(1), diel1,   flag )
               call text_to_real (field(2), tbd1,    flag2)
               call text_to_real (field(3),  dm1,    flag3)
               call text_to_real (field(4), atm1,    flag4)
               call text_to_real (field(5), xsw1,    flag5)
               call text_to_real (field(6), xch1,    flag6)
               material = field(1)

               if (flag+flag2+flag3+flag4+flag5+flag6 .eq. noerr) then
                  num_var_parms(nvarl) = 6
                  var_model(1,nvarl)   = diel1
                  var_model(2,nvarl)   = tbd1
                  var_model(3,nvarl)   = dm1
                  var_model(4,nvarl)   = atm1
                  var_model(5,nvarl)   = xsw1
                  var_model(6,nvarl)   = xch1
                  var_model(7,nvarl)   = 0.0   ! Initialize variable irta1
                  write(9,'(A/A,a3,A,1pe10.3,A,
     &              1pe10.3,A,1pe10.3,A,1pe10.3,A,1pe10.3)')
     &            '      SW1 model  :',
     &            '      DIEL1=',material(1:3),'  TBD1=',tbd1,
     &            '  DM1=',dm1,'  ATM1=',atm1,
     &            '  XSW1=',xsw1,'  XCH1=',xch1
               else
                  call print_bad_line (currline, nlines, numerr)
               end if
c
            else
               call show_end_of_file (nlines, numerr)
               status=1000
               return
            end if
c
c-----------------------------------------------------------------------
c Diode model of S.A.Slutz
c
         else if (keyword .eq. k_sdiode_model) then
            status           = k_found
            ivar_type(nvarl)  = sdiode_model
            write(9,'(A,a2,A,i3)')
     &      '     ',label2,' is a variable element in block ',nublks

            if ((kelement .ne. r1_var) .and.
     &         (kelement .ne. r3_var)) then
               write(9,'(A,a2,A)')
     &         '      ### ERROR ###  element ',label2,
     &         ' may not be used with this model.'
               numerr = numerr + 1
            end if

            call get_next_line
     &        (currline, currline_lc, field, nlines, eofflg, max_fields)

            if (eofflg .eq. noerr) then
               call text_to_real (field(1), tdelay,      flag )
               call text_to_real (field(2), rmax,        flag2)
               call text_to_real (field(3), rmin,        flag3)
               call text_to_real (field(4), area,        flag4)
               call text_to_real (field(5), gap,         flag5)
               call text_to_real (field(6), velocity,    flag6)
               call text_to_real (field(7), gapmin,      flag7)
               call text_to_real (field(8), pmass_ratio, flag8)
c          
c        If user did not enter pmass_ratio, set it to 1.0
c
               if (pmass_ratio .eq. 0.0) then
                  pmass_ratio = 1.0
               end if
c
               if (flag+flag2+flag3+flag4+flag5+flag6+flag7 .eq. noerr)
     &                                                              then
                  num_var_parms(nvarl) = 8
                  var_model(1,nvarl)   = tdelay
                  var_model(2,nvarl)   = 1.0 / (rmax + 1.0e-20)
                  var_model(3,nvarl)   = 1.0 / (rmin + 1.0e-20)
                  var_model(4,nvarl)   = area
                  var_model(5,nvarl)   = gap
                  var_model(6,nvarl)   = velocity
                  var_model(7,nvarl)   = gapmin
                  var_model(8,nvarl)   = pmass_ratio
                  var_model(9,nvarl)   = 0.0
                  write(9,'(A/A,1pe10.3,A,1pe10.3,A,1pe10.3,A,1pe10.3/A,
     &              1pe10.3,A,1pe10.3,A,1pe10.3,A,1pe10.3)')
     &            '      SDiode model:','        Tdelay=',tdelay,
     &            ' Rmax=',rmax,'Rmin=',rmin,' Area=',area,
     &            '        Gap=',gap,' Vel.=',velocity,
     &            'Min-Gap=',gapmin,' Pmass_ratio=',pmass_ratio
               else
                  call print_bad_line (currline, nlines, numerr)
               end if
c
            else
               call show_end_of_file (nlines, numerr)
               status=1000
               return
            end if
c
c-----------------------------------------------------------------------
c Classical high-voltage Diode model of W. A. Stygar
c
         else if (keyword .eq. k_diode_model) then
            status           = k_found
            ivar_type(nvarl)  = diode_model
            write(9,'(A,a2,A,i3)')
     &      '     ',label2,' is a variable element in block ',nublks

            if ((kelement .ne. r1_var) .and.
     &          (kelement .ne. r3_var)) then
               write(9,'(A,a2,A)')
     &         '      ### ERROR ###  element ',label2,
     &         ' may not be used with this model.'
               numerr = numerr + 1
            end if

c
c Get the diode parameters from the next line
c

            call get_next_line
     &        (currline, currline_lc, field, nlines, eofflg, max_fields)

            if (eofflg .eq. noerr) then
               call text_to_real (field(1),  V1,   flag )
               call text_to_real (field(2),  I1,   flag2)
               call text_to_real (field(3),  V2,   flag3)
               call text_to_real (field(4),  I2,   flag4)
               call text_to_real (field(5),  V3,   flag5)
               call text_to_real (field(6),  I3,   flag6)
               call text_to_real (field(7),  V4,   flag7)
               call text_to_real (field(8),  I4,   flag8)
               call text_to_real (field(9),  V5,   flag9)
               call text_to_real (field(10), I5,   flag10)
               call text_to_real (field(11), V6,   flag11)
               call text_to_real (field(12), I6,   flag12)

c
               if
     &            ( flag+flag2+flag3+flag4+flag5+flag6+flag7+flag8+
     &             flag9+flag10+flag11+flag12 .eq. noerr )
     &         then

                  num_var_parms(nvarl) = 12
c
c pass diode curve, voltages and currents, to the storage array var_model
c
                  var_model(1,nvarl)   = V1
                  var_model(2,nvarl)   = I1
                  var_model(3,nvarl)   = V2
                  var_model(4,nvarl)   = I2
                  var_model(5,nvarl)   = V3
                  var_model(6,nvarl)   = I3
                  var_model(7,nvarl)   = V4
                  var_model(8,nvarl)   = I4
                  var_model(9,nvarl)   = V5
                  var_model(10,nvarl)  = I5
                  var_model(11,nvarl)  = V6
                  var_model(12,nvarl)  = I6

c Confirm diode curve is in the passed array

c      print *,var_model(1,nvarl),var_model(2,nvarl),var_model(3,nvarl),
c     &      var_model(4,nvarl),var_model(5,nvarl),var_model(6,nvarl)
c      print *,var_model(7,nvarl),var_model(8,nvarl),var_model(9,nvarl),
c     &      var_model(10,nvarl),var_model(11,nvarl),var_model(12,nvarl)

c
c Get the capacitor parameters from the next line
c
c
c       call get_next_line
c     &        (currline, currline_lc, field, nlines, eofflg, max_fields)
c
c       if (eofflg .eq. noerr) then
c          call text_to_real (field(1),  Vmin,   flag )
c          call text_to_real (field(2),  Immin,  flag2)
c          call text_to_real (field(3),  Vbreak, flag3)
c          call text_to_real (field(4),  Ibreak, flag4)

c write out log file contents of diode model inputs

                  write(9,'(A/A,1pe10.3,A,1pe10.3,
     &                       /A,1pe10.3,A,1pe10.3,
     &                       /A,1pe10.3,A,1pe10.3,
     &                       /A,1pe10.3,A,1pe10.3,
     &                       /A,1pe10.3,A,1pe10.3,
     &                       /A,1pe10.3,A,1pe10.3)')
     &            '      Diode model:',
     &          '        V1= ' ,V1, ' I1= ' ,I1,
     &          '        V2= ' ,V2, ' I2= ' ,I2,
     &          '        V3= ' ,V3, ' I3= ' ,I3,
     &          '        V4= ' ,V4, ' I4= ' ,I4,
     &          '        V5= ' ,V5, ' I5= ' ,I5,
     &          '        V6= ' ,V6, ' I6= ' ,I6

               else
                  call print_bad_line (currline, nlines, numerr)
               end if
c
            else
               call show_end_of_file (nlines, numerr)
               status=1000
               return
            end if

c
c-----------------------------------------------------------------------
c Applied-B Diode model of M.P.DesJarlais
c
         else if (keyword .eq. k_abdiode_model) then
            status           = k_found
            ivar_type(nvarl) = abdiode_model
            write(9,'(A,a2,A,i3)')
     &      '     ',label2,' is a variable element in block ',nublks

            if ((kelement .ne. r1_var) .and.
     &         (kelement .ne. r3_var)) then
               write(9,'(A,a2,A)')
     &         '      ### ERROR ###  element ',label2,
     &         ' may not be used with this model.'
               numerr = numerr + 1
            end if
            call get_next_line
     &        (currline, currline_lc, field, nlines, eofflg, max_fields)

            if (eofflg .eq. noerr) then
               call text_to_real (field(1), gap,    flag )
               call text_to_real (field(2), area,   flag2)
               call text_to_real (field(3), charge, flag3)
               call text_to_real (field(4), amu,    flag4)
               call text_to_real (field(5), b0,     flag5)
               call text_to_real (field(6), x0,     flag6)
               call text_to_real (field(7), rmin,   flag7)
               call text_to_real (field(8), rmax,   flag8)
               call text_to_real (field(9), frac,   flag9)
               call strip (field(10), istart10, iend10)

               if (istart10 .ne. notext) then
                  call text_to_real (field(10), ratemax, flag10)
               else
                  ratemax = abdratemax
                 flag10 = noerr
               end if

               if (flag +flag2+flag3+flag4+flag5
     &                  +flag6+flag7+flag8+flag9+flag10 .eq. noerr) then
                  num_var_parms(nvarl) = 10
                  var_model(1,nvarl)   = gap
                  var_model(2,nvarl)   = area
                  var_model(3,nvarl)   = charge
                  var_model(4,nvarl)   = amu
                  var_model(5,nvarl)   = b0
                  var_model(6,nvarl)   = x0
                  var_model(7,nvarl)   = 1.0 / (rmin + 1.0e-20) !gmax
                  var_model(8,nvarl)   = 1.0 / (rmax + 1.0e-20) !gmin
                  var_model(9,nvarl)   = frac
                  var_model(10,nvarl)  = ratemax
                  write(9,
     &            '(A/A,1pe10.3,A,1pe10.3,A,1pe10.3,A,1pe10.3,A,1pe10.3,
     &              A,1pe10.3/A,1pe10.3,A,1pe10.3,A,1pe10.3,A,1pe10.3)')
     &            '      Applied-B Diode model:',
     &            '        Gap=',gap,'  Area=',area,
     &            '  Ion Charge=',charge,'  Ion AMU=',amu,
     &            '  B0=',b0,'  X0=',x0,'        Rmin=',rmin,
     &            '  Rmax=',rmax,'  IonFraction=',frac,
     &            '  Ratemax=',ratemax
c
c Now call a subroutine which will set the many parameters needed to
c do this diode model.
c
                  call abdiodeparm
     &                        (var_model(1,nvarl), num_var_parms(nvarl))
               else
                  call print_bad_line (currline, nlines, numerr)
               end if
c
            else
               call show_end_of_file (nlines, numerr)
               status=1000
               return
            end if

c
c-----------------------------------------------------------------------
c Saturable core inductor as a magnetic switch model.
c
         else if (keyword .eq. k_magsw_model) then
            status           = k_found
            ivar_type(nvarl) = magsw_model
            write(9,'(A,a2,A,i3)')
     &      '     ',label2,' is a variable element in block ',nublks

            if (kelement .ne. l2_var) then
               write(9,'(A,a2,A)')
     &         '      ### ERROR ###  element ',label2,
     &         ' may not be used with this model.'
               numerr = numerr + 1
            end if

            call get_next_line
     &        (currline, currline_lc, field, nlines, eofflg, max_fields)

            if (eofflg .eq. noerr) then
               call text_to_real (field(1), pf,   flag)
               call text_to_real (field(2), ri,   flag2)
               call text_to_real (field(3), ro,   flag3)
               call text_to_real (field(4), w,    flag4)
               call text_to_real (field(5), h1,   flag5)
               call text_to_real (field(6), hsat, flag6)
               call text_to_real (field(7), hrev, flag7)
               call text_to_real (field(8), bsat, flag8)
               isum = flag+flag2+flag3+flag4+flag5+flag6+flag7+flag8

               if (isum .eq. noerr) then
                  num_var_parms(nvarl) = 9
                  var_model(1,nvarl)   = pf
                  var_model(2,nvarl)   = ri
                  var_model(3,nvarl)   = ro
                  var_model(4,nvarl)   = w
                  var_model(5,nvarl)   = h1
                  var_model(6,nvarl)   = hsat
                  var_model(7,nvarl)   = hrev
                  var_model(8,nvarl)   = bsat
c
c set 9 to the initial value of the inductance
c
                  iiin                 = iin(2,nblks,nbrns)
                  var_model(9,nvarl)   = pin(iiin,nblks,nbrns)
                  write(9,'(A/A,1pe10.3,A,1pe10.3,A,1pe10.3,A,
     &              1pe10.3/A,1pe10.3,A,1pe10.3,A,1pe10.3,A,1pe10.3)')
     &            '      Saturable core inductor switch model:',
     &            '        PkFrac=',pf,'   Rinner=',ri,
     &            '   Router=',ro,'   Width=',w,
     &            '        H1=',h1,'   Hsat=',hsat,
     &            '   Hrev=',hrev,'   Bsat=',bsat
c
c Now call a subroutine which will set the many parameters needed to
c do this switch model.
c
                  call magparm (var_model(1,nvarl),num_var_parms(nvarl))
               else
                  call print_bad_line (currline, nlines, numerr)
               end if

            else
               call show_end_of_file (nlines, numerr)
               status=1000
               return
            end if
c
c-----------------------------------------------------------------------
c Least squares fit to some function.
c
         else if (keyword .eq. k_lsf_model) then
            status           = k_found
            ivar_type(nvarl)   = lsf_model
            write(9,'(A,a2,A,i3)')
     &      '     ',label2,' is a variable element in block ',nublks
            nsaverr            = numerr
            call read_lsf_parms (var_model(1,nvarl),
     &                           num_var_parms(nvarl), eofflg,
     &                           nlines, numerr)

            if (eofflg .eq. noerr) then

               if (numerr .eq. nsaverr) then
                  write(9,47) (j-1,var_model(j,nvarl),
     &                  j=1,num_var_parms(nvarl))
               end if

            else
               call show_end_of_file (nlines, numerr)
               status=1000
               return
            end if
c
c-----------------------------------------------------------------------
c Piece-wise linear model.
c
         else if (keyword .eq. k_pwl_model) then
            status            = k_found
            ivar_type (nvarl) = pwl_model
            nsaverr           = numerr
            write(9,'(A,a2,A,i3)')
     &      '     ',label2,' is a variable element in block ',nublks
            call read_pwl_parms (var_model(1,nvarl),
     &                           num_var_parms(nvarl), eofflg,
     &                           nlines, numerr)

            if (eofflg .eq. noerr) then

               if (numerr .eq. nsaverr) then
                  write(9,48) 'Indep.Var.', 'Depen.Var.',
     &                  ((j+1)/2, var_model(j,nvarl),
     &                  var_model(j+1,nvarl),
     &                  j=1,num_voltf_parms(nvoltsource)-1,2)
               end if

            else
               call show_end_of_file (nlines, numerr)
               status=1000
               return
            end if

         else
            call print_bad_line (currline, nlines, numerr)
         end if

c
c-----------------------------------------------------------------------------
c End of variable elment section, end of IF for field 3
c Return to first IF looking at field 1
c-----------------------------------------------------------------------------
c Switched Variable element in the last block type.
c
      else if (keyword .eq. k_svariable) then
         status            = k_found
         last_block        = iin(1,nblks,nbrns)
         nvarl             = nvarl + 1
         ivar_block(nvarl) = last_block
         ivar_block_num(nvarl) = nublks

c-----------------------------------------------------------------------------
c New IF looking at field 2
c-----------------------------------------------------------------------------
c Second keyword to specify which one is the variable element.
c Check against block type.
c
         keyword = field(2)(1:keyword_len)

         if (last_block .eq. rcground) then

            if (keyword .eq. k_r1_var) then
               iin(2,nblks,nbrns) = r1_var
            else if (keyword .eq. k_c1_var) then
               iin(2,nblks,nbrns) = c1_var
            else
               call print_bad_line (currline, nlines, numerr)
               status=305
               return
            end if

         else if (last_block .eq. rlseries) then
            if (keyword .eq. k_r2_var) then
               iin(2,nblks,nbrns) = r2_var
            else if (keyword .eq. k_l2_var) then
               iin(2,nblks,nbrns) = l2_var
            else
               call print_bad_line (currline, nlines, numerr)
               status=305
               return
            end if

         else
            call print_bad_line (currline, nlines, numerr)
            status=305
            return
         end if
c
c-----------------------------------------------------------------------------
c End IF for field 2
c

         kelement = iin(2,nblks,nbrns)
         label2   = keyword(1:2)
c
c Third keyword for model type (lsf and pwl models not active yet).
c
         keyword  = field(3)(1:keyword_len)

c-----------------------------------------------------------------------------
c New IF looking at field 3
c-----------------------------------------------------------------------
c Exponential model of variable resistor.
c
         if (keyword .eq. k_exp_model) then
            status           = k_found
            ivar_type(nvarl) = exp_model
            write(9,'(A,a2,A,i3)')
     &      '     ',label2,' is a variable element in block ',nublks

            if ((kelement .ne.  r1_var)  .and.
     &         (kelement .ne. r2_var)  .and.
     &         (kelement .ne. r3_var)) then
               write(9,'(A,a2,A)')
     &         '      ### ERROR ###  element ',label2,
     &         ' may not be used with this model.'
               numerr = numerr + 1
            end if

            call get_next_line
     &        (currline, currline_lc, field, nlines, eofflg, max_fields)

            if (eofflg .eq. noerr) then
               call text_to_real (field(1), ropen,   flag)
               call text_to_real (field(2), rclose,  flag2)
               call text_to_int  (field(3), iswitch, flag3)
               call text_to_real (field(4), tau,     flag4)
               call text_to_real (field(5), zsw,     flag5)

               if (flag+flag2+flag3+flag4+flag5 .eq. noerr) then
                  num_var_parms(nvarl) = 5
                  var_model(1,nvarl)   = ropen
                  var_model(2,nvarl)   = rclose
                  var_model(3,nvarl)   = switch_time(iswitch)
                  var_model(4,nvarl)   = 1.0 / tau
                  var_model(5,nvarl)   = zsw
                  write(9,
     &          '(A/A,1pe10.3,A,1pe10.3,A,1pe10.3,A,1pe10.3,A,1pe10.3)')
     &            '      Exponential model of a resistive switch:',
     &            '        Ropen=',ropen,'   Rclose=',rclose,
     &            '   Tswitch=',switch_time(iswitch),'   Tau=',tau,
     &            '   Zswitch=',zsw
               else
                  call print_bad_line (currline, nlines, numerr)
               end if

            else
               call show_end_of_file (nlines, numerr)
               status=1000
               return
            end if

c
c-----------------------------------------------------------------------
c Decay or Rise model of variable resistor.
c
         else if ((keyword .eq. k_decay_model) .or.
     &                             (keyword .eq. k_rise_model)) then
            status           = k_found

            if (keyword .eq. k_decay_model) then
               ivar_type(nvarl) = decay_model
            else
               ivar_type(nvarl) = rise_model
            endif
c
            write(9,'(A,a2,A,i3)')
     &      '     ',label2,' is a svariable element in block ',nublks

            if ((kelement .ne. r1_var)  .and.
     &          (kelement .ne. r2_var)  .and.
     &          (kelement .ne. r3_var)) then
               write(9,'(A,a2,A)')
     &         '      ### ERROR ###  element ',label2,
     &         ' may not be used with this model.'
               numerr = numerr + 1
            end if

            call get_next_line
     &        (currline, currline_lc, field, nlines, eofflg, max_fields)
c
            if (eofflg .eq. noerr) then
               call text_to_real (field(1), ropen,   flag)
               call text_to_real (field(2), rclose,  flag2)
               call text_to_int  (field(3), iswitch, flag3)
               call text_to_real (field(4), tau,     flag4)

               if (flag+flag2+flag3+flag4 .eq. noerr) then
                  num_var_parms(nvarl) = 4
                  var_model(1,nvarl)   = ropen
                  var_model(2,nvarl)   = rclose
                  var_model(3,nvarl)   = switch_time(iswitch)
                  var_model(4,nvarl)   = 1.0 / tau

                  if (keyword .eq. k_decay_model) then
                     write
     &               (9,'(A/A,1pe10.3,A,1pe10.3,A,1pe10.3,A,1pe10.3)')
     &               '      Decay model of a resistive switch:',
     &               '        Ropen=',ropen,'   Rclose=',rclose,
     &               '   Tswitch=',switch_time(iswitch),'   Tau=',tau
                  else
                     write
     &               (9,'(A/A,1pe10.3,A,1pe10.3,A,1pe10.3,A,1pe10.3)')
     &               '      Rise model of a resistive switch:',
     &               '        Ropen=',ropen,'   Rclose=',rclose,
     &               '   Tswitch=',switch_time(iswitch),'   Tau=',tau
                  end if

               else
                  call print_bad_line (currline, nlines, numerr)
               end if

            else
               call show_end_of_file (nlines, numerr)
               status=1000
               return
            end if

         else
            call print_bad_line (currline, nlines, numerr)
         end if

c-----------------------------------------------------------------------------
c END of IF for field 3 switched
c-----------------------------------------------------------------------------
c Set an initial voltage or current.
c
      else if (keyword .eq. k_initial) then
c
c Set the keyword for voltage or current and what element.
c
         status           = k_found
         ninit_cond       = ninit_cond + 1
         keyword          = field(2)(1:keyword_len)

         if (keyword .eq. k_vcapacitor1) then
            iin(4,nblks,nbrns) = vcapacitor1
            label18            = 'Voltage on C1 = '
         else if (keyword .eq. k_vcapacitor3) then
            iin(4,nblks,nbrns) = vcapacitor3
            label18            = 'Voltage on C3 = '
         else if (keyword .eq. k_vtrline) then
            iin(4,nblks,nbrns) = vtrline
            label18            = 'Voltage on line = '
         else if (keyword .eq. k_cinductor) then
            iin(4,nblks,nbrns) = cinductor
            label18            = 'Current in L2 = '
         else if (keyword .eq. k_ctrline) then
            iin(4,nblks,nbrns) = ctrline
            label18            = 'Current in line = '
         else
            call print_bad_line (currline, nlines, numerr)
            status=305
            return
         end if
c
c Get the initial value and store it.
c
         call text_to_real (field(3), value, flag)

         if (flag .eq. noerr) then
            value_init(ninit_cond) = value
            write(9,'(A,a18,1pe10.3)')
     &      '  Initial condition: ',label18,value
         else
            call print_bad_line (currline, nlines, numerr)
         end if

c
c-----------------------------------------------------------------------
c Top branch exitting this block.
c
      else if (keyword .eq. k_topbranch) then
         status             = k_found
         iin(3,nblks,nbrns) = topbranch
         nsecbrn            = nsecbrn + 1
         write(9,'(A,i3,A,A,i2,A)')
     &   ' ***** Block ',nublks,' has a top branch ',
     &   'exiting it (branch ',nsecbrn,') *****'
c
c-----------------------------------------------------------------------
c End branch exitting this block.
c
      else if (keyword .eq. k_endbranch) then
         status             = k_found
         iin(3,nblks,nbrns) = endbranch
         nsecbrn            = nsecbrn + 1
         write(9,'(A,i3,A,A,i2,A)')
     &   ' ***** Block ',nublks,' has an end branch ',
     &   'exiting it (branch ',nsecbrn,') *****'
c
c-----------------------------------------------------------------------
      end if
      
      return
      end   

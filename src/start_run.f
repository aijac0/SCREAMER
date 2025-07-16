      subroutine init_problem(status)
c
c 7jul2008 dlr added logic and calls for time w/ gfortran
c 2014-12-11 RBS: Corrected the max_node limit to the total # nodes
c                 not the nodes per branch, more efficent matrix limit
c
c-----Description-------------------------------------------------------
c
c Author/Date: Mathias Bavay  11/04
c      Modifications:
c 2008-09-10 RBS Redefine buffer to 80
c 2014-11-16 RBS: Modify the branch node definitions in lines 312-223
c                 To allow branches in branches
c 2014-12-12 RBS: Changed the max nodes test to test the total number of
c                 nodes in ALL of the branches.
c 2015-01-08 RBS: Added comments describing new blocks
c 2015-01-09 RBS: Added lossy line setup
c 2015-06-23 RBS: Declared time_flag, half_step, whole_step internal to
c                 function to eliminate compiler warnings
c 2015-06-23 RBS: Definition of nsteps now explicitly integer
c 2015-06-23 RBS: nvar_node initialized
c 2015-09-25 RBS: Buffer*80 size exceeded in node max check write. That
c                 write replaced with a direct screen write.
c 2018-07-20 RBS: Initialized itabnum
c 2019-11-29 RBS: Added output of node total in the node check section
c
c ----------------------------------------------------------------------
c
c Purpose: This subroutines initialize variables for a run. 
c          It does it by reading the input deck
c
c Called by: Program ZDEM
c
c Calls:  Subroutines setup_trline,setup_rcground,setup_rlseries,
c setup_pisection,setup_rlseries,setup_rlseries,setup_rlseries,
c setup_voltsource,setup_vendsource,setup_currsource,
c setup_cendsource,setup_cendsource,setup_trline,setup_trline,
c setup_adder,setup_mzflow,echo_indicies,banner,cycle_print,
c open_outfile,get_variable
c
c Define passed variables
c
      integer    status
c
c Include files
c
      include 'zdemparm.h'
      include 'zdempprm.h'
      include 'zdemmax.h'
      include 'zdemcomm.h'
      include 'zdemwork.h'
      include 'zdemout.h'
      include 'zdemvars.h'
      include 'zdemloop.h'
c
c Internal Variables
c
      integer   newfile, oldfile, fflag
      parameter (newfile=1, oldfile=2)
c
c ***** Time flag parameters ******
c
      integer time_flag, half_step,     whole_step
      parameter         (half_step = 1, whole_step = 2)
c
c if everything is fine, we will return 0 in status
      character  initln*80
c
c----Variable definition for C++ stdout-----------------------------
c      character buffer*80
c-------------------------------------------------------------------
c
      initln  = 'Initial SCREAMER Circuit Status'
      cyclln  = 'SCREAMER Circuit Status for t > 0'
      status=0
c
c -------------------------------------------------------------------
c     set-up information
c     nb = number of branches
c     ib = branch number
c     nr = counts up the total # of nodes as branches are added
c     nbk(ib) = number of blocks in branch ib
c     ibk = block number
c     ibt = block type
c       0 = transmission line - tau, zin, zout, 0, 0, 0
c       1 = output request - no nodes or elements (quasi-block)
c       2 = resistor and capacitor to ground - r1, c1, 0, 0, 0, 0
c       3 = pi section - r1, c1, r2, l2, r3, c3
c       5 = voltage source - r, l, 0, 0, 0, 0
c       6 = MITL line - circumf, gap, tau, zin, zout
c       7 = adder block - 0, 0, 0, 0, 0, 0
c       8 = end-of-branch voltage source - r, l, 0, 0, 0, 0
c       9 = PMITL line - perveance, tau, zline, 0, 0, 0
c      10 = RL series - r, l, 0, 0, 0, 0
c      11 = Current source - r, c, 0, 0, 0, 0
c      12 = end-of-branch current source - r, c, 0, 0, 0, 0
c      13 = end-of-branch SCL current source - r, c, 0, 0, 0, 0
c      14 = transformer block - (unused)
c
c      31 = cylfoil implosion model - r, l, 0, 0, 0, 0
c      32 = gas puff implosion model - r, l, 0, 0, 0, 0
c      33 = sphfoil implosion model - r, l, 0, 0, 0, 0
c      34 = measure zflow block
c      35 = nshell implosion block
c      36 = dynamic hohlraum block
c      37 = lossy transmission line block
c 
c 
c     ivn = element number for variable element
c       0=none,1=r1,2=c1,3=r2,4=c2,5=r3,6=c3
c     ibr = branch type from block
c       0=no branch,1=top branch,2=end branch
c
c     the blocks will be converted into the following general circuit 
c
c              ^   |                                    ^   |
c              |   |                                    |   |
c            zib(i)|                                zib(i+1)|
c                  |               zir(i) ->                |
c                  | v,vn(i)                                | v,vn(i+1)
c     -------------o---------- rr(i) ----- zlr(i) ----------o------------
c               |     |                                  |     |
c               |     |                                  |     |
c             g(i)   c(i)                             g(i+1) c(i+1)
c               |     |                                  |     |
c               |     |                                  |     |
c     --------------------------------------------------------------------
c                node:i                                  node:i+1
c
c
c ----------------------------------------------------------------------
c
c
      data icycle, esour / 0, 0.0 /
      data ecap, eind, econ, eres / 0.0, 0.0, 0.0, 0.0 /
      data elossind, elosscap / 0.0, 0.0 /
      ecapsource = 0.0
      eindsource = 0.0
      nvar_node = 0
c
c
c
c------------------------------------------------------------------------
c
c ----- problem set-up ----------------------------------------------------
c
c Set the number of steps according to iset.
c
      nbm      = nb - 1
      if (iset .eq. one_cycle) then
        tmax   = ht
        nsteps = 1
      else
        nsteps = int(tmax / ht)
      end if
c
c Set the inverse of the time step, half a time step, and the number of cycles
c between printouts.
c
      rht    = 1.0 / ht
      htd2   = ht / 2.0
      ncycle = nsteps / nprint
c
c
c Determine the size of the screamer temporary file and ask the user
c if he wants to continue 
c
c
      itfilesz = 12 + 10 * numout + 8 * (1 + numout) * nsteps
      xtfilesz = FLOAT(itfilesz) / 1.0e6
c
c  Write message to log file that SCREAMER is now running
c
c      write (buffer,947)
c      call writebuffer(buffer)
c  947 format (' SCREAMER is now running'/)
c
c
c Set-up from block elements
c
c Set some counters to zero: Variable element counter, secondary branch
c counter, lossless trans. line counter, MITL counter,
c voltage source counter, current source counter,
c initial condition (V or I) counter,
c output request counter.
c
      icv           = 0
      icb           = 0
      itrl_counter  = 0
      imitl_counter = 0
      ivs_counter   = 0
      ics_counter   = 0
      ic_counter    = 0
      itab_counter  = 0
      itabnum       = 0
      icout         = 0
c
c Loop over the branches and set the number of blocks in that branch.
c
      do 200 ib = 1, nb
        nbkx    = nbk(ib)
c
c Specify first block for ib > 1 to be a short (zero series resistance
c and inductance). This connects the new branch to the main via this
c short (as all blocks are connected).  This adds one node (nr=1) to this
c branch.  Else set the node counter to zero for the first branch.
c
        if (ib .gt. 1) then
          nr(ib)        = 1
          g(1,ib)       = 0.0
          rr(1,ib)      = 0.0
          zlr(1,ib)     = 0.0
          c(1,ib)       = 0.0
          cechk(1,ib)   = 0.0
          zlrechk(1,ib) = 0.0
        else
          nr(ib)        = 0
        end if
c
c Set an index which only counts the circuit blocks and not the output requests
c
        ncirblk         = 0
c
c Loop over the blocks. (Each output request is considered as a block)
c
        do 201 ibk = 1,nbkx
c
c Set a flag for output to id each node.
c ibt is the block type, ivn is the variable element number, ibr is the branch
c exit type, iinit is the initial condition type, ibk is the block number,
c ioutreq is the type of output request.
c
          ibt       = iin(1,ibk,ib)
          if (ibt .ne. outputreq) then
            ncirblk = ncirblk + 1
          end if
          ivn       = iin(2,ibk,ib)
          ibr       = iin(3,ibk,ib)
          iinit     = iin(4,ibk,ib)
          ioutreq   = iin(5,ibk,ib)
          iflgs     = 10000000*ib + ncirblk*100000 + 1000*ibt
     &              + 100*iinit + 10*ivn + ibr
c
c Reset the counter if we have an initial voltage or current in this block.
c
          if (iinit .gt. 0) then
            ic_counter = ic_counter + 1
          end if
c
c------------------------------------------------------------------------
c Now sort out the block types.
c
          if (ibt .eq. transline) then
            itrl_counter = itrl_counter + 1
            call setup_trline (ib, ibk, itrl_counter, na, iflgs,
     &                         transline, iinit, ic_counter)
c
          else if (ibt .eq. lossyline) then
            itrl_counter = itrl_counter + 1
            call setup_trline (ib, ibk, itrl_counter, na, iflgs,
     &                         lossyline, iinit, ic_counter)
c
c If an output request, set the node index.
c
          else if (ibt .eq. outputreq) then
            icout              = icout + 1
            ixnodout(icout)    = ifnodreq(itypout(icout), nr(ib), na)
            ixlstnodout(icout) = nr(ib)
c
          else if (ibt .eq. rcground) then
            call setup_rcground (ib,ibk,na,iflgs,iinit,ic_counter)
c
          else if (ibt .eq. rlseries) then
            call setup_rlseries (ib,ibk,na,iflgs,iinit,ic_counter)
c
          else if (ibt .eq. pisection) then
            call setup_pisection (ib,ibk,na,iflgs,iinit,ic_counter)
c
          else if (ibt .eq. cylfoilblock) then
            call setup_rlseries (ib,ibk,na,iflgs,iinit,ic_counter)
c
          else if (ibt .eq. sphfoilblock) then
            call setup_rlseries (ib,ibk,na,iflgs,iinit,ic_counter)
c
          else if (ibt .eq. gaspuffblock) then
            call setup_rlseries (ib,ibk,na,iflgs,iinit,ic_counter)
c
          else if (ibt .eq. voltsource) then
            ivs_counter = ivs_counter + 1
            call setup_voltsource (ib, ibk, ivs_counter, na, iflgs,
     &                             iinit, ic_counter)
c
          else if (ibt .eq. vendsource) then
            ivs_counter = ivs_counter + 1
            call setup_vendsource (ib, ibk, ivs_counter, na, iflgs,
     &                             iinit, ic_counter)
c
          else if (ibt .eq. currsource) then
            ics_counter = ics_counter + 1
            call setup_currsource (ib, ibk, ics_counter, na, iflgs,
     &                             iinit, ic_counter)
c
          else if (ibt .eq. cendsource) then
            ics_counter = ics_counter + 1
            itypcs      = itypcend(ics_counter)
            call setup_cendsource (ib, ibk, ics_counter, na, iflgs,
     &                             iinit, ic_counter, itypcs)
c
          else if (ibt .eq. csclsource) then
            ics_counter = ics_counter + 1
            itypcs      = itypcend(ics_counter)
            call setup_cendsource (ib, ibk, ics_counter, na, iflgs,
     &                             iinit, ic_counter, itypcs)
c
          else if (ibt .eq. mitline) then
            imitl_counter = imitl_counter + 1
            call setup_trline (ib, ibk, imitl_counter, na, iflgs,
     &                         mitline, iinit, ic_counter)
c
          else if (ibt .eq. pmitline) then
            imitl_counter = imitl_counter + 1
            call setup_trline (ib, ibk, imitl_counter, na, iflgs,
     &                        pmitline, iinit, ic_counter)
c
          else if (ibt .eq. adder) then
            call setup_adder (ib, ibk, na, iflgs)
c
          else if (ibt .eq. measurezflow) then
            call setup_mzflow (ib, ibk, na, iflgs)
c
          end if
c
c------------------------------------------------------------------------
c Set the node for a variable element and the current number of nodes in
c this branch.
c
          if (ibt .ne. outputreq) then
            nvar_node = nr(ib) + 1
            nrx       = nr(ib) + na
            nr(ib)    = nrx
          end if
c      write(6,'(A,I2,A,I4)') 'START_RUN: Branch ',ib,' node count:',nrx
c
c If we have a branch exitting this block, increment the exit
c branch counter, then set the circuit element index and the branch
c exit type for this exit branch.
c
          if (ibr .gt. 0) then
            icb           = icb + 1
            nodessofar = 0
            do i=1,ib-1
              nodessofar = nodessofar + nr(i)
              enddo
              nnrx = nodessofar + nrx
c            write(6,'(A,I2,A,I2)') 'Node count:',nnrx,
c     &                        ' at branch',ib
            indexb(1,icb) = nnrx
            indexb(2,icb) = ibr
          end if
c
c If we have a variable element in this block, increment the variable
c element counter, then set the circuit element index, the number of
c the variable element in this block, and the branch index for this
c variable element.
c
          if (ivn .gt. 0) then
            icv           = icv + 1
            indexv(1,icv) = nvar_node
            indexv(2,icv) = ivn
            indexv(3,icv) = ib
          end if
  201   continue
  200 continue
c
c Check number of nodes to see if limit exceeded
c Count nodes in branches
c
        nbr = 0
        do ib=1,nb
         nbr = nbr + nr(ib)
         if (nbr .gt. max_nodes) then
           write(6,203) ib, nbr, max_nodes
c           call writebuffer(buffer)
           write(9,203) ib, nbr, max_nodes
           status=1
           goto 802
           endif
         enddo
        write(6,'(A,I5/)') '   Total nodes in simulation = ', nbr
  203 format(' Maximum number of nodes exceeded.  Increase',
     &' TL resolution time.'/' Or, recompile with larger',
     &' MAX_NODES parameter in the file zdemmax.h.'/
     &' Limit exceeded in Branch = ',i3,'  Number of nodes = ',i4,
     $' .  Maximum is ',i4/
     &' Run stopped.')
c
c
c
c List the various indicies we have just set.
c
      if (echoset .eq. yes_echo) then
        call echo_indicies
      end if
c
c -----------------------------------------------------------------------------
c
c Set the initial energy in the circuit and
c print out the initial status of the circuit.
c
      esour = ecapsource + eindsource
      i2    = 0
      tim   = 0.0
      error = 0.0
      call banner (initln)
      call cycle_print (i2, tim, esour, eindsource, ecapsource, econ,
     &                  eres, elossind, elosscap, error)
c
c Initialize everything needed for plots, print, files, tables, ufo, idr.
c Determine the first points.
c Set t=0 points if requested.
c
      fflag = newfile
      iunit   = outunit
      call open_outfile (iunit, fflag, ierr)
c
c outunit is set in zdemout.h
c
      write(outunit) numout
      write(outunit) (tagout(jj),jj=1,numout)
c
c Variables needed to calculate block number used in "pin" array
c to pass to get_variable:
c ibrnsav is the number of the branch associated with the last output request
c iblksav is the number of the block associated with the last output request
c ipinsav is the number of output requests in this branch
c ipin is the block number used in "pin"
c
      ibrnsav = 0
      iblksav = 0
      ipin    = 0
      ipinsav = 0

      do i = 1, numout
        tstart    = tbegout(i)
        tstop     = tendout(i)
        node      = ixnodout(i)
        lnode     = ixlstnodout(i)
        ibrn      = ixbrnout(i)
        iblk      = ixblkout(i)
        ivartyp   = itypout(i)
        iblktyp   = iblkout(i)
        valsave   = 0.0
        valsave2  = 0.0
        time_flag = half_step

        if (ibrnsav .ne. ibrn) then
           ibrnsav = ibrn
           iblksav = 0
           ipin    = 0
           ipinsav = 0
        end if

        if (iblksav .ne. iblk) then
          iblksav = iblk
          ipin    = ipinsav + iblk
        end if

        ipinsav = ipinsav + 1
        call get_variable (ivartyp, ibrn, node, lnode, iblktyp, ht, 
     &                     rht, 0.0, valsave, valsave2, value1, 
     &                     time_flag, iblk, ipin)
        val1(i) = value1
        saveout1(i)   = valsave
        saveout2(i)   = valsave2
        time_flag = whole_step
        valsave      = 0.0
        valsave2     = 0.0
        call get_variable (ivartyp, ibrn, node, lnode, iblktyp, ht, 
     &                     rht, 0.0, valsave, valsave2, value2, 
     &                     time_flag, iblk, ipin)
        val2(i) = value2
        saveout3(i)   = valsave
        saveout4(i)   = valsave2
      end do

      write(outunit) 0.0,0.0,(val1(jj),val2(jj),jj=1,numout)
c
c Set some invariant counters:
c ntot is the total number of nodes, nbv(i) is the total number of nodes in all
c previous branches and the current one, nbe is the total number of nodes times
c 2 in all the previous branches, nadd_array is the total number of nodes in
c all the previous branches.
c
      k               = 2
      nxx             = 0
      ntot            = nr(1)
      nbv(1)          = ntot
      nbe(1)          = 0
      nadd_array(1)   = 0
      do i = 2, nb
        ntot          = ntot + nr(i)
        nbv(i)        = ntot
        nxx           = nxx + nbv(i-1)
        nbe(i)        = nxx * k
        nadd_array(i) = nadd_array(i-1) + nr(i-1)
      end do

802   return
      end

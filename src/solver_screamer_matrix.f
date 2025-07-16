      subroutine screamermatrix

c
c-----Description-------------------------------------------------------
c
c Author/Date: Mathias Bavay  11/04
c      Modifications:
c
c 2015-03-25 RBS: The double precision variable a is now passed and
c                 defined.
c 2015-03-25 RBS: The branching coefficient vectors bb and cc are were
c                 removed.
c 2015-06-17 RBS: Real variables defined internally and taken from
c                 common in zdemloops.h.
c 2015-12-25 YG:  The parameter a was removed from the screamermatrix,
C                 the vector a is defined in the common block in 
C                 zdemwork.h now.
c
c ----------------------------------------------------------------------
c
c Purpose: This subroutine performs the calculations. 
c     
c Called by: main_loop.f
c    
c Include the files with the various keywords and integer flags.
c
      include 'zdemparm.h'
      include 'zdempprm.h'
c
c Include the files specifying the array dimensions and the common
c blocks.
c
      include 'zdemmax.h'
      include 'zdemcomm.h'
      include 'zdemwork.h'
      include 'zdemout.h'
      include 'zdemenv.h'
      include 'zdemvars.h'
      include 'zdemloop.h'

c
c Include file with version string
c

      include 'version.h'

c
c Define internal variables
c

      real
     & any3, any4, ctime, vcond, vtime
      integer
     & iblock1, icbp, icx, icy, iexit_type, if_cendsource,
     & if_vendsource, imitl, nele, node_num, nr2, nrm, nrow, ny, nzz

c
c
c branch connections - load bb,cc matrices
c Note that the do 211 loop will not be executed if nb=1 .
c

c        nbm = nb - 1   This line removed since nbm already defined!!!!
        do 211 icb = 1, nbm
          icbp       = icb + 1
          node_num   = indexb(1,icb)
          iexit_type = indexb(2,icb)
          icx        = (node_num-2)*k + 1 + nbe(icb)
          icy        = (node_num-1)*k + 1 + nbe(icb)
c
c Fill for top or end branch.
c
          if (iexit_type .eq. topbranch) then
            zib(node_num-1,1) = -zir(1,icbp)
            zib(node_num,1)   = -zib(node_num-1,1)
            vsour(icb)        = v(node_num-1,1) - v(node_num,1)
          else
            zib(node_num,1) = -zir(1,icbp)
            vsour(icb)      = v(node_num,1)
          end if
  211   continue
c
c Reset variable element values.
c
        itab_counter=0
c------------------------------------------------------------------------
c
      call get_model
c
c------------------------------------------------------------------------
c
c Reset MITL conductances
c
        do imitl = 1, nmitline
          if (indexmitl(5,imitl) .eq. mitline) then
            call reset_mitl (imitl)
          else if (indexmitl(5,imitl) .eq. pmitline) then
            call reset_pmitl (imitl)
          end if
        end do
c
c ----------------------------------------------------------------------
c
c Load the 'a' vector using the voltage and current equations at each
c node.
c
c A note about entering zero values:  There is a divide by certain 'a'
c elements in the matrix solver, hence these elements may not be zero.
c These elements are:
c  For the current equations:  First and last nodes in a branch: a(ny+3)
c                              All other nodes in a branch:      a(ny+4)
c  For the voltage equations:  First and last nodes in a branch: a(ny+4)
c                              All other nodes in a branch:      a(ny+3)
c
        k    = 2
        nrow = 3*k + 1
c
c Set the total number of elements in a.
c Then zero it.
c

        nele = nrow * k * ntot
        do i = 1, nele
          a(i) = 0.0
        end do
c
c Now enter the appropriate values into a.
c
c
c First, loop over the branches.
c
        do 102   ib = 1, nb
          nrx       = nr(ib)
          nzz       = nrow * k * nadd_array(ib)
          nrm       = nrx - 1
c
c Loop over the nodes, from node 2 to the next-to-last using the general
c voltage and current equations.
c
          do 5 i = 2, nrm
c
c Current equation, second row.  a(ny+4) is the diagonal element
c and is never zero.
c
            j       = (i-1)*2 + 1
            ny      = nrow*j + nzz
c this is the -1 that always preceeds the AVi and the AIi in the row
            a(ny+2) = -0.5
c a(ny+3) loads AVi, in this case it was not scaled by 2X as in the manual
c rht is the inverse time step
            a(ny+3) = rht*c(i,ib) + 0.5*g(i,ib)
c this is the +1 that always follows the AVi and the AIi in the row
            a(ny+4) = +0.5
c loads the value for BVi
            a(ny+7) = (rht*c(i,ib) - 0.5*g(i,ib)) * v(i,ib)
     &                - 0.5 * (zir(i,ib)-zir(i-1,ib)) + 0.5*zib(i,ib)
c
c Voltage equation, first row.  a(ny+3) is the diagonal element and is
c never zero.
c

            ny      = ny - nrow
c this is the -1 that always preceeds the AVi and the AIi in the row 
            a(ny+3) = -0.5
c loads AIi, it is not scaled by 2x
            a(ny+4) = rht*zlr(i,ib) + 0.5*rr(i,ib)
c this is the +1 that always follows the AVi and the AIi in the row
            a(ny+5) = +0.5
c loads the value for BIi
            a(ny+7) = (rht*zlr(i,ib) - 0.5*rr(i,ib)) * zir(i,ib)
     &                + 0.5*(v(i,ib)-v(i+1,ib))
c loops to load up all of the nodes except the first and last nodes.


    5     continue
c
c Do the first and last nodes of each branch separately because of
c boundary conditions and the possibility of sources.
c
          nr2 = 2 * nrx
c
c First row.  Current equation.  a(ny+3) is the diagonal element and
c should not be zero.
c
          ny  = nzz
c
c If this is the first block, look for voltage and current sources.
c iblock sets the block type.
c
          if (ib .eq. 1) then



            iblock1 = iin(1,1,1)
c
c Voltage source for block 1 in branch 1 (current equation).
c Set the voltage at the new time.
c Note: that we are assuming that at most one voltage source block is
c present and that it is block 1 of branch 1.
c
            if (iblock1 .eq. voltsource) then
              call set_voltage (tim, 1, vtime)
              a(ny+3) = +1.0
              a(ny+7) = vtime
c
c Current source for block 1 in branch 1 (current equation).
c Note: that we are assuming that at most one current source block is
c present and that it is block 1 of branch 1.
c Here we set the voltage at node 1 = 0.
c
            else if (iblock1 .eq. currsource) then
              a(ny+3) = +1.0
c
c Anything else at the beginning (current equation).
c
            else
              any3    = 0.5*g(1,1) + rht*c(1,1)
              if (abs(any3) .lt. 1.0e-6) then
                a(ny+3) = 1.0e-6
              else
                a(ny+3) = any3
              end if
              a(ny+4) = +0.5
              a(ny+7) = (-0.5*g(1,1) + rht*c(1,1))*v(1,1) - 0.5*zir(1,1)
c
            end if
c
c First node for all but first branch (current equation).
c Set the voltage at the first node = 0, the branch connection is
c made elsewhere.
c
          else
            a(ny+3) = +1.0
c
          end if

c
c Second row (voltage equation) for first node. a(ny+4) is the diagonal
c element and may not fall below 1e-6.
c Only need to modify the usual equation if we have a current source
c (as the first block in branch 1.
c
          ny = nrow + nzz
c
c Current source, set the current at this time: I1 = I(t).
c
          if ((ib .eq. 1) .and. (iin(1,1,1) .eq. currsource)) then
            call set_current (tim, 1, ctime)
            a(ny+4) = +1.0
            a(ny+7) = ctime
c
c Every other block type.
c
          else
            a(ny+3) = -0.5
            any4    = rht*zlr(1,ib) + 0.5*rr(1,ib)
            if (abs(any4) .lt. 1.0e-6) then
              a(ny+4) = 1.0e-6
            else
              a(ny+4) = any4
            end if
            a(ny+5) = +0.5
            a(ny+7) = (rht*zlr(1,ib) - 0.5*rr(1,ib)) * zir(1,ib)
     &                + 0.5*(v(1,ib)-v(2,ib))
c
          end if
c
c First row for last node.  Current equation.  a(ny+3) may not fall below
c 1e-6.  Check for voltage or current source at end or normal termination.
c
          ny = (nr2-2)*nrow + nzz
c
c Check the end of branch index which indicates a source or no source.
c If it is greater then zero, it is the index for the source.
c
          if_vendsource = ivbranch_end(ib)
          if_cendsource = icbranch_end(ib)
c
c Voltage source
c
          if (if_vendsource .gt. 0) then
            call set_voltage (tim, if_vendsource, vtime)
            a(ny+3) = +1.0
            a(ny+7) = vtime
c
c Normal termination, any other block including a current source (cendsource).
c
          else
            a(ny+2) = -0.5
            any3    = rht*c(nrx,ib) + 0.5*g(nrx,ib)
            if (abs(any3) .lt. 1.0e-6) then
              a(ny+3) = 1.0e-6
            else
              a(ny+3) = any3
            end if
            a(ny+4) = +0.5
            a(ny+7) = (rht*c(nrx,ib) - 0.5*g(nrx,ib)) * v(nrx,ib)
     &                - 0.5*(zir(nrx,ib)-zir(nrx-1,ib))
          end if
c
c Second row for last node, voltage equation. a(ny+4) can not be zero.
c Set the current flowing in or out of the branch.
c
          ny = (nr2-1)*nrow + nzz
c
c Current source and SCL current source.
c
          if (if_cendsource .gt. 0) then
            itypcs = itypcend(if_cendsource)
            if (itypcs .eq. cendsource) then
              call set_current (tim, if_cendsource, ctime)
              a(ny+4) = +1.0
              a(ny+7) = ctime
            else if (itypcs .eq. csclsource) then
              vcond   = v(nrx,ib)
              call set_sclcurr (tim, vcond, if_cendsource, ctime)
              a(ny+4) = +1.0
              a(ny+7) = ctime
            end if
c
c Termination without a current source, set for no current in the branch.
c
          else
            a(ny+4) = 1.0
          end if
            
c
  102   continue


      return
      end
      

c----------------------------------------------------------------------
c    @(#)rdchklimits.f   version 1.0   created 06/15/2005 by Mathias Bavay
c----------------------------------------------------------------------
c---------------------------------------------------------------------
c
c  This subroutine checks that no limits have been exceeded after 
c  reading the input deck.
c
c  Modifications:
c  2008-07-12 RBS Print the value of the parameter exceeded in maxout
c---------------------------------------------------------------------

      subroutine checkscreamerlimits
      
      include 'zdemmax.h'
      include 'zdemcomm.h'
      include 'zdemout.h'
      include 'zdemparm.h'
      include 'zdempprm.h'
      include 'rdscrdat.h'

c
c Check to see if any limits have been exceeded.
c
      if (nbrns .gt. max_branches) then
        write(9,2000) max_branches
 2000   format ('0', 'You have exceeded the maximum number of ',
     &               'branches allowed = ', i5)
        numerr = numerr + 1
      end if
c
      do i = 1, nbrns
        if (nbk(i) .gt. max_blocks) then
          write(9,2100) max_blocks, i
 2100     format ('0', 'You have exceeded the maximum number of ',
     &                 'blocks allowed = ', i5, ' in branch ', i2)
          numerr = numerr + 1
        end if
      end do
c
      if (numout .gt. maxout) then
        write(9,2200) numout,maxout
 2200   format ('-- At', i12,
     &          ' You exceeded the maximum number of ',
     &          'output requests allowed = ', i5)
        numerr = numerr + 1
      end if
c
      if (nvar .gt. max_var_elem) then
        write(9,2600) max_var_elem
 2600   format ('0', 'You have exceeded the maximum number of ',
     &               'nonlinear elements allowed = ', i5)
        numerr = numerr + 1
      end if
c
      if (ninit_cond .gt. max_init_cond) then
        write(9,2700) max_init_cond
 2700   format ('0', 'You have exceeded the maximum number of ',
     &               'initial conditions allowed = ', i5)
        numerr = numerr + 1
      end if
c
      if (ntransline .gt. max_trline) then
        write(9,2800) max_trline
 2800   format ('0', 'You have exceeded the maximum number of ',
     &               'transmission lines allowed = ', i5)
        numerr = numerr + 1
      end if
c
      if (nmitline .gt. max_mitl) then
        write(9,2900) max_mitl
 2900   format ('0', 'You have exceeded the maximum number of ',
     &               'MITLs allowed = ', i5)
        numerr = numerr + 1
      end if
      
      end

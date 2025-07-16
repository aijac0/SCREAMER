c----------------------------------------------------------------------
c    @(#)prcrparm.f   version 1.0   created 06/15/2005 by Mathias Bavay
c----------------------------------------------------------------------
c---------------------------------------------------------------------
c
c  This subroutine does prints on screen the general parameters
c  read from the begining of an input deck.
c
c  Modifications:
c 2008-12-08 RBS Return added
c                Formats from undefined writes included
c 2009-09-11 RBS Removed commented out grid commands
c                and user subroutine comments
c---------------------------------------------------------------------

      subroutine printscreamerparameters
      
      use zdemmax
      include 'zdemcomm.h'
      include 'zdemparm.h'
      include 'rdscrdat.h'
      
      if (tstepflg .eq. noerr) then
         write(9,*)  'Time step       ', ht
        else
         write(9,*) 'time step       '
         numerr = numerr + 1
        end if
      if (restflg .eq. noerr) then
         write(9,*)  'Default Res-time', res_time
        else
         write(9,*) 'default res-time'
         numerr = numerr + 1
        end if
      if (endtflg .eq. noerr) then
         write(9,*)  'End time        ', tmax
        else
         write(9,*) 'end time        '
         numerr = numerr + 1
        end if
      if (nprtflg .eq. noerr) then
         write(9,*)  'Number of prints', nprint
        else
         write(9,*) 'number of prints'
         numerr = numerr + 1
        end if
      write(9,*)    'max-pnts stored ', maxfpts
      if (ncyclflg .eq. noerr) then
         if (iset .eq. all_cycles) then
            write(9,*)
           else
            write(9,*)
           end if
        else
         write(9,*) 'execute cycles  '
         numerr = numerr + 1
        end if
      return
      end

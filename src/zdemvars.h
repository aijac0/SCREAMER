c----------------------------------------------------------------------
c    @(#)zdemvars.h   version 1.2.1   created 11/19/04 17:50:07
c    Last modified:   19-Nov-2004 17:50:07   MB 
C_Groups @(#)
c----------------------------------------------------------------------
c *****************************************************************************
c  This is the common block used for sharing data between zdem.f and models.f
c *****************************************************************************
c 
c  file zdemvars.h    
c
c ----------------------------------------------------------------------
c  Modified:
c
c 2004-11-19 MB:  Created by MB when separated from zdemcomm.
c 2012-04-13 RBS: zdemvars was mislabeled in the first line. Corrected.
c 2014-02-06 RBS: Change real*4 to real
c                 Define real for variables in misc_for_models
c                 Reorder misc_for_models 64-bit first
c 2014-04-04 RBS: Added mdl common drdt variable and real definition
c 2014-04-11 RBS: Noticed that htd2 had not been defined real - added
c 2014-04-11 RBS: Defined reals and integer in last common blocks
c 2015-06-18 RBS: Common block /misc for models/ does not get used in
c                 any subroutine but models. We do not need this common.
c 2015-06-18 RBS: Common block /mdl_vars/ does not get used in
c                 any subroutine but models. We do not need this common.
c 2015-06-18 RBS: Common block /misc_energy_checks/  reals do not get
c                 used in any subroutine but energy_checks.
c                 We do not need the reals in this common.
c ----------------------------------------------------------------------
c      
      integer               ibranch, itab_counter
      common /circuit_vars/ ibranch, itab_counter
      
      real               rht, timehalf, htd2
      common /time_vars/ rht, timehalf, htd2

c
c  Common block for user variables, user must also include in his
c   user subroutine(s) if he wishes to use the user variable feature
c
      real             u1, u2, u3, u4, u5, u6, u7, u8, u9, u10
      common /uservar/ u1, u2, u3, u4, u5, u6, u7, u8, u9, u10
      
c
c Variables needed for the energy check in main_loop, start_run,
c energy_checks
c
      real
     & econ, eres, esour, elosscap, elossind, error, ecap, eind

      common /for_energy_checks/
     & econ, eres, esour, elosscap, elossind, error, ecap, eind

c
c Misc variables node1 and node2 are only used in energy_checks and
c could be moved to energy_checks.f. The following common could be
c combined with the previous common.
c
      integer
     & i, ib, node, node1, node2, nrx

      common /misc_energy_checks/
     & i, ib, node, node1, node2, nrx


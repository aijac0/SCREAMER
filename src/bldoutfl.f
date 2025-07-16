      subroutine build_out (tim, timehalf, timestep, rtimstep)
c
c-------Description-----------------------------------------------------
c
c  Source File : bldoutfl.f
c
c  Author/Date : Kelley Fugelso, 1265 (SEA)    12/89
c
c  Purpose     : Write the SCREAMER output parameter file.
c
c  Called by   : program zdem
c
c  Calls       : Subroutine GET_VARIABLE
c
c  Modifications:
c
c  MLK, 03/07/95, Change include filenames to be 8 characters or less
c  MLK, 01/16/96, Added argument to get_variable call for referencing
c                 the "pin" array directly within that subroutine
c  2014-02-06 RBS: Changed real*4 to real
c 2015-06-23 RBS: Declared time_flag, half_step, whole_step internal to
c                 function to eliminate compiler warnings
c
c-------Define Passed Variables-----------------------------------------
c
      real      tim,      ! Current problem time at whole time step
     +          timehalf, ! Current problem time at half time step
     +          timestep, ! delta t
     +          rtimstep  ! 1/delta t
c
c-------Include Files---------------------------------------------------
c
      include 'zdemparm.h'
      include 'zdempprm.h'
      include 'zdemmax.h'
      include 'zdemcomm.h'
      include 'zdemwork.h'
      include 'zdemout.h'
c
c-------Output Parameters-----------------------------------------------
c
c     NONE
c
c-------Define Internal Variables---------------------------------------
c
      integer vartype, branch, node, blktype, block, lastnode
      integer ipin, ipinsav, ibrnsav, iblksav
c
c ***** Time flag parameters ******
c
      integer time_flag, half_step,     whole_step
      parameter         (half_step = 1, whole_step = 2)

c
c-------Subroutine Body-------------------------------------------------
c
c Loop over each output request and set the branch and node index, the
c type of variable, the type of block, and the value saved (if any) from
c the last time step.
c
c Variables needed to calculate block number used in "pin" array
c to pass to get_variable:
c ibrnsav is the number of the branch associated with the last output
c request
c iblksav is the number of the block associated with the last output
c request
c ipinsav is the number of output requests in this branch
c ipin is the block number used in "pin"
c
      ibrnsav = 0
      iblksav = 0
      ipin    = 0
      ipinsav = 0
      do iout = 1, numout
        node    = ixnodout(iout)
        lastnode= ixlstnodout(iout)!Needed for energy or power stored or
c                                  ! lost in a transmission line only
        branch  = ixbrnout(iout)
        block   = ixblkout(iout)!Needed for user variable retrieval only
        vartype = itypout(iout)
        blktype = iblkout(iout)

      if (ibrnsav .ne. branch) then
        ibrnsav = branch
        iblksav = 0
        ipin    = 0
        ipinsav = 0
      end if

      if (iblksav .ne. block) then
        iblksav = block
        ipin    = ipinsav + block
      end if

        ipinsav = ipinsav + 1
c
c Find the variable value (at the half time step and the
c    the whole time step) and the saved value and write them to the
c    SCREAMER output parameter file
c
c 06/89 klf Added BLOCK argument for user variable retrieval
c
        time_flag = half_step
        valsave = saveout1(iout)
        valsave2= saveout2(iout)   ! Needed for plotting energy or power
c                                  !  stored on transmission line only
        call get_variable (vartype, branch, node, lastnode, blktype,
     &                     timestep, rtimstep, timehalf, valsave,
     &                     valsave2, value1, time_flag, block, ipin)
        val1(iout) = value1
        saveout1(iout) = valsave
        saveout2(iout) = valsave2  ! Needed for plotting energy or power
c                                  !  stored on transmission line only
        time_flag = whole_step
        valsave = saveout3(iout)
        valsave2= saveout4(iout)   ! Needed for plotting energy or power
c                                  !  stored on transmission line only
        call get_variable (vartype, branch, node, lastnode, blktype,
     &                     timestep, rtimstep, timehalf, valsave,
     &                     valsave2, value2, time_flag, block, ipin)
        val2(iout) = value2
        saveout3(iout) = valsave
        saveout4(iout) = valsave2  ! Needed for plotting energy or power
c                                  !  stored on transmission line only
      end do
c
c  Write out the current halftime step, the current whole time step,
c   and the values of each output variable (at the halftime step
c   and the whole time step)
c
      write(outunit) timehalf,tim,(val1(jj),val2(jj),jj=1,numout)
c      write(9,*) 'Time = ',tim, '     First output = ',val1(1),val2(1)
c
c-------End of Subroutine-----------------------------------------------
c
      return
      end

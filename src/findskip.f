      integer function ifsteps (maxpts, dt, tstart, tstop)
c
c ----------------------------------------------------------------------
c Modifications:
c   08/18/95, MLK, changed intrinsic from jmax0 to generic max for
c                  improved portability
c  2014-02-06 RBS: Chaged real*4 to real
c  2014-05-07 RBS: All internal variables defined explicitly
c ----------------------------------------------------------------------
c
c Finds the number of steps to skip for each plot-print-file-table-UFO
c request so that we don't store more than maxpts points for any
c output request.
c
c Define passed variables
c
      integer    maxpts
      real       dt, tstart, tstop
c
c Define internal variables
c
      integer ntimstp, maxpts1
c
c Set the number of time steps in the plotting window.
c Set the max points allowed for storage to one less than the actual
c maximum (to be safe).
c
      ntimstp = ifix ((tstop - tstart) / dt)  +  1
      maxpts1 = maxpts - 1
c
c Divide the steps in the window by the max allowed to get the steps to skip,
c then add one to this if there was a remainder after the integer divide.
c (skip one means take every point, skip two means take every other point, etc)
c
      ifsteps = max (ntimstp/maxpts1, 1)
      if (ifsteps*maxpts1 .lt. ntimstp) then
        ifsteps = ifsteps + 1
      end if
c
      return
      end

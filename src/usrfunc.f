      subroutine user (timestep, time, p1, p2, p3, p4, v1, v2, v3)
c
c Dummy user subroutine for SCREAMER.
c Need to link to this for the version which does not require a user
c subroutine.  (This will resolve all calls to this subroutine).
c
c
c Dummy use of passed variables to prevent compiler warnings
c
      timesteptmp = timestep
      timetmp = time
      p1tmp = p1
      p2tmp = p2
      p3tmp = p3
      p4tmp = p4
      v1tmp = v1
      v2tmp = v2
      v3tmp = v3
c
      return
      end


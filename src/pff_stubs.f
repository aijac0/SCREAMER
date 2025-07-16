c Purpose: Stub routines which are missing but required by mkpfffl.for 
c
c Author:  Aidan Collins 02 July 2025
c
c Source File: mkpfffl.f
c

      subroutine pfuopn(iouttype, opff, maxout, indices, numpff)
      implicit none
      integer iouttype, maxout, indices(*), numpff
      real    opff(*)
      return
      end

      subroutine pfwuf1(iouttype, opff, maxout, indices, numpff)
      implicit none
      integer iouttype, maxout, indices(*), numpff
      real    opff(*)
      return
      end

      subroutine pfsvrb(iouttype, opff, maxout, indices, numpff)
      implicit none
      integer iouttype, maxout, indices(*), numpff
      real    opff(*)
      return
      end

      subroutine pfucls(iouttype, opff, maxout, indices, numpff)
      implicit none
      integer iouttype, maxout, indices(*), numpff
      real    opff(*)
      return
      end

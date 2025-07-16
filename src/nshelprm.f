      subroutine nshellparm (parms, nparms)
c
c  June 6, 1997   Ken Struve
c 2014-02-06 RBS: Changed real*4 to real
c 2014-05-02 RBS: Changed integer*4 to integer
c
c This routine sets up arrays for the nshellmodel.
c Include the common block for plotting so that we can set some
c initial plotting values.
c
c Define passed variables
c
      real       parms(*)
      integer    nparms
c
      include   'zdemmax.h'
      include   'zdemout.h'
c
c Sets up the parameters needed for the nshell implosion model and
c returns them in parms(i).
c nparms is the number of parameters in parms.
c
c parms is sent with the basic parameters needed to rearrange and fill
c the actual parms array.
c
      real       temp(10)
c
c Internal parameter
c
      real       mu
      parameter (mu = 2.0e-7)
c
c Fill temp with parms, then rearrange parms and fill it in.
c
      do i = 1, nparms
        temp(i) = parms(i)
      end do
c ***********************************************************************
c In parms we want we store a number of parameters
c
c In temp:
c 1: length
c 2: rmin
c 3: akgap
c 4: ttrap
c ***********************************************************************
c
      nparms    = 14                                 ! Number of parameters
      parms(1)  = shellradius(1)                     ! Initial radius
      parms(2)  = temp(2)                            ! Minimum shell radius
      parms(3)  = shellmass(1)                       ! Mass(t)
      parms(4)  = temp(1)                            ! Shell length
      parms(5)  = mu*temp(1)                         ! lconst
      parms(6)  = (mu*temp(1))*log(parms(1)/parms(2))  ! L at min radius
      parms(7)  = 0.0                                ! L from last half time step
      parms(8)  = parms(1)                           ! radius from last half time step
      parms(9)  = 0.0                                ! velocity from last half time step
      parms(10) = 0.0                                ! logical parameter for final radius
      parms(11) = 0.0                                ! energy into the load
      parms(12) = 0.0                                ! peak current
      parms(13) = temp(3) + parms(1)                 ! wall radius of return current
      parms(14) = parms(5)*log(parms(13)/parms(1))   ! Initial inductance
      parms(15) = temp(4)                            ! Time of magnetic trapping of currents
c
      trapped = .false.
c
      do i=1,max_shells
      shell(i)=i
      shellcurr(i)=0.0
      end do
      shellcurr(max_shells+1)=0.0
c
c Set initial plotting values for foil radius, velocity,
c acceleration, kinetic energy.
c
      shellrad   = parms(1)
      shellvel   = 0.0
      shellacc   = 0.0
      shellke    = 0.0
      shellm     = parms(3)
c
      return
      end

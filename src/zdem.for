c
c ----------------------------------------------------------------------
c
c    SSSS    CCCC   RRRRR   EEEEEE    AA    M    M  EEEEEE  RRRRR
c   S    S  C    C  R    R  E        A  A   MM  MM  E       R    R
c   S       C       R    R  E       A    A  M MM M  E       R    R
c    SSSS   C       RRRRR   EEEEE   AAAAAA  M MM M  EEEEE   RRRRR
c        S  C       R   R   E       A    A  M    M  E       R   R
c   S    S  C    C  R    R  E       A    A  M    M  E       R    R
c    SSSS    CCCC   R    R  EEEEEE  A    A  M    M  EEEEEE  R    R
c
c ----------------------------------------------------------------------
c
c     Point of Contact:
c       Rick B. Spielman
c       Idaho State University
c       -- Idaho Accelerator Center
c       1500 Alvin Ricken Drive
c       Pocatello, ID  83201-2783
c       505-715-0416 (cell)
c       spierick@isu.edu (e-mail)
c
c     SCREAMER is open source under the GNU GPL 3.0 License
c
c ----------------------------------------------------------------------
c
c     series matrix load and solution
c
c     single line code for pulse power design
c     array parameters - nr mesh points - 1 at top
c     v,vn = old, new node voltage
c     zib = current entering branch segment
c     zir,zirn = old, new long. current in R - L segment
c     zlr = series inductance, c = shunt capacitance, 
c     g = shunt conductance, rr = series resistance
c     shunt capacitance at the half time step.
c     inductance (vacuum) = impedance/speed of light
c     capacitance (vacuum) = 1/(impedance*speed of light)
c     capacitance (vacuum) = epso*width*length/height
c     inductance (vacuum) = zmuo*height*length/width
c
c ----------------------------------------------------------------------
c Modifications:
c
c 2015-06-23 RBS: Integer ibgin was removed as unused.
c 2018-08-09 RBS: Removed all c++ links, added read filename
c 2019-02-01 RBS: Enter the input file name on the command line to allow
c                 batch processing
c 2019-11-15 YG:  Added !$ statements for OpenMP version (they do
c                 not effect the sequential version)
c 2025-07-10 AJC: Added calls to array alloc/dealloc subroutine
c ----------------------------------------------------------------------

!$      use omp_lib

c Include the modules containing keywords and the integer flags,
c and parameter which are to be filled by this subroutine.
c

      use zdemmax
      use zdemwork
      use zdemcomm
      use zdemloop
      use zdemparm
      include 'zdemout.h'
      include 'zdemenv.h'
      include 'zdemvars.h'
      include 'zdempprm.h'
c
c Define local variables
c
      character     ifile*80, ithread*80
      integer       clen
      integer       status
      integer       narg
      integer       nc, nt
      integer       intsign, parseflag

c---------------------------------------------------------
c INITIALIZATIONS
c
c Allocate allocatable arrays
c Clear all needed arrays
c
      call allocarry
      call clrarry
c
c Read in the file name using the GNU Fortran compiler option
c

      narg = command_argument_count()
      if (narg > 0) then
        call get_command_argument (1,ifile)
      else
       print '(a)', ' ### Zdem - Input file not found'
       stop
      end if
c
c Input the number of threads for the program
c
      if (narg > 1) then
        call get_command_argument(2,ithread)
        call text_to_int (ithread(1:5),nt,parseflag)

      end if
      If (parseflag > 0) then
        print '(a)', ' ### No thread number provided in input line'
      end if

      clen = len(ifile)

c
c print a start banner (date, time, version...)
c
      call banner_start_run(ifile,clen,bgin)

c
c Set the maximum number of threads based on the number of
c logical cores available on the computer. Allowed to set this
c number manually.
c

!$      nc = OMP_GET_NUM_PROCS()
!$      print '(a,i2)', '   # of cores (logical) available =  ', nc
!$      print '(a,i2)', '   # of threads used =  ', nt
!$      CALL OMP_SET_NUM_THREADS(nt)

c
c Get environment variables
c
      call get_env_var(ifile,clen)
c
c Read in the SCREAMER input file (assigned to logical unit 4)
c This subroutine also gives a "nice" print-out of the data.
c

      call readscreamerdata(status)
      if (status .ne. 0) then
        goto 802
      endif

c
c Echo the set-up showing all values (slightly cryptic, but useful
c for debugging).
c

      if (echoset .eq. yes_echo) then
        call echo_setup
      end if

c
c initialize the problem
c

      call init_problem(status)
      if (status .ne. 0) then
        call close_outfile(iunit,ierr)
        goto 802
      endif

c
c---------------------------------------------------------
c CALCULATIONS
c
c  First, put out a banner page for the status cycle printing for t > 0.
c
      call banner (cyclln)
c
c ***** Loop over all time steps. *****
c   tim is the problem time and timehalf is the current half-step time.
c   icycle is a counter for printout of current circuit status.
c

      do 2 i2 = 1, nsteps
        tim        = i2 * ht
        timehalf   = tim - htd2
        icycle     = icycle + 1
        call main_loop
    2 continue      

c---------------------------------------------------------
c OUTPUTS and END
c

      call close_outfile(iunit,ierr)

c
c ***** Now do the output requests if we executed all cycles.
c

       call write_outs
  802  continue
       call banner_end_run(ifile,clen,bgin)

c Deallocate allocated arrays
      call deallocarry

      stop
      end

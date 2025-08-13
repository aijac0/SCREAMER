!------------------------------------------------------------------------------
!
!
!
!------------------------------------------------------------------------------

! -----------------------------------------------------------------------------
! Subroutine created to dynamically allocate arrays declared with ALLOCATABLE
! These arrays are to be allocated BEFORE input is read
! Dimensions are set based on their upper bound defined in zdemmax.f90
subroutine preallocarry
      use zdemsolve
      use zdemmax
      use zdemcomm
      use zdemwork

! Voltages, currents, circuit elements, etc.
      IF (.NOT. ALLOCATED(v))               ALLOCATE(v(max_nodes, max_branches))
      IF (.NOT. ALLOCATED(vold))            ALLOCATE(vold(max_nodes, max_branches))
      IF (.NOT. ALLOCATED(vn))              ALLOCATE(vn(max_nodes, max_branches))
      IF (.NOT. ALLOCATED(g))               ALLOCATE(g(max_nodes, max_branches))
      IF (.NOT. ALLOCATED(zir))             ALLOCATE(zir(max_nodes, max_branches))
      IF (.NOT. ALLOCATED(zlr))             ALLOCATE(zlr(max_nodes, max_branches))
      IF (.NOT. ALLOCATED(c))               ALLOCATE(c(max_nodes, max_branches))
      IF (.NOT. ALLOCATED(cdot))            ALLOCATE(cdot(max_nodes, max_branches))
      IF (.NOT. ALLOCATED(rr))              ALLOCATE(rr(max_nodes, max_branches))
      IF (.NOT. ALLOCATED(zlrechk))         ALLOCATE(zlrechk(max_nodes, max_branches))
      IF (.NOT. ALLOCATED(cechk))           ALLOCATE(cechk(max_nodes, max_branches))
      IF (.NOT. ALLOCATED(vsour))           ALLOCATE(vsour(max_branches))
      IF (.NOT. ALLOCATED(zirn))            ALLOCATE(zirn(max_nodes, max_branches))
      IF (.NOT. ALLOCATED(nr))              ALLOCATE(nr(max_branches))

! Various indexing arrays
      IF (.NOT. ALLOCATED(iflg))            ALLOCATE(iflg(max_nodes, max_branches))
      IF (.NOT. ALLOCATED(indexb))          ALLOCATE(indexb(2, max_branches))
      IF (.NOT. ALLOCATED(indexv))          ALLOCATE(indexv(3, max_var_elem))
      IF (.NOT. ALLOCATED(indexvs))         ALLOCATE(indexvs(2, max_volt_source))
      IF (.NOT. ALLOCATED(indexcs))         ALLOCATE(indexcs(2, max_curr_source))
      IF (.NOT. ALLOCATED(indexmitl))       ALLOCATE(indexmitl(5, max_mitl))
      IF (.NOT. ALLOCATED(indextrnf))       ALLOCATE(indextrnf(5, max_transformer))
      IF (.NOT. ALLOCATED(lastvoltf_time))  ALLOCATE(lastvoltf_time(max_volt_source))
      IF (.NOT. ALLOCATED(lastcurrf_time))  ALLOCATE(lastcurrf_time(max_curr_source))
      IF (.NOT. ALLOCATED(lasttabm_time))   ALLOCATE(lasttabm_time(max_var_elem))
      IF (.NOT. ALLOCATED(nbv))             ALLOCATE(nbv(0:max_branches))
      IF (.NOT. ALLOCATED(nbe))             ALLOCATE(nbe(max_branches))
      IF (.NOT. ALLOCATED(uservars))        ALLOCATE(uservars(max_blocks, max_branches, max_uservars))
      IF (.NOT. ALLOCATED(nadd_array))      ALLOCATE(nadd_array(max_branches))

end subroutine

! -----------------------------------------------------------------------------
! Subroutine created to dynamically allocate arrays declared with ALLOCATABLE
! These arrays are to be allocated AFTER input is read & BEFORE solver routine
! Dimensions are set based on the size of problem read from input
subroutine postallocarry
      use zdemsolve
      use zdemmax
      use zdemcomm
      use zdemwork

! Bounds of array dimensions which reflect actual size of input problem
! Each n_X variable has a counterpart max_X defined in zdemwork.f90
      integer :: n_branches, n_blocks, n_nodes, n_vars
      integer :: n_a, n_am

! Set bounds of array dimensions
      n_branches = max_branches
      n_blocks = max_blocks
      n_nodes = nbr
      n_vars = max_vars
      n_a = n_nodes * n_vars * n_branches * n_cols
      n_am = n_nodes * 2

! Solver arrays
      IF (.NOT. ALLOCATED(a))               ALLOCATE(a(n_a))
      IF (.NOT. ALLOCATED(a_prl))           ALLOCATE(a_prl(n_a))
      IF (.NOT. ALLOCATED(am_band))         ALLOCATE(am_band(n_am, n_am))
      IF (.NOT. ALLOCATED(rhs_band))        ALLOCATE(rhs_band(n_am))

! Voltages, currents, circuit elements, etc.
      IF (.NOT. ALLOCATED(zirold))          ALLOCATE(zirold(n_nodes, n_branches))
      IF (.NOT. ALLOCATED(zib))             ALLOCATE(zib(n_nodes, n_branches))
      IF (.NOT. ALLOCATED(gdot))            ALLOCATE(gdot(n_nodes, n_branches))
      IF (.NOT. ALLOCATED(zlrdot))          ALLOCATE(zlrdot(n_nodes, n_branches))
      IF (.NOT. ALLOCATED(rrdot))           ALLOCATE(rrdot(n_nodes, n_branches))

end subroutine

! -----------------------------------------------------------------------------
! Subroutine created to deallocate arrays declared with ALLOCATABLE
!
subroutine deallocarry
      use zdemsolve
      use zdemmax
      use zdemwork

! Solver arrays
      IF (ALLOCATED(a))               DEALLOCATE(a)
      IF (ALLOCATED(a_prl))           DEALLOCATE(a_prl)
      IF (ALLOCATED(am_band))         DEALLOCATE(am_band)
      IF (ALLOCATED(rhs_band))        DEALLOCATE(rhs_band)

! Voltages, currents, circuit elements, etc.
      IF (ALLOCATED(v))               DEALLOCATE(v)
      IF (ALLOCATED(vold))            DEALLOCATE(vold)
      IF (ALLOCATED(vn))              DEALLOCATE(vn)
      IF (ALLOCATED(zir))             DEALLOCATE(zir)
      IF (ALLOCATED(zirn))            DEALLOCATE(zirn)
      IF (ALLOCATED(zirold))          DEALLOCATE(zirold)
      IF (ALLOCATED(zib))             DEALLOCATE(zib)
      IF (ALLOCATED(g))               DEALLOCATE(g)
      IF (ALLOCATED(zlr))             DEALLOCATE(zlr)
      IF (ALLOCATED(c))               DEALLOCATE(c)
      IF (ALLOCATED(rr))              DEALLOCATE(rr)
      IF (ALLOCATED(gdot))            DEALLOCATE(gdot)
      IF (ALLOCATED(zlrdot))          DEALLOCATE(zlrdot)
      IF (ALLOCATED(cdot))            DEALLOCATE(cdot)
      IF (ALLOCATED(rrdot))           DEALLOCATE(rrdot)
      IF (ALLOCATED(zlrechk))         DEALLOCATE(zlrechk)
      IF (ALLOCATED(cechk))           DEALLOCATE(cechk)
      IF (ALLOCATED(vsour))           DEALLOCATE(vsour)
      IF (ALLOCATED(uservars))        DEALLOCATE(uservars)
      IF (ALLOCATED(nr))              DEALLOCATE(nr)

! Various indexing arrays
      IF (ALLOCATED(iflg))            DEALLOCATE(iflg)
      IF (ALLOCATED(indexb))          DEALLOCATE(indexb)
      IF (ALLOCATED(indexv))          DEALLOCATE(indexv)
      IF (ALLOCATED(indexvs))         DEALLOCATE(indexvs)
      IF (ALLOCATED(indexcs))         DEALLOCATE(indexcs)
      IF (ALLOCATED(indexmitl))       DEALLOCATE(indexmitl)
      IF (ALLOCATED(indextrnf))       DEALLOCATE(indextrnf)
      IF (ALLOCATED(lastvoltf_time))  DEALLOCATE(lastvoltf_time)
      IF (ALLOCATED(lastcurrf_time))  DEALLOCATE(lastcurrf_time)
      IF (ALLOCATED(lasttabm_time))   DEALLOCATE(lasttabm_time)
      IF (ALLOCATED(nbv))             DEALLOCATE(nbv)
      IF (ALLOCATED(nbe))             DEALLOCATE(nbe)
      IF (ALLOCATED(nadd_array))      DEALLOCATE(nadd_array)

      end subroutine
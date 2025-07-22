! -----------------------------
      subroutine allocarry
!
! Subroutine created to dynamically allocate arrays declared with ALLOCATABLE
!
! Include the files specifying the array dimensions and the common blocks.
!
      use zdemmax
      use zdemwork

! Voltages, currents, circuit elements, etc.
      IF (.NOT. ALLOCATED(v))               ALLOCATE(v(max_nodes, max_branches))
      IF (.NOT. ALLOCATED(vold))            ALLOCATE(vold(max_nodes, max_branches))
      IF (.NOT. ALLOCATED(vn))              ALLOCATE(vn(max_nodes, max_branches))
      IF (.NOT. ALLOCATED(zir))             ALLOCATE(zir(max_nodes, max_branches))
      IF (.NOT. ALLOCATED(zirn))            ALLOCATE(zirn(max_nodes, max_branches))
      IF (.NOT. ALLOCATED(zirold))          ALLOCATE(zirold(max_nodes, max_branches))
      IF (.NOT. ALLOCATED(zib))             ALLOCATE(zib(max_nodes, max_branches))
      IF (.NOT. ALLOCATED(g))               ALLOCATE(g(max_nodes, max_branches))
      IF (.NOT. ALLOCATED(zlr))             ALLOCATE(zlr(max_nodes, max_branches))
      IF (.NOT. ALLOCATED(c))               ALLOCATE(c(max_nodes, max_branches))
      IF (.NOT. ALLOCATED(rr))              ALLOCATE(rr(max_nodes, max_branches))
      IF (.NOT. ALLOCATED(gdot))            ALLOCATE(gdot(max_nodes, max_branches))
      IF (.NOT. ALLOCATED(zlrdot))          ALLOCATE(zlrdot(max_nodes, max_branches))
      IF (.NOT. ALLOCATED(cdot))            ALLOCATE(cdot(max_nodes, max_branches))
      IF (.NOT. ALLOCATED(rrdot))           ALLOCATE(rrdot(max_nodes, max_branches))
      IF (.NOT. ALLOCATED(zlrechk))         ALLOCATE(zlrechk(max_nodes, max_branches))
      IF (.NOT. ALLOCATED(cechk))           ALLOCATE(cechk(max_nodes, max_branches))
      IF (.NOT. ALLOCATED(vsour))           ALLOCATE(vsour(max_branches))
      IF (.NOT. ALLOCATED(uservars))        ALLOCATE(uservars(max_blocks, max_branches, max_uservars))
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
      IF (.NOT. ALLOCATED(nadd_array))      ALLOCATE(nadd_array(max_branches))

      end subroutine

! ---------------------------

      subroutine deallocarry
!
! Subroutine created to deallocate arrays declared with ALLOCATABLE
!
! Include the files specifying the array dimensions and the common blocks.
!
      use zdemmax
      use zdemwork

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
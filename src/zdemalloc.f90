! -----------------------------
      subroutine allocarry
!
! Subroutine created to dynamically allocate arrays declared with ALLOCATABLE
!
! Include the files specifying the array dimensions and the common blocks.
!
      use zdemmax
      include 'zdemwork.h'
      
      IF (.NOT. ALLOCATED(v))        ALLOCATE(v(max_nodes, max_branches))
      IF (.NOT. ALLOCATED(vold))     ALLOCATE(vold(max_nodes, max_branches))
      IF (.NOT. ALLOCATED(vn))       ALLOCATE(vn(max_nodes, max_branches))
      IF (.NOT. ALLOCATED(zir))      ALLOCATE(zir(max_nodes, max_branches))
      IF (.NOT. ALLOCATED(zirn))     ALLOCATE(zirn(max_nodes, max_branches))
      IF (.NOT. ALLOCATED(zirold))   ALLOCATE(zirold(max_nodes, max_branches))
      IF (.NOT. ALLOCATED(zib))      ALLOCATE(zib(max_nodes, max_branches))
      IF (.NOT. ALLOCATED(g))        ALLOCATE(g(max_nodes, max_branches))
      IF (.NOT. ALLOCATED(zlr))      ALLOCATE(zlr(max_nodes, max_branches))
      IF (.NOT. ALLOCATED(c))        ALLOCATE(c(max_nodes, max_branches))
      IF (.NOT. ALLOCATED(rr))       ALLOCATE(rr(max_nodes, max_branches))
      IF (.NOT. ALLOCATED(gdot))     ALLOCATE(gdot(max_nodes, max_branches))
      IF (.NOT. ALLOCATED(zlrdot))   ALLOCATE(zlrdot(max_nodes, max_branches))
      IF (.NOT. ALLOCATED(cdot))     ALLOCATE(cdot(max_nodes, max_branches))
      IF (.NOT. ALLOCATED(rrdot))    ALLOCATE(rrdot(max_nodes, max_branches))
      IF (.NOT. ALLOCATED(zlrechk))  ALLOCATE(zlrechk(max_nodes, max_branches))
      IF (.NOT. ALLOCATED(cechk))    ALLOCATE(cechk(max_nodes, max_branches))

      end subroutine

! ---------------------------

      subroutine deallocarry
!
! Subroutine created to deallocate arrays declared with ALLOCATABLE
!
! Include the files specifying the array dimensions and the common blocks.
!
      use zdemmax
      include 'zdemwork.h'

      IF (ALLOCATED(v))        DEALLOCATE(v)
      IF (ALLOCATED(vold))     DEALLOCATE(vold)
      IF (ALLOCATED(vn))       DEALLOCATE(vn)
      IF (ALLOCATED(zir))      DEALLOCATE(zir)
      IF (ALLOCATED(zirn))     DEALLOCATE(zirn)
      IF (ALLOCATED(zirold))   DEALLOCATE(zirold)
      IF (ALLOCATED(zib))      DEALLOCATE(zib)
      IF (ALLOCATED(g))        DEALLOCATE(g)
      IF (ALLOCATED(zlr))      DEALLOCATE(zlr)
      IF (ALLOCATED(c))        DEALLOCATE(c)
      IF (ALLOCATED(rr))       DEALLOCATE(rr)
      IF (ALLOCATED(gdot))     DEALLOCATE(gdot)
      IF (ALLOCATED(zlrdot))   DEALLOCATE(zlrdot)
      IF (ALLOCATED(cdot))     DEALLOCATE(cdot)
      IF (ALLOCATED(rrdot))    DEALLOCATE(rrdot)
      IF (ALLOCATED(zlrechk))  DEALLOCATE(zlrechk)
      IF (ALLOCATED(cechk))    DEALLOCATE(cechk)

      end subroutine
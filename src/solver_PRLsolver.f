      SUBROUTINE bandPRLsolver(a,a_prl,nr,indexb,nadd_array,nb,
     &                         topbranch)
!$    use omp_lib

C This SUBROUTINE bandPRLsolver executes the factorization and
C back substitution steps in the Gaussian elimination process.
C The first (factorization) step is done from the last row to
C the top row and presents the matrix in the LOWER Triangular form. The
C process is parallelized between twigs and branches. The result is the
C the lower triangular matrix. The back substitution step is
C implemented from the first row to the last. It is also parallelized
C below the main branch.
C=======================================================================
C input: a             - the coefficient matrix in the Screamer format
c        a_prl         - a copy of a used in parallel solver
C        nr(ib)        - the total number of nodes in ib-th branch
C        nb            - the total number of branches
C        indexb(1,ib)  - the connection node of the ib-th branch to
C                           the main branch
C        indexb(2,ib)  - the type of the ib-th branch
C        nadd_array(ib)- the total number of nodes in the main branch
C                           and branches from 1 to (ib-1)st.
C        topbranch=1   - the indicator of the top branch
C
C output:a_prl         - the coefficient matrix in the Screamer format
C                        which include the lower triangular matrix
C                        after the forward step of Gaussian elimination.
C                        In the next to the last spot (a_prl(ny +6)), it
C                        also includes the solution vector.
C=======================================================================
C
C Written by Dr. Yury Gryazin, 06/19/2019, ISU, Pocatello, ID
C
C Called by: main_loop.f
C
c     Define passed variables
c

      REAL*8  a(*),a_prl(*)
      INTEGER nk, nr(*),indexb(2,*),nadd_array(*), nb, topbranch
c
c Define internal variables
c
      INTEGER bln,bl(100), ib, iexit_type,
     &        k,node_num,nrow,nrx, ny,ny1, nzz,
     &        Ntwig, Nlastwig, l, i,n_nm, ie_tp, me, nthreads

      real*8 a03,a07,a13,a14,a17
c
c First, loop over the branches. Initialize am array.
c
      k    = 2
      nrow = 3*k + 1

c
c Copy a into new workin array a_prl. a_prl has seven spots
c for all nonzero entries of the nth row ot the system.
C The solution vector in the six-th spot of a_prl.
c

        DO ib = 1,nb

            nzz = 14*nadd_array(ib)
            nrx       = nr(ib)
            DO i = 1, nrx
                ny  = nrow*(i-1)*2 + nzz
                ny1 = ny + nrow
                DO l = 1, 7
                    a_prl(ny +l) = a(ny  + l)
                    a_prl(ny1+l) = a(ny1 + l)
                ENDDO
            ENDDO
        ENDDO


c
c Find the number of branch levels bln, the number of branches at each level,
c and the first and last branches at each level
c
      nbm = nb - 1

C
C Find the number bln of layers of branches in the circuit and the
C first branch bl(i) in every ith level.
C
        bln = 0
        
        if (nb.gt.1) then

C
C Find the connection branch for the last branch at every level. If it is 1 then the
C process is finished. It generalize the calculation to any number of layers.
C

            Ntwig = nb
 11         ib = 2
            Nlastwig = indexb(1,Ntwig - 1)

 10         if (Nlastwig .ge. nadd_array(ib) ) then

                ib = ib+1
                if (ib .gt. nb) then
                    STOP 'ERROR LAST TWIG Connection is wrong'
                endif
                goto 10
            endif
            Ntwig = ib - 1
            bln = bln + 1
            bl(bln) = ib

            if(ib.gt.2) goto 11

        endif

        bln = bln + 1
        bl(bln) = 1

C
C For convenience, the bl(1) is the last branch + 1, so we can uniformly loop
C through them later. So, bl(i+1) is the starting branch of the ith level and
C bl(i) - 1 is the last branch on the ith level.
C
        DO icb = bln, 1, -1

            bl(icb + 1) = bl(icb)

        ENDDO
        bl(1) = nb +1

!$OMP PARALLEL PRIVATE(nzz,nrx,ny,ny1,ib,l,
!$OMP&         a03,a07,a13,a14,a17,n_nm,ie_tp,me,nthreads)
!$OMP& FIRSTPRIVATE(nrow,bln)

cc!$                me = OMP_GET_THREAD_NUM()
cc!$          nthreads = OMP_GET_NUM_THREADS()

C
C First, the twigs will be put in the lower triagular form.
C
        if (bln .gt. 1 ) then

!$OMP DO SCHEDULE(STATIC)
            DO ib = bl(1)-1, bl(2), -1

cc!$              print *, "I am",me, "1st processing", ib

                nzz       = 14*nadd_array(ib)
                nrx       = nr(ib)

                call bandPRL3diag_BRANCH(a_prl,nzz,nrx)

                ny  = nzz
                ny1 = ny + nrow

c
c Eliminate the connection terms in the upper trangular part,
c corresponding to the branch.
c
                a03 = a_prl(ny+3)
                a07 = a_prl(ny+7)
                a13 = a_prl(ny1+3)
                a14 = a_prl(ny1+4)
                a17 = a_prl(ny1+7)

                n_nm   = indexb(1,ib-1)
                ie_tp = indexb(2,ib-1)

                call solver_PRLConElim(a_prl,n_nm,ie_tp,
     &                             topbranch,a03,a07,a13, a14,a17)

            ENDDO
!$OMP END DO

        endif

c
c
c Loop through the branch levels
c

        DO l = 2, bln - 1
!$OMP DO SCHEDULE(STATIC)
            DO ib = bl(l)-1, bl(l+1), -1

cc!$              print *, "I am",me, "2st processing", ib


                nzz       = 14*nadd_array(ib)
                nrx       = nr(ib)

                call bandPRL3diag_BRANCH(a_prl,nzz,nrx)

                ny  = nzz
                ny1 = ny + nrow

c
c Eliminate the connection terms in the upper trangular part,
c corresponding to the branch.
c
                a03 = a_prl(ny+3)
                a07 = a_prl(ny+7)
                a13 = a_prl(ny1+3)
                a14 = a_prl(ny1+4)
                a17 = a_prl(ny1+7)

                n_nm   = indexb(1,ib-1)
                ie_tp = indexb(2,ib-1)



                call solver_PRLConElim(a_prl,n_nm,ie_tp,
     &                             topbranch,a03,a07,a13, a14,a17)

            ENDDO
!$OMP END DO
!$OMP BARRIER
        ENDDO
!$OMP END PARALLEL
C
C The main branch factorization
C
        ib = 1
        nzz       = 0
        nrx       = nr(ib)

                call bandPRL3diag_MN_BRANCH(a_prl,nrx)

cc!$                me = OMP_GET_THREAD_NUM()
cc!$          nthreads = OMP_GET_NUM_THREADS()

cc!$              print *, "I am",me, "3rd processing", ib

C
C Back (actually forward in this case) substitution step
C

c
c First, main branch
c
        nrx = nr(1)

        call PRLBackSub_MainBRANCH_NP(a_prl,nrx)
c
c Loop thorough the branches at every level, do the backsubstitution
c
c
!$OMP PARALLEL PRIVATE(jadd,nrx,ib,l,n_nm,ie_tp,me,nthreads)
!$OMP& FIRSTPRIVATE(bln)

cc!$                me = OMP_GET_THREAD_NUM()
cc!$          nthreads = OMP_GET_NUM_THREADS()

        DO l = bln-1, 1, -1
!$OMP DO SCHEDULE(STATIC)
            DO ib = bl(l)-1, bl(l+1), -1

cc!$              print *, "I am",me, "4st processing", ib
                jadd   = 2*nadd_array(ib)
                nrx    = nr(ib)
                n_nm   = indexb(1,ib-1)
                ie_tp  = indexb(2,ib-1)

                call PRLBackSub_BRANCH(a_prl,jadd,nrx,n_nm,
     &                                 ie_tp,topbranch)

            ENDDO
!$OMP END DO
!$OMP BARRIER
        ENDDO
!$OMP END PARALLEL

cc      read *, i

      RETURN
      END



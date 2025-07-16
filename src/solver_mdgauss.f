      SUBROUTINE solvermdgauss(am,rhs,nk,max_am,nadd_array,nb,nr)

C
C This SUBROUTINE solvermdgaussH solves the linear system using modified
C Gaussian elimination procedure based on highly structured sparse
C matrix obtained on the previous step. The solution vector is in the
C rhs_band vector.
C

C=======================================================================
C input:        am            - the array containing the nk by nk
C                               coefficient matrix
C               rhs           - the vector containing the right hand
C                               side in the system
C               nk            - the size of the system
C               max_am        - the leading dimension of array am
C               nadd_array    - the branch connection location
C               nb            - the number of branches
C               nr            - the number on nodes in the branches
C output:       
C               rhs           - the solution vector.
C=======================================================================

C Dr. Yury Gryazin, 06/19/2014, ISU, Pocatello, ID
C Final version 11/10/2014
C Modifications:
C 2014-12-12 RBS: Changed the array size passed variable from max_bb
C                 to max_am, changed all references to max_bb to max_am
c 2015-06-18 RBS: Cleaned up passed variables and internal variable
c                 definitions
c 2015-06-23 RBS; Removed unused variables CTS and jadd
c
c Define Passed Variables
c
      use omp_lib
      REAL*8  am(max_am,*), rhs(*)
      INTEGER nk, max_am, nadd_array(*), nb, nr(*), nt, nthreads,threadID, nc, nm

c
c Define internal variables
c
      REAL*8      ATS, BTS
      INTEGER     ib, ibb, ibbb, ii, im1, im2, nbm, nrm
      CHARACTER*5 NAME, NAME1

c        DO I = 1,nb
c           PRINT *,' I=',I,nadd_array(I)
c           END DO
c        read *, kkkk
c

        nbm = nb - 1

        DO ib = 1,nb

C
C The first node in the ib branch
C

            i = 2 * nadd_array(ib) + 1
            
            ATS = 1.0/am(i,i)
            am(i,i+1) = am(i,i+1)*ATS
            am(i,i  ) = 1.0
            am(i,i+2) = 0.0
            
            DO ibb = ib+1, nb
                j = 2 * nadd_array(ibb) + 2
                am(i,j) = am(i,j)*ATS
                END DO
            
            rhs(i) = rhs(i)*ATS

            BTS = 1.0/( am(i+1,i+1) - am(i+1,i)*am(i,i+1) )
            am(i+1,i+2) = am(i+1,i+2)*BTS
            am(i+1,i+1) = 1.0
     
            DO ibb = ib+1, nb
                j = 2 * nadd_array(ibb) + 2
                am(i+1,j)= (am(i+1,j) - am(i+1,i)*am(i,j) )*BTS
                END DO

            rhs(i+1)= (rhs(i+1) - am(i+1,i)*rhs(i) )*BTS

            DO ibb = ib+1, nb
                ii = 2 * nadd_array(ibb) + 1
                am(ii,i+1) = am(ii,i+1) - am(ii,i)*am(i,i+1)
               
                DO ibbb = ib+1, nb
                    j = 2 * nadd_array(ibbb) + 2
                    am(ii,j)= am(ii,j)- am(ii,i)*am(i,j)
                    END DO

                rhs(ii) = rhs(ii) - am(ii,i)*rhs(i)
                                                                        
                END DO

C
C  The nodes from 2 to nr(ib)-1
C
             
            nrm = nr(ib) - 1
             
            DO n = 2,nrm
                i   = 2 * nadd_array(ib) + 2 * n
                im1 = i-1
                im2 = i-2

c
c The first row (Voltage equation) is already in the upper diagonal
c form so we are working only with the second equation in a block.
c Current equation, second row.  Nonzero elements are
c am(j,j+1),am(j,j+2) and possibly am(j,nb(ib)).
c

                ATS  = am(i,i-1) - am(i-2,i-1)*am(i,i-2)
                BTS  = 1.0/( am(i  ,i  ) - am(i-1,i)*ATS )
                am(i,i+1) = -am(i-1,i+1)*ATS*BTS
                am(i,i  ) = 1.0
                
                DO ibb = ib+1, nb
                    j = 2 * nadd_array(ibb) + 2
                    am(i,j) =  am(i,j) - am(i-2,j)*am(i,i-2)
                    am(i,j) = (am(i,j) - am(i-1,j)*ATS)*BTS
                    END DO
                
                rhs(i)  = rhs(i)-rhs(i-2)*am(i,i-2)
                rhs(i)  = (rhs(i)-rhs(i-1)*ATS)*BTS

                DO ibb = ib+1, nb
                    ii = 2 * nadd_array(ibb) + 1
                    ATS = am(ii,i-1)-am(i-2,i-1)*am(ii,i-2)
                    am(ii,i  ) = am(ii,i  ) - am(i-1,i  )*ATS
                    am(ii,i+1) = am(ii,i+1) - am(i-1,i+1)*ATS
                                   
                    DO ibbb = ib+1, nb
                        j = 2 * nadd_array(ibbb) + 2
                        am(ii,j) = am(ii,j) - am(i-2,j)*am(ii,i-2)
                        am(ii,j) = am(ii,j) - am(i-1,j)*ATS
                        END DO

                    rhs(ii) = rhs(ii) - rhs(i-2)*am(ii,i-2)
                    rhs(ii) = rhs(ii) - rhs(i-1)*ATS
                                                                        
                    END DO
                END DO
C
C The last node in the ib branch
C

            i = nr(ib)*2 + 2*nadd_array(ib) - 1  
            ATS = 1/(am(i,i)-am(i-1,i)*am(i,i-1))
            BTS = 1.0/am(i+1,i+1)
            am(i  ,i+1) = am(i,i+1)*ATS
            am(i  ,i  ) = 1.0
            am(i+1,i+1) = 1.0
            
            DO ibb = ib+1, nb
                j = 2 * nadd_array(ibb) + 2
                am(i  ,j) = ( am(i,j) - am(i-1,j)*am(i,i-1) )*ATS
                am(i+1,j) = am(i+1,j)*BTS
                END DO
            
            rhs(i  ) = ( rhs(i) - rhs(i-1)*am(i,i-1) )*ATS
            rhs(i+1) = rhs(i+1)*BTS

            DO ibb = ib+1, nb
                
                ii = 2 * nadd_array(ibb) + 1
                        
                ATS = am(ii,i  ) - am(i-1,i  )*am(ii,i-1)
                BTS = am(ii,i+1) - am(i  ,i+1)*ATS
                                                                    
                DO ibbb = ib+1, nb
                    j = 2 * nadd_array(ibbb) + 2
                    am(ii,j) = am(ii,j) - am(i-1,j)*am(ii,i-1)
                    am(ii,j) = am(ii,j) - am(i  ,j)*ATS
                    am(ii,j) = am(ii,j) - am(i+1,j)*BTS
                    END DO

                rhs(ii) = rhs(ii) - rhs(i-1)*am(ii,i-1)
                rhs(ii) = rhs(ii) - rhs(i  )*ATS
                rhs(ii) = rhs(ii) - rhs(i+1)*BTS
                                                                        
                END DO
            END DO

        ktime = 1
        if (ktime .eq. 0) then

            NAME ='UMTRX'
            NAME1='URHS'

            OPEN(57,FILE=NAME)

            DO I = 1,nk
                WRITE(57,*) 'I=',I
                WRITE(57,*)  (am(I,J),J=1,nk)
                ENDDO

            CLOSE(57)
            OPEN(58,FILE=NAME1)

            DO I = 1, nk
                WRITE(58,*) I,rhs(I)
                ENDDO

            CLOSE(58)

            PRINT *, 'THE UPPER TRIANGULAR MATRIX AND CORRESPONDING RHS
     &ARE STORED in UMTRX and URHS files.'
            STOP

        end if
C        
C Back substitution          
C     
C
C The last branch
C 
        ibr = nb
        
        i = nr(ibr)*2 + 2*nadd_array(ibr) - 1
        rhs(i) = rhs(i) - am(i,i+1)*rhs(i+1)
        
        nrm = nr(ibr)-1
             
        DO n = 1,nrm
            i   = 2 * nadd_array(ibr) + 2*(nr(ibr)-n)
            rhs(i  ) = rhs(i  ) - am(i  ,i+1)*rhs(i+1)
            rhs(i-1) = rhs(i-1) - am(i-1,i  )*rhs(i  )
     &                          - am(i-1,i+1)*rhs(i+1) 
            END DO
C
C  The branches from nb-1 to 1
C

        DO ib = 1,nb-1
        
            ibr = nb - ib
        
            i = nr(ibr)*2 + 2*nadd_array(ibr) 
                    
            DO ibb = ibr+1, nb
                j = 2 * nadd_array(ibb) + 2
                rhs(i) = rhs(i) - am(i,j)*rhs(j)
                END DO
            
            rhs(i-1) = rhs(i-1) - am(i-1,i)*rhs(i)

            DO ibb = ibr+1, nb
                j = 2 * nadd_array(ibb) + 2
                rhs(i-1) = rhs(i-1) - am(i-1,j)*rhs(j)
                END DO
                
            nrm = nr(ibr) - 1
             
            DO n = 1,nrm
                i = 2 * nadd_array(ibr) + 2*(nr(ibr)-n)
                rhs(i) = rhs(i) - am(i,i+1)*rhs(i+1)
                
                DO ibb = ibr+1, nb
                    j = 2 * nadd_array(ibb) + 2
                    rhs(i) = rhs(i) - am(i,j)*rhs(j)
                    END DO
                
                rhs(i-1) = rhs(i-1) - am(i-1,i)*rhs(i) 
     &                              - am(i-1,i+1)*rhs(i+1) 
                DO ibb = ibr+1, nb
                    j = 2 * nadd_array(ibb) + 2
                    rhs(i-1) = rhs(i-1) - am(i-1,j)*rhs(j)
                    END DO
                END DO

           END DO

      RETURN
      END

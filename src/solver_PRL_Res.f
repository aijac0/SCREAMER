      SUBROUTINE PRL_Res(ib,i,nrx,a,a_prl,indexb,nadd_array,nb,
     &                         topbranch,ep,ep1)
!$    use omp_lib

C This SUBROUTINE PRL_Res calculate the residual at the given node.

C========================================================================
C input:
C        ib            - the number of the (current) branch
C        i             - the number of the node in the branch
C        nrx           - the total number of nodes in the branch
C        a             - the coefficient matrix in the Screamer format
C        a_prl         - the factorized a used in parallel solver
C                        including the solution vector in ny+6 entry
C        nb            - the total number of branches
C        indexb(1,ib)  - the connection node of the ib-th branch to
C                        the main branch
C        indexb(2,ib)  - the type of the ib-th branch
C        nadd_array(ib)- the total number of nodes in the branches from 1
C                        to (ib-1)st.
C        topbranch=1   - the indicator of the top branch
C
C output:
C        ep,ep1        - the residuals corresponding ith node in ibth
C                        branch (1st and 2nd equation)
C=======================================================================
C
C Written by Dr. Yury Gryazin, 10/20/2019, ISU, Pocatello, ID
C
C Called by: main_loop.f
C
c     Define passed variables
c

      REAL*8  a(*),a_prl(*),ep,ep1
      INTEGER ib,i,nrx,indexb(2,*),nadd_array(*),nb,topbranch
c
c Define internal variables
c
      INTEGER k,nrow,iadd,nzz,j,ny,ny1,ny2,nyM1,nyM2,
     &        ll,node_num,jadd,iexit_type,jj

c
c First, residual without connenction to the branches.
c
      k    = 2
      nrow = 3*k + 1

      iadd = nadd_array(ib)
      nzz  = nadd_array(ib) * k



        ind = i + iadd
        j   = (i-1)*k + 1 + nzz
        ny  = nrow*(j-1)
        ny1 = ny   + nrow
        ny2 = ny1  + nrow
        nyM1= ny   - nrow
        nyM2= nyM1 - nrow

        if (i.eq.1) then
            ep = a(ny+3)*a_prl(ny + 6)+a(ny+4)*
     &           a_prl(ny1 + 6) - a(ny + 7)
            ep1= a(ny1+3)*a_prl(ny + 6)+
     &           a(ny1+4)*a_prl(ny1 + 6)+
     &           a(ny1+5)*a_prl(ny2 + 6)-a(ny1 + 7)
            else
                if (i.eq.nrx) then
                    ep = a(ny+2)*a_prl(nyM1 + 6)+
     &                   a(ny+3)*a_prl(ny + 6)+
     &                   a(ny+4)*a_prl(ny1 + 6)-a(ny + 7)
                    ep1= a(ny1+4)*a_prl(ny1 + 6)-a(ny1 + 7)
                else
           
                    ep =a(ny+3)*a_prl(ny + 6)+
     &              a(ny+4)*a_prl(ny1 + 6)+
     &              a(ny+5)*a_prl(ny2 + 6)-a(ny + 7)
                    ep1=a(ny1+2)*a_prl(nyM1 + 6)+
     &              a(ny1+3)*a_prl(ny + 6)+
     &              a(ny1+4)*a_prl(ny1 + 6)-a(ny1 + 7)
                endif
        endif

c
c Next, the connenction terms are included in the residuals.
c

        do ll = 1,nb-1
            node_num = indexb(1,ll)
            jadd = nadd_array(ll+1)
            iexit_type = indexb(2,ll)

            jj = k*jadd + 1
            ny = jj*nrow

            if(ind.eq. node_num-1)then
                if(iexit_type.eq.topbranch)then

                    ep1 = ep1 + 0.5*a_prl(ny + 6)
                end if

            end if

            if(ind.eq. node_num)then
                if(iexit_type.eq.topbranch)then
                    ep1 = ep1 - 0.5*a_prl(ny + 6)
                else
                    ep1 = ep1 + 0.5*a_prl(ny + 6)

                end if
            end if

            ny = (node_num-2)*k*nrow
            ny1= ny + k*nrow
            if((ind.eq.jadd+1).and.(ind.ne.1))then
                if(iexit_type.eq.topbranch)then
                    ep = ep - a_prl(ny + 6) + a_prl(ny1 + 6)
                else
                    ep = ep - a_prl(ny1 + 6)
                end if
            end if

        enddo

      RETURN
      END



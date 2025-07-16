      SUBROUTINE PRL_Seq(ib,i,nrx,a,sol,indexb,nadd_array,nb,
     &                         topbranch,ep,ep1)
!$    use omp_lib

C This SUBROUTINE PRL_Res calculate the residual at the given node.

C========================================================================
C input:
C        ib            - the number of the (current) branch
C        i             - the number of the node in the branch
C        nrx           - the total number of nodes in the branch
C        a             - the coefficient matrix in the Screamer format
C        sol           - the solution vector of the sequential algorithm
C        nb            - the total number of branches
C        indexb(1,ib)  - the connection node of the ib-th branch to
C                        the main branch
C        indexb(2,ib)  - the type of the ib-th branch
C        nadd_array(ib)- the total number of nodes in the branches from 1
C                        to (ib-1)st.
C        topbranch=1   - the indicator of the top branch
C
C output:
C        ep,ep1      - the residuals corresponding ith node in ibth
C                        branch
C=======================================================================
C
C Written by Dr. Yury Gryazin, 10/20/2019, ISU, Pocatello, ID
C
C Called by: main_loop.f
C
c     Define passed variables
c

      REAL*8  a(*),sol(*),ep,ep1
      INTEGER ib,i,nrx,indexb(2,*),nadd_array(*),nb,topbranch
c
c Define internal variables
c
      INTEGER k,nrow,iadd,nzz,j,ny,ny1,ny2,nyM1,
     &        ll,node_num,jadd,iexit_type,jj,jj1

c
c First, residual without connenction to the branches.
c
      k    = 2
      nrow = 3*k + 1

      iadd = nadd_array(ib)
      nzz  = nadd_array(ib) * k

        ind = i + iadd
        j   = (i-1)*k + 1 + nzz
        print *, '*ib=',ib,'i=',i,' nzz=',nzz,'ind=',ind,'j=',j

        ny  = nrow*(j-1)
        ny1 = ny   + nrow
        ny2 = ny1  + nrow
        nyM1= ny   - nrow
    

        if (i.eq.1) then
            ep = a(ny+3)*sol(j)+a(ny+4)*
     &           sol(j+1) - a(ny + 7)
            ep1= a(ny1+3)*sol(j)+
     &           a(ny1+4)*sol(j+1)+
     &           a(ny1+5)*sol(j+2)-a(ny1 + 7)
            else
                if (i.eq.nrx) then
                    ep = a(ny+2)*sol(j-1)+
     &                   a(ny+3)*sol(j)+
     &                   a(ny+4)*sol(j+1)-a(ny + 7)
                    ep1= a(ny1+4)*sol(j+1)-a(ny1 + 7)
                else
           
                    ep =a(ny+3)*sol(j)+
     &              a(ny+4)*sol(j+1)+
     &              a(ny+5)*sol(j+2)-a(ny + 7)
                    ep1=a(ny1+2)*sol(j-1)+
     &              a(ny1+3)*sol(j)+
     &              a(ny1+4)*sol(j+1)-a(ny1 + 7)
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

            if(ind.eq. node_num-1)then
                if(iexit_type.eq.topbranch)then

                    ep1 = ep1 + 0.5*sol(jj+1)
                end if

            end if

            if(ind.eq. node_num)then
                if(iexit_type.eq.topbranch)then
                    ep1 = ep1 - 0.5*sol(jj+1)
                else
                    ep1 = ep1 + 0.5*sol(jj+1)

                end if
            end if

            jj = (node_num-2)*k + 1
            jj1= (node_num-1)*k + 1
    
            if((ind.eq.jadd+1).and.(ind.ne.1))then
                if(iexit_type.eq.topbranch)then
C
C There is an error in the sequential algorith: in many situations
C a(ny+3)=1,a(ny+4)=0 and a(ny+7)=0 in the first row of the
C branch so sol(j) - sol(jj) + sol(jj1) =0 but that is not the
C case. And since sol(j) = 15.0000000000 it seems this is a
C rounding error appeared during the cleaning and rounding of
C the Gaussian elimination in the first row of a brance
C after Dr.Gryazin finished work on the solver. If we will need
C the old versition, we will return and fix it !!!!!!!!!!!!
C        print *, 'ib=',ib,'i=',i,a(ny+3),a(ny+4),a(ny + 7)
C        print *, 'ib=',ib,'i=',i,'j=',sol(j)
C        print *, 'jj=',jj,'jj1=',jj1, ep, sol(jj), sol(jj1)

                    ep = ep - sol(jj) + sol(jj1)
                else
                    ep = ep - sol(jj1)
                end if
            end if

        enddo

      RETURN
      END



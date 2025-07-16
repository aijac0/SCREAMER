      subroutine clrarry
c
c Subroutine created to clear all common arrays prior to run
c to prevent potential data from being used from prior run
c
c Modifications
c
c 2018-07-17 RBS: Changed the clear of iin from 1,max_elem_parms to
c                 max_iin_specs as in zdemmax.h the limit for
c                 max_iin_specs is 5 and max_elem_parms is 12. The size
c                 of the iin array is set in zdemcomm.h to max_iin_specs
c 2019-01-28 RBS: add include 'zdemout' and init scale_out
c
c Include the files with the various keywords and integer flags.
c
      include 'zdemparm.h'
c
c Include the files specifying the array dimensions and the common blocks.
c
      include 'zdemmax.h'
      include 'zdemcomm.h'
      include 'zdemwork.h'
      include 'zdemout.h'

      do i=1,max_elem_parms
        do j=1,max_blocks
          do k=1,max_branches
            pin(i,j,k)= 0.0
          end do
        end do
      end do

      do i=1,max_iin_specs
        do j=1,max_blocks
          do k=1,max_branches
            iin(i,j,k)= 0
          end do
        end do
      end do

      do i=1,max_var_parms
        do j=1,max_var_elem
          var_model(i,j)= 0.0
        end do
      end do

      do i=1,max_var_elem
        ivar_type(i) = 0
        num_var_parms(i) = 0
        ivar_block(i) = 0
        ivar_block_num(i) = 0
        num_tablem_vals(i) = 0
      end do

      do i=1,max_init_cond
        value_init(i) = 0.0
      end do

      do i=1,max_volt_source
        ivoltf(i) = 0
        num_voltf_parms(i) = 0
      end do

      do i=1,max_volt_func_parms
        do j=1,max_volt_source
          voltf_parms(i,j) = 0.0
        end do
      end do

      do i=1,max_branches
        ivbranch_end(i) = 0
        nbk(i) = 0
        icbranch_end(i) = 0
        itypcend(i) = 0
      end do

      do i=1,max_curr_source
        icurrf(i) = 0
        num_currf_parms(i) = 0
      end do

      do i=1,max_curr_func_parms
        do j=1,max_curr_source
          currf_parms(i,j) = 0.0
        end do
      end do

      do i=1,max_trline
        itrl_type(i) = 0
      end do

      do i=1,max_mitl
        imitl_type(i) = 0
      end do

      do i=1,max_tablem_vals
        do j=1,max_var_elem
          tablem_vals(i,j) = 0.0
        end do
      end do

      do i=1,max_nodes
        do j=1,max_branches
          v(i,j) = 0.0
          vold(i,j) = 0.0
          vn(i,j) = 0.0
          zir(i,j) = 0.0
          zirn(i,j) = 0.0
          zirold(i,j) = 0.0
          zib(i,j) = 0.0
          g(i,j) = 0.0
          c(i,j) = 0.0
          rr(i,j) = 0.0
          gdot(i,j) = 0.0
          rrdot(i,j) = 0.0
          zlrechk(i,j) = 0.0
          cechk(i,j) = 0.0
          iflg(i,j) = 0
        end do
      end do

      do i=1,max_branches
        vsour(i) = 0.0
        nbe(i) = 0
        nr(i) = 0
        nadd_array(i) = 0
      end do

      do i=1,max_blocks
        do j=1,max_branches
          do k=1,max_uservars
            uservars(i,j,k) = 0.0
          end do
        end do
      end do

      do i=1,2
        do j=1,max_branches
          indexb(i,j) = 0
        end do
      end do 

      do i=1,3
        do j=1,max_var_elem
          indexv(i,j) = 0
        end do
      end do 

c works below
      do i=1,2
        do j=1,max_volt_source
          indexvs(i,j) = 0
        end do
      end do 

      do i=1,2
        do j=1,max_curr_source
          indexcs(i,j) = 0
        end do
      end do 

      do i=1,5
        do j=1,max_mitl
          indexmitl(i,j) = 0
        end do
      end do 

      do i=1,5
        do j=1,max_transformer
          indextrnf(i,j) = 0
        end do
      end do 

      do i = 1,max_volt_source
        lastvoltf_time(i) = 0
      end do 

      do i = 1,max_curr_source
        lastcurrf_time(i) = 0
      end do 

      do i = 1,max_var_elem
        lasttabm_time(i) = 0
      end do 

      do i = 1,maxout
        scale_out(i) = 1.0
      end do

      end subroutine

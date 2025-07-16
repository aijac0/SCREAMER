      subroutine energy_checks
c
c-----Description-------------------------------------------------------
c
c Author/Date: Mathias Bavay  11/04
c      Modifications:
c 2015-06-18 RBS: Moved variables in /misc_energy_checks/ in zdemvars.h
c                 here as they were used nowhere else.
c
c ----------------------------------------------------------------------
c
c Purpose: This subroutines does an energy check at the half time step. 
c     
c Called by: Program ZDEM
c
c    

c Include the files with the various keywords and integer flags.
c
      include 'zdemparm.h'
      include 'zdempprm.h'
c
c Include the files specifying the array dimensions and the common
c blocks.
c
      include 'zdemmax.h'
      include 'zdemcomm.h'
      include 'zdemwork.h'
      include 'zdemout.h'
      include 'zdemenv.h'
      include 'zdemvars.h'
c
c Include file with version string
c
      include 'version.h'

c
c Define internal variables
c

      real
     & decon, deres, dlosscap, dlossind,
     & halfi, halfi2, halfv, halfv2,
     & esource, esum, psource

c ----------------------------------------------------------------------

c
c ----------------------------------------------------------------------
c
c Energy check at the half time step.
c Calculate:
c
c the energy currently stored in the capacitors (ecap),
c the energy currently stored in the inductors (eind),
c the energy dissipated in the shunt conductances this step (decon),
c the energy dissipated in the series resistors this step (deres),
c the total energy dissipated in the shunt conductances (econ),
c the total energy dissipated in the series resistors (eres),
c the energy dissipated by a variable inductor in this step (dlossind),
c the energy dissipated by a variable capacitor in this step (dlosscap),
c the total energy dissipated by variable inductors (elossind),
c the total energy dissipated by variable capacitors (elosscap).
c
        ecap     = 0.0
        eind     = 0.0
        decon    = 0.0
        deres    = 0.0
        dlossind = 0.0
        dlosscap = 0.0
c
c Calculate the energy associated with each circuit g, c, l, r.
c
        do ib = 1, nb
          nrx = nr(ib)
          do i = 1, nrx
            halfi    = 0.5 * (zirn(i,ib) + zir(i,ib))
            halfi2   = halfi * halfi
            halfv    = 0.5 * (vn(i,ib) + v(i,ib))
            halfv2   = halfv * halfv
            eind     = 0.5  * zlrechk(i,ib) * halfi2  +  eind
            ecap     = 0.5  * cechk(i,ib)   * halfv2  +  ecap
            decon    = ht   * g(i,ib)       * halfv2  +  decon
            deres    = ht   * rr(i,ib)      * halfi2  +  deres
            dlossind = htd2 * zlrdot(i,ib)  * halfi2  +  dlossind
            dlosscap = htd2 * cdot(i,ib)    * halfv2  +  dlosscap
          end do
        end do
c
c Find the energy added by sources in the last time step.
c
        psource = 0.0
        do i = 1, nvoltsource
          node    = indexvs(1,i)
          ibranch = indexvs(2,i)
          halfv   = 0.5 * (v(node,ibranch) + vn(node,ibranch))
c
c For a voltsource block.
c
          if (node .eq. 1) then
            halfi   = 0.5 * (zir(node,ibranch) + zirn(node,ibranch))
            psource = psource + (halfv * halfi)
c
c For a vendsource block.
c
          else
            node1   = node - 1
            halfi   = 0.5 * (zir(node1,ibranch)+zirn(node1,ibranch))
            psource = psource - (halfv * halfi)
          end if
        end do
c
        do i = 1, ncurrsource
          node    = indexcs(1,i)
          ibranch = indexcs(2,i)
          halfi   = 0.5 * (zir(node,ibranch) + zirn(node,ibranch))
c
c For a currsource block.
c
          if (node .eq. 1) then
            node2   = node + 1
            halfv   = 0.5 * (v(node2,ibranch)  + vn(node2,ibranch))
            psource = psource + (halfv * halfi)
c
c For a cendsource block.
c
          else
            halfv   = 0.5 * (v(node,ibranch) + vn(node,ibranch))
            psource = psource - (halfv * halfi)
          end if
        end do
c
        esource     = psource * ht
c
c Check total energy from all components against the total
c from all sources.
c
        econ     = econ     + decon
        eres     = eres     + deres
        elossind = elossind + dlossind
        elosscap = elosscap + dlosscap
        esour    = esour    + esource 
        esum     = -esour + eind + ecap + eres + econ
     &             + elossind + elosscap
        error    = esum / (esour+1.0e-4)
c

       return
       end

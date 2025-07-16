      subroutine yield (Z,massm,rfm,Lzm,Kin,eta,T,Yw,Ym)
c
c-------Description--------------------------------------------------
c
c Source File : yield.f
c
c Author/Date : Ken Struve, 10/97
c Modifications
c
c 2015-06-23 RBS: Fixed up complex math
c
c
c Purpose     :
c
c     This routine is meant for post-processing for the K-line yield
c     from Jupiter circuit models
c
c     David Mosher, NRL PRS Scoping Team
c
c     This note documents the fortran subroutine YIELD that calculates the 
c     K-line yield from a PRS using both the Mosher-Krisnan-Qi 2-level model
c     and the Whitney model as modified by John Giuliani.  It is designed
c     to post process radiation yields from the output of circuit models
c     coupled to a slug model for the PRS.  Input parameters for the 
c     subroutine are the real, single-precision numbers
c 
c        Z = atomic number
c        massm = load mass in kg (line mass x length)
c        rfm = final stagnation radius in m
c        Lzm = load length in m
c        Kin = imploded kinetic energy in J
c
c     Output parameters are
c
c        eta = Whitney's eta parameter
c        T = temperature in eV from energy balance assuming 40% of the
c            kinetic energy is used in processes other than internal energy
c            and K-line radiation
c        Yw = yield in J from the Whitney-Guiliani model
c        Ym = yield in J from the 2-level model
c
c Called by   : subroutine cylfoil_model
c
c Calls       : none
c
c
c     Implemented into SCREAMER 1/3/94 by Ken Struve (MRC) at SNL
c
      implicit none
c
c Define passed variables
c
      real Z, massm,rfm, Lzm, Kin, eta, T, Yw, Ym
c
c Define internal variables
c
      integer it
      real m, rf, Kt, mass, Lz , CEi
      real Ctau,Ei,Emin,K,S,T0,T1,V,Y,Yeff,Yineff,Yt,a0,az,dif,etaL,
     &     hv,tau
      double precision CY, Ni
c
c-------Subroutine Body-------------------------------------------------
c
      if (Kin.lt.1.) then
         return
      endif
c
c     Convert inputs to cgs
c     
      mass=massm*1000.0
      rf=rfm*100.0
      Lz=Lzm*100.0
c
      hv=10.2*Z**2                               !eV K-line energy
      m=mass/Lz                                  !g/cm line mass
      Ni = 3.8D23 * dble(m / Z**1.1)             !ions/cm line density
      Kt=Kin/Lz                                  !J/cm KE/length


      K=Kt*0.5                                   !J/cm avail. 2-level
c
c     ***  Start 2-level algorithm ***
c
      CEi = 4.7e-19 * Z**0.82 * real(Ni)               !J/cm-eV
      CY = 2.7d-31 * dble(Z) * Ni**2 / dble(rf)        !J-eV/cm
      Ctau=5.5e-12 * real(Ni) / (rf**1.67 * Z**3.83)   !eV^1/2
      T0=K/CEi                                   !eV temperature for Y=0
      T1=0.                                      !eV lower limit
      T=0.75*T0                                  !ev 1st iteration
c
c     ***  Iterate temperature ***
c
      do 6 it=1,25
      Ei=CEi*T                                   !J/cm internal energy
      dif=K-Ei                                   !J/cm
      Yt = real(CY) * exp(-hv/T)/T               !J/cm thin yield
      tau=Ctau/sqrt(T)                           !optical depth
      V=9.47e-7*tau**1.5
      S=1./(1.+V**0.84)**1.19                    !escape fraction
      Y=S*Yt                                     !J/cm radiated
      if(abs(Y-dif).le..01*Y) go to 3            !energy balance OK
      if(Y.gt.dif) go to 4                       !too much radiation
      if(Y.lt.dif) go to 5                       !too little radiation
 4    T0=T
      T=0.5*(T+T1)
      go to 6
 5    T1=T
      T=0.5*(T+T0)
 6    continue
 3    Ym=Lz*Y                                    !J radiated 2-level
c
c     ***  Start Whitney-Giuliani algorithm  ***
c
      Emin=1.49*Z**3.51                          !eV/ion
      az=Z**(-3.55)*exp(-20.6/Z**0.9)
      eta = Kt / (1.6e-19 * Emin * real(Ni))
      a0=4.5e11/eta**0.667                       !J-cm/mg^2
      etaL=1.
      if(eta.lt.3.) etaL=0.2*(2.*eta-1.)
      if(eta.le.0.5) etaL=0.
      Yeff=etaL*0.3*Kt                           !J/cm efficient
      Yineff=etaL*a0*az*(0.05/rf)*(1.e3*m)**2    !J/cm inefficient
      Yw=Lz*MIN(Yeff,Yineff)                   !J radiated Whitney
c
c
      return
      end

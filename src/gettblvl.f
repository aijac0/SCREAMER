      subroutine get_tablem_value (time, tabnum, resistance, slope)
c
c-------Description--------------------------------------------------
c
c Source File: gettblvl.f
c
c Author/Date:  Kelley Fugelso, 1265 (SEA)   04/89
c  2014-02-06 RBS: Changed real*4 to real
c  2014-03-24 RBS: Added the slope as a passed value.
c  2014-04-04 RBS: Added real definition for slope
c  2014-05-02 RBS: Changed integer*4 to integer
c
c Purpose: This subroutine determines the resistance (nductance) based
c          on the time and variable resistance (inductance) table model
c          number. The resistance (inductance) is calculated by
c          interpolating using time as the independent variable.
c
c Called by: Program zdem
c
c Calls:  Function CALC_RESISTANCE
c
c-------Include Files---------------------------------------------------
c
      use zdemmax
      use zdemwork
      include 'zdemparm.h'
      include 'zdemcomm.h'
c
c-------Input Parameters------------------------------------------------
c                                                                     */
      real       time    ! Current time of problem                    */
      integer    tabnum  ! Variable resistor table model number       */
c                                                                     */
c-------Output Parameters---------------------------------------------*/
c                                                                     */
      real       resistance,  ! Interpolated resistance               */
     &           slope        ! Interpolated slope                    */
c                                                                     */
c-------Define Internal Variables-------------------------------------*/
c                                                                     */
      real       scale,      ! Scale factor                           */
     +           delay       ! Time delay                             */
      integer    num_tpoints ! Number of points in table              */
c                                                                     */
c-------Subroutine Body-----------------------------------------------*/
c
      scale   = tablem_vals(1,tabnum)
      delay   = tablem_vals(2,tabnum)
      num_tpoints = num_tablem_vals(tabnum) - 2
      resistance = calc_resistance (time-delay, num_tpoints,
     &             tablem_vals(3,tabnum), lasttabm_time(tabnum))
      resistance = resistance * scale
      slope = calc_slope (time-delay, num_tpoints,
     &             tablem_vals(3,tabnum), lasttabm_time(tabnum))
      slope = slope * scale

c
c-------Subroutine End--------------------------------------------------
c
      return
      end

c      include 'micromet.inc'
C     Last change:  VS   30 Jul 1999   11:44 am
      ! ignore this - for simple testing purposes only
      subroutine test()
      dll_export test
      pause 'test'
      return
      end
*====================================================================
      real function micromet_PenmanMonteith (
     :               latitude
     :              ,day
     :              ,timestep
     :              ,SunshineHrs
     :              ,radn
     :              ,albedo
     :              ,EmmisCanopy
     :              ,mint
     :              ,maxt
     :              ,vp
     :              ,AirPressure
     :              ,AerodynamicCond
     :              ,CanopyCond)
*====================================================================
      Use MicrometModule
      implicit none
      dll_export micromet_PenmanMonteith
      include   'data.pub'                         
      include   'science.pub'
      include   'error.pub'                         

*+  Sub-Program Arguments
      real    latitude            !decimal degrees
      integer day                 !day of year
      real    albedo              !of the crop
      real    radn                !MJ/m2
      real    mint                !degC
      real    maxt                !degC
      real    vp                  !humidity in hPa
      real    AerodynamicCond     !m/s
      real    CanopyCond          !m/s
      real    SunshineHrs         !length of bright sunshine
      REAL    EmmisCanopy         !emmisivity of the canopy -
      REAL    AirPressure         !hPa
      REAL    timestep            !minutes

*+  Purpose
*     Calculate the Penman-Monteith water demand

*+  Notes

*+  Changes
*       050799 - VOS specified and programmed

*+  Calls
      real       micromet_Non_dQs_dT       ! function
      dll_import micromet_Non_dQs_dT

      real       micromet_LongWave       ! function
      dll_import micromet_LongWave

      real       micromet_RhoA       ! function
      dll_import micromet_RhoA

      real       micromet_Lambda       ! function
      dll_import micromet_Lambda

      real       micromet_SpecificHumidity          ! function
      dll_import micromet_SpecificHumidity

      real       micromet_SatSpecificHumidity          ! function
      dll_import micromet_SatSpecificHumidity

      real       micromet_Radn2SolRad          ! function
      dll_import micromet_Radn2SolRad

      real       micromet_SpecificVPD          ! function
      dll_import micromet_SpecificVPD

*+  Local Variables
      REAL Non_dQs_dT
      REAL SolRad               !short wave radiation W/m2
      REAL LongWave             !W/m2
      REAL NetRadiation
      REAL RhoA
      REAL Lambda
      REAL SpecificVPD
      REAL RadiationTerm
      REAL AerodynamicTerm
      real DayLength           !length of day (hours)
      REAL denominator         !of the Penman-Monteith equation

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_PenmanMonteith')

*- Implementation Section ----------------------------------
 
      call push_routine (myname)

      if (NINT(timestep/60.0) .eq. 24 ) then
         DayLength = day_length (day,latitude, -6.0)
      else
         DayLength = timestep/60.0   !convert from min to hours
      end if

      Non_dQs_dT = micromet_Non_dQs_dT ((mint+maxt )/2.0 ,AirPressure)
      SolRad = micromet_Radn2SolRad (
     :               latitude
     :              ,day
     :              ,timestep
     :              ,radn)
      LongWave = micromet_longwave (   (mint+maxt )/2.0
     :                                ,SunshineHrs/DayLength
     :                                ,EmmisCanopy)
      NetRadiation = (1-Albedo) * SolRad + LongWave

      RhoA = micromet_RhoA ((mint + maxt)/2.0, AirPressure)
      Lambda = micromet_Lambda((mint + maxt)/2.0)
      SpecificVPD = micromet_SpecificVPD (
     :                                     vp
     :                                   , mint
     :                                   , maxt
     :                                   , AirPressure)

      Denominator = Non_dQs_dT
     :            + Divide( AerodynamicCond, CanopyCond, 0.0)
     :            + 1.0

      RadiationTerm =
     :          Divide( Non_dQs_dT * NetRadiation
     :                , Denominator, 0.0)
     :                * 1000.0 * (DayLength *3600.0)
     :                / Lambda / RhoW
      AerodynamicTerm =
     :          Divide( RhoA * Lambda* SpecificVPD * AerodynamicCond
     :                , Denominator, 0.0)
     :                * 1000.0 * (DayLength *3600.0)
     :                / Lambda / RhoW

      micromet_PenmanMonteith = RadiationTerm + AerodynamicTerm

      call pop_routine (myname)

      return
      end

*====================================================================
      real function micromet_Omega (
     :               mint
     :              ,maxt
     :              ,AirPressure
     :              ,AerodynamicCond
     :              ,CanopyCond)
*====================================================================
      Use MicrometModule
      implicit none
      dll_export micromet_Omega
      include   'data.pub'                         
      include   'science.pub'
      include   'error.pub'                         

*+  Sub-Program Arguments
      real    mint                !degC
      real    maxt                !degC
      real    AerodynamicCond     !m/s
      real    CanopyCond          !m/s
      REAL    AirPressure         !hPa

*+  Purpose
*     Calculate the Jarvis & McNaughton decoupling coefficient, omega

*+  Notes

*+  Changes
*       050799 - VOS specified and programmed

*+  Calls
      real       micromet_Non_dQs_dT       ! function
      dll_import micromet_Non_dQs_dT

*+  Local Variables
      REAL Non_dQs_dT

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_Omega')

*- Implementation Section ----------------------------------
 
      call push_routine (myname)

      Non_dQs_dT = micromet_Non_dQs_dT ((mint+maxt )/2.0 ,AirPressure)

      micromet_Omega =
     :   Divide( Non_dQs_dT + 1.0
     :         , Non_dQs_dT + 1.0 + Divide( AerodynamicCond
     :                                    , CanopyCond
     :                                    , 0.0)
     :         , 0.0)

      call pop_routine (myname)

      return
      end
*====================================================================
      real function micromet_ActualCanopyCond (
     :               latitude
     :              ,day
     :              ,timestep
     :              ,SunshineHrs
     :              ,radn
     :              ,albedo
     :              ,EmmisCanopy
     :              ,mint
     :              ,maxt
     :              ,vp
     :              ,AirPressure
     :              ,AerodynamicCond
     :              ,Transpiration)
*====================================================================
      Use MicrometModule
      implicit none
      dll_export micromet_ActualCanopyCond
      include   'data.pub'                         
      include   'science.pub'
      include   'error.pub'                         

*+  Sub-Program Arguments
      real    latitude            !decimal degrees
      integer day                 !day of year
      real    albedo              !of the crop
      real    radn                !MJ/m2
      real    mint                !degC
      real    maxt                !degC
      real    vp                  !humidity in hPa
      real    AerodynamicCond     !m/s
      real    Transpiration       !mm
      real    SunshineHrs         !length of bright sunshine
      REAL    EmmisCanopy         !emmisivity of the canopy -
      REAL    AirPressure         !hPa
      REAL    timestep            !minutes

*+  Purpose
*     given the actual water use, calculate the
*     effective canopy conductance

*+  Notes

*+  Changes
*       050799 - VOS specified and programmed

*+  Calls
      real       micromet_Non_dQs_dT       ! function
      dll_import micromet_Non_dQs_dT

      real       micromet_LongWave       ! function
      dll_import micromet_LongWave

      real       micromet_RhoA       ! function
      dll_import micromet_RhoA

      real       micromet_Lambda       ! function
      dll_import micromet_Lambda

      real       micromet_SpecificHumidity          ! function
      dll_import micromet_SpecificHumidity

      real       micromet_SatSpecificHumidity          ! function
      dll_import micromet_SatSpecificHumidity

      real       micromet_Radn2SolRad          ! function
      dll_import micromet_Radn2SolRad

      real       micromet_SpecificVPD          ! function
      dll_import micromet_SpecificVPD

*+  Local Variables
      REAL Non_dQs_dT
      REAL SolRad               !short wave radiation W/m2
      REAL LongWave             !W/m2
      REAL NetRadiation
      REAL RhoA
      REAL Lambda
      REAL SpecificVPD
      real DayLength           !length of day (hours)
      REAL denominator         !of the Penman-Monteith equation
      real LambdaE             !transpiration time specific heat

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_ActualCanopyCond')

*- Implementation Section ----------------------------------
 
      call push_routine (myname)

      if (NINT(timestep/60.0) .eq. 24 ) then
         DayLength = day_length (day,latitude, -6.0)
      else
         DayLength = timestep/60.0   !convert from min to hours
      end if

      Non_dQs_dT = micromet_Non_dQs_dT ((mint+maxt )/2.0 ,AirPressure)
      SolRad = micromet_Radn2SolRad (
     :               latitude
     :              ,day
     :              ,timestep
     :              ,radn)
      LongWave = micromet_longwave (   (mint+maxt )/2.0
     :                                ,SunshineHrs/DayLength
     :                                ,EmmisCanopy)
      NetRadiation = (1-Albedo) * SolRad + LongWave

      RhoA = micromet_RhoA ((mint + maxt)/2.0, AirPressure)
      Lambda = micromet_Lambda((mint + maxt)/2.0)
      SpecificVPD = micromet_SpecificVPD (
     :                                     vp
     :                                   , mint
     :                                   , maxt
     :                                   , AirPressure)

      LambdaE = Transpiration
     :                * 1000.0 * (DayLength *3600.0)
     :                / Lambda / RhoW

      Denominator = Non_dQs_dT * NetRadiation
     :            + Lambda * RhoA * SpecificVPD * AerodynamicCond
     :            - LambdaE * Non_dQs_dT
     :            - LambdaE


      micromet_ActualCanopyCond =
     :          Divide( AerodynamicCond * LambdaE
     :                , Denominator
     :                , 0.0)

      call pop_routine (myname)


      return
      end
*====================================================================
      real function micromet_RhoA (temperature
     :                            ,AirPressure)
*====================================================================
      Use MicrometModule
      implicit none
      dll_export micromet_RhoA
      include 'error.pub'                         
      include 'data.pub'

*+  Sub-Program Arguments
      real       temperature           ! (INPUT) temperature (oC)
      real       AirPressure           ! (INPUT) air pressure (hPa)

*+  Purpose
*     calculate the density of air (kg/m3) at a given temperature
*     and pressure

*+  Changes
*       291098 - NIH adapted from grandis module
*       020799 - VOS checked units

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_RhoA')

*- Implementation Section ----------------------------------
 
      call push_routine (myname)

      micromet_RhoA = divide(
     :                 mwair 
     :              *  AirPressure * 100.  !air pressure converted to Pa
     :              , (abs_temp + temperature)
     :              * r_gas
     :              , 0.0)
 
      call pop_routine (myname)
      return
      end

*====================================================================
      real function micromet_Lambda (temperature)
*====================================================================
      implicit none
      dll_export micromet_Lambda
      include 'error.pub'                         

*+  Sub-Program Arguments
      real temperature   ! temperature (oC)

*+  Purpose
*     calculate the lambda (latent heat of vapourisation for water)(J/kg)

*+  Changes
*       291098 - NIH Adapted from Eo module

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_lambda')

*- Implementation Section ----------------------------------
 
      call push_routine (myname)
 
      micromet_lambda = (2501.0 - 2.38 * temperature)*1000.0      ! J/kg
 
      call pop_routine (myname)
      return
      end

*====================================================================
      real function micromet_svp (temperature)
*====================================================================
      Use MicrometModule
      implicit none
      dll_export micromet_svp
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       temperature           ! (INPUT) temperature (oC)

*+  Purpose
*     calculate the saturated vapour pressure for a given temperature

*+  Changes
*       291098 - NIH adapted from Eo module

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_svp')

*- Implementation Section ----------------------------------
 
      call push_routine (myname)
 
      micromet_svp = svp_A*exp(svp_B*temperature/(temperature + svp_C))
 
      call pop_routine (myname)
      return
      end

*====================================================================
      real function micromet_Non_dQs_dT    (temperature
     :                                     ,air_pressure)
*====================================================================
      Use MicrometModule
      implicit none
      dll_export micromet_Non_dQs_dT
      include 'error.pub'                         

*+  Sub-Program Arguments
      real       temperature    ! (INPUT) minimum temperature (oC)
      real       air_pressure   ! (INPUT) air pressure (hPa)

*+  Purpose
*     calculate Non_dQs_dT - the dimensionless value for
*     d(sat spec humidity)/dT ((kg/kg)/K) FROM TETEN FORMULA

*+  Changes
*       291098 - NIH adapted from Eo module

*+  Calls
      real       micromet_lambda       ! function
      dll_import micromet_Lambda
      real       micromet_svp          ! function
      dll_import micromet_svp

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_Non_dQs_dT')

*+  Local Variables
      real       desdt                 ! d(sat VP)/dT: (mb/K)
      real       dqsdt                 ! d(sat spec hum)/dT: (kg/kg)/K
      real       esat                  ! saturated vapour pressure (mb)

*- Implementation Section ----------------------------------
 
      call push_routine (myname)
 
      esat = micromet_svp (temperature)  !saturated vapour pressure
 
      desdt = esat*svp_B*svp_C/ (svp_C + temperature)**2   ! d(sat VP)/dT: (mb/K)
      dqsdt = (mwh2o/mwair) *desdt/air_pressure            ! d(sat spec hum)/dT: (kg/kg)/K

      micromet_Non_dQs_dT = micromet_lambda (temperature)/Cp *dqsdt    ! dimensionless
 
      call pop_routine (myname)
      return
      end

*====================================================================
      real function micromet_longwave (temperature
     :                                ,FracClearSkyRad
     :                                ,emmis_canopy)
*====================================================================
      Use MicrometModule
      implicit none
      dll_export micromet_LongWave
      include 'error.pub'                         
      include 'data.pub'

*+  Sub-Program Arguments
      real temperature    ! temperature (oC)
      real FracClearSkyRad ! R/Ro, SunshineHrs/DayLength (0-1)
      real emmis_canopy   ! canopy emmissivity

*+  Purpose
*     calculate the net longwave radiation 'in' (W/m2)

*+  Notes
*   Emissivity of the sky comes from Swinbank, W.C. (1963).
*   Longwave radiation from clear skies Quart. J. Roy. Meteorol.
*   Soc. 89, 339-348.

*+  Changes
*       291098 - NIH Adapted from Grandis module
*       050799 - VOS Changed sign so that is net INWARDS longwave
*       060799 - VOS Changed arguments from sunhine hours and daylength
*           to FracClearSkyRadn for compatability with variable timestep


*+  Local Variables
      real emmis_sky    ! emmisivity of the sky
      real cloud_effect ! cloud effect on net long wave (0-1)

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_longwave')

*- Implementation Section ----------------------------------
 
      call push_routine (myname)

      emmis_sky = 9.37e-6*(temperature+abs_temp)**2
      FracClearSkyRad = bound(FracClearSkyRad, 0.0, 1.0)
      cloud_effect = (c_cloud + (1.0-c_cloud)*FracClearSkyRad)

      micromet_longwave = cloud_effect     
     :                  * (emmis_canopy - emmis_sky)
     :                  * stef_boltz*(temperature+abs_temp)**4
   
      call pop_routine (myname)
      return
      end
*====================================================================
      real function micromet_DayLength      (latitude
     :                                      ,day)
*====================================================================
      Use MicrometModule
      implicit none
      dll_export  micromet_DayLength
      include   'error.pub'                         
      include   'science.pub'

*+  Sub-Program Arguments
      real    latitude
      integer day       !day of year

*+  Purpose
*     calculate the day length from latitude, and day of year
*     - only usedfor Excel

*+  Notes

*+  Changes
*       050799 - VOS specified and programmed

*+  Local Variables

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_DayLength')

*- Implementation Section ----------------------------------
 
      call push_routine (myname)

      micromet_DayLength = day_length (day,latitude, -6.0)
   
      call pop_routine (myname)

      return
      end
*====================================================================
      real function micromet_sunshine_hours (radn
     :                                      ,latitude
     :                                      ,day)
*====================================================================
      Use MicrometModule
      implicit none
      dll_export micromet_sunshine_hours
      include   'error.pub'                         
      include   'science.pub'

*+  Sub-Program Arguments
      real    radn      !shortwave radiation from met file
      real    latitude
      integer day       !day of year

*+  Purpose
*     calculate the number of sunshine hours from shortwave radiation
*     latitude, and day of year

*+  Notes
*     Assume that sunshine hours is the day length times measured
*     shortwave radiation divided by maximum possible shortwave 
*     radiation.  Scheme for max. possible radiation is taken from:
*     Smith, M. R., Allen, R. G., Monteith, J. L., Perrier, A., 
*             Satos Pereira, L., and Segeren, A. (1992). Expert 
*             consultation on revision of FAO methodologies for 
*             crop water requirements. Land and Water Division, 
*             Food and Agriculture Organization of the United 
*             Nations, Rome.
*     and
*     Grayson, R. B., Argent, R. M., Nathan, R. J., 
*             McMahon, T. A., and Mein, R. G. (1996). 'Hydrological 
*             Recipes: Estimation Techniques in Australian Hydrology.' 
*             (Cooperative Research Center for Catchment Hydrology: 
*             Clayton, Victoria, Australia.)

*+  Changes
*       020799 - VOS specified and programmed

*+  Local Variables
      real MaxSunHrs           !maximum possible number of sunshine hours (i.e. daylength)
      real RelativeDistance    !relative distance between sun and earth
      real SolarDeclination
      real SunsetAngle
      real ExtraTerrestrialRadn 
      real MaxRadn             !maximum possible shortwave radiation

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_sunshine_hours')

*- Implementation Section ----------------------------------
 
      call push_routine (myname)

      MaxSunHrs = day_length (day,latitude, -6.0)
      
      RelativeDistance = 1.0 + 0.033 * cos(0.0172 * day)

      SolarDeclination = 0.409 * sin(0.0172 * day - 1.39)

      SunsetAngle = acos(-tan(latitude * Deg2Rad)
     :                   *tan(SolarDeclination))

      ExtraTerrestrialRadn = 37.6 * RelativeDistance * 
     :        ( SunsetAngle 
     :        * sin(latitude * Deg2Rad) 
     :        * sin(SolarDeclination)
     :        + 
     :          cos(latitude * Deg2Rad) 
     :        * cos(SolarDeclination) 
     :        * sin(SunsetAngle))

      MaxRadn = 0.75 * ExtraTerrestrialRadn


!finally calculate the sunshine hours as the ratio of 
!maximum possible radiation
      micromet_sunshine_hours = min(MaxSunHrs * Radn / MaxRadn,
     :                              MaxSunHrs)
   
      call pop_routine (myname)

      return
      end
*====================================================================
      real function micromet_SpecificHumidity (vp
     :                                        ,AirPressure)
*====================================================================
      Use MicrometModule
      implicit none
      dll_export micromet_SpecificHumidity
      include   'error.pub'                         

*+  Sub-Program Arguments
      real vp              !vapour pressure in hPa (=mbar)
      REAL AirPressure     !hPa

*+  Purpose
*     calculate specific humidity from vapour pressure

*+  Notes

*+  Changes
*       050799 - VOS specified and programmed

*+  Local Variables

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_SpecificHumidity')

*- Implementation Section ----------------------------------
 
      call push_routine (myname)

      micromet_SpecificHumidity = (mwh2o/mwair) *vp /AirPressure
   
      call pop_routine (myname)

      return
      end
*====================================================================
      real function micromet_SatSpecificHumidity (mint
     :                                           ,maxt
     :                                           ,AirPressure)
*====================================================================
      Use MicrometModule
      implicit none
      dll_export micromet_SatSpecificHumidity
      include   'error.pub'                         

*+  Sub-Program Arguments
      real mint            !degC
      real maxt            !degC
      REAL AirPressure     !hPa

*+  Purpose
*     calculate saturated specific humidity from temperature

*+  Notes

*+  Changes
*       050799 - VOS specified and programmed

*+  Calls
      real       micromet_svp          ! function
      dll_import micromet_svp

*+  Local Variables
      REAL SatVP        !saturated vapour pressure in hPa

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_SatSpecificHumidity')

*- Implementation Section ----------------------------------
 
      call push_routine (myname)

      SatVP = (micromet_svp(mint)+micromet_svp(maxt))/ 2.0

      micromet_SatSpecificHumidity = (mwh2o/mwair) / AirPressure
     :                             * SatVP
   
      call pop_routine (myname)

      return
      end
*====================================================================
      real function micromet_FrictionVelocity (
     :                         WindSpeed
     :                       , MeasurementHeight
     :                       , ZeroPlaneDispl
     :                       , RoughnessLength)
*====================================================================
      Use MicrometModule
      implicit none
      dll_export micromet_FrictionVelocity
      include   'error.pub'                         
      include   'data.pub'

*+  Sub-Program Arguments
      real Windspeed          !m/s
      REAL MeasurementHeight  !m
      REAL ZeroPlaneDispl     !m
      REAL RoughnessLength    !m

*+  Purpose
*     Calculate the friction velocity

*+  Notes

*+  Changes
*       060599 - VOS specified and programmed

*+  Calls

*+  Local Variables
      REAL denominator    !intermediation variable (-)

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_FrictionVelocity')

      REAL       von_karman
      parameter (von_karman = 0.41)
*- Implementation Section ----------------------------------
 
      call push_routine (myname)

      denominator =
     :      divide( MeasurementHeight - ZeroPlaneDispl
     :            , RoughnessLength
     :            , 0.0)
      if (denominator .lt. 1.0 ) then
         micromet_FrictionVelocity = 0.0
      else
         micromet_FrictionVelocity = von_karman * Windspeed
     :                             / log(denominator)  !natural log
      end if

      call pop_routine (myname)

      return
      end
*====================================================================
      real function micromet_ZeroPlaneDispl (CropHeight, CropLAI)
*====================================================================
      Use MicrometModule
      implicit none
      dll_export micromet_ZeroPlaneDispl
      include   'error.pub'                         
      include   'data.pub'

*+  Sub-Program Arguments
      real CropHeight          !m
      REAL CropLAI             !m

*+  Purpose
*     calculate the zero-plane displacement height

*+  Notes

*+  Changes
*       060599 - VOS specified and programmed

*+  Calls

*+  Local Variables

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_ZeroPlaneDispl')

*- Implementation Section ----------------------------------
 
      call push_routine (myname)

      if (CropLAI .lt. 0.001) then
         micromet_ZeroPlaneDispl = 0.0
      else
         micromet_ZeroPlaneDispl =
     :                    l_bound( 0.74 * CropHeight
     :                           + 0.1 * CropHeight * LOG10(CropLAI)
     :                           , 0.0)
      end if

      call pop_routine (myname)

      return
      end
*====================================================================
      real function micromet_RoughnessLength (CropHeight,ZeroPlaneDispl)
*====================================================================
      Use MicrometModule
      implicit none
      dll_export micromet_RoughnessLength
      include   'error.pub'                         
      include   'data.pub'

*+  Sub-Program Arguments
      real CropHeight          !m
      REAL ZeroPlaneDispl      !m

*+  Purpose
*     calculate the roughness length of the crop

*+  Notes

*+  Changes
*       060599 - VOS specified and programmed

*+  Calls

*+  Local Variables

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_RoughnessLength')

*- Implementation Section ----------------------------------
 
      call push_routine (myname)

      micromet_RoughnessLength = 0.36 * CropHeight *
     :                 (1 - divide(ZeroPlaneDispl, CropHeight, 0.0))

      call pop_routine (myname)

      return
      end
*====================================================================
      real function micromet_AerodynamicConductance (
     :                         WindSpeed
     :                       , MeasurementHeight
     :                       , CropHeight
     :                       , CropLAI)
*====================================================================
      Use MicrometModule
      implicit none
      dll_export micromet_AerodynamicConductance
      include   'error.pub'                         
      include   'data.pub'

*+  Sub-Program Arguments
      real Windspeed           !m/s
      REAL MeasurementHeight   !m
      real CropHeight          !m
      REAL CropLAI             !m

*+  Purpose
*     Calculate the Aerodynamic Conductance

*+  Notes

*+  Changes
*       060599 - VOS specified and programmed

*+  Calls
      real       micromet_ZeroPlaneDispl       ! function
      dll_import micromet_ZeroPlaneDispl

      real       micromet_RoughnessLength       ! function
      dll_import micromet_RoughnessLength

      real       micromet_FrictionVelocity       ! function
      dll_import micromet_FrictionVelocity

*+  Local Variables
      REAL ZeroPlaneDispl       !zero-plance displacement (m)
      REAL RoughnessLength      !roughness length (m)
      real FrictionVelocity     !u_star (m/s)

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_AerodynamicConductance')

*- Implementation Section ----------------------------------
 
      call push_routine (myname)

      ZeroPlaneDispl = micromet_ZeroPlaneDispl(CropHeight, CropLAI)
      RoughnessLength = micromet_RoughnessLength(CropHeight,
     :                                         ZeroPlaneDispl)
      FrictionVelocity =
     :            micromet_FrictionVelocity (WindSpeed
     :                                     , MeasurementHeight
     :                                       , ZeroPlaneDispl
     :                                   , RoughnessLength)
      micromet_AerodynamicConductance =
     :        divide( FrictionVelocity**2, WindSpeed, 0.0)

      micromet_AerodynamicConductance =
     :        l_bound(micromet_AerodynamicConductance, 0.001)

      call pop_routine (myname)

      return
      end
*====================================================================
      real function micromet_CanopyConductance (CropGsMax
     :                                        , CropR50
     :                                        , CropRGfac
     :                                        , CropLAIfac
     :                                        , LayerK
     :                                        , LayerLAI
     :                                        , LayerSolRad)
*====================================================================
      Use MicrometModule
      implicit none
      dll_export micromet_CanopyConductance
      include   'error.pub'                         
      include   'data.pub'

*+  Sub-Program Arguments
      real CropGsMax        !crop-specific maximum stomatal conductance (m/s)
      real CropR50          !crop-specific SolRad at which stomatal conductance decreases to 50% (W/m2)
      real CropRGfac        !crop-specific relative growth stress factor (0-1)
      real CropLAIfac       !crop-specific LAI fraction of total LAI in current layer (0-1)
      real LayerK           !layer-averaged light extinction coeficient (-)
      real LayerLAI         !LAI within the current layer (m2/m2)
      real LayerSolRad      !solar radiation arriving at the top of the current layer(W/m2)

*+  Purpose
*       calculate the crop canpopy conductance

*+  Notes

*+  Changes
*       060599 - VOS specified and programmed

*+  Calls

*+  Local Variables
      real Numerator    !intermediate variable
      real Denominator  !intermediate variable
      real Hyperbolic   !intermediate variable

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_CanopyConductance')

*- Implementation Section ----------------------------------
 
      call push_routine (myname)

      Numerator   =   LayerSolRad 
     :            + CropR50
      Denominator = LayerSolRad 
     :            * exp(-1 
     :                  * LayerK
     :                  * LayerLAI)
     :            + CropR50
      Hyperbolic = divide(Numerator, Denominator, 0.0)
      Hyperbolic = L_Bound(Hyperbolic, 1.0)

      micromet_CanopyConductance = 
     :       divide (CropGsMax * CropRGfac * CropLAIfac
     :              ,LayerK
     :              ,0.0)
     :       * log(Hyperbolic)  !natural log

      micromet_CanopyConductance =
     :        l_bound(micromet_CanopyConductance, 0.0001)

      call pop_routine (myname)

      return
      end
*====================================================================
      real function micromet_Radn2SolRad (
     :               latitude
     :              ,day
     :              ,timestep
     :              ,radn)
*====================================================================
      Use MicrometModule
      implicit none
      dll_export micromet_Radn2SolRad
      include   'data.pub'                         
      include   'science.pub'
      include   'error.pub'                         

*+  Sub-Program Arguments
      real    latitude            !decimal degrees
      integer day                 !day of year
      REAL    timestep            !minutes
      real    radn                !MJ/m2

*+  Purpose
*     Convert "radn" in MJ/m2/period to solar radiation in W/m2

*+  Notes

*+  Changes
*       050799 - VOS specified and programmed

*+  Calls

*+  Local Variables
      REAL DayLength

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_Radn2SolRad')


*- Implementation Section ----------------------------------
 
      call push_routine (myname)

      if (NINT(timestep/60.0) .eq. 24 ) then
         DayLength = day_length (day,latitude, -6.0)
      else
         DayLength = timestep/60.0   !convert from min to hours
      end if

      micromet_Radn2SolRad = radn * 10**6 / (DayLength *3600.0)    !W/m2

      call pop_routine (myname)

      return
      end
*====================================================================
      real function micromet_PM_PropRad (
     :               latitude
     :              ,day
     :              ,timestep
     :              ,SunshineHrs
     :              ,radn
     :              ,albedo
     :              ,EmmisCanopy
     :              ,mint
     :              ,maxt
     :              ,vp
     :              ,AirPressure
     :              ,AerodynamicCond
     :              ,CanopyCond)
*====================================================================
      Use MicrometModule
      implicit none
      dll_export micromet_PM_PropRad
      include   'data.pub'                         
      include   'science.pub'
      include   'error.pub'                         

*+  Sub-Program Arguments
      real    latitude            !decimal degrees
      integer day                 !day of year
      real    albedo              !of the crop
      real    radn                !MJ/m2
      real    mint                !degC
      real    maxt                !degC
      real    vp                  !humidity in hPa
      real    AerodynamicCond     !m/s
      real    CanopyCond          !m/s
      real    SunshineHrs         !length of bright sunshine
      REAL    EmmisCanopy         !emmisivity of the canopy -
      REAL    AirPressure         !hPa
      REAL    timestep            !minutes

*+  Purpose
*     Calculate the Penman-Monteith water demand

*+  Notes

*+  Changes
*       050799 - VOS specified and programmed

*+  Calls
      real       micromet_Non_dQs_dT       ! function
      dll_import micromet_Non_dQs_dT

      real       micromet_LongWave       ! function
      dll_import micromet_LongWave

      real       micromet_RhoA       ! function
      dll_import micromet_RhoA

      real       micromet_Lambda       ! function
      dll_import micromet_Lambda

      real       micromet_SpecificHumidity          ! function
      dll_import micromet_SpecificHumidity

      real       micromet_SatSpecificHumidity          ! function
      dll_import micromet_SatSpecificHumidity

      real       micromet_Radn2SolRad          ! function
      dll_import micromet_Radn2SolRad

      real       micromet_SpecificVPD          ! function
      dll_import micromet_SpecificVPD

*+  Local Variables
      REAL Non_dQs_dT
      REAL SolRad               !short wave radiation W/m2
      REAL LongWave             !W/m2
      REAL NetRadiation
      REAL RhoA
      REAL Lambda
      REAL SpecificVPD
      REAL RadiationTerm
      REAL AerodynamicTerm
      real DayLength           !length of day (hours)
      REAL denominator         !of the Penman-Monteith equation

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_PM_PropRad')

*- Implementation Section ----------------------------------
 
      call push_routine (myname)

      if (NINT(timestep/60.0) .eq. 24 ) then
         DayLength = day_length (day,latitude, -6.0)
      else
         DayLength = timestep/60.0   !convert from min to hours
      end if

      Non_dQs_dT = micromet_Non_dQs_dT ((mint+maxt )/2.0 ,AirPressure)
      SolRad = micromet_Radn2SolRad (
     :               latitude
     :              ,day
     :              ,timestep
     :              ,radn)
      LongWave = micromet_longwave (   (mint+maxt )/2.0
     :                                ,SunshineHrs/DayLength
     :                                ,EmmisCanopy)
      NetRadiation = (1-Albedo) * SolRad + LongWave

      RhoA = micromet_RhoA ((mint + maxt)/2.0, AirPressure)
      Lambda = micromet_Lambda((mint + maxt)/2.0)
      SpecificVPD = micromet_SpecificVPD (
     :                                     vp
     :                                   , mint
     :                                   , maxt
     :                                   , AirPressure)

      Denominator = Non_dQs_dT
     :            + Divide( AerodynamicCond, CanopyCond, 0.0)
     :            + 1.0

      RadiationTerm =
     :          Divide( Non_dQs_dT * NetRadiation
     :                , Denominator, 0.0)
     :                * 1000.0 * (DayLength *3600.0)
     :                / Lambda / RhoW
      AerodynamicTerm =
     :          Divide( RhoA * Lambda* SpecificVPD * AerodynamicCond
     :                , Denominator, 0.0)
     :                * 1000.0 * (DayLength *3600.0)
     :                / Lambda / RhoW

      micromet_PM_PropRad = Divide(
     :                      RadiationTerm
     :                    , RadiationTerm + AerodynamicTerm
     :                    , 0.0)

      call pop_routine (myname)

      return
      end
*====================================================================
      real function micromet_PropThroughfall (
     :               CropK
     :             , CropLAI)
*====================================================================
      Use MicrometModule
      implicit none
      dll_export micromet_PropThroughfall
      include   'error.pub'

*+  Sub-Program Arguments
      real    CropK                  !crop exctinction coefficient
      REAL    CropLAI                !crio leaf area index

*+  Purpose
*     calculate the proportion of precipitation falling through
*     the canopy

*+  Notes
*     assumes that the light extinction coeffient can be applied to
*     water

*+  Changes
*       160799 - VOS specified and programmed

*+  Calls

*+  Local Variables

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_PropThroughfall')

*- Implementation Section ----------------------------------
 
      call push_routine (myname)

      micromet_PropThroughfall = EXP (-1 * CropK * CropLAI)

      call pop_routine (myname)

      return
      end
*====================================================================
      real function micromet_MaxStorage (
     :                CropSpecificStorage
     :              , CropLAI
     :              , PropCanopyWetted)
*====================================================================
      Use MicrometModule
      implicit none
      dll_export micromet_MaxStorage
      include   'data.pub'
      include   'error.pub'

*+  Sub-Program Arguments
      real    CropSpecificStorage     !mm water stored per uni LAI
      REAL    CropLAI                 !leaf area index
      REAL    PropCanopyWetted        !1.0 for rainfall, maybe < 1 for irrigation

*+  Purpose
*    calculated the maximum amount of water that can be stored on
*    the leaves corrected for irrigation height relative to canopy
*    height if necessary

*+  Notes

*+  Changes
*       160799 - VOS specified and programmed

*+  Calls

*+  Local Variables

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_MaxStorage')

*- Implementation Section ----------------------------------
 
      call push_routine (myname)

      micromet_MaxStorage = CropSpecificStorage
     :                    * CropLAI
     :                    * PropCanopyWetted

      micromet_MaxStorage = L_BOUND(micromet_MaxStorage, 0.0)

      call pop_routine (myname)

      return
      end
*====================================================================
      real function micromet_FreeEvapRate (
     :               EmmisCanopy
     :              ,mint
     :              ,maxt
     :              ,vp
     :              ,AirPressure
     :              ,AerodynamicCond)
*====================================================================
      Use MicrometModule
      implicit none
      dll_export micromet_FreeEvapRate
      include   'data.pub'                         
      include   'science.pub'
      include   'error.pub'                         

*+  Sub-Program Arguments
      REAL    EmmisCanopy         !emmisivity of the canopy -
      real    mint                !degC
      real    maxt                !degC
      real    vp                  !humidity in hPa
      REAL    AirPressure         !hPa
      real    AerodynamicCond     !m/s

*+  Purpose
*     Calculate the Penman-Monteith water demand for zero
*     surface resistance (appropriate to wetted leaves)
*     Assume no sunshine during rainfall - therefore longwave radiation only

*+  Notes

*+  Changes
*       160799 - VOS specified and programmed

*+  Calls
      real       micromet_PenmanMonteith       ! function
      dll_import micromet_PenmanMonteith

*+  Local Variables

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_FreeEvapRate')

*- Implementation Section ----------------------------------
 
      call push_routine (myname)

      micromet_FreeEvapRate = micromet_PenmanMonteith (
     :                -35.0         ! latitude not used
     :              , 2             ! day not used
     :              , 1440.0        ! timestep not used
     :              , 0.0           ! SunshineHrs, zero for longwave
     :              , 0.0           ! radn not used
     :              , 0.15          ! albedo not used
     :              , EmmisCanopy
     :              , mint
     :              , maxt
     :              , vp
     :              , AirPressure
     :              , AerodynamicCond
     :              , 0.0)          ! CanopyCond is zero - free water
     :              / (day_length (2, -35.0, -6.0) * 3600.0) !mm/sec

      call pop_routine (myname)

      return
      end
*====================================================================
      real function micromet_GashIntercep (
     :                Precip
     :              , PrecipDurn
     :              , FreeEvapRate
     :              , MaxStorage
     :              , PropThroughfall)
*====================================================================
      Use MicrometModule
      implicit none
      dll_export micromet_GashIntercep
      include   'data.pub'                         
      include   'error.pub'

*+  Sub-Program Arguments
      real    Precip              !mm irrigation or rainfall
      real    PrecipDurn          !sec, duration of rainfall or irrigation
      real    FreeEvapRate        !mm/sec
      real    MaxStorage          !mm water stored / unit LAI
      real    PropThroughfall     !propportioin of precipitation that falls though the canopy

*+  Purpose
*     calculate the amount (mm) of rainfall or irrigation
*     interception according ot the Gash model

*+  Notes

*+  Changes
*       160799 - VOS specified and programmed

*+  Calls

*+  Local Variables
      REAL temp                      !temprary variable
      REAL PrecipRate                !rainfall or irrigation rate, mm/s
      REAL MaxIntercep               !maximum possible interception, mm
      REAL Precip2Sat                !mm of prcipitation to saturate the canopy, mm

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_GashIntercep')

*- Implementation Section ----------------------------------
 
      call push_routine (myname)

      PrecipRate = Divide(Precip, PrecipDurn, 1.0) !set top 1.0 mm/s

      MaxIntercep = (1.0 - PropThroughfall) * Precip

      FreeEvapRate = l_bound(FreeEvapRate, 1e-6)

      if (PrecipRate .le. FreeEvapRate) then
         micromet_GashIntercep = MaxIntercep  !everything that lands evaporates
      elseif (PropThroughfall .gt. 0.999) then
         micromet_GashIntercep = 0.0          !nothing is intercepted
      else                       !go and calculate interception
         temp = 1.0 - (FreeEvapRate/PrecipRate)/(1.0-PropThroughfall)
         if (temp .le. 0.0) then
            micromet_GashIntercep = MaxIntercep
         else
            Precip2Sat = -PrecipRate / FreeEvapRate
     :                 * MaxStorage * log(temp)
            micromet_GashIntercep =
     :                  (1-PropThroughfall) * Precip2Sat
     :                + FreeEvapRate / PrecipRate * (Precip-Precip2Sat)
         end if
      end if

      micromet_GashIntercep
     :        = bound(micromet_GashIntercep, 0.0, MaxIntercep)

      call pop_routine (myname)

      return
      end
*====================================================================
      real function micromet_Intercep (
     :                CropK
     :              , CropLAI
     :              , CropSpecificStorage
     :              , PropCanopyWetted
     :              , EmmisCanopy
     :              , Precip
     :              , PrecipDurn
     :              , mint
     :              , maxt
     :              , vp
     :              , AirPressure
     :              , AerodynamicCond)
*====================================================================
      Use MicrometModule
      implicit none
      dll_export micromet_Intercep
      include   'data.pub'                         
      include   'science.pub'
      include   'error.pub'                         

*+  Sub-Program Arguments
       real CropK                !crop extinction coefficient
       real CropLAI              !crop LAI
       real CropSpecificStorage  !mm water store per unit LAI
       real PropCanopyWetted     !for irrigation below canopy
       real EmmisCanopy          !usually 0.96
       real Precip               !amount of rainfall or irrigation, mm
       real PrecipDurn           !duration of precipitation, sec
       real mint                 !minimum temperature  C
       real maxt                 !maximum temperature  C
       real vp                   !humidity  hPa = mbar
       real AirPressure          !air pressure hPa
       real AerodynamicCond      !m/s

*+  Purpose
*

*+  Notes

*+  Changes
*       160799 - VOS specified and programmed

*+  Calls
      real       micromet_PropThroughfall       ! function
      dll_import micromet_PropThroughfall

      real       micromet_MaxStorage       ! function
      dll_import micromet_MaxStorage

      real       micromet_GashIntercep       ! function
      dll_import micromet_GashIntercep

      real       micromet_PenmanMonteith       ! function
      dll_import micromet_PenmanMonteith

*+  Local Variables
      REAL PropThroughfall                      !
      REAL MaxStorage           !
      REAL FreeEvaprate

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_Intercep')

*- Implementation Section ----------------------------------
 
      call push_routine (myname)

      PropThroughfall = micromet_PropThroughfall (
     :                                CropK
     :                              , CropLAI)
      MaxStorage = micromet_MaxStorage (
     :                                CropSpecificStorage
     :                              , CropLAI
     :                              , PropCanopyWetted)
      FreeEvapRate = micromet_PenmanMonteith (
     :                -35.0         ! latitude not used
     :              , 2             ! day not used
     :              , 1440.0        ! timestep not used
     :              , 0.0           ! SunshineHrs
     :              , 0.0           ! radn not used
     :              , 0.15          ! albedo not used
     :              , EmmisCanopy
     :              , mint
     :              , maxt
     :              , vp
     :              , AirPressure
     :              , AerodynamicCond
     :              , 0.0)          ! CanopyCond is zero - free water
     :              / (day_length (2, -35.0, -6.0) * 3600.0)

      micromet_Intercep = micromet_GashIntercep (
     :                               Precip
     :                             , PrecipDurn
     :                             , FreeEvapRate
     :                             , MaxStorage
     :                             , PropThroughfall)

      call pop_routine (myname)

      return
      end
*====================================================================
      real function micromet_InterpTemp (
     :                time
     :              , maxt_time
     :              , mint
     :              , maxt
     :              , mint_yesterday
     :              , maxt_yesterday)
*====================================================================
      Use MicrometModule
      implicit none
      dll_export micromet_InterpTemp
      include   'data.pub'
      include   'error.pub'

*+  Sub-Program Arguments
      real time           !time of day in hours
      REAL maxt_time      !time of day for minimum temperature, hours
      REAL mint           !minimum temperature, C
      REAL maxt           !maximum temperature, C
      REAL mint_yesterday !minimum temperature yesterady, C
      REAL maxt_yesterday !maximum temperature yesterady, C

*+  Purpose
*    Interpolate air temperature

*+  Notes
*    Between midinight and mint_time just a linear interpolation between
*    yesterday's midnight temperature and today's mint.  For the rest of
*    the day use a sin function

*+  Changes
*       290799 - VOS specified and programmed

*+  Calls

*+  Local Variables
      REAL p_time               !time as proportion of the day
      REAL p_maxt_time          !mint_time as proportion of the day
      REAL p_mint_time          !mint_time as proportion of the day
      REAL t_midnight           !temperature last midnight
      REAL t_scale              !0 at midnight, 1 at mint_time

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_InterpTemp')

*- Implementation Section ----------------------------------
 
      call push_routine (myname)

      p_time = time / 24.0
      p_maxt_time = maxt_time / 24.0
      p_mint_time = p_maxt_time - 0.5

      if (p_time .lt. p_mint_time) then
         t_midnight = SIN((0.0 + 0.25 - p_maxt_time)*2.0*pi)
     :              * (maxt_yesterday - mint_yesterday) / 2.0
     :              + (maxt_yesterday + mint_yesterday) / 2.0
         t_scale = (p_mint_time - p_time) / p_mint_time
         t_scale = bound(t_scale, 0.0, 1.0)
         micromet_InterpTemp = mint + t_scale*(t_midnight-mint)
      else
         micromet_InterpTemp = SIN((p_time + 0.25 - p_maxt_time)*2.0*pi)
     :                       * (maxt - mint) / 2.0
     :                       + (maxt + mint) / 2.0
      end if

      call pop_routine (myname)

      return
      end

*====================================================================
      real function micromet_VPD (
     :                          vp
     :                        , mint
     :                        , maxt)
*====================================================================
      Use MicrometModule
      implicit none
      dll_export micromet_VPD
      include   'data.pub'                         
      include   'error.pub'                         

*+  Sub-Program Arguments
      real       vp             ! (INPUT) vapour pressure (hPa = mbar)
      real       mint           ! (INPUT) minimum temperature (oC)
      real       maxt           ! (INPUT) maximum temperature (oC)

*+  Purpose
*     calculate the vapour pressure deficit 

*+  Changes
*       230300 - VOS specified and programmed

*+  Calls
      real       micromet_svp       ! function
      dll_import micromet_svp

*+  Local Variables
      real       VPDmint !VPD at minimium temperature
      real       VPDmaxt !VPD at maximium temperature

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_VPD')

*- Implementation Section ----------------------------------
 
      call push_routine (myname)

      VPDmint = micromet_svp(mint) - vp
      VPDmint = l_bound(VPDmint,0.0)
      VPDmaxt = micromet_svp(maxt) - vp
      VPDmaxt = l_bound(VPDmaxt,0.0)

      micromet_VPD = svp_fract * VPDmaxt
     :             + (1 - svp_fract) * VPDmint
 
      call pop_routine (myname)
      return
      end

*====================================================================
      real function micromet_SpecificVPD (
     :                                   vp
     :                                 , mint
     :                                 , maxt
     :                                 , AirPressure)
*====================================================================
      Use MicrometModule
      implicit none
      dll_export micromet_SpecificVPD
      include   'data.pub'                         
      include   'error.pub'                         

*+  Sub-Program Arguments
      real       vp             ! (INPUT) vapour pressure (hPa = mbar)
      real       mint           ! (INPUT) minimum temperature (oC)
      real       maxt           ! (INPUT) maximum temperature (oC)
      real       AirPressure    ! (INPUT) Air pressure (hPa)

*+  Purpose
*     calculate the vapour pressure deficit 

*+  Changes
*       230300 - VOS specified and programmed

*+  Calls
      real       micromet_VPD       ! function
      dll_import micromet_VPD

      real       micromet_SpecificHumidity       ! function
      dll_import micromet_SpecificHumidity

*+  Local Variables
      real       VPD     !VPD in hPa

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_SpecificVPD')

*- Implementation Section ----------------------------------
 
      call push_routine (myname)

      VPD = micromet_VPD (vp, mint, maxt)
      micromet_SpecificVPD = micromet_SpecificHumidity (VPD
     :                                                 ,AirPressure)
 
      call pop_routine (myname)
      return
      end

*====================================================================
      real function micromet_RefWindSpeed (
     :                         WindSpeed
     :                       , MetHeight
     :                       , MetCropLAI
     :                       , MetCropHeight
     :                       , CropHeight
     :                       , CropLAI
     :                       , RefHeight)
*====================================================================
      Use MicrometModule
      implicit none
      dll_export micromet_RefWindSpeed
      include   'error.pub'                         
      include   'data.pub'

*+  Sub-Program Arguments
      real Windspeed               !m/s
      real MetHeight              !m
      real MetCropLAI             !m2/m2
      real MetCropHeight          !m
      real CropHeight              !m
      real CropLAI                 !m
      real RefHeight               !m

*+  Purpose
*     Correct windspeed for measurement and simulation crop characteristics

*+  Notes

*+  Changes
*       240300 - VOS specified and programmed

*+  Calls
      real       micromet_ZeroPlaneDispl       ! function
      dll_import micromet_ZeroPlaneDispl

      real       micromet_RoughnessLength       ! function
      dll_import micromet_RoughnessLength

      real       micromet_FrictionVelocity       ! function
      dll_import micromet_FrictionVelocity

*+  Local Variables
      real MetZeroPlaneDispl       !zero-plance displacement (m)
      real MetRoughnessLength      !roughness length (m)
      real MetFrictionVelocity     !u_star (m/s)
      real RecalcHeight             !height where wind unaffected by vegetation
      real RecalcWindSpeed          !wind speed at RecalcHeight
      real RefZeroPlaneDispl        !zero-plance displacement (m)
      real RefRoughnessLength       !roughness length (m)
      real RefFrictionVelocity      !u_star (m/s)
      real NewWindSpeed             !temporary variable

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_RefWindSpeed')

      real       von_karman
      parameter (von_karman = 0.41)

*- Implementation Section ----------------------------------
 
      call push_routine (myname)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!! Calculate properties of the measurement site
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      MetZeroPlaneDispl = micromet_ZeroPlaneDispl(MetCropHeight, 
     :                                             MetCropLAI)
      MetRoughnessLength = micromet_RoughnessLength(MetCropHeight,
     :                                               MetZeroPlaneDispl)
      MetFrictionVelocity =
     :            micromet_FrictionVelocity (WindSpeed
     :                                     , MetHeight
     :                                     , MetZeroPlaneDispl
     :                                     , MetRoughnessLength)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!! Calculate height/properties where wind speed not affected by vegetation
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      RecalcHeight    = max (MetCropHeight, CropHeight) + 20.0
      RecalcWindSpeed = (RecalcHeight - MetZeroPlaneDispl)
     :                / MetRoughnessLength
      RecalcWindSpeed = max(RecalcWindSpeed, 1.0) !i.e. WindSpeed is ln(1)=0
      RecalcWindSpeed = von_karman / MetFrictionVelocity
     :                * log(RecalcWindSpeed)  !natural log

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!! Calculate properties of the simulation/reference site
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      RefZeroPlaneDispl = micromet_ZeroPlaneDispl(CropHeight, 
     :                                            CropLAI)
      RefRoughnessLength = micromet_RoughnessLength(CropHeight,
     :                                              RefZeroPlaneDispl)
      RefFrictionVelocity =
     :            micromet_FrictionVelocity (RecalcWindSpeed
     :                                     , RecalcHeight
     :                                     , RefZeroPlaneDispl
     :                                     , RefRoughnessLength)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!! Calculate reference wind speed to be used in further calculations
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      NewWindSpeed = (RefHeight - RefZeroPlaneDispl)
     :             / RefRoughnessLength
      NewWindSpeed = max(NewWindSpeed, 1.0) !i.e. WindSpeed is zero
      NewWindSpeed = von_karman / RefFrictionVelocity
     :             * log(NewWindSpeed)      !natural log

      micromet_RefWindSpeed = NewWindSpeed

      call pop_routine (myname)

      return
      end

*====================================================================
      real function micromet_AerodynamicCondNew (
     :                         WindSpeed
     :                       , MetHeight
     :                       , MetCropLAI
     :                       , MetCropHeight
     :                       , CropHeight
     :                       , CropLAI)
*====================================================================
      Use MicrometModule
      implicit none
      dll_export micromet_AerodynamicCondNew
      include   'error.pub'                         
      include   'data.pub'

*+  Sub-Program Arguments
      real Windspeed               !m/s
      real MetHeight               !m
      real MetCropLAI              !m2/m2
      real MetCropHeight           !m
      real CropHeight              !m
      real CropLAI                 !m

*+  Purpose
*     Calculate the Aerodynamic Conductance for the topmost layer

*+  Notes

*+  Changes
*       240300 - VOS specified and programmed

*+  Calls
      real       micromet_ZeroPlaneDispl       ! function
      dll_import micromet_ZeroPlaneDispl

      real       micromet_RoughnessLength       ! function
      dll_import micromet_RoughnessLength

      real       micromet_FrictionVelocity       ! function
      dll_import micromet_FrictionVelocity

      real       micromet_RefWindSpeed       ! function
      dll_import micromet_RefWindSpeed

*+  Local Variables
      real ZeroPlaneDispl       !zero-plance displacement (m)
      real RoughnessLength      !roughness length (m)
      real FrictionVelocity     !u_star (m/s)
      real RefHeight            !height to calculate windspeed at
      real RefWindSpeed         !recalculated wind speed

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_AerodynamicCondNew')

*- Implementation Section ----------------------------------
 
      call push_routine (myname)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!! Calculate reference wind speed to be used in further calculations
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      RefHeight = CropHeight + 2.0
      RefWindSpeed = micromet_RefWindSpeed (
     :                         WindSpeed
     :                       , MetHeight
     :                       , MetCropLAI
     :                       , MetCropHeight
     :                       , CropHeight
     :                       , CropLAI
     :                       , RefHeight)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!! Calculate site properties
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      ZeroPlaneDispl = 
     :        micromet_ZeroPlaneDispl (CropHeight, CropLAI)
      RoughnessLength = 
     :        micromet_RoughnessLength(CropHeight, ZeroPlaneDispl)
      FrictionVelocity =
     :        micromet_FrictionVelocity (RefWindSpeed
     :                                 , RefHeight
     :                                 , ZeroPlaneDispl
     :                                 , RoughnessLength)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!! Calculate conductance
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      micromet_AerodynamicCondNew =
     :        divide( FrictionVelocity**2, RefWindSpeed, 0.0)
      micromet_AerodynamicCondNew =
     :        l_bound(micromet_AerodynamicCondNew, 0.001)

      call pop_routine (myname)

      return
      end
*====================================================================
      real function micromet_AerodynamicCondSub (
     :                                        WindSpeed
     :                                      , WindAttenuation
     :                                      , MetHeight
     :                                      , MetCropLAI
     :                                      , MetCropHeight
     :                                      , CropLAI
     :                                      , CropHeight
     :                                      , SourceTop
     :                                      , SourceSub)
*====================================================================
      Use MicrometModule
      implicit none
      dll_export micromet_AerodynamicCondSub
      include   'error.pub'                         
      include   'data.pub'

*+  Sub-Program Arguments
      real WindSpeed               !wind speed (m/s)
      real WindAttenuation         !wind attenutation coefficient (-) usually 0.5
      real MetHeight               !height at which met variables are measured (m)
      real MetCropLAI              !LAI at met station (usually 3)
      real MetCropHeight           !height of vegetation at met station (m) usually 0.05
      real CropLAI                 !LAI of simulated vegetation (m)
      real CropHeight              !height of simulated vegetation (m)
      real SourceTop               !middle of topmost layer (m)
      real SourceSub               !middle of sub-layer (m)

*+  Purpose
*     Calculate the Aerodynamic Conductance of a sub-canopy layer

*+  Notes

*+  Changes
*       110500 - VOS specified and programmed

*+  Calls
      real       micromet_ZeroPlaneDispl        ! function
      dll_import micromet_ZeroPlaneDispl

      real       micromet_RoughnessLength       ! function
      dll_import micromet_RoughnessLength

      real       micromet_FrictionVelocity      ! function
      dll_import micromet_FrictionVelocity

      real       micromet_RefWindSpeed          ! function
      dll_import micromet_RefWindSpeed

*+  Local Variables
      real ZeroPlaneDispl       !zero-plance displacement (m)
      real RoughnessLength      !roughness length (m)
      real FrictionVelocity     !u_star (m/s)
      real RefHeight            !height to calculate windspeed at
      real RefWindSpeed         !recalculated wind speed (m/s)
      real Diffusivity          !diffusivity function for vapour transport
      real tempTop              !temporary variable for topmost layer
      real tempSub              !temporary variable for sub layer

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_AerodynamicCondSub')

      real       von_karman
      parameter (von_karman = 0.41)

*- Implementation Section ----------------------------------
 
      call push_routine (myname)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!! Calculate reference wind speed to be used in further calculations
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      RefHeight = CropHeight + 2.0
      RefWindSpeed = micromet_RefWindSpeed (
     :                         WindSpeed
     :                       , MetHeight
     :                       , MetCropLAI
     :                       , MetCropHeight
     :                       , CropHeight
     :                       , CropLAI
     :                       , RefHeight)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!! Calculate site properties
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ZeroPlaneDispl = 
     :        micromet_ZeroPlaneDispl (CropHeight, CropLAI)
      RoughnessLength = 
     :        micromet_RoughnessLength(CropHeight, ZeroPlaneDispl)
      FrictionVelocity =
     :        micromet_FrictionVelocity (RefWindSpeed
     :                                 , RefHeight
     :                                 , ZeroPlaneDispl
     :                                 , RoughnessLength)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!! Calculate diffusivity
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      Diffusivity =  von_karman**2
     :            * (CropHeight - ZeroPlaneDispl)
     :            / log ((RefHeight-ZeroPlaneDispl)/RoughnessLength) !natural log

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!! Calculate conductance
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      tempTop = -1.0 * WindAttenuation 
     :        * SourceTop 
     :        / (2.0 * CropHeight)
      tempSub = -1.0 * WindAttenuation 
     :        * SourceSub 
     :        / (2.0 * CropHeight)

      micromet_AerodynamicCondSub = 
     :              CropHeight 
     :           *  exp(WindAttenuation)
     :           /  Diffusivity 
     :           /  WindAttenuation
     :           * (exp(tempSub) - exp(tempTop))
      micromet_AerodynamicCondSub = 
     :          Divide (1.0, micromet_AerodynamicCondSub, 0.0)

      call pop_routine (myname)

      return
      end


C     Tamet code ex apsim.
*====================================================================
      subroutine tamet2_zero_variables ()
*====================================================================
      implicit none
      include 'tamet2a.inc'

*   Short description:
*     Set all variables in this module to zero.

*   Changes:
*       210995 jngh programmed

*   Constant values

*   Initial data values
*      none
      integer i

*- Implementation Section ----------------------------------

         !variables 
      do i = 1, offset
         g_a_day(i) = 0
         g_a_year(i) =0 
         g_a_min(i) = 0

         g_a_max(i) = 0
         g_a_evap(i) = 0
         g_a_rain(i) =0
         g_a_radn(i) =0
         g_a_wind(i) =0
      enddo

      g_day   = 0
      g_year  = 0
      g_rain  = 0.0
      g_maxt  = 0.0
      g_mint  = 0.0
      g_evap  = 0.0
      g_wind  = 0.0
      g_radn  = 0.0
      g_dis_maxt  = 0.0
      g_dis_mint  = 0.0
      g_dis_evap  = 0.0
      g_dis_wind  = 0.0
      g_dis_radn  = 0.0
      g_string = ''
      do i = 1,31
         g_rd(i)    = 0.0
      enddo
      g_nv = 0
      g_radn_rcal = 0.0
      g_wind_found = .false.
             
      call tamet2_zero_daily_variables ()

      return
      end

*====================================================================
      subroutine tamet2_zero_daily_variables ()
*====================================================================
      implicit none
      include 'tamet2a.inc'
      integer date_to_jday

*   Short description:
*     Set all variables in this module to zero.

*   Changes:
*       210995 jngh programmed


*   Internal variables
      integer   i, idate(3)

*   Constant values

*- Implementation Section ----------------------------------

      do i = 1, offset-1
         g_a_day(i) = g_a_day(i+1)
         g_a_year(i) = g_a_year(i+1) 
         g_a_min(i) = g_a_min(i+1)  
         g_a_max(i) = g_a_max(i+1)  
         g_a_evap(i) = g_a_evap(i+1) 
         g_a_rain(i) = g_a_rain(i+1) 
         g_a_radn(i) = g_a_radn(i+1) 
         g_a_wind(i) = g_a_wind(i+1) 
      enddo

      g_a_day(offset) = 0  
      g_a_year(offset) = 0 
      g_a_min(offset) = 0  
      g_a_max(offset) = 0  
      g_a_evap(offset) = 0 
      g_a_rain(offset) = 0 
      g_a_radn(offset) = 0 
      g_a_wind(offset) = 0 
      
      g_type = ''
      g_field = ''


      call day_of_year_to_date (g_day, g_year, idate)
      g_last_day = Date_to_jday (idate(1), idate(2), idate(3))
     :           - 1
      
      return
      end
*===========================================================
      subroutine tamet2_setup_links ()
*===========================================================
      implicit none
      include   'tamet2a.inc'            ! Global constant definitions

*   Short description:
*       setup linked variables

*   Changes:
*       210995 jngh programmed
*       020398 jngh added in reading and reporting for reference height,
*                   disp and Z0 at instrument site.
*                   changed z0soil to mm


*   Internal variables
      integer    numvals               ! number of values read
      integer    ds
      integer    idate(3)
      character *2 section_name, unknown_module

*   Constant values

*- Implementation Section ----------------------------------

      call link_real_var (
     :     section_name,          ! Section header
     :     'dis_evap',            ! Keyword
     :     '(mm)',               ! Units
     :     p_dis_evap,                 ! Variable
     :     numvals,               ! Number of values returned
     :     0.0,         ! Lower Limit for bound checking
     :     100.0)         ! Upper Limit for bound checking
 
      call link_real_var (
     :     section_name,          ! Section header
     :     'dish_evap',            ! Keyword
     :     '(mm)',               ! Units
     :     p_dish_evap,                 ! Variable
     :     numvals,               ! Number of values returned
     :     -100.0,         ! Lower Limit for bound checking
     :     0.0)         ! Upper Limit for bound checking
 
      call link_real_var (
     :     section_name,          ! Section header
     :     'disvh_evap',            ! Keyword
     :     '(mm)',               ! Units
     :     p_disvh_evap,                 ! Variable
     :     numvals,               ! Number of values returned
     :     -100.0,         ! Lower Limit for bound checking
     :     0.0)         ! Upper Limit for bound checking
 
      call link_real_var (
     :     section_name,          ! Section header
     :     'dis_maxt_dry_season',            ! Keyword
     :     '(oC)',               ! Units
     :     p_dis_maxt_dry,                 ! Variable
     :     numvals,               ! Number of values returned
     :     0.0,         ! Lower Limit for bound checking
     :     60.0)         ! Upper Limit for bound checking
 
      call link_real_var (
     :     section_name,          ! Section header
     :     'dis_maxt_other',            ! Keyword
     :     '(oC)',               ! Units
     :     p_dis_maxt_other,                 ! Variable
     :     numvals,               ! Number of values returned
     :     0.0,         ! Lower Limit for bound checking
     :     60.0)         ! Upper Limit for bound checking
 
      call link_real_var (
     :     section_name,          ! Section header
     :     'dis_mint',            ! Keyword
     :     '(oC)',               ! Units
     :     p_dis_mint,                 ! Variable
     :     numvals,               ! Number of values returned
     :     0.0,         ! Lower Limit for bound checking
     :     60.0)         ! Upper Limit for bound checking
 

 
      call link_real_var (
     :     section_name,          ! Section header
     :     'maxt_to_mint',            ! Keyword
     :     '(oC)',               ! Units
     :     p_Maxt_to_minT,                 ! Variable
     :     numvals,               ! Number of values returned
     :     0.0,         ! Lower Limit for bound checking
     :     60.0)         ! Upper Limit for bound checking
 


 
      call link_real_var (
     :     section_name,          ! Section header
     :     'rain_lb',            ! Keyword
     :     '(mm)',               ! Units
     :     p_rain_lb,                 ! Variable
     :     numvals,               ! Number of values returned
     :     0.0,         ! Lower Limit for bound checking
     :     100.0)         ! Upper Limit for bound checking
 


 
      call link_real_var (
     :     section_name,          ! Section header
     :     'rain_ub',            ! Keyword
     :     '(mm)',               ! Units
     :     p_rain_ub,                 ! Variable
     :     numvals,               ! Number of values returned
     :     0.0,         ! Lower Limit for bound checking
     :     2000.0)         ! Upper Limit for bound checking
 


 
      call link_real_var (
     :     section_name,          ! Section header
     :     'gauge_capacity',            ! Keyword
     :     '(mm)',               ! Units
     :     p_gauge_capacity,                 ! Variable
     :     numvals,               ! Number of values returned
     :     0.0,         ! Lower Limit for bound checking
     :     1000.0)         ! Upper Limit for bound checking
 


 
      call link_real_var (
     :     section_name,          ! Section header
     :     'maxt_lb',            ! Keyword
     :     '(oC)',               ! Units
     :     p_maxt_lb,                 ! Variable
     :     numvals,               ! Number of values returned
     :     0.0,         ! Lower Limit for bound checking
     :     30.0)         ! Upper Limit for bound checking
 


 
      call link_real_var (
     :     section_name,          ! Section header
     :     'maxt_ub',            ! Keyword
     :     '(oC)',               ! Units
     :     p_maxt_ub,                 ! Variable
     :     numvals,               ! Number of values returned
     :     0.0,         ! Lower Limit for bound checking
     :     60.0)         ! Upper Limit for bound checking
 


 
      call link_real_var (
     :     section_name,          ! Section header
     :     'mint_lb',            ! Keyword
     :     '(oC)',               ! Units
     :     p_mint_lb,                 ! Variable
     :     numvals,               ! Number of values returned
     :     -20.0,         ! Lower Limit for bound checking
     :     30.0)         ! Upper Limit for bound checking
 


 
      call link_real_var (
     :     section_name,          ! Section header
     :     'mint_ub',            ! Keyword
     :     '(oC)',               ! Units
     :     p_mint_ub,                 ! Variable
     :     numvals,               ! Number of values returned
     :     0.0,         ! Lower Limit for bound checking
     :     40.0)         ! Upper Limit for bound checking
 


 
      call link_real_var (
     :     section_name,          ! Section header
     :     'evap_lb',            ! Keyword
     :     '(mm)',               ! Units
     :     p_evap_lb,                 ! Variable
     :     numvals,               ! Number of values returned
     :     0.0,         ! Lower Limit for bound checking
     :     100.0)         ! Upper Limit for bound checking
 


 
      call link_real_var (
     :     section_name,          ! Section header
     :     'evap_ub',            ! Keyword
     :     '(mm)',               ! Units
     :     p_evap_ub,                 ! Variable
     :     numvals,               ! Number of values returned
     :     0.0,         ! Lower Limit for bound checking
     :     100.0)         ! Upper Limit for bound checking
 


 
      call link_real_var (
     :     section_name,          ! Section header
     :     'wind_lb',            ! Keyword
     :     '(km)',               ! Units
     :     p_wind_lb,                 ! Variable
     :     numvals,               ! Number of values returned
     :     0.0,         ! Lower Limit for bound checking
     :     100.0)         ! Upper Limit for bound checking
 


 
      call link_real_var (
     :     section_name,          ! Section header
     :     'wind_ub',            ! Keyword
     :     '(km)',               ! Units
     :     p_wind_ub,                 ! Variable
     :     numvals,               ! Number of values returned
     :     0.0,         ! Lower Limit for bound checking
     :     2000.0)         ! Upper Limit for bound checking
 


 
      call link_real_var (
     :     section_name,          ! Section header
     :     'radn_lb',            ! Keyword
     :     '()',               ! Units
     :     p_radn_lb,                 ! Variable
     :     numvals,               ! Number of values returned
     :     0.0,         ! Lower Limit for bound checking
     :     2.0)         ! Upper Limit for bound checking
 


 
      call link_real_var (
     :     section_name,          ! Section header
     :     'radn_ub',            ! Keyword
     :     '()',               ! Units
     :     p_radn_ub,                 ! Variable
     :     numvals,               ! Number of values returned
     :     0.0,         ! Lower Limit for bound checking
     :     2.0)         ! Upper Limit for bound checking
 


 
      call link_real_var (
     :     section_name,          ! Section header
     :     'radn_low',            ! Keyword
     :     '()',               ! Units
     :     p_radn_low,                 ! Variable
     :     numvals,               ! Number of values returned
     :     0.0,         ! Lower Limit for bound checking
     :     1.0)         ! Upper Limit for bound checking
 


 
      call link_real_var (
     :     section_name,          ! Section header
     :     'radn_vlow',            ! Keyword
     :     '()',               ! Units
     :     p_radn_vlow,                 ! Variable
     :     numvals,               ! Number of values returned
     :     0.0,         ! Lower Limit for bound checking
     :     1.0)         ! Upper Limit for bound checking
 

      call link_real_var (
     :           unknown_module
     :          ,'latitude'
     :          ,'(deg)'
     :          ,g_latitude
     :          ,numvals
     :          ,-90.0
     :          ,90.0)
 
     
       call link_integer_var (
     :      unknown_module
     :     ,'day'        
     :     ,'()'      
     :     ,g_day        
     :     ,numvals       
     :     ,1         
     :     ,366)
     
       call link_integer_var (
     :      unknown_module
     :     ,'year'        
     :     ,'()'      
     :     ,g_year        
     :     ,numvals       
     :     ,1800         
     :     ,2050)
          
      call link_real_var (
     :      unknown_module
     :     ,'radn'
     :     ,'(MJ/m2)'
     :     ,g_radn
     :     ,numvals
     :     ,0.0
     :     ,100.0)
 
      
      call link_real_var(
     :      unknown_module  ! Module that responds (Not Used)
     :    , 'maxt'          ! Variable Name
     :    , '(oC)'          ! Units                (Not Used)
     :    , g_maxt          ! Variable
     :    , numvals         ! Number of values returned
     :    , 0.0             ! Lower Limit for bound checking
     :    , 50.0)           ! Upper Limit for bound checking
     
      call link_real_var(
     :      unknown_module  ! Module that responds (Not Used)
     :    , 'mint'          ! Variable Name
     :    , '(oC)'          ! Units                (Not Used)
     :    , g_mint          ! Variable
     :    , numvals         ! Number of values returned
     :    , -20.0           ! Lower Limit for bound checking
     :    , 40.0)           ! Upper Limit for bound checking
     
      call link_real_var(
     :      unknown_module  ! Module that responds (Not Used)
     :    , 'rain'          ! Variable Name
     :    , '(mm)'          ! Units                (Not Used)
     :    , g_rain          ! Variable
     :    , numvals         ! Number of values returned
     :    , 0.0             ! Lower Limit for bound checking
     :    , 1000.0)         ! Upper Limit for bound checking
     
      call link_real_var(
     :      unknown_module  ! Module that responds (Not Used)
     :    , 'evap'          ! Variable Name
     :    , '(mm)'          ! Units                (Not Used)
     :    , g_evap          ! Variable
     :    , numvals         ! Number of values returned
     :    , 0.0             ! Lower Limit for bound checking
     :    , 1000.0)         ! Upper Limit for bound checking
     
      call link_real_var(
     :      unknown_module
     :     ,'wind'
     :     ,'(km/day)'
     :     ,g_wind
     :     ,numvals
     :     ,0.0
     :     ,1000.0)

      call link_integer_var(
     :      unknown_module
     :     ,'wind_found'
     :     ,'()'
     :     ,g_wind_found
     :     ,numvals
     :     ,0
     :     ,1)

      return
      end

*====================================================================
      subroutine link_real_var(
     :     module, name, units, address
     :     ,numvals, lb, ub)
*====================================================================
      implicit none
      character *(*) module, name, units
      double precision address
      real lb, ub
      integer numvals

      call makelink_dbl(1, address, name)
      return
      end

*====================================================================
      subroutine link_integer_var(
     :     module, name, units, address
     :     ,numvals, lb, ub)
*====================================================================
      implicit none
      character *(*) module, name, units
      integer address, lb, ub
      integer numvals

      call makelink_int(1, address, name)
      return
      end

*====================================================================
      subroutine tamet2_get_other_variables ()
*====================================================================
      implicit none
      include   'tamet2a.inc'            ! Constant definitions

*   Short description:
*      Get the values of variables from other modules

*   Changes:
*       210995 jngh programmed

*   Internal variables
      integer    numvals               ! number of values returned
      integer    i                     
      integer idate(3)

*   Constant values
   
*- Implementation Section ----------------------------------
      
!   D(I,3-5)  DAY, MONTH, YEAR
!   D(I,6)    RAIN
!   D(I,7)    EVAPORATION
!   D(I,8-12) TEMPERATURES: DRY, WET, MAX, MIN, TERR MIN
!   D(I,13)   WIND RUN
!   D(I,14)   RADIATION
     
      g_a_year(offset) = g_year
      g_a_day(offset) = g_day
      g_a_radn(offset) = g_radn
      g_a_max(offset) = g_maxt
      g_a_min(offset) = g_mint
      g_a_rain(offset) = g_rain
      g_a_evap(offset) = g_evap
      g_a_wind(offset) = g_wind
 
      return
      end
*================================================================
      subroutine tamet2_main()
*================================================================
      implicit none
      include   'tamet2a.inc'            ! Constant definitions

      integer Date_to_jday
      logical end_month
      character string*80
      
      real  radn_max
      real  radn_cal

      DOUBLE PRECISION LAT
      INTEGER DS, YR
      integer idate(3)
      CHARACTER STN*4
      integer  lag
      integer  lag2
      integer  j
      integer  nv
      real     rcal
      real     rcal20
      real     bottom_limit
      real     top_limit
      real     dec
      integer  i
      integer  ia
      integer  ib
      real    dk
      real    dkj
      integer  k
      integer  nm
      integer  mth
      
! ***************
!   AV      R MONTH AVERAGE
!   BL      R LOWER LIMIT FOR RADIATION
!   CT      I COUNT OF NO OF DATA VALUES OF EACH ELEMENT
!   D       I INPUT DATA ARRAY
!   D(I,1)    STATION CODE
!   D(I,2)    MET BUREAU STATION NO, THEN USED FOR WET BULB DEPRESSION
!   D(I,3-5)  DAY, MONTH, YEAR
!   D(I,6)    RAIN
!   D(I,7)    EVAPORATION
!   D(I,8-12) TEMPERATURES: DRY, WET, MAX, MIN, TERR MIN
!   D(I,13)   WIND RUN
!   D(I,14)   RADIATION
!   DEC     R SOLAR DECLINATION
!   DPR     I WET BULB DEPRESSION
!   DS      I 1 IN DRY SEASON, 0 OTHERWISE
!   EV      R EVAPORATION
!   F6      I LOCATIONS OF F6.1 MEANS IN PRIN
!   HR      R HOURS OF RAIN
!   I,IA,IB,J  INDEXING VARIABLES
!   ICENT   I CENTURY NO
!   IDT     I PARAMETERS OF STATION BEING PROCESSED
!   IRD     I RADIATION EXPRESSED AS % OF THEORETICAL FOR EACH DAY
!   JA,JB,JC  COUNTERS FOR * STRINGS
!   K,L,M,N,P,Q  I TEMPORARY OR INDEXING VARIABLES
!   NUMERR(0)   I NO OF OCCURRENCES, IN ONE RECORD, OF NON-NUMERIC INPWT
!             FIELDS WHICH ARE USUALLY NUMERIC
!   LAG     I ERROR FLAG   0 NO ERRORS, TAPE1 WILL BE OUTPUT
!                          1 POSSIBLE ERRORS, TAPES1,2,4 OUTPUT
!                          2 DEFINITE ERRORS, TAPES2,4 OUTPUT
!   LA      I SPECIFIES WHICH COLUMN IN PRIN THE CURRENT MONTH'S RAINFAL
!             IS TO GO INTO. SET BY RAINFLAG RF
!   LB      I LIMITS NO OF FIELDS READ IN WHEN RF=1
!   LAT     R LATITUDE IN DEGREES
!   LIMDIS  I DISCONTINUITY LIMITS
!   LYR     I 1 IN LEAP YEAR, 0 OTHERWISE
!   MAP     I ARRAY OF FLAGS, CORRESPONDING TO ARRAY D
!             FLAGS ARE:-
!                   0  NORMAL DATA
!                   1  BLANK
!                   2  *
!                   3  * STRINGS WHICH BEGIN ON FIRST CARD OR END ON
!                       CARD OF MONTH IN WIND, RADIATION, OR EVAPORATION
!                   4  DATA WHICH FOLLOWS * STRINGS FLAGGED WITH 3
!                   5  DATA WHICH FOLLOWS * STRINGS FLAGGED WITH 2
!   MEAN    I MONTH AVERAGE
!   MES     I CONTAINS DISCONTINUITY ERROR MESSAGES
!   MP      I VALUES OF MAP(NCP,7-13-14)
!   MTH     I MONTH
!   NC      I NO OF DAYS IN MONTH
!   NCP     I NO OF DAYS IN PREVIOUS MONTH (INITIALLY SET AT 30)
!   NM      I MONTHLY PART OF DAY NO
!   NV      I NO OF LOW RADIATION VALUES IN A MONTH
!   PARS    I  STATION PARAMETERS OF ALL STATIONS (NAME, COORDINATES,
!             ELEVATION, STATION NO, SOLARIMETER TYPE, STATION CODE)
!   PRIN    I ARRAY OF OUTPUT DATA FOR PRINTING
!   R       I 1 STATION UNIDENTIFIED, RADIATION CHECKS CANNOT BE DONE, 0
!   RA      R RAIN
!   RAINYR  I YEAR USED IN RAIN ONLY PRINTOUT
!   RCAL    R CALCULATED CLEAR SKY RADIATION
!   RF      I RAINFLAG. IDENTIFIED BY AN 'R' IN COL 5 OF INPUT DATA
!             SIGNIFIES RECORD CONTAINS ONLY RAINFALL DATA AND SETS
!             'RF' TO 1, 0 OTHERWISE
!   RH      I RELATIVE HUMIDITY
!   RHP     I RH PREVIOUSLY CALCULATED
!   RN      R RADIATION
!   STN     I STATION CODE
!   TE      R TEMPERATURE
!   TL      R UPPER LIMIT FOR RADIATION
!   TTL     R MONTH TOTALS OF EACH ELEMENT
!   VP      R VAPOUR PRESSURE
!   Y       I INDEXING VARIABLE FOR ARRAY IRD
!   YR      I YEAR
 
!   UNITS USED ARE AS FOLLOWS:-
!                          1   OUTPUT DATA FOR PRINTING
!                          2   ERROR MESSAGES
!                          3   INPUT DATA
! *************
      DATA STN, MTH, YR/'    ', 0, 0/
 
      LAG = 0

      call day_of_year_to_date (g_day, g_year, idate)
!         COMPUTE SEASON, COMPUTE MONTHLY PART OF DAY NUMBER
!         ASSUMING FEB ALWAYS 28 DAYS.   SET UP DISCONTINUITY LIMITS
 
      IF (g_latitude.LT.-18. .OR. g_latitude.GT.-11.) then
         DS = 0
      else
         DS = idate(2)/6*(1-idate(2)/10)  ! 1 = dry season (6-9)
    
      endif
      if (ds.eq.0) then
         p_dis_maxt = p_dis_maxt_other
      else
         p_dis_maxt = p_dis_maxt_dry
      endif
 
      call jday_to_date (I, mth, yr, g_last_day)
      call day_of_year_to_date (g_day, g_year, idate)
      MTH = idate(2)
!65    continue 
!      LYR = (YR-(YR-1)/4*4)/4
      I = MTH + MTH/8
 
      LAT = g_latitude
!      IF (LAT.EQ.0E0) GO TO 10
 

      K = MTH - 1
      IF (K.GE.2) then
         NM = K*30 + (K+K/8-1)/2 - 1
      else
         NM = 31*K
      endif
!*************
 
!   SET DISCONTINUITY LIMITS
 

            call day_of_year_to_date (g_day, g_year, idate)
!        FIND INDEXES FOR PREVIOUS 2 CARDS
         I = 3 
         IA = 1
         IB = 2
 
      
      if (g_last_day + 1
     :   .eq. Date_to_jday (iDate(1), idate(2), idate(3))) then
         ! dates ok
      else
         call t2_error (LAG,2,STN,g_day,g_year,
     :                 'Dates inconsistant')
         WRITE (string, '(3x, a, 2I3, I5)') 
     :                    ' Expected date =', I, MTH, YR
         call write_string (string)
         write (string, '(1x, i7, i7)') 
     :        g_last_day, Date_to_jday (iDate(1), idate(2), idate(3))
         call write_string (string)
      endif
 
      g_last_day = Date_to_jday (iDate(1), idate(2), idate(3))
      
!********************    TEMPERATURES    *******************
 
!         CHECK DB NOT LESS THAN WB, COMPUTE WB DEPRESSION
!       CHECK IF DB=WB AND RAIN=0
!       COMPUTE AND CHECK VP,RH,ENTER RH
!       KEEP YESTERDAYS RHP(9AM) AND RH(3PM)
 
      if (g_a_day(1).gt.0) then
            g_dis_radn = ABS((g_a_radn(IA) - g_a_radn(IB))
     :         +  (g_radn - g_a_radn(IB)))
            g_dis_wind = ABS((g_a_wind(IA) - g_a_wind(IB)) 
     :         +  (g_wind - g_a_wind(IB)))
            g_dis_evap = ABS((g_a_evap(IA) - g_a_evap(IB))
     :              + (g_evap - g_a_evap(IB)))
!          *****     + EVAP X RAIN
!         CHECK FOR DISCONTINUITY IN EVAP, MAX AND MIN
 
         !print*, ' 1'
         IF (g_a_rain(IB).LE.3.0) then 
         else
            dk = (g_a_evap(IA) - g_a_evap(IB)) 
     :         + (g_evap - g_a_evap(IB))
            IF (dk.GE.p_dish_evap) then 
!            IF (dk.GE.-8.5) then 
            else
               IF (dk.LE.p_disvh_evap) then
!               IF (dk.LE.-12.0) then
                  call t2_error (LAG,2,STN, g_a_day(IB), g_a_year(IB)
     :                           ,'Very high Evaporation with rain')
               else
                  call t2_error (LAG,LAG2,STN, g_a_day(IB), g_a_year(IB)
     :            ,'High Evaporation with rain    Possible error')
               endif
               LAG2 = 1 + LAG/2
               write (string, '(3x, a, 10f6.1)') ' Values = ', g_a_evap
               call write_string (string)
               WRITE (string, '(3x, a, f6.1)') 
     :                          ' Discontinuity =', g_dis_evap
               call write_string (string)
               
               g_field = 'evap'
               g_type = 'discont'
              
            endif
         endif
         !print*, ' 2'
 
            g_dis_mint = ABS((g_a_min(IA) - g_a_min(IB)) 
     :              + (g_mint - g_a_min(IB)))
            IF (g_dis_mint.LE.p_dis_mint) then 
            else
               LAG2 = 1 + LAG/2
               call t2_error (LAG,LAG2,STN,g_a_day(IB), g_a_year(IB)
     :                     , 'Discontinuity in minT')
               write (string, '(3x, a, 10f6.1)') ' Values = ', g_a_min
               call write_string (string)
               WRITE (string, '(3x, a, f6.1)') 
     :               ' Possible error. Discontinuity =', g_dis_mint
               call write_string (string)
               IF (g_rain.EQ.0) then
                  WRITE (string, '(3x, a)') ' No rain next day'
                  call write_string (string)
               else
               endif
               g_field = 'minT'
               g_type = 'discont'
              
            endif
         !print*, ' 3'
            g_dis_maxt = ABS((g_a_max(IA) - g_a_max(IB))
     :              + (g_maxt- g_a_max(IB)))
            !!!!!print*, g_dis_maxt, p_dis_maxt
            IF (g_dis_maxt.LE.p_dis_maxt) then 
            else
               LAG2 = 1 + LAG/2
         !print*, ' 31, lag, lag2, stn, ib, g_a_day(ib)'
         !print*, ' 31', lag, lag2, stn, ib, g_a_day(ib)
               call t2_error (LAG,LAG2,STN,g_a_day(IB), g_a_year(IB)
     :                     , 'Discontinuity in maxT')
         !print*, ' 32'
               write (string, '(3x, a, 10f6.1)') ' Values = ',g_a_max
               call write_string (string)
               WRITE (string, '(3x, a, f6.1)') 
     :               ' Possible error. Discontinuity =', g_dis_maxt
               call write_string (string)
         !print*, ' 33'
               IF (g_a_rain(IB).EQ.0)  then
                  WRITE (string, '(3x, a)') ' No rain this day'
                  call write_string (string)
               else
               endif
               IF (g_rain.EQ.0)  then
                  WRITE (string, '(3x, a)') ' No rain next day'
                  call write_string (string)
               else
               endif
               g_field = 'maxT'
               g_type = 'discont'
              
            endif
         !print*, ' 34'
            IF (g_dis_evap.LE.p_dis_evap) then 
            else
               LAG2 = 1 + LAG/2
               call t2_error (LAG,LAG2,STN,g_a_day(IB), g_a_year(IB)
     :                     , 'Discontinuity in evap')
               write (string,'(3x, a, 10f6.1)') ' Values = ',g_a_evap
               call write_string (string)
               WRITE (string, '(3x, a, f6.1)') 
     :               ' Possible error. Discontinuity =', g_dis_evap
               call write_string (string)
               IF (g_rain.EQ.0) then
                  WRITE (string, '(3x, a)') ' No rain next day'
                  call write_string (string)
               else
               endif

               g_field = 'evap'
               g_type = 'discont'
              
            endif
 
         !print*, ' 4'
      else
      endif

      NV = 0
 
      I = idate(1)
      
 
!******************    RAINFALL    *******************
!       CHECK RAINFALL
 
 
      IF (g_rain.GE.p_rain_lb .AND. g_rain.LE.p_rain_ub) then
         LAG2 = 1 + LAG/2
         IF (g_rain.GT.p_gauge_capacity) then
            call t2_errorR (LAG,LAG2,STN,g_day,g_year,g_rain
     :   ,' mm Rainfall exceeds capacity of gauge   Possible error')
         g_field = 'rain'
         g_type = 'gauge'
              
         else
         endif
      else
         call t2_errorR (LAG,2,STN,g_day,g_year,g_rain
     :            ,' mm Rainfall outside limits')
         g_field = 'rain'
         g_type = 'limits'
              
      endif
 
         !print*, ' 5'
 
!*******************    EVAPORATION    *********************
      !print*, evap
      IF (g_evap.GE.p_evap_lb .AND. g_evap.LE.p_evap_ub) then 
      else
         call t2_errorr (LAG,2,STN,g_day,g_year, g_evap
     :               ,'Evaporation outside limits')
         g_field = 'evap'
         g_type = 'limits'
              
      endif
 
 
!********************    MAX TEMPERATURE    *********************
!               CHECK MAX WITHIN LIMITS
 
      IF (g_maxt.LT.p_maxt_lb .OR. g_maxt.GT.p_maxt_ub) then
         call t2_errorr (LAG,2,STN,g_day,g_year, g_maxt
     :               ,'MaxT outside limits')
         g_field = 'MaxT'
         g_type = 'limits'
              
      else
      endif
 
!*********************    MIN TEMPERATURE    ********************
!  CHECK MIN WITHIN LIMITS 
 
      IF (g_mint.GE.p_mint_lb .AND. g_mint.LE.p_mint_ub) then !GO TO 530
      else
         call t2_errorr (LAG,2,STN,g_day,g_year, g_mint
     :               ,'MinT outside limits')
         g_field = 'MinT'
         g_type = 'limits'
              
      endif

!********************    WIND    ***********************
      if (g_wind_found) then 
         IF (g_wind.GE.p_wind_lb .AND. g_wind.LE.p_wind_ub) then 
         else
            call t2_errorr (LAG,2,STN,g_day,g_year, g_wind
     :               , 'Wind run outside limitS')
         endif
         g_field = 'Wind'
         g_type = 'limits'
              
      else
      endif
         !print*, ' 6'
 
!********************    RADIATION    *******************
 
      DEC = 23.45 * DSIN(.0172 * DBLE(NM + I - 83))
      RCAL  = (318.24 * DCOS((DEC-LAT)/57.3) - 17.46) 
     :      * (1E0 + DEC * LAT/5157.0)
     :      / 10.0 
      
      radn_cal = radn_max (g_day, g_latitude, -0.83)
      !print*, g_latitude, lat, g_day  
      !print*, g_radn, rcal, radn_cal
      g_radn_rcal = rcal - g_radn
      bottom_limit = p_radn_lb*RCAL
      top_limit = p_radn_ub*RCAL
      
      g_RD(idate(1)) = g_radn*100.0/RCAL
      IF (g_radn.LT.p_radn_low*RCAL-0.05) then
         g_NV = g_NV + 1
      else
      endif
 
      IF (g_radn.GE.bottom_limit-0.5 
     :   .AND. g_radn.LE.top_limit+0.5) then 
         RCAL20 = p_radn_vlow*RCAL
         IF (g_radn.GE.int(RCAL20-0.5)) then 
         else
            call t2_errorr (LAG,LAG2,STN,g_day,g_year, g_radn
     :               ,'Radiation very low   Possible error ')
            WRITE (string, 99890) g_radn, RCAL20
99890       FORMAT (3X, ' Recorded =', F7.1, ' Min =', F6.2)
            call write_string (string)
            g_field = 'radn'
            g_type = 'VLow'
              
         endif
      else
         LAG2 = 1 + LAG/2
         call t2_errorr (LAG,LAG2,STN,g_day,g_year, g_radn
     :               ,'Radiation outside limits   Possible error ')
         WRITE (string, 99990)  g_radn, bottom_limit, top_limit
99990    FORMAT(3x, ' Recorded =', F7.1, ' Limits =', 2F10.2)
         call write_string (string)
         g_field = 'radn'
         g_type = 'limits'
              
      endif
 
 
      if (End_month (g_day, g_Year)) then 
         IF (NV.LE.idate(1)-2) then  
         else
            LAG2 = 1 + LAG/2
            call t2_error (LAG,LAG2,STN,g_day,g_year,
     :  'Radiation values well below clear sky value   Possible error')
         WRITE (string, 99989)  g_NV, ' 65% of ', RCAL20
99989    FORMAT (3x, I8, ' Values below', a, F6.2)
         call write_string (string)
         
         write (string, '(a)')' Observed values,'//
     : ' expressed as Percentage of calculated clear sky value, are:-'
         call write_string (string)
         
         write (string, '(1x, 16f6.1)') (g_rd(j), j=1, 16)
         call write_string (string)
         write (string, '(1x, 16f6.1)') (g_rd(j), j=17, idate(1))
         call write_string (string)
!         LAG = 1 + LAG/2
         endif
         do j = 1,31
            g_rd(j)    = 0.0
         enddo
         g_nv = 0
      else
      endif
 
      IF (g_maxt.le. g_mint) then
         call t2_errorr (LAG,2,STN,g_day,g_year, g_maxt
     :               ,' MaxT < or = MinT')
         write (string, '(3x, a, 2f6.1)') 
     :               ' MaxT, MinT = ', g_maxt, g_mint
         call write_string (string)
         g_field = 'MaxT'
         g_type = 'MinT'
              
      elseIF (g_maxt.le. g_mint+p_Maxt_to_minT
     :   .and. g_rain.le.0.0001) then
         if ( g_radn .gt.p_radn_low*RCAL) then
            call t2_errorr (LAG,2,STN,g_day,g_year, g_maxt
     :       ,' MaxT is close to MinT and not overcast. Possible error')
            write (string, '(3x, a, 2f6.1)') 
     :               ' MaxT, MinT = ', g_maxt, g_mint
            call write_string (string)
            g_field = 'MaxT'
            g_type = 'MinT'
         else
         endif
              
      else

      endif
 
         !print*, ' 7'
      g_error = lag
      return
      END

*================================================================
      SUBROUTINE t2_ERROR (LAG,N2,STN,K1,K3,RMESS)
*================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      !!include   'const.inc'            ! Constant definitions
      CHARACTER*4 STN
      CHARACTER RMESS*(*)
      character string*80
 
      LAG = N2
 
         !print*, ' e1'
 
      !!WRITE (string, 1000) STN,K1,K3,trim(rmess)
      WRITE (string, 1000) STN, rmess
 1000 FORMAT(A1,a)
         !print*, ' e2'
      call write_string (string)
      RETURN
      END
 
 
*================================================================
      SUBROUTINE t2_ERRORR (LAG,N2,STN,K1,K3,r4,RMESS)
*================================================================
      IMPLICIT none
      include   'tamet2a.inc'            ! Constant definitions
      character string*80
      CHARACTER*4 STN
      CHARACTER RMESS*(*)
      integer lag, N2, k1, k3
      double precision r4
 
      LAG = N2
 
 
      WRITE (string, 1000) STN,r4,rmess
      !!!!WRITE (string, 1000) STN,K1,K3,r4,trim(rmess)
 1000 FORMAT(A1,f7.1,1x,a)
      call write_string (string)
      RETURN
      END
 

*     ===========================================================
      real function radn_max (dyoyr, lat, sun_angle)
*     ===========================================================
      implicit none
 
*+ Sub-Program Arguments
      real       sun_angle             ! (INPUT) angle to measure time between
                                       ! such as twilight (deg).
                                       ! angular distance between 90 deg
                                       ! and end of twilight - altitude
                                       ! of sun. +ve up, -ve down.
      integer    dyoyr                 ! (INPUT) day of year number
      double precision       lat       ! (INPUT) latitude of site (deg)
 
*+ Purpose
*      return the time elasped in hours between the specified sun angle
*      from 90 deg in am and pm. +ve above the horizon, -ve below the horizon.
 
*+ Notes
*                    there is a small err in cos (90), thus a special
*                    case is made for this.
 
*+  Mission Statement
*      day length for %1 and %2
 
*+ Changes
*       020392 jngh specified and programmed
*       130592 jngh limited altitude for twilight to increase range_of
*                   of latitudes. - cr324
*                   limited cos of the hourangle between -1 and 1 - cr324
*       190592 jngh renamed day_length routine - cr323
*       290592 jngh set cos hourangle to +/-1 when latitude is +/- 90 - cr350
*                   corrected descriptions - cr352
*       230792 jngh corrected coshra to take account of latitude sign - cr401
*       200893 jngh corrected problem with precision which occurred when
*                   latitude is very close to tropic line and declination
*                   is also very close, abs(slsd-clcd) may go slightly above
*                   1.0, which asin doesn't like.
*       071293 jngh added sun (twilight) angle to arguments
*       270295 jngh put in function to test for equal reals.
 
*+ Calls
      real       bound                 ! function
      logical reals_are_equal          ! function
 
*+ Constant Values
      real       aeqnox                ! average day number of autumnal
      parameter (aeqnox = 82.25)       !   equinox
*
      real       pi                    ! pi
      parameter (pi =  3.14159265359)
*
      real       dg2rdn                ! convert degrees to radians
      parameter (dg2rdn = (2.0*pi) /360.0)
*
      real       decsol                ! amplitude of declination of sun
                                       !   - declination of sun at solstices.
      parameter (decsol = 23.45116 * dg2rdn)
                                       ! cm says here that the maximum
                                       ! declination is 23.45116 or 23 degrees
                                       ! 27 minutes.
                                       ! I have seen else_where that it should
                                       ! be 23 degrees 26 minutes 30 seconds -
                                       ! 23.44167
*
      real       dy2rdn                ! convert days to radians
      parameter (dy2rdn = (2.0*pi) /365.25)
*
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'radn_max')
*
      real       rdn2hr                ! convert radians to hours
      parameter (rdn2hr = 24.0/(2.0*pi))
      
      real      water_vap_abs          ! water vapour absorption
      parameter (water_vap_abs = 0.07)
 
      real      ozone_abs          ! ozone absorption
      parameter (ozone_abs = 0.02)
      
      real       transmission_coef
      parameter (transmission_coef = 0.6)
 
*+ Local Variables
      real       alt                   ! twilight altitude limited to max/min
                                       !   sun altitudes end of twilight
                                       !   - altitude of sun. (radians)
      real       altmn                 ! altitude of sun at midnight
      real       altmx                 ! altitude of sun at midday
      real       clcd                  ! cos of latitude * cos of declination
      real       coshra                ! cos of hour angle - angle between the
                                       !   sun and the meridian.
      real       dec                   ! declination of sun in radians - this
                                       !   is the angular distance at solar
                                       !   noon between the sun and the equator.
      real       hrangl                ! hour angle - angle between the sun
                                       !   and the meridian (radians).
      real       hrlt                  ! day_length in hours
      real       latrn                 ! latitude in radians
      real       slsd                  ! sin of latitude * sin of declination
      real       sun_alt               ! angular distance between
                                       ! sunset and end of twilight - altitude
                                       ! of sun. (radians)
                                       ! Twilight is defined as the interval
                                       ! between sunrise or sunset and the
                                       ! time when the true centre of the sun
                                       ! is 6 degrees below the horizon.
                                       ! Sunrise or sunset is defined as when
                                       ! the true centre of the sun is 50'
                                       ! below the horizon.
      real       radius_vector         ! radius vector of earth
      real       ra                    ! radiation at top of atmosphere (MJ/m2)
      real       rs                    ! radiation at surface (MJ/m2)
      real       diffuse_sky_radn
      real       cosz
      
      real       solar_const           ! MJ/m2/hr
      parameter (solar_const = 4.871)
      real      A
      parameter (A=0.25)
      real      B
      parameter (B=0.50)
 
*- Implementation Section ----------------------------------
 
 
      sun_alt = sun_angle * dg2rdn
 
          ! calculate daylangth in hours by getting the
          ! solar declination (radians) from the day of year, then using
          ! the sin and cos of the latitude.
 
          ! declination ranges from -.41 to .41 (summer and winter solstices)
 
      dec = decsol*sin (dy2rdn* (real(dyoyr) - aeqnox))
 
          ! get the max and min altitude of sun for today and limit
          ! the twilight altitude between these.
 
      if (reals_are_equal(abs(lat), 90.0)) then
         coshra = sign (1.0, -dec) * sign (1.0, lat)
      else
         latrn = lat*dg2rdn
         slsd = sin(latrn)*sin(dec)
         clcd = cos(latrn)*cos(dec)
 
         altmn = asin (bound (slsd - clcd, -1.0, 1.0))
         altmx = asin (bound (slsd + clcd, -1.0, 1.0))
         alt = bound (sun_alt, altmn, altmx)
 
             ! get cos of the hour angle
 
         coshra = (sin (alt) - slsd) /clcd
         coshra = bound (coshra, -1.0, 1.0)
      endif
 
          ! now get the hour angle and the hours of light
 
      hrangl = acos (coshra)
      radius_vector = 1.0/sqrt(1.0+0.033*cos(dy2rdn*real(dyoyr)))
      ra = (24.0/pi)
     :   * (solar_const/radius_vector**2)
     :   * (hrangl*sin(latrn)*sin(dec) 
     :      + sin(hrangl)*cos(latrn)*cos(dec))
!      cosz = (slsd + clcd*cos(hrangl))/dg2rdn
      rs = ra*(A + B)
!      rs = transmission_coef ** (1.0/cosz) * ra
      diffuse_sky_radn = (ra*(1.0-ozone_abs-water_vap_abs) - rs)*0.5
      radn_max = rs  + diffuse_sky_radn
      
      !print*, hrangl, radius_vector, lat, latrn, dec
 
      return
      end

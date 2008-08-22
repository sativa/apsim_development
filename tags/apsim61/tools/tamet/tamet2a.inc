      integer offset
      parameter (offset = 3)

      integer   g_dummy
      character g_string*80
      character g_type*10
      character g_field*10

      logical   g_wind_found
      integer   g_a_day(offset)
      integer   g_a_year(offset)
      double precision   g_a_rain(offset)
      double precision   g_a_min(offset)
      double precision   g_a_max(offset)
      double precision   g_a_evap(offset)
      double precision   g_a_radn(offset)
      double precision   g_a_wind(offset)
      integer   g_day
      integer   g_year
      integer   g_error
      double precision   g_rain
      double precision   g_evap
      double precision   g_maxt
      double precision   g_mint
      double precision   g_wind
      double precision   g_radn
      integer            g_last_day
      double precision      g_latitude
      double precision   g_rd(31)
      integer g_nv
      double precision    g_radn_rcal
      double precision   g_dis_evap
      double precision   g_dis_maxt
      double precision   g_dis_mint
      double precision   g_dis_wind
      double precision   g_dis_radn

      common /tglobals/ 
     +     g_rain,
     +     g_evap,
     +     g_maxt,
     +     g_mint,
     +     g_wind,
     +     g_radn,
     +     g_latitude,
     +     g_radn_rcal,
     +     g_dis_evap,
     +     g_dis_maxt,
     +     g_dis_mint,
     +     g_dis_wind,
     +     g_dis_radn,
     +     g_a_rain,
     +     g_a_min,
     +     g_a_max,
     +     g_a_evap,
     +     g_a_radn,
     +     g_a_wind,
     +     g_rd,
     +     g_last_day,
     +     g_day,
     +     g_year,
     +     g_error,
     +     g_nv,
     +     g_dummy, g_string, g_type, g_field,
     +     g_wind_found,
     +     g_a_day,
     +     g_a_year

*     ================================================================
      double precision     p_dis_evap      
      double precision     p_dish_evap      
      double precision     p_disvh_evap      
      double precision     p_dis_maxt      
      double precision     p_dis_maxt_dry      
      double precision     p_dis_maxt_other      
      double precision     p_dis_mint      
      double precision     p_Maxt_to_minT  
      double precision     p_rain_lb       
      double precision     p_rain_ub       
      double precision     p_gauge_capacity
      double precision     p_maxt_lb       
      double precision     p_maxt_ub       
      double precision     p_mint_lb       
      double precision     p_mint_ub       
      double precision     p_evap_lb       
      double precision     p_evap_ub       
      double precision     p_wind_lb       
      double precision     p_wind_ub       
      double precision     p_radn_lb       
      double precision     p_radn_ub       
      double precision     p_radn_low      
      double precision     p_radn_vlow     

      common /tparams/ 
     +     p_dis_evap,
     +     p_dish_evap,      
     +     p_disvh_evap,      
     +     p_dis_maxt,      
     +     p_dis_maxt_dry,      
     +     p_dis_maxt_other,      
     +     p_dis_mint,      
     +     p_Maxt_to_minT,  
     +     p_rain_lb,       
     +     p_rain_ub,       
     +     p_gauge_capacity,
     +     p_maxt_lb,       
     +     p_maxt_ub,       
     +     p_mint_lb,       
     +     p_mint_ub,       
     +     p_evap_lb,       
     +     p_evap_ub,       
     +     p_wind_lb,       
     +     p_wind_ub,       
     +     p_radn_lb,       
     +     p_radn_ub,       
     +     p_radn_low,      
     +     p_radn_vlow     


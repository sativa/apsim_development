#if !defined(__params_h)              // Sentry, use file only if it's not already included.
#define __params_h

#include <gobjs\gstring.h>
#include <gobjs\gdate.h>
#define MAX_INI_DEPTHS   10            // Maximum num. of depths allowed in .ini
#define MAX_INI_SOILS    6             // maximum num. of soils allowed in .ini
#define NUM_WEEKS        53            // Maximum num. of weeks in year
#define NUM_SOIL_LAYERS  3             // Number of soil layers.
#define NUM_MONTHS       12            // Number of months in year
#define MAX_SITES        20            // maximum number of sites

// *******************************************************************
       class Parameters : public TStreamableBase {
// *******************************************************************

//  Short description:
//    Parameter objects

//  Notes:

//  Changes:
//    DPH 7/10/95

// ------------------------- Declaration section -------------------------

//   Data
//      none

//   Member functions
protected:
      // Site variables

      String_array Site_list;          // List of all sites from .ini file
      int User_current_site;           // Current site

      // Soil variables

      String_array Soil_list;          // soil list from .ini file

      int Num_ini_depths;              // Number of .ini depths specified in .ini
      float Ini_depths[MAX_INI_DEPTHS];// Depths read from .ini file
      float Ini_waters[MAX_INI_SOILS][MAX_INI_DEPTHS];
                                       // Waters read from .ini file
      float CN2[MAX_INI_SOILS];        // Curve numbers
      float air_dry[MAX_INI_SOILS];    // airdry

      int User_current_soil;           // Current soil type
      float User_soil_water;           // user specified soil water
      float User_soil_depth;           // Current soil depth
      float User_SW_init_perc;         // Initial soil water (%)
      float User_SW_init_mm;           // Initial soil water (mm)

      float Calc_soil_water_max[NUM_SOIL_LAYERS];
                                       // calculated maximum soil waters
      int Calc_layer_depths[NUM_SOIL_LAYERS];
                                       // calculated soil layer depths (mm)

      // Organic Carbon / Nitrogen variables

      float Init_orgn[MAX_INI_SOILS];  // Initial organic N
      float Half_life[MAX_INI_SOILS];  // Half life
      float Start_n[MAX_INI_SOILS];    // Starting N
      float Final_n[MAX_INI_SOILS];    // Final N
      float minz_coeff[MAX_INI_SOILS]; // Mineralization coefficient
      float ksat[MAX_INI_SOILS];       // ksat
      float User_age_cultivation;      // Age of cultivation
      float User_organic_carbon;       // Organic carbon entered by user
      float Calc_organic_carbon;       // Organic carbon content (%)
      float CN_ratio;                  // Carbon / Nitrogen ratio
      float Bulk_density;              // bulk density
      float nhed;                      // Number of layers to use as hydrol.
                                       // effective depth.
      String_array kgno3_perc_tonne_list;
                                       // list of all kgno3/%/tonne figures from
                                       // .ini file - used in water-n balance table

      // Miscellaneous variables

      float Ini_evap[NUM_WEEKS];       // Pan. Evaporations from .ini file
      float Ini_temp[NUM_WEEKS];       // Temp. from .ini file
      float Avg_monthly_rainfall[NUM_MONTHS];
                                       // Average monthly rainfalls.
      float Uritch;                    // Uritch from .ini
      float User_slope;                // Slope (%)
      float Start_soil_cover;          // Starting soil cover (%)
      float End_soil_cover;            // Ending soil cover (%)
      GDate Start_date;                // Start date for simulation
      GDate End_date;                  // End date for simulation

      // USER SPECIFIED Parameters

      void Read_site_from_ini(void);   // read a site from .ini file
      void Copy_array(GString& St,
                      float Arr[],
                      int& Num_read,
                      int Max_to_read);
                                       // copy an array of real values from string.
      void Read_weekly_data(const char *Key_name, float Arr[]);
                                       // read all weekly data from .ini file
                                       // into specified array.
      void Read_defaults_from_ini(void);
                                       // read defaults from .ini file
      void Write_defaults_to_ini(void);
                                       // write defaults to .ini file
      void Calc_PAWC(void);            // Calcualte PAWC.
      void Calc_ORGC(void);            // Calcualte organic carbon
      void Get_key(GString& Line, const char *Key,
                   GString& Key_value);// Get key value from line.

   public:
      Parameters(void);                // constructor
      ~Parameters(void);               // destructor

      // ****** GET routines ******

      const char *Get_site_name(void)
         {
         return Site_list[User_current_site].c_str();
         };                            // Return current site
      const char *Get_soil_name(void)
         {
         return Soil_list[User_current_soil].c_str();
         };                            // Return current soil type
      void Get_calc_soil_water_max(float sw_max[NUM_SOIL_LAYERS])
         {
         Calc_PAWC();
         sw_max[0] = Calc_soil_water_max[0];
         sw_max[1] = Calc_soil_water_max[1];
         sw_max[2] = Calc_soil_water_max[2];
         };                            // Return calculate soil water max
      float Get_calc_soil_water_max(void)
         {
         Calc_PAWC();
         return Calc_soil_water_max[0] +
                Calc_soil_water_max[1] +
                Calc_soil_water_max[2];
         };                            // Return calculate soil water max
      void Get_init_soil_water(float sw[NUM_SOIL_LAYERS]);
                                       // Return initial soil water to caller
      float Get_init_soil_water(void);
                                       // Return total initial soil water
      void Get_calc_layer_depths(int Layer_depths[NUM_SOIL_LAYERS])
         {
         Calc_PAWC();
         Layer_depths[0] = Calc_layer_depths[0];
         Layer_depths[1] = Calc_layer_depths[1];
         Layer_depths[2] = Calc_layer_depths[2];
         };                            // Return calculate depth layers
      void Get_soil_list(String_array * *Soil_list_ptr)
         {
         *Soil_list_ptr = &Soil_list;
         };                            // return soil descriptions to caller
      void Get_kgno3_perc_tonne_list(String_array * *kgno3_perc_tonne_ptr)
         {
         *kgno3_perc_tonne_ptr = &kgno3_perc_tonne_list;
         };                            // return soil descriptions to caller
      void Get_ini_soil_depths(float *Soil_depths[])
         {
         *Soil_depths = Ini_depths;
         };                            // return soil descriptions to caller
      void Get_site_list(String_array * *Site_list_ptr)
         {
         *Site_list_ptr = &Site_list;
         };                            // return soil descriptions to caller
      int Get_current_site(void)
         {
         return User_current_site;
         };                            // return current site to caller

      int Get_current_soil(void)
         {
         return User_current_soil;
         };                            // return current soil to caller
      float Get_user_soil_depth(void)
         {
         return User_soil_depth;
         };                            // return user soil depth to caller
      float Get_user_soil_water_max(void)
         {
         return User_soil_water;
         };                            // return user soil water to caller
      float Get_user_age_cultivation(void)
         {
         return User_age_cultivation;
         };                            // return user age cultivation
      float Get_calc_organic_carbon(void)
         {
         return Calc_organic_carbon;
         };                            // return calculated organic carbon
      float Get_user_organic_carbon(void)
         {
         return User_organic_carbon;
         };                            // return user organic carbon
      float Get_user_slope(void)
         {
         return User_slope;
         };                            // return user slope

      float Get_start_soil_cover(void)
         {                                          
         return Start_soil_cover;
         };                            // Return starting soil cover (%)
      float Get_end_soil_cover(void)
         {
         return End_soil_cover;
         };                            // Return ending soil cover (%)
      GDate Get_start_date(void)
         {
         return Start_date;
         };                            // Return start date for simulation
      GDate Get_end_date(void)
         {
         return End_date;
         };                            // Return end date for simulation
      float Get_User_SW_init_perc(void)
         {
         return User_SW_init_perc;
         };                            // Return initial soil water (%)

      float Get_User_SW_init_mm(void)
         {
         return User_SW_init_mm;
         };                            // Return initial soil water (mm)

      float Get_cn_ratio(void)
         {
         return CN_ratio;
         };                            // return CN ratio to caller
      float Get_bulk_density(void)
         {
         return Bulk_density;
         };                            // return bulk density to caller
      float Get_evap(GDate& Date)
         {
         return Ini_evap[Date.Get_day_of_year() / 7];
         };                            // return evap for given day
      float Get_temp(GDate& Date)
         {
         return Ini_temp[Date.Get_day_of_year() / 7];
         };                            // return temp for given day
      float Get_cn2(void)
         {
         return CN2[User_current_soil];
         };                            // return curve number to caller.
      float Get_air_dry(void)
         {
         return air_dry[User_current_soil];
         };                            // return air dry
      float Get_ksat(void)
         {
         return ksat[User_current_soil];
         };                            // return ksat to caller
      float Get_minz_coeff(void)
         {
         return minz_coeff[User_current_soil];
         };                            // return mineralisation coefficient to caller
      float Get_start_n(void)
         {
         return Start_n[User_current_soil];
         };                            // Return starting N to caller
      float Get_uritch(void)
         {
         return Uritch;
         };                            // return uritch to caller
      float Get_nhed(void)
         {
         return nhed;
         };                            // return nhed to caller


      float Get_average_rain(GDate& Start_date, GDate& End_date);
                                       // return longterm average rainfall for period.
      float Get_average_rain_for_day(GDate& Date);
                                       // return longterm average rainfall for a day
      float Get_average_rain_for_month(int Month)
         {
         return Avg_monthly_rainfall[Month-1];
         }                             // return longterm average monthly rain.

      // ****** SET routines ******

      void Set_current_site(int Site)
         {
         User_current_site = Site;
         Read_site_from_ini();
         };                            // Set current site
      void Set_soil_water_max(float Total_water)
         {
         User_soil_water = Total_water;
         User_soil_depth = 0.0;
         Calc_PAWC();
         };                            // Set soil water

      void Set_current_soil(int Soil)
         {
         User_current_soil = Soil;
         Calc_PAWC();
         };                            // set current soil
      void Set_soil_depth(float Depth)
         {
         User_soil_depth = Depth;
         User_soil_water = 0.0;
         Calc_PAWC();
         };                            // set soil depth
      void Set_age_cultivation(float Age_cult)
         {
         User_age_cultivation = Age_cult;
         User_organic_carbon = 0.0;
         Calc_ORGC();
         };                            // set age cultivation
      void Set_organic_carbon(float Orgc)
         {
         User_organic_carbon = Orgc;
         User_age_cultivation = 0.0;
         Calc_ORGC();
         };                            // set organic carbon
      void Set_slope(float S)
         {
         User_slope = S;
         };                            // set slope
      void Set_start_soil_cover(float Cover)
         {
         Start_soil_cover = Cover;
         };                            // set starting soil cover (%)
      void Set_end_soil_cover(float Cover)
         {
         End_soil_cover = Cover;
         };                            // set ending soil cover (%)
      void Set_start_date(GDate& Date)
         {
         Start_date = Date;
         };                            // set start date for simulation
      void Set_end_date(GDate& Date)
         {
         End_date = Date;
         };                            // set end date for simulation
      void Set_User_SW_init_perc(float Init)
         {
         User_SW_init_perc = Init;
         User_SW_init_mm = 0.0;
         };                            // set initial soil water (%)

      void Set_User_SW_init_mm(float Init)
         {
         User_SW_init_mm = Init;
         User_SW_init_perc = 0.0;
         };                            // set initial soil water (mm)
      void Edit(TWindow& Parent);      // Allow user to change climate region


   };

#endif



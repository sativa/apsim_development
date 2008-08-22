#if !defined(__simul_h)              // Sentry, use file only if it's not already included.
#define __simul_h

static const int MAX_DAYS = 700;
static const int MAX_LAYERS = 3;

#include "params.h"

class RainfallFile;
// *******************************************************************
       class Simul  {
// *******************************************************************

//  Short description:
//    Simulation object

//  Notes:

//  Changes:
//    DPH 7/10/95

// ------------------------- Declaration section -------------------------

//   Data
//      none

//   Member functions

   protected:
      Parameters *Param_ptr;           // pointer to parameters object
      ofstream Out_stream;             // output stream.
      RainfallFile* rainfallFile;

      // Stats about the simulation

      float Total_rain;                // Total rainfall in simulation (mm)
      float Max_rain;                  // Maximum rainfall in simulation (mm)
      float Total_runoff;              // Total runoff in simulation (mm)
      float Total_evaporation;         // Total evaporation in simulation (mm)
      float Total_drainage;            // Total drainage in simulation (mm)
      int Num_days;                    // Number of days in simulation
      float Rain_15;                   // Amount of rainfall in falls < 15mm
      float Rain_15_50;                // Amount of rainfall in falls from 15 to 50 mm
      float Rain_50;                   // Amount of rainfall in falls > 50mm

      // Dates

      GDate Start_date;                // Starting date
      GDate End_date;                  // Ending date

      // Current day variables.

      float Soil_water[MAX_LAYERS];    // Current day's soil water (mm)
      float Soil_water_max[MAX_LAYERS];// Maximum soil water for each layer (mm)
      int Soil_depth[MAX_LAYERS];      // Soil depth (mm)
      float Nitrate_n;                 // NO3
      GDate Current_date;              // Current date
      float Rain;                      // Rainfall for today (mm)
      float Runoff;                    // Runoff for today (mm)
      float Soil_cover;                // Soil cover for today (%)
      float Drainage;                  // Drainage for today (mm)
      float Soil_evap;                 // Soil evaporation for today (mm)

      // Variables used in calculations.

      float g_sumes2;                  // Used in soil evap routine
      float g_sumes1;                  // Used in soil evap routine
      float g_t;                       // Used in soil evap routine
      float CN2_mod;                   // Used in runoff routine

      // Saved variables for entire simulation

      float Saved_soil_water[MAX_LAYERS][MAX_DAYS];
                                       // Saved calculated soil water for each day.
      float Saved_nitrate[MAX_DAYS];   // Saved nitrate for each day.
      float Saved_rainfall[MAX_DAYS];  // Saved rainfall
      float Saved_runoff[MAX_DAYS];    // Saved runoff
      float Saved_evap[MAX_DAYS];      // Saved evaporation

      void Zero_all_variables (void);  // Zero all simulation variables.
      float Calc_soil_evap (GDate& Current_date, float Rain, float Runoff, float Soil_cover);
                                       // calculate actual evaporation from soil
                                       // surface (es)
      float Calc_crop_cover(GDate& Current_date,
                            GDate& Start_date,
                            GDate& End_date);
                                       // Calculate crop cover for today.
      float Calc_runoff (float Rain, float Crop_cover);
                                       // Calculate runoff for today.
      void Calc_one_day (GDate& Current_date,
                         float& Rain, float& Runoff,
                         float& Soil_cover,
                         float& Drainage, float& Soil_evap);
                                       // Go perform simulation for one day only.
      void Do_water_balance (float Rain, float& Runoff);
                                       // Go perform simulation for one day only.
      float Calc_drain (void);         // Calculate drainage - Creams method for
                                       // calculating drainage.
      float Calc_minz(GDate& Current_date);
                                       // Calculate mineralized N
      void Init_evap (void);
                                       // initialise the soil evap routine.

	public:
      Simul(Parameters *Param_ptr, RainfallFile* rainfallFile);    // constructor
      void Go(TWindow *Parent);        // Go perform simulation

      // ****** Get routines ******

      float Get_total_rain(void)
         {
         return Total_rain;
         };                            // Return total rainfall to caller
      float Get_total_evaporation(void)
         {
         return Total_evaporation;
         };                            // Return total evaporation to caller
      float Get_total_runoff(void)
         {
         return Total_runoff;
         };                            // Return total runoff to caller
      float Get_total_drainage(void)
         {
         return Total_drainage;
         };                            // Return total drainage to caller
      float Get_final_soil_water(void)
         {
         return Saved_soil_water[0][Num_days-1] +
                Saved_soil_water[1][Num_days-1] +
                Saved_soil_water[2][Num_days-1];
         };                            // Return final soil water to caller
      float Get_init_nitrate(void)
         {
         return Saved_nitrate[0];
         };                            // Return initial nitrate to caller
      float Get_final_nitrate(void)
         {
         return Saved_nitrate[Num_days-1];
         };                            // Return final nitrate to caller
      float Get_rain_15(void)
         {
         return Rain_15;
         };                            // Return amount of rainfall in falls < 15mm

      float Get_rain_15_50(void)
         {
         return Rain_15_50;
         };                            // Return amount of rainfall in falls from 15 to 50 mm

      float Get_rain_50(void)
         {
         return Rain_50;
         };                            // Return amount of rainfall in falls > 50mm

      float Get_todays_surface_moisture(void)
         {
         return Soil_water[0];
         };                            // Return todays surface moisture.
      float Get_todays_soil_water(void)
         {
         return Soil_water[0] + Soil_water[1] + Soil_water[2];
         };                            // Return todays soil water
      float Get_todays_rain_less_runoff(void)
         {
         return Rain - Runoff;
         };                            // Return todays effective rainfall.
      float Get_todays_rain(void)
         {
         return Rain;
         };                            // Return todays rainfall.
      float Get_todays_runoff(void)
         {
         return Runoff;
         };                            // Return todays runoff.
      float Get_todays_nitrate(void)
         {
         return Nitrate_n;
         };                            // Return todays nitrate
      float Get_todays_soil_loss (float Slope_percent,
                                  float Slope_length,
                                  float K);
                                       // return todays soil loss
      GDate Get_todays_date(void);
                                       // Return todays date

      void Start_simul (TWindow *Parent);
                                       // Go start the simulation
      bool Simul_today (void);         // Run simulation for next day.
      void End_simul (void);           // Go end the simulation

      int Get_num_days(void)
         {
         return Num_days;
         };                            // return number of days to caller
      float Get_saved_surface_moisture (int Index)
         {
         return Saved_soil_water[0][Index];
         };                            // Return saved calculated soil water for a day.
      float Get_saved_soil_water (int Index)
         {
         return Saved_soil_water[1][Index] + Saved_soil_water[2][Index];
         };                            // Return saved calculated soil water for a day.
      float Get_saved_nitrate (int Index)
         {
         return Saved_nitrate[Index];
         };                            // Return saved nitrate for a day.
      float Get_saved_rainfall (int Index)
         {
         return Saved_rainfall[Index];
         };                            // Return saved rainfall for a day
      float Get_saved_runoff (int Index)
         {
         return Saved_runoff[Index];
         };                            // Return saved runoff for a day
      float Get_saved_evap (int Index)
         {
         return Saved_evap[Index];
         };                            // Return saved evaporation for a day

	};

#endif

/*  Project howwet
    APSRU
    Copyright © 1995. All Rights Reserved.

    SUBSYSTEM:    howwet.apx Application
    FILE:         climate.cpp
    AUTHOR:


    OVERVIEW
    ========
    Source file for implementation of Parameters
*/

#include <owl\owlpch.h>
#pragma hdrstop

#include <consts.h>
#include "params.h"

#include <gobjs\ginifile.h>
#include <gobjs\brkupstr.h>
#include <math.h>
#include <dir.h>
#include <stdio.h>

extern GIni_file Howwet_ini_file;      // global ini file object

// *******************************************************************
      Parameters::Parameters(void)
         : Site_list(10, 0, 10), Soil_list(10, 0, 10),
           kgno3_perc_tonne_list(10, 0, 10) {
// *******************************************************************

//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 7/10/95

//  Internal variables

// -------------------- Executable code section ----------------------

   Read_defaults_from_ini();
   Read_site_from_ini();

   Calc_PAWC ();
   Calc_ORGC ();
   }

// *******************************************************************
      Parameters::~Parameters(void)  {
// *******************************************************************

//  Short description:
//    destructor

//  Notes:

//  Changes:
//    DPH 7/10/95

//  Internal variables

// -------------------- Executable code section ----------------------

   Write_defaults_to_ini();
   }

// *******************************************************************
      void Parameters::Copy_array(GString& St,
                                  float Arr[],
                                  int& Num_read,
                                  int Max_to_read)  {
// *******************************************************************

//  Short description:
//    copy an array of real values from string.

//  Notes:

//  Changes:
//    DPH 7/10/95

//  Internal variables
      GString Msg;                     // Error message.
      String_array St_array(10, 0, 10);
                                       // String broken up.

// -------------------- Executable code section ----------------------

   Break_up_string(St, St_array);

   // Loop through each element of array and convert sting to real and store

   for (int Index = 0;
        Index < St_array.GetItemsInContainer();
        Index++)
      {
      if (Index >= Max_to_read)
         {
         // Problem - Too many values found.

         Msg = "Too many values were found on line in howwet.ini.  Line = ";
         Msg += St;
         throw Msg;
         }
      Arr[Index] = St_array[Index].Get_real();
      }
   Num_read = St_array.GetItemsInContainer();
   }

// *******************************************************************
      void Parameters::Read_weekly_data(const char *Key_name,
                                        float Arr[])  {
// *******************************************************************

//  Short description:
//    read all weekly data from .ini file into specified array.

//  Notes:

//  Changes:
//    DPH 7/10/95

//  Internal variables
      char Key[100];                   // key
      GString St;                      // String read from .ini file
      int Num;                         // Num read from line.

// -------------------- Executable code section ----------------------

   for (int Index = 1;
        Index <= 51;
        Index += 10)
      {
      // Create a keyname

      wsprintf(Key, "%s%i", Key_name, Index);
      Howwet_ini_file.Read(Get_site_name(), Key, St);

      Copy_array(St, &Arr[Index - 1], Num, 10);
      }
   Arr[52] = Arr[51];
   }

// *******************************************************************
      void Parameters::Read_site_from_ini(void)  {
// *******************************************************************

//  Short description:
//    read a site from .ini file

//  Notes:

//  Changes:
//    DPH 7/10/95

//  Internal variables
      GString St;                      // String from .ini
      String_array St_array(10, 0, 10);// String broken up.
      int Num;                         // temp. variable.

// -------------------- Executable code section ----------------------

   // Remove all soils from system.

   Soil_list.Flush();

   // Get depths from .ini

   Howwet_ini_file.Read(Get_site_name(), "depths", St);
   Copy_array(St, Ini_depths, Num_ini_depths, MAX_INI_DEPTHS);

   // Get waters from .ini

   Howwet_ini_file.Read_list(Get_site_name(), "soil", St_array);
   if (St_array.GetItemsInContainer() > MAX_INI_SOILS)
      {
      // Problem - throw exception

      St = "Too many soils are specified in howwet.ini file for site : ";
      St += Get_site_name();
      throw St;
      }

   for (int Soil_index = 0;
        Soil_index < St_array.GetItemsInContainer();
        Soil_index++)
      {
      istrstream In_stream((char*) St_array[Soil_index].c_str());

      for (int Depth = 0;
           Depth < Num_ini_depths;
           Depth++)
         {
         In_stream >> Ini_waters[Soil_index][Depth];
         }
      In_stream >> CN2[Soil_index];
      In_stream >> St;
      Init_orgn[Soil_index] = St.Get_real();

      In_stream >> St;
      Half_life[Soil_index] = St.Get_real();

      In_stream >> St;
      Start_n[Soil_index] = St.Get_real();

      In_stream >> St;
      Final_n[Soil_index] = St.Get_real();

      In_stream >> St;
      minz_coeff[Soil_index] = St.Get_real();

      In_stream >> St;
      ksat[Soil_index] = St.Get_real();

      In_stream >> St;
      air_dry[Soil_index] = St.Get_real();

      In_stream >> ws;
      getline(In_stream, St);
      Soil_list.Add(St);
      }

   // Read in evaporations and temperatures.

   Read_weekly_data("evap_week", Ini_evap);
   Read_weekly_data("temp_week", Ini_temp);

   // Read in average monthly rainfalls.

   Howwet_ini_file.Read(Get_site_name(), "average_monthly_rain", St);
   Copy_array(St, Avg_monthly_rainfall, Num, NUM_MONTHS);

   Howwet_ini_file.Read(Get_site_name(), "uritch", St);
   Uritch = St.Get_real();

   }

// *******************************************************************
      void Parameters::Read_defaults_from_ini(void)  {
// *******************************************************************

//  Short description:
//    read all defaults from .ini file

//  Notes:

//  Changes:
//    DPH 7/10/95

//  Internal variables
      GString St;                      // String from .ini

// -------------------- Executable code section ----------------------

   Howwet_ini_file.Read("defaults", "User_soil_water", St);
   User_soil_water = St.Get_real();

   Howwet_ini_file.Read("defaults", "Soil_depth", St);
   User_soil_depth = St.Get_real();

   Howwet_ini_file.Read("defaults", "Current_soil", St);
   User_current_soil = St.Get_integer();

   Howwet_ini_file.Read("defaults", "Current_site", St);
   User_current_site = St.Get_integer();

   Howwet_ini_file.Read("defaults", "age_cultivation", St);
   User_age_cultivation = St.Get_real();

   Howwet_ini_file.Read("defaults", "organic_carbon", St);
   User_organic_carbon = St.Get_real();

   Howwet_ini_file.Read("defaults", "slope", St);
   User_slope = St.Get_real();

   Howwet_ini_file.Read("defaults", "SW_init_mm", St);
   User_SW_init_mm = St.Get_real();

   Howwet_ini_file.Read("defaults", "SW_init_%full", St);
   User_SW_init_perc = St.Get_real();

   Howwet_ini_file.Read("defaults", "Start_soil_cover", St);
   Start_soil_cover = St.Get_real();

   Howwet_ini_file.Read("defaults", "End_soil_cover", St);
   End_soil_cover = St.Get_real();

   Howwet_ini_file.Read("n defaults", "cn_ratio", St);
   CN_ratio = St.Get_real();

   Howwet_ini_file.Read("n defaults", "Bulk_density", St);
   Bulk_density = St.Get_real();

   Howwet_ini_file.Read("n defaults", "nhed", St);
   nhed = St.Get_real();

   // Read in all sites from .ini file.

   Howwet_ini_file.Read_list("sites", "site", Site_list);

   // Read in all kg no3/perc/tonne figures.

   Howwet_ini_file.Read_list("defaults", "kgno3/%/tonne", kgno3_perc_tonne_list);
   }

// *******************************************************************
      void Parameters::Write_defaults_to_ini(void)  {
// *******************************************************************

//  Short description:
//    write all defaults to .ini file

//  Notes:

//  Changes:
//    DPH 7/10/95

//  Internal variables
      GString St;                      // String from .ini

// -------------------- Executable code section ----------------------

   St.Set_integer(User_current_site);
   Howwet_ini_file.Write("defaults", "Current_site", St);

   St.Set_real(User_soil_water);
   Howwet_ini_file.Write("defaults", "User_soil_water", St);

   St.Set_real(User_soil_depth);
   Howwet_ini_file.Write("defaults", "Soil_depth", St);

   St.Set_real(User_current_soil);
   Howwet_ini_file.Write("defaults", "Current_soil", St);

   St.Set_real(User_age_cultivation);
   Howwet_ini_file.Write("defaults", "age_cultivation", St);

   St.Set_real(User_organic_carbon);
   Howwet_ini_file.Write("defaults", "organic_carbon", St);

   St.Set_real(User_slope);
   Howwet_ini_file.Write("defaults", "slope", St);

   St.Set_real(User_SW_init_mm);
   Howwet_ini_file.Write("defaults", "SW_init_mm", St);

   St.Set_real(User_SW_init_perc);
   Howwet_ini_file.Write("defaults", "SW_init_%full", St);

   St.Set_real(Start_soil_cover);
   Howwet_ini_file.Write("defaults", "Start_soil_cover", St);

   St.Set_real(End_soil_cover);
   Howwet_ini_file.Write("defaults", "End_soil_cover", St);
   }

// ********************************************************
// Interpolation routine.  If Row = 0 then start at position 0 in x array
// ********************************************************
float Interp(float x[], int Num_x, float y[], float Key, int Row)
	{
	float Prop;                         // Proportion of number
	bool Interpolate = FALSE;           // Are we ready to interpolate yet ?

	// Go look for Key in x's

	while (!Interpolate)
		{
		if (Row == Num_x)
			{
         Row -= 2;
         Interpolate = TRUE;
         }

		else if (Key <= x[Row])
			Interpolate = TRUE;

		else
			{
			Interpolate = FALSE;
         Row++;
         }
      }

	// Interpolate between numbers

	Prop = (Key - x[Row]) / (x[Row+1] - x[Row]);
	return y[Row] + Prop * (y[Row+1] - y[Row]);
	}

// *******************************************************************
      void Parameters::Calc_PAWC(void)  {
// *******************************************************************

//  Short description:
//    Calcualte PAWC.

//  Notes:

//  Changes:
//    DPH 7/10/95

//  Internal variables
      GString St;                      // String from .ini
      float Pawc;                      // Calcualted pawc
      float Total_depth;               // Total depth (m)

// -------------------- Executable code section ----------------------

   if (User_soil_depth > 0.0)
      {
      Total_depth = User_soil_depth * 100.0;
      Pawc = Interp(Ini_depths,
	   	           Num_ini_depths,
		              Ini_waters[User_current_soil],
			   	     Total_depth,
                    0);
      }
   else
      {
      Pawc = User_soil_water;
		Total_depth = Interp(Ini_waters[User_current_soil],
                           Num_ini_depths,
                           Ini_depths,
                           Pawc,
                           0);
      }

   Pawc = max(Pawc, (float) 0.0);
   Total_depth = max(Total_depth, (float) 0.0);

   // Calculate layer depths.

   Calc_layer_depths[0] = max(Ini_depths[0], (float) 0.0);
   Calc_soil_water_max[0] = min(Ini_waters[User_current_soil][0], Pawc);

   Calc_layer_depths[1] = Ini_depths[1];
   Calc_soil_water_max[1] = Ini_waters[User_current_soil][1];
   if (Calc_layer_depths[0] + Calc_layer_depths[1] > Total_depth)
      {
      Calc_layer_depths[1] = Total_depth - Calc_layer_depths[0];
      Calc_soil_water_max[1] = Pawc - Calc_soil_water_max[0];
      }

   Calc_layer_depths[2] = max(Total_depth - Calc_layer_depths[0] - Calc_layer_depths[1], (float) 0.0);
   Calc_soil_water_max[2] = max(Pawc - Calc_soil_water_max[0] - Calc_soil_water_max[1], (float) 0.0);
   }

// *******************************************************************
      void Parameters::Get_init_soil_water(float sw[NUM_SOIL_LAYERS])  {
// *******************************************************************

//  Short description:
//    Return initial soil water to caller

//  Notes:

//  Changes:
//    DPH 7/10/95

//  Internal variables
      float Percent;                   // Percent full

// -------------------- Executable code section ----------------------

   float mm = Get_User_SW_init_mm();
   if (mm > 0)
      Percent = mm / Get_calc_soil_water_max();
   else
      Percent = Get_User_SW_init_perc() / 100.0;

   for (int Layer = 0;
        Layer < NUM_SOIL_LAYERS;
        Layer++)
      {
      sw[Layer] = Calc_soil_water_max[Layer] * Percent;
      }
   }

// *******************************************************************
      float Parameters::Get_init_soil_water(void)  {
// *******************************************************************

//  Short description:
//    Return total initial soil water to caller

//  Notes:

//  Changes:
//    DPH 7/10/95

//  Internal variables
      float sw[NUM_SOIL_LAYERS];       // soil water for each layer

// -------------------- Executable code section ----------------------

   Get_init_soil_water(sw);
   return sw[0] + sw[1] + sw[2];
   }

// *******************************************************************
      void Parameters::Calc_ORGC(void)  {
// *******************************************************************

//  Short description:
//    Calcualte organic carbon

//  Notes:

//  Changes:
//    DPH 7/10/95

//  Internal variables
      float Calc_organic_nitrogen;     // calculated organic nitrogen.

// -------------------- Executable code section ----------------------

   if (User_age_cultivation > 0)
      {
      float Decay_constant = log(2) / Half_life[User_current_soil];
      Calc_organic_nitrogen = Final_n[User_current_soil] +
            (Init_orgn[User_current_soil] - Final_n[User_current_soil])
             * exp(-Decay_constant * User_age_cultivation);
      Calc_organic_carbon = Calc_organic_nitrogen * CN_ratio;
      }
   else
      Calc_organic_carbon = User_organic_carbon;
   }

// *******************************************************************
      void Parameters::Get_key(GString& Line, const char *Key,
                               GString& Key_value)  {
// *******************************************************************

//  Short description:
//    Get key value from line.

//  Notes:

//  Changes:
//    DPH 28/10/95

//  Internal variables

// -------------------- Executable code section ----------------------

   Key_value = Line;
   if (Key_value.find(Key) != NPOS && Key_value.find("=") != NPOS)
      {
      // Yep got the key.  Return it to caller.

      size_t Pos = Key_value.find("=") + 1;

      Key_value = Key_value.substr(Pos);
      Key_value.Strip(string::Both);
      }
      else
         Key_value = "";
   }

// *******************************************************************
      float Parameters::Get_average_rain(GDate& Start_date,
                                         GDate& End_date)  {
// *******************************************************************

//  Short description:
//    return longterm average rainfall for period.

//  Notes:

//  Changes:
//    DPH 28/10/95

//  Internal variables
      GDate Date;                      // our dummy date
      float Total = 0.0;               // Total so far

// -------------------- Executable code section ----------------------

   for (Date = Start_date;
        Date < End_date;
        Date++)
      {
      Total += Get_average_rain_for_day(Date);
      }

   return Total;
   }

// *******************************************************************
      float Parameters::Get_average_rain_for_day(GDate& Date)  {
// *******************************************************************

//  Short description:
//    Get longterm average rainfall for a day

//  Notes:

//  Changes:
//    DPH 28/10/95

//  Internal variables

// -------------------- Executable code section ----------------------

   int Days_in_month = Date.Get_days_in_month(Date.Get_month(),
                                              Date.Get_year());

   // Return proportion of rainfall for given day

   return (Avg_monthly_rainfall[Date.Get_month() - 1] * 1.0 / Days_in_month);
   }


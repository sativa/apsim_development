// OWLCVT 01/15/94 11:01:40
#include <owl\owlpch.h>
#pragma hdrstop

#include <consts.h>
#include <gobjs\gstring.h>

#include "simul.h"

#include <math.h>
#include <strstrea.h>
#include <iomanip.h>
#include <owl\inputdia.h>
#include "howwet.rh"
#include <values.h>
#include "Rainfall\HowwetRainfall.h"

// *******************************************************************
      Simul::Simul (Parameters *Param_p, RainfallFile* rainfallF)  {
// *******************************************************************

//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 2/11/95

//  Internal variables

// -------------------- Executable code section ----------------------

   Param_ptr = Param_p;
   rainfallFile = rainfallF;
   }

// *******************************************************************
      void Simul::Zero_all_variables (void)  {
// *******************************************************************

//  Short description:
//    Zero all simulation variables.

//  Notes:

//  Changes:
//    DPH 2/11/95

//  Internal variables

// -------------------- Executable code section ----------------------

   Total_rain = 0.0;
   Max_rain = -MAXFLOAT;
   Total_runoff = 0.0;
   Total_evaporation = 0.0;
   Total_drainage = 0.0;
   Num_days = 0;
   Nitrate_n = Param_ptr->Get_start_n();
   Rain_15 = 0.0;
   Rain_15_50 = 0.0;
   Rain_50 = 0.0;
   g_sumes1 = 0.0;
   g_sumes2 = 0.0;
   g_t = 0.0;

   for (int Day = 0;
        Day < MAX_DAYS;
        Day++)
      {
      for (int Layer = 0;
           Layer < MAX_LAYERS;
           Layer++)
         {
         Saved_soil_water[Layer][Day] = 0.0;
         }
      Saved_nitrate[Day] = 0.0;
      Saved_rainfall[Day] = 0.0;
      Saved_runoff[Day] = 0.0;
      Saved_evap[Day] = 0.0;
      }
   }

// *******************************************************************
      void Simul::Start_simul (TWindow *)  {
// *******************************************************************

//  Short description:
//    Go get ready to start the simulation

//  Notes:

//  Changes:
//    DPH 2/11/95

//  Internal variables
      char File_name[90];              // File name

// -------------------- Executable code section ----------------------

   // Set write format for date.

   Current_date.Set_write_option(GDate::DD_MMM_YYYY);

   // Ask user for name of output file

   strcpy(File_name, "howwet.out");
//   TInputDialog(Parent, "DEBUG screen only",
//                        "Enter name of output file to create : ",
//                        File_name,
//                        sizeof(File_name)).Execute();

   Out_stream.open(File_name);

	// Write headings to stream

	Out_stream << "  Date          Rain   Crop_cov     Runoff  Soil_evap";
	Out_stream << " Soil_water1 Soil_water2 Soil_water3 Airdry";
	Out_stream << " Soil_water_max1  Soil_water_max2  Soil_water_max3     NitrateN";
   Out_stream << "            CN2" << endl;
	Out_stream << setprecision(3);

   // Get initial conditions from parameters object.

   Start_date = Param_ptr->Get_start_date ();
   End_date = Param_ptr->Get_end_date ();
   Current_date = Start_date;
   if (!Start_date.Is_valid())
      throw string("You need to specify a start and end date for the simulation first");

   Param_ptr->Get_calc_soil_water_max(Soil_water_max);
   Param_ptr->Get_calc_layer_depths(Soil_depth);
   Param_ptr->Get_init_soil_water(Soil_water);

   Soil_cover = Param_ptr->Get_start_soil_cover();

   Zero_all_variables ();

   // initialise the soil evep routine.

   Init_evap ();
   }

// ------------------------------------------------------------------
//  Short description:
//    initialise the soil evap routine.

//  Notes:

//  Changes:
//    DPH 14/9/1996

// ------------------------------------------------------------------
void Simul::Init_evap (void)
   {
   float pan = Param_ptr->Get_evap(Current_date);
   float cona = 0.6222 * pan + 1.711;
   float uritch = Param_ptr->Get_uritch();
   float air_dry = Param_ptr->Get_air_dry();

   // NB airdry is negative.
   float sumes2max = Soil_water_max[0] - air_dry - uritch;

   if (Soil_water_max[0] - Soil_water[0] < uritch)
      {
      // in first stage

      g_sumes1 = Soil_water_max[0] - Soil_water[0];
      if (g_sumes1 < 0.0)
         g_sumes1 = 0.0;
      g_sumes2 = 0.0;
      g_t = 0;
      }
   else
      {
      // second stage evap.

      g_sumes1 = uritch;
      g_sumes2 = Soil_water_max[0] - Soil_water[0] - uritch;
      if (g_sumes2 > sumes2max)
         {
         // initial soil water < airdry

         g_sumes2 = sumes2max;
         Soil_water[0] = air_dry;
         }
      g_t = pow(g_sumes2 / cona, 2);
      }
   }

// *******************************************************************
      bool Simul::Simul_today (void)  {
// *******************************************************************

//  Short description:
//    Go run the simulation for today.  Return TRUE if more days to go.

//  Notes:

//  Changes:
//    DPH 2/11/95

//  Internal variables

// -------------------- Executable code section ----------------------

   Calc_one_day(Current_date,
                Rain, Runoff, Soil_cover,
                Drainage, Soil_evap);

   // Save the nitrate and soil water to saved arrays.

   Saved_soil_water[0][Num_days] = Soil_water[0];
   Saved_soil_water[1][Num_days] = Soil_water[1];
   Saved_soil_water[2][Num_days] = Soil_water[2];
   Saved_nitrate[Num_days] = Nitrate_n;
   Saved_rainfall[Num_days] = Rain;
   Saved_runoff[Num_days] = Runoff;
   Saved_evap[Num_days] = Soil_evap;

   // Write out data to output stream

   Out_stream.setf(ios::fixed, ios::floatfield);
   Current_date.Write(Out_stream);
   Out_stream << setw(11) << setprecision(1) << Rain;
   Out_stream << setw(11) << setprecision(1) << Soil_cover;
   Out_stream << setw(11) << setprecision(1) << Runoff;
   Out_stream << setw(11) << setprecision(1) << Soil_evap;
   Out_stream << setw(11) << setprecision(1) << Soil_water[0];
   Out_stream << setw(11) << setprecision(1) << Soil_water[1];
   Out_stream << setw(11) << setprecision(1) << Soil_water[2];
   Out_stream << setw(11) << setprecision(1) << Param_ptr->Get_air_dry();
   Out_stream << setw(15) << setprecision(1) << Soil_water_max[0];
   Out_stream << setw(15) << setprecision(1) << Soil_water_max[1];
   Out_stream << setw(15) << setprecision(1) << Soil_water_max[2];
   Out_stream << setw(15) << setprecision(1) << Nitrate_n;
   Out_stream << setw(15) << setprecision(1) << CN2_mod;

   Out_stream << endl;

   Num_days++;
   Current_date++;

   return (Current_date <= End_date);
   }

// *******************************************************************
      void Simul::End_simul (void)  {
// *******************************************************************

//  Short description:
//    Go end the simulation

//  Notes:

//  Changes:
//    DPH 2/11/95

//  Internal variables

// -------------------- Executable code section ----------------------

   Out_stream.close();

   }

// *******************************************************************
      void Simul::Go (TWindow *Parent)  {
// *******************************************************************

//  Short description:
//    Go run the simulation

//  Notes:

//  Changes:
//    DPH 2/11/95

//  Internal variables

// -------------------- Executable code section ----------------------

   Start_simul(Parent);
   if (Start_date.Is_valid())
      {
      // Display hourglass cursor

      ::SetCursor(LoadCursor(NULL, IDC_WAIT));

      // Put up dialog box saying "please wait"

      TDialog Please_wait(Parent, ID_PLEASE_WAIT);
      Please_wait.Create();

      // Perform soil water simulation

      while (Simul_today());

      // End the simulation.

      End_simul();

      // Display arrow cursor

      ::SetCursor(LoadCursor(NULL, IDC_ARROW));

      // Remove dialog box

      Please_wait.CloseWindow();
      }
	}

// *******************************************************************
      void Simul::Calc_one_day (GDate& Current_date,
                                float& Rain, float& Runoff,
                                float& Soil_cover,
                                float& Drainage, float& Soil_evap)  {
// *******************************************************************

//  Short description:
//    Go perform simulation for one day only.

//  Notes:

//  Changes:
//    DPH 2/11/95

//  Internal variables

// -------------------- Executable code section ----------------------

   // Get rainfall for today.

   Rain = getRain(rainfallFile, Current_date.Get_jday());
   Total_rain += Rain;
   if (Rain > Max_rain)
      Max_rain = Rain;
   if (Rain >= 50.0)
      Rain_50 += Rain;

   else if (Rain >= 15.0)
      Rain_15_50 += Rain;

   else
      Rain_15 += Rain;

   // Calculate crop cover for today

   Soil_cover = Calc_crop_cover(Current_date,
                                Param_ptr->Get_start_date(),
                                Param_ptr->Get_end_date());

   // Calculate runoff for today

   Runoff = Calc_runoff(Rain, Soil_cover);
   Total_runoff += Runoff;

   // Do water balance for today

   Do_water_balance (Rain, Runoff);

   // Calculate drainage from bottom layer.

   Drainage = Calc_drain();
   Soil_water[2] -= Drainage;
   Total_drainage += Drainage;

   // Calculate soil evaporation

   Soil_evap = Calc_soil_evap(Current_date, Rain, Runoff, Soil_cover);
   Total_evaporation += Soil_evap;

   // Take total soil evap out of layer 1.

   Soil_water[0] -= Soil_evap;
   Soil_water[0] = max(Soil_water[0], Param_ptr->Get_air_dry());

   // Calculate Nitrate N

   Nitrate_n = Nitrate_n + Calc_minz(Current_date);

   }

// *******************************************************************
      void Simul::Do_water_balance (float Rain, float& Runoff)  {
// *******************************************************************

//  Short description:
//    Go perform simulation for one day only.

//  Notes:

//  Changes:
//    DPH 2/11/95

//  Internal variables

// -------------------- Executable code section ----------------------

   // Put water in top layer if possible.  If it can't fit then
   // put remainder in next layer.  Tipping bucket.

   Soil_water[0] += Rain - Runoff;

   for (int Layer_num = 0;
            Layer_num < 2;
            Layer_num++)
      {
      float Remainder = Soil_water[Layer_num] - Soil_water_max[Layer_num];
      if (Remainder > 0)
         {
         // Won't fit in layer.  Put into next layer

         Soil_water[Layer_num + 1] += Remainder;
         Soil_water[Layer_num] = Soil_water_max[Layer_num];
         }
      }

   // Any water in layer 3 over the maximum is sent to runoff.

   if (Soil_water[2] > Soil_water_max[2])
      {
      float Remainder = Soil_water[2] - Soil_water_max[2];
      Runoff += Remainder;
      Total_runoff += Remainder;
      Soil_water[2] = Soil_water_max[2];
      }
   }

// *******************************************************************
      float Simul::Calc_drain (void)  {
// *******************************************************************

//  Short description:
//    Calculate drainage - Creams method for calculating drainage.

//  Notes:

//  Changes:
//    DPH 2/11/95

//  Internal variables
   float Drainage;

// -------------------- Executable code section ----------------------

   if (Soil_water[2] >= Soil_water_max[2] &&
       Soil_water[2] != 0.0)
      {

//    Here PERFECT code has been corrected to equal that of CREAMS.

//      Drain_factor = 48.0 / ((2.0 * SAT - Soil_water_max[2]) /
//                             (Param_ptr->Get_ksat() + 24.0));

//      Drainage = Drain_factor * (Soil_water[2] - Soil_water_max[2]);
      Drainage = 0.0;
      }
   else
      Drainage = 0.0;

   return Drainage;
   }

// *******************************************************************
      float Simul::Calc_minz(GDate& Current_date)  {
// *******************************************************************

//  Short description:
//    Calculate mineralized N

//  Notes:

//  Changes:
//    DPH 2/11/95

//  Internal variables

// -------------------- Executable code section ----------------------

   float Org_n = Param_ptr->Get_calc_organic_carbon() / Param_ptr->Get_cn_ratio();

   float Moist_factor = (Soil_water[0] - (Param_ptr->Get_air_dry() * 0.25)) /
                  (Soil_water_max[0] - (Param_ptr->Get_air_dry() * 0.25));
   Moist_factor = max(Moist_factor, (float) 0.0);

   float potm = 0.097 * (43 + 970 * Org_n) *10 * 2;

	float Temp_factor = exp(15.807 - 6350 / (Param_ptr->Get_temp(Current_date) + 273));

	return potm * Moist_factor * (1 - exp (-Temp_factor));
	}

// *******************************************************************
      float Simul::Calc_soil_evap
      	(GDate& Current_date,
          float Rain, float Runoff, float Soil_cover)  {
// *******************************************************************

//  Short description:
//    ****** calculate actual evaporation from soil surface (es) ******
//    most es takes place in two stages: the constant rate stage
//    and the falling rate stage (philip, 1957).  in the constant
//    rate stage (stage 1), the soil is sufficiently wet for water
//    be transported to the surface at a rate at least equal to the
//    evaporation potential (eos).
//    in the falling rate stage (stage 2), the surface soil water
//    content has decreased below a threshold value, so that es
//    depends on the flux of water through the upper layer of soil
//    to the evaporating site near the surface.

//  Notes:

//  Changes:
//    DPH 2/11/95

//  Internal variables
   float      eos;                  // potential rate of evaporation (mm/day)
   float      eos_max;              // upper limit of soil evaporation (mm/day)

   float      esoil1;               // actual soil evap in stage 1
   float      esoil2;               // actual soil evap in stage 2

   float      esoil;                // actual soil evap (mm)
   float      sumes1_max;           // upper limit of sumes1
   float      w_inf;                // infiltration into top layer (mm)
   float      cona;

// -------------------- Executable code section ----------------------

   eos = Param_ptr->Get_evap(Current_date);

   // related to average min & max wkly av pan across sites.

   cona = 0.6222 * eos + 1.711;
   float newt = pow(g_sumes2 / cona, 2);
   if (newt != g_t)
      {
      // cona has changed could be a problem due to rounding errors
      // adjust t to compensate.

      g_t = newt;
      }


   // Modify eos due to cover

   float Residue_wt = Soil_cover * 6.0 / 100.0;

   eos = eos * exp(-0.22 * Residue_wt);

   eos_max = Soil_water[0] - Param_ptr->Get_air_dry();

   sumes1_max = Param_ptr->Get_uritch();
   w_inf = Rain - Runoff;

      // if infiltration, reset sumes1
      // reset sumes2 if infil exceeds sumes1

   if (w_inf > 0.0)
      {

      g_sumes2 = max ((float) 0.0, g_sumes2 - max ((float) 0.0, w_inf-g_sumes1));
      g_sumes1 = max ((float) 0.0, g_sumes1 - w_inf);

         // update t (incase sumes2 changed)

      g_t = pow(g_sumes2 / cona, 2.0);
      }

      // are we in stage1 ?

   if (g_sumes1 < sumes1_max)
      {

         // we are in stage1
         // set esoil1 = potential, or limited by u.

      esoil1 = min (eos, sumes1_max - g_sumes1);

      if (eos > esoil1 && esoil1 < eos_max)
         {

         if (g_sumes2 > 0.0)
            {
            g_t = g_t + 1.0;
            esoil2 = min (eos - esoil1, (float) (cona *
                     pow(g_t, 0.5) - g_sumes2));
            }
         else
            esoil2 = 0.6 * (eos - esoil1);
         }
      else
            // no deficit (or esoil1.eq.eos_max,) no esoil2 on this day
         esoil2 = 0.0;


            // check any esoil2 with lower limit of evaporative sw.
      esoil2 = min (esoil2, eos_max - esoil1);

            // update 1st and 2nd stage soil evaporation.

      g_sumes1 = g_sumes1 + esoil1;
      g_sumes2 = g_sumes2 + esoil2;
      g_t = pow(g_sumes2 / cona, 2.0);
      }
   else
      {
         // no 1st stage drying. calc. 2nd stage

      esoil1 = 0.0;

      g_t = g_t + 1.0;
      esoil2 = min (eos, (float) (cona * pow(g_t, 0.5) - g_sumes2));

         // check with lower limit of evaporative sw.

      esoil2 = min (esoil2, eos_max);

         // update 2nd stage soil evaporation.

      g_sumes2 = g_sumes2 + esoil2;
      }

   esoil = esoil1 + esoil2;

      // make sure we are within bounds

   esoil = max(esoil, (float) 0.0);
   esoil = min(esoil, eos);
   esoil = min(esoil, eos_max);

   return esoil;
   }

// *******************************************************************
      float Simul::Calc_crop_cover(GDate& Current_date,
                                   GDate& Start_date,
                                   GDate& End_date)  {
// *******************************************************************

//  Short description:
//    Calculate crop cover for today.

//  Notes:

//  Changes:
//    DPH 2/11/95

//  Internal variables
      float Soil_cover;

// -------------------- Executable code section ----------------------

   // Ramp cover from start to end.

   long Total_days = End_date - Start_date;
   long  Num_days_into_simulation = Current_date - Start_date;

   float Cover_incr = (Param_ptr->Get_start_soil_cover() -
                       Param_ptr->Get_end_soil_cover()) * 1.0 / Total_days;

   Soil_cover = Param_ptr->Get_start_soil_cover() - Num_days_into_simulation *
                Cover_incr;

	return Soil_cover;
	}

// *******************************************************************
      float Simul::Calc_runoff (float Rain, float Crop_cover)  {
// *******************************************************************

//  Short description:
//    Calculate runoff for today.

//  Notes:

//  Changes:
//    DPH 2/11/95

//  Internal variables
	float Runoff;                       // Calculated runoff
	float CN1;                          // Calculated CN1
	float SMX;                          // Calculated SMX
	float S;                            // Calculated S
	const float CN2_slope = -20.0/80;   // CN2 to crop cover slope
	float WF[3];                        // Weighting factor for 3 layers
   float CN2_reduct;                   // CN2 reduction
   float HED = 0.0;                    // Hydrol effective depth = Rooting depth
   float Cum_depth = 0.0;              // cummulative depth
   float Cum_WF = 0.0;                 // Cummulative weighting factor.
   int nhed_depth = Param_ptr->Get_nhed();

// -------------------- Executable code section ----------------------

   // Calculate the weighting factor for top 2 layers (WF[0] and WF[1]).

   for (int Layer = 0;
        Layer < MAX_LAYERS;
        Layer++)
      {
      WF[Layer] = 0.0;
      }
   for (int Layer = 0;
        Layer < nhed_depth;
        Layer++)
      {
      HED = HED + Soil_depth[Layer];
      }

   for (int Layer = 0;
        Layer < nhed_depth;
        Layer++)
      {
      Cum_depth = Cum_depth + Soil_depth[Layer];
      WF[Layer] = 1.016 * (1.0 - exp(-4.16 * (Cum_depth / HED))) - Cum_WF;
      Cum_WF = Cum_WF + WF[Layer];
      }

	CN2_reduct = Crop_cover * CN2_slope;
   if (CN2_reduct < -20.0)
      CN2_reduct = -20.0;
   CN2_mod = Param_ptr->Get_cn2() + CN2_reduct;

	CN1 = -16.91 + 1.348 * CN2_mod - 0.01379 * pow(CN2_mod, 2) +
			0.00011777 * pow(CN2_mod, 3);
	SMX = 254 * (100.0 / CN1 - 1);
   if (Soil_depth[2] == 0.0)
   	S = SMX * (1.0 - (WF[0] * ((Soil_water[0] - Param_ptr->Get_air_dry()) /
                                 (Soil_water_max[0] - Param_ptr->Get_air_dry()))
                      + WF[1] * (Soil_water[1] / Soil_water_max[1])
                        )
                );
   else
   	S = SMX * (1.0 - (WF[0] * ((Soil_water[0] - Param_ptr->Get_air_dry()) /
                                 (Soil_water_max[0] - Param_ptr->Get_air_dry()))
                     + WF[1] * (Soil_water[1] / Soil_water_max[1])
                     + WF[2] * (Soil_water[2] / Soil_water_max[2])
                        )
                );

	if (Rain > 0.2 * S)
		{
		Runoff = pow(Rain - 0.2 * S, 2) / (Rain + 0.8 * S);
		Runoff = min(Runoff, Rain);
		Runoff = max(Runoff, (float) 0.0);
		}

	else
		Runoff = 0.0;

	return Runoff;
	}

// *******************************************************************
      GDate Simul::Get_todays_date(void)  {
// *******************************************************************

//  Short description:
//    return todays date to caller.

//  Notes:

//  Changes:
//    DPH 2/11/95

//  Internal variables
      GDate Return_date = Current_date;

// -------------------- Executable code section ----------------------
   Return_date--;

   return Return_date;
   };

// ------------------------------------------------------------------
//  Short description:
//    return todays soil loss

//  Notes:

//  Changes:
//    DPH 8/8/1996

// ------------------------------------------------------------------
float Simul::Get_todays_soil_loss (float Slope_percent,
                                   float Slope_length,
                                   float K)
   {
   // calculate exponent.

   float Exponent;
   if (Slope_percent > 4.9)
      Exponent = 0.5;
   else if (Slope_percent > 3.5)
      Exponent = 0.4;
   else if (Slope_percent > 1)
      Exponent = 0.3;
   else if (Slope_percent > 0.1)
      Exponent = 0.2;
   else
      Exponent = 0.1;

   // calculate ls.

   float sin_tan = sin(tan(Slope_percent / 100.0));
   float LS = pow(Slope_length * 3.28 / 72.6, Exponent);
   LS = LS * (65.41 * sin_tan * sin_tan + 4.56 * sin_tan + 0.065);

   // calculate cover

   float C;
   if (Soil_cover < 50)
      C = 16.25 - 0.46 * Soil_cover + 0.0031 * pow(Soil_cover, 2);
   else
      C = -0.0254 * Soil_cover + 2.54;

   // P = 1.0  ie. ignore

   return LS * K * C * Runoff / 10.0;
   }


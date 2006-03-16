/*  Project howwet
    APSRU
    Copyright © 1995. All Rights Reserved.

    SUBSYSTEM:    howwet.apx Application
    FILE:         scon_dlg.cpp
    AUTHOR:


    OVERVIEW
    ========
    Class definition for Soil_conditions_dlg (TDialog).
*/

#include <owl\owlpch.h>
#pragma hdrstop

#include <consts.h>
#include <consts.rh>
#include "params.h"
#include "scon_dlg.h"
#include <stdio.h>
#include "so_what.h"
#include "Rainfall\Howwetrainfall.h"
#include <gobjs\gdate.h>
//
// Build a response table for all messages/commands handled
// by the application.
//
DEFINE_RESPONSE_TABLE1(Soil_conditions_dlg, TDialog)
//{{Soil_conditions_dlgRSP_TBL_BEGIN}}
    EV_EN_CHANGE(IDC_EDIT2, Perc_full_change),
    EV_EN_CHANGE(IDC_EDIT3, mm_full_change),
    EV_BN_CLICKED(INFO1, Help1),
    EV_BN_CLICKED(INFO2, Help2),
//{{Soil_conditions_dlgRSP_TBL_END}}
END_RESPONSE_TABLE;


//{{Soil_conditions_dlg Implementation}}

Soil_conditions_dlg::Soil_conditions_dlg (TWindow* parent, Parameters *Param_p,
                                          RainfallFile* rainfallF,
                                          TResId resId, TModule* module):
    TDialog(parent, resId, module), rainfallFile(rainfallF)
{
    // INSERT>> Your constructor code here.

   Start_date_control = new TEdit(this, IDC_EDIT1, 11);
   Perc_full_control = new TEdit(this, IDC_EDIT2);
   mm_full_control = new TEdit(this, IDC_EDIT3);
   Start_cover_control = new TEdit(this, IDC_EDIT4);
   End_date_control = new TEdit(this, IDC_EDIT5, 11);
   End_cover_control = new TEdit(this, IDC_EDIT6);
   PAWC_control = new TStatic(this, IDC_STATIC1);
   Met_range_label = new TStatic(this, IDC_STATIC2);

   Param_ptr = Param_p;

}


Soil_conditions_dlg::~Soil_conditions_dlg ()
{
    Destroy();

    // INSERT>> Your destructor code here.

}


// *******************************************************************
      void Soil_conditions_dlg::SetupWindow ()  {
// *******************************************************************

//  Short description:

//  Notes:

//  Changes:
//    DPH

//  Internal variables
		char St[20];

// -------------------- Executable code section ----------------------


    TDialog::SetupWindow();

   // INSERT>> Your code here.

   Param_ptr->Get_start_date().Write(St);
   Start_date_control->SetText(St);

	if (Param_ptr->Get_User_SW_init_mm() > 0.0)
		{
		sprintf (St, "%3.0f", Param_ptr->Get_User_SW_init_mm());
   	mm_full_control->SetText(St);
      Set_pawc_static();
		}
	else
      {
		sprintf (St, "%3.0f", Param_ptr->Get_User_SW_init_perc());
      Perc_full_control->SetText(St);
      Set_pawc_static();
		}

	sprintf(St, "%3.0f", Param_ptr->Get_start_soil_cover());
   Start_cover_control->SetText(St);

   Param_ptr->Get_end_date().Write(St);
   End_date_control->SetText(St);

	sprintf(St, "%3.0f", Param_ptr->Get_end_soil_cover());
   End_cover_control->SetText(St);

   unsigned minJDate, maxJDate;
   getRainfallDateRange(rainfallFile, minJDate, maxJDate);
   GDate minDate;
   minDate.Set(minJDate);
   GDate maxDate;
   maxDate.Set(maxJDate);

   minDate.Write(St);
   string metRangeText = "Weather file date range: ";
   metRangeText += St;
   maxDate.Write(St);
   metRangeText += " to ";
   metRangeText += St;
   Met_range_label->SetText(metRangeText.c_str());
   }

// *******************************************************************
      void Soil_conditions_dlg::Set_pawc_static (void)  {
// *******************************************************************

//  Short description:
//    Set the value of the PAWC static control

//  Notes:

//  Changes:
//    DPH 28/10/95

//  Internal variables
      char St[50];
      GString Str;

// -------------------- Executable code section ----------------------

   mm_full_control->GetText(St, sizeof(St));
   if (strlen(St) > 0)
      {
      // We have amount of mm already.

      }
   else
      {
      Perc_full_control->GetText(St, sizeof(St));
      float Percent = atof(St);
      sprintf(St, "%5.0f", Percent / 100.0 * Param_ptr->Get_calc_soil_water_max());
      }

   PAWC_control->SetText(St);
   }

// *******************************************************************
      void Soil_conditions_dlg::Perc_full_change ()  {
// *******************************************************************

//  Short description:
//    Percent full edit box was changed so send a settext to
//    mm full box.

//  Notes:

//  Changes:
//    DPH 28/10/95

//  Internal variables
      char St[50];

// -------------------- Executable code section ----------------------


   // INSERT>> Your code here.

   Perc_full_control->GetText(St, sizeof(St));
   if (strlen(St) > 0)
      mm_full_control->SetText("");

   Set_pawc_static();
   }


// *******************************************************************
      void Soil_conditions_dlg::mm_full_change ()  {
// *******************************************************************

//  Short description:
//    mm full edit box has changed

//  Notes:

//  Changes:
//    DPH 28/10/95

//  Internal variables
      char St[50];

// -------------------- Executable code section ----------------------


   // INSERT>> Your code here.

   mm_full_control->GetText(St, sizeof(St));
   if (strlen(St) > 0)
      Perc_full_control->SetText("");

   Set_pawc_static();
   }


// *******************************************************************
      bool Soil_conditions_dlg::CanClose ()  {
// *******************************************************************

//  Short description:

//  Notes:

//  Changes:
//    DPH 

//  Internal variables
      char St[300];
      GString Str;
      GDate Start_date;                // Start date of simulation
      GDate End_date;                  // End date of simulation

// -------------------- Executable code section ----------------------


    bool result;

    result = TDialog::CanClose();

   // INSERT>> Your code here.

   // Check the validity of dates

   Start_date_control->GetText(St, sizeof(St));
   Start_date.Read(St);
   if (!Start_date.Is_valid())
      {
      MessageBox("Invalid starting date of estimation",
                 "Error",
                 MB_OK | MB_ICONSTOP);
      result = FALSE;
      }
   End_date_control->GetText(St, sizeof(St));
   End_date.Read(St);
   if (!End_date.Is_valid())
      {
      MessageBox("Invalid ending date of estimation",
                 "Error",
                 MB_OK | MB_ICONSTOP);
      result = FALSE;
      }

   // Check that the start date is not > than the end date

   if (result)
      {
      if (Start_date >= End_date)
         {
         MessageBox("Your start date of estimation is greater than the end "
                    "date of estimation",
                    "Error",
                    MB_OK | MB_ICONSTOP);
         result = FALSE;
         }
      }

   // IF all ok so far then make sure the simulation period is
   // no greater than 18 months

   if (result)
      {
      if (End_date - Start_date > 550)
         {
         MessageBox("The difference between your start date and end date is "
                    "too great.  Please reduce interval to less than 18 months",
                    "Error",
                    MB_OK | MB_ICONSTOP);
         result = FALSE;
         }
      }

   // check the soil water boxes.

   Perc_full_control->GetText(St, sizeof(St));
   if (strlen(St) > 0)
      {
      Str = St;
      if (Str.Get_integer() < 0 || Str.Get_integer() > 100)
         {
         MessageBox("The percentage full soil water must be between 0 and 100%",
                    "Error",
                    MB_OK | MB_ICONSTOP);
         result = FALSE;
         }
      }
   else
      {
      mm_full_control->GetText(St, sizeof(St));
      Str = St;
      if (Str.Get_real() > Param_ptr->Get_calc_soil_water_max())
         {
         sprintf(St, "The mm of soil water cannot be greater than the maximum soil water "
                     "(%4.0fmm) specified on the soil type screen",
                 Param_ptr->Get_calc_soil_water_max());
         MessageBox(St, "Error", MB_OK | MB_ICONSTOP);
         result = FALSE;
         }
      }


   // Check the starting soil cover

   Start_cover_control->GetText(St, sizeof(St));
   Str = St;
   if (Str.Get_integer() < 0 || Str.Get_integer() > 100)
      {
      MessageBox("The starting soil cover must be between 0 and 100%",
                 "Error",
                 MB_OK | MB_ICONSTOP);
      result = FALSE;
      }

   // Check the ending soil cover

   End_cover_control->GetText(St, sizeof(St));
   Str = St;
   if (Str.Get_integer() < 0 || Str.Get_integer() > 100)
      {
      MessageBox("The ending soil cover must be between 0 and 100%",
                 "Error",
                 MB_OK | MB_ICONSTOP);
      result = FALSE;
      }
   if (result)
      {
      unsigned minJDate, maxJDate;
      getRainfallDateRange(rainfallFile, minJDate, maxJDate);
      GDate minDate;
      minDate.Set(minJDate);
      GDate maxDate;
      maxDate.Set(maxJDate);
      if (Start_date < minDate || End_date > maxDate)
         {
         char st[30];
         minDate.Write(st);
         string msg = "The start or end date of the simulation is not between ";
         msg += st;
         msg += " and ";
         maxDate.Write(st);
         msg += st;
         MessageBox(msg.c_str(),
                    "Error",
                    MB_OK | MB_ICONSTOP);
         result = FALSE;
         }
      }

   // IF all ok then we can save the settings to the parameters object.
   if (result)
      {
      Perc_full_control->GetText(St, sizeof(St));
      Str = St;
      if (Str.length() > 0)
         Param_ptr->Set_User_SW_init_perc(Str.Get_real());
      else
         {
         mm_full_control->GetText(St, sizeof(St));
         Str = St;
         Param_ptr->Set_User_SW_init_mm(Str.Get_real());
         }

      Start_cover_control->GetText(St, sizeof(St));
      Str = St;
      Param_ptr->Set_start_soil_cover(Str.Get_real());
      End_cover_control->GetText(St, sizeof(St));
      Str = St;
      Param_ptr->Set_end_soil_cover(Str.Get_real());

      // Save the dates back to the param. object.

      Param_ptr->Set_start_date(Start_date);
      Param_ptr->Set_end_date(End_date);
      }
   return result;
   }


// *******************************************************************
      void Soil_conditions_dlg::Help1 ()  {
// *******************************************************************

//  Short description:

//  Notes:

//  Changes:
//    DPH

//  Internal variables
//    none

// -------------------- Executable code section ----------------------


   // INSERT>> Your code here.

   So_what_dlg(this, 2, 1).Execute();
   }


// *******************************************************************
      void Soil_conditions_dlg::Help2 ()  {
// *******************************************************************

//  Short description:

//  Notes:

//  Changes:
//    DPH 

//  Internal variables
//    none

// -------------------- Executable code section ----------------------


   // INSERT>> Your code here.

   So_what_dlg(this, 2, 2).Execute();
   }


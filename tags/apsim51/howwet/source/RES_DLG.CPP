/*  Project howwet
    APSRU
    Copyright © 1995. All Rights Reserved.

    SUBSYSTEM:    howwet.apx Application
    FILE:         res_dlg.cpp
    AUTHOR:


    OVERVIEW
    ========
    Source file for implementation of Results_dialog (TDialog).
*/

#include <owl\owlpch.h>
#pragma hdrstop

#include <consts.h>
#include "res_dlg.h"
#include <stdio.h>
#include "so_what.h"
#include <consts.rh>
#include <gobjsx\texttble.h>
#include "Rainfall\HowwetRainfall.h"

//
// Build a response table for all messages/commands handled
// by the application.
//
DEFINE_RESPONSE_TABLE1(Results_dialog, TDialog)
//{{Results_dialogRSP_TBL_BEGIN}}
    EV_BN_CLICKED(PRINT, Print_button),
    EV_BN_CLICKED(INFO1, Help1),
    EV_BN_CLICKED(INFO2, Help2),
    EV_WM_CTLCOLOR,
//{{Results_dialogRSP_TBL_END}}
END_RESPONSE_TABLE;


//{{Results_dialog Implementation}}


Results_dialog::Results_dialog (TWindow* parent,
                                Parameters *Param_p,
                                Simul *Simul_p,
                                RainfallFile* rainfallF,
                                TResId resId,
                                TModule* module):
    TDialog(parent, resId, module)
   {
   Param_ptr = Param_p;
   Simul_ptr = Simul_p;
   rainfallFile = rainfallF;

   Period_control = new TStatic(this, IDC_STATIC1);
   Soil_type_control = new TStatic(this, IDC_STATIC2);
   Cover_control = new TStatic(this, IDC_STATIC3);
   Start_water_mm_control = new TStatic(this, IDC_STATIC4);
   Start_water_perc_control = new TStatic(this, IDC_STATIC5);
   Final_water_mm_control = new TStatic(this, IDC_STATIC6);
   Final_water_perc_control = new TStatic(this, IDC_STATIC7);
   Fallow_rain_control = new TStatic(this, IDC_STATIC8);
   Evaporation_mm_control = new TStatic(this, IDC_STATIC9);
   Evaporation_perc_control = new TStatic(this, IDC_STATIC10);
   Runoff_mm_control = new TStatic(this, IDC_STATIC11);
   Runoff_perc_control = new TStatic(this, IDC_STATIC12);
   Drainage_mm_control = new TStatic(this, IDC_STATIC13);
   Drainage_perc_control = new TStatic(this, IDC_STATIC14);
   Gain_water_mm_control = new TStatic(this, IDC_STATIC15);
   Gain_water_perc_control = new TStatic(this, IDC_STATIC16);
   Gain_nitrate_control = new TStatic(this, IDC_STATIC17);
   mm1_control = new TStatic(this, IDC_STATIC18);
   perc1_control = new TStatic(this, IDC_STATIC19);
   mm2_control = new TStatic(this, IDC_STATIC20);
   perc2_control = new TStatic(this, IDC_STATIC21);
   Fallow_eff_control = new TStatic(this, IDC_STATIC22);

   Bold_font = new TFont("MS Sans Serif", 8, 0, 0, 0, FW_HEAVY);
}


Results_dialog::~Results_dialog ()
{
    Destroy();

    // INSERT>> Your destructor code here.

    delete Bold_font;
}


// *******************************************************************
      void Results_dialog::SetupWindow ()  {
// *******************************************************************

//  Short description:

//  Notes:

//  Changes:
//    DPH

//  Internal variables
      char St1[100];                   // String 1
      char St2[100];                   // String 2
      char St[100];                    // String

// -------------------- Executable code section ----------------------


    TDialog::SetupWindow();

   // INSERT>> Your code here.

   // Fill period control

   Param_ptr->Get_start_date().Write(St1);
   Param_ptr->Get_end_date().Write(St2);
   sprintf(St, "%s to %s", St1, St2);
   Period_control->SetText(St);

   // Setup the soil type control

   Soil_type_control->SetText(Param_ptr->Get_soil_name());

   // Setup the cover control

   sprintf(St, "%4.0f%% to %4.0f%%",
      Param_ptr->Get_start_soil_cover(),
      Param_ptr->Get_end_soil_cover());
   Cover_control->SetText(St);

   // Setup starting water.

   sprintf(St, "%4.0f", Param_ptr->Get_init_soil_water());
   Start_water_mm_control->SetText(St);
   sprintf(St, "%4.0f", Param_ptr->Get_init_soil_water() * 100.0 /
                        Param_ptr->Get_calc_soil_water_max());
   Start_water_perc_control->SetText(St);

   // Setup final water.

   sprintf(St, "%4.0f", Simul_ptr->Get_final_soil_water());
   Final_water_mm_control->SetText(St);
   sprintf(St, "%4.0f", Simul_ptr->Get_final_soil_water() * 100.0 /
                        Param_ptr->Get_calc_soil_water_max());
   Final_water_perc_control->SetText(St);

   // Setup fallow rainfall control

   sprintf(St, "%4.0f", Simul_ptr->Get_total_rain());
   Fallow_rain_control->SetText(St);

   // Setup evaporation controls

   sprintf(St, "%4.0f", Simul_ptr->Get_total_evaporation());
   Evaporation_mm_control->SetText(St);
   if (Simul_ptr->Get_total_rain() > 0)
      {
      sprintf(St, "%4.0f", Simul_ptr->Get_total_evaporation() * 100.0 /
                           Simul_ptr->Get_total_rain());
      Evaporation_perc_control->SetText(St);
      }

   // Setup runoff controls

   sprintf(St, "%4.0f", Simul_ptr->Get_total_runoff());
   Runoff_mm_control->SetText(St);
   if (Simul_ptr->Get_total_rain() > 0)
      {
      sprintf(St, "%4.0f", Simul_ptr->Get_total_runoff() * 100.0/
                           Simul_ptr->Get_total_rain());
      Runoff_perc_control->SetText(St);
      }

   // Setup drainage controls

   sprintf(St, "%4.0f", Simul_ptr->Get_total_drainage());
   Drainage_mm_control->SetText(St);
   if (Simul_ptr->Get_total_rain() > 0)
      {
      sprintf(St, "%4.0f", Simul_ptr->Get_total_drainage() * 100.0 /
                           Simul_ptr->Get_total_rain());
      Drainage_perc_control->SetText(St);
      }

   // Setup gain in water controls

   sprintf(St, "%4.0f", Simul_ptr->Get_final_soil_water() -
                        Param_ptr->Get_init_soil_water());
   Gain_water_mm_control->SetText(St);
   if (Simul_ptr->Get_total_rain() > 0)
      {
      sprintf(St, "%4.0f",
         (Simul_ptr->Get_final_soil_water() - Param_ptr->Get_init_soil_water()) * 100.0 /
          Simul_ptr->Get_total_rain());
      Gain_water_perc_control->SetText(St);
      }

   // Setup nitrate control

   sprintf(St, "%4.0f", Simul_ptr->Get_final_nitrate() -
                        Simul_ptr->Get_init_nitrate());
   Gain_nitrate_control->SetText(St);

   // change font of all 'mm' and '%' labels.

   mm1_control->SetWindowFont(*Bold_font, true);
   mm2_control->SetWindowFont(*Bold_font, true);
   perc1_control->SetWindowFont(*Bold_font, true);
   perc2_control->SetWindowFont(*Bold_font, true);
   }

// *******************************************************************
      void Results_dialog::Help1 ()  {
// *******************************************************************

//  Short description:

//  Notes:

//  Changes:
//    DPH

//  Internal variables
//    none

// -------------------- Executable code section ----------------------


   // INSERT>> Your code here.

   So_what_dlg(this, 3, 1).Execute();
   }

// *******************************************************************
      void Results_dialog::Print_button ()  {
// *******************************************************************

//  Short description:
//    Print button was clicked.

//  Notes:

//  Changes:
//    DPH

//  Internal variables
      Text_table Our_table;            // our table.
      char St[100];                    // string from control

// -------------------- Executable code section ----------------------


   // INSERT>> Your code here.

   // create rectangles.

   Table_rect_item R1(4, 1, 70, 8);
   Table_rect_item R2(4, 8, 70, 12);
   Table_rect_item R3(4, 12, 70, 22);
   Table_rect_item R4(4, 22, 70, 24);

   Our_table.Add_item(&R1);
   Our_table.Add_item(&R2);
   Our_table.Add_item(&R3);
   Our_table.Add_item(&R4);

   // create all text items.
   char rainTitle[200];
   getTitle(rainfallFile, rainTitle);

   Table_text_item T1(5, 2, GString("Site name : "));
   Our_table.Add_item(&T1);
   Table_text_item T2(20, 2, GString(Param_ptr->Get_site_name()));
   Our_table.Add_item(&T2);

   Table_text_item T3(5, 3, GString("Rainfall name : "));
   Our_table.Add_item(&T3);
   Table_text_item T4(20, 3, GString(rainTitle));
   Our_table.Add_item(&T4);

   Table_text_item T5(5, 4, GString("For period : "));
   Our_table.Add_item(&T5);
   Period_control->GetText(St, sizeof(St));
   Table_text_item T6(20, 4, GString(St));
   Our_table.Add_item(&T6);

   Table_text_item T7(5, 5, GString("Soil type : "));
   Our_table.Add_item(&T7);
   Soil_type_control->GetText(St, sizeof(St));
   Table_text_item T8(20, 5, GString(St));
   Our_table.Add_item(&T8);

   Table_text_item T9(5, 6, GString("Cover : "));
   Our_table.Add_item(&T9);
   Cover_control->GetText(St, sizeof(St));
   Table_text_item T10(20, 6, GString(St));
   Our_table.Add_item(&T10);

   Table_text_item T11(5, 7, GString("Organic Carbon : "));
   Our_table.Add_item(&T11);
   sprintf (St, "%4.1f%%", Param_ptr->Get_calc_organic_carbon());
   Table_text_item T12(20, 7, GString(St));
   Our_table.Add_item(&T12);

   Table_text_item T13(42, 9, GString("mm"));
   Our_table.Add_item(&T13);
   Table_text_item T14(58, 9, GString("% of full"));
   Our_table.Add_item(&T14);

   Table_text_item T15(5, 10, GString("Starting soil water:"));
   Our_table.Add_item(&T15);
   Start_water_mm_control->GetText(St, sizeof(St));
   Table_text_item T16(40, 10, GString(St));
   Our_table.Add_item(&T16);
   Start_water_perc_control->GetText(St, sizeof(St));
   Table_text_item T17(60, 10, GString(St));
   Our_table.Add_item(&T17);

   Table_text_item T18(5, 11, GString("Final soil water:"));
   Our_table.Add_item(&T18);
   Final_water_mm_control->GetText(St, sizeof(St));
   Table_text_item T19(40, 11, GString(St));
   Our_table.Add_item(&T19);
   Final_water_perc_control->GetText(St, sizeof(St));
   Table_text_item T20(60, 11, GString(St));
   Our_table.Add_item(&T20);

   Table_text_item T21(42, 13, GString("mm"));
   Our_table.Add_item(&T21);
   Table_text_item T22(58, 13, GString("% of rain"));
   Our_table.Add_item(&T22);

   Table_text_item T23(5, 14, GString("Fallow rainfall:"));
   Our_table.Add_item(&T23);
   Fallow_rain_control->GetText(St, sizeof(St));
   Table_text_item T24(40, 14, GString(St));
   Our_table.Add_item(&T24);

   Table_text_item T25(5, 15, GString("less     Evaporation:"));
   Our_table.Add_item(&T25);
   Evaporation_mm_control->GetText(St, sizeof(St));
   Table_text_item T26(40, 15, GString(St));
   Our_table.Add_item(&T26);
   Evaporation_perc_control->GetText(St, sizeof(St));
   Table_text_item T27(60, 15, GString(St));
   Our_table.Add_item(&T27);

   Table_text_item T28(5, 16, GString("              Runoff:"));
   Our_table.Add_item(&T28);
   Runoff_mm_control->GetText(St, sizeof(St));
   Table_text_item T29(40, 16, GString(St));
   Our_table.Add_item(&T29);
   Runoff_perc_control->GetText(St, sizeof(St));
   Table_text_item T30(60, 16, GString(St));
   Our_table.Add_item(&T30);

/*   Table_text_item T29(5, 16, GString("            Drainage:"));
   Our_table.Add_item(&T29);
   Drainage_mm_control->GetText(St, sizeof(St));
   Table_text_item T30(40, 16, GString(St));
   Our_table.Add_item(&T30);
   Drainage_perc_control->GetText(St, sizeof(St));
   Table_text_item T31(60, 16, GString(St));
   Our_table.Add_item(&T31);

   Table_text_item T32(5, 17, GString("       (not estimated)"));
   Our_table.Add_item(&T32);
*/
   Table_text_item T33(5, 20, GString("= Gain in soil water:"));
   Our_table.Add_item(&T33);
   Gain_water_mm_control->GetText(St, sizeof(St));
   Table_text_item T34(40, 20, GString(St));
   Our_table.Add_item(&T34);
   Gain_water_perc_control->GetText(St, sizeof(St));
   Table_text_item T35(60, 20, GString(St));
   Our_table.Add_item(&T35);

   Table_text_item T36(50, 21, GString("(fallow efficiency)"));
   Our_table.Add_item(&T36);

   Table_text_item T37(5, 23, GString("Gain in soil nitrate:"));
   Our_table.Add_item(&T37);
   Gain_nitrate_control->GetText(St, sizeof(St));
   Table_text_item T38(40, 23, GString(St));
   Our_table.Add_item(&T38);
   Table_text_item T39(47, 23, GString("(kg/ha)"));
   Our_table.Add_item(&T39);

   GDate Current_date;
   Current_date.Set_to_current();
   char Date_str[30];
   Current_date.Write (Date_str);
   GString Current_date_st;
   Current_date_st = "Date printed : ";
   Current_date_st += Date_str;

   Table_text_item T40(5, 25, Current_date_st);
   Our_table.Add_item(&T40);

   // Send chart to printer.

   TPrinter Printer;

   Text_table_printout Printout("Howwet results table", &Our_table);
   Printer.Print(this, Printout, TRUE);
   }


// *******************************************************************
      void Results_dialog::Help2 ()  {
// *******************************************************************

//  Short description:

//  Notes:

//  Changes:
//    DPH 

//  Internal variables
//    none

// -------------------- Executable code section ----------------------


   // INSERT>> Your code here.

   So_what_dlg(this, 3, 2).Execute();
   }

// ------------------------------------------------------------------
//  Short description:
//    Change colour of some controls.

//  Notes:

//  Changes:
//    DPH 7/6/1996

// ------------------------------------------------------------------
HBRUSH Results_dialog::EvCtlColor (HDC dc, HWND hWndChild, uint ctlType)
   {
   HBRUSH result = TDialog::EvCtlColor(dc, hWndChild, ctlType);

   if (hWndChild == Fallow_rain_control->HWindow ||
       hWndChild == Fallow_eff_control->HWindow)
      {
      ::SetTextColor (dc, TColor(0, 0, 255));
      }

   if (hWndChild == Evaporation_mm_control->HWindow ||
       hWndChild == Evaporation_perc_control->HWindow)
      {
      ::SetTextColor (dc, TColor(255, 0, 0));
      }
   if (hWndChild == Runoff_mm_control->HWindow ||
       hWndChild == Runoff_perc_control->HWindow)
      {
      ::SetTextColor (dc, TColor(128, 128, 0));
      }
   else if (hWndChild == Gain_water_mm_control->HWindow ||
            hWndChild == Gain_water_perc_control->HWindow)
      {
      ::SetTextColor (dc, TColor(0, 128, 0));
      }
   else if (hWndChild == Gain_nitrate_control->HWindow)
      {
      ::SetTextColor (dc, TColor(0, 128, 0));
      }

   return result;
   }


/*  Project howwet
    APSRU
    Copyright © 1995. All Rights Reserved.

    SUBSYSTEM:    howwet.apx Application
    FILE:         rainanal.cpp
    AUTHOR:       


    OVERVIEW
    ========
    Source file for implementation of Rain_analysis_dlg (TDialog).
*/

#include <owl\owlpch.h>
#pragma hdrstop

#include <consts.h>
#include "rainanal.h"

#include "so_what.h"
#include <consts.rh>
#include <stdio.h>
#include <gobjsx\texttble.h>
#include "Rainfall\HowwetRainfall.h"
//
// Build a response table for all messages/commands handled
// by the application.
//
DEFINE_RESPONSE_TABLE1(Rain_analysis_dlg, TDialog)
//{{Rain_analysis_dlgRSP_TBL_BEGIN}}
    EV_BN_CLICKED(INFO1, Help1),
    EV_BN_CLICKED(INFO2, Help2),
    EV_BN_CLICKED(PRINT, Print_button),
//{{Rain_analysis_dlgRSP_TBL_END}}
END_RESPONSE_TABLE;


//{{Rain_analysis_dlg Implementation}}


Rain_analysis_dlg::Rain_analysis_dlg (TWindow* parent,
                                      Parameters *Param_p,
                                      Simul *Simul_p,
                                      RainfallFile* rainfallF,
                                      TResId resId, TModule* module):

    TDialog(parent, resId, module)
{
    // INSERT>> Your constructor code here.

    Simul_ptr = Simul_p;
    Param_ptr = Param_p;
    rainfallFile = rainfallF;

    Period_control = new TStatic(this, IDC_STATIC1);
    mm15_control = new TStatic(this, IDC_STATIC2);
    mm15_50_control = new TStatic(this, IDC_STATIC3);
    mm50_control = new TStatic(this, IDC_STATIC4);
    Total_control = new TStatic(this, IDC_STATIC5);
    Long_term_av_control = new TStatic(this, IDC_STATIC6);
    perc15_control = new TStatic(this, IDC_STATIC7);
    perc15_50_control = new TStatic(this, IDC_STATIC8);
    perc50_control = new TStatic(this, IDC_STATIC9);
    Long_term_perc_control = new TStatic(this, IDC_STATIC10);
    Expected_rain1_control = new TStatic(this, IDC_STATIC11);
    Expected_rain2_control = new TStatic(this, IDC_STATIC12);
    Current_sw_control = new TStatic(this, IDC_STATIC13);

}


Rain_analysis_dlg::~Rain_analysis_dlg ()
{
    Destroy();

    // INSERT>> Your destructor code here.

}


// *******************************************************************
      void Rain_analysis_dlg::SetupWindow ()  {
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

   // Fill mm15 control

   sprintf(St, "%4.0f", Simul_ptr->Get_rain_15());
   mm15_control->SetText(St);

   if (Simul_ptr->Get_total_rain() > 0.0)
      {
      sprintf(St, "%4.0f",
             (Simul_ptr->Get_rain_15() * 100.0 / Simul_ptr->Get_total_rain()));
      perc15_control->SetText(St);
      }

   // Fill mm15_50 control

   sprintf(St, "%4.0f", Simul_ptr->Get_rain_15_50());
   mm15_50_control->SetText(St);
   if (Simul_ptr->Get_total_rain() > 0.0)
      {
      sprintf(St, "%4.0f",
             (Simul_ptr->Get_rain_15_50() * 100.0 / Simul_ptr->Get_total_rain()));
      perc15_50_control->SetText(St);
      }

   // Fill mm50 control

   sprintf(St, "%4.0f", Simul_ptr->Get_rain_50());
   mm50_control->SetText(St);
   if (Simul_ptr->Get_total_rain() > 0.0)
      {
      sprintf(St, "%4.0f",
             (Simul_ptr->Get_rain_50() * 100.0 / Simul_ptr->Get_total_rain()));
      perc50_control->SetText(St);
      }

   // Fill Total control

   sprintf(St, "%4.0f", Simul_ptr->Get_total_rain());
   Total_control->SetText(St);

   // Fill longterm average for period controls.

   sprintf(St, "%4.0f", Param_ptr->Get_average_rain(Param_ptr->Get_start_date(),
                                                    Param_ptr->Get_end_date()));
   Long_term_av_control->SetText(St);

   if (Simul_ptr->Get_total_rain() > 0.0)
      {
      sprintf(St, "%4.0f",
             (Simul_ptr->Get_total_rain() * 100.0 /
              Param_ptr->Get_average_rain(Param_ptr->Get_start_date(),
                                          Param_ptr->Get_end_date())));
      Long_term_perc_control->SetText(St);
      }

   // Fill expected rain controls.

   GDate Start_date = Param_ptr->Get_end_date();
   GDate End_date = Start_date;
   End_date += 30;
   sprintf(St, "%4.0f", Param_ptr->Get_average_rain(Start_date, End_date));
   Expected_rain1_control->SetText(St);

   End_date += 30;
   sprintf(St, "%4.0f", Param_ptr->Get_average_rain(Start_date, End_date));
   Expected_rain2_control->SetText(St);

   // Fill current soil water control

   sprintf(St, "%4.0f", Simul_ptr->Get_final_soil_water());
   Current_sw_control->SetText(St);
   }


// *******************************************************************
      void Rain_analysis_dlg::Help1 ()  {
// *******************************************************************

//  Short description:

//  Notes:

//  Changes:
//    DPH

//  Internal variables
//    none

// -------------------- Executable code section ----------------------


   // INSERT>> Your code here.

   So_what_dlg(this, 5, 1).Execute();
   }


// *******************************************************************
      void Rain_analysis_dlg::Help2 ()  {
// *******************************************************************

//  Short description:

//  Notes:

//  Changes:
//    DPH

//  Internal variables
//    none

// -------------------- Executable code section ----------------------


   // INSERT>> Your code here.

   So_what_dlg(this, 5, 2).Execute();
   }


// *******************************************************************
      void Rain_analysis_dlg::Print_button ()  {
// *******************************************************************

//  Short description:

//  Notes:

//  Changes:
//    DPH

//  Internal variables
      Text_table Our_table;            // our table.
      char St[100];                    // string from control

// -------------------- Executable code section ----------------------


   // INSERT>> Your code here.

   // create rectangles.

   Table_rect_item R1(4, 1, 70, 5);
   Table_rect_item R2(4, 5, 70, 7);
   Table_rect_item R3(4, 7, 70, 13);
   Table_rect_item R4(4, 13, 70, 15);
   Table_rect_item R5(4, 15, 70, 17);
   Table_rect_item R6(4, 17, 70, 21);

   Our_table.Add_item(&R1);
   Our_table.Add_item(&R2);
   Our_table.Add_item(&R3);
   Our_table.Add_item(&R4);
   Our_table.Add_item(&R5);
   Our_table.Add_item(&R6);

   // create line

   Table_line_item L1(30, 5, 30, 15);
   Our_table.Add_item(&L1);

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

   Table_text_item T7(5, 6, GString("Daily falls : "));
   Our_table.Add_item(&T7);
   Table_text_item T8(38, 6, GString("Total mm"));
   Our_table.Add_item(&T8);
   Table_text_item T9(53, 6, GString("%"));
   Our_table.Add_item(&T9);

   Table_text_item T10(5, 8, GString("< 15 mm"));
   Our_table.Add_item(&T10);
   mm15_control->GetText(St, sizeof(St));
   Table_text_item T11(40, 8, GString(St));
   Our_table.Add_item(&T11);
   perc15_control->GetText(St, sizeof(St));
   Table_text_item T12(50, 8, GString(St));
   Our_table.Add_item(&T12);

   Table_text_item T13(5, 10, GString("15-50 mm"));
   Our_table.Add_item(&T13);
   mm15_50_control->GetText(St, sizeof(St));
   Table_text_item T14(40, 10, GString(St));
   Our_table.Add_item(&T14);
   perc15_50_control->GetText(St, sizeof(St));
   Table_text_item T15(50, 10, GString(St));
   Our_table.Add_item(&T15);

   Table_text_item T16(5, 12, GString("> 50 mm"));
   Our_table.Add_item(&T16);
   mm50_control->GetText(St, sizeof(St));
   Table_text_item T17(40, 12, GString(St));
   Our_table.Add_item(&T17);
   perc50_control->GetText(St, sizeof(St));
   Table_text_item T18(50, 12, GString(St));
   Our_table.Add_item(&T18);

   Table_text_item T19(5, 14, GString("Total for period"));
   Our_table.Add_item(&T19);
   Total_control->GetText(St, sizeof(St));
   Table_text_item T20(40, 14, GString(St));
   Our_table.Add_item(&T20);
   Table_text_item T21(51, 14, GString("100"));
   Our_table.Add_item(&T21);

   Table_text_item T22(5, 16, GString("Long term average"));
   Our_table.Add_item(&T22);
   Long_term_av_control->GetText(St, sizeof(St));
   strcat(St, " mm");
   Table_text_item T23(40, 16, GString(St));
   Our_table.Add_item(&T23);
   Long_term_perc_control->GetText(St, sizeof(St));
   strcat(St, " % of long term");
   Table_text_item T24(50, 16, GString(St));
   Our_table.Add_item(&T24);

   Table_text_item T29(5, 18, GString("Current soil water :"));
   Our_table.Add_item(&T29);
   Current_sw_control->GetText(St, sizeof(St));
   strcat(St, " mm");
   Table_text_item T30(40, 18, GString(St));
   Our_table.Add_item(&T30);

   Table_text_item T25(5, 19, GString("Expected rain in next 30 days :"));
   Our_table.Add_item(&T25);
   Expected_rain1_control->GetText(St, sizeof(St));
   strcat(St, " mm");
   Table_text_item T26(40, 19, GString(St));
   Our_table.Add_item(&T26);

   Expected_rain2_control->GetText(St, sizeof(St));
   strcat(St, " mm");
   Table_text_item T27(40, 20, GString(St));
   Our_table.Add_item(&T27);
   Table_text_item T28(5, 20, GString("                 next 60 days :"));
   Our_table.Add_item(&T28);

   GDate Current_date;
   Current_date.Set_to_current();
   char Date_str[30];
   Current_date.Write (Date_str);
   GString Current_date_st;
   Current_date_st = "Date printed : ";
   Current_date_st += Date_str;

   Table_text_item Z29(5, 22, Current_date_st);
   Our_table.Add_item(&Z29);

   // Send chart to printer.

   TPrinter Printer;

   Text_table_printout Printout("Howwet rainfall analysis table", &Our_table);
   Printer.Print(this, Printout, TRUE);
   }


/*  Project howwet
    APSRU
    Copyright © 1995. All Rights Reserved.

    SUBSYSTEM:    howwet.apx Application
    FILE:         cal_dlg.cpp
    AUTHOR:       


    OVERVIEW
    ========
    Source file for implementation of Calender_dialog (TDialog).
*/

#include <owl\owlpch.h>
#pragma hdrstop

#include <consts.h>
#include "cal_dlg.h"
#include <math.h>
#include <stdio.h>

#include <consts.rh>
#include "so_what.h"
#include "Rainfall\HowwetRainfall.h"

//
// Build a response table for all messages/commands handled
// by the application.
//
DEFINE_RESPONSE_TABLE1(Calender_dialog, TDialog)
//{{Calender_dialogRSP_TBL_BEGIN}}
    EV_BN_CLICKED(INFO1, Help1),
    EV_BN_CLICKED(PRINT, Print_button),
//{{Calender_dialogRSP_TBL_END}}
END_RESPONSE_TABLE;


//{{Calender_dialog Implementation}}


Calender_dialog::Calender_dialog (TWindow* parent,
                                  Parameters *Param_p,
                                  RainfallFile* rainfallF,
                                  TResId resId, TModule* module):
    TDialog(parent, resId, module)
   {
   rainfallFile = rainfallF;

   Calender_cont = new Calender_control(this, IDC_PUSHBUTTON1, &Cal);
   Param_ptr = Param_p;

   Period_control = new TStatic(this, IDC_STATIC1);
   Rain_title_control = new TStatic(this, IDC_STATIC2);

   // Setup our calender.

   Setup_calender();
}


Calender_dialog::~Calender_dialog ()
{
    Destroy();

    // INSERT>> Your destructor code here.

   delete Data_array;
}

// ********************************************************
// Setup the calender object
// ********************************************************
void Calender_dialog::Setup_calender(void)
   {
   GString Rain_string;                // Rainfall string
   GDate Date;

   Data_array = new String_array((int) (Param_ptr->Get_end_date() -
                                 Param_ptr->Get_start_date() + 1));
   for (Date = Param_ptr->Get_start_date();
        Date <= Param_ptr->Get_end_date();
        Date++)
      {
      Rain_string.Set_real(floor(getRain(rainfallFile, Date.Get_jday()) + 0.5));
      if (Rain_string != "0")
         Data_array->Add(Rain_string);

      else
         Data_array->Add("");
      }

   Cal.Setup(Param_ptr->Get_start_date(),
             Param_ptr->Get_end_date(),
             Data_array, TRUE, TRUE);
   Cal.Set_font("Courier new", 10);

   // get longterm averages and store in calender.

   for (int Month = 1;
        Month <= 12;
        Month++)
      {
      Cal.Averages[Month-1] = Param_ptr->Get_average_rain_for_month(Month);
      }
   }


// *******************************************************************
      void Calender_dialog::SetupWindow ()  {
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

   // Fill rainfall control

   char rainTitle[200];
   getTitle(rainfallFile, rainTitle);
   Rain_title_control->SetText(rainTitle);
   }


// *******************************************************************
      void Calender_dialog::Help1 ()  {
// *******************************************************************

//  Short description:

//  Notes:

//  Changes:
//    DPH 

//  Internal variables
//    none

// -------------------- Executable code section ----------------------


   // INSERT>> Your code here.

   So_what_dlg(this, 4, 1).Execute();
   }


// *******************************************************************
      void Calender_dialog::Print_button ()  {
// *******************************************************************

//  Short description:

//  Notes:

//  Changes:
//    DPH 

//  Internal variables
//    none

// -------------------- Executable code section ----------------------


   // INSERT>> Your code here.

   // Send chart to printer.

   TPrinter Printer;

   Calender_printout Printout("Howwet calender", Calender_cont);

   // setup the header strings.

   char rainTitle[200];
   getTitle(rainfallFile, rainTitle);

   String_array Strings(4);
   GString St;
   St = "Site name = ";
   St += Param_ptr->Get_site_name();
   Strings.Add(St);
   St = "Rainfall name = ";
   St += rainTitle;
   Strings.Add(St);

   char St0[100];
   char St1[40];
   char St2[40];
   Param_ptr->Get_start_date().Write(St1);
   Param_ptr->Get_end_date().Write(St2);
   sprintf(St0, "Period = %s to %s", St1, St2);
   Strings.Add(GString(St0));

   GDate Current_date;
   Current_date.Set_to_current();
   char Date_str[30];
   Current_date.Write (Date_str);
   GString Current_date_st;
   Current_date_st = "Date printed : ";
   Current_date_st += Date_str;
   Strings.Add(Current_date_st);

   Printout.Set_header_strings(&Strings);

   // go print the page.

   Printer.Print(this, Printout, TRUE);

   }


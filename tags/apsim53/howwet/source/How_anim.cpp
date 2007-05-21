/*  Project howwet
    APSRU
    Copyright © 1995. All Rights Reserved.

    SUBSYSTEM:    fileview.apx Application
    FILE:         anim_dlg.cpp
    AUTHOR:


    OVERVIEW
    ========
    Source file for implementation of Animated_dlg (TDialog).
*/

#include <graph\global.h>
#include <graph\dchart.h>
#pragma hdrstop

#include "how_anim.h"
#include <consts.rh>
#include <graph\printout.h>
#include "howwet.rh"
#include "rainfall\HowwetRainfall.h"

//
// Build a response table for all messages/commands handled
// by the application.
//
DEFINE_RESPONSE_TABLE1(Howwet_animated_dlg, Animated_dlg)
    EV_BN_CLICKED(PRINT, Print_button),
END_RESPONSE_TABLE;


//{{Animated_dlg Implementation}}


Howwet_animated_dlg::Howwet_animated_dlg (TWindow*       parent,
                                          TResId         resId,
                                          TModule*       module,
                                          Parameters*    Param_p,
                                          Simul*         Simul_p,
                                          RainfallFile*  rainfallF)
    : Animated_dlg(parent, resId, module)
   {
   // INSERT>> Your constructor code here.

   Param_ptr = Param_p;
   Simul_ptr = Simul_p;
   rainfallFile = rainfallF;

   // setup the date stuff.

   Set_start_date (Param_ptr->Get_start_date());
   Date_control = new TStatic (this, IDC_STATIC1);
   }

// *******************************************************************
      void Howwet_animated_dlg::Print_button (void)  {
// *******************************************************************

//  Short description:

//  Notes:

//  Changes:
//    DPH 

//  Internal variables
//    none

// -------------------- Executable code section ----------------------


   // INSERT>> Your code here.

   Slide_control->SetPosition(0);

   // Send chart to printer.
   TPrinter Printer;

   Chart_printout Printout("Howwet graphics", 1);
   Printout.Add_chart(Chart_control_ptr);
   Printout.Set_chart_print_range(1,
                                  1,
                                  1);

   char rainTitle[200];
   getTitle(rainfallFile, rainTitle);

   // setup the header strings.

   String_array Strings(7);
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

   St = "Soil type = ";
   St += Param_ptr->Get_soil_name();
   Strings.Add(St);

   sprintf(St0, "Cover = %3.0f%% to %3.0f%%",
           Param_ptr->Get_start_soil_cover(),
           Param_ptr->Get_end_soil_cover());
   Strings.Add(St0);

   float orgc = Param_ptr->Get_user_organic_carbon();
   if (orgc == 0)
      orgc = Param_ptr->Get_calc_organic_carbon();

   sprintf(St0, "Organic carbon = %3.0f%%",
           orgc);
   Strings.Add(St0);

   GDate Current_date;
   Current_date.Set_to_current();
   char Date_str[30];
   Current_date.Write (Date_str);
   GString Current_date_st;
   Current_date_st = "Date printed : ";
   Current_date_st += Date_str;
   Strings.Add(Current_date_st);

   Printout.Set_header_strings(&Strings);

   Printer.Print(this, Printout, TRUE);
   }

// ------------------------------------------------------------------
//  Short description:

//  Notes:

//  Changes:
//    DPH 7/6/1996

// ------------------------------------------------------------------
void Howwet_animated_dlg::Display_date (GDate& Date)
   {
   char St[50];
   Date.Set_write_option(GDate::DD_MMMMMM_YYYY);
   Date.Write(St);
   Date_control->SetText(St);
   }


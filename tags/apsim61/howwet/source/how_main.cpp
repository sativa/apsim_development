/*  Project howwet
    APSRU
    Copyright © 1995. All Rights Reserved.

    SUBSYSTEM:    howwet.exe Application
    FILE:         how_main.cpp
    AUTHOR:


    OVERVIEW
    ========
    Source file for implementation of Howwet_main (TWindow).
*/


#include <graph\global.h>
#include <graph\dchart.h> 

#include "prof_win.h"
#pragma hdrstop

#include <consts.h>

#include "howwet.h"
#include "how_main.h"

#include <stdio.h>

#include "clmt_dlg.h"
#include "soil_dlg.h"
#include "scon_dlg.h"
#include "res_dlg.h"
#include "rainanal.h"
#include "cal_dlg.h"
#include "nitgraph.h"
#include "sw_graph.h"
#include "rain_avg.h"
#include "watn_dlg.h"
#include "eros_dlg.h"
#include "Rainfall\HowwetRainfall.h"
#include <gobjs\brkupstr.h>
//
// Build a response table for all messages/commands handled
// by the application.
//
DEFINE_RESPONSE_TABLE1(Howwet_main, TWindow)
//{{Howwet_mainRSP_TBL_BEGIN}}
    EV_COMMAND(CM_CLIMATE, Climate),
    EV_COMMAND(CM_EXIT1, Exit),
    EV_COMMAND(CM_SOILTYPE, Soil_type_menu),
    EV_COMMAND(CM_SOIL_CONDITIONS, Soil_conditions),
    EV_COMMAND(CM_ABOUT_HOWWET, About_menu),
    EV_COMMAND(CM_TABLE, Results_table),
    EV_COMMAND(CM_RAINFALL_ANALYSIS, Rainfall_analysis),
    EV_COMMAND(CM_RAINFALL_CALENDER, Rainfall_calender),
    EV_COMMAND(CM_NITROGEN_GRAPH, Nitrogen_graph_dlg),
    EV_COMMAND(CM_SOIL_WATER_GRAPH, Soil_water_graph),
    EV_COMMAND(CM_RAIN_AVERAGES, Rainfall_average),
    EV_COMMAND(CM_WATERN_BALANCE, Water_NBalance),
    EV_COMMAND(CM_ANIMATED_PROFILE, Animated_profile),
    EV_COMMAND(CM_EROSION_ESTIMATION, Erosion_estimation),
    EV_COMMAND_AND_ID(500, RainfallMenuItemClick),
    EV_COMMAND_AND_ID(500+1, RainfallMenuItemClick),
    EV_COMMAND_AND_ID(500+2, RainfallMenuItemClick),
    EV_COMMAND_AND_ID(500+3, RainfallMenuItemClick),
    EV_COMMAND_AND_ID(500+4, RainfallMenuItemClick),
    EV_COMMAND_AND_ID(500+5, RainfallMenuItemClick),
    EV_COMMAND_AND_ID(500+6, RainfallMenuItemClick),
    EV_COMMAND_AND_ID(500+7, RainfallMenuItemClick),
    EV_COMMAND_AND_ID(500+8, RainfallMenuItemClick),
    EV_COMMAND_AND_ID(500+9, RainfallMenuItemClick),
  EV_COMMAND(CM_RAINFALL, RainfallEntry),
//{{Howwet_mainRSP_TBL_END}}
END_RESPONSE_TABLE;


//{{Howwet_main Implementation}}


//////////////////////////////////////////////////////////
// Howwet_main
// ==========
// Construction/Destruction handling.
Howwet_main::Howwet_main (TWindow* parent, const char far* title, TModule* module)
    : TWindow(parent, title, module)
{
    // INSERT>> Your constructor code here.

   Main_bitmap = NULL;
   Logo_bitmap = NULL;
   Desc_bitmap = NULL;
   rainfallFile = newRainfallFile();

   Display_logo_screen = TRUE;
   Param_ptr = new Parameters;
   Simul_ptr = new Simul(Param_ptr, rainfallFile);
   Remove_logos = FALSE;
   Display_init_dialog = TRUE;
   Screen_height = 0;
   Water_n_dlg_ptr = new Water_n_dlg (this, Param_ptr, Simul_ptr, rainfallFile);
   }
// *******************************************************************
// destructor
// *******************************************************************
Howwet_main::~Howwet_main ()
   {
   Destroy();

   delete Main_bitmap;
   delete Logo_bitmap;
   delete Desc_bitmap;
   delete Param_ptr;
   delete Simul_ptr;
   delete Water_n_dlg_ptr;

   deleteRainfallFile(rainfallFile);
   }
// *******************************************************************
// Setup the main window.
// *******************************************************************
void Howwet_main::SetupWindow()
   {
   TWindow::SetupWindow();
   }

// *******************************************************************
      void Howwet_main::Climate ()  {
// *******************************************************************

//  Short description:
//    Climate menu was pressed

//  Notes:

//  Changes:
//    DPH 7/10/95

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   // INSERT>> Your code here.

   try
      {
      Climate_dlg(this, Param_ptr).Execute();
      }
   catch (GString Msg)
      {
      MessageBox(Msg.c_str(), "Error", MB_OK | MB_ICONSTOP);
      }
   }


// *******************************************************************
      void Howwet_main::Paint (TDC& dc, bool erase, TRect& rect)  {
// *******************************************************************

//  Short description:
//    Paint the main screen of howwet

//  Notes:

//  Changes:
//    DPH 7/10/95

//  Internal variables
   TRect Client_rect = Parent->GetClientRect();

// -------------------- Executable code section ----------------------

   TWindow::Paint(dc, erase, rect);

   // INSERT>> Your code here.
   static first = true;
   if (first)
      {
      first = false;
      setupRainfallMenu();
      }

   // force window size to 640x480 if we only have 16 colours.

   if (Screen_height == 0)
      {
      if (dc.GetDeviceCaps(NUMCOLORS) == 16 && Client_rect.right > 640)
         {
         Parent->MoveWindow (1, 1, 640, 480, true);
         Screen_height = 480;
         Client_rect = Parent->GetClientRect();
         }
      else
         Screen_height = GetSystemMetrics(SM_CYSCREEN);
      }

   if (!Remove_logos)
      {

      if (Main_bitmap == NULL)
         {
         // do we use 256 colour bitmap ?

         if (Client_rect.right > 640)
            Main_bitmap = new TDib(*GetApplication(), TResId(BITMAP_STORM256));
         else
            Main_bitmap = new TDib(*GetApplication(), TResId(BITMAP_STORM16));

         Logo_bitmap = new TBitmap(*GetApplication(), IDC_LOGO);
         Desc_bitmap = new TBitmap(*GetApplication(), IDC_DESC);
         }

      // Paint the bitmaps on screen if necessary

      TPalette Palette(*Main_bitmap);
      TMemoryDC Memory_dc(dc);
      dc.SelectObject(Palette);
      dc.RealizePalette();
//      TRect Source_rect = Parent->GetClientRect();
//      Source_rect.top =  Screen_height - Client_rect.bottom;
//      Source_rect.bottom = Screen_height;
      TRect Source_rect(0, 0, Main_bitmap->Size().X(), Main_bitmap->Size().Y());

      dc.StretchDIBits(Parent->GetClientRect(),
                       Source_rect,
                       *Main_bitmap);

//      dc.BitBlt(0, 0, 800, 600, Memory_dc, 0, 0, SRCCOPY);

      if (Display_logo_screen)
         {

         // Put logo on screen

         Memory_dc.SelectObject(*Logo_bitmap);
         dc.BitBlt((Client_rect.right - 270)/2,
                   Client_rect.bottom - 150,
                   270, 120, Memory_dc, 0, 0, SRCCOPY);

         // Put desc on screen

         Memory_dc.SelectObject(*Desc_bitmap);
         dc.BitBlt((Client_rect.right - 240)/2,
                    50,
                    240, 120, Memory_dc, 0, 0, SRCCOPY);

         // Put funded on screen

//         Memory_dc.SelectObject(*Funded_bitmap);
//         dc.BitBlt(300, 326, 240, 75, Memory_dc, 0, 0, SRCCOPY);

         // put up initial dialog box.

         if (Display_init_dialog)
            {
            Display_init_dialog = FALSE;
            TDialog(this, IDC_INIT_DLG).Execute();
            }
         else
            {
            Display_logo_screen = FALSE;
            Remove_logos = TRUE;
            }
         }
      }
   else
      {
      Invalidate();
      Remove_logos = FALSE;
      }
   }


// *******************************************************************
      void Howwet_main::Exit ()  {
// *******************************************************************

//  Short description:
//    Exit Howwet?

//  Notes:

//  Changes:
//    DPH 7/10/95

//  Internal variables
//    none

// -------------------- Executable code section ----------------------


   // INSERT>> Your code here.

   CloseWindow(IDOK);
   }


// *******************************************************************
      void Howwet_main::Soil_type_menu ()  {
// *******************************************************************

//  Short description:

//  Notes:

//  Changes:
//    DPH

//  Internal variables
//    none

// -------------------- Executable code section ----------------------


   // INSERT>> Your code here.

   Soil_type_dialog (this, Param_ptr).Execute();
   }


// *******************************************************************
      void Howwet_main::Soil_conditions ()  {
// *******************************************************************

//  Short description:
//    Soil conditions menu option selected.

//  Notes:

//  Changes:
//    DPH 28/10/95

//  Internal variables
//    none

// -------------------- Executable code section ----------------------


   // INSERT>> Your code here.

   Soil_conditions_dlg (this, Param_ptr, rainfallFile).Execute();
   }

// *******************************************************************
      void Howwet_main::About_menu ()  {
// *******************************************************************

//  Short description:

//  Notes:

//  Changes:
//    DPH 

//  Internal variables
//    none

// -------------------- Executable code section ----------------------


   // INSERT>> Your code here.

   TDialog(this, IDC_ABOUT_DIALOG).Execute();
   }


// *******************************************************************
      void Howwet_main::Results_table ()  {
// *******************************************************************

//  Short description:

//  Notes:

//  Changes:
//    DPH 

//  Internal variables
//    none

// -------------------- Executable code section ----------------------


   // INSERT>> Your code here.

   try
      {
      Simul_ptr->Go(this);
      Results_dialog(this, Param_ptr, Simul_ptr, rainfallFile).Execute();
      }
   catch (const string& err)
      {
      ::MessageBox(NULL, err.c_str(), "Error", MB_ICONSTOP | MB_OK);
      }
   }


// *******************************************************************
      void Howwet_main::Rainfall_analysis ()  {
// *******************************************************************

//  Short description:

//  Notes:

//  Changes:
//    DPH

//  Internal variables
//    none

// -------------------- Executable code section ----------------------


   // INSERT>> Your code here.
   try
      {
      Simul_ptr->Go(this);
      Rain_analysis_dlg(this, Param_ptr, Simul_ptr, rainfallFile).Execute();
      }
   catch (const string& err)
      {
      ::MessageBox(NULL, err.c_str(), "Error", MB_ICONSTOP | MB_OK);
      }

   }


// *******************************************************************
      void Howwet_main::Rainfall_calender ()  {
// *******************************************************************

//  Short description:

//  Notes:

//  Changes:
//    DPH

//  Internal variables
//    none

// -------------------- Executable code section ----------------------


   // INSERT>> Your code here.
   try
      {
      Calender_dialog(this, Param_ptr, rainfallFile).Execute();
      }
   catch (const string& err)
      {
      ::MessageBox(NULL, err.c_str(), "Error", MB_ICONSTOP | MB_OK);
      }
   }


// *******************************************************************
      void Howwet_main::Nitrogen_graph_dlg ()  {
// *******************************************************************

//  Short description:

//  Notes:

//  Changes:
//    DPH

//  Internal variables
//    none

// -------------------- Executable code section ----------------------


   // INSERT>> Your code here.
   try
      {
      Nitrogen_graph(this, IDC_NITROGEN_GRAPH, NULL, Param_ptr, Simul_ptr, rainfallFile).Execute();
      }
   catch (const string& err)
      {
      ::MessageBox(NULL, err.c_str(), "Error", MB_ICONSTOP | MB_OK);
      }

   }


// *******************************************************************
      void Howwet_main::Soil_water_graph ()  {
// *******************************************************************

//  Short description:

//  Notes:

//  Changes:
//    DPH

//  Internal variables
//    none

// -------------------- Executable code section ----------------------


   // INSERT>> Your code here.
   try
      {
      SW_graph(this, IDC_NITROGEN_GRAPH, NULL, Param_ptr, Simul_ptr, rainfallFile).Execute();
      }
   catch (const string& err)
      {
      ::MessageBox(NULL, err.c_str(), "Error", MB_ICONSTOP | MB_OK);
      }
   }


// *******************************************************************
      void Howwet_main::Rainfall_average ()  {
// *******************************************************************

//  Short description:

//  Notes:

//  Changes:
//    DPH

//  Internal variables
//    none

// -------------------- Executable code section ----------------------


   // INSERT>> Your code here.
   try
      {
      Rain_avg_graph(this, IDC_NITROGEN_GRAPH, NULL, Param_ptr, Simul_ptr, rainfallFile).Execute();
      }
   catch (const string& err)
      {
      ::MessageBox(NULL, err.c_str(), "Error", MB_ICONSTOP | MB_OK);
      }
   }


// *******************************************************************
      void Howwet_main::Water_NBalance ()  {
// *******************************************************************

//  Short description:

//  Notes:

//  Changes:
//    DPH 

//  Internal variables
//    none

// -------------------- Executable code section ----------------------


   // INSERT>> Your code here.
   try
      {
      Simul_ptr->Go(this);
      Water_n_dlg_ptr->Execute();
      }
   catch (const string& err)
      {
      ::MessageBox(NULL, err.c_str(), "Error", MB_ICONSTOP | MB_OK);
      }
   }


void Howwet_main::Animated_profile ()
   {
   try
      {
      Simul_ptr->Go(this);
      Animated_profile_window* Profile_window =
         new Animated_profile_window(this, Simul_ptr, Param_ptr, "Soil profile");
      Profile_window->Create();
      }
   catch (const string& err)
      {
      ::MessageBox(NULL, err.c_str(), "Error", MB_ICONSTOP | MB_OK);
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    user has selected erosion estimation menu option.

//  Notes:

//  Changes:
//    DPH 6/8/1996

// ------------------------------------------------------------------
void Howwet_main::Erosion_estimation ()
   {
   // INSERT>> Your code here.
   try
      {
      Erosion_dialog(this,
                     Param_ptr,
                     Simul_ptr).Execute();
      }
   catch (const string& err)
      {
      ::MessageBox(NULL, err.c_str(), "Error", MB_ICONSTOP | MB_OK);
      }
   }

// ------------------------------------------------------------------
// Let user manipulate rainfall data.
// ------------------------------------------------------------------
void Howwet_main::RainfallEntry()
   {
   showRainfallManipulation(rainfallFile);
   setupRainfallMenu();
   }
// ------------------------------------------------------------------
// put the most recent rainfall titles on rainfall menu.
// ------------------------------------------------------------------
void Howwet_main::setupRainfallMenu()
   {
   char titlesCSV[2000];
   getPreviousRainTitles(titlesCSV);
   String_array titles(20, 0, 20);
   Split_string(titlesCSV, ",", titles);

   // delete previous menu items.
   TMenu rainfallMenu = TMenu(Parent->GetMenu()).GetSubMenu(2);
   while (rainfallMenu.GetMenuItemCount() > 2)
      rainfallMenu.DeleteMenu(2, MF_BYPOSITION);

   // add new menu items.
   for (int m = 0; m != titles.GetItemsInContainer(); m++)
      {
      if (m == 0)
         rainfallMenu.AppendMenu(MF_CHECKED | MF_STRING, 500+m, titles[m].c_str());
      else
         rainfallMenu.AppendMenu(MF_UNCHECKED | MF_STRING, 500+m, titles[m].c_str());
      }
   }
// ------------------------------------------------------------------
// user has clicked on a menu item - do something.
// ------------------------------------------------------------------
void Howwet_main::RainfallMenuItemClick(WPARAM id)
   {
   openPreviousRainTitle(rainfallFile, id-500);
   setupRainfallMenu();
   }


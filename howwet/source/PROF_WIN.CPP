/*  Project howwet
    APSRU
    Copyright © 1995. All Rights Reserved.

    SUBSYSTEM:    howwet.apx Application
    FILE:         prof_win.cpp
    AUTHOR:       


    OVERVIEW
    ========
    Source file for implementation of Animated_profile_window (TWindow).      
*/

#include <owl\owlpch.h>
#pragma hdrstop

#include <consts.h>
#include "prof_win.h"
#include <consts.rh>
#include <stdio.h>
#include <owl\button.h>
#include "so_what.h"

//#define IDC_MHSLIDE1   4000
#define IDC_SLIDER1 4000

#define RUNNING_MEAN_DAYS  3

//
// Build a response table for all messages/commands handled
// by the application.
//
DEFINE_RESPONSE_TABLE1(Animated_profile_window, TWindow)
//{{Animated_profile_windowRSP_TBL_BEGIN}}
    EV_WM_ERASEBKGND,
    EV_WM_TIMER,
    EV_BN_CLICKED (IDOK, Ok_button),
    EV_WM_KEYDOWN,
    EV_WM_CTLCOLOR,
    EV_WM_HSCROLL,
    EV_BN_CLICKED (FORWARD_BUTTON, Forward_button),
    EV_BN_CLICKED (REWIND_BUTTON, Rewind_button),
    EV_BN_CLICKED (STOP_BUTTON, Stop_button),
    EV_BN_CLICKED (FF_BUTTON, FF_button),
    EV_BN_CLICKED (RR_BUTTON, RR_button),
    EV_BN_CLICKED (INFO1, Help1),
    EV_BN_CLICKED (INFO2, Help2),
    EV_BN_CLICKED (INFO3, Help3),
    EV_BN_CLICKED (INFO4, Help4),
//{{Animated_profile_windowRSP_TBL_END}}
END_RESPONSE_TABLE;


//{{Animated_profile_window Implementation}}


Animated_profile_window::Animated_profile_window (TWindow* parent,
                                                  Simul* Simul_p,
                                                  Parameters* Param_p,
                                                  const char far* title,
                                                  TModule* module):
    TWindow(parent, title, module)
{

   Simul_ptr = Simul_p;
   Param_ptr = Param_p;
   Going_forwards = TRUE;

    // Override the default window style for TWindow.
    Attr.Style |= WS_POPUPWINDOW | WS_CAPTION;
    Attr.X = 1;
    Attr.Y = 1;
    Attr.W = 640;
    Attr.H = 480;

    // Change the window's background color
    SetBkgndColor(RGB(0x1b, 0x5c, 0xe4));

    // INSERT>> Your constructor code here.

    Our_bitmap = new TBitmap(*GetApplication(), PROFILE_BITMAP);
    Sun_bitmap = new TBitmap(*GetApplication(), BITMAP_SUN);
    Rain_bitmap = new TBitmap(*GetApplication(), BITMAP_RAIN);

    Evap_shade.Setup(TPoint(267, 154),
                     TColor(255, 255, 255),
                     TColor(255, 0, 0),
                     0.0,
                     6.0);
    Runoff_shade.Setup(TPoint(549, 222),
                       TColor(219, 255, 0),
                       TColor(219, 108, 0),
                       0.0,
                       50.0);


    float sw[NUM_SOIL_LAYERS];
    Param_ptr->Get_calc_soil_water_max(sw);

    Surface1.Setup(TPoint(270, 280),
                  TColor(0, 255, 255),
                  TColor(0, 0, 255),
                  0.0,
                  sw[0]);
    Surface2.Setup(TPoint(398, 275),
                   TColor(0, 255, 255),
                   TColor(0, 0, 255),
                   0.0,
                   sw[0]);
    Profile1.Setup(TColor(0, 0, 255),
                   TColor(255, 255, 255),
                   TPoint(189, 282),
                   TPoint(193, 386),
                   TPoint(368, 432),
                   TPoint(368, 323),
                   0.0,
                   sw[1] + sw[2]);
    Profile2.Setup(TColor(0, 0, 255),
                   TColor(255, 255, 255),
                   TPoint(369, 320),
                   TPoint(369, 431),
                   TPoint(442, 338),
                   TPoint(442, 249),
                   0.0,
                   sw[1] + sw[2]);

   // ****** create all static controls ******

   mm_water_control = new TStatic (this, IDC_STATIC2, "", 94, 295, 65, 15);
	perc_water_control = new TStatic (this, IDC_STATIC3, "", 94, 324, 65, 15);
	Rain_control = new TStatic (this, IDC_STATIC4, "", 528, 32, 65, 15);
	Rain_total_control = new TStatic (this, IDC_STATIC5, "", 528, 61, 65, 15);
	Runoff_control = new TStatic (this, IDC_STATIC6, "", 536, 276, 65, 15);
	Runoff_total_control = new TStatic (this, IDC_STATIC7, "", 536, 305, 65, 15);

   new TButton(this, IDOK, "", 440, 400, 74, 50);
   Slide_control = new THSlider (this, IDC_SLIDER1, 15, 30, 160, 26);
   Date_control = new TStatic(this, IDC_STATIC1, "", 50, 12, 75, 15);
   Start_date_control = new TStatic (this, IDC_STATIC8, "", 15, 58, 65, 15);
   End_date_control = new TStatic (this, IDC_STATIC9, "", 110, 58, 65, 15);
   new TButton(this, RR_BUTTON, "", 40, 97, 20, 20);
   new TButton(this, REWIND_BUTTON, "", 60, 97, 20, 20);
   new TButton(this, STOP_BUTTON, "", 80, 97, 20, 20);
   new TButton(this, FORWARD_BUTTON, "", 100, 97, 20, 20);
   new TButton(this, FF_BUTTON, "", 120, 97, 20, 20);

   new TButton (this, INFO1, "", 161, 97, 30, 30);
   new TButton (this, INFO2, "", 309, 152, 30, 30);
   new TButton (this, INFO3, "", 500, 236, 30, 30);
   new TButton (this, INFO4, "", 280, 436, 30, 30);

   Simulation_done = FALSE;
   Doing_animation = FALSE;

   Current_index = -1;
   Total_rain_so_far = 0.0;
   Total_runoff_so_far = 0.0;

   Big_font = new TFont("MS Sans Serif", 18);
   Tiny_font = new TFont("MS Sans Serif", 6);
   Medium_font = new TFont("MS Sans Serif", 18);
   }

Animated_profile_window::~Animated_profile_window ()
{
    Destroy();

    // INSERT>> Your destructor code here.

    delete Our_bitmap;
    delete Sun_bitmap;
    delete Rain_bitmap;
    delete Big_font;
    delete Tiny_font;
    delete Medium_font;

   End_animation();

   if (DC_ptr != NULL)
      delete DC_ptr;

   }

void Animated_profile_window::SetupWindow ()
{
    TWindow::SetupWindow();

    // INSERT>> Your code here.

   DC_ptr = new TDC(GetDC(*this));

   // setup slider stuff.

   unsigned long Num_days = Param_ptr->Get_end_date() - Param_ptr->Get_start_date();
   Slide_control->SetRange (0, (int) Num_days);
   Slide_control->SetRuler (1, false);
   char St[50];
   Param_ptr->Get_start_date().Write(St);
   Start_date_control->SetText(St);
   Param_ptr->Get_end_date().Write(St);
   End_date_control->SetText(St);

   // change all fonts

   mm_water_control->SetWindowFont (*Big_font, true);
	perc_water_control->SetWindowFont (*Big_font, true);
	Rain_control->SetWindowFont (*Big_font, true);
	Rain_total_control->SetWindowFont (*Big_font, true);
	Runoff_control->SetWindowFont (*Big_font, true);
	Runoff_total_control->SetWindowFont (*Big_font, true);

   Date_control->SetWindowFont (*Medium_font, true);
   Start_date_control->SetWindowFont (*Tiny_font, true);
   End_date_control->SetWindowFont (*Tiny_font, true);

   Display_slider_stuff ();
   }

// ------------------------------------------------------------------
//  Short description:
//    Display dates etc in slider window.

//  Notes:

//  Changes:
//    DPH 7/6/1996

// ------------------------------------------------------------------
void Animated_profile_window::Display_slider_stuff(void)
   {
   char St[50];
   GDate Current_date;
   Current_date = Param_ptr->Get_start_date();
   Current_date += Slide_control->GetPosition();
   Current_date.Write(St);
   Date_control->SetText(St);
   }

bool Animated_profile_window::EvEraseBkgnd (HDC dc)
{
    TWindow::EvEraseBkgnd(dc);

    // INSERT>> Your code here.

    return TRUE;
}

// *******************************************************************
      void Animated_profile_window::Start_animation (void)  {
// *******************************************************************

//  Short description:
//    Start the animation.

//  Notes:

//  Changes:
//    DPH

//  Internal variables

// -------------------- Executable code section ----------------------

   if (Doing_animation)
      End_animation();

   if (Speed_value != stop)
      {
      Doing_animation = TRUE;

      // setup current index so that it is the same as the date slider.

      Current_index = Slide_control->GetPosition();

      // Create a timer.

      int Speed;
      switch (Speed_value)
         {
         case    f : Speed = 1; Going_forwards = true; break;
         case   ff : Speed = 2; Going_forwards = true; break;
         case  fff : Speed = 3; Going_forwards = true; break;
         case    r : Speed = 1; Going_forwards = false; break;
         case   rr : Speed = 2; Going_forwards = false; break;
         case  rrr : Speed = 3; Going_forwards = false; break;
         };

      SetTimer(1, (3 - Speed) * 220 + 100);

      if (Going_forwards)
         Current_index--;
      else
         Current_index++;

      // need to recalc all accumulated fields from start of simulation.

      Recalc();
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    recalc all accumulated fields.

//  Notes:

//  Changes:
//    DPH 11/6/1996

// ------------------------------------------------------------------
void Animated_profile_window::Recalc(void)
   {

   Total_rain_so_far = 0.0;
   Total_runoff_so_far = 0.0;
   for (int Index = 0;
        Index < Current_index;
        Index++)
      {
      Total_rain_so_far += Simul_ptr->Get_saved_rainfall(Index);
      Total_runoff_so_far += Simul_ptr->Get_saved_runoff(Index);
      }
   }

// *******************************************************************
      void Animated_profile_window::End_animation (void)  {
// *******************************************************************

//  Short description:
//    End the animation.

//  Notes:

//  Changes:
//    DPH

//  Internal variables

// -------------------- Executable code section ----------------------

   if (Doing_animation)
      {
      KillTimer(1);
      Doing_animation = FALSE;
      }
   }


// *******************************************************************
      void Animated_profile_window::EvTimer (uint timerId)  {
// *******************************************************************

//  Short description:
//    Timer tick from Windows.

//  Notes:

//  Changes:
//    DPH

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

    TWindow::EvTimer(timerId);

   // INSERT>> Your code here.

   if (!Paint_next_point())
      {
      Simulation_done = TRUE;
      End_animation();
      }
   }

// *******************************************************************
      void Animated_profile_window::Display_statics (void)  {
// *******************************************************************

//  Short description:
//    Display all static windows.

//  Notes:

//  Changes:
//    DPH

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   // fill in current date.

   char St[30];
   GDate Today;
   Today = Param_ptr->Get_start_date();
   Today += Current_index;
   Today.Write(St);
   Date_control->SetText(St);

   // set the mm water control.

   float Total_moisture = Simul_ptr->Get_saved_surface_moisture(Current_index) +
                          Simul_ptr->Get_saved_soil_water (Current_index);
   float sw[NUM_SOIL_LAYERS];
   Param_ptr->Get_calc_soil_water_max(sw);
   float Total_moisture_max = sw[0] + sw[1] + sw[2];

   sprintf(St, "%5.0f", Total_moisture);
   mm_water_control->SetText(St);

   sprintf(St, "%5.0f", Total_moisture / Total_moisture_max* 100.0);
   perc_water_control->SetText(St);

   sprintf(St, "%5.0f", Simul_ptr->Get_saved_rainfall(Current_index));
   Rain_control->SetText(St);
   sprintf(St, "%5.0f", Total_rain_so_far);
   Rain_total_control->SetText(St);

   sprintf(St, "%5.0f", Simul_ptr->Get_saved_runoff(Current_index));
   Runoff_control->SetText(St);
   sprintf(St, "%5.0f", Total_runoff_so_far);
   Runoff_total_control->SetText(St);
   }

// *******************************************************************
      bool Animated_profile_window::Paint_next_point (void)  {
// *******************************************************************

//  Short description:
//    Paint the next set of points.  Return TRUE if all went ok.  FALSE
//    if we run out of data.

//  Notes:

//  Changes:
//    DPH

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   // goto next point.

   if (Going_forwards)
      Current_index++;
   else
      Current_index--;

   if (Current_index >= 0 &&
       Current_index < Simul_ptr->Get_num_days())
       {
       // update slide control

       Slide_control->SetPosition(Current_index);

      // accumulate totals for today.

      if (Going_forwards)
         {
         Total_rain_so_far += Simul_ptr->Get_saved_rainfall(Current_index);
         Total_runoff_so_far += Simul_ptr->Get_saved_runoff(Current_index);
         }
      else
         {
         Total_rain_so_far -= Simul_ptr->Get_saved_rainfall(Current_index);
         Total_runoff_so_far -= Simul_ptr->Get_saved_runoff(Current_index);
         }

      // calculate surface moisture.

      float Surface_moisture = Simul_ptr->Get_saved_surface_moisture(Current_index);

      // calculate profile moisture using a running mean.

      float Profile_water = 0.0;
      if (Going_forwards)
         {
         int Num_values = min(RUNNING_MEAN_DAYS, Current_index) + 1;
         for (int Index = 0;
              Index < Num_values;
              Index++)
            Profile_water += Simul_ptr->Get_saved_soil_water (Current_index - Index);
         Profile_water /= Num_values;
         }
      else
         {
         int Num_values = min(RUNNING_MEAN_DAYS, Current_index) + 1;
         for (int Index = 0;
              Index < Num_values;
              Index++)
            Profile_water += Simul_ptr->Get_saved_soil_water (Current_index - Index);
         Profile_water /= Num_values;
         }

      // animate surface profile.  Make sure if there is some rain that
      // we flash the top layer.  Otherwise simply display the normal
      // shading.

      if (Simul_ptr->Get_saved_rainfall(Current_index) > 0)
         {
         Surface1.Paint(*DC_ptr, 100.0);
         Surface2.Paint(*DC_ptr, 100.0);
         }
      else
         {
         Surface1.Paint(*DC_ptr, Surface_moisture);
         Surface2.Paint(*DC_ptr, Surface_moisture);
         }

      // animate other components.

      Profile1.Paint(*DC_ptr, Profile_water);
      Profile2.Paint(*DC_ptr, Profile_water);

      Evap_shade.Paint(*DC_ptr, Simul_ptr->Get_saved_evap(Current_index));
      Runoff_shade.Paint(*DC_ptr, Simul_ptr->Get_saved_runoff(Current_index));

      // display either a sun or rain depeding if rain today.

      TMemoryDC Memory_dc(*DC_ptr);
      if (Simul_ptr->Get_saved_rainfall(Current_index) > 0)
         {
         // display rain.

         Memory_dc.SelectObject(*Rain_bitmap);
         }
      else
         {
         // display sun

         Memory_dc.SelectObject(*Sun_bitmap);
         }
      DC_ptr->BitBlt(324, 12, 124, 106, Memory_dc, 0, 0, SRCCOPY);

      // display all static boxes.

      Display_statics();
      return TRUE;
      }
   else
      return FALSE;

   }

void Animated_profile_window::Paint (TDC& dc, bool erase, TRect& rect)
{
    TWindow::Paint(dc, erase, rect);

    // INSERT>> Your code here.

   TMemoryDC Memory_dc(dc);
   Memory_dc.SelectObject(*Our_bitmap);
   dc.BitBlt(0, 0, 636, 480, Memory_dc, 0, 0, SRCCOPY);


}

void Animated_profile_window::EvKeyDown (uint key, uint repeatCount, uint flags)
{

    // INSERT>> Your code here.

    if (key == VK_ESCAPE)
      Destroy();
    else
       TWindow::EvKeyDown(key, repeatCount, flags);
}

void Animated_profile_window::Ok_button (void)
{
    // INSERT>> Your code here.

   Destroy();
}

// ------------------------------------------------------------------
//  Short description:
//    Change the background colour of the slide control.

//  Notes:

//  Changes:
//    DPH 7/6/1996

// ------------------------------------------------------------------
HBRUSH Animated_profile_window::EvCtlColor (HDC hDC, HWND hWndChild, uint ctlType)
   {
   static HBRUSH Grey_brush = (HBRUSH) GetStockObject (NULL_BRUSH);

   HBRUSH result;

   result = TWindow::EvCtlColor(hDC, hWndChild, ctlType);

   // INSERT>> Your code here.

   if (hWndChild == Start_date_control->HWindow ||
       hWndChild == End_date_control->HWindow ||
       hWndChild == Date_control->HWindow)
      {
      ::SetBkColor(hDC, TColor(128, 128, 128));
      return Grey_brush;
      }
   else if (hWndChild == Slide_control->HWindow)
      {
//      ::SetBkColor(hDC, TColor(128, 128, 128));
      return (HBRUSH) GetStockObject (GRAY_BRUSH);
      }

   return result;
   }

// ------------------------------------------------------------------
//  Short description:

//  Notes:

//  Changes:
//    DPH 7/6/1996

// ------------------------------------------------------------------
void Animated_profile_window::EvHScroll(uint Scroll_code,
                                        uint Thumb_pos,
                                        HWND Window_ctl)
   {
   TWindow::EvHScroll(Scroll_code,
                      Thumb_pos,
                      Window_ctl);

   Display_slider_stuff();
   End_animation ();
   }

// ------------------------------------------------------------------
//  Short description:
//    The forward button was clicked.

//  Notes:

//  Changes:
//    DPH 11/6/1996

// ------------------------------------------------------------------
void Animated_profile_window::Forward_button (void)
   {
   Speed_value = f;
   Start_animation();
   }

// ------------------------------------------------------------------
//  Short description:
//    The fast forward button was clicked.

//  Notes:

//  Changes:
//    DPH 11/6/1996

// ------------------------------------------------------------------
void Animated_profile_window::FF_button (void)
   {
   if (Speed_value == ff)
      Speed_value = fff;
   else
      Speed_value = ff;
   Start_animation();
   }

// ------------------------------------------------------------------
//  Short description:
//    The rewind button was clicked.

//  Notes:

//  Changes:
//    DPH 11/6/1996

// ------------------------------------------------------------------
void Animated_profile_window::Rewind_button (void)
   {
   Speed_value = r;
   Start_animation();
   }

// ------------------------------------------------------------------
//  Short description:
//    The fast rewind button was clicked.

//  Notes:

//  Changes:
//    DPH 11/6/1996

// ------------------------------------------------------------------
void Animated_profile_window::RR_button (void)
   {
   if (Speed_value == rr)
      Speed_value = rrr;
   else
      Speed_value = rr;
   Start_animation();
   }

// ------------------------------------------------------------------
//  Short description:
//    The stop rewind button was clicked.

//  Notes:

//  Changes:
//    DPH 11/6/1996

// ------------------------------------------------------------------
void Animated_profile_window::Stop_button (void)
   {
   Speed_value = stop;
   Start_animation();
   }

void Animated_profile_window::Help1 ()
   {
   // INSERT>> Your code here.

   So_what_dlg(this, 12, 1).Execute();
   }


void Animated_profile_window::Help2 ()
   {
   // INSERT>> Your code here.

   So_what_dlg(this, 12, 2).Execute();
   }


void Animated_profile_window::Help3 ()
   {
   // INSERT>> Your code here.

   So_what_dlg(this, 12, 3).Execute();
   }


void Animated_profile_window::Help4 ()
   {
   // INSERT>> Your code here.

   So_what_dlg(this, 12, 4).Execute();
   }


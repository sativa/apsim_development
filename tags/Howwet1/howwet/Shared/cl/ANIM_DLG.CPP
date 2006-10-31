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

#include <cl\anim_dlg.h>
//#include <dialogs\anim_dlg.rh>
#include <consts.rh>
#include <gobjs\delay.h>
#include <graph\printout.h>
#include <stdio.h>

#define RESTART 321

#define NUM_TIME_STEPS   100

#define IDC_SLIDE_CONTROL   4000
//
// Build a response table for all messages/commands handled
// by the application.
//
DEFINE_RESPONSE_TABLE1(Animated_dlg, TDialog)
//{{Animated_dlgRSP_TBL_BEGIN}}
    EV_WM_TIMER,
    EV_WM_HSCROLL,
    EV_WM_ERASEBKGND,
    EV_BN_CLICKED(RESTART, Restart_button),
//    EV_WM_CTLCOLOR,
//{{Animated_dlgRSP_TBL_END}}
END_RESPONSE_TABLE;


//{{Animated_dlg Implementation}}


Animated_dlg::Animated_dlg (TWindow*   parent,
                            TResId     resId,
                            TModule*   module)
    :  TDialog(parent, resId, module)
   {
    // INSERT>> Your constructor code here.

   Chart_control_ptr = new Chart_control(this, IDC_PUSHBUTTON1);

   Slide_control = new THSlider (this, IDC_SLIDE_CONTROL);

   Speed_value = 2;
   Doing_animation = FALSE;
   DC_ptr = NULL;
   Simulation_done = FALSE;
   Current_point = 0;
   Printer = new TPrinter;
   }


Animated_dlg::~Animated_dlg ()
{
   Destroy();

   // INSERT>> Your destructor code here.

   End_animation();

   if (DC_ptr != NULL)
      delete DC_ptr;

   delete Printer;
}


// *******************************************************************
      void Animated_dlg::SetupWindow ()  {
// *******************************************************************

//  Short description:

//  Notes:

//  Changes:
//    DPH

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

    TDialog::SetupWindow();

   // INSERT>> Your code here.

   DSeries* Series_ptr = Chart_control_ptr->Get_chart(0)->Series(1);
   Total_number_of_points = Series_ptr->Data[0]->Get_num_elements();
   Points_per_timer = Total_number_of_points / NUM_TIME_STEPS;
   if (Points_per_timer == 0)
      Points_per_timer = 1;
   DC_ptr = new GDC(GetDC(*this));

   Slide_control->PageMagnitude = 1;
   Slide_control->SetRange (0, 5);
   Slide_control->SetRuler (1, true);
   Slide_control->SetPosition(Speed_value);
   Start_animation();
   }

// *******************************************************************
      bool Animated_dlg::EvEraseBkgnd (HDC dc)  {
// *******************************************************************

//  Short description:
//    don't let windows paint our window, or it will flicker.

//  Notes:

//  Changes:
//    DPH

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

    TDialog::EvEraseBkgnd(dc);

   // INSERT>> Your code here.

    return TRUE;
   }

// *******************************************************************
      void Animated_dlg::EvHScroll(uint Scroll_code,
                                   uint Thumb_pos,
                                   HWND Window_ctl)  {
// *******************************************************************

//  Short description:
//    User has changed slide.

//  Notes:

//  Changes:
//    DPH

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   TWindow::EvHScroll(Scroll_code,
                      Thumb_pos,
                      Window_ctl);

   Speed_value = Slide_control->GetPosition();

   // Kill current timer and create new one.

   Start_animation();
   }

// *******************************************************************
      bool Animated_dlg::Paint_next_point (void)  {
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

   int End_point = min(Current_point + Points_per_timer,
                       Total_number_of_points);

   // loop through all charts

   for (int Chart_number = 0;
        Chart_number < Chart_control_ptr->Get_num_charts();
        Chart_number++)
      {
      // loop through all series for this chart.

      int Series_number = 1;
      DSeries *Series_ptr;
      Series_ptr = Chart_control_ptr->Get_chart(Chart_number)->Series(Series_number);
      while (Series_ptr != NULL)
         {
         // select the proper pen and brush into the DC as the paint_points
         // method doesn't do this.

         DC_ptr->Select_object(Series_ptr->Pen);
         DC_ptr->Select_object(Series_ptr->Brush);

         // paint points for this series.

         Series_ptr->Paint_points(*DC_ptr, Current_point, End_point);

         // goto next series.

         Series_number++;
         Series_ptr = Chart_control_ptr->Get_chart(Chart_number)->Series(Series_number);
         }
      }

   Current_point = End_point;

   GDate Today;
   Today = Start_date;
   Today += Current_point - 1;
   Display_date (Today);

   return (Current_point < Total_number_of_points);
   }

// *******************************************************************
      void Animated_dlg::EvTimer (uint timerId)  {
// *******************************************************************

//  Short description:
//    Timer tick from Windows.

//  Notes:

//  Changes:
//    DPH

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

    TDialog::EvTimer(timerId);

   // INSERT>> Your code here.

   if (!Paint_next_point())
      {
      Simulation_done = TRUE;
      End_animation();
      }
   }

// *******************************************************************
      void Animated_dlg::Start_animation (void)  {
// *******************************************************************

//  Short description:
//    Start the animation.

//  Notes:

//  Changes:
//    DPH

//  Internal variables

// -------------------- Executable code section ----------------------

   if (!Simulation_done)
      {
      if (Doing_animation)
         End_animation();

      Doing_animation = TRUE;

      Change_series_visible(FALSE);

      // Create a timer.

      if (Speed_value > 0)
         SetTimer(1, (5 - Speed_value) * 60 + 5);
      }
   }

// *******************************************************************
      void Animated_dlg::End_animation (void)  {
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
      Change_series_visible(TRUE);
      }
   }


// *******************************************************************
      void Animated_dlg::Restart_button ()  {
// *******************************************************************

//  Short description:

//  Notes:

//  Changes:
//    DPH

//  Internal variables
//    none

// -------------------- Executable code section ----------------------


   // INSERT>> Your code here.

   Simulation_done = FALSE;
   End_animation();

   Current_point = 0;

   Invalidate();
   Start_animation();
   }

// *******************************************************************
    void Animated_dlg::Change_series_visible (bool Make_visible)  {
// *******************************************************************

//  Short description:
//    Make all series visible or not visible depending on value of make_visible.

//  Notes:

//  Changes:
//    DPH

//  Internal variables

// -------------------- Executable code section ----------------------


   // INSERT>> Your code here.


   // loop through all charts

   for (int Chart_number = 0;
        Chart_number < Chart_control_ptr->Get_num_charts();
        Chart_number++)
      {
      // loop through all series for this chart.

      int Series_number = 1;
      DSeries *Series_ptr;
      Series_ptr = Chart_control_ptr->Get_chart(Chart_number)->Series(Series_number);
      while (Series_ptr != NULL)
         {
         // make this series visible or not visible.

         Series_ptr->Set_visible(Make_visible);

         // goto next series.

         Series_number++;
         Series_ptr = Chart_control_ptr->Get_chart(Chart_number)->Series(Series_number);
         }
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    Change the background colour of the slide control.

//  Notes:

//  Changes:
//    DPH 7/6/1996

// ------------------------------------------------------------------
HBRUSH Animated_dlg::EvCtlColor (HDC hDC, HWND hWndChild, uint ctlType)
   {
   static HBRUSH Grey_brush = (HBRUSH) GetStockObject (NULL_BRUSH);

   HBRUSH result;

   result = TWindow::EvCtlColor(hDC, hWndChild, ctlType);

   // INSERT>> Your code here.

   if (hWndChild == Slide_control->HWindow)
      {
      ::SetBkColor(hDC, TColor(128, 128, 128));
      return Grey_brush;
      }

   return result;
   }

void anim_dlg_dummy (void)
   {
   new Chart_array(1);
   new Object_array(1);
   new DSeries_array(1);
   }


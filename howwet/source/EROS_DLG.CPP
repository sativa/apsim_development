/*  Project howwet
    APSRU
    Copyright © 1995. All Rights Reserved.

    SUBSYSTEM:    howwet.apx Application
    FILE:         eros_dlg.cpp
    AUTHOR:


    OVERVIEW
    ========
    Source file for implementation of Erosion_dialog (TDialog).
*/

#include <owl\owlpch.h>
#pragma hdrstop

#include <consts.h>
#include "eros_dlg.h"
#include <consts.rh>
#include <stdio.h>
#include <math.h>
#include <gobjs\grealcol.h>
#include <gobjs\gcharcol.h>
#include "so_what.h"

#define MAX_DATA_POINTS 600

//
// Build a response table for all messages/commands handled
// by the application.
//
DEFINE_RESPONSE_TABLE1(Erosion_dialog, TDialog)
//{{Erosion_dialogRSP_TBL_BEGIN}}
    EV_BN_CLICKED(IDC_PUSHBUTTON3, Redraw_graphs),
    EV_BN_CLICKED(INFO1, Help1),
    EV_BN_CLICKED(INFO2, Help2),
    EV_BN_CLICKED(INFO3, Help3),
    EV_BN_CLICKED(INFO4, Help4),
    EV_BN_CLICKED(INFO5, Help5),
//{{Erosion_dialogRSP_TBL_END}}
END_RESPONSE_TABLE;


//{{Erosion_dialog Implementation}}

// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 6/8/1996

// ------------------------------------------------------------------
Erosion_dialog::Erosion_dialog (TWindow*    parent,
                                Parameters* Param_p,
                                Simul*      Simul_p,
                                TResId      resId,
                                TModule*    module)
    : TDialog(parent, resId, module)
   {
   // INSERT>> Your constructor code here.

   All_data.Init(3);
   Slope_edit = new TEdit           (this, IDC_EDIT1);
   Slope_length_edit = new TEdit    (this, IDC_EDIT2);
   Erodability_edit = new TEdit     (this, IDC_EDIT3);
   Start_cover_edit = new TEdit     (this, IDC_EDIT4);
   End_cover_edit = new TEdit       (this, IDC_EDIT5);
   Start_date_static = new TStatic  (this, IDC_STATIC1);
   End_date_static = new TStatic    (this, IDC_STATIC2);
   Runoff_static = new TStatic      (this, IDC_STATIC3);
   Soil_loss_static = new TStatic   (this, IDC_STATIC4);
   Runoff_text = new TStatic        (this, IDC_STATIC5);
   Soil_loss_text = new TStatic     (this, IDC_STATIC6);

   Param_ptr = Param_p;
   Simul_ptr = Simul_p;

   // create chart controls

   Chart_control_ptr1 = new Chart_control (this, IDC_PUSHBUTTON1);
   Chart_control_ptr2 = new Chart_control (this, IDC_PUSHBUTTON2);
   }

// ------------------------------------------------------------------
//  Short description:
//    destructor

//  Notes:

//  Changes:
//    DPH 6/8/1996

// ------------------------------------------------------------------
Erosion_dialog::~Erosion_dialog ()
   {
   Destroy();

   // INSERT>> Your destructor code here.

   }

// ------------------------------------------------------------------
//  Short description:
//    setup dialog

//  Notes:

//  Changes:
//    DPH 6/8/1996

// ------------------------------------------------------------------
void Erosion_dialog::SetupWindow ()
   {
   TDialog::SetupWindow();

   // INSERT>> Your code here.

   char St[40];

   // set the slope box.

   float Slope = Param_ptr->Get_user_slope();
   sprintf (St, "%4.1f", Slope);
   Slope_edit->SetText(St);

   // set the slope length

   Set_slope_length_edit ();

   // set the erodability

   Erodability_edit->SetText ("0.35");

   // set the start and end date boxes.
   
   Param_ptr->Get_start_date().Write(St);
   Start_date_static->SetText(St);
   Param_ptr->Get_end_date().Write(St);
   End_date_static->SetText(St);

   // set the cover boxes.

   Saved_start_cover = Param_ptr->Get_start_soil_cover();
   sprintf (St, "%4.0f", Saved_start_cover);
   Start_cover_edit->SetText (St);

   Saved_end_cover = Param_ptr->Get_end_soil_cover();
   sprintf (St, "%4.0f", Saved_end_cover);
   End_cover_edit->SetText (St);

   Setup_charts ();
   }

// ------------------------------------------------------------------
//  Short description:
//    set the slope length edit box.

//  Notes:

//  Changes:
//    DPH 6/8/1996

// ------------------------------------------------------------------
void Erosion_dialog::Set_slope_length_edit (void)
   {
   char St[20];

   // set the slope box.

   Slope_edit->GetText(St, sizeof St);
   if (strlen(St) > 0)
      {
      float Slope = atof (St);

      float Slope_length = 80.274 * pow (Slope, -0.4521);
      sprintf (St, "%3.0f", Slope_length);
      Slope_length_edit->SetText(St);
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    setup the runoff chart.

//  Notes:

//  Changes:
//    DPH 6/8/1996

// ------------------------------------------------------------------
void Erosion_dialog::Setup_charts (void)
   {
   // Display hourglass cursor

   ::SetCursor(LoadCursor(NULL, IDC_WAIT));

   // setup the covers based on what user has typed.

   float Start_cover;
   float End_cover;
   char St[50];
   Start_cover_edit->GetText (St, sizeof St);
   Start_cover = atof(St);
   End_cover_edit->GetText (St, sizeof St);
   End_cover = atof(St);
   Param_ptr->Set_start_soil_cover(Start_cover);
   Param_ptr->Set_end_soil_cover(End_cover);

   // remove all old charts from chart control

   Chart_control_ptr1->Flush();
   Chart_control_ptr2->Flush();
   All_data.Delete_all_columns();

   All_data.Add(new GChar_column(MAX_DATA_POINTS));     // month
   All_data.Add(new GReal_column(MAX_DATA_POINTS));     // runoff
   All_data.Add(new GReal_column(MAX_DATA_POINTS));     // soil loss

   All_data[0]->Set_heading("Date");
   All_data[1]->Set_heading("Runoff (mm)");
   All_data[2]->Set_heading("Soil loss (t/ha)");

   Simul_ptr->Start_simul(this);

   // Go fill all data.

   char Label[50];
   float Cum_runoff = 0.0;
   float Cum_soil_loss = 0.0;
   for (GDate Date = Param_ptr->Get_start_date();
        Date <= Param_ptr->Get_end_date();
        Date++)
      {
      Simul_ptr->Simul_today();

      // get month string.

      Date.Set_write_option(GDate::MMM);
      Date.Write(Label);
      All_data[0]->Add_char(Label);

      // calculate cumulative runoff

      Cum_runoff += Simul_ptr->Get_todays_runoff();

      // need to get current values for slope, slope length and k

      float Slope, Slope_length, K;
      Get_params (Slope, Slope_length, K);

      // calculate cumulative soil loss

      Cum_soil_loss += Simul_ptr->Get_todays_soil_loss(Slope, Slope_length, K);

      // get actual runoff

      All_data[1]->Add_real(Cum_runoff);
      All_data[2]->Add_real(Cum_soil_loss);
      }

   // put total runoff into runoff control

   sprintf (Label, "%3.0f mm", Cum_runoff);
   Runoff_static->SetText(Label);

   // put total runoff into runoff control

   sprintf (Label, "%4.1f t/ha", Cum_soil_loss);
   Soil_loss_static->SetText(Label);

   // turn off simulation object.

   Simul_ptr->End_simul();

   // setup the series attributes.

   Series_attributes Series_attr;
   Series_attr.Columns_array.Add(&All_data);
   Series_attr.Data_start_index = 0;
   Series_attr.Data_end_index = 0;
   Series_attr.Column_types[0] = x1_axis;
   Series_attr.Column_types[1] = y1_axis;
   Series_attr.Column_types[2] = y1_axis;
   Series_attr.Column_matchups[0] = -1;
   Series_attr.Column_matchups[1] = 0;
   Series_attr.Column_matchups[2] = -1;
   Series_attr.Chart_type[1] = xy_scatter;
   Series_attr.Chart_type[2] = xy_scatter;
   Series_attr.Title = "Runoff over time";

   // create a new runoff chart

   DChart* Runoff_chart_ptr = new DChart;
   Runoff_chart_ptr->Create_series (Series_attr);

   // make runoff line brown.

   DSeries* Series_ptr = Runoff_chart_ptr->Series (1);
   Series_ptr->Pen.Set_colour (GColour(128, 128, 0));

   // create a new soil loss chart.

   Series_attr.Column_matchups[1] = -1;
   Series_attr.Column_matchups[2] = 0;
   Series_attr.Title = "Soil loss over time";
   DChart* Soil_loss_chart_ptr = new DChart;
   Soil_loss_chart_ptr->Create_series (Series_attr);

   // make soil loss line red

   Series_ptr = Soil_loss_chart_ptr->Series (1);
   Series_ptr->Pen.Set_colour (GColour(255, 0, 0));

   // give charts to chart control.

   Chart_control_ptr1->Add_chart(Runoff_chart_ptr);
   Chart_control_ptr2->Add_chart(Soil_loss_chart_ptr);

   // setup the chart and plot area.

   Runoff_chart_ptr->Plot_area()->Point1.Set_world(1500, 1000);
   Runoff_chart_ptr->Plot_area()->Point2.Set_world(9600, 8200);
   Soil_loss_chart_ptr->Plot_area()->Point1.Set_world(1500, 1000);
   Soil_loss_chart_ptr->Plot_area()->Point2.Set_world(9600, 8200);

   // remove the legends.

   Runoff_chart_ptr->Objects.Delete ("legend");
   Soil_loss_chart_ptr->Objects.Delete ("legend");

   // redraw chart controls

   Chart_control_ptr1->Invalidate();
   Chart_control_ptr2->Invalidate();

   // setup the covers back to what they were at the
   // start of the simulation

   Param_ptr->Set_start_soil_cover(Saved_start_cover);
   Param_ptr->Set_end_soil_cover(Saved_end_cover);

   // Display hourglass cursor

   ::SetCursor(LoadCursor(NULL, IDC_ARROW));
   }

// ------------------------------------------------------------------
//  Short description:
//    return current values for params.

//  Notes:

//  Changes:
//    DPH 8/8/1996

// ------------------------------------------------------------------
void Erosion_dialog::Get_params (float&   Slope,
                                 float&   Slope_length,
                                 float&   K)
   {
   char St[20];

   Slope_edit->GetText        (St, sizeof St);
   Slope = atof (St);

   Slope_length_edit->GetText (St, sizeof St);
   Slope_length = atof (St);

   Erodability_edit->GetText  (St, sizeof St);
   K = atof (St);
   }

void Erosion_dialog::Redraw_graphs ()
   {
   // INSERT>> Your code here.

   Setup_charts();
   Runoff_text->Invalidate();
   Soil_loss_text->Invalidate();
   Runoff_static->Invalidate();
   Soil_loss_static->Invalidate();
   }

void Erosion_dialog::Help1 ()
   {
   // INSERT>> Your code here.

   So_what_dlg(this, 11, 1).Execute();
   }


void Erosion_dialog::Help2 ()
   {
   // INSERT>> Your code here.

   So_what_dlg(this, 11, 2).Execute();
   }


void Erosion_dialog::Help3 ()
   {
   // INSERT>> Your code here.

   So_what_dlg(this, 11, 3).Execute();
   }


void Erosion_dialog::Help4 ()
   {
   // INSERT>> Your code here.

   So_what_dlg(this, 11, 4).Execute();
   }


void Erosion_dialog::Help5 ()
   {
   // INSERT>> Your code here.

   So_what_dlg(this, 11, 5).Execute();
   }


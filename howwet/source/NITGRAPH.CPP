/*  Project howwet
    APSRU
    Copyright © 1995. All Rights Reserved.

    SUBSYSTEM:    howwet.apx Application
    FILE:         nitgraph.cpp
    AUTHOR:


    OVERVIEW
    ========
    Source file for implementation of Nitrogen_graph_dlg (TDialog).
*/

#include <graph\global.h>
#include <graph\dchart.h>
#pragma hdrstop

#include "nitgraph.h"
#include <consts.rh>
#include "so_what.h"
#include <values.h>
#include <gobjs\grealcol.h>
#include <gobjs\gcharcol.h>
#include "Rainfall\HowwetRainfall.h"

DEFINE_RESPONSE_TABLE1(Nitrogen_graph, Howwet_animated_dlg)
    EV_BN_CLICKED(INFO1, Help1),
END_RESPONSE_TABLE;

// *******************************************************************
      Nitrogen_graph::Nitrogen_graph (TWindow*     parent,
						                    TResId       resId,
                                      TModule*     module,
                                      Parameters*  Param_p,
                                      Simul*       Simul_p,
                                      RainfallFile* rainfallF)
         : Howwet_animated_dlg(parent, resId, module, Param_p, Simul_p, rainfallF)  {
// *******************************************************************

//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   rainfallFile = rainfallF;
   All_data.Init(5);
   SetCaption("Soil Nitrogen Graph");
   }

// *******************************************************************
      Nitrogen_graph::~Nitrogen_graph (void)  {
// *******************************************************************

//  Short description:
//    destructor

//  Notes:

//  Changes:
//    DPH

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   }


// *******************************************************************
      void Nitrogen_graph::SetupWindow (void)  {
// *******************************************************************

//  Short description:
//    setup all charts.

//  Notes:

//  Changes:
//    DPH

//  Internal variables

// -------------------- Executable code section ----------------------

   // Create charts

   Temp_chart = new DChart();
   Chart_control_ptr->Add_chart(Temp_chart);
   Water_chart = new DChart();
   Chart_control_ptr->Add_chart(Water_chart);
   Nitrate_chart = new DChart();
   Chart_control_ptr->Add_chart(Nitrate_chart);

   // get all data.

   Series_attributes Series_attr;
   Get_all_data (Series_attr);

   // create all series.

   Series_attr.Column_matchups[1] = 0;
   Series_attr.Column_matchups[2] = -1;
   Series_attr.Column_matchups[3] = -1;
//   Series_attr.Column_matchups[4] = -1;
   Temp_chart->Create_series (Series_attr);
   Temp_chart->Title()->Text = "Temperature";
   Temp_chart->Title()->Brush.Set_colour(GColour(192, 192, 192));
   Temp_chart->Title()->Brush.Set_style(BS_Solid);
   Temp_chart->Title()->Set_draw_box(true);

   Series_attr.Column_matchups[1] = -1;
   Series_attr.Column_matchups[2] = 0;
   Series_attr.Column_matchups[3] = -1;
//   Series_attr.Column_matchups[4] = -1;
   Water_chart->Create_series (Series_attr);
   Water_chart->Title()->Text = "Surface moisture";
   Water_chart->Title()->Brush.Set_colour(GColour(192, 192, 192));
   Water_chart->Title()->Brush.Set_style(BS_Solid);
   Water_chart->Title()->Set_draw_box(true);

   Series_attr.Column_matchups[1] = -1;
   Series_attr.Column_matchups[2] = -1;
   Series_attr.Column_matchups[3] = 0;
//   Series_attr.Column_matchups[4] = 0;
   Series_attr.Title = "Gain in nitrate";
   Nitrate_chart->Create_series (Series_attr);
   Nitrate_chart->Title()->Text = "Gain in nitrate";
   Nitrate_chart->Title()->Brush.Set_colour(GColour(192, 192, 192));
   Nitrate_chart->Title()->Brush.Set_style(BS_Solid);
   Nitrate_chart->Title()->Set_draw_box(true);

   // setup all charts

   Setup_temp_chart();
   Setup_water_chart();
   Setup_nitrate_chart();

   // setup lines across graphs

   DRectangle *Rect1 = new DRectangle;
   Rect1->Point1.Set_logical(-MAXFLOAT, -MAXFLOAT, x1_axis, y1_axis);
   Rect1->Point2.Set_logical(MAXFLOAT, MAXFLOAT, x1_axis, y1_axis);
   Temp_chart->Objects.Add(Rect1, Back);

   DRectangle *Rect2 = new DRectangle;
   Rect2->Point1.Set_logical(-MAXFLOAT, -MAXFLOAT, x1_axis, y1_axis);
   Rect2->Point2.Set_logical(MAXFLOAT, MAXFLOAT, x1_axis, y1_axis);
   Water_chart->Objects.Add(Rect2, Back);

   DRectangle *Rect3 = new DRectangle;
   Rect3->Point1.Set_logical(-MAXFLOAT, -MAXFLOAT, x1_axis, y1_axis);
   Rect3->Point2.Set_logical(MAXFLOAT, MAXFLOAT, x1_axis, y1_axis);
   Nitrate_chart->Objects.Add(Rect3, Back);

   // go setup animated dialog window

   Howwet_animated_dlg::SetupWindow();
   }

// *******************************************************************
      void Nitrogen_graph::Get_all_data
         (Series_attributes& Series_attr)  {
// *******************************************************************

//  Short description:
//    Fill the series attributes structure.

//  Notes:

//  Changes:
//    DPH

//  Internal variables
      GDate Date;                      // Current date.
      char Label[20];                  // month label

// -------------------- Executable code section ----------------------

   All_data.Add(new GChar_column(MAX_DATA_POINTS));     // month
   All_data.Add(new GReal_column(MAX_DATA_POINTS));     // average rain.
   All_data.Add(new GReal_column(MAX_DATA_POINTS));     // actual rain.
   All_data.Add(new GReal_column(MAX_DATA_POINTS));     // actual rain.
//   All_data.Add(new GReal_column(MAX_DATA_POINTS));     // actual rain.
   Series_attr.Columns_array.Add(&All_data);

   All_data[0]->Set_heading("Month");
   All_data[1]->Set_heading("oC");
   All_data[2]->Set_heading("mm");
   All_data[3]->Set_heading("kg/ha");
//   All_data[4]->Set_heading("kg/ha");

   Simul_ptr->Start_simul(this);

   // Go fill all data.

//   float Yesterdays_n = 0.0;
   for (Date = Param_ptr->Get_start_date();
        Date <= Param_ptr->Get_end_date();
        Date++)
      {
      Simul_ptr->Simul_today();

      // get month string.

      Date.Set_write_option(GDate::MMM);
      Date.Write(Label);
      All_data[0]->Add_char(Label);

      // get temperature

      All_data[1]->Add_real(Param_ptr->Get_temp(Date));

      // get soil surface moisture

      All_data[2]->Add_real(Simul_ptr->Get_todays_surface_moisture());

      // get accumulated nitrate

      float Todays_n = Simul_ptr->Get_todays_nitrate();
      All_data[3]->Add_real(Todays_n);

      // get todays minz n

//      All_data[4]->Add_real(Todays_n - Yesterdays_n);
//      Yesterdays_n = Todays_n;
      }

   Simul_ptr->End_simul();

   Series_attr.Data_start_index = 0;
   Series_attr.Data_end_index = 0;
   Series_attr.Column_types[0] = x1_axis;
   Series_attr.Column_types[1] = y1_axis;
   Series_attr.Column_types[2] = y1_axis;
   Series_attr.Column_types[3] = y1_axis;
//   Series_attr.Column_types[4] = y1_axis;
   Series_attr.Column_matchups[0] = -1;
   Series_attr.Chart_type[1] = xy_scatter;
   Series_attr.Chart_type[2] = xy_scatter;
   Series_attr.Chart_type[3] = xy_scatter;
//   Series_attr.Chart_type[4] = column;
   Series_attr.X_heading = "";
   }

// *******************************************************************
      void Nitrogen_graph::Setup_temp_chart (void)  {
// *******************************************************************

//  Short description:
//    setup temp chart

//  Notes:

//  Changes:
//    DPH

//  Internal variables

// -------------------- Executable code section ----------------------

   // setup the chart and plot area.

   Temp_chart->Chart_area()->Point1.Set_world(0, 0);
   Temp_chart->Chart_area()->Point2.Set_world(10000, 3100);
   Temp_chart->Chart_area()->Pen.Set_style(PS_Null);

   Temp_chart->Plot_area()->Point1.Set_world(1500, 1000);
   Temp_chart->Plot_area()->Point2.Set_world(8500, 8500);
   Temp_chart->Plot_area()->Pen.Set_style(PS_Solid);
   Temp_chart->Plot_area()->Pen.Set_colour(GColour(128, 128, 128));
   Temp_chart->Plot_area()->Brush.Set_style(BS_Null);

   // setup temp series.

   DXY_scatter_series *Temp_series = TYPESAFE_DOWNCAST(Temp_chart->Series(1),
                                                       DXY_scatter_series);
   Temp_series->Marker.Set_marker_type(M_none);
   Temp_series->Set_colour_type(GColour(255, 0, 0));
   Temp_series->Pen.Set_width(3);

   // change y axis heading rotation

   DAxis* Axis_ptr = Temp_chart->Y1_axis();
   Axis_ptr->Title.Font.Set_angle(0);
   GMin_max_scale *Scale_ptr = TYPESAFE_DOWNCAST(&Axis_ptr->Axis_scale(), GMin_max_scale);
   Scale_ptr->Set_fixed_min_max_int(0, 30, 10);

   // set the x axis to invisible.

   Axis_ptr = Temp_chart->X1_axis();
   Axis_ptr->Set_visible(FALSE);

   // make legend invisible.

   Temp_chart->Legend()->Set_visible(FALSE);
   }

// *******************************************************************
      void Nitrogen_graph::Setup_water_chart (void)  {
// *******************************************************************

//  Short description:
//    setup water chart

//  Notes:

//  Changes:
//    DPH

//  Internal variables

// -------------------- Executable code section ----------------------

   // setup the chart and plot area.

   Water_chart->Chart_area()->Point1.Set_world(0, 3100);
   Water_chart->Chart_area()->Point2.Set_world(10000, 6200);
   Water_chart->Chart_area()->Pen.Set_style(PS_Null);

   Water_chart->Plot_area()->Point1.Set_world(1500, 1000);
   Water_chart->Plot_area()->Point2.Set_world(8500, 8500);
   Water_chart->Plot_area()->Pen.Set_style(PS_Solid);
   Water_chart->Plot_area()->Pen.Set_colour(GColour(128, 128, 128));
   Water_chart->Plot_area()->Brush.Set_style(BS_Null);

   // setup series.

   DXY_scatter_series *Water_series = TYPESAFE_DOWNCAST(Water_chart->Series(1),
                                                       DXY_scatter_series);
   Water_series->Marker.Set_marker_type(M_none);
   Water_series->Set_colour_type(GColour(0, 0, 255));
   Water_series->Pen.Set_width(3);

   // change y axis heading rotation

   DAxis* Axis_ptr = Water_chart->Y1_axis();
   Axis_ptr->Title.Font.Set_angle(0);
   GMin_max_scale *Scale_ptr = TYPESAFE_DOWNCAST(&Axis_ptr->Axis_scale(), GMin_max_scale);
   Scale_ptr->Set_fixed_min_max_int(-20, 60, 20);

   // set the x axis to invisible.

   Axis_ptr = Water_chart->X1_axis();
   Axis_ptr->Set_visible(FALSE);

   // make legend invisible.

   Water_chart->Legend()->Set_visible(FALSE);
   }

// *******************************************************************
      void Nitrogen_graph::Setup_nitrate_chart (void)  {
// *******************************************************************

//  Short description:
//    setup nitrate chart

//  Notes:

//  Changes:
//    DPH

//  Internal variables

// -------------------- Executable code section ----------------------

   // setup the chart and plot area.

   Nitrate_chart->Chart_area()->Point1.Set_world(0, 6200);
   Nitrate_chart->Chart_area()->Point2.Set_world(10000, 9300);
   Nitrate_chart->Chart_area()->Pen.Set_style(PS_Null);

   Nitrate_chart->Plot_area()->Point1.Set_world(1500, 1000);
   Nitrate_chart->Plot_area()->Point2.Set_world(8500, 8500);
   Nitrate_chart->Plot_area()->Pen.Set_style(PS_Solid);
   Nitrate_chart->Plot_area()->Pen.Set_colour(GColour(128, 128, 128));
   Nitrate_chart->Plot_area()->Brush.Set_style(BS_Null);

   // setup series.

   DXY_scatter_series *Nitrate_series = TYPESAFE_DOWNCAST(Nitrate_chart->Series(1),
                                                       DXY_scatter_series);
   Nitrate_series->Marker.Set_marker_type(M_none);
   Nitrate_series->Set_colour_type(GColour(0, 255, 0));
   Nitrate_series->Pen.Set_width(3);

   // change y axis heading rotation

   DAxis* Axis_ptr = Nitrate_chart->Y1_axis();
   Axis_ptr->Title.Font.Set_angle(0);

   // make legend invisible.

   Nitrate_chart->Legend()->Set_visible(FALSE);
   }

// *******************************************************************
      void Nitrogen_graph::Help1 ()  {
// *******************************************************************

//  Short description:

//  Notes:

//  Changes:
//    DPH 

//  Internal variables
//    none

// -------------------- Executable code section ----------------------


   // INSERT>> Your code here.

   So_what_dlg(this, 6, 1).Execute();
   }


/*  Project howwet
    APSRU
    Copyright © 1995. All Rights Reserved.

    SUBSYSTEM:    howwet.apx Application
    FILE:         sw_graph.cpp
    AUTHOR:


    OVERVIEW
    ========
    Source file for implementation of SW_graph_dlg (TDialog).
*/

#include <graph\global.h>
#include <graph\dchart.h>
#pragma hdrstop

#include "sw_graph.h"
#include <consts.rh>
#include "so_what.h"
#include <values.h>
#include <gobjs\grealcol.h>
#include <gobjs\gcharcol.h>
#include "Rainfall\HowwetRainfall.h"

DEFINE_RESPONSE_TABLE1(SW_graph, Howwet_animated_dlg)
    EV_BN_CLICKED(INFO1, Help1),
END_RESPONSE_TABLE;

// *******************************************************************
      SW_graph::SW_graph (TWindow*     parent,
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

   All_data.Init(4);

   // Tell simulator object to get ready to simulate.

   SetCaption("Rainfall and Soil Water Graph");
   }

// *******************************************************************
      SW_graph::~SW_graph (void)  {
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
      void SW_graph::SetupWindow (void)  {
// *******************************************************************

//  Short description:
//    setup all charts.

//  Notes:

//  Changes:
//    DPH

//  Internal variables
   GPoint Point;

// -------------------- Executable code section ----------------------

   // Create charts

   SW_chart = new DChart();
   Chart_control_ptr->Add_chart(SW_chart);

   // setup the chart and plot area.

   SW_chart->Chart_area()->Point1.Set_world(0, 0);
   SW_chart->Chart_area()->Point2.Set_world(10000, 10000);
   SW_chart->Chart_area()->Pen.Set_style(PS_Null);

   SW_chart->Plot_area()->Point1.Set_world(1500, 1000);
   SW_chart->Plot_area()->Point2.Set_world(8500, 8500);

   // get all data.

   Series_attributes Series_attr;
   Get_all_data (Series_attr);

   // create all series.

   SW_chart->Create_series (Series_attr);

   // setup soil water series

   DXY_scatter_series *SW_series = TYPESAFE_DOWNCAST(SW_chart->Series(1),
                                                     DXY_scatter_series);
   SW_series->Marker.Set_marker_type(M_none);
   SW_series->Set_colour_type(GColour(0, 0, 255));
   SW_series->Pen.Set_width(3);

   // Setup rainfall series

   DColumn_series *Rain_series = TYPESAFE_DOWNCAST(SW_chart->Series(2),
                                                   DColumn_series);
   Rain_series->Pen.Set_colour(GColour(0, 255, 0));
   Rain_series->Brush.Set_colour(GColour(0, 255, 0));
   Rain_series->Brush.Set_style(BS_Solid);
   Rain_series->Set_bar_width(1);
   Rain_series->Name = "Rain (mm)";

   // Setup runoff series

   DColumn_series *Runoff_series = TYPESAFE_DOWNCAST(SW_chart->Series(3),
                                                     DColumn_series);
   Runoff_series->Pen.Set_colour(GColour(255, 0, 0));
   Runoff_series->Brush.Set_colour(GColour(255, 0, 0));
   Runoff_series->Brush.Set_style(BS_Solid);
   Runoff_series->Set_bar_width(1);
   Runoff_series->Name = "Runoff (mm)";

   // Setup legend

   DLegend* Legend = SW_chart->Legend();
   Legend->Set_num_rows_columns(1, 2);
   GPoint Legend_point;
   Legend_point.Set_world(5000, 700);
   Legend->Set_position(Legend_point,
                        TA_CENTER,
                        TA_CENTER);

   // change y axis heading rotation and fix the maximum scale.

   DAxis* Axis_ptr = SW_chart->Y1_axis();
   Axis_ptr->Title.Font.Set_angle(0);
   GMin_max_scale* Scale_ptr = TYPESAFE_DOWNCAST(&Axis_ptr->Axis_scale(), GMin_max_scale);
   Scale_ptr->Set_auto_min_max(0.0,
                               Param_ptr->Get_calc_soil_water_max());

   // setup full and half lines.

   float Num_days = Param_ptr->Get_end_date() - Param_ptr->Get_start_date();
   DLine *Full_line = new DLine;
   Full_line->Point1.Set_logical(0, Param_ptr->Get_calc_soil_water_max(), x1_axis, y1_axis);
   Full_line->Point2.Set_logical(Num_days, Param_ptr->Get_calc_soil_water_max(), x1_axis, y1_axis);
   Full_line->Pen.Set_style(PS_Dot);
   Full_line->Pen.Set_colour(GColour(255,255,0));
   SW_chart->Objects.Add(Full_line);

   DLine *Half_line = new DLine;
   Half_line->Point1.Set_logical(0, Param_ptr->Get_calc_soil_water_max() / 2.0, x1_axis, y1_axis);
   Half_line->Point2.Set_logical(Num_days, Param_ptr->Get_calc_soil_water_max() / 2.0, x1_axis, y1_axis);
   Half_line->Pen.Set_style(PS_Dot);
   Half_line->Pen.Set_colour(GColour(255,255,0));
   SW_chart->Objects.Add(Half_line);

   // Set full and half text.

   DText* Full_text = new DText;
   Full_text->Text = "Full";
   Point.Set_logical(0, Param_ptr->Get_calc_soil_water_max(), x1_axis, y1_axis);
   Point.Add_mm(5, -5);
   Full_text->Set_position(Point,
                           TA_CENTER,
                           TA_CENTER);
   Full_text->Pen.Set_colour(GColour(255,255,0));
   SW_chart->Objects.Add(Full_text);

   DText* Half_text = new DText;
   Half_text->Text = "Half";
   Point.Set_logical(0, Param_ptr->Get_calc_soil_water_max() / 2.0, x1_axis, y1_axis);
   Point.Add_mm(5, -5);
   Half_text->Set_position(Point,
                           TA_CENTER,
                           TA_CENTER);
   Half_text->Pen.Set_colour(GColour(255,255,0));
   SW_chart->Objects.Add(Half_text);

   // go setup animated dialog window

   Howwet_animated_dlg::SetupWindow();

   }

// *******************************************************************
      void SW_graph::Get_all_data
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
   Series_attr.Columns_array.Add(&All_data);

   All_data[0]->Set_heading("Month");
   All_data[1]->Set_heading("Plant available water (mm)");
   All_data[2]->Set_heading("Rain (mm)");
   All_data[3]->Set_heading("Runoff (mm)");

   Simul_ptr->Start_simul(this);

   // Go fill all data.

   for (Date = Param_ptr->Get_start_date();
        Date <= Param_ptr->Get_end_date();
        Date++)
      {
      Simul_ptr->Simul_today();

      // get month string.

      Date.Set_write_option(GDate::MMM);
      Date.Write(Label);
      All_data[0]->Add_char(Label);

      // get soil water

      All_data[1]->Add_real(Simul_ptr->Get_todays_soil_water());

      // get actual rain.

      All_data[2]->Add_real(Simul_ptr->Get_todays_rain());

      // get actual runoff

      All_data[3]->Add_real(Simul_ptr->Get_todays_runoff());
      }

   Simul_ptr->End_simul();

   Series_attr.Data_start_index = 0;
   Series_attr.Data_end_index = 0;
   Series_attr.Column_types[0] = x1_axis;
   Series_attr.Column_types[1] = y1_axis;
   Series_attr.Column_types[2] = y1_axis;
   Series_attr.Column_types[3] = y1_axis;
   Series_attr.Column_matchups[0] = -1;
   Series_attr.Column_matchups[1] = 0;
   Series_attr.Column_matchups[2] = 0;
   Series_attr.Column_matchups[3] = 0;
   Series_attr.Chart_type[1] = xy_scatter;
   Series_attr.Chart_type[2] = column;
   Series_attr.Chart_type[3] = column;
   Series_attr.Title = "Accumulation of rainfall";
   Series_attr.Y_heading = "mm";
   Series_attr.X_heading = "";
   }

// *******************************************************************
      void SW_graph::Help1 ()  {
// *******************************************************************

//  Short description:

//  Notes:

//  Changes:
//    DPH

//  Internal variables
//    none

// -------------------- Executable code section ----------------------


   // INSERT>> Your code here.

   So_what_dlg(this, 7, 1).Execute();
   }


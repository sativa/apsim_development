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

#include "rain_avg.h"
#include <consts.rh>
#include "so_what.h"
#include <values.h>
#include <gobjs\grealcol.h>
#include <gobjs\gcharcol.h>
#include "Rainfall\HowwetRainfall.h"

DEFINE_RESPONSE_TABLE1(Rain_avg_graph, Howwet_animated_dlg)
    EV_BN_CLICKED(INFO1, Help1),
END_RESPONSE_TABLE;

// *******************************************************************
      Rain_avg_graph::Rain_avg_graph
                         (TWindow*     parent,
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
   All_data.Init(3);
   SetCaption("Monthly Rainfall Graph");
   }

// *******************************************************************
      Rain_avg_graph::~Rain_avg_graph (void)  {
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
      void Rain_avg_graph::SetupWindow (void)  {
// *******************************************************************

//  Short description:
//    setup all charts.

//  Notes:

//  Changes:
//    DPH

//  Internal variables
   GPoint Title_point;

// -------------------- Executable code section ----------------------

   // Create chart

   Rain_avg_chart = new DChart();
   Chart_control_ptr->Add_chart(Rain_avg_chart);

   // setup the chart and plot area.

   Rain_avg_chart->Chart_area()->Point1.Set_world(0, 0);
   Rain_avg_chart->Chart_area()->Point2.Set_world(10000, 10000);
   Rain_avg_chart->Chart_area()->Pen.Set_style(PS_Null);

   Rain_avg_chart->Plot_area()->Point1.Set_world(1500, 1000);
   Rain_avg_chart->Plot_area()->Point2.Set_world(8500, 8500);

   // get all data.

   Series_attributes Series_attr;
   Get_all_data (Series_attr);

   // create all series.

   Rain_avg_chart->Create_series (Series_attr);

   // setup avg_series colours and attributes.

   DXY_scatter_series *Avg_series =
         TYPESAFE_DOWNCAST(Rain_avg_chart->Series(1), DXY_scatter_series);
   Avg_series->Marker.Set_marker_type(M_none);
   Avg_series->Set_colour_type(GColour(RGB(0, 0, 255)));
   Avg_series->Pen.Set_width(3);

   // setup rain_series colours and attributes.

   DColumn_series *Rain_series = TYPESAFE_DOWNCAST(Rain_avg_chart->Series(2),
                                                   DColumn_series);
   Rain_series->Pen.Set_colour(GColour(0, 255, 0));
   Rain_series->Brush.Set_colour(GColour(0, 255, 0));
   Rain_series->Brush.Set_style(BS_Solid);
   Rain_series->Set_bar_width(8);

   // Setup legend

   DLegend* Legend = Rain_avg_chart->Legend();
   Legend->Set_num_rows_columns(1, 2);
   GPoint Legend_point;
   Legend_point.Set_world(5000, 700);
   Legend->Set_position(Legend_point,
                        TA_CENTER,
                        TA_CENTER);

   // change y axis heading rotation.

   DAxis* Axis_ptr = Rain_avg_chart->Y1_axis();
   Axis_ptr->Title.Font.Set_angle(0);

   // go setup animated dialog window

   Howwet_animated_dlg::SetupWindow();
   }

// *******************************************************************
      void Rain_avg_graph::Get_all_data
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
   Series_attr.Columns_array.Add(&All_data);

   All_data[0]->Set_heading("Month");
   All_data[1]->Set_heading("Average longterm rainfall (mm)");
   All_data[2]->Set_heading("Rain (mm)");

   // Go fill all data.

   for (Date = Param_ptr->Get_start_date();
        Date <= Param_ptr->Get_end_date();
        Date.Add_months(1))
      {
      // get month string.

      Date.Set_write_option(GDate::MMM);
      Date.Write(Label);
      All_data[0]->Add_char(Label);

      // get average rain.

      All_data[1]->Add_real(Param_ptr->Get_average_rain_for_month(Date.Get_month()));

      // get actual rain.

      All_data[2]->Add_real(getRainForMonth(rainfallFile, Date.Get_month(), Date.Get_year()));
      }
   Series_attr.Data_start_index = 0;
   Series_attr.Data_end_index = 0;
   Series_attr.Column_types[0] = x1_axis;
   Series_attr.Column_types[1] = y1_axis;
   Series_attr.Column_types[2] = y1_axis;
   Series_attr.Column_matchups[0] = -1;
   Series_attr.Column_matchups[1] = 0;
   Series_attr.Column_matchups[2] = 0;
   Series_attr.Chart_type[1] = xy_scatter;
   Series_attr.Chart_type[2] = column;
   Series_attr.Title = "Actual and longterm rainfall";
   Series_attr.Y_heading = "mm";
   Series_attr.X_heading = "";
   }

// *******************************************************************
      void Rain_avg_graph::Help1 ()  {
// *******************************************************************

//  Short description:

//  Notes:

//  Changes:
//    DPH

//  Internal variables
//    none

// -------------------- Executable code section ----------------------


   // INSERT>> Your code here.

   So_what_dlg(this, 8, 1).Execute();
   }

void rain_avg_graph_dummy(void)
   {
   new GColumns_array(1);
   }


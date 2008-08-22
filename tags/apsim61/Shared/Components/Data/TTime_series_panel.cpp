//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TTime_series_panel.h"
#include "TTime_series_analysis.h"

#pragma link "TAnalysis_panel"
using namespace std;
#pragma package(smart_init)
//---------------------------------------------------------------------------
// ValidCtrCheck is used to assure that the components created do not have
// any pure virtual functions.
//

static inline void ValidCtrCheck(TTime_series_panel *)
{
   new TTime_series_panel(NULL);
}
//---------------------------------------------------------------------------
namespace Ttime_series_panel
{
   void __fastcall PACKAGE Register()
   {
      TComponentClass classes[1] = {__classid(TTime_series_panel)};
      RegisterComponents("APSRU", classes, 0);
   }
}
// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 20/7/1998

// ------------------------------------------------------------------
__fastcall TTime_series_panel::TTime_series_panel(TComponent* Owner)
   : TAnalysis_panel(Owner)
   {
   }
// ------------------------------------------------------------------
//  Short description:
//    create all objects.

//  Notes:

//  Changes:
//    DPH 20/7/1998

// ------------------------------------------------------------------
void TTime_series_panel::Create_objects (void)
   {
   Analyses[0] = new TTime_series_analysis(this);
   Analyses[1] = NULL;
   Analysis_charts[0] = new TAnalysis_chart(this);
   Analysis_charts[1] = NULL;
   }

// ------------------------------------------------------------------
//  Short description:
//    refresh the chart object

//  Notes:

//  Changes:
//    DPH 20/7/1998
//    DAH 6/2/02:    D-496 temporary fix. This module and associated panel expect
//                   a field called "Statistic" which interfered with the field
//                   "StatisticName" in the macro substitution

// ------------------------------------------------------------------
void TTime_series_panel::Refresh_chart_objects (void)
   {
   TTime_series_analysis* timeSeriesPtr = dynamic_cast<TTime_series_analysis*> (Analyses[0]);

   // clear old series.
   Analysis_charts[0]->AnalysisSeriesList->Clear();

   // put a mean line on chart if necessary.
   if (timeSeriesPtr->Diff_from_mean || timeSeriesPtr->Diff_from_percentile ||
       timeSeriesPtr->With_mean_line || timeSeriesPtr->With_percentile_line)
      {
      TAnalysisSeries* statisticSeries = Analysis_charts[0]->AnalysisSeriesList->Add();
      statisticSeries->FieldNames->Add("Year");
      statisticSeries->FieldNames->Add("Statistic");
      statisticSeries->SeriesType = lines;
      //statisticSeries->SeriesTitle = "%StatisticName";
      statisticSeries->SeriesTitle = "%StatName";
      statisticSeries->SingleSeriesOnly = true;
      statisticSeries->PutLabelOnY = false;
      }
   string title;
   for (int field = 0; field < timeSeriesPtr->Field_names_to_analyse->Items->Count; field++)
      {
      // Put the normal time series bars on chart.
      TAnalysisSeries* series = Analysis_charts[0]->AnalysisSeriesList->Add();
      string fieldName = timeSeriesPtr->Field_names_to_analyse->Items->Strings[field].c_str();
      series->FieldNames->Add("Year");
      series->FieldNames->Add(fieldName.c_str());
      series->SeriesType = vert_bar;
      if (timeSeriesPtr->Field_names_to_analyse->Items->Count == 1)
         series->SeriesTitle = "%Simulation";
      else
         series->SeriesTitle = string(fieldName + "(%Simulation)").c_str();

      // Give chart a title.
      if (title != "")
         title += " and ";
      title += fieldName;
      }

   if (timeSeriesPtr->Diff_from_mean || timeSeriesPtr->Diff_from_percentile)
      title += " Differences Over Time";
   else
      title += " Over Time";
   Analysis_charts[0]->ChartTitle = title.c_str();

   // call to base class.
   TAnalysis_panel::Refresh_chart_objects();
   }


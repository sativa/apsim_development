//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TDifference_panel.h"
#include "TDifference_analysis.h"
using namespace std;
#pragma link "TAnalysis_panel"
#pragma package(smart_init)
//---------------------------------------------------------------------------
// ValidCtrCheck is used to assure that the components created do not have
// any pure virtual functions.
//

static inline void ValidCtrCheck(TDifference_panel *)
{
   new TDifference_panel(NULL);
}
//---------------------------------------------------------------------------
namespace Tdifference_panel
{
   void __fastcall PACKAGE Register()
   {
      TComponentClass classes[1] = {__classid(TDifference_panel)};
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
__fastcall TDifference_panel::TDifference_panel(TComponent* Owner)
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
void TDifference_panel::Create_objects (void)
   {
   Analyses[0] = new TDifference_analysis(this);
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

// ------------------------------------------------------------------
void TDifference_panel::Refresh_chart_objects (void)
   {
   TDifference_analysis* differencePtr = dynamic_cast<TDifference_analysis*> (Analyses[0]);

   if (differencePtr->Field_names_to_analyse->Items->Count > 0)
      {
      // clear old series.
      Analysis_charts[0]->AnalysisSeriesList->Clear();

      string fieldName = differencePtr->Field_names_to_analyse->Items->Strings[0].c_str();
      TAnalysisSeries* statisticSeries = Analysis_charts[0]->AnalysisSeriesList->Add();
      statisticSeries->FieldNames->Add("Year");
      statisticSeries->FieldNames->Add(fieldName.c_str());
      statisticSeries->SeriesType = vert_bar;
      statisticSeries->SeriesTitle = "%Simulation";

      // Give chart a title.
      string title = fieldName + " Differences Over Time";
      Analysis_charts[0]->ChartTitle = title.c_str();

      // call to base class.
      TAnalysis_panel::Refresh_chart_objects();
      }
   }


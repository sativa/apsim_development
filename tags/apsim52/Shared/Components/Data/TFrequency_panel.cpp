//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TFrequency_panel.h"
#include "TFrequency_analysis.h"
using namespace std;
#pragma link "TAnalysis_panel"
#pragma package(smart_init)
//---------------------------------------------------------------------------
// ValidCtrCheck is used to assure that the components created do not have
// any pure virtual functions.
//

static inline void ValidCtrCheck(TFrequency_panel *)
{
   new TFrequency_panel(NULL);
}
//---------------------------------------------------------------------------
namespace Tfrequency_panel
{
   void __fastcall PACKAGE Register()
   {
      TComponentClass classes[1] = {__classid(TFrequency_panel)};
      RegisterComponents("APSRU", classes, 0);
   }
}
// ------------------------------------------------------------------
//  Short description:
//      constructor

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
__fastcall TFrequency_panel::TFrequency_panel(TComponent* Owner)
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
void TFrequency_panel::Create_objects (void)
   {
   Analyses[0] = new TFrequency_analysis(this);
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
//    DAH 7/2/2002:  D-408 fix

// ------------------------------------------------------------------
void TFrequency_panel::Refresh_chart_objects (void)
   {
   TFrequency_analysis* frequencyPtr = dynamic_cast<TFrequency_analysis*> (Analyses[0]);

   if (frequencyPtr->Field_names_to_analyse->Items->Count > 0)
      {
      // clear old series.
      Analysis_charts[0]->AnalysisSeriesList->Clear();

      string fieldName = frequencyPtr->Field_names_to_analyse->Items->Strings[0].c_str();
      TAnalysisSeries* statisticSeries = Analysis_charts[0]->AnalysisSeriesList->Add();
      statisticSeries->FieldNames->Add(fieldName.c_str());
      if (frequencyPtr->Frequency_analysis)
         statisticSeries->FieldNames->Add(FREQUENCY_FIELD_NAME);
      else
         statisticSeries->FieldNames->Add(PROBABILITY_FIELD_NAME);
      statisticSeries->SeriesType = vert_bar;
      statisticSeries->SeriesTitle = "%Simulation";

      // Give chart a title.
      string title = fieldName + " ";
      if (frequencyPtr->Frequency_analysis)
         title += FREQUENCY_FIELD_NAME;
      else
         title += PROBABILITY_FIELD_NAME;
      title += " Plot";
      Analysis_charts[0]->ChartTitle = title.c_str();

      // call to base class.
      TAnalysis_panel::Refresh_chart_objects();
      }
   }


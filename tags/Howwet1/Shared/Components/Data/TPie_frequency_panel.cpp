//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TPie_frequency_panel.h"
#include "TPie_frequency_analysis.h"
using namespace std;
#pragma link "TAnalysis_panel"
#pragma package(smart_init)
//---------------------------------------------------------------------------
// ValidCtrCheck is used to assure that the components created do not have
// any pure virtual functions.
//

static inline void ValidCtrCheck(TPie_frequency_panel *)
{
   new TPie_frequency_panel(NULL);
}
//---------------------------------------------------------------------------
namespace Tpie_frequency_panel
{
   void __fastcall PACKAGE Register()
   {
      TComponentClass classes[1] = {__classid(TPie_frequency_panel)};
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
__fastcall TPie_frequency_panel::TPie_frequency_panel(TComponent* Owner)
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
void TPie_frequency_panel::Create_objects (void)
   {
   Analyses[0] = new TPie_frequency_analysis(this);
   Analyses[1] = NULL;
   Analysis_charts[0] = new TAnalysis_chart(this);
   Analysis_charts[1] = new TAnalysis_chart(this);
   Analysis_charts[2] = NULL;
   }

// ------------------------------------------------------------------
//  Short description:
//    refresh the chart object

//  Notes:

//  Changes:
//    DPH 20/7/1998

// ------------------------------------------------------------------
void TPie_frequency_panel::Refresh_chart_objects (void)
   {
   TPie_frequency_analysis* frequencyPtr = dynamic_cast<TPie_frequency_analysis*> (Analyses[0]);

   if (frequencyPtr->Field_names_to_analyse->Items->Count > 0)
      {
      Analysis_charts[1]->APSTable = Analyses[0];

      // clear old series.
      Analysis_charts[0]->AnalysisSeriesList->Clear();

      string fieldName = frequencyPtr->Field_names_to_analyse->Items->Strings[0].c_str();
      TAnalysisSeries* pie1Series = Analysis_charts[0]->AnalysisSeriesList->Add();
      pie1Series->FieldNames->Add(fieldName.c_str());
      pie1Series->FieldNames->Add("Frequency");
      pie1Series->SeriesType = pie;
      pie1Series->SeriesTitle = "%Simulation";
      pie1Series->SpecificDataBlockName = frequencyPtr->BaseDatasetName;
      pie1Series->SingleSeriesOnly = true;

      TAnalysisSeries* pie2Series = Analysis_charts[1]->AnalysisSeriesList->Add();
      pie2Series->FieldNames->Add(fieldName.c_str());
      pie2Series->FieldNames->Add("Frequency");
      pie2Series->SeriesType = pie;
      pie2Series->SeriesTitle = "%Simulation";
      pie2Series->SpecificDataBlockName = frequencyPtr->SecondDatasetName;
      pie2Series->SingleSeriesOnly = true;

      // Give chart a title.
      Analysis_charts[0]->ChartTitle = frequencyPtr->BaseDatasetName;
      Analysis_charts[1]->ChartTitle = frequencyPtr->SecondDatasetName;

      // Stop the series names being put into the legend.  We want series values.
      Analysis_charts[0]->ShowSeriesNamesInLegend = false;
      Analysis_charts[1]->ShowSeriesNamesInLegend = false;

      // call to base class.
      TAnalysis_panel::Refresh_chart_objects();
      }
   }


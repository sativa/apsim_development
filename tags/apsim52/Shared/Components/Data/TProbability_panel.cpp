//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TProbability_panel.h"
#include "TProbability_analysis.h"

#pragma link "TAnalysis_panel"
//#pragma link "TAuto_size_panel"
#pragma package(smart_init)
//---------------------------------------------------------------------------
// ValidCtrCheck is used to assure that the components created do not have
// any pure virtual functions.
//

static inline void ValidCtrCheck(TProbability_panel *)
{
   new TProbability_panel(NULL);
}
//---------------------------------------------------------------------------
namespace Tprobability_panel
{
   void __fastcall PACKAGE Register()
   {
      TComponentClass classes[1] = {__classid(TProbability_panel)};
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
__fastcall TProbability_panel::TProbability_panel(TComponent* Owner)
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
void TProbability_panel::Create_objects (void)
   {
   Analyses[0] = new TProbability_analysis(this);
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
//    DAH 31/1/02: D-466 fix: set limits of chart of 0 to 100

// ------------------------------------------------------------------
void TProbability_panel::Refresh_chart_objects (void)
   {
   TProbability_analysis* probAnalysisPtr = dynamic_cast<TProbability_analysis*> (Analyses[0]);

   // clear old series.
   Analysis_charts[0]->AnalysisSeriesList->Clear();

   string title;
   for (int i = 0; i != probAnalysisPtr->Field_names_to_analyse->Items->Count; i++)
      {
      // Format the analysis chart.
      TAnalysisSeries* series = Analysis_charts[0]->AnalysisSeriesList->Add();

      string fieldName = probAnalysisPtr->Field_names_to_analyse->Items->Strings[i].c_str();
      series->FieldNames->Add(fieldName.c_str());
      series->FieldNames->Add("Probability");
      series->SeriesType = lines;
      series->SeriesTitle = "%Simulation";
      if (probAnalysisPtr->Prob_exceedence)
         series->specificYAxisTitle = "Probability of Exceedence (%)";
      else
         series->specificYAxisTitle = "Cumulative Probability (%)";

      if (probAnalysisPtr->Field_names_to_analyse->Items->Count == 1)
         series->SeriesTitle = "%Simulation";
      else
         series->SeriesTitle = string(fieldName + "(%Simulation)").c_str();

      // Give chart a title.
      if (title != "")
         title += " and ";
      title += fieldName;
      }
   title += " probablity";
   Analysis_charts[0]->ChartTitle = title.c_str();
   Analysis_charts[0]->LeftAxis->SetMinMax(0, 100);

   // call to base class.
   TAnalysis_panel::Refresh_chart_objects();
   }


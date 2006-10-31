//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TSummary_panel.h"
#include "TSummary_analysis.h"
#include <vector>
#include <string>

#pragma link "TAnalysis_panel"
//#pragma link "TAuto_size_panel"
#pragma package(smart_init)

using namespace std;
//---------------------------------------------------------------------------
// ValidCtrCheck is used to assure that the components created do not have
// any pure virtual functions.
//

static inline void ValidCtrCheck(TSummary_panel *)
{
   new TSummary_panel(NULL);
}
//---------------------------------------------------------------------------
namespace Tsummary_panel
{
   void __fastcall PACKAGE Register()
   {
      TComponentClass classes[1] = {__classid(TSummary_panel)};
      RegisterComponents("APSRU", classes, 0);
   }
}
// ------------------------------------------------------------------
//  Short description:
//      constructor

//  Notes:
//

//  Changes:
//    DPH 15/7/98

// ------------------------------------------------------------------
__fastcall TSummary_panel::TSummary_panel(TComponent* Owner)
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
void TSummary_panel::Create_objects (void)
   {
   Analyses[0] = new TSummary_analysis(this);
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
void TSummary_panel::Refresh_chart_objects (void)
   {
   TSummary_analysis* SummaryAnalysisPtr = dynamic_cast<TSummary_analysis*> (Analyses[0]);

   if (SummaryAnalysisPtr->TableOnly)
      Analysis_charts[0]->Visible = false;
   else
      {
      // clear old series.
      Analysis_charts[0]->AnalysisSeriesList->Clear();

      // Format the analysis chart.
      TAnalysisSeries* Series = Analysis_charts[0]->AnalysisSeriesList->Add();

      Series->FieldNames->Add("Simulation");
      Series->FieldNames->AddStrings(SummaryAnalysisPtr->Field_names_to_analyse->Items);

      Series->SeriesType = SummaryAnalysisPtr->SeriesType;
      if (Series->SeriesType < box)
         {
          Series->SeriesTitle = "%Statistic";
         }

      }

   // call to base class.
   TAnalysis_panel::Refresh_chart_objects();

   // specialized formatting
   Analysis_charts[0]->BottomAxis->Automatic = true;
   double range, min, max;
   Analysis_charts[0]->BottomAxis->CalcMinMax(min, max);
   Analysis_charts[0]->BottomAxis->SetMinMax(min - .325, max + .325);

   Analysis_charts[0]->LeftAxis->Automatic = true;
   Analysis_charts[0]->LeftAxis->CalcMinMax(min, max);
   range = max - min;
   Analysis_charts[0]->LeftAxis->SetMinMax(min - 0.1*range, max + 0.1*range);

   }

// ------------------------------------------------------------------
//  Short description:
//    return true to indicate we want to show data.

//  Notes:

//  Changes:
//    DPH 5/4/01

// ------------------------------------------------------------------
bool TSummary_panel::ShowData(void)
   {
   TSummary_analysis* SummaryAnalysisPtr = dynamic_cast<TSummary_analysis*> (Analyses[0]);
   return SummaryAnalysisPtr->TableOnly;
   }


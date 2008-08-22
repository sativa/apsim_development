//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TXY_panel.h"
#include "TXY_analysis.h"
#include <general\string_functions.h>

#pragma link "TAnalysis_panel"
using namespace std;
#pragma package(smart_init)
//---------------------------------------------------------------------------
// ValidCtrCheck is used to assure that the components created do not have
// any pure virtual functions.
//

static inline void ValidCtrCheck(TXY_panel *)
{
   new TXY_panel(NULL);
}
//---------------------------------------------------------------------------
namespace Txy_panel
{
   void __fastcall PACKAGE Register()
   {
      TComponentClass classes[1] = {__classid(TXY_panel)};
      RegisterComponents("APSRU", classes, 0);
   }
}
// ------------------------------------------------------------------
//  Short description:
//      constructor

//  Notes:

//  Changes:
//    DPH 30/7/98

// ------------------------------------------------------------------
__fastcall TXY_panel::TXY_panel(TComponent* Owner)
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
void TXY_panel::Create_objects (void)
   {
   Analyses[0] = new TXY_analysis(this);
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
//    DAH 9/2/02:    there were two local variables "series". Changed the for loop
//                   iterator to be "iseries"

// ------------------------------------------------------------------
void TXY_panel::Refresh_chart_objects (void)
   {
   TXY_analysis* xySeriesPtr = dynamic_cast<TXY_analysis*> (Analyses[0]);

   if (xySeriesPtr->Field_names_to_analyse->Items->Count > 0)
      {
      // clear old series.
      Analysis_charts[0]->AnalysisSeriesList->Clear();

      // Put all series on chart
      for (int iseries = 0; iseries < xySeriesPtr->seriesCount(); iseries++)
         {
         AnsiString yFieldName;
         bool plotOnY2;
         bool isCumulative;
         SeriesTypes seriesType;
         xySeriesPtr->getSeriesDetails(iseries, yFieldName, plotOnY2,
                                       isCumulative, seriesType);

         TAnalysisSeries* series = Analysis_charts[0]->AnalysisSeriesList->Add();
         AnsiString xFieldName = xySeriesPtr->Field_names_to_analyse->Items->Strings[0];
         series->FieldNames->Add(xFieldName);
         series->FieldNames->Add(yFieldName);
         series->SeriesType = seriesType;
         series->SeriesTitle = yFieldName + ":%Simulation";
         if (plotOnY2)
            series->YAxisLocation = right_axis;
         series->IsCumulative = isCumulative;
         }

      // put all stat lines on chart.
      vector<string> fieldNames;
      xySeriesPtr->getFieldNames(fieldNames);
      string xFieldName;
      string xSpecificFieldName;
      for (vector<string>::iterator fieldI = fieldNames.begin();
                                    fieldI != fieldNames.end();
                                    fieldI++)
         {
         string fieldName = *fieldI;
         if (fieldName.find("stat_") == 0)
            {
            unsigned posFieldName = fieldI->substr(5).find("_") + 6;

            if (xFieldName == "")
               {
               xFieldName = fieldName;
               xSpecificFieldName = fieldI->substr(posFieldName);
               fieldI++;
               }
            TAnalysisSeries* series = Analysis_charts[0]->AnalysisSeriesList->Add();

            series->FieldNames->Add(xFieldName.c_str());
            series->specificXAxisTitle = xSpecificFieldName.c_str();

            series->FieldNames->Add(fieldI->c_str());
            series->specificYAxisTitle = fieldI->substr(posFieldName).c_str();
            fieldI++;
            series->SeriesType = lines;
            AnsiString statFieldName = "%";
            statFieldName += fieldI->c_str();
            series->SeriesTitle = statFieldName;

            // see if we need to link the statline to the y2 axis or not.
            for (int iseries = 0; iseries < xySeriesPtr->seriesCount(); iseries++)
               {
               AnsiString seriesYFieldName;
               bool plotOnY2;
               bool isCumulative;
               SeriesTypes seriesType;
               xySeriesPtr->getSeriesDetails(iseries, seriesYFieldName, plotOnY2,
                                             isCumulative, seriesType);
               if (Str_i_Eq(seriesYFieldName.c_str(), series->specificYAxisTitle.c_str()))
                  {
                  if (plotOnY2)
                     series->YAxisLocation = right_axis;
                  }
               }
            }
         }

      // call to base class.
      TAnalysis_panel::Refresh_chart_objects();
      }
   }


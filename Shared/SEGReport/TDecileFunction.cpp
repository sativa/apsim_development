//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TDecileFunction.h"
#include <general\math_functions.h>
#pragma package(smart_init)

//---------------------------------------------------------------------------
// Constructor
//---------------------------------------------------------------------------
__fastcall TDecileFunction::TDecileFunction(TComponent* owner)
   : TTeeFunction(owner)
   {
   haveGotPercentileFromUser = 2;
   }

//---------------------------------------------------------------------------
// get percentile from user.
//---------------------------------------------------------------------------
void TDecileFunction::getPercentile(void)
   {
   if (haveGotPercentileFromUser == 2)
      {
      AnsiString st;
      if (InputQuery("Enter decile to calculate (10-90%)", "Decile input", st))
         percentile = StrToInt(st);
      else
         percentile = 50;
      haveGotPercentileFromUser = 3;
      }
   }
//---------------------------------------------------------------------------
// Calculate the required decile from the source series data.
//---------------------------------------------------------------------------
double __fastcall TDecileFunction::Calculate(Teengine::TChartSeries* sourceSeries,
                                             int first, int last)
   {
   getPercentile();

   int startPoint = 0;
   int endPoint = sourceSeries->Count()-1;
   if (first != -1)
      startPoint = first;
   if (last != -1)
      endPoint = last;
   vector<double> values;
   TChartValueList* vals = sourceSeries->MandatoryValueList;
   for (int i = startPoint; i != endPoint; i++)
      values.push_back((*vals)[i]);
   return Calculate_percentile(values, false, percentile);
   }
//---------------------------------------------------------------------------
// Calculate the required decile from the source series.
//---------------------------------------------------------------------------
double __fastcall TDecileFunction::CalculateMany(Classes::TList* sourceSeriesList,
                                                 int valueIndex)
   {
   getPercentile();

   vector<double> values;
   for (int i = 0; i != sourceSeriesList->Count-1; i++)
      {
      TChartSeries* series = (TChartSeries*) (sourceSeriesList->Items[i]);
      for (int j = 0; j != series->Count()-1; j++)
         values.push_back((*series->MandatoryValueList)[valueIndex]);
      }
   return Calculate_percentile(values, false, percentile);
   }

//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TPercentileLineSeries.h"
#include <values.h>
#include <math>
#include <algorithm>

#pragma package(smart_init)
#pragma link "ErrorBar"

using namespace std;
// ------------------------------------------------------------------
//  Short description:
//      constructor

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
__fastcall TPercentileLineSeries::TPercentileLineSeries(Classes::TComponent* AOwner)
   : TErrorBarSeries(AOwner)
   {
   doWhiskers = false;
   doMedian = false;
   }

// ------------------------------------------------------------------
//  Short description:
//      add box point data

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void TPercentileLineSeries::addPoint (double Y1, double Y2, double Mean, double Median, const char* XLabel)
   {
   double X = y1.size()+1;
   x.push_back (X);
   xLabels.push_back (XLabel);
   y1.push_back (Y1);
   y2.push_back (Y2);
   mean.push_back (Mean);
   median.push_back (Median);
   AddErrorBar(X, Mean, 0, XLabel, clTeeColor);
   }

// ------------------------------------------------------------------
//  Short description:
//      clear all points

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void TPercentileLineSeries::clearPoints()
   {
   Clear();
   x.erase(x.begin(), x.end());
   xLabels.erase(xLabels.begin(), xLabels.end());
   y1.erase(y1.begin(), y1.end());
   y2.erase(y2.begin(), y2.end());
   mean.erase(mean.begin(), mean.end());
   median.erase(median.begin(), median.end());
   }
// ------------------------------------------------------------------
//  Short description:
//      first method to be called directly from chart.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void __fastcall TPercentileLineSeries::DrawValue(int ValueIndex)
   {
   ErrorPen->Width = 2;
   ErrorWidth = 50;
   TErrorBarSeries::DrawValue(ValueIndex);
   }

// ------------------------------------------------------------------
//  Short description:
//      draw the series

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void __fastcall TPercentileLineSeries::DrawBar(int BarIndex, int StartPos, int EndPos)
   {
   ErrorPen->Visible = false;
   TErrorBarSeries::DrawBar(BarIndex, StartPos, EndPos);

   ParentChart->Canvas->Pen->Assign(ErrorPen);

   long Pos_y1 = CalcYPosValue(y1[BarIndex]);
   long Pos_y2 = CalcYPosValue(y2[BarIndex]);
   long Pos_median = CalcYPosValue(median[BarIndex]);

   long tmpWidth;
   long tmpBarWidth = (BarBounds.Right - BarBounds.Left);
   if (ErrorWidth == 0)
      tmpWidth = tmpBarWidth;
   else
      {
      if (ErrorWidthUnits == ewuPercent)
         tmpWidth = floor(1.0*ErrorWidth*tmpBarWidth/100.0 + 0.5);
      else
         tmpWidth = ErrorWidth;
      }

   long tmpHalfX = ((BarBounds.Right + BarBounds.Left) / 2) + (ParentChart->SeriesWidth3D / 2);
   Pos_y1 = Pos_y1 - (ParentChart->SeriesHeight3D / 2);

   // draw the bottom vertical line
   ParentChart->Canvas->DoVertLine(tmpHalfX, Pos_y1, Pos_y2);

   // draw the whiskers.
   if (doWhiskers)
      {
      ParentChart->Canvas->DoHorizLine(tmpHalfX-(tmpWidth / 2),tmpHalfX+(tmpWidth / 2), Pos_y1);
      ParentChart->Canvas->DoHorizLine(tmpHalfX-(tmpWidth / 2),tmpHalfX+(tmpWidth / 2), Pos_y2);
      }

   // draw the median line.
   if (doMedian)
      ParentChart->Canvas->DoHorizLine(tmpHalfX, tmpHalfX+10, Pos_median);
    }

// ------------------------------------------------------------------
//  Short description:
//      return the lowest y1 value to caller.  Used by zooming/axis scaling
//      algorithms.

//  Notes:

//  Changes:
//    DPH 5/2/98
//    DAH 12/9/00 - removed the call to TErrorBarSeries::MinValue() as it is
//                  irrelevant to the absolute minimum.

// ------------------------------------------------------------------
double __fastcall TPercentileLineSeries::MinYValue(void)
   {
//   double result = TErrorBarSeries::MinYValue();
   double result = 0;

   for (unsigned int i = 0; i < y1.size(); i++)
      result = min(result, y2[i]);
   result -= 0.1 * result;
   return result;
   }

// ------------------------------------------------------------------
//  Short description:
//      return the lowest y1 value to caller.  Used by zooming/axis scaling
//      algorithms.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
double __fastcall TPercentileLineSeries::MaxYValue(void)
   {
   double result = 0;

   for (unsigned int i = 0; i < y1.size(); i++)
      result = max(result, y2[i]);
   result += 0.1 * result;
   return result;
   }


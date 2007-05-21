//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TBox_series.h"
#include <values.h>
#include <math>
#include <algorithm>
using namespace std;

#pragma package(smart_init)
#pragma link "ErrorBar"
// ------------------------------------------------------------------
//  Short description:
//      constructor

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
__fastcall TBox_series::TBox_series(Classes::TComponent* AOwner)
   : TErrorBarSeries(AOwner)
   {
   Column_box = false;
   With_whiskers = true;
   displayMedian = true;
   displayMean = true;
   }

// ------------------------------------------------------------------
//  Short description:
//      add box point data

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void TBox_series::Add_box_point (double Y1, double Y2, double Y3, double Y4, double Y5, double Y6, const char* XLabel)
   {
   double X = y1.size()+1;
   x.push_back (X);
   x_labels.push_back (XLabel);
   y1.push_back (Y1);
   y2.push_back (Y2);
   y3.push_back (Y3);
   y4.push_back (Y4);
   y5.push_back (Y5);
   y6.push_back (Y6);
   AddErrorBar(X, Y4, Y5-Y4, XLabel, clTeeColor);
   }

// ------------------------------------------------------------------
//  Short description:
//      clear all points

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void TBox_series::Clear_box_points ()
   {
   Clear();
   x.erase(x.begin(), x.end());
   y1.erase(y1.begin(), y1.end());
   y2.erase(y2.begin(), y2.end());
   y3.erase(y3.begin(), y3.end());
   y4.erase(y4.begin(), y4.end());
   y5.erase(y5.begin(), y5.end());
   y6.erase(y6.begin(), y6.end());
   }
// ------------------------------------------------------------------
//  Short description:
//      first method to be called directly from chart.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void __fastcall TBox_series::DrawValue(int ValueIndex)
   {
   YOrigin = y2[ValueIndex];
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
void __fastcall TBox_series::DrawBar(int BarIndex, int StartPos, int EndPos)
   {
   if (!Column_box)
      DrawBox (BarIndex, StartPos, EndPos);
   else
      DrawColumnBar (BarIndex, StartPos, EndPos);
   }

// ------------------------------------------------------------------
//  Short description:
//      draw the bar with whiskers.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void __fastcall TBox_series::DrawBox(int BarIndex, int StartPos, int EndPos)
   {
   ErrorPen->Visible = false;
   TErrorBarSeries::DrawBar(BarIndex, StartPos, EndPos);

   ParentChart->Canvas->Pen->Assign(ErrorPen);

   long Pos_y1 = CalcYPosValue(y1[BarIndex]);
   long Pos_y2 = CalcYPosValue(y2[BarIndex]);
   long Pos_y3 = CalcYPosValue(y3[BarIndex]);
   long Pos_y4 = CalcYPosValue(y4[BarIndex]);
   long Pos_y5 = CalcYPosValue(y5[BarIndex]);
   long Pos_y6 = CalcYPosValue(y6[BarIndex]);

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
   ParentChart->Canvas->DoVertLine(tmpHalfX, Pos_y2, Pos_y1);

   // draw the top vertical line
   ParentChart->Canvas->DoVertLine(tmpHalfX, Pos_y4, Pos_y5);

   // draw the whiskers.
   if (With_whiskers)
      {
      ParentChart->Canvas->DoHorizLine(tmpHalfX-(tmpWidth / 2),tmpHalfX+(tmpWidth / 2), Pos_y1);
      ParentChart->Canvas->DoHorizLine(tmpHalfX-(tmpWidth / 2),tmpHalfX+(tmpWidth / 2), Pos_y5);
      }

   // draw the median line.
   if (displayMedian)
      ParentChart->Canvas->DoHorizLine(BarBounds.Left+1, BarBounds.Right-1, Pos_y3);

   // draw the dotted mean line
   if (displayMean)
      {
      ParentChart->Canvas->Pen->Width = 1;
      ParentChart->Canvas->Pen->Style = psDot;
      ParentChart->Canvas->DoHorizLine(BarBounds.Left+1, BarBounds.Right-1, Pos_y6);
      }
   }

// ------------------------------------------------------------------
//  Short description:
//      draw the bar without whiskers.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void __fastcall TBox_series::DrawColumnBar(int BarIndex, int StartPos, int EndPos)
   {
   TBarSeries::DrawBar(BarIndex, StartPos, EndPos);

//   ParentChart->Canvas->Pen->Assign(ErrorPen);
   ParentChart->Canvas->Brush->Color = clLime;

   long Pos_y1 = CalcYPosValue(y1[BarIndex]);
   long Pos_y2 = CalcYPosValue(y2[BarIndex]);
   long Pos_y3 = CalcYPosValue(y3[BarIndex]);
   long Pos_y4 = CalcYPosValue(y4[BarIndex]);
   long Pos_y5 = CalcYPosValue(y5[BarIndex]);
   long Pos_y6 = CalcYPosValue(y6[BarIndex]);

   BarRectangle (clLime, BarBounds.Left, Pos_y2, BarBounds.Right, Pos_y1);
   BarRectangle (clLime, BarBounds.Left, Pos_y5, BarBounds.Right, Pos_y4);

   // draw the median line.
   if (displayMedian)
      ParentChart->Canvas->DoHorizLine(BarBounds.Left+1, BarBounds.Right-1, Pos_y3);

   // draw the dotted mean line
   if (displayMean)
      {
      ParentChart->Canvas->Pen->Width = 1;
      ParentChart->Canvas->Pen->Style = psDot;
      ParentChart->Canvas->DoHorizLine(BarBounds.Left+1, BarBounds.Right-1, Pos_y6);
      }
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
double __fastcall TBox_series::MinYValue(void)
   {
//   double result = TErrorBarSeries::MinYValue();
   double result = MAXDOUBLE;

   for (unsigned int i = 0; i < y1.size(); i++)
      result = min(result, y1[i]);
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
double __fastcall TBox_series::MaxYValue(void)
   {
   double result = TErrorBarSeries::MaxYValue();

   result += 0.1 * result;
   return result;
   }


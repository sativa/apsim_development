//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TAnalysis_chart.h"
#include <general\string_functions.h>
#include <series.hpp>
#include <algorithm>
#include <vector>
#include <ApsimShared\ApsimDirectories.h>
#include <ApsimShared\ApsimSettings.h>
#include <general\inifile.h>

using namespace std;
#pragma package(smart_init)
#pragma resource "*.res"


static const int AXIS_LABEL_FONT_SIZE = 10;
static const int AXIS_TITLE_FONT_SIZE = 12;
static const int TITLE_FONT_SIZE = 12;
static const int LEGEND_FONT_SIZE = 10;
static const int FOOTER_FONT_SIZE = 10;

static const int LARGE_AXIS_LABEL_FONT_SIZE = 18;
static const int LARGE_AXIS_TITLE_FONT_SIZE = 20;
static const int LARGE_TITLE_FONT_SIZE = 24;
static const int LARGE_LEGEND_FONT_SIZE = 14;
static const int LARGE_FOOTER_FONT_SIZE = 14;

static const TColor CustomisedSeriesColours[10] =
   {clBlue,    clRed,    clGreen, clYellow, clBlack,
    clFuchsia, clMaroon, clAqua,  clLime,   clDkGray};
//---------------------------------------------------------------------------
// ValidCtrCheck is used to assure that the components created do not have
// any pure virtual functions.
//
static inline void ValidCtrCheck(TAnalysis_chart *)
   {
   new TAnalysis_chart(NULL);
   }
//---------------------------------------------------------------------------
namespace Tanalysis_chart
{
   void __fastcall PACKAGE Register()
   {
      TComponentClass classes[1] = {__classid(TAnalysis_chart)};
      RegisterComponents("APSRU", classes, 0);
   }
}
// ------------------------------------------------------------------
//  Short description:
//      constructor

//  Notes:

//  Changes:
//    DPH 5/2/98
//    dph 13/12/99 turned on animated zoom - c257

// ------------------------------------------------------------------
__fastcall TAnalysis_chart::TAnalysis_chart(TComponent* Owner)
   : TChart(Owner)
   {
   FAPSTable = NULL;
   FAnalysisSeriesList = new TAnalysisSeriesList(this);
   
   ApsimSettings settings;
   string st;
   settings.read("Outlook Chart|LegendPos", st);
   if (Str_i_Eq(st, "left"))
      Legend->Alignment = laLeft;
   else if (Str_i_Eq(st, "bottom"))
      Legend->Alignment = laBottom;
   else if (Str_i_Eq(st, "right") || st == "")
      Legend->Alignment = laRight;
   else if (Str_i_Eq(st, "top"))
      Legend->Alignment = laTop;
   settings.read("Outlook Chart|ResizeChart", st);
   Legend->ResizeChart = Str_i_Eq(st, "yes");
   Legend->TopPos = 0;
   Legend->Frame->Visible = false;
   Legend->ShadowSize = 0;
   Legend->Brush->Style = bsClear;
   Frame->Visible = false;
   View3D = false;
   FDraw_background = true;
   BevelOuter = bvNone;
   FLarge_fonts = false;
   AnimatedZoom = true;
   FShowSeriesNamesInLegend = true;
   FColour_background = true;
   }

// ------------------------------------------------------------------
//  Short description:
//      destructor

//  Notes:

//  Changes:
//    DPH 27/3/2001

// ------------------------------------------------------------------
__fastcall TAnalysis_chart::~TAnalysis_chart(void)
   {
   RemoveAllSeries();
   delete FAnalysisSeriesList;
   }

// ------------------------------------------------------------------
//  Short description:
//      Set the AnalysisSeriesList property

//  Notes:

//  Changes:
//    DPH 27/3/2001

// ------------------------------------------------------------------
void __fastcall TAnalysis_chart::SetAnalysisSeriesList(TAnalysisSeriesList* list)
   {
   FAnalysisSeriesList->Assign(list);
   }
// ------------------------------------------------------------------
//  Short description:
//      Set the XY pattern to use to create chart series.  All data
//      comes from the DataSource

//  Notes:

//  Changes:
//    DPH 5/2/98
//    DAH 5/2/02: D-410 Copying without background turned the gradient on.
//                Fixed by restoring Gradient->Visible to it's original value
//                rather than always true.

// ------------------------------------------------------------------
void __fastcall TAnalysis_chart::FillPanelRect(const TRect& r)
   {
   bool grad_state = Gradient->Visible;
   if (!FDraw_background)
      {
      Printing = true;
      Gradient->Visible = false;
      }

   TChart::FillPanelRect(r);

   if (!FDraw_background)
      {
      Printing = false;
      Gradient->Visible = grad_state;
      }

   }

// ------------------------------------------------------------------
//  Short description:
//      set the draw background property.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void __fastcall TAnalysis_chart::Set_draw_background (bool draw_background)
   {
   FDraw_background = draw_background;
   Left = Left;
   }

// ------------------------------------------------------------------
//  Short description:
//      Refresh the component.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void TAnalysis_chart::Refresh (void)
   {
   if (FAPSTable != NULL)
      {
      if (FColour_background)
         {
         Gradient->StartColor = (TColor) 0xff8080;
         Gradient->EndColor = (TColor) 0xffeaea;
         Gradient->Visible = true;
         }
      else
         Gradient->Visible = false;

      if (FLarge_fonts)
         {
         Axis_label_font_size = LARGE_AXIS_LABEL_FONT_SIZE;
         Axis_title_font_size = LARGE_AXIS_TITLE_FONT_SIZE;
         Title_font_size = LARGE_TITLE_FONT_SIZE;
         Legend_font_size = LARGE_LEGEND_FONT_SIZE;
         Footer_font_size = LARGE_FOOTER_FONT_SIZE;
         }
      else
         {
         Axis_label_font_size = AXIS_LABEL_FONT_SIZE;
         Axis_title_font_size = AXIS_TITLE_FONT_SIZE;
         Title_font_size = TITLE_FONT_SIZE;
         Legend_font_size = LEGEND_FONT_SIZE;
         Footer_font_size = FOOTER_FONT_SIZE;
         }

      Legend->Font->Size = Legend_font_size;

      ProduceAllSeries();
      }
   }
// ------------------------------------------------------------------
//  Short description:
//      produce a set of series on chart bassed on current pattern.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void TAnalysis_chart::ProduceAllSeries(void)
   {
   // clear all our lists.
   RemoveAllSeries();
   BottomAxis->Title->Caption = "";
   LeftAxis->Title->Caption = "";
   TopAxis->Title->Caption = "";
   RightAxis->Title->Caption = "";

   bool ok = FAPSTable->first();
   while (ok)
      {
      ProduceSeriesForData(FAPSTable->begin(), FAPSTable->end());

      ok = FAPSTable->next();
      }
   FormatAxes();
   FormatChart();
   }
// ------------------------------------------------------------------
//  Short description:
//      produce a set of series on chart bassed on current pattern and the
//      values passed in.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void TAnalysis_chart::ProduceSeriesForData(const vector<TAPSRecord>::const_iterator& begin,
                                           const vector<TAPSRecord>::const_iterator& end)
   {
   // get a list of all fields.
   vector<string> fieldNames;
   FAPSTable->getFieldNames (fieldNames);

   // loop through all series in our series list.
   for (int seriesIndex = 0; seriesIndex < FAnalysisSeriesList->Count; seriesIndex++)
      {
      FAnalysisSeriesList->Items[seriesIndex]->AddAndFormatSeriesObject(this);
      FAnalysisSeriesList->Items[seriesIndex]->AddDataToSeries(begin, end);
      }
   }

// ------------------------------------------------------------------
//  Short description:
//      format the chart

//  Notes:

//  Changes:
//    DPH 27/3/2001

// ------------------------------------------------------------------
void TAnalysis_chart::FormatChart()
   {
   Title->Text->Text = FTitle;
   Title->Font->Size = Title_font_size;
   Foot->Text->Text = FFooter;
   Foot->Font->Size = Footer_font_size;
   Foot->Alignment = taLeftJustify;


   if (FShowSeriesNamesInLegend)
      {
      Legend->LegendStyle = lsSeries;
      Legend->Visible = !(FAnalysisSeriesList->Count > 0 &&
                          FAnalysisSeriesList->Items[0]->SeriesTitle == "");
      }
   else
      Legend->LegendStyle = lsValues;
   }
// ------------------------------------------------------------------
//  Short description:
//      format the chart axes

//  Notes:

//  Changes:
//    DPH 27/3/2001

// ------------------------------------------------------------------
void TAnalysis_chart::FormatAxes()
   {
   BottomAxis->AxisValuesFormat = "###0.###";
   LeftAxis->AxisValuesFormat = "###0.###";
   TopAxis->AxisValuesFormat = "###0.###";
   RightAxis->AxisValuesFormat = "###0.###";

   BottomAxis->LabelsFont->Size = Axis_label_font_size;
   LeftAxis->LabelsFont->Size = Axis_label_font_size;
   TopAxis->LabelsFont->Size = Axis_label_font_size;
   RightAxis->LabelsFont->Size = Axis_label_font_size;

   BottomAxis->Title->Font->Size = Axis_title_font_size;
   LeftAxis->Title->Font->Size = Axis_title_font_size;
   TopAxis->Title->Font->Size = Axis_title_font_size;
   RightAxis->Title->Font->Size = Axis_title_font_size;

   BottomAxis->Grid->Visible = false;
   LeftAxis->Grid->Visible = false;
   TopAxis->Grid->Visible = false;
   RightAxis->Grid->Visible = false;

   // loop through all series in our series list.
   for (int seriesIndex = 0; seriesIndex < FAnalysisSeriesList->Count; seriesIndex++)
      {
      FAnalysisSeriesList->Items[seriesIndex]->AddToAxisLabel(BottomAxis);
      FAnalysisSeriesList->Items[seriesIndex]->AddToAxisLabel(LeftAxis);
      FAnalysisSeriesList->Items[seriesIndex]->AddToAxisLabel(TopAxis);
      FAnalysisSeriesList->Items[seriesIndex]->AddToAxisLabel(RightAxis);
      }
   }




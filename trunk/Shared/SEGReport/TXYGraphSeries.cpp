//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TXYGraphSeries.h"
#include <general\string_functions.h>
#include <general\inifile.h>
#include "TSEGTable.h"

#pragma package(smart_init)
using namespace std;
// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
__fastcall TXYGraphSeries::TXYGraphSeries(TCollection* parent)
   : TCollectionItem(parent)
   {
   FFieldNames = new TStringList;
   SeriesPtr = NULL;
   FXAxisLocation = bottom_axis;
   FYAxisLocation = left_axis;
   FSeriesType = lines;
   FSingleSeriesOnly = false;
   FPutLabelOnY = true;
   FIsXCumulative = false;
   FIsYCumulative = false;
   specificXAxisTitle = "";
   specificYAxisTitle = "";
   }

// ------------------------------------------------------------------
// destructor
// ------------------------------------------------------------------
__fastcall TXYGraphSeries::~TXYGraphSeries()
   {
   delete FFieldNames;
   }
// ------------------------------------------------------------------
// create a single series object and format it
// ------------------------------------------------------------------
void TXYGraphSeries::CreateAndFormatSeriesObject(TQRDBChart* ChartPtr,
                                                 TSEGTable* source)
   {
   if (SeriesPtr == NULL)
      {
      switch (SeriesType)
         {
         case lines:
            {
            TLineSeries* Line_series = new TLineSeries(ChartPtr);
            Line_series->Pointer->Visible = false;
            Line_series->XValues->Order = loNone;
            Line_series->YValues->Order = loNone;
            Line_series->LinePen->Width = 2;
            SeriesPtr = Line_series;
            break;
            }
         case markers_lines:
            {
            TLineSeries* Line_series = new TLineSeries(ChartPtr);
            Line_series->Pointer->Visible = true;
            Line_series->XValues->Order = loNone;
            Line_series->YValues->Order = loNone;
            Line_series->LinePen->Width = 2;
            SeriesPtr = Line_series;
            break;
            }
         case markers:
            {
            TPointSeries* Point_series = new TPointSeries(ChartPtr);
            Point_series->XValues->Order = loNone;
            Point_series->YValues->Order = loNone;
            SeriesPtr = Point_series;
            break;
            }
         case vert_bar:
            {
            TBarSeries* Bar_series = new TBarSeries(ChartPtr);
            Bar_series->MultiBar = mbSide;
            Bar_series->Marks->Visible = false;
            Bar_series->XValues->Order = loNone;
            Bar_series->YValues->Order = loNone;
            Bar_series->BarPen->Visible = false;
            SeriesPtr = Bar_series;
            break;
            }
         case vert_bar_stacked:
            {
            TBarSeries* Bar_series = new TBarSeries(ChartPtr);
            Bar_series->MultiBar = mbStacked;
            Bar_series->Marks->Visible = false;
            Bar_series->XValues->Order = loNone;
            Bar_series->YValues->Order = loNone;
            Bar_series->BarPen->Visible = false;
            SeriesPtr = Bar_series;
            break;
            }
         case vert_bar_100:
            {
            TBarSeries* Bar_series = new TBarSeries(ChartPtr);
            Bar_series->MultiBar = mbStacked100;
            Bar_series->Marks->Visible = false;
            Bar_series->XValues->Order = loNone;
            Bar_series->YValues->Order = loNone;
            Bar_series->BarPen->Visible = false;
            SeriesPtr = Bar_series;
            break;
            }
         case horz_bar :
            {
            THorizBarSeries* Bar_series = new THorizBarSeries(ChartPtr);
            Bar_series->MultiBar = mbSide;
            Bar_series->Marks->Visible = false;
            Bar_series->XValues->Order = loNone;
            Bar_series->YValues->Order = loNone;
            Bar_series->BarPen->Visible = false;
            SeriesPtr = Bar_series;
            break;
            }
         case horz_bar_stacked:
            {
            THorizBarSeries* Bar_series = new THorizBarSeries(ChartPtr);
            Bar_series->MultiBar = mbStacked;
            Bar_series->Marks->Visible = false;
            Bar_series->XValues->Order = loNone;
            Bar_series->YValues->Order = loNone;
            Bar_series->BarPen->Visible = false;
            SeriesPtr = Bar_series;
            break;
            }
         case horz_bar_100:
            {
            THorizBarSeries* Bar_series = new THorizBarSeries(ChartPtr);
            Bar_series->MultiBar = mbStacked100;
            Bar_series->Marks->Visible = false;
            Bar_series->XValues->Order = loNone;
            Bar_series->YValues->Order = loNone;
            Bar_series->BarPen->Visible = false;
            SeriesPtr = Bar_series;
            break;
            }
         default:
            {
            SeriesPtr = new TLineSeries(ChartPtr);
            break;
            }
         }
      ChartPtr->AddSeries(SeriesPtr);
      SeriesPtr->DataSource = source;

      AnsiString xField = FXFieldName;
      AnsiString yField = FFieldNames->Strings[0];
      if (FIsXCumulative)
         xField = "CUMULATIVE_" + xField;
      if (FIsYCumulative)
         yField = "CUMULATIVE_" + yField;

      if (source->Fields->FieldByName(xField)->DataType == ftDate ||
          source->Fields->FieldByName(xField)->DataType == ftFloat)
         SeriesPtr->XValues->ValueSource = xField;
      else
         SeriesPtr->XLabelsSource = xField;

      if (source->Fields->FieldByName(xField)->DataType == ftDate)
         SeriesPtr->XValues->DateTime = true;

      SeriesPtr->YValues->ValueSource = yField;
      SeriesPtr->Title = FSeriesTitle;

      // setup axis titles.
      // link series to proper axes.
      if (FXAxisLocation == bottom_axis)
         {
         SeriesPtr->HorizAxis = aBottomAxis;
         addTitleToAxis(ChartPtr->BottomAxis, xField);
         }
      else
         {
         SeriesPtr->HorizAxis = aTopAxis;
         addTitleToAxis(ChartPtr->TopAxis, xField);
         }
      if (FYAxisLocation == left_axis)
         {
         SeriesPtr->VertAxis = aLeftAxis;
         addTitleToAxis(ChartPtr->LeftAxis, yField);
         }
      else
         {
         SeriesPtr->VertAxis = aRightAxis;
         addTitleToAxis(ChartPtr->RightAxis, yField);
         }
      }
   }
// ------------------------------------------------------------------
// Assign from another graph series.
// ------------------------------------------------------------------
void __fastcall TXYGraphSeries::Assign(TPersistent* fromObj)
   {
   TXYGraphSeries* from = dynamic_cast<TXYGraphSeries*> (fromObj);
   if (from != NULL)
      {
      FXFieldName = from->FXFieldName;
      FFieldNames->Assign(from->FFieldNames);
      FSeriesTitle = from->FSeriesTitle;
      FXAxisLocation = from->FXAxisLocation;
      FYAxisLocation = from->FYAxisLocation;
      FTitle = from->FTitle;
      FSeriesType = from->FSeriesType;
      FSingleSeriesOnly = from->FSingleSeriesOnly;
      FPutLabelOnY = from->FPutLabelOnY;
      FIsXCumulative = from->FIsXCumulative;
      FIsYCumulative = from->FIsYCumulative;
      FSpecificDataBlockName = from->FSpecificDataBlockName;
      FspecificXAxisTitle = from->FspecificXAxisTitle;
      FspecificYAxisTitle = from->FspecificYAxisTitle;
      }
   }
// ------------------------------------------------------------------
// Add the specified title to the specified axis if it doesn't already
// exist.
// ------------------------------------------------------------------
void TXYGraphSeries::addTitleToAxis(TChartAxis* axis, AnsiString title)
   {
   if (title != "" && axis->Title->Caption.Pos(title) == 0)
      {
      if (axis->Title->Caption.Length() > 0)
         axis->Title->Caption = axis->Title->Caption + "\r";
      axis->Title->Caption = axis->Title->Caption + title;
      }
   }
// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
TXYGraphSeriesList::TXYGraphSeriesList(TComponent* owner)
   : TCollection(__classid(TXYGraphSeries))
   {
   Owner = owner;
   }

// ------------------------------------------------------------------
// create a new series, add to collection and return
// pointer
// ------------------------------------------------------------------
TXYGraphSeries* __fastcall TXYGraphSeriesList::Add(void)
   {
   return (TXYGraphSeries*) TCollection::Add();
   }
// ------------------------------------------------------------------
// Returnj index of series.
// ------------------------------------------------------------------
int TXYGraphSeriesList::IndexOf(AnsiString yname)
   {
   for (int i = 0; i != Count; i++)
      {
      if (Items[i]->FieldNames->IndexOf(yname) != -1)
         return i;
      }
   return -1;
   }
// ------------------------------------------------------------------
// return a specific analysis series.
// ------------------------------------------------------------------
TXYGraphSeries* __fastcall TXYGraphSeriesList::GetItem(int index)
   {
   return (TXYGraphSeries*) TCollection::GetItem(index);
   }
// ------------------------------------------------------------------
// return a specific analysis series.
// ------------------------------------------------------------------
void __fastcall TXYGraphSeriesList::SetItem(int index, TXYGraphSeries* value)
   {
   TCollection::SetItem(index, value);
   }
// ------------------------------------------------------------------
// Assign from another list.
// ------------------------------------------------------------------
void __fastcall TXYGraphSeriesList::Assign(TPersistent* fromObj)
   {
   TXYGraphSeriesList* from = dynamic_cast<TXYGraphSeriesList*> (fromObj);
   if (from != NULL)
      {
      Clear();
      for (int i = 0; i != from->Count; i++)
         {
         TXYGraphSeries* newSeries = Add();
         newSeries->Assign(from->Items[i]);
         }
      }
   }
// ------------------------------------------------------------------
// convert a series type to a string.
// ------------------------------------------------------------------
std::string SeriesTypeToString(SeriesTypes seriesType)
   {
   switch (seriesType)
      {
      case lines:             return "lines";
      case markers_lines:     return "markers & lines";
      case markers:           return "markers";
      case vert_bar:          return "bars";
      case vert_bar_stacked:  return "stacked bars";
      case vert_bar_100:      return "stacked bars 100%";
      default:
         {
         throw runtime_error("Invalid series type.");
         }
      }
   }

// ------------------------------------------------------------------
// convert a string to a series type
// ------------------------------------------------------------------
SeriesTypes SeriesTypeToString(const std::string& st)
   {
   if (st == "lines")
      return lines;
   else if (st == "markers & lines")
      return markers_lines;
   else if (st == "markers")
      return markers;
   else if (st == "bars")
      return vert_bar;
   else if (st == "stacked bars")
      return vert_bar_stacked;
   else if (st == "stacked bars 100%")
      return vert_bar_100;
   else
      throw runtime_error("Invalid series type string: " + st);
   }


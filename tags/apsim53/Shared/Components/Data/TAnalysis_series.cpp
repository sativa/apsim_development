//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TAnalysis_series.h"
#include "TBox_series.h"
#include "TPercentileLineSeries.h"
#include <general\string_functions.h>
#include <general\inifile.h>

#pragma package(smart_init)
using namespace std;
// ------------------------------------------------------------------
//  Short description:
//      constructor

//  Notes:

//  Changes:
//    DPH 26/3/2001

// ------------------------------------------------------------------
__fastcall TAnalysisSeries::TAnalysisSeries(TCollection* parent)
   : TCollectionItem(parent)
   {
   FFieldNames = new TStringList;
   SeriesPtr = NULL;
   FXAxisLocation = bottom_axis;
   FYAxisLocation = left_axis;
   FSeriesType = lines;
   FSingleSeriesOnly = false;
   FPutLabelOnY = true;
   FIsCumulative = false;
   specificXAxisTitle = "";
   specificYAxisTitle = "";
   }

// ------------------------------------------------------------------
//  Short description:
//      constructor

//  Notes:

//  Changes:
//    DPH 26/3/2001

// ------------------------------------------------------------------
__fastcall TAnalysisSeries::~TAnalysisSeries()
   {
   delete FFieldNames;
   }

// ------------------------------------------------------------------
//  Short description:
//      create a single series object, format it and add it to it's parent
//      chart.

//  Notes:

//  Changes:
//    DPH 27/3/2001

// ------------------------------------------------------------------
void TAnalysisSeries::AddAndFormatSeriesObject(TChart* ChartPtr)
   {
   bool addAnotherSeries =
      !( (SeriesType == box ||
          SeriesType == box_no_whiskers ||
          SeriesType == box_column ||
          SeriesType == bar_percentile_line) && SeriesPtr != NULL);
   if (addAnotherSeries)
      CreateAndFormatSeriesObject(ChartPtr);
   }

// ------------------------------------------------------------------
//  Short description:
//      create a single series object and format it

//  Notes:

//  Changes:
//    DPH 27/3/2001

// ------------------------------------------------------------------
void TAnalysisSeries::CreateAndFormatSeriesObject(TChart* ChartPtr)
   {
   if (SeriesPtr == NULL || !FSingleSeriesOnly)
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
         case box:
            {
            TBox_series* Box_series_ptr = new TBox_series(ChartPtr);
            Box_series_ptr->Set_box_with_whiskers(true);
            Box_series_ptr->Set_column_box(false);
            SeriesPtr = Box_series_ptr;
            break;
            }
         case box_no_whiskers:
            {
            TBox_series* Box_series_ptr = new TBox_series(ChartPtr);
            Box_series_ptr->Set_box_with_whiskers(false);
            Box_series_ptr->Set_column_box(false);
            SeriesPtr = Box_series_ptr;
            break;
            }
         case box_column:
            {
            TBox_series* Box_series_ptr = new TBox_series(ChartPtr);
            Box_series_ptr->Set_box_with_whiskers(false);
            Box_series_ptr->Set_column_box(true);
            SeriesPtr = Box_series_ptr;
            break;
            }
         case bar_percentile_line:
            {
            SeriesPtr = new TPercentileLineSeries(ChartPtr);
            break;
            }
         case pie:
            {
            TPieSeries* Pie_series_ptr = new TPieSeries(ChartPtr);
            Pie_series_ptr->Circled = true;
            Pie_series_ptr->Marks->Visible = false;
            SeriesPtr = Pie_series_ptr;
            break;
            }
         default:
            {
            SeriesPtr = new TLineSeries(ChartPtr);
            break;
            }
         }
      // link series to proper axes.
      if (FXAxisLocation == bottom_axis)
         SeriesPtr->HorizAxis = aBottomAxis;
      else
         SeriesPtr->HorizAxis = aTopAxis;
      if (FYAxisLocation == left_axis)
         SeriesPtr->VertAxis = aLeftAxis;
      else
         SeriesPtr->VertAxis = aRightAxis;

      ChartPtr->AddSeries(SeriesPtr);
      }
   }

// ------------------------------------------------------------------
//  Short description:
//      Fill the series object with XY data from the data passed in.

//  Notes:

//  Changes:
//    DPH 27/3/2001

// ------------------------------------------------------------------
void TAnalysisSeries::AddDataToSeries(const vector<TAPSRecord>::const_iterator& begin,
                                      const vector<TAPSRecord>::const_iterator& end)
   {
   // if user has specified a specific data block name then make sure this
   // data block matches it.  If not then simply return
   if (FSpecificDataBlockName != "" &&
       FSpecificDataBlockName != (*begin).getFieldValue("Simulation").c_str())
       return;

   if (SeriesType == box || SeriesType == box_no_whiskers ||
       SeriesType == box_column)
      AddDataToBoxSeries(begin, end);
   else if (SeriesType == bar_percentile_line)
      AddDataToPercentileLineSeries(begin, end);
   else
      AddDataToXYSeries(begin, end);

   // give the series a name, if necessary, from the data passed in.
   FormatSeriesTitle(*begin);
   }
// ------------------------------------------------------------------
//  Short description:
//      Fill the BOX series object with XY data from the data passed in.

//  Notes:

//  Changes:
//    DPH 27/3/2001
//    DAH 4/6/01:    Replaced semicolons in simulation names with carriage returns

// ------------------------------------------------------------------
void TAnalysisSeries::AddDataToBoxSeries(const vector<TAPSRecord>::const_iterator& begin,
                                         const vector<TAPSRecord>::const_iterator& end)
   {
   TBox_series* BoxSeriesPtr = dynamic_cast<TBox_series*> (SeriesPtr);
   if (BoxSeriesPtr != NULL)
      {
      string xLabel;
      string xTitle;
      CalcAxisLabelAndTitle(*begin, xLabel, xTitle);
      specificXAxisTitle = xTitle.c_str();

      double y[6];
      int yIndex = 0;
      bool haveFoundMedian = false;
      bool haveFoundMean = false;
      for (vector<TAPSRecord>::const_iterator i = begin; i != end; i++)
         {
         string statistic = i->getFieldValue("Statistic");
         if (statistic > "50%" && !haveFoundMedian)
            {
            y[yIndex] = 0.0;
            yIndex++;
            BoxSeriesPtr->Show_median(false);
            haveFoundMedian = true;
            }
         else if (statistic == "Mean")
            haveFoundMean = true;

         else if (statistic == "50%")
            haveFoundMedian = true;

         y[yIndex] = StrToFloat(i->getFieldValue(FFieldNames->Strings[1].c_str()).c_str());
         yIndex++;
         }
      if (!haveFoundMean)
         {
         BoxSeriesPtr->Show_mean(false);
         y[5] = 0.0;
         }
      BoxSeriesPtr->Add_box_point (y[0], y[1], y[2], y[3], y[4], y[5], xLabel.c_str());
      }
   }

// ------------------------------------------------------------------
//  Short description:
//      Fill the Percentile Lineseries object with XY data from the data passed in.

//  Notes:

//  Changes:
//    DPH 4/9/2001

// ------------------------------------------------------------------
void TAnalysisSeries::AddDataToPercentileLineSeries(const vector<TAPSRecord>::const_iterator& begin,
                                                    const vector<TAPSRecord>::const_iterator& end)
   {
   TPercentileLineSeries* percentileSeries = dynamic_cast<TPercentileLineSeries*> (SeriesPtr);
   if (percentileSeries != NULL && end - begin >= 3)
      {
      string xLabel;
      string xTitle;
      CalcAxisLabelAndTitle(*begin, xLabel, xTitle);
      specificXAxisTitle = xTitle.c_str();

      vector<TAPSRecord>::const_iterator i = begin;
      double y1, y2, mean, median;
      if (end-begin == 3)
         {
         y1 = StrToFloat(i->getFieldValue(FFieldNames->Strings[1].c_str()).c_str());
         i++;
         y2 = StrToFloat(i->getFieldValue(FFieldNames->Strings[1].c_str()).c_str());
         i++;
         mean = StrToFloat(i->getFieldValue(FFieldNames->Strings[1].c_str()).c_str());
         percentileSeries->showMedian = false;
         }
      else
         {
         y1 = StrToFloat(i->getFieldValue(FFieldNames->Strings[1].c_str()).c_str());
         i++;
         median = StrToFloat(i->getFieldValue(FFieldNames->Strings[1].c_str()).c_str());
         i++;
         y2 = StrToFloat(i->getFieldValue(FFieldNames->Strings[1].c_str()).c_str());
         i++;
         mean = StrToFloat(i->getFieldValue(FFieldNames->Strings[1].c_str()).c_str());
         percentileSeries->showMedian = true;
         }
      percentileSeries->addPoint (y1, y2, mean, median, xLabel.c_str());
      }
   }
// ------------------------------------------------------------------
//  Short description:
//    We need to work out a x axis label for this data point AND
//    an x axis title e.g.  x = soiphase=0; crop=sorghum
//    becomes x = 0\rsorghum and the title = soiphase & sorghum

//  Changes:
//    DPH 4/9/2001
// ------------------------------------------------------------------
void TAnalysisSeries::CalcAxisLabelAndTitle(const TAPSRecord& record,
                                            string& axisLabel, string& axisTitle)
   {
   axisLabel = record.getFieldValue(FFieldNames->Strings[0].c_str()).c_str();
   vector<string> titleBits;
   string st = specificXAxisTitle.c_str();
   Replace_all(st, " & ", "&");
   Split_string(st.c_str(), "&", titleBits);

   vector<string> factorBits;
   Split_string(axisLabel, ";", factorBits);
   axisLabel = "";
   for (vector<string>::iterator f = factorBits.begin();
                                 f != factorBits.end();
                                 f++)
      {
      string factorName, factorValue;
      getKeyNameAndValue(f->c_str(), factorName, factorValue);
      if (axisLabel.length() > 0)
         axisLabel += "\r";
      if (factorName == "")
         axisLabel += *f;
      else
         {
         axisLabel += factorValue;
         if (find(titleBits.begin(), titleBits.end(), factorName) == titleBits.end())
            titleBits.push_back(factorName);
         }
      }
   axisTitle = "";
   for (vector<string>::iterator t = titleBits.begin();
                                 t != titleBits.end();
                                 t++)
      {
      if (axisTitle != "")
         axisTitle += " & ";
      axisTitle += t->c_str();
      }
   }

// ------------------------------------------------------------------
//  Short description:
//      Fill the BOX series object with XY data from the data passed in.

//  Notes:

//  Changes:
//    DPH 27/3/2001
//    DAH 4/6/01:    Replaced semicolons with carriage returns (if present) in xFieldValues

// ------------------------------------------------------------------
void TAnalysisSeries::AddDataToXYSeries(const vector<TAPSRecord>::const_iterator& begin,
                                        const vector<TAPSRecord>::const_iterator& end)
   {
   if (FFieldNames->Count >= 1)
      {
      SeriesPtr->Clear();

      // loop through all data records, get the x and y values and give to
      // series object.
      double cumulativeTotal = 0.0;
      for (vector<TAPSRecord>::const_iterator i = begin; i != end; i++)
         {
         try
            {
            // work out type of x we're dealing with.
            //AnsiString xFieldValue = (*i).getFieldValue(FFieldNames->Strings[0].c_str()).c_str();
            string xFieldValue = (*i).getFieldValue(FFieldNames->Strings[0].c_str()).c_str();
            unsigned int pos = xFieldValue.find(";");
            while (pos != string::npos)
            {
               xFieldValue.replace(pos, 1, "\r");
               pos = xFieldValue.find(";");
            }

            AnsiString yFieldValue;
            if (FFieldNames->Count > 1)
               {
               yFieldValue = (*i).getFieldValue(FFieldNames->Strings[1].c_str()).c_str();
               bool xIsNumeric = Is_numerical(xFieldValue.c_str());

               double x, y;
               if (xIsNumeric)
                  {
                  x = StrToFloat(xFieldValue.c_str());
                  xFieldValue = "";
                  }
               else
                  x = i - begin;
               if (yFieldValue == "")
                  y = 0;
               else
                  {
                  y = StrToFloat(yFieldValue);
                  if (FIsCumulative)
                     {
                     cumulativeTotal += y;
                     y = cumulativeTotal;
                     }
                  }

               SeriesPtr->AddXY(x, y, xFieldValue.c_str(), clTeeColor);
               }
            else
               SeriesPtr->Add(StrToFloat(xFieldValue.c_str()), "", clTeeColor);
            }
         catch (const Exception& error)
            {
            // probably a "cannot convert "" to a floating point number" - ignore
            // that point.
            }
         }
      if (SeriesPtr->Count() == 0)
         {
         SeriesPtr->ParentChart->RemoveSeries(SeriesPtr);
//         SeriesPtr->ParentChart = NULL;
//         delete SeriesPtr;
//         SeriesPtr = NULL;
         }
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    Make sure this series adds its label to the specified axis
//    if this series is linked to it.

//  Notes:

//  Changes:
//    DPH 27/3/2001
//    DAH 7/2/02:    D-451 fix

// ------------------------------------------------------------------
void TAnalysisSeries::AddToAxisLabel(TChartAxis* AxisPtr)
   {
   if (PutLabelOnY)
      {
      if (AxisPtr->Horizontal)
         {
         if ( (AxisPtr->OtherSide && FXAxisLocation == top_axis) ||
              (!AxisPtr->OtherSide && FXAxisLocation == bottom_axis) )
            FormatAxis(AxisPtr);
         }
      else
         {
         if ( (AxisPtr->OtherSide && FYAxisLocation == right_axis) ||
              (!AxisPtr->OtherSide && FYAxisLocation == left_axis) )
            FormatAxis(AxisPtr);
         }
      }
   }
// ------------------------------------------------------------------
//  Short description:
//    We are linked to the specified axis so format it and make
//    sure we label it appropriately.

//  Notes:

//  Changes:
//    DPH 27/3/2001

// ------------------------------------------------------------------
void TAnalysisSeries::FormatAxis(TChartAxis* AxisPtr)
   {
   AnsiString title;
   if (AxisPtr->Horizontal)
      {
      if (specificXAxisTitle != "")
         title = specificXAxisTitle;
      else
         title = FFieldNames->Strings[0];
      AxisPtr->TitleSize = 10;
      AxisPtr->LabelsSize = 50;
      if (FSeriesType == box ||
          FSeriesType == box_no_whiskers ||
          FSeriesType == box_column ||
          FSeriesType == bar_percentile_line)
         AxisPtr->LabelsSeparation = 0;
      else
         AxisPtr->LabelsSeparation = 10;

      }
   else if (FFieldNames->Count > 1)
      {
      if (specificYAxisTitle != "")
         title = specificYAxisTitle;
      else
         title = FFieldNames->Strings[1];
      }
   if (title != "" && AxisPtr->Title->Caption.Pos(title) == 0)
      {
      if (AxisPtr->Title->Caption.Length() > 0)
         AxisPtr->Title->Caption = AxisPtr->Title->Caption + "\r";
      AxisPtr->Title->Caption = AxisPtr->Title->Caption + title;
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    Format the series title.

//  Notes:

//  Changes:
//    DPH 27/3/2001

// ------------------------------------------------------------------
void TAnalysisSeries::FormatSeriesTitle(const TAPSRecord& record)
   {
   vector<string> fieldNames = record.getFieldNames();

   string title = FSeriesTitle.c_str();

   // loop through all field in record and replace any macros with
   // the values from this field.
   for (vector<string>::iterator fieldI = fieldNames.begin();
                                 fieldI != fieldNames.end() && (title.find("%") != string::npos);
                                 fieldI++)
      {
      string macroName = "%" + *fieldI;
      string macroValue = record.getFieldValue(*fieldI);
      Replace_all(title, macroName.c_str(), macroValue.c_str());
      }
   SeriesPtr->Title = title.c_str();
   }

// ------------------------------------------------------------------
//  Short description:
//      constructor

//  Notes:

//  Changes:
//    DPH 26/3/2001

// ------------------------------------------------------------------
TAnalysisSeriesList::TAnalysisSeriesList(TComponent* owner)
   : TCollection(__classid(TAnalysisSeries))
   {
   Owner = owner;
   }

// ------------------------------------------------------------------
//  Short description:
//      create a new analysis series, add to collection and return
//      pointer to caller.

//  Notes:

//  Changes:
//    DPH 26/3/2001

// ------------------------------------------------------------------
TAnalysisSeries* __fastcall TAnalysisSeriesList::Add(void)
   {
   return (TAnalysisSeries*) TCollection::Add();
   }

// ------------------------------------------------------------------
//  Short description:
//      return a specific analysis series.

//  Notes:

//  Changes:
//    DPH 26/3/2001

// ------------------------------------------------------------------
TAnalysisSeries* __fastcall TAnalysisSeriesList::GetItem(int index)
   {
   return (TAnalysisSeries*) TCollection::GetItem(index);
   }

// ------------------------------------------------------------------
//  Short description:
//      return a specific analysis series.

//  Notes:

//  Changes:
//    DPH 26/3/2001

// ------------------------------------------------------------------
void __fastcall TAnalysisSeriesList::SetItem(int index, TAnalysisSeries* value)
   {
   TCollection::SetItem(index, value);
   }


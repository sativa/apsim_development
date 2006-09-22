//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TGraph.h"
#include <general\string_functions.h>
#include <general\vcl_functions.h>
#include <general\stl_functions.h>
#include <general\db_functions.h>
#include <TeEngine.hpp>
#include <TeeEdit.hpp>
#include <DBEditCh.hpp>
#include <EditChar.hpp>
#include <sstream>
using namespace std;
#pragma package(smart_init)
//---------------------------------------------------------------------------
// ValidCtrCheck is used to assure that the components created do not have
// any pure virtual functions.
//

static inline void ValidCtrCheck(TGraph *)
{
   new TGraph(NULL);
}
//---------------------------------------------------------------------------
namespace tGraph
{
   void __fastcall PACKAGE Register()
   {
       TComponentClass classes[1] = {__classid(TGraph)};
       RegisterComponents("SEG", classes, 0);
   }
}
//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
__fastcall TGraph::TGraph(TComponent* Owner)
   : TgtQRChart(Owner)
   {
   Frame->Style = psClear;
   if (Chart != NULL)
      {
      // Some defaults that make the charts look better.
      Chart->View3D = false;
      Chart->Legend->LegendStyle = lsSeries;
      Chart->LeftAxis->Grid->Visible = false;
      Chart->LeftAxis->TickLength = 7;
      Chart->LeftAxis->LabelsSeparation = 100;
      Chart->LeftAxis->MinorTicks->Visible = false;
      Chart->LeftAxis->AxisValuesFormat = "###0.###";
      Chart->TopAxis->Grid->Visible = false;
      Chart->TopAxis->TickLength = 7;
      Chart->TopAxis->LabelsSeparation = 100;
      Chart->TopAxis->MinorTicks->Visible = false;
      Chart->TopAxis->AxisValuesFormat = "###0.###";
      Chart->RightAxis->Grid->Visible = false;
      Chart->RightAxis->TickLength = 7;
      Chart->RightAxis->LabelsSeparation = 100;
      Chart->RightAxis->MinorTicks->Visible = false;
      Chart->RightAxis->AxisValuesFormat = "###0.###";
      Chart->BottomAxis->Grid->Visible = false;
      Chart->BottomAxis->TickLength = 7;
      Chart->BottomAxis->LabelsSeparation = 100;
      Chart->BottomAxis->MinorTicks->Visible = false;
      Chart->BottomAxis->AxisValuesFormat = "###0.###";
      Chart->BackWall->Visible = false;
      }
   }
//---------------------------------------------------------------------------
// destructor
//---------------------------------------------------------------------------
__fastcall TGraph::~TGraph(void)
   {
   }
//---------------------------------------------------------------------------
// Read in additional properties from stream.
//---------------------------------------------------------------------------
void __fastcall TGraph::LoadStringProperty(TReader *Reader)
   {
   *stRef = Reader->ReadString();
   if (*stRef == " ")
      *stRef = "";
   }
//---------------------------------------------------------------------------
// Write out additional properties to stream.
//---------------------------------------------------------------------------
void __fastcall TGraph::StoreStringProperty(TWriter *Writer)
   {
   if (*stRef == "")
      Writer->WriteString(" ");
   else
      Writer->WriteString(*stRef);
   }
//---------------------------------------------------------------------------
// called by filer to let us define all our properties.
//---------------------------------------------------------------------------
void __fastcall TGraph::DefineProperties(TFiler *Filer)
   {
   TgtQRChart::DefineProperties(Filer);
   stRef = &title;
   Filer->DefineProperty("title", LoadStringProperty, StoreStringProperty, true);
   stRef = &subTitle;
   Filer->DefineProperty("subTitle", LoadStringProperty, StoreStringProperty, true);
   stRef = &leftAxisTitle;
   Filer->DefineProperty("leftAxisTitle", LoadStringProperty, StoreStringProperty, true);
   stRef = &topAxisTitle;
   Filer->DefineProperty("topAxisTitle", LoadStringProperty, StoreStringProperty, true);
   stRef = &rightAxisTitle;
   Filer->DefineProperty("rightAxisTitle", LoadStringProperty, StoreStringProperty, true);
   stRef = &bottomAxisTitle;
   Filer->DefineProperty("bottomAxisTitle", LoadStringProperty, StoreStringProperty, true);
   stRef = &footTitle;
   Filer->DefineProperty("footTitle", LoadStringProperty, StoreStringProperty, true);
   stRef = &seriesTitle1;
   Filer->DefineProperty("seriesTitle1", LoadStringProperty, StoreStringProperty, true);
   stRef = &seriesTitle2;
   Filer->DefineProperty("seriesTitle2", LoadStringProperty, StoreStringProperty, true);
   stRef = &seriesTitle3;
   Filer->DefineProperty("seriesTitle3", LoadStringProperty, StoreStringProperty, true);
   stRef = &seriesTitle4;
   Filer->DefineProperty("seriesTitle4", LoadStringProperty, StoreStringProperty, true);
   stRef = &seriesTitle5;
   Filer->DefineProperty("seriesTitle5", LoadStringProperty, StoreStringProperty, true);
   }
//---------------------------------------------------------------------------
// Component has finished loading.
//---------------------------------------------------------------------------
void __fastcall TGraph::Loaded(void)
   {
   TgtQRChart::Loaded();
   }
//---------------------------------------------------------------------------
// refresh the chart
//---------------------------------------------------------------------------
void TGraph::refresh(void)
   {
   if (!ComponentState.Contains(csLoading))
      {
      Chart->Refresh();
      replaceChartMacros();
      fixBottomAxisScaling();
      }
   }
//---------------------------------------------------------------------------
// replace all chart macros.
//---------------------------------------------------------------------------
void TGraph::replaceChartMacros(void)
   {
   if (title != "")
      Chart->Title->Text->Text = macros.doReplacement(Owner, title);
   if (subTitle != "")
      Chart->SubTitle->Text->Text = macros.doReplacement(Owner, subTitle);
   if (leftAxisTitle != "")
      Chart->LeftAxis->Title->Caption = macros.doReplacement(Owner, leftAxisTitle);
   if (topAxisTitle != "")
      Chart->TopAxis->Title->Caption = macros.doReplacement(Owner, topAxisTitle);
   if (rightAxisTitle != "")
      Chart->RightAxis->Title->Caption = macros.doReplacement(Owner, rightAxisTitle);
   if (bottomAxisTitle != "")
      Chart->BottomAxis->Title->Caption = macros.doReplacement(Owner, bottomAxisTitle);
   if (footTitle != "")
      Chart->Foot->Text->Text = macros.doReplacement(Owner, footTitle);
   if (Chart->SeriesCount() >= 1 && seriesTitle1 != "" && seriesTitle1 != "$seriesName")
      Chart->Series[0]->Title = macros.doReplacement(Owner, seriesTitle1);
   if (Chart->SeriesCount() >= 2 && seriesTitle2 != "")
      Chart->Series[1]->Title = macros.doReplacement(Owner, seriesTitle2);
   if (Chart->SeriesCount() >= 3 && seriesTitle3 != "")
      Chart->Series[2]->Title = macros.doReplacement(Owner, seriesTitle3);
   if (Chart->SeriesCount() >= 4 && seriesTitle4 != "")
      Chart->Series[3]->Title = macros.doReplacement(Owner, seriesTitle4);
   if (Chart->SeriesCount() >= 5 && seriesTitle5 != "")
      Chart->Series[4]->Title = macros.doReplacement(Owner, seriesTitle5);
   }
//---------------------------------------------------------------------------
// Let the user edit the chart.
//---------------------------------------------------------------------------
void TGraph::userEdit(void)
   {
   Chart->Title->Text->Text = title;
   Chart->SubTitle->Text->Text = subTitle;
   Chart->LeftAxis->Title->Caption = leftAxisTitle;
   Chart->TopAxis->Title->Caption = topAxisTitle;
   Chart->RightAxis->Title->Caption = rightAxisTitle;
   Chart->BottomAxis->Title->Caption = bottomAxisTitle;
   Chart->Foot->Text->Text = footTitle;
   if (Chart->SeriesCount() >= 1)
      Chart->Series[0]->Title = seriesTitle1;
   if (Chart->SeriesCount() >= 2)
      Chart->Series[1]->Title = seriesTitle2;
   if (Chart->SeriesCount() >= 3)
      Chart->Series[2]->Title = seriesTitle3;
   if (Chart->SeriesCount() >= 4)
      Chart->Series[3]->Title = seriesTitle4;
   if (Chart->SeriesCount() >= 5)
      Chart->Series[4]->Title = seriesTitle5;

   TDataSetSeriesSource* dummy = new TDataSetSeriesSource((TComponent*)NULL);

   TeeSaveBoolOption(TeeMsg_TreeMode, true);
   EditChart(NULL, this->Chart);
   delete dummy;

   title = Chart->Title->Text->Text;
   subTitle = Chart->SubTitle->Text->Text;
   leftAxisTitle = Chart->LeftAxis->Title->Caption;
   topAxisTitle = Chart->TopAxis->Title->Caption;
   rightAxisTitle = Chart->RightAxis->Title->Caption;
   bottomAxisTitle = Chart->BottomAxis->Title->Caption;
   footTitle = Chart->Foot->Text->Text;
   if (Chart->SeriesCount() >= 1)
      seriesTitle1 = Chart->Series[0]->Title;
   if (Chart->SeriesCount() >= 2)
      seriesTitle2 = Chart->Series[1]->Title;
   if (Chart->SeriesCount() >= 3)
      seriesTitle3 = Chart->Series[2]->Title;
   if (Chart->SeriesCount() >= 4)
      seriesTitle4 = Chart->Series[3]->Title;
   if (Chart->SeriesCount() >= 5)
      seriesTitle5 = Chart->Series[4]->Title;
   replaceChartMacros();
   }

//void dummyLink(void)
//  {
//   // This next line forces the linker to link in the DBEditCh unit.
//   // This will in turn add dataset to the list of series data sources.
//   new TDBChartEditor((TComponent*)NULL);
//   }

//---------------------------------------------------------------------------
// When the x axis is a date time axis and it's interval is set to 1 month
// and the data range is less than a month, then the axis doesn't show anything.
// this method fixes that problem.
//---------------------------------------------------------------------------
void TGraph::fixBottomAxisScaling()
   {
   TChartAxis* BottomAxis = Chart->BottomAxis;
   if (BottomAxis->Automatic && BottomAxis->ExactDateTime)
      {
      BottomAxis->AdjustMaxMin();
      if (BottomAxis->Minimum > 0 && BottomAxis->Maximum > 0)
         {
         TDateTime MinDate = TDateTime(BottomAxis->Minimum);
         TDateTime MaxDate = TDateTime(BottomAxis->Maximum);
         unsigned short minYear, minMonth, minDay, maxYear, maxMonth, maxDay;
         MinDate.DecodeDate(&minYear, &minMonth, &minDay);
         MaxDate.DecodeDate(&maxYear, &maxMonth, &maxDay);
         maxMonth++;
         if (maxMonth == 13)
            {
            maxMonth = 1;
            maxYear++;
            }
         TDateTime MinDateOnAxis = TDateTime(minYear, minMonth, 1);
         TDateTime MaxDateOnAxis = TDateTime(maxYear, maxMonth, 1);
         BottomAxis->SetMinMax(MinDateOnAxis, MaxDateOnAxis);
         }
      }
   }

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
   dataSeriesNumbers = "";
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
   stRef = &seriesTitle;
   Filer->DefineProperty("seriesTitle", LoadStringProperty, StoreStringProperty, true);
   }
//---------------------------------------------------------------------------
// setter for the 'seriesNumbers' property
//---------------------------------------------------------------------------
void __fastcall TGraph::setSeriesNumbers(AnsiString seriesNumbers)
   {
   if (seriesNumbers != dataSeriesNumbers)
      {
      removeTemplatedChartSeries();
      dataSeriesNumbers = seriesNumbers;
      refresh();
      }
   }
//---------------------------------------------------------------------------
// Set the scientific scaling option.
//---------------------------------------------------------------------------
void __fastcall TGraph::setScientificScaling(bool scaling)
   {
   scientificScaling = scaling;
   refresh();
   }
//---------------------------------------------------------------------------
// Component has finished loading.
//---------------------------------------------------------------------------
void __fastcall TGraph::Loaded(void)
   {
   TgtQRChart::Loaded();
   for (int s = 0; s != Chart->SeriesCount(); s++)
      Chart->Series[s]->OnBeforeAdd = onBeforeAdd;
   addDataChangeSubscriptions();
   }
//---------------------------------------------------------------------------
// Go through all series data sources and add a data subscription.
//---------------------------------------------------------------------------
void TGraph::addDataChangeSubscriptions(void)
   {
   for (int s = 0; s != Chart->SeriesCount(); s++)
      {
      TSEGTable* source = dynamic_cast<TSEGTable*> (Chart->Series[s]->DataSource);
      if (source != NULL)
         source->addDataChangeSubscription(Name + ".afterDataRefresh");
      }
   }
//---------------------------------------------------------------------------
// Remove all data subscriptions.
//---------------------------------------------------------------------------
void TGraph::removeDataChangeSubscriptions(void)
   {
   for (int s = 0; s != Chart->SeriesCount(); s++)
      {
      TSEGTable* source = dynamic_cast<TSEGTable*> (Chart->Series[s]->DataSource);
      if (source != NULL)
         source->removeDataChangeSubscription(Name + ".afterDataRefresh");
      }
   }
//---------------------------------------------------------------------------
// One of the source datasets is now open - refresh chart.
//---------------------------------------------------------------------------
void __fastcall TGraph::afterDataRefresh(TDataSet* dataset)
   {
   refresh();
   }
//---------------------------------------------------------------------------
// refresh the chart
//---------------------------------------------------------------------------
void TGraph::refresh(void)
   {
   if (!ComponentState.Contains(csLoading))
      {
      removeTemplatedChartSeries();
      createTemplatedChartSeries();
      Chart->Refresh();
      for (int s = 0; s != Chart->SeriesCount(); s++)
         Chart->Series[s]->CheckDataSource();
      scaleAxis();
      }
   }
//---------------------------------------------------------------------------
// using the 1st chart series as a template, create a new chart series for
// each data series.
//---------------------------------------------------------------------------
void TGraph::createTemplatedChartSeries(void)
   {
   if (dataSeriesNumbers != "" && Chart->SeriesCount() == 1)
      {
      TSEGTable* source = dynamic_cast<TSEGTable*> (Chart->Series[0]->DataSource);
      if (source != NULL)
         {
         source->getSeriesNames(seriesNames);

         vector<unsigned> seriesNumbers;
         if (dataSeriesNumbers == "*")
            {
            for (unsigned i = 0; i != seriesNames.size(); i++)
               seriesNumbers.push_back(i);
            }
         else
            {
            vector<string> numberStrings;
            Split_string(dataSeriesNumbers.c_str(), " ", numberStrings);
            for (unsigned i = 0; i != numberStrings.size(); i++)
               seriesNumbers.push_back(StrToInt(numberStrings[i].c_str())-1);
            }

         Chart->Series[0]->Tag = seriesNumbers[0];
         for(unsigned i = 1; i != min(seriesNumbers.size(), seriesNames.size()); i++)
            {
            TChartSeries* series = CloneChartSeries(Chart->Series[0]);
            series->OnBeforeAdd = onBeforeAdd;
            series->Tag = seriesNumbers[i];
            series->Color = Chart->GetFreeSeriesColor();
            }

         for (int s = 0; s != Chart->SeriesCount(); s++)
            {
            string title = seriesTitle.c_str();
            replaceAll(title, "$seriesName", seriesNames[seriesNumbers[s]]);
            Chart->Series[s]->Title = title.c_str();
            }
         }
      }
   }
//---------------------------------------------------------------------------
// remove all templated chart series
//---------------------------------------------------------------------------
void TGraph::removeTemplatedChartSeries(void)
   {
   if (dataSeriesNumbers != "" && Chart->SeriesCount() > 1)
      {
      while (Chart->SeriesCount() > 1)
         {
         TChartSeries* series = Chart->Series[1];
         Chart->RemoveSeries(series);
         delete series;
         }
      Chart->Series[0]->Title = seriesTitle;
      }
   }
//---------------------------------------------------------------------------
// Called by the TChartSeries before a point is added.  If we return false
// then the point won't be added to the series.
//---------------------------------------------------------------------------
bool __fastcall TGraph::onBeforeAdd(TChartSeries* series)
   {
   TSEGTable* source = dynamic_cast<TSEGTable*> (series->DataSource);
   if (source != NULL && dataSeriesNumbers != "")
      {
      AnsiString seriesName = source->FieldValues["Series"];
      bool seriesMatch = (seriesNames[series->Tag] == seriesName.c_str());
      return seriesMatch;
      }
   else
      return true;
   }
//---------------------------------------------------------------------------
// replace all chart macros.
//---------------------------------------------------------------------------
void TGraph::replaceChartMacros(void)
   {
   Chart->Title->Text->Text = macros.doReplacement(Owner, title);
   Chart->LeftAxis->Title->Caption = macros.doReplacement(Owner, leftAxisTitle);
   Chart->TopAxis->Title->Caption = macros.doReplacement(Owner, topAxisTitle);
   Chart->RightAxis->Title->Caption = macros.doReplacement(Owner, rightAxisTitle);
   Chart->BottomAxis->Title->Caption = macros.doReplacement(Owner, bottomAxisTitle);
   Chart->Foot->Text->Text = macros.doReplacement(Owner, footTitle);
   }
//---------------------------------------------------------------------------
// Let the user edit the chart.
//---------------------------------------------------------------------------
void TGraph::userEdit(void)
   {
   Chart->Title->Text->Text = title;
   Chart->LeftAxis->Title->Caption = leftAxisTitle;
   Chart->TopAxis->Title->Caption = topAxisTitle;
   Chart->RightAxis->Title->Caption = rightAxisTitle;
   Chart->BottomAxis->Title->Caption = bottomAxisTitle;
   Chart->Foot->Text->Text = footTitle;
   if (Chart->SeriesCount() == 1)
      Chart->Series[0]->Title = seriesTitle;
   removeDataChangeSubscriptions();

   TChartEditor* editor = new TChartEditor(Chart);
   editor->Chart = Chart;
   editor->Execute();
   delete editor;

   title = Chart->Title->Text->Text;
   leftAxisTitle = Chart->LeftAxis->Title->Caption;
   topAxisTitle = Chart->TopAxis->Title->Caption;
   rightAxisTitle = Chart->RightAxis->Title->Caption;
   bottomAxisTitle = Chart->BottomAxis->Title->Caption;
   footTitle = Chart->Foot->Text->Text;
   if (Chart->SeriesCount() == 1)
      seriesTitle = Chart->Series[0]->Title;

   for (int s = 0; s != Chart->SeriesCount(); s++)
      Chart->Series[s]->OnBeforeAdd = onBeforeAdd;
   addDataChangeSubscriptions();
   }

void dummyLink(void)
   {
   // This next line forces the linker to link in the DBEditCh unit.
   // This will in turn add dataset to the list of series data sources.
   new TDBChartEditor((TComponent*)NULL);
   }
//---------------------------------------------------------------------------
// Scale the axis using a much better scaling rule.
//---------------------------------------------------------------------------
void TGraph::scaleAxis(void)
   {
//   Chart->LeftAxis->
   }


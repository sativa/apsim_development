//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TGraph.h"
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
   }
//---------------------------------------------------------------------------
// getter for the 'DuplicateChartSeries' property
//---------------------------------------------------------------------------
bool __fastcall TGraph::getDuplicateChartSeries(void)
   {
   return (Chart->SeriesCount() > 0 && Chart->Series[0]->Active == false);
   }
//---------------------------------------------------------------------------
// setter for the 'DuplicateChartSeries' property
//---------------------------------------------------------------------------
void __fastcall TGraph::setDuplicateChartSeries(bool duplicate)
   {
   if (!ComponentState.Contains(csLoading))
      {
      if (duplicate)
         {
         if (duplicate != duplicateChartSeries)
            {
            // remove ourself from all source data lists.
            for (unsigned i = 0; i != sourceNames.size(); i++)
               {
               TSEGTable* source = getComponent<TSEGTable>(Owner, sourceNames[i].c_str());
               if (source != NULL)
                  source->removeDataChangeSubscription(Name.c_str());
               }

            int numTemplateSeries = Chart->SeriesCount();
            for (int series = 0; series != numTemplateSeries; series++)
               {
               TChartSeries* chartSeries = Chart->Series[series];
               chartSeries->Active = false;

               // if the series datasource is not NULL and it isn't in our
               // list of known datasources then add it.
               TSEGTable* source = dynamic_cast<TSEGTable*> (Chart->Series[series]->DataSource);
               if (source != NULL && find(sourceNames.begin(), sourceNames.end(),
                                          source->Name.c_str()) == sourceNames.end())
                  {
                  sourceNames.push_back(source->Name.c_str());
                  source->addDataChangeSubscription(source->Name + ".afterDataRefresh");
                  }
               }
            }
         }
      else
         reinstateChartSeries();
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
   if (duplicateChartSeries)
      setDuplicateChartSeries(duplicateChartSeries);
   }
//---------------------------------------------------------------------------
// Some other component is being added or removed - check our dependencies.
//---------------------------------------------------------------------------
void __fastcall TGraph::Notification(TComponent* comp, TOperation operation)
   {
   TgtQRChart::Notification(comp, operation);
//   if (operation == opInsert)
//      trapSourceDataRefresh();
   }
//---------------------------------------------------------------------------
// One of the source datasets is now open - refresh chart.
//---------------------------------------------------------------------------
void __fastcall TGraph::afterDataRefresh(TDataSet* dataset)
   {
   refresh();
   }
//---------------------------------------------------------------------------
// reinstate the original chart series.
//---------------------------------------------------------------------------
void TGraph::reinstateChartSeries(void)
   {
   // remove all non template chart series
   for (int chartSeriesI = Chart->SeriesCount()-1; chartSeriesI >= 0; chartSeriesI--)
      {
      if (Chart->Series[chartSeriesI]->Active)
         Chart->RemoveSeries(Chart->Series[chartSeriesI]);
      else
         Chart->Series[chartSeriesI]->Active = true;
      }
   }
//---------------------------------------------------------------------------
// refresh the chart
//---------------------------------------------------------------------------
void TGraph::refresh(void)
   {
   if (!ComponentState.Contains(csLoading) && duplicateChartSeries)
      {
      removeNonTemplateChartSeries();

      // loop through all cloned chart series.
      unsigned numTemplateSeries = Chart->SeriesCount();
      for (unsigned chartSeriesI = 0; chartSeriesI != numTemplateSeries; chartSeriesI++)
         {
         TSEGTable* source = dynamic_cast<TSEGTable*> (Chart->Series[chartSeriesI]->DataSource);
         if (source->Active)
            {
            seriesNames.erase(seriesNames.begin(), seriesNames.end());
            source->getSeriesNames(seriesNames);

            // loop through all data series and clone the initial chart series so
            // that all data series have a chart series.
            for (unsigned dataSeriesNum = 0; dataSeriesNum != seriesNames.size(); dataSeriesNum++)
               {
               TChartSeries* series = CloneChartSeries(Chart->Series[chartSeriesI]);
               series->ParentChart = Chart;
               series->Active = true;
               series->OnBeforeAdd = onBeforeAdd;
               series->SeriesColor = Chart->GetFreeSeriesColor(true);
               series->DataSource = NULL;
               series->DataSource = source;
               series->Tag = dataSeriesNum;
               }
            }
         }
      }
   else
      Chart->Refresh();
   replaceChartMacros();
   scaleAxis();
   }
//---------------------------------------------------------------------------
// remove all non template chart series
//---------------------------------------------------------------------------
void TGraph::removeNonTemplateChartSeries(void)
   {
   for (int chartSeriesI = Chart->SeriesCount()-1; chartSeriesI >= 0; chartSeriesI--)
      {
      if (Chart->Series[chartSeriesI]->Active)
         Chart->RemoveSeries(Chart->Series[chartSeriesI]);
      }
   }
//---------------------------------------------------------------------------
// Called by the TChartSeries before a point is added.  If we return false
// then the point won't be added to the series.
//---------------------------------------------------------------------------
bool __fastcall TGraph::onBeforeAdd(TChartSeries* series)
   {
   TSEGTable* source = dynamic_cast<TSEGTable*> (series->DataSource);
   if (source != NULL)
      {
      bool seriesMatch = source->getSeriesName() == seriesNames[series->Tag];
      if (seriesMatch && series->Title.Pos("$") > 0)
         series->Title = macros.doReplacement(Owner, series->Title);
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


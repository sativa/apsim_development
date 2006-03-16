//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TXYGraph.h"

//---------------------------------------------------------------------------

#pragma package(smart_init)

//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
__fastcall TXYGraph::TXYGraph(TComponent* Owner)
   : TGraph(Owner)
   {
   series = new TXYGraphSeriesList(Owner);
   }
//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
__fastcall TXYGraph::~TXYGraph(void)
   {
   delete Series;
   }
//---------------------------------------------------------------------------
// Set the X variable.
//---------------------------------------------------------------------------
void __fastcall TXYGraph::setSeries(TXYGraphSeriesList* s)
   {
   series->Assign(s);
   refresh();
   }
// ------------------------------------------------------------------
// Set the source data set.
// ------------------------------------------------------------------
void __fastcall TXYGraph::setSource(TSEGTable* source)
   {
   sourceDataset = source;
   refresh();
   }
// ------------------------------------------------------------------
// Create all chart series.
// ------------------------------------------------------------------
void TXYGraph::createChartSeries()
   {
   Chart->SeriesList->Clear();
   for (int i = 0; i != series->Count; i++)
      {
      series->Items[i]->CreateAndFormatSeriesObject(Chart, sourceDataset);
      }
   }


//---------------------------------------------------------------------------

#ifndef TXYGraphH
#define TXYGraphH
#include "TXYGraphSeries.h"
#include "TGraph.h"
#include <SysUtils.hpp>
#include <Controls.hpp>
#include <Classes.hpp>
#include "TSEGTable.h"
//---------------------------------------------------------------------------
// This chart component extends the base QuickReport Chart component by adding
// a series expansion capability.  This component will optionally look at the
// number of data series in the source dataset and make sure there is a chart
// series for each data series.
//---------------------------------------------------------------------------
class PACKAGE TXYGraph : public TGraph
   {
   private:
      TXYGraphSeriesList* series;
      TSEGTable* sourceDataset;

      void __fastcall setSeries(TXYGraphSeriesList* series);
      void __fastcall setSource(TSEGTable* source);
      virtual void createChartSeries(void);
   protected:
   public:
      __fastcall TXYGraph(TComponent* Owner);
      __fastcall ~TXYGraph(void);

   __published:
      __property TXYGraphSeriesList* Series = {read=series, write=setSeries};
      __property TSEGTable* source = {read=sourceDataset, write=setSource};
   };
//---------------------------------------------------------------------------
#endif

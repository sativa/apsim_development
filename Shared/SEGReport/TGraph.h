//---------------------------------------------------------------------------

#ifndef TGraphH
#define TGraphH
//---------------------------------------------------------------------------
#include <SysUtils.hpp>
#include <Controls.hpp>
#include <Classes.hpp>
#include <Forms.hpp>
#include <gtQrCtrls.hpp>
#include <QuickRpt.hpp>
#include "TSEGTable.h"
#include "ReportMacros.h"
class SeriesDataSource;
//---------------------------------------------------------------------------
// This chart component extends the base QuickReport Chart component by adding
// a series expansion capability.  This component will optionally look at the
// number of data series in the source dataset and make sure there is a chart
// series for each data series.
//---------------------------------------------------------------------------
class PACKAGE TGraph : public TgtQRChart
   {
   private:
      AnsiString title;
      AnsiString leftAxisTitle;
      AnsiString topAxisTitle;
      AnsiString rightAxisTitle;
      AnsiString bottomAxisTitle;
      AnsiString footTitle;
      AnsiString* stRef;
      bool ourScaling;
      ReportMacros macros;

      vector<std::string> sourceNames;
      vector<string> seriesNames;

      virtual void __fastcall DefineProperties(TFiler *Filer);
      void __fastcall LoadStringProperty(TReader *Reader);
      void __fastcall StoreStringProperty(TWriter *Writer);
      bool __fastcall getDuplicateChartSeries(void);
      void __fastcall setDuplicateChartSeries(bool duplicateChartSeries);
      void __fastcall setScientificScaling(bool scaling);

      void reinstateChartSeries(void);

      void refresh(void);
      bool __fastcall onBeforeAdd(TChartSeries* series);
      void __fastcall afterDataRefresh(TDataSet* dataset);
      virtual void __fastcall Loaded(void);
      void __fastcall Notification(TComponent* comp, TOperation operation);
      void replaceChartMacros(void);
      void removeNonTemplateChartSeries(void);
      void scaleAxis(void);

   protected:
   public:
      __fastcall TGraph(TComponent* Owner);
      __fastcall ~TGraph(void);

      void userEdit(void);

   __published:
      __property bool duplicateChartSeries = {read=getDuplicateChartSeries, write=setDuplicateChartSeries};
      __property bool scientificScaling = {read=ourScaling, write=setScientificScaling};
   };
//---------------------------------------------------------------------------
#endif

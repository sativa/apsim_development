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
      int dataSeriesNumber;

      vector<std::string> sourceNames;

      virtual void __fastcall DefineProperties(TFiler *Filer);
      void __fastcall LoadStringProperty(TReader *Reader);
      void __fastcall StoreStringProperty(TWriter *Writer);
      void __fastcall setScientificScaling(bool scaling);
      void __fastcall setSeriesNumber(int seriesNumber);

      void refresh(void);
      bool __fastcall onBeforeAdd(TChartSeries* series);
      void __fastcall afterDataRefresh(TDataSet* dataset);
      virtual void __fastcall Loaded(void);
      void replaceChartMacros(void);
      void removeNonTemplateChartSeries(void);
      void scaleAxis(void);

   protected:
   public:
      __fastcall TGraph(TComponent* Owner);
      __fastcall ~TGraph(void);

      void userEdit(void);

   __published:
      __property int seriesNumber = {read=dataSeriesNumber, write=setSeriesNumber};
      __property bool scientificScaling = {read=ourScaling, write=setScientificScaling};
   };
//---------------------------------------------------------------------------
#endif

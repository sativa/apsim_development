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
      AnsiString seriesTitle;
      AnsiString* stRef;
      bool ourScaling;
      ReportMacros macros;
      AnsiString dataSeriesNumbers;
      vector<string> seriesNames;

      vector<std::string> sourceNames;

      virtual void __fastcall DefineProperties(TFiler *Filer);
      void __fastcall LoadStringProperty(TReader *Reader);
      void __fastcall StoreStringProperty(TWriter *Writer);
      void __fastcall setScientificScaling(bool scaling);
      void __fastcall setSeriesNumbers(AnsiString seriesNumbers);

      void refresh(void);
      bool __fastcall onBeforeAdd(TChartSeries* series);
      virtual void __fastcall Loaded(void);
      void replaceChartMacros(void);
      void scaleAxis(void);

      //---------------------------------------------------------------------------
      // using the 1st chart series as a template, create a new chart series for
      // each data series.
      //---------------------------------------------------------------------------
      void createTemplatedChartSeries(void);

      //---------------------------------------------------------------------------
      // remove all templated chart series
      //---------------------------------------------------------------------------
      void removeTemplatedChartSeries(void);

      //---------------------------------------------------------------------------
      // Add all data subscriptions.
      //---------------------------------------------------------------------------
      void addDataChangeSubscriptions(void);

      //---------------------------------------------------------------------------
      // Remove all data subscriptions.
      //---------------------------------------------------------------------------
      void removeDataChangeSubscriptions(void);
   protected:
   public:
      __fastcall TGraph(TComponent* Owner);
      __fastcall ~TGraph(void);

      void userEdit(void);

   __published:
      void __fastcall afterDataRefresh(TDataSet* dataset);

      __property AnsiString seriesNumbers = {read=dataSeriesNumbers, write=setSeriesNumbers};
      __property bool scientificScaling = {read=ourScaling, write=setScientificScaling};
   };
//---------------------------------------------------------------------------
#endif

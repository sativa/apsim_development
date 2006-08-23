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
      AnsiString subTitle;
      AnsiString leftAxisTitle;
      AnsiString topAxisTitle;
      AnsiString rightAxisTitle;
      AnsiString bottomAxisTitle;
      AnsiString footTitle;
      AnsiString seriesTitle1;
      AnsiString seriesTitle2;
      AnsiString seriesTitle3;
      AnsiString seriesTitle4;
      AnsiString seriesTitle5;
      AnsiString* stRef;
      bool ourScaling;
      ReportMacros macros;
      AnsiString dataSeriesNumbers;
      vector<string> seriesNames;

      virtual void __fastcall DefineProperties(TFiler *Filer);
      void __fastcall LoadStringProperty(TReader *Reader);
      void __fastcall StoreStringProperty(TWriter *Writer);
      void __fastcall setScientificScaling(bool scaling);
      void __fastcall setSeriesNumbers(AnsiString seriesNumbers);

      bool __fastcall onBeforeAdd(TChartSeries* series);
      virtual void __fastcall Loaded(void);
      void __fastcall TGraph::OnClearValues(TChartSeries* dataset);
      void __fastcall TGraph::OnAfterDraw(TObject* sender);
      void replaceChartMacros(void);
      void scaleAxis(void);
      void fixBottomAxisScaling();

      //---------------------------------------------------------------------------
      // using the 1st chart series as a template, create a new chart series for
      // each data series.
      //---------------------------------------------------------------------------
      virtual void createChartSeries(void);

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
      void refresh(void);
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

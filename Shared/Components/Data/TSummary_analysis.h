//---------------------------------------------------------------------------
#ifndef TSummary_analysisH
#define TSummary_analysisH
//---------------------------------------------------------------------------
#include <SysUtils.hpp>
#include <Controls.hpp>
#include <Classes.hpp>
#include <Forms.hpp>
#include "TAnalysis.h"
#include "TAPSTable.h"
#include "TAnalysis_series.h"
// ------------------------------------------------------------------
//  Short description:
//      this component produces summary statistics.

//  Notes:
//      Format of data table:
//         0%   5%   10%   15%   ...   100%   Mean   Minimum   Maximum   Count

//  Changes:
//    DPH 15/7/98
//    dph 19/3/01 generalised and merged with box plots C358

// ------------------------------------------------------------------
class PACKAGE TSummary_analysis : public TAnalysis
   {
   private:
      TStringList* FPercentiles;
      bool FMean;
      bool FMinimum;
      bool FMaximum;
      bool FCount;
      SeriesTypes FSeriesType;
      bool FTableOnly;

      std::string Double_2_str(double value, int precision);
   protected:
      virtual void calcAndStoreRecords();
      virtual TAPSTable_form* createPropertiesForm();
      virtual void load();
      virtual void save();
   public:
      __fastcall TSummary_analysis(TComponent* Owner);
      __fastcall ~TSummary_analysis(void);
   __published:
      __property TStringList* Percentiles = {read=FPercentiles, write=FPercentiles};
      __property SeriesTypes SeriesType = {read=FSeriesType, write=FSeriesType};
      __property bool TableOnly = {read=FTableOnly, write=FTableOnly};
      __property bool Mean = {read=FMean, write=FMean};
      __property bool Minimum = {read=FMinimum, write=FMinimum};
      __property bool Maximum = {read=FMaximum, write=FMaximum};
      __property bool Count = {read=FCount, write=FCount};
   };
//---------------------------------------------------------------------------
#endif

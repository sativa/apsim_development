//---------------------------------------------------------------------------
#ifndef TXY_analysisH
#define TXY_analysisH
//---------------------------------------------------------------------------
#include <SysUtils.hpp>
#include <Controls.hpp>
#include <Classes.hpp>
#include <Forms.hpp>
#include "TAnalysis.h"
#include "TAnalysis_series.h"
// ------------------------------------------------------------------
//  Short description:
//      this component simply captures the settings for an xy chart.

//  Notes:

//  Changes:
//    DPH 18/4/01

// ------------------------------------------------------------------
class PACKAGE TXY_analysis : public TAnalysis
   {
   public:
      enum StatType {mean, P10, P20, P30, P40, P50, P60, P70, P80, P90};

   private:
      struct XYSeries
         {
         XYSeries(AnsiString& n, SeriesTypes s, bool y2, bool iscum)
            : yname(n), seriesType(s), plotOnY2(y2), isCumulative(iscum) { }
         XYSeries(const std::string& st);

         AnsiString yname;
         SeriesTypes seriesType;
         bool plotOnY2;
         bool isCumulative;

         bool operator== (const AnsiString& name)
            {return (yname == name);}

         std::string toString();
         };
      std::vector<XYSeries> series;

      struct XYStatLine
         {
         AnsiString dataBlockName;
         StatType statType;
         XYStatLine(AnsiString d, StatType s) : dataBlockName(d), statType(s) {}
         XYStatLine(const std::string& st);

         std::string toString();
         };
      std::vector<XYStatLine> statLines;

      AnsiString statTypeToName(StatType type) const;
      void addStatLineDataFor(const std::string& dataBlockName);
      double calcStatValue(std::vector<double>& yValues, StatType statType);

   protected:
      virtual TAPSTable_form* createPropertiesForm();
      virtual void calcAndStoreRecords(void);
      virtual void load();
      virtual void save();

   public:
      __fastcall TXY_analysis(TComponent* Owner);

      bool isYVariableSelected(AnsiString& variableName);
      bool isYVariableOnY2(AnsiString& variableName);
      bool isYVariableCumulative(AnsiString& variableName);
      int  seriesCount(void) {return series.size();}
      SeriesTypes getSeriesType(AnsiString& name);
      void getSeriesDetails(unsigned int seriesIndex,
                            AnsiString& name, bool& plotOnY2,
                            bool& isCumulative,
                            SeriesTypes& seriesType);
      void clearSeries(void) {series.erase(series.begin(), series.end());}
      void addSeries(AnsiString name, SeriesTypes seriesType, bool y2, bool isCumulative)
         {series.push_back(XYSeries(name, seriesType, y2, isCumulative));}
      void addStatLine(AnsiString dataBlockName, StatType statType)
         {statLines.push_back(XYStatLine(dataBlockName, statType));}

   };
//---------------------------------------------------------------------------
#endif

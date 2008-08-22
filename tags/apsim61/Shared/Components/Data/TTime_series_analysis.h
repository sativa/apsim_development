//---------------------------------------------------------------------------
#ifndef TTime_series_analysisH
#define TTime_series_analysisH
//---------------------------------------------------------------------------
#include <SysUtils.hpp>
#include <Controls.hpp>
#include <Classes.hpp>
#include <Forms.hpp>
#include "TAnalysis.h"
// ------------------------------------------------------------------
//  Short description:
//      this component analyses the data by firstly calculating a base line
//      value (eg mean or some percentile) and then subtracting every value
//      from this base line value.

//  Notes:

//  Changes:
//    DPH 14/7/98
//    DAH 31/10/00: D-380    - removed  Get_year_field_name() and put it up
//                             with TAPSTable

// ------------------------------------------------------------------
class PACKAGE TTime_series_analysis : public TAnalysis
   {
   private:
      bool FWith_mean_line;
      bool FWith_percentile_line;
      bool FDiff_from_mean;
      bool FDiff_from_percentile;
      bool FProb_exceed;
      int FPercentile;
      double FOrigin;
      AnsiString FBase_pivot_value;
      bool FDifferent_above_below_colours;

      // getters and setters.
      void __fastcall Set_with_mean_line (bool With_mean_line);
      void __fastcall Set_with_percentile_line (bool With_percentile_line);
      void __fastcall Set_diff_from_mean (bool Diff_from_mean);
      void __fastcall Set_diff_from_percentile (bool Diff_from_percentile);

      void calcOriginValue(const std::string& fieldName);
      void storeStatisticFields(void);
      void storeValues(std::vector<std::string>& yearValues,
                       std::vector<std::string>& values,
                       const std::string& fieldName);
      void storeValues(std::vector<double>& yearValues,
                       std::vector<double>& values,
                       const std::string& fieldName);
      void createNewPivotField(const std::string& fieldValue);
      void subtractOrigin(std::vector<std::string>& fieldValues, double FOrigin);
   protected:
      virtual void calcAndStoreRecords();
      virtual TAPSTable_form* createPropertiesForm();
      virtual void load();
      virtual void save();

   public:
      __fastcall TTime_series_analysis(TComponent* Owner);

   __published:
      __property bool With_mean_line = {read=FWith_mean_line, write=Set_with_mean_line};
      __property bool With_percentile_line = {read=FWith_percentile_line, write=Set_with_percentile_line};
      __property bool Diff_from_mean = {read=FDiff_from_mean, write=Set_diff_from_mean};
      __property bool Diff_from_percentile = {read=FDiff_from_percentile, write=Set_diff_from_percentile};
      __property int  Percentile = {read=FPercentile, write=FPercentile};
      __property bool Prob_exceedence = {read=FProb_exceed, write=FProb_exceed};
      __property double Origin = {read=FOrigin};
      __property AnsiString Base_pivot_value = {read=FBase_pivot_value, write=FBase_pivot_value};
      __property bool Different_above_below_colours = {read=FDifferent_above_below_colours, write=FDifferent_above_below_colours};
   };
//---------------------------------------------------------------------------
#endif

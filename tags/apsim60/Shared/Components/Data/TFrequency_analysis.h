//---------------------------------------------------------------------------
#ifndef TFrequency_analysisH
#define TFrequency_analysisH
//---------------------------------------------------------------------------
#include <SysUtils.hpp>
#include <Controls.hpp>
#include <Classes.hpp>
#include <Forms.hpp>
#include "TAnalysis.h"
#define MAX_CATEGORIES 20

#define FREQUENCY_FIELD_NAME "Frequency"
#define PROBABILITY_FIELD_NAME "Probability"


// ------------------------------------------------------------------
//  Short description:
//      this class performs a frequency analysis on some source data.

//  Notes:
//

//  Changes:
//    DPH 15/7/98
//    DAH 1/11/00 fixing D362 by increasing dimension of  FCategory_start_value and
//                FCategory_end_value by 1.

// ------------------------------------------------------------------
class PACKAGE TFrequency_analysis : public TAnalysis
   {
   private:
      double FCategory_start_value[MAX_CATEGORIES+1];
      double FCategory_end_value[MAX_CATEGORIES+1];
      int FNum_categories;
      bool FCreate_categories;
      bool FFrequency_analysis;

      // getters and setters.
      double            __fastcall Get_start_value (int index);
      void              __fastcall Set_start_value (int index, double value);
      double            __fastcall Get_end_value (int index);
      void              __fastcall Set_end_value (int index, double value);
      void              __fastcall Set_num_categories(int num_categories);

      virtual void createCategories(void);

   protected:
      virtual void calcAndStoreRecords();
      virtual TAPSTable_form* createPropertiesForm();

      // this method performs all the frequency calculations.
      virtual void calculateFreqDist(std::vector<double>& Values,
                                     std::vector<std::string>& Frequency_labels,
                                     std::vector<double>& Frequency_numbers);
      virtual void load();
      virtual void save();

   public:
      __fastcall TFrequency_analysis(TComponent* Owner);

   __published:
      __property double Category_start_value[int index] = {read=Get_start_value, write=Set_start_value};
      __property double Category_end_value[int index] = {read=Get_end_value, write=Set_end_value};
      __property int    Num_categories = {read=FNum_categories, write=Set_num_categories};
      __property bool   Frequency_analysis = {read=FFrequency_analysis, write=FFrequency_analysis};

   };
//---------------------------------------------------------------------------
#endif

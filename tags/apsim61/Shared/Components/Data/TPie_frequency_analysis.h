//---------------------------------------------------------------------------
#ifndef TPie_frequency_analysisH
#define TPie_frequency_analysisH
//---------------------------------------------------------------------------
#include <SysUtils.hpp>
#include <Controls.hpp>
#include <Classes.hpp>
#include <Forms.hpp>
#include "TFrequency_analysis.h"
// ------------------------------------------------------------------
//  Short description:
//      this class performs a percentile frequency analysis on some source data.

//  Notes:
//

//  Changes:
//    DPH 15/7/98

// ------------------------------------------------------------------
class PACKAGE TPie_frequency_analysis : public TFrequency_analysis
   {
   private:
      double FCategory_percentile[MAX_CATEGORIES];
      AnsiString FBaseDatasetName;
      AnsiString FSecondDatasetName;

      // getters and setters.
      double __fastcall Get_percentile (int index);
      void   __fastcall Set_percentile (int index, double percentile);
      int    __fastcall Get_num_equal_percentiles (void);
      void   __fastcall Set_num_equal_percentiles (int num_percentiles);

   protected:
      virtual TAPSTable_form* createPropertiesForm();

      // this method performs all the frequency calculations.
      virtual void createCategories(void);
      virtual void load();
      virtual void save();

   public:
      __fastcall TPie_frequency_analysis(TComponent* Owner);

   __published:
      __property double Category_percentile[int index] = {read=Get_percentile, write=Set_percentile};
      __property int Num_equal_percentiles = {read=Get_num_equal_percentiles, write=Set_num_equal_percentiles};
      __property AnsiString BaseDatasetName = {read=FBaseDatasetName, write=FBaseDatasetName};
      __property AnsiString SecondDatasetName = {read=FSecondDatasetName, write=FSecondDatasetName};
   };
//---------------------------------------------------------------------------
#endif

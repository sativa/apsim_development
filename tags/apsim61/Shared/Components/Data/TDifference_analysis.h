//---------------------------------------------------------------------------
#ifndef TDifference_analysisH
#define TDifference_analysisH
//---------------------------------------------------------------------------
#include <SysUtils.hpp>
#include <Controls.hpp>
#include <Classes.hpp>
#include <Forms.hpp>
#include "TAnalysis.h"
#include "TAPSTable.h"
// ------------------------------------------------------------------
//  Short description:
//      This class encapsulates a Difference analysis

//  Notes:

//  Changes:
//    DPH 21/7/98

// ------------------------------------------------------------------
class PACKAGE TDifference_analysis : public TAnalysis
   {
   private:
      TStringList* FLHS_pair;
      TStringList* FRHS_pair;
      void subtractData(std::vector<double>& LHS_values,
                        std::vector<double>& LHS_years,
                        std::vector<double>& RHS_values,
                        std::vector<double>& RHS_years,
                        std::vector<double>& Years,
                        std::vector<double>& Values);

   protected:
      virtual void calcAndStoreRecords();
      virtual TAPSTable_form* createPropertiesForm();
      virtual void load();
      virtual void save();

   public:
      __fastcall TDifference_analysis(TComponent* Owner);
      __fastcall ~TDifference_analysis();
   __published:
      __property TStringList* LHS_pair = {read=FLHS_pair, write=FLHS_pair};
      __property TStringList* RHS_pair = {read=FRHS_pair, write=FRHS_pair};
   };
//---------------------------------------------------------------------------
#endif

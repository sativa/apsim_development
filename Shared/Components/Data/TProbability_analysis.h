//---------------------------------------------------------------------------
#ifndef TProbability_analysisH
#define TProbability_analysisH
//---------------------------------------------------------------------------
#include <vcl\SysUtils.hpp>
#include <vcl\Controls.hpp>
#include <vcl\Classes.hpp>
#include <vcl\Forms.hpp>
#include <vcl\DB.hpp>
#include "TAnalysis.h"
#include <general\math_functions.h>
#include <components\general\TMultiStringList.h>
// ------------------------------------------------------------------
//  Short description:
//      This class encapsulates a probability analysis of some dataset.
//      It takes an active dataset as input, performs a probability analysis
//      and writes records to the base memory table.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
class PACKAGE TProbability_analysis : public TAnalysis
   {
   private:
      bool FProb_exceedence;

      // getters and setters.
      void __fastcall Set_prob_exceedence (bool Prob_exceedence);

   protected:
      virtual void calcAndStoreRecords();
      virtual TAPSTable_form* createPropertiesForm();
      virtual void load();
      virtual void save();

   public:
      __fastcall TProbability_analysis(TComponent* Owner);

   __published:
      __property bool Prob_exceedence = {read=FProb_exceedence, write=Set_prob_exceedence};
   };
//---------------------------------------------------------------------------
#endif

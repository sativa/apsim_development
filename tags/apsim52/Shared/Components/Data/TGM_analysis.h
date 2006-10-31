//---------------------------------------------------------------------------
#ifndef TGM_analysisH
#define TGM_analysisH
//---------------------------------------------------------------------------
#include <vcl\SysUtils.hpp>
#include <vcl\Controls.hpp>
#include <vcl\Classes.hpp>
#include <vcl\Forms.hpp>
#include <vcl\DB.hpp>
#include "TAnalysis.h"
#include <general\math_functions.h>
#include <components\general\TMultiStringList.h>
#include <components\data\TSelected_simulations.h>
// ------------------------------------------------------------------
//  Short description:
//      This class encapsulates a probability analysis of some dataset.
//      It takes an active dataset as input, performs a probability analysis
//      and writes records to the base memory table.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
class PACKAGE TGM_analysis : public TAPSTable
   {
   private:
      TSelected_simulations* FSimulations;

      string GetYieldFieldName(TAPSRecord& Raw_data);

   protected:
      virtual void Calc_and_store_records();

   public:
      __fastcall TGM_analysis(TComponent* Owner);
      virtual bool Edit(void);

   __published:
      __property TSelected_simulations* Simulations = {read=FSimulations, write=FSimulations};
   };
//---------------------------------------------------------------------------
#endif

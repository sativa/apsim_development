//---------------------------------------------------------------------------
#ifndef TGM_analysisH
#define TGM_analysisH
//---------------------------------------------------------------------------
#include <vcl\SysUtils.hpp>
#include <vcl\Controls.hpp>
#include <vcl\Classes.hpp>
#include <vcl\Forms.hpp>
#include "MemTable.hpp"
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
class PACKAGE TGM_analysis : public TAnalysis
   {
   private:

   protected:
      virtual void Calc_and_store_records();

   public:
      __fastcall TGM_analysis(TComponent* Owner);

   __published:
   };
//---------------------------------------------------------------------------
#endif

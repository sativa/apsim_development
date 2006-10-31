//---------------------------------------------------------------------------
#ifndef TAnalysisH
#define TAnalysisH
//---------------------------------------------------------------------------
#include <SysUtils.hpp>
#include <Controls.hpp>
#include <Classes.hpp>
#include <Forms.hpp>
#include "TAPSTable.h"
// ------------------------------------------------------------------
//  Short description:
//      base class for all our analysis tables.  It derives from
//      TAPSTable.

//  Notes:

//  Changes:
//    DPH 17/7/98

// ------------------------------------------------------------------
class PACKAGE TAnalysis : public TAPSTable
   {
   private:
      TMultiStringList* FField_names_to_analyse;

      // getters and setters.
      TMultiStringList* __fastcall Get_field_names_to_analyse (void);
   protected:
      virtual TAPSTable_form* Create_properties_form();
      virtual void setupPropertiesForm(TAPSTable_form* form);
      virtual void load(void);
      virtual void save(void);

   public:
      __fastcall TAnalysis(TComponent* Owner);
      __fastcall ~TAnalysis();

   __published:
      __property sourceDataset;
      __property TMultiStringList* Field_names_to_analyse = {read=Get_field_names_to_analyse, write=FField_names_to_analyse};
   };
//---------------------------------------------------------------------------
#endif

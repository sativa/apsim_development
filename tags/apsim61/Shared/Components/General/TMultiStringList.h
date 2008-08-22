//---------------------------------------------------------------------------
#ifndef TMultiStringListH
#define TMultiStringListH

#include <vcl\designeditors.hpp>
// ------------------------------------------------------------------
//  Short description:
//      This class encapsulates a collection of datasets
//      whether they be a list of tables in a database or
//      a list of active datasets in the parent form.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
class PACKAGE TMultiStringList : public TPersistent
   {
   private:
      TStringList* FItems;
      TStringList* FPossibleItems;
   protected:
   public:
      __fastcall TMultiStringList(void);
      __fastcall ~TMultiStringList(void);

      void __fastcall Edit();

   __published:
      __property TStringList* Items = {read=FItems, write=FItems};
      __property TStringList* PossibleItems = {read=FPossibleItems, write=FPossibleItems};
   };


#endif

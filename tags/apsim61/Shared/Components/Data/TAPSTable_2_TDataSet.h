//---------------------------------------------------------------------------
#ifndef TAPSTable_2_TDataSetH
#define TAPSTable_2_TDataSetH
//---------------------------------------------------------------------------
#include <SysUtils.hpp>
#include <Controls.hpp>
#include <Classes.hpp>
#include <Forms.hpp>
#include "kbmMemTable.hpp"
#include <Db.hpp>
#include <DBTables.hpp>
#include "TAPSTable.h"
// ------------------------------------------------------------------
//  Short description:
//      this class is derived from memory table and is capable of displaying
//      the data in a TAPSTable component.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
class PACKAGE TAPSTable_2_TDataSet : public TkbmMemTable
   {
   private:
      TAPSTable* FAPSTable;

      void __fastcall Set_APSTable (TAPSTable* APSTable);
      void __fastcall DoBeforeOpen(void);
      void __fastcall DoAfterOpen(void);

   protected:
   public:
      __fastcall TAPSTable_2_TDataSet(TComponent* Owner);

      void __fastcall Refresh (void);

   __published:
      __property TAPSTable* APSTable = {read=FAPSTable, write=FAPSTable};
   };
//---------------------------------------------------------------------------
 #endif

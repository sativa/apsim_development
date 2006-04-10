//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TAPSTable_2_TDataSet.h"
#pragma link "kbmMemTable"
#pragma package(smart_init)
#pragma resource "*.res"
using namespace std;
//---------------------------------------------------------------------------
// ValidCtrCheck is used to assure that the components created do not have
// any pure virtual functions.
//

static inline void ValidCtrCheck(TAPSTable_2_TDataSet *)
{
   new TAPSTable_2_TDataSet(NULL);
}
//---------------------------------------------------------------------------
namespace Tapstable_2_tdataset
{
   void __fastcall PACKAGE Register()
   {
      TComponentClass classes[1] = {__classid(TAPSTable_2_TDataSet)};
      RegisterComponents("APSRU", classes, 0);
   }
}
// ------------------------------------------------------------------
//  Short description:
//      constructor

//  Notes:

//  Changes:
//    DPH 7/7/98

// ------------------------------------------------------------------
__fastcall TAPSTable_2_TDataSet::TAPSTable_2_TDataSet(TComponent* Owner)
   : TkbmMemTable(Owner)
   {
   FAPSTable = NULL;
   }

// ------------------------------------------------------------------
//  Short description:
//      set the source APSTable component and update our memory table.

//  Notes:

//  Changes:
//    DPH 7/7/98

// ------------------------------------------------------------------
void __fastcall TAPSTable_2_TDataSet::Refresh (void)
   {
   if (FAPSTable != NULL)
      {
      Active = false;
      Active = true;
      }
   }

// ------------------------------------------------------------------
//  Short description:
//      our table is about to be opened.  Fill with fields.

//  Notes:

//  Changes:
//    DPH 7/7/98
//    DAH 10/8/2000  saved and restored cursor

// ------------------------------------------------------------------
void __fastcall TAPSTable_2_TDataSet::DoBeforeOpen(void)
   {
   if (FAPSTable != NULL)
      {
      TCursor Saved_cursor = Screen->Cursor;
      Screen->Cursor = crHourGlass;

      FieldDefs->Clear();
      FAPSTable->getFielddefs(FieldDefs);
      if (FieldDefs->Count == 0)
         {
         TFieldDef* new_field = FieldDefs->AddFieldDef();
         new_field->Name = "Nofields";
         new_field->DataType = ftString;
         new_field->Size = 10;
         }
      Screen->Cursor = Saved_cursor;
      }
   }

// ------------------------------------------------------------------
//  Short description:
//      our table is opened.  Fill with records.

//  Notes:

//  Changes:
//    DPH 7/7/98
//    DAH 10/8/2000  saved and restored cursor

// ------------------------------------------------------------------
void __fastcall TAPSTable_2_TDataSet::DoAfterOpen(void)
   {
   if (FAPSTable != NULL)
      {
      TCursor Saved_cursor = Screen->Cursor;
      Screen->Cursor = crHourGlass;
      vector<TAPSRecord> Data;

      bool ok = FAPSTable->first();
      while (ok)
         {
         for (vector<TAPSRecord>::const_iterator i = FAPSTable->begin();
                                                 i != FAPSTable->end();
                                                 i++)
            (*i).write (this);

         ok = FAPSTable->next();
         }
      Screen->Cursor = Saved_cursor;
      }
   }

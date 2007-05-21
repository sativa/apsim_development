//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TDataSet_2_TAPSTable.h"
#include <general\vcl_functions.h>
#include <general\db_functions.h>

#pragma package(smart_init)
#pragma resource "*.res"
using namespace std;
//---------------------------------------------------------------------------
// ValidCtrCheck is used to assure that the components created do not have
// any pure virtual functions.
//

static inline void ValidCtrCheck(TDataSet_2_TAPSTable *)
{
   new TDataSet_2_TAPSTable(NULL);
}
//---------------------------------------------------------------------------
namespace Tdataset_2_tapstable
{
   void __fastcall PACKAGE Register()
   {
      TComponentClass classes[1] = {__classid(TDataSet_2_TAPSTable)};
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
__fastcall TDataSet_2_TAPSTable::TDataSet_2_TAPSTable(TComponent* Owner)
   : TAPSTable(Owner)
   {
   }

// ------------------------------------------------------------------
//  Short description:
//      constructor

//  Notes:

//  Changes:
//    DPH 7/7/98

// ------------------------------------------------------------------
__fastcall TDataSet_2_TAPSTable::~TDataSet_2_TAPSTable (void)
   {
   }

// ------------------------------------------------------------------
//  Short description:
//      read in all records from source dataset.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void TDataSet_2_TAPSTable::refresh (void)
   {
   Read_all_records();
   }

// ------------------------------------------------------------------
//  Short description:
//     return a list of source field names to caller.

//  Notes:

//  Changes:
//    DPH 5/2/98
//    DAH 13/4/02:   Changed the call Get_field_list to the new equivalent
//                   from db_functions.h

// ------------------------------------------------------------------
void TDataSet_2_TAPSTable::Get_source_field_names (vector<string>& field_names)
   {
   if (Fdataset != NULL)
      getDBFieldNames (Fdataset, field_names);
   }

// ------------------------------------------------------------------
//  Short description:
//      read in all records from source dataset.

//  Notes:

//  Changes:
//    DPH 5/2/98
//    DAH 10/8/2000  saved and restored cursor
//    DAH 13/4/02:   Changed the call Get_field_list to the new equivalent
//                   from db_functions.h

// ------------------------------------------------------------------
void TDataSet_2_TAPSTable::Read_all_records (void)
   {
   if (Fdataset != NULL)
      {
      TCursor Saved_Cursor = Screen->Cursor;
      Screen->Cursor = crHourGlass;
      beginStoringData();
      TAPSRecord rec;
      vector<string> stl_field_names;
      getDBFieldNames (Fdataset, stl_field_names);

      for (vector<string>::iterator i = stl_field_names.begin();
                                    i != stl_field_names.end();
                                    i++)
         addField(*i);

      Fdataset->First();
      while (!Fdataset->Eof)
         {
         rec.read(Fdataset, stl_field_names);
         storeRecord(rec);
         Fdataset->Next();
         }
      endStoringData();

      Screen->Cursor = Saved_Cursor;
      }
   }



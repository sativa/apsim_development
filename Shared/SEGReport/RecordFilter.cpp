//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "RecordFilter.h"
#include "DataContainer.h"
#include <general\db_functions.h>
#include <general\string_functions.h>

using namespace std;

//---------------------------------------------------------------------------
// Create the necessary fields in the result dataset.
//---------------------------------------------------------------------------
void processRecordFilter(DataContainer& parent,
                         const XMLNode& properties,
                         TDataSet& result)
   {
   result.Active = false;
   result.FieldDefs->Clear();
   TDataSet* source = parent.data(parent.read(properties, "source"));

   if (source != NULL && source->Active)
      {
      result.FieldDefs->Assign(source->FieldDefs);
      result.Active = true;

      bool firstRecord = Str_i_Eq(parent.read(properties, "firstRecord"), "yes");
      bool lastRecord = Str_i_Eq(parent.read(properties, "lastRecord"), "yes");
      int recordNumber = atoi(parent.read(properties, "RecordNumber").c_str());

      // loop through all records.
      source->First();
      while (!source->Eof)
         {
         if (firstRecord && source->RecNo == 1)
            copyDBRecord(source, &result);
         if (lastRecord && source->RecNo == source->RecordCount)
            copyDBRecord(source, &result);
         if (recordNumber != 0 && source->RecNo == recordNumber)
            copyDBRecord(source, &result);
         source->Next();
         }
      }
   }


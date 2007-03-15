//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "RecordFilter.h"
#include <general\db_functions.h>
#include <general\string_functions.h>

using namespace std;

//---------------------------------------------------------------------------
// Create the necessary fields in the result dataset.
//---------------------------------------------------------------------------
void RecordFilter::createFields(TDataSet* source, TDataSet* result)
   {
   if (source != NULL)
      result->FieldDefs->Assign(source->FieldDefs);
   }
//---------------------------------------------------------------------------
// Go do our processing, putting all results into 'data'
//---------------------------------------------------------------------------
void RecordFilter::process(TDataSet* source, TDataSet* result)
   {
   bool firstRecord = Str_i_Eq(getProperty("firstRecord"), "yes");
   bool lastRecord = Str_i_Eq(getProperty("lastRecord"), "yes");
   int recordNumber = atoi(getProperty("RecordNumber").c_str());

   // loop through all records.
   source->First();
   while (!source->Eof)
      {
      if (firstRecord && source->RecNo == 1)
         copyDBRecord(source, result);
      if (lastRecord && source->RecNo == source->RecordCount)
         copyDBRecord(source, result);
      if (recordNumber != 0 && source->RecNo == recordNumber)
         copyDBRecord(source, result);
      source->Next();
      }
   }


//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TCumulative.h"
#include <vector>
#include <string>
#include <general\string_functions.h>
#include <general\db_functions.h>

using namespace std;
#pragma package(smart_init)
#pragma resource "ComponentRegistration.res"

//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
__fastcall TCumulative::TCumulative(TComponent* owner)
   : TSEGTable(owner)
   {
   }

//---------------------------------------------------------------------------
// destructor
//---------------------------------------------------------------------------
__fastcall TCumulative::~TCumulative()
   {
   }
//---------------------------------------------------------------------------
// Called by our base class to allow us to add any fielddefs we may want to.
// The table will be closed (Active=false) when this routine is called.
//---------------------------------------------------------------------------
bool TCumulative::createFields(void) throw(runtime_error)
   {
   if (source != NULL)
      {
      FieldDefs->Assign(source->FieldDefs);
      for (int i = 0; i != source->FieldDefs->Count; i++)
         {
         TFieldDef* fieldDef = source->FieldDefs->Items[i];
         if (fieldDef->DataType == ftFloat)
            {
            AnsiString newField = "CUMULATIVE_" + fieldDef->Name;
            addDBField(this, newField.c_str(), "1.0");
            }
         }
      return true;
      }
   return false;
   }
//---------------------------------------------------------------------------
// Called by our base class to allow us to add records to the table.
// The table is open (Active = true) when this routine is called.
//---------------------------------------------------------------------------
void TCumulative::storeRecords(void) throw(runtime_error)
   {
   if (source != NULL)
      {
      // setup some space to store cumulative values for each column.
      int numColumns = source->FieldDefs->Count;
      double* sums = new double[numColumns];
      for (int i = 0; i != numColumns; i++)
         sums[i] = 0.0;

      // loop through all records.
      source->First();
      while (!source->Eof)
         {
         // add a new record that is identical to the current source record.
         copyDBRecord(source, this);

         Edit();
         for (int i = 0; i != source->FieldDefs->Count; i++)
            {
            TFieldDef* fieldDef = source->FieldDefs->Items[i];
            if (fieldDef->DataType == ftFloat)
               {
               sums[i] += source->Fields->Fields[i]->AsFloat;
               AnsiString newField = "CUMULATIVE_" + fieldDef->Name;
               FieldValues[newField] = sums[i];
               }
            }
         Post();
         source->Next();
         }

      delete [] sums;
      }
   }


//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TFilter.h"
#include <vector>
#include <string>
#include <fstream>
#include <sstream>
#include <general\string_functions.h>
#include <general\db_functions.h>
#include <general\vcl_functions.h>
#include <general\path.h>
#include <general\inifile.h>

using namespace std;
#pragma package(smart_init)
#pragma resource "ComponentRegistration.res"  

//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
__fastcall TFilter::TFilter(TComponent* owner)
   : TSEGTable(owner)
   {
   columnNames = new TStringList;
   columnValues = new TStringList;
   }

//---------------------------------------------------------------------------
// destructor
//---------------------------------------------------------------------------
__fastcall TFilter::~TFilter()
   {
   delete columnNames;
   delete columnValues;
   }
//---------------------------------------------------------------------------
// set the 'columnName' property and refresh all data.
//---------------------------------------------------------------------------
void __fastcall TFilter::setColumnName(AnsiString name)
   {
   if (columnName != name)
      {
      columnName = name;
      getColumnValues();
      refresh();
      }
   }
//---------------------------------------------------------------------------
// set the 'columnValue' property and refresh all data.
//---------------------------------------------------------------------------
void __fastcall TFilter::setColumnValue(AnsiString value)
   {
   if (columnValue != value)
      {
      columnValue = value;
      refresh();
      }
   }
//---------------------------------------------------------------------------
// Called by our base class to allow us to add any fielddefs we may want to.
// The table will be closed (Active=false) when this routine is called.
//---------------------------------------------------------------------------
void TFilter::createFields(void) throw(runtime_error)
   {
   if (source != NULL)
      {
      getColumnNames();
      FieldDefs->Assign(source->FieldDefs);
      }
   }
//---------------------------------------------------------------------------
// Called by our base class to allow us to add records to the table.
// The table is open (Active = true) when this routine is called.
//---------------------------------------------------------------------------
void TFilter::storeRecords(void) throw(runtime_error)
   {
   if (source != NULL && fieldName != "" && fieldValue != "")
      {
      // loop through all records.
      source->First();
      while (!source->Eof)
         {
         string thisValue = getDBValue(source, fieldName.c_str());
         if (Str_i_Eq(thisValue, fieldValue.c_str()))
            {
            // add a new record that is identical to the current source record.
            copyDBRecord(source, this);
            }
         source->Next();
         }
      }
   }
// ------------------------------------------------------------------
// Fill the column names list.
// ------------------------------------------------------------------
void TFilter::getColumnNames()
   {
   if (source != NULL)
      {
      vector<string> fieldNames;
      getDBFieldNames(source, fieldNames);
      Stl_2_tstrings(fieldNames, columnNames);
      }
   }
// ------------------------------------------------------------------
// Fill the column values list.
// ------------------------------------------------------------------
void TFilter::getColumnValues()
   {
   columnValues->Clear();
   if (source != NULL && fieldName != "")
      {
      source->First();
      while (!source->Eof)
         {
         string thisValue = getDBValue(source, fieldName.c_str());
         if (columnValues->IndexOf(thisValue.c_str()) == -1)
            columnValues->Add(thisValue.c_str());

         source->Next();
         }
      }
   }


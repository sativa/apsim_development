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
   }
//---------------------------------------------------------------------------
// destructor
//---------------------------------------------------------------------------
__fastcall TFilter::~TFilter()
   {
   }
//---------------------------------------------------------------------------
// set the 'filter' property and refresh all data.
//---------------------------------------------------------------------------
void __fastcall TFilter::setFilter(AnsiString filter)
   {
   if (Filter != filter)
      {
      filterString = filter;
      Active = false;
      Active = true;
      }
   }
//---------------------------------------------------------------------------
// Called by our base class to allow us to add any fielddefs we may want to.
// The table will be closed (Active=false) when this routine is called.
//---------------------------------------------------------------------------
void TFilter::createFields(void) throw(runtime_error)
   {
   if (source != NULL)
      FieldDefs->Assign(source->FieldDefs);
   }
//---------------------------------------------------------------------------
// Called by our base class to allow us to add records to the table.
// The table is open (Active = true) when this routine is called.
//---------------------------------------------------------------------------
void TFilter::storeRecords(void) throw(runtime_error)
   {
   if (source != NULL)
      {
      source->Filter = filterString;
      source->Filtered = true;
      // loop through all records.
      source->First();
      while (!source->Eof)
         {
         copyDBRecord(source, this);
         source->Next();
         }
      source->Filtered = false;
      source->Filter = "";
      }
   }


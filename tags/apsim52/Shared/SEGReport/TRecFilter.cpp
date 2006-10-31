//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TRecFilter.h"
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
__fastcall TRecFilter::TRecFilter(TComponent* owner)
   : TSEGTable(owner)
   {
   FirstRecord = false;
   LastRecord = false;
   RecordNumber = 0;
   }
//---------------------------------------------------------------------------
// destructor
//---------------------------------------------------------------------------
__fastcall TRecFilter::~TRecFilter()
   {
   }
//---------------------------------------------------------------------------
// set the 'firstrecord' property and refresh all data.
//---------------------------------------------------------------------------
void __fastcall TRecFilter::setFirstRecord(bool first)
   {
   if (FirstRecord != first)
      {
      FirstRecord = first;
      forceRefresh();
      }
   }
//---------------------------------------------------------------------------
// set the 'lastrecord' property and refresh all data.
//---------------------------------------------------------------------------
void __fastcall TRecFilter::setLastRecord(bool last)
   {
   if (LastRecord != last)
      {
      LastRecord = last;
      forceRefresh();
      }
   }
//---------------------------------------------------------------------------
// set the 'recordNumber' property and refresh all data.
//---------------------------------------------------------------------------
void __fastcall TRecFilter::setRecordNumber(int recNo)
   {
   if (RecordNumber != recNo)
      {
      RecordNumber = recNo;
      forceRefresh();
      }
   }
//---------------------------------------------------------------------------
// Called by our base class to allow us to add any fielddefs we may want to.
// The table will be closed (Active=false) when this routine is called.
//---------------------------------------------------------------------------
bool TRecFilter::createFields(void) throw(runtime_error)
   {
   try
      {
      if (source != NULL)
         {
         FieldDefs->Assign(source->FieldDefs);
         return true;
         }
      }
   catch (const Exception& err)
      {

      }
   return false;
   }
//---------------------------------------------------------------------------
// Called by our base class to allow us to add records to the table.
// The table is open (Active = true) when this routine is called.
//---------------------------------------------------------------------------
void TRecFilter::storeRecords(void) throw(runtime_error)
   {
   try
      {
      if (source != NULL)
         {
         // loop through all records.
         source->First();
         while (!source->Eof)
            {
            if (FirstRecord && source->RecNo == 1)
               copyDBRecord(source, this);
            if (LastRecord && source->RecNo == source->RecordCount)
               copyDBRecord(source, this);
            if (RecordNumber != 0 && source->RecNo == RecordNumber)
               copyDBRecord(source, this);
            source->Next();
            }
         }
      }
   catch (const Exception& err)
      {
      }
   }


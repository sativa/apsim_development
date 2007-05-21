//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "ChartLine.h"
#include <general\db_functions.h>
#include <general\math_functions.h>
#include <general\string_functions.h>

using namespace std;
#pragma package(smart_init)
#pragma resource "ComponentRegistration.res"

static const char* PROBABILITY_FIELD_NAME = "Probability";
//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
__fastcall ChartLine::ChartLine(TComponent* owner)
   : TSEGTable(owner)
   {
   }

//---------------------------------------------------------------------------
// destructor
//---------------------------------------------------------------------------
__fastcall ChartLine::~ChartLine()
   {
   }
//---------------------------------------------------------------------------
// Set the fieldName property.
//---------------------------------------------------------------------------
void __fastcall ChartLine::setFieldName(AnsiString fieldName)
   {
   if (FieldName != fieldName)
      {
      FieldName = fieldName;
      forceRefresh();
      }
   }
//---------------------------------------------------------------------------
// Set the x1 property.
//---------------------------------------------------------------------------
void __fastcall ChartLine::setX1(float x1)
   {
   if (X1 != x1)
      {
      X1 = x1;
      forceRefresh();
      }
   }
//---------------------------------------------------------------------------
// Set the x2 property.
//---------------------------------------------------------------------------
void __fastcall ChartLine::setX2(float x2)
   {
   if (X2 != x2)
      {
      X2 = x2;
      forceRefresh();
      }
   }
//---------------------------------------------------------------------------
// Set the label property.
//---------------------------------------------------------------------------
void __fastcall ChartLine::setLabel(AnsiString label)
   {
   if (Label != label)
      {
      Label = label;
      forceRefresh();
      }
   }
//---------------------------------------------------------------------------
// Called by our base class to allow us to add any fielddefs we may want to.
// The table will be closed (Active=false) when this routine is called.
//---------------------------------------------------------------------------
bool ChartLine::createFields(void) throw(runtime_error)
   {
   if (source != NULL && FieldName != "")
      {
      FieldDefs->Clear();
      addDBField(this, "Label", Label.c_str());
      addDBField(this, "X", "1.0");
      addDBField(this, fieldName.c_str(), "1.0");
      return true;
      }
   return false;
   }
//---------------------------------------------------------------------------
// Called by our base class to allow us to add records to the table.
// The table is open (Active = true) when this routine is called.
//---------------------------------------------------------------------------
void ChartLine::storeRecords(void) throw(runtime_error)
   {
   if (source != NULL && FieldName != "")
      {
      // Loop through all series blocks and all records within that series.
      bool ok = source->firstSeries();
      while (ok)
         {
         if (source->FieldDefs->IndexOf(FieldName) != -1)
            {
            Append();
            FieldValues["Label"] = Label;
            FieldValues[FieldName] = source->FieldValues[FieldName];
            FieldValues["X"] = X1;
            Post();
            Append();
            FieldValues["Label"] = Label;
            FieldValues[FieldName] = source->FieldValues[FieldName];
            FieldValues["X"] = X2;
            Post();
            }
         ok = source->nextSeries();
         }
      source->cancelSeries();
      }
   }


//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TPredObs.h"
#include <general\db_functions.h>
#include <general\math_functions.h>
#include <general\string_functions.h>
#include <general\stl_functions.h>
#include <numeric>

using namespace std;
#pragma package(smart_init)
#pragma resource "ComponentRegistration.res"      

//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
__fastcall TPredObs::TPredObs(TComponent* owner)
   : TSEGTable(owner)
   {
   predDataset = NULL;
   obsDataset = NULL;
   TStringList* fieldNames = new TStringList();
   fieldNames->CaseSensitive = false;
   keyFieldNames = fieldNames;
   }

//---------------------------------------------------------------------------
// destructor
//---------------------------------------------------------------------------
__fastcall TPredObs::~TPredObs()
   {
   delete keyFieldNames;
   }
//---------------------------------------------------------------------------
// Set the predicted dataset property.
//---------------------------------------------------------------------------
void __fastcall TPredObs::setPredDataset(TSEGTable* dataset)
   {
   if (predDataset != dataset)
      {
      predDataset = dataset;
      forceRefresh();
      }
   }
//---------------------------------------------------------------------------
// Set the observed dataset property.
//---------------------------------------------------------------------------
void __fastcall TPredObs::setObsDataset(TSEGTable* dataset)
   {
   if (obsDataset != dataset)
      {
      obsDataset = dataset;
      forceRefresh();
      }
   }
//---------------------------------------------------------------------------
// Set the key field names property.
//---------------------------------------------------------------------------
void __fastcall TPredObs::setKeyFieldNames(TStrings* fieldNames)
   {
   keyFieldNames->Assign(fieldNames);
   forceRefresh();
   }
//---------------------------------------------------------------------------
// Called by our base class to allow us to add any fielddefs we may want to.
// The table will be closed (Active=false) when this routine is called.
//---------------------------------------------------------------------------
bool TPredObs::createFields(void) throw(runtime_error)
   {
   if (predDataset != NULL && obsDataset != NULL && keyFieldNames->Count > 0)
      {
      FieldDefs->Clear();
      for (int f = 0; f != obsDataset->FieldDefs->Count; f++)
         {
         // if the obs field is in the keyfields then add it to our fielddefs.
         if (keyFieldNames->IndexOf(obsDataset->FieldDefs->Items[f]->Name) >= 0)
            FieldDefs->Add(obsDataset->FieldDefs->Items[f]->Name,
                           obsDataset->FieldDefs->Items[f]->DataType,
                           obsDataset->FieldDefs->Items[f]->Size,
                           false);
         else
            {
            // if the obs field is in the predicted dataset then add a
            // pred and a obs version of the field to our fielddefs.
            if (predDataset->FieldDefs->IndexOf(obsDataset->FieldDefs->Items[f]->Name) >= 0)
               {
               FieldDefs->Add("Pred" + obsDataset->FieldDefs->Items[f]->Name,
                              obsDataset->FieldDefs->Items[f]->DataType,
                              obsDataset->FieldDefs->Items[f]->Size,
                              false);
               FieldDefs->Add("Obs" + obsDataset->FieldDefs->Items[f]->Name,
                              obsDataset->FieldDefs->Items[f]->DataType,
                              obsDataset->FieldDefs->Items[f]->Size,
                              false);
               }
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
void TPredObs::storeRecords(void) throw(runtime_error)
   {
   if (predDataset != NULL && obsDataset != NULL && keyFieldNames->Count > 0)
      {
      // Loop through all series blocks and all records within that series.
      obsDataset->First();
      while (!obsDataset->Eof)
         {
         Append();

         // copy all key fields to us.
         for (int f = 0; f != obsDataset->FieldDefs->Count; f++)
            {
            String FieldName = obsDataset->FieldDefs->Items[f]->Name;
            if (keyFieldNames->IndexOf(FieldName) >= 0)
               FieldValues[FieldName] = obsDataset->FieldValues[FieldName];
            }

         // need to locate the correct record in predicted dataset by creating
         // a filter.
         AnsiString Filter;
         for (int k = 0; k != keyFieldNames->Count; k++)
            {
            String FieldName = keyFieldNames->Strings[k];
            if (k > 0)
               Filter += " and ";
            Filter += FieldName + " = '" + obsDataset->FieldValues[FieldName] + "'";
            }
         predDataset->Filter = Filter;
         predDataset->Filtered = true;

         // If we found a single record that matches the observed record then
         // go through all observed fields and copy the corresponding field
         // from the predicted record.
         if (predDataset->RecordCount == 1)
            {
            for (int f = 0; f != obsDataset->FieldDefs->Count; f++)
               {
               String FieldName = obsDataset->FieldDefs->Items[f]->Name;
               if (keyFieldNames->IndexOf(FieldName) == -1)
                  {
                  if (predDataset->FieldDefs->IndexOf(FieldName) >= 0 &&
                      !obsDataset->FieldValues[FieldName].IsNull())
                     {
                     FieldValues["Obs" + FieldName] = obsDataset->FieldValues[FieldName];
                     FieldValues["Pred" + FieldName] = predDataset->FieldValues[FieldName];
                     }
                  }
               }
            }

         // Clean up the predicted dataset by getting rid of the filter and us
         // by issuing a Post.
         predDataset->Filtered = false;
         Post();

         obsDataset->Next();
         }
      }
   }


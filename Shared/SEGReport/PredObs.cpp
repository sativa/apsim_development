//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "PredObs.h"
#include "ApsimFileReader.h"
#include <general\db_functions.h>
#include <general\math_functions.h>
#include <general\string_functions.h>
#include <general\stl_functions.h>
#include <general\xml.h>
#include <numeric>
#include <kbmmemtable.hpp>

using namespace std;


//---------------------------------------------------------------------------
// Create the necessary fields in the result dataset.
//---------------------------------------------------------------------------
void PredObs::createFields(TDataSet* pred, TDataSet* result)
   {
   string obsFileName = getProperty("obsfilename");
   vector<string> keyFieldNames = getProperties("keyfieldname");

   if (obsFileName != "" && keyFieldNames.size() > 0)
      {
      TDataSet* obs = getObservedData(obsFileName);

      for (int f = 0; f != obs->FieldDefs->Count; f++)
         {
         // if the obs field is in the keyfields then add it to our fielddefs.
         if (isKeyField(keyFieldNames, obs->FieldDefs->Items[f]->Name.c_str()))
            result->FieldDefs->Add(obs->FieldDefs->Items[f]->Name,
                                   obs->FieldDefs->Items[f]->DataType,
                                   obs->FieldDefs->Items[f]->Size,
                                   false);
         else
            {
            // if the obs field is in the predicted dataset then add a
            // pred and a obs version of the field to our fielddefs.
            if (pred->FieldDefs->IndexOf(obs->FieldDefs->Items[f]->Name) >= 0)
               {
               result->FieldDefs->Add("Pred" + obs->FieldDefs->Items[f]->Name,
                                      obs->FieldDefs->Items[f]->DataType,
                                      obs->FieldDefs->Items[f]->Size,
                                      false);
               result->FieldDefs->Add("Obs" + obs->FieldDefs->Items[f]->Name,
                                      obs->FieldDefs->Items[f]->DataType,
                                      obs->FieldDefs->Items[f]->Size,
                                      false);
               }
            }
         }
      }
   }

//---------------------------------------------------------------------------
// Go do our processing, putting all results into 'data'
//---------------------------------------------------------------------------
void PredObs::process(TDataSet* pred, TDataSet* result)
   {
   string obsFileName = getProperty("obsfilename");
   vector<string> keyFieldNames = getProperties("keyfieldname");

   if (obsFileName != "" && keyFieldNames.size() > 0)
      {
      TDataSet* obs = getObservedData(obsFileName);

      // Loop through all series blocks and all records within that series.
      obs->First();
      while (!obs->Eof)
         {
         // need to locate the correct record in predicted dataset by creating
         // a filter.
         String Filter;
         for (unsigned k = 0; k != keyFieldNames.size(); k++)
            {
            String fieldName = keyFieldNames[k].c_str();
            if (k > 0)
               Filter += " and ";
            Filter += fieldName + " = '" + obs->FieldValues[fieldName] + "'";
            }
         pred->Filter = Filter;
         pred->Filtered = true;

         // If we found a single record that matches the observed record then
         // go through all observed fields and copy the corresponding field
         // from the predicted record.
         if (pred->RecordCount == 1)
            {
            bool haveCreatedRecord = false;
            for (int f = 0; f != obs->FieldDefs->Count; f++)
               {
               String FieldName = obs->FieldDefs->Items[f]->Name;
               if (!isKeyField(keyFieldNames, FieldName.c_str()))
                  {
                  if (pred->FieldDefs->IndexOf(FieldName) >= 0 &&
                      !obs->FieldValues[FieldName].IsNull())
                     {
                     if (!haveCreatedRecord)
                        {
                        result->Append();

                        // copy all key fields to us.
                        for (int f = 0; f != obs->FieldDefs->Count; f++)
                           {
                           string fieldName = obs->FieldDefs->Items[f]->Name.c_str();
                           if (isKeyField(keyFieldNames, fieldName) && !obs->FieldValues[fieldName.c_str()].IsNull())
                              result->FieldValues[fieldName.c_str()] = obs->FieldValues[fieldName.c_str()];
                           }
                        haveCreatedRecord = true;
                        }

                     result->FieldValues["Obs" + FieldName] = obs->FieldValues[FieldName];
                     result->FieldValues["Pred" + FieldName] = pred->FieldValues[FieldName];

                     }
                  }
               }
            result->Post();
            }

         // Clean up the predicted dataset by getting rid of the filter and us
         // by issuing a Post.
         pred->Filtered = false;
         obs->Next();
         }

      // cleanup
      delete obs;
      }
   }

//---------------------------------------------------------------------------
// Return true if specified field name is a key field.
//---------------------------------------------------------------------------
bool PredObs::isKeyField(const std::vector<std::string>& keyFieldNames, const string& fieldName)
   {
   return (find_if(keyFieldNames.begin(), keyFieldNames.end(),
                   CaseInsensitiveStringComparison(fieldName)) != keyFieldNames.end());
   }

//---------------------------------------------------------------------------
// Return an observed dataset to caller. Caller should delete it when done.
//---------------------------------------------------------------------------
TDataSet* PredObs::getObservedData(const std::string& obsFileName)
   {
   if (!FileExists(obsFileName.c_str()))
      throw runtime_error("Cannot find observed file: " + obsFileName);

   ApsimFileReader obsReader("ApsimFileReader");

   string fileReaderProperties = "<ApsimFileReader><FileName>" + obsFileName + "</FileName><ParseTitle>yes</ParseTitle></ApsimFileReader>";
   XMLDocument doc(fileReaderProperties, XMLDocument::xmlContents);

   TDataSet* obs = new TkbmMemTable(NULL);
   obsReader.setProperties(doc.documentElement());
   obsReader.refresh(NULL, obs);
   return obs;
   }

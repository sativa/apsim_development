//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "Joiner.h"


#include <general\db_functions.h>
#include "DataContainer.h"

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
// this function joins dataset together.
//---------------------------------------------------------------------------
void processJoiner(DataContainer& parent,
                   const XMLNode& properties,
                   TDataSet& result)
   {
   vector<string> sourceNames = parent.reads(properties, "source");

   result.Active = false;
   result.FieldDefs->Clear();
   if (sourceNames.size() > 0)
      {
      TDataSet* source = parent.data(sourceNames[0]);
      if (source != NULL)
         result.FieldDefs->Assign(source->FieldDefs);
      }

   if (result.FieldDefs->Count > 0)
      {
      result.Active = true;

      for (unsigned i = 0; i != sourceNames.size(); i++)
         {
         TDataSet* source = parent.data(sourceNames[i]);

         if (source != NULL)
            {
            source->First();
            while (!source->Eof)
               {
               copyDBRecord(source, &result);
               source->Next();
               }
            }
         }
      }


   }


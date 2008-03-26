//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TProbability_analysis.h"
#include <general\math_functions.h>
#include <general\vcl_functions.h>
#include <general\stl_functions.h>
#include <ApsimShared\ApsimSettings.h>
#include "TProbability_analysis_form.h"

#pragma package(smart_init)

static const char* PROBABILITY_FIELD_NAME = "Probability";

//---------------------------------------------------------------------------
namespace Tprobability_analysis
   {
   void __fastcall PACKAGE Register()
      {
      TComponentClass classes[1] = {__classid(TProbability_analysis)};
      RegisterComponents("APSRU", classes, 0);
      }
   }
// ------------------------------------------------------------------
//  Short description:
//      constructor

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
__fastcall TProbability_analysis::TProbability_analysis(TComponent* Owner)
   : TAnalysis(Owner)
   {
   }
// ------------------------------------------------------------------
// Load settings.
// ------------------------------------------------------------------
void TProbability_analysis::load()
   {
   TAnalysis::load();
   ApsimSettings settings;

   try
      {
      settings.read(CHART_SETTINGS_KEY + "|ProbExceed", FProb_exceedence);
      }
   catch (const exception& err)
      {
      FProb_exceedence = false;
      }
   }

// ------------------------------------------------------------------
// Save settings.
// ------------------------------------------------------------------
void TProbability_analysis::save()
   {
   TAnalysis::save();
   ApsimSettings settings;
   settings.write(CHART_SETTINGS_KEY + "|ProbExceed", FProb_exceedence);
   }
// ------------------------------------------------------------------
//  Short description:
//      set the field names.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void __fastcall TProbability_analysis::Set_prob_exceedence (bool Prob_exceedence)
   {
   FProb_exceedence = Prob_exceedence;
   }
// ------------------------------------------------------------------
//  Short description:
//      calculate and store all records in memory table.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void TProbability_analysis::calcAndStoreRecords()
   {
   if (Field_names_to_analyse->Items->Count > 0)
      {
      beginStoringData();

      // tell the destination apstable (us) to get ready for first data block.
      first();

      // loop through all pivot series.
      bool ok = sourceDataset->first();
      while (ok)
         {
         // for each of our fields, calculate a probability distribution and
         // store in new data matrix.
         vector<double> values, probValues;
         for (int i = 0; i < Field_names_to_analyse->Items->Count; i++)
            {
            const string fieldName = Field_names_to_analyse->Items->Strings[i].c_str();
            sourceDataset->fieldAsNumericArray(fieldName, values);

            // calculate a probability distribution.
            Calculate_prob_dist(values, FProb_exceedence, probValues);

            // store the probability values once only.
            if (i == 0)
               storeNumericArray(PROBABILITY_FIELD_NAME, probValues);
            storeNumericArray(fieldName, values);
            }
         // copy all pivot fields from source to destination.
         copyPivotsFrom(*sourceDataset);

         // goto next pivot series in source dataset.
         ok = sourceDataset->next();

         // tell the destination apstable to get ready for next data.
         next();
         }
      }
   }

// ------------------------------------------------------------------
//  Short description:
//      create and return a pointer to an analysis form.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
TAPSTable_form* TProbability_analysis::createPropertiesForm()
   {
   TProbability_analysis_form* form = new TProbability_analysis_form(Application);
   //form->allowMultipleVariables();
   return form;
   }


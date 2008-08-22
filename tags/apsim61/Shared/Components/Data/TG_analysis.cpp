#pragma link "MemTable"
//---------------------------------------------------------------------------
#include <vcl\vcl.h>
#pragma hdrstop

#include "TGM_analysis.h"
#include <general\math_functions.h>
#include <general\vcl_functions.h>
#include <general\stl_functions.h>
#include "TGM_analysis_form.h"

#pragma package(smart_init)
#pragma resource "*.res"

static const char* PROBABILITY_FIELD_NAME = "Probability";

//---------------------------------------------------------------------------
namespace TGM_analysis
   {
   void __fastcall PACKAGE Register()
      {
      TComponentClass classes[1] = {__classid(TGM_analysis)};
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
__fastcall TGM_analysis::TGM_analysis(TComponent* Owner)
   : TAnalysis(Owner)
   {
   }

// ------------------------------------------------------------------
//  Short description:
//      calculate and store all records in memory table.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void TGM_analysis::Calc_and_store_records()
   {
   Begin_storing_data();

   // goto first record in source dataset.
   vector<TAPSRecord> Raw_data;
   bool ok = Source_dataset->First(FPivot_values->Items);

   // loop through all pivot series.
   while (ok)
      {
      vector<TAPSRecord> New_data;
      Source_dataset->Get(Raw_data);

      // for each pivot series - get a descriptor.
      string Descriptor = Raw_data[0].Get_pivot_value ();

         // for each of our fields, calculate a probability distribution and
         // store in new data matrix.
         vector<double> Values, Prob_values;
         for (int i = 0; i < Field_names_to_analyse->Items->Count; i++)
            {
            const string Field_name = Field_names_to_analyse->Items->Strings[i].c_str();

            Array_records_2_array_numbers(Raw_data, Values, Field_name.c_str());

            // calculate a probability distribution.
            Calculate_prob_dist(Values, FProb_exceedence, Prob_values);

            // store the probability values once only.
            if (i == 0)
               Array_numbers_2_array_records(Prob_values, New_data, PROBABILITY_FIELD_NAME);

            Array_numbers_2_array_records(Values, New_data, Field_name.c_str());
            }
         // set base descriptors.
         Set_base_descriptors (New_data, Descriptor.c_str());

         // store data.
         Store_data (New_data);

         // goto next pivot series.
         Raw_data.erase(Raw_data.begin(), Raw_data.end());
         ok = Source_dataset->Next();
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
TAPSTable_form* TGM_analysis::Create_properties_form()
   {
   return new TGM_analysis_form(Application);
   }


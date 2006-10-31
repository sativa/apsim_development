//---------------------------------------------------------------------------
#include <vcl\vcl.h>
#pragma hdrstop

#include "TGM_analysis.h"
#include <general\math_functions.h>
#include <general\vcl_functions.h>
#include <general\stl_functions.h>
#include "economics_tlb.h"
#include <assert.h>

#pragma package(smart_init)
#pragma resource "*.res"

//static const char* YIELD_FIELD_NAME = "Yield (kg/ha)";
static const char* GROSS_MARGIN_FIELD_NAME = "Gross Margin";
static const char* PROBABILITY_FIELD_NAME = "Probability";

//---------------------------------------------------------------------------
namespace Tgm_analysis
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
   : TAPSTable(Owner)
   {
   FSimulations = NULL;
   }


// ------------------------------------------------------------------
//  Short description:
//      go work out what the yield column is called.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
string TGM_analysis::GetYieldFieldName(TAPSRecord& Raw_data)
   {
   vector<string> FieldNames;
   Raw_data.Get_field_names (FieldNames);
   for (vector<string>::iterator i = FieldNames.begin();
                                 i != FieldNames.end();
                                 i++)
      {
      string FieldName = *i;
      if (Str_i_Eq(FieldName.substr(0,5), "yield"))
         return FieldName;
      }
   Application->MessageBox("Cannot find a yield column in your dataset.  "
                           "Cannot calculate any gross margins.",
                           "Error",
                           MB_ICONSTOP | MB_OK);
   return "";
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
   // setup an array of probabilities.
   vector<double> Probability;
   for (int prob = 0; prob <= 100; prob+=10)
      Probability.push_back(prob);

   // get an array of tabs.
   vector<string> Tabs;
   list<string> SimulationNames;
   FSimulations->Get_selected_simulation_names(SimulationNames);
   std::copy(SimulationNames.begin(), SimulationNames.end(), std::back_inserter(Tabs));

   vector<double> Yields;
   vector<string> Descriptors;
   vector<string> UniqueDescriptors;

   // goto first record in source dataset.
   vector<TAPSRecord> Raw_data;
   bool ok = Source_dataset->First(FPivot_values->Items);

   // loop through all pivot series.
   while (ok)
      {
      // get data.
      vector<TAPSRecord> New_data;
      Source_dataset->Get(Raw_data);

      // go work out the name of the yield column.
      string YieldFieldName = GetYieldFieldName(Raw_data[0]);

      // create and setup yields array.
      vector<double> LocalYields;
      Array_records_2_array_numbers(Raw_data, LocalYields, YieldFieldName.c_str());
      std::copy (LocalYields.begin(), LocalYields.end(), std::back_inserter(Yields));

      // add descriptors to descriptor array.
      string Descriptor = Raw_data[0].Get_title () + Raw_data[0].Get_pivot_value();
      std::fill_n(std::back_inserter(Descriptors), LocalYields.size(), Descriptor);
      UniqueDescriptors.push_back (Descriptor);

      // goto next pivot series.
      Raw_data.erase(Raw_data.begin(), Raw_data.end());
      ok = Source_dataset->Next();
      }

   VARIANT OleTabs;
   Strings_to_olevariant (Tabs, OleTabs);
   VARIANT OleDescriptors;
   Strings_to_olevariant (Descriptors, OleDescriptors);
   VARIANT OleYields;
   Doubles_to_olevariant (Yields, OleYields);
   VARIANT OleGrossMargins;
   OleVariantInit(OleGrossMargins, UniqueDescriptors.size() * 11, VT_R4);

   // call activex GM dll.
   TCOM_Gross_Margin gm = CoGross_Margin::Create();
   gm->econ_summary(&OleTabs, &OleDescriptors, &OleYields, &OleGrossMargins);

   // store all data from the probability and the grossmargins vectors into our recordset.
   Begin_storing_data();

   vector<TAPSRecord> NewRecords;
   Array_numbers_2_array_records(Probability, NewRecords, PROBABILITY_FIELD_NAME);
   vector<double> GrossMargins;
   Olevariant_to_doubles (OleGrossMargins, GrossMargins);
   Array_numbers_2_array_records(GrossMargins, NewRecords, GROSS_MARGIN_FIELD_NAME);
   ShowMessage(IntToStr(NewRecords.size()));
   ShowMessage(IntToStr(UniqueDescriptors.size()));
   assert (NewRecords.size() == UniqueDescriptors.size() * 11);
   for (unsigned int pivot = 0; pivot < UniqueDescriptors.size(); pivot++)
      {
      for (unsigned int prob = pivot*11; prob < (pivot+1)*11; prob++)
         NewRecords[prob].Set_base_descriptor(UniqueDescriptors[pivot].c_str());
      }
   Store_data (NewRecords);

   End_storing_data();

   // clean up our ole arrays
   SafeArrayDestroy(OleTabs.parray);
   SafeArrayDestroy(OleYields.parray);
   SafeArrayDestroy(OleDescriptors.parray);
   SafeArrayDestroy(OleGrossMargins.parray);
   }

// ------------------------------------------------------------------
//  Short description:
//      allow user to edit the component.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
bool TGM_analysis::Edit(void)
   {
   return true;
   }


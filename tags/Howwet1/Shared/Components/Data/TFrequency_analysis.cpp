//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TFrequency_analysis.h"
#include <general\math_functions.h>
#include <general\vcl_functions.h>
#include <ApsimShared\ApsimSettings.h>
#include "TFrequency_form.h"
#include <numeric>
#include <math.h>

#pragma link "TAPSTable"
#pragma package(smart_init)

//---------------------------------------------------------------------------
// ValidCtrCheck is used to assure that the components created do not have
// any pure virtual functions.
//

static inline void ValidCtrCheck(TFrequency_analysis *)
{
   new TFrequency_analysis(NULL);
}
//---------------------------------------------------------------------------
namespace Tfrequency_analysis
{
   void __fastcall PACKAGE Register()
   {
      TComponentClass classes[1] = {__classid(TFrequency_analysis)};
      RegisterComponents("APSRU", classes, 0);
   }
}
// ------------------------------------------------------------------
//  Short description:
//      constructor

//  Notes:
//

//  Changes:
//    DPH 15/7/98

// ------------------------------------------------------------------
__fastcall TFrequency_analysis::TFrequency_analysis(TComponent* Owner)
   : TAnalysis(Owner)
   {
   FCreate_categories = true;
   }
// ------------------------------------------------------------------
// Load settings.
// ------------------------------------------------------------------
void TFrequency_analysis::load()
   {
   TAnalysis::load();
   ApsimSettings settings;
   try
      {
      int numCategories;
      settings.read(CHART_SETTINGS_KEY + "|NumFrequencyCategories", numCategories);
      Num_categories = numCategories;
      settings.read(CHART_SETTINGS_KEY + "|FrequencyAnalysis", FFrequency_analysis);
      }
   catch (const exception& err)
      {
      FNum_categories = 6;
      FCategory_start_value[0] = 0;
      FCategory_end_value[0] = 200;
      FCategory_start_value[1] = 200;
      FCategory_end_value[1] = 400;
      FCategory_start_value[2] = 400;
      FCategory_end_value[2] = 600;
      FCategory_start_value[3] = 600;
      FCategory_end_value[3] = 800;
      FFrequency_analysis = true;
      }
   }

// ------------------------------------------------------------------
// Save settings.
// ------------------------------------------------------------------
void TFrequency_analysis::save()
   {
   TAnalysis::save();
   ApsimSettings settings;

   settings.write(CHART_SETTINGS_KEY + "|NumFrequencyCategories", Num_categories);
   settings.write(CHART_SETTINGS_KEY + "|FrequencyAnalysis", FFrequency_analysis);
   }

// ------------------------------------------------------------------
//  Short description:
//      get a specific starting value for a category.

//  Notes:

//  Changes:
//    DPH 15/7/98

// ------------------------------------------------------------------
double __fastcall TFrequency_analysis::Get_start_value (int index)
   {
   if (index >= 0 && index < MAX_CATEGORIES)
      return FCategory_start_value[index];
   else
      return 0;
   }

// ------------------------------------------------------------------
//  Short description:
//      set a specific starting value for a category.

//  Notes:

//  Changes:
//    DPH 15/7/98

// ------------------------------------------------------------------
void __fastcall TFrequency_analysis::Set_start_value (int index, double value)
   {
   FCreate_categories = false;
   if (index >= 0 && index < MAX_CATEGORIES)
      FCategory_start_value[index] = value;
   }

// ------------------------------------------------------------------
//  Short description:
//      get a specific ending value for a category.

//  Notes:

//  Changes:
//    DPH 15/7/98

// ------------------------------------------------------------------
double __fastcall TFrequency_analysis::Get_end_value (int index)
   {
   if (index >= 0 && index < MAX_CATEGORIES)
      return FCategory_end_value[index];
   else
      return 0;
   }

// ------------------------------------------------------------------
//  Short description:
//      set a specific ending value for a category.

//  Notes:

//  Changes:
//    DPH 15/7/98

// ------------------------------------------------------------------
void __fastcall TFrequency_analysis::Set_end_value (int index, double value)
   {
   FCreate_categories = false;
   if (index >= 0 && index < MAX_CATEGORIES)
      FCategory_end_value[index] = value;
   }

// ------------------------------------------------------------------
//  Short description:
//      set the number of categories to use.  This will be used
//      instead of the category values above.

//  Notes:

//  Changes:
//    DPH 15/7/98

// ------------------------------------------------------------------
void __fastcall TFrequency_analysis::Set_num_categories (int num_categories)
   {
   FNum_categories = num_categories;
   FCreate_categories = true;
   }

// ------------------------------------------------------------------
//  Short description:
//      calculate and store all records in memory table.

//  Notes:

//  Changes:
//    DPH 15/7/98
//    DAH 7/2/02: fix for D-408

// ------------------------------------------------------------------
void TFrequency_analysis::calcAndStoreRecords()
   {
   if (Field_names_to_analyse->Items->Count == 1)
      {
      beginStoringData();

      // create a list of field names to keep.
      string fieldName = Field_names_to_analyse->Items->Strings[0].c_str();

      // if we need to create categories then we'd better do it now.
      if (FCreate_categories)
         createCategories();

      first();

      // loop through all data blocks
      bool ok = sourceDataset->first();
      while (ok)
         {
         vector<double> values;
         sourceDataset->fieldAsNumericArray(fieldName, values);

         // calculate a frequency distribution.
         vector<string> frequencyLabels;
         vector<double> frequencyNumbers;
         calculateFreqDist(values, frequencyLabels, frequencyNumbers);

         storeStringArray(fieldName, frequencyLabels);
         if (Frequency_analysis)
            storeNumericArray(FREQUENCY_FIELD_NAME, frequencyNumbers);
         else
            storeNumericArray(PROBABILITY_FIELD_NAME, frequencyNumbers);

         copyPivotsFrom(*sourceDataset);

         next();
         ok = sourceDataset->next();
         }

      endStoringData();
      }
   }

// ------------------------------------------------------------------
//  Short description:
//      calculate the frequency distribution

//  Notes:

//  Changes:
//    DPH 15/7/98

// ------------------------------------------------------------------
void TFrequency_analysis::calculateFreqDist(vector<double>& Values,
                                            vector<string>& Frequency_labels,
                                            vector<double>& Frequency_numbers)
   {
   vector<double> Start_values, End_values;
   int category = 0;
   while (category < MAX_CATEGORIES && FCategory_end_value[category] > 0)
      {
      Start_values.push_back (FCategory_start_value[category]);
      End_values.push_back (FCategory_end_value[category]);
      category++;
      }

   ::Calculate_freq_dist (Values,
                          Start_values, End_values,
                          Frequency_labels,
                          Frequency_numbers,
                          0);
   if (!FFrequency_analysis)
      {
      // convert each frequency to a percentage from 0 to 1.
      double Total = std::accumulate(Frequency_numbers.begin(), Frequency_numbers.end(), 0.0);
      if (Total > 0.0)
         {
         for (unsigned int i = 0; i < Frequency_numbers.size(); i++)
            Frequency_numbers[i] = Frequency_numbers[i] / Total;
         }
      }
   }

// ------------------------------------------------------------------
//  Short description:
//      create the categories based on the values passed in.

//  Notes:

//  Changes:
//    DPH 5/2/98
//    DAH 31/1/02: D-497: the call to fieldAsNumericArray was erasing the vector 'values'
//                 on every pass through the while loop and hence not calculating
//                 max and min vals correctly

// ------------------------------------------------------------------
void TFrequency_analysis::createCategories (void)
   {
   string fieldName = Field_names_to_analyse->Items->Strings[0].c_str();

   vector<double> values, all_values;

   // get all data.
   bool ok = sourceDataset->first();
   while (ok)
      {
      sourceDataset->fieldAsNumericArray(fieldName, values);
      all_values.insert(all_values.end(), values.begin(), values.end());
      ok = sourceDataset->next();
      }

   double minimum = *std::min_element(all_values.begin(), all_values.end());
   double maximum = *std::max_element(all_values.begin(), all_values.end());

   double category_size = (maximum - minimum) / FNum_categories;
   int Magnitude = 0;
   if (category_size > 0)
      Magnitude = log10(category_size);
   int Nearest = pow(10, Magnitude);

   Round_to_nearest (minimum, Nearest, false);
   Round_to_nearest (maximum, Nearest, true);
   category_size = (maximum - minimum) / FNum_categories;

   int index;
   double start_category = minimum;
   for (index = 0; index < FNum_categories; index++)
      {
      FCategory_start_value[index] = start_category;
      FCategory_end_value[index] = start_category + category_size;
      start_category += category_size;
      }
   FCategory_start_value[index] = 0;
   FCategory_end_value[index] = 0;
   }

// ------------------------------------------------------------------
//  Short description:
//      create and return a pointer to an analysis form.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
TAPSTable_form* TFrequency_analysis::createPropertiesForm()
   {
   return new TFrequency_form(Application);
   }



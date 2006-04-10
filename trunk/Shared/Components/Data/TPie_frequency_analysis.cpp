//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TPie_frequency_analysis.h"
#include <general\math_functions.h>
#include <general\vcl_functions.h>
#include <general\string_functions.h>
#include <ApsimShared\ApsimSettings.h>
#include "TPie_frequency_form.h"
using namespace std;

#pragma link "TAPSTable"
#pragma package(smart_init)

//---------------------------------------------------------------------------
// ValidCtrCheck is used to assure that the components created do not have
// any pure virtual functions.
//

static inline void ValidCtrCheck(TPie_frequency_analysis *)
{
   new TPie_frequency_analysis(NULL);
}
//---------------------------------------------------------------------------
namespace tPie_frequency_analysis
{
   void __fastcall PACKAGE Register()
   {
      TComponentClass classes[1] = {__classid(TPie_frequency_analysis)};
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
__fastcall TPie_frequency_analysis::TPie_frequency_analysis(TComponent* Owner)
   : TFrequency_analysis(Owner)
   {
   }
// ------------------------------------------------------------------
// Load settings.
// ------------------------------------------------------------------
void TPie_frequency_analysis::load()
   {
   TAnalysis::load();
   ApsimSettings settings;
   Frequency_analysis = true;

   try
      {
      string st;
      settings.read(CHART_SETTINGS_KEY + "|BasePivotValue", st);
      FBaseDatasetName = st.c_str();

      settings.read(CHART_SETTINGS_KEY + "|SecondPivotValue", st);
      FSecondDatasetName = st.c_str();

      vector<string> categories;
      settings.read(CHART_SETTINGS_KEY + "|PieCategories", categories);
      for (unsigned i = 0; i != categories.size(); i++)
         FCategory_percentile[i] = atof(categories[i].c_str());

      int numEqualPercentiles;
      settings.read(CHART_SETTINGS_KEY + "|PieNumEqualPercentiles", numEqualPercentiles);
      Num_equal_percentiles = numEqualPercentiles;


      }
   catch (const exception& err)
      {
      FCategory_percentile[0] = 33.3;
      FCategory_percentile[1] = 66.6;
      FCategory_percentile[2] = 100;
      FCategory_percentile[3] = 0.0;
      }
   }

// ------------------------------------------------------------------
// Save settings.
// ------------------------------------------------------------------
void TPie_frequency_analysis::save()
   {
   TAnalysis::save();
   ApsimSettings settings;

   vector<string> categories;
   for (unsigned i = 0; i != MAX_CATEGORIES; i++)
      {
      if (FCategory_percentile[i] == 0.0)
         break;
      categories.push_back(ftoa(FCategory_percentile[i], 2));
      }
   settings.write(CHART_SETTINGS_KEY + "|PieCategories", categories);
   settings.write(CHART_SETTINGS_KEY + "|PieNumEqualPercentiles", Num_equal_percentiles);
   settings.write(CHART_SETTINGS_KEY + "|BasePivotValue", string(FBaseDatasetName.c_str()));
   settings.write(CHART_SETTINGS_KEY + "|SecondPivotValue", string(FSecondDatasetName.c_str()));
   }

// ------------------------------------------------------------------
//  Short description:
//      return a specified category percentile to caller.

//  Notes:

//  Changes:
//    DPH 15/7/98

// ------------------------------------------------------------------
double __fastcall TPie_frequency_analysis::Get_percentile (int index)
   {
   if (index >= 0 && index < MAX_CATEGORIES)
      return FCategory_percentile[index];
   else
      return 0;
   }

// ------------------------------------------------------------------
//  Short description:
//      set a specific category percentile

//  Notes:

//  Changes:
//    DPH 15/7/98

// ------------------------------------------------------------------
void __fastcall TPie_frequency_analysis::Set_percentile (int index, double percentile)
   {
   if (index >= 0 && index < MAX_CATEGORIES)
      FCategory_percentile[index] = percentile;
   }

// ------------------------------------------------------------------
//  Short description:
//      return the number of equal percentiles to caller.

//  Notes:

//  Changes:
//    DPH 15/7/98

// ------------------------------------------------------------------
int __fastcall TPie_frequency_analysis::Get_num_equal_percentiles (void)
   {
   int category = 0;
   while (category < MAX_CATEGORIES && FCategory_percentile[category] > 0)
      category++;
   return category;
   }
// ------------------------------------------------------------------
//  Short description:
//      set the number of equal percentiles.

//  Notes:

//  Changes:
//    DPH 15/7/98

// ------------------------------------------------------------------
void __fastcall TPie_frequency_analysis::Set_num_equal_percentiles (int num_percentiles)
   {
   if (num_percentiles > 0)
      {
      int category;
      for (category = 0; category < num_percentiles; category++)
         {
         FCategory_percentile[category] = 100.0 / num_percentiles;
         if (category > 0)
             FCategory_percentile[category] += FCategory_percentile[category-1];
         }
      FCategory_percentile[category] = 0.0;
      }
   }

// ------------------------------------------------------------------
//  Short description:
//      calculate the frequency distribution

//  Notes:

//  Changes:
//    DPH 15/7/98

// ------------------------------------------------------------------
void TPie_frequency_analysis::createCategories(void)
   {
   string fieldName = Field_names_to_analyse->Items->Strings[0].c_str();

   // get all data.
   vector<double> values;
   bool ok = sourceDataset->first();
   while (ok && values.size() == 0)
      {
      if (Str_i_Eq(sourceDataset->getDataBlockName(), BaseDatasetName.c_str()))
         sourceDataset->fieldAsNumericArray(fieldName, values);
      ok = sourceDataset->next();
      }
   if (values.size() > 0)
      {
      Category_start_value[0] = Calculate_percentile(values, false, 0.0);
      int category = 0;
      while (category < MAX_CATEGORIES && FCategory_percentile[category] > 0)
         {
         Category_end_value[category] = Calculate_percentile(values, false, FCategory_percentile[category]);
         if (FCategory_percentile[category+1] > 0)
            Category_start_value[category+1] = Category_end_value[category];
         category++;
         }
      Category_start_value[category] = 0.0;
      Category_end_value[category] = 0.0;
      }
   }

// ------------------------------------------------------------------
//  Short description:
//      allow user to edit the component.

//  Notes:

//  Changes:
//    DPH 15/7/98

// ------------------------------------------------------------------
TAPSTable_form* TPie_frequency_analysis::createPropertiesForm()
   {
   return new TPie_frequency_form(Application);
   }


//---------------------------------------------------------------------------
#pragma hdrstop
#include "RealSet.h"
#include "Kruskal_wallis.h"
#include <general\pch.h>
#include <vcl.h>

#include "TKWTest.h"
#include <general\db_functions.h>
#include <general\math_functions.h>
#include <general\string_functions.h>

using namespace std;
#pragma package(smart_init)
#pragma resource "ComponentRegistration.res"

static const double KRUSKAL_WALLIS_CRITICAL_VALUE = 0.1;

//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
__fastcall TKWTest::TKWTest(TComponent* owner)
   : TSEGTable(owner)
   {
   }

//---------------------------------------------------------------------------
// destructor
//---------------------------------------------------------------------------
__fastcall TKWTest::~TKWTest()
   {
   }
// ------------------------------------------------------------------
// The source2 dataset property has changed.  May need to do a
// refresh.
// ------------------------------------------------------------------
void __fastcall TKWTest::setSource2Dataset(TSEGTable* source)
   {
   if (source2 != source)
      {
      if (source2 != NULL)
         source2->removeDataChangeSubscription(Name.c_str());
      source2 = source;
      source2->addDataChangeSubscription(Name + ".onSourceDataChanged");
      forceRefresh(false);
      }
   }
//---------------------------------------------------------------------------
// Set the fieldName property.
//---------------------------------------------------------------------------
void __fastcall TKWTest::setFieldName(AnsiString fieldName)
   {
   if (fieldNameToAnalyse != fieldName)
      {
      fieldNameToAnalyse = fieldName;
      forceRefresh();
      }
   }
//---------------------------------------------------------------------------
// Called by our base class to allow us to add any fielddefs we may want to.
// The table will be closed (Active=false) when this routine is called.
//---------------------------------------------------------------------------
bool TKWTest::createFields(void) throw(runtime_error)
   {
   if (source != NULL && fieldNameToAnalyse != "")
      {
      FieldDefs->Clear();
      addDBField(this, "PValue", "1.0");
      addDBField(this, "Description", "a");
      addDBField(this, "AreNot", "a");
      return true;
      }
   return false;
   }

// ------------------------------------------------------------------
// Loop through all series in specified table and store the values
// as a new distribution in the specified vector of distributions.
// ------------------------------------------------------------------
void TKWTest::AddDistributionFromTable(TSEGTable* table, vector<RealSet>& distributions)
   {
   bool ok = table->firstSeries();
   while (ok)
      {
      vector<double> values;
      getDBFieldValues(table, fieldNameToAnalyse.c_str(), values);
      if (values.size() > 0)
         {
         RealSet distribution;
         for (unsigned i = 0; i < values.size(); i++)
            distribution.add(values[i]);
         distributions.push_back(distribution);
         }
      ok = table->nextSeries();
      }
   }

//---------------------------------------------------------------------------
// Called by our base class to allow us to add records to the table.
// The table is open (Active = true) when this routine is called.
//---------------------------------------------------------------------------
void TKWTest::storeRecords(void) throw(runtime_error)
   {
   if (source != NULL && source2 != NULL && fieldNameToAnalyse != "")
      {
      vector<RealSet> distributions;
      AddDistributionFromTable(source, distributions);
      AddDistributionFromTable(source2, distributions);

      if (distributions.size() == 2)
         {
         KruskalWallisResult result = KruskalWallis(distributions);
         double pValue = result.p;

         Append();
         FieldValues["PValue"] = pValue;
         if (pValue < KRUSKAL_WALLIS_CRITICAL_VALUE)
            {
            FieldValues["Description"] = "There is a significant difference between the distributions.";
            FieldValues["AreNot"] = "ARE";
            }
         else
            {
            FieldValues["Description"] = "There is NO significant difference between the distributions.";
            FieldValues["AreNot"] = "ARE NOT";
            }
         Post();
         }
      source->cancelSeries();
      }
   }
// ------------------------------------------------------------------
// set one of our properties.
// ------------------------------------------------------------------
void TKWTest::setProperty(const std::string& propertyName,
                               const std::string& propertyValue)
   {
   if (Str_i_Eq(propertyName, "fieldName"))
      fieldName = propertyValue.c_str();
   }


//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TAnalysis.h"
#include <general\vcl_functions.h>
#include <ApsimShared\ApsimSettings.h>
#include "TAnalysis_form.h"

#pragma link "TAPSTable"
#pragma package(smart_init)
using namespace std;
//---------------------------------------------------------------------------
// ValidCtrCheck is used to assure that the components created do not have
// any pure virtual functions.
//
                               
static inline void ValidCtrCheck(TAnalysis *)
{
   new TAnalysis(NULL);
}
//---------------------------------------------------------------------------
namespace Tanalysis
{
   void __fastcall PACKAGE Register()
   {
      TComponentClass classes[1] = {__classid(TAnalysis)};
      RegisterComponents("APSRU", classes, 0);
   }
}
// ------------------------------------------------------------------
//  Short description:
//     return a list of filter pivot values.

//  Notes:

//  Changes:
//    DPH 17/7/98

// ------------------------------------------------------------------
__fastcall TAnalysis::TAnalysis(TComponent* Owner)
   : TAPSTable(Owner)
   {
   FField_names_to_analyse = new TMultiStringList;
   }
// ------------------------------------------------------------------
//  Short description:
//      destructor

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
__fastcall TAnalysis::~TAnalysis()
   {
   delete FField_names_to_analyse;
   }
// ------------------------------------------------------------------
// Load settings
// ------------------------------------------------------------------
void TAnalysis::load()
   {
   TAPSTable::load();
   vector<string> selectedFieldNames;
   ApsimSettings settings;
   settings.read(CHART_SETTINGS_KEY + "|SelectedFieldName", selectedFieldNames);
   for (unsigned i = 0; i != selectedFieldNames.size(); i++)
      {
      if (selectedFieldNames[i].find(';') != string::npos)
         selectedFieldNames[i].erase(selectedFieldNames[i].find(';'));
      }
   Stl_2_tstrings(selectedFieldNames, Field_names_to_analyse->Items);
   }
// ------------------------------------------------------------------
// Save settings
// ------------------------------------------------------------------
void TAnalysis::save()
   {
   TAPSTable::save();
   vector<string> selectedFieldNames;
   TStrings_2_stl(Field_names_to_analyse->Items, selectedFieldNames);

   ApsimSettings settings;
   settings.write(CHART_SETTINGS_KEY + "|SelectedFieldName", selectedFieldNames);
   }

// ------------------------------------------------------------------
//  Short description:
//     return a list of filter pivot values.

//  Notes:

//  Changes:
//    DPH 17/7/98

// ------------------------------------------------------------------
TMultiStringList* __fastcall TAnalysis::Get_field_names_to_analyse (void)
   {
   if (sourceDataset != NULL)
      {
      vector<string> field_names;
      //sourceDataset->getFieldNames (field_names);
      sourceDataset->getFieldNamesMinusPivots (field_names);
      Stl_2_tstrings (field_names, FField_names_to_analyse->PossibleItems);
      }
   return FField_names_to_analyse;
   }

// ------------------------------------------------------------------
//  Short description:
//      create and return a pointer to an analysis form.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
TAPSTable_form* TAnalysis::Create_properties_form()
   {
   return new TAnalysis_form(Application);
   }

// ------------------------------------------------------------------
//  Short description:
//      setup the analysis form.

//  Notes:

//  Changes:
//    DPH 2/8/2001

// ------------------------------------------------------------------
void TAnalysis::setupPropertiesForm(TAPSTable_form* form)
   {
   TAPSTable::setupPropertiesForm(form);

   TAnalysis_form* analysisForm = dynamic_cast<TAnalysis_form*> (form);
   analysisForm->Analysis_ptr = this;
   }


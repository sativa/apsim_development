//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "WhopEcon.h"
#include "About.h"
#include "Crop.h"
#include "DataForm.h"
#include "Econ.h"
#include "WheatMatrix.h"
#include "TWEValueSelectionForm.h"
#include <general\path.h>
#include <general\ini_file.h>
#include <general\stl_functions.h>
#include <general\string_functions.h>
#include <sstream>
#include "SeedWeight.h"
#include "CropFields.h"
#include "EconConfigData.h"

//---------------------------------------------------------------------------

#pragma package(smart_init)
//---------------------------------------------------------------------------
#define WHOPECON_SECTION "WhopEcon"
#define WHOPECON_FACTOR_NAME "Econ Config"
#define ECON_DB_NAME "Econ Database"
#define BITMAP_NAME_KEY "bitmap"
#define SIMULATION_FACTOR_NAME "Simulation"
#define WHOPECON_FIELDS "Crops"
// ------------------------------------------------------------------
//  Short description:
//    Exported function for created an instance of this add-in

//  Changes:
//    DAH 29/8/01 created

// ------------------------------------------------------------------
extern "C" AddInBase* _export __stdcall createAddIn(const string& parameters, bool& success)
   {
   // will be called with begin
   // and end years from the database
   success = true; // this line may require more thought later, ie, success should really
                   // be indicated by the WhopEcon constructor
   return new WhopEcon(parameters);
   }


// ------------------------------------------------------------------
//  Short description:
//      constructor

//  Notes:

//  Changes:
//    DAH 29/08/01   created

// ------------------------------------------------------------------
WhopEcon::WhopEcon(const string& parameters)
   {
   static bool firstTime = true;
   if (firstTime)
      {
      DATA = new TDATA(Application->MainForm);
      EconForm = new TEconForm(Application->MainForm);
      CropForm = new TCropForm(Application->MainForm);
      AboutBox = new TAboutBox(Application->MainForm);
      WheatMatrixForm = new TWheatMatrixForm(Application->MainForm);
      SeedWeightsForm = new TSeedWeightsForm(Application->MainForm);
      firstTime = false;
      }

   // handle parameter string

   // read settings from ini
   Read_inifile_settings();

   Path bitmap_path(Application->ExeName.c_str());
   bitmap_path.Set_name (Econ_bitmap_name.c_str());
   Graphics::TBitmap* bitmap = new Graphics::TBitmap;
   bitmap->LoadFromFile(bitmap_path.Get_path().c_str());

   // get a default econ config name
   AnsiString default_config_name;
   EconForm->OpenEconDB();
   DATA->Scenario->First();
   if (!DATA->Scenario->Eof)
      default_config_name =  DATA->Scenario->FieldValues["ScenarioName"];
   else
      default_config_name = "Empty";
   EconForm->CloseEconDB();

   // create a factor with the default name
   Factor econ(bitmap,WHOPECON_FACTOR_NAME,default_config_name.c_str(),this);
   factors.push_back(econ);
   }

WhopEcon::~WhopEcon(void)
   {
   delete DATA;
   DATA = NULL;
   delete EconForm;
   EconForm = NULL;
   delete CropForm;
   CropForm = NULL;
   delete AboutBox;
   AboutBox = NULL;
   delete WheatMatrixForm;
   WheatMatrixForm = NULL;
   delete SeedWeightsForm;
   SeedWeightsForm = NULL;
   }


void WhopEcon::makeScenarioValid(Scenario& scenario,
                                     const std::string factor_name) const {}


Scenario WhopEcon::getDefaultScenario(void) const {
   // check the following to see if "" is the right thing to pass in.
   return Scenario("",factors);
}


void WhopEcon::getFactorValues(const Scenario& scenario,
                                   const std::string& factorName,
                                   std::vector<std::string>& factorValues) const
{
   TListItems* config_names;
   EconForm->OpenEconDB();
   DATA->Scenario->First();
   while (!DATA->Scenario->Eof)
   {
      AnsiString name = DATA->Scenario->FieldValues["ScenarioName"];
      factorValues.push_back(name.c_str());
      DATA->Scenario->Next();
   }
   EconForm->CloseEconDB();
}


TValueSelectionForm*  WhopEcon::getUIForm(const string& factorName,
                                                             TComponent* Owner) const{
   if ( WEValueSelectionForm == NULL)
      WEValueSelectionForm = new TWEValueSelectionForm(Owner);

   return  WEValueSelectionForm;
}


// ------------------------------------------------------------------
//  Short description:
//      read all defaults from .ini file.

//  Notes:

//  Changes:
//    DAH 29/8/01 :   Created

// ------------------------------------------------------------------
void WhopEcon::Read_inifile_settings (void)
{
   Path p(Application->ExeName.c_str());
   p.Set_extension(".ini");
   Ini_file ini;
   ini.Set_file_name (p.Get_path().c_str());

   // read all defaults.
   string st;
   ini.Read (WHOPECON_SECTION, BITMAP_NAME_KEY, st);
   Econ_bitmap_name = st;
   ini.Read (WHOPECON_SECTION, ECON_DB_NAME, st);
   Econ_DB_name = p.Get_directory() +  "\\"  +  st;
   EconForm->DBFileName = Econ_DB_name.c_str();

}


// ------------------------------------------------------------------
//  Short description:
//      calculate and store all records in memory table.

//  Notes:

//  Created:   DAH   7/9/01   Adapted from G. McLean's WhopEcon and Dameasy
//                            doCalculations routine
// ------------------------------------------------------------------
void WhopEcon::doCalculations(TAPSTable& data,
                              const vector<Scenario*>& selectedScenarios)
   {
   TCursor savedCursor = Screen->Cursor;
   Screen->Cursor = crHourGlass;

   // Flag for determining if we need to tell 'data' about the new fields
   // we're about to create.
   bool haveInformedDataOfNewFields = false;

   // Comments displayed at end of this routine.  Used for warning messages.
   warnings.erase(warnings.begin(), warnings.end());

   // go thru the table 'data' and add a new factor field to reflect the
   // economic configuration
   data.markFieldAsAPivot(WHOPECON_FACTOR_NAME);

   // create an instance of our economic configuration class.
   EconConfigData econConfig;

   // open the relevant tables and convert the economic configuration name
   // into an index.
   EconForm->OpenEconDB();
   DATA->CropList->Open();

   bool ok = data.first();

   while (ok)
      {
      string econConfigName;
      // find the corresponding Scenario and the economic configuration name
      // that is used for this data block.
      string rec_name = data.begin()->getFieldValue(SIMULATION_FACTOR_NAME);
      vector<Scenario*>::const_iterator scenarioI =
            find_if(selectedScenarios.begin(),selectedScenarios.end(),
                    PEqualToName<Scenario>(rec_name));
      Graphics::TBitmap* bitmap;
      (*scenarioI)->getFactorAttributes(WHOPECON_FACTOR_NAME, econConfigName, bitmap);

      // get a list of crops that have variables on the current record.
      CropFields cropFields(data.begin());

      // dph - need to remove const record iterators.
      typedef vector<TAPSRecord>::iterator RecordsIterator;
      for (RecordsIterator record = const_cast<RecordsIterator> (data.begin());
                           record != const_cast<RecordsIterator> (data.end());
                           record++)
         {
         // create a new column for the configuration name.
         record->setFieldValue(WHOPECON_FACTOR_NAME, econConfigName);

         // go get a list of crop acronyms.
         vector<string> cropAcronyms;
         cropFields.getCropAcronyms(*record, cropAcronyms);

         // Loop through all crops on this record and calculate a total
         // cost and return.
         float gmCost = 0.0;
         float gmReturn = 0.0;
         for (vector<string>::iterator cropAcronymI = cropAcronyms.begin();
                                       cropAcronymI != cropAcronyms.end();
                                       cropAcronymI++)
            {
            if (cropFields.cropWasSownByAcronym(*record, *cropAcronymI))
               {
               string cropName = cropFields.realCropName(*cropAcronymI).c_str();
               EconConfigCropData* cropData;
               if (econConfig.getCropData(econConfigName, cropName, cropData))
                  {
                  // if crop is wheat then get protein.
                  float protein = 0.0;
                  if (Str_i_Eq(cropName, "Wheat"))
                     {
                     if (!cropFields.getCropValue(*record, "protein", *cropAcronymI, protein))
                        {
                        addWarning("Cannot find a wheat protein column.");
                        continue;
                        }
                     }

                  // get yield from file, change yield in file from a WET weight
                  // to a DRY weight and add a new WET weight column.
                  string yieldFieldName = cropFields.getCropFieldName
                     (*record, "yield", *cropAcronymI);
                  string wetYieldFieldName = yieldFieldName + "WET";

                  float yield = 0.0;
                  if (!cropFields.getCropValue(*record, "yield", *cropAcronymI, yield))
                     addWarning("Cannot find a yield column for crop: " + cropName);
                  record->setFieldValue(wetYieldFieldName, FloatToStr(yield).c_str());
                  yield = cropData->calculateDryYield(yield);
                  record->setFieldValue(yieldFieldName, FloatToStr(yield).c_str());

                  // Calculate a return for this crop and store it as a new field
                  // for this crop.
                  float ret = cropData->calculateReturn(yield, protein);
                  string returnFieldName = "Return-" + cropName + " ($/ha)";
                  record->setFieldValue(returnFieldName, FloatToStr(ret).c_str());
                  gmReturn += ret;

                  // get an nrate, a planting rate and calculate cost in $/ha
                  float nitrogenRate = 0.0;
                  if (!cropFields.getCropValue(*record, "NRate", *cropAcronymI, nitrogenRate))
                     addWarning("Cannot find a nitrogen rate column for crop: " + cropName);
                  float plantingRate = 0.0;
                  if (!cropFields.getCropValue(*record, "PlantRate", *cropAcronymI, plantingRate))
                     addWarning("Cannot find a planting rate column for crop: " + cropName);

                  gmCost += cropData->calculateCost(nitrogenRate, plantingRate);

                  // tell 'data' about the new crop fields.
                  if (!haveInformedDataOfNewFields)
                     {
                     data.addField(wetYieldFieldName);
                     data.addField(returnFieldName);
                     }
                  }
               else
                  {
                  addWarning("No gross margin information has been specified for crop: " + cropName);
                  continue;
                  }
               }
            }

         // create new columns for cost, return, gm and gm split by crop
         record->setFieldValue("Cost ($/ha)", FloatToStr(gmCost).c_str());
         record->setFieldValue("Return ($/ha)", FloatToStr(gmReturn).c_str());
         record->setFieldValue("GM ($/ha)", FloatToStr(gmReturn - gmCost).c_str());

         // tell 'data' about the new GM fields.
         if (!haveInformedDataOfNewFields)
            {
            data.addField("Cost ($/ha)");
            data.addField("Return ($/ha)");
            data.addField("GM ($/ha)");
            haveInformedDataOfNewFields = true;
            }
         }
      ok = data.next();
      }

   data.endStoringData();
   EconForm->CloseEconDB();
   Screen->Cursor = savedCursor;

   if (warnings.size() > 0)
      {
      ostringstream messageStream;
      ostream_iterator<string, char> out(messageStream, "\n");
      copy(warnings.begin(), warnings.end(), out);
      Application->MessageBox(messageStream.str().c_str(), "Gross margin warnings...",
                              MB_ICONINFORMATION | MB_OK);
      }
   }

//---------------------------------------------------------------------------
// Make sure a warning is added if it doesn't alread exist.
//---------------------------------------------------------------------------
void WhopEcon::addWarning(const string& msg)
   {
   warnings.insert(msg.c_str());
   }



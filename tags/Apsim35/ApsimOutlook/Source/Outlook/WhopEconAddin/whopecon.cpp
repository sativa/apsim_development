//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "WhopEcon.h"
#include <general\path.h>
#include <general\stl_functions.h>
#include <general\string_functions.h>
#include <sstream>
#include "CropFields.h"
#include <iterator>
#include <ApsimShared\ApsimDirectories.h>
#include <ApsimShared\ApsimSettings.h>
#include <general\db_functions.h>
//---------------------------------------------------------------------------

#pragma package(smart_init)
//---------------------------------------------------------------------------
#define MDB_KEY "Economics|Database"
#define WHOPECON_FACTOR_NAME "Econ Config"
#define BITMAP_NAME_KEY "WhopEcon|bitmap"
#define SIMULATION_FACTOR_NAME "Simulation"
#define WHOPECON_FIELDS "Crops"
#define NO_ECONOMICS_NAME "(no economics)"
// ------------------------------------------------------------------
// Exported function for created an instance of this add-in
// ------------------------------------------------------------------
extern "C" AddInBase* _export __stdcall createAddIn()
   {
   return new WhopEcon();
   }
// ------------------------------------------------------------------
// Exported function to delete the specified addin.
// ------------------------------------------------------------------
extern "C" void _export __stdcall deleteAddIn(AddInBase* addin)
   {
   delete addin;
   }


// ------------------------------------------------------------------
// set any startup parameters.
// ------------------------------------------------------------------
void WhopEcon::setStartupParameters(const std::string& parameters)
   {
   SetCurrentDir(getAppHomeDirectory().c_str());
   
   // open the last economics data base.
   ApsimSettings settings;
   settings.read(MDB_KEY, econMDB, true);
   gm.open(econMDB);

   // create a factor with the default name
   Factor econ(WHOPECON_FACTOR_NAME, NO_ECONOMICS_NAME);
   factors.push_back(econ);
   }
// ------------------------------------------------------------------
// destructor
// ------------------------------------------------------------------
WhopEcon::~WhopEcon(void)
   {
   ApsimSettings settings;
   settings.write(MDB_KEY, econMDB);
   }
// ------------------------------------------------------------------
// return true if the simulation is valid.  False otherwise.
// ------------------------------------------------------------------
bool WhopEcon::isScenarioValid(Scenario& scenario) const
   {
   string econName = scenario.getFactorValue(WHOPECON_FACTOR_NAME);

   vector<string> scenarioNames;
   gm.getScenarioNames(scenarioNames);
   return (find(scenarioNames.begin(), scenarioNames.end(), econName) != scenarioNames.end());
   }
// ------------------------------------------------------------------
// Make the specified scenario valid.
// ------------------------------------------------------------------
void WhopEcon::makeScenarioValid(Scenario& scenario,
                                     const std::string factor_name) const
   { }
// ------------------------------------------------------------------
// Return a default scenario.
// ------------------------------------------------------------------
Scenario WhopEcon::getDefaultScenario(void) const
   {
   // check the following to see if "" is the right thing to pass in.
   return Scenario("",factors);
   }
// ------------------------------------------------------------------
// Return factor values for specified factor name to caller.
// ------------------------------------------------------------------
void WhopEcon::getFactorValues(const Scenario& scenario,
                                   const std::string& factorName,
                                   std::vector<std::string>& factorValues) const
   {
   if (factorName == WHOPECON_FACTOR_NAME)
      {
      factorValues.push_back(NO_ECONOMICS_NAME);
      gm.getScenarioNames(factorValues);
      }
   }
// ------------------------------------------------------------------
// display our form.
// ------------------------------------------------------------------
void WhopEcon::showUI(void)
   {
   gm.close();
   showEconomicScenariosUI(econMDB);
   gm.open(econMDB);
   }
// ------------------------------------------------------------------
// calculate and store all records in memory table.
// ------------------------------------------------------------------
void WhopEcon::doCalculations(TAPSTable& data, const Scenario& scenario)
   {
   TCursor savedCursor = Screen->Cursor;
   Screen->Cursor = crHourGlass;

   // Flag for determining if we need to tell 'data' about the new fields
   // we're about to create.
   bool haveInformedDataOfNewFields = false;

   // go thru the table 'data' and add a new factor field to reflect the
   // economic configuration
   data.markFieldAsAPivot(WHOPECON_FACTOR_NAME);

   bool ok = data.first();

   while (ok)
      {
      // find the corresponding Scenario and the economic configuration name
      // that is used for this data block.
      string econConfigName = scenario.getFactorValue(WHOPECON_FACTOR_NAME);
      if (econConfigName != NO_ECONOMICS_NAME)
         {
         // get a list of crops that have variables on the current record.
         CropFields cropFields(data.begin());

         // dph - need to remove const record iterators.
         typedef vector<TAPSRecord>::iterator RecordsIterator;
         for (RecordsIterator record = const_cast<RecordsIterator> (data.begin());
                              record != const_cast<RecordsIterator> (data.end());
                              record++)
            {
            string rec_name = record->getFieldValue(SIMULATION_FACTOR_NAME);
            unsigned posEconFactorName = rec_name.find(string(WHOPECON_FACTOR_NAME) + "=");
            string recordEconName;
            if (posEconFactorName != string::npos)
               {
               recordEconName = rec_name.substr(posEconFactorName+strlen(WHOPECON_FACTOR_NAME)+1);
               unsigned posSemiColon = recordEconName.find(';');
               if (posSemiColon != string::npos)
                  recordEconName.erase(posSemiColon);
               }
            if (recordEconName != "" && recordEconName != econConfigName)
               break;

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
                  try
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

                     // get yield from file and add a new WET weight column.
                     string yieldFieldName = cropFields.getCropFieldName
                        (*record, "yield", *cropAcronymI);
                     string wetYieldFieldName = yieldFieldName + "WET";

                     float dryYield = 0.0;
                     if (!cropFields.getCropValue(*record, "yield", *cropAcronymI, dryYield))
                        addWarning("Cannot find a yield column for crop: " + cropName);
                     gm.adjustYieldForHarvestLoss(econConfigName, cropName, dryYield);
                     float wetYield = gm.calculateWetYield(econConfigName, cropName, dryYield);
                     record->setFieldValue(wetYieldFieldName, FloatToStr(wetYield).c_str());

                     // Calculate a return for this crop and store it as a new field
                     // for this crop.
                     float ret = gm.calculateReturn(econConfigName, cropName, wetYield, protein);
                     string returnFieldName = *cropAcronymI + "_Return($ per ha)";
                     record->setFieldValue(returnFieldName, FloatToStr(ret).c_str());
                     gmReturn += ret;

                     // get an nrate, a planting rate and calculate cost in $/ha
                     float nitrogenRate = 0.0;
                     if (!cropFields.getCropValue(*record, "NRate", *cropAcronymI, nitrogenRate))
                        addWarning("Cannot find a nitrogen rate column for crop: " + cropName);
                     float plantingRate = 0.0;
                     if (!cropFields.getCropValue(*record, "PlantRate", *cropAcronymI, plantingRate))
                        addWarning("Cannot find a planting rate column for crop: " + cropName);

                     gmCost += gm.calculateCost(econConfigName, cropName, nitrogenRate, plantingRate, wetYield);

                     // tell 'data' about the new crop fields.
                     data.addField(wetYieldFieldName);
                     data.addField(returnFieldName);
                     }
                  catch (const runtime_error& err)
                     {
                     addWarning(err.what());
                     continue;
                     }
                  }
               }

            // create new columns for cost, return, gm and gm split by crop
            record->setFieldValue("Cost($ per ha)", FloatToStr(gmCost).c_str());
            record->setFieldValue("Return($ per ha)", FloatToStr(gmReturn).c_str());
            record->setFieldValue("GM($ per ha)", FloatToStr(gmReturn - gmCost).c_str());

            // tell 'data' about the new GM fields.
            if (!haveInformedDataOfNewFields)
               {
               data.addField("Cost($ per ha)");
               data.addField("Return($ per ha)");
               data.addField("GM($ per ha)");
               haveInformedDataOfNewFields = true;
               }
            }
         }
      ok = data.next();
      }
   data.markFieldAsAPivot(WHOPECON_FACTOR_NAME);

   Screen->Cursor = savedCursor;
   }

//---------------------------------------------------------------------------
// Make sure a warning is added if it doesn't alread exist.
//---------------------------------------------------------------------------
void WhopEcon::addWarning(const string& msg)
   {
   warnings.insert(msg.c_str());
   }

//---------------------------------------------------------------------------
// gives the add-in the chance to return any information to the
// 'displaySettings' window e.g. warnings, errors etc.
//---------------------------------------------------------------------------
string WhopEcon::getDisplaySettings(void)
   {
   string returnString;

   if (warnings.size() > 0)
      {
      ostringstream messageStream;
      ostream_iterator<string, char> out(messageStream, "\r\n");
      copy(warnings.begin(), warnings.end(), out);
      returnString = messageStream.str();
      }

   // Comments displayed at end of this routine.  Used for warning messages.
   warnings.erase(warnings.begin(), warnings.end());

   return returnString;
   }


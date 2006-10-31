//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "GMCalculatorMDB.h"
#include <ADODB.hpp>
#include <Db.hpp>
#include <general\string_functions.h>
#include <general\db_functions.h>
#include <general\math_functions.h>
#include "TGMForm.h"

#pragma package(smart_init)
typedef enum ADCPROP_UPDATERESYNC_ENUM
{
  adResyncNone = 0,
  adResyncAutoIncrement = 1,
  adResyncConflicts = 2,
  adResyncUpdates = 4,
  adResyncInserts = 8,
  adResyncAll = 15
} ADCPROP_UPDATERESYNC_ENUM;

//---------------------------------------------------------------------------
// constructor.
//---------------------------------------------------------------------------
GMCalculatorMDB::GMCalculatorMDB(void)
   {
   db = new TADOConnection(NULL);
   db->LoginPrompt = false;

   scenarioTable = new TADOTable(NULL);
   cropTable = new TADOTable(NULL);
   unitCostsTable = new TADOTable(NULL);
   cropListTable = new TADOTable(NULL);
   wheatMatrixTable = new TADOTable(NULL);

   scenarioTable->Connection = db;
   scenarioTable->CursorLocation == clUseServer;
   scenarioTable->TableName = "Scenario";

   cropTable->Connection = db;
   cropTable->CursorLocation == clUseServer;
   cropTable->TableName = "Crop";

   unitCostsTable->Connection = db;
   unitCostsTable->CursorLocation == clUseServer;
   unitCostsTable->TableName = "UnitCosts";

   cropListTable->Connection = db;
   cropListTable->CursorLocation == clUseServer;
   cropListTable->TableName = "CropList";

   wheatMatrixTable->Connection = db;
   wheatMatrixTable->CursorLocation == clUseServer;
   wheatMatrixTable->TableName = "WheatMatrix";
   }
//---------------------------------------------------------------------------
// destructor.
//---------------------------------------------------------------------------
GMCalculatorMDB::~GMCalculatorMDB(void)
   {
   delete db;
   delete scenarioTable;
   delete cropTable;
   delete unitCostsTable;
   delete cropListTable;
   delete wheatMatrixTable;
   }
//---------------------------------------------------------------------------
// Open the specified database file.
//---------------------------------------------------------------------------
void GMCalculatorMDB::open(const string& filename)
   {
   fileName = filename;

   // Open all tables.
   string connectionString = "Provider=Microsoft.Jet.OLEDB.4.0;"
                             "Data Source=?;"
                             "Persist Security Info=False";
   Replace_all(connectionString, "?", fileName.c_str());
   db->Connected = false;
   db->ConnectionString = connectionString.c_str();
   db->Connected = true;

   scenarioTable->Active = true;
   cropTable->Active = true;
   unitCostsTable->Active = true;
   cropListTable->Active = true;
   wheatMatrixTable->Active = true;

   scenarioTable->Recordset->Properties->Item[WideString("Update Resync")]->Value = adResyncAutoIncrement + adResyncInserts;
   cropTable->Recordset->Properties->Item[WideString("Update Resync")]->Value = adResyncAutoIncrement + adResyncInserts;
   unitCostsTable->Recordset->Properties->Item[WideString("Update Resync")]->Value = adResyncAutoIncrement + adResyncInserts;
   cropListTable->Recordset->Properties->Item[WideString("Update Resync")]->Value = adResyncAutoIncrement + adResyncInserts;
   wheatMatrixTable->Recordset->Properties->Item[WideString("Update Resync")]->Value = adResyncAutoIncrement + adResyncInserts;

   checkDatabaseConversion();
   }
//---------------------------------------------------------------------------
// Close the database file.
//---------------------------------------------------------------------------
void GMCalculatorMDB::close(void)
   {
   db->Connected = false;
   }
//---------------------------------------------------------------------------
// check to see if we need to convert database to new format.
//---------------------------------------------------------------------------
void GMCalculatorMDB::checkDatabaseConversion(void)
   {
   bool doConversion = true;

   // see if the areaCosts table exists.
   TADOTable* areaCostsTable = new TADOTable(NULL);
   areaCostsTable->Connection = db;
   areaCostsTable->TableName = "AreaCosts";
   try
      {
      areaCostsTable->Active = true;
      }
   catch (const Exception& err)
      {
      doConversion = false;
      }

   delete areaCostsTable;
   if (doConversion)
      convertDatabase();
   }
//---------------------------------------------------------------------------
// convert database to new format.
//---------------------------------------------------------------------------
void GMCalculatorMDB::convertDatabase(void)
   {
   // go through the areacosts table and move data to unitcosts table.
   TADOTable* areaCostsTable = new TADOTable(NULL);
   areaCostsTable->Connection = db;
   areaCostsTable->TableName = "AreaCosts";
   areaCostsTable->Active = true;
   while (!areaCostsTable->Eof)
      {
      unitCostsTable->Append();
      unitCostsTable->FieldValues["CropIndex"] = areaCostsTable->FieldValues["CropIndex"];
      unitCostsTable->FieldValues["Operation"] = areaCostsTable->FieldValues["Operation"];
      unitCostsTable->FieldValues["OperationCost"] = areaCostsTable->FieldValues["OperationCost"];
      unitCostsTable->FieldValues["ProductCost"] = areaCostsTable->FieldValues["ProductCost"];
      unitCostsTable->FieldValues["ProductUnits"] = "kg";
      unitCostsTable->FieldValues["ProductRate"] = 1.0;
      unitCostsTable->Post();
      areaCostsTable->Next();
      }

   // go through the cropcosts table and move data to unitcosts table.
   TADOTable* cropCostsTable = new TADOTable(NULL);
   cropCostsTable->Connection = db;
   cropCostsTable->TableName = "CropCosts";
   cropCostsTable->Active = true;
   while (!cropCostsTable->Eof)
      {
      unitCostsTable->Append();
      unitCostsTable->FieldValues["CropIndex"] = cropCostsTable->FieldValues["CropIndex"];
      unitCostsTable->FieldValues["Operation"] = cropCostsTable->FieldValues["Operation"];
      unitCostsTable->FieldValues["OperationCost"] = cropCostsTable->FieldValues["OperationCost"];
      unitCostsTable->FieldValues["ProductCost"] = 0.0;
      unitCostsTable->FieldValues["ProductUnits"] = "kg";
      unitCostsTable->FieldValues["ProductRate"] = 0.0;
      unitCostsTable->Post();
      cropCostsTable->Next();
      }

   // go through the crop table and move levy and freight to the unit costs table.
   cropTable->First();
   while (!cropTable->Eof)
      {
      static const char* fieldNamesToMove[2]= {"Levy", "Freight"};
      for (unsigned i = 0; i != 2; i++)
         {
         double value = getDBDouble(cropTable, fieldNamesToMove[i]);
         if (value > 0)
            {
            unitCostsTable->Append();
            unitCostsTable->FieldValues["CropIndex"] = cropTable->FieldValues["CropIndex"];
            unitCostsTable->FieldValues["Operation"] = fieldNamesToMove[i];
            unitCostsTable->FieldValues["OperationCost"] = 0;
            unitCostsTable->FieldValues["ProductCost"] = value;
            unitCostsTable->FieldValues["ProductUnits"] = "tonne";
            unitCostsTable->FieldValues["ProductRate"] = 0;
            unitCostsTable->Post();
            }
         }
      cropTable->Next();
      }

   delete areaCostsTable;
   delete cropCostsTable;

   TADOQuery* query = new TADOQuery(NULL);
   query->Connection = db;
   query->SQL->Text = "DROP TABLE AreaCosts";
   query->ExecSQL();
   query->SQL->Text = "DROP TABLE CropCosts";
   query->ExecSQL();
   query->SQL->Text = "ALTER TABLE UnitCosts Add COLUMN CostType integer";
   query->ExecSQL();
   query->SQL->Text = "ALTER TABLE Crop Drop COLUMN Levy";
   query->ExecSQL();
   query->SQL->Text = "ALTER TABLE Crop Drop COLUMN Freight";
   query->ExecSQL();

   // Go through the UnitCosts table and look for 'Sowing *' and
   // 'Nitrogen Application *' in operation field.  When found
   // get rid of the * and set the RateInterp field appropriately.
   unitCostsTable->Active = false;
   unitCostsTable->Active = true;
   cropTable->Active = false;
   cropTable->Active = true;
   unitCostsTable->First();
   while (!unitCostsTable->Eof)
      {
      unitCostsTable->Edit();
      AnsiString operationName = unitCostsTable->FieldValues["operation"];
      if (operationName.AnsiCompareIC("Sowing *") == 0)
         {
         unitCostsTable->FieldValues["operation"] = "Sowing";
         unitCostsTable->FieldValues["CostType"] = 2;
         }
      else if (operationName.AnsiCompareIC("Nitrogen Application *") == 0)
         {
         unitCostsTable->FieldValues["operation"] = "Nitrogen Application";
         unitCostsTable->FieldValues["CostType"] = 3;
         }
      else if (operationName.AnsiCompareIC("Levy") == 0)
         unitCostsTable->FieldValues["CostType"] = 4;
      else if (operationName.AnsiCompareIC("Freight") == 0)
         unitCostsTable->FieldValues["CostType"] = 4;
      else
         unitCostsTable->FieldValues["CostType"] = 1;

      if (unitCostsTable->FieldValues["ProductUnits"] == AnsiString("l"))
         unitCostsTable->FieldValues["ProductUnits"] = "litre";

      unitCostsTable->Post();
      unitCostsTable->Next();
      }
   }
//---------------------------------------------------------------------------
// Return a complete list of available crops to caller.
//---------------------------------------------------------------------------
void GMCalculatorMDB::getPossibleCrops(std::vector<string>& cropNames)
   {
   getDBFieldValues(cropListTable, "CropName", cropNames);
   sort(cropNames.begin(), cropNames.end());
   }
//---------------------------------------------------------------------------
// Return a complete list of operation names that have been used before.
//---------------------------------------------------------------------------
void GMCalculatorMDB::getPreviousOperations(std::vector<string>& operationNames)
   {
   getDBFieldValues(unitCostsTable, "Operation", operationNames);
   sort(operationNames.begin(), operationNames.end());
   operationNames.erase(unique(operationNames.begin(), operationNames.end()),
                        operationNames.end());
   }
//---------------------------------------------------------------------------
// Locate a scenario.  Return true if found.
//---------------------------------------------------------------------------
bool GMCalculatorMDB::locateScenario(const string& scenarioName) const
   {
   Variant nameVariant = Variant(scenarioName.c_str());
   return scenarioTable->Locate("ScenarioName", nameVariant, TLocateOptions());
   }
//---------------------------------------------------------------------------
// Return a list of all scenarios.
//---------------------------------------------------------------------------
void GMCalculatorMDB::getScenarioNames(std::vector<string>& scenarioNames) const
   {
   getDBFieldValues(scenarioTable, "ScenarioName", scenarioNames);
   }
//---------------------------------------------------------------------------
// Add a scenario.  Returns true if added.
//---------------------------------------------------------------------------
bool GMCalculatorMDB::addScenario(const std::string& scenarioName)
   {
   if (!locateScenario(scenarioName))
      {
      scenarioTable->Append();
      scenarioTable->FieldValues["ScenarioName"] = scenarioName.c_str();
      scenarioTable->Post();
      return true;
      }
   return false;
   }
//---------------------------------------------------------------------------
// Delete the specified scenario.
//---------------------------------------------------------------------------
bool GMCalculatorMDB::deleteScenario(const std::string& scenarioName)
   {
   if (locateScenario(scenarioName))
      {
      scenarioTable->Delete();
      return true;
      }
   return false;
   }
//---------------------------------------------------------------------------
// Rename a scenario.
//---------------------------------------------------------------------------
bool GMCalculatorMDB::renameScenario(const std::string& oldName,
                                  const std::string& newName)
   {
   if (locateScenario(oldName))
      {
      scenarioTable->Edit();
      scenarioTable->FieldValues["ScenarioName"] = newName.c_str();
      scenarioTable->Post();
      return true;
      }
   return false;
   }
//---------------------------------------------------------------------------
// Return a list of crops for specified scenario.
//---------------------------------------------------------------------------
void GMCalculatorMDB::getCropsInScenario(const string& scenarioName,
                                      std::vector<std::string>& cropNames)
   {
   if (locateScenario(scenarioName))
      {
      cropTable->Filter = "ScenarioIndex = " + scenarioTable->FieldValues["scenarioindex"];
      cropTable->Filtered = true;
      getDBFieldValues(cropTable, "CropName", cropNames);
      cropTable->Filtered = false;
      }
   }
//---------------------------------------------------------------------------
// Locate a crop for a specified scenario.  Return true if found.
//---------------------------------------------------------------------------
bool GMCalculatorMDB::locateCrop(const string& scenarioName, const string& cropName) const
   {
   if (locateScenario(scenarioName))
      {
      Variant locvalues[2] = {scenarioTable->FieldValues["scenarioIndex"],
                              Variant(cropName.c_str())};
      return cropTable->Locate("ScenarioIndex;CropName", VarArrayOf(locvalues, 1),
                               TLocateOptions());
      }
   return false;
   }
//---------------------------------------------------------------------------
// Add a crop to scenario.  Return true if found.
//---------------------------------------------------------------------------
bool GMCalculatorMDB::addCrop(const std::string& scenarioName, const std::string& cropName)
   {
   if (locateScenario(scenarioName))
      {
      if (!locateCrop(scenarioName, cropName))
         {
         cropTable->Append();
         cropTable->FieldValues["ScenarioIndex"] = scenarioTable->FieldValues["scenarioIndex"];
         cropTable->FieldValues["CropName"] = cropName.c_str();
         cropTable->Post();
         return true;
         }
      }
   return false;
   }
//---------------------------------------------------------------------------
// Delete the specified crop from the scenario.
//---------------------------------------------------------------------------
bool GMCalculatorMDB::deleteCrop(const std::string& scenarioName, const std::string& cropName)
   {
   if (locateCrop(scenarioName, cropName))
      {
      cropTable->Delete();
      return true;
      }
   return false;
   }
//---------------------------------------------------------------------------
// Return all costs for specified crop.
//---------------------------------------------------------------------------
void GMCalculatorMDB::getCosts(const string& scenarioName,
                               const string& cropName,
                               vector<GMCalculator::Costs>& costs) const
   {
   if (locateCrop(scenarioName, cropName))
      {
      unitCostsTable->Filter = "CropIndex = " + cropTable->FieldValues["cropIndex"];
      unitCostsTable->Filtered = true;

      unitCostsTable->First();
      while (!unitCostsTable->Eof)
         {
         GMCalculator::Costs cost;
         cost.operationName = AnsiString(unitCostsTable->FieldValues["operation"]).c_str();
         cost.operationCost = unitCostsTable->FieldValues["operationCost"];
         cost.productCost   = unitCostsTable->FieldValues["productCost"];
         cost.productUnits  = AnsiString(unitCostsTable->FieldValues["productUnits"]).c_str();
         cost.productRate   = unitCostsTable->FieldValues["productRate"];
         cost.costType      = (GMCalculator::CostType) (int) unitCostsTable->FieldValues["costType"];
         costs.push_back(cost);

         unitCostsTable->Next();
         }
      unitCostsTable->Filtered = false;
      }
   else
      throw runtime_error("Cannot find cost information for scenario: "
            + scenarioName + " and crop: " + cropName);
   }
//---------------------------------------------------------------------------
// Set all costs for specified crop.
//---------------------------------------------------------------------------
void GMCalculatorMDB::setCosts(const string& scenarioName,
                               const string& cropName,
                               const vector<GMCalculator::Costs>& costs)
   {
   if (locateCrop(scenarioName, cropName))
      {
      unitCostsTable->Filter = "CropIndex = " + cropTable->FieldValues["cropIndex"];
      unitCostsTable->Filtered = true;

      unitCostsTable->First();
      for (unsigned costsIndex = 0; costsIndex != costs.size(); costsIndex++)
         {
         if (unitCostsTable->Eof)
            unitCostsTable->Append();
         else
            unitCostsTable->Edit();
         unitCostsTable->FieldValues["CropIndex"] = cropTable->FieldValues["cropIndex"];
         unitCostsTable->FieldValues["Operation"] = costs[costsIndex].operationName.c_str();
         unitCostsTable->FieldValues["OperationCost"] = costs[costsIndex].operationCost;
         unitCostsTable->FieldValues["ProductCost"] = costs[costsIndex].productCost;
         unitCostsTable->FieldValues["ProductUnits"] = costs[costsIndex].productUnits.c_str();
         unitCostsTable->FieldValues["ProductRate"] = costs[costsIndex].productRate;
         unitCostsTable->FieldValues["CostType"] = costs[costsIndex].costType;
         unitCostsTable->Post();
         unitCostsTable->Next();
         }
      // Delete unwanted records.
      int numUnwantedRecords = unitCostsTable->RecordCount - costs.size();
      unitCostsTable->Last();
      for (int rec = 0; rec != numUnwantedRecords; rec++)
         unitCostsTable->Delete();

      unitCostsTable->Filtered = false;
      }
   else
      throw runtime_error("Cannot find cost information for scenario: "
            + scenarioName + " and crop: " + cropName + ". Cannot set costs.");
   }
//---------------------------------------------------------------------------
// Return price structure for specified crop.
//---------------------------------------------------------------------------
void GMCalculatorMDB::getPrice(const std::string& scenarioName,
                            const std::string& cropName,
                            GMCalculator::Price& price) const
   {
   if (locateCrop(scenarioName, cropName))
      {
      price.price = getDBDouble(cropTable, "Price");
      price.harvestLoss = getDBDouble(cropTable, "HarvestLoss");
      price.downgradePercent = getDBDouble(cropTable, "Downgrade%");
      price.downgradeReturn = getDBDouble(cropTable, "DowngradeReturn");
      price.moistureContent = getDBDouble(cropTable, "MoistureContent");
      }
   else
      throw runtime_error("Cannot find price information for scenario: "
            + scenarioName + " and crop: " + cropName);
   }
//---------------------------------------------------------------------------
// Set price structure for specified crop.
//---------------------------------------------------------------------------
void GMCalculatorMDB::setPrice(const std::string& scenarioName,
                            const std::string& cropName,
                            const GMCalculator::Price& price)
   {
   if (locateCrop(scenarioName, cropName))
      {
      cropTable->Edit();
      cropTable->FieldValues["Price"] = price.price;
      cropTable->FieldValues["HarvestLoss"] = price.harvestLoss;
      cropTable->FieldValues["Downgrade%"] = price.downgradePercent;
      cropTable->FieldValues["DowngradeReturn"] = price.downgradeReturn;
      cropTable->FieldValues["MoistureContent"] = price.moistureContent;
      cropTable->Post();
      }
   }
//---------------------------------------------------------------------------
// Adjust a dry yield for harvest loss.
//---------------------------------------------------------------------------
void GMCalculatorMDB::adjustYieldForHarvestLoss(const std::string& scenarioName,
                                             const std::string& cropName,
                                             float& dryYield) const
   {
   GMCalculator::Price price;
   getPrice(scenarioName, cropName, price);
   dryYield = dryYield - (price.harvestLoss / 100.0) * dryYield;
   }
//---------------------------------------------------------------------------
// convert a DRY yield into a WET yield.
//---------------------------------------------------------------------------
float GMCalculatorMDB::calculateWetYield(const std::string& scenarioName,
                                      const std::string& cropName,
                                      float dryYield) const
   {
   GMCalculator::Price price;
   getPrice(scenarioName, cropName, price);
   return dryYield / (1 - price.moistureContent / 100);
   }
//---------------------------------------------------------------------------
// Calculate and return a gross margin return($/ha) given the WET
// yield(kg/ha) and protein(%).  Protein is ignored for non-wheat crops.
//---------------------------------------------------------------------------
float GMCalculatorMDB::calculateReturn(const std::string& scenarioName,
                                    const std::string& cropName,
                                    float wetYield, float protein) const
   {
   GMCalculator::Price priceStruct;
   getPrice(scenarioName, cropName, priceStruct);

   // if crop is wheat, add protein adjustment
   float price = priceStruct.price;
   if (Str_i_Eq(cropName, "wheat"))
      {
      std::vector<double> proteinValues;
      std::vector<double> proteinIncrements;
      getProteinIncrements(proteinValues, proteinIncrements);
      bool didInterp;
      double increment = linear_interp_real(protein, proteinValues,
                                            proteinIncrements, didInterp);
      price += increment;
      }
   price = (100 - priceStruct.downgradePercent)/100.0 * price
         + priceStruct.downgradePercent/100.0 * priceStruct.downgradeReturn;

   if (Str_i_Eq(cropName, "cotton"))
      return price * wetYield;            // $/bale * bale/ha = $/ha
   else
      return price * wetYield / 1000.0;  // $/tonne * kg/ha / 1000 = $/ha
   }

//---------------------------------------------------------------------------
// Caclulate and return a gross margin cost($/ha) given the
// nitrogen rate(kg/ha) and planting rate(plants/m2).
//---------------------------------------------------------------------------
float GMCalculatorMDB::calculateCost(const std::string& scenarioName,
                                  const std::string& cropName,
                                  float nitrogenRate, float plantingRate,
                                  float yield) const
   {
   vector<GMCalculator::Costs> costs;
   getCosts(scenarioName, cropName, costs);

   float cost = 0.0;
   for (unsigned c = 0; c != costs.size(); c++)
      {
      switch (costs[c].costType)
         {
         case GMCalculator::fixedCost:
            {
            cost += costs[c].operationCost + costs[c].productCost * costs[c].productRate;
            break;
            }
         case GMCalculator::sowingCost:
            {
            float factor;
            if (costs[c].productUnits == "kg")
               factor = 10.0;
            else if (costs[c].productUnits == "g")
               factor = 10000.0;
            else
               throw runtime_error("Invalid units specified for seed cost in sowing.  Should be kg or g");
            cost += costs[c].operationCost + plantingRate * factor
                 * getSeedWeight(cropName) * costs[c].productCost;
            break;
            }
         case GMCalculator::nitrogenCost:
            {
            float factor;
            if (costs[c].productUnits == "tonne")
               factor = 0.001;
            else if (costs[c].productUnits == "kg")
               factor = 1;
            else if (costs[c].productUnits == "g")
               factor = 1000;
            else
               throw runtime_error("Invalid units specified for nitrogen cost in nitrogen application.  Should be tonne, kg or g");
            if (nitrogenRate > 0)
               cost += costs[c].operationCost + costs[c].productCost * factor * nitrogenRate;
            break;
            }
         case GMCalculator::yieldCost:
            {
            float factor;
            if (costs[c].productUnits == "tonne")
               factor = 0.001;
            else if (costs[c].productUnits == "kg")
               factor = 1;
            else if (costs[c].productUnits == "g")
               factor = 1000;
            else
               throw runtime_error("Invalid units specified for yield related cost.  Should be tonne, kg or g");
            cost += costs[c].operationCost + costs[c].productCost * factor * yield;
            break;
            }
         }
      }
   return cost;
   }
//---------------------------------------------------------------------------
// Return the protein increments to caller.
//---------------------------------------------------------------------------
void GMCalculatorMDB::getProteinIncrements(std::vector<double>& proteinValues,
                                        std::vector<double>& proteinIncrements) const
   {
   wheatMatrixTable->First();
   while (!wheatMatrixTable->Eof)
      {
      proteinValues.push_back(wheatMatrixTable->FieldValues["protein%"]);
      proteinIncrements.push_back(wheatMatrixTable->FieldValues["increment"]);
      wheatMatrixTable->Next();
      }
   }
//---------------------------------------------------------------------------
// Set the protein increments
//---------------------------------------------------------------------------
void GMCalculatorMDB::setProteinIncrements(const std::vector<double>& proteinValues,
                                        const std::vector<double>& proteinIncrements)
   {
   wheatMatrixTable->First();
   for (unsigned i = 0; i != proteinValues.size(); i++)
      {
      if (wheatMatrixTable->Eof)
         wheatMatrixTable->Append();
      else
         wheatMatrixTable->Edit();
      wheatMatrixTable->FieldValues["protein%"]  = proteinValues[i];
      wheatMatrixTable->FieldValues["increment"] = proteinIncrements[i];
      wheatMatrixTable->Post();
      wheatMatrixTable->Next();
      }

   // Delete unwanted records.
   int numUnwantedRecords = wheatMatrixTable->RecordCount - proteinValues.size();
   wheatMatrixTable->Last();
   for (int rec = 0; rec != numUnwantedRecords; rec++)
      wheatMatrixTable->Delete();
   }
//---------------------------------------------------------------------------
// get the seed weight for specified crop.
//---------------------------------------------------------------------------
double GMCalculatorMDB::getSeedWeight(const std::string& cropName) const
   {
   Variant nameVariant = Variant(cropName.c_str());
   if (cropListTable->Locate("CropName", nameVariant, TLocateOptions()))
      return cropListTable->FieldValues["seedwt"];
   throw runtime_error("Cannot find a seed weight for crop: " + cropName);
   }
//---------------------------------------------------------------------------
// get the seed weights for all crops.
//---------------------------------------------------------------------------
void GMCalculatorMDB::getSeedWeights(vector<std::string>& cropNames,
                                  vector<double>& seedWeights) const
   {
   getDBFieldValues(cropListTable, "CropName", cropNames);
   getDBFieldValues(cropListTable, "SeedWt", seedWeights);
   }
//---------------------------------------------------------------------------
// get the seed weights for all crops.
//---------------------------------------------------------------------------
void GMCalculatorMDB::setSeedWeights(const vector<std::string>& cropNames,
                                  const vector<double> seedWeights)
   {
   cropListTable->First();
   for (unsigned c = 0; c != cropNames.size(); c++)
      {
      if (cropListTable->Eof)
         cropListTable->Append();
      else
         cropListTable->Edit();
      cropListTable->FieldValues["CropName"] = cropNames[c].c_str();
      cropListTable->FieldValues["SeedWt"]   = seedWeights[c];
      cropListTable->Post();
      cropListTable->Next();
      }
   // Delete unwanted records.
   int numUnwantedRecords = cropListTable->RecordCount - cropNames.size();
   cropListTable->Last();
   for (int rec = 0; rec != numUnwantedRecords; rec++)
      cropListTable->Delete();
   }


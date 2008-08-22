//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "GMCalculator.h"
#include <general\string_functions.h>
#include <general\db_functions.h>
#include <general\math_functions.h>
#include <general\stl_functions.h>
#include <general\exec.h>
#include <general\path.h>
#include <ApsimShared\ApsimDirectories.h>
#include "TGMForm.h"

#pragma package(smart_init)

//---------------------------------------------------------------------------
// constructor.
//---------------------------------------------------------------------------
GMCalculator::GMCalculator(void)
   {
   doc = NULL;
   }
//---------------------------------------------------------------------------
// destructor.
//---------------------------------------------------------------------------
GMCalculator::~GMCalculator(void)
   {
   close();
   }
//---------------------------------------------------------------------------
// Open the specified database file.
//---------------------------------------------------------------------------
void GMCalculator::open(const string& filename)
   {
   fileName = filename;

   // Open all tables.
   doc = new XMLDocument(filename);
   }
//---------------------------------------------------------------------------
// Close the database file.
//---------------------------------------------------------------------------
void GMCalculator::close()
   {
   delete doc;
   doc = NULL;
   }
//---------------------------------------------------------------------------
// Save the database file.
//---------------------------------------------------------------------------
void GMCalculator::save()
   {
   doc->write(fileName);
   }
//---------------------------------------------------------------------------
// Return a complete list of available crops to caller.
//---------------------------------------------------------------------------
void GMCalculator::getPossibleCrops(std::vector<string>& cropNames) const
   {
   for_each_if(doc->documentElement().begin(),
               doc->documentElement().end(),
               GetNameAttributeFunction<XMLNode>(cropNames),
               EqualToName<XMLNode>("crop"));
   sort(cropNames.begin(), cropNames.end());
   }
//---------------------------------------------------------------------------
// Return a complete list of operation names that have been used before.
//---------------------------------------------------------------------------
void GMCalculator::getPreviousOperations(std::vector<string>& operationNames)
   {
   for (XMLNode::iterator scenarioNode = doc->documentElement().begin();
                          scenarioNode != doc->documentElement().end();
                          scenarioNode++)
      {
      if (scenarioNode->getName() == "scenario")
         {
         for (XMLNode::iterator cropNode = scenarioNode->begin();
                                cropNode != scenarioNode->end();
                                cropNode++)
            {
            if (cropNode->getName() == "crop")
               for_each_if(cropNode->begin(),
                           cropNode->end(),
                           GetNameAttributeFunction<XMLNode>(operationNames),
                           EqualToName<XMLNode>("operation"));
            }
         }
      }

   sort(operationNames.begin(), operationNames.end());
   operationNames.erase(unique(operationNames.begin(), operationNames.end()),
                        operationNames.end());
   }
//---------------------------------------------------------------------------
// Return an iterator to a scenario or doc->documentElement().end()
// if not found.
//---------------------------------------------------------------------------
XMLNode::iterator GMCalculator::getScenario(const string& scenarioName) const
   {
   return find_if(doc->documentElement().begin(),
                  doc->documentElement().end(),
                  NodeEquals<XMLNode>("scenario", scenarioName));
   }
//---------------------------------------------------------------------------
// Return a list of all scenarios.
//---------------------------------------------------------------------------
void GMCalculator::getScenarioNames(std::vector<string>& scenarioNames) const
   {
   for_each_if(doc->documentElement().begin(),
               doc->documentElement().end(),
               GetNameAttributeFunction<XMLNode>(scenarioNames),
               EqualToName<XMLNode>("scenario"));
   }
//---------------------------------------------------------------------------
// Add a scenario.  Returns true if added.
//---------------------------------------------------------------------------
bool GMCalculator::addScenario(const std::string& scenarioName)
   {
   XMLNode::iterator scenario = getScenario(scenarioName);
   if (scenario == doc->documentElement().end())
      {
      XMLNode newNode = doc->documentElement().appendChild("scenario", true);
      newNode.setAttribute("name", scenarioName);
      return true;
      }
   return false;
   }
//---------------------------------------------------------------------------
// Delete the specified scenario.
//---------------------------------------------------------------------------
bool GMCalculator::deleteScenario(const std::string& scenarioName)
   {
   XMLNode::iterator scenario = getScenario(scenarioName);
   if (scenario != doc->documentElement().end())
      {
      doc->documentElement().erase(scenario);
      return true;
      }
   return false;
   }
//---------------------------------------------------------------------------
// Rename a scenario.
//---------------------------------------------------------------------------
bool GMCalculator::renameScenario(const std::string& oldName,
                                  const std::string& newName)
   {
   XMLNode::iterator scenario = getScenario(oldName);
   if (scenario != doc->documentElement().end())
      {
      scenario->setAttribute("name", newName);
      return true;
      }
   return false;
   }
//---------------------------------------------------------------------------
// Return a list of crops for specified scenario.
//---------------------------------------------------------------------------
void GMCalculator::getCropsInScenario(const string& scenarioName,
                                      std::vector<std::string>& cropNames)
   {
   XMLNode::iterator scenario = getScenario(scenarioName);
   if (scenario != doc->documentElement().end())
      for_each_if(scenario->begin(), scenario->end(),
                  GetNameAttributeFunction<XMLNode>(cropNames),
                  EqualToName<XMLNode>("crop"));
   }
//---------------------------------------------------------------------------
// Locate a crop for a specified scenario.  Return true if found.
//---------------------------------------------------------------------------
XMLNode::iterator GMCalculator::getCropInScenario(XMLNode::iterator scenario, const string& cropName) const
   {
   return find_if(scenario->begin(), scenario->end(),
                  NodeEquals<XMLNode>("crop", cropName));
   }
//---------------------------------------------------------------------------
// Add a crop to scenario.  Return true if found.
//---------------------------------------------------------------------------
bool GMCalculator::addCrop(const std::string& scenarioName, const std::string& cropName)
   {
   XMLNode::iterator scenario = getScenario(scenarioName);
   if (scenario != doc->documentElement().end())
      {
      XMLNode::iterator crop = getCropInScenario(scenario, cropName);
      if (crop == scenario->end())
         {
         XMLNode newCrop = scenario->appendChild("crop", true);
         newCrop.setAttribute("name", cropName);
         return true;
         }
      }
   return false;
   }
//---------------------------------------------------------------------------
// Delete the specified crop from the scenario.
//---------------------------------------------------------------------------
bool GMCalculator::deleteCrop(const std::string& scenarioName, const std::string& cropName)
   {
   XMLNode::iterator scenario = getScenario(scenarioName);
   if (scenario != doc->documentElement().end())
      {
      XMLNode::iterator crop = getCropInScenario(scenario, cropName);
      if (crop != scenario->end())
         {
         scenario->erase(crop);
         return true;
         }
      }
   return false;
   }
//---------------------------------------------------------------------------
// Return all costs for specified crop.
//---------------------------------------------------------------------------
void GMCalculator::getCosts(const string& scenarioName,
                            const string& cropName,
                            vector<Costs>& costs) const
   {
   XMLNode::iterator scenario = getScenario(scenarioName);
   if (scenario != doc->documentElement().end())
      {
      XMLNode::iterator crop = getCropInScenario(scenario, cropName);
      if (crop != scenario->end())
         {
         for (XMLNode::iterator operation = crop->begin();
                                operation != crop->end();
                                operation++)
            {
            if (operation->getName() == "operation")
               {
               XMLNode product = findNode(*operation, "product");
               Costs cost;
               cost.operationName = operation->getAttribute("name");
               cost.operationCost = atof(findNode(*operation, "cost").getValue().c_str());
               cost.productCost   = atof(findNode(product, "cost").getValue().c_str());
               cost.productUnits  = findNode(product, "units").getValue();
               cost.productRate   = atof(findNode(product, "rate").getValue().c_str());
               cost.costType      = (CostType) atoi(findNode(product, "type").getValue().c_str());
               costs.push_back(cost);
               }
            }
         }
      else
         throw runtime_error("Cannot find crop: " + cropName + ". Cannot get costs.");
      }
   else
      throw runtime_error("Cannot find scenario: " + scenarioName);
   }
//---------------------------------------------------------------------------
// Set all costs for specified crop.
//---------------------------------------------------------------------------
void GMCalculator::setCosts(const string& scenarioName,
                            const string& cropName,
                            const vector<Costs>& costs)
   {
   XMLNode::iterator scenario = getScenario(scenarioName);
   if (scenario != doc->documentElement().end())
      {
      XMLNode::iterator crop = getCropInScenario(scenario, cropName);
      if (crop != scenario->end())
         {
         eraseNodes(*crop, "operation");
         for (unsigned costsIndex = 0; costsIndex != costs.size(); costsIndex++)
            {
            XMLNode operation = crop->appendChild("operation", true);
            operation.setAttribute("name", costs[costsIndex].operationName);
            operation.appendChild("cost", true).setValue(ftoa(costs[costsIndex].operationCost, 2));

            XMLNode product = operation.appendChild("product", true);
            product.appendChild("cost", true).setValue(ftoa(costs[costsIndex].productCost, 2));
            product.appendChild("units", true).setValue(costs[costsIndex].productUnits);

            product.appendChild("rate", true).setValue(ftoa(costs[costsIndex].productRate, 2));
            product.appendChild("type", true).setValue(itoa(costs[costsIndex].costType));
            }
         }
      else
         throw runtime_error("Cannot find crop: " + cropName + ". Cannot set costs.");
      }
   else
      throw runtime_error("Cannot find scenario: " + scenarioName);
   }
//---------------------------------------------------------------------------
// Return price structure for specified crop.
//---------------------------------------------------------------------------
void GMCalculator::getPrice(const std::string& scenarioName,
                            const std::string& cropName,
                            Price& price) const
   {
   XMLNode::iterator scenario = getScenario(scenarioName);
   if (scenario != doc->documentElement().end())
      {
      XMLNode::iterator crop = getCropInScenario(scenario, cropName);
      if (crop != scenario->end())
         {
         price.price = atof(findNode(*crop, "price").getValue().c_str());
         price.harvestLoss = atof(findNode(*crop, "harvestloss").getValue().c_str());
         price.downgradePercent = atof(findNode(*crop, "downgrade").getValue().c_str());
         price.downgradeReturn = atof(findNode(*crop, "downgradeReturn").getValue().c_str());
         price.moistureContent = atof(findNode(*crop, "moistureContent").getValue().c_str());
         }
      else
         throw runtime_error("Cannot find crop: " + cropName + ". Cannot get price info.");
      }
   else
      throw runtime_error("Cannot find scenario: " + scenarioName);
   }
//---------------------------------------------------------------------------
// Set price structure for specified crop.
//---------------------------------------------------------------------------
void GMCalculator::setPrice(const std::string& scenarioName,
                            const std::string& cropName,
                            const Price& price)
   {
   XMLNode::iterator scenario = getScenario(scenarioName);
   if (scenario != doc->documentElement().end())
      {
      XMLNode::iterator crop = getCropInScenario(scenario, cropName);
      if (crop != scenario->end())
         {
         crop->appendChild("price", false).setValue(ftoa(price.price, 2));
         crop->appendChild("harvestloss", false).setValue(ftoa(price.harvestLoss, 2));
         crop->appendChild("downgrade", false).setValue(ftoa(price.downgradePercent, 2));
         crop->appendChild("downgradeReturn", false).setValue(ftoa(price.downgradeReturn, 2));
         crop->appendChild("moistureContent", false).setValue(ftoa(price.moistureContent, 2));
         }
      else
         throw runtime_error("Cannot find crop: " + cropName + ". Cannot set price.");
      }
   else
      throw runtime_error("Cannot find scenario: " + scenarioName);
   }
//---------------------------------------------------------------------------
// Adjust a dry yield for harvest loss.
//---------------------------------------------------------------------------
void GMCalculator::adjustYieldForHarvestLoss(const std::string& scenarioName,
                                             const std::string& cropName,
                                             float& dryYield) const
   {
   Price price;
   getPrice(scenarioName, cropName, price);
   dryYield = dryYield - (price.harvestLoss / 100.0) * dryYield;
   }
//---------------------------------------------------------------------------
// convert a DRY yield into a WET yield.
//---------------------------------------------------------------------------
float GMCalculator::calculateWetYield(const std::string& scenarioName,
                                      const std::string& cropName,
                                      float dryYield) const
   {
   Price price;
   getPrice(scenarioName, cropName, price);
   return dryYield / (1 - price.moistureContent / 100);
   }
//---------------------------------------------------------------------------
// Calculate and return a gross margin return($/ha) given the WET
// yield(kg/ha) and protein(%).  Protein is ignored for non-wheat crops.
//---------------------------------------------------------------------------
float GMCalculator::calculateReturn(const std::string& scenarioName,
                                    const std::string& cropName,
                                    float wetYield, float protein) const
   {
   Price priceStruct;
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
float GMCalculator::calculateCost(const std::string& scenarioName,
                                  const std::string& cropName,
                                  float nitrogenRate, float plantingRate,
                                  float yield) const
   {
   vector<Costs> costs;
   getCosts(scenarioName, cropName, costs);

   float cost = 0.0;
   for (unsigned c = 0; c != costs.size(); c++)
      {
      switch (costs[c].costType)
         {
         case fixedCost:
            {
            cost += costs[c].operationCost + costs[c].productCost * costs[c].productRate;
            break;
            }
         case sowingCost:
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
         case nitrogenCost:
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
         case yieldCost:
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
void GMCalculator::getProteinIncrements(std::vector<double>& proteinValues,
                                        std::vector<double>& proteinIncrements) const
   {
   for (XMLNode::iterator protein = doc->documentElement().begin();
                          protein != doc->documentElement().end();
                          protein++)
      {
      if (protein->getName() == "protein")
         {
         proteinValues.push_back(atof(findNode(*protein, "percent").getValue().c_str()));
         proteinIncrements.push_back(atof(findNode(*protein, "increment").getValue().c_str()));
         }
      }
   }
//---------------------------------------------------------------------------
// Set the protein increments
//---------------------------------------------------------------------------
void GMCalculator::setProteinIncrements(const std::vector<double>& proteinValues,
                                        const std::vector<double>& proteinIncrements)
   {
   eraseNodes(doc->documentElement(), "protein");
   for (unsigned i = 0; i != proteinValues.size(); i++)
      {
      XMLNode protein = doc->documentElement().appendChild("protein", true);
      protein.appendChild("percent", true).setValue(ftoa(proteinValues[i], 2));
      protein.appendChild("increment", true).setValue(ftoa(proteinIncrements[i], 2));
      }
   }
//---------------------------------------------------------------------------
// get the seed weight for specified crop.
//---------------------------------------------------------------------------
double GMCalculator::getSeedWeight(const std::string& cropName) const
   {
   XMLNode::iterator crop = find_if(doc->documentElement().begin(),
                                    doc->documentElement().end(),
                                    NodeEquals<XMLNode>("crop", cropName));
   if (crop != doc->documentElement().end())
      return atof(findNode(*crop, "seedwt").getValue().c_str());
   throw runtime_error("Cannot find a seed weight for crop: " + cropName);
   }
//---------------------------------------------------------------------------
// get the seed weights for all crops.
//---------------------------------------------------------------------------
void GMCalculator::getSeedWeights(vector<std::string>& cropNames,
                                  vector<double>& seedWeights) const
   {
   getPossibleCrops(cropNames);
   for (unsigned i = 0; i != cropNames.size(); i++)
      seedWeights.push_back(getSeedWeight(cropNames[i]));
   }
//---------------------------------------------------------------------------
// get the seed weights for all crops.
//---------------------------------------------------------------------------
void GMCalculator::setSeedWeights(const vector<std::string>& cropNames,
                                  const vector<double> seedWeights)
   {
   eraseNodes(doc->documentElement(), "crop");
   for (unsigned c = 0; c != cropNames.size(); c++)
      {
      XMLNode crop = doc->documentElement().appendChild("crop", true);
      crop.setAttribute("name", cropNames[c]);
      crop.appendChild("seedwt", true).setValue(ftoa(seedWeights[c], 4));
      }
   }
//---------------------------------------------------------------------------
// Let user edit economic scenarios.
//---------------------------------------------------------------------------
void __declspec(dllexport) showEconomicScenariosUI(std::string& fileName)
   {
   TGMForm* form = new TGMForm(Application);
   form->fileName = fileName;
   form->ShowModal();
   fileName = form->fileName;
   delete form;
   }

//---------------------------------------------------------------------------
// Convert an MDB to an XML file and return the new file name.
//---------------------------------------------------------------------------
string __declspec(dllexport) convertMDBToXML(const std::string& mdbFileName)
   {
   string cmdLine = getApsimDirectory();
   if (DirectoryExists(AnsiString(getApsimDirectory().c_str()) + "\\bin"))
      cmdLine += "\\bin";
   cmdLine += "\\ConvertEconomics.exe " + doubleQuoted(mdbFileName);
   Exec(cmdLine.c_str(), SW_SHOW, true);
   string newFileName = mdbFileName;
   To_lower(newFileName);
   replaceAll(newFileName, ".mdb", ".xml");
   return newFileName;
   }

//---------------------------------------------------------------------------
// Return a descriptive bit of text for the specified scenario and crop.
//---------------------------------------------------------------------------
string GMCalculator::econDescription(const std::string& scenarioName, const std::string& cropName)
   {
   ostringstream out;
   out << "\r\n";

   try
      {
      Price price;
      getPrice(scenarioName, cropName, price);
      out << "   Price=$" << price.price;
      if (price.harvestLoss > 0)
         out << " (downgrade " << price.downgradePercent << "% @ $" << price.downgradeReturn << ")\r\n";
      if (price.harvestLoss > 0)
         out << "      harvest loss=$" << price.harvestLoss << "\r\n";
      out << "      moisture content=" << price.moistureContent << "%";

      vector<Costs> costs;
      getCosts(scenarioName, cropName, costs);
      for (unsigned c = 0; c != costs.size(); c++)
         {
         out << "\r\n   " << costs[c].operationName<< " Costs=$" << costs[c].operationCost;
         if (costs[c].productCost > 0)
            {
            out << " (+ ";
            if (costs[c].costType == fixedCost)
               out << costs[c].productRate << costs[c].productUnits << " @ ";
            out << "$" << costs[c].productCost << "/" << costs[c].productUnits << ")";
            }
         }
      }
   catch (const exception& err)
      {
      }


   return out.str();
   }


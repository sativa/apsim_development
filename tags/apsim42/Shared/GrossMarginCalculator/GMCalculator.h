//---------------------------------------------------------------------------
#ifndef GMCalculatorH
#define GMCalculatorH
#include <string>
#include <general\xml.h>

class TGMForm;
class TMainForm;
class GMCalculatorMDB;
//---------------------------------------------------------------------------
// This class completely encapsulates a gross margin calculator
//---------------------------------------------------------------------------
class __declspec(dllexport) GMCalculator
   {
   public:
      //---------------------------------------------------------------------------
      // constructor / destructor
      //---------------------------------------------------------------------------
      GMCalculator(void);
      ~GMCalculator(void);

      //---------------------------------------------------------------------------
      // Open the specified database file.
      //---------------------------------------------------------------------------
      void open(const std::string& filename);

      //---------------------------------------------------------------------------
      // Close the database file.
      //---------------------------------------------------------------------------
      void close();

      //---------------------------------------------------------------------------
      // Save the database file.
      //---------------------------------------------------------------------------
      void save();

      //---------------------------------------------------------------------------
      // Return a list of all scenarios.
      //---------------------------------------------------------------------------
      void getScenarioNames(std::vector<string>& scenarioNames) const;

      //---------------------------------------------------------------------------
      // Adjust a dry yield for harvest loss.
      //---------------------------------------------------------------------------
      void adjustYieldForHarvestLoss(const std::string& scenarioName,
                                     const std::string& cropName,
                                     float& dryYield) const;

      //---------------------------------------------------------------------------
      // convert a DRY yield into a WET yield.
      //---------------------------------------------------------------------------
      float calculateWetYield(const std::string& scenarioName,
                              const std::string& cropName,
                              float dryYield) const;

      //---------------------------------------------------------------------------
      // Calculate and return a gross margin return($/ha) given the WET
      // yield(kg/ha) and protein(%).  Protein is ignored for non-wheat crops.
      //---------------------------------------------------------------------------
      float calculateReturn(const std::string& scenarioName,
                            const std::string& cropName,
                            float wetYield, float protein) const;

      //---------------------------------------------------------------------------
      // Caclulate and return a gross margin cost($/ha) given the
      // nitrogen rate(kg/ha) and planting rate(plants/m2).
      //---------------------------------------------------------------------------
      float calculateCost(const std::string& scenarioName,
                          const std::string& cropName,
                          float nitrogenRate, float plantingRate,
                          float yield) const;

      //---------------------------------------------------------------------------
      // Return a descriptive bit of text for the specified scenario and crop.
      //---------------------------------------------------------------------------
      std::string econDescription(const std::string& scenarioName, const std::string& cropName);

   private:
      std::string fileName;
      XMLDocument* doc;

      //---------------------------------------------------------------------------
      // Return a complete list of available crops to caller.
      //---------------------------------------------------------------------------
      void getPossibleCrops(std::vector<string>& cropNames) const;

      //---------------------------------------------------------------------------
      // Return a complete list of operation names that have been used before.
      //---------------------------------------------------------------------------
      void getPreviousOperations(std::vector<string>& operationNames);

      //---------------------------------------------------------------------------
      // Scenario manipulation methods.
      //---------------------------------------------------------------------------
      bool addScenario(const std::string& scenarioName);
      bool deleteScenario(const std::string& scenarioName);
      bool renameScenario(const std::string& oldName,
                          const std::string& newName);
      void getCropsInScenario(const std::string& scenarioName,
                              std::vector<std::string>& cropNames);

      //---------------------------------------------------------------------------
      // Crop manipulation methods.
      //---------------------------------------------------------------------------
      bool addCrop(const std::string& scenarioName, const std::string& cropName);
      bool deleteCrop(const std::string& scenarioname, const std::string& cropName);

      //---------------------------------------------------------------------------
      // Cost manipulation methods.
      //---------------------------------------------------------------------------
      enum CostType {fixedCost=1, sowingCost=2, nitrogenCost=3, yieldCost=4};
      struct Costs
         {
         std::string operationName;
         double      operationCost;
         double      productCost;
         std::string productUnits;
         double      productRate;
         CostType    costType;
         };
      void getCosts(const std::string& scenarioName,
                    const std::string& cropName,
                    vector<Costs>& costs) const;
      void setCosts(const std::string& scenarioName,
                    const std::string& cropName,
                    const std::vector<Costs>& costs);

      //---------------------------------------------------------------------------
      // Price manipulation methods
      //---------------------------------------------------------------------------
      struct Price
         {
         double price;
         double harvestLoss;
         double downgradePercent;
         double downgradeReturn;
         double moistureContent;
         };
      void getPrice(const std::string& scenarioName,
                    const std::string& cropName,
                    Price& price) const;
      void setPrice(const std::string& scenarioName,
                    const std::string& cropName,
                    const Price& price);

      //---------------------------------------------------------------------------
      // Return the protein increments to caller.
      //---------------------------------------------------------------------------
      void getProteinIncrements(std::vector<double>& proteinValues,
                                std::vector<double>& proteinIncrements) const;
      void setProteinIncrements(const std::vector<double>& proteinValues,
                                const std::vector<double>& proteinIncrements);

      //---------------------------------------------------------------------------
      // get the seed weight for specified crop.
      //---------------------------------------------------------------------------
      double getSeedWeight(const std::string& cropName) const;
      void getSeedWeights(vector<std::string>& cropNames,
                          vector<double>& seedWeights) const;
      void setSeedWeights(const vector<std::string>& cropNames,
                          const vector<double> seedWeights);

      //---------------------------------------------------------------------------
      // Return an iterator to a scenario or doc->documentElement().end()
      // if not found.
      //---------------------------------------------------------------------------
      XMLNode::iterator getScenario(const std::string& scenarioName) const;

      //---------------------------------------------------------------------------
      // Locate a crop for a specified scenario.  Return true if found.
      //---------------------------------------------------------------------------
      XMLNode::iterator getCropInScenario(XMLNode::iterator scenario, const std::string& cropName) const;


      friend TGMForm;
      friend TMainForm; // for ConvertEconomics
      friend GMCalculatorMDB;
   };

//---------------------------------------------------------------------------
// Let user edit economic scenarios.
//---------------------------------------------------------------------------
void __declspec(dllexport) showEconomicScenariosUI(std::string& fileName);

//---------------------------------------------------------------------------
// Convert an MDB to an XML file and return the new file name.
//---------------------------------------------------------------------------
string __declspec(dllexport) convertMDBToXML(const std::string& mdbFileName);

#endif

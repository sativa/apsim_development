//---------------------------------------------------------------------------
#ifndef GMCalculatorMDBH
#define GMCalculatorMDBH
#include <string>
#include <GrossMarginCalculator\GMCalculator.h>
class TGMForm;
class TMainForm;
namespace Adodb
   {
   class TADOConnection;
   class TADOTable;
   };
//---------------------------------------------------------------------------
// This class completely encapsulates a gross margin calculator
//---------------------------------------------------------------------------
class __declspec(dllexport) GMCalculatorMDB
   {
   public:
      //---------------------------------------------------------------------------
      // constructor / destructor
      //---------------------------------------------------------------------------
      GMCalculatorMDB(void);
      ~GMCalculatorMDB(void);

      //---------------------------------------------------------------------------
      // Open the specified database file.
      //---------------------------------------------------------------------------
      void open(const std::string& filename);

      //---------------------------------------------------------------------------
      // Close the database file.
      //---------------------------------------------------------------------------
      void close(void);

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

   private:
      std::string fileName;
      Adodb::TADOConnection* db;
      Adodb::TADOTable* scenarioTable;
      Adodb::TADOTable* cropTable;
      Adodb::TADOTable* unitCostsTable;
      Adodb::TADOTable* cropListTable;
      Adodb::TADOTable* wheatMatrixTable;

      //---------------------------------------------------------------------------
      // Return a complete list of available crops to caller.
      //---------------------------------------------------------------------------
      void getPossibleCrops(std::vector<string>& cropNames);

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
      void getCosts(const std::string& scenarioName,
                    const std::string& cropName,
                    vector<GMCalculator::Costs>& costs) const;
      void setCosts(const std::string& scenarioName,
                    const std::string& cropName,
                    const std::vector<GMCalculator::Costs>& costs);

      //---------------------------------------------------------------------------
      // Price manipulation methods
      //---------------------------------------------------------------------------
      void getPrice(const std::string& scenarioName,
                    const std::string& cropName,
                    GMCalculator::Price& price) const;
      void setPrice(const std::string& scenarioName,
                    const std::string& cropName,
                    const GMCalculator::Price& price);

      //---------------------------------------------------------------------------
      // check to see if we need to convert database to new format.
      //---------------------------------------------------------------------------
      void checkDatabaseConversion(void);

      //---------------------------------------------------------------------------
      // convert database to new format.
      //---------------------------------------------------------------------------
      void convertDatabase(void);

      //---------------------------------------------------------------------------
      // Locate a scenario.  Return true if found.
      //---------------------------------------------------------------------------
      bool locateScenario(const std::string& scenarioName) const;

      //---------------------------------------------------------------------------
      // Locate a crop for a specified scenario.  Return true if found.
      //---------------------------------------------------------------------------
      bool locateCrop(const std::string& scenarioName, const std::string& cropName) const;

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

      friend TGMForm;
      friend TMainForm; // for ConvertEconomics
   };

//---------------------------------------------------------------------------
// Let user edit economic scenarios.
//---------------------------------------------------------------------------
void __declspec(dllexport) showEconomicScenariosUI(std::string& mdbFileName);

#endif

//---------------------------------------------------------------------------
#ifndef ApsimRunsH
#define ApsimRunsH
#include <string>
#include <vector>
class ApsimRun;
//---------------------------------------------------------------------------
// This class keeps track of a series of APSIM runs and runs one at a time.
//---------------------------------------------------------------------------
class ApsimRuns
   {
   public:
      ApsimRuns(void) { };
      ~ApsimRuns(void);
      //---------------------------------------------------------------------------
      // Add a .run file to the list of runs to run.
      //---------------------------------------------------------------------------
      void addFile(const std::string& fileName, bool allSimulations);

      //---------------------------------------------------------------------------
      // Perform all APSIM runs.
      //---------------------------------------------------------------------------
      void runAll(bool withConsole, bool quiet, bool run);

      //---------------------------------------------------------------------------
      // Create SIM files for all runs.
      //---------------------------------------------------------------------------
      void createSIMs(void);

      //---------------------------------------------------------------------------
      // Get a list of control files that need converting.
      //---------------------------------------------------------------------------
      void getFilesNeedingConversion(std::vector<std::string>& fileNames);

      //---------------------------------------------------------------------------
      // Get a list of control files that are to be run.
      //---------------------------------------------------------------------------
      void getFilesToRun(std::vector<std::string>& fileNames);

      //---------------------------------------------------------------------------
      // Get the list of simulations to run for the specified control file.
      //---------------------------------------------------------------------------
      void getSimulationsToRun(const std::string& fileName,
                               std::vector<std::string>& simulations);

      //---------------------------------------------------------------------------
      // Set the list of simulations to run for the specified control file.
      //---------------------------------------------------------------------------
      void setSimulationsToRun(const std::string& fileName,
                               const std::vector<std::string>& simulations);

      //---------------------------------------------------------------------------
      // Perform all Apsim runs.
      //---------------------------------------------------------------------------
      void runApsim(bool quiet);
                               
   private:
      std::vector<ApsimRun*> runs;
      bool console;

      //---------------------------------------------------------------------------
      // Create all sim files.
      //---------------------------------------------------------------------------
      void createApsimSims(void);

      //---------------------------------------------------------------------------
      // Perform a single APSIM run.
      //---------------------------------------------------------------------------
      bool performRun(const std::string& simFileName, bool moreToGo);

      //---------------------------------------------------------------------------
      // Convert all Apsim runs if necessary.
      //---------------------------------------------------------------------------
      void convertFiles();

   };
#endif


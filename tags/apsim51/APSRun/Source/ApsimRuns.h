//---------------------------------------------------------------------------
#ifndef ApsimRunsH
#define ApsimRunsH
#include <string>
#include <vector>
class ApsimRun;
typedef void __fastcall (__closure *TApsimRunEvent)(const std::string& simFileName);
//---------------------------------------------------------------------------
// This class keeps track of a series of APSIM runs and runs one at a time.
//---------------------------------------------------------------------------
class ApsimRuns
   {
   public:
      ApsimRuns() : newFormat(true), paused(false), stopApsim(false) { }
      ~ApsimRuns()
         {
         }

      void clearSimulations();
      unsigned count() {return fileNames.size();}

      //---------------------------------------------------------------------------
      // Add simulations from the specified file to the pending list of runs.
      //---------------------------------------------------------------------------
      void addSimulationsFromFile(const std::string& fileName);

      //---------------------------------------------------------------------------
      // Add a specific simulation to the pending list of runs.
      //---------------------------------------------------------------------------
      void addSimulation(const std::string& fileName, const std::string& simName);

      //---------------------------------------------------------------------------
      // Get a list of control files that need converting.
      //---------------------------------------------------------------------------
      void getFilesNeedingConversion(std::vector<std::string>& fileNames);

      //---------------------------------------------------------------------------
      // Get the list of simulations to run for the specified control file.
      //---------------------------------------------------------------------------
      void getSimulations(std::vector<std::string>& files,
                          std::vector<std::string>& sims)
         {
         files = fileNames;
         sims = simNames;
         }

      //---------------------------------------------------------------------------
      // Perform all Apsim runs.
      //---------------------------------------------------------------------------
      void runApsim(bool quiet, TApsimRunEvent notifyEvent, TApsimRunEvent msgEvent);

      //---------------------------------------------------------------------------
      // Convert all Apsim runs if necessary.
      //---------------------------------------------------------------------------
      void convertFiles();

      // ---------------------
      // Set .sim file format
      // ---------------------
      void setSimFormat(bool NewFormat) {newFormat = NewFormat;}

      void Pause(bool doPause) {paused = doPause;}
      void StopApsim() {stopApsim = true;}

   private:
      bool newFormat;
      std::vector<std::string> fileNames;
      std::vector<std::string> simNames;
      bool paused;
      bool stopApsim;

      //---------------------------------------------------------------------------
      // Add the specified simulations from the specified CON file.
      //---------------------------------------------------------------------------
      void addSimulationsFromConFile(const std::string& fileName,
                                     const std::vector<std::string>& sims);

      //---------------------------------------------------------------------------
      // Add the specified simulations from the specified .APSIM file.
      //---------------------------------------------------------------------------
      void addSimulationsFromApsimFile(const std::string& fileName);

      bool ApsExec(const char* Command_line, TApsimRunEvent msgEvent);

   };
#endif


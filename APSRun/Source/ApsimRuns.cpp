//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "ApsimRuns.h"
#include "TRunForm.h"
#include <ApsimShared\ControlFileConverter.h>
#include <ApsimShared\ApsimRunFile.h>
#include <ApsimShared\ApsimControlFile.h>
#include <ApsimShared\SimCreator.h>
#include <ApsimShared\ApsimDirectories.h>
#include <ApsimShared\ApsimSettings.h>
#include <general\path.h>
#include <general\exec.h>
#pragma package(smart_init)
using namespace std;
//---------------------------------------------------------------------------
// Add a .run file to the list of runs to run.
//---------------------------------------------------------------------------
class ApsimRun
   {
   public:
      ApsimRun(const string& file)
         : fileName(file) { }

      void setSimulationsToRun(const vector<string>& simulations)
         {
         sections = simulations;
         }
      void getSimulationsToRun(vector<string>& simulations)
         {
         simulations = sections;
         }
      void doAllSimulations(void)
         {
         if (Path(fileName).Get_extension() == ".con")
            {
            ApsimControlFile con(fileName);
            con.getAllSectionNames(sections);
            }
         }
      string getFileName(void) const {return fileName;}
   private:
      string fileName;
      vector<string> sections;
   };

//---------------------------------------------------------------------------
// destructor
//---------------------------------------------------------------------------
ApsimRuns::~ApsimRuns(void)
   {
   for (unsigned r = 0; r != runs.size(); r++)
      delete runs[r];
   }
//---------------------------------------------------------------------------
// Add a .run file to the list of runs to run.
//---------------------------------------------------------------------------
void ApsimRuns::addFile(const std::string& fileName, bool allSimulations)
   {
   if (!FileExists(fileName.c_str()))
      throw runtime_error("Cannot find file: " + fileName);

   if (Path(fileName).Get_extension() == ".run")
      {
      ApsimRunFile runFile(fileName);
      vector<string> runNames;
      runFile.getRuns(runNames);
      for (unsigned r = 0; r != runNames.size(); r++)
         {
         string fileName;
         vector<string> conFileSections;
         runFile.getRun(runNames[r], fileName, conFileSections);
         runs.push_back(new ApsimRun(fileName));
         if (conFileSections.size() == 0)
            allSimulations = true;
         else
            runs[runs.size()-1]->setSimulationsToRun(conFileSections);
         }
      }
   else
      runs.push_back(new ApsimRun(fileName));

   if (allSimulations)
      runs[runs.size()-1]->doAllSimulations();
   }
//---------------------------------------------------------------------------
// Perform all APSIM runs.
//---------------------------------------------------------------------------
void ApsimRuns::runAll(bool withConsole, bool quiet)
   {
   console = withConsole;
   if (!quiet)
      {
      RunForm = new TRunForm(NULL);
      RunForm->runs = this;
      RunForm->ShowModal();
      }
   else
      runApsim();
   }
//---------------------------------------------------------------------------
// Create SIM files for all runs.
//---------------------------------------------------------------------------
void ApsimRuns::createSIMs(void)
   {
   createApsimSims();
   }
//---------------------------------------------------------------------------
// Get a list of control files that need converting.
//---------------------------------------------------------------------------
void ApsimRuns::getFilesNeedingConversion(std::vector<std::string>& fileNames)
   {
   for (unsigned f = 0; f != runs.size(); f++)
      {
      string fileName = runs[f]->getFileName();
      if (ControlFileConverter::needsConversion(fileName))
         fileNames.push_back(fileName);
      }
   }
//---------------------------------------------------------------------------
// Get a list of control files that are to be run.
//---------------------------------------------------------------------------
void ApsimRuns::getFilesToRun(std::vector<std::string>& fileNames)
   {
   for (unsigned f = 0; f != runs.size(); f++)
      {
      string fileName = runs[f]->getFileName();
      fileNames.push_back(fileName);
      }
   }
//---------------------------------------------------------------------------
// Set the list of simulations to run for the specified control file.
//---------------------------------------------------------------------------
void ApsimRuns::setSimulationsToRun(const std::string& fileName,
                                    const std::vector<std::string>& simulations)
   {
   for (unsigned f = 0; f != runs.size(); f++)
      {
      if (runs[f]->getFileName() == fileName)
         runs[f]->setSimulationsToRun(simulations);
      }
   }
//---------------------------------------------------------------------------
// Perform all Apsim runs.
//---------------------------------------------------------------------------
void ApsimRuns::runApsim(void)
   {
   ApsimSettings settings;
   bool continueWithRuns = true;
   for (unsigned f = 0; f != runs.size() && continueWithRuns; f++)
      {
      Path filePath(runs[f]->getFileName());
      if (filePath.Get_extension() == ".con")
         {
         Path simFilePath(filePath);
         simFilePath.Set_extension(".sim");

         SimCreator simCreator(filePath.Get_path());
         vector<string> simulations;
         runs[f]->getSimulationsToRun(simulations);
         for (unsigned s = 0; s != simulations.size() && continueWithRuns; s++)
            {
            if (s == simulations.size()-1)
               settings.write("Apsim|MoreRunsToGo", "false");
            else
               settings.write("Apsim|MoreRunsToGo", "true");
            simCreator.createSim(simulations[s], "");

            string commandLine = "\"" + getApsimDirectory() + "\\bin\\apsim.exe\" ";
            if (console)
               commandLine += "/console ";
            commandLine += "\"" + simFilePath.Get_path() + "\"";
            Exec(commandLine.c_str(), SW_SHOW, true);
            settings.refresh();
            string nextWasClicked;
            settings.read("Apsim|NextWasClicked", nextWasClicked);
            continueWithRuns = (nextWasClicked == "true");
            }
         }
      }
   settings.deleteKey("Apsim|MoreRunsToGo");
   settings.deleteKey("Apsim|NextWasClicked");
   }
//---------------------------------------------------------------------------
// Create all sim files.
//---------------------------------------------------------------------------
void ApsimRuns::createApsimSims(void)
   {
   for (unsigned f = 0; f != runs.size(); f++)
      {
      string fileName = runs[f]->getFileName();
      SimCreator simCreator(fileName);
      if (Path(fileName).Get_extension() == ".con")
         {
         vector<string> simulations;
         runs[f]->getSimulationsToRun(simulations);
         for (unsigned s = 0; s != simulations.size(); s++)
            simCreator.createSims(simulations, "", (TSimCreatorEvent)NULL);
         }
      }
   }


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
            runs[runs.size()-1]->doAllSimulations();
         else
            runs[runs.size()-1]->setSimulationsToRun(conFileSections);
         }
      }
   else if (Path(fileName).Get_extension() == ".con")
      {
      runs.push_back(new ApsimRun(fileName));
      runs[runs.size()-1]->doAllSimulations();
      }
   else
      runs.push_back(new ApsimRun(fileName));

   if (allSimulations)
      {
      for (unsigned r = 0; r != runs.size(); r++)
         runs[r]->doAllSimulations();
      }
   }
//---------------------------------------------------------------------------
// Perform all APSIM runs.
//---------------------------------------------------------------------------
void ApsimRuns::runAll(bool withConsole, bool quiet, bool run)
   {
   console = withConsole;
   if (!quiet && !run)
      {
      RunForm = new TRunForm(NULL);
      RunForm->runs = this;
      RunForm->ShowModal();
      }
   else
      {
      RunForm = new TRunForm(NULL);
      RunForm->runs = this;
      RunForm->Show();
      RunForm->MainPanel->Visible = false;
      convertFiles();
      runApsim(quiet);
      }
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
      if (Path(fileName).Get_extension() == ".con"
          && ControlFileConverter::needsConversion(fileName))
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
// Get the list of simulations to run for the specified control file.
//---------------------------------------------------------------------------
void ApsimRuns::getSimulationsToRun(const std::string& fileName,
                                    std::vector<std::string>& simulations)
   {
   for (unsigned f = 0; f != runs.size(); f++)
      {
      if (runs[f]->getFileName() == fileName)
         runs[f]->getSimulationsToRun(simulations);
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
void ApsimRuns::runApsim(bool quiet)
   {
   ApsimSettings settings;
   if (quiet)
      settings.write("Apsim|Quiet", "true");
   else
      settings.write("Apsim|Quiet", "false");

   bool continueWithRuns = true;
   for (unsigned f = 0; f != runs.size() && continueWithRuns; f++)
      {
      Path filePath(runs[f]->getFileName());
      try
         {
         if (filePath.Get_extension() == ".con")
            {
            Path simFilePath(filePath);
            simFilePath.Set_extension(".sim");

            SimCreator simCreator(filePath.Get_path());
            vector<string> simulations;
            runs[f]->getSimulationsToRun(simulations);
            for (unsigned s = 0; s != simulations.size() && continueWithRuns; s++)
               {
               simCreator.createSim(simulations[s], "");
               bool moreToGo = ((f != runs.size()-1
                                 || s != simulations.size()-1));
               continueWithRuns = performRun(simFilePath.Get_path(), moreToGo);
               }
            }
         else
            {
            bool moreToGo = (f != runs.size()-1);
            continueWithRuns = performRun(filePath.Get_path(), moreToGo);
            }
         }
      catch (const runtime_error& err)
         {
         if (quiet)
            {
            filePath.Set_extension(".log");
            ofstream log(filePath.Get_path().c_str());
            log << err.what();
            }
         else
            ::MessageBox(NULL, err.what(), "Error", MB_ICONSTOP | MB_OK);
         }
      }
   settings.refresh();
   settings.deleteKey("Apsim|Quiet");
   settings.deleteKey("Apsim|MoreRunsToGo");
   settings.deleteKey("Apsim|NextWasClicked");
   }
//---------------------------------------------------------------------------
// Perform a single APSIM run.
//---------------------------------------------------------------------------
bool ApsimRuns::performRun(const std::string& simFileName, bool moreToGo)
   {
   if (FileExists(simFileName.c_str()))
      {
      ApsimSettings settings;
      if (moreToGo)
         settings.write("Apsim|MoreRunsToGo", "true");
      else
         settings.write("Apsim|MoreRunsToGo", "false");

      string commandLine = "\"" + getApsimDirectory() + "\\bin\\apsim.exe\" ";
      if (console)
         commandLine += "/console ";
      commandLine += "\"" + simFileName + "\"";

      string caption = "Running ";
      caption += simFileName;
      caption.erase(caption.find(".sim"));
      RunForm->FileNameLabel->Caption = caption.c_str();

      Exec(commandLine.c_str(), SW_SHOW, true);
      settings.refresh();
      string nextWasClicked;
      settings.read("Apsim|NextWasClicked", nextWasClicked);
      return (nextWasClicked == "true");
      }
   return false;
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
//---------------------------------------------------------------------------
// Convert all Apsim runs if necessary.
//---------------------------------------------------------------------------
void ApsimRuns::convertFiles()
   {
   ControlFileConverter converter;

   for (unsigned f = 0; f != runs.size(); f++)
      {
      Path filePath(runs[f]->getFileName());
      try
         {
         if (filePath.Get_extension() == ".con")
            {
            converter.convert(filePath.Get_path(),
                              (TControlFileConverterEvent)NULL);
            }
         }
      catch (const exception& err)
         {
         filePath.Set_extension("log");
         ofstream log(filePath.Get_path().c_str());
         log << err.what();
         }
      }
   }


//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "ApsimRuns.h"
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
// Add simulations from the specified file to the pending list of runs.
//---------------------------------------------------------------------------
void ApsimRuns::addSimulationsFromFile(const std::string& fileName)
   {
   if (!FileExists(fileName.c_str()))
      throw runtime_error("Cannot find file: " + fileName);

   string fileExtension = Path(fileName).Get_extension();
   if (fileExtension == ".run")
      {
      ApsimRunFile runFile(fileName);
      vector<string> runNames;
      runFile.getRuns(runNames);
      for (unsigned r = 0; r != runNames.size(); r++)
         {
         string fileName;
         vector<string> conFileSections;
         runFile.getRun(runNames[r], fileName, conFileSections);
         if (conFileSections.size() == 0)
            addSimulationsFromFile(fileName);
         else
            addSimulationsFromConFile(fileName, conFileSections);
         }
      }
   else if (fileExtension == ".con")
      {
      vector<string> conFileSections;
      addSimulationsFromConFile(fileName, conFileSections);
      }
   else if (fileExtension == ".sim")
      addSimulation(fileName, Path(fileName).Get_name_without_ext());
   else
      throw runtime_error("Invalid simulation file: " + fileName);
   }
//---------------------------------------------------------------------------
// Add the specified simulations from the specified CON file.
//---------------------------------------------------------------------------
void ApsimRuns::addSimulationsFromConFile(const std::string& fileName,
                                          const std::vector<std::string>& sims)
   {
   vector<string> sections = sims;
   if (sections.size() == 0)
      {
      ApsimControlFile con(fileName);
      con.getAllSectionNames(sections);
      }
   for (unsigned s = 0; s != sections.size(); s++)
      addSimulation(fileName, sections[s]);
   }
//---------------------------------------------------------------------------
// Add a specific simulation to the pending list of runs.
//---------------------------------------------------------------------------
void ApsimRuns::addSimulation(const std::string& fileName, const std::string& simName)
   {
   fileNames.push_back(fileName);
   simNames.push_back(simName);
   }
//---------------------------------------------------------------------------
// Get a list of control files that need converting.
//---------------------------------------------------------------------------
void ApsimRuns::getFilesNeedingConversion(std::vector<std::string>& filesNeedingConversion)
   {
   for (unsigned f = 0; f != fileNames.size(); f++)
      {
      string fileName = fileNames[f];
      if (Path(fileName).Get_extension() == ".con"
          && ControlFileConverter::needsConversion(fileName))
         filesNeedingConversion.push_back(fileName);
      }
   }
//---------------------------------------------------------------------------
// Perform all Apsim runs.
//---------------------------------------------------------------------------
void ApsimRuns::runApsim(bool quiet, bool console, TApsimRunEvent notifyEvent)
   {
   ApsimSettings settings;
   if (quiet)
      settings.write("Apsim|Quiet", "true");
   else
      settings.write("Apsim|Quiet", "false");

   bool continueWithRuns = true;
   for (unsigned f = 0; f != fileNames.size() && continueWithRuns; f++)
      {
      Path filePath(fileNames[f]);
      try
         {
         if (filePath.Get_extension() == ".con")
            {
            SimCreator simCreator(filePath.Get_path());
            simCreator.createSim(simNames[f], "");
            filePath.Set_extension(".sim");
            }
         bool moreToGo = (f != fileNames.size()-1);
         continueWithRuns = performRun(filePath.Get_path(), moreToGo, console, notifyEvent);
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
bool ApsimRuns::performRun(const std::string& simFileName, bool moreToGo, bool console,
                           TApsimRunEvent notifyEvent)
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
      if (notifyEvent != NULL)
         notifyEvent(caption.c_str());

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
void ApsimRuns::createSims(void)
   {
   string previousFileName;
   if (fileNames.size() > 0)
      previousFileName = fileNames[0];

   vector<string> simulations;
   for (unsigned f = 0; f != fileNames.size(); f++)
      {
      string fileName = fileNames[f];
      if (fileName == previousFileName)
         simulations.push_back(simNames[f]);
      else
         {
         if (Path(previousFileName).Get_extension() == ".con")
            {
            SimCreator simCreator(previousFileName);
            simCreator.createSims(simulations, "", (TSimCreatorEvent)NULL);
            }
         simulations.erase(simulations.begin(), simulations.end());
         simulations.push_back(simNames[f]);
         previousFileName = fileName;
         }
      }
   }
//---------------------------------------------------------------------------
// Convert all Apsim runs if necessary.
//---------------------------------------------------------------------------
void ApsimRuns::convertFiles()
   {
   ControlFileConverter converter;

   for (unsigned f = 0; f != fileNames.size(); f++)
      {
      Path filePath(fileNames[f]);
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


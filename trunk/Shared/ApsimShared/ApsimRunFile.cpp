//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "ApsimRunFile.h"
#include "ApsimSimulationFile.h"
#include "ApsimControlFile.h"
#include <general\IniFile.h>

using namespace std;
#pragma package(smart_init)

static const char* SIMULATION_FILE_KEY = "Simulation_file";
static const char* CONFIGURATION_FILE_KEY  = "Configuration_file";
static const char* SIMULATION_KEY   = "Simulation";

// ------------------------------------------------------------------
// Constructor
// ------------------------------------------------------------------
ApsimRunFile::ApsimRunFile(const std::string& file)
   : fileName(file)
   {
   }

// ------------------------------------------------------------------
// Run this run file
// ------------------------------------------------------------------
void ApsimRunFile::run(bool console) const
   {
   IniFile ini(fileName);
   vector<string> sections;
   ini.readSectionNames(sections);
   for (vector<string>::iterator sectionI = sections.begin();
                                 sectionI != sections.end();
                                 sectionI++)
      {
      string simulationFileName, configFileName;
      vector<string> simulations;

      ini.read(*sectionI, SIMULATION_FILE_KEY, simulationFileName);
      ini.read(*sectionI, CONFIGURATION_FILE_KEY, configFileName);
      ini.read(*sectionI, SIMULATION_KEY, simulations);

      if (ExtractFileExt(simulationFileName.c_str()).AnsiCompareIC(".sim") == 0)
         {
         ApsimSimulationFile::run(simulationFileName, console);
         }
      else
         {
         if(simulations.size() == 0)
            ApsimControlFile::getAllSectionNames(simulationFileName, simulations);
         for (vector<string>::iterator sim = simulations.begin();
                                       sim != simulations.end();
                                       sim++)
            {
            ApsimControlFile simulation(simulationFileName, *sim);
            HANDLE childProcess = simulation.run(configFileName, console);
            DWORD exitCode;
            GetExitCodeProcess(childProcess, &exitCode);
            while (exitCode == STILL_ACTIVE)
               {
               Application->ProcessMessages();
               GetExitCodeProcess(childProcess, &exitCode);
               }
            }
         }
      }
   }



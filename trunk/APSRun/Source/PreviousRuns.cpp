//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "PreviousRuns.h"
#include <ApsimShared\ApsimSettings.h>
using namespace std;

static const char* NUM_REMEMBERED_RUNS = "most_recent_apsim_runs|num_apsim_runs_remembered";
static const char* RUN_KEY = "most_recent_apsim_runs|run";
static const char* SIMULATION_KEY = "simulation";

// ------------------------------------------------------------------
// Constructor
// ------------------------------------------------------------------
PreviousRuns::PreviousRuns(void)
   {
   // get the maximum number of runs to remember in the .ini file.
   string st;
   ApsimSettings settings;
   settings.read(NUM_REMEMBERED_RUNS, st);
   if (st == "")
      maxNumRememberedRuns = 10;
   else
      maxNumRememberedRuns = atoi(st.c_str());

   // get the current remembered runs.
   settings.read(RUN_KEY, rememberedRuns);
   }
// ------------------------------------------------------------------
// Get the details of a specified run if possible.  Returns
// true if found.
// ------------------------------------------------------------------
bool PreviousRuns::getPreviousRun(const string& controlFilename,
                                  vector<string>&simulationNames)
   {
   RememberedRuns::iterator i = find(rememberedRuns.begin(),
                                     rememberedRuns.end(),
                                     controlFilename);
   if (i != rememberedRuns.end())
      {
      ApsimSettings settings;
      string simulationKey = controlFilename + "|" + SIMULATION_KEY;
      settings.read(simulationKey, simulationNames);
      return true;
      }
   else
      return false;
   }

// ------------------------------------------------------------------
// Set the most recent run to the one passed into this routine.
// ------------------------------------------------------------------
void PreviousRuns::setCurrentRun(const string& controlFilename,
                                 const vector<string>&simulationNames)
   {
   ApsimSettings settings;

   // Write the control file section to the .ini file.
   string simulationKey = controlFilename + "|" + SIMULATION_KEY;
   settings.write(simulationKey, simulationNames);

   // if this control file name isn't in the current list of
   // remembered runs, then add it to the front of the list.
   // If the number of remembered runs is > maxNumRememberedRuns
   // then drop one off the list and delete its section from the
   // ini file.
   RememberedRuns::iterator i = find(rememberedRuns.begin(),
                                     rememberedRuns.end(),
                                     controlFilename);
   if (i == rememberedRuns.end())
      {

      if (rememberedRuns.size() >= maxNumRememberedRuns)
         {
         i--;
         string controlFileToRemove = *i;
         rememberedRuns.erase(i);
         settings.deleteSection(controlFileToRemove);
         }
      }
   else
      {
      // remove it from the list
      rememberedRuns.erase(i);
      }

   // write all remembered runs to .ini file.
   vector<string> newRuns;
   newRuns.push_back(controlFilename);
   copy(rememberedRuns.begin(), rememberedRuns.end(), back_inserter(newRuns));
   settings.write(RUN_KEY, newRuns);
   }



//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "PreviousRuns.h"
#include <aps\apsuite.h>
using namespace std;

static const char* PREVIOUS_RUN_SECTION = "most_recent_apsim_runs";
static const char* NUM_REMEMBERED_RUNS = "num_apsim_runs_remembered";
static const char* RUN_KEY = "run";
static const char* CONFIGURATION_KEY = "configuration";
static const char* SIMULATION_KEY = "simulation";

// ------------------------------------------------------------------
//  Short description:
//     constructor

//  Notes:

//  Changes:
//    DPH 11/7/2001

// ------------------------------------------------------------------
PreviousRuns::PreviousRuns(void)
   {
   // get the maximum number of runs to remember in the .ini file.
   string st;
   APSConfig().Read("apsuite", PREVIOUS_RUN_SECTION, NUM_REMEMBERED_RUNS, st);
   if (st == "")
      maxNumRememberedRuns = 10;
   else
      maxNumRememberedRuns = atoi(st.c_str());

   // get the current remembered runs.
   readList(PREVIOUS_RUN_SECTION, RUN_KEY, rememberedRuns);
   }
// ------------------------------------------------------------------
//  Short description:
//     read a list of key values from the .ini file for the
//     specified application, section and key names.

//  Notes:

//  Changes:
//    DPH 11/7/2001

// ------------------------------------------------------------------
void PreviousRuns::readList(const string& sectionName,
                            const string& keyName,
                            vector<string>& values)
   {
   string sts[MAX_NUM_KEYS];
   int numSts;
   APSConfig().Read_list("apsuite",
                         sectionName.c_str(),
                         keyName.c_str(),
                         sts, numSts);
   for (int i = 0; i < numSts; i++)
      values.push_back(sts[i]);
   }
// ------------------------------------------------------------------
//  Short description:
//     write a list of key values to the .ini file for the
//     specified application, section and key names.

//  Notes:

//  Changes:
//    DPH 11/7/2001

// ------------------------------------------------------------------
void PreviousRuns::writeList(const string& sectionName,
                             const string& keyName,
                             vector<string>& values)
   {
   string sts[MAX_NUM_KEYS];
   for (unsigned int i = 0; i < values.size(); i++)
      sts[i] = values[i];
   APSConfig().Write_list ("apsuite", sectionName.c_str(), keyName.c_str(),
                           sts, values.size());
   }
// ------------------------------------------------------------------
//  Short description:
//     get the details of a specified run if possible.  Returns
//     true if found.

//  Notes:

//  Changes:
//    DPH 11/7/2001

// ------------------------------------------------------------------
bool PreviousRuns::getPreviousRun(const string& controlFilename,
                                 string& configurationName,
                                 vector<string>&simulationNames)
   {
   RememberedRuns::iterator i = find(rememberedRuns.begin(),
                                     rememberedRuns.end(),
                                     controlFilename);
   if (i != rememberedRuns.end())
      {
      APSConfig().Read("apsuite", controlFilename.c_str(), CONFIGURATION_KEY,
                       configurationName);

      // get the current remembered runs.
      readList(controlFilename.c_str(), SIMULATION_KEY, simulationNames);
      return true;
      }
   else
      return false;
   }

// ------------------------------------------------------------------
//  Short description:
//     set the most recent run to the one passed into this routine.

//  Notes:

//  Changes:
//    DPH 11/7/2001

// ------------------------------------------------------------------
void PreviousRuns::setCurrentRun(const string& controlFilename,
                                 const string& configurationName,
                                 const vector<string>&simulationNames)
   {
   // Write the control file section to the .ini file.
   APSConfig().Write("apsuite", controlFilename.c_str(),
                     CONFIGURATION_KEY, configurationName.c_str());
   writeList(controlFilename.c_str(), string(SIMULATION_KEY), simulationNames);

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
         APSConfig().Delete_section("apsuite", controlFileToRemove.c_str());
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
   writeList(PREVIOUS_RUN_SECTION, RUN_KEY, newRuns);
   }



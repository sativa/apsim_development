//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "PreviousRuns.h"
#include <ApsimShared\ApsimSettings.h>
#include <general\inifile.h>
#include <general\string_functions.h>
using namespace std;

static const char* SECTION = "Apsim Recent Runs";

// ------------------------------------------------------------------
// Constructor
// ------------------------------------------------------------------
PreviousRuns::PreviousRuns(void)
   {
   maxNumRememberedRuns = 10;
   }
// ------------------------------------------------------------------
// Get the details of a specified run if possible.  Returns
// true if found.
// ------------------------------------------------------------------
bool PreviousRuns::getPreviousRun(const string& controlFilename,
                                  vector<string>&simulationNames)
   {
   ApsimSettings settings;
   settings.read(string(SECTION) + "|" + controlFilename, simulationNames);
   return (simulationNames.size() > 0);
   }

// ------------------------------------------------------------------
// Set the most recent run to the one passed into this routine.
// ------------------------------------------------------------------
void PreviousRuns::setCurrentRun(const string& controlFilename,
                                 const vector<string>&simulationNames)
   {
   ApsimSettings settings;

   // Write the control file section to the .ini file.
   string contents;
   settings.readSection(SECTION, contents);

   // Loop through all lines in section, removing any lines matching controlFileName
   // and making sure there are only maxNumRememberedRuns-1 control files.
   istringstream in(contents.c_str());
   ostringstream out;
   string line;
   string previousKey;
   unsigned numSoFar=0;
   while (getline(in, line))
      {
      string key, value;
      getKeyNameAndValue(line, key, value);
      if (key != "" && !Str_i_Eq(key, controlFilename))
         {
         if (!Str_i_Eq(key, previousKey))
            numSoFar++;
         if (numSoFar < maxNumRememberedRuns)
            out << line << endl;
         previousKey = key;
         }
      }

   // Now change the section contents to have our controlFileName first and then
   // the rest of the older control filenames.
   contents = "";
   for (unsigned s = 0; s != simulationNames.size(); s++)
      {
      contents += controlFilename + " = " + simulationNames[s] + "\n";
      }
   contents += out.str();
   settings.writeSection(SECTION, contents);
   }



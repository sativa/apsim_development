//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "SOI.h"
#include <vector>
#include <string>
#include <fstream>
#include <sstream>
#include <general\string_functions.h>
#include <general\stl_functions.h>
#include <general\date_functions.h>
#include <general\db_functions.h>
#include <general\path.h>
#include <general\xml.h>
#include <ApsimShared\ApsimSettings.h>
#include <ApsimShared\ApsimDirectories.h>

using namespace std;

#define SOI_SECTION "soi"
#define SOI_PHASE_FIELD_NAME "SOI Phase"
#define SOI_PHASE_NUMBER_FIELD_NAME "SOI Phase number"
#define PHASE_NAMES_KEY "PhaseNames"

//---------------------------------------------------------------------------
// Create the necessary fields in the result dataset.
//---------------------------------------------------------------------------
void SOI::createFields(TDataSet* source, TDataSet* result)
   {
   result->FieldDefs->Assign(source->FieldDefs);
   addDBField(result, SOI_PHASE_FIELD_NAME, "xxxx");
   }
//---------------------------------------------------------------------------
// Go do our processing, putting all results into 'data'
//---------------------------------------------------------------------------
void SOI::process(TDataSet* source, TDataSet* result)
   {
   string soiFilename = getProperty("filename");
   bool getSOIFromSource = Str_i_Eq("getSOIFromSource", "yes");
   unsigned month = longMonthToInt(getProperty("month"));
   vector<string> phaseNamesToKeep = getProperties("Phase");

   allOtherYears = (find_if(phaseNamesToKeep.begin(), phaseNamesToKeep.end(),
                            CaseInsensitiveStringComparison("AllOtherYears"))
                    != phaseNamesToKeep.end());
   if (getSOIFromSource)
      {
      phaseNamesToKeep.erase(phaseNamesToKeep.begin(), phaseNamesToKeep.end());
      phaseNamesToKeep.push_back(AnsiString(source->FieldValues["SoiPhase"]).c_str());
      }

   if (soiFilename == "")
      {
      ApsimSettings settings;
      settings.read("soi|soi file", soiFilename, true);
      }

   // read in all soi data from soi file.
   readSoiData(soiFilename);

   // get the sowing year field name
   string sowYearFieldName = getSowYearFieldName(source);

   int monthToUse;
   if (getSOIFromSource)
      monthToUse = longMonthToInt(AnsiString(source->FieldValues["SoiMonth"]).c_str());

   else
      monthToUse = month;

   // loop through all records.
   source->First();
   while (!source->Eof)
      {
      string currentPhaseName;
      int year = source->FieldValues[sowYearFieldName.c_str()];
      currentPhaseName = getPhase(year, monthToUse);

      if (keepPhase(currentPhaseName, phaseNamesToKeep))
         {
         // add a new record that is identical to the current source record.
         copyDBRecord(source, result);

         result->Edit();
         result->FieldValues[SOI_PHASE_FIELD_NAME] = currentPhaseName.c_str();
         result->Post();
         }

      source->Next();
      }
   }

// ------------------------------------------------------------------
// Look through all the field names and go find a sow_year
// field.
// ------------------------------------------------------------------
string SOI::getSowYearFieldName(TDataSet* data) const throw (runtime_error)
   {
   int i = data->FieldDefs->IndexOf("sow_year");
   if (i == -1)
      {
      i = data->FieldDefs->IndexOf("year");
      if (i == -1)
         throw runtime_error("Cannot find a sow_year column for SOI analysis");
      }
   return data->FieldDefs->Items[i]->Name.c_str();
   }
// ------------------------------------------------------------------
// Read in all soi data from SOI file.  The file is assumed to be in
// the same directory as this DLL.
// ------------------------------------------------------------------
void SOI::readSoiData(const string& soiFilename)
   {
   phaseNames.erase(phaseNames.begin(), phaseNames.end());
   phases.erase(phases.begin(), phases.end());

   string fileName = soiFilename.c_str();
   replaceAll(fileName, "%apsuite", getApsimDirectory());
   if (!FileExists(fileName.c_str()))
      throw runtime_error("Cannot find soi data file: " + string(soiFilename.c_str()));

   // Read in all soi data.
   ifstream in (fileName.c_str());
   string line;
   static const char* defaultPhaseNamesString
      = "Unknown,Negative,Positive,Falling,Rising,Zero";
   Split_string(defaultPhaseNamesString, "," , phaseNames);

   vector<string> words;
   unsigned year,month,phase;
   while (getline(in, line))
      {
      Split_string(line, " ", words);
      if (words.size() == 4)
         {
         year = StrToInt(words[0].c_str());
         month = StrToInt(words[1].c_str());
         phase = StrToInt(words[3].c_str());
         string yearMonth = AnsiString(IntToStr(year) + "/" + IntToStr(month)).c_str();
         phases.insert(Phases::value_type(yearMonth.c_str(), phase));
         }
      }
   }
// ------------------------------------------------------------------
// Return an soi phase name for the specified
// year and month.  Throws runtime_error if not found.
// ------------------------------------------------------------------
string SOI::getPhase(unsigned year, unsigned month)
   {
   string yearMonth = AnsiString(IntToStr(year) + "/" + IntToStr(month)).c_str();
   Phases::iterator phaseI = phases.find(yearMonth.c_str());
   if (phaseI == phases.end())
      throw runtime_error("Cannot find an SOI phase for year/month: " + yearMonth);

   else
      {
      unsigned phase = phaseI->second;
      if (phase < phaseNames.size())
         return phaseNames[phase];
      else
         {
         ostringstream msg;
         msg << "Invalid phase number: " << phase;
         throw runtime_error(msg.str());
         }
      }
   }
// ------------------------------------------------------------------
// Return true if we should keep the specified phase.
// ------------------------------------------------------------------
bool SOI::keepPhase(const string& phaseName, const std::vector<std::string>& phaseNamesToKeep)
   {
   bool keep = (find_if(phaseNamesToKeep.begin(), phaseNamesToKeep.end(),
                        CaseInsensitiveStringComparison(phaseName))
                != phaseNamesToKeep.end());

   if (allOtherYears)
      keep = !keep;
   return keep;
   }


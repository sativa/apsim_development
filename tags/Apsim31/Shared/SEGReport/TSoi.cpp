//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TSOI.h"
#include <vector>
#include <string>
#include <fstream>
#include <sstream>
#include <general\string_functions.h>
#include <general\db_functions.h>
#include <general\path.h>
#include <general\inifile.h>

using namespace std;
#pragma package(smart_init)
#pragma resource "ComponentRegistration.res"  

#define SOI_SECTION "soi"
#define SOI_PHASE_FIELD_NAME "SOI Phase"
#define SOI_PHASE_NUMBER_FIELD_NAME "SOI Phase number"
#define PHASE_NAMES_KEY "PhaseNames"

//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
__fastcall TSOI::TSOI(TComponent* owner)
   : TSEGTable(owner)
   {
   soiMonth = 1;
   soiFilename = "soi.dat";
   }

//---------------------------------------------------------------------------
// destructor
//---------------------------------------------------------------------------
__fastcall TSOI::~TSOI()
   {
   }
//---------------------------------------------------------------------------
// set the 'month' property and refresh all data.
//---------------------------------------------------------------------------
void __fastcall TSOI::setMonth(AnsiString month)
   {
   for (soiMonth = 0; soiMonth < 12; soiMonth++)
      {
      if (LongMonthNames[soiMonth].AnsiCompareIC(month) == 0)
         break;
      }
   if (soiMonth == 12)
      {
      soiMonth = 1;
      ShowMessage("Invalid soi month: " + month);
      }
   else
      {
      soiMonth++;
      refresh();
      }
   }
//---------------------------------------------------------------------------
// return the 'month' property to caller.
//---------------------------------------------------------------------------
AnsiString __fastcall TSOI::getMonth(void)
   {
   return LongMonthNames[soiMonth-1];
   }

//---------------------------------------------------------------------------
// set the 'filename' property and refresh all data.
//---------------------------------------------------------------------------
void __fastcall TSOI::setFilename(AnsiString filename)
   {
   soiFilename = filename;
   refresh();
   }
//---------------------------------------------------------------------------
// Called by our base class to allow us to add any fielddefs we may want to.
// The table will be closed (Active=false) when this routine is called.
//---------------------------------------------------------------------------
void TSOI::createFields(void) throw(runtime_error)
   {
   if (source != NULL)
      {
      FieldDefs->Assign(source->FieldDefs);
      addDBField(this, SOI_PHASE_FIELD_NAME, "xxxx");
      addDBField(this, SOI_PHASE_NUMBER_FIELD_NAME, "1");
      SortFields = getSowYearFieldName(source).c_str();
      }
   }
//---------------------------------------------------------------------------
// Called by our base class to allow us to add records to the table.
// The table is open (Active = true) when this routine is called.
//---------------------------------------------------------------------------
void TSOI::storeRecords(void) throw(runtime_error)
   {
   if (source != NULL)
      {
      // read in all soi data from soi file.
      readSoiData();

      // get the sowing year field name
      string sowYearFieldName = getSowYearFieldName(source);

      // loop through all records.
      source->First();
      while (!source->Eof)
         {
         // add a new record that is identical to the current source record.
         copyDBRecord(source, this);

         int year = FieldValues[sowYearFieldName.c_str()];
         unsigned phase;
         string phaseName;
         getPhase(year, soiMonth, phase, phaseName);
         Edit();
         FieldValues[SOI_PHASE_FIELD_NAME] = phaseName.c_str();
         FieldValues[SOI_PHASE_NUMBER_FIELD_NAME] = phase;
         Post();
         addFactorToSeriesName("soi phase=" + phaseName);

         // append an all years record.
         copyDBRecord(source, this);
         Edit();
         FieldValues[SOI_PHASE_FIELD_NAME] = "All years";
         FieldValues[SOI_PHASE_NUMBER_FIELD_NAME] = 0;
         Post();
         addFactorToSeriesName("soi phase=All years");

         source->Next();
         }
      }
   }

// ------------------------------------------------------------------
// Look through all the field names and go find a sow_year
// field.
// ------------------------------------------------------------------
string TSOI::getSowYearFieldName(TSEGTable* data) const throw (runtime_error)
   {
   if (data->FieldDefs->IndexOf("sow_year") == -1)
      {
      string fieldName = getYearFieldName();
      string msg = "Cannot find a sow_year column.  An SOI analysis cannot\n"
                   "be performed without a sowing year so the field: " + fieldName
                   + "\nwill be used as a sowing year.";
      ::MessageBox(NULL, msg.c_str(), "Warning", MB_ICONINFORMATION | MB_OK);
      return fieldName;
      }
   else
      return "sow_year";
   }
// ------------------------------------------------------------------
// Read in all soi data from SOI file.  The file is assumed to be in
// the same directory as this DLL.
// ------------------------------------------------------------------
void TSOI::readSoiData(void) throw (runtime_error)
   {
   phaseNames.erase(phaseNames.begin(), phaseNames.end());
   phases.erase(phases.begin(), phases.end());

   Path soiFilePath(Application->ExeName.c_str());
   soiFilePath.Set_name(soiFilename.c_str());
   if (!soiFilePath.Exists())
      throw runtime_error("Cannot find soi data file: " + soiFilePath.Get_path());

   // Read the first line of the file - may contain phase names.
   ifstream in (soiFilePath.Get_path().c_str());
   string line;
   getline(in, line);
   string phaseNamesString = getKeyValue(line, PHASE_NAMES_KEY);
   if (phaseNamesString != "")
      {
      Split_string(phaseNamesString, "," , phaseNames);
      getline(in, line);  // get the column header line
      }
   else  // no phase names specified, use default
      {
      static const char* defaultPhaseNamesString
         = "Unknown,Negative,Positive,Falling,Rising,Zero";
      Split_string(defaultPhaseNamesString, "," , phaseNames);
      }

   vector<string> words;
   unsigned year,month,phase;
   while (getline(in, line))
      {
      Split_string(line, " ", words);
      if (words.size() == 3)
         {
         year = StrToInt(words[0].c_str());
         month = StrToInt(words[1].c_str());
         phase = StrToInt(words[2].c_str());
         ostringstream yearMonth;
         yearMonth << year << '/' << month;
         phases.insert(Phases::value_type(yearMonth.str(), phase));
         }
      }
   }


// ------------------------------------------------------------------
// Return an soi phase plus a string representation for the specified
// year and month.  Throws runtime_error if not found.
// ------------------------------------------------------------------
void TSOI::getPhase(unsigned year, unsigned month,
                    unsigned& phase, string& phaseName) throw (runtime_error)
   {
   ostringstream yearMonth;
   yearMonth << year << '/' << month;
   Phases::iterator phaseI = phases.find(yearMonth.str());
   if (phaseI == phases.end())
      throw runtime_error("Cannot find an SOI phase for year/month: " + yearMonth.str());

   else
      {
      phase = phaseI->second;
      if (phase < phaseNames.size())
         phaseName = phaseNames[phase];
      else
         {
         ostringstream msg;
         msg << "Invalid phase number: " << phase;
         throw runtime_error(msg.str());
         }
      }
   }



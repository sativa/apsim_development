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
#include <ApsimShared\ApsimSettings.h>

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
   ApsimSettings settings;
   string fileName;
   settings.read("soi|soi file", fileName, true);
   soiFilename = fileName.c_str();
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
   if (month != soiMonth)
      {
      if (soiMonth == 12)
         {
         soiMonth = 1;
         ShowMessage("Invalid soi month: " + month);
         }
      else
         {
         soiMonth++;
         forceRefresh();
         }
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
   if (soiFilename != filename)
      {
      soiFilename = filename;
      forceRefresh();
      }
   }
//---------------------------------------------------------------------------
// Set negative phase.
//---------------------------------------------------------------------------
void __fastcall TSOI::setNegativePhase(bool neg)
   {
   if (negativePhase != neg)
      {
      negativePhase = neg;
      forceRefresh();
      }
   }
//---------------------------------------------------------------------------
// Set positive phase.
//---------------------------------------------------------------------------
void __fastcall TSOI::setPositivePhase(bool pos)
   {
   if (positivePhase != pos)
      {
      positivePhase = pos;
      forceRefresh();
      }
   }
//---------------------------------------------------------------------------
// Set the falling phase.
//---------------------------------------------------------------------------
void __fastcall TSOI::setFallingPhase(bool fall)
   {
   if (fallingPhase != fall)
      {
      fallingPhase = fall;
      forceRefresh();
      }
   }
//---------------------------------------------------------------------------
// Set the rising phase.
//---------------------------------------------------------------------------
void __fastcall TSOI::setRisingPhase(bool ris)
   {
   if (risingPhase != ris)
      {
      risingPhase = ris;
      forceRefresh();
      }
   }
//---------------------------------------------------------------------------
// Set the zero phase.
//---------------------------------------------------------------------------
void __fastcall TSOI::setZeroPhase(bool zer)
   {
   if (zeroPhase != zer)
      {
      zeroPhase = zer;
      forceRefresh();
      }
   }
//---------------------------------------------------------------------------
// Called by our base class to allow us to add any fielddefs we may want to.
// The table will be closed (Active=false) when this routine is called.
//---------------------------------------------------------------------------
bool TSOI::createFields(void) throw(runtime_error)
   {
   if (source != NULL)
      {
      FieldDefs->Assign(source->FieldDefs);
      addDBField(this, SOI_PHASE_FIELD_NAME, "xxxx");
      addDBField(this, SOI_PHASE_NUMBER_FIELD_NAME, "1");
      sortFields = getSowYearFieldName(source).c_str();
      return true;
      }
   return false;
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
         int year = source->FieldValues[sowYearFieldName.c_str()];
         unsigned phase;
         string phaseName;
         getPhase(year, soiMonth, phase, phaseName);
         if (keepPhase(phase))
            {
            // add a new record that is identical to the current source record.
            copyDBRecord(source, this);

            Edit();
            FieldValues[SOI_PHASE_FIELD_NAME] = phaseName.c_str();
            FieldValues[SOI_PHASE_NUMBER_FIELD_NAME] = phase;
            Post();
            }

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

   if (!FileExists(soiFilename))
      throw runtime_error("Cannot find soi data file: " + string(soiFilename.c_str()));

   // Read the first line of the file - may contain phase names.
   ifstream in (soiFilename.c_str());
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
         string yearMonth = AnsiString(IntToStr(year) + "/" + IntToStr(month)).c_str();
         phases.insert(Phases::value_type(yearMonth.c_str(), phase));
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
   string yearMonth = AnsiString(IntToStr(year) + "/" + IntToStr(month)).c_str();
   Phases::iterator phaseI = phases.find(yearMonth.c_str());
   if (phaseI == phases.end())
      throw runtime_error("Cannot find an SOI phase for year/month: " + yearMonth);

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
// ------------------------------------------------------------------
// Return true if we should keep the specified phase.
// ------------------------------------------------------------------
bool TSOI::keepPhase(unsigned phase)
   {
   if (negativePhase && phase == 1)
      return true;
   if (positivePhase && phase == 2)
      return true;
   if (fallingPhase && phase == 3)
      return true;
   if (risingPhase && phase == 4)
      return true;
   if (zeroPhase && phase == 5)
      return true;
   return false;
   }
// ------------------------------------------------------------------
// set one of our properties.
// ------------------------------------------------------------------
void TSOI::setProperty(const std::string& propertyName,
                         const std::string& propertyValue)
   {
   if (Str_i_Eq(propertyName, "filename"))
      filename = propertyValue.c_str();
   else if (Str_i_Eq(propertyName, "month"))
      month = propertyValue.c_str();
   else if (Str_i_Eq(propertyName, "negative"))
      negative = Str_i_Eq(propertyValue, "true");
   else if (Str_i_Eq(propertyName, "positive"))
      positive = Str_i_Eq(propertyValue, "true");
   else if (Str_i_Eq(propertyName, "falling"))
      falling = Str_i_Eq(propertyValue, "true");
   else if (Str_i_Eq(propertyName, "rising"))
      rising = Str_i_Eq(propertyValue, "true");
   else if (Str_i_Eq(propertyName, "zero"))
      zero = Str_i_Eq(propertyValue, "true");
   }


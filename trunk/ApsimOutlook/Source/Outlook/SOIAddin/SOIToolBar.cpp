//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "SOIToolBar.h"
#include "TSOI_form.h"
#include <general\string_functions.h>
#include <general\stl_functions.h>
#include <general\path.h>
#include <general\inifile.h>
#include <general\math_functions.h>
#include <Math.hpp>
#include <ApsimShared\ApsimDirectories.h>
#pragma link "TAPSTable"
#pragma link "TAPSTable_Form"
#pragma link "TAPSRecord"
#pragma link "TMultiSTringListForm"
//---------------------------------------------------------------------------

#pragma package(smart_init)

#define SOI_SECTION "soi"
#define TOOLBAR_BITMAP_KEY "Outlook Bitmaps|soi"
#define SOI_FILE_KEY "soi|soi file"
#define SOI_PHASE_FIELD_NAME "SOI Phase"
#define SOI_PHASE_NUMBER_FIELD_NAME "SOI Phase number"
#define PHASE_NAMES_KEY "PhaseNames"

using namespace std;

// static member variable declarations:
TToolButton* SOIToolBar::SOI_button;
Graphics::TBitmap* SOIToolBar::glyph;
int SOIToolBar::glyph_position;
TToolBar* SOIToolBar::Toolbar;


// ------------------------------------------------------------------
//  Short description:
//    Exported function for created an instance of this add-in

//  Changes:
//    DAH 25/7/01

// ------------------------------------------------------------------
extern "C" ToolBarAddInBase* _export __stdcall createToolBarAddIn(const string& parameters)
{
   return new SOIToolBar(parameters);
}

SOIToolBar::SOIToolBar(const string& parameters)
{
   string bitmap_name;
   settings.read(TOOLBAR_BITMAP_KEY, bitmap_name);

   // get stuff from ini file like image name
   string bitmapPath = getAppHomeDirectory() + "\\" + bitmap_name;
   glyph = new Graphics::TBitmap;
   glyph->LoadFromFile(bitmapPath.c_str());

   FPhase_month = 1;
   string data_file;
   settings.read(SOI_FILE_KEY, data_file, true);
   FSOI_data_file = data_file.c_str();

   SOI_form = new TSOI_form(Application->MainForm);
   SOI_form->SOI_ptr = this;

   needs_update = false;
   SOI_enabled = false;
}

SOIToolBar::~SOIToolBar()
{
   delete SOI_form;
   Toolbar->RemoveControl(SOI_button);
   delete SOI_button;
   Toolbar->Images->Delete(glyph_position);
   delete glyph;
}

void SOIToolBar::decorateToolBar(TToolBar* toolbar)
{
   Toolbar = toolbar;
   SOI_button = new TToolButton(toolbar);
   SOI_button->Left = toolbar->Width; // ensures button goes at right end of row
   SOI_button->Parent = toolbar;
   int pos = toolbar->Images->Add(glyph, NULL);
   SOI_button->ImageIndex = pos;
   SOI_button->Hint = "SOI Analysis";
   SOI_button->OnClick = buttonClick;
   glyph_position = pos;
}

void __fastcall SOIToolBar::buttonClick(TObject* Sender)
{
   if (SOI_form->ShowModal()== mrOk)
      needs_update = true;
   else
      needs_update = false;
}



bool SOIToolBar::needsUpdate()
{
   return needs_update;
}


void SOIToolBar::youNeedUpdating()
{
   needs_update = true;
}

// ------------------------------------------------------------------
// Work out what the sow year field name is
// ------------------------------------------------------------------
void SOIToolBar::calcSowYearFieldName(const TAPSRecord& record) throw (runtime_error)
   {
   vector<string> fieldNames = record.getFieldNames();
   for (vector<string>::iterator i = fieldNames.begin();
                                 i != fieldNames.end();
                                 i++)
      {
      string fieldName = *i;
      To_lower(fieldName);
      if (fieldName.find("sow_year") == 0)
         {
         sowYearFieldName = *i;
         return;
         }
      if (fieldName.find("sow year") == 0)
         {
         sowYearFieldName = *i;
         return;
         }
      }
   throw runtime_error("To perform an SOI analysis the data must contain a\n"
                       "'sow_year' field.");
   }

// ------------------------------------------------------------------
// Return a sowing year from the specified record.  Throws a
// runtime_error if not found.
// ------------------------------------------------------------------
unsigned SOIToolBar::getSowYear(const TAPSRecord& record) throw(runtime_error)
   {
   if (sowYearFieldName == "")
      calcSowYearFieldName(record);

   string stringValue = record.getFieldValue(sowYearFieldName);
   return StrToInt(stringValue.c_str());
   }


// ------------------------------------------------------------------
//  Short description:
//      read in all soi data from SOI file.

//  Notes:

//  Changes:
//    DPH 5/2/98
//    DAH 8/8/01  added ability to read Phase names from the dat file header
//                Default phase names are defined here rather than Get_phase

// ------------------------------------------------------------------
void SOIToolBar::Read_all_soi_data (void)
   {
   FPhase_names.erase(FPhase_names.begin(), FPhase_names.end());
   soi_phases.erase(soi_phases.begin(), soi_phases.end());

   ifstream in (FSOI_data_file.c_str());
   string Line;
   getline(in, Line); // get first line, and check if it specs Phase names
   string Phase_names_string = getKeyValue(Line, PHASE_NAMES_KEY);
   if (Phase_names_string != "")
      {
      Split_string(Phase_names_string, "," , FPhase_names);
      getline(in, Line);  // get the column header line
      }
   else  // no phase names specified, use default
      {
      char* default_phases[6] = {"Unknown", "Negative", "Positive", "Falling", "Rising", "Zero"};
      for (int i=0; i<ARRAYSIZE(default_phases); i++)
         FPhase_names.push_back(default_phases[i]);
      }

   soi soi_obj;
   while (!in.eof())
      {
      in >> soi_obj >> ws;
      soi_phases.insert (soi_obj);
      }
}


// ------------------------------------------------------------------
//  Short description:
//      return a particular soi phase plus a string representation
//      of it.  Returns 0 if not found.

//  Notes:

//  Changes:
//    DPH 5/2/98
//    DAH 8/8/01  removed definition of Phase names from here and placed in
//                Read_all_soi_data  (see above). Phase names are now found in
//                the member variable FPhase_names.
//                Changed the condition testing whether or not a phase name is
//                known for a particular phase number. It is assumed that
//                FPhase_names first entry will cover 'unknown' phases, and that
//                all other entries will cover legitimate phases. If a phase number
//                is found that has no corresponding entry in FPhase_names, it is
//                assigned "unknown"

// ------------------------------------------------------------------
void SOIToolBar::Get_phase (int Year, int Month, unsigned& SOI_phase, string& SOI_phase_st)
   {
   soi_set::iterator i = std::find(soi_phases.begin(),
                                   soi_phases.end(),
                                   soi(Year, Month));
   if (i == soi_phases.end())
      {
      SOI_phase = 0;
      SOI_phase_st = "Unknown";
      }
   else
      {
      SOI_phase = max((*i).Phase, 0);
      if (SOI_phase > FPhase_names.size() - 1)
         SOI_phase = 0;
      SOI_phase_st = FPhase_names[SOI_phase];
      }
   }


// ------------------------------------------------------------------
//  Short description:
//      calculate and store all records in memory table.

//  Notes:

//  Changes:
//    DPH 5/2/98
//    DAH 26/7/01 - changed to ToolBarAddin framework

// ------------------------------------------------------------------
void SOIToolBar::doCalculations(TAPSTable& data)
{
   if (SOI_enabled)
   {
      TCursor savedCursor = Screen->Cursor;
      Screen->Cursor = crHourGlass;

      sowYearFieldName = "";
      TAPSTable* new_data;
      vector<TAPSRecord>* allData;
      try
         {
         // read in all soi data from soi file.
         Read_all_soi_data();

         // create a new table with same structure as 'data'
         new_data = new TAPSTable(NULL);
         new_data->copyFieldNamesFrom(data);
         new_data->copyPivotsFrom(data);
         // add new fields required
         new_data->addField(SOI_PHASE_FIELD_NAME);
         new_data->addField(SOI_PHASE_NUMBER_FIELD_NAME);
         new_data->markFieldAsAPivot(SOI_PHASE_FIELD_NAME);

         // create some storage for all phases for all records
         allData = new vector<TAPSRecord>[FPhase_names.size() + 1];
         const unsigned allYearsFieldNum = FPhase_names.size();

         // loop through all datasets.
         unsigned SOI_phase;
         string SOI_phase_st;

         bool ok = data.first();
         while (ok)
            {
            // C421a force getSowYear to locate a sowing year for this datablock.
            sowYearFieldName = "";

            string blockName = data.getDataBlockName();

            // loop through all rows in data array and store the soi value.
            for (vector<TAPSRecord>::const_iterator i = data.begin();
                                                    i != data.end();
                                                    i++)
               {
               TAPSRecord newRecord = *i;
               int Sow_year = getSowYear(*i);
               Get_phase (Sow_year, FPhase_month, SOI_phase, SOI_phase_st);
               newRecord.setFieldValue(SOI_PHASE_FIELD_NAME, SOI_phase_st.c_str());
               newRecord.setFieldValue(SOI_PHASE_NUMBER_FIELD_NAME, IntToStr(SOI_phase).c_str());
               string blockSuffix = string(SOI_PHASE_FIELD_NAME) + "=" + SOI_phase_st;
               string blockForRecord = blockName + ";" + blockSuffix;
               newRecord.setFieldValue("Simulation", blockForRecord.c_str());

               allData[SOI_phase].push_back(newRecord);

               // store all years records.
               newRecord.setFieldValue(SOI_PHASE_FIELD_NAME, "All years");
               blockSuffix = string(SOI_PHASE_FIELD_NAME) + "=" + "All years";
               blockForRecord = blockName + ";" + blockSuffix;
               newRecord.setFieldValue("Simulation", blockForRecord.c_str());

               allData[allYearsFieldNum].push_back(newRecord);
               }

            // goto next dataset.
            ok = data.next();
            }

         // Loop through all the soi phases.  For each, store the vector
         // of numbers in new_data.
         for (unsigned int phase = 0; phase <= allYearsFieldNum; phase++)
            {
            new_data->storeData(allData[phase]);
            }

         // we're finished storing data.
         new_data->endStoringData();

         // copy new_data back into data
         data.storeData(*new_data);
         }
      catch (const runtime_error& err)
         {
         ShowMessage(err.what());
         }
      delete new_data;
      delete [] allData;

      needs_update = false;

      Screen->Cursor = savedCursor;
      }
   }



//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "SOIToolBar.h"
#include "TSOI_form.h"
#include <general\string_functions.h>
#include <general\stl_functions.h>
#include <general\path.h>
#include <general\ini_file.h>
#include <general\math_functions.h>
#include <Math.hpp>

//---------------------------------------------------------------------------

#pragma package(smart_init)

#define SOI_SECTION "soi"
#define SOI_PHASE_FIELD_NAME "SOI Phase"
#define SOI_PHASE_NUMBER_FIELD_NAME "SOI Phase number"

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
   Path p(Application->ExeName.c_str());
   p.Set_extension(".ini");
   Ini_file ini;
   ini.Set_file_name (p.Get_path().c_str());

   string bitmap_name;
   ini.Read (SOI_SECTION, "toolbitmap", bitmap_name);

   FPhase_month = 1;
   string data_file;
   ini.Read (SOI_SECTION, "soi file", data_file);
   FSOI_data_file = data_file.c_str();


   // get stuff from ini file like image name
   Path bitmap_path(Application->ExeName.c_str());
   bitmap_path.Set_name (bitmap_name.c_str());
   glyph = new Graphics::TBitmap;
   glyph->LoadFromFile(bitmap_path.Get_path().c_str());

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
//  Short description:
//      return the name of the sow year field name.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
string SOIToolBar::Get_sow_year_field_name(TAPSTable& data)
   {
   vector<string> Field_names;
   data.getFieldNames (Field_names);
   for (vector<string>::iterator i = Field_names.begin();
                                 i != Field_names.end();
                                 i++)
      {
      string field_name (*i);
      To_lower(field_name);
      if (field_name.find("sow") != string::npos &&
          field_name.find("year") != string::npos)
         return *i;
      }
   return "year";
   }


// ------------------------------------------------------------------
//  Short description:
//      read in all soi data from SOI file.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void SOIToolBar::Read_all_soi_data (void)
   {
   ifstream in (FSOI_data_file.c_str());
   string Line;
   getline(in, Line);

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

// ------------------------------------------------------------------
void SOIToolBar::Get_phase (int Year, int Month, int& SOI_phase, string& SOI_phase_st)
   {
   static const char* Phase_names[6] =
      {"Unknown", "Negative", "Positive", "Falling", "Rising", "Zero"};

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
      if (SOI_phase > 5)
         SOI_phase = 0;
      SOI_phase_st = Phase_names[SOI_phase];
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

      // read in all soi data from soi file.
      Read_all_soi_data();

      // get the sowing year field name
      string Sow_year_field_name = Get_sow_year_field_name(data);

      // create a new table with same structure as 'data'
      TAPSTable* new_data = new TAPSTable(NULL);
      new_data->copyFieldNamesFrom(data);
      new_data->copyPivotsFrom(data);
      // add new fields required
      new_data->addField(SOI_PHASE_FIELD_NAME);
      new_data->addField(SOI_PHASE_NUMBER_FIELD_NAME);
      new_data->markFieldAsAPivot(SOI_PHASE_FIELD_NAME);

      // loop through all datasets.
      int SOI_phase;
      string SOI_phase_st;

      bool ok = data.first();
      while (ok)
         {
         string blockName = data.getDataBlockName();

         // loop through all rows in data array and store the soi value.
         for (vector<TAPSRecord>::const_iterator i = data.begin();
                                                 i != data.end();
                                                 i++)
            {
            TAPSRecord newRecord = *i;
            int Sow_year = atoi((*i).getFieldValue(Sow_year_field_name.c_str()).c_str());
            Get_phase (Sow_year, FPhase_month, SOI_phase, SOI_phase_st);
            newRecord.setFieldValue(SOI_PHASE_FIELD_NAME, SOI_phase_st.c_str());
            newRecord.setFieldValue(SOI_PHASE_NUMBER_FIELD_NAME, IntToStr(SOI_phase).c_str());
            string blockSuffix = string(SOI_PHASE_FIELD_NAME) + "=" + SOI_phase_st;
            string blockForRecord = blockName + ";" + blockSuffix;
            newRecord.setFieldValue("Simulation", blockForRecord.c_str());

            new_data->storeRecord(newRecord);

            // store all years records.
            newRecord.setFieldValue(SOI_PHASE_FIELD_NAME, "All years");
            blockSuffix = string(SOI_PHASE_FIELD_NAME) + "=" + "All years";
            blockForRecord = blockName + ";" + blockSuffix;
            newRecord.setFieldValue("Simulation", blockForRecord.c_str());
            new_data->storeRecord(newRecord);
            }


         // goto next dataset.
         ok = data.next();
         }

      // we're finished storing data.
      new_data->addSortField(data.getYearFieldName());
      new_data->endStoringData();

      // copy new_data back into data
      data.storeData(*new_data);
      
      needs_update = false;

      Screen->Cursor = savedCursor;
   }
}



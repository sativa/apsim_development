//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "TPreferences_form.h"
#include <general\ini_file.h>
#include <general\path.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "ToolEdit"
#pragma resource "*.dfm"
TPreferences_form *Preferences_form;

static const char* SOI_SECTION = "soi";
static const char* SOI_FILE_KEY_WORD = "soi file";
static const char* OPTIONS_SECTION = "options";
//---------------------------------------------------------------------------
__fastcall TPreferences_form::TPreferences_form(TComponent* Owner)
   : TForm(Owner)
   {
   }
//---------------------------------------------------------------------------
void __fastcall TPreferences_form::FormShow(TObject *Sender)
   {
   // read a list of directory names from .ini file.
   Path p(Application->ExeName.c_str());
   p.Set_extension(".ini");
   Ini_file Ini;
   Ini.Set_file_name(p.Get_path().c_str());
   string SOI_file_name;
   Ini.Read (SOI_SECTION, SOI_FILE_KEY_WORD, SOI_file_name);
   File_name_edit->Text = SOI_file_name.c_str();

   string St;
   Ini.Read (OPTIONS_SECTION, "colour_background", St);
   ColourBackgroundCheckbox->Checked = !Str_i_Eq(St, "off");
   }
//---------------------------------------------------------------------------
void __fastcall TPreferences_form::FormClose(TObject *Sender,
      TCloseAction &Action)
   {
   if (ModalResult == mrOk)
      {
      // read a list of directory names from .ini file.
      Path p(Application->ExeName.c_str());
      p.Set_extension(".ini");
      Ini_file Ini;
      Ini.Set_file_name(p.Get_path().c_str());
      Ini.Write (SOI_SECTION, SOI_FILE_KEY_WORD, File_name_edit->Text.c_str());
      if (ColourBackgroundCheckbox->Checked)
         Ini.Write(OPTIONS_SECTION, "colour_background", "on");
      else
         Ini.Write(OPTIONS_SECTION, "colour_background", "off");

     }
   }


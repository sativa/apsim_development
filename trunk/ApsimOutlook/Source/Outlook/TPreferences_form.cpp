//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TPreferences_form.h"
#include <general\path.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "AdvEdBtn"
#pragma link "AdvEdit"
#pragma link "AdvFileNameEdit"
#pragma resource "*.dfm"
TPreferences_form *Preferences_form;

static const char* SOI_KEY = "soi|soi file";
static const char* COLOUR_KEY = "Outlook|colour_background";
//---------------------------------------------------------------------------
__fastcall TPreferences_form::TPreferences_form(TComponent* Owner)
   : TForm(Owner)
   {
   }
//---------------------------------------------------------------------------
void __fastcall TPreferences_form::FormShow(TObject *Sender)
   {
   // make the soi stuff not visible when SOI is not in the skin.
   string soi;
   settings.read("Outlook Skin|soi", soi);
   SOILabel->Visible = !Str_i_Eq(soi, "off");
   File_name_edit->Visible = !Str_i_Eq(soi, "off");
   
   // read a list of directory names from .ini file.
   string SOI_file_name;
   settings.read (SOI_KEY, SOI_file_name);
   File_name_edit->Text = SOI_file_name.c_str();

   string St;
   settings.read (COLOUR_KEY, St);
   ColourBackgroundCheckbox->Checked = !Str_i_Eq(St, "off");
   }
//---------------------------------------------------------------------------
void __fastcall TPreferences_form::FormClose(TObject *Sender,
      TCloseAction &Action)
   {
   if (ModalResult == mrOk)
      {
      // read a list of directory names from .ini file.
      settings.write(SOI_KEY, File_name_edit->Text.c_str());
      if (ColourBackgroundCheckbox->Checked)
         settings.write(COLOUR_KEY, "on");
      else
         settings.write(COLOUR_KEY, "off");

     }
   }


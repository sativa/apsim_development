//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "About.h"
#include <general\path.h>
//---------------------------------------------------------------------
#pragma resource "*.dfm"
TAboutBox *AboutBox;
//---------------------------------------------------------------------
__fastcall TAboutBox::TAboutBox(TComponent *Owner)
	: TForm(Owner)
   {
   // change caption.
   Path p(Application->ExeName.c_str());
   if (Str_i_Eq(p.Get_name_without_ext(), "whoppercropper"))
      {
      Label1->Caption = "Whopper Cropper";
      Label3->Caption = "Version 2.0";
      }
   }
//---------------------------------------------------------------------


//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "TFileEdit.h"

#pragma package(smart_init)
#pragma resource "*.res"
//---------------------------------------------------------------------------
// ValidCtrCheck is used to assure that the components created do not have
// any pure virtual functions.
//

static inline void ValidCtrCheck(TFileEdit *)
{
   new TFileEdit(NULL);
}
//---------------------------------------------------------------------------
namespace Tfileedit
{
   void __fastcall PACKAGE Register()
   {
      TComponentClass classes[1] = {__classid(TFileEdit)};
      RegisterComponents("APSRU", classes, 0);
   }
}

// ------------------------------------------------------------------
//  Short description:
//      constructor

//  Notes:

//  Changes:
//    DPH 23/10/98

// ------------------------------------------------------------------
__fastcall TFileEdit::TFileEdit(TComponent* Owner)
   : TRichEdit(Owner)
   {
   PlainText = true;
   WordWrap = true;
   }

// ------------------------------------------------------------------
//  Short description:
//      set the filename property

//  Notes:

//  Changes:
//    DPH 23/10/98

// ------------------------------------------------------------------
void __fastcall TFileEdit::SetFileName (AnsiString FileName)
   {
   Write();
   FFileName = FileName;
   Read();
   }

// ------------------------------------------------------------------
//  Short description:
//      Refresh the control.

//  Notes:

//  Changes:
//    DPH 23/10/98

// ------------------------------------------------------------------
void TFileEdit::Read (void)
   {
   if (FileName != "")
      {
      if (FileExists(FileName))
         {
         Lines->LoadFromFile(FileName);
         ReadOnly = ( (GetFileAttributes(FileName.c_str()) & FILE_ATTRIBUTE_READONLY) != 0);
         }
      else
         {
         AnsiString msg = "Cannot find file : " + FileName;
         Application->MessageBox(msg.c_str(), "Error", MB_ICONSTOP | MB_OK);
         Lines->Clear();
         ReadOnly = true;
         }
      }
   else
      Lines->Clear();
   }

// ------------------------------------------------------------------
//  Short description:
//      Refresh the control.

//  Notes:

//  Changes:
//    DPH 23/10/98

// ------------------------------------------------------------------
void TFileEdit::Write (void)
   {
   if (FileName != "" && ReadOnly == false)
      Lines->SaveToFile(FileName);
   }

// ------------------------------------------------------------------
//  Short description:
//      Focus has shifted away from us to another control on the parent
//      form.

//  Notes:

//  Changes:
//    DPH 23/10/98

// ------------------------------------------------------------------
void __fastcall TFileEdit::KillFocus (TMessage& msg)
   {
   Write();
   TRichEdit::Dispatch(&msg);
   }

// ------------------------------------------------------------------
//  Short description:
//      Position the text to the specified text string and select it.

//  Notes:

//  Changes:
//    DPH 23/10/98

// ------------------------------------------------------------------
void TFileEdit::Goto (const char* Search_text)
   {
   int StartPos = 0;
   int ToEnd = Text.Length();

   int FoundAt = FindText(Search_text, StartPos, ToEnd, TSearchTypes()<< stWholeWord);
   if (FoundAt != -1)
      {
      SetFocus();
      SelStart = FoundAt;
      SelLength = strlen(Search_text);
      SendMessage(Handle, EM_LINESCROLL, 0, 20);
      SendMessage(Handle, EM_SCROLLCARET, 0, 0);
      }
   }



/****************************************************************************\

    TPopupForm.cpp -- Created by Damon Chandler <dmc27@cornell.edu>

\****************************************************************************/

//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "PopupFormUnit.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TPopupForm *PopupForm;
//---------------------------------------------------------------------------

__fastcall TPopupForm::TPopupForm(TComponent* Owner)
   : TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TPopupForm::CreateParams(TCreateParams& Params)
{
   TForm::CreateParams(Params);
   Params.Style &= ~(WS_CAPTION | WS_SIZEBOX | WS_POPUP);
   Params.Style |= WS_CHILD | WS_BORDER;   
   Params.ExStyle |= WS_EX_PALETTEWINDOW;
   Params.WindowClass.style |= CS_SAVEBITS;
}
//---------------------------------------------------------------------------

void __fastcall TPopupForm::CreateWnd()
{
   TForm::CreateWnd();
   ::SetParent(Handle, GetDesktopWindow());
}
//---------------------------------------------------------------------------

void __fastcall TPopupForm::VisibleChanging()
{
   TForm::VisibleChanging();
   if (Visible) // if the form is being hidden
   {
      ReleaseCapture();
   }
   else // if the form is being shown
   {
      if (ActiveControl)
      {
         SetCapture(ActiveControl->Handle);
         SNDMSG(ActiveControl->Handle, WM_SETFOCUS, 0, 0);
      }
      else SetCapture(Handle);
   }
}
//---------------------------------------------------------------------------

void __fastcall TPopupForm::CMMouseEnter(TMessage& AMsg)
{
   // when the cursor is within the bounds of the
   // window, release the mouse capture
   ReleaseCapture();
}
//---------------------------------------------------------------------------

void __fastcall TPopupForm::CMMouseLeave(TMessage& AMsg)
{
   // when the cursor goes beyond the bounds of the
   // window, give mouse capture to the window
   if (Visible)
   {
      if (ActiveControl)
      {
         SetCapture(ActiveControl->Handle);
      }
      else SetCapture(Handle);
   }
}
//---------------------------------------------------------------------------

void __fastcall TPopupForm::WMActivateApp(TMessage& AMsg)
{
   // if deactivating...
   if (!AMsg.WParam)
   {
      // close the form
      Close();
   }
}
//---------------------------------------------------------------------------

void __fastcall TPopupForm::FormMouseDown(TObject *Sender,
   TMouseButton Button, TShiftState Shift, int X, int Y)
{
   // if the specified point is beyond
   // the bounds of the form...
   if (X < 0 || X >= Width ||
       Y < 0 || Y >= Height)
   {
      // close the form
      Close();
   }
}
//---------------------------------------------------------------------------



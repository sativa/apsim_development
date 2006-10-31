#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "ApsimForms.h"
#include "TAboutForm.h"
#include "TSplashForm.h"

#pragma package(smart_init)

//---------------------------------------------------------------------------
// Display an about box.
//---------------------------------------------------------------------------
void _export displayAboutBox(void)
   {
   TAboutForm* form = new TAboutForm(Application);
   form->ShowModal();
   delete form;
   }

//---------------------------------------------------------------------------
// Display a splash as a modeless form.
//---------------------------------------------------------------------------
void _export displaySplashForm(void)
   {
   TSplashForm* form = new TSplashForm(Application);
   form->Show();
   Application->ProcessMessages();
   }


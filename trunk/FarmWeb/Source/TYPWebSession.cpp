//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TYPWebSession.h"
#include "TYPPaddockForm.h"
#include "TYPSetupForm.h"
#include "TYPNitrogenReportForm.h"
#include "utilities.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "TWebSession"
#pragma resource "*.dfm"
TYPWebSession *YPWebSession;
//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
__fastcall TYPWebSession::TYPWebSession(TComponent* Owner)
   : TWebSession(Owner)
   {
   paddockForm = NULL;
   ypsetupForm = NULL;
   nitrogenReportForm = NULL;
   }
//---------------------------------------------------------------------------
// destructor
//---------------------------------------------------------------------------
__fastcall TYPWebSession::~TYPWebSession()
   {
   delete paddockForm;
   }
//---------------------------------------------------------------------------
// Show the paddock form.
//---------------------------------------------------------------------------
void TYPWebSession::showPaddockForm(const std::string& userName,
                                    const string& paddockName,
                                    bool readOnly,
                                    bool fromGrowerManagement)
   {
   try
      {
      if (paddockForm == NULL)
         {
         paddockForm = new TYPPaddockForm(webApplication);
         setupForm(paddockForm);
         }
      if (data->userIsOfType(userName, Data::user))
         paddockForm->setup(this, data, userName, paddockName, fromGrowerManagement);
      else
         paddockForm->setup(this, data, userName, paddockName, fromGrowerManagement);
      show(paddockForm);
      }
   catch (const exception& err)
      {
      showMessage(err.what());
      }
   }
//---------------------------------------------------------------------------
// Show the paddock setup form.
//---------------------------------------------------------------------------
void TYPWebSession::showSetupForm(const std::string& userName,
                                  const string& paddockName,
                                  bool readOnly,
                                  bool fromGrowerManagement)
   {
   try
      {
      if (ypsetupForm == NULL)
         {
         ypsetupForm = new TYPSetupForm(webApplication);
         setupForm(ypsetupForm);
         }
      ypsetupForm->setup(this, data, userName, paddockName, fromGrowerManagement);
      show(ypsetupForm);
      }
   catch (const exception& err)
      {
      showMessage(err.what());
      }
   }
//---------------------------------------------------------------------------
// Show the nitrogen report form.
//---------------------------------------------------------------------------
void TYPWebSession::showNitrogenReportForm(const string& userName,
                                           const string& paddockName,
                                           TReportCallback callback)
   {
   try
      {
      if (nitrogenReportForm == NULL)
         nitrogenReportForm = new TYPNitrogenReportForm(webApplication);

      nitrogenReportForm->setup(this, data, userName, paddockName, callback);
      show(nitrogenReportForm);
      }
   catch (const exception& err)
      {
      showMessage(err.what());
      }
   }



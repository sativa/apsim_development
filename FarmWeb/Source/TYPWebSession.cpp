//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TYPWebSession.h"
#include "TYPPaddockForm.h"
#include "TYPSetupForm.h"
#include "TSoilsForm.h"
#include "TYPNitrogenReportForm.h"
#include "TMetStationForm.h"
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
   soilsForm = NULL;
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
         paddockForm->setup(this, data, userName, paddockName, readOnly, fromGrowerManagement);
      else
         paddockForm->setup(this, data, userName, paddockName, false, fromGrowerManagement);
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
      ypsetupForm->setup(this, data, userName, paddockName, readOnly, fromGrowerManagement);
      show(ypsetupForm);
      }
   catch (const exception& err)
      {
      showMessage(err.what());
      }
   }
//---------------------------------------------------------------------------
// Show the soils form.
//---------------------------------------------------------------------------
void TYPWebSession::showSoilsForm()
   {
   try
      {
      if (soilsForm == NULL)
         {
         soilsForm = new TSoilsForm(webApplication);
         setupForm(soilsForm);
         }
      soilsForm->setup(this, data);
      show(soilsForm);
      }
   catch (const exception& err)
      {
      showMessage(err.what());
      }
   }
//---------------------------------------------------------------------------
// Show the met stations form.
//---------------------------------------------------------------------------
void TYPWebSession::showMetStationsForm()
   {
   try
      {
      if (metStationForm == NULL)
         {
         metStationForm = new TMetStationForm(webApplication);
         setupForm(metStationForm);
         }

      metStationForm->setup(this, data);
      show(metStationForm);
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
                                           const std::string& emailAddress)
   {
   try
      {
      if (nitrogenReportForm == NULL)
         nitrogenReportForm = new TYPNitrogenReportForm(webApplication);

      nitrogenReportForm->setup(this, data, userName, paddockName, emailAddress);
      show(nitrogenReportForm);
      }
   catch (const exception& err)
      {
      showMessage(err.what());
      }
   }
//---------------------------------------------------------------------------
// setup the specified form.
//---------------------------------------------------------------------------
int TYPWebSession::setupForm(TIWAppForm* form)
   {
   int itemIndex = TWebSession::setupForm(form);
   if (!data->userIsOfType(userName, Data::user))
      {
      drawMenuItem(form, itemIndex, "Soils", "soils.gif", onMenuItemClick);
      drawMenuItem(form, itemIndex+1, "Met Stations", "metstations.gif", onMenuItemClick);
      return itemIndex+2;
      }
   else
      return itemIndex;
   }
//---------------------------------------------------------------------------
// Process the bar item click event.
//---------------------------------------------------------------------------
void TYPWebSession::processClickItem(AnsiString caption)
   {
   if (caption == "Soils")
      showSoilsForm();
   else if (caption == "Met Stations")
      showMetStationsForm();
   else
      TWebSession::processClickItem(caption);
   }



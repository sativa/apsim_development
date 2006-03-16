//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TAfloPaddockForm.h"
#include "Data.h"
#include "TWebSession.h"
#include "Utilities.h"
#include <general\vcl_functions.h>
#include <general\string_functions.h>
#include "Dir.h"
using namespace boost::gregorian;
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "IWBaseControl"
#pragma link "IWCompLabel"
#pragma link "IWControl"
#pragma link "IWVCLBaseControl"
#pragma link "IWOutlookBar"
#pragma link "IWCompEdit"
#pragma link "IWCompListbox"
#pragma link "IWTMSCal"
#pragma link "IWCompButton"
#pragma link "IWTMSCtrls"
#pragma link "IWBaseHTMLControl"
#pragma link "IWCompRectangle"
#pragma link "IWCompCheckbox"
#pragma link "IWExtCtrls"
#pragma link "IWHTMLControls"
#pragma resource "*.dfm"

//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
__fastcall TAfloPaddockForm::TAfloPaddockForm(TComponent* Owner)
        : TIWAppForm(Owner)
   {
   }
//---------------------------------------------------------------------------
// setup this form.
//---------------------------------------------------------------------------
void TAfloPaddockForm::setup(TWebSession* session,
                         Data* d,
                         const string& userN,
                         const string& paddockN,
                         bool readOnly)
   {
   webSession = session;
   data = d;
   userName = userN;
   paddockName = paddockN;

   UserLabel->Text = "Paddock details for user xxx and paddock yyy.";
   UserLabel->Text = StringReplace(UserLabel->Text, "xxx", data->getNameOfUser(userName).c_str(), TReplaceFlags());
   UserLabel->Text = StringReplace(UserLabel->Text, "yyy",  paddockName.c_str(), TReplaceFlags());

   populateStationNumberCombo(data, userName, paddockName, "All", WeatherStationCombo);
   populateSoilTypeCombo(data, userName, paddockName, "All", SoilTypeCombo);
   populateDatePicker(PlantingDate, data, userName, paddockName, "sowdate");
   populateCombo(CultivarCombo, data, userName, paddockName, "cultivar");
   populateCombo(StartingSWCombo, data, userName, paddockName, "startsw");
   SaveButton->Enabled = (webSession->isSaveAllowed());
   }
//---------------------------------------------------------------------------
// User has clicked the save button.
//---------------------------------------------------------------------------
void __fastcall TAfloPaddockForm::SaveButtonClick(TObject *Sender)
   {
   if (webSession->isSaveAllowed())
      {
      try
         {
         data->setProperty(userName, paddockName, "region", "all");
         saveCombo(WeatherStationCombo, data, userName, paddockName, "metstation");
         saveCombo(SoilTypeCombo, data, userName, paddockName, "soiltype");
         saveDatePicker(PlantingDate, data, userName, paddockName, "sowdate");
         saveCombo(CultivarCombo, data, userName, paddockName, "cultivar");
         saveCombo(StartingSWCombo, data, userName, paddockName, "startsw");
         }
      catch (const exception& err)
         {
         WebApplication->ShowMessage(err.what());
         }
      }
   }
//---------------------------------------------------------------------------
// User has clicked on rainfall.
//---------------------------------------------------------------------------
void __fastcall TAfloPaddockForm::RainfallEntryButtonClick(TObject *Sender)
   {
   webSession->showRainfallForm(userName, paddockName, false);
   }
//---------------------------------------------------------------------------
// User has clicked on soil temperature.
//---------------------------------------------------------------------------
void __fastcall TAfloPaddockForm::SoilTempButtonClick(TObject *Sender)
   {
   webSession->showSoilTempForm(userName, paddockName, false);
   }
//---------------------------------------------------------------------------
// User has clicked on air temperature.
//---------------------------------------------------------------------------
void __fastcall TAfloPaddockForm::AirTempButtonClick(TObject *Sender)
   {
   webSession->showAirTempForm(userName, paddockName, false);
   }
//---------------------------------------------------------------------------
// User has requested a report.
//---------------------------------------------------------------------------
void __fastcall TAfloPaddockForm::CreateReportButtonClick(TObject *Sender)
   {
   SaveButtonClick(NULL);
   webSession->showGenerateReportForm(userName, paddockName, false);
   }
//---------------------------------------------------------------------------
// User has clicked help.
//---------------------------------------------------------------------------
void __fastcall TAfloPaddockForm::HelpButtonClick(TObject *Sender)
   {
   webSession->showHelp();
   }
//---------------------------------------------------------------------------
// User has clicked irrigation.
//---------------------------------------------------------------------------
void __fastcall TAfloPaddockForm::IrrigationButtonClick(TObject *Sender)
   {
   webSession->showIrrigationForm(userName, paddockName, false);
   }
//---------------------------------------------------------------------------


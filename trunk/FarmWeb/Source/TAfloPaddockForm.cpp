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

   populateReportCombo();
   populateCombo(WeatherStationCombo, data, userName, paddockName, "metstation");
   populateCombo(SoilTypeCombo, data, userName, paddockName, "soiltype");
   populateCombo(SoilDepthCombo, data, userName, paddockName, "soildepth");
   populateDatePicker(PlantingDate, data, userName, paddockName, "sowdate");
   populateCombo(CultivarCombo, data, userName, paddockName, "cultivar");
   populateCombo(StartingSWCombo, data, userName, paddockName, "startsw");

   // make the controls read-only if necessary.
   WeatherStationCombo->Editable = !readOnly;
   SoilTypeCombo->Editable = !readOnly;
   SoilDepthCombo->Editable = !readOnly;
   PlantingDate->Enabled = !readOnly;
   CultivarCombo->Editable = !readOnly;
   StartingSWCombo->Editable = !readOnly;
   SaveButton->Visible = !readOnly;
   EmailFilesButton->Visible = !readOnly;
   EmailFilesEdit->Visible = !readOnly;
   }
//---------------------------------------------------------------------------
// Populate the report combo.
//---------------------------------------------------------------------------
void TAfloPaddockForm::populateReportCombo()
   {
   vector<string> names;
   data->getReportTemplateNames(names);
   Stl_2_tstrings(names, ReportCombo->Items);
   ReportCombo->ItemIndex = 0;
   }
//---------------------------------------------------------------------------
// User has clicked the save button.
//---------------------------------------------------------------------------
void __fastcall TAfloPaddockForm::SaveButtonClick(TObject *Sender)
   {
   try
      {
      saveCombo(WeatherStationCombo, data, userName, paddockName, "metstation");
      saveCombo(SoilTypeCombo, data, userName, paddockName, "soiltype");
      saveCombo(SoilDepthCombo, data, userName, paddockName, "soildepth");
      saveDatePicker(PlantingDate, data, userName, paddockName, "sowdate");
      saveCombo(CultivarCombo, data, userName, paddockName, "cultivar");
      saveCombo(StartingSWCombo, data, userName, paddockName, "startsw");
      }
   catch (const exception& err)
      {
      WebApplication->ShowMessage(err.what());
      }
   }
//---------------------------------------------------------------------------
// User has clicked on rainfall.
//---------------------------------------------------------------------------
void __fastcall TAfloPaddockForm::RainfallEntryButtonClick(TObject *Sender)
   {
   try
      {
      string sowDate = data->getProperty(userName, paddockName, "sowdate");
      webSession->showRainfallForm(userName, paddockName);
      }
   catch (const exception& err)
      {
      WebApplication->ShowMessage("You must enter a sowing date before entering rainfall");
      }
   }
//---------------------------------------------------------------------------
// User has clicked on soil temperature.
//---------------------------------------------------------------------------
void __fastcall TAfloPaddockForm::SoilTempButtonClick(TObject *Sender)
   {
   try
      {
      webSession->showSoilTempForm(userName, paddockName);
      }
   catch (const exception& err)
      {
      WebApplication->ShowMessage("You must enter a sowing date before entering soil temperature data");
      }
   }
//---------------------------------------------------------------------------
// User has clicked on air temperature.
//---------------------------------------------------------------------------
void __fastcall TAfloPaddockForm::AirTempButtonClick(TObject *Sender)
   {
   try
      {
      webSession->showAirTempForm(userName, paddockName);
      }
   catch (const exception& err)
      {
      WebApplication->ShowMessage("You must enter a sowing date before entering air temperature data");
      }
   }
//---------------------------------------------------------------------------
// User has requested a report.
//---------------------------------------------------------------------------
void __fastcall TAfloPaddockForm::RequestButtonClick(TObject *Sender)
   {
   Data::Properties properties;
   generateReport("apsimweb@dpi.qld.gov.au", webSession, data, userName, paddockName,
                  ReportCombo->Text.c_str(), properties, true, false);
   }
//---------------------------------------------------------------------------
// Send all report files for debugging purposes.
//---------------------------------------------------------------------------
void __fastcall TAfloPaddockForm::EmailFilesButtonClick(TObject *Sender)
   {
   if (EmailFilesEdit->Text != "")
      {
      Data::Properties properties;
      generateReport(EmailFilesEdit->Text.c_str(), webSession, data, userName, paddockName,
                     ReportCombo->Text.c_str(), properties, true, false);
      }
   }


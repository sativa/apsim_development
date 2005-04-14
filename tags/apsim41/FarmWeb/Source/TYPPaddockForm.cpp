//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TYPPaddockForm.h"
#include "Data.h"
#include "TYPWebSession.h"
#include "TTestFrame.h"
#include <general\vcl_functions.h>
#include <general\date_functions.h>
#include <general\string_functions.h>
#include <boost\lexical_cast.hpp>
#include "Dir.h"
#include "Utilities.h"
using namespace boost::gregorian;
using namespace boost;
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
#pragma link "IWAdvWebGrid"
#pragma link "IWWebGrid"
#pragma link "IWCompCheckbox"
#pragma link "IWExtCtrls"
#pragma link "IWHTMLControls"
#pragma link "IWCompMemo"
#pragma resource "*.dfm"

//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
__fastcall TYPPaddockForm::TYPPaddockForm(TComponent* Owner)
        : TIWAppForm(Owner)
   {
   }
//---------------------------------------------------------------------------
// setup this form.
//---------------------------------------------------------------------------
void TYPPaddockForm::setup(TYPWebSession* session,
                           Data* d,
                           const string& userN,
                           const string& paddockN,
                           bool fromGrowerMan)
   {
   webSession = session;
   data = d;
   userName = userN;
   paddockName = paddockN;
   fromGrowerManagement = fromGrowerMan;

   UserLabel->Text = "In crop management for user: xxx and paddock: yyy.";
   UserLabel->Text = StringReplace(UserLabel->Text, "xxx", data->getNameOfUser(userName).c_str(), TReplaceFlags());
   UserLabel->Text = StringReplace(UserLabel->Text, "yyy",  paddockName.c_str(), TReplaceFlags());

   populateReportCombo();
   try
      {
      PlantingDateCheck->Checked = (data->getProperty(userName, paddockName, "sowdate") != "");
      PlantingDateCheckClick(NULL);
      populateDatePicker(PlantingDate, data, userName, paddockName, "sowdate");
      InvalidDateLabel->Visible = false;
      }
   catch (const exception& err)
      {
      webSession->showMessage(err.what());
      InvalidDateLabel->Visible = true;
      }
   populateCombo(CultivarCombo, data, userName, paddockName, "cultivar");
   populateFertGrid();

   // make the controls read-only if necessary.
   EmailFilesCheckBox->Visible = data->userIsOfType(userName, Data::administrator);
   BackButton->Visible = fromGrowerManagement;
   BackImage->Visible = fromGrowerManagement;
   SaveButton->Enabled = (webSession->isSaveAllowed());

   // show / hide the visitor labels.
   VisitorLabel1->Visible = (userN == "Visitor");
   VisitorLabel2->Visible = (userN == "Visitor");
   VisitorLabel3->Visible = (userN == "Visitor");
   VisitorLabel4->Visible = (userN == "Visitor");
   VisitorLabel5->Visible = (userN == "Visitor");
   VisitorLabel6->Visible = (userN == "Visitor");
   VisitorLabel7->Visible = (userN == "Visitor");
   VisitorLabel8->Visible = (userN == "Visitor");
   VisitorLabel9->Visible = (userN == "Visitor");
   }
//---------------------------------------------------------------------------
// Populate the report combo.
//---------------------------------------------------------------------------
void TYPPaddockForm::populateReportCombo()
   {
   vector<string> reportNames;
   data->getReportTemplateNames(reportNames);
   Stl_2_tstrings(reportNames, ReportCombo->Items);
   ReportCombo->ItemIndex = 0;
   }
//---------------------------------------------------------------------------
// Populate the fertiliser grid
//---------------------------------------------------------------------------
void TYPPaddockForm::populateFertGrid()
   {
   try
      {
      FertGrid->ClearCells();
      for (unsigned row = 0; row <= 4; row++)
         {
         string dateString = data->getProperty(userName, paddockName,
                                               "fert" + lexical_cast<string>(row+1) + "date");
         string rateString = data->getProperty(userName, paddockName,
                                               "fert" + lexical_cast<string>(row+1) + "rate");
         if (dateString != "" && rateString != "")
            {
            date d(from_string(dateString));
            TDateTime dateTime(d.year(), d.month(), d.day());
            FertGrid->Cells[0][row] = dateTime.FormatString("dd/mm/yyyy");
            FertGrid->Cells[1][row] = rateString.c_str();
            }
         }
      }
   catch (const exception& err)
      {
      webSession->showMessage(err.what());
      }
   }
//---------------------------------------------------------------------------
// Save the fertiliser grid
//---------------------------------------------------------------------------
void TYPPaddockForm::saveFertGrid()
   {
   try
      {
      for (unsigned row = 0; row <= 4; row++)
         {
         if (FertGrid->Cells[0][row] != "" && FertGrid->Cells[1][row] != "")
            {
            date fertDate = fromDmyString(FertGrid->Cells[0][row].c_str());
            string rateString = FertGrid->Cells[1][row].c_str();

            data->setProperty(userName, paddockName,
                              "fert" + lexical_cast<string>(row+1) + "date",
                              to_iso_extended_string(fertDate));

            data->setProperty(userName, paddockName,
                              "fert" + lexical_cast<string>(row+1) + "rate",
                              rateString);
            }
         else
            {
            data->deleteProperty(userName, paddockName,
                                 "fert" + lexical_cast<string>(row+1) + "date");
            data->deleteProperty(userName, paddockName,
                                 "fert" + lexical_cast<string>(row+1) + "rate");
            }
         }
      }
   catch (const exception& err)
      {
      }
   }
//---------------------------------------------------------------------------
// User has clicked the save button.
//---------------------------------------------------------------------------
void __fastcall TYPPaddockForm::SaveButtonClick(TObject *Sender)
   {
   if (webSession->isSaveAllowed())
      {
      try
         {
         if (PlantingDateCheck->Checked)
            {
            saveDatePicker(PlantingDate, data, userName, paddockName, "sowdate");
            saveCombo(CultivarCombo, data, userName, paddockName, "cultivar");
            }
         else
            {
            data->deleteProperty(userName, paddockName, "sowdate");
            data->deleteProperty(userName, paddockName, "cultivar");
            }
         saveFertGrid();
         }
      catch (const exception& err)
         {
         webSession->showMessage(err.what());
         }
      }
   }
//---------------------------------------------------------------------------
// User has clicked on rainfall.
//---------------------------------------------------------------------------
void __fastcall TYPPaddockForm::RainfallEntryButtonClick(TObject *Sender)
   {
   SaveButtonClick(NULL);
   try
      {
      string sowDate = data->getProperty(userName, paddockName, "sowdate");
      webSession->showRainfallForm(userName, paddockName, fromGrowerManagement);
      }
   catch (const exception& err)
      {
      webSession->showMessage("You must enter a sowing date before entering rainfall");
      }
   }
//---------------------------------------------------------------------------
// User has requested a report.
//---------------------------------------------------------------------------
void __fastcall TYPPaddockForm::CreateReportButtonClick(TObject *Sender)
   {
   SaveButtonClick(NULL);
   if (checkForMissingData())
      {
      if (ReportCombo->Text == "Nitrogen comparison report")
         webSession->showNitrogenReportForm(userName, paddockName, NitrogenReportCallback);
      else
         {
         Data::Properties properties;
         webSession->onGenerateReportClick(userName.c_str(),
                                           paddockName.c_str(),
                                           ReportCombo->Text,
                                           "",
                                           properties,
                                           EmailFilesCheckBox->Checked);
         }
      }
   }
//---------------------------------------------------------------------------
// Nitrogen report callback.
//---------------------------------------------------------------------------
void __fastcall TYPPaddockForm::NitrogenReportCallback(bool okClicked,
                                                       AnsiString reportDescription,
                                                       const Data::Properties& properties)
   {
   Show();
   if (okClicked)
      webSession->onGenerateReportClick(userName.c_str(),
                                        paddockName.c_str(),
                                        ReportCombo->Text,
                                        reportDescription,
                                        properties,
                                        EmailFilesCheckBox->Checked);
   }
//---------------------------------------------------------------------------
// User has clicked the planting date checkbox.
//---------------------------------------------------------------------------
void __fastcall TYPPaddockForm::PlantingDateCheckClick(TObject *Sender)
   {
   PlantingDate->Visible = PlantingDateCheck->Checked;
   CultivarCombo->Visible = PlantingDateCheck->Checked;
   DateLabel->Visible = PlantingDateCheck->Checked;
   CultivarLabel->Visible = PlantingDateCheck->Checked;
   }
//---------------------------------------------------------------------------
// User has clicked setting up.
//---------------------------------------------------------------------------
void __fastcall TYPPaddockForm::SetupButtonClick(TObject *Sender)
   {
   SaveButtonClick(NULL);
   webSession->showSetupForm(userName, paddockName, false, fromGrowerManagement);
   }
//---------------------------------------------------------------------------
// User has clicked the back button.
//---------------------------------------------------------------------------
void __fastcall TYPPaddockForm::BackButtonClick(TObject *Sender)
   {
   SaveButtonClick(NULL);
   webSession->showUserManagementForm();
   }
//---------------------------------------------------------------------------
// Produce an error message if there is missing data.
//---------------------------------------------------------------------------
bool TYPPaddockForm::checkForMissingData()
   {
   bool ok = true;
   if (data->getProperty(userName, paddockName, "region") == "")
      {
      ok = false;
      webSession->showMessage("You need to select a region before creating a report.");
      }
   if (data->getProperty(userName, paddockName, "metstation") == "")
      {
      ok = false;
      webSession->showMessage("You need to select the closest weather station before creating a report.");
      }
   if (data->getProperty(userName, paddockName, "soiltype") == "")
      {
      ok = false;
      webSession->showMessage("You need to select a soiltype before creating a report.");
      }
   if (data->getProperty(userName, paddockName, "resetdate") == "")
      {
      ok = false;
      webSession->showMessage("You need to enter an initial conditions date before creating a report.");
      }
   if (data->getProperty(userName, paddockName, "soilsample1") == "")
      {
      ok = false;
      webSession->showMessage("You need to enter starting soil water and no3 values before creating a report.");
      }

   if (data->getProperty(userName, paddockName, "sowdate") == "")
      {
      ok = false;
      webSession->showMessage("You need to enter a sow date before creating a report.");
      }
   if (data->getProperty(userName, paddockName, "cultivar") == "")
      {
      ok = false;
      webSession->showMessage("You need to enter a cultivar before creating a report.");
      }
   Show();
   return ok;
   }
//---------------------------------------------------------------------------
// User has clicked on help file link.
//---------------------------------------------------------------------------
void __fastcall TYPPaddockForm::HelpFileLinkClick(TObject *Sender)
   {
   string helpScriptURL = "http://www.yieldprophet.com.au/yieldprophet/help/nitrogen fertiliser report.htm";
   webSession->newWindow(helpScriptURL.c_str(), "Help", true);
   }
//---------------------------------------------------------------------------
// User has clicked on help.
//---------------------------------------------------------------------------
void __fastcall TYPPaddockForm::HelpButtonClick(TObject *Sender)
   {
   webSession->showHelp();
   }
//---------------------------------------------------------------------------



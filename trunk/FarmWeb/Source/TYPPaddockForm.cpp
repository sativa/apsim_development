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
                           bool readOnly,
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
   EmailFilesButton->Visible = !readOnly;
   EmailFilesImage->Visible = !readOnly;
   BackButton->Visible = fromGrowerManagement;
   BackImage->Visible = fromGrowerManagement;
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
// Send all report files to an email address for debugging purposes.
//---------------------------------------------------------------------------
void __fastcall TYPPaddockForm::EmailFilesButtonClick(TObject *Sender)
   {
   SaveButtonClick(NULL);
   if (checkForMissingData())
      {
      if (ReportCombo->Text == "Nitrogen comparison report")
         webSession->showInfoForm("Enter your email address:", "", "", "", emailFilesCallback);
      else
         webSession->showInfoForm("Enter a descriptive name for the report:",
                                  "Enter your email address:", "", "", createReportCallback);
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
         generateReportFilesAndEmail("", "");
      else
         webSession->showInfoForm("Enter a descriptive name for the report:", "", "", "", createReportCallback);
      }
   }
//---------------------------------------------------------------------------
// User has finished entering an email address - send files.
//---------------------------------------------------------------------------
void __fastcall TYPPaddockForm::createReportCallback(bool okClicked,
                                                        AnsiString text1,
                                                        AnsiString text2,
                                                        AnsiString text3,
                                                        AnsiString text4)
   {
   if (okClicked && text1 != "")
      {
      webSession->show(this);
      string emailAddress = text2.c_str();
      generateReportFilesAndEmail(text1.c_str(), emailAddress);
      }
   else
      webSession->show(this);
   }
//---------------------------------------------------------------------------
// User has finished entering an email address - send files.
//---------------------------------------------------------------------------
void __fastcall TYPPaddockForm::emailFilesCallback(bool okClicked,
                                                   AnsiString text1,
                                                   AnsiString text2,
                                                   AnsiString text3,
                                                   AnsiString text4)
   {
   if (okClicked && text1 != "")
      {
      webSession->show(this);
      string emailAddress = text1.c_str();
      generateReportFilesAndEmail("", emailAddress);
      }
   else
      webSession->show(this);
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
// generate report files and send to the specified email address.
//---------------------------------------------------------------------------
void TYPPaddockForm::generateReportFilesAndEmail(const std::string& reportName,
                                                 const std::string& emailAddress)
   {
   if (ReportCombo->Text == "Nitrogen comparison report")
      webSession->showNitrogenReportForm(userName, paddockName, emailAddress);
   else if (ReportCombo->Text != "")
      {
      Data::Properties properties;
      generateReport(emailAddress, webSession, data, userName, paddockName,
                     ReportCombo->Items->Strings[ReportCombo->ItemIndex].c_str(),
                     properties, false, true, reportName);
      }
   }

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


//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TReportsForm.h"
#include <general\vcl_functions.h>
#include <general\string_functions.h>
#include <general\stl_functions.h>
#include <general\io_functions.h>
#include <general\db_functions.h>
#include <general\path.h>
#include "Data.h"
#include "TWebSession.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "IWBaseControl"
#pragma link "IWBaseHTMLControl"
#pragma link "IWCompLabel"
#pragma link "IWControl"
#pragma link "IWOutlookBar"
#pragma link "IWVCLBaseControl"
#pragma link "IWHTMLControls"
#pragma link "IWCompListbox"
#pragma link "IWCompButton"
#pragma link "IWExtCtrls"
#pragma link "IWCompRectangle"
#pragma link "IWHTMLControls"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
__fastcall TReportsForm::TReportsForm(TComponent* Owner)
        : TIWAppForm(Owner)
   {

   }
//---------------------------------------------------------------------------
// setup this form.
//---------------------------------------------------------------------------
void TReportsForm::setup(TWebSession* session,
                         Data* d,
                         const string& userN,
                         bool fromGrowerMan)
   {
   webSession = session;
   data = d;
   userName = userN;
   fromGrowerManagement = fromGrowerMan;
   ReportList->ItemIndex = -1;
   SuckLink->Visible = Str_i_Eq(userN, "DeanoConsultant");

   populateReportList();
   DeleteButton->Enabled = webSession->isSaveAllowed();
   }
//---------------------------------------------------------------------------
// Populate the report list.
//---------------------------------------------------------------------------
void TReportsForm::populateReportList(void)
   {
   ReportList->Items->Clear();
   reportNames.erase(reportNames.begin(), reportNames.end());
   data->getReports(webSession->getFilesDir(), userName, reportNames);
   for (unsigned i = 0; i != reportNames.size(); i++)
      {
      string reportName = reportNames[i];
      stripLeadingTrailing(reportName, " ");
      ReportList->Items->Add(reportName.c_str());
      }
   }
//---------------------------------------------------------------------------
// User want's to delete a report.
//---------------------------------------------------------------------------
void __fastcall TReportsForm::DeleteButtonClick(TObject *Sender)
   {
   if (ReportList->ItemIndex >= 0)
      {
      AnsiString reportName = ReportList->Items->Strings[ReportList->ItemIndex];
      AnsiString msg = "Are you sure you want to delete report: " + reportName + "?";
      webSession->showQuestionForm(msg.c_str(), deleteCallback);
      }
   }
//---------------------------------------------------------------------------
// User has clicked on Yes or No to confirm deleting a paddock
//---------------------------------------------------------------------------
void __fastcall TReportsForm::deleteCallback(bool deleteConfirmed)
   {
   if (deleteConfirmed)
      {
      try
         {
         string reportName = reportNames[ReportList->ItemIndex];
         data->deleteReport(webSession->getFilesDir(), userName, reportName.c_str());
         populateReportList();
         }
      catch (const exception& err)
         {
         webSession->showMessage(err.what());
         }
      }
   webSession->show(this);
   }
//---------------------------------------------------------------------------
// User has clicked back button.
//---------------------------------------------------------------------------
void __fastcall TReportsForm::PaddockButtonClick(TObject *Sender)
   {
   if (fromGrowerManagement)
      webSession->showUserManagementForm();
   else
      webSession->showUserDetailsForm(userName);
   }
//---------------------------------------------------------------------------
// Display help
//---------------------------------------------------------------------------
void __fastcall TReportsForm::HelpButtonClick(TObject *Sender)
   {
   webSession->showHelp();
   }
//---------------------------------------------------------------------------
// Show the specified report.
//---------------------------------------------------------------------------
void __fastcall TReportsForm::ShowButtonClick(TObject *Sender)
   {
   if (ReportList->ItemIndex >= 0)
      {
      string reportName = reportNames[ReportList->ItemIndex];
      string fileName = data->getReportFileName(webSession->getFilesDir(), userName, reportName);
      string url = fileName;
      replaceAll(url, webSession->getFilesDir(), webSession->getBaseURL() + "/files");
      replaceAll(url, "\\", "/");
      webSession->showViewReportForm(url, userName);
      }
   }
//---------------------------------------------------------------------------
void __fastcall TReportsForm::RenameButtonClick(TObject *Sender)
   {
   webSession->showInfoForm("New report name:", "", "", "", renameCallback);
   }
//---------------------------------------------------------------------------
// User has finished renaming a report.
//---------------------------------------------------------------------------
void __fastcall TReportsForm::renameCallback(bool okClicked,
                                             AnsiString text1,
                                             AnsiString text2,
                                             AnsiString text3,
                                             AnsiString text4)
   {
   if (okClicked && text1 != "")
      {
      try
         {
         string reportName = reportNames[ReportList->ItemIndex];
         string newReportName = text1.c_str();
         data->renameReport(webSession->getFilesDir(), userName, reportName, newReportName);
         populateReportList();
         }
      catch (const exception& err)
         {
         webSession->showMessage(err.what());
         }
      }
   webSession->show(this);
   }
//---------------------------------------------------------------------------
void __fastcall TReportsForm::SuckLinkClick(TObject *Sender)
   {
   ostringstream sql;
   sql << "SELECT Reports.*, Users.userName FROM Users INNER JOIN Reports ON Users.id = Reports.userId";
   TDataSet* query = runQuery(data->connectn(), sql.str());

   while (!query->Eof)
      {
      string UserName = AnsiString(query->FieldByName("username")->AsString).c_str();
      string ReportName = AnsiString(query->FieldByName("name")->AsString).c_str();
      TDateTime ReportDate = query->FieldByName("date")->AsDateTime;

      string Folder = webSession->getFilesDir() + "\\" + UserName;
      if (!DirectoryExists(Folder))
         CreateDir(Folder.c_str());

      string ReportFileName = ReportName;
      stripLeadingTrailing(ReportFileName, " ");

      // If there is a "inetpub" in the filename then set the filename to ???
      if (ReportFileName.find("inetpub") != string::npos)
         ReportFileName = "(unknown).gif";
      else
         {
         // get rid of date after the dash.
         unsigned PosDash = ReportName.rfind(" - ");
         if (PosDash != string::npos)
            ReportFileName.erase(PosDash);

         // get rid of #GIF# and add an appropriate extension.
         if (ReportFileName.substr(0, 5) == "#GIF#")
            {
            ReportFileName.erase(0, 5);
            ReportFileName += ".gif";
            }
         else
            ReportFileName += ".jpg";
         }

      // Add a date to the front of the filename.
      ReportFileName = ReportDate.FormatString("dd mmm yyyy(hnnam/pm)").c_str() + string(" ") + ReportFileName;
      ReportFileName = Folder + "\\" + ReportFileName;

      TBlobField* blob = (TBlobField*)query->FieldByName("contents");
      blob->SaveToFile(ReportFileName.c_str());
      FileSetDate(ReportFileName.c_str(), DateTimeToFileDate(ReportDate));

      query->Next();
      }
   }
//---------------------------------------------------------------------------


//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TReportsForm.h"
#include <general\vcl_functions.h>
#include <general\string_functions.h>
#include <general\stl_functions.h>
#include <general\io_functions.h>
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

   suckInReports();
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
   data->getReports(userName, reportNames);
   for (unsigned i = 0; i != reportNames.size(); i++)
      {
      string reportName = reportNames[i];
      if (reportName.substr(0, strlen("#GIF#")) == "#GIF#")
         reportName.erase(0, strlen("#GIF#"));
      stripLeadingTrailing(reportName, " ");
      ReportList->Items->Add(reportName.c_str());
      }
   }
//---------------------------------------------------------------------------
// Suck in all reports for this grower.
//---------------------------------------------------------------------------
void TReportsForm::suckInReports(void)
   {
   suckInReportsMatching(".gif");

   // do cleanup
   vector<string> fileNames;
   getDirectoryListing(webSession->getFilesDir(), userName + "*.*", fileNames, FA_NORMAL, true);
   for (unsigned f = 0; f != fileNames.size(); f++)
      {
      DeleteFile(fileNames[f].c_str());
      replaceAll(fileNames[f], " ", "_");
      DeleteFile(fileNames[f].c_str());
      }

   string userNameNoSpaces = userName;
   replaceAll(userNameNoSpaces, " ", "_");
   vector<string> fileNames2;
   getDirectoryListing(webSession->getFilesDir(), userNameNoSpaces + "*.*", fileNames2, FA_NORMAL, true);
   for (unsigned f = 0; f != fileNames2.size(); f++)
      {
      DeleteFile(fileNames2[f].c_str());
      replaceAll(fileNames2[f], " ", "_");
      DeleteFile(fileNames2[f].c_str());
      }
   }
//---------------------------------------------------------------------------
// Suck in file that match the specified extension
//---------------------------------------------------------------------------
void TReportsForm::suckInReportsMatching(const std::string& extension)
   {
   vector<string> reportNames;
   vector<string> reportFileNames;
   getDirectoryListing(webSession->getFilesDir(), userName + "*" + extension, reportFileNames, FA_NORMAL, true);
   for (unsigned f = 0; f != reportFileNames.size(); f++)
      {
      try
         {
         // The report name is everything after the " - " and before the
         // extension.
         string fileName = reportFileNames[f];
         string reportName = fileName.substr(fileName.find(" - ")+3);
         reportName.erase(reportName.find("."));
         reportName = "#GIF#" + reportName;

         reportNames.push_back(reportName);
         data->storeReport(userName, reportName, fileName);
         DeleteFile(fileName.c_str());
         }
      catch (const exception& err)
         {
         webSession->showMessage(err.what());
         }
      }
//   if (reportNames.size() > 0)
//      {
//      string msg = "New reports added: ";
//      msg += buildString(reportNames, ",");
//      webSession->showMessage(msg);
//      }
   }
//---------------------------------------------------------------------------
// User want's to delete a report.
//---------------------------------------------------------------------------
void __fastcall TReportsForm::DeleteButtonClick(TObject *Sender)
   {
   if (ReportList->ItemIndex >= 0)
      {
      AnsiString reportName = ReportList->Items->Strings[ReportList->ItemIndex];
      AnsiString msg = "Are you sure you want to delete report " + reportName + "?";
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
         data->deleteReport(userName, reportName.c_str());
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
// write a report html page to display the report nicely.
//---------------------------------------------------------------------------
void TReportsForm::writeReportHtml(const string& fileName)
   {
   string htmlFileName = webSession->getFilesDir() + "\\" + userName + ".htm";
   ofstream html(htmlFileName.c_str());
   html << "<html>" << endl;
   html << "<head><title>Yield Prophet Report</title></head>" << endl;
   html << "<body>" << endl;
   html << "<img src=\"" << ExtractFileName(fileName.c_str()).c_str() << "\">" << endl;
   html << "</body>" << endl;
   html << "</html>" << endl;
   }
//---------------------------------------------------------------------------
// Show the specified report.
//---------------------------------------------------------------------------
void __fastcall TReportsForm::ShowButtonClick(TObject *Sender)
   {
   if (ReportList->ItemIndex >= 0)
      {
      string reportName = reportNames[ReportList->ItemIndex];
      string extension;
      if (reportName.substr(0, strlen("#GIF#")) == "#GIF#")
         extension = ".gif";
      else
         extension = ".jpg";

      string fileName = webSession->getFilesDir() + "\\" + userName + extension;
      data->generateReport(userName, reportName, fileName);
      writeReportHtml(fileName);
      string url = webSession->getBaseURL() + "/files/" + userName + ".htm";
      webSession->newWindow(url, "Report", true);
      }
   }
//---------------------------------------------------------------------------
void __fastcall TReportsForm::RenameButtonClick(TObject *Sender)
   {
   webSession->showInfoForm("Consultant name", "", "", "", renameCallback);
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
         string newReportName = string("#GIF#") + text1.c_str();
         data->renameReport(userName, reportName, newReportName);
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


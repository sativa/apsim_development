//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TReportManagementForm.h"
#include "TWebSession.h"
#include <general\vcl_functions.h>
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
#pragma link "IWCompRectangle"
#pragma link "IWBaseHTMLControl"
#pragma link "IWCompMemo"
#pragma link "IWExtCtrls"
#pragma link "IWHTMLControls"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
__fastcall TReportManagementForm::TReportManagementForm(TComponent* Owner)
        : TIWAppForm(Owner)
   {
   }
//---------------------------------------------------------------------------
// setup this form.
//---------------------------------------------------------------------------
void TReportManagementForm::setup(TWebSession* session, Data* d, unsigned page)
   {
   webSession = session;
   data = d;
   pageNumber = page;

   switch (pageNumber)
      {
      case 1 : PromptLabel->Text = "CON/PAR generation template"; break;
      case 2 : PromptLabel->Text = "APSIMReport generation template"; break;
      case 3 : PromptLabel->Text = "Other files generation template"; break;
      };

   populateReportCombo();
   ReportCombo->ItemIndex = 0;
   populateReportMemo();
   }
//---------------------------------------------------------------------------
// Populate the report combo.
//---------------------------------------------------------------------------
void TReportManagementForm::populateReportCombo()
   {
   vector<string> names;
   data->getReportTemplateNames(names);
   Stl_2_tstrings(names, ReportCombo->Items);
   }
//---------------------------------------------------------------------------
// populate the report memo.
//---------------------------------------------------------------------------
void TReportManagementForm::populateReportMemo()
   {
   if (ReportCombo->ItemIndex >= 0)
      {
      try
         {
         AnsiString reportName = ReportCombo->Items->Strings[ReportCombo->ItemIndex];
         ReportMemo->Lines->Text = data->getReportTemplate(reportName.c_str(), pageNumber).c_str();
         }
      catch (const exception& err)
         {
         ReportMemo->Lines->Clear();
         }
      }
   }
//---------------------------------------------------------------------------
// User has clicked on delete.
//---------------------------------------------------------------------------
void __fastcall TReportManagementForm::SaveButtonClick(TObject *Sender)
   {
   if (ReportCombo->ItemIndex >= 0)
      {
      try
         {
         AnsiString reportName = ReportCombo->Items->Strings[ReportCombo->ItemIndex];
         AnsiString contents = ReportMemo->Lines->Text;
         data->setReportTemplate(reportName.c_str(), contents.c_str(), pageNumber);
         }
      catch (const exception& err)
         {
         webSession->showMessage(err.what());
         }
      }
   }
//---------------------------------------------------------------------------
// User has changed the report type.
//---------------------------------------------------------------------------
void __fastcall TReportManagementForm::ReportComboChange(TObject *Sender)
   {
   populateReportMemo();
   }
//---------------------------------------------------------------------------
// user has clicked add.
//---------------------------------------------------------------------------
void __fastcall TReportManagementForm::AddButtonClick(TObject *Sender)
   {
   webSession->showInfoForm("Enter new report name:", "", "", "", AddReportCallBack);
   }
//---------------------------------------------------------------------------
// Add report callback.
//---------------------------------------------------------------------------
void __fastcall TReportManagementForm::AddReportCallBack(bool okClicked,
                                               AnsiString text1,
                                               AnsiString text2,
                                               AnsiString text3,
                                               AnsiString text4)
   {
   if (okClicked && text1 != "")
      {
      try
         {
         data->setReportTemplate(text1.c_str(), "Insert template here", pageNumber);
         populateReportCombo();
         ReportCombo->ItemIndex = ReportCombo->Items->IndexOf(text1);
         populateReportMemo();
         }
      catch (const exception& err)
         {
         webSession->showMessage(err.what());
         }
      }
   webSession->show(this);
   }
//---------------------------------------------------------------------------
// user wants to delete a report type.
//---------------------------------------------------------------------------
void __fastcall TReportManagementForm::DeleteButtonClick(TObject *Sender)
   {
   if (ReportCombo->ItemIndex >= 0)
      {
      AnsiString reportName = ReportCombo->Items->Strings[ReportCombo->ItemIndex];
      AnsiString msg = "Are you sure you want to delete report template " + reportName + "?";
      webSession->showQuestionForm(msg.c_str(), deleteReportCallback);
      }
   }
//---------------------------------------------------------------------------
// User has clicked on Yes or No to confirm deleting a report
//---------------------------------------------------------------------------
void __fastcall TReportManagementForm::deleteReportCallback(bool deleteConfirmed)
   {
   if (deleteConfirmed)
      {
      try
         {
         AnsiString reportName = ReportCombo->Items->Strings[ReportCombo->ItemIndex];
         data->deleteReportTemplate(reportName.c_str(), pageNumber);
         populateReportCombo();
         ReportCombo->ItemIndex = 0;
         populateReportMemo();
         }
      catch (const exception& err)
         {
         webSession->showMessage(err.what());
         }
      }
   webSession->show(this);
   }
//---------------------------------------------------------------------------
// User has clicked on import
//---------------------------------------------------------------------------
void __fastcall TReportManagementForm::ImportButtonClick(TObject *Sender)
   {
   if (ImportFile->Filename != "")
      {
      TMemoryStream* stream = new TMemoryStream;
      ImportFile->SaveToStream(stream);
      string reportContents = string((char*)stream->Memory, stream->Size);
      try
         {
         data->setReportTemplate(ReportCombo->Text.c_str(), reportContents, pageNumber);
         }
      catch (const exception& err)
         {
         webSession->showMessage(err.what());
         }
      delete stream;
      }
   }
//---------------------------------------------------------------------------


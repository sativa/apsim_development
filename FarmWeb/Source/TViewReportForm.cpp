//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TViewReportForm.h"
#include "TWebSession.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "IWBaseControl"
#pragma link "IWBaseHTMLControl"
#pragma link "IWCompRectangle"
#pragma link "IWControl"
#pragma link "IWExtCtrls"
#pragma link "IWHTMLControls"
#pragma link "IWVCLBaseControl"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
__fastcall TViewReportForm::TViewReportForm(TComponent* Owner)
        : TIWAppForm(Owner)
   {

   }
//---------------------------------------------------------------------------
void TViewReportForm::setup(TWebSession* session, const std::string& url,
                            const std::string& username, const std::string& paddockname,
                            bool backToReports)
   {
   webSession = session;
   userName = username;
   paddockName = paddockname;
   Image->ImageFile->URL = url.c_str();
   backToReportsForm = backToReports;
   }

void __fastcall TViewReportForm::BackButtonClick(TObject *Sender)
   {
   if (backToReportsForm)
      webSession->showReportsForm(userName, false, false);
   else
      webSession->showPaddockForm(userName, paddockName, false, false);
   }
//---------------------------------------------------------------------------


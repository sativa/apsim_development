//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TGenerateReportForm.h"
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
__fastcall TGenerateForm::TGenerateForm(TComponent* Owner)
        : TIWAppForm(Owner)
   {
   }
//---------------------------------------------------------------------------
// setup this form.
//---------------------------------------------------------------------------
void TGenerateForm::setup(TWebSession* session,
                         Data* d,
                         const string& userN,
                         const string& paddockN,
                         bool readOnly)
   {
   webSession = session;
   data = d;
   userName = userN;
   paddockName = paddockN;

   populateReportCombo();
   EmailFilesCheckBox->Visible = data->userIsOfType(webSession->getCurrentLoggedInUser(),
                                                    Data::administrator);
   }
//---------------------------------------------------------------------------
// Populate the report combo.
//---------------------------------------------------------------------------
void TGenerateForm::populateReportCombo()
   {
   vector<string> names;
   data->getReportTemplateNames(names);
   Stl_2_tstrings(names, ReportCombo->Items);
   ReportCombo->ItemIndex = 0;
   }
//---------------------------------------------------------------------------
// User has requested a report.
//---------------------------------------------------------------------------
void __fastcall TGenerateForm::CreateReportButtonClick(TObject *Sender)
   {
   Data::Properties properties;
   webSession->onGenerateReportClick(userName.c_str(),
                                     paddockName.c_str(),
                                     ReportCombo->Text,
                                     "",
                                     properties,
                                     EmailFilesCheckBox->Checked);
   }


//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TYPNitrogenReportForm.h"
#include "TYPWebSession.h"
#include "utilities.h"
#include <boost\lexical_cast.hpp>
#include <general\date_functions.h>
using namespace boost;
using namespace boost::gregorian;
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "IWBaseControl"
#pragma link "IWBaseHTMLControl"
#pragma link "IWCompEdit"
#pragma link "IWCompLabel"
#pragma link "IWCompRectangle"
#pragma link "IWControl"
#pragma link "IWVCLBaseControl"
#pragma link "IWAdvWebGrid"
#pragma link "IWCompListbox"
#pragma link "IWTMSCal"
#pragma link "IWWebGrid"
#pragma link "IWOutlookBar"
#pragma link "IWHTMLControls"
#pragma link "IWExtCtrls"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
// constructor.
//---------------------------------------------------------------------------
__fastcall TYPNitrogenReportForm::TYPNitrogenReportForm(TComponent* Owner)
        : TIWAppForm(Owner)
   {

   }
//---------------------------------------------------------------------------
// Setup the form
//---------------------------------------------------------------------------
void TYPNitrogenReportForm::setup(TYPWebSession* session,
                                  Data* d,
                                  const string& userN,
                                  const string& paddockN,
                                  TReportCallback callb)
   {
   webSession = session;
   data = d;
   userName = userN;
   paddockName = paddockN;
   callback = callb;
   }
//---------------------------------------------------------------------------
// User has clicked back - call callback
//---------------------------------------------------------------------------
void __fastcall TYPNitrogenReportForm::BackButtonClick(TObject *Sender)
   {
   webSession->showPaddockForm(userName, paddockName, false, false);
   }
//---------------------------------------------------------------------------
// User has clicked ok - call callback
//---------------------------------------------------------------------------
void __fastcall TYPNitrogenReportForm::OkButtonClick(TObject *Sender)
   {
   Data::Properties properties;
   if (allOk())
      {
      try
         {
         saveFertGrid(properties);
         callback(true, ReportDescription->Text, properties);
         }
      catch (const exception& err)
         {
         webSession->showMessage(err.what());
         }
      }
   }
//---------------------------------------------------------------------------
// Save the fertiliser grid
//---------------------------------------------------------------------------
void TYPNitrogenReportForm::saveFertGrid(Data::Properties& properties)
   {
   try
       {
       properties.push_back(Data::Property("scenario1desc",
                                            Scenario1Edit->Text.c_str()));
       properties.push_back(Data::Property("scenario2desc",
                                            Scenario2Edit->Text.c_str()));
       properties.push_back(Data::Property("scenario3desc",
                                            Scenario3Edit->Text.c_str()));
       for (unsigned col = 0; col <= 4; col +=2)
         {
         unsigned n = 1;
         unsigned scenarioNumber = col/2+1;
         for (unsigned row = 0; row <= 4; row++)
            {
            if (FertGrid->Cells[col][row] != "" && FertGrid->Cells[col+1][row] != "")
               {
               date fertDate = fromDmyString(FertGrid->Cells[col][row].c_str());
               string dateString = to_iso_extended_string(fertDate);
               string rateString = FertGrid->Cells[col+1][row].c_str();

               properties.push_back(Data::Property("scenario" + lexical_cast<string>(scenarioNumber) +
                                                   "fert" + lexical_cast<string>(n) + "date",
                                                   dateString));
               properties.push_back(Data::Property("scenario" + lexical_cast<string>(scenarioNumber) +
                                                   "fert" + lexical_cast<string>(n) + "rate",
                                                   rateString));
               n++;
               }
            }
         }
      }
   catch (const exception& err)
      {
      }
   }
//---------------------------------------------------------------------------
// Return true if all edit boxes are ok.
//---------------------------------------------------------------------------
bool TYPNitrogenReportForm::allOk()
   {
   if (ReportDescription->Text == "")
      webSession->showMessage("You must enter a report description in the top edit box");
   if (Scenario1Edit->Text == "")
      webSession->showMessage("You must enter a description for scenario1");
   else if (scenarioHasData(2) && Scenario2Edit->Text == "")
      webSession->showMessage("You must enter at description for scenario2");
   else if (scenarioHasData(3) && Scenario3Edit->Text == "")
      webSession->showMessage("You must enter at description for scenario3");
   else
      return true;
   return false;
   }
//---------------------------------------------------------------------------
// Return true a scenario has data ie fertiliser applications
//---------------------------------------------------------------------------
bool TYPNitrogenReportForm::scenarioHasData(int scenarioNumber)
   {
   for (unsigned row = 0; row <= 4; row++)
      {
      if (FertGrid->Cells[scenarioNumber][row] != "" && FertGrid->Cells[scenarioNumber+1][row] != "")
         return true;
      }
   return false;
   }


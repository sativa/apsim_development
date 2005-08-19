//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TMetStationForm.h"
#include "TWebSession.h"
#include <general\vcl_functions.h>
#include <general\xml.h>
#include <boost\lexical_cast.hpp>

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
#pragma link "IWCompRectangle"
#pragma link "IWBaseHTMLControl"
#pragma link "IWCompMemo"
#pragma link "IWExtCtrls"
#pragma link "IWHTMLControls"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
__fastcall TMetStationForm::TMetStationForm(TComponent* Owner)
        : TIWAppForm(Owner)
   {
   }
//---------------------------------------------------------------------------
// setup this form.
//---------------------------------------------------------------------------
void TMetStationForm::setup(TWebSession* session, Data* d)
   {
   webSession = session;
   data = d;

   populateRegionCombo();
   RegionCombo->ItemIndex = 0;
   MetStationList->ItemIndex = 0;
   populateMetStationList();
   }
//---------------------------------------------------------------------------
// populate the region combo
//---------------------------------------------------------------------------
void TMetStationForm::populateRegionCombo()
   {
   vector<string> values;
   data->getRegions(values);
   Stl_2_tstrings(values, RegionCombo->Items);
   }
//---------------------------------------------------------------------------
// Populate the MetStationList list.
//---------------------------------------------------------------------------
void TMetStationForm::populateMetStationList()
   {
   vector<string> names;
   data->getMetStations(RegionCombo->Text.c_str(), names);
   Stl_2_tstrings(names, MetStationList->Items);
   }
//---------------------------------------------------------------------------
// User has changed the soil type.
//---------------------------------------------------------------------------
void __fastcall TMetStationForm::RegionComboChange(TObject *Sender)
   {
   populateMetStationList();
   }
//---------------------------------------------------------------------------
// User has clicked add region.
//---------------------------------------------------------------------------
void __fastcall TMetStationForm::AddRegionButtonClick(TObject *Sender)
   {
   webSession->showInfoForm("Enter a new region name:", "", "", "", addRegionCallback);
   }
//---------------------------------------------------------------------------
// User has finished entering a new region name
//---------------------------------------------------------------------------
void __fastcall TMetStationForm::addRegionCallback(bool okClicked,
                                              AnsiString text1,
                                              AnsiString text2,
                                              AnsiString text3,
                                              AnsiString text4)
   {
   if (okClicked && text1 != "")
      data->addRegion(text1.c_str());
   populateRegionCombo();
   webSession->show(this);
   }
//---------------------------------------------------------------------------
// User has clicked delete region
//---------------------------------------------------------------------------
void __fastcall TMetStationForm::DeleteRegionButtonClick(TObject *Sender)
   {
   if (RegionCombo->ItemIndex >= 0)
      {
      string msg = "Are you sure you want to delete the region: " + string(RegionCombo->Text.c_str());
      webSession->showQuestionForm(msg, deleteRegionCallback);
      }
   }
//---------------------------------------------------------------------------
// Callback for deleteRegion
//---------------------------------------------------------------------------
void __fastcall TMetStationForm::deleteRegionCallback(bool yesClicked)
   {
   if (yesClicked)
      {
      data->deleteRegion(RegionCombo->Text.c_str());
      populateRegionCombo();
      }
   webSession->show(this);
   }
//---------------------------------------------------------------------------
// User has clicked delete metstation.
//---------------------------------------------------------------------------
void __fastcall TMetStationForm::DeleteMetStationButtonClick(TObject *Sender)
   {
   if (RegionCombo->ItemIndex >= 0 && MetStationList->ItemIndex >= 0)
      {
      string msg = "Are you sure you want to delete the selected met stations?";
      webSession->showQuestionForm(msg, deleteMetStationCallback);
      }
   }
//---------------------------------------------------------------------------
// Callback for delete metstation
//---------------------------------------------------------------------------
void __fastcall TMetStationForm::deleteMetStationCallback(bool yesClicked)
   {
   if (yesClicked)
      {
      for (int i = 0; i != MetStationList->Items->Count; i++)
         {
         if (MetStationList->Selected[i])
            data->deleteMetStation(RegionCombo->Text.c_str(),
                                   MetStationList->Items->Strings[i].c_str());
         }
      populateMetStationList();
      }
   webSession->show(this);
   }
//---------------------------------------------------------------------------
// user has clicked import.
//---------------------------------------------------------------------------
void __fastcall TMetStationForm::ImportButtonClick(TObject *Sender)
   {
   if (ImportFile->Filename != "")
      {
      TMemoryStream* stream = new TMemoryStream;
      ImportFile->SaveToStream(stream);
      string metStationContents = string((char*)stream->Memory, stream->Size);
      istringstream in(metStationContents.c_str());
      string line;
      while (getline(in, line))
         {
         try
            {
            vector<string> values;
            splitIntoValues(line, ",", values);
            if (values.size() == 2)
               {
               stripLeadingTrailing(values[0], " ");
               stripLeadingTrailing(values[1], " ");
               unsigned number = lexical_cast<unsigned> (values[0]);
               string name = values[1];
               data->deleteMetStation(RegionCombo->Text.c_str(), name);
               data->addMetStation(RegionCombo->Text.c_str(), name, number);
               }
            }
         catch (const exception& err)
            {
            webSession->showMessage(err.what());
            }
         }
      delete stream;
      populateMetStationList();
      }
   }


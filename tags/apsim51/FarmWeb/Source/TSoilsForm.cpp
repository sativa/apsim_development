//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TSoilsForm.h"
#include "TWebSession.h"
#include <general\vcl_functions.h>
#include <general\xml.h>
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
__fastcall TSoilsForm::TSoilsForm(TComponent* Owner)
        : TIWAppForm(Owner)
   {
   }
//---------------------------------------------------------------------------
// setup this form.
//---------------------------------------------------------------------------
void TSoilsForm::setup(TWebSession* session, Data* d)
   {
   webSession = session;
   data = d;

   populateRegionCombo();
   RegionCombo->ItemIndex = 0;
   SoilList->ItemIndex = 0;
   populateSoilList();
   }
//---------------------------------------------------------------------------
// populate the region combo
//---------------------------------------------------------------------------
void TSoilsForm::populateRegionCombo()
   {
   vector<string> values;
   data->getRegions(values);
   Stl_2_tstrings(values, RegionCombo->Items);
   }
//---------------------------------------------------------------------------
// Populate the soil list.
//---------------------------------------------------------------------------
void TSoilsForm::populateSoilList()
   {
   vector<string> names;
   data->getSoils(RegionCombo->Text.c_str(), names);
   Stl_2_tstrings(names, SoilList->Items);
   }
//---------------------------------------------------------------------------
// User has changed the soil type.
//---------------------------------------------------------------------------
void __fastcall TSoilsForm::RegionComboChange(TObject *Sender)
   {
   populateSoilList();
   }
//---------------------------------------------------------------------------
// User has clicked add region.
//---------------------------------------------------------------------------
void __fastcall TSoilsForm::AddRegionButtonClick(TObject *Sender)
   {
   webSession->showInfoForm("Enter a new region name:", "", "", "", addRegionCallback);
   }
//---------------------------------------------------------------------------
// User has finished entering a new region name
//---------------------------------------------------------------------------
void __fastcall TSoilsForm::addRegionCallback(bool okClicked,
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
void __fastcall TSoilsForm::DeleteRegionButtonClick(TObject *Sender)
   {
   if (RegionCombo->ItemIndex >= 0)
      {
      string msg = "Are you sure you want to delete region " + string(RegionCombo->Text.c_str());
      webSession->showQuestionForm(msg, deleteRegionCallback);
      }
   }
//---------------------------------------------------------------------------
// Callback for deleteRegion
//---------------------------------------------------------------------------
void __fastcall TSoilsForm::deleteRegionCallback(bool yesClicked)
   {
   if (yesClicked)
      {
      data->deleteRegion(RegionCombo->Text.c_str());
      populateRegionCombo();
      }
   webSession->show(this);
   }
//---------------------------------------------------------------------------
// User has clicked delete soil.
//---------------------------------------------------------------------------
void __fastcall TSoilsForm::DeleteSoilButtonClick(TObject *Sender)
   {
   if (RegionCombo->ItemIndex >= 0 && SoilList->ItemIndex >= 0)
      {
      string msg = "Are you sure you want to delete the selected soils?";
      webSession->showQuestionForm(msg, deleteSoilCallback);
      }
   }
//---------------------------------------------------------------------------
// Callback for deleteSoil
//---------------------------------------------------------------------------
void __fastcall TSoilsForm::deleteSoilCallback(bool yesClicked)
   {
   if (yesClicked)
      {
      for (int i = 0; i != SoilList->Items->Count; i++)
         {
         if (SoilList->Selected[i])
            data->deleteSoil(RegionCombo->Text.c_str(),
                             SoilList->Items->Strings[i].c_str());
         }
      populateSoilList();
      }
   webSession->show(this);
   }
//---------------------------------------------------------------------------
// user has clicked import.
//---------------------------------------------------------------------------
void __fastcall TSoilsForm::ImportButtonClick(TObject *Sender)
   {
   if (ImportFile->Filename != "")
      {
      TMemoryStream* stream = new TMemoryStream;
      ImportFile->SaveToStream(stream);
      string soilsContents = string((char*)stream->Memory, stream->Size);
      XMLDocument* doc = new XMLDocument(soilsContents, XMLDocument::xmlContents);
      for (XMLNode::iterator soil = doc->documentElement().begin();
                             soil != doc->documentElement().end();
                             soil++)
         {
         try
            {
            string soilContents = soil->write();
            string soilName = soil->getAttribute("name");
            data->deleteSoil(RegionCombo->Text.c_str(), soilName);
            data->setSoil(RegionCombo->Text.c_str(), soilName, soilContents);
            }
         catch (const exception& err)
            {
            webSession->showMessage(err.what());
            }
         }
      delete stream;
      populateSoilList();
      }
   }


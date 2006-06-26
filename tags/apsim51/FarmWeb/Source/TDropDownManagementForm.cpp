//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TDropDownManagementForm.h"
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
__fastcall TDropDownManagementForm::TDropDownManagementForm(TComponent* Owner)
        : TIWAppForm(Owner)
   {
   }
//---------------------------------------------------------------------------
// setup this form.
//---------------------------------------------------------------------------
void TDropDownManagementForm::setup(TWebSession* session, Data* d)
   {
   webSession = session;
   data = d;

   populateTypeCombo();
   }
//---------------------------------------------------------------------------
// Populate the type combo box.
//---------------------------------------------------------------------------
void TDropDownManagementForm::populateTypeCombo()
   {
   vector<string> names;
   data->getLookupNames(names);
   Stl_2_tstrings(names, TypeCombo->Items);
   }
//---------------------------------------------------------------------------
// Populate the values list
//---------------------------------------------------------------------------
void TDropDownManagementForm::populateList()
   {
   if (TypeCombo->ItemIndex >= 0)
      {
      vector<string> values;
      data->getLookupValues(TypeCombo->Items->Strings[TypeCombo->ItemIndex].c_str(), values);
      Stl_2_tstrings(values, ValueList->Items);
      }
   }
//---------------------------------------------------------------------------
// User has clicked AddType
//---------------------------------------------------------------------------
void __fastcall TDropDownManagementForm::AddTypeButtonClick(TObject *Sender)
   {
   webSession->showInfoForm("Enter a new type name", "", "", "", addTypeCallback);
   }
//---------------------------------------------------------------------------
// Callback for addtype.
//---------------------------------------------------------------------------
void __fastcall TDropDownManagementForm::addTypeCallback
   (bool okClicked, AnsiString text1, AnsiString text2,
    AnsiString text3, AnsiString text4)

   {
   if (okClicked)
      {
      if (TypeCombo->Items->IndexOf(text1) == -1)
         TypeCombo->ItemIndex = TypeCombo->Items->Add(text1);
      }
   webSession->show(this);
   }
//---------------------------------------------------------------------------
// User has clicked delete type.
//---------------------------------------------------------------------------
void __fastcall TDropDownManagementForm::DeleteTypeButtonClick(
      TObject *Sender)
   {
   if (TypeCombo->ItemIndex >= 0)
      {
      string msg = "Are you sure you want to delete " + string(TypeCombo->Text.c_str());
      webSession->showQuestionForm(msg, deleteTypeCallback);
      }
   }
//---------------------------------------------------------------------------
// Callback for deleteype.
//---------------------------------------------------------------------------
void __fastcall TDropDownManagementForm::deleteTypeCallback(bool yesClicked)

   {
   if (yesClicked)
      {
      data->deleteLookupValues(TypeCombo->Text.c_str());
      populateTypeCombo();
      }
   webSession->show(this);
   }
//---------------------------------------------------------------------------
// User has clicked Add Value.
//---------------------------------------------------------------------------
void __fastcall TDropDownManagementForm::AddValueButtonClick(TObject *Sender)
   {
   if (TypeCombo->ItemIndex >= 0)
      {
      webSession->showInfoForm("Enter a new value", "", "", "", addValueCallback);
      }
   }
//---------------------------------------------------------------------------
// Callback for addvalue
//---------------------------------------------------------------------------
void __fastcall TDropDownManagementForm::addValueCallback
   (bool okClicked, AnsiString text1, AnsiString text2,
    AnsiString text3, AnsiString text4)

   {
   if (okClicked)
      {
      if (ValueList->Items->IndexOf(text1) == -1)
         {
         ValueList->ItemIndex = ValueList->Items->Add(text1);
         vector<string> values;
         TStrings_2_stl(ValueList->Items, values);
         data->setLookupValues(TypeCombo->Text.c_str(), values);
         }
      }
   webSession->show(this);
   }
//---------------------------------------------------------------------------
// User has clicked delete value.
//---------------------------------------------------------------------------
void __fastcall TDropDownManagementForm::DeleteValueButtonClick(
      TObject *Sender)
   {
   if (TypeCombo->ItemIndex >= 0 && ValueList->ItemIndex >= 0)
      {
      string msg = "Are you sure you want to delete " + string(ValueList->Text.c_str());
      webSession->showQuestionForm(msg, deleteValueCallback);
      }

   }
//---------------------------------------------------------------------------
// Callback for deletevalue
//---------------------------------------------------------------------------
void __fastcall TDropDownManagementForm::deleteValueCallback(bool yesClicked)

   {
   if (yesClicked)
      {
      ValueList->Items->Delete(ValueList->ItemIndex);
      vector<string> values;
      TStrings_2_stl(ValueList->Items, values);
      data->setLookupValues(TypeCombo->Text.c_str(), values);
      }
   webSession->show(this);
   }
//---------------------------------------------------------------------------
// User has changed the type combo.
//---------------------------------------------------------------------------
void __fastcall TDropDownManagementForm::TypeComboChange(TObject *Sender)
   {
   populateList();
   }
//---------------------------------------------------------------------------


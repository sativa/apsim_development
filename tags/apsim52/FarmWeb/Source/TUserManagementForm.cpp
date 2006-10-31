//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TUserManagementForm.h"
#include "Data.h"
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
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
__fastcall TUserManagementForm::TUserManagementForm(TComponent* Owner)
        : TIWAppForm(Owner)
   {
   }
//---------------------------------------------------------------------------
// setup this form.
//---------------------------------------------------------------------------
void TUserManagementForm::setup(TWebSession* session,
                                Data* d, const string& userN)
   {
   webSession = session;
   data = d;
   userName = userN;
   populateUserList();
   AddConsultantButton->Visible = data->userIsOfType(userName, Data::administrator);
   }
//---------------------------------------------------------------------------
// populate the list box.
//---------------------------------------------------------------------------
void TUserManagementForm::populateUserList(void)
   {
   vector<string> userNames;
   if (data->userIsOfType(userName, Data::administrator))
      data->getUsers(userNames);
   else
      data->getUsersForConsultant(userName, userNames);

   Stl_2_tstrings(userNames, UserList->Items);
   UserList->ItemIndex = -1;
   }
//---------------------------------------------------------------------------
// User has clicked add.
//---------------------------------------------------------------------------
void __fastcall TUserManagementForm::AddConsultantButtonClick(TObject *Sender)
   {
   webSession->showInfoForm("Consultant name", "Consultant login id", "Consultant password", "Consultant email address",
                            addConsultantCallback);
   }
//---------------------------------------------------------------------------
// User has finished entering a new consultant
//---------------------------------------------------------------------------
void __fastcall TUserManagementForm::addConsultantCallback(bool okClicked,
                                                      AnsiString text1,
                                                      AnsiString text2,
                                                      AnsiString text3,
                                                      AnsiString text4)
   {
   webSession->show(this);
   if (okClicked && text1 != "" && text2 != "" && text3 != "" && text4 != "")
      {
      try
         {
         data->addConsultantUser(text1.c_str(), text2.c_str(),
                                 text3.c_str(), text4.c_str());
         populateUserList();
         }
      catch (const runtime_error& err)
         {
         webSession->showMessage(err.what());
         }
      }
   }
//---------------------------------------------------------------------------
// User has clicked add grower.
//---------------------------------------------------------------------------
void __fastcall TUserManagementForm::AddGrowerButtonClick(TObject *Sender)
   {
   webSession->showInfoForm("Grower name", "Grower login id", "Grower password", "Grower email address",
                            addGrowerCallback);
   }
//---------------------------------------------------------------------------
// User has finished entering a new grower
//---------------------------------------------------------------------------
void __fastcall TUserManagementForm::addGrowerCallback(bool okClicked,
                                                  AnsiString text1,
                                                  AnsiString text2,
                                                  AnsiString text3,
                                                  AnsiString text4)
   {
   webSession->show(this);
   if (okClicked && text1 != "" && text2 != "" && text3 != "" && text4 != "")
      {
      try
         {
         data->addUser(text1.c_str(), text2.c_str(),
                       text3.c_str(), userName, text4.c_str());
         populateUserList();
         }
      catch (const runtime_error& err)
         {
         webSession->showMessage(err.what());
         }
      }
   }
//---------------------------------------------------------------------------
// User has clicked on delete.
//---------------------------------------------------------------------------
void __fastcall TUserManagementForm::DeleteGrowerButtonClick(TObject *Sender)
   {
   if (UserList->ItemIndex >= 0)
      {
      AnsiString userName = UserList->Items->Strings[UserList->ItemIndex];
      AnsiString msg = "Are you sure you want to delete " + userName + "?";
      webSession->showQuestionForm(msg.c_str(), deleteUserCallback);
      }
   }
//---------------------------------------------------------------------------
// User has clicked on Yes or No to confirm deleting a user.
//---------------------------------------------------------------------------
void __fastcall TUserManagementForm::deleteUserCallback(bool deleteConfirmed)
   {
   if (deleteConfirmed)
      {
      try
         {
         AnsiString userName = UserList->Items->Strings[UserList->ItemIndex];
         data->deleteUser(userName.c_str());
         populateUserList();
         populatePaddockList();
         webSession->show(this);
         }
      catch (const exception& err)
         {
         webSession->showMessage(err.what());
         }
      }
   }
//---------------------------------------------------------------------------
// User has clicked a user name - update paddock list.
//---------------------------------------------------------------------------
void __fastcall TUserManagementForm::UserListClick(TObject *Sender)
   {
   populatePaddockList();
   }
//---------------------------------------------------------------------------
// Fill paddock list with paddock names.
//---------------------------------------------------------------------------
void TUserManagementForm::populatePaddockList(void)
   {
   if (UserList->ItemIndex >= 0)
      {
      AnsiString userName = UserList->Items->Strings[UserList->ItemIndex];
      vector<string> paddockNames;
      data->getPaddocks(userName.c_str(), paddockNames);
      Stl_2_tstrings(paddockNames, PaddockList->Items);
      }
   }
//---------------------------------------------------------------------------
// User has clicked on add paddock.
//---------------------------------------------------------------------------
void __fastcall TUserManagementForm::AddPaddockButtonClick(TObject *Sender)
   {
   webSession->showInfoForm("Paddock name", "", "", "", addPaddockCallback);
   }
//---------------------------------------------------------------------------
// User has finished adding a new paddock.
//---------------------------------------------------------------------------
void __fastcall TUserManagementForm::addPaddockCallback(bool okClicked,
                                                   AnsiString text1,
                                                   AnsiString text2,
                                                   AnsiString text3,
                                                   AnsiString text4)
   {
   if (okClicked && text1 != "")
      {
      AnsiString userName = UserList->Items->Strings[UserList->ItemIndex];
      try
         {
         data->addPaddock(userName.c_str(), text1.c_str());
         populatePaddockList();
         }
      catch (const exception& err)
         {
         webSession->showMessage(err.what());
         }
      }
   webSession->show(this);
   }
//---------------------------------------------------------------------------
// User has clicked delete paddock.
//---------------------------------------------------------------------------
void __fastcall TUserManagementForm::DeletePaddockButtonClick(
      TObject *Sender)
   {
   if (UserList->ItemIndex >= 0 && PaddockList->ItemIndex >= 0)
      {
      AnsiString paddockName = PaddockList->Items->Strings[PaddockList->ItemIndex];
      AnsiString msg = "Are you sure you want to delete paddock " + paddockName + "?";
      webSession->showQuestionForm(msg.c_str(), deletePaddockCallback);
      }
   }
//---------------------------------------------------------------------------
// User has clicked on Yes or No to confirm deleting a paddock
//---------------------------------------------------------------------------
void __fastcall TUserManagementForm::deletePaddockCallback(bool deleteConfirmed)
   {
   if (deleteConfirmed)
      {
      try
         {
         AnsiString userName = UserList->Items->Strings[UserList->ItemIndex];
         AnsiString paddockName = PaddockList->Items->Strings[PaddockList->ItemIndex];
         data->deletePaddock(userName.c_str(), paddockName.c_str());
         populatePaddockList();
         webSession->show(this);
         }
      catch (const exception& err)
         {
         webSession->showMessage(err.what());
         }
      }
   }
//---------------------------------------------------------------------------
// User has clicked on edit paddock.
//---------------------------------------------------------------------------
void __fastcall TUserManagementForm::EditPaddockButtonClick(
      TObject *Sender)
   {
   if (UserList->ItemIndex >= 0 && PaddockList->ItemIndex >= 0)
      {
      AnsiString userName = UserList->Items->Strings[UserList->ItemIndex];
      AnsiString paddockName = PaddockList->Items->Strings[PaddockList->ItemIndex];
      webSession->showPaddockForm(userName.c_str(), paddockName.c_str(), false, true);
      }
   }
//---------------------------------------------------------------------------
void __fastcall TUserManagementForm::ReportsButtonClick(TObject *Sender)
   {
   if (UserList->ItemIndex >= 0)
      {
      AnsiString userName = UserList->Items->Strings[UserList->ItemIndex];
      webSession->showReportsForm(userName.c_str(), false, true);
      }
   }
//---------------------------------------------------------------------------
void __fastcall TUserManagementForm::ChangeDetailsButtonClick(
      TObject *Sender)
   {
   if (UserList->ItemIndex >= 0)
      {
      string userName = UserList->Items->Strings[UserList->ItemIndex].c_str();
      webSession->showUserDetailsForm(userName);
      }
   }
//---------------------------------------------------------------------------

//---------------------------------------------------------------------------

//---------------------------------------------------------------------------


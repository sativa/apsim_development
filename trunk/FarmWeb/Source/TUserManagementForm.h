//---------------------------------------------------------------------------
#ifndef TUserManagementFormH
#define TUserManagementFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <IWAppForm.hpp>
#include "IWBaseControl.hpp"
#include "IWCompLabel.hpp"
#include "IWControl.hpp"
#include "IWVCLBaseControl.hpp"
#include "IWOutlookBar.hpp"
#include "IWCompEdit.hpp"
#include "IWCompListbox.hpp"
#include "IWTMSCal.hpp"
#include "IWCompButton.hpp"
#include <ImgList.hpp>
#include "IWCompRectangle.hpp"
#include "IWBaseHTMLControl.hpp"
class Data;
class TWebSession;
//---------------------------------------------------------------------------
// This form shows the user all paddock information.
//---------------------------------------------------------------------------
class TUserManagementForm: public TIWAppForm
   {
   __published:	// IDE-managed Components
      TIWButton *DeleteGrowerButton;
      TIWLabel *IWLabel1;
      TIWButton *AddConsultantButton;
      TIWListbox *UserList;
      TIWListbox *PaddockList;
      TIWLabel *IWLabel6;
      TIWButton *DeletePaddockButton;
      TIWButton *AddPaddockButton;
      TIWButton *EditPaddockButton;
      TIWButton *ReportsButton;
      TIWButton *ChangeDetailsButton;
      TIWButton *AddGrowerButton;
      void __fastcall AddConsultantButtonClick(TObject *Sender);
      void __fastcall DeleteGrowerButtonClick(TObject *Sender);
      void __fastcall UserListClick(TObject *Sender);
      void __fastcall AddPaddockButtonClick(TObject *Sender);
      void __fastcall DeletePaddockButtonClick(TObject *Sender);
      void __fastcall EditPaddockButtonClick(TObject *Sender);
      void __fastcall ReportsButtonClick(TObject *Sender);
      void __fastcall ChangeDetailsButtonClick(TObject *Sender);
      void __fastcall AddGrowerButtonClick(TObject *Sender);
   private:
      TWebSession* webSession;
      Data* data;
      std::string userName;

      //---------------------------------------------------------------------------
      // populate the list box.
      //---------------------------------------------------------------------------
      void populateUserList(void);

      //---------------------------------------------------------------------------
      // Fill paddock list with paddock names.
      //---------------------------------------------------------------------------
      void populatePaddockList(void);

      //---------------------------------------------------------------------------
      // User has clicked on Yes or No to confirm deleting a user.
      //---------------------------------------------------------------------------
      void __fastcall deleteUserCallback(bool deleteConfirmed);

      //---------------------------------------------------------------------------
      // User has clicked on Yes or No to confirm deleting a paddock
      //---------------------------------------------------------------------------
      void __fastcall deletePaddockCallback(bool deleteConfirmed);

      //---------------------------------------------------------------------------
      // User has finished entering a new consultant
      //---------------------------------------------------------------------------
      void __fastcall addConsultantCallback(bool okClicked,
                                            AnsiString text1,
                                            AnsiString text2,
                                            AnsiString text3,
                                            AnsiString text4);

      //---------------------------------------------------------------------------
      // User has finished entering a new grower
      //---------------------------------------------------------------------------
      void __fastcall addGrowerCallback(bool okClicked,
                                        AnsiString text1,
                                        AnsiString text2,
                                        AnsiString text3,
                                        AnsiString text4);

      //---------------------------------------------------------------------------
      // User has finished adding a new paddock.
      //---------------------------------------------------------------------------
      void __fastcall addPaddockCallback(bool okClicked,
                                         AnsiString text1,
                                         AnsiString text2,
                                         AnsiString text3,
                                         AnsiString text4);




   public:
      __fastcall TUserManagementForm(TComponent* Owner);

      void setup(TWebSession* webSession,
                 Data* d, const std::string& userName);

   };
//---------------------------------------------------------------------------
#endif

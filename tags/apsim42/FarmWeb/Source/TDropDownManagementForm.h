//---------------------------------------------------------------------------
#ifndef TDropDownManagementFormH
#define TDropDownManagementFormH
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
#include "IWCompMemo.hpp"
#include "Data.h"
#include "IWExtCtrls.hpp"
#include "IWHTMLControls.hpp"

class TWebSession;
//---------------------------------------------------------------------------
// This form shows the user all paddock information.
//---------------------------------------------------------------------------
class TDropDownManagementForm: public TIWAppForm
   {
   __published:	// IDE-managed Components
      TIWLabel *PromptLabel;
      TIWComboBox *TypeCombo;
      TIWListbox *ValueList;
      TIWLabel *IWLabel1;
      TIWRectangle *IWRectangle1;
   TIWImageFile *IWImageFile1;
   TIWLink *AddTypeButton;
   TIWLink *DeleteTypeButton;
   TIWImageFile *IWImageFile2;
   TIWImageFile *IWImageFile3;
   TIWLink *AddValueButton;
   TIWLink *DeleteValueButton;
   TIWImageFile *IWImageFile4;
      void __fastcall AddTypeButtonClick(TObject *Sender);
      void __fastcall DeleteTypeButtonClick(TObject *Sender);
      void __fastcall AddValueButtonClick(TObject *Sender);
      void __fastcall DeleteValueButtonClick(TObject *Sender);
      void __fastcall TypeComboChange(TObject *Sender);
   private:
      TWebSession* webSession;
      Data* data;

      void populateTypeCombo();
      void populateList();
      void __fastcall addTypeCallback(bool okClicked, AnsiString text1, AnsiString text2,
                                      AnsiString text3, AnsiString text4);
      void __fastcall deleteTypeCallback(bool yesClicked);
      void __fastcall addValueCallback(bool okClicked, AnsiString text1, AnsiString text2,
                                       AnsiString text3, AnsiString text4);
      void __fastcall deleteValueCallback(bool yesClicked);

   public:
      __fastcall TDropDownManagementForm(TComponent* Owner);

      void setup(TWebSession* session, Data* d);

   };
//---------------------------------------------------------------------------
#endif

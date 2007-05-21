//---------------------------------------------------------------------------
#ifndef TReportManagementFormH
#define TReportManagementFormH
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
class TReportManagementForm: public TIWAppForm
   {
   __published:	// IDE-managed Components
      TIWComboBox *ReportCombo;
      TIWMemo *ReportMemo;
      TIWRectangle *IWRectangle1;
   TIWFile *ImportFile;
   TIWLabel *PromptLabel;
   TIWLink *SaveButton;
   TIWImageFile *IWImageFile1;
   TIWImageFile *IWImageFile2;
   TIWLink *AddButton;
   TIWImageFile *IWImageFile3;
   TIWLink *DeleteButton;
   TIWImageFile *IWImageFile4;
   TIWLink *ImportButton;
      void __fastcall SaveButtonClick(TObject *Sender);
      void __fastcall DeleteButtonClick(TObject *Sender);
      void __fastcall ReportComboChange(TObject *Sender);
      void __fastcall AddButtonClick(TObject *Sender);
      void __fastcall ImportButtonClick(TObject *Sender);
   private:
      TWebSession* webSession;
      Data* data;
      unsigned pageNumber;

      void populateReportCombo();
      void populateReportMemo();
      void __fastcall deleteReportCallback(bool deleteConfirmed);

      //---------------------------------------------------------------------------
      // Add report callback.
      //---------------------------------------------------------------------------
      void __fastcall AddReportCallBack(bool okClicked,
                                        AnsiString text1,
                                        AnsiString text2,
                                        AnsiString text3,
                                        AnsiString text4);


   public:
      __fastcall TReportManagementForm(TComponent* Owner);

      void setup(TWebSession* webSession, Data* d, unsigned pageNumber);

   };
//---------------------------------------------------------------------------
#endif

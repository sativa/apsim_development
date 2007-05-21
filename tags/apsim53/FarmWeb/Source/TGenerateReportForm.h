//---------------------------------------------------------------------------
#ifndef TGenerateFormH
#define TGenerateFormH
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
#include "IWTMSCtrls.hpp"
#include "IWBaseHTMLControl.hpp"
#include "IWCompRectangle.hpp"
#include "IWCompCheckbox.hpp"
#include "IWExtCtrls.hpp"
#include "IWHTMLControls.hpp"
class Data;
class TWebSession;
//---------------------------------------------------------------------------
// This form shows the user all paddock information.
//---------------------------------------------------------------------------
class TGenerateForm: public TIWAppForm
   {
   __published:	// IDE-managed Components
      TIWRectangle *IWRectangle1;
      TIWListbox *ReportCombo;
      TIWButton *GenerateReportButton;
   TIWCheckBox *EmailFilesCheckBox;
   TIWLabel *IWLabel7;
   TIWLabel *IWLabel8;
   TIWLabel *IWLabel9;
      void __fastcall CreateReportButtonClick(TObject *Sender);
   private:
      TWebSession* webSession;
      Data* data;
      std::string userName;
      std::string paddockName;

      //---------------------------------------------------------------------------
      // Populate the report combo.
      //---------------------------------------------------------------------------
      void populateReportCombo();

   public:
      __fastcall TGenerateForm(TComponent* Owner);

      void setup(TWebSession* webSession,
                 Data* d,
                 const std::string& userName,
                 const std::string& paddockName,
                 bool readOnly);

   };
//---------------------------------------------------------------------------
#endif

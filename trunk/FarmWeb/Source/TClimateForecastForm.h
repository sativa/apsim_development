//---------------------------------------------------------------------------

#ifndef TClimateForecastFormH
#define TClimateForecastFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <IWAppForm.hpp>
#include "IWBaseControl.hpp"
#include "IWBaseHTMLControl.hpp"
#include "IWCompEdit.hpp"
#include "IWCompLabel.hpp"
#include "IWCompRectangle.hpp"
#include "IWControl.hpp"
#include "IWVCLBaseControl.hpp"
#include "IWCompListbox.hpp"
#include "IWOutlookBar.hpp"
#include "IWCompMemo.hpp"
#include "IWExtCtrls.hpp"
#include "IWHTMLControls.hpp"
class TWebSession;
class Data;
//---------------------------------------------------------------------------
class TClimateForecastForm: public TIWAppForm
   {
   __published:	// IDE-managed Components
      TIWLabel *Prompt1;
      TIWRectangle *IWRectangle1;
      TIWComboBox *PhaseCombo;
      TIWLabel *IWLabel1;
      TIWComboBox *MonthCombo;
      TIWLabel *IWLabel2;
      TIWEdit *Year1Edit;
      TIWEdit *Year2Edit;
      TIWEdit *Year5Edit;
      TIWEdit *Year3Edit;
      TIWEdit *Year4Edit;
   TIWLabel *IWLabel3;
   TIWMemo *SOIDescriptionMemo;
   TIWMemo *DavidSDescriptionMemo;
   TIWLabel *IWLabel4;
   TIWLink *SaveButton;
   TIWImageFile *IWImageFile1;
      void __fastcall SaveButtonClick(TObject *Sender);
   private:
      TWebSession* session;
      Data* data;

   public:		// User declarations
      __fastcall TClimateForecastForm(TComponent* Owner);

      void setup(TWebSession* sess, Data* d);
   };
//---------------------------------------------------------------------------
#endif

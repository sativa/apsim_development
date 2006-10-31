//---------------------------------------------------------------------------

#ifndef TQuestionFormH
#define TQuestionFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <IWAppForm.hpp>
#include "IWBaseControl.hpp"
#include "IWBaseHTMLControl.hpp"
#include "IWCompButton.hpp"
#include "IWCompLabel.hpp"
#include "IWControl.hpp"
#include "IWVCLBaseControl.hpp"
//---------------------------------------------------------------------------
typedef void __fastcall (__closure *TQuestionEvent)(bool userClickedYes);
class TQuestionForm: public TIWAppForm
   {
   __published:	// IDE-managed Components
      TIWButton *YesButton;
      TIWButton *NoButton;
   TIWLabel *PromptLabel;
      void __fastcall YesButtonClick(TObject *Sender);
      void __fastcall NoButtonClick(TObject *Sender);
   private:	// User declarations
      TQuestionEvent OnClickNotify;
   public:		// User declarations
      __fastcall TQuestionForm(TComponent* Owner);
      void setup(AnsiString prompt, TQuestionEvent callback);


   };
//---------------------------------------------------------------------------
#endif

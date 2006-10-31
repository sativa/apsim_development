//---------------------------------------------------------------------------

#ifndef TInfoFormH
#define TInfoFormH
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
#include "IWExtCtrls.hpp"
#include "IWHTMLControls.hpp"
typedef void __fastcall (__closure *TInfoEvent)(bool OkClicked,
                                                AnsiString text1,
                                                AnsiString text2,
                                                AnsiString text3,
                                                AnsiString text4);

//---------------------------------------------------------------------------
class TInfoForm: public TIWAppForm
   {
   __published:	// IDE-managed Components
      TIWLabel *Prompt1;
      TIWEdit *Edit1;
      TIWEdit *Edit2;
      TIWLabel *Prompt2;
      TIWEdit *Edit3;
      TIWLabel *Prompt3;
      TIWEdit *Edit4;
      TIWLabel *Prompt4;
      TIWRectangle *IWRectangle1;
   TIWImageFile *IWImageFile2;
   TIWLink *BackButton;
   TIWLink *OkButton;
   TIWImageFile *IWImageFile1;
      void __fastcall BackButtonClick(TObject *Sender);
      void __fastcall OkButtonClick(TObject *Sender);
   private:
      TInfoEvent onClickNotify;

   public:		// User declarations
           __fastcall TInfoForm(TComponent* Owner);

      void setup(AnsiString prompt1,
                 AnsiString prompt2,
                 AnsiString prompt3,
                 AnsiString prompt4,
                 TInfoEvent callback);
   };
//---------------------------------------------------------------------------
#endif

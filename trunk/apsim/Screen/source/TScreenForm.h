//---------------------------------------------------------------------------

#ifndef TScreenFormH
#define TScreenFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TScreenForm : public TForm
   {
   __published:	// IDE-managed Components
      TProgressBar *ProgressBar;
      TLabel *StartDateLabel;
      TLabel *EndDateLabel;
      TLabel *Label1;
      TPanel *Panel1;
      TButton *CloseButton;
      TCheckBox *PauseCheckBox;
      TMemo *Memo;
      void __fastcall PauseCheckBoxClick(TObject *Sender);
      void __fastcall FormShow(TObject *Sender);
   private:	// User declarations
   public:		// User declarations
      __fastcall TScreenForm(TComponent* Owner);

      void addLine(const string& line);
   };
//---------------------------------------------------------------------------
extern PACKAGE TScreenForm *ScreenForm;
//---------------------------------------------------------------------------
#endif

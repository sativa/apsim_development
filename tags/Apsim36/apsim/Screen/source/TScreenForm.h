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

#define WM_CLOSE_CLICKED_MSG WM_USER+1

//---------------------------------------------------------------------------
class TScreenForm : public TForm
   {
   __published:	// IDE-managed Components
      TProgressBar *ProgressBar;
      TLabel *StartDateLabel;
      TLabel *EndDateLabel;
   TLabel *TitleLabel;
      TLabel *ErrorLabel;
      TLabel *FinishedLabel;
      TLabel *CurrentDateLabel;
   TButton *CancelButton;
   TButton *PauseButton;
   TMemo *Memo;
   TCheckBox *PauseCheckBox;
      void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
      void __fastcall CancelButtonClick(TObject *Sender);
   void __fastcall PauseButtonClick(TObject *Sender);
   void __fastcall PauseCheckBoxClick(TObject *Sender);
   private:	// User declarations
   public:		// User declarations
      __fastcall TScreenForm(TComponent* Owner);
      __fastcall TScreenForm(HWND handle);

      void setup(void);
      void addLine(const string& line);
      void errorsWereEncountered();
      void simulationHasFinished(void);
   };
//---------------------------------------------------------------------------
extern PACKAGE TScreenForm *ScreenForm;
//---------------------------------------------------------------------------
#endif

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
   TLabel *ErrorLabel;
   TLabel *FinishedLabel;
   TLabel *CurrentDateLabel;
      void __fastcall PauseCheckBoxClick(TObject *Sender);
      void __fastcall FormShow(TObject *Sender);
   void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
   void __fastcall CloseButtonClick(TObject *Sender);
   private:	// User declarations
   public:		// User declarations
      __fastcall TScreenForm(TComponent* Owner);

      void addLine(const string& line);
      void errorsWereEncountered();
      void simulationHasFinished(void);
   };
//---------------------------------------------------------------------------
extern PACKAGE TScreenForm *ScreenForm;
//---------------------------------------------------------------------------
#endif

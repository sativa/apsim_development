//---------------------------------------------------------------------------

#ifndef TWizardFormH
#define TWizardFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include <Graphics.hpp>
#include <vector>
//---------------------------------------------------------------------------
class TWizardForm : public TForm
   {
   __published:	// IDE-managed Components
      TPanel *WizardPanel;
      TButton *NextButton;
      TButton *BackButton;
      TButton *CancelButton;
      TImage *Image1;
      void __fastcall FormShow(TObject *Sender);
      void __fastcall BackButtonClick(TObject *Sender);
      void __fastcall NextButtonClick(TObject *Sender);
      void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
   private:	// User declarations
      std::vector<TForm*> componentForms;
      unsigned currentForm;
      void showForm();
      void hideForm();

   public:		// User declarations
      __fastcall TWizardForm(TComponent* Owner);
      void addComponentForm(TForm* componentForm);
   };
//---------------------------------------------------------------------------
extern PACKAGE TWizardForm *WizardForm;
//---------------------------------------------------------------------------
#endif

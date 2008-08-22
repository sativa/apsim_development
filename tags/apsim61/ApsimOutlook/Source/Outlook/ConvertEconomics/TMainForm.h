//---------------------------------------------------------------------------

#ifndef TMainFormH
#define TMainFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
//---------------------------------------------------------------------------
class TMainForm : public TForm
   {
   __published:	// IDE-managed Components
      TLabel *Label1;
      TLabel *PromptLabel;
      TButton *OkButton;
      void __fastcall FormShow(TObject *Sender);
   void __fastcall OkButtonClick(TObject *Sender);
   private:	// User declarations
      void doConversion(const std::string& mdbFileName, const std::string& xmlFileName);

   public:		// User declarations
      __fastcall TMainForm(TComponent* Owner);
   };
//---------------------------------------------------------------------------
extern PACKAGE TMainForm *MainForm;
//---------------------------------------------------------------------------
#endif

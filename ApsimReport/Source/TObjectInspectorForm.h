//---------------------------------------------------------------------------

#ifndef TObjectInspectorFormH
#define TObjectInspectorFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TObjectInspectorForm : public TForm
   {
   __published:	// IDE-managed Components
      void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
   private:	// User declarations
   public:		// User declarations
      __fastcall TObjectInspectorForm(TComponent* Owner);
   };
//---------------------------------------------------------------------------
extern PACKAGE TObjectInspectorForm *ObjectInspectorForm;
//---------------------------------------------------------------------------
#endif

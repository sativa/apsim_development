//---------------------------------------------------------------------------

#ifndef TRotationFormH
#define TRotationFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include <Grids.hpp>
#include <Buttons.hpp>
#include <Mask.hpp>
#include <ComCtrls.hpp>
#include <vector>
class RotationAddIn;
//---------------------------------------------------------------------------
class TRotationForm : public TForm
{
__published:	// IDE-managed Components
   TBitBtn *BitBtn2;
   TBitBtn *BitBtn1;
   TCheckBox *RotationCheck;
   void __fastcall FormShow(TObject *Sender);
   void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
private:	// User declarations

public:		// User declarations
   __fastcall TRotationForm(TComponent* Owner);
   RotationAddIn* rotationAddIn;
};
//---------------------------------------------------------------------------
extern PACKAGE TRotationForm *RotationForm;
//---------------------------------------------------------------------------
#endif

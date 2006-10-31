//---------------------------------------------------------------------------

#ifndef TMissingParametersFormH
#define TMissingParametersFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
//---------------------------------------------------------------------------
class TMissingParametersForm : public TForm
{
__published:	// IDE-managed Components
   TMemo *Memo;
   TLabel *Label1;
   TButton *OkButton;
private:	// User declarations
public:		// User declarations
   __fastcall TMissingParametersForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TMissingParametersForm *MissingParametersForm;
//---------------------------------------------------------------------------
#endif

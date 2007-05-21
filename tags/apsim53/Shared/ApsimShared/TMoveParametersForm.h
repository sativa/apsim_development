//---------------------------------------------------------------------------

#ifndef TMoveParametersFormH
#define TMoveParametersFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "AdvEdBtn.hpp"
#include "AdvEdit.hpp"
#include "AdvFileNameEdit.hpp"
//---------------------------------------------------------------------------
class TMoveParametersForm : public TForm
{
__published:	// IDE-managed Components
   TLabel *Label1;
   TLabel *Label3;
   TAdvFileNameEdit *FileEdit;
   TButton *Button1;
   TButton *Button2;
private:	// User declarations
public:		// User declarations
   __fastcall TMoveParametersForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TMoveParametersForm *MoveParametersForm;
//---------------------------------------------------------------------------
#endif

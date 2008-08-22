//---------------------------------------------------------------------------
#ifndef TFilespec_formH
#define TFilespec_formH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Buttons.hpp>
#include <Mask.hpp>
#include "AdvDirectoryEdit.hpp"
#include "AdvEdBtn.hpp"
#include "AdvEdit.hpp"
//---------------------------------------------------------------------------
class TFilespec_import_form : public TForm
{
__published:	// IDE-managed Components
   TLabel *Label1;
   TLabel *Label2;
   TEdit *FilespecEdit;
   TBitBtn *BitBtn1;
   TBitBtn *BitBtn2;
   TAdvDirectoryEdit *DirectoryEdit;
private:	// User declarations
public:		// User declarations
   __fastcall TFilespec_import_form(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFilespec_import_form *Filespec_import_form;
//---------------------------------------------------------------------------
#endif

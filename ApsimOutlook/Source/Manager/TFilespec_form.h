//---------------------------------------------------------------------------
#ifndef TFilespec_formH
#define TFilespec_formH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "ToolEdit.hpp"
#include <Buttons.hpp>
#include <Mask.hpp>
//---------------------------------------------------------------------------
class TFilespec_import_form : public TForm
{
__published:	// IDE-managed Components
   TDirectoryEdit *DirectoryEdit;
   TLabel *Label1;
   TLabel *Label2;
   TEdit *FilespecEdit;
   TBitBtn *BitBtn1;
   TBitBtn *BitBtn2;
private:	// User declarations
public:		// User declarations
   __fastcall TFilespec_import_form(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFilespec_import_form *Filespec_import_form;
//---------------------------------------------------------------------------
#endif

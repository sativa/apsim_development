//---------------------------------------------------------------------------

#ifndef TLibraryFormH
#define TLibraryFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ComCtrls.hpp>
#include "AdvListV.hpp"
#include <Buttons.hpp>
#include <ExtCtrls.hpp>
#include "FlCtrlEx.hpp"
#include <FileCtrl.hpp>
//---------------------------------------------------------------------------
class TLibraryForm : public TForm
{
__published:	// IDE-managed Components
   TTabControl *TabControl;
   TFileListBoxEx *FileList;
   TButton *OkButton;
   TButton *Button1;
   void __fastcall FormShow(TObject *Sender);
   void __fastcall TabControlChange(TObject *Sender);
private:	// User declarations
   std::string libraryDirectory;
public:		// User declarations
   __fastcall TLibraryForm(TComponent* Owner);

   AnsiString getSelectedFile(void);
};
//---------------------------------------------------------------------------
extern PACKAGE TLibraryForm *LibraryForm;
//---------------------------------------------------------------------------
#endif

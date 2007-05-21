//---------------------------------------------------------------------------
#ifndef TMultiStringListFormH
#define TMultiStringListFormH
//---------------------------------------------------------------------------
#include <vcl\Classes.hpp>
#include <vcl\Controls.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include "TMultiStringList.h"
// ------------------------------------------------------------------
//  Short description:
//      Dialog for the TMultiStringListEditor that allows editing
//      of a TMultiStringList.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
class PACKAGE TMultiStringListForm : public TForm
   {
   __published:	// IDE-managed Components
      TListBox *ListBox;
      TButton *Button1;
      void __fastcall FormShow(TObject *Sender);
      void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
   void __fastcall ListBoxDblClick(TObject *Sender);
   private:	// User declarations
   public:		// User declarations
      __fastcall TMultiStringListForm(TComponent* Owner);

      TMultiStringList* Multi_string_list_ptr;
   };
//---------------------------------------------------------------------------
extern TMultiStringListForm *MultiStringListForm;
//---------------------------------------------------------------------------
#endif

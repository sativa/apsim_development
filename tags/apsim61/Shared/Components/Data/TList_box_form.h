//---------------------------------------------------------------------------
#ifndef TList_box_formH
#define TList_box_formH
//---------------------------------------------------------------------------
#include <vcl\Classes.hpp>
#include <vcl\Controls.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include <vcl\Buttons.hpp>
//---------------------------------------------------------------------------
class TList_box_form : public TForm
{
__published:	// IDE-managed Components
   TListBox *List_box;
   TBitBtn *Ok_button;
   TLabel *Label;
   TBitBtn *BitBtn1;
private:	// User declarations
public:		// User declarations
   __fastcall TList_box_form(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern TList_box_form *List_box_form;
//---------------------------------------------------------------------------
#endif

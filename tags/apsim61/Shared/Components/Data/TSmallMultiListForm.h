//---------------------------------------------------------------------------
#ifndef tSmallMultiListFormH
#define tSmallMultiListFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Buttons.hpp>
//---------------------------------------------------------------------------
class TSmallMultiListForm : public TForm
{
__published:	// IDE-managed Components
   TListBox *ListBox1;
   TBitBtn *BitBtn1;
   void __fastcall BitBtn1Click(TObject *Sender);
private:	// User declarations
public:		// User declarations
   __fastcall TSmallMultiListForm(TComponent* Owner);
   void Set_items (TStrings* Items);
   void Set_selected_items (TStrings* Items_to_select);
   void Get_selected_items (TStrings* Selected_items);
};
//---------------------------------------------------------------------------
extern PACKAGE TSmallMultiListForm *SmallMultiListForm;
//---------------------------------------------------------------------------
#endif

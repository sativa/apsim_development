//---------------------------------------------------------------------------

#ifndef viewH
#define viewH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "AdvGrid.hpp"
#include "BaseGrid.hpp"
#include <ComCtrls.hpp>
#include <Grids.hpp>
//---------------------------------------------------------------------------
class TViewForm : public TForm
{
__published:	// IDE-managed Components
   TAdvStringGrid *outputGrid;
   TStatusBar *StatusBar;
   void __fastcall outputGridClickCell(TObject *Sender, int ARow,int ACol);
   void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
private:	// User declarations
   void __fastcall OpenFile(void);
public:		// User declarations
   void __fastcall ViewFile(String fileName);
   __fastcall TViewForm(TComponent* Owner);
   void __fastcall RefreshView(void);
};
//---------------------------------------------------------------------------
extern PACKAGE TViewForm *ViewForm;
//---------------------------------------------------------------------------
#endif

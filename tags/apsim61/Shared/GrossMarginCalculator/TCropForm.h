//---------------------------------------------------------------------------

#ifndef TCropFormH
#define TCropFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Buttons.hpp>
#include <CheckLst.hpp>
#include <GrossMarginCalculator\GMCalculator.h>
//---------------------------------------------------------------------------
class TCropForm : public TForm
   {
   __published:	// IDE-managed Components
      TCheckListBox *CropList;
   TLabel *Label1;
   TButton *Button1;
   TButton *Button2;
   private:	// User declarations

   public:		// User declarations
      __fastcall TCropForm(TComponent* Owner);
      GMCalculator* data;
   };
//---------------------------------------------------------------------------
extern PACKAGE TCropForm *CropForm;
//---------------------------------------------------------------------------
#endif

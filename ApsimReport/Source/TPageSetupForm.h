//---------------------------------------------------------------------------

#ifndef TPageSetupFormH
#define TPageSetupFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Buttons.hpp>
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TPageSetupForm : public TForm
{
__published:	// IDE-managed Components
   TLabel *Label1;
   TBevel *Bevel1;
   TLabel *Label2;
   TLabel *Label3;
   TLabel *Label4;
   TLabel *Label5;
   TEdit *TopEdit;
   TEdit *LeftEdit;
   TEdit *BottomEdit;
   TEdit *RightEdit;
   TLabel *Label6;
   TBevel *Bevel2;
   TSpeedButton *PortraitButton;
   TSpeedButton *LandscapeButton;
   TBitBtn *BitBtn1;
   TBitBtn *BitBtn2;
private:	// User declarations
public:		// User declarations
   __fastcall TPageSetupForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TPageSetupForm *PageSetupForm;
//---------------------------------------------------------------------------
#endif

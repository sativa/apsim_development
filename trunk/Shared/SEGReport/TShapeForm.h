//---------------------------------------------------------------------------

#ifndef TShapeFormH
#define TShapeFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "TShape.h"
#include "TPropertyForm.h"
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TShapeForm : public TPropertyForm
{
__published:	// IDE-managed Components
   TLabel *Label3;
   TLabel *Label4;
   TColorBox *BrushColourCombo;
   TColorBox *PenColourCombo;
   TComboBox *ShapeCombo;
   TLabel *Label5;
   void __fastcall ShapeComboChange(TObject *Sender);
   void __fastcall PenColourComboChange(TObject *Sender);
   void __fastcall BrushColourComboChange(TObject *Sender);
private:	// User declarations
   ::TShape* shape;

public:		// User declarations
   __fastcall TShapeForm(TComponent* Owner);
   virtual void setComponent(TComponent* component);

};
//---------------------------------------------------------------------------
extern PACKAGE TShapeForm *ShapeForm;
//---------------------------------------------------------------------------
#endif

//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TShapeForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "TPropertyForm"
#pragma link "AdvPanel"
#pragma resource "*.dfm"
TShapeForm *ShapeForm;

TColor colours[16] =
   {clBlack, clMaroon, clGreen, clOlive, clNavy, clPurple, clTeal, clGray,
    clSilver, clRed, clLime, clYellow, clBlue, clFuchsia, clAqua, clWhite};

//---------------------------------------------------------------------------
__fastcall TShapeForm::TShapeForm(TComponent* Owner)
   : TPropertyForm(Owner)
{
}
//---------------------------------------------------------------------------
void TShapeForm::setComponent(TComponent* component)
   {
   TPropertyForm::setComponent(component);
   shape = dynamic_cast< ::TShape*>(component);
   
   ShapeCombo->ItemIndex = shape->Shape;
   for (int i = 0; i != sizeof(colours); i++)
      {
      PenColourCombo->Selected = shape->Pen->Color;
      BrushColourCombo->Selected = shape->Brush->Color;
      }
   }
//---------------------------------------------------------------------------
void __fastcall TShapeForm::ShapeComboChange(TObject *Sender)
   {
   shape->Shape = (int)ShapeCombo->ItemIndex;
   }
//---------------------------------------------------------------------------
void __fastcall TShapeForm::PenColourComboChange(TObject *Sender)
   {
   shape->Pen->Color = PenColourCombo->Selected;
   }
//---------------------------------------------------------------------------
void __fastcall TShapeForm::BrushColourComboChange(TObject *Sender)
   {
   shape->Brush->Color = BrushColourCombo->Selected;
   }
//---------------------------------------------------------------------------


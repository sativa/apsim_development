#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TShapeForm.h"
TColor colours[16] =
   {clBlack, clMaroon, clGreen, clOlive, clNavy, clPurple, clTeal, clGray,
    clSilver, clRed, clLime, clYellow, clBlue, clFuchsia, clAqua, clWhite};
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
#pragma link "asgcombo"
#pragma link "ColorCombo"
TShapeForm *ShapeForm;
//---------------------------------------------------------------------------
__fastcall TShapeForm::TShapeForm(TComponent* Owner)
   : TForm(Owner)
   {
   }
//---------------------------------------------------------------------------
void TShapeForm::setComponent(::TShape* s)
   {
   shape = s;
   ShapeCombo->ItemIndex = shape->Shape;
   for (int i = 0; i != sizeof(colours); i++)
      {
      if (shape->Pen->Color == colours[i])
         PenColourCombo->ItemIndex = i;
      if (shape->Brush->Color == colours[i])
         BrushColourCombo->ItemIndex = i;
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
   shape->Pen->Color = colours[PenColourCombo->ItemIndex];
   }
//---------------------------------------------------------------------------
void __fastcall TShapeForm::BrushColourComboChange(TObject *Sender)
   {
   shape->Brush->Color = colours[BrushColourCombo->ItemIndex];
   }
//---------------------------------------------------------------------------


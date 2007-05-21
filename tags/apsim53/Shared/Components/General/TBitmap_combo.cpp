//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TBitmap_combo.h"
#pragma package(smart_init)
#pragma resource "*.res"
//---------------------------------------------------------------------------
namespace Tbitmap_combo
   {
   void __fastcall PACKAGE Register()
      {
      TComponentClass classes[1] = {__classid(TBitmap_combo_box)};
      RegisterComponents("APSRU", classes, 0);
      }
   }
//---------------------------------------------------------------------------
__fastcall TBitmap_combo_box::TBitmap_combo_box(TComponent* Owner)
   : TCustomComboBox(Owner)
   {
   Glyphs = new Graphics::TBitmap;
   Style = csOwnerDrawFixed;
   }
//---------------------------------------------------------------------------
Graphics::TBitmap* __fastcall TBitmap_combo_box::Get_glyph()
   {
   return Glyphs;
   }
//---------------------------------------------------------------------------
void __fastcall TBitmap_combo_box::Set_glyph(Graphics::TBitmap* gly)
   {
   Glyphs->Assign(gly);
   Set_num_glyphs(1);
   }
//---------------------------------------------------------------------------
void __fastcall TBitmap_combo_box::Set_num_glyphs (int Num)
   {
   Num_glyphs = Num;

   // add 1 string to list for each glyph.
   Items->Clear();
   for (int i = 0; i < Num_glyphs; i++)
      Items->Add("");
   }
//---------------------------------------------------------------------------
int __fastcall TBitmap_combo_box::Get_num_glyphs ()
   {
   return Num_glyphs;
   }
//---------------------------------------------------------------------------
void __fastcall TBitmap_combo_box::DrawItem(int Index, const Windows::TRect &Rect,  TOwnerDrawState State)
   {
   // fill our rectangle.
   Canvas->Brush->Color = clWhite;
   Canvas->FillRect(Rect);
   Canvas->FillRect (Rect);

   // calculate the individual image width and height.
   int Width = Glyph->Width / Num_glyphs;
   int Height = Glyph->Height;

   // display bitmap using API calls.  There is a defect in TImageList.
   // On NT systems it works perfectly.  On Win95 systems it returns the
   // first image in the bitmap correctly but all subsequent images are
   // not returned correctly.
   HDC Memory_dc = CreateCompatibleDC(Canvas->Handle);
   SelectObject (Memory_dc, Glyphs->Handle);
   StretchBlt (Canvas->Handle, Rect.Left, Rect.Top, Width, Height,
               Memory_dc, Width*Index, 0, Width, Height, SRCCOPY);
   DeleteDC(Memory_dc);
   }


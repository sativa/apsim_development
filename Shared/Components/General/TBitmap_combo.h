//---------------------------------------------------------------------------
#ifndef bitcomboH
#define bitcomboH
//---------------------------------------------------------------------------
#include <vcl\SysUtils.hpp>
#include <vcl\Controls.hpp>
#include <vcl\Classes.hpp>
#include <vcl\Forms.hpp>
#include <vcl\StdCtrls.hpp>
#if (__BORLANDC__ == 0x520)
   #define PACKAGE
#endif
//---------------------------------------------------------------------------
class PACKAGE TBitmap_combo_box : public TCustomComboBox
{
private:
   Graphics::TBitmap* Glyphs;
   int Num_glyphs;

   // glyphs get and set methods.
   Graphics::TBitmap* __fastcall Get_glyph();
   void __fastcall Set_glyph(Graphics::TBitmap* gly);

   // num_glyphs get and set methods.
   int  __fastcall Get_num_glyphs ();
   void __fastcall Set_num_glyphs (int Num);

protected:
   virtual void __fastcall DrawItem(int Index, const Windows::TRect &Rect,  TOwnerDrawState State);
public:
   __fastcall TBitmap_combo_box(TComponent* Owner);
__published:
   __property DropDownCount;
   __property ItemHeight;
   __property Graphics::TBitmap* Glyph = {read=Get_glyph, write=Set_glyph};
   __property int Num_glyph = {read=Get_num_glyphs, write=Set_num_glyphs};

};
//---------------------------------------------------------------------------
#endif

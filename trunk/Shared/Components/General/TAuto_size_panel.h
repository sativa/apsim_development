//---------------------------------------------------------------------------
#ifndef TAuto_size_panelH
#define TAuto_size_panelH
//---------------------------------------------------------------------------
#include <SysUtils.hpp>
#include <Controls.hpp>
#include <Classes.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
// ------------------------------------------------------------------
//  Short description:
//      this class keeps track of all components and auto sizes and
//      positions the components on it.

//  Notes:

//  Changes:
//    DPH 15/7/98

// ------------------------------------------------------------------
class PACKAGE TAuto_size_panel : public TPanel
   {
   private:
      void __fastcall Position_all_components();
   protected:
   public:
      __fastcall TAuto_size_panel(TComponent* Owner);
      DYNAMIC void __fastcall Resize(void);
   __published:
      __property OnResize;
   };
//---------------------------------------------------------------------------
#endif

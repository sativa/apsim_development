//---------------------------------------------------------------------------
#ifndef TXY_panelH
#define TXY_panelH
//---------------------------------------------------------------------------
#include <SysUtils.hpp>
#include <Controls.hpp>
#include <Classes.hpp>
#include <Forms.hpp>
#include "TAnalysis_panel.h"
#include <components\general\TAuto_size_panel.h>
#include <ExtCtrls.hpp>
// ------------------------------------------------------------------
//  Short description:
//      this class encapsulates an XY chart on a panel.

//  Notes:

//  Changes:
//    DPH 30/7/98

// ------------------------------------------------------------------
class PACKAGE TXY_panel : public TAnalysis_panel
   {
   private:
   protected:
      virtual void Create_objects (void);
      void Refresh_chart_objects (void);

   public:
      __fastcall TXY_panel(TComponent* Owner);
   __published:
   };
//---------------------------------------------------------------------------
#endif

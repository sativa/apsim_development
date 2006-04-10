//---------------------------------------------------------------------------
#ifndef TSummary_panelH
#define TSummary_panelH
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
//      this component gives the user a summary panel.

//  Notes:
//

//  Changes:
//    DPH 15/7/98

// ------------------------------------------------------------------
class PACKAGE TSummary_panel : public TAnalysis_panel
   {
   private:
   protected:
      virtual void Create_objects (void);
      virtual void Refresh_chart_objects (void);

   public:
      __fastcall TSummary_panel(TComponent* Owner);
      virtual bool ShowData(void);
   __published:
   };
//---------------------------------------------------------------------------
#endif

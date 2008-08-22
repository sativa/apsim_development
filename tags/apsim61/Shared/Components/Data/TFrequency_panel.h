//---------------------------------------------------------------------------
#ifndef TFrequency_panelH
#define TFrequency_panelH
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
//      this component gives the user a frequency analysis component +
//      a frequency chart.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
class PACKAGE TFrequency_panel : public TAnalysis_panel
   {
   private:
   protected:
      virtual void Create_objects (void);
      virtual void Refresh_chart_objects (void);
   public:
      __fastcall TFrequency_panel(TComponent* Owner);
   __published:
   };
//---------------------------------------------------------------------------
#endif

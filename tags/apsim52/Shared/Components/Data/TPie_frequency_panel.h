//---------------------------------------------------------------------------
#ifndef TPie_frequency_panelH
#define TPie_frequency_panelH
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
//    constructor

//  Notes:

//  Changes:
//    DPH 20/7/1998

// ------------------------------------------------------------------
class PACKAGE TPie_frequency_panel : public TAnalysis_panel
   {
   private:
   protected:
      virtual void Create_objects (void);
      virtual void Refresh_chart_objects (void);

   public:
      __fastcall TPie_frequency_panel(TComponent* Owner);
   __published:
   };
//---------------------------------------------------------------------------
#endif

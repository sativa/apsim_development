//---------------------------------------------------------------------------
#ifndef TTime_series_panelH
#define TTime_series_panelH
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
//    this component looks after a time series analysis and chart.

//  Notes:

//  Changes:
//    DPH 20/7/1998

// ------------------------------------------------------------------
class PACKAGE TTime_series_panel : public TAnalysis_panel
   {
   private:
   protected:
      virtual void Create_objects (void);
      virtual void Refresh_chart_objects (void);

   public:
      __fastcall TTime_series_panel(TComponent* Owner);
   __published:
   };
//---------------------------------------------------------------------------
#endif

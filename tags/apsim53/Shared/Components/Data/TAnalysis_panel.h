//---------------------------------------------------------------------------
#ifndef TAnalysis_panelH
#define TAnalysis_panelH
//---------------------------------------------------------------------------
#include <SysUtils.hpp>
#include <Controls.hpp>
#include <Classes.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include <components\general\TAuto_size_panel.h>
#include "TAnalysis.h"
#include "TAnalysis_chart.h"
// ------------------------------------------------------------------
//  Short description:
//    this component acts as a base class for all analysis panels.

//  Notes:

//  Changes:
//    DPH 20/7/1998
//    dph 9/12/1999 added colour_background as an option C232

// ------------------------------------------------------------------
class PACKAGE TAnalysis_panel : public TAuto_size_panel
   {
   private:
      TAPSTable* FSource_data;
      bool FLarge_fonts;
      bool FColour_background;

      void __fastcall Set_large_fonts(bool large_fonts);
      void __fastcall Set_colour_background(bool colour_background);
      AnsiString __fastcall Get_main_field_name();

      void Delete_objects (void);

   protected:
      #define MAX_ANALYSES 5
      TAnalysis* Analyses[MAX_ANALYSES];
      #define MAX_CHARTS 5
      TAnalysis_chart* Analysis_charts[MAX_CHARTS];

      TAPSTable* __fastcall Get_destination_data (void);
      void       __fastcall Set_source_data (TAPSTable* source_data);


      virtual void Create_objects (void) {}
      virtual void Refresh_analysis_objects (void);
      virtual void Refresh_chart_objects (void);

   public:
      __fastcall TAnalysis_panel(TComponent* Owner);

      virtual void Init(void);
      virtual bool Edit(void);
      virtual void Refresh(void);
      virtual bool ShowData(void) {return false;}
      void Edit_chart(void);
      void Copy_to_clipboard(bool With_background = true);

   __published:
      __property TAPSTable* Source_data = {read=FSource_data, write=Set_source_data};
      __property TAPSTable* Destination_data = {read=Get_destination_data};
      __property bool Large_fonts = {read=FLarge_fonts, write=Set_large_fonts};
      __property bool Colour_background = {read=FColour_background, write=Set_colour_background};
      __property AnsiString Main_field_name = {read=Get_main_field_name};
   };
//---------------------------------------------------------------------------
#endif

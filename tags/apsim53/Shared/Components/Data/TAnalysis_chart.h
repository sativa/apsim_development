//---------------------------------------------------------------------------
#ifndef TAnalysis_chartH
#define TAnalysis_chartH
//---------------------------------------------------------------------------
#include <SysUtils.hpp>
#include <Controls.hpp>
#include <Classes.hpp>
#include <Forms.hpp>
#include <Chart.hpp>
#include <ExtCtrls.hpp>
#include <TeEngine.hpp>
#include <TeeProcs.hpp>
#include "TAPSTable.h"
#include <series.hpp>
#include "TAnalysis_Series.h"

// ------------------------------------------------------------------
//  Short description:
//      this component is derived from TChart and is integrated with the
//      analysis components allowing easy charting taking pivot fields
//      into account.

//  Notes:

//  Changes:
//    DPH 5/2/98
//    dph 9/12/1999 added colour_background as an option C232
//    DAH 31/10/00:  fixing d365 - made axis variable list an unordered set.
//                   added a typedef and new routine to add members to this structure


// ------------------------------------------------------------------
class PACKAGE TAnalysis_chart : public TChart
   {
   private:
      TAPSTable* FAPSTable;
      TAnalysisSeriesList* FAnalysisSeriesList;
      bool FDraw_background;
      bool FLarge_fonts;
      bool FColour_background;

      int Axis_label_font_size;
      int Axis_title_font_size;
      int Title_font_size;
      int Legend_font_size;
      int Footer_font_size;
      bool FShowSeriesNamesInLegend;

      AnsiString FTitle;
      AnsiString FFooter;

      void __fastcall FillPanelRect(const TRect& r);
      void __fastcall SetAnalysisSeriesList(TAnalysisSeriesList* list);
      void __fastcall Set_draw_background (bool draw_background);
      void ProduceAllSeries(void);
      void ProduceSeriesForData(const std::vector<TAPSRecord>::const_iterator& begin,
                                const std::vector<TAPSRecord>::const_iterator& end);
      void FormatAxes(void);
      void FormatChart(void);
   protected:

   public:
      __fastcall TAnalysis_chart(TComponent* Owner);
      __fastcall ~TAnalysis_chart(void);
      void Refresh (void);
      virtual bool edit (void) {return true;}

   __published:
      __property TAPSTable* APSTable = {read=FAPSTable, write=FAPSTable};
      __property TAnalysisSeriesList* AnalysisSeriesList = {read=FAnalysisSeriesList, write=SetAnalysisSeriesList};
      __property bool Draw_background = {read=FDraw_background, write=Set_draw_background};
      __property bool Large_fonts = {read=FLarge_fonts, write=FLarge_fonts};
      __property bool Colour_background = {read=FColour_background, write=FColour_background, nodefault};
      __property AnsiString ChartTitle = {read=FTitle, write=FTitle};
      __property AnsiString ChartFooter = {read=FFooter, write=FFooter};
      __property bool ShowSeriesNamesInLegend = {read=FShowSeriesNamesInLegend, write=FShowSeriesNamesInLegend};
   };
//---------------------------------------------------------------------------
#endif

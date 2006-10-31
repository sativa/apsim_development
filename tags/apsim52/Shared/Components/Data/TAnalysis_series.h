//---------------------------------------------------------------------------
#ifndef TAnalysis_seriesH
#define TAnalysis_seriesH
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
#include <list>
typedef enum {xyyy, yxxx} FieldPatternsType;
//typedef enum {bottom_axis, top_axis} XAxisLocationType;
//typedef enum {left_axis, right_axis} YAxisLocationType;
typedef enum {bottom_axis, top_axis, left_axis, right_axis} AxisLocationType;
typedef enum {lines, markers_lines, markers,
              vert_bar, vert_bar_stacked, vert_bar_100,
              horz_bar, horz_bar_stacked, horz_bar_100,
              box, box_no_whiskers, box_column, bar_percentile_line,
              pie} SeriesTypes;
// ------------------------------------------------------------------
//  Short description:
//      this class encapsulates a single series on a chart.

//  Notes:

//  Changes:
//    DPH 26/3/2001

// ------------------------------------------------------------------
class PACKAGE TAnalysisSeries : public TCollectionItem
   {
   private:
      TStringList* FFieldNames;
      AnsiString FSeriesTitle;
//      XAxisLocationType FXAxisLocation;
//      YAxisLocationType FYAxisLocation;
      AxisLocationType FXAxisLocation;
      AxisLocationType FYAxisLocation;
      AnsiString FTitle;
      SeriesTypes FSeriesType;
      bool FSingleSeriesOnly;
      bool FPutLabelOnY;
      bool FIsCumulative;
      AnsiString FSpecificDataBlockName;
      AnsiString FspecificXAxisTitle;
      AnsiString FspecificYAxisTitle;

      TChartSeries* SeriesPtr;

      virtual AnsiString __fastcall GetDisplayName(void) {return "TAnalysisSeries";}
      void CreateAndFormatSeriesObject(TChart* ChartPtr);
      void AddDataToBoxSeries(const std::vector<TAPSRecord>::const_iterator& begin,
                              const std::vector<TAPSRecord>::const_iterator& end);
      void AddDataToXYSeries(const std::vector<TAPSRecord>::const_iterator& begin,
                             const std::vector<TAPSRecord>::const_iterator& end);
      void AddDataToPercentileLineSeries(const std::vector<TAPSRecord>::const_iterator& begin,
                                         const std::vector<TAPSRecord>::const_iterator& end);
      void FormatAxis(TChartAxis* AxisPtr);
      void FormatSeriesTitle(const TAPSRecord& record);
      void CalcAxisLabelAndTitle(const TAPSRecord& record,
                                 std::string& axisLabel, std::string& axisTitle);
   public:
      __fastcall TAnalysisSeries(TCollection* parent);
      __fastcall ~TAnalysisSeries(void);

      void AddAndFormatSeriesObject(TChart* ChartPtr);
      void AddDataToSeries(const std::vector<TAPSRecord>::const_iterator& begin,
                           const std::vector<TAPSRecord>::const_iterator& end);
      void AddToAxisLabel(TChartAxis* AxisPtr);

   __published:
      __property TStringList*      FieldNames = {read=FFieldNames, write=FFieldNames};
      __property AnsiString        SeriesTitle = {read=FSeriesTitle, write=FSeriesTitle};
//      __property XAxisLocationType XAxisLocation = {read=FXAxisLocation, write=FXAxisLocation};
//      __property YAxisLocationType YAxisLocation = {read=FYAxisLocation, write=FYAxisLocation};
      __property AxisLocationType XAxisLocation = {read=FXAxisLocation, write=FXAxisLocation};
      __property AxisLocationType YAxisLocation = {read=FYAxisLocation, write=FYAxisLocation};
      __property SeriesTypes       SeriesType = {read=FSeriesType, write=FSeriesType};
      __property AnsiString        Title = {read=FTitle, write=FTitle};
      __property bool              SingleSeriesOnly = {read=FSingleSeriesOnly, write=FSingleSeriesOnly};
      __property bool              PutLabelOnY = {read=FPutLabelOnY, write=FPutLabelOnY};
      __property AnsiString        SpecificDataBlockName = {read=FSpecificDataBlockName, write=FSpecificDataBlockName};
      __property bool              IsCumulative = {read=FIsCumulative, write=FIsCumulative};
      __property AnsiString specificXAxisTitle = {read=FspecificXAxisTitle,write=FspecificXAxisTitle};
      __property AnsiString specificYAxisTitle = {read=FspecificYAxisTitle,write=FspecificYAxisTitle};

   };

// ------------------------------------------------------------------
//  Short description:
//      this class encapsulates a collection of series.

//  Notes:

//  Changes:
//    DPH 26/3/2001

// ------------------------------------------------------------------
class PACKAGE TAnalysisSeriesList : public TCollection
   {
   private:
      TComponent* Owner;
      virtual TAnalysisSeries* __fastcall GetItem(int index);
      virtual void __fastcall SetItem(int index, TAnalysisSeries* value);
   protected:
      DYNAMIC TPersistent* __fastcall GetOwner(void) {return Owner;}
   public:
      TAnalysisSeriesList(TComponent* owner);
      virtual TAnalysisSeries* __fastcall Add(void);
      __property TAnalysisSeries* Items[int index] = {read=GetItem, write=SetItem};
   };

//---------------------------------------------------------------------------
#endif

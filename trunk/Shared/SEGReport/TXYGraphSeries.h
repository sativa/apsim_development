//---------------------------------------------------------------------------
#ifndef TXYGraphSeriesH
#define TXYGraphSeriesH

#include <SysUtils.hpp>
#include <Controls.hpp>
#include <Classes.hpp>
#include <Forms.hpp>
#include <Chart.hpp>
#include <ExtCtrls.hpp>
#include <TeEngine.hpp>
#include <TeeProcs.hpp>
#include <series.hpp>
#include <list>
#include <gtQrCtrls.hpp>
class TSEGTable;

typedef enum {bottom_axis, top_axis, left_axis, right_axis} AxisLocationType;
typedef enum {lines, markers_lines, markers,
              vert_bar, vert_bar_stacked, vert_bar_100,
              horz_bar, horz_bar_stacked, horz_bar_100} SeriesTypes;
// ------------------------------------------------------------------
// this class encapsulates a single series on a chart.
// ------------------------------------------------------------------
class PACKAGE TXYGraphSeries : public TCollectionItem
   {
   private:
      AnsiString FXFieldName;
      TStringList* FFieldNames;
      AnsiString FSeriesTitle;
      AxisLocationType FXAxisLocation;
      AxisLocationType FYAxisLocation;
      AnsiString FTitle;
      SeriesTypes FSeriesType;
      bool FSingleSeriesOnly;
      bool FPutLabelOnY;
      bool FIsXCumulative;
      bool FIsYCumulative;
      AnsiString FSpecificDataBlockName;
      AnsiString FspecificXAxisTitle;
      AnsiString FspecificYAxisTitle;

      TChartSeries* SeriesPtr;

      void FormatAxis(TChartAxis* AxisPtr);
      void addTitleToAxis(TChartAxis* axis, AnsiString title);
      
   public:
      __fastcall TXYGraphSeries(TCollection* parent);
      __fastcall ~TXYGraphSeries(void);

      void CreateAndFormatSeriesObject(TQRDBChart* ChartPtr, TSEGTable* source);
      virtual void __fastcall Assign(TPersistent* fromObj);

   __published:
      __property AnsiString        XFieldName = {read=FXFieldName, write=FXFieldName};
      __property TStringList*      FieldNames = {read=FFieldNames, write=FFieldNames};
      __property AnsiString        SeriesTitle = {read=FSeriesTitle, write=FSeriesTitle};
      __property AxisLocationType XAxisLocation = {read=FXAxisLocation, write=FXAxisLocation};
      __property AxisLocationType YAxisLocation = {read=FYAxisLocation, write=FYAxisLocation};
      __property SeriesTypes       SeriesType = {read=FSeriesType, write=FSeriesType};
      __property AnsiString        Title = {read=FTitle, write=FTitle};
      __property bool              SingleSeriesOnly = {read=FSingleSeriesOnly, write=FSingleSeriesOnly};
      __property bool              PutLabelOnY = {read=FPutLabelOnY, write=FPutLabelOnY};
      __property AnsiString        SpecificDataBlockName = {read=FSpecificDataBlockName, write=FSpecificDataBlockName};
      __property bool              IsXCumulative = {read=FIsXCumulative, write=FIsXCumulative};
      __property bool              IsYCumulative = {read=FIsYCumulative, write=FIsYCumulative};
      __property AnsiString specificXAxisTitle = {read=FspecificXAxisTitle,write=FspecificXAxisTitle};
      __property AnsiString specificYAxisTitle = {read=FspecificYAxisTitle,write=FspecificYAxisTitle};

   };

// ------------------------------------------------------------------
// this class encapsulates a collection of series.
// ------------------------------------------------------------------
class PACKAGE TXYGraphSeriesList : public TCollection
   {
   private:
      TComponent* Owner;
      virtual TXYGraphSeries* __fastcall GetItem(int index);
      virtual void __fastcall SetItem(int index, TXYGraphSeries* value);
   protected:
      DYNAMIC TPersistent* __fastcall GetOwner(void) {return Owner;}
   public:
      TXYGraphSeriesList(TComponent* owner);

      virtual TXYGraphSeries* __fastcall Add(void);
      virtual void __fastcall Assign(TPersistent* fromObj);
      int IndexOf(AnsiString yname);

      __property TXYGraphSeries* Items[int index] = {read=GetItem, write=SetItem};
   };

// ------------------------------------------------------------------
// convert a series type to a string.
// ------------------------------------------------------------------
std::string SeriesTypeToString(SeriesTypes seriesType);

// ------------------------------------------------------------------
// convert a string to a series type
// ------------------------------------------------------------------
SeriesTypes SeriesTypeToString(const std::string& st);



#endif

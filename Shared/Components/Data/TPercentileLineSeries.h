//---------------------------------------------------------------------------
#ifndef TPercentileLineSeriesH
#define TPercentileLineSeriesH
#include <ErrorBar.hpp>
#include <vector>
#include <string>
// ------------------------------------------------------------------
//  Short description:
//      this class provides a percentile line series for TeeChart.

//  Notes:

//  Changes:
//    DPH 4/9/01

// ------------------------------------------------------------------
class PACKAGE TPercentileLineSeries : public TErrorBarSeries
   {
   private:
      std::vector<std::string> xLabels;
      std::vector<double> x;
      std::vector<double> y1, y2, mean, median;
      bool doWhiskers;
      bool doMedian;

   protected:
      virtual void __fastcall DrawValue(int ValueIndex);
      virtual void __fastcall DrawBar(int BarIndex, int StartPos, int EndPos);
      virtual double __fastcall MinYValue(void);
      virtual double __fastcall MaxYValue(void);

   public:
      __fastcall TPercentileLineSeries(Classes::TComponent* AOwner);
      void addPoint(double y1, double y2, double mean, double median, const char* XLabel);
      void clearPoints(void);

      __property bool showWhiskers = {read=doWhiskers, write=doWhiskers};
      __property bool showMedian   = {read=doMedian,   write=doMedian};

   };
#endif

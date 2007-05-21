//---------------------------------------------------------------------------
#ifndef TBox_seriesH
#define TBox_seriesH
#include <ErrorBar.hpp>
#include <vector>
#include <string>
using std::vector;
using std::string;
// ------------------------------------------------------------------
//  Short description:
//      this class provides an box plot series for TeeChart.

//  Notes:

//  Changes:
//    DPH 29/7/98
//    DAH 12/9/00 - Made MinYValue() and MaxYValue() public functions rather than
//                  protected so that it can be used outside of this class heirarchy

// ------------------------------------------------------------------
class PACKAGE TBox_series : public TErrorBarSeries
   {
   private:
      vector<string> x_labels;
      vector<double> x;
      vector<double> y1, y2, y3, y4, y5;
      vector<double> y6;
      bool With_whiskers;
      bool Column_box;
      bool displayMedian;
      bool displayMean;

      void __fastcall DrawBox(int BarIndex, int StartPos, int EndPos);
      void __fastcall DrawColumnBar(int BarIndex, int StartPos, int EndPos);

   protected:
      virtual void __fastcall TBox_series::DrawValue(int ValueIndex);
      virtual void __fastcall DrawBar(int BarIndex, int StartPos, int EndPos);
//      virtual double __fastcall MinYValue(void);
//      virtual double __fastcall MaxYValue(void);

   public:
      __fastcall TBox_series(Classes::TComponent* AOwner);
      void Add_box_point (double y1, double y2, double y3, double y4, double y5, double y6, const char* XLabel);
      void Clear_box_points(void);
      void Set_box_with_whiskers (bool with_whiskers) {With_whiskers = with_whiskers;}
      void Set_column_box(bool column_box) {Column_box = column_box;}
      void Show_median(bool doShow) {displayMedian = doShow;}
      void Show_mean(bool doShow) {displayMean = doShow;}
      virtual double __fastcall MinYValue(void);
      virtual double __fastcall MaxYValue(void);
   };
#endif

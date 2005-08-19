//---------------------------------------------------------------------------
#ifndef TDecileFunctionH
#define TDecileFunctionH
#include <TeeFunci.hpp>
//---------------------------------------------------------------------------
// This class provides a decile function for TChart.
//---------------------------------------------------------------------------
class TDecileFunction : public TTeeFunction
   {
   private:
      int percentile;
      int haveGotPercentileFromUser;

      void getPercentile(void);

   public:
      __fastcall TDecileFunction(TComponent* owner);
   	virtual double __fastcall Calculate(Teengine::TChartSeries* SourceSeries, int FirstIndex, int LastIndex);
	   virtual double __fastcall CalculateMany(Classes::TList* SourceSeriesList, int ValueIndex);

   __published:
      __property int dummy = {read=haveGotPercentileFromUser, write=haveGotPercentileFromUser};
      __property int decile = {read=percentile, write=percentile};
   };
#endif

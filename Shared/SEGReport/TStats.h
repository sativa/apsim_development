//---------------------------------------------------------------------------
#ifndef TStatsH
#define TStatsH
#include "TSEGTable.h"
#include <stdexcept>
#include <string>
//---------------------------------------------------------------------------
// derived from TSEGTable, this creates a dataset that provides some
// simple stats on a source dataset variable. e.g. mean, median, percentiles...
//---------------------------------------------------------------------------
enum StatType {statMean, statCount, statMin, statMax, statSum, stat10, stat20, stat30,
               stat40, stat50, stat60, stat70, stat80, stat90};
typedef Set<StatType, statMean, stat90> StatSet;
class TStats : public TSEGTable
   {
   private:
      AnsiString fieldNameToAnalyse;
      StatSet statsToCalc;

      void __fastcall setFieldName(AnsiString fieldName);
      void __fastcall setStats(StatSet stats);

      virtual bool createFields(void) throw(std::runtime_error);
      virtual void storeRecords(void) throw(std::runtime_error);

   public:
      __fastcall TStats(TComponent* owner);
      __fastcall ~TStats();

      virtual void setProperty(const std::string& propertyName,
                               const std::string& propertyValue);
      
   __published:
      __property AnsiString fieldName = {read=fieldNameToAnalyse, write=setFieldName};
      __property StatSet stats = {read=statsToCalc, write=setStats};
   };
#endif

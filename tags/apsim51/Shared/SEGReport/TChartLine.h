//---------------------------------------------------------------------------

#ifndef TChartLineH
#define TChartLineH
#include "TSEGTable.h"
#include <stdexcept>
#include <vector>
#include <string>
#include <map>
//---------------------------------------------------------------------------
// derived from TSEGTable, this creates a dataset that represents a
// probability distribution of the source dataset.
//---------------------------------------------------------------------------
class TChartLine : public TSEGTable
   {
   private:
      AnsiString FieldName;
      float X1;
      float X2;
      AnsiString Label;

      void __fastcall setFieldName(AnsiString fieldName);
      void __fastcall setX1(float x1);
      void __fastcall setX2(float x2);
      void __fastcall setLabel(AnsiString label);
      virtual bool createFields(void) throw(std::runtime_error);
      virtual void storeRecords(void) throw(std::runtime_error);

   public:
      __fastcall TChartLine(TComponent* owner);
      __fastcall ~TChartLine();

   __published:
      __property AnsiString fieldName = {read=FieldName, write=setFieldName};
      __property float x1 = {read=X1, write=setX1};
      __property float x2 = {read=X2, write=setX2};
      __property AnsiString label = {read=Label, write=setLabel};

   };
#endif

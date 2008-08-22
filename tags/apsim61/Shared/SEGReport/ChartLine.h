//---------------------------------------------------------------------------

#ifndef ChartLineH
#define ChartLineH
#include <stdexcept>
#include <vector>
#include <string>
#include <map>
//---------------------------------------------------------------------------
// derived from TSEGTable, this creates a dataset that represents a
// probability distribution of the source dataset.
//---------------------------------------------------------------------------
class ChartLine : public TSEGTable
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
      __fastcall ChartLine(TComponent* owner);
      __fastcall ~ChartLine();

   __published:
      __property AnsiString fieldName = {read=FieldName, write=setFieldName};
      __property float x1 = {read=X1, write=setX1};
      __property float x2 = {read=X2, write=setX2};
      __property AnsiString label = {read=Label, write=setLabel};

   };
#endif

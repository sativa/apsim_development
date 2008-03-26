//---------------------------------------------------------------------------

#ifndef TRotationAddInH
#define TRotationAddInH
#include "TSEGTable.h"
#include <stdexcept>
#include <vector>
#include <string>
#include <map>
//---------------------------------------------------------------------------
// derived from TSEGTable, this creates a dataset that represents a
// probability distribution of the source dataset.
//---------------------------------------------------------------------------
class TRotationAddIn : public TSEGTable
   {
   private:
      TStrings* recognisedCropNames;
      TStrings* fieldsToAverage;

      void __fastcall setCropNames(TStrings* cropNames);
      void __fastcall setAveragedFields(TStrings* averagedFields);

      virtual bool createFields(void) throw(std::runtime_error);
      virtual void storeRecords(void) throw(std::runtime_error);
      bool isCropVariable(AnsiString fieldName) const;
      bool cropWasSown(AnsiString fieldName);
      bool cropHasNonZeroValue(AnsiString cropAcronym) const;
      bool doTotalVariable(AnsiString fieldName) const;

   public:
      __fastcall TRotationAddIn(TComponent* owner);
      __fastcall ~TRotationAddIn();

   __published:
      __property TStrings* cropNames = {read=recognisedCropNames, write=setCropNames};
      __property TStrings* averagedFields = {read=fieldsToAverage, write=setAveragedFields};
   };
#endif

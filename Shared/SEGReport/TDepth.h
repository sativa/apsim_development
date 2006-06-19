//---------------------------------------------------------------------------

#ifndef TDepthH
#define TDepthH
#include "TSEGTable.h"
#include <stdexcept>
#include <vector>
#include <string>
#include <map>
//---------------------------------------------------------------------------
// derived from TSEGTable, this creates a dataset that represents a
// probability distribution of the source dataset.
//---------------------------------------------------------------------------
class TDepth : public TSEGTable
   {
   private:
      virtual bool createFields(void) throw(std::runtime_error);
      virtual void storeRecords(void) throw(std::runtime_error);

      void discoverVariables(std::vector<std::string>& variableNames,
                             int& numLayers);

   public:
      __fastcall TDepth(TComponent* owner);
      __fastcall ~TDepth();

   __published:
   };
#endif

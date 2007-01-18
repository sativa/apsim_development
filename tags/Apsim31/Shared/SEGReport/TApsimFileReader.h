//---------------------------------------------------------------------------

#ifndef TApsimFileReaderH
#define TApsimFileReaderH
#include "TSEGTable.h"
#include <stdexcept>
//---------------------------------------------------------------------------
// derived from TSEGTable, this class reads all data from 1 or more
// APSIM output files.
//---------------------------------------------------------------------------
class TApsimFileReader : public TSEGTable
   {
   private:
      TStrings* files;
      std::vector<std::string> titles;

      void __fastcall setFileNames(TStrings* apsimFiles);
      virtual void createFields(void) throw(std::runtime_error);
      virtual void storeRecords(void) throw(std::runtime_error);
      void readAndStoreFields(const std::string& filename) throw(std::runtime_error);
      void readAndStoreRecords(const std::string& filename,
                               std::vector<std::string>& factorNames,
                               std::vector<std::string>& factorValues) throw(std::runtime_error);
      void readApsimHeader(std::istream& in,
                           std::vector<std::string>& fieldNames,
                           std::string& title) throw(std::runtime_error);
      bool readNextRecord(istream& in, vector<string>& fieldValues);
      void splitTitleIntoFactors(const std::string& title,
                                 std::vector<std::string>& factorNames,
                                 std::vector<std::string>& factorValues);

   public:
      __fastcall TApsimFileReader(TComponent* owner);
      __fastcall ~TApsimFileReader(void);

   __published:
      __property TStrings* filenames = {read=files, write=setFileNames};
   };
#endif
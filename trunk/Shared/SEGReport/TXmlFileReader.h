//---------------------------------------------------------------------------

#ifndef TXmlFileReaderH
#define TXmlFileReaderH
#include "TSEGTable.h"
#include <stdexcept>

class XMLNode;
//---------------------------------------------------------------------------
// derived from TSEGTable, this class reads all xml data from a file
//---------------------------------------------------------------------------
class TXmlFileReader : public TSEGTable
   {
   private:
      AnsiString fileName;
      AnsiString reportDirectory;
      std::vector<std::string> fieldNames;
      std::vector<std::string> fieldValues;

      void __fastcall setFileName(AnsiString fileName);
      virtual bool createFields(void) throw(std::runtime_error);
      virtual void storeRecords(void) throw(std::runtime_error);
      void readXmlFile();
      void readXmlNode(const XMLNode& node, const std::string& name);
      void relativeToAbsoluteFile(void);
      void absoluteToRelativeFile(void);
      virtual void __fastcall Loaded(void);

   public:
      __fastcall TXmlFileReader(TComponent* owner);
      __fastcall ~TXmlFileReader(void);

      // Called by SEGReport to give components a chance to know the current
      // report directory.  Used by ApsimFileReader to use relative paths.
      virtual void setReportDirectory(AnsiString reportDir);

      virtual void setProperty(const std::string& propertyName,
                               const std::string& propertyValue);

   __published:
      __property AnsiString filename = {read=fileName, write=setFileName};
   };
#endif

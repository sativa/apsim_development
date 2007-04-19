//---------------------------------------------------------------------------

#ifndef XmlFileReaderH
#define XmlFileReaderH
#include "DataProcessor.h"
class XMLNode;
//---------------------------------------------------------------------------
// derived from DataProcessor, this class reads all xml data from a file
//---------------------------------------------------------------------------
class XmlFileReader : public DataProcessor
   {
   private:
      std::vector<std::string> fieldNames, fieldValues;

      virtual void createFields(TDataSet* source, TDataSet* result);
      virtual void process(TDataSet* source, TDataSet* result);

      void readXmlFile(const std::string& fileName,
                       std::vector<std::string>& fieldNames,
                       std::vector<std::string>& fieldValues);
      void readXmlNode(const XMLNode& node, const std::string& name,
                       std::vector<std::string>& fieldNames,
                       std::vector<std::string>& fieldValues);
      std::string makeNameUnique(const XMLNode& parentNode, const XMLNode& node);

   public:
      XmlFileReader(const std::string& type, TComponent* owner)
         : DataProcessor(type, owner) { };
   };
#endif

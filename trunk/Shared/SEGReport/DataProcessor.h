//---------------------------------------------------------------------------
#ifndef DataProcessorH
#define DataProcessorH

#include <db.hpp>
class XMLNode;
//---------------------------------------------------------------------------
// This class is a base class for all data processors.
// A data processor is a class that takes a dataset (columns / rows) and
// produces a dataset. It is passed XML for configuration settings.
//---------------------------------------------------------------------------
class DataProcessor
   {
   public:
      DataProcessor(const std::string& type) : typeOfProcessor(type) { }
      virtual ~DataProcessor() { }

      // Create a dataprocessor object based on the settings
      // passed in. Caller is expected to free the object
      // when finished with it.
      static DataProcessor* factory(const XMLNode& properties);

      // Return true if the specified type is a valid one.
      static bool isValidType(const std::string& propertyType);

      // Set the properties of this processor.
      // Returns true if the state of the processor has been changed.
      bool setProperties(const XMLNode& properties);

      // Refresh this dataset
      void refresh(TDataSet* source, TDataSet* result);

      // Return any error message to caller.
      std::string getErrorMessage() {return errorMessage;}

      std::string type() {return typeOfProcessor;}

   protected:
      TDataSet* getDataSet(const std::string& dataSetName);

      std::string getProperty(const std::string& name);
      std::vector<std::string> getProperties(const std::string& name);

   private:
      std::string typeOfProcessor;
      std::string errorMessage;
      std::vector<std::string> propertyNames;
      std::vector<std::string> propertyValues;
      std::vector<std::string> groupByFilters;
      std::vector<std::string>::iterator currentGroupByFilter;

      virtual void createFields(TDataSet* source, TDataSet* result) = 0;
      virtual void process(TDataSet* source, TDataSet* result) = 0;

      bool groupRecords(TDataSet* source);
      void calcGroupByFilters(TDataSet* data);
      std::string calcGroupByFilter(TDataSet* data, const std::vector<std::string>& groupByFieldNames);
      void addGroupByFieldDefs(TDataSet* source, TDataSet* result);
      void addGroupByFieldValues(TDataSet* source, TDataSet* result, int startingRecNo);




   };

class DataContainer;
extern DataContainer* TopLevelContainer;   
#endif

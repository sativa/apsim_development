//---------------------------------------------------------------------------
#ifndef DataContainerH
#define DataContainerH
#include <general\xml.h>

class DataProcessor;
//---------------------------------------------------------------------------
// This class houses a data processor and result data and a collection of
// processors. This in effect gives us a tree of data processors and their
// resulting datasets.
//---------------------------------------------------------------------------
class DataContainer
   {
   public:
      DataContainer(TComponent* _owner);

      // Get and set the full XML for the system.
      void setup(const std::string& xml);

      // Return a dataset for the object with the specified name
      TDataSet* data(const std::string& name);

      // Return an error message for the object as specified
      // by the name
      std::string errorMessage(const std::string& name);

      // Refresh all data
      void refresh();

      // Add a new node with the given properties
      void add(const XMLNode& properties);

      // Delete a node with the given name
      void erase(const std::string& name);

      // Rename node
      void rename(const std::string& name, const std::string& newName);

      // Go set the properties for an existing node.
      void set(const XMLNode& properties);

      // Invalidate the specified node
      void invalidate(const std::string& name);

      // Return xml for whole system to caller.
      std::string xml();

      // Return a list of all child names to caller.
      std::vector<std::string> childNames();
   private:
      struct ProcessorData
         {
         std::string name;
         TDataSet* data;
         bool refreshNeeded;
         std::vector<std::string> sources;
         std::string errorMessage;
         std::string xml;
         };
      std::vector<ProcessorData> children;

      TComponent* owner;

      void save(string& st, int level);
      void refreshIfNecessary();

   };
#endif

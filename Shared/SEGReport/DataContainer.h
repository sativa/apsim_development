//---------------------------------------------------------------------------
#ifndef DataContainerH
#define DataContainerH
#include <general\xml.h>

class DataProcessor;
//---------------------------------------------------------------------------
// This class houses a data processor and result data and a collection of child
// data containers. This in effect gives us a tree of data processors and their
// resulting datasets.
// Paths should be of the form: rootNode\childNode\subChildNode
//---------------------------------------------------------------------------
class DataContainer
   {
   public:
      DataContainer(TComponent* _owner, DataContainer* parentContainer);
      ~DataContainer();

      // Get and set the full XML for the system.
      void setXML(const std::string& xml);
      std::string getXML();

      // Return a dataset for the object at the specified path.
      TDataSet* findData(const std::string& path);

      // Return all matching properties for the object at the
      // specified path.
      std::vector<std::string> findProperties(const std::string& path,
                                              const std::string& propertyName);

      // Return an error message for the object as specified
      // by the path.
      std::string findErrorMessage(const std::string& path);

      // Refresh all data
      void refresh();

   private:
      std::string name;
      DataContainer* parent;
      DataProcessor* processor;
      TDataSet* data;
      std::vector<DataContainer*> children;
      TComponent* owner;

      void setProperties(const XMLNode& properties);
      void save(string& st, int level);
      DataContainer* findContainer(const std::string& path);

   };
#endif

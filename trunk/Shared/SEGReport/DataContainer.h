//---------------------------------------------------------------------------
#ifndef DataContainerH
#define DataContainerH
#include "DataProcessor.h"
//---------------------------------------------------------------------------
// This class houses a data processor and result data and a collection of child
// data containers. This in effect gives us a tree of data processors and their
// resulting datasets.
//    Paths are of the form:  root\child1\subchild1
//    Property strings are XML.
//---------------------------------------------------------------------------
class __declspec(dllexport) DataContainer
   {
   public:
      DataContainer(TComponent* _owner) : processor(NULL), data(NULL), owner(_owner) {}
      virtual ~DataContainer() {clear();}

      // Clear everything from this data container and all children.
      void clear();

      // Setup the data container using the specified properties. This method is
      // called to completely initialise the system.
      void setup(const std::string& properties);

      // Set the properties for the specified data path.
      // Path must be an absolute path including the name of the root node.
      // e.g. data\apsimfilereader
      // Return true if properties were actually changed ie. a refresh is needed.
      bool setProperties(const std::string& path, const std::string& properties);

      // Go find the DataContainer as specified by path.
      // Path must be an absolute path including the name of the root node.
      // e.g. data\apsimfilereader
      DataContainer* findContainer(const std::string& path);

      // Return a result dataset for the absolute edata path.
      TDataSet* findData(const std::string& path);

      // Return a result dataset for the specified container name.
      // Name (not a path) will be searched for recursively through the
      // tree of containers.
      TDataSet* searchForData(const std::string& name);

      // Return an error message for the specified data path.
      std::string getErrorMessage(const std::string& path);

      // Refresh the data container as specified by path.
      void refresh(const std::string& path);

      // return processor to caller.
      DataProcessor* getProcessor() {return processor;}

      // save all properties to the specified xml node.
      void save(string& st, int level);

      // return name to caller.
      std::string getName() {return name;}
   private:
      std::string name;
      DataProcessor* processor;
      TDataSet* data;
      std::vector<DataContainer*> children;
      TComponent* owner;

      bool setProperties(const XMLNode& properties);
      void refresh(TDataSet* source);

   };
#endif

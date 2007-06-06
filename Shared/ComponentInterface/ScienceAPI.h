//---------------------------------------------------------------------------

#ifndef ScienceAPIH
#define ScienceAPIH

#include <string>
#include <vector>
#include <general/platform.h>
#include <boost/function.hpp>
#include <ComponentInterface/datatypes.h>

namespace protocol {
   class Component;
   };

class DeletableThing
   {
   public:
      virtual ~DeletableThing() {}
   };

class EXPORT ScienceAPI
   {
   public:
      ScienceAPI(protocol::Component* component);
      ~ScienceAPI();

      protocol::Component* getComponent() {return component;}   // get rid of this ASAP.

      // read methods.
      bool read(const std::string& name, int& data, int lower, int upper);
      bool read(const std::string& name, float& data, float lower, float upper);
      bool read(const std::string& name, double& data, double lower, double upper);
      bool read(const std::string& name, std::string& data);

      bool read(const std::string& name, std::vector<int>& data, int lower, int upper);
      bool read(const std::string& name, std::vector<float>& data, float lower, float upper);
      bool read(const std::string& name, float data[], int& numVals, float lower, float upper);
      bool read(const std::string& name, std::vector<double>& data, float lower, float upper);
      bool read(const std::string& name, std::vector<std::string>& data);

      bool readOptional(const std::string& name, int& data, int lower, int upper);
      bool readOptional(const std::string& name, float& data, float lower, float upper);
      bool readOptional(const std::string& name, double& data, double lower, double upper);
      bool readOptional(const std::string& name, std::string& data);

      bool readOptional(const std::string& name, std::vector<int>& data, int lower, int upper);
      bool readOptional(const std::string& name, std::vector<float>& data, float lower, float upper);
      bool readOptional(const std::string& name, float data[], int& numVals, float lower, float upper);
      bool readOptional(const std::string& name, std::vector<double>& data, float lower, float upper);
      bool readOptional(const std::string& name, std::vector<std::string>& data);

      void setClass1(const std::string& class1) {currentClass1 = class1;}
      void setClass2(const std::string& class2) {currentClass2 = class2;}

      // get methods
      bool get(const std::string& name, const std::string& units, std::vector<float>& data, float lower, float upper);
      bool getOptional(const std::string& name, const std::string& units, std::vector<float>& data, float lower, float upper);

      // set methods
      void set(const std::string& name, const std::string& units, std::vector<float>& data);

      // event handlers
      #define TimeFunctionType boost::function1<void, protocol::TimeType& >
      #define TimeFunction(address) TimeFunctionType(boost::bind(address, this, _1))
      void subscribe(const std::string& name, TimeFunctionType handler);

      #define NewMetFunctionType boost::function1<void, protocol::NewMetType& >
      #define NewMetFunction(address) NewMetFunctionType(boost::bind(address, this, _1))
      void subscribe(const std::string& name, NewMetFunctionType handler);

   private:
      std::string currentClass1;
      std::string currentClass2;
      protocol::Component* component;
      std::vector<DeletableThing*> stuffToDelete;

      std::string readFromSection(const std::string& section, const std::string& name);

   };

#endif

//---------------------------------------------------------------------------
#ifndef PlantComponentH
#define PlantComponentH


#include <boost/function.hpp>

class Plant;

// Pure virtual class to send variables to the rest of the system
// via sendVariable()
class baseInfo {
  protected:
   int                    myLength;
   protocol::DataTypeCode myType;
   string                 myName;
   string                 myUnits;
   string                 myDescription;
  public:
   baseInfo();
   ~baseInfo() {};
   virtual void sendVariable(protocol::Component *, protocol::QueryValueData&) = 0;
};

////////////////////////////////////////////////////////////////////////////
// A class to wrap a variable for reporting/manager/etc. Keeps a pointer 
// to memory region of scalar or array object, and knows how to send this
// to the system via sendVariable when asked.  
class varInfo : public baseInfo {
  private:
   void                  *myPtr;
  public:
   varInfo(const char *name, protocol::DataTypeCode type, int length, void *ptr, const char *units, const char *desc) {
      myName = name;
      myType = type;
      myLength = length;
      myPtr = ptr;
      myUnits = units;
      myDescription = desc;
   };
   ~varInfo() {};
   void sendVariable(protocol::Component *, protocol::QueryValueData&);
};

// Same as above, but stores pointers to function calls 
//typedef void (*ptr2getFn) (protocol::Component *, protocol::QueryValueData &);
class fnInfo : public baseInfo {
  private:
    boost::function2<void, protocol::Component *, protocol::QueryValueData &> myFn;
  public:
    fnInfo(const char *name, 
           protocol::DataTypeCode type, int length, 
           boost::function2<void, protocol::Component *, protocol::QueryValueData &> fn, 
           const char *units, const char *desc) {
      myFn = fn;
      myName = name;
      myType = type;
      myLength = length;
      myUnits = units;
      myDescription = desc;
   };
   ~fnInfo() {};
   void sendVariable(protocol::Component *s, protocol::QueryValueData &qd) {
      myFn(s, qd);
   };
};

typedef std::map<unsigned, baseInfo*>   UInt2InfoMap;
// ------------------------------------------------------------------
// This component acts as the interface between an instance of a
// Plant model and an APSIM simulation.
// ------------------------------------------------------------------
class PlantComponent : public protocol::Component
   {
   private:
      Plant     *plant;    // The plant module
      UInt2InfoMap vMap;   // List of variables we can send to system

      unsigned int getReg(const char *systemName,
                          protocol::DataTypeCode type, 
                          bool isArray, 
                          const char *units);
   public:
      PlantComponent(void);
      ~PlantComponent(void);
      virtual void doInit1(const FString& sdml);
      virtual void doInit2(void);
      virtual void respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);
      virtual void respondToMethod(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);
      virtual void respondToGet(unsigned int& fromID, protocol::QueryValueData& queryData);
      virtual bool respondToSet(unsigned int& fromID, protocol::QuerySetValueData& setValueData);
      virtual void onApsimGetQuery(struct protocol::ApsimGetQueryData& apsimGetQueryData);

      void   PlantComponent::addGettableVar(const char *systemName,
                                            protocol::DataTypeCode type,
                                            int length,
                                            void *ptr,
                                            const char *units,
                                            const char *desc);

      void   PlantComponent::addGettableVar(const char *systemName,
                                            protocol::DataTypeCode type,
                                            int length,
                                            boost::function2<void, protocol::Component *, protocol::QueryValueData &> ptr,
                                            const char *units,
                                            const char *desc);

      std::string readParameter(const string& sectionName,
                                const string& variableName)
          {
          std::string valueString = componentData->getProperty(sectionName, variableName);
          if (valueString.length() <= 0)
             {
             string baseSection = componentData->getProperty(sectionName, "derived_from");
             if (baseSection.length() > 0)
                {
                return readParameter(baseSection, variableName);
                }
             }
          return valueString;
          }
      std::string getProperty(const std::string &a, const std::string& b) const
         {
         return componentData->getProperty(a,b);
         }
   };
#endif

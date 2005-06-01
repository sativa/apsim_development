//---------------------------------------------------------------------------
#ifndef ComponentH
#define ComponentH

// oohhh this is messy
#include <map>
#include <boost/lexical_cast.hpp>
#include <boost/function.hpp>
#include <boost/bind.hpp>

#include "Messages.h"
#include "ProtocolVector.h"
#include "RegistrationType.h"
#include <ApsimShared/ApsimComponentData.h>

// turn of the warnings about "Functions containing for are not expanded inline.
#pragma warn -inl

namespace protocol {

// forward declarations of our friends.
class RegistrationItem;
class Registrations;
class Variants;
extern "C" _export void __stdcall messageToLogic (unsigned* instanceNumber,
                                                  Message* message,
                                                  bool* processed);
typedef _stdcall void (CallbackType)(const unsigned int *compInst, Message *message);
extern "C" void __stdcall createInstance(const char* dllFileName,
                                         const unsigned int* compID,
                                         const unsigned int* parentID,
                                         unsigned int* instanceNumber,
                                         const unsigned int* callbackArg,
                                         CallbackType* callback);
void callCallback(const unsigned int* callbackArg,
                   CallbackType* messageCallback,
                   Message* message);

// ------------------------------------------------------------------
// Abstract class to send variables to the rest of the system
// via sendVariable()
// ------------------------------------------------------------------
class __declspec(dllexport) baseInfo {
  protected:
   bool                   myIsArray;
   int                    myLength;
   protocol::DataTypeCode myType;
   FString                myName;
   FString                myUnits;
   FString                myDescription;
  public:
   baseInfo()
      {
      myLength = 0;  myIsArray = false;
      myType = protocol::DTunknown;
      };
   ~baseInfo() {};
   virtual void sendVariable(Component *, QueryValueData&) = 0;
   virtual std::string getXML();
};

// ------------------------------------------------------------------
// A class to wrap a variable for reporting/manager/etc. Keeps a pointer
// to memory region of scalar or array object, and knows how to send this
// to the system via sendVariable when asked.
// ------------------------------------------------------------------
class __declspec(dllexport) varInfo : public baseInfo {
  private:
   void *myPtr;
  public:
   varInfo(const char *name, DataTypeCode type, int length, void *ptr, const char *units, const char *desc) {
      myName = name;
      myType = type;
      myLength = length;
      myIsArray = length > 1;
      myPtr = ptr;
      myUnits = units;
      myDescription = desc;
   };
   ~varInfo() {};
   void sendVariable(Component *, QueryValueData&);

  };
class __declspec(dllexport) stringInfo : public baseInfo {
  private:
   string *myPtr;
  public:
   stringInfo(const char *name, string *ptr, const char *units, const char *desc) {
      myName = name;
      myType = DTstring;
      myLength = 1;
      myIsArray = 0;
      myPtr = ptr;
      myUnits = units;
      myDescription = desc;
   };
   ~stringInfo() {};
   void sendVariable(Component *, QueryValueData&);
};

// Same as above, but stores pointers to function calls, not memory regions.
class __declspec(dllexport) fnInfo : public baseInfo {
  private:
    boost::function2<void, Component *, QueryValueData &> myFn;
  public:
    fnInfo(const char *name,
           protocol::DataTypeCode type, bool isArray,
           boost::function2<void, Component *, QueryValueData &> fn,
           const char *units, const char *desc) {
      myFn = fn;
      myName = name;
      myType = type;
      myLength = -1;          // length is variable..
      myIsArray = isArray;
      myUnits = units;
      myDescription = desc;
   };
   ~fnInfo() {};
   void sendVariable(Component *s, QueryValueData &qd) { myFn(s, qd); };
};

typedef std::map<unsigned, baseInfo*>   UInt2InfoMap;
typedef boost::function3<void, unsigned &, unsigned &, protocol::Variant &> pfcall;
typedef std::multimap<unsigned, pfcall, less<unsigned> >   UInt2EventMap;

// ------------------------------------------------------------------
// Manages a single instance of an APSIM Component
// ------------------------------------------------------------------
class __declspec(dllexport) Component
   {
   public:
      Component(void);
      virtual ~Component(void);

      const char  *getName(void) {return name;};
      unsigned int getId(void) {return componentID;};

      // Add a registration.
      unsigned addRegistration(RegistrationType kind,
                               const FString& name,
                               const Type& type,
                               const FString& alias = "",
                               const FString& componentNameOrID = "");
      void deleteRegistration(RegistrationType kind,
                              unsigned int regID);

      std::string getProperty(const std::string &a, const std::string &b) const
         {
         return componentData->getProperty(a,b);
         }

      // Notify system of an error.
      void error(const FString& msg, bool isFatal);

      // Terminate simulation
      void terminateSimulation(void);

      // Get the value of a specific variable.  Return true if a value was
      // returned.
      bool getVariable(unsigned int variableID,
                       Variant*& value,
                       bool optional = false);

      // Get the multiple values of a specific variable.  Return true
      // if some values are returned.
      bool getVariables(unsigned int variableID, Variants*& values);

      // Set the value of a variable.  Returns true if value was changed.
      template <class T>
      bool setVariable(unsigned int variableID, const T& data)
         {
         setVariableSuccess = false;
         sendMessage(newRequestSetValueMessage(componentID, parentID, variableID,
                                               getRegistrationType(variableID),
                                               data));
         if (!setVariableSuccess)
            setVariableError(variableID);
         return setVariableSuccess;
         }

      // Publish an event.
      template <class T>
      void publish(unsigned int eventID, T& data)
         {
         sendMessage(newPublishEventMessage(componentID, parentID, eventID,
                                            getRegistrationType(eventID),
                                            data));
         }
      // Publish an event.
      template <class T>
      void publishArray(unsigned int eventID, T data[], unsigned int numValues)
         {
         sendMessage(newPublishEventMessage(componentID, parentID, eventID,
                                            getRegistrationType(eventID),
                                            data, numValues));
         }

      // Write a line to the summary file.
      void writeString(const FString& st);

      // Send a variable to another component.
      template <class T>
      void sendVariable(QueryValueData& queryValueData,
                        const T& value)
         {
         sendMessage(newReplyValueMessage(componentID,
                                          parentID,
                                          queryValueData.ID,
                                          getRegistrationType(queryValueData.ID),
                                          value));
         }

      // convert to and from a compname to an ID.
      bool componentNameToID(const FString& name, unsigned int& compID);
      bool componentIDToName(unsigned int compID, FString& name);

      // send a change order message.
      void changeComponentOrder(const FStrings& names)
         {
         sendMessage(newApsimChangeOrderMessage(componentID, parentID, names));
         }
      void setRegistrationType(unsigned int regID, const Type& type);
      std::string getDescription(void);

      // override these methods if necessary.
      virtual void doInit1(const FString& sdml);
      virtual void doInit2(void) { }
      virtual void doCommence(void) { }
      virtual void respondToEvent(unsigned int& fromID, unsigned int& eventID, Variant& variant);
      virtual void respondToGet(unsigned int& fromID, QueryValueData& queryData);
      virtual bool respondToSet(unsigned int& fromID, QuerySetValueData& setValueData) {return false;}
      virtual void notifyTermination(void) { }
      virtual void messageToLogic(const Message* message);

   protected:
      char         *dllName;
      unsigned int componentID;
      unsigned int parentID;
      ApsimComponentData* componentData;
      char* name;
      bool beforeInit2;

      // ********* LOW LEVEL ROUTINES - should not be used by regular components.
      virtual void onRequestComponentIDMessage(unsigned int fromID, RequestComponentIDData& data) { }
      virtual void onRequestSetValueMessage(unsigned int fromID, RequestSetValueData& data) { }
      virtual void onReturnComponentIDMessage(ReturnComponentIDData& data) { }
      virtual void onRegisterMessage(unsigned int fromID, protocol::RegisterData& registerData) { }
      virtual void onDeregisterMessage(unsigned int fromID, protocol::DeregisterData& registerData) { }
      virtual void onTerminateSimulationMessage(void) { }
      virtual void onGetValueMessage(unsigned int fromId, GetValueData& getValueData) { }
      virtual void onPublishEventMessage(unsigned int fromID, PublishEventData& publishEventData) { }
      virtual void onQueryInfoMessage(unsigned int fromID, unsigned int messageID, QueryInfoData& queryInfo) { }
      virtual void onQueryValueMessage(unsigned int fromID, QueryValueData& queryData);
      virtual void onCompleteMessage(CompleteData& completeData);
      virtual void onApsimGetQuery(ApsimGetQueryData& apsimGetQueryData) { }
      virtual bool onApsimSetQuery(ApsimSetQueryData& apsimSetQueryData) {return false;}
      virtual void onApsimChangeOrderData(protocol::MessageData& messageData) { }
      virtual void onQuerySetValueMessage(unsigned fromID, QuerySetValueData& querySetData);
      virtual void onReplyValueMessage(unsigned fromID, ReplyValueData replyValueData) { }

      // Send a message
      void sendMessage(Message* message)
         {
         bool doAck = message->toAcknowledge;
         if (doAck)
            {
            completeIDs.push_back(message->messageID);
            completeFound = false;
            }
         if (messageCallback != NULL)
            (*messageCallback)(callbackArg, message);
         if (doAck)
            waitForComplete();
         deleteMessage(message);
         }
      RegistrationItem* addRegistrationToList(RegistrationType kind,
                                              const FString& name,
                                              const Type& type,
                                              const FString& componentNameOrID = "");
      Type getRegistrationType(unsigned int regID);
      const char *getRegistrationName(unsigned int regID);
      unsigned getRegistrationID(const RegistrationType& type, const FString& eventName);
      bool getSetVariableSuccess(void) {return setVariableSuccess;}
      void setVariableError(unsigned int regID);

   private:
      Registrations* registrations;
      vector<ReturnInfoData*> returnInfos;
      unsigned int nameID;
      unsigned int typeID;
      unsigned int versionID;
      unsigned int authorID;
      unsigned int activeID;
      unsigned int stateID;
      unsigned int errorID;
      unsigned int summaryID;
      bool setVariableSuccess;
      vector<unsigned> completeIDs;
      bool completeFound;

      UInt2InfoMap getVarMap;                  // List of variables we can send to system
      UInt2EventMap eventMap;                  // List of events we handle

      const unsigned int* callbackArg;
      CallbackType* messageCallback;

      void setup(const char *dllname,
                 const unsigned int componentid,
                 const unsigned int parentid,
                 const unsigned int* callbackarg,
                 void* messagecallback);
      void storeName(const FString& fqn, const FString& sdml);

      void clearReturnInfos(void);
      void waitForComplete(void);

      friend TypeConverter;
      friend void __stdcall messageToLogic (unsigned* instanceNumber,
                                            Message* message,
                                            bool* processed);
      friend void __stdcall createInstance(const char* dllFileName,
                                           const unsigned int* compID,
                                           const unsigned int* parentID,
                                           unsigned int* instanceNumber,
                                           const unsigned int* callbackArg,
                                           CallbackType* callback);
      unsigned int getReg(const char *systemName,
                          DataTypeCode type,
                          bool isArray,
                          const char *units);

 public:
      // Get a variable from the system (into basic C datatypes)
      template <class T>
      bool getVariable(int regId,
                       T& value,
                       double lower,
                       double upper,
                       bool isOptional = false)
         {
         protocol::Variant* variant;
         if (getVariable(regId, variant, isOptional))
            {
            bool ok = variant->unpack(value);
            if (!ok)
               {
               char buffer[100];
               strcpy(buffer, "TypeConverter failed.\n"
                              "VariableName:");
               const char *variableName = getRegistrationName(regId);
               strcat(buffer, variableName);
               error(buffer, true);
               return false;
               }
            if (value < lower || value > upper)
               {
               const char *variableName = getRegistrationName(regId);
               string msg = string("Bound check warning while getting variable.\n"
                                   "Variable  : ") + variableName + string("\n"
                                   "Condition : ") + ftoa(lower, 2) + string(" <= ") +
                                    boost::lexical_cast<std::string>(value) + string(" <= ") + ftoa(upper, 2);
               error(msg.c_str(), false);
               }
            }
         else
            return false;

         return true;
         }

      template <class T>
      bool getVariable(int regId,
                       std::vector<T>& values,
                       double lower,
                       double upper,
                       bool isOptional = false)
         {
         values.clear();
         protocol::Variant* variant;
         if (getVariable(regId, variant, isOptional))
            {
            bool ok = variant->unpack(values);
            if (!ok)
               {
               char buffer[100];
               strcpy(buffer, "TypeConverter failed.\n"
                              "VariableName:");
               const char *variableName = getRegistrationName(regId);
               strcat(buffer, variableName);
               error(buffer, true);
               return false;
               }

            for (int i = 0; i < values.size(); i++)
               {
               if (values[i] < lower || values[i] > upper)
                   {
                   string variableName = getRegistrationName(regId);
                   string msg = string("Bound check warning while getting variable.\n"
                                       "Variable  : ") + variableName + string("(") + itoa(i+1) +  string(")\n"
                                       "Condition : ") + ftoa(lower, 2) + string(" <= ") +
                                boost::lexical_cast<string>(values[i]) + string(" <= ") + ftoa(upper, 2);
                   error(msg.c_str(), false);
                   }
               }
            }
         else
            return false;

         return (values.size()> 0);
         }

   // Read variable permutations
   bool readParameter(const FString& sectionName,
                      const FString& variableName,
                      FString& variableValue,
                      bool optional);

   std::string readParameter(const std::string& sectionName,
                             const std::string& variableName)
       {
       std::string valueString = componentData->getProperty(sectionName, variableName);
       if (valueString.length() <= 0)
          {
          std::string baseSection = componentData->getProperty(sectionName, "derived_from");
          if (baseSection.length() > 0)
             {
             return readParameter(baseSection, variableName);
             }
          }
       // remove any Units specifier "(..)" here
       int posBracket = valueString.find('(');
       if (posBracket != string::npos)
         valueString = valueString.substr(0,posBracket);

       return valueString;
       };

      // Search a list of "sections" for a parameter.
    std::string readParameter(const std::vector<std::string> &sectionNames,
                              const std::string& variableName)
       {
       string result;
       for (unsigned int i = 0; i < sectionNames.size(); i++)
         if ((result = readParameter(sectionNames[i], variableName)) != "")
            return result;
       return result;
       };


   template <class T>
   bool readParameter(const string &sectionName,
                      const string &variableName,
                      T &value,
                      double lower,
                      double upper,
                      bool optional=false)
      {
      // 1. Get the variable as string
      string datastring = readParameter(sectionName, variableName);

      if (datastring.size() == 0)
         {
         if (!optional)
             {
             string msg = string("Cannot find a parameter in any of the files/sections\n"
                                 "specified in the control file.\n"
                                 "Parameter name = ") + variableName;
             error(msg.c_str(), true);
             }
         return false;
         }

      // 2. Convert it
      try { value = boost::lexical_cast<T> (datastring); }
      catch(boost::bad_lexical_cast &e)
         {
         string msg = string("Problem converting variable to ") +
                             string(typeid(T).name()) + string(" type.\n"
                             "Parameter name = ") + variableName + string("\n"
                             "Value          = '") + datastring + string("'");
         error(msg.c_str(), true);
         return false;
         }

      // 3. Check bounds
      if (value < lower || value > upper)
         {
         string msg = string(
                    "Bound check warning while reading parameter.\n"
                    "Variable  : ") + variableName + string("\n"
                    "Condition : ") + ftoa(lower, 2) + string(" <= ") +
                    datastring + string(" <= ") + ftoa(upper, 2);
         error(msg.c_str(), false);
         }

      return true;
      }

   template <class T>
   bool readParameter(const string &sectionName,
                      const string &variableName,
                      std::vector <T> &values,
                      double lower,
                      double upper,
                      bool optional=false)
      {
      values.clear();

      // 1. Get the variable as string
      string datastring = readParameter(sectionName, variableName);
      if (datastring == "")
         {
         if (!optional)
             {
             string msg = string("Cannot find a parameter in any of the files/sections\n"
                                 "specified in the control file.\n"
                                 "Parameter name = ") + variableName;
             error(msg.c_str(), true);
             }
         return false;
         }

      // 2. Convert it
      std::vector <string> value_strings;
      splitIntoValues (datastring," ",value_strings);
      for (int i=0; i!=value_strings.size();i++)
        {
        T value;
        try { value = boost::lexical_cast<T> (value_strings[i]); }
        catch(boost::bad_lexical_cast &e)
           {
           string msg = string("Problem converting variable to ") +
                               string(typeid(T).name()) + string(" type.\n"
                               "Parameter name = ") + variableName + string("\n"
                               "Value          = '") + value_strings[i] + string("'\n");
           error(msg.c_str(), true);
           return false;
           }
        values.push_back(value);

        // 3. Check bounds
        if (value < lower || value > upper)
            {
            //NB. array index reported as origin 1 indexing!!!
            string msg = string(
                    "Bound check warning while reading parameter.\n"
                    "Variable  : ") + variableName + string("(") + itoa(i+1) +  string(")\n"
                    "Condition : ") + ftoa(lower, 2) + string(" <= ") +
                    value_strings[i] + string(" <= ") + ftoa(upper, 2);
            error(msg.c_str(), false);
            }
        }
      return (value_strings.size() > 0);
      };

       // C arrays
      template <class T>
      bool readParameter(const string &sect, const string &name,
                         T *valarray, int &numvals,
                         double lower, double upper, bool isoptional = false)
         {
         std::vector<T> vv;
         bool result = readParameter(sect, name, vv, lower, upper, isoptional);
         for (unsigned int i = 0; i < vv.size(); i++)
             valarray[i] = vv[i];
         numvals = vv.size();
         return result;
         };

      template <class T>
      bool readParameter(const std::vector<std::string> &sections,
                         const std::string &variableName,
                         T &value,
                         double lower,
                         double upper,
                         bool optional=false)
         {
         for (unsigned int i = 0; i < sections.size(); i++)
           if (readParameter(sections[i], variableName, value, lower, upper, true))
              return true;

         if (!optional)
            {
            std::string msg = string("Cannot find a parameter in any of the files/sections\n"
                                     "specified in the control file.\n"
                                     "Parameter name = ") + variableName;
            error(msg.c_str(), true);
            }
         return false;
         };

      template <class T>
      bool readParameter(const std::vector<string> &sects,
                         const std::string &name,
                         T *v, int &numvals,
                         double lower, double upper,
                         bool isOptional = false)
         {
         for (unsigned int i = 0; i < sects.size(); i++)
           if (readParameter(sects[i], name,  v, numvals, lower, upper, true))
              return true;

         if (!isOptional)
            {
            string msg = string("Cannot find a parameter in any of the files/sections\n"
                                 "specified in the control file.\n"
                                 "Parameter name = ") + name;
            error(msg.c_str(), true);
            }
         return false;
         };

      template <class T>
      bool readParameter(const std::vector<string> &sections,
                         const std::string &variableName,
                         std::vector <T> &values,
                         double lower,
                         double upper,
                         bool isOptional=false)
         {
         for (unsigned int i = 0; i < sections.size(); i++)
           if (readParameter(sections[i], variableName, values, lower, upper, true))
              return true;

         if (!isOptional)
            {
            string msg = string("Cannot find a parameter in any of the files/sections\n"
                                 "specified in the control file.\n"
                                 "Parameter name = ") + variableName;
            error(msg.c_str(), true);
            }
         return false;
         };


      // Add Variables to the Get list.
      // Function
      void addGettableVar(const char *systemName,
                          DataTypeCode type,
                          bool isArray,
                          boost::function2<void, Component *, QueryValueData &> ptr,
                          const char *units,
                          const char *desc)
          {
          // Get a system ID for it
          unsigned int id = getReg(systemName, type, isArray, units);
          // Add to variable map
          fnInfo *v = new fnInfo(systemName, type, isArray, ptr, units, desc);
          getVarMap.insert(UInt2InfoMap::value_type(id,v));
          };

      // scalar
      template <class T>
      void addGettableVar(const char *systemName,
                          T &value,
                          const char *units,
                          const char *desc)
          {
          DataTypeCode type = dataTypeCodeOf(value);
          unsigned int id = getReg(systemName, type, false, units);
          varInfo *v = new varInfo(systemName, type, 1, &value, units, desc);
          getVarMap.insert(UInt2InfoMap::value_type(id,v));
          };
      // Special case for stl strings
      void addGettableVar(const char *systemName,
                          std::string &value,
                          const char *units,
                          const char *desc)
          {
          unsigned int id = getReg(systemName, DTstring, false, units);
          stringInfo *v = new stringInfo(systemName, &value, units, desc);
          getVarMap.insert(UInt2InfoMap::value_type(id,v));
          };

      // C array
      template <class T>
      void addGettableVar(const char *systemName,
                          int length,
                          T *value,
                          const char *units,
                          const char *desc)
          {
          DataTypeCode type = dataTypeCodeOf(* value);
          unsigned int id = getReg(systemName, type, 1, units);
          varInfo *v = new varInfo(systemName, type, length, value, units, desc);
          getVarMap.insert(UInt2InfoMap::value_type(id,v));
          };

      // vector
      template <class T>
      void addGettableVar(const char *systemName,
                          std::vector<T> &value,
                          const char *units,
                          const char *desc)
           {
           throw "vector addGettableVar not yet implemented";
           };

      // Add a procedure to be called when events occur
      unsigned int addEvent(const char *systemName,
                            RegistrationType type,
                            boost::function3<void, unsigned &, unsigned &, protocol::Variant &> ptr);

   }; // end class Component
} // end namespace protocol

protocol::Component* createComponent(void);

// restore the warnings about "Functions containing for are not expanded inline.
#pragma warn .inl

#endif

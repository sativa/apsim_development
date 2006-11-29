#ifndef CMPComponentInterfaceH
#define CMPComponentInterfaceH
#include <general/platform.h>
#include <string>
#include <map>
#include <ComponentInterface2/Interfaces.h>
#include <ComponentInterface2/ScienceAPI.h>
class Message;
class XMLDocument;
class ScienceAPI;
class CMPMethod0;

typedef EXPORT STDCALL void (CallbackType)(const unsigned int *compInst,
                                           Message& message);



class CMPComponentInterface
   {
   public:
      CMPComponentInterface(unsigned* callbackarg, CallbackType* callback, unsigned componentID, unsigned parentID);
      ~CMPComponentInterface();

      bool get(const std::string& name, const std::string& units, bool optional, IPackableData* data);
      void set(const std::string& name, const std::string& units, IPackableData* data);
      bool read(const std::string& name, IPackableData& value, bool optional);

      void publish(const std::string& name, IPackableData* data);
      void subscribe(const std::string& eventName, IPackableData* handler);
      void query(const std::string& pattern, std::vector<QueryMatch>& matches);

      // Export a variable. The variable passed in is stored directly
      // in our map so the assumption is that we are now owners.
      // ie. don't delete this data object elsewhere!
      void expose(const std::string& name,
                  const std::string& units,
                  const std::string& description,
                  bool writable,
                  IPackableData* variable);

      void write(const std::string& msg);

      std::string getName() {return name;}
      std::string getParentName() {return parentName;}

      // internal stuff.
      void messageToLogic(const Message& message);

   private:
      ScienceAPI* scienceAPI;
      unsigned* callbackArg;
      unsigned componentID;
      unsigned parentID;
      std::string name;
      std::string parentName;
      CallbackType* messageCallback;
      bool errorHasOccurred;
      XMLDocument* simScript;
      std::vector<std::string> simSectionsToSearch;
      CMPMethod0* init1;
      CMPMethod0* init2;

      typedef std::map<std::string, IPackableData*> NameToRegMap;
		typedef std::vector<Message*> Messages;
      NameToRegMap regNames;
      Messages messages;

      enum RegistrationKind {getReg=1,         respondToGetReg=2,
                             setReg=9,         respondToSetReg=3,
                             eventReg=5,       respondToEventReg=6,
                                               respondToGetSetReg=4};

      void clearMessages();
      int RegisterWithPM(const std::string& Name, const std::string& Units,
                         const std::string& Description,
                         RegistrationKind regKind,
                         IPackableData* Data);

      int nameToRegistrationID(const std::string& name,
                               RegistrationKind regKind);

      void sendMessage(Message& message);
      void error(const std::string& errorMessage, bool isFatal);

      void onInit1(const Message& message);
      void onInit2(const Message& message);
      void onQueryValue(const Message& message);
      void onQuerySetValue(const Message& message);
      void onEvent(const Message& message);
   };

#endif

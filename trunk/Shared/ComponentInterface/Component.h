//---------------------------------------------------------------------------
#ifndef ComponentH
#define ComponentH
#include "Messages.h"
#include "Vector.h"
#include <values.h>
class ApsimComponentData;
namespace protocol {

// forward declarations of our friends.
class RegistrationItem;
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
// Manages a single instance of an APSIM Component
// ------------------------------------------------------------------
class Component
   {
   public:
      Component(void);
      virtual ~Component(void);

      // Add a registration.
      unsigned addRegistration(RegistrationType kind,
                               const FString& name,
                               const Type& type,
                               const FString& alias = "",
                               const FString& componentNameOrID = "");
      void deleteRegistration(RegistrationType kind,
                              unsigned int regID);

      // Read a variable from a parameter file
      bool readParameter
         (const FString& sectionName,
          const FString& variableName,
          FString& variableValue,
          bool optional);

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
      // Call a method in another component.
      template <class T>
      void methodCall(unsigned int methodID, T& data)
         {
         sendMessage(newPublishEventMessage(componentID, parentID, methodID,
                                            getRegistrationType(methodID),
                                            data));
         }

      // Write a line to the summary file.
      void writeString(const FString& st);

      // Send a variable to another component.
      template <class T>
      void sendVariable(QueryValueData& queryValueData,
                        const T& value)
         {
         sendMessage(newReturnValueMessage(componentID,
                                           queryValueData.replytoID,
                                           queryValueData.replyID,
                                           getRegistrationType(queryValueData.ID),
                                           value));
         }

      // convert to and from a compname to an ID.
      bool componentNameToID(const FString& name, unsigned int& compID);
      bool componentIDToName(unsigned int compID, FString& name);

   protected:
      unsigned int componentID;
      unsigned int parentID;
      ApsimComponentData* componentData;
      char* name;
      bool beforeInit2;

      // override these methods if necessary.
      virtual void doInit1(const FString& sdml);
      virtual void doInit2(void) { }
      virtual void doCommence(void) { }
      virtual void respondToEvent(unsigned int& fromID, unsigned int& eventID, Variant& variant) { }
      virtual void respondToMethod(unsigned int& fromID, unsigned int& methodID, Variant& variant) { }
      virtual void respondToGet(unsigned int& fromID, QueryValueData& queryData) { }
      virtual bool respondToSet(unsigned int& fromID, QuerySetValueData& setValueData) {return false;}
      virtual void notifyTermination(void) { }

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

      // Send a message
      void sendMessage(Message* message)
         {
         bool doAck = message->toAcknowledge;
         if (doAck)
            {
            completeIDs.push_back(message->messageID);
            completeFound = false;
            }
         (*messageCallback)(callbackArg, message);
         if (doAck)
            waitForComplete();
         deleteMessage(message);
         }
      Type& getRegistrationType(unsigned int regID);
      FString getRegistrationName(unsigned int regID);
      unsigned getRegistrationID(const RegistrationType& type, const FString& eventName);
      bool getSetVariableSuccess(void)
         {return setVariableSuccess;}

      void setVariableError(unsigned int regID);
      virtual void messageToLogic(Message* message);

   private:
      vector<RegistrationItem*> registrations;
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

      const unsigned int* callbackArg;
      CallbackType* messageCallback;

      void setup(const unsigned int componentid,
                 const unsigned int parentid,
                 const unsigned int* callbackarg,
                 void* messagecallback);
      void storeName(const FString& fqn, const FString& sdml);

      void clearReturnInfos(void);
      void readAllRegistrations(void);
      void readRegistrations(RegistrationType kind, const char* kindString);
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
   };

   } // end namespace protocol

protocol::Component* createComponent(void);

#endif

//---------------------------------------------------------------------------
#include <windows.h>
#pragma hdrstop

#include "Component.h"
#include "RegistrationItem.h"
#include "Registrations.h"
#include <ApsimShared\FApsimComponentData.h>
#include <limits.h>
#define FARPROC void*
using namespace protocol;

#define min(a, b)  (((a) < (b)) ? (a) : (b))

static const unsigned int MAX_NESTED_COMPLETES = 10;
static const char* ERROR_TYPE = "<type name=\"error\">"
                                   "<field name=\"isFatal\" kind=\"boolean\"/>"
                                   "<field name=\"message\" kind=\"string\"/>"
                                "</type>";

static const char* SUMMARY_FILE_WRITE_TYPE = "<type name=\"SummaryFileWrite\">"
                                             "   <field name=\"componentName\" kind=\"string\"/>"
                                             "   <field name=\"lines\" kind=\"string\"/>"
                                             "</type>";
enum ProtocolRegistrationType {drivingProperty=1, readableProperty=2,
                       writableProperty=3, readableWritableProperty=4,
                       publishedEvent=5, subscribedEvent=6,
                       methodHandler=7};

Component* component;
// ------------------------------------------------------------------
//  Short description:
//     destructor

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
Component::Component(void)
   : completeIDs(MAX_NESTED_COMPLETES),registrations(new Registrations(this))
   {
   componentData = NULL;
   name = NULL;
   beforeInit2 = true;
   initMessages();
   component = this;
   }

// ------------------------------------------------------------------
//  Short description:
//     destructor

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
Component::~Component(void)
   {
   deleteMessages();

   // free the somcomponent
   if (componentData != NULL)
      deleteApsimComponentData(componentData);

   delete [] name;
   clearReturnInfos();
   }

// -----------------------------------------------------------------
//  Short description:
//     clear all return infos

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
void Component::clearReturnInfos(void)
   {
   for (unsigned int i = 0; i < returnInfos.size(); i++)
      delete returnInfos[i];
   returnInfos.empty();
   }
// -----------------------------------------------------------------
//  Short description:
//     setup the Component

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
void Component::setup(const unsigned int componentid,
                      const unsigned int parentid,
                      const unsigned int* callbackarg,
                      void* messagecallback)
   {
   parentID = parentid;
   componentID = componentid;
   callbackArg = callbackarg;
   (void*)messageCallback = messagecallback;
   name = NULL;
   }

// ------------------------------------------------------------------
//  Short description:
//     destructor

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
void Component::messageToLogic(Message* message)
   {
   MessageData messageData(message);
//   char st[100];
//   itoa(message->messageType, st, 10);
//   OutputDebugString(st);
   switch (message->messageType)
      {
      case Init1:               {Init1Data init1Data;
                                 messageData >> init1Data;
                                 storeName(init1Data.fqn, init1Data.sdml);
                                 doInit1(init1Data.sdml);
                                 readAllRegistrations();
                                 break;}
      case Init2:               {beforeInit2 = false;
                                 doInit2();
                                 break;}
      case Commence:            {doCommence();
                                 break;}
      case Event:               {EventData eventData;
                                 messageData >> eventData;
                                 if (((RegistrationItem*) eventData.ID)->getKind() == respondToMethodCallReg)
                                    respondToMethod(eventData.publishedByID, eventData.ID, eventData.params);
                                 else
                                    respondToEvent(eventData.publishedByID, eventData.ID, eventData.params);
                                 break;}
      case QueryValue:          {QueryValueData queryData;
                                 messageData >> queryData;
                                 onQueryValueMessage(message->from, queryData);
                                 break;}
      case RequestSetValue:     {RequestSetValueData setValueData;
                                 messageData >> setValueData;
                                 onRequestSetValueMessage(message->from, setValueData);
                                 break;}
      case QuerySetValue:       {QuerySetValueData querySetData;
                                 messageData >> querySetData;
                                 bool ok = respondToSet(message->from, querySetData);
                                 sendMessage(newNotifySetValueSuccessMessage
                                                (componentID,
                                                 querySetData.replyToID,
                                                 querySetData.replyID,
                                                 ok));
                                 break;}
      case NotifySetValueSuccess:{NotifySetValueSuccessData notifySetValueSuccess;
                                 messageData >> notifySetValueSuccess;
                                 setVariableSuccess = notifySetValueSuccess.success;
                                 break;}
      case QueryInfo:           {QueryInfoData queryInfo;
                                 messageData >> queryInfo;
                                 onQueryInfoMessage(message->from, message->messageID, queryInfo);
                                 break;}
      case ReturnValue:         {ReturnValueData returnData;
                                 messageData >> returnData;
                                 ((RegistrationItem*) returnData.ID)->addReturnValueMessage(message->from,
                                                                                     returnData);
                                 break;}
      case NotifyTermination:   {notifyTermination();
                                 break;}
      case PublishEvent:        {PublishEventData publishEventData;
                                 messageData >> publishEventData;
                                 onPublishEventMessage(message->from, publishEventData);
                                 break;}
      case ReturnInfo:          {ReturnInfoData* returnInfo = new ReturnInfoData;
                                 messageData >> *returnInfo;
                                 returnInfos.push_back(returnInfo);
                                 break;}
      case RequestComponentID:  {RequestComponentIDData requestComponentIDData;
                                 messageData >> requestComponentIDData;
                                 onRequestComponentIDMessage(message->from, requestComponentIDData);
                                 break;}
      case ReturnComponentID:   {ReturnComponentIDData returnComponentIDData;
                                 messageData >> returnComponentIDData;
                                 onReturnComponentIDMessage(returnComponentIDData);
                                 break;}
      case Register:            {RegisterData registerData;
                                 messageData >> registerData;
                                 onRegisterMessage(message->from, registerData);
                                 break;}
      case TerminateSimulation: {onTerminateSimulationMessage();
                                 break;}
      case GetValue:            {GetValueData getValueData;
                                 messageData >> getValueData;
                                 onGetValueMessage(message->from, getValueData);
                                 break;}
      case Deregister:          {DeregisterData deregisterData;
                                 messageData >> deregisterData;
                                 onDeregisterMessage(message->from, deregisterData);
                                 break;}
      case Complete:            {CompleteData completeData;
                                 messageData >> completeData;
                                 onCompleteMessage(completeData);
                                 break;}
      case ApsimGetQuery:       {ApsimGetQueryData apsimGetQueryData;
                                 messageData >> apsimGetQueryData;
                                 onApsimGetQuery(apsimGetQueryData);
                                 break;}
      case ApsimSetQuery:       {ApsimSetQueryData apsimSetQueryData;
                                 messageData >> apsimSetQueryData;
                                 bool ok = onApsimSetQuery(apsimSetQueryData);
                                 if (ok)
                                    {
                                    sendMessage(newNotifySetValueSuccessMessage
                                                   (componentID,
                                                    apsimSetQueryData.replyToID,
                                                    apsimSetQueryData.replyID,
                                                    ok));
                                    addRegistration(protocol::respondToSetReg,
                                                    apsimSetQueryData.name, " ");
                                    }
                                 break;}
      case ApsimChangeOrder:    {onApsimChangeOrderData(messageData);
                                 break;}
      }

   // if acknowledgement is required, then give it.
   if (message->toAcknowledge)
      sendMessage(newCompleteMessage(componentID,
                                     message->from,
                                     message->messageID));
   }

// ------------------------------------------------------------------
//  Short description:
//     Do the standard registrations.

//  Changes:
//    DPH 7/6/2001
// ------------------------------------------------------------------
void Component::doInit1(const FString& sdml)
   {
   static const char* STRING_TYPE = "<type kind=\"string\"/>";
   static const char* INTEGER_TYPE = "<type kind=\"integer4\"/>";
   nameID = addRegistration(respondToGetReg,
                            "name",
                            STRING_TYPE);
   typeID = addRegistration(respondToGetReg,
                            "type",
                            STRING_TYPE);
   versionID = addRegistration(respondToGetReg,
                               "version",
                               STRING_TYPE);
   authorID = addRegistration(respondToGetReg,
                              "author",
                              STRING_TYPE);
   activeID = addRegistration(respondToGetReg,
                              "active",
                              INTEGER_TYPE);
   stateID = addRegistration(respondToGetReg,
                             "state",
                             STRING_TYPE);
   errorID = addRegistration(eventReg,
                             "error",
                             ERROR_TYPE);
   summaryID = addRegistration(eventReg,
                               "summaryFileWrite",
                               SUMMARY_FILE_WRITE_TYPE);
   }

// ------------------------------------------------------------------
//  Short description:
//     Do INIT1

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
void Component::storeName(const FString& fqn, const FString& sdml)
   {
   // get instance name by locating the last period and assuming the
   // name of this component follows the period.
   unsigned posLastPeriod = FString::npos;
   unsigned posPeriod = fqn.find(".");
   while (posPeriod != FString::npos)
      {
      posLastPeriod = posPeriod;
      posPeriod = fqn.find(".", posLastPeriod+1);
      }
   FString componentName = fqn;
   if (posLastPeriod != FString::npos)
      componentName = fqn.substr(posLastPeriod+1);

   // now create memory block for this name and fill it.
   name = new char[componentName.length() + 1];

   strncpy(name, componentName.f_str(), componentName.length());
   name[componentName.length()] = 0;

   componentData = newApsimComponentData(sdml.f_str(), sdml.length());
   }
// ------------------------------------------------------------------
// add a registration
// ------------------------------------------------------------------
unsigned Component::addRegistration(RegistrationType kind,
                                    const FString& name,
                                    const Type& type,
                                    const FString& alias,
                                    const FString& componentNameOrID)
   {
   RegistrationItem* reg = registrations->find(kind, name, componentNameOrID);
   if (reg == NULL)
      {
      reg = registrations->add(kind, name, type, componentNameOrID);

      unsigned id = (unsigned) reg;
      int destID = reg->getComponentID();
      if (destID == -1) destID = 0;
      char fqn[100];
      strcpy(fqn, "");
      reg->getFQN(fqn);

      sendMessage(newRegisterMessage(componentID,
                                     parentID,
                                     kind,
                                     id,
                                     destID,
                                     fqn,
                                     reg->getType()));
      }

   return (unsigned) reg;
   }
// ------------------------------------------------------------------
// delete the specified registration.
// ------------------------------------------------------------------
void Component::deleteRegistration(RegistrationType kind,
                                   unsigned int regID)
   {
   sendMessage(newDeregisterMessage(componentID,
                                    parentID,
                                    kind,
                                    regID));
   }
// ------------------------------------------------------------------
//  Short description:
//     send a message to infrastructure.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
bool Component::readParameter
   (const FString& sectionName, const FString& variableName,
    FString& variableValue, bool optional)
   {
   if (ApsimComponentData_getProperty(componentData,
                                      sectionName,
                                      variableName,
                                      variableValue))
      return true;
   if (!optional)
      {
      char msg[200];
      strcpy(msg, "Cannot find a parameter in any of the files/sections\n"
                  "specified in the control file.\n"
                  "Parameter name = ");
      strncat(msg, variableName.f_str(), variableName.length());
      strcat(msg, "\n");
      strcat(msg, "Section name = ");
      strncat(msg, sectionName.f_str(), sectionName.length());
      error(msg, strlen(msg));
      }
   return false;
   }

// ------------------------------------------------------------------
//  Short description:
//     send an error to the system

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
void Component::error(const FString& msg, bool isFatal)
   {
   char cMessage[1000];

   if (beforeInit2)
      {
      strncpy(cMessage, msg.f_str(), min((int)msg.length(), 999));
      ::MessageBox(NULL, cMessage, "Init1 error", MB_ICONSTOP | MB_OK);
      }
   else
      {
      strcpy(cMessage, "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n");
      if (isFatal)
         strcat(cMessage, "                 APSIM  Fatal  Error               \n");
      else
         strcat(cMessage, "                 APSIM Warning Error               \n");
      strcat(cMessage, "                 -------------------              \n");

      strncat(cMessage, msg.f_str(), min((int)msg.length(), 999));
      strcat(cMessage, "\nComponent name: ");
      strcat(cMessage, name);
      strcat(cMessage, "\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\n");

      // create and send a message.
      Message* errorMessage = newPublishEventMessage(componentID,
                                                     parentID,
                                                     errorID,
                                                     Type(ERROR_TYPE),
                                                     ErrorData(isFatal, cMessage));
      errorMessage->toAcknowledge = true;
      sendMessage(errorMessage);
      if (isFatal)
         terminateSimulation();
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    create and send a message.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
void Component::terminateSimulation(void)
   {
   sendMessage(newTerminateSimulationMessage(componentID, parentID));
   }
// ------------------------------------------------------------------
//  Short description:
//     get the values of a variable from the system and return to caller.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
bool Component::getVariables(unsigned int registrationID, Variants*& values)
   {
   RegistrationItem* regItem = (RegistrationItem*) registrationID;
   if (beforeInit2)
      {
      char st[500];
      strcpy(st, "Cannot do GET's before the INIT2.\n");
      strcat(st, "Variable name: ");
      strncat(st, regItem->getName().f_str(),
                  regItem->getName().length());
      error(st, true);
      return false;
      }
   else
      {

      // clean up old values if necessary.
      regItem->empty();

      // send a GetValue message
      sendMessage(newGetValueMessage(componentID, parentID, registrationID));
      values = &regItem->getVariants();
      return (values->size() > 0);
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     get a variable from the system and return to caller.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
bool Component::getVariable(unsigned int registrationID,
                            Variant*& value,
                            bool optional)
   {
   Variants* variants = NULL;
   getVariables(registrationID, variants);
   if (variants->size() > 1 ||
       (!optional && variants->size() == 0))
      {
      RegistrationItem* regItem = (RegistrationItem*) registrationID;
      char st[500];
      strcpy(st, "Expected a SINGLE response to a getVariable request, but \n");
      strcat(st, "got ");
      itoa(variants->size(), &st[strlen(st)], 10);
      strcat(st, " responses instead.\n");
      strcat(st, "Variable name: ");
      strncat(st, regItem->getName().f_str(),
                  regItem->getName().length());
      error(st, true);
      return false;
      }
   else if (variants->size() == 0)
      return false;
   else
      {
      value = variants->getVariant(0);
      return true;
      }
   }
// ------------------------------------------------------------------
//  Short description:
//     respond to the queryvalue message.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
void Component::onQueryValueMessage(unsigned int fromID,
                                    QueryValueData& queryData)
   {
   if (queryData.ID == nameID)
      sendVariable(queryData, FString(name));
   else if (queryData.ID == typeID)
      sendVariable(queryData, FString("apsru"));
   else if (queryData.ID == versionID)
      sendVariable(queryData, FString("1.0"));
   else if (queryData.ID == authorID)
      sendVariable(queryData, FString("apsru"));
   else if (queryData.ID == activeID)
      sendVariable(queryData, 0);
   else if (queryData.ID == stateID)
      sendVariable(queryData, FString(""));
   else
      respondToGet(fromID, queryData);
   }

// ------------------------------------------------------------------
// component name to ID
// ------------------------------------------------------------------
bool Component::componentNameToID(const FString& name, unsigned int& compID)
   {
   clearReturnInfos();

   sendMessage(newQueryInfoMessage(componentID, parentID, name, componentInfo));
   if (returnInfos.size() == 1)
      {
      ReturnInfoData* returnInfo = returnInfos[0];
      compID = returnInfo->componentID;
      return true;
      }
   else
      return false;
   }
// ------------------------------------------------------------------
// component ID to name
// ------------------------------------------------------------------
bool Component::componentIDToName(unsigned int compID, FString& name)
   {
   clearReturnInfos();

   char idString[100];
   itoa(compID, idString, 10);

   sendMessage(newQueryInfoMessage(componentID, parentID, idString, componentInfo));
   if (returnInfos.size() == 1)
      {
      ReturnInfoData* returnInfo = returnInfos[0];
      unsigned posPeriod = returnInfo->name.find(".");
      if (posPeriod != FString::npos)
         name = returnInfo->name.substr(posPeriod+1,
                                        returnInfo->name.length()-posPeriod-1);
      else
         name = returnInfo->name;
      return true;
      }
   else
      return false;
   }

// ------------------------------------------------------------------
//  Short description:
//     write a string to the summary file.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
namespace protocol {
   struct SummaryData
      {
      SummaryData(const FString& modName, const FString& st)
         : componentName(modName), lines(st)
         { }
      FString componentName;
      FString lines;
      };
   MessageData& operator<< (MessageData& messageData, const SummaryData& summaryData)
      {
      messageData << summaryData.componentName;
      messageData << summaryData.lines;
      return messageData;
      }
   unsigned int memorySize(const SummaryData& summaryData)
      {
      return memorySize(summaryData.componentName) + memorySize(summaryData.lines);
      }
   };
void Component::writeString(const FString& st)
   {
   protocol::SummaryData summaryData(name, st);

   sendMessage(newPublishEventMessage(componentID,
                                      parentID,
                                      summaryID,
                                      SUMMARY_FILE_WRITE_TYPE,
                                      summaryData));
   }

// ------------------------------------------------------------------
//  Short description:
//     return a reference to a registration type to caller.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
Type Component::getRegistrationType(unsigned int regID)
   {
   return ((RegistrationItem*) regID)->getType();
   }
// ------------------------------------------------------------------
// Set the registration type of the specified registration.
// ------------------------------------------------------------------
void Component::setRegistrationType(unsigned int regID, const Type& type)
   {
   ((RegistrationItem*) regID)->setType(type);
   }
// ------------------------------------------------------------------
// Return a registration id to caller.  Returns 0 if not found
// ------------------------------------------------------------------
unsigned Component::getRegistrationID(const RegistrationType& kind, const FString& name)
   {
   return (unsigned) registrations->find(kind, name);
   }
// ------------------------------------------------------------------
//  Short description:
//     return a reference to a registration type to caller.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
FString Component::getRegistrationName(unsigned int regID)
   {
   return ((RegistrationItem*) regID)->getName();
   }

// ------------------------------------------------------------------
//  Short description:
//     read all registrations for this Component.

//  Notes:

//  Changes:
//    dph 6/3/2001

// ------------------------------------------------------------------
void Component::readAllRegistrations(void)
   {
   readRegistrations(getVariableReg, "getVariableReg");
   readRegistrations(respondToGetReg, "respondToGetReg");
   readRegistrations(setVariableReg, "setVariableReg");
   readRegistrations(respondToSetReg, "respondToSetReg");
   readRegistrations(respondToGetSetReg, "respondToGetSetReg");
   readRegistrations(eventReg, "eventReg");
   readRegistrations(respondToEventReg, "respondToEventReg");
   readRegistrations(methodCallReg, "methodCallReg");
   readRegistrations(respondToMethodCallReg, "respondToMethodCallReg");
   }

// ------------------------------------------------------------------
//  Short description:
//     read registrations for this Component that match the specified
//     type.

//  Notes:

//  Changes:
//    dph 6/3/2001

// ------------------------------------------------------------------
void Component::readRegistrations(RegistrationType kind, const char* kindString)
   {
/*   static const unsigned REGISTRATION_NAME_SIZE = 50;
   static const unsigned MAX_ALIAS_SIZE = 50;
   static const unsigned MAX_DATA_TYPE_SIZE = 100;
   char registrationNames[MAX_NUM_REGISTRATIONS][REGISTRATION_NAME_SIZE];
   unsigned numRegistrationNames;
   somcomponent_getregistrationnames(&componentData,
                                     (char*) registrationNames,
                                     kindString,
                                     &MAX_NUM_REGISTRATIONS,
                                     &numRegistrationNames,
                                     REGISTRATION_NAME_SIZE,
                                     strlen(kindString));
   for (unsigned regI = 0; regI < numRegistrationNames; regI++)
      {
      unsigned registration = component_getregistration(&componentData,
                                                        registrationNames[regI],
                                                        kindString,
                                                        REGISTRATION_NAME_SIZE,
                                                        strlen(kindString));

      char alias[MAX_ALIAS_SIZE];
      registration_getalias(&registration, alias, MAX_ALIAS_SIZE);

      char dataType[MAX_DATA_TYPE_SIZE];
      registration_getdatatype(&registration, dataType, MAX_DATA_TYPE_SIZE);

      addRegistration(kind,
                      FString(registrationNames[regI], REGISTRATION_NAME_SIZE),
                      FString(dataType, MAX_DATA_TYPE_SIZE),
                      FString(alias, MAX_ALIAS_SIZE));

      component_freeregistration(&registration);
      }
*/   }

// ------------------------------------------------------------------
//  Short description:
//     display a setvariable error.

//  Changes:
//    dph 6/3/2001
// ------------------------------------------------------------------
void Component::setVariableError(unsigned int regID)
   {
   RegistrationItem* regItem = (RegistrationItem*) regID;
   char buffer[200];
   strcpy(buffer, "Cannot set value of the specified variable.\nVariable name: ");
   strncat(buffer, regItem->getName().f_str(), regItem->getName().length());
   error(buffer, false);
   }

// ------------------------------------------------------------------
//  Short description:
//     perform some action on the complete message.

//  Changes:
//    dph 6/3/2001
// ------------------------------------------------------------------
void Component::onCompleteMessage(CompleteData& completeData)
   {
   if (completeIDs.size() > 0
       && completeIDs[completeIDs.size()-1] == completeData.ack_ID)
      {
      completeFound = true;
      completeIDs.erase(completeIDs.size()-1);
      }
   else
      {
      char buffer[200];
      strcpy(buffer, "Invalid complete message.  Was waiting for a complete\n");
      strcat(buffer, "message with ID=");
      itoa(completeIDs[completeIDs.size()-1], &buffer[strlen(buffer)], 10);
      error(buffer, true);
      completeFound = false;
      }
   }
// ------------------------------------------------------------------
//  Short description:
//     perform some action on the complete message.

//  Changes:
//    dph 6/3/2001
// ------------------------------------------------------------------
void Component::waitForComplete(void)
   {
   int maxNumIterations = 50;
   for (int iter = 0; iter != maxNumIterations && !completeFound; iter++);
   if (!completeFound)
      {
      char buffer[200];
      strcpy(buffer, "Timeout while waiting for a COMPLETE message");
      error(buffer, true);
      } 
   }
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
//  Short description:
//     The PM is instructing us to create an instance of all our data.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
namespace protocol {
extern "C" _export void __stdcall createInstance
   (const char* dllFileName,
    const unsigned int* compID,
    const unsigned int* parentID,
    unsigned int* instanceNumber,
    const unsigned int* callbackArg,
    CallbackType* callback)
   {
   protocol::Component* component = ::createComponent();
   component->setup(*compID, *parentID, callbackArg, (FARPROC)callback);
   *instanceNumber = (unsigned) component;
   }
}
// ------------------------------------------------------------------
//  Short description:
//     The PM is instructing us to delete an instance of our data.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" _export void __stdcall deleteInstance (unsigned* instanceNumber)
   {
   delete (protocol::Component*) *instanceNumber;
   }

// ------------------------------------------------------------------
//  Short description:
//     All messages to component go through here.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
namespace protocol {
extern "C" _export void __stdcall messageToLogic (unsigned* instanceNumber,
                                                  Message* message,
                                                  bool* processed)
   {
   ((protocol::Component*) *instanceNumber)->messageToLogic(message);
   *processed = true; // ???? not sure why we need this.
   }
}
// ------------------------------------------------------------------
//  Short description:
//     Return a blank string when requested to indicate that we
//     don't need a wrapper DLL.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" _export void __stdcall wrapperDLL(char* wrapperDll)
   {
   strcpy(wrapperDll, "");
   }

void fatalError(const FString& st)
   {
   component->error(st, true);
   }

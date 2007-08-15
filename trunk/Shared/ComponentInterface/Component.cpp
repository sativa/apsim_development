//---------------------------------------------------------------------------
#pragma hdrstop
#include <stdexcept>
#include <limits.h>

#include <string>
#include <sstream>
#include <fstream>
#include <iostream>
#include <iomanip>
#include <boost/lexical_cast.hpp>

#include <ApsimShared/FStringExt.h>
#include <general/path.h>
#include <general/date_class.h>
#include "ScienceAPI.h"

#include "ProtocolVector.h"
#include "Component.h"

using namespace protocol;

#define min(a, b)  (((a) < (b)) ? (a) : (b))

static const unsigned int MAX_NESTED_COMPLETES = 10;
static const char* ERROR_TYPE = "<type name=\"error\">"
                                   "<field name=\"fatal\" kind=\"boolean\"/>"
                                   "<field name=\"message\" kind=\"string\"/>"
                                   "</type>";

static const char* SUMMARY_FILE_WRITE_TYPE = "<type name=\"SummaryFileWrite\">"
                                             "   <field name=\"componentName\" kind=\"string\"/>"
                                             "   <field name=\"lines\" kind=\"string\"/>"
                                             "</type>";

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
   beforeInit2 = true;
   beforeCommence = true;
   tick.startday = 0;
   initMessages();
   component = this;
   haveWrittenToStdOutToday = false;
   sendTickToComponent = false;
   api = new ScienceAPI(this);
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

   clearReturnInfos();
   delete registrations;
   delete api;
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
void Component::setup(const char *dllname,
                      const unsigned int componentid,
                      const unsigned int parentid,
                      const unsigned int* callbackarg,
                      CallbackType messagecallback)
   {
   dllName = dllname;
   parentID = parentid;
   componentID = componentid;
   callbackArg = callbackarg;
   messageCallback = (void STDCALL (*)(const unsigned int*, protocol::Message*))messagecallback;
   }

// ------------------------------------------------------------------
//  Short description:
//     destructor

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
void Component::messageToLogic(const Message* message)
   {
   // We need to keep track of bits of the message because the FARMWI$E infrastructure
   // doesn't guarantee that a message is still valid at the end of this method.
   // eg. it deletes the Init1 message before we get to test the ack flag at the bottom.
   bool ack = message->toAcknowledge;
   unsigned fromID = message->from;
   unsigned msgID = message->messageID;
   currentMsgID = msgID;

try {
   MessageData messageData(message);
   switch (message->messageType)
      {
      case Init1:               {Init1Data init1Data;
                                 messageData >> init1Data;
                                 doInit1(init1Data);
                                 break;}
      case Init2:               {beforeInit2 = false;
                                 doInit2();
                                 beforeCommence = false;
                                 break;}
      case Commence:            {doCommence();
                                 break;}
      case Event:               {EventData eventData;
                                 messageData >> eventData;
                                 if (eventData.ID == tickID)
                                    {
                                    eventData.params.unpack(tick);
                                    eventData.params.getMessageData().reset();
                                    haveWrittenToStdOutToday = false;
                                    if (sendTickToComponent)
                                       respondToEvent(eventData.publishedByID, eventData.ID, eventData.params);
                                    }
                                 else
                                    respondToEvent(eventData.publishedByID, eventData.ID, eventData.params);
                                 break;}
      case QueryValue:          {QueryValueData queryData(fromID);
                                 messageData >> queryData;
                                 onQueryValueMessage(message->from, queryData);
                                 break;}
      case RequestSetValue:     {RequestSetValueData setValueData;
                                 messageData >> setValueData;
                                 onRequestSetValueMessage(message->from, setValueData);
                                 break;}
      case QuerySetValue:       {QuerySetValueData querySetData;
                                 messageData >> querySetData;
                                 onQuerySetValueMessage(message->from, querySetData);
                                 break;}
      case ReplySetValueSuccess:
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
      case ReplyValue:          {ReplyValueData replyData;
                                 messageData >> replyData;
                                 onReplyValueMessage(message->from, replyData);
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
                                    addRegistration(RegistrationType::respondToSet,
                                                    apsimSetQueryData.name, " ");
                                    }
                                 break;}
      case ApsimChangeOrder:    {onApsimChangeOrderData(messageData);
                                 break;}
      }

   // if acknowledgement is required, then give it.
   if (ack)
      sendMessage(newCompleteMessage(componentID, fromID, msgID));
   }
catch (const std::exception &e)
   {
   this->error(e.what(), true);
   }
catch (const std::string& e)
   {
   this->error(e.c_str(), true);
   }

}

// ------------------------------------------------------------------
//  Short description:
//     Do the standard registrations.

//  Changes:
//    DPH 7/6/2001
// ------------------------------------------------------------------
void Component::doInit1(const Init1Data& init1Data)
   {
   // get instance name from fqn.
   string fqn =  asString(init1Data.fqn);
   unsigned posPeriod = fqn.rfind('.');
   if (posPeriod == string::npos)
      {
      name = fqn;
      pathName = ".";
      }
   else
      {
      name = fqn.substr(posPeriod+1);
      pathName = fqn.substr(0, posPeriod);
      }

   type = "?";
   version = "1";
   author = "APSRU";
   active = 1;
   state = "";
   
   componentData = newApsimComponentData(init1Data.sdml.f_str(), 
                                         init1Data.sdml.length());
      
   addGettableVar("name", name, "", "");
   addGettableVar("type", type, "", "");
   addGettableVar("version", version, "", "");
   addGettableVar("author", author, "", "");
   addGettableVar("active", active, "", "");
   addGettableVar("state", state, "", "");

   errorID = addRegistration(RegistrationType::event,
                             "error",
                             ERROR_TYPE);
   tickID = addRegistration(RegistrationType::respondToEvent,
                            "tick",
                            DDML(TimeType()).c_str());
   sendTickToComponent = false;
   }

// ------------------------------------------------------------------
// add a registration to our internal list of registrations.
// ------------------------------------------------------------------
RegistrationItem* Component::addRegistrationToList(RegistrationType kind,
                                                   const FString& name,
                                                   const Type& type)
{
   return (addRegistrationToList(kind, name, type, FString("")));
}

RegistrationItem* Component::addRegistrationToList(RegistrationType kind,
                                                   const FString& name,
                                                   const Type& type,
                                                   const FString& componentNameOrID)
   {
   RegistrationItem* reg = registrations->find(kind, name, componentNameOrID);
   if (reg == NULL)
      reg = registrations->add(kind, name, type, componentNameOrID);
   return reg;
   }
// ------------------------------------------------------------------
// add a registration
// ------------------------------------------------------------------
unsigned Component::addRegistration(RegistrationType kind,
                                    const FString& name,
                                    const Type& type)
{
   return (addRegistration(kind, name, type, FString(""), FString("")));
}

unsigned Component::addRegistration(RegistrationType kind,
                                    const FString& name,
                                    const Type& type,
                                    const FString& alias)
{
   return (addRegistration(kind, name, type, alias, FString("")));
}
unsigned Component::addRegistration(RegistrationType kind,
                                    const FString& regName,
                                    const Type& type,
                                    const FString& alias,
                                    const FString& componentNameOrID)
   {
   RegistrationItem* reg = registrations->find(kind, regName, componentNameOrID);
   if (reg == NULL)
      {
      reg = registrations->add(kind, regName, type, componentNameOrID);

      unsigned id = (unsigned) reg;

      std::string registrationName = reg->getName();
      int destID = 0;
      if (strlen(reg->getComponentName()) > 0)
         {
         char* endPtr;
         destID = strtol(reg->getComponentName(), &endPtr, 10);
         if (*endPtr != '\0')
            registrationName = std::string(reg->getComponentName()) + "." + registrationName;
         }

      sendMessage(newRegisterMessage(componentID,
                                     parentID,
                                     kind,
                                     id,
                                     destID,
                                     registrationName.c_str(),
                                     reg->getType()));
      }
   if (Str_i_Eq(asString(regName), "tick"))
      sendTickToComponent = true;
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

void Component::respondToGet(unsigned int& /*fromID*/, QueryValueData& queryData)
   {
   if (getVarMap.find(queryData.ID) != getVarMap.end())
      {
      baseInfo *v = getVarMap[queryData.ID];
      if (v)
         v->sendVariable(this, queryData);
      }
   }

void Component::respondToEvent(unsigned int& fromID, unsigned int& eventID, Variant& variant)
  {
  boost::function3<void, unsigned &, unsigned &, protocol::Variant &> pf;
  UInt2EventMap::iterator ipf, ipf1, ipf2;

  ipf1 = eventMap.lower_bound(eventID);
  ipf2 = eventMap.upper_bound(eventID);

  for (ipf = ipf1; ipf != ipf2; ipf++)
     {
     variant.getMessageData().reset();
     pf = ipf->second;
     (pf)(fromID, eventID, variant);
     }
  }
// ------------------------------------------------------------------
//  Short description:
//     Read a parameter from initialisation data.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
bool Component::readParameter
   (const FString& sectionName, const FString& variableName,
    FString& variableValue, bool optional)
   {
   std::string value = readParameter(asString(sectionName),
                                     asString(variableName));
   variableValue = value.c_str();

   if (value == "")
      {
      if (!optional)
         {
         char msg[600];
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
   return true;
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
   string message = asString(msg) + "\nComponent name: " + name;
      
   // create and send a message.
   Message* errorMessage = newPublishEventMessage(componentID,
                                                  parentID,
                                                  errorID,
                                                  Type(ERROR_TYPE),
                                                  ErrorData(isFatal, message.c_str()));
   errorMessage->toAcknowledge = true;
   sendMessage(errorMessage);
   if (isFatal)
      terminateSimulation();
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

   // clean up old values if necessary.
   regItem->empty();

   // send a GetValue message
   sendMessage(newGetValueMessage(componentID, parentID, registrationID));
   values = &regItem->getVariants();
   return (values->size() > 0);
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
   if (variants->size() > 1)
      {
      RegistrationItem* regItem = (RegistrationItem*) registrationID;
      std::string st;
      st = "The module " + std::string(name) + " has asked for the value of the variable ";
      st += regItem->getName();
      st += ".\nIt received multiple responses when only 1 was expected.\nIt received values from the following modules:\n";
      for (unsigned v = 0; v != variants->size(); v++)
         {
         unsigned fromID = variants->getVariant(v)->getFromId();
         FString fromName;
         if (componentIDToName(fromID, fromName))
            st += "   " + asString(fromName);
         else
            st += "   unknown module!!";
         st += "\n";
         }

      error(st.c_str(), true);
      return false;
      }
   else if (!optional && variants->size() == 0)
      {
      RegistrationItem* regItem = (RegistrationItem*) registrationID;
      std::string st;
      st = "The module " + std::string(name) + " has asked for the value of the variable ";
      st += regItem->getName();
      st += ".\nIt received no responses.";
      error(st.c_str(), true);
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
      {
      compID = INT_MAX;
      return false;
      }
   }
// ------------------------------------------------------------------
// component ID to name
// ------------------------------------------------------------------
bool Component::componentIDToName(unsigned int compID, FString& name)
   {
   clearReturnInfos();

   string idString = itoa(compID);

   sendMessage(newQueryInfoMessage(componentID, parentID, idString.c_str(), componentInfo));
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
// process the querySetValueMessage.
// ------------------------------------------------------------------
void Component::onQuerySetValueMessage(unsigned fromID, QuerySetValueData& querySetData)
   {
   bool ok = respondToSet(fromID, querySetData);
   sendMessage(newReplySetValueSuccessMessage
                  (componentID,
                   fromID,
                   currentMsgID,
                   ok));
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

void Component::writeString(const FString& lines)
   {
   writeStringToStream(asString(lines), cout, name);   
   }
void Component::writeStringToStream(const std::string& lines, ostream& out,
                                    const std::string& componentName)
   {
   if (!haveWrittenToStdOutToday)
      {
      if (beforeCommence)
         {
         if (componentName != "")
            {
            out << endl;
            out << "------- " << componentName << " Initialisation ";
            out.width(79-24-componentName.length());
            out.fill('-');
            out << '-' << endl;
            out.fill(' ');
            }
         }
      else
         {
         GDate Today;
         Today.Set(tick.startday);
         Today.Set_write_format("D MMMMMM YYYY");
         Today.Write(cout);
         out << "(Day of year=" << Today.Get_day_of_year() << ")";
         if (componentName != "")
            out << ", " << componentName;
         out << ": " << endl;
         }
      haveWrittenToStdOutToday = true;
      }

   // write out the lines.
   unsigned posStart = 0;
   unsigned posEndText;
   unsigned posCR;
   do
      {
      posCR = lines.find("\n", posStart);
      posEndText = posCR;
      if (posEndText == string::npos)
         posEndText = lines.length();
      if (posEndText > 0)
         {
         posEndText = lines.find_last_not_of(" ", posEndText-1);
         if (posEndText == string::npos)
            posEndText = posStart;  // must be all spaces.
         else
            posEndText++;
         }
      out << "     " << lines.substr(posStart, posEndText-posStart) << endl;
      posStart = posCR + 1;
      }
   while (posCR != string::npos);
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
const char *Component::getRegistrationName(unsigned int regID)
   {
   return ((RegistrationItem*) regID)->getName();
   }
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
   strcat(buffer, regItem->getName());
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
      strcat(buffer, itoa(completeIDs[completeIDs.size()-1]).c_str());
//      error(buffer, true);
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
//      error(buffer, true);
      }
   }


void fatalError(const FString& st)
   {
   component->error(st, true);
   }
// ------------------------------------------------------------------
//  Short description:
//    Send a variable to the system
//  Notes:
//    Using a pointer to a memory region of scalar or array object, calls
//    Component::sendVariable() with the correct typecast w
// ------------------------------------------------------------------
void stringInfo::sendVariable(Component *systemInterface, QueryValueData& qd)
   {
    const char *s = myPtr->c_str();
    systemInterface->sendVariable(qd, FString(s));
   }

void varInfo::sendVariable(Component *systemInterface, QueryValueData& qd)
   {
   switch (myType)
      {
      case DTdouble:
         if (myLength == 1)
           systemInterface->sendVariable(qd, *(double *)myPtr);
         else if (myLength > 1)
           systemInterface->sendVariable(qd,
             protocol::vector<double>((double *)myPtr, (double *)myPtr + myLength));
         else
           throw "Length = 0 in varInfo::sendVariable";
         break;
      case DTsingle:
         if (myLength == 1)
           systemInterface->sendVariable(qd, *(float *)myPtr);
         else if (myLength > 1)
           systemInterface->sendVariable(qd,
             protocol::vector<float>((float *)myPtr, (float *)myPtr + myLength));
         else
           throw "Length = 0 in varInfo::sendVariable";
         break;
      case DTint4 :
         if (myLength == 1)
           systemInterface->sendVariable(qd, *(int *)myPtr);
         else if (myLength > 1)
           systemInterface->sendVariable(qd,
             protocol::vector<int>((int *)myPtr, (int *)myPtr + myLength));
         else
           throw "Length = 0 in varInfo::sendVariable";
         break;
      case DTstring :
         if (myLength == 1)
           systemInterface->sendVariable(qd, FString(*(char **)myPtr));
         else if (myLength > 1)
           throw  "String Array not yet implemented";
         else
           throw "Length = 0 in varInfo::sendVariable";
         break;

      default:
         throw "Aiee unknown type in varInfo::sendVariable";
      }
   }

unsigned int Component::addEvent(const char *systemName,
                                 RegistrationType type,
                                 boost::function3<void, unsigned &, unsigned &, protocol::Variant &> ptr,
                                 const char* DDML)
   {
   unsigned int id = addRegistration(type, systemName, DDML);
   eventMap.insert(UInt2EventMap::value_type(id,ptr));
   return id;
   }

// Build the xml fragment that describes this variable and publish to system
unsigned int Component::getReg(const char *systemName,
                                DataTypeCode type,
                                bool isArray,
                                const char *units)
   {
   char buffer[200];
   strcpy(buffer, "<type kind=\"");
   switch (type)
      {
   	case DTchar:   {strcat(buffer, "char"); break;}
   	case DTint4:   {strcat(buffer, "integer4"); break;}
   	case DTsingle: {strcat(buffer, "single"); break;}
   	case DTboolean:{strcat(buffer, "boolean"); break;}
   	case DTstring: {strcat(buffer, "string"); break;}
    case DTdouble: {strcat(buffer, "double"); break;}
   	default: {throw "Undefined gettable var type";}
      }
   strcat(buffer, "\" array=\"");
   if  (isArray) {
       strcat(buffer, "T");
   } else {
       strcat(buffer, "F");
   }
   strcat(buffer, "\" unit=\"(");
   strcat(buffer, units);
   strcat(buffer, ")\"/>");
   return this->addRegistration(RegistrationType::respondToGet, systemName, buffer);
   }


// Build the xml fragment that describes this variable and publish to system
std::string baseInfo::getXML()
   {
   std::string st = "   <property name=\"" + myName + "\" description=\"" + myDescription + "\" access=\"read\" init=\"F\">\n";
   st += "      <type kind=\"" + asString(Type::codeToString(myType)) + "\" array=\"";
   if (myIsArray)
      st += "T";
   else
      st += "F";
   st += "\" unit=\"" + myUnits + "\"/>\n";
   st += "   </property>";
   return st;
   }

// Build the xml fragment that describes this variable and publish to system
std::string Component::getDescription()
   {
   std::string returnString;
   try
      {
      returnString = "<describecomp name=\"" + name + "\">\n";

      returnString += string("<executable>") + dllName + "</executable>\n";
      returnString += string("<class>") + Path(dllName).Get_name_without_ext() + "</class>\n";
      returnString += "<version>1.0</version>\n";
      returnString += "<author>APSRU</author>\n";

      for (UInt2InfoMap::iterator var = getVarMap.begin();
                                  var != getVarMap.end();
                                  var++)
         {
         returnString += var->second->getXML() + "\n";
         }

      if (getVarMap.size() <= 6)
         {
         for (unsigned i = getVarMap.size(); i != registrations->size(); i++)
            {
            RegistrationItem* reg = registrations->get(i);
            if (reg->getKind() == RegistrationType::respondToGet)
               {
               returnString += "   <property name=\"";
               returnString += reg->getName();
               returnString += "\" access=\"read\" init=\"F\">\n";
               returnString += reg->getType();
               returnString += "</property>\n";
               }
            else if (reg->getKind() == RegistrationType::respondToSet)
               {
               returnString += "   <property name=\"";
               returnString += reg->getName();
               returnString += "\" access=\"write\" init=\"F\">\n";
               returnString += reg->getType();
               returnString += "</property>\n";
               }
            else if (reg->getKind() == RegistrationType::respondToGetSet)
               {
               returnString += "   <property name=\"";
               returnString += reg->getName();
               returnString += "\" access=\"both\" init=\"F\">\n";
               returnString += reg->getType();
               returnString += "</property>\n";
               }
            }
         }
      for (unsigned i = 0; i != registrations->size(); i++)
         {
         RegistrationItem* reg = registrations->get(i);
         if (reg->getKind() == RegistrationType::respondToEvent)
            {
            returnString += "   <event name=\"";
            returnString += reg->getName();
            returnString += "\" kind=\"subscribed\">";
            XMLDocument* doc = new XMLDocument(reg->getType(), XMLDocument::xmlContents);
            returnString += doc->documentElement().innerXML();
            delete doc;
            returnString += "</event>\n";
            }
         else if (reg->getKind() == RegistrationType::event)
            {
            returnString += "   <event name=\"";
            returnString += reg->getName();
            returnString += "\" kind=\"published\">";
            XMLDocument* doc = new XMLDocument(reg->getType(), XMLDocument::xmlContents);
            returnString += doc->documentElement().innerXML();
            delete doc;
            returnString += "</event>\n";
            }
         else if (reg->getKind() == RegistrationType::get)
            {
            returnString += "   <driver name=\"";
            returnString += reg->getName();
            returnString += "\">\n";
            returnString += reg->getType();
            returnString += "</driver>\n";
            }
         }
      returnString += "\n</describecomp>\n";
      std::ofstream out("d:\\tmp\\tempplant.xml");
      out << returnString;
      return returnString;
      }
   catch (const std::exception& err)
      {
	  throw runtime_error(err.what());
      }
   }
  
void Component::removeGettableVar(const char *systemName)
// remove a variable from our list of variables
   {
   //newend = getVarMap.remove_if(....??XXX
   //getVarMap.erase(newend, getVarMap.end());
   bool found = 1;
   while (found)
      {
      UInt2InfoMap::iterator i;
      for (i = getVarMap.begin();i != getVarMap.end();i++)
         if ((*i).second->name() == systemName) getVarMap.erase(i);  // and should probably deleteReg too..??
      if (i == getVarMap.end()) found = 0;
      }
   }

//---------------------------------------------------------------------------
#include <stdexcept>
#include <limits.h>

#include <string>
#include <sstream>
#include <fstream>
#include <iostream>
#include <iomanip>
#include <boost/lexical_cast.hpp>

#include <ApsimShared/FStringExt.h>
#include <ApsimShared/ApsimRegistry.h>
#include <general/path.h>
#include <general/date_class.h>
#include "ScienceAPI.h"

#include "ProtocolVector.h"
#include "Component.h"
#include "Variants.h"

using namespace protocol;

#define min(a, b)  (((a) < (b)) ? (a) : (b))

static const unsigned int MAX_NESTED_COMPLETES = 10;

Component* component;
// ------------------------------------------------------------------
//  Short description:
//     destructor

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
Component::Component(void)
   : completeIDs(MAX_NESTED_COMPLETES)
   {
   componentData = NULL;
   beforeInit2 = true;
   beforeCommence = true;
   tick.startday = 0;
   initMessages();
   component = this;
   haveWrittenToStdOutToday = false;
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

   for (getVariableResponses::iterator v = myGetVariableResponses.begin();
        v != myGetVariableResponses.end(); 
        v++)
        myGetVariableResponses.erase(v);

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
                                    eventData.params.unpack(NULL, NULL, tick);
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
                                 addReturnValueMessage(returnData);
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
                                 onApsimGetQuery(message->from, apsimGetQueryData);
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
                                    addRegistration(::respondToSet,
                                                    -1,
                                                    apsimSetQueryData.name, "");
                                    }
                                 break;}
      case ApsimChangeOrder:    {onApsimChangeOrderData(fromID, messageData);
                                 break;}
      }

   // if acknowledgement is required, then give it.
   if (ack)
      sendMessage(newCompleteMessage(componentID, fromID, msgID));
   }
catch (const std::exception &e)
   {
   this->error(string(e.what()), true);
   }
catch (const std::string& e)
   {
   this->error(e, true);
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
   ApsimRegistry &registry = ApsimRegistry::getApsimRegistry();
   registry.clearForeignTaint(componentID);

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

   tickID = addRegistration(::respondToEvent,
                            -1,
                            string("tick"),
                            DDML(TimeType()));
   sendTickToComponent = false;
   }

// ------------------------------------------------------------------
// add a registration
// ------------------------------------------------------------------
// new name: RegisterWithPM
unsigned Component::addRegistration(EventTypeCode kind,
                                    int destinationComponentID,
                                    const std::string& regName,
                                    const std::string& ddml,
                                    const std::string& alias)
   {
   ApsimRegistry &registry = ApsimRegistry::getApsimRegistry();

   ApsimRegistration *newReg = registry.createNativeRegistration(kind, regName, ddml, destinationComponentID, componentID);

   unsigned int regID = registry.add(newReg);

   sendMessage(newRegisterMessage(componentID,    // from
                                  parentID,       // to
                                  kind,           // kind
                                  regID,          // regID
                                  destinationComponentID, // destination ID 
                                  FString(regName.c_str()),
                                  FString(ddml.c_str())));

   if (Str_i_Eq(regName, "tick"))
      sendTickToComponent = true;

   return regID;
   }
// ------------------------------------------------------------------
// delete the specified registration.
// ------------------------------------------------------------------
void Component::deleteRegistration(EventTypeCode kind,
                                   unsigned int regID)
   {
   sendMessage(newDeregisterMessage(componentID,
                                    parentID,
                                    kind,
                                    regID));
   }

void Component::respondToGet(unsigned int& /*fromID*/, QueryValueData& queryData)
   {
   UInt2InfoMap::iterator i = getVarMap.find(queryData.ID);
   if (i != getVarMap.end())
      {
      baseInfo *v = i->second;
      v->sendVariable(this, queryData);
      }
   }

bool Component::respondToSet(unsigned int& /*fromID*/, QuerySetValueData& setValueData)
   {
   UInt2SetInfoMap::iterator i = setVarMap.find(setValueData.ID);
   if (i != setVarMap.end())
      {
      fnSetInfo *v = i->second;
      return v->callSetter(this, setValueData);
      }
   return false;
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
         string msg= "Cannot find a parameter in any of the files/sections\n"
                     "specified in the control file.\n"
                     "Parameter name = ";
         msg += string(variableName.f_str(), variableName.length());
         msg += "\n";
         msg += "Section name = ";
         msg += string(sectionName.f_str(), sectionName.length());
         error(msg, true);
         }
      return false;
      }
   return true;
   }

void Component::getMultipleProperties(const std::string& sectionName,
                                      const std::string& variableName,
                                      std::vector<std::string>& returnValues)
    {
   // ------------------------------------------------------------------
   // Return multiple values for the specified variable.
   // ------------------------------------------------------------------
    std::vector<string> names;
    std::vector<string> values;
    componentData->getProperties(sectionName, names, values);
    for (unsigned i = 0; i != names.size(); i++)
       {
       if (Str_i_Eq(names[i], variableName))
          returnValues.push_back(values[i]);
       }
    }

// ------------------------------------------------------------------
//  Short description:
//     send an error to the system

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
void Component::error(const char *msg, bool isFatal)
   {
   string message = string(msg) + "\nComponent name: " + name;

   // create and send a message.
   unsigned id = addRegistration(::event,
                                 -1,
                                 string("error"),
                                 DDML(ErrorType()));

   Message* errorMessage = newPublishEventMessage(componentID,
                                                  parentID,
                                                  id,
                                                  DDML(ErrorType()),
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
bool Component::getVariables(unsigned int registrationID, 
                             Variants **values)
   {
   getVariableResponses::iterator v = 
        myGetVariableResponses.find (registrationID);

   if (v == myGetVariableResponses.end())
      {
      // add a new entry for this registration
      v = myGetVariableResponses.insert(
         v,
         getVariableResponses::value_type(
                registrationID, 
                new Variants(this)));
      }

   // clean up old values if necessary.
   v->second->empty();

   // send a GetValue message. Responses will be tucked away 
   // in the map "v" created above
   sendMessage(newGetValueMessage(componentID,
                                  parentID,
                                  registrationID));

   // return whatever came back
   *values = v->second;
   return ((*values)->size() > 0);
   }

// ------------------------------------------------------------------
//  Short description:
//     get a variable from the system and return to caller.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
bool Component::getVariable(unsigned int registrationID,
                            Variant **value,
                            bool optional)
   {
   Variants *variants = NULL;
   getVariables(registrationID, &variants);
   if (variants != NULL && variants->size() > 1)
      {
      ApsimRegistry &registry = ApsimRegistry::getApsimRegistry();
      ApsimRegistration* regItem = registry.find(::get, componentID, registrationID);   
      if (regItem == NULL) {throw std::runtime_error("Invalid registration ID in Component::getVariable 1");}
      std::string st;
      st = "The module " + std::string(name) + " has asked for the value of the variable ";
      st += regItem->getName();
      st += ".\nIt received multiple responses when only 1 was expected.\nIt received values from the following modules:\n";
      for (unsigned v = 0; v != variants->size(); v++)
         {
         unsigned fromID = variants->getVariant(v)->getFromId();
         st += "   " + registry.componentByID(fromID);
         st += "\n";
         }

      error(st, true);
      return false;
      }
   else if (!optional && variants != NULL && variants->size() == 0)
      {
      ApsimRegistry &registry = ApsimRegistry::getApsimRegistry();
      ApsimRegistration* regItem = registry.find(::get, componentID, registrationID);   
      if (regItem == NULL) {throw std::runtime_error("Invalid registration ID in Component::getVariable 2");}
      std::string st;
      st = "The module " + std::string(name) + " has asked for the value of the variable ";
      if (regItem->getDestinationID() > 0) {
         st += registry.componentByID(regItem->getDestinationID());
         st += ".";
      }
      st += regItem->getName();
      st += ".\nIt received no responses.";
      error(st, true);
      return false;
      }
   else if (variants != NULL && variants->size() == 1)
      {
      (*value) = variants->getVariant(0);
      return true;
      }
   else 
      return false;
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
bool Component::componentNameToID(const std::string& name, int &compID)
   {
   ApsimRegistry &registry = ApsimRegistry::getApsimRegistry();
   string fqName = name;
   if (fqName[0] != '.') 
     fqName = pathName + string(".") + fqName;
   
   compID = registry.componentByName(fqName);
   return compID > 0;
   }
// ------------------------------------------------------------------
// component ID to name
// ------------------------------------------------------------------
bool Component::componentIDToName(int compID, std::string& name)
   {
   ApsimRegistry &registry = ApsimRegistry::getApsimRegistry();
   name = registry.componentByID(compID);
   return (name != "");
   }
// ------------------------------------------------------------------
// process the querySetValueMessage.
// ------------------------------------------------------------------
void Component::onQuerySetValueMessage(unsigned fromID, QuerySetValueData& querySetData)
   {
   bool ok = respondToSet(fromID, querySetData);
   unsigned int regID = querySetData.ID;
//cout << "Component::onQuerySetValueMessage id="<<regID<<endl;
   sendMessage(newReplySetValueSuccessMessage
                  (componentID,
                   fromID,
                   regID,
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
void Component::writeString(const std::string& st)
   {
   writeStringToStream(st, cout, name);   
   }
void Component::writeString(const char *st)
   {
   writeStringToStream(st, cout, name);   
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
EventTypeCode Component::getRegistrationType(unsigned int regID)
   {
   return (getRegistration(componentID, regID)->getTypeCode());
   }
// ------------------------------------------------------------------
//  Short description:
//     return a registration type to caller.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
ApsimRegistration* Component::getRegistration(int fromID, unsigned int regID)
   {
   ApsimRegistry &registry = ApsimRegistry::getApsimRegistry(); 
   EventTypeCode types[] = {::get, ::set, ::respondToGet, ::respondToSet,
                            ::event, ::respondToEvent, ::respondToGetSet};
   ApsimRegistration* reg;
   for (int i = 0; i < 7; i++) 
     if ((reg = registry.find(types[i], fromID, regID)) != NULL)
       return reg;
     
   string msg = "Invalid registration ID in Component::getRegistration ";
   msg += itoa(fromID);
   msg += ".";
   msg += itoa(regID);
   throw std::runtime_error(msg);
   }

void Component::getRegistrationName(int fromID, unsigned int regID, std::string &name)
   {
   name = getRegistration(fromID, regID)->getName();
   }

// ------------------------------------------------------------------
//  Short description:
//     display a setvariable error.

//  Changes:
//    dph 6/3/2001
// ------------------------------------------------------------------
void Component::setVariableError(unsigned int regID)
   {
   ApsimRegistry &registry = ApsimRegistry::getApsimRegistry();
   ApsimRegistration* regItem = registry.find(::set, componentID, regID);
   if (regItem == NULL) {throw std::runtime_error("Invalid registration ID in Component::setVariableError");}
   string buffer= "Cannot set value of the specified variable.\nVariable name: ";
   buffer += regItem->getName();
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
   component->error(asString(st), true);
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
   systemInterface->sendVariable(qd, *myPtr);
   }

void varInfo::sendVariable(Component *systemInterface, QueryValueData& qd)
   {
   switch (myType)
      {
      case DTdouble:
         if (myLength == 1)
           systemInterface->sendVariable(qd, *(double *)myPtr);
         else if (myLength > 1)
           {
           std::vector<double> v((double *)myPtr, (double *)myPtr + myLength); 
           systemInterface->sendVariable(qd,v);
           }  
         else
           throw "Length = 0 in varInfo::sendVariable";
         break;
      case DTsingle:
         if (myLength == 1)
           systemInterface->sendVariable(qd, *(float *)myPtr);
         else if (myLength > 1)
           {
           std::vector<float> v((float *)myPtr, (float *)myPtr + myLength) ;
           systemInterface->sendVariable(qd,v);
           }
         else
           throw "Length = 0 in varInfo::sendVariable";
         break;
      case DTint4 :
         if (myLength == 1)
           systemInterface->sendVariable(qd, *(int *)myPtr);
         else if (myLength > 1)
           {
           std::vector<int> v((int *)myPtr, (int *)myPtr + myLength) ;
           systemInterface->sendVariable(qd,v);
           }
         else
           throw "Length = 0 in varInfo::sendVariable";
         break;
      case DTstring :
         if (myLength == 1) 
           {
           std::string s(*(char **)myPtr);
           systemInterface->sendVariable(qd, s);
           }
         else if (myLength > 1)
           throw  "String Array not yet implemented";
         else
           throw "Length = 0 in varInfo::sendVariable";
         break;

      default:
         throw "Aiee unknown type in varInfo::sendVariable";
      }
   }

void varVectorFloatInfo::sendVariable(Component *systemInterface, QueryValueData& qd)
   {
   systemInterface->sendVariable(qd, myPtr);
   }

unsigned int Component::addEvent(const char *systemName,
                                 boost::function3<void, unsigned &, unsigned &, protocol::Variant &> ptr,
                                 const char* DDML)
   {
   unsigned int id = addRegistration(::respondToEvent, -1, string(systemName), DDML);
   eventMap.insert(UInt2EventMap::value_type(id,ptr));
   return id;
   }

// Build the xml fragment that describes this variable and publish to system
unsigned int Component::getReg(const char *systemName,
                                DataTypeCode type,
                                bool isArray,
                                const char *units)
   {
   // It's important that any old registration / function pointers are removed before adding a new one
   ApsimRegistry &registry = ApsimRegistry::getApsimRegistry();
   ApsimRegistration *reg = registry.find(::respondToGet, componentID, systemName);
   if (reg != NULL) 
      {
      registry.erase(::respondToGet, componentID, (unsigned int)reg);
      removeGettableVar(systemName);
      }

   string buffer = "<type kind=\"";
   switch (type)
      {
   	case DTchar:   {buffer += "char"; break;}
   	case DTint4:   {buffer += "integer4"; break;}
   	case DTsingle: {buffer += "single"; break;}
   	case DTboolean:{buffer += "boolean"; break;}
   	case DTstring: {buffer += "string"; break;}
      case DTdouble: {buffer += "double"; break;}
   	default: {throw "Undefined gettable var type";}
      }
   buffer += "\" array=\"";
   if  (isArray) {
       buffer += "T";
   } else {
       buffer += "F";
   }
   buffer += "\" unit=\"(";
   buffer += units;
   buffer += ")\"/>";
   return this->addRegistration(::respondToGet,
                                -1,
                                systemName, 
                                buffer);
   }

// Build the xml fragment that describes this variable and publish to system
std::string Component::getDescription()
   {
      string returnString = "<describecomp name=\"" + name + "\">\n";
      returnString += string("<executable>") + dllName + "</executable>\n";
      returnString += string("<class>") + fileRoot(fileTail(dllName)) + "</class>\n";
      returnString += "<version>1.0</version>\n";
      returnString += "<author>APSRU</author>\n";

      returnString += ApsimRegistry::getApsimRegistry().getDescription(componentID);
    
      returnString += "</describecomp>\n";
      return returnString;
   }

void Component::removeGettableVar(const char *systemName)
// remove a variable from our list of variables
   {
   //newend = getVarMap.remove_if(....??XXX
   //getVarMap.erase(newend, getVarMap.end());
   bool found = 1;
   while (found)
      {
      found = 0;
      UInt2InfoMap::iterator i;
      for (i = getVarMap.begin();i != getVarMap.end();i++)
         if ((*i).second->name() == systemName) 
            {
            getVarMap.erase(i);  // and should probably deleteReg too..?? fixme
            found = 1;
            break;
            }
      }
   }

// A component has sent us a Variant. Pack it into a registration entry
void Component::addReturnValueMessage(ReturnValueData &returnValueData)
   {
   getVariableResponses::iterator v =
        myGetVariableResponses.find (returnValueData.ID);

   if (v == myGetVariableResponses.end())
      {
      string msg = name;
      msg += " was sent a ReturnValue message without asking for it!!";
      throw std::runtime_error(msg);
      }

   // unnecessary!!
   returnValueData.variant.setFromId(returnValueData.fromID);

   Variants *myVariants = v->second;
   myVariants->addVariant(returnValueData.variant);
   }

void Component::changeComponentOrder(const FStrings& names)
   {
   //int masterPMID = 1;
   //sendMessage(newApsimChangeOrderMessage(componentID, masterPMID, names));
   sendMessage(newApsimChangeOrderMessage(componentID, parentID, names));
   }

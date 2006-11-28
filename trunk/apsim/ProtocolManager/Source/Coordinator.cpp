#include <general/pch.h>
#pragma hdrstop

#include <assert.h>

#include <sstream>
#include <list>
#include <functional>
#include <memory>
#include <stack>

#include <general/string_functions.h>
#include <general/stl_functions.h>
#include <general/TreeNodeIterator.h>
#include <general/xml.h>

#include <ApsimShared/FStringExt.h>
#include <ApsimShared/ApsimComponentData.h>
#include <ApsimShared/ApsimDataTypeData.h>
#include <ApsimShared/ApsimSystemData.h>
#include <ApsimShared/ApsimServiceData.h>
#include <ApsimShared/ApsimRegistrationData.h>
#include <ApsimShared/ApsimSimulationFile.h>

#include <ComponentInterface/MessageDataExt.h>
#include <ComponentInterface/Component.h>

#include "ComponentAlias.h"
#include "Registrations.h"
#include "Coordinator.h"

using namespace std;
using namespace protocol;

// ------------------------------------------------------------------
// Return a blank string when requested to indicate that we don't need a wrapper DLL.
// ------------------------------------------------------------------
extern "C" EXPORT void STDCALL wrapperDLL(char* wrapperDll)
   {
   strcpy(wrapperDll, "");
   }

// ------------------------------------------------------------------
//  Short description:
//    Create an instance of our PM.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
Component* createComponent(void)
   {
   return new Coordinator();
   }

// ------------------------------------------------------------------
//  Short description:
//    Constructor

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
Coordinator::Coordinator(void)
   {
   runningMessageID = 0;
   afterInit2 = false;
   doTerminate = false;
   printReport = false;
   }

// ------------------------------------------------------------------
//  Short description:
//    destructor

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
Coordinator::~Coordinator(void)
   {
   for (Components::iterator i = components.begin();
                             i != components.end();
                             i++)
      delete i->second;
   }
// ------------------------------------------------------------------
//  Short description:
//    do init1 stuff.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void Coordinator::doInit1(const FString& sdml)
   {
   try
      {
      Component::doInit1(sdml);

      string sdmlString = string(sdml.f_str(), sdml.length());
      ApsimSimulationFile simulationData(sdmlString, true);

      title = simulationData.getTitle();
      if (componentID == parentID)
         {
         titleID = addRegistration(RegistrationType::respondToGet, "title", "<type kind=\"string\"/>");
         componentsID = addRegistration(RegistrationType::respondToGet, "components", "<type kind=\"string\" array=\"T\"/>");
         }
      printReport = simulationData.doPrintReport();
      readAllRegistrations();


      // loop through all services specified in SDML and create
      // and add a componentAlias object to our list of components.
      std::vector<string> serviceNames;
      simulationData.getServiceNames(serviceNames);
      for (std::vector<string>::iterator serviceI = serviceNames.begin();
                                         serviceI != serviceNames.end();
                                         serviceI++)
         {
         ApsimServiceData service = simulationData.getService(*serviceI);
         addComponent(service.getName(),
                      service.getExecutableFileName(),
                      "",
                      service.getXML());
         }

      // loop through all components specified in SDML and create
      // and add a componentAlias object to our list of components.
      std::vector<string> componentNames;
      simulationData.getComponentNames(componentNames);
      for (std::vector<string>::iterator componentI = componentNames.begin();
                                         componentI != componentNames.end();
                                         componentI++)
         {
         ApsimComponentData component = simulationData.getComponent(*componentI);
         addComponent(component.getName(),
                      component.getExecutableFileName(),
                      component.getComponentInterfaceFileName(),
                      component.getXML());
         }

      // loop through all systems specified in SDML and create
      // and add a componentAlias object to our list of components.
      std::vector<string> systemNames;
      simulationData.getSystemNames(systemNames);
      for (std::vector<string>::iterator systemI = systemNames.begin();
                                         systemI != systemNames.end();
                                         systemI++)
         {
         ApsimSystemData system = simulationData.getSystem(*systemI);
         addComponent(system.getName(),
                      system.getExecutableFileName(),
                      "",
                      system.getXML());
         }

      }
   catch (const runtime_error& error)
      {
      // Can't seem to throw runtime_error's across DLL boundaries.
      // Seems to work with debug info turned on but not in 'release' mode.
      // So throw a const char* instead.
      throw error.what();
      }
   }
// ------------------------------------------------------------------
//  Short description:
//    initialise this coordinator.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void Coordinator::doInit2(void)
   {
   Component::doInit2();

   // resolve all registrations.
   afterInit2 = true;

   // initialise all components.
   for (Components::iterator componentI = components.begin();
                             componentI != components.end() && !doTerminate;
                             componentI++)
      {
      if (componentI->second->ID != componentID &&
          componentI->second->ID != parentID)
         sendMessage(newInit2Message(componentID, componentI->second->ID));
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    Send start message to sequencer

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void Coordinator::doCommence(void)
   {
   // should only be called as a top level PM
   assert (parentID == 0);

   // send the commence message on to the sequencer.
   sendMessage(newCommenceMessage(componentID, sequencerID));
   }

// ------------------------------------------------------------------
//  Short description:
//    Add a component to this coordinator (system)

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void Coordinator::addComponent(const string& compName,
                               const string& compExecutable,
                               const string& componentInterfaceExecutable,
                               const string& compSdml)
   {
   // get a unique id for the component we're about to create.
   unsigned int childID = getComponentID(compName);

   // dph hack - shouldn't hardwire clock as sequencer.
   if (Str_i_Eq(compName, "clock"))
      sequencerID = childID;

   ComponentAlias* componentAlias = new ComponentAlias
         (compName,
          compExecutable,
          componentInterfaceExecutable,
          childID,
          componentID);
   components.insert(Components::value_type(childID, componentAlias));

   string fqn = name;
   fqn += ".";
   fqn += compName;

   // send component an init1 message.
   try
      {
      sendMessage(newInit1Message(componentID,
                                  childID,
                                  compSdml.c_str(),
                                  fqn.c_str(),
                                  true));
      }
   catch (const runtime_error& error)
      {
      components.erase(childID);
      delete componentAlias;
      throw;
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     method call comming in from another system

//  Notes:

//  Changes:
//    dph 6/3/2001

// ------------------------------------------------------------------
unsigned int Coordinator::getComponentID(const std::string& name)
   {
   sendMessage(newRequestComponentIDMessage(componentID,
                                            parentID,
                                            componentID,
                                            name.c_str()));
   return childComponentID;
   }
// ------------------------------------------------------------------
//  Short description:
//    a requestComponentID message has come in.  Store ID.

//  Notes:

//  Changes:
//    dph 6/3/2001

// ------------------------------------------------------------------
void Coordinator::onReturnComponentIDMessage(ReturnComponentIDData& data)
   {
   childComponentID = data.ID;
   }
// ------------------------------------------------------------------
//  Short description:
//    handle incoming getValue messages.

//  Notes:

//  Changes:
//    dph 15/5/2001

// ------------------------------------------------------------------
void Coordinator::onRequestComponentIDMessage(unsigned int fromID,
                                              RequestComponentIDData& data)
   {
   static unsigned newID = 0;
   sendMessage(newReturnComponentIDMessage(componentID,
                                           fromID,
                                           "",
                                           ++newID));
   }

// ------------------------------------------------------------------
//  Short description:
//    handle incoming registration messages.

//  Notes:

//  Changes:
//    dph 15/5/2001

// ------------------------------------------------------------------
void Coordinator::onRegisterMessage(unsigned int fromID, RegisterData& registerData)
   {
   try
      {
      string regName = asString(registerData.name);
      unsigned destID = registerData.destID;
      unsigned posPeriod = regName.find('.');

      if (registerData.kind != RegistrationType::respondToGet
          && posPeriod != string::npos)
         {
         string componentName = regName.substr(0, posPeriod);
         if (Str_i_Eq(componentName.c_str(), name))
            destID = componentID;
         else
            destID = componentNameToID(componentName);
         if (destID == INT_MAX)
            throw runtime_error("Cannot find component " + regName.substr(0, posPeriod));
         regName.erase(0, posPeriod+1);
         }

      ::Registration newReg(fromID, registerData.ID, regName, asString(registerData.type), registerData.kind);
      registrations.add(newReg, destID);
      }
   catch (const runtime_error& err)
      {
      error(err.what(), true);
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    handle incoming registration messages.

//  Notes:

//  Changes:
//    dph 15/5/2001

// ------------------------------------------------------------------
void Coordinator::onDeregisterMessage(unsigned int fromID, DeregisterData& deregisterData)
   {
   try
      {
      registrations.erase(fromID, deregisterData.ID, deregisterData.kind);
      }
   catch (const runtime_error& err)
      {
      error(err.what(), true);
      }
   }

// ------------------------------------------------------------------
// Handle incoming publish event messages.
// ------------------------------------------------------------------
void Coordinator::onPublishEventMessage(unsigned int fromID, PublishEventData& publishEventData)
   {
   try
      {
      ::Registrations::Subscriptions subscriptions;
      registrations.getSubscriptions(fromID, publishEventData.ID, RegistrationType::event, subscriptions);

      if (componentOrders.size() > 0)
         reorderSubscriptions(subscriptions);

      for (::Registrations::Subscriptions::iterator s = subscriptions.begin();
                                                    s != subscriptions.end() && !doTerminate;
                                                    s++)
         {
         // if the event is going to our parent then we need to say that it is
         // coming from us rather than our child.
         if (s->componentId == parentID)
            fromID = componentID;

         sendMessage(newEventMessage(componentID,
                                     s->componentId,
                                     s->id,
                                     fromID,
                                     publishEventData.variant));
         }
      }
   catch (const runtime_error& err)
      {
      error(err.what(), true);
      }
   }
// ------------------------------------------------------------------
//  Short description:
//    handle incoming terminate simulation messages.

//  Notes:

//  Changes:
//    dph 15/5/2001

// ------------------------------------------------------------------
void Coordinator::onTerminateSimulationMessage(void)
   {
   if (componentID != parentID)
      sendMessage(newTerminateSimulationMessage(componentID, parentID));
   else
      notifyTermination();
   doTerminate = true;
   }
// ------------------------------------------------------------------
// handle incoming getValue messages.
// ------------------------------------------------------------------
void Coordinator::onGetValueMessage(unsigned int fromID, GetValueData& getValueData)
   {
   if (!afterInit2)
      {
      string msg = "Cannot do GET's before the INIT2.\n";
      msg += "Variable name: ";
      msg += registrations.getName(fromID, getValueData.ID, RegistrationType::get);
      error(msg.c_str(), true);
      }
   else
      sendQueryValueMessage(fromID, getValueData.ID);
   }
// ------------------------------------------------------------------
// Send queryValue messages to all subscribed components.
// ------------------------------------------------------------------
void Coordinator::sendQueryValueMessage(unsigned fromID, unsigned regID)
   {
   try
      {
      ::Registrations::Subscriptions subs;

      registrations.getSubscriptions(fromID, regID, RegistrationType::get, subs);

      // apsim hack to poll modules for variables.  This is because we haven't
      // yet got all the .interface files up to date.
      if (subs.size() == 0)
         {
         string regName = registrations.getName(fromID, regID, RegistrationType::get);
         unsigned destID = registrations.getDestId(fromID, regID, RegistrationType::get);
         pollComponentsForGetVariable(regName, destID);
         registrations.getSubscriptions(fromID, regID, RegistrationType::get, subs);
         }

      previousGetValueCompID.push(fromID);
      previousGetValueRegID.push(regID);
      for (::Registrations::Subscriptions::iterator s = subs.begin();
                                                    s != subs.end();
                                                    s++)
         {
         sendMessage(newQueryValueMessage(componentID,
                                          s->componentId,
                                          s->id,
                                          fromID));
         }
      previousGetValueCompID.pop();
      previousGetValueRegID.pop();
      }
   catch (const runtime_error& err)
      {
      error(err.what(), true);
      }
   }
// ------------------------------------------------------------------
// Send a returnValue message back to originating component.
// ------------------------------------------------------------------
void Coordinator::onReplyValueMessage(unsigned fromID, ReplyValueData replyValueData)
   {
   try
      {
      unsigned toID = previousGetValueCompID.top();
      if (toID == parentID || components[toID]->isSystem())
         {
         sendMessage(newReplyValueMessage(componentID,
                                          toID,
                                          previousGetValueRegID.top(),
                                          replyValueData.variant));
         }
      else
         {
         sendMessage(newReturnValueMessage(componentID,
                                           toID,
                                           fromID,
                                           previousGetValueRegID.top(),
                                           replyValueData.variant));
         }
      }
   catch (const runtime_error& err)
      {
      error(err.what(), true);
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    handle the incoming query info message.

//  Notes:

//  Changes:
//    dph 15/5/2001

// ------------------------------------------------------------------
void Coordinator::onQueryInfoMessage(unsigned int fromID,
                                     unsigned int messageID,
                                     QueryInfoData& queryInfo)
   {
   unsigned componentId = 0;

   string childName = asString(queryInfo.name);
   unsigned posPeriod = childName.find('.');
   if (posPeriod != string::npos)
      {
      string componentName = childName.substr(0, posPeriod);
      childName.erase(0, posPeriod+1);
      if (componentName != "*")
         componentId = componentNameToID(componentName);
      }
   std::vector< ::Registration> matches;

   if (queryInfo.kind == respondToGetInfo)
      {
      registrations.findMatching(componentId, childName, RegistrationType::respondToGet, matches);
      if (matches.size() == 0)
         {
         pollComponentsForGetVariable(childName, componentId);
         registrations.findMatching(componentId, childName, RegistrationType::respondToGet, matches);
         }
      }
   else if (queryInfo.kind == respondToSetInfo)
      registrations.findMatching(componentId, childName, RegistrationType::respondToSet, matches);
   else if (queryInfo.kind == respondToEventInfo)
      registrations.findMatching(componentId, childName, RegistrationType::respondToEvent, matches);
   else if (queryInfo.kind == componentInfo)
      {
      unsigned childID;
      if (Is_numerical(childName.c_str()))
         {
         childID = atoi(childName.c_str());
         if (components.find(childID) != components.end())
            childName = components[childID]->getName();
         if (childID == componentID)
            childName = name;
         }
      else
         childID = componentNameToID(childName);

      if (childID == INT_MAX || childName == "") return; // XX Yuck!!

      string fqn = name;
      fqn += ".";
      fqn += childName;
      sendMessage(newReturnInfoMessage(componentID,
                                       fromID,
                                       messageID,
                                       childID,
                                       childID,
                                       fqn.c_str(),
                                       " ",
                                       queryInfo.kind));
      }

   for (unsigned i = 0; i != matches.size(); i++)
      {
      char buffer[100];
      FString st(buffer, sizeof(buffer), CString);
      componentIDToName(matches[i].componentId, st);
      string fqn = asString(st);
      fqn += ".";
      fqn += matches[i].name;

      sendMessage(newReturnInfoMessage(componentID,
                                       fromID,
                                       messageID,
                                       matches[i].componentId,
                                       matches[i].id,
                                       fqn.c_str(),
                                       matches[i].ddml.c_str(),
                                       queryInfo.kind));
      }
   }

// ------------------------------------------------------------------
// Handle the incoming requestSetValue message.
// ------------------------------------------------------------------
void Coordinator::onRequestSetValueMessage(unsigned int fromID,
                                           RequestSetValueData& setValueData)
   {
   sendQuerySetValueMessage(fromID, fromID,
                            setValueData.ID, setValueData.ID,
                            setValueData.variant);
   }
// ------------------------------------------------------------------
// Send a querySetValueMessage
// ------------------------------------------------------------------
void Coordinator::sendQuerySetValueMessage(unsigned ourComponentID,
                                           unsigned foreignComponentID,
                                           unsigned ourRegID,
                                           unsigned foreignRegID,
                                           protocol::Variant& variant)
   {
   try
      {
      ::Registrations::Subscriptions subs;
      bool hasBeenResolved = registrations.isResolved(ourComponentID, ourRegID, RegistrationType::set);
      registrations.getSubscriptions(ourComponentID, ourRegID, RegistrationType::set, subs);

      // apsim hack to poll modules for variables.  This is because we haven't
      // yet got all the .interface files up to date.
      if (!hasBeenResolved && subs.size() == 0)
         {
         string regName = registrations.getName(ourComponentID, ourRegID, RegistrationType::set);
         string fqn = itoa(registrations.getDestId(ourComponentID, ourRegID, RegistrationType::set));
         fqn += "." + regName;
         bool havePolled = (variablesBeenPolledForSets.find(fqn)
            != variablesBeenPolledForSets.end());
         if (!havePolled)
            {
            variablesBeenPolledForSets.insert(fqn);
            unsigned destID = registrations.getDestId(ourComponentID, ourRegID, RegistrationType::set);
            pollComponentsForSetVariable(regName, destID, foreignComponentID, foreignRegID, variant);
            registrations.getSubscriptions(ourComponentID, ourRegID, RegistrationType::set, subs);
            return;
            }
         }

      if (subs.size() == 0)
         {
         string regName = registrations.getName(ourComponentID, ourRegID, RegistrationType::set);
         throw runtime_error("No module allows a set of the variable " + regName);
         }

      else if (subs.size() == 1)
         sendMessage(newQuerySetValueMessage(foreignComponentID,
                                             subs[0].componentId,
                                             subs[0].id,
                                             variant));
      else if (subs.size() > 1)
         {
         string regName = registrations.getName(ourComponentID, ourRegID, RegistrationType::set);
         throw runtime_error("Too many modules allow a set of the variable " + regName);
         }
      }
   catch (const runtime_error& err)
      {
      error(err.what(), true);
      }
   }
// ------------------------------------------------------------------
// process the querySetValueMessage.
// ------------------------------------------------------------------
void Coordinator::onQuerySetValueMessage(unsigned fromID, QuerySetValueData& querySetData)
   {
   respondToSet(fromID, querySetData);
   }
// ------------------------------------------------------------------
//  Short description:
//    Return a component ID for the specified name.  Returns
//    INTMAX if not found.

//  Notes:

//  Changes:
//    dph 15/5/2001

// ------------------------------------------------------------------
unsigned Coordinator::componentNameToID(const std::string& name)
   {
   for (Components::iterator componentI = components.begin();
                             componentI != components.end();
                             componentI++)
      {
      if (Str_i_Eq(componentI->second->getName(), name))
         return componentI->first;
      }
   return INT_MAX;
   }
// ------------------------------------------------------------------
// apsim hack to poll modules for gettable variables.  This is because we haven't
// yet got all the .interface files up to date.
// ------------------------------------------------------------------
void Coordinator::pollComponentsForGetVariable(const string& variableName,
                                               unsigned destID)
   {
   string fqn = itoa(destID) + string(".") + variableName;

   bool havePolled = (variablesBeenPolledForGets.find(fqn)
      != variablesBeenPolledForGets.end());
   if (!havePolled)
      {
      variablesBeenPolledForGets.insert(fqn);

      string lowerName = variableName;
      To_lower(lowerName);
      if (destID > 0)
         {
         sendMessage(newApsimGetQueryMessage(componentID, destID,
                                             lowerName.c_str()));
         }
      else
         {
         for (Components::iterator i = components.begin();
                                   i != components.end();
                                   i++)
            {
            sendMessage(newApsimGetQueryMessage(componentID, i->second->ID,
                                                lowerName.c_str()));
            }
         }
      }
   }

// ------------------------------------------------------------------
// apsim hack to poll modules for settable variables.  This is because we haven't
// yet got all the .interface files up to date.
// ------------------------------------------------------------------
void Coordinator::pollComponentsForSetVariable(const string& variableName,
                                               unsigned destID,
                                               unsigned fromID,
                                               unsigned ourRegID,
                                               protocol::Variant& variant)
   {
   string lowerName = variableName;
   To_lower(lowerName);
   if (destID > 0)
      sendMessage(newApsimSetQueryMessage(componentID,
                                          destID,
                                          variableName.c_str(),
                                          fromID,
                                          ourRegID,
                                          variant));

   else
      {
      for (Components::iterator i = components.begin();
                                i != components.end();
                                i++)
         {
         sendMessage(newApsimSetQueryMessage(componentID,
                                             i->second->ID,
                                             variableName.c_str(),
                                             fromID,
                                             ourRegID,
                                             variant));
         }
      }
   }
// ------------------------------------------------------------------
// Called by component (CANOPY) to change the order that events are sent
// to a module.
//     Modules are swapped thus:
//      e.g. if componentNames[0] = 'x',
//              componentNames[1] = 'y',
//              componentNames[2] = 'z'
//         Then y will replace x, z will replace y, and x will replace z
//         leaving: y, z, x
// ------------------------------------------------------------------
void Coordinator::onApsimChangeOrderData(MessageData& messageData)
   {
   std::vector<string> componentNames;
   messageData >> componentNames;
   if (componentOrders.size() == 0)
      {
      for (unsigned i = 0; i != componentNames.size(); ++i)
         {
         unsigned componentID = componentNameToID(componentNames[i]);
         if (componentID == INT_MAX)
            {
            string msg;
            msg = "The CANOPY module has specified that " + componentNames[i]
                + " be intercropped\nbut that module doesn't exist in the control file.";
            throw runtime_error(msg);
            }
         componentOrders.push_back(componentID);
         }
      }
   // move all items up 1 spot.  Move top spot to bottom.
   for (unsigned i = 1; i != componentOrders.size(); i++)
      swap(componentOrders[i-1], componentOrders[i]);
   }
// ------------------------------------------------------------------
// Handle incoming publish event message but make sure the order
// of events is the same as that specified in the componentOrder.
// ------------------------------------------------------------------
void Coordinator::reorderSubscriptions(::Registrations::Subscriptions& subs)
   {
   ::Registrations::Subscriptions subsToMove = subs;
   ::Registrations::Subscriptions newSubs;

   while (subsToMove.size() > 0)
      {
      ::Registrations::Subscriptions::iterator sub = subsToMove.begin();
      if (find(componentOrders.begin(), componentOrders.end(),
               sub->componentId) == componentOrders.end())
         {
         newSubs.push_back(*sub);
         subsToMove.erase(subsToMove.begin());
         }
      else
         {
         for (unsigned o = 0; o != componentOrders.size(); o++)
            {
            for (::Registrations::Subscriptions::iterator s = subsToMove.begin();
                                                          s != subsToMove.end();
                                                          s++)
               {
               if (s->componentId == componentOrders[o])
                  {
                  newSubs.push_back(*s);
                  s = subsToMove.erase(s);
                  break;
                  }
               }
            }
         }
      }
   subs = newSubs;
   }
// ------------------------------------------------------------------
// read all registrations for this Component.
// ------------------------------------------------------------------
void Coordinator::readAllRegistrations(void)
   {
   for (ApsimComponentData::RegIterator reg = componentData->regBegin();
                                        reg != componentData->regEnd();
                                        reg++)
      {
      RegistrationType regType(reg->getType());
      RegistrationType oppositeRegType = regType.opposite();
      string internalName = reg->getInternalName();
      if (internalName == "")
         internalName = reg->getName();

      ApsimDataTypeData dataType = componentData->getDataType(reg->getDataTypeName());
      unsigned regId = addRegistration(regType, reg->getName().c_str(),
                                       dataType.getTypeString().c_str());

      registrations.add(::Registration(parentID, regId, internalName, "", oppositeRegType));
      }
   }
// ------------------------------------------------------------------
// respond to a event that has happened.
// ------------------------------------------------------------------
void Coordinator::respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
   {
   PublishEventData publishEventData;
   publishEventData.ID = eventID;
   publishEventData.variant = variant;

   // need to work out if this event has come from outside this system or
   // from one of our child components.  If from outside then set the
   // fromID to point to our parent.
   unsigned foreignComponentID = fromID;
   if (components.find(fromID) == components.end())
      foreignComponentID = parentID;
   onPublishEventMessage(foreignComponentID, publishEventData);
   }
// ------------------------------------------------------------------
// return one of our variables to caller
// ------------------------------------------------------------------
void Coordinator::respondToGet(unsigned int& fromID, QueryValueData& queryValueData)
   {                  
   if (queryValueData.ID == titleID)
      sendVariable(queryValueData, FString(title.c_str()));
   else if (queryValueData.ID == componentsID)
      {
      std::vector<string> comps;
      for (Components::iterator c = components.begin();
                                c != components.end();
                                c++)
         {
         string dll = c->second->getExecutable();
         if (dll != "")
            comps.push_back(dll);
         }
      sendVariable(queryValueData, comps);
      }
   else
      sendQueryValueMessage(fromID, queryValueData.ID);
   }
// ------------------------------------------------------------------
// respond to a method call request
// ------------------------------------------------------------------
bool Coordinator::respondToSet(unsigned int& fromID, QuerySetValueData& setValueData)
   {
   sendQuerySetValueMessage(parentID, fromID,
                            setValueData.ID, setValueData.ID,
                            setValueData.variant);
   return getSetVariableSuccess();
   }
// ------------------------------------------------------------------
// respond to a method call request
// ------------------------------------------------------------------
void Coordinator::notifyTermination(void)
   {
   for (Components::iterator componentI = components.begin();
                             componentI != components.end();
                             componentI++)
      sendMessage(newNotifyTerminationMessage(componentID,
                                              componentI->second->ID));
   }
// ------------------------------------------------------------------
// A parent PM has asked us to provide a registration for the
// specified variable.
// ------------------------------------------------------------------
void Coordinator::onApsimGetQuery(ApsimGetQueryData& apsimGetQueryData)
   {
   string fqn = asString(apsimGetQueryData.name);
   unsigned posPeriod = fqn.find('.');
   if (posPeriod != string::npos)
      {
      string componentName = fqn.substr(0, posPeriod);
      unsigned childComponentID = componentNameToID(componentName);
      if (childComponentID != INT_MAX)
         {
         string variableName = fqn.substr(posPeriod+1);
         std::vector< ::Registration> matches;
         registrations.findMatching(childComponentID, variableName, RegistrationType::respondToGet, matches);
         if (matches.size() == 0)
            pollComponentsForGetVariable(variableName, childComponentID);

         matches.erase(matches.begin(), matches.end());
         registrations.findMatching(childComponentID, variableName, RegistrationType::respondToGet, matches);
         if (matches.size() > 0)
            {
            unsigned parentRegId = addRegistration(RegistrationType::respondToGet,
                                                   fqn.c_str(),
                                                   "<type/>");
            registrations.add(::Registration(parentID, parentRegId, matches[0].name, matches[0].ddml, RegistrationType::get));
            }
         }
      }
   }

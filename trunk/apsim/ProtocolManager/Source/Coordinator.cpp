#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include <ComponentInterface\messageDataExt.h>
#include "Coordinator.h"
#include <ApsimShared\FStringExt.h>
#include <ComponentInterface\messages.h>
#include <assert.h>
#include <general\stl_functions.h>
#include <ApsimShared\ApsimSimulationFile.h>
#include <sstream>
#include <list>
#include <functional>
#include <memory>
#include "DebugHook.h"
using namespace std;
using namespace protocol;

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

      string sdmlString(sdml.f_str(), sdml.length());
      ApsimSimulationFile simulationData(sdmlString, true);

      title = simulationData.getTitle();
      if (componentID == parentID)
         {
         titleID = addRegistration(respondToGetReg, "title", "<type kind=\"string\"/>");
         componentsID = addRegistration(respondToGetReg, "components", "<type kind=\"string\" array=\"T\"/>");
         }
      printReport = simulationData.doPrintReport();

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
                      system.getXML());
         }

      readAllRegistrations();

      // Perform a debug interrupt so that the FORTRAN debugger can
      // stop.
      DebugException();
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
   registrations.resolveAll();
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
   string regName;
   if (registerData.destID > 0)
      regName = components[registerData.destID]->getName() + "." + asString(registerData.name);
   else
      regName = asString(registerData.name);
   try
      {
      convertKindToMethodCall(registerData.kind, regName);

      string childName;
      if (fromID == componentID)
         childName = name;
      else
         childName = components[fromID]->getName();
      registrations.add(fromID, registerData.ID, regName,
                        registerData.kind, childName,
                        afterInit2);
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
      registrations.erase(fromID, deregisterData.ID);
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
      registrations.getSubscriptions(fromID, publishEventData.ID, subscriptions);

      if (componentOrders.size() > 0)
         reorderSubscriptions(subscriptions);

      if (subscriptions.size() == 0 && registrations.isMethodCall(fromID, publishEventData.ID))
         throw runtime_error("No module responded to method call: "
            + registrations.getRegistrationName(fromID, publishEventData.ID));

      for (::Registrations::Subscriptions::iterator s = subscriptions.begin();
                                                  s != subscriptions.end() && !doTerminate;
                                                  s++)
         {
         // if the event is going to our parent then we need to say that it is
         // coming from us rather than our child.
         if (s->first == parentID)
            fromID = componentID;

         sendMessage(newEventMessage(componentID,
                                     s->first,
                                     s->second,
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
   doTerminate = true;
   if (parentID == 0)
      {
      notifyTermination();
      for (Components::iterator componentI = components.begin();
                                componentI != components.end();
                                componentI++)
         sendMessage(newNotifyTerminationMessage(componentID,
                                                 componentI->second->ID));
      }
   else
      sendMessage(newTerminateSimulationMessage(componentID, parentID));
   }
// ------------------------------------------------------------------
// handle incoming getValue messages.
// ------------------------------------------------------------------
void Coordinator::onGetValueMessage(unsigned int fromID, GetValueData& getValueData)
   {
   sendQueryValueMessage(fromID, fromID, getValueData.ID, getValueData.ID);
   }
// ------------------------------------------------------------------
// Send queryValue messages to all subscribed components.  The toID
// is used so that the receiving component can send a returnValue
// message straight back to the originating module.  The fromID
// can be different to the toID in a multi-paddock simulation where
// the parent PM may be simply routing a queryValue message on behalf
// of the originating component.
// ------------------------------------------------------------------
void Coordinator::sendQueryValueMessage(unsigned ourComponentID,
                                        unsigned foreignComponentID,
                                        unsigned ourRegID,
                                        unsigned foreignRegID)
   {
   try
      {
      ::Registrations::Subscriptions subs;
      registrations.getSubscriptions(ourComponentID, ourRegID, subs);

      // apsim hack to poll modules for variables.  This is because we haven't
      // yet got all the .interface files up to date.
      if (subs.size() == 0)
         {
         string regName = registrations.getRegistrationName(ourComponentID, ourRegID);
         pollComponentsForGetVariable(regName);
         registrations.getSubscriptions(ourComponentID, ourRegID, subs);
         }

      for (::Registrations::Subscriptions::iterator s = subs.begin();
                                                    s != subs.end();
                                                    s++)
         {
         sendMessage(newQueryValueMessage(componentID,
                                          s->first,
                                          s->second,
                                          foreignComponentID,
                                          foreignRegID));
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
   std::vector< ::Registrations::Info> info;
   if (queryInfo.kind == respondToGetInfo)
      registrations.findMatchingRegistrations(asString(queryInfo.name), respondToGetReg, info);
   else if (queryInfo.kind == respondToSetInfo)
      registrations.findMatchingRegistrations(asString(queryInfo.name), respondToSetReg, info);
   else if (queryInfo.kind == respondToMethodInfo)
      registrations.findMatchingRegistrations(asString(queryInfo.name), respondToMethodCallReg, info);
   else if (queryInfo.kind == respondToEventInfo)
      registrations.findMatchingRegistrations(asString(queryInfo.name), respondToEventReg, info);
   else if (queryInfo.kind == componentInfo)
      {
      string name = asString(queryInfo.name);
      unsigned int componentID;
      if (Is_numerical(name.c_str()))
         componentID = atoi(name.c_str());
      else
         {
         componentID = componentNameToID(name);
         if (componentID == INT_MAX)
            return;
         }
      string fqn = name;
      fqn += ".";
      fqn += components[componentID]->getName();
      sendMessage(newReturnInfoMessage(componentID,
                                       fromID,
                                       messageID,
                                       componentID,
                                       componentID,
                                       fqn.c_str(),
                                       " ",
                                       queryInfo.kind));
      }

   for (unsigned i = 0; i != info.size(); i++)
      sendMessage(newReturnInfoMessage(componentID,
                                       fromID,
                                       messageID,
                                       info[i].componentId,
                                       info[i].regId,
                                       info[i].fqn.c_str(),
                                       " ",
                                       queryInfo.kind));
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
      registrations.getSubscriptions(ourComponentID, ourRegID, subs);

      // apsim hack to poll modules for variables.  This is because we haven't
      // yet got all the .interface files up to date.
      if (subs.size() == 0)
         {
         string regName = registrations.getRegistrationName(ourComponentID, ourRegID);
         pollComponentsForSetVariable(regName, ourComponentID, ourRegID, variant);
         registrations.getSubscriptions(ourComponentID, ourRegID, subs);
         if (subs.size() == 0)
            throw runtime_error("No module allows a set of the variable " + regName);
         }
      else if (subs.size() == 1)
         sendMessage(newQuerySetValueMessage(componentID,
                                             subs[0].first,
                                             subs[0].second,
                                             foreignComponentID,
                                             foreignRegID,
                                             variant));
      else
         {
         string regName = registrations.getRegistrationName(ourComponentID, ourRegID);
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
void Coordinator::pollComponentsForGetVariable(const string& variableName)
   {
   string lowerName = variableName;
   To_lower(lowerName);
   for (Components::iterator i = components.begin();
                             i != components.end();
                             i++)
      {
      sendMessage(newApsimGetQueryMessage(componentID, i->second->ID,
                                          lowerName.c_str()));
      }
   }

// ------------------------------------------------------------------
// apsim hack to poll modules for settable variables.  This is because we haven't
// yet got all the .interface files up to date.
// ------------------------------------------------------------------
void Coordinator::pollComponentsForSetVariable(const string& variableName,
                                               unsigned fromID,
                                               unsigned ourRegID,
                                               protocol::Variant& variant)
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
         if (componentID == INT_MAX) {throw runtime_error("attempt to change order of non-existant module");}
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
   ::Registrations::Subscriptions newSubs;

   // Loop through all the component orders and make sure the subs passed in
   // are in the same order.
   for (unsigned o = 0; o != componentOrders.size(); o++)
      {
      for (::Registrations::Subscriptions::iterator sub = subs.begin();
                                                    sub != subs.end();
                                                    sub++)
         {
         if (sub->first == componentOrders[o])
            {
            newSubs.push_back(*sub);
            subs.erase(sub);
            break;
            }
         }
      }
   // copy whatever is left in subs to the end of the newSubs container.
   copy(subs.begin(), subs.end(), back_inserter(newSubs));

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
      RegistrationType kind;
      RegistrationType oppositeKind;
      string kindString = reg->getType();
      string internalName = reg->getInternalName();
      if (internalName == "")
         internalName = reg->getName();
      if (Str_i_Eq(kindString, "getVariableReg"))
         {
         kind = getVariableReg;
         oppositeKind = respondToGetReg;
         }
      else if (Str_i_Eq(kindString, "setVariableReg"))
         {
         kind = setVariableReg;
         oppositeKind = respondToSetReg;
         }
      else if (Str_i_Eq(kindString, "methodCallReg"))
         {
         kind = methodCallReg;
         oppositeKind = respondToMethodCallReg;
         }
      else if (Str_i_Eq(kindString, "eventReg"))
         {
         kind = eventReg;
         oppositeKind = respondToEventReg;
         }
      else if (Str_i_Eq(kindString, "respondToGetReg"))
         {
         kind = respondToGetReg;
         oppositeKind = getVariableReg;
         }
      else if (Str_i_Eq(kindString, "respondToSetReg"))
         {
         kind = respondToSetReg;
         oppositeKind = setVariableReg;
         }
      else if (Str_i_Eq(kindString, "respondToMethodCallReg"))
         {
         kind = respondToMethodCallReg;
         oppositeKind = methodCallReg;
         }
      else if (Str_i_Eq(kindString, "respondToEventReg"))
         {
         kind = respondToEventReg;
         oppositeKind = eventReg;
         }

      ApsimDataTypeData dataType = componentData->getDataType(reg->getDataTypeName());
      unsigned regId = addRegistration(kind, reg->getName().c_str(),
                                       dataType.getTypeString().c_str());

      registrations.add(parentID, regId, internalName, oppositeKind, name, true);
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
// respond to a method call request
// ------------------------------------------------------------------
void Coordinator::respondToMethod(unsigned int& fromID, unsigned int& methodID, protocol::Variant& variant)
   {
   PublishEventData publishEventData;
   publishEventData.ID = methodID;
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
void Coordinator::respondToGet(unsigned int& fromID, QueryValueData& queryData)
   {
   if (queryData.ID == titleID)
      sendVariable(queryData, FString(title.c_str()));
   else if (queryData.ID == componentsID)
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
      sendVariable(queryData, comps);
      }
   else
      sendQueryValueMessage(fromID, queryData.replytoID, queryData.ID, queryData.replyID);
   }
// ------------------------------------------------------------------
// respond to a method call request
// ------------------------------------------------------------------
bool Coordinator::respondToSet(unsigned int& fromID, QuerySetValueData& setValueData)
   {
   sendQuerySetValueMessage(parentID, setValueData.replyToID,
                            setValueData.ID, setValueData.replyID,
                            setValueData.variant);
   return getSetVariableSuccess();
   }
// ------------------------------------------------------------------
// respond to a method call request
// ------------------------------------------------------------------
void Coordinator::notifyTermination(void)
   {
   if (printReport)
      {
      string msg = "---------- Registrations for: ";
      msg += name;
      msg += "----------";
      writeString(msg.c_str());

      string reportContents;
      registrations.printReport(reportContents);
      writeString(reportContents.c_str());

      writeString("--------------------");
      }
   }
// ------------------------------------------------------------------
// convert and event or respondtoevent to a methodCall or
// respondToMethodCall if the registration name has a period in it.
// ------------------------------------------------------------------
void Coordinator::convertKindToMethodCall(protocol::RegistrationType& kind,
                                          const string& regName)
   {
   if (regName.find('.') != string::npos)
      {
      if (kind == eventReg)
         kind = methodCallReg;
      else if (kind == respondToEventReg)
         kind = respondToMethodCallReg;
      }
   }


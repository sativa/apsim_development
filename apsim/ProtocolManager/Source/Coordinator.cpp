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
      // Add a ComponentAlias for ourselves and our parent.
      // This is because we sometimes send messages (e.g error)to ourselves
      // and sometimes to our parent.
      components.insert(Components::value_type(componentID,
            new ComponentAlias(name, componentID)));
      components.insert(Components::value_type(parentID,
            new ComponentAlias("parent", parentID)));

      Component::doInit1(sdml);

      // cast the componentData to a systemData.
      string sdmlString(sdml.f_str(), sdml.length());
      ApsimSimulationFile simulationData(sdmlString, true);

      // read in title and register a respondToGet
      static const char* stringDDML = "<type kind=\"string\"\\>";
      static const char* stringArrayDDML = "<type kind=\"string\" array=\"T\"\\>";

      title = simulationData.getTitle();
      if (componentID == parentID)
         titleID = addInternalRegistration(componentID, respondToGetReg, "title", stringDDML);
      componentsID = addInternalRegistration(componentID, respondToGetReg, "components", stringArrayDDML);

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
// Register a property for our child components
// ------------------------------------------------------------------
unsigned Coordinator::addInternalRegistration(unsigned fromID,
                                              RegistrationType kind,
                                              const string& name,
                                              const string& typeString,
                                              unsigned foreignID)
   {
   RegisterData registerData;
   registerData.kind = kind;
   registerData.ID = (unsigned) addRegistrationToList(kind,
                                                      name.c_str(),
                                                      typeString.c_str());
   if (foreignID != 0)
      registerData.ID = foreignID;
   registerData.destID = 0;
   registerData.name = name.c_str();
   registerData.type = typeString.c_str();
   onRegisterMessage(fromID, registerData);
   return registerData.ID;
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
   resolveRegistrations();
   afterInit2 = true;

   // initialise all components.
   for (Components::iterator componentI = components.begin();
                             componentI != components.end();
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
   if (registerData.kind == respondToGetSetReg)
      {
      registerData.kind = respondToGetReg;
      onRegisterMessage(fromID, registerData);
      registerData.kind = respondToSetReg;
      onRegisterMessage(fromID, registerData);
      }
   else
      {
      ComponentAlias::Registrations* registrations = components[fromID]->getRegistrationsForKind(registerData.kind);

      string regName = asString(registerData.name);
      string componentName;
      unsigned destID = registerData.destID;
      unsigned posPeriod = regName.find('.');
      if (destID == 0 && posPeriod != string::npos)
         {
         componentName = regName.substr(0, posPeriod);
         regName = regName.substr(posPeriod+1, regName.length()-posPeriod);
         }

      PMRegistrationItem* regItem = new PMRegistrationItem(regName,
                                                           componentName,
                                                           destID,
                                                           registerData.kind,
                                                           fromID,
                                                           registerData.ID);
      registrations->insert(ComponentAlias::Registrations::value_type(registerData.ID, regItem));
      if (afterInit2)
         {
         fixupRegistrationID(*regItem);
         resolveRegistration(regItem);
         }
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
   components[fromID]->getRegistrationsForKind(deregisterData.kind)->erase(deregisterData.ID);
   }

// ------------------------------------------------------------------
// Handle incoming publish event messages.
// ------------------------------------------------------------------
void Coordinator::onPublishEventMessage(unsigned int fromID, PublishEventData& publishEventData)
   {
   ComponentAlias::Registrations& registrations = *components[fromID]->getRegistrationsForKind(protocol::eventReg);
   PMRegistrationItem* registrationItem = registrations[publishEventData.ID];
   if (componentOrders.size() > 0)
      publishEventsInOrder(fromID, publishEventData, registrationItem);
   else
      {
      for (PMRegistrationItem::InterestedItems::iterator
                                     interestI = registrationItem->interestedItems.begin();
                                     interestI != registrationItem->interestedItems.end();
                                     interestI++)
         {
         if ((*interestI)->componentID == parentID)
            fromID = componentID;
         sendMessage(newEventMessage(componentID,
                                     (*interestI)->componentID,
                                     (*interestI)->registrationID,
                                     fromID,
                                     publishEventData.variant));
         }
      // display an error message when a event/method is directed to a module
      // but that module has registered an interest in it.
      if (registrationItem->destID != 0 && registrationItem->interestedItems.size() == 0)
         {
         string msg = "Cannot deliver method: " + registrationItem->name +
                      " to module: " + registrationItem->componentName +
                      "\nThe module has not registered an interest in that event/method.";
         error(msg.c_str(), true);
         }
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
   if (parentID == 0)
      for (Components::iterator componentI = components.begin();
                                componentI != components.end();
                                componentI++)
         sendMessage(newNotifyTerminationMessage(componentID,
                                                 componentI->second->ID));
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
   ComponentAlias::Registrations& registrations = *components[ourComponentID]->getRegistrationsForKind(protocol::getVariableReg);

   // See if we have a registration.
   ComponentAlias::Registrations::iterator i = registrations.find(ourRegID);
   if (i == registrations.end())
      {
      char msg[300];
      strcpy(msg, "A component has requested the value of a variable that hasn't\n"
                  "been registered.\nRequesting component:");
      strcat(msg, components[ourComponentID]->getName().c_str());
      strcat(msg, "\nGetValue ID:");
      itoa(ourRegID, &msg[strlen(msg)], 10);
      ::MessageBox(NULL, msg, "Error", MB_ICONSTOP | MB_OK);
      }
   else
      {
      PMRegistrationItem& registrationItem = *i->second;

      // apsim hack to poll modules for variables.  This is because we haven't
      // yet got all the .interface files up to date.
      if (registrationItem.interestedItems.size() == 0)
         pollComponentsForGetVariable(registrationItem);

      for (PMRegistrationItem::InterestedItems::iterator
                                     interestI = registrationItem.interestedItems.begin();
                                     interestI != registrationItem.interestedItems.end();
                                     interestI++)
         {
         sendMessage(newQueryValueMessage(componentID,
                                          (*interestI)->componentID,
                                          (*interestI)->registrationID,
                                          foreignComponentID,
                                          foreignRegID));
         }
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
   PMRegistrationItem* reg = NULL;
   if (queryInfo.kind == respondToGetInfo)
      reg = findRegistration(asString(queryInfo.name), respondToGetReg);
   else if (queryInfo.kind == respondToSetInfo)
      reg = findRegistration(asString(queryInfo.name), respondToSetReg);
   else if (queryInfo.kind == respondToMethodInfo)
      reg = findRegistration(asString(queryInfo.name), respondToMethodCallReg);
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

   if (reg != NULL)
      {
      string fqn = name;
      fqn += ".";
      fqn += components[reg->componentID]->getName();
      fqn += ".";
      fqn += asString(queryInfo.name);
      sendMessage(newReturnInfoMessage(componentID,
                                       fromID,
                                       messageID,
                                       reg->componentID,
                                       reg->registrationID,
                                       fqn.c_str(),
                                       " ",
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
   ComponentAlias::Registrations& registrations = *components[ourComponentID]->getRegistrationsForKind(protocol::setVariableReg);
   PMRegistrationItem& registrationItem = *registrations[ourRegID];

   // apsim hack to poll modules for variables.  This is because we haven't
   // yet got all the .interface files up to date.
   if (registrationItem.interestedItems.size() == 0)
      pollComponentsForSetVariable(registrationItem, ourComponentID, ourRegID, variant);

   else if (registrationItem.interestedItems.size() == 1)
      sendMessage(newQuerySetValueMessage(componentID,
                                          registrationItem.interestedItems[0]->componentID,
                                          registrationItem.interestedItems[0]->registrationID,
                                          foreignComponentID,
                                          foreignRegID,
                                          variant));
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
//    go find a specific registration that matches the specified name
//    and type.  It searches all components.  It more than one match
//    is found, an error is issued and NULL is returned.

//  Changes:
//    dph 15/5/2001
// ------------------------------------------------------------------
PMRegistrationItem* Coordinator::findRegistration(const std::string& name,
                                                  RegistrationType kind)
   {
   for (unsigned c = 0; c < components.size(); c++)
      {
      ComponentAlias::Registrations* registrations
         = components[c]->getRegistrationsForKind(kind);
      for (ComponentAlias::Registrations::iterator regI = registrations->begin();
                                                   regI != registrations->end();
                                                   regI++)
         {
         if (Str_i_Eq(name, regI->second->name))
            return regI->second;
         }
      }
   return NULL;
   }
// ------------------------------------------------------------------
// find registrations for the specified registration.
// ------------------------------------------------------------------
void Coordinator::resolveRegistration(PMRegistrationItem* reg)
   {
   if (reg->destID == 0)
      {
      for (Components::iterator componentI = components.begin();
                                componentI != components.end();
                                componentI++)
         {
         ComponentAlias::Registrations* registrations
            = componentI->second->getRegistrationsForKind(getOppositeType(reg->type));
         for (ComponentAlias::Registrations::iterator regI = registrations->begin();
                                                      regI != registrations->end();
                                                      regI++)
            {
            if (RegMatch(*reg, *regI->second))
               {
               if (find(reg->interestedItems.begin(), reg->interestedItems.end(),
                        regI->second) == reg->interestedItems.end())
                  reg->interestedItems.push_back(regI->second);
               if (find(regI->second->interestedItems.begin(), regI->second->interestedItems.end(),
                   reg) == regI->second->interestedItems.end())
                  regI->second->interestedItems.push_back(reg);
               }
            }
         }
      }
   else
      {
      ComponentAlias::Registrations* registrations
         = components[reg->destID]->getRegistrationsForKind(getOppositeType(reg->type));
      for (ComponentAlias::Registrations::iterator regI = registrations->begin();
                                                   regI != registrations->end();
                                                   regI++)
         {
         if (RegMatch(*reg, *regI->second))
            {
            reg->interestedItems.push_back(regI->second);
            regI->second->interestedItems.push_back(reg);
            }
         }
      }
   }
// ------------------------------------------------------------------
//  Short description:
//    Go resolve all registrations for this system.

//  Notes:

//  Changes:
//    dph 15/5/2001

// ------------------------------------------------------------------
void Coordinator::resolveRegistrations(void)
   {
   fixupRegistrationIDs(getVariableReg);
   fixupRegistrationIDs(methodCallReg);
   fixupRegistrationIDs(respondToGetReg);

   // loop through all registrations in all components.
   for (Components::iterator componentI = components.begin();
                             componentI != components.end();
                             componentI++)
      {
      // resolve get registrations.
      ComponentAlias::Registrations* registrations = componentI->second->getRegistrationsForKind(getVariableReg);
      resolveRegistrations(registrations);

      // resolve set registrations
      registrations = componentI->second->getRegistrationsForKind(setVariableReg);
      resolveRegistrations(registrations);

      // resolve event registrations
      registrations = componentI->second->getRegistrationsForKind(eventReg);
      resolveRegistrations(registrations);
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    Go resolve all registrations for this system.

//  Notes:

//  Changes:
//    dph 15/5/2001

// ------------------------------------------------------------------
void Coordinator::resolveRegistrations(ComponentAlias::Registrations* registrations)
   {
   for (ComponentAlias::Registrations::iterator regI = registrations->begin();
                                                regI != registrations->end();
                                                regI++)
      resolveRegistration(regI->second);
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
// Fixup all registration destID's.
// ------------------------------------------------------------------
void Coordinator::fixupRegistrationIDs(const protocol::RegistrationType& type)
   {
   for (Components::iterator component = components.begin();
                             component != components.end();
                             component++)
      {
      ComponentAlias::Registrations* registrations
         = component->second->getRegistrationsForKind(type);
      for (ComponentAlias::Registrations::iterator regI = registrations->begin();
                                                   regI != registrations->end();
                                                   regI++)
         {
         fixupRegistrationID(*regI->second);
         }
      }
   }
// ------------------------------------------------------------------
// fixup registration ID for the specified registration
// ------------------------------------------------------------------
void Coordinator::fixupRegistrationID(PMRegistrationItem& registrationItem)
   {
   // convert all component names to ID's
   if (registrationItem.destID == 0 && registrationItem.componentName != "")
      {
      unsigned id = componentNameToID(registrationItem.componentName);
      if (id != INT_MAX)
         registrationItem.destID = id;
      else
         {
         string msg = "Cannot find a component called: " + registrationItem.componentName;
         error(msg.c_str(), true);
         registrationItem.name = registrationItem.componentName + "." + registrationItem.name;
         registrationItem.destID = 0;
         }
      }
   }
// ------------------------------------------------------------------
// apsim hack to poll modules for gettable variables.  This is because we haven't
// yet got all the .interface files up to date.
// ------------------------------------------------------------------
void Coordinator::pollComponentsForGetVariable(PMRegistrationItem& registrationItem)
   {
   string lowerName = registrationItem.getName();
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
void Coordinator::pollComponentsForSetVariable(PMRegistrationItem& registrationItem,
                                               unsigned fromID,
                                               unsigned ourRegID,
                                               protocol::Variant& variant)
   {
   static unsigned lastModuleID = 0;

   // try the last responding module first
   if (lastModuleID != NULL)
      sendMessage(newApsimSetQueryMessage(componentID,
                                          lastModuleID,
                                          registrationItem.getName().c_str(),
                                          fromID,
                                          ourRegID,
                                          variant));

   // if we still don't have any registrations then loop through all modules.
   if (registrationItem.interestedItems.size() == 0)
      {
      for (Components::iterator i = components.begin();
                                i != components.end();
                                i++)
         {
         sendMessage(newApsimSetQueryMessage(componentID,
                                             i->second->ID,
                                             registrationItem.getName().c_str(),
                                             fromID,
                                             ourRegID,
                                             variant));
         if (registrationItem.interestedItems.size() != 0)
            {
            lastModuleID = i->second->ID;
            return;
            }
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
void Coordinator::publishEventsInOrder(unsigned int fromID,
                                       PublishEventData& publishEventData,
                                       PMRegistrationItem* registrationItem)
   {
   bool SentToOrderedComponents = false;

   // Loop through all the subscribed components.  Those that aren't in the list,
   // send the event as normal.  The first time we strike a component that IS
   // in the list then send the event to all the components in the componentOrder
   // list in one hit.  Then send the event to the remaining components.
   for (PMRegistrationItem::InterestedItems::iterator
                                  interestI = registrationItem->interestedItems.begin();
                                  interestI != registrationItem->interestedItems.end();
                                  interestI++)
      {
      std::vector<unsigned>::iterator componentOrderI
         = find(componentOrders.begin(), componentOrders.end(), (*interestI)->componentID);
      if (componentOrderI == componentOrders.end())
         sendMessage(newEventMessage(componentID,
                                     (*interestI)->componentID,
                                     (*interestI)->registrationID,
                                     fromID,
                                     publishEventData.variant));
      else if (!SentToOrderedComponents)
         {
         SentToOrderedComponents = true;
         for (unsigned i = 0; i != componentOrders.size(); ++i)
            {
            // find the registration that matches this component and send
            // the event to it.
            for (PMRegistrationItem::InterestedItems::iterator
                                           item = registrationItem->interestedItems.begin();
                                           item != registrationItem->interestedItems.end();
                                           ++item)
               {
               if ((*item)->componentID == componentOrders[i])
                  {
                  sendMessage(newEventMessage(componentID,
                                              (*item)->componentID,
                                              (*item)->registrationID,
                                              fromID,
                                              publishEventData.variant));
                  break;
                  }
               }
            }
         }
      }
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
      if (kindString == "getVariableReg")
         {
         kind = getVariableReg;
         oppositeKind = respondToGetReg;
         }
      else if (kindString == "setVariableReg")
         {
         kind = setVariableReg;
         oppositeKind = respondToSetReg;
         }
      else if (kindString == "methodCallReg")
         {
         kind = methodCallReg;
         oppositeKind = respondToMethodCallReg;
         }
      else if (kindString == "eventReg")
         {
         kind = eventReg;
         oppositeKind = respondToEventReg;
         }
      else if (kindString == "respondToGetReg")
         {
         kind = respondToGetReg;
         oppositeKind = getVariableReg;
         }
      else if (kindString == "respondToSetReg")
         {
         kind = respondToSetReg;
         oppositeKind = setVariableReg;
         }
      else if (kindString == "respondToMethodCallReg")
         {
         kind = respondToMethodCallReg;
         oppositeKind = methodCallReg;
         }
      else if (kindString == "respondToEventReg")
         {
         kind = respondToEventReg;
         oppositeKind = eventReg;
         }

      ApsimDataTypeData dataType = componentData->getDataType(reg->getDataTypeName());
      unsigned id = addRegistration(kind, reg->getName().c_str(),
                                    dataType.getTypeString().c_str(),
                                    reg->getAlias().c_str());

      addInternalRegistration(parentID,
                              oppositeKind,
                              reg->getName(),
                              dataType.getTypeString().c_str(),
                              id);
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


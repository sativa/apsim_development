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
   // If this is the GOD PM then add a ComponentAlias for ourselves.
   // This is because we sometimes send messages (e.g error)to our 'parent' PM
   // which is ourself.
   try
      {
      if (componentID == parentID)
         components.insert(Components::value_type(componentID,
               new ComponentAlias("MasterPM", componentID)));

      Component::doInit1(sdml);

      // cast the componentData to a systemData.
      string sdmlString(sdml.f_str(), sdml.length());
      ApsimSimulationFile simulationData(sdmlString, true);

      // read in title and register a respondToGet
      static const char* stringDDML = "<type kind=\"string\"\\>";
      static const char* stringArrayDDML = "<type kind=\"string\" array=\"T\"\\>";

      title = simulationData.getTitle();
      titleID = addRegistration(respondToGetReg, "title", stringDDML);
      componentsID = addRegistration(respondToGetReg, "components", stringArrayDDML);

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
   resolveRegistrations();
   afterInit2 = true;

   // initialise all components.
   for (Components::iterator componentI = components.begin();
                             componentI != components.end();
                             componentI++)
      {
      if (componentI->second->ID != 0)
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
          parentID);
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
      if (posPeriod != string::npos)
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
         sendMessage(newEventMessage(componentID,
                                     (*interestI)->componentID,
                                     (*interestI)->registrationID,
                                     fromID,
                                     publishEventData.variant));
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
//  Short description:
//    handle incoming getValue messages.

//  Notes:

//  Changes:
//    dph 15/5/2001

// ------------------------------------------------------------------
void Coordinator::onGetValueMessage(unsigned int fromID, GetValueData& getValueData)
   {
   ComponentAlias::Registrations& registrations = *components[fromID]->getRegistrationsForKind(protocol::getVariableReg);

   // See if we have a registration.
   ComponentAlias::Registrations::iterator i = registrations.find(getValueData.ID);
   if (i == registrations.end())
      {
      char msg[300];
      strcpy(msg, "A component has requested the value of a variable that hasn't\n"
                  "been registered.\nRequesting component:");
      strcat(msg, components[fromID]->getName().c_str());
      strcat(msg, "\nGetValue ID:");
      itoa(getValueData.ID, &msg[strlen(msg)], 10);
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
                                          fromID,
                                          getValueData.ID));
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
//  Short description:
//    handle the incoming requestSetValue message.

//  Changes:
//    dph 15/5/2001
// ------------------------------------------------------------------
void Coordinator::onRequestSetValueMessage(unsigned int fromID,
                                           RequestSetValueData& setValueData)
   {
   ComponentAlias::Registrations& registrations = *components[fromID]->getRegistrationsForKind(protocol::setVariableReg);
   PMRegistrationItem& registrationItem = *registrations[setValueData.ID];

   // apsim hack to poll modules for variables.  This is because we haven't
   // yet got all the .interface files up to date.
   if (registrationItem.interestedItems.size() == 0)
      pollComponentsForSetVariable(registrationItem, fromID, setValueData);

   else if (registrationItem.interestedItems.size() == 1)
      sendMessage(newQuerySetValueMessage(componentID,
                                          registrationItem.interestedItems[0]->componentID,
                                          registrationItem.interestedItems[0]->registrationID,
                                          fromID,
                                          setValueData.ID,
                                          setValueData.variant));
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
      for (unsigned c = 0; c < components.size(); c++)
         {
         ComponentAlias::Registrations* registrations
            = components[c]->getRegistrationsForKind(getOppositeType(reg->type));
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
   }
// ------------------------------------------------------------------
// Fixup all registration destID's.
// ------------------------------------------------------------------
void Coordinator::fixupRegistrationIDs(const protocol::RegistrationType& type)
   {
   for (unsigned c = 0; c < components.size(); c++)
      {
      ComponentAlias::Registrations* registrations
         = components[c]->getRegistrationsForKind(type);
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
                                               RequestSetValueData& setValueData)
   {
   static unsigned lastModuleID = 0;

   // try the last responding module first
   if (lastModuleID != NULL)
      sendMessage(newApsimSetQueryMessage(componentID,
                                          lastModuleID,
                                          registrationItem.getName().c_str(),
                                          fromID,
                                          setValueData.ID,
                                          setValueData.variant));

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
                                             setValueData.ID,
                                             setValueData.variant));
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


#include <general\pch.h>
#pragma hdrstop

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
      ApsimSimulationFile simulationData;
      string sdmlString(sdml.f_str(), sdml.length());
      simulationData.readXML(sdmlString);

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
         ApsimServiceData component = simulationData.getService(*componentI);
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
         ApsimServiceData system = simulationData.getService(*systemI);
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
      PMRegistrationItem* regItem = new PMRegistrationItem(asString(registerData.name),
                                                           registerData.destID,
                                                           registerData.kind,
                                                           fromID,
                                                           registerData.ID);
      registrations->insert(ComponentAlias::Registrations::value_type(registerData.ID, regItem));
      if (afterInit2)
         resolveRegistration(regItem, afterInit2);
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
//  Short description:
//    handle incoming publish event messages.

//  Notes:

//  Changes:
//    dph 15/5/2001

// ------------------------------------------------------------------
void Coordinator::onPublishEventMessage(unsigned int fromID, PublishEventData& publishEventData)
   {
   ComponentAlias::Registrations& registrations = *components[fromID]->getRegistrationsForKind(protocol::eventReg);
   PMRegistrationItem* registrationItem = registrations[publishEventData.ID];
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
      unsigned int componentID = componentNameToID(asString(queryInfo.name));
      string fqn = name;
      fqn += ".";
      fqn += asString(queryInfo.name);
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
   if (registrationItem.interestedItems.size() == 1)
      sendMessage(newQuerySetValueMessage(componentID,
                                          registrationItem.interestedItems[0]->componentID,
                                          registrationItem.interestedItems[0]->registrationID,
                                          fromID,
                                          setValueData.ID,
                                          setValueData.variant));
   }

// ------------------------------------------------------------------
//  Short description:
//    go find a specific registration for the specified component.

//  Changes:
//    dph 15/5/2001
// ------------------------------------------------------------------
PMRegistrationItem* Coordinator::findRegistration(unsigned int componentID,
                                                  const std::string& name,
                                                  RegistrationType kind)
   {
   ComponentAlias::Registrations& registrations = *components[componentID]->getRegistrationsForKind(kind);
   for (ComponentAlias::Registrations::iterator reg = registrations.begin();
                                                reg != registrations.end();
                                                reg++)
      {
      if (Str_i_Eq(reg->second->name, name))
         return reg->second;
      }
   return NULL;
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
   std::vector<PMRegistrationItem*> regs;
   findRegistrations(name, 0, kind, regs);
   if (regs.size() == 0)
      return NULL;
   else if (regs.size() == 1)
      return regs[0];
   else
      {
      string msg = "Non unique registration for " + name;
      ::MessageBox(NULL, msg.c_str(), "Error", MB_ICONSTOP | MB_OK);
      return NULL;
      }
   }
// ------------------------------------------------------------------
//  Short description:
//    go find all registrations that matches the specified name
//    and type.  It searches all components.  All registrations are
//    returned in 'regs'

//  Changes:
//    dph 15/5/2001
// ------------------------------------------------------------------
void Coordinator::findRegistrations(const std::string& name,
                                    unsigned int destID,
                                    RegistrationType type,
                                    std::vector<PMRegistrationItem*>& regs)
   {
   // This may be a fully qualified name,
   // either componentID.name OR componentName.name
   string regName = name;
   if (destID == 0)
      {
      unsigned int posPeriod = name.find(".");
      if (posPeriod != string::npos)
         destID = componentNameToID(name.substr(0, posPeriod));
      if (destID == INT_MAX)
         destID = 0;
      else
         regName = name.substr(posPeriod+1);
      }
   if (destID != 0)
      {
      PMRegistrationItem* reg = findRegistration(destID, regName, type);
      if (reg != NULL)
         {
         regs.push_back(reg);
         return;
         }
      }

   // If we've got this far, then we can just just look for the variable
   // without having to worry about a component prefix.
   for (unsigned componentID = 0; componentID < components.size(); componentID++)
      {
      PMRegistrationItem* reg = findRegistration(componentID, name, type);
      if (reg != NULL)
         regs.push_back(reg);
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
      resolveRegistration(regI->second, false);
   }

// ------------------------------------------------------------------
//  Short description:
//    Go resolve a specific registration for this system.

//  Notes:

//  Changes:
//    dph 15/5/2001

// ------------------------------------------------------------------
void Coordinator::resolveRegistration(PMRegistrationItem* regI, bool afterInit2)
   {
   // For a respondToEventReg, go find an eventReg.
   // For a getVariableReg, go find a respondToGetReg or a respondToGetSetReg
   // For a setVariableReg, go find a respondToSetReg or a respondToGetSetReg
   // For a methodCallReg, go find a respondToMethodCallReg
   if (regI->type == eventReg)
      findRegistrations(regI->name, regI->destID, respondToEventReg, regI->interestedItems);

   else if (regI->type == getVariableReg)
      findRegistrations(regI->name, regI->destID, respondToGetReg, regI->interestedItems);

   else if (regI->type == setVariableReg)
      findRegistrations(regI->name, regI->destID, respondToSetReg, regI->interestedItems);

   else if (regI->type == respondToEventReg)
      {
      std::vector<PMRegistrationItem*> registrations;
      findRegistrations(regI->name, 0, eventReg, registrations);
      for (std::vector<PMRegistrationItem*>::iterator reg = registrations.begin();
                                                 reg != registrations.end();
                                                 reg++)
         (*reg)->interestedItems.push_back(regI);
      }
   else if (regI->type == respondToGetReg)
      {
      std::vector<PMRegistrationItem*> registrations;
      findRegistrations(regI->name, 0, getVariableReg, registrations);
      for (std::vector<PMRegistrationItem*>::iterator reg = registrations.begin();
                                                 reg != registrations.end();
                                                 reg++)
         (*reg)->interestedItems.push_back(regI);
      }
   else if (regI->type == respondToSetReg)
      {
      std::vector<PMRegistrationItem*> registrations;
      findRegistrations(regI->name, 0, setVariableReg, registrations);
      for (std::vector<PMRegistrationItem*>::iterator reg = registrations.begin();
                                                 reg != registrations.end();
                                                 reg++)
         (*reg)->interestedItems.push_back(regI);
      }
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


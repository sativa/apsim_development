//---------------------------------------------------------------------------
#include "Coordinator.h"
#include "InterfaceLayer.h"
#include "message.h"
#include <assert.h>
#include <general\stl_functions.h>
#include <sstream>
#include <list>
#include <functional>
using namespace std;

HANDLE DLLHandle;
void _stdcall (*RENAME_VARIABLE) (const char* oldName,
                                  const char* newName,
                                  unsigned int oldNameSize,
                                  unsigned int newNameSize) = NULL;

// ------------------------------------------------------------------
//  Short description:
//    Class for encapsulating a particular event and the components
//    that have registered an interest in it.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
class EventRegistration
   {
   public:
      EventRegistration(void) { };
      void registerComponent(PROTOCOLComponent* component)
         {registeredComponents.push_back(component);}
      void deregisterComponent(PROTOCOLComponent* component)
         {
         registeredComponents.erase(Pfind(registeredComponents.begin(),
                                          registeredComponents.end(),
                                          component));
         }
      void publishEvent(PROTOCOLEvent& anEvent)
         {
         for (ComponentSet::iterator i = registeredComponents.begin();
                                     i != registeredComponents.end();
                                     i++)
            {
            (*i)->inEvent(anEvent);
            }
         }

      // ------------------------------------------------------------------
      //  Short description:
      //     change the order of the specified components for this event
      //     registration

      //  Notes:
      //     Modules are swapped thus:
      //      e.g. if ComponentsToChange(1) = 'x',
      //              ComponentsToChange(2) = 'y',
      //              ComponentsToChange(3) = 'z'
      //         Then y will replace x, z will replace y, and x will replace z
      //         in the loader instantiation vector leaving: y, z, x

      //  Changes:
      //    DPH 5/2/2001

      // ------------------------------------------------------------------
      void changeComponentOrder(vector<string>& componentsToChange)
         {
         if (componentsToChange.size() > 0)
            {
            // work out the first component index - this is then the base component for
            // all swaps.
            ComponentSet::iterator baseComponentI
               = find_if(registeredComponents.begin(),
                         registeredComponents.end(),
                         PEqualToName<PROTOCOLComponent>(componentsToChange[0]));
            if (baseComponentI != registeredComponents.end())
               {
               // for all other components swap with the base component.
               for (unsigned int i = 1; i < componentsToChange.size(); i++)
                  {
                  ComponentSet::iterator swapComponentI
                     = find_if(registeredComponents.begin(),
                               registeredComponents.end(),
                               PEqualToName<PROTOCOLComponent>(componentsToChange[i]));
                  if (swapComponentI != registeredComponents.end())
                     {
                     ComponentList::iterator baseComponentIPlus1 = baseComponentI;
                     baseComponentIPlus1++;
                     baseComponentI = swap_ranges(baseComponentI, baseComponentIPlus1, swapComponentI);
                     baseComponentI--;
                     }
                  }
               }
            }
         }


   private:
      typedef list<PROTOCOLComponent*> ComponentSet;
      ComponentSet registeredComponents;
   };

// ------------------------------------------------------------------
//  Short description:
//    Class for encapsulating a particular variable and the components
//    that have registered an interest in it and exported it.

//  Notes:

//  Changes:
//    dph 6/3/2001

// ------------------------------------------------------------------
class VariableRegistration
   {
   public:
      VariableRegistration(PROTOCOLCoordinator* coord,
                           bool variablesend,
                           const string& realname)
         : coordinator(coord),
           variableSend(variablesend),
           realName(realname) { };

      bool getVariable(const FString& aliasName)
         {
         // If the variable is a variableReceive then go ask parent for variable.  If found
         bool ok;
         if (!variableSend)
            {
            ok = coordinator->parent->getVariable(FString(realName.c_str()));
            RENAME_VARIABLE(realName.c_str(), aliasName.f_str(),
                            realName.length(), aliasName.length());
            return true;
            }
         else
            return false;
         }
      bool isPublicVariable(void)
         {
         return variableSend;
         }
      string getRealName(void)
         {
         return realName;
         }
      bool setVariable(void)
         {
         // If the variable is a variableReceive then go ask parent to set the variable.  If found
         if (!variableSend)
            return coordinator->parent->setVariable(FString(realName.c_str()));
         else
            return false;
         }

   private:
      bool variableSend;
      string realName;
      PROTOCOLCoordinator* coordinator;
   };
// ------------------------------------------------------------------
//  Short description:
//    Class for encapsulating a particular method registration

//  Notes:

//  Changes:
//    dph 6/3/2001

// ------------------------------------------------------------------
class MethodRegistration
   {
   public:
      MethodRegistration(PROTOCOLCoordinator* coord,
                         const string& internalName)
         : coordinator(coord)
         {
         unsigned int posSpace = internalName.find(" ");
         if (posSpace == string::npos)
            throw string("Bad method name: " + internalName);
         componentName = internalName.substr(0, posSpace);
         action = internalName.substr(posSpace+1);
         };

      bool sendMessage(void)
         {
         return coordinator->sendMessage(FString(componentName.c_str()),
                                         PROTOCOLMessage(FString(action.c_str())));
         }

   private:
      string componentName;
      string action;
      PROTOCOLCoordinator* coordinator;
   };


// DPH - need to remove this - see coordinator::publishevent.
extern CallbackFunction<int>* TickCallback;

// ------------------------------------------------------------------
//  Short description:
//    Constructor

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
PROTOCOLCoordinator::PROTOCOLCoordinator(const string& name,
                                         ICoordinator* aParent,
                                         ISystemConfiguration* sysConfiguration)
   : PROTOCOLComponent(name, this), systemConfiguration(sysConfiguration)
   {
   parent = aParent;
   if (RENAME_VARIABLE == NULL)
      {
      DLLHandle = LoadLibrary ("apsinfra.dll");
      if (DLLHandle != NULL)
         (FARPROC) RENAME_VARIABLE = GetProcAddress (DLLHandle, "rename_variable");
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    destructor

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
PROTOCOLCoordinator::~PROTOCOLCoordinator(void)
   {
   FreeLibrary(DLLHandle);
   delete systemConfiguration;
   while (!components.empty())
      {
		IComponent* Ptr = components.back();
      components.pop_back();
		delete Ptr;
		}
   for (EventRegistrationList::iterator i = eventRegistrations.begin();
                                        i != eventRegistrations.end();
                                        i++)
      {
		EventRegistration* Ptr = (*i).second;
		delete Ptr;
		}
   for (MethodRegistrationList::iterator i = methodRegistrations.begin();
                                         i != methodRegistrations.end();
                                         i++)
      {
		MethodRegistration* Ptr = (*i).second;
		delete Ptr;
		}
   for (VariableRegistrationList::iterator i = variableRegistrations.begin();
                                           i != variableRegistrations.end();
                                           i++)
      {
		VariableRegistration* Ptr = (*i).second;
		delete Ptr;
		}
   }
// ------------------------------------------------------------------
//  Short description:
//    initialise this coordinator.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void PROTOCOLCoordinator::create()
   {
   // read in all event and variable registrations.
   readEventRegistrations();
   readVariableRegistrations();
   readMethodRegistrations();

   // loop through all components specified in configuration and create
   // and add a component object to our list of components.
   list<string> componentNames;
   systemConfiguration->getComponentNames(componentNames);
   for (list<string>::iterator componentI = componentNames.begin();
                               componentI != componentNames.end();
                               componentI++)
      {
      string dllFileName;
      IComponentConfiguration* comp = systemConfiguration->getIComponent(*componentI);

      // get all configuration data for this component and pass to new component.
      std::ostringstream ssdl;
      comp->write(ssdl);
      addComponent(comp->getComponentName(), comp->getDllFilename(), ssdl.str());
      delete comp;
      }
   // loop through all systems specified in configuration and create
   // and add a component object to our list of components.
   list<string> systemNames;
   systemConfiguration->getSystemNames(systemNames);
   for (list<string>::iterator systemI = systemNames.begin();
                               systemI != systemNames.end();
                               systemI++)
      {
      string dllFileName;
      ISystemConfiguration* sim = systemConfiguration->getISystem(*systemI);

      addCoordinator(sim->getSystemName(), sim);
//      delete sim;
      }

   // initialise the previous component - used in sendMessageToFirst.
   previousComponent = components.begin();

   // if we are the top level coordinator then get the name of the sequencer.
   // DPH HACK - shouldn't hardwire CLOCK
   if (parent == NULL)
      sequencerName = "clock";
   }

// ------------------------------------------------------------------
//  Short description:
//    initialise this coordinator.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void PROTOCOLCoordinator::init()
   {
   PROTOCOLComponent::init();

   // initialise all components.
   for (ComponentList::iterator componentI = components.begin();
                                componentI != components.end();
                                componentI++)
      {
      (*componentI)->init();
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    terminate this coordinator.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void PROTOCOLCoordinator::term(void)
   {
   for (ComponentList::iterator componentI = components.begin();
                                componentI != components.end();
                                componentI++)
      {
      (*componentI)->term();
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     return true if component exists in system.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
bool PROTOCOLCoordinator::existsComponent(const FString& componentName)
   {
   return (componentNameToAddress(componentName).address != NULL);
   }

// ------------------------------------------------------------------
//  Short description:
//    Send a message to the specifeid address.  The address passed in
//    is resolved to a component pointer.

//  Notes:
//     Address resolution algorithm:
//        IF first "part" of address is a component name AND
//           there are more parts of address THEN pass
//           remainder of address to that component's  sendMsg method
//        IF first "part" of address is a component name AND
//           there are no more parts of address THEN call the
//           interface layer to deliver the message.
//        IF first "part" of address is not a component THEN pass
//           full address to parent coordinator's sendMsg method
//
//    Address "parts" are separated by periods

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
bool PROTOCOLCoordinator::sendMessage(const FString& componentName,
                                      PROTOCOLMessage& aMsg) const
   {
   PROTOCOLTransportAddress address = componentNameToAddress(componentName);

   // If we do have a component with specified name then deliver
   // message.
   if (address.address == NULL)
      return false;
   else
      IL->sendMessage(address, aMsg);
   return true;
   }

// ------------------------------------------------------------------
//  Short description:
//    Send start message to sequencer

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void PROTOCOLCoordinator::start(void)
   {
   // should only be called as a top level PM
   assert (parent == NULL);

   // initialise all components.
   init();
   sendMessage(sequencerName.c_str(), PROTOCOLMessage("start"));
   }
// ------------------------------------------------------------------
//  Short description:
//    Send finish message to sequencer

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void PROTOCOLCoordinator::finish(void)
   {
   // should only be called as a top level PM
   assert (parent == NULL);
   sendMessage(sequencerName.c_str(), PROTOCOLMessage("finish"));
   }

// ------------------------------------------------------------------
//  Short description:
//    distribute the specified event to all interested components

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void PROTOCOLCoordinator::distribute(PROTOCOLEvent& Event)
   {
   }

// ------------------------------------------------------------------
//  Short description:
//     Broadcast a message to all components.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void PROTOCOLCoordinator::broadcastMessage(PROTOCOLMessage& event) const
   {
   for (ComponentList::const_iterator componentI = components.begin();
                                      componentI != components.end();
                                      componentI++)
      {
      // If the current component is a coordinator then call it's
      // broadcastmessage method.  If not then send message straight
      // to component.
      ICoordinator* coordinator = dynamic_cast<ICoordinator*> (*componentI);
      if (coordinator != NULL)
         coordinator->broadcastMessage(event);

      else
         IL->sendMessage(PROTOCOLTransportAddress(*componentI), event);
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     send a message to first component that responds.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
bool PROTOCOLCoordinator::sendMessageToFirst(PROTOCOLMessage& event)
   {
   setComponentResponded(false);
   ComponentList::iterator initialComponent = previousComponent;
   do
      {
      IL->sendMessage(PROTOCOLTransportAddress(*previousComponent), event);
      if (!getComponentResponded())
         {
         previousComponent++;
         if (previousComponent == components.end())
            previousComponent = components.begin();
         }
      }
   while (previousComponent != initialComponent &&
          !getComponentResponded());
   return getComponentResponded();
   }

// ------------------------------------------------------------------
//  Short description:
//     publish an event.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void PROTOCOLCoordinator::publishEvent(PROTOCOLEvent& event) const
   {
   // dph - this code has to go.  Summary needs the current date.
   if (event.action == "tick" && TickCallback != NULL)
      TickCallback->callback(1);

//   broadcastMessage(event);
   // go locate the appropriate event registration object.
   // If found then call its publishevent method.
   // If not found then assume noone has registered an interest in it.
   for (EventRegistrationList::const_iterator eventRegI = eventRegistrations.begin();
                                              eventRegI != eventRegistrations.end();
                                              eventRegI++)
      {
      if (event.action == (*eventRegI).first.c_str())
         {
         ((*eventRegI).second)->publishEvent(event);
         break;
         }
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    convert a component name into an address.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
PROTOCOLTransportAddress PROTOCOLCoordinator::componentNameToAddress
   (const FString& componentName) const
   {
   ComponentList::const_iterator componentI = components.begin();
   while (componentI != components.end() &&
          componentName != (*componentI)->getName().c_str())
      componentI++;

   if (componentI != components.end())
      return PROTOCOLTransportAddress(*componentI);
   else
      return PROTOCOLTransportAddress();
   }

// ------------------------------------------------------------------
//  Short description:
//    Add a component to this coordinator (system)

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
PROTOCOLComponent* PROTOCOLCoordinator::addComponent(const string& name,
                                                     const string& dllFileName,
                                                     const string& ssdl)
   {
   PROTOCOLComponent* component = new PROTOCOLComponent(name, this, dllFileName, ssdl);
   components.push_back(component);
   component->create();

   return component;
   }

// ------------------------------------------------------------------
//  Short description:
//    Delete a component from this coordinator (system)

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void PROTOCOLCoordinator::deleteComponent(const string& name)
   {
   ComponentList::iterator componentI = find_if(components.begin(),
                                                components.end(),
                                                PEqualToName<IComponent>(name));

   if (componentI != components.end())
      {
      // delete object.
      delete *componentI;

      // remove from our list.
      components.erase(componentI);
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    Add a child coordinator (system to this coordinator (system)

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
PROTOCOLComponent* PROTOCOLCoordinator::addCoordinator(const string& name,
                                                       ISystemConfiguration* sysConfiguration)
   {
   PROTOCOLCoordinator* coordinator = new PROTOCOLCoordinator(name, this, sysConfiguration);
   components.push_back(coordinator);
   coordinator->create();

   return coordinator;
   }

// ------------------------------------------------------------------
//  Short description:
//     enumerate through all child components and call the specified function

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void PROTOCOLCoordinator::enumerateComponents
   (ConstCallbackFunction<const IComponent*>& f) const
   {
   for (ComponentList::const_iterator compI = components.begin();
                                      compI != components.end();
                                      compI++)
      f.callback(*compI);
   }

// ------------------------------------------------------------------
//  Short description:
//     register an interest in an event.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void PROTOCOLCoordinator::registerSubscribedEvent(FString& eventName,
                                                  FString& componentName)
   {
   // go locate a pre-existing event registration.  If found then add this
   // component to it.  If not found then create a new event registration
   // and then add component to it.
   EventRegistrationList::iterator eventRegI
      = eventRegistrations.find(eventName.asString());
   EventRegistration* ptr;
   if (eventRegI == eventRegistrations.end())
      {
      ptr = new EventRegistration;
      eventRegistrations.insert(
         EventRegistrationList::value_type(eventName.asString(), ptr));
      }
   else
      ptr = (*eventRegI).second;

   // go locate component.
   ComponentList::iterator componentI = find_if(components.begin(),
                                                components.end(),
                                                PEqualToName<IComponent>(componentName.asString()));

   if (componentI == components.end())
      {
      string msg = "Cannot register event: ";
      msg += eventName.asString();
      msg += ".  Component: ";
      msg += componentName.asString();
      msg += " not found.";
      throw msg;
      }

   ptr->registerComponent(*componentI);
   }

// ------------------------------------------------------------------
//  Short description:
//     change the order of the specified components.

//  Notes:
//     Modules are swapped thus:
//      e.g. if ComponentsToChange(1) = 'x',
//              ComponentsToChange(2) = 'y',
//              ComponentsToChange(3) = 'z'
//         Then y will replace x, z will replace y, and x will replace z
//         in the loader instantiation vector leaving: y, z, x

//  Changes:
//    DPH 4/10/99
//    dph 18/1/01 moved from the old APSIMLoader.cpp to coordinator.cpp

// ------------------------------------------------------------------
void PROTOCOLCoordinator::changeComponentOrder(vector<string>& componentsToChange)
   {
//   string out;
//   for (ComponentList::iterator i = components.begin(); i != components.end(); i++)
//      out += (*i)->getName();
   if (componentsToChange.size() > 0)
      {
      // work out the first component index - this is then the base component for
      // all swaps.
      ComponentList::iterator baseComponentI
         = find_if(components.begin(),
                   components.end(),
                   PEqualToName<PROTOCOLComponent>(componentsToChange[0]));
      if (baseComponentI == components.end())
         throw string("The APSIM infrastructure cannot find component: "
                      + componentsToChange[0] + ".\n"
                      "Routine name: ChangeComponentOrder");

      // for all other components swap with the base component.
      for (unsigned int i = 1; i < componentsToChange.size(); i++)
         {
         ComponentList::iterator swapComponentI
            = find_if(components.begin(),
                      components.end(),
                      PEqualToName<PROTOCOLComponent>(componentsToChange[i]));
         if (swapComponentI == components.end())
            throw string("The APSIM infrastructure cannot find component: "
                         + componentsToChange[i] + ".\n"
                         "Routine name: ChangeComponentOrder");

         ComponentList::iterator baseComponentIPlus1 = baseComponentI;
         baseComponentIPlus1++;
         baseComponentI = swap_ranges(baseComponentI, baseComponentIPlus1, swapComponentI);
         baseComponentI--;
         }
      }
   // initialise the previous component - used in sendMessageToFirst.
   previousComponent = components.begin();

   // need to also re-order the event registrations.
   for (EventRegistrationList::iterator i = eventRegistrations.begin();
                                        i != eventRegistrations.end();
                                        i++)
      {
		EventRegistration* Ptr = (*i).second;
      Ptr->changeComponentOrder(componentsToChange);
      }

//   string out2;
//   for (ComponentList::iterator i = components.begin(); i != components.end(); i++)
//      out2 += (*i)->getName();

   }

// ------------------------------------------------------------------
//  Short description:
//     read all event registrations for this Coordinator.

//  Notes:

//  Changes:
//    dph 6/3/2001

// ------------------------------------------------------------------
void PROTOCOLCoordinator::readEventRegistrations(void)
   {
   list<string> subscribedEventNames;
   systemConfiguration->getEventReceives(subscribedEventNames);
   for (list<string>::iterator subNameI = subscribedEventNames.begin();
                               subNameI != subscribedEventNames.end();
                               subNameI++)
      {
      parent->registerSubscribedEvent(FString((*subNameI).c_str()),
                                      FString(getName().c_str()));
      }
   }
// ------------------------------------------------------------------
//  Short description:
//     read all variable registrations for this Coordinator.

//  Notes:

//  Changes:
//    dph 6/3/2001

// ------------------------------------------------------------------
void PROTOCOLCoordinator::readVariableRegistrations(void)
   {
   // read in all variable sends.
   list<string> variableSendNames, variableSendAliases;
   systemConfiguration->getVariableSends(variableSendNames, variableSendAliases);
   for (list<string>::iterator nameI = variableSendNames.begin(),
                               aliasI = variableSendAliases.begin();
                               nameI != variableSendNames.end();
                               nameI++,aliasI++)
      {
      variableRegistrations.insert(VariableRegistrationList::value_type
         (*aliasI, new VariableRegistration(this, true, *nameI)));
      }

   // read in all variable receives.
   list<string> variableReceiveNames, variableReceiveAliases;
   systemConfiguration->getVariableReceives(variableReceiveNames, variableReceiveAliases);
   for (list<string>::iterator nameI = variableReceiveNames.begin(),
                               aliasI = variableReceiveAliases.begin();
                               nameI != variableReceiveNames.end();
                               nameI++,aliasI++)
      {
      variableRegistrations.insert(VariableRegistrationList::value_type
         (*aliasI, new VariableRegistration(this, false, *nameI)));
      }
   }
// ------------------------------------------------------------------
//  Short description:
//     read all method registrations for this Coordinator.

//  Notes:

//  Changes:
//    dph 6/3/2001

// ------------------------------------------------------------------
void PROTOCOLCoordinator::readMethodRegistrations(void)
   {
   // read in all variable sends.
   list<string> methodReceiveNames, methodReceiveAliases;
   systemConfiguration->getMethodReceives(methodReceiveNames, methodReceiveAliases);
   for (list<string>::iterator nameI = methodReceiveNames.begin(),
                               aliasI = methodReceiveAliases.begin();
                               nameI != methodReceiveNames.end();
                               nameI++,aliasI++)
      {
      methodRegistrations.insert(MethodRegistrationList::value_type
         (*aliasI, new MethodRegistration(this, *nameI)));
      }
   }
// ------------------------------------------------------------------
//  Short description:
//     retrieve a variable from the system.

//  Notes:

//  Changes:
//    dph 6/3/2001

// ------------------------------------------------------------------
bool PROTOCOLCoordinator::getVariable(const FString& variableName)
   {
   // try and locate our variable in the variable registration list.  If found
   // then tell the registration to get the variable.
   VariableRegistrationList::const_iterator varI
      = variableRegistrations.find(string(variableName.asString()));
   if (varI != variableRegistrations.end())
      {
      if ((*varI).second->getVariable(variableName))
         return true;
      }

   // NOT FOUND in Variable registration list - go ask APSIM
   unsigned posArray = variableName.find('(');
   try
      {
      if (posArray != std::string::npos)
         return sendMessageToFirst
            (PROTOCOLMessage("get", variableName.substr(0, posArray-1)));
      else
         return sendMessageToFirst
         (PROTOCOLMessage("get", variableName));
      }
   catch (std::string& msg)
      {
      return false;
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     retrieve a variable from a specific component.

//  Notes:

//  Changes:
//    dph 6/3/2001

// ------------------------------------------------------------------
bool PROTOCOLCoordinator::getVariable(const FString& componentName,
                                      const FString& variableName)
   {
   try
      {
      setComponentResponded(false);
      unsigned posArray = variableName.find('(');
      bool ok;
      if (posArray != std::string::npos)
         ok = sendMessage
            (componentName, PROTOCOLMessage("get", variableName.substr(0, posArray)));
      else
         ok = sendMessage
            (componentName, PROTOCOLMessage("get", variableName));
      return (ok && getComponentResponded());
      }
   catch (std::string& msg)
      {
      return false;
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     set a variable in a given component.

//  Notes:

//  Changes:
//    dph 6/3/2001

// ------------------------------------------------------------------
bool PROTOCOLCoordinator::setVariable(const FString& variableName)
   {
   // try and locate our variable in the variable registration list.  If found
   // then tell the registration to set the variable.
   VariableRegistrationList::const_iterator varI
      = variableRegistrations.find(string(variableName.asString()));
   if (varI != variableRegistrations.end())
      {
      if ((*varI).second->setVariable())
         return true;
      }

   return sendMessageToFirst(PROTOCOLMessage("set", variableName));
   }

// ------------------------------------------------------------------
//  Short description:
//     retrieve a variable from this system only.  Return true if found.

//  Notes:

//  Changes:
//    dph 6/3/2001

// ------------------------------------------------------------------
bool PROTOCOLCoordinator::getSystemVariable(const FString& variableName)
   {
   // make sure this variable is a public variable.
   VariableRegistrationList::const_iterator varI
      = variableRegistrations.find(string(variableName.asString()));
   if (varI != variableRegistrations.end() && (*varI).second->isPublicVariable())
      {
      // Go locate variable - go ask APSIM
      string realName = (*varI).second->getRealName();
      unsigned posArray = realName.find('(');
      try
         {
         bool ok;
         if (posArray != std::string::npos)
            ok = sendMessageToFirst(PROTOCOLMessage("get", realName.substr(0, posArray-1).c_str()));
         else
            ok = sendMessageToFirst(PROTOCOLMessage("get", realName.c_str()));
         if (ok)
            RENAME_VARIABLE(realName.c_str(), variableName.f_str(),
                            realName.length(), variableName.length());
         return ok;
         }
      catch (std::string& msg)
         {
         return false;
         }
      }
   else
      return false;
   }
// ------------------------------------------------------------------
//  Short description:
//     set a variable in this system

//  Notes:

//  Changes:
//    dph 6/3/2001

// ------------------------------------------------------------------
bool PROTOCOLCoordinator::setSystemVariable(const FString& variableName)
   {
   // make sure this variable is a public variable.
   VariableRegistrationList::const_iterator varI
      = variableRegistrations.find(string(variableName.asString()));
   if (varI != variableRegistrations.end() && (*varI).second->isPublicVariable())
      {
      string realName = (*varI).second->getRealName();
      RENAME_VARIABLE(variableName.f_str(), realName.c_str(),
                      variableName.length(), realName.length());

      return sendMessageToFirst(PROTOCOLMessage("set", realName.c_str()));
      }
   else
      return false;
   }
// ------------------------------------------------------------------
//  Short description:
//     method call comming in from another system

//  Notes:

//  Changes:
//    dph 6/3/2001

// ------------------------------------------------------------------
bool PROTOCOLCoordinator::doSystemMessage(PROTOCOLMessage& Message)
   {
   // try and locate our variable in the variable registration list.  If found
   // then tell the registration to set the variable.
   MethodRegistrationList::const_iterator methodI
      = methodRegistrations.find(string(Message.action.asString()));
   if (methodI != methodRegistrations.end())
      return (*methodI).second->sendMessage();
   else
      return false;
   }


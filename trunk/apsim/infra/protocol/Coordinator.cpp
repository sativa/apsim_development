//---------------------------------------------------------------------------
#include "Coordinator.h"
#include "InterfaceLayer.h"
#include "message.h"
#include <assert.h>
#include <general\stl_functions.h>
#include <sstream>
#include <set>
#include <functional>
using namespace std;

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
         {registeredComponents.insert(component);}
      void deregisterComponent(PROTOCOLComponent* component)
         {registeredComponents.erase(component);}
      void publishEvent(PROTOCOLEvent& anEvent)
         {
         for (ComponentSet::iterator i = registeredComponents.begin();
                                     i != registeredComponents.end();
                                     i++)
            {
            (*i)->getComputation()->inEvent(anEvent);
            }
         }

   private:
      typedef set<PROTOCOLComponent*, Pless<PROTOCOLComponent> > ComponentSet;
      ComponentSet registeredComponents;
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
   while (!components.empty())
      {
		IComponent* Ptr = components.back();
      components.pop_back();
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
void PROTOCOLCoordinator::init()
   {
   PROTOCOLComponent::init();

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
      addComponent(comp->getName(), comp->getDllFilename(), ssdl.str());
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
      ISystemConfiguration* sim = systemConfiguration->getISystem(name);

      addCoordinator(sim->getName(), sim);
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
   for (ComponentList::iterator componentI = components.begin();
                                componentI != components.end();
                                componentI++)
      {
      (*componentI)->init();
      }

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
   componentResponded = false;
   ComponentList::iterator initialComponent = previousComponent;
   do
      {
      IL->sendMessage(PROTOCOLTransportAddress(*previousComponent), event);
      if (!componentResponded)
         {
         previousComponent++;
         if (previousComponent == components.end())
            previousComponent = components.begin();
         }
      }
   while (previousComponent != initialComponent &&
          !componentResponded);
   return componentResponded;
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

   broadcastMessage(event);
   // go locate the appropriate event registration object.
   // If found then call its publishevent method.
   // If not found then assume noone has registered an interest in it.
//   EventRegistrationList::const_iterator eventRegI
//      = eventRegistrations.find(event.action.c_str());
//   if (eventRegI != eventRegistrations.end())
//      {
//      EventRegistration* ptr = (*eventRegI).second;
//      ptr->publishEvent(event);
//      }
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
      = eventRegistrations.find(eventName.c_str());
   EventRegistration* ptr;
   if (eventRegI == eventRegistrations.end())
      {
      ptr = new EventRegistration;
      eventRegistrations.insert(
         EventRegistrationList::value_type(eventName.c_str(), ptr));
      }
   else
      ptr = (*eventRegI).second;

   // go locate component.
   ComponentList::iterator componentI = find_if(components.begin(),
                                                components.end(),
                                                PEqualToName<IComponent>(componentName.c_str()));

   if (componentI == components.end())
      {
      string msg = "Cannot register event: ";
      msg += eventName.c_str();
      msg += ".  Component: ";
      msg += componentName.c_str();
      msg += " not found.";
      throw msg;
      }

   ptr->registerComponent(*componentI);
   }


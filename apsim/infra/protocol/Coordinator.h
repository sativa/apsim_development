//---------------------------------------------------------------------------
#ifndef CoordinatorH
#define CoordinatorH
#include "interfaces.h"
#include "component.h"
#include <list>
#include <map>
#include <vector>
class EventRegistration;

typedef std::list<PROTOCOLComponent*> ComponentList;
typedef std::map<string, EventRegistration*> EventRegistrationList;
// ------------------------------------------------------------------
//  Short description:
//    This unit provides the functionality to manage a "system".
//    A "system" is a group of components.  It provides methods
//    to add/remove components, send messages and events between
//    components.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
class PROTOCOL_EXPORT PROTOCOLCoordinator : public PROTOCOLComponent,
                                            public ICoordinator
   {
   public:
      PROTOCOLCoordinator(const string& name,
                          ICoordinator* aParent,
                          ISystemConfiguration* sysConfiguration);
      ~PROTOCOLCoordinator(void);

      // initialise this coordinator given the specified configuration.
      virtual void init(void);

      // terminate this coordinator
      virtual void term(void);

      // return true if component exists in system.
      virtual bool existsComponent(const FString& componentName);

      // send a message to the specified address.  Return true
      // if message was delivered.
      virtual bool sendMessage(const FString& toAddress,
                               PROTOCOLMessage& aMsg) const;

      // broadcast a message to all components.
      virtual void broadcastMessage(PROTOCOLMessage& aMsg) const;

      // send a message to first component that responds.
      virtual bool sendMessageToFirst(PROTOCOLMessage& aMsg);

      // publish an event.
      virtual void publishEvent(PROTOCOLEvent& anEvent) const;

      // register an interest in an event.
      virtual void registerSubscribedEvent(FString& eventName,
                                           FString& componentName);

      // reorder the component's - used for competition.
      // will eventually be removed.
      void changeComponentOrder(std::vector<std::string>& componentsToChange);

      // start and end the simulation.
      virtual void start(void);
      virtual void finish(void);

      // enumerate through all child components and call the specified function
      virtual void enumerateComponents
         (ConstCallbackFunction<const IComponent*>& f) const;

   private:
      ComponentList components;
      ICoordinator* parent;
      ISystemConfiguration* systemConfiguration;
      string sequencerName;
      ComponentList::iterator previousComponent;

      EventRegistrationList eventRegistrations;

      void getComponentInfo(const string& name);
      void getComponentList(list<string>& names);
      virtual void distribute(PROTOCOLEvent& Event);

      PROTOCOLComponent* addComponent(const string& name,
                                      const string& dllFileName,
                                      const string& ssdl);
      PROTOCOLComponent* addCoordinator(const string& name,
                                        ISystemConfiguration* sysConfiguration);
      void deleteComponent(const string& name);
      PROTOCOLTransportAddress componentNameToAddress(const FString& componentName) const;

   };
#endif

//---------------------------------------------------------------------------
#ifndef EventInterfaceH
#define EventInterfaceH
#include "interfaces.h"
// ------------------------------------------------------------------
//  Short description:
//    Each component creates an instance of this class to enable
//    communication with the rest of the system.  This class
//    provides an API to send/receive/decode events.

//  Notes:

//  Changes:
//    dph 13/3/2000

// ------------------------------------------------------------------
class EventInterface
   {
   public:
      EventInterface(const IComputation* computation)
         {
         coordinator = computation->getComponent()->getCoordinator();
         componentName = computation->getComponent()->getName();
         }

      std::string getComponentName(void) {return componentName;}

      // return true if component exists in this system.
      // GRASP uses this but probably shouldn't
      bool existsComponent(const FString& componentName)
         {
         return coordinator->existsComponent(componentName);
         }

      // Send an action to a specified component.
      void sendAction(const FString& componentName,
                      const FString& actionName,
                      const FString& actionData)
         {
         if (!coordinator->sendMessage(componentName,
                                       PROTOCOLMessage(actionName, actionData)))
            {
            std::string msg = "Cannot deliver message to address: ";
            msg += componentName.asString();
            throw msg;
            }
         }

      // Broadcast an action to all components in the current system.
      void broadcastAction(const FString& actionName,
                           const FString& actionData)
         {
         coordinator->broadcastMessage(PROTOCOLMessage(actionName, actionData));
         }

      // register an interest in an event.
      void registerSubscribedEvent(FString& eventName)
         {
         coordinator->registerSubscribedEvent(eventName, componentName.c_str());
         }

      // Publish an event to the system.
      void publishEvent(const FString& eventName)
         {
         coordinator->publishEvent(PROTOCOLEvent(eventName));
         }
      bool getVariable(const FString& variableName)
         {
         return coordinator->getVariable(variableName);
         }
      bool getVariable(const FString& componentName, const FString& variableName)
         {
         return coordinator->getVariable(componentName, variableName);
         }
      bool setVariable(const FString& variableName)
         {
         return coordinator->setVariable(variableName);
         }

      void setComponentResponded(bool componentDidRespond)
         {
         coordinator->setComponentResponded(componentDidRespond);
         }
      bool componentResponded(void)
         {
         return coordinator->getComponentResponded();
         }

      // reorder the component's - used for competition.
      // will eventually be removed.
      void changeComponentOrder(std::vector<std::string>& componentsToChange)
         {
         coordinator->changeComponentOrder(componentsToChange);
         }


   private:
      std::string componentName;
      ICoordinator* coordinator;
   };

#endif

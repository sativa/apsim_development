//---------------------------------------------------------------------------
#ifndef CoordinatorH
#define CoordinatorH
#include <ComponentInterface\component.h>
#include "ComponentAlias.h"
#include <vector>
#include <map>
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
class Coordinator : public protocol::Component
   {
   public:
      Coordinator(void);
      ~Coordinator(void);

   private:
      typedef std::map<unsigned int, ComponentAlias*> Components;
      Components components;
      unsigned int sequencerID;
      unsigned int runningMessageID;
      unsigned int childComponentID;
      bool afterInit2;
      string title;
      unsigned titleID;
      unsigned componentsID;
      vector<unsigned> componentOrders;

      virtual void doInit1(const FString& sdml);
      virtual void doInit2(void);
      virtual void doCommence(void);
      virtual void onReturnComponentIDMessage(protocol::ReturnComponentIDData& data);
      virtual void onRegisterMessage(unsigned int fromID, protocol::RegisterData& registerData);
      virtual void onDeregisterMessage(unsigned int fromID, protocol::DeregisterData& registerData);
      virtual void onPublishEventMessage(unsigned int fromID, protocol::PublishEventData& publishEventData);
      virtual void onTerminateSimulationMessage(void);
      virtual void onGetValueMessage(unsigned int fromID, protocol::GetValueData& getValueData);
      virtual void onRequestComponentIDMessage(unsigned int fromID, protocol::RequestComponentIDData& data);
      virtual void onQueryInfoMessage(unsigned int fromID, unsigned int messageID, protocol::QueryInfoData& queryInfo);
      virtual void onRequestSetValueMessage(unsigned int fromID, protocol::RequestSetValueData& setValueData);
      virtual void onApsimChangeOrderData(protocol::MessageData& messageData);
      virtual void onQuerySetValueMessage(unsigned fromID, protocol::QuerySetValueData& querySetData);


      virtual void respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);
      virtual void respondToMethod(unsigned int& fromID, unsigned int& methodID, protocol::Variant& variant);
      virtual void respondToGet(unsigned int& fromID, protocol::QueryValueData& queryData);
      virtual bool respondToSet(unsigned int& fromID, protocol::QuerySetValueData& setValueData);

      void addComponent(const std::string& name,
                        const std::string& executable,
                        const std::string& sdml);
      unsigned int getComponentID(const std::string& name);

      // ------------------------------------------------------------------
      // Send queryValue messages to all subscribed components.  The toID
      // is used so that the receiving component can send a returnValue
      // message straight back to the originating module.  The fromID
      // can be different to the toID in a multi-paddock simulation where
      // the parent PM may be simply routing a queryValue message on behalf
      // of the originating component.
      // ------------------------------------------------------------------
      void sendQueryValueMessage(unsigned ourComponentID,
                                 unsigned foreignComponentID,
                                 unsigned ourRegID,
                                 unsigned foreignRegID);
      // ------------------------------------------------------------------
      // Send a querySetValueMessage
      // ------------------------------------------------------------------
      void sendQuerySetValueMessage(unsigned ourComponentID,
                                    unsigned foreignComponentID,
                                    unsigned ourRegID,
                                    unsigned foreignRegID,
                                    protocol::Variant& variant);

      PMRegistrationItem* findRegistration(const std::string& name,
                                           protocol::RegistrationType type);
      unsigned componentNameToID(const std::string& name);
      void resolveRegistrations(void);
      void resolveRegistrations(ComponentAlias::Registrations* registrations);
      void resolveRegistration(PMRegistrationItem* reg);
      void fixupRegistrationIDs(const protocol::RegistrationType& type);
      void fixupRegistrationID(PMRegistrationItem& registrationItem);
      void pollComponentsForGetVariable(PMRegistrationItem& registrationItem);
      void pollComponentsForSetVariable(PMRegistrationItem& registrationItem,
                                        unsigned fromID,
                                        unsigned ourRegID,
                                        protocol::Variant& variant);

      void publishEventsInOrder(unsigned int fromID,
                                protocol::PublishEventData& publishEventData,
                                PMRegistrationItem* registrationItem);

      // ------------------------------------------------------------------
      // Register a property for our child components
      // ------------------------------------------------------------------
      unsigned addInternalRegistration(unsigned fromID,
                                       protocol::RegistrationType kind,
                                       const std::string& name,
                                       const std::string& typeString,
                                       unsigned foreignID = 0);

      void readAllRegistrations(void);

   };
#endif

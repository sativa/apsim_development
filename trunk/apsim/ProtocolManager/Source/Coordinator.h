//---------------------------------------------------------------------------
#ifndef CoordinatorH
#define CoordinatorH
#include "component.h"
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
      std::vector<PMRegistrationItem> registrations;
      bool afterInit2;

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

      void addComponent(SOMComponent componentData);
      unsigned int getComponentID(const std::string& name);

      PMRegistrationItem* findRegistration(const std::string& name,
                                           protocol::RegistrationType type);
      PMRegistrationItem* findRegistration(unsigned int componentID,
                                           const std::string& name,
                                           protocol::RegistrationType kind);
      void findRegistrations(const std::string& name,
                             unsigned int destID,
                             protocol::RegistrationType type,
                             std::vector<PMRegistrationItem*>& regs);
      unsigned componentNameToID(const std::string& name);
      void resolveRegistrations(void);
      void resolveRegistrations(ComponentAlias::Registrations* registrations);
      void resolveRegistration(PMRegistrationItem* regI, bool afterInit2);
   };
#endif

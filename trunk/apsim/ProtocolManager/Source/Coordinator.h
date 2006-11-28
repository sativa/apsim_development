//---------------------------------------------------------------------------
#ifndef CoordinatorH
#define CoordinatorH

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
      Registrations registrations;
      unsigned int sequencerID;
      unsigned int runningMessageID;
      unsigned int childComponentID;
      bool afterInit2;
      string title;
      unsigned titleID;
      unsigned componentsID;
      vector<unsigned> componentOrders;
      bool doTerminate;
      bool printReport;
      std::string parentName;
      std::stack<unsigned> previousGetValueCompID;
      std::stack<unsigned> previousGetValueRegID;
      std::set<std::string> variablesBeenPolledForGets;
      std::set<std::string> variablesBeenPolledForSets;

      virtual void doInit1(const FString& sdml);
      virtual void doInit2(void);
      virtual void doCommence(void);
      virtual void onReturnComponentIDMessage(protocol::ReturnComponentIDData& data);
      virtual void onRegisterMessage(unsigned int fromID, protocol::RegisterData& registerData);
      virtual void onDeregisterMessage(unsigned int fromID, protocol::DeregisterData& registerData);
      virtual void onPublishEventMessage(unsigned int fromID, protocol::PublishEventData& publishEventData);

      virtual void onTerminateSimulationMessage(void);
      virtual void onGetValueMessage(unsigned int fromID, protocol::GetValueData& getValueData);
      virtual void onReplyValueMessage(unsigned fromID, protocol::ReplyValueData replyValueData);
      virtual void onRequestComponentIDMessage(unsigned int fromID, protocol::RequestComponentIDData& data);
      virtual void onQueryInfoMessage(unsigned int fromID, unsigned int messageID, protocol::QueryInfoData& queryInfo);
      virtual void onRequestSetValueMessage(unsigned int fromID, protocol::RequestSetValueData& setValueData);
      virtual void onApsimChangeOrderData(protocol::MessageData& messageData);
      virtual void onQuerySetValueMessage(unsigned fromID, protocol::QuerySetValueData& querySetData);

      virtual void respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);
      virtual void respondToGet(unsigned int& fromID, protocol::QueryValueData& queryData);
      virtual bool respondToSet(unsigned int& fromID, protocol::QuerySetValueData& setValueData);
      virtual void notifyTermination(void);

      void addComponent(const std::string& name,
                        const std::string& executable,
                        const std::string& componentInterfaceExecutable,
                        const std::string& sdml);
      unsigned int getComponentID(const std::string& name);

      // ------------------------------------------------------------------
      // Send queryValue messages to all subscribed components.
      // ------------------------------------------------------------------
      void sendQueryValueMessage(unsigned fromID, unsigned regID);
      
      // ------------------------------------------------------------------
      // Send a querySetValueMessage
      // ------------------------------------------------------------------
      void sendQuerySetValueMessage(unsigned ourComponentID,
                                    unsigned foreignComponentID,
                                    unsigned ourRegID,
                                    unsigned foreignRegID,
                                    protocol::Variant& variant);

      unsigned componentNameToID(const std::string& name);
      void pollComponentsForGetVariable(const std::string& variableName,
                                        unsigned destID);
      void pollComponentsForSetVariable(const std::string& variableName,
                                        unsigned destID,
                                        unsigned fromID,
                                        unsigned ourRegID,
                                        protocol::Variant& variant);

      void reorderSubscriptions(Registrations::Subscriptions& subs);
      void readAllRegistrations(void);

      virtual void onApsimGetQuery(protocol::ApsimGetQueryData& apsimGetQueryData);


   };
#endif

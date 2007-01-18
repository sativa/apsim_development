#ifndef MessagesH
#define MessagesH

#include "message.h"
#include "variant.h"
#include "ProtocolVector.h"
namespace protocol {

// --------------- NO DATA structure ------------
struct NoData
   {
   unsigned int dummy;
   };
inline MessageData& operator<< (MessageData& messageData, const NoData& data)
   {
   return messageData;
   }
inline unsigned int memorySize(const NoData& data)
   {
   return 0;
   }
// --------------- ERROR DATA structure ------------
struct ErrorData
   {
   ErrorData(bool fatal, const FString& msg)
      : isFatal(fatal), errorMessage(msg)
      { }
   bool isFatal;
   FString errorMessage;
   };
inline MessageData& operator<< (MessageData& messageData, const ErrorData& data)
   {
   messageData << data.isFatal << data.errorMessage;
   return messageData;
   }
inline MessageData& operator>> (MessageData& messageData, ErrorData& data)
   {
   messageData >> data.isFatal >> data.errorMessage;
   return messageData;
   }
inline unsigned int memorySize(const ErrorData& data)
   {
   return memorySize(data.isFatal) + memorySize(data.errorMessage);
   }


// --------------- Commence ------------
inline Message* newCommenceMessage(unsigned int from,
                                   unsigned int to)
   {
   return constructMessage(Commence, from, to, false, 0);
   }

// --------------- Complete ------------
struct CompleteData
   {
   unsigned int ack_ID;
   };
inline MessageData& operator>> (MessageData& messageData, CompleteData& data)
   {
   messageData >> data.ack_ID;
   return messageData;
   }
inline Message* newCompleteMessage(unsigned int from,
                                   unsigned int to,
                                   unsigned int ack_ID)
   {
   Message* msg = constructMessage(Complete, from, to, false,
                                   memorySize(ack_ID));
   MessageData messageData(msg);
   messageData << ack_ID;
   return msg;
   }

// --------------- Deregister ------------
enum RegistrationType {getVariableReg=1, respondToGetReg=2,
                       setVariableReg=8, respondToSetReg=3,
                                         respondToGetSetReg=4,
                       eventReg=5,       respondToEventReg=6,
                       methodCallReg=9,  respondToMethodCallReg=7};
struct DeregisterData
   {
   RegistrationType kind;
   unsigned int ID;
   };
inline MessageData& operator>> (MessageData& messageData, DeregisterData& data)
   {
   int k;
   messageData >> k >> data.ID;
   data.kind = (RegistrationType) k;
   return messageData;
   }
inline Message* newDeregisterMessage(unsigned int from,
                                     unsigned int to,
                                     RegistrationType kind,
                                     unsigned int ID)
   {
   Message* msg = constructMessage(Deregister, from, to, false,
                                   memorySize(kind) + memorySize(ID));
   MessageData messageData(msg);
   messageData << kind << ID;
   return msg;
   }

// -------------- Event ---------------
struct EventData
   {
   unsigned int ID;
   unsigned int publishedByID;
   Variant params;
   };
inline MessageData& operator>> (MessageData& messageData, EventData& data)
   {
   messageData >> data.ID >> data.publishedByID >> data.params;
   return messageData;
   }
inline Message* newEventMessage(unsigned int from,
                                unsigned int to,
                                unsigned int ID,
                                unsigned int publishedByID,
                                Variant& variant)
   {
   Message* msg = constructMessage(Event, from, to, false,
                             memorySize(ID) + memorySize(publishedByID) + memorySize(variant));
   MessageData messageData(msg);
   messageData << ID << publishedByID << variant;
   return msg;
   }
template <class T>
inline Message* newEventMessage(unsigned int from,
                                unsigned int to,
                                unsigned int ID,
                                unsigned int publishedByID,
                                const Type& type,
                                const T& data)
   {
   Message* msg = constructMessage(Event, from, to, false,
                             memorySize(ID) + memorySize(publishedByID) + memorySize(type) + memorySize(data));
   MessageData messageData(msg);
   messageData << ID << publishedByID << type << data;
   return msg;
   }


// -------------- GetValue ------------
struct GetValueData
   {
   unsigned int ID;
   };
inline MessageData& operator>> (MessageData& messageData, GetValueData& data)
   {
   messageData >> data.ID;
   return messageData;
   }
inline Message* newGetValueMessage(unsigned int from,
                                   unsigned int to,
                                   unsigned int ID)
   {
   Message* msg = constructMessage(GetValue, from, to, true,
                             memorySize(ID));
   MessageData messageData(msg);
   messageData << ID;
   return msg;
   }

// ---------------- Init1 ---------------
struct Init1Data
   {
   FString sdml;
   FString fqn;
   bool inStartup;
   };
inline MessageData& operator>> (MessageData& messageData, Init1Data& data)
   {
   messageData >> data.sdml >> data.fqn >> data.inStartup;
   return messageData;
   }
inline Message* newInit1Message(unsigned int from,
                                unsigned int to,
                                const FString& sdml,
                                const FString& fqn,
                                bool inStartup)
   {
   Message* msg = constructMessage(Init1, from, to, true,
                                   memorySize(sdml) + memorySize(fqn) + memorySize(inStartup));
   MessageData messageData(msg);
   messageData << sdml << fqn << inStartup;
   return msg;
   }
// ---------------- Init2 ---------------
inline Message* newInit2Message(unsigned int from,
                                unsigned int to)
   {
   return constructMessage(Init2, from, to, true, 0);
   }
// --------------- NotifySetValueSuccess ---------------
struct NotifySetValueSuccessData
   {
   unsigned int ID;
   bool success;
   };
inline MessageData& operator>> (MessageData& messageData, NotifySetValueSuccessData& data)
   {
   messageData >> data.ID >> data.success;
   return messageData;
   }
inline Message* newNotifySetValueSuccessMessage(unsigned int from,
                                                unsigned int to,
                                                unsigned int ID,
                                                bool success)
   {
   Message* msg = constructMessage(NotifySetValueSuccess, from, to, false,
                                   memorySize(ID) + memorySize(success));
   MessageData messageData(msg);
   messageData << ID << success;
   return msg;
   }
// ---------------- NotifyTermination ---------------
inline Message* newNotifyTerminationMessage(unsigned int from,
                                            unsigned int to)
   {
   return constructMessage(NotifyTermination, from, to, true, 0);
   }
// --------------- PublishEvent ---------------
struct PublishEventData
   {
   unsigned int ID;
   Variant variant;
   };
inline MessageData& operator>> (MessageData& messageData, PublishEventData& data)
   {
   messageData >> data.ID >> data.variant;
   return messageData;
   }
template <class T>
inline Message* newPublishEventMessage(unsigned int from,
                                       unsigned int to,
                                       unsigned int ID,
                                       const Type& type,
                                       const T& data)
   {
   Message* msg = constructMessage(PublishEvent, from, to, false,
                             memorySize(ID) + memorySize(type) + memorySize(data));
   MessageData messageData(msg);
   messageData << ID << type << data;
   return msg;
   }
template <class T>
inline Message* newPublishEventMessage(unsigned int from,
                                       unsigned int to,
                                       unsigned int ID,
                                       const Type& type,
                                       const T data[],
                                       unsigned numValues)
   {
   Message* msg = constructMessage(PublishEvent, from, to, false,
                                   memorySize(ID) + memorySize(type) + memorySize(numValues) +
                                   numValues * memorySize(data[0]));
   MessageData messageData(msg);
   messageData << ID << type << numValues;
   for (unsigned i = 0; i != numValues; i++)
      messageData << data[i];
   return msg;
   }

// -------------- QueryInfo --------------
enum SimulationInformationKind {respondToGetInfo = 2,
                                respondToSetInfo = 3,
                                respondToGetSetInfo = 4,
                                eventInfo = 5,
                                respondToEventInfo = 6,
                                respondToMethodInfo = 7,
                                componentInfo = 8,
                                methodCallInfo = 9};
struct QueryInfoData
   {
   FString name;
   SimulationInformationKind kind;
   };
inline MessageData& operator>> (MessageData& messageData, QueryInfoData& data)
   {
   int kind;
   messageData >> data.name >> kind;
   data.kind = (SimulationInformationKind)kind;
   return messageData;
   }
inline Message* newQueryInfoMessage(unsigned int from,
                                    unsigned int to,
                                    const FString& name,
                                    unsigned int kind)
   {
   Message* msg = constructMessage(QueryInfo, from, to, false,
                             memorySize(name) + memorySize(kind));
   MessageData messageData(msg);
   messageData << name << kind;
   return msg;
   }
// ------------ QuerySetValue ------------
struct QuerySetValueData
   {
   unsigned int ID;
   unsigned int replyToID;
   unsigned int replyID;
   Variant variant;
   };
inline MessageData& operator>> (MessageData& messageData, QuerySetValueData& data)
   {
   messageData >> data.ID >> data.replyToID >> data.replyID >> data.variant;
   return messageData;
   }
inline Message* newQuerySetValueMessage(unsigned int from,
                                        unsigned int to,
                                        unsigned int ID,
                                        unsigned int replytoID,
                                        unsigned int replyID,
                                        Variant& variant)
   {
   Message* msg = constructMessage(QuerySetValue, from, to, false,
                                   memorySize(ID) +
                                   memorySize(replytoID) + memorySize(replyID) +
                                   memorySize(variant));
   MessageData messageData(msg);
   messageData << ID << replytoID << replyID << variant;
   return msg;
   }
// ------------ QueryValue ------------
struct QueryValueData
   {
   unsigned int ID;
   unsigned int replytoID;
   unsigned int replyID;
   };
inline MessageData& operator>> (MessageData& messageData, QueryValueData& data)
   {
   messageData >> data.ID >> data.replytoID >> data.replyID;
   return messageData;
   }
inline Message* newQueryValueMessage(unsigned int from,
                                     unsigned int to,
                                     unsigned int ID,
                                     unsigned int replytoID,
                                     unsigned int replyID)
   {
   Message* msg = constructMessage(QueryValue, from, to, false,
                             memorySize(ID) +
                             memorySize(replytoID) + memorySize(replyID));
   MessageData messageData(msg);
   messageData << ID << replytoID << replyID;
   return msg;
   }

// -------------- Register -------------
struct RegisterData
   {
   RegistrationType kind;
   unsigned int ID;
   unsigned int destID;
   FString name;
   FString type;
   };
inline MessageData& operator>> (MessageData& messageData, RegisterData& data)
   {
   int kind;
   messageData >> kind >> data.ID >> data.destID >> data.name >> data.type;
   data.kind = (RegistrationType) kind;
   return messageData;
   }
inline Message* newRegisterMessage(unsigned int from,
                                   unsigned int to,
                                   RegistrationType kind,
                                   unsigned int ID,
                                   unsigned int destID,
                                   const FString& name,
                                   const Type& type)
   {
   Message* msg = constructMessage(Register, from, to, false,
                             memorySize(kind) + memorySize(ID) + memorySize(destID) +
                             memorySize(name) + memorySize(type));
   MessageData messageData(msg);
   messageData << kind << ID << destID << name << type;
   return msg;
   }
// -------------- RequestComponentID -------------
struct RequestComponentIDData
   {
   unsigned int replytoID;
   FString name;
   };
inline MessageData& operator>> (MessageData& messageData, RequestComponentIDData& data)
   {
   messageData >> data.replytoID >> data.name;
   return messageData;
   }
inline Message* newRequestComponentIDMessage(unsigned int from,
                                             unsigned int to,
                                             unsigned int replytoID,
                                             const FString& name)
   {
   Message* msg = constructMessage(RequestComponentID, from, to, false,
                             memorySize(replytoID) + memorySize(name));
   MessageData messageData(msg);
   messageData << replytoID << name;
   return msg;
   }
// -------------- RequestSetValue -------------
struct RequestSetValueData
   {
   unsigned int ID;
   Variant variant;
   };
inline MessageData& operator>> (MessageData& messageData, RequestSetValueData& data)
   {
   messageData >> data.ID >> data.variant;
   return messageData;
   }
template <class T>
inline Message* newRequestSetValueMessage(unsigned int from,
                                          unsigned int to,
                                          unsigned int regID,
                                          const Type& type,
                                          const T& data)
   {
   Message* msg = constructMessage(RequestSetValue, from, to, false,
                                   memorySize(regID) +
                                   memorySize(type) + memorySize(data));
   MessageData messageData(msg);
   messageData << regID << type << data;
   return msg;
   }

// ----------- ReturnComponentID ------------
struct ReturnComponentIDData
   {
   FString fqdn;
   unsigned int ID;
   };
inline MessageData& operator>> (MessageData& messageData, ReturnComponentIDData& data)
   {
   messageData >> data.fqdn >> data.ID;
   return messageData;
   }
inline Message* newReturnComponentIDMessage(unsigned int from,
                                            unsigned int to,
                                            const FString& fqdn,
                                            unsigned int ID)
   {
   Message* msg = constructMessage(ReturnComponentID, from, to, false,
                             memorySize(fqdn) + memorySize(ID));
   MessageData messageData(msg);
   messageData << fqdn << ID;
   return msg;
   }

// ------------- ReturnInfo ---------------
struct ReturnInfoData
   {
   unsigned int queryID;
   unsigned int componentID;
   unsigned int ID;
   FString name;
   FString type;
   SimulationInformationKind kind;
   };
inline MessageData& operator>> (MessageData& messageData, ReturnInfoData& data)
   {
   int kind;
   messageData >> data.queryID >> data.componentID >> data.ID
               >> data.name >> data.type >> kind;
   data.kind = (SimulationInformationKind) kind;
   return messageData;
   }
inline Message* newReturnInfoMessage(unsigned int from,
                                     unsigned int to,
                                     unsigned int queryID,
                                     unsigned int componentID,
                                     unsigned int ID,
                                     const FString& name,
                                     const FString& type,
                                     SimulationInformationKind kind)
   {
   Message* msg = constructMessage(ReturnInfo, from, to, false,
                             memorySize(queryID) + memorySize(componentID) +
                             memorySize(ID) + memorySize(name) +
                             memorySize(type) + memorySize(kind));
   MessageData messageData(msg);
   messageData << queryID << componentID << ID << name << type << kind;
   return msg;
   }
// ---------------- ReturnValue ----------------
struct ReturnValueData
   {
   unsigned int ID;
   Variant variant;
   };
inline MessageData& operator>> (MessageData& messageData, ReturnValueData& data)
   {
   messageData >> data.ID >> data.variant;
   return messageData;
   }

template <class T>
inline Message* newReturnValueMessage(unsigned int from,
                                      unsigned int to,
                                      unsigned int ID,
                                      const Type& type,
                                      const T& data)
   {
   Message* msg = constructMessage(ReturnValue, from, to, false,
                          memorySize(ID) + memorySize(type) + memorySize(data));
   MessageData messageData(msg);
   messageData << ID;
   messageData << type;
   messageData << data;
   return msg;
   }

// ---------------- TerminateSimulation ----------------
inline Message* newTerminateSimulationMessage(unsigned int from,
                                              unsigned int to)
   {
   return constructMessage(TerminateSimulation, from, to, false, 0);
   }

// -------------- ApsimGetQuery -------------
struct ApsimGetQueryData
   {
   FString name;
   };
inline MessageData& operator>> (MessageData& messageData, ApsimGetQueryData& data)
   {
   messageData >> data.name;
   return messageData;
   }
inline Message* newApsimGetQueryMessage(unsigned int from,
                                        unsigned int to,
                                        const FString& name)
   {
   Message* msg = constructMessage(ApsimGetQuery, from, to, false,
                                   memorySize(name));
   MessageData messageData(msg);
   messageData << name;
   return msg;
   }
// -------------- ApsimSetQuery -------------
struct ApsimSetQueryData
   {
   FString name;
   unsigned int replyToID;
   unsigned int replyID;
   Variant variant;
   };
inline MessageData& operator>> (MessageData& messageData, ApsimSetQueryData& data)
   {
   messageData >> data.name >> data.replyToID >> data.replyID >> data.variant;
   return messageData;
   }
inline Message* newApsimSetQueryMessage(unsigned int from,
                                        unsigned int to,
                                        const FString& regName,
                                        unsigned replyToID,
                                        unsigned replyID,
                                        Variant& variant)
   {
   Message* msg = constructMessage(ApsimSetQuery, from, to, false,
                                   memorySize(regName) + memorySize(replyToID) +
                                   memorySize(replyID) + memorySize(variant));
   MessageData messageData(msg);
   messageData << regName << replyToID << replyID << variant;
   return msg;
   }
// -------------- ApsimChangeOrder -------------
inline Message* newApsimChangeOrderMessage(unsigned int from,
                                           unsigned int to,
                                           const FStrings& names)
   {
   Message* msg = constructMessage(ApsimChangeOrder, from, to, false,
                                   memorySize(names));
   MessageData messageData(msg);
   messageData << names;
   return msg;
   }

} // end namespace protocol
#endif
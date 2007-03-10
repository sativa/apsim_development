//---------------------------------------------------------------------------
#ifndef MessageH
#define MessageH
#include <mem.h>

namespace protocol
   {

enum MessageType
   {
   ActivateComponent = 1,
   AddComponent = 2,
   Checkpoint = 3,
   Commence = 4,
   Complete = 5,
   DeactivateComponent = 6,
   DeleteComponent = 7,
   Deregister = 8,
   Event = 9,
   GetValue = 10,
   Init1 = 11,
   Init2 = 12,
   NotifyAboutToDelete = 13,
   NotifyRegistrationChange = 14,
   NotifySetValueSuccess = 15,
   NotifyTermination = 16,
   PauseSimulation = 17,
   PublishEvent = 18,
   QueryInfo = 19,
   QueryType = 20,
   QueryValue = 21,
   Register = 22,
   ReinstateCheckpoint = 23,
   RequestComponentID = 24,
   RequestSetValue = 25,
   ResumeSimulation = 26,
   ReturnComponentID = 27,
   ReturnInfo = 28,
   ReturnType = 29,
   ReturnValue = 30,
   TerminateSimulation = 31,
   QuerySetValue = 40,
   ApsimGetQuery = 41,
   ApsimSetQuery = 42,
   ApsimChangeOrder = 43
   };


// ------------------------------------------------------------------
//  Short description:
//    This class encapsulates a protocol message.

//  Notes:

//  Changes:
//    dph 22/2/2000
//    dph 20/4/2001 made protocol 1.0 compliant

// ------------------------------------------------------------------
struct Message
   {
   unsigned short int version;
   unsigned short int messageType;
   unsigned int from;
   unsigned int to;
   unsigned int messageID;
   unsigned int toAcknowledge;
   unsigned int nDataBytes;
   char* dataPtr;
   };

// ------------------------------------------------------------------
//  Short description:
//    Creates a new message.

//  Notes:

//  Changes:
//    dph 14/5/2001

// ------------------------------------------------------------------
Message* constructMessage(MessageType messageType,
                          unsigned int fromID,
                          unsigned int toID,
                          bool acknowledgementRequired,
                          unsigned int numDataBytes);

// ------------------------------------------------------------------
//  Short description:
//    Construct a new message without any data.

//  Notes:

//  Changes:
//    dph 14/5/2001

// ------------------------------------------------------------------
/*inline Message* newMessage(MessageType messageType,
                           unsigned int fromID,
                           unsigned int toID,
                           unsigned int messageID,
                           bool acknowledgement)
   {
   return constructMessage(messageType, fromID, toID, messageID, acknowledgement, 0);
   }
// ------------------------------------------------------------------
//  Short description:
//    construct a message from another message.

//  Notes:

//  Changes:
//    dph 14/5/2001

// ------------------------------------------------------------------
inline Message* newMessage(const Message* message, unsigned int messageID)
   {
   Message* newMessage = (Message*) malloc(sizeof(Message) + message->nDataBytes);
   std::memcpy(newMessage, message, message->nDataBytes + sizeof(Message));
   newMessage->dataPtr = ((char*)&newMessage->dataPtr) + 4;
   newMessage->messageID     = messageID;

   return newMessage;
   }
*/// ------------------------------------------------------------------
//  Short description:
//    Delete a message

//  Notes:

//  Changes:
//    dph 14/5/2001

// ------------------------------------------------------------------
void deleteMessage(Message* message);

void initMessages(void);
void deleteMessages(void);


} // namespace protocol
#endif
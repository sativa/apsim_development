#include <windows.h>
#pragma hdrstop

#include "Message.h"
#include "Alloc.h"
#include <windows.h>

using namespace std;
namespace protocol {

static int runningMessageID = 0;

static const unsigned MAX_NUM_MESSAGES = 20;
static const unsigned MAX_MESSAGE_SIZE = 5000;
Message* messages[MAX_NUM_MESSAGES]
   = {NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL};
static unsigned nextFreeMessage = 0;
// ------------------------------------------------------------------
//  Short description:
//    Initialise all static messages

//  Notes:

//  Changes:
//    dph 14/5/2001

// ------------------------------------------------------------------
void initMessages(void)
   {
   if (messages[0] == NULL)
      {
      for (unsigned messageI = 0; messageI < MAX_NUM_MESSAGES; messageI++)
         messages[messageI] = (Message*) new char[MAX_MESSAGE_SIZE];
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    Delete all static messages

//  Notes:

//  Changes:
//    dph 14/5/2001

// ------------------------------------------------------------------
void deleteMessages(void)
   {
   if (messages[0] != NULL)
      {
      for (unsigned messageI = 0; messageI < MAX_NUM_MESSAGES; messageI++)
         {
         delete [] messages[messageI];
         messages[messageI] = NULL;
         }
      }
   }


// ------------------------------------------------------------------
//  Short description:
//    Creates a new message.

//  Notes:

//  Changes:
//    dph 14/5/2001

// ------------------------------------------------------------------
Message* _export constructMessage(MessageType messageType,
                                  unsigned int fromID,
                                  unsigned int toID,
                                  bool acknowledgementRequired,
                                  unsigned int numDataBytes)
   {
   Message* message;
   if (nextFreeMessage > MAX_NUM_MESSAGES)
      {
      ::MessageBox(NULL, "Internal error: Too many messages sent from component.",
                   "Error", MB_ICONSTOP | MB_OK);
      return NULL;
      }
   else if (numDataBytes > MAX_MESSAGE_SIZE - sizeof(Message))
      message = (Message*) new char[sizeof(Message) + numDataBytes];

   else
      {
      message = messages[nextFreeMessage];
      ++nextFreeMessage;
      }

   message->version       = 256;
   message->messageType   =  messageType;
   message->from          = (unsigned int)fromID;
   message->to            = (unsigned int)toID;
   message->messageID     = ++runningMessageID;
   message->toAcknowledge = acknowledgementRequired;
   message->nDataBytes    = numDataBytes;
   if (numDataBytes > 0)
      message->dataPtr = ((char*)&message->dataPtr) + 4;

   else
      message->dataPtr = NULL;
   return message;
   }

void _export deleteMessage(Message* message)
   {
   if (message->nDataBytes > MAX_MESSAGE_SIZE - sizeof(Message))
      delete [] message;
   else
      nextFreeMessage--;
   }


} // namespace protocol

// ------------------------------------------------------------------
//  Short description:
//     FORTRAN: Create a message and return

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" unsigned  __stdcall get_componentID(void);
extern "C" unsigned  __stdcall construct_message
   (protocol::MessageType* messageType, unsigned int* to, unsigned* ack, unsigned* dataSize)
   {
   return (unsigned) constructMessage(*messageType,
                                      get_componentID(),
                                      *to,
                                      *ack,
                                      *dataSize);
   }


#include "StdAfx.h"
#include "MessageFactory.h"

#using <mscorlib.dll>

#include <string>
using namespace std;
using namespace ComponentInterface;

const unsigned MAX_MESSAGE_SIZE = 3000;
const unsigned PROTOCOL_VERSION = 1;


// -----------
// constructor
// -----------
MessageFactory::MessageFactory(void)
	{
	runningMessageID = 0;
	nextFreeMessage = 0;
	for (unsigned i = 0; i < MAX_NUM_MESSAGES; i++)
		messageData[i] = new char[MAX_MESSAGE_SIZE];
	}
// -----------
// destructor
// -----------
MessageFactory::~MessageFactory(void)
	{
	for (unsigned i = 0; i < MAX_NUM_MESSAGES; i++)
		delete messageData[i];
	}

// ---------------------------------------------------
// create and return a messages to caller - can throw.
// ---------------------------------------------------		
Message MessageFactory::create(int messageType)
	{
	if (nextFreeMessage >= MAX_NUM_MESSAGES)
		throw string("Internal error: The APSIM MessageFactory has run out of messages.");
	char* newMessage =  messageData[nextFreeMessage];
	nextFreeMessage++;
	runningMessageID++;
	return Message(newMessage, messageType, runningMessageID);
	}
	
// ---------------------------------------------------
// create and return a messages to caller - can throw.
// ---------------------------------------------------		
void MessageFactory::release(void)
	{
	if (nextFreeMessage == 0)
		throw string("Internal error: Tried to free a message that hadn't been created in MessageStore.");
	--nextFreeMessage;
	}

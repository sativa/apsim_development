#include "StdAfx.h"
#include "message.h"

#using <mscorlib.dll>
#include <string>
using namespace std;
using namespace ComponentInterface;

const unsigned MAX_NUM_MESSAGES = 20;
const unsigned MAX_MESSAGE_SIZE = 3000;
const unsigned PROTOCOL_VERSION = 1;


// ------------------------------------------
// Constructor:
// Create a message as a copy of the specified
// message.
// ------------------------------------------
Message::Message(Message& rhs)
	{
	weOwnData = true;
	// allocate enough memory and copy main message header (7 * 4 bytes)
	unsigned numBytes = rhs.dataSize() + 7 * 4;
	data = new char[numBytes];
	memcpy((void*) data, (void*) rhs.byteStream(), 7 * 4);

	// make data 'datapointer' 4 byte word point to just after the message header.	
	unsigned* unsignedInts = (unsigned*) data;
	unsignedInts += 6;
  	*unsignedInts = (unsigned)unsignedInts+4;								// datapointer
  	resetDataStream();  	
  	
  	// now copy the data bit of the 'rhs' message.
	memcpy((void*) dataPtr, (void*) rhs.dataPtr, rhs.dataSize());
	}

// ----------
// Destructor
// ----------
Message::~Message(void)
	{
	if (weOwnData)
		delete [] data;
	}

// ----------------
// Create a message
// ----------------
Message::Message(char* dataBytes, int messageType, int msgID)
	{
	weOwnData = false;
  	int null = 0;
	data = dataBytes;

	short* wordData = (short*) data;
	*wordData = PROTOCOL_VERSION; wordData++;								// protocol version
	*wordData = messageType; wordData++;									// msgtype

	unsigned* unsignedInts = (unsigned*) wordData;
	unsignedInts += 2;														// skip over from and to
  	*unsignedInts = (unsigned)msgID; unsignedInts++;						// msgID
  	unsignedInts++;															// skip over ack.
  	*unsignedInts = (unsigned)null; unsignedInts++;							// ndatabytes
  	*unsignedInts = (unsigned)unsignedInts+4;								// datapointer
  	
  	resetDataStream();
	}


// -------------------
// Set message address
// -------------------
void Message::setAddress(int fromID, int toID, bool acknowledge)
	{
  	int acknowledgeAsInt = acknowledge;
	unsigned* unsignedInts = (unsigned*) data;
	unsignedInts++;

  	*unsignedInts = (unsigned)fromID; unsignedInts++;						// from
  	*unsignedInts = (unsigned)toID; unsignedInts += 2;						// to
  	*unsignedInts = (unsigned)acknowledgeAsInt;                				// ack
	}

#pragma once

namespace ComponentInterface {

class Messages;
class MessageFactory;
// ------------------------
// A lowlevel message class
// ------------------------
class Message
	{
	public:
		// ------------------------------------------
		// Constructor:
		// Create a message as a copy of the specified
		// message.
		// ------------------------------------------
		Message(Message& rhs);

		// ------------------------------------------
		// Constructor:
		// Create a message using the specified bytes
		// ------------------------------------------
		Message(char* dataBytes) : weOwnData(false), data(dataBytes) {resetDataStream(); }
		
		// ----------------
		// destructor
		// ----------------
		~Message(void);
		
		// -------------------
		// Set message address
		// -------------------
		void setAddress(int fromID, int toID, bool acknowledge);
		
		// ----------------------
		// return from ID of message
		// ----------------------
		unsigned fromID(void) {return * (unsigned*) &data[4];}

		// ----------------------
		// return type of message
		// ----------------------
		unsigned type(void) {return * (short*) &data[2];}

		// ----------------------
		// return type of message
		// ----------------------
		unsigned msgID(void) {return * (unsigned*) &data[12];}
		
		// --------------------------------
		// return the bytestream to caller.
		// --------------------------------
		char* byteStream(void) {return data;}

		// -----------------------------------------------
		// Reset the data stream position to the beginning
		// of the data area.
		// -----------------------------------------------
		void resetDataStream(void) {dataPtr = *((char**)(data + 24));}

		// -----------------------------------------------
		// Reset the data stream position to the beginning
		// of the data area.
		// -----------------------------------------------
		void advanceDataStream(unsigned size) {dataPtr += size;}
		
		// --------------------------------
		// return the bytestream to caller.
		// --------------------------------
		char* dataStream(void) {return dataPtr;}

		// -----------------------------------------------
		// Add to the data size.
		// -----------------------------------------------
		void addToDataSize(unsigned size) {*(unsigned*)(data+20) += size;}		

		// -----------------------------------------------
		// Return the data size.
		// -----------------------------------------------
		unsigned dataSize() {return *(unsigned*)(data+20);}		

	private:
		char* data;
		char* dataPtr;
		bool weOwnData;

		Message(void);

		// ------------------------------------------
		// Constructor: (called by MessageFactory)
		// Create a message using the specified bytes
		// ------------------------------------------
		Message(char* dataBytes, int messageType, int msgID);

		friend Messages;
		friend MessageFactory;
	};

};


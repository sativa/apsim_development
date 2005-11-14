#pragma once

#include "Message.h"
namespace ComponentInterface {

// ------------------------
// Message factory
// ------------------------
class MessageFactory
	{
	public:
		// -----------
		// constructor
		// -----------
		MessageFactory(void);

		// -----------
		// destructor
		// -----------
		~MessageFactory(void);

		// ---------------------------------------------------
		// create and return a messages to caller - can throw.
		// ---------------------------------------------------		
		Message create(int messageType);
			
		// ---------------------------------------------------
		// create and return a messages to caller - can throw.
		// ---------------------------------------------------		
		void release(void);
			
		
	private:
		static const unsigned MAX_NUM_MESSAGES = 20;

		int runningMessageID;
		char* messageData[MAX_NUM_MESSAGES];
		unsigned nextFreeMessage;
   
    };
};
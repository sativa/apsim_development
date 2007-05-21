#pragma once

#include "Message.h"
#include "MessageFactory.h"
#include "MessageData.h"
#include "MessageType.h"
#include <string>
using namespace System;
using namespace System::ComponentModel;
using namespace System::Collections;
using namespace System::Diagnostics;
using namespace NUnit::Framework;
 
//------------------------------------
// Class for testing the Message class
//------------------------------------
[TestFixture]
__gc public class TestMessage
	{
	public:
			
		// ------------------------------------------------
		// Test that a message's bytes are correct.
		// ------------------------------------------------
		[Test]
		void MessageBytesOk()
			{
			MessageFactory messageFactory;

			Message message = messageFactory.create(MessageType::Commence);
			message.setAddress(1, 2, true);

			pack(message, 100);

			const unsigned* messageByteStream = (const unsigned*) message.byteStream();
			NUnit::Framework::Assert::IsTrue(*messageByteStream == 0x00040001); messageByteStream++;		  // version
			NUnit::Framework::Assert::IsTrue(*messageByteStream == 0x00000001); messageByteStream++;		  // from
			NUnit::Framework::Assert::IsTrue(*messageByteStream == 0x00000002); messageByteStream++;		  // to
			NUnit::Framework::Assert::IsTrue(*messageByteStream == 0x00000001); messageByteStream++;		  // msgid
			NUnit::Framework::Assert::IsTrue(*messageByteStream == 0x00000001); messageByteStream++;		  // ack
			NUnit::Framework::Assert::IsTrue(*messageByteStream == 0x00000004); messageByteStream++;		  // ndatabytes
			NUnit::Framework::Assert::IsTrue(*messageByteStream == (unsigned)messageByteStream+4); messageByteStream++; // datapointer
			NUnit::Framework::Assert::IsTrue(*messageByteStream == 0x00000064);                     		  // actual data
			}
			

		// ---------------------------------------------------------
		// Test that data can be stored and retrieved from a message
		// ---------------------------------------------------------
		[Test]
		void MessageDataRetrieval()
			{
			MessageFactory messageFactory;

			Message message = messageFactory.create(MessageType::Commence);
			message.setAddress(1, 2, true);
			NUnit::Framework::Assert::IsTrue(message.type() == MessageType::Commence);
			
			pack(message, 200);
			pack(message, "Hello");

			message.resetDataStream();
			int integerReceived;
			String* stringReceived;
			unpack(message, integerReceived);
			unpack(message, stringReceived);
			NUnit::Framework::Assert::IsTrue(integerReceived == 200);
			NUnit::Framework::Assert::IsTrue(stringReceived->CompareTo("Hello") == 0);
			NUnit::Framework::Assert::IsTrue(message.dataSize() == 13);
			}				
			

	};

#pragma once

#include "Message.h"
#include "MessageFactory.h"
#include "MessageData.h"
#include "MessageType.h"
#include "DataTypes.h"
#include "ComponentInterface.h"
#include "ApsimEvents.h"


using namespace System;
using namespace System::ComponentModel;
using namespace System::Collections;
using namespace System::Diagnostics;
using namespace NUnit::Framework;
 
using namespace std;

// -----------------------------------------------------
// Test that a component's setup method is called.
// -----------------------------------------------------
__gc class Init1ComponentStub : public ::IComponent
	{
	public:
		
		virtual	void init1(String* name, String* sdml, IComms* comms, ApsimEvents* events)
			{
			Assert::IsTrue(sdml->CompareTo("<initdata></initdata>") == 0);
			Assert::IsTrue(name->CompareTo("ComponentStub") == 0);
			}
		virtual void init2() { }
	};



// ------------------------------------------------
// Test that a component can terminate a simulation
// ------------------------------------------------
__gc class TerminateSimulationComponentStub : public ::IComponent
	{
	public:
		virtual	void init1(String* name, String __gc * sdml, IComms* comms, ApsimEvents* events)
			{
			comms->terminateSimulation();
			}
		virtual void init2() { }
	};
void __stdcall TerminateSimulationCallback(const unsigned* arg, char* messageBytes)
	{
	Message message(messageBytes);
	Assert::AreEqual(message.type(), MessageType::TerminateSimulation);
	}			


// ----------------------------------------
// Test that a component can send a line to
// the summary file
// ----------------------------------------
__gc class WriteToSummaryComponentStub : public ::IComponent
	{
	public:
		virtual	void init1(String* name, String __gc * sdml, IComms* comms, ApsimEvents* events)
			{
			comms->writeToSummary("Hello from .net");
			}
		virtual void init2() { }
	};
	
void __stdcall WriteToSummaryCallback(const unsigned* arg, char* messageBytes)
	{
	Message message(messageBytes);

	if (message.msgID() == 1)
		{
		Assert::AreEqual(message.type(), MessageType::Register);
		}
	else
		{
		PublishEvent publishEvent;
		publishEvent.unpack(message);
		SummaryFileWrite summaryFileWrite;
		summaryFileWrite.unpack(message);
		Assert::IsTrue(summaryFileWrite.componentName == "ComponentStub");
		Assert::IsTrue(summaryFileWrite.lines == "Hello from .net");
		}
	}
	
	
// -----------------------------------------
// Test that a component can issue a warning
// -----------------------------------------
__gc class WarningComponentStub : public ::IComponent
	{
	public:
		virtual	void init1(String* name, String __gc * sdml, IComms* comms, ApsimEvents* events)
			{
			comms->warning("Hello from .net");
			}
		virtual void init2() { }
	};
	
void __stdcall WarningCallback(const unsigned* arg, char* messageBytes)
	{
	Message message(messageBytes);

	if (message.msgID() == 1)
		{
		Assert::AreEqual(message.type(), MessageType::Register);
		}
	else
		{
		PublishEvent publishEvent;
		publishEvent.unpack(message);
		Error error;
		error.unpack(message);
		Assert::IsTrue(error.msg == 		
				"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n"
				"                 APSIM Warning Error               \n"
				"                 -------------------               \n"
				"Hello from .net\n"
				"Component name: ComponentStub\n"
				"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\n");
		}
	}	

// ----------------------------------------
// Test that a component can get a variable
// ----------------------------------------
__gc class GetPropertyComponentStub : public ::IComponent
	{
	public:
		static ::IComponent* comp;
		virtual	void init1(String* name, String __gc * sdml, IComms* comms, ApsimEvents* events)
			{
			comp = this;
			Integer year;
			String* from = comms->getProperty("year", year);
			Assert::AreEqual(from, new String("ComponentStub2"));
			Assert::AreEqual(year.value, 2005);
			}
		virtual void init2() { }
	};
	
void __stdcall GetPropertyCallback(const unsigned* arg, char* messageBytes)
	{
	Message message(messageBytes);
	ComponentInterface* componentInterface = (ComponentInterface*) arg;

	if (message.type() == MessageType::GetValue)
		{

		MessageFactory messageFactory;
		Message returnValueMessage = messageFactory.create(MessageType::ReturnValue);
		ReturnValue returnValue;
		returnValue.compID = 15;
		returnValue.id = 1;
		returnValue.type = "";
		returnValue.pack(returnValueMessage);
		Integer returnYear;
		returnYear.value = 2005;
		returnYear.pack(returnValueMessage);

		componentInterface->messageToLogic(GetPropertyComponentStub::comp, returnValueMessage.byteStream());
		}
	else if (message.type() == MessageType::QueryInfo)
		{
		MessageFactory messageFactory;
		Message returnInfoMessage = messageFactory.create(MessageType::ReturnInfo);
		ReturnInfo returnInfo;
		returnInfo.queryID = 1;
		returnInfo.compID = 15;
		returnInfo.id = 15;
		returnInfo.name = "ComponentStub2";
		returnInfo.pack(returnInfoMessage);
		componentInterface->messageToLogic(GetPropertyComponentStub::comp, returnInfoMessage.byteStream());
		}
	}	
	
// ---------------------------------------------------------
// Test that an invalid returnvalue in response to a getValue
// is trapped properly.
// ---------------------------------------------------------
__gc class InvalidReturnValueComponentStub : public ::IComponent
	{
	public:
		static ::IComponent* comp;
		virtual	void init1(String* name, String __gc * sdml, IComms* comms, ApsimEvents* events)
			{
			comp = this;
			Integer year;
			comms->getProperty("year", year);
			Assert::Fail();
			}
		virtual void init2() { }

	};
void __stdcall InvalidReturnValueCallback(const unsigned* arg, char* messageBytes)
	{
	Message message(messageBytes);

	if (message.type() == MessageType::GetValue)
		{
		ComponentInterface* componentInterface = (ComponentInterface*) arg;

		MessageFactory messageFactory;
		Message returnValueMessage = messageFactory.create(MessageType::ReturnValue);
		ReturnValue returnValue;
		returnValue.compID = 15;
		returnValue.id = 10000;	  // invalid value.
		returnValue.type = "";
		returnValue.pack(returnValueMessage);
		Integer returnYear;
		returnYear.value = 2005;
		returnYear.pack(returnValueMessage);

		componentInterface->messageToLogic(GetPropertyComponentStub::comp, returnValueMessage.byteStream());
		}
	}	
// ----------------------------------------
// Test that a component can set a variable
// in another component.
// ----------------------------------------
__gc class SetPropertyComponentStub : public ::IComponent
	{
	public:
		static ::IComponent* comp;
		virtual	void init1(String* name, String __gc * sdml, IComms* comms, ApsimEvents* events)
			{
			comp = this;
			Integer year;
			year.value = 300;
			bool ok = comms->setProperty("year", year);
			Assert::IsTrue(ok);
			}
		virtual void init2() { }
	};
	
void __stdcall SetPropertyCallback(const unsigned* arg, char* messageBytes)
	{
	Message message(messageBytes);
	ComponentInterface* componentInterface = (ComponentInterface*) arg;

	if (message.type() == MessageType::RequestSetValue)
		{
		RequestSetValue requestSetValue;
		requestSetValue.unpack(message);
		
		MessageFactory messageFactory;
		Message notifySetValueSuccessMessage = messageFactory.create(MessageType::NotifySetValueSuccess);
		NotifySetValueSuccess notifySetValueSuccess;
		notifySetValueSuccess.id = requestSetValue.id;
		notifySetValueSuccess.success = true;
		notifySetValueSuccess.pack(notifySetValueSuccessMessage);
		componentInterface->messageToLogic(SetPropertyComponentStub::comp, notifySetValueSuccessMessage.byteStream());
		}
	}	

// -----------------------------------------
// Test that a component can public an event
// -----------------------------------------
__gc class PublishComponentStub : public ::IComponent
	{
	public:
		static ::IComponent* comp;
		static bool publishWasSuccessfull;
		virtual	void init1(String* name, String __gc * sdml, IComms* comms, ApsimEvents* events)
			{
			comp = this;
			publishWasSuccessfull = false;
			Integer year;
			year.value = 300;
			comms->publishEvent("myevent", year);
			Assert::IsTrue(publishWasSuccessfull);
			}
		virtual void init2() { }
	};
	
void __stdcall PublishCallback(const unsigned* arg, char* messageBytes)
	{
	Message message(messageBytes);
	ComponentInterface* componentInterface = (ComponentInterface*) arg;

	if (message.type() == MessageType::PublishEvent)
		{
		PublishEvent publishEvent;
		publishEvent.unpack(message);
		Assert::IsTrue(publishEvent.type == "<type name = \"Integer\">" 
										    "   <field name=\"value\" kind=\"integer4\"/>" 
										    "</type>");
		Integer data;
		data.unpack(message);
		Assert::AreEqual(data.value, 300);
		PublishComponentStub::publishWasSuccessfull = true;
		}
	}	
	
// ----------------------------------------------------------------
// Test that a component can return a property to another component
// ----------------------------------------------------------------
__gc class RespondToGetComponentStub : public ::IComponent
	{
	public:
		static ::IComponent* comp;
		static bool ok;
		virtual	void init1(String* name, String __gc * sdml, IComms* comms, ApsimEvents* events)
			{
			comp = this;
			ok = false;
			ApsimInteger* myVariable = new ApsimInteger;
			comms->registerProperty("myvariable", IComms::read, myVariable);

			myVariable->value = 300;
			
			// try and get our own variable.
			Integer returnVariable;
			comms->getProperty("myvariable", returnVariable);
			Assert::AreEqual(returnVariable.value, 300);

			ok = true;
	
			}
		virtual void init2() { }
	};
	
void __stdcall RespondToGetCallback(const unsigned* arg, char* messageBytes)
	{
	Message message(messageBytes);
	ComponentInterface* componentInterface = (ComponentInterface*) arg;
	static unsigned id;

	if (message.type() == MessageType::Register)
		{
		Register reg;
		reg.unpack(message);
		id = reg.id;
		}

	else if (message.type() == MessageType::GetValue)
		{
		MessageFactory messageFactory;
		Message queryValueMessage = messageFactory.create(MessageType::QueryValue);
		QueryValue queryValue;
		queryValue.id = id;
		queryValue.requestedBy = 0;
		queryValue.pack(queryValueMessage);
		componentInterface->messageToLogic(RespondToGetComponentStub::comp, queryValueMessage.byteStream());
		}
				
	else if (message.type() == MessageType::ReplyValue)
		{
		ReplyValue replyValue;
		replyValue.unpack(message);
		Assert::AreEqual(replyValue.queryID, id);
		Integer variable;
		variable.unpack(message);

		MessageFactory messageFactory;
		Message returnValueMessage = messageFactory.create(MessageType::ReturnValue);
		ReturnValue returnValue;
		returnValue.compID = 15;
		returnValue.id = id;
		returnValue.type = "";
		returnValue.pack(returnValueMessage);
		variable.pack(returnValueMessage);
		componentInterface->messageToLogic(GetPropertyComponentStub::comp, returnValueMessage.byteStream());
		}
	else if (message.type() == MessageType::QueryInfo)
		{
		MessageFactory messageFactory;
		Message returnInfoMessage = messageFactory.create(MessageType::ReturnInfo);
		ReturnInfo returnInfo;
		returnInfo.queryID = 1;
		returnInfo.compID = 15;
		returnInfo.id = 15;
		returnInfo.name = "ComponentStub2";
		returnInfo.pack(returnInfoMessage);
		componentInterface->messageToLogic(GetPropertyComponentStub::comp, returnInfoMessage.byteStream());
		}
	}	

// ---------------------------------------------------------
// Test that another component can set one of our properties
// ---------------------------------------------------------
__gc class RespondToSetComponentStub : public ::IComponent
	{
	public:
		static ::IComponent* comp;
		static bool ok;
		ApsimInteger* myVariable;
		virtual	void init1(String* name, String __gc * sdml, IComms* comms, ApsimEvents* events)
			{
			comp = this;
			ok = false;
			myVariable = new ApsimInteger;
			comms->registerProperty("myvariable", IComms::readWrite, myVariable);

			Integer dummy;
			comms->publishEvent("myevent", dummy);
			
			Assert::AreEqual(myVariable->value, 456);
			ok = true;
			}
		virtual void init2() { }
	};
	
void __stdcall RespondToSetCallback(const unsigned* arg, char* messageBytes)
	{
	Message message(messageBytes);
	ComponentInterface* componentInterface = (ComponentInterface*) arg;
	static unsigned id;

	if (message.type() == MessageType::Register)
		{
		Register reg;
		reg.unpack(message);
		if (reg.kind == 4)
			id = reg.id;
		}
	else if (message.type() == MessageType::PublishEvent)
		{
		Integer dummy;
		dummy.value = 456;

		MessageFactory messageFactory;
		Message querySetValueMessage = messageFactory.create(MessageType::QuerySetValue);
		QuerySetValue querySetValue;
		querySetValue.id = id;
		querySetValue.type = dummy.ddml();
		querySetValue.pack(querySetValueMessage);
		
		dummy.pack(querySetValueMessage);
		componentInterface->messageToLogic(RespondToGetComponentStub::comp, querySetValueMessage.byteStream());
		}
				
	else if (message.type() == MessageType::NotifySetValueSuccess)
		{
	    NotifySetValueSuccess setValueSuccess;
	    setValueSuccess.unpack(message);
	    Assert::IsTrue(setValueSuccess.success);
		}
	}	

// ----------------------------------------------------------------
// Test that a component can receive an event
// ----------------------------------------------------------------
__gc class RespondToEventComponentStub : public ::IComponent
	{
	public:
		static ::IComponent* comp;
		static bool ok;
		virtual	void init1(String* name, String __gc * sdml, IComms* comms, ApsimEvents* events)
			{
			comp = this;
			ok = false;
			events->registerTickHandler("tick", new ApsimEvents::OnTick(this, &RespondToEventComponentStub::onTick));
			
			Integer dummy;
			comms->publishEvent("myevent", dummy);
			
			Assert::IsTrue(ok);	
			}
		void onTick(ApsimTime* tick)
			{
			ok = true;
			Assert::AreEqual(tick->startday, 1);
			Assert::AreEqual(tick->endday, 300);
			}
		virtual void init2() { }
	};
	
void __stdcall RespondToEventCallback(const unsigned* arg, char* messageBytes)
	{
	Message message(messageBytes);
	ComponentInterface* componentInterface = (ComponentInterface*) arg;
	static unsigned id;

	if (message.type() == MessageType::Register)
		{
		Register reg;
		reg.unpack(message);
		if (reg.kind == 6) // respondToEvent
			id = reg.id;
		}
	else if (message.type() == MessageType::PublishEvent)
		{
		Time time;
		time.startday = 1;
		time.endday = 300;

		MessageFactory messageFactory;
		Message eventMessage = messageFactory.create(MessageType::Event);
		Event event;
		event.id = id;
		event.publishedBy = 15;
		event.type = time.ddml();
		event.pack(eventMessage);
		time.pack(eventMessage);
		componentInterface->messageToLogic(RespondToEventComponentStub::comp, eventMessage.byteStream());
		}
	else if (message.type() == MessageType::QueryInfo)
		{
		MessageFactory messageFactory;
		Message returnInfoMessage = messageFactory.create(MessageType::ReturnInfo);
		ReturnInfo returnInfo;
		returnInfo.queryID = 1;
		returnInfo.compID = 15;
		returnInfo.id = 15;
		returnInfo.name = "ComponentStub2";
		returnInfo.pack(returnInfoMessage);
		componentInterface->messageToLogic(GetPropertyComponentStub::comp, returnInfoMessage.byteStream());
		}
	}	

//------------------------------------
// Class for testing the Message class
//------------------------------------
[TestFixture]
__gc public class TestComponentInterface
	{
	public:
			
		// ----------------------------------------------------------
		// Load an APSIM component assembly and send an Init1 message
		// ----------------------------------------------------------
		void runTest(::IComponent* component, 
					 ComponentInterface::CallbackType callback)
			{
			MessageFactory messageFactory;
			Message init1Message = messageFactory.create(MessageType::Init1);
			init1Message.setAddress(1, 2, true);
			Init1 init1;
			init1.sdml = "<component name=\"test\"><initdata></initdata></component>";
			init1.fqn = "ComponentStub";
			init1.pack(init1Message);

			ComponentInterface componentInterface;
			componentInterface.createInstance(component, "DllFileName", 1, 0, 
			                                  (unsigned*)&componentInterface, callback);
			componentInterface.messageToLogic(component, init1Message.byteStream());
			
			Message init2Message = messageFactory.create(MessageType::Init2);
			init2Message.setAddress(1, 2, true);
			componentInterface.messageToLogic(component, init2Message.byteStream());
			}

		
		// -----------------------------------------------------
		// Test that a component's init method is called.
		// -----------------------------------------------------
		[Test]
		void TestInit1()
			{
			runTest(new Init1ComponentStub, NULL);
			}         

		// ------------------------------------------------
		// Test that a component can terminate a simulation
		// ------------------------------------------------
		[Test]
		void TestTerminateSimulationEvent()
			{
			runTest(new TerminateSimulationComponentStub, &TerminateSimulationCallback);
			}				
  
		// ----------------------------------------
		// Test that a component can send a line to
		// the summary file
		// ----------------------------------------
		[Test]
		void TestWriteToSummary() 
			{
			runTest(new WriteToSummaryComponentStub, &WriteToSummaryCallback);
			}
	
		// ----------------------------------------
		// Test that a component can issue a warning
		// ----------------------------------------
		[Test]
		void TestWarning() 
			{
			runTest(new WarningComponentStub, &WarningCallback);
			}	
			
		// ----------------------------------------
		// Test that a component can get a variable
		// ----------------------------------------
		[Test]
		void TestGetProperty() 
			{
			runTest(new GetPropertyComponentStub, &GetPropertyCallback);
			}			
			
		// ----------------------------------------
		// Test that an invalid returnvalue in response to a getValue
		// is trapped properly.
		// ----------------------------------------
		[Test]
		void TestInvalidReturnValue() 
			{
			runTest(new InvalidReturnValueComponentStub, &InvalidReturnValueCallback);
			}			

		// ----------------------------------------
		// Test that a component can set a variable
		// in another component.
		// ----------------------------------------
		[Test]
		void TestSetProperty() 
			{
			runTest(new SetPropertyComponentStub, &SetPropertyCallback);
			}			

		// ----------------------------------------
		// Test that a component can public an event
		// ----------------------------------------
		[Test]
		void TestPublish() 
			{
			runTest(new PublishComponentStub, &PublishCallback);
			}			

		// ----------------------------------------
		// Test that a component can public an event
		// ----------------------------------------
		[Test]
		void TestRespondToGet() 
			{
			runTest(new RespondToGetComponentStub, &RespondToGetCallback);
			Assert::IsTrue(RespondToGetComponentStub::ok);
			}			
		
		// ---------------------------------------------------------
		// Test that another component can set one of our properties
		// ---------------------------------------------------------
		[Test]
		void TestRespondToSet() 
			{
			runTest(new RespondToSetComponentStub, &RespondToSetCallback);
			Assert::IsTrue(RespondToSetComponentStub::ok);
			}			
	
		// ----------------------------------------------------------------
		// Test that a component can receive an event
		// ----------------------------------------------------------------
		[Test]
		void TestRespondToEvent() 
			{
			runTest(new RespondToEventComponentStub, &RespondToEventCallback);
			Assert::IsTrue(RespondToEventComponentStub::ok);
			}			
				 
		};
	
#pragma once

#include "Message.h"
#include "Messages.h"
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
		static bool ok = false;;
		virtual	void init1(String* name, String* sdml, IComms* comms, ApsimEvents* events)
			{
			Assert::IsTrue(sdml->CompareTo("<initdata></initdata>") == 0);
			Assert::IsTrue(name->CompareTo("ComponentStub") == 0);
			ok = true;
			}
		virtual void init2() { }
	};



// ------------------------------------------------
// Test that a component can terminate a simulation
// ------------------------------------------------
__gc class TerminateSimulationComponentStub : public ::IComponent
	{
	public:
		static bool ok = false;;
		virtual	void init1(String* name, String __gc * sdml, IComms* comms, ApsimEvents* events)
			{
			comms->terminateSimulation();
			ok = true;
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
		static bool ok = false;;
		virtual	void init1(String* name, String __gc * sdml, IComms* comms, ApsimEvents* events)
			{
			comms->writeToSummary("Hello from .net");
			ok = true;
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
		SummaryFileWrite* summaryFileWrite = new SummaryFileWrite;
		summaryFileWrite->unpack(message);
		Assert::IsTrue(summaryFileWrite->componentName->CompareTo("ComponentStub") == 0);
		Assert::IsTrue(summaryFileWrite->lines->CompareTo("Hello from .net") == 0);
		}
	}
	
	
// -----------------------------------------
// Test that a component can issue a warning
// -----------------------------------------
__gc class WarningComponentStub : public ::IComponent
	{
	public:
		static bool ok = false;;
		virtual	void init1(String* name, String __gc * sdml, IComms* comms, ApsimEvents* events)
			{
			comms->warning("Hello from .net");
			ok = true;
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
		Error* error = new Error;
		error->unpack(message);
		Assert::IsTrue(error->msg->CompareTo( 		
				"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n"
				"                 APSIM Warning Error               \n"
				"                 -------------------               \n"
				"Hello from .net\n"
				"Component name: ComponentStub\n"
				"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\n") == 0);
		}
	}	

// ----------------------------------------
// Test that a component can get a variable
// ----------------------------------------
__gc class GetPropertyComponentStub : public ::IComponent
	{
	public:
		static ::IComponent* comp;
		static bool ok = false;;
		
		virtual	void init1(String* name, String __gc * sdml, IComms* comms, ApsimEvents* events)
			{
			comp = this;
			System::Int32 year;
			String* from = comms->getProperty("year", year);
			Assert::AreEqual(from, new String("ComponentStub2"));
			Assert::AreEqual(year, 2005);
			ok = true;
			}
		virtual void init2() { }
	};
	
void __stdcall GetPropertyCallback(const unsigned* arg, char* messageBytes)
	{
	Message message(messageBytes);
	ComponentInterface* componentInterface = (ComponentInterface*) arg;
	static unsigned id;
	if (message.type() == MessageType::Register)
		{
		Register reg;
		reg.unpack(message);
		if (reg.kind == 1)	 // get
			id = reg.id;
		}
	else if (message.type() == MessageType::GetValue)
		{

		MessageFactory messageFactory;
		Message returnValueMessage = messageFactory.create(MessageType::ReturnValue);
		ReturnValue returnValue;
		returnValue.compID = 15;
		returnValue.id = id;
		returnValue.type = "";
		returnValue.pack(returnValueMessage);
		int returnYear;
		returnYear = 2005;
		pack(returnValueMessage, returnYear);

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
			Int32 year;
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
		int returnYear;
		returnYear = 2005;
		pack(returnValueMessage, returnYear);

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
		static bool ok = false;;
		virtual	void init1(String* name, String __gc * sdml, IComms* comms, ApsimEvents* events)
			{
			comp = this;
			Int32 year;
			year = 300;
			ok = comms->setProperty("year", year);
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
		static bool ok = false;;
		virtual	void init1(String* name, String __gc * sdml, IComms* comms, ApsimEvents* events)
			{
			comp = this;
			Int32 year = 300;
			comms->publishEvent("myevent", year);
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
		Assert::IsTrue(publishEvent.type == "<type kind=\"integer4\"/>"); 
		int data;
		unpack(message, data);
		Assert::AreEqual(data, 300);
		PublishComponentStub::ok = true;
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
			int myVariable;
			comms->registerProperty("myvariable", IComms::read, myVariable);

			myVariable = 300;
			
			// try and get our own variable.
			int returnVariable;
			comms->getProperty("myvariable", returnVariable);
			Assert::AreEqual(returnVariable, 300);

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
		int variable;
		unpack(message, variable);

		MessageFactory messageFactory;
		Message returnValueMessage = messageFactory.create(MessageType::ReturnValue);
		ReturnValue returnValue;
		returnValue.compID = 15;
		returnValue.id = id;
		returnValue.type = "";
		returnValue.pack(returnValueMessage);
		pack(returnValueMessage, variable);
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
		Int32 myVariable;
		virtual	void init1(String* name, String __gc * sdml, IComms* comms, ApsimEvents* events)
			{
			comp = this;
			ok = false;
			comms->registerProperty("myvariable", IComms::readWrite, myVariable);

			Int32 dummy;
			comms->publishEvent("myevent", dummy);
			
			Assert::AreEqual(myVariable, 456);
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
		MessageFactory messageFactory;
		Message querySetValueMessage = messageFactory.create(MessageType::QuerySetValue);
		QuerySetValue querySetValue;
		querySetValue.id = id;
		querySetValue.type = "";
		querySetValue.pack(querySetValueMessage);
		
		pack(querySetValueMessage, 456);
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
		static bool ok = false;
		virtual	void init1(String* name, String __gc * sdml, IComms* comms, ApsimEvents* events)
			{
			comp = this;
			events->registerTickHandler("tick", new Time::EventHandler(this, &RespondToEventComponentStub::onTick));
			
			Int32 dummy;
			comms->publishEvent("myevent", dummy);
			
			Assert::IsTrue(ok);	
			}
		void onTick(Time* tick)
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
		Time* time = new Time;
		time->startday = 1;
		time->endday = 300;

		MessageFactory messageFactory;
		Message eventMessage = messageFactory.create(MessageType::Event);
		Event event;
		event.id = id;
		event.publishedBy = 15;
		event.type = stringToStdString(time->ddml());
		event.pack(eventMessage);
		time->pack(eventMessage);
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


// ----------------------------------------------------
// Test that a component can get an array of structures
// ----------------------------------------------------
__gc class GetArrayOfStructuresComponentStub : public ::IComponent
	{
	public:
		static ::IComponent* comp;
		static bool ok = false;;
		
		virtual	void init1(String* name, String __gc * sdml, IComms* comms, ApsimEvents* events)
			{
			comp = this;
			SoilWaterProfileLayer* profiles = new SoilWaterProfileLayer;
            String* from = comms->getProperty("profiles", *profiles);
			Assert::AreEqual(profiles->Count, 2);
			Assert::AreEqual(profiles->value[0]->thickness, 100);
			Assert::AreEqual(profiles->value[1]->thickness, 200);
			ok = true;
			}
		virtual void init2() { }
	};
	
void __stdcall GetArrayOfStructuresCallback(const unsigned* arg, char* messageBytes)
	{
	Message message(messageBytes);
	ComponentInterface* componentInterface = (ComponentInterface*) arg;
	static unsigned id;
	if (message.type() == MessageType::Register)
		{
		Register reg;
		reg.unpack(message);
		if (reg.kind == 1)	 // get
			id = reg.id;
		}
	else if (message.type() == MessageType::GetValue)
		{

		MessageFactory messageFactory;
		Message returnValueMessage = messageFactory.create(MessageType::ReturnValue);
		ReturnValue returnValue;
		returnValue.compID = 15;
		returnValue.id = id;
		returnValue.type = "";
		returnValue.pack(returnValueMessage);
		SoilWaterProfileLayer* profiles = new SoilWaterProfileLayer(2);
		SoilWaterProfileLayer::Value* value1 = new SoilWaterProfileLayer::Value;
		SoilWaterProfileLayer::Value* value2 = new SoilWaterProfileLayer::Value;
		value1->thickness = 100;		
		value2->thickness = 200;
        profiles->value[0] = value1;
        profiles->value[1] = value2;
		profiles->pack(returnValueMessage);

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
			Assert::IsTrue(Init1ComponentStub::ok);
			}         

		// ------------------------------------------------
		// Test that a component can terminate a simulation
		// ------------------------------------------------
		[Test]
		void TestTerminateSimulationEvent()
			{
			runTest(new TerminateSimulationComponentStub, &TerminateSimulationCallback);
			Assert::IsTrue(TerminateSimulationComponentStub::ok);
			}				
  
		// ----------------------------------------
		// Test that a component can send a line to
		// the summary file
		// ----------------------------------------
		[Test]
		void TestWriteToSummary() 
			{
			runTest(new WriteToSummaryComponentStub, &WriteToSummaryCallback);
			Assert::IsTrue(WriteToSummaryComponentStub::ok);
			}
	
		// ----------------------------------------
		// Test that a component can issue a warning
		// ----------------------------------------
		[Test]
		void TestWarning() 
			{
			runTest(new WarningComponentStub, &WarningCallback);
			Assert::IsTrue(WarningComponentStub::ok);
			}	
			
		// ----------------------------------------
		// Test that a component can get a variable
		// ----------------------------------------
		[Test]
		void TestGetProperty() 
			{
			runTest(new GetPropertyComponentStub, &GetPropertyCallback);
			Assert::IsTrue(GetPropertyComponentStub::ok);
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
			Assert::IsTrue(SetPropertyComponentStub::ok);
			}			

		// ----------------------------------------
		// Test that a component can public an event
		// ----------------------------------------
		[Test]
		void TestPublish() 
			{
			runTest(new PublishComponentStub, &PublishCallback);
			Assert::IsTrue(PublishComponentStub::ok);
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
			
		// ----------------------------------------------------------------
		// Test that a component can get an array of structures.
		// ----------------------------------------------------------------
		[Test]
		void TestGetArrayOfStructures() 
			{
			runTest(new GetArrayOfStructuresComponentStub, &GetArrayOfStructuresCallback);
			Assert::IsTrue(GetArrayOfStructuresComponentStub::ok);
			}			
				 
		};
	
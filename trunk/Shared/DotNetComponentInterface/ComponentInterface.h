#pragma once

#include "interfaces.h"
#include "message.h"
#include "MessageFactory.h"
#include "Utility.h"
#include <map>
#include <vector>

using namespace System;

// ---------------------------------------------------
// This class implements the Common Modelling Protocol
// allowing components to be hooked into APSIM.
// ---------------------------------------------------
class ComponentInterface
	{
	public:
		typedef  void  (__stdcall *CallbackType)(const unsigned *callbackArg, char *message);	
		
		void createInstance(IComponent* component,
							const char* dllFileName,
							unsigned compID,
							unsigned parentID,
							const unsigned* callbackArg,
							CallbackType callback);
		
		
		void deleteInstance(IComponent* component);
		void messageToLogic(IComponent* component, char* message);
							
							
		// ------------------------------------------------------
		// Event methods - Events include initialise and commence
		// ------------------------------------------------------
		void registerEventHandler(const std::string& eventName, const std::string& ddml, IEvent* event);
		void publishEvent(const std::string& eventName, IData& data);

		// -------------------
		// Property methods
		// -------------------
		void registerProperty(const std::string& propertyName, IComms::ReadWriteType readWrite, IData& data);
		std::string getProperty(const std::string& propertyName, IData& data);
		
		void getProperties(const std::string& propertyName, IData* data[]) { }

		bool setProperty(const std::string& propertyName, IData& data);

		// ---------------------------------------
		// Notify system of a warning.
		// Errors should be thrown.
		// ---------------------------------------
		void warning(const std::string& msg) {error(msg, false);}

		// ---------------------
		// Write to summary file
		// ---------------------
		void writeToSummary(const std::string& msg);

		// --------------------
		// Terminate simulation
		// --------------------
		void terminateSimulation(void);
		
									
	private:
		class Registration
			{
			public:
			enum Type {	get=1,         respondToGet=2,
						set=8,         respondToSet=3,
						event=5,       respondToEvent=6,
                        respondToGetSet=4};

				std::string name;
				Type type;
				IData* data;
			};
			
		typedef std::map<std::string, Registration*> Registrations;
		typedef std::vector<Message*> Messages;
		typedef std::map<unsigned, std::string> Components;
		
		Registrations registrations;
		Messages messages;
		Components components;

		MessageFactory messageFactory;	
		std::string name;
		CallbackType callback;
		const unsigned* callbackArg;
		unsigned ourID;
		unsigned parentID;
		bool errorHasOccurred;

		
		void error(const std::string& errorMessage, bool isFatal);
		void onInit1Message(IComponent* component, Message& message);
		void onInit2Message(IComponent* component, Message& message);
		void onQueryValueMessage(IComponent* component, Message& message);
		void onQuerySetValueMessage(IComponent* component, Message& message);
		void onEventMessage(IComponent* component, Message& message);
		
		enum QueryType			{	component=8};
		unsigned nameToRegistrationID(const std::string& name, 
										Registration::Type regType,
										IData& ddml);
		Registration* idToRegistration(unsigned regID);

		std::string componentIDToName(unsigned componentID);
		void clearMessages(void);

		void sendMessage(Message& message, unsigned toID, bool ack);
		void publish(const std::string& eventName, IData& data);
		
	};
	
public __gc class CommsStub : public IComms
	{
	private:
		ComponentInterface* componentInterface;
	
	public:
		CommsStub(ComponentInterface* ci) : componentInterface(ci) { }
		// ------------------------------------------------------
		// Event methods - Events include initialise and commence
		// ------------------------------------------------------
		virtual void registerEventHandler(String* eventName, const std::string& ddml, IEvent* event) 
			{
			componentInterface->registerEventHandler(stringToStdString(eventName), ddml, event);
			};
		virtual void publishEvent(String* eventName, IData& data) 
			{ 
			componentInterface->publishEvent(stringToStdString(eventName), data);
			}
		virtual void publishEvent(String* eventName, IManagedData* managedData) 
			{ 
			IData* data = managedData->data();
			componentInterface->publishEvent(stringToStdString(eventName), *data);
			}

		// -------------------
		// Property methods
		// -------------------
		virtual void registerProperty(String* propertyName, IComms::ReadWriteType readWrite, IData& data) 
			{ 
			componentInterface->registerProperty(stringToStdString(propertyName), readWrite, data);
			}
		virtual void registerProperty(String* propertyName, IComms::ReadWriteType readWrite, IManagedData* managedData) 
			{ 
			IData* data = managedData->data();
			componentInterface->registerProperty(stringToStdString(propertyName), readWrite, *data);
			}
		virtual String* getProperty(String* propertyName, IData& data)
			{
			return componentInterface->getProperty(stringToStdString(propertyName), data).c_str();
			}
		virtual String* getProperty(String* propertyName, IManagedData* managedData)
			{
			IData* data = managedData->data();
			std::string fromComponent = componentInterface->getProperty(stringToStdString(propertyName), *data);
			return new String(fromComponent.c_str());
			}
			
		virtual void getProperties(const std::string& propertyName, IData& data) { }

		virtual bool setProperty(String* propertyName, IData& data)
			{
			return componentInterface->setProperty(stringToStdString(propertyName), data);
			}
		virtual bool setProperty(String* propertyName, IManagedData* managedData)
			{
			IData* data = managedData->data();
			bool ok = componentInterface->setProperty(stringToStdString(propertyName), *data);
			return ok;
			}

		// ---------------------------------------
		// Notify system of a warning.
		// Errors should be thrown.
		// ---------------------------------------
		virtual void warning(String* msg) 
			{ 
			componentInterface->warning(stringToStdString(msg));
			}

		// ---------------------
		// Write to summary file
		// ---------------------
		virtual void writeToSummary(String* line) 
			{ 
			componentInterface->writeToSummary(stringToStdString(line));
			}

		// --------------------
		// Terminate simulation
		// --------------------
		virtual void terminateSimulation(void) 
			{ 
			componentInterface->terminateSimulation();
			}	
	};	

#pragma once
#include <string>
#include "Message.h"
using namespace System;

// ----------------------------------------
// Data interface.
// ----------------------------------------
class IMessage
	{
	public:
		virtual void pack(Message& message) = 0;
		virtual void unpack(Message& message) = 0;
	};	

class IData : public IMessage
	{
	public:
		virtual std::string ddml() = 0;
	};

public __gc class IManagedData
	{
	public:
	    virtual IData* data() = 0;
	};	


// ----------------------------------------
// Handler type
// ----------------------------------------

public __gc class IEvent
	{
	public:
		virtual void invokeEvent(Message& message) = 0;

	
	};
	
	
// ----------------------------------------
// This interface describes how a component
// communicates with other components.
// ----------------------------------------
public __gc class IComms
	{
	public:
		IComms(void) {};
		~IComms(void) {};

		__value enum ReadWriteType {read=1, readWrite=2, write=3};
		
		// ------------------------------------------------------
		// Event methods - Events include initialise and commence
		// ------------------------------------------------------
		virtual void registerEventHandler(String* eventName, const std::string& ddml, IEvent* event) = 0;
		virtual void publishEvent(String* eventName, IData& data) = 0;
		virtual void publishEvent(String* eventName, IManagedData* data) = 0;

		// -------------------
		// Property methods
		// -------------------
		virtual void registerProperty(String* propertyName, ReadWriteType readwrite, IData& data) = 0;
		virtual void registerProperty(String* propertyName, ReadWriteType readwrite, IManagedData* managedData) = 0;
		virtual String* getProperty(String* propertyName, IData& data) = 0;
		virtual String* getProperty(String* propertyName, IManagedData* data) = 0;
		virtual void getProperties(const std::string& propertyName, IData& data) = 0;

		virtual bool setProperty(String* propertyName, IData& data) = 0;
		virtual bool setProperty(String* propertyName, IManagedData* data) = 0;

		// ---------------------------------------
		// Notify system of a warning.
		// Errors should be thrown.
		// ---------------------------------------
		virtual void warning(String* msg) = 0;

		// ---------------------
		// Write to summary file
		// ---------------------
		virtual void writeToSummary(String* msg) = 0;

		// --------------------
		// Terminate simulation
		// --------------------
		virtual void terminateSimulation(void) = 0;

	};
	

	
	
// ---------------------------------------------
// This class is a base class for all components
// ---------------------------------------------
public __gc class ApsimEvents;
public __gc class IComponent
	{
	public:
		virtual void init1(String* name, String* sdml, IComms* comms, ApsimEvents* events) = 0;
		virtual void init2(void) = 0;
		

	};
	
// -----------------------------------
// Create an instance of the component
// -----------------------------------
IComponent* createInstanceOfComponent(const std::string& dllFileName);
	

// This is the main DLL file.

#include "stdafx.h"

#include "ComponentInterface.h"
#include "Message.h"
#include "MessageType.h"
#include "MessageData.h"
#include "DataTypes.h"
#include "ApsimEvents.h"
#include "RegisteredEvents.h"
#include <general\stristr.h>
#include <string.h>
#include <sstream>

#using <System.dll>
using namespace std;

// ---------------------------------------------------------
// This exception class is sent by this component interface
// to signal to messageToLogic that an error has already
// occurred and that execution should just continue.
// It is used to unwind out of nested messages and VB, C# routines.
// It is brought about by not allowing exceptions to escape to
// the ProtocolManager and the rest of the System. The 
// protocol does not allow exceptions across component
// boundaries.
// ---------------------------------------------------------
class error_has_already_occurred : public exception {
public:
   error_has_already_occurred() { };
};

// -------------------------------
// The createInstance entry point
// -------------------------------
void ComponentInterface::createInstance(IComponent* component,
										const char* dllFileName,
										unsigned ourID,
										unsigned parentID,
										const unsigned* callbackArg,
										CallbackType callback)
	{
	this->ourID = ourID;
	this->parentID = parentID;
	this->callback = callback;
	this->callbackArg = callbackArg;
	errorHasOccurred = false;
	}


// ---------------------------------
// The deleteInstance entry point
// ---------------------------------
void ComponentInterface::deleteInstance(IComponent* component)
	{
	
	
	}
	
	
// ------------------------------
// The messageToLogic entry point
// ------------------------------
void ComponentInterface::messageToLogic(IComponent* component,
										char* messageBytes)
	{
	try
		{
		Message message(messageBytes);
		switch (message.type())
			{
			case MessageType::Init1:					onInit1Message(component, message); break;
			case MessageType::Init2:					onInit2Message(component, message); break;
			case MessageType::QueryValue:				onQueryValueMessage(component, message); break;
			case MessageType::QuerySetValue:			onQuerySetValueMessage(component, message); break;
			case MessageType::Event:					onEventMessage(component, message); break;
			case MessageType::ReturnValue:	
			case MessageType::ReturnInfo:
			case MessageType::NotifySetValueSuccess:	messages.push_back(new Message(message)); break;
			} 
		}
	catch (const error_has_already_occurred&)
		{
		// error has already occured - continue execution.
		}
	catch (const exception& err)
		{
		error(err.what(), true);
		}
	catch (NUnit::Framework::AssertionException* )
		{
		throw;
		}
	catch (System::Exception* err)
		{
		error(stringToStdString(err->Message), true);
		}
	}
// --------------------------------------------
// Return a registration id for the specified
// name. If name hasn't been registered then
// use the regType to send a message to our PM.
// --------------------------------------------
unsigned ComponentInterface::nameToRegistrationID(const std::string& name, 
													Registration::Type regType,
													IData& data)
	{
	Registrations::iterator reg = registrations.find(name);
	if (reg == registrations.end())
		{
		Registration* newRegistration = new Registration;
		newRegistration->name = name;
		newRegistration->type = regType;
		newRegistration->data = &data;
		registrations.insert(make_pair(name, newRegistration));
		unsigned newRegId = (unsigned) newRegistration;
		
		Register reg;
		reg.kind = regType;
		reg.id = newRegId;
		reg.destID = parentID;
		reg.name = name;
		reg.type = data.ddml();
		Message message = messageFactory.create(MessageType::Register);
		reg.pack(message);
		sendMessage(message, parentID, false);
		return newRegId;
		}
	else
	   return (unsigned) reg->second;
	}
// --------------------------------------------
// Return a registration name for the specified
// id. If id doesn't exist, throw exception.
// --------------------------------------------
ComponentInterface::Registration* ComponentInterface::idToRegistration(unsigned regID)
	{
	return (Registration*) regID;
	}	
// -----------------------------------
// Converts a component id into a name
// -----------------------------------
std::string ComponentInterface::componentIDToName(unsigned componentID)
	{
	Components::iterator component = components.find(componentID);
	if (component == components.end())
		{
		clearMessages();
		Message message = messageFactory.create(MessageType::QueryInfo);
		QueryInfo queryInfo;
		ostringstream out;
		out << componentID;
		queryInfo.name = out.str();
		queryInfo.kind = ComponentInterface::component;
		queryInfo.pack(message);
		sendMessage(message, parentID, false);
		if (messages.size() == 0)
			throw runtime_error("The system didn't respond to a queryInfo message.");
		ReturnInfo returnInfo;
		returnInfo.unpack(*messages[0]);
		unsigned period = returnInfo.name.find_last_of('.');
		if (period != string::npos)
			returnInfo.name.erase(0, period+1);
		components.insert(make_pair(componentID, returnInfo.name));
		clearMessages();
		return returnInfo.name;
		}
	else
		return component->second;
	}
// -----------------------------------
// Converts a component id into a name
// -----------------------------------
void ComponentInterface::clearMessages()
	{
	for (unsigned i = 0; i != messages.size(); i++)
		delete messages[i];
	messages.erase(messages.begin(), messages.end());
	}

// -----------------------------
// Handler for the Init1 message
// -----------------------------
void ComponentInterface::onInit1Message(IComponent* component, Message& message)
	{	
	Init1 init1;
	init1.unpack(message);
	
	// extract the name of the component.
	unsigned posLastPeriod = init1.fqn.find_last_of('.');
	if (posLastPeriod != string::npos)
		name = init1.fqn.substr(posLastPeriod+1);
	else
		name = init1.fqn;
	
	// we only want to pass the stuff under the <initdata> tag to the component.
	char* posOpenInitData = stristr(init1.sdml.c_str(), "<initdata>");
	char* posCloseInitData = stristr(init1.sdml.c_str(), "</initdata>");
	if (posOpenInitData == NULL || posCloseInitData == NULL)
		throw std::runtime_error("Cannot find <initdata> tag in sim file for component: " + name);
	posCloseInitData += strlen("</initdata>");
	string sdml;
	sdml.assign(posOpenInitData, posCloseInitData-posOpenInitData);
	CommsStub* comms = new CommsStub(this);
	component->init1(name.c_str(), sdml.c_str(), comms, new ApsimEvents(comms));
	}
	
// -----------------------------
// Handler for the Init2 message
// -----------------------------
void ComponentInterface::onInit2Message(IComponent* component, Message& message)
	{
	component->init2();	
	}

	
// -----------------------------------
// Called to signal an error condition
// -----------------------------------
void ComponentInterface::error(const string& errorMessage, bool isFatal)
	{
	Error error;
	error.msg =  "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n";
	if (isFatal)
		error.msg += "                 APSIM  Fatal  Error               \n";
	else
		error.msg += "                 APSIM Warning Error               \n";
	error.msg += "                 -------------------               \n";

	error.msg += errorMessage + "\n";
	error.msg += "Component name: " + name + "\n";
	error.msg += "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\n";
	publish("error", error);
	if (isFatal)
		{
		terminateSimulation();
		errorHasOccurred = true;
		}
	}
	
// --------------------
// Terminate simulation
// --------------------
void ComponentInterface::terminateSimulation(void)
	{
	Message message = messageFactory.create(MessageType::TerminateSimulation);
	sendMessage(message, parentID, false);
	}
	
// --------------------
// Send a message.
// --------------------
void ComponentInterface::sendMessage(Message& message, unsigned toID, bool ack)
	{
	message.setAddress(ourID, toID, ack);
	callback(callbackArg, message.byteStream());
	if (errorHasOccurred)
		throw error_has_already_occurred();
	messageFactory.release();
	}	
	
// ---------------------
// Write to summary file
// ---------------------
void ComponentInterface::writeToSummary(const string& line)
	{
	SummaryFileWrite summaryFileWrite;
	summaryFileWrite.componentName = name;
	summaryFileWrite.lines = line;
	publish("summaryFileWrite", summaryFileWrite);
	}
	
// ---------------------
// Publish an event
// ---------------------
void ComponentInterface::publish(const std::string& eventName, IData& data)
	{
	unsigned id = nameToRegistrationID(eventName, Registration::event, data);
	Message message = messageFactory.create(MessageType::PublishEvent);

	PublishEvent publishEvent;
	publishEvent.id = id;
	publishEvent.type = data.ddml();
	publishEvent.pack(message);
	
	data.pack(message);
	sendMessage(message, parentID, false);
	}
	
// -----------------------------------
// Get a property from another module.
// -----------------------------------
std::string ComponentInterface::getProperty(const std::string& propertyName, IData& data)
	{
	clearMessages();
	unsigned id = nameToRegistrationID(propertyName, Registration::get, data);
	Message message = messageFactory.create(MessageType::GetValue);
	GetValue getValue;
	getValue.id = id;
	getValue.pack(message);
	sendMessage(message, parentID, false);
	if (messages.size() == 1)
		{
		ReturnValue returnValue;
		returnValue.unpack(*messages[0]);
		if (id != returnValue.id)
			throw runtime_error("Invalid returnValue id");
		data.unpack(*messages[0]);	
		return componentIDToName(returnValue.compID);
		}
	else if (messages.size() == 0)
		throw runtime_error("No component responded to a 'get' for variable: " + propertyName);
	else
		throw runtime_error("Too many components responded to a 'get' for variable: " + propertyName);
	
	}

// ------------------------------------------------------
// Set the value of a variable in another component.
// Return true if successful, false and a warning message
// otherwise.
// ------------------------------------------------------
bool ComponentInterface::setProperty(const std::string& propertyName, IData& data)
	{
	unsigned id = nameToRegistrationID(propertyName, Registration::set, data);
	Message message = messageFactory.create(MessageType::RequestSetValue);
	RequestSetValue setValue;
	setValue.id = id;
	setValue.type = data.ddml();
	setValue.pack(message);
	data.pack(message);
	sendMessage(message, parentID, false);
	bool ok = false;
	if (messages.size() == 1 && messages[0]->type() == MessageType::NotifySetValueSuccess)
		{
		NotifySetValueSuccess notifySetValue;
		notifySetValue.unpack(*messages[0]);
		if (id != notifySetValue.id)
			throw runtime_error("Invalid notifySetValueSuccess id");
		ok = notifySetValue.success;
		}
	else if (messages.size() > 1)
		throw runtime_error("Too many components responded to a 'set' for variable: " + propertyName);
	if (!ok)
		warning("No component responded to a 'set' for variable: " + propertyName); 	
	return ok;
	}
	
// ------------------------------------------------------
// Publish an event with the specified data.
// ------------------------------------------------------
void ComponentInterface::publishEvent(const std::string& eventName, IData& data)
	{
	unsigned id = nameToRegistrationID(eventName, Registration::event, data);
	Message message = messageFactory.create(MessageType::PublishEvent);
	PublishEvent publishEvent;
	publishEvent.id = id;
	publishEvent.type = data.ddml();
	publishEvent.pack(message);
	data.pack(message);
	sendMessage(message, parentID, false);
	}

// ---------------------------------------------------------
// Register the specified data so that other components can
// get the data.
// ---------------------------------------------------------		
void ComponentInterface::registerProperty(const std::string& propertyName, 
										  IComms::ReadWriteType readWrite, 
										  IData& data)
	{
	Registration::Type regType;
	if (readWrite == IComms::read)
	   regType = Registration::respondToGet;
	else if (readWrite == IComms::write)
		regType = Registration::respondToSet;
	else
		regType = Registration::respondToGetSet;
	unsigned regID = nameToRegistrationID(propertyName, regType, data);
	}

// ---------------------------------------------------------
// Another component has asked for one of our variables.
// Return the value to caller by sending a replyValue message.
// ---------------------------------------------------------		
void ComponentInterface::onQueryValueMessage(IComponent* component, Message& message)
	{
	QueryValue queryValue;
	queryValue.unpack(message);
	IData* data = idToRegistration(queryValue.id)->data;
	
	Message replyMessage = messageFactory.create(MessageType::ReplyValue);
	ReplyValue replyValue;
	replyValue.queryID = queryValue.id;
	replyValue.type = data->ddml();
	replyValue.pack(replyMessage);
	data->pack(replyMessage);
	sendMessage(replyMessage, parentID, false);
	}
// ---------------------------------------------------------
// Another component wants to change one  of our variables.
// Return the true/false to caller by sending a notifySetValueSuccess message.
// ---------------------------------------------------------		
void ComponentInterface::onQuerySetValueMessage(IComponent* component, Message& message)
	{
	QuerySetValue querySetValue;
	querySetValue.unpack(message);
	Registration* registration = idToRegistration(querySetValue.id);
	if (registration->type == Registration::respondToSet || registration->type == Registration::respondToGetSet)
		{
		registration->data->unpack(message);
		
		Message notifySetValueSuccessMessage = messageFactory.create(MessageType::NotifySetValueSuccess);
		NotifySetValueSuccess notifySetValueSuccess;
		notifySetValueSuccess.id = querySetValue.replyID;
		notifySetValueSuccess.success = true;
		notifySetValueSuccess.pack(notifySetValueSuccessMessage);
		sendMessage(notifySetValueSuccessMessage, querySetValue.replyToID, false);
		}
	}

// ---------------------------------------------------------		
// Register an event handler.
// ---------------------------------------------------------		
void ComponentInterface::registerEventHandler(const std::string& eventName, const std::string& ddml, IEvent* event)
	{
	Register reg;
	reg.kind = Registration::respondToEvent;
	reg.id = RegisteredEventHandlers::add(event);
	reg.destID = parentID;
	reg.name = eventName;
	reg.type = ddml;
	Message message = messageFactory.create(MessageType::Register);
	reg.pack(message);
	sendMessage(message, parentID, false);
	}
// ---------------------------------------------------------		
// Respond to an incoming event.
// ---------------------------------------------------------		
void ComponentInterface::onEventMessage(IComponent* component, Message& message)
	{
	Event event;
	event.unpack(message);
	IEvent* eventHandler = RegisteredEventHandlers::at(event.id);
	eventHandler->invokeEvent(message);
	}

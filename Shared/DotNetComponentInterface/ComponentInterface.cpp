// This is the main DLL file.

#include "stdafx.h"

#using <VBGeneral.dll>
#include "ComponentInterface.h"
#include "Message.h"
#include "MessageType.h"
#include "MessageData.h"
#include "Messages.h"
#include "DataTypes.h"
#include "ApsimEvents.h"
#include "RegisteredEvents.h"
#include "ApsimComponent.h"
#include "TypeConverter.h"
#include "../general/stristr.h"
#include <string.h>
#include <sstream>

#using <System.dll>
using namespace std;
using namespace ComponentInterface;
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
void ComponentComms::createInstance(ApsimComponent^ component,
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
void ComponentComms::deleteInstance(ApsimComponent^ component)
	{
	
	
	}
	
	
// ------------------------------
// The messageToLogic entry point
// ------------------------------
void ComponentComms::messageToLogic(ApsimComponent^ component,
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
			case MessageType::NotifySetValueSuccess:
			case MessageType::ReplySetValueSuccess:		messages.push_back(new Message(message)); break;
			} 
		}
	catch (const exception& err)
		{
		error(err.what(), true);
		}
	catch (System::Exception^ err)
		{
		error(stringToStdString(err->Message), true);
		}
	}
// --------------------------------------------
// Return a registration id for the specified
// name. If name hasn't been registered then
// use the regType to send a message to our PM.
// --------------------------------------------
unsigned ComponentComms::nameToRegistrationID(const std::string& name, 
											  const std::string& units,
													Registration::Type regType,
													IData& data)
	{
	ostringstream regTypeString;
	regTypeString << regType << ends;
	string FullRegName = name + regTypeString.str();
	Registrations::iterator reg = registrations.find(FullRegName);
	if (reg == registrations.end())
		{
		Registration* newRegistration = new Registration;
		newRegistration->name = FullRegName;
		newRegistration->type = regType;
		newRegistration->data = &data;
		registrations.insert(make_pair(FullRegName, newRegistration));
		unsigned newRegId = (unsigned) newRegistration;
		
		Register reg;
		reg.kind = regType;
		reg.id = newRegId;
		reg.destID = 0;
		reg.name = name;
		reg.type = data.ddml();
		//if (units != "")
		//	reg.type = reg.type.substr(0, reg.type.length()-2) + " units=\"" + units + "\"/>";
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
ComponentComms::Registration* ComponentComms::idToRegistration(unsigned regID)
	{
	return (Registration*) regID;
	}	
// -----------------------------------
// Converts a component id into a name
// -----------------------------------
std::string ComponentComms::componentIDToName(unsigned componentID)
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
		queryInfo.kind = ComponentComms::component;
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
void ComponentComms::clearMessages()
	{
	for (unsigned i = 0; i != messages.size(); i++)
		delete messages[i];
	messages.erase(messages.begin(), messages.end());
	}

// -----------------------------
// Handler for the Init1 message
// -----------------------------
void ComponentComms::onInit1Message(ApsimComponent^ component, Message& message)
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
	component->Setup(this, gcnew String(name.c_str()), gcnew String(sdml.c_str()));
	component->Init1();
	}
	
// -----------------------------
// Handler for the Init2 message
// -----------------------------
void ComponentComms::onInit2Message(ApsimComponent^ component, Message& message)
	{
	component->Init2();	
	}

	
// -----------------------------------
// Called to signal an error condition
// -----------------------------------
void ComponentComms::error(const string& errorMessage, bool isFatal)
	{
	string msg;
	msg =  "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n";
	if (isFatal)
		msg += "                 APSIM  Fatal  Error               \n";
	else
		msg += "                 APSIM Warning Error               \n";
	msg += "                 -------------------               \n";

	msg += errorMessage + "\n";
	msg += "Component name: " + name + "\n";
	msg += "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\n";

	Error^ error = gcnew Error;
	error->msg = gcnew String(msg.c_str());
	error->isFatal = isFatal;
	publish("error", WrapManaged<Error^>(error));
	if (isFatal)
		{
		terminateSimulation();
		errorHasOccurred = true;
		}
	}
	
// --------------------
// Terminate simulation
// --------------------
void ComponentComms::terminateSimulation(void)
	{
	Message message = messageFactory.create(MessageType::TerminateSimulation);
	sendMessage(message, parentID, false);
	}
	
// --------------------
// Send a message.
// --------------------
void ComponentComms::sendMessage(Message& message, unsigned toID, bool ack)
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
void ComponentComms::writeToSummary(const string& line)
	{
	SummaryFileWrite^ summaryFileWrite = gcnew SummaryFileWrite;
	summaryFileWrite->componentName = gcnew String(name.c_str());
	summaryFileWrite->lines = gcnew String(line.c_str());
	publish("summaryFileWrite", WrapManaged<SummaryFileWrite^>(summaryFileWrite));
	}
	
// ---------------------
// Publish an event
// ---------------------
void ComponentComms::publish(const std::string& eventName, IData& data)
	{
	unsigned id = nameToRegistrationID(eventName, "", Registration::event, data);
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
std::string ComponentComms::getProperty(const std::string& propertyName, 
										const std::string& units,
										IData& data)
	{
	clearMessages();
	unsigned id = nameToRegistrationID(propertyName, units, Registration::get, data);
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

		TypeConverter Converter;
		Converter.unpack(*messages[0], returnValue.type, &data);
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
bool ComponentComms::setProperty(const std::string& propertyName, 
								const std::string& units,
								IData& data)
	{
	messages.erase(messages.begin(), messages.end());
	unsigned id = nameToRegistrationID(propertyName, units, Registration::set, data);
	Message message = messageFactory.create(MessageType::RequestSetValue);
	RequestSetValue setValue;
	setValue.id = id;
	setValue.type = data.ddml();
	setValue.pack(message);
	data.pack(message);
	sendMessage(message, parentID, false);
	bool ok = false;
	if (messages.size() == 1 && (messages[0]->type() == MessageType::NotifySetValueSuccess || 
								 messages[0]->type() == MessageType::ReplySetValueSuccess))
		{
		NotifySetValueSuccess notifySetValue;
		notifySetValue.unpack(*messages[0]);
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
void ComponentComms::publishEvent(const std::string& eventName, IData* data)
	{
	unsigned id = nameToRegistrationID(eventName, "", Registration::event, *data);
	Message message = messageFactory.create(MessageType::PublishEvent);
	PublishEvent publishEvent;
	publishEvent.id = id;
	publishEvent.type = data->ddml();
	publishEvent.pack(message);
	data->pack(message);
	sendMessage(message, parentID, false);
	}

// ---------------------------------------------------------
// Register the specified data so that other components can
// get the data.
// ---------------------------------------------------------		
void ComponentComms::registerProperty(const std::string& propertyName, 
									  const std::string& units,
										  int readWrite, 
										  IData* data)
	{
	Registration::Type regType;
	if (readWrite == read)
	   regType = Registration::respondToGet;
	else if (readWrite == write)
		regType = Registration::respondToSet;
	else
		regType = Registration::respondToGetSet;
	unsigned regID = nameToRegistrationID(propertyName, units, regType, *data);
	}

// ---------------------------------------------------------
// Another component has asked for one of our variables.
// Return the value to caller by sending a replyValue message.
// ---------------------------------------------------------		
void ComponentComms::onQueryValueMessage(ApsimComponent^ component, Message& message)
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
void ComponentComms::onQuerySetValueMessage(ApsimComponent^ component, Message& message)
	{
	QuerySetValue querySetValue;
	querySetValue.unpack(message);
	Registration* registration = idToRegistration(querySetValue.id);
	if (registration->type == Registration::respondToSet || registration->type == Registration::respondToGetSet)
		{
		TypeConverter Converter;
		Converter.unpack(message, querySetValue.type, registration->data);
				
		Message notifySetValueSuccessMessage = messageFactory.create(MessageType::NotifySetValueSuccess);
		NotifySetValueSuccess notifySetValueSuccess;
		notifySetValueSuccess.id = message.msgID();
		notifySetValueSuccess.success = true;
		notifySetValueSuccess.pack(notifySetValueSuccessMessage);
		sendMessage(notifySetValueSuccessMessage, message.fromID(), false);
		}
	}

// ---------------------------------------------------------		
// Register an event handler.
// ---------------------------------------------------------		
void ComponentComms::registerEventHandler(const std::string& eventName, IEventData* event)
	{
	Register reg;
	reg.kind = Registration::respondToEvent;
	reg.id = RegisteredEventHandlers::add(event);
	reg.destID = parentID;
	reg.name = eventName;
	reg.type = event->ddml();
	Message message = messageFactory.create(MessageType::Register);
	reg.pack(message);
	sendMessage(message, parentID, false);
	}
// ---------------------------------------------------------		
// Respond to an incoming event.
// ---------------------------------------------------------		
void ComponentComms::onEventMessage(ApsimComponent^ component, Message& message)
	{
	Event event;
	event.unpack(message);
	IEventData* eventHandler = RegisteredEventHandlers::at(event.id);
	eventHandler->unpack(message);
	eventHandler->invokeEvent(message);
	}

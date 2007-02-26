#pragma hdrstop

#include <iostream>

#include "CMPComponentInterface.h"
#include <ComponentInterface2/datatypes.h>
#include <ComponentInterface2/CMPData.h>
#include <ComponentInterface2/CMPScienceAPI.h>
#include <general/xml.h>
#include <general/stl_functions.h>
#include <general/string_functions.h>
#include <general/dll.h>


string componentType = "APSRU";
string version = "2.0";
string author = "APSRU";
int active = 1;
string state = "";

// =============================================================================
// =============================================================================
// =============================================================================
CMPComponentInterface::CMPComponentInterface(unsigned* callbackarg, CallbackType* callback,
                                             unsigned componentid, unsigned parentid)
   // -----------------------------------------------------------------------
   // constructor
   // -----------------------------------------------------------------------
   {
   callbackArg = callbackarg;
   messageCallback = callback;
   componentID = componentid;
   parentID = parentid;
   initMessageFactory();
   errorHasOccurred = false;
   simScript = NULL;
   init1 = NULL;
   init2 = NULL;
   }

CMPComponentInterface::~CMPComponentInterface()
   // -----------------------------------------------------------------------
   // destructor
   // -----------------------------------------------------------------------
   {
   shutDownMessageFactory();
   for (NameToRegMap::iterator i = regNames.begin();
                               i != regNames.end();
                               i++)
      delete i->second;

   clearMessages();
   delete simScript;
   }


void CMPComponentInterface::messageToLogic(const Message& message)
   // -----------------------------------------------------------------------
   // Called for all incoming messages.
   // -----------------------------------------------------------------------
   {
   // We need to keep track of bits of the message because the FARMWI$E infrastructure
   // doesn't guarantee that a message is still valid at the end of this method.
   // eg. it deletes the Init1 message before we get to test the ack flag at the bottom.
   bool ack = message.toAcknowledge;
   //unsigned fromID = message.from;
   unsigned msgID = message.messageID;

	try
		{
		switch (message.messageType)
			{
         case Message::Event:                         onEvent(message); break;
         case Message::Init1:                         onInit1(message); break;
         case Message::Init2:                         onInit2(message); break;
			case Message::QueryValue:                    onQueryValue(message); break;
			case Message::QuerySetValue:                 onQuerySetValue(message); break;
			case Message::ReturnValue:
			case Message::ReturnInfo:
			case Message::NotifySetValueSuccess:
			case Message::ReplySetValueSuccess:		      messages.push_back(cloneMessage(message)); break;
			}

      // if acknowledgement is required, then give it.
      if (ack)
         {
         Complete complete;
         complete.ackID = msgID;
         sendMessage(newMessage(Message::Complete, componentID, parentID, false, complete));
         }

		}
	catch (const exception& err)
		{
		error(err.what(), true);
		}
   }

bool CMPComponentInterface::get(const std::string& name, const std::string& units, bool optional, IPackableData* data)
   // -----------------------------------------------------------------------
   // Get the value of a variable from another component.
   // -----------------------------------------------------------------------
   {
	clearMessages();

   // see if we have an array specifier.
   ArraySpecifier* arraySpecifier = ArraySpecifier::create(name);
   string nameWithoutArraySpec = name;
   if (arraySpecifier != NULL)
      nameWithoutArraySpec = arraySpecifier->variableName();

   int id = nameToRegistrationID(nameWithoutArraySpec, getReg);
   bool alreadyRegistered = (id != 0);
   if (!alreadyRegistered)
      id = RegisterWithPM(nameWithoutArraySpec, units, "", getReg, data);

	GetValue getValue;
	getValue.ID = id;
   sendMessage(newMessage(Message::GetValue, componentID, parentID, false,
                          getValue));

   string errorMsg;
   if (messages.size() == 0)
		errorMsg = "No component responded to a 'get' for variable: " + name;

	else if (messages.size() == 1)
      {
		ReturnValue returnValue;
      MessageData returnMessageData(*messages[0]);
		unpack(returnMessageData, returnValue);
      data->unpack(returnMessageData, returnValue.ddml, arraySpecifier);
      }

   else if (messages.size() > 1)
      errorMsg = "Too many components responded to a 'get' for variable: " + name;

   if (alreadyRegistered)
      delete data;

   delete arraySpecifier;
   if (errorMsg != "")
      {
      if (!optional)
         throw runtime_error(errorMsg);
      return false;
      }
   else
      return true;
   }

void CMPComponentInterface::set(const std::string& name,
                                const std::string& units,
                                IPackableData* data)
   {
   // -----------------------------------------------------------------------
   // Set the value of a variable in another component.
   // -----------------------------------------------------------------------
	clearMessages();
   int id = nameToRegistrationID(name, getReg);
   if (id == 0)
      id = RegisterWithPM(name, units, "", setReg, data);

	RequestSetValue requestSetValue;
	requestSetValue.ID = id;
	requestSetValue.ddml = data->ddml;

	Message& requestSetValueMessage = constructMessage(Message::RequestSetValue, componentID, parentID, false,
                                                      memorySize(requestSetValue) + data->memorySize());
   MessageData requestSetValueMessageData(requestSetValueMessage);
   pack(requestSetValueMessageData, requestSetValue);
   data->pack(requestSetValueMessageData);
	sendMessage(requestSetValueMessage);
   }

bool CMPComponentInterface::read(const std::string& parName, IPackableData& value, bool optional)
   {
   // -----------------------------------------------------------------------
   // Read a parameter from the initial SIM script.
   // -----------------------------------------------------------------------

   XMLNode::iterator initdata = find_if(simScript->documentElement().begin(),
                                        simScript->documentElement().end(),
                                        EqualToName<XMLNode>("initdata"));
   if (initdata == simScript->documentElement().end())
      throw runtime_error("Cannot find <initdata> element for component: " + name);

   vector<string> values;
   for_each_if(initdata->begin(), initdata->end(),
               GetValueFunction<vector<string>, XMLNode>(values),
               EqualToName<XMLNode>(parName));
   if (values.size() >= 1)
      {
      value.setValue(values);
      return true;
      }
   else if (!optional)
      throw runtime_error("Cannot read parameter: " + parName + " for module: " + name);
   return false;
   }

void CMPComponentInterface::expose(const std::string& name,
                                   const std::string& units,
                                   const std::string& description,
                                   bool writable,
                                   IPackableData* variable)
   {
   // -----------------------------------------------------------------------
   // Export a variable. The variable passed in is stored directly
   // in our map so the assumption is that we are now owners.
   // ie. don't delete this data object elsewhere!
   // -----------------------------------------------------------------------
   RegistrationKind kind = respondToGetReg;
   if (writable)
      kind = respondToGetSetReg;
   if (nameToRegistrationID(name, respondToGetReg) == 0)
      RegisterWithPM(name, units, description, kind, variable);
   else
      delete variable;
   }

void CMPComponentInterface::subscribe(const std::string& name, IPackableData* handler)
   // -----------------------------------------------------------------------
   // Subscribe to an event
   // -----------------------------------------------------------------------
   {
   if (Str_i_Eq(name, "init1"))
      init1 = dynamic_cast<CMPMethod0*> (handler);
   else if (Str_i_Eq(name, "init2"))
      init2 = dynamic_cast<CMPMethod0*> (handler);

   else if (nameToRegistrationID(name, respondToEventReg) == 0)
      RegisterWithPM(name, "", "", respondToEventReg, handler);
   else
      delete handler;
   }

void CMPComponentInterface::publish(const std::string& name, IPackableData* data)
   // -----------------------------------------------------------------------
   // Publish an event
   // -----------------------------------------------------------------------
   {
   int id = nameToRegistrationID(name, respondToEventReg);
   bool alreadyRegistered = (id != 0);
   if (!alreadyRegistered)
      id = RegisterWithPM(name, "", "", eventReg, data);

	PublishEvent publishEvent;
	publishEvent.ID = id;
	publishEvent.ddml = data->ddml;

	Message& publishEventMessage = constructMessage(Message::PublishEvent, componentID, parentID, false,
                                                   memorySize(publishEvent) + data->memorySize());
   MessageData publishEventMessageData(publishEventMessage);
   pack(publishEventMessageData, publishEvent);
   data->pack(publishEventMessageData);
	sendMessage(publishEventMessage);

   if (alreadyRegistered)
      delete data;
   }

void CMPComponentInterface::query(const std::string& pattern, std::vector<QueryMatch>& matches)
   {
   // -----------------------------------------------------------------------
   // Return a list of all variables or components (fully qualified)
   // that match the specified pattern.
   // e.g. * will return a list of all components.
   //      wheat.* will return a list of all variables for the wheat module
   //      *.lai will return a list of all lai variables for all modules.
   // -----------------------------------------------------------------------

   clearMessages();

   // the report component sometimes passes through an array specifier
   // e.g. tt_tot() - need to remove that before sending message to PM.
   ArraySpecifier* arraySpecifier = ArraySpecifier::create(pattern);
   string nameWithoutArraySpec = pattern;
   if (arraySpecifier != NULL)
      nameWithoutArraySpec = arraySpecifier->variableName();

   QueryInfo queryInfo;
   queryInfo.name = nameWithoutArraySpec;

   // work out if we dealing with a list of components or a list of variables.
   unsigned posPeriod = pattern.find('.');
   if (posPeriod != string::npos)
      queryInfo.kind = 2;
   else
      queryInfo.kind = 7;
   sendMessage(newMessage(Message::QueryInfo, componentID, parentID, false, queryInfo));

   matches.erase(matches.begin(), matches.end());
	for (unsigned i = 0; i != messages.size(); i++)
      {
		ReturnInfo returnInfo;
      MessageData returnInfoData(*messages[i]);
		unpack(returnInfoData, returnInfo);
      QueryMatch queryMatch;
      queryMatch.name = returnInfo.name;
      if (arraySpecifier != NULL)
         arraySpecifier->adornVariableName(queryMatch.name);
      queryMatch.ddml = returnInfo.type;
      matches.push_back(queryMatch);
      }
   delete arraySpecifier;
   }

void CMPComponentInterface::write(const std::string& msg)
   {
   // -----------------------------------------------------------------------
   // write a message to the summary stream.
   // -----------------------------------------------------------------------

   cout <<  msg;
   }


void CMPComponentInterface::clearMessages()
	{
   // -----------------------------------------------------------------------
   // Clear all messages in our message list.
   // -----------------------------------------------------------------------
	for (unsigned i = 0; i != messages.size(); i++)
		deleteClonedMessage(messages[i]);
	messages.erase(messages.begin(), messages.end());
	}

void CMPComponentInterface::sendMessage(Message& message)
   {
   // -----------------------------------------------------------------------
   // Send a message to PM
   // -----------------------------------------------------------------------
   if (messageCallback != NULL)
      (*messageCallback)(callbackArg, message);
   deleteMessage(message);
   }


int CMPComponentInterface::nameToRegistrationID(const std::string& name,
                                                RegistrationKind regKind)
   // -----------------------------------------------------------------------
   // Return a registration id for the specified
   // name.
   // -----------------------------------------------------------------------
   {
   string FullRegName = name + itoa(regKind);
   NameToRegMap::iterator reg = regNames.find(FullRegName);
   if (reg == regNames.end())
      return 0;
   else
      return (int) reg->second;
   }

int CMPComponentInterface::RegisterWithPM(const string& name, const string& units,
                                          const string& description,
                                          RegistrationKind regKind,
                                          IPackableData* data)
   // -----------------------------------------------------------------------
   // Register something with our PM given the specified information.
   // The data passed in is stored directly in our map so the assumption
   // is that we are now owners.
   // ie. don't delete this data object elsewhere!
   // -----------------------------------------------------------------------
   {
   if (units != "")
      addAttributeToXML(data->ddml, "unit=\"" + units + "\"");
   if (description != "")
      addAttributeToXML(data->ddml, "description=\"" + description + "\"");

   // Add new object to our map.
   string fullRegName = name + itoa(regKind);
   int ID = (int) data;
   regNames.insert(make_pair(fullRegName, data));

   // send register message to PM.
   Register registerData;
   registerData.kind = regKind;
   registerData.ID = ID;
   registerData.destID = 0;
   registerData.name = name;
   registerData.ddml = data->ddml;
   sendMessage(newMessage(Message::Register, componentID, parentID, false,
                          registerData));
   return ID;
   }


void CMPComponentInterface::error(const string& errorMessage, bool isFatal)
   // -----------------------------------------------------------------------
   // Called to signal an error to the PM.
   // -----------------------------------------------------------------------
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

	Error errorData;
	errorData.msg = msg;
	errorData.isFatal = isFatal;
/*	publish("error", errorData);
	if (isFatal)
		{
		terminateSimulation();
		errorHasOccurred = true;
		}
*/	}

void CMPComponentInterface::onInit1(const Message& message)
   // -----------------------------------------------------------------------
   // Handler for Init1 message.
   // -----------------------------------------------------------------------
   {
   MessageData messageData(message);
   Init1 init1;
   unpack(messageData, init1);
   // get instance name from fqn.
   unsigned posPeriod = init1.fqn.rfind('.');
   if (posPeriod == string::npos)
      name = init1.fqn;
   else
      {
      name = init1.fqn.substr(posPeriod+1);
      parentName = init1.fqn.substr(0, posPeriod);
      }
   expose("name", "", "", false, new CMPBuiltIn<string>(name));
   expose("type", "", "", false, new CMPBuiltIn<string>(componentType));
   expose("version", "", "", false, new CMPBuiltIn<string>(version));
   expose("author", "", "", false, new CMPBuiltIn<string>(author));
   expose("active", "", "", false, new CMPBuiltIn<int>(active));
   expose("state", "", "", false, new CMPBuiltIn<string>(state));

   simScript = new XMLDocument(init1.sdml, XMLDocument::xmlContents);
   if (this->init1 != NULL)
      this->init1->invoke();
   }

void CMPComponentInterface::onInit2(const Message& message)
   // -----------------------------------------------------------------------
   // Handler for Init2 message.
   // -----------------------------------------------------------------------
   {
   if (init2 != NULL)
      init2->invoke();
   }

void CMPComponentInterface::onQueryValue(const Message& message)
   // -----------------------------------------------------------------------
   // Handler for all QueryValue messages.
   // -----------------------------------------------------------------------
   {
   MessageData messageData(message);
	QueryValue queryValue;
   unpack(messageData, queryValue);
   IPackableData& data = *(IPackableData*) queryValue.ID;

	ReplyValue replyValue;
	replyValue.queryID = message.messageID;
	replyValue.ddml = data.ddml;

	Message& replyValueMessage = constructMessage(Message::ReplyValue, componentID, parentID, false,
                                                 memorySize(replyValue) + data.memorySize());
   MessageData replyValueMessageData(replyValueMessage);
   pack(replyValueMessageData, replyValue);
   data.pack(replyValueMessageData);
	sendMessage(replyValueMessage);
   }

void CMPComponentInterface::onQuerySetValue(const Message& message)
   // -----------------------------------------------------------------------
   // Handler for all QuerySetValue messages.
   // -----------------------------------------------------------------------
   {
   MessageData messageData(message);
	QuerySetValue querySetValue;
   unpack(messageData, querySetValue);
   IPackableData& data = *(IPackableData*) querySetValue.ID;

   // change the value of the variable.
   data.unpack(messageData, querySetValue.ddml, NULL);

   // now send back a NotifySetValueSuccess message.
   NotifySetValueSuccess notifySetValueSuccess;
   notifySetValueSuccess.ID = querySetValue.ID;
   notifySetValueSuccess.success = true;
   sendMessage(newMessage(Message::NotifySetValueSuccess, componentID, parentID, false,
                          notifySetValueSuccess));
   }
void CMPComponentInterface::onEvent(const Message& message)
   // -----------------------------------------------------------------------
   // Handler for all Event messages.
   // -----------------------------------------------------------------------
   {
   MessageData messageData(message);
	Event event;
   unpack(messageData, event);
   IPackableData& data = *(IPackableData*) event.ID;

   // unpack the data - this will unpack and then call the function.
   data.unpack(messageData, event.ddml, NULL);
   }


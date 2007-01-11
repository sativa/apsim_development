//---------------------------------------------------------------------------
#pragma hdrstop

#include "TestVariables.h"
#include "../DataTypes.h"
#include "../CMPScienceAPI.h"
#include "../CMPComponentInterface.h"
#include "../MessageData.h"
#include <boost/test/unit_test.hpp>

using namespace boost::unit_test_framework;

CMPComponentInterface* componentInterface;
ScienceAPI* scienceAPI;
unsigned messageArg = 123;
int parentID = 0;
int componentID = 1;
vector<Message*> messages;
vector<MessageData> messagesSent;

//---------------------------------------------------------------------------
// Simple helper function for sending a message.
//---------------------------------------------------------------------------
void sendMessage(Message& message)
   {
   componentInterface->messageToLogic(message);
   deleteMessage(message);
   }


//---------------------------------------------------------------------------
// Generic callback routine that a component will call when it want's to
// send a message.
//---------------------------------------------------------------------------

void STDCALL PMCallback(const unsigned* arg, Message& message)
   {
   static string lastRegisteredName;

   BOOST_ASSERT(*arg == messageArg);
   BOOST_ASSERT(message.version == 256);
   BOOST_ASSERT(message.from == componentID);
   BOOST_ASSERT(message.to == parentID);
   BOOST_ASSERT(message.toAcknowledge == false);
   messages.push_back(cloneMessage(message));
   messagesSent.push_back(MessageData(*messages[messages.size()-1]));

   // if component wants the value of a variable then give it one.
   if (message.messageType == Message::GetValue)
      {
      GetValue getValue;
      MessageData messageData(message);
      unpack(messageData, getValue);

      // send a returnValue back to component.
      ReturnValue returnValue;
      returnValue.compID = 10;
      returnValue.ID = getValue.ID;
      returnValue.ddml = "<type kind=\"single\" array=\"T\" unit=\"mm\"/>";
      vector<float> values;
      if (lastRegisteredName == "sw")
         {
         values.push_back(11);
         values.push_back(22);
         values.push_back(33);
         values.push_back(44);
         Message& returnValueMsg = constructMessage(Message::ReturnValue, parentID, componentID, false,
                                                   memorySize(returnValue) + memorySize(values));
         MessageData returnValueData(returnValueMsg);
         pack(returnValueData, returnValue);
         pack(returnValueData, values);
         sendMessage(returnValueMsg);
         }
      }
   else if (message.messageType == Message::Register)
      {
      Register registerData;
      MessageData messageData(message);
      unpack(messageData, registerData);
      lastRegisteredName = registerData.name;
      }
   else if (message.messageType == Message::QueryInfo)
      {
      QueryInfo queryInfo;
      MessageData messageData(message);
      unpack(messageData, queryInfo);
      ReturnInfo returnInfo;
      returnInfo.name = "comp1.sw";
      returnInfo.type = "<type kind=\"single\" array=\"T\" unit=\"mm\"/>";
      sendMessage(newMessage(Message::ReturnInfo, parentID, componentID, false, returnInfo));

      returnInfo.name = "comp2.sw";
      sendMessage(newMessage(Message::ReturnInfo, parentID, componentID, false, returnInfo));
      }
   }

//---------------------------------------------------------------------------
// setup the component/PM testbed.
//---------------------------------------------------------------------------
void setup()
   {
   componentInterface = new CMPComponentInterface(&messageArg, &PMCallback, componentID, parentID);
   scienceAPI = new CMPScienceAPI(*componentInterface);
   }

//---------------------------------------------------------------------------
// tear down the component/PM testbed.
//---------------------------------------------------------------------------
void teardown()
   {
   delete componentInterface;
   delete scienceAPI;
   for (unsigned i = 0; i != messages.size(); i++)
      deleteClonedMessage(messages[i]);
   messages.erase(messages.begin(), messages.end());
   messagesSent.erase(messagesSent.begin(), messagesSent.end());
   }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
// UseCase: Expose a single variable "rain" and make sure it's value is
//          returned to the PM in response to a QueryValue.
//---------------------------------------------------------------------------
void ExposeVariableAndReturnValue()
   {
   setup();

   float component2Rain = 456.0;
   scienceAPI->expose("rain", "mm", "Rainfall", false, component2Rain);

   // make sure registration happened.
   Register registerData;
   unpack(messagesSent[0], registerData);
   BOOST_ASSERT(registerData.destID == 0);
   BOOST_ASSERT(registerData.name == "rain");
   BOOST_ASSERT(registerData.kind == 2 /*respondToGet*/);
   BOOST_ASSERT(registerData.ddml == "<type kind=\"single\" unit=\"mm\" description=\"Rainfall\"/>");

   // ask our component the value of rain.
   QueryValue queryValue;
   queryValue.ID = registerData.ID;
   queryValue.requestedByID = 1234;
   Message& queryValueMsg = newMessage(Message::QueryValue, parentID, componentID, false, queryValue);
   sendMessage(queryValueMsg);

   // check that the component gave us the correct ReplyValue message.
   ReplyValue replyValue;
   unpack(messagesSent[1], replyValue);
   BOOST_ASSERT(replyValue.queryID == queryValueMsg.messageID);
   BOOST_ASSERT(replyValue.ddml == "<type kind=\"single\" unit=\"mm\" description=\"Rainfall\"/>");
   float rain;
   unpack(messagesSent[1], rain);
   BOOST_ASSERT(rain == 456);

   teardown();
   }

//---------------------------------------------------------------------------
// UseCase: Expose a single variable "rain" twice but ensure only 1
//          register message is sent from component.
//---------------------------------------------------------------------------
void ExposeVariableTwice()
   {
   setup();

   int component2Rain = 456;
   scienceAPI->expose("rain", "mm", "Rainfall", false, component2Rain);
   scienceAPI->expose("rain", "mm", "Rainfall", false, component2Rain);
   BOOST_ASSERT(messagesSent.size() == 1);

   teardown();
   }

//---------------------------------------------------------------------------
// UseCase: Expose a vector<float> variable "sw" using a getter function
//          and make sure it's value is returned to the PM in response
//          to a QueryValue.
//---------------------------------------------------------------------------
class SWClass
   {
   private:
      vector<float> sw;
   public:
      SWClass()
         {
         scienceAPI->exposeFunction("sw", "mm/mm", "Soil water",floatArrayFunction(&SWClass::getter));
         sw.push_back(1.0); sw.push_back(2.0);
         }

      void getter(vector<float>& values) {values = sw;}
   };

void ExposeVariableUsingFunction()
   {
   setup();

   SWClass sw;

   // make sure registration happened.
   Register registerData;
   unpack(messagesSent[0], registerData);
   BOOST_ASSERT(registerData.destID == 0);
   BOOST_ASSERT(registerData.name == "sw");
   BOOST_ASSERT(registerData.kind == 2 /*respondToGet*/);
   BOOST_ASSERT(registerData.ddml == "<type kind=\"single\" array=\"T\" unit=\"mm/mm\" description=\"Soil water\"/>");

   // ask our component the value of sw.
   QueryValue queryValue;
   queryValue.ID = registerData.ID;
   queryValue.requestedByID = 1234;
   Message& queryValueMsg = newMessage(Message::QueryValue, parentID, componentID, false, queryValue);
   sendMessage(queryValueMsg);

   // check that the component gave us the correct ReplyValue message.
   ReplyValue replyValue;
   unpack(messagesSent[1], replyValue);
   BOOST_ASSERT(replyValue.queryID == queryValueMsg.messageID);
   BOOST_ASSERT(replyValue.ddml == "<type kind=\"single\" array=\"T\" unit=\"mm/mm\" description=\"Soil water\"/>");
   vector<float> values;
   unpack(messagesSent[1], values);
   BOOST_ASSERT(values.size() == 2);
   BOOST_ASSERT(values[0] == 1);
   BOOST_ASSERT(values[1] == 2);

   teardown();
   }
//---------------------------------------------------------------------------
// UseCase: do a get for variable "sw" with no type conversion.
//---------------------------------------------------------------------------
void GetVariable()
   {
   setup();

   vector<float> sw;
   BOOST_CHECK_THROW(scienceAPI->get("doesntexist", "mm", sw), runtime_error);
   scienceAPI->get("sw", "mm", sw);
   BOOST_CHECK(sw.size() == 4);
   BOOST_CHECK(sw[0] == 11);
   BOOST_CHECK(sw[1] == 22);
   BOOST_CHECK(sw[2] == 33);
   BOOST_CHECK(sw[3] == 44);

   teardown();
   }

//---------------------------------------------------------------------------
// UseCase: do a get for variable "sw" with a float to string type conversion.
//---------------------------------------------------------------------------
void GetVariableFloatToString()
   {
   setup();

   string swString;
   scienceAPI->get("sw", "mm", swString);
   BOOST_CHECK(swString == "11.000000 22.000000 33.000000 44.000000");

   teardown();
   }

//---------------------------------------------------------------------------
// UseCase: do a get for variable "sw(2)", sw(2-3), sw(), sum(sw), sum(sw(3-4))
//---------------------------------------------------------------------------
void GetVariableWithArraySpec()
   {
   setup();

   string swString;
   scienceAPI->get("sw(2)", "mm", swString);

   // make sure the message sent was a getvalue without the arrayspec.
   Register registerData;
   unpack(messagesSent[0], registerData);
   BOOST_ASSERT(registerData.name == "sw");

   // make sure the correct element was returned.
   BOOST_CHECK(swString == "22.000000");

   scienceAPI->get("sw(2-3)", "mm", swString);
   BOOST_CHECK(swString == "22.000000 33.000000");

   scienceAPI->get("sw()", "mm", swString);
   BOOST_CHECK(swString == "110.000000");

   scienceAPI->get("sum(sw)", "mm", swString);
   BOOST_CHECK(swString == "110.000000");

   scienceAPI->get("sum(sw(3-4))", "mm", swString);
   BOOST_CHECK(swString == "77.000000");

   teardown();
   }

//---------------------------------------------------------------------------
// UseCase: Expose a settable double variable "rain" and make sure it's value is
//          changed when a QuerySetValue msg is sent by the PM
//---------------------------------------------------------------------------
void ExposeSettableVariable()
   {
   setup();

   double rain = 0.0;
   scienceAPI->expose("rain", "mm", "Rainfall", true, rain);

   // make sure registration happened.
   Register registerData;
   unpack(messagesSent[0], registerData);
   BOOST_ASSERT(registerData.destID == 0);
   BOOST_ASSERT(registerData.name == "rain");
   BOOST_ASSERT(registerData.kind == 4 /*respondToGetSet*/);
   BOOST_ASSERT(registerData.ddml == "<type kind=\"double\" unit=\"mm\" description=\"Rainfall\"/>");

   // change the value of rain.
   double newRain = 1234;
   QuerySetValue querySetValue;
   querySetValue.ID = registerData.ID;
   querySetValue.ddml = "<type kind=\"double\" unit=\"mm\"/>";
   Message& querySetValueMsg = constructMessage(Message::QuerySetValue, parentID, componentID, false,
                                                memorySize(querySetValue) + memorySize(newRain));
   MessageData messageData(querySetValueMsg);
   pack(messageData, querySetValue);
   pack(messageData, newRain);
   sendMessage(querySetValueMsg);

   // check that the component gave us a notifySetValueSuccess
   NotifySetValueSuccess notifySuccess;
   unpack(messagesSent[1], notifySuccess);
   BOOST_ASSERT(notifySuccess.ID == registerData.ID);
   BOOST_ASSERT(notifySuccess.success);
   BOOST_ASSERT(rain == newRain);

   teardown();
   }
//---------------------------------------------------------------------------
// UseCase: Expose a string variable "st" using a setter function
//          and make sure it's value is changed when component gets a querySetValue
//          message from the PM.
//---------------------------------------------------------------------------
class StClass
   {
   public:
      std::string st;
      StClass()
         {
         scienceAPI->exposeFunction("st", "", "", stringFunction(&StClass::getter), stringFunction(&StClass::setter));
         st = "empty string";
         }

      void setter(std::string& value) {st = value;}
      void getter(std::string& value) {value = st;}
   };

void ExposeSettableVariableUsingFunction()
   {
   setup();

   StClass test;

   // make sure registration happened.
   Register registerData;
   unpack(messagesSent[0], registerData);
   BOOST_ASSERT(registerData.destID == 0);
   BOOST_ASSERT(registerData.name == "st");
   BOOST_ASSERT(registerData.kind == 4 /*respondToGetSet*/);
   BOOST_ASSERT(registerData.ddml == "<type kind=\"string\"/>");

   // send a queryValue message to component.
   string newValue = "new value";
   QuerySetValue querySetValue;
   querySetValue.ID = registerData.ID;
   querySetValue.ddml = registerData.ddml;
   Message& querySetValueMsg = constructMessage(Message::QuerySetValue, parentID, componentID, false,
                                                memorySize(querySetValue) + memorySize(newValue));
   MessageData messageData(querySetValueMsg);
   pack(messageData, querySetValue);
   pack(messageData, newValue);
   sendMessage(querySetValueMsg);

   // check that the component gave us a notifySetValueSuccess
   NotifySetValueSuccess notifySuccess;
   unpack(messagesSent[1], notifySuccess);
   BOOST_ASSERT(notifySuccess.ID == registerData.ID);
   BOOST_ASSERT(notifySuccess.success);
   BOOST_ASSERT(test.st == newValue);

   teardown();
   }

//---------------------------------------------------------------------------
// UseCase: do a get for variable "sw" with no type conversion.
//---------------------------------------------------------------------------
void SetVariable()
   {
   setup();

   int i = 123456;
   scienceAPI->set("i", "mm", i);

   // make sure registration happened.
   Register registerData;
   unpack(messagesSent[0], registerData);
   BOOST_ASSERT(registerData.destID == 0);
   BOOST_ASSERT(registerData.name == "i");
   BOOST_ASSERT(registerData.kind == 9 /*set*/);
   BOOST_ASSERT(registerData.ddml == "<type kind=\"integer4\" unit=\"mm\"/>");

   // make sure the second message was a requestSetValue message
   RequestSetValue requestSetValue;
   unpack(messagesSent[1], requestSetValue);
   BOOST_ASSERT(requestSetValue.ID == registerData.ID);
   BOOST_ASSERT(requestSetValue.ddml == registerData.ddml);
   int value;
   unpack(messagesSent[1], value);
   BOOST_ASSERT(value == i);
   teardown();
   }

//---------------------------------------------------------------------------
// UseCase: Test to make sure query sends out QueryInfo messages and
//          returns matches to caller.
//---------------------------------------------------------------------------
void Query()
   {
   setup();

   vector<QueryMatch> matches;
   scienceAPI->query("*", matches);

   // make sure a queryInfo message happened.
   QueryInfo queryInfo;
   unpack(messagesSent[0], queryInfo);
   BOOST_ASSERT(queryInfo.name == "*");
   BOOST_ASSERT(queryInfo.kind == 7 /* component */);

   scienceAPI->query("*.sw", matches);
   unpack(messagesSent[1], queryInfo);
   BOOST_ASSERT(queryInfo.name == "*.sw");
   BOOST_ASSERT(queryInfo.kind == 2 /* variable */);
   BOOST_ASSERT(matches.size() == 2);
   BOOST_ASSERT(matches[0].name == "comp1.sw");
   BOOST_ASSERT(matches[0].units == "mm");
   BOOST_ASSERT(matches[1].name == "comp2.sw");
   BOOST_ASSERT(matches[1].units == "mm");

   teardown();
   }


//---------------------------------------------------------------------------
// test method
//---------------------------------------------------------------------------
void TestVariables(void)
   {
   ExposeVariableAndReturnValue();
   ExposeVariableTwice();
   ExposeVariableUsingFunction();
   GetVariable();
   GetVariableFloatToString();
   GetVariableWithArraySpec();
   ExposeSettableVariable();
   ExposeSettableVariableUsingFunction();
   SetVariable();
   Query();
   }




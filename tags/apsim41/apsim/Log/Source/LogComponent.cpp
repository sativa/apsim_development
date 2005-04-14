#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "LogComponent.h"
#include <Protocol\transport.h>
#include <ApsimShared\FStringExt.h>
#include <general\string_functions.h>
#include <ApsimShared\ApsimComponentData.h>
#include <sstream>
using namespace std;
using namespace protocol;
// ------------------------------------------------------------------
//  Short description:
//     Return a blank string when requested to indicate that we
//     don't need a wrapper DLL.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" _export void __stdcall wrapperDLL(char* wrapperDll)
   {
   strcpy(wrapperDll, "");
   }
extern "C" void __stdcall getDescriptionInternal(char* initScript,
                                                 char* description);
// ------------------------------------------------------------------
// Return component description info.
// ------------------------------------------------------------------
extern "C" _export void __stdcall getDescription(char* initScript, char* description)
   {
   getDescriptionInternal(initScript, description);
   }
// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
LogComponent::LogComponent(void)
   {
   nesting = 3;
   previousNesting = -1;
   }
// ------------------------------------------------------------------
// destructor
// ------------------------------------------------------------------
LogComponent::~LogComponent(void)
   {
   out << "\/>" << endl;
   for (int i = previousNesting-1; i > 0; i--)
      {
      out.width(i*3);
      out << ' ';
      out << "</message" << i << '>';
      out << endl;
      }
   out << "</messages>\n";
   }
// ------------------------------------------------------------------
//  Short description:
//     createComponent

//  Notes:

//  Changes:
//    DPH 29/7/99

// ------------------------------------------------------------------
protocol::Component* createComponent(void)
   {
   return new LogComponent;
   }

// ------------------------------------------------------------------
//  Short description:
//     INIT1 method handler.

//  Notes:

//  Changes:
//    dph 27/6/2001

// ------------------------------------------------------------------
void LogComponent::doInit1(const FString& sdml)
   {
   protocol::Component::doInit1(sdml);

   string filename = componentData->getProperty("parameters", "logfile");
   if (filename == "")
      filename = "log.xml";
   out.open(filename.c_str());
   if (!out)
      {
      string msg = "Cannot open log file: " + filename;
      ::MessageBox(NULL, msg.c_str(), "Error", MB_ICONSTOP | MB_OK);
      }
   string debugOutputString = componentData->getProperty("parameters", "debug_output");
   bool doOutput = (debugOutputString == "" || Str_i_Eq(debugOutputString, "on"));
   if (doOutput)
      setMessageHook(this);
   }
// ------------------------------------------------------------------
//  Short description:
//     message callback routine.

//  Notes:

//  Changes:
//    dph 27/6/2001

// ------------------------------------------------------------------
void LogComponent::callback(const std::string& toName,
                            const protocol::Message* message)
   {
   static bool firstTimeThrough = true;

   if (firstTimeThrough)
      {
      firstTimeThrough = false;
      out << "<messages>" << endl;
      out << "   <message1 to=\"MasterPM\" msgtype=\"Init1\" ack=\"false\"";
      }
   else
      {
      if (message != NULL)
         {
         nesting++;
         if (nesting > previousNesting)
            {
            out << '>' << endl;

            out.width(nesting*3);
            out << ' ';
            out << "<message" << nesting << ' ';
            writeMessage(toName, message, out);
            }
         else if (nesting == previousNesting)
            {
            out << "/>" << endl;
            out.width(nesting*3);
            out << ' ';
            out << "<message" << nesting << ' ';
            writeMessage(toName, message, out);
            }
         else
            {
            out << "\/>" << endl;
            for (int i = previousNesting-1; i >= nesting; i--)
               {
               out.width(i*3);
               out << ' ';
               out << "</message" << i << '>';
               out << endl;
               }
            out.width(nesting*3);
            out << ' ';
            out << "<message" << nesting << ' ';
            writeMessage(toName, message, out);
            }
         previousNesting = nesting;
         out << flush;
         }
      else
         nesting--;
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    Get message log string

//  Notes:

//  Changes:
//    dph 14/5/2001

// ------------------------------------------------------------------
void LogComponent::writeMessage(const string& toName,
                                const protocol::Message* message,
                                ostream& out)
   {
   static const char* messageNames[45] =
     {"ActivateComponent", "AddComponent", "Checkpoint", "Commence",
      "Complete", "DeactivateComponent", "DeleteComponent", "Deregister",
      "Event", "GetValue", "Init1", "Init2",
      "NotifyAboutToDelete", "NotifyRegistrationChange", "NotifySetValueSuccess",
      "NotifyTermination", "PauseSimulation", "PublishEvent", "QueryInfo",
      "QuerySetValue", "QueryValue", "Register", "ReinstateCheckpoint",
      "ReplySetValueSuccess", "ReplyValue",
      "RequestComponentID", "RequestSetValue", "ResumeSimulation",
      "ReturnComponentID", "ReturnInfo", "ReturnValue",
      "TerminateSimulation", "", "", "", "", "", "", "",
      "ApsimGetQuery", "ApsimSetQuery", "ApsimChangeOrder"};

   if (message->messageType == protocol::Register)
      storeRegistration(message);
   out << "to=\"" << toName << "\"";
   out << " msgtype=\"" << messageNames[message->messageType-1] << "\"";
   out << " ack=\"";
   if (message->toAcknowledge)
      out << "true\"";
   else
      out << "false\"";
//   out << " id=\"" << message->messageID << "\"";
   writeMessageData(message);
   }

// ------------------------------------------------------------------
//  Short description:
//    Write message data

//  Notes:

//  Changes:
//    dph 14/5/2001

// ------------------------------------------------------------------
void LogComponent::writeMessageData(const protocol::Message* message)
   {
   protocol::MessageData messageData(message->dataPtr, message->nDataBytes);
   switch(message->messageType)
      {
      case protocol::Register:
         {
         protocol::RegisterData registerData;
         messageData >> registerData;
         out << " kind=\"" << registerData.kind.asString() << "\"";
         out << " name=\"" << asString(registerData.name) << "\"";
         if (registerData.destID > 0)
            out << " directedToComponent=\"" << registerData.destID << "\"";
         out << " type=\"" << formatType(asString(registerData.type)) << "\"";
         break;
         }
      case protocol::RequestSetValue:
         {
         protocol::RequestSetValueData requestSetValueData;
         messageData >> requestSetValueData;
         writeVariant(requestSetValueData.variant);
         break;
         }
      case protocol::QuerySetValue:
         {
         protocol::QuerySetValueData querySetValueData;
         messageData >> querySetValueData;
         writeVariant(querySetValueData.variant);
         break;
         }
      case protocol::GetValue:
         {
         writeRegistrationData(message, RegistrationType::get);
         break;
         }
      case protocol::ReturnValue:
         {
         protocol::ReturnValueData returnValueData;
         messageData >> returnValueData;
         out << " compid = \"" << returnValueData.fromID << "\"";
         break;
         }
      case protocol::PublishEvent:
         {
         protocol::PublishEventData eventData;
         messageData >> eventData;
         writeRegistrationData(message,  RegistrationType::event);
         writeVariant(eventData.variant);
         break;
         }
      case protocol::ApsimGetQuery:
         {
         protocol::ApsimGetQueryData apsimGetQuery;
         messageData >> apsimGetQuery;
         out << " name=\"" << asString(apsimGetQuery.name) << "\"";
         break;
         }
      case protocol::ApsimSetQuery:
         {
         protocol::ApsimSetQueryData apsimSetQuery;
         messageData >> apsimSetQuery;
         out << " name=\"" << asString(apsimSetQuery.name) << "\"";
         writeVariant(apsimSetQuery.variant);
         break;
         }
      case protocol::QueryInfo:
         {
         protocol::QueryInfoData queryInfo;
         messageData >> queryInfo;
         out << " name=\"" << asString(queryInfo.name) << "\"";
         out << " kind=\"" << queryInfo.kind << "\"";
         break;
         }
      }
   }
// ------------------------------------------------------------------
//  Short description:
//    Store this registration

//  Changes:
//    dph 14/5/2001

// ------------------------------------------------------------------
void LogComponent::storeRegistration(const Message* message)
   {
   MessageData messageData((Message*)message);
   RegisterData registerData;
   messageData >> registerData;
   components[message->from].registrations.insert(
      LogComponent::Registrations::value_type(make_pair(registerData.ID, (unsigned)registerData.kind.type()), asString(registerData.name)));
   }
// ------------------------------------------------------------------
//  Short description:
//    write the registration data for the specified message.

//  Changes:
//    dph 14/5/2001

// ------------------------------------------------------------------
void LogComponent::writeRegistrationData(const Message* message, RegistrationType kind)
   {
   MessageData messageData((Message*) message);
   unsigned int ID;
   messageData >> ID;

   out << " regName=\"" << components[message->from].registrations[make_pair(ID, (unsigned)kind.type())] << "\"";
//   out << " regID=\"" << ID << "\"";
   }
// ------------------------------------------------------------------
//  Short description:
//    write the registration data for the specified message.

//  Changes:
//    dph 14/5/2001

// ------------------------------------------------------------------
void LogComponent::writeVariant(const protocol::Variant& variant)
   {
   out << " type=\"" << formatType(asString(variant.getType().getTypeString())) << "\"";
   }

// ------------------------------------------------------------------
// Format a ddml string ready for outputting.
// ------------------------------------------------------------------
string LogComponent::formatType(string RegistrationTypeString)
   {
   replaceAll(RegistrationTypeString, "><", "   ");
   replaceAll(RegistrationTypeString, "<", "");
   replaceAll(RegistrationTypeString, ">", "");
   replaceAll(RegistrationTypeString, "/", "");
   replaceAll(RegistrationTypeString, "\"", "'");
   return RegistrationTypeString;
   }

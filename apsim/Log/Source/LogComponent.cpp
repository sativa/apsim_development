#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "LogComponent.h"
#include <Protocol\transport.h>
#include <ApsimShared\FStringExt.h>
#include <ApsimShared\ApsimServiceData.h>
#include <general\string_functions.h>
#include <sstream>
using namespace std;
using namespace protocol;
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

   string sdmlString(sdml.f_str(), sdml.length());
   ApsimServiceData service(sdmlString);

   string filename = service.getProperty("filename");
   out.open(filename.c_str());
   if (!out)
      {
      string msg = "Cannot open log file: " + filename;
      ::MessageBox(NULL, msg.c_str(), "Error", MB_ICONSTOP | MB_OK);
      }
   bool doOutput = Str_i_Eq(service.getProperty("debug_output"), "on");
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
   static int nesting = 2;
   static int previousNesting = -1;
   static bool firstTimeThrough = true;

   if (firstTimeThrough)
      {
      firstTimeThrough = false;
      out << "<messages";
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
            out << "\/>" << endl;
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
            previousNesting = nesting;
            out.width(nesting*3);
            out << ' ';
            out << "<message" << nesting << ' ';
            writeMessage(toName, message, out);
            }
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
   static const char* messageNames[42] =
     {"ActivateComponent", "AddComponent", "Checkpoint", "Commence",
      "Complete", "DeactivateComponent", "DeleteComponent", "Deregister",
      "Event", "GetValue", "Init1", "Init2",
      "NotifyAboutToDelete", "NotifyRegistrationChange", "NotifySetValueSuccess",
      "NotifyTermination", "PauseSimulation", "PublishEvent", "QueryInfo",
      "QueryType", "QueryValue", "Register", "ReinstateCheckpoint",
      "RequestComponentID", "RequestSetValue", "ResumeSimulation",
      "ReturnComponentID", "ReturnInfo", "ReturnType", "ReturnValue",
      "TerminateSimulation", "", "", "", "", "", "", "", "",
      "QuerySetValue", "ApsimGetQuery", "ApsimSetQuery"};

   if (message->messageType == protocol::Register)
      storeRegistration(message);
   out << "to=\"" << toName << "\"";
   out << " type=\"" << messageNames[message->messageType-1] << "\"";
   out << " ack=\"";
   if (message->toAcknowledge)
      out << "true\"";
   else
      out << "false\"";
   out << " id=\"" << message->messageID << "\"";
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
         static const char* RegistrationKindNames[10] =
                      {"", "getVariableReg", "respondToGetReg",
                           "respondToSetReg", "respondToGetSetReg",
                           "eventReg", "respondToEventReg",
                           "respondToMethodCallReg",
                           "setVariableReg", "methodCallReg"};

         protocol::RegisterData registerData;
         messageData >> registerData;
         out << " kind=\"" << RegistrationKindNames[registerData.kind] << "\"";
         out << " regID=\"" << registerData.ID << "\"";
         out << " name=\"" << asString(registerData.name) << "\"";
         out << " type=\"" << asString(registerData.type) << "\"";
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
         writeRegistrationData(message);
         break;
         }
      case protocol::PublishEvent:
         {
         protocol::PublishEventData eventData;
         messageData >> eventData;
         writeRegistrationData(message);
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
      LogComponent::Registrations::value_type(registerData.ID, asString(registerData.name)));
   }
// ------------------------------------------------------------------
//  Short description:
//    write the registration data for the specified message.

//  Changes:
//    dph 14/5/2001

// ------------------------------------------------------------------
void LogComponent::writeRegistrationData(const Message* message)
   {
   MessageData messageData((Message*) message);
   unsigned int ID;
   messageData >> ID;

   out << " regName=\"" << components[message->from].registrations[ID] << "\"";
   out << " regID=\"" << ID << "\"";
   }
// ------------------------------------------------------------------
//  Short description:
//    write the registration data for the specified message.

//  Changes:
//    dph 14/5/2001

// ------------------------------------------------------------------
void LogComponent::writeVariant(const protocol::Variant& variant)
   {
   out << " type=\"" << asString(variant.getType().getTypeString()) << "\"";
   }

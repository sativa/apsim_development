#pragma once

#include "Interfaces.h"
#include "MessageData.h"
//-------------------- GetValue type
class GetValue
   {
   public:
      int id;
      void pack(Message& message)
         {
         ::pack(message, id);
         }
      void unpack(Message& message)
         {
         ::unpack(message, id);
         }

   };

//-------------------- QueryValue type
class QueryValue
   {
   public:
      int id;
      int requestedBy;
      void pack(Message& message)
         {
         ::pack(message, id);
         ::pack(message, requestedBy);
         }
      void unpack(Message& message)
         {
         ::unpack(message, id);
         ::unpack(message, requestedBy);
         }

   };

//-------------------- ReplyValue type
class ReplyValue
   {
   public:
      int queryID;
      std::string type;
      char* value;
      void pack(Message& message)
         {
         ::pack(message, queryID);
         ::pack(message, type);

         }
      void unpack(Message& message)
         {
         ::unpack(message, queryID);
         ::unpack(message, type);

         }

   };

//-------------------- ReturnValue type
class ReturnValue
   {
   public:
      int compID;
      int id;
      std::string type;
      char* value;
      void pack(Message& message)
         {
         ::pack(message, compID);
         ::pack(message, id);
         ::pack(message, type);

         }
      void unpack(Message& message)
         {
         ::unpack(message, compID);
         ::unpack(message, id);
         ::unpack(message, type);

         }

   };

//-------------------- Init1 type
class Init1
   {
   public:
      std::string sdml;
      std::string fqn;
      bool inStartup;
      void pack(Message& message)
         {
         ::pack(message, sdml);
         ::pack(message, fqn);
         ::pack(message, inStartup);
         }
      void unpack(Message& message)
         {
         ::unpack(message, sdml);
         ::unpack(message, fqn);
         ::unpack(message, inStartup);
         }

   };

//-------------------- Event type
class Event
   {
   public:
      int id;
      int publishedBy;
      std::string type;
      char* params;
      void pack(Message& message)
         {
         ::pack(message, id);
         ::pack(message, publishedBy);
         ::pack(message, type);

         }
      void unpack(Message& message)
         {
         ::unpack(message, id);
         ::unpack(message, publishedBy);
         ::unpack(message, type);

         }

   };

//-------------------- Register type
class Register
   {
   public:
      int kind;
      int id;
      int destID;
      std::string name;
      std::string type;
      void pack(Message& message)
         {
         ::pack(message, kind);
         ::pack(message, id);
         ::pack(message, destID);
         ::pack(message, name);
         ::pack(message, type);
         }
      void unpack(Message& message)
         {
         ::unpack(message, kind);
         ::unpack(message, id);
         ::unpack(message, destID);
         ::unpack(message, name);
         ::unpack(message, type);
         }

   };

//-------------------- PublishEvent type
class PublishEvent
   {
   public:
      int id;
      std::string type;
      char* params;
      void pack(Message& message)
         {
         ::pack(message, id);
         ::pack(message, type);

         }
      void unpack(Message& message)
         {
         ::unpack(message, id);
         ::unpack(message, type);

         }

   };

//-------------------- QueryInfo type
class QueryInfo
   {
   public:
      std::string name;
      int kind;
      void pack(Message& message)
         {
         ::pack(message, name);
         ::pack(message, kind);
         }
      void unpack(Message& message)
         {
         ::unpack(message, name);
         ::unpack(message, kind);
         }

   };

//-------------------- RequestSetValue type
class RequestSetValue
   {
   public:
      int id;
      std::string type;
      char* value;
      void pack(Message& message)
         {
         ::pack(message, id);
         ::pack(message, type);

         }
      void unpack(Message& message)
         {
         ::unpack(message, id);
         ::unpack(message, type);

         }

   };

//-------------------- QuerySetValue type
class QuerySetValue
   {
   public:
      int id;
      int replyToID;
      int replyID;
      std::string type;
      char* value;
      void pack(Message& message)
         {
         ::pack(message, id);
         ::pack(message, replyToID);
         ::pack(message, replyID);
         ::pack(message, type);

         }
      void unpack(Message& message)
         {
         ::unpack(message, id);
         ::unpack(message, replyToID);
         ::unpack(message, replyID);
         ::unpack(message, type);

         }

   };

//-------------------- NotifySetValueSuccess type
class NotifySetValueSuccess
   {
   public:
      int id;
      bool success;
      void pack(Message& message)
         {
         ::pack(message, id);
         ::pack(message, success);
         }
      void unpack(Message& message)
         {
         ::unpack(message, id);
         ::unpack(message, success);
         }

   };

//-------------------- ReturnInfo type
class ReturnInfo
   {
   public:
      int queryID;
      int compID;
      int id;
      std::string name;
      std::string type;
      int kind;
      void pack(Message& message)
         {
         ::pack(message, queryID);
         ::pack(message, compID);
         ::pack(message, id);
         ::pack(message, name);
         ::pack(message, type);
         ::pack(message, kind);
         }
      void unpack(Message& message)
         {
         ::unpack(message, queryID);
         ::unpack(message, compID);
         ::unpack(message, id);
         ::unpack(message, name);
         ::unpack(message, type);
         ::unpack(message, kind);
         }

   };


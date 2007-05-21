#pragma once

#include "Interfaces.h"
#include "MessageData.h"
namespace ComponentInterface {

//-------------------- GetValue type
class GetValue
   {
   public:
      int id;
      void pack(Message& message)
         {
         ApsimInteger4(id).pack(message);
         }
      void unpack(Message& message)
         {
         id = ApsimInteger4(message).value();
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
         ApsimInteger4(id).pack(message);
         ApsimInteger4(requestedBy).pack(message);
         }
      void unpack(Message& message)
         {
         id = ApsimInteger4(message).value();
         requestedBy = ApsimInteger4(message).value();
         }

   };

//-------------------- ReplyValue type
class ReplyValue
   {
   public:
      int queryID;
      std::string type;
      /*char* value;*/
      void pack(Message& message)
         {
         ApsimInteger4(queryID).pack(message);
         ApsimString(type).pack(message);
         }
      void unpack(Message& message)
         {
         queryID = ApsimInteger4(message).value();
         type = ApsimString(message).CValue();
         }

   };

//-------------------- ReturnValue type
class ReturnValue
   {
   public:
      int compID;
      int id;
      std::string type;
      /*char* value;*/
      void pack(Message& message)
         {
         ApsimInteger4(compID).pack(message);
         ApsimInteger4(id).pack(message);
         ApsimString(type).pack(message);
         }
      void unpack(Message& message)
         {
         compID = ApsimInteger4(message).value();
         id = ApsimInteger4(message).value();
         type = ApsimString(message).CValue();
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
         ApsimString(sdml).pack(message);
         ApsimString(fqn).pack(message);
         ApsimBoolean(inStartup).pack(message);
         }
      void unpack(Message& message)
         {
         sdml = ApsimString(message).CValue();
         fqn = ApsimString(message).CValue();
         inStartup = ApsimBoolean(message).value();
         }

   };

//-------------------- Event type
class Event
   {
   public:
      int id;
      int publishedBy;
      std::string type;
      /*char* params;*/
      void pack(Message& message)
         {
         ApsimInteger4(id).pack(message);
         ApsimInteger4(publishedBy).pack(message);
         ApsimString(type).pack(message);
         }
      void unpack(Message& message)
         {
         id = ApsimInteger4(message).value();
         publishedBy = ApsimInteger4(message).value();
         type = ApsimString(message).CValue();
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
         ApsimInteger4(kind).pack(message);
         ApsimInteger4(id).pack(message);
         ApsimInteger4(destID).pack(message);
         ApsimString(name).pack(message);
         ApsimString(type).pack(message);
         }
      void unpack(Message& message)
         {
         kind = ApsimInteger4(message).value();
         id = ApsimInteger4(message).value();
         destID = ApsimInteger4(message).value();
         name = ApsimString(message).CValue();
         type = ApsimString(message).CValue();
         }

   };

//-------------------- PublishEvent type
class PublishEvent
   {
   public:
      int id;
      std::string type;
      /*char* params;*/
      void pack(Message& message)
         {
         ApsimInteger4(id).pack(message);
         ApsimString(type).pack(message);
         }
      void unpack(Message& message)
         {
         id = ApsimInteger4(message).value();
         type = ApsimString(message).CValue();
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
         ApsimString(name).pack(message);
         ApsimInteger4(kind).pack(message);
         }
      void unpack(Message& message)
         {
         name = ApsimString(message).CValue();
         kind = ApsimInteger4(message).value();
         }

   };

//-------------------- RequestSetValue type
class RequestSetValue
   {
   public:
      int id;
      std::string type;
      /*char* value;*/
      void pack(Message& message)
         {
         ApsimInteger4(id).pack(message);
         ApsimString(type).pack(message);
         }
      void unpack(Message& message)
         {
         id = ApsimInteger4(message).value();
         type = ApsimString(message).CValue();
         }

   };

//-------------------- QuerySetValue type
class QuerySetValue
   {
   public:
      int id;
      std::string type;
      /*char* value;*/
      void pack(Message& message)
         {
         ApsimInteger4(id).pack(message);
         ApsimString(type).pack(message);
         }
      void unpack(Message& message)
         {
         id = ApsimInteger4(message).value();
         type = ApsimString(message).CValue();
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
         ApsimInteger4(id).pack(message);
         ApsimBoolean(success).pack(message);
         }
      void unpack(Message& message)
         {
         id = ApsimInteger4(message).value();
         success = ApsimBoolean(message).value();
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
         ApsimInteger4(queryID).pack(message);
         ApsimInteger4(compID).pack(message);
         ApsimInteger4(id).pack(message);
         ApsimString(name).pack(message);
         ApsimString(type).pack(message);
         ApsimInteger4(kind).pack(message);
         }
      void unpack(Message& message)
         {
         queryID = ApsimInteger4(message).value();
         compID = ApsimInteger4(message).value();
         id = ApsimInteger4(message).value();
         name = ApsimString(message).CValue();
         type = ApsimString(message).CValue();
         kind = ApsimInteger4(message).value();
         }

   };

};
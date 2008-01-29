#include <general/stl_functions.h>
#include <general/TreeNodeIterator.h>
#include <general/xml.h>
#include "DataTypes.h"
#include "MessageData.h"
#include "Interfaces.h"

   //------ Null ------
   void EXPORT pack(MessageData& messageData, const Null& data) { }
   void EXPORT unpack(MessageData& messageData, Null& data) { }
   unsigned EXPORT memorySize(const Null& data) {return 0;}
   std::string EXPORT DDML(const Null& data) {return "<type/>";}


   //------ Complete ------

   void EXPORT pack(MessageData& messageData, const CompleteType& data)
      {
      pack(messageData, data.ackID);
      }
   void EXPORT unpack(MessageData& messageData, CompleteType& data)
      {
      unpack(messageData, data.ackID);
      }
   unsigned EXPORT memorySize(const CompleteType& data)
      {
      return 0
              + ::memorySize(data.ackID)
              ;
      }
   std::string EXPORT DDML(const CompleteType& data)
      {return "<type name=\"Complete\">"
               "<field name=\"ackID\" kind=\"integer4\" />"
               "</type>";}

   //------ Error ------

   void EXPORT pack(MessageData& messageData, const ErrorType& data)
      {
      pack(messageData, data.isFatal);
      pack(messageData, data.msg);
      }
   void EXPORT unpack(MessageData& messageData, ErrorType& data)
      {
      unpack(messageData, data.isFatal);
      unpack(messageData, data.msg);
      }
   unsigned EXPORT memorySize(const ErrorType& data)
      {
      return 0
              + ::memorySize(data.isFatal)
              + ::memorySize(data.msg)
              ;
      }
   std::string EXPORT DDML(const ErrorType& data)
      {return "<type name=\"Error\">"
               "<field name=\"isFatal\" kind=\"boolean\" />"
               "<field name=\"msg\" kind=\"string\" />"
               "</type>";}

   //------ Event ------

   void EXPORT pack(MessageData& messageData, const EventType& data)
      {
      pack(messageData, data.ID);
      pack(messageData, data.publishedBy);
      pack(messageData, data.ddml);
      }
   void EXPORT unpack(MessageData& messageData, EventType& data)
      {
      unpack(messageData, data.ID);
      unpack(messageData, data.publishedBy);
      unpack(messageData, data.ddml);
      }
   unsigned EXPORT memorySize(const EventType& data)
      {
      return 0
              + ::memorySize(data.ID)
              + ::memorySize(data.publishedBy)
              + ::memorySize(data.ddml)
              ;
      }
   std::string EXPORT DDML(const EventType& data)
      {return "<type name=\"Event\">"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"publishedBy\" kind=\"integer4\" />"
               "<field name=\"ddml\" kind=\"string\" />"
               "</type>";}

   //------ GetValue ------

   void EXPORT pack(MessageData& messageData, const GetValueType& data)
      {
      pack(messageData, data.ID);
      }
   void EXPORT unpack(MessageData& messageData, GetValueType& data)
      {
      unpack(messageData, data.ID);
      }
   unsigned EXPORT memorySize(const GetValueType& data)
      {
      return 0
              + ::memorySize(data.ID)
              ;
      }
   std::string EXPORT DDML(const GetValueType& data)
      {return "<type name=\"GetValue\">"
               "<field name=\"ID\" kind=\"integer4\" />"
               "</type>";}

   //------ Init1 ------

   void EXPORT pack(MessageData& messageData, const Init1Type& data)
      {
      pack(messageData, data.sdml);
      pack(messageData, data.fqn);
      pack(messageData, data.inStartup);
      }
   void EXPORT unpack(MessageData& messageData, Init1Type& data)
      {
      unpack(messageData, data.sdml);
      unpack(messageData, data.fqn);
      unpack(messageData, data.inStartup);
      }
   unsigned EXPORT memorySize(const Init1Type& data)
      {
      return 0
              + ::memorySize(data.sdml)
              + ::memorySize(data.fqn)
              + ::memorySize(data.inStartup)
              ;
      }
   std::string EXPORT DDML(const Init1Type& data)
      {return "<type name=\"Init1\">"
               "<field name=\"sdml\" kind=\"string\" />"
               "<field name=\"fqn\" kind=\"string\" />"
               "<field name=\"inStartup\" kind=\"boolean\" />"
               "</type>";}

   //------ NotifySetValueSuccess ------

   void EXPORT pack(MessageData& messageData, const NotifySetValueSuccessType& data)
      {
      pack(messageData, data.ID);
      pack(messageData, data.success);
      }
   void EXPORT unpack(MessageData& messageData, NotifySetValueSuccessType& data)
      {
      unpack(messageData, data.ID);
      unpack(messageData, data.success);
      }
   unsigned EXPORT memorySize(const NotifySetValueSuccessType& data)
      {
      return 0
              + ::memorySize(data.ID)
              + ::memorySize(data.success)
              ;
      }
   std::string EXPORT DDML(const NotifySetValueSuccessType& data)
      {return "<type name=\"NotifySetValueSuccess\">"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"success\" kind=\"boolean\" />"
               "</type>";}

   //------ PublishEvent ------

   void EXPORT pack(MessageData& messageData, const PublishEventType& data)
      {
      pack(messageData, data.ID);
      pack(messageData, data.ddml);
      }
   void EXPORT unpack(MessageData& messageData, PublishEventType& data)
      {
      unpack(messageData, data.ID);
      unpack(messageData, data.ddml);
      }
   unsigned EXPORT memorySize(const PublishEventType& data)
      {
      return 0
              + ::memorySize(data.ID)
              + ::memorySize(data.ddml)
              ;
      }
   std::string EXPORT DDML(const PublishEventType& data)
      {return "<type name=\"PublishEvent\">"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"ddml\" kind=\"string\" />"
               "</type>";}

   //------ QueryInfo ------

   void EXPORT pack(MessageData& messageData, const QueryInfoType& data)
      {
      pack(messageData, data.name);
      pack(messageData, data.kind);
      }
   void EXPORT unpack(MessageData& messageData, QueryInfoType& data)
      {
      unpack(messageData, data.name);
      unpack(messageData, data.kind);
      }
   unsigned EXPORT memorySize(const QueryInfoType& data)
      {
      return 0
              + ::memorySize(data.name)
              + ::memorySize(data.kind)
              ;
      }
   std::string EXPORT DDML(const QueryInfoType& data)
      {return "<type name=\"QueryInfo\">"
               "<field name=\"name\" kind=\"string\" />"
               "<field name=\"kind\" kind=\"integer4\" />"
               "</type>";}

   //------ Register ------

   void EXPORT pack(MessageData& messageData, const RegisterType& data)
      {
      pack(messageData, data.kind);
      pack(messageData, data.ID);
      pack(messageData, data.destID);
      pack(messageData, data.name);
      pack(messageData, data.ddml);
      }
   void EXPORT unpack(MessageData& messageData, RegisterType& data)
      {
      unpack(messageData, data.kind);
      unpack(messageData, data.ID);
      unpack(messageData, data.destID);
      unpack(messageData, data.name);
      unpack(messageData, data.ddml);
      }
   unsigned EXPORT memorySize(const RegisterType& data)
      {
      return 0
              + ::memorySize(data.kind)
              + ::memorySize(data.ID)
              + ::memorySize(data.destID)
              + ::memorySize(data.name)
              + ::memorySize(data.ddml)
              ;
      }
   std::string EXPORT DDML(const RegisterType& data)
      {return "<type name=\"Register\">"
               "<field name=\"kind\" kind=\"integer4\" />"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"destID\" kind=\"integer4\" />"
               "<field name=\"name\" kind=\"string\" />"
               "<field name=\"ddml\" kind=\"string\" />"
               "</type>";}

   //------ ReplyValue ------

   void EXPORT pack(MessageData& messageData, const ReplyValueType& data)
      {
      pack(messageData, data.queryID);
      pack(messageData, data.ddml);
      }
   void EXPORT unpack(MessageData& messageData, ReplyValueType& data)
      {
      unpack(messageData, data.queryID);
      unpack(messageData, data.ddml);
      }
   unsigned EXPORT memorySize(const ReplyValueType& data)
      {
      return 0
              + ::memorySize(data.queryID)
              + ::memorySize(data.ddml)
              ;
      }
   std::string EXPORT DDML(const ReplyValueType& data)
      {return "<type name=\"ReplyValue\">"
               "<field name=\"queryID\" kind=\"integer4\" />"
               "<field name=\"ddml\" kind=\"string\" />"
               "</type>";}

   //------ RequestSetValue ------

   void EXPORT pack(MessageData& messageData, const RequestSetValueType& data)
      {
      pack(messageData, data.ID);
      pack(messageData, data.ddml);
      }
   void EXPORT unpack(MessageData& messageData, RequestSetValueType& data)
      {
      unpack(messageData, data.ID);
      unpack(messageData, data.ddml);
      }
   unsigned EXPORT memorySize(const RequestSetValueType& data)
      {
      return 0
              + ::memorySize(data.ID)
              + ::memorySize(data.ddml)
              ;
      }
   std::string EXPORT DDML(const RequestSetValueType& data)
      {return "<type name=\"RequestSetValue\">"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"ddml\" kind=\"string\" />"
               "</type>";}

   //------ ReturnInfo ------

   void EXPORT pack(MessageData& messageData, const ReturnInfoType& data)
      {
      pack(messageData, data.queryID);
      pack(messageData, data.compID);
      pack(messageData, data.ID);
      pack(messageData, data.name);
      pack(messageData, data.type);
      pack(messageData, data.kind);
      }
   void EXPORT unpack(MessageData& messageData, ReturnInfoType& data)
      {
      unpack(messageData, data.queryID);
      unpack(messageData, data.compID);
      unpack(messageData, data.ID);
      unpack(messageData, data.name);
      unpack(messageData, data.type);
      unpack(messageData, data.kind);
      }
   unsigned EXPORT memorySize(const ReturnInfoType& data)
      {
      return 0
              + ::memorySize(data.queryID)
              + ::memorySize(data.compID)
              + ::memorySize(data.ID)
              + ::memorySize(data.name)
              + ::memorySize(data.type)
              + ::memorySize(data.kind)
              ;
      }
   std::string EXPORT DDML(const ReturnInfoType& data)
      {return "<type name=\"ReturnInfo\">"
               "<field name=\"queryID\" kind=\"integer4\" />"
               "<field name=\"compID\" kind=\"integer4\" />"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"name\" kind=\"string\" />"
               "<field name=\"type\" kind=\"string\" />"
               "<field name=\"kind\" kind=\"integer4\" />"
               "</type>";}

   //------ ReturnValue ------

   void EXPORT pack(MessageData& messageData, const ReturnValueType& data)
      {
      pack(messageData, data.compID);
      pack(messageData, data.ID);
      pack(messageData, data.ddml);
      }
   void EXPORT unpack(MessageData& messageData, ReturnValueType& data)
      {
      unpack(messageData, data.compID);
      unpack(messageData, data.ID);
      unpack(messageData, data.ddml);
      }
   unsigned EXPORT memorySize(const ReturnValueType& data)
      {
      return 0
              + ::memorySize(data.compID)
              + ::memorySize(data.ID)
              + ::memorySize(data.ddml)
              ;
      }
   std::string EXPORT DDML(const ReturnValueType& data)
      {return "<type name=\"ReturnValue\">"
               "<field name=\"compID\" kind=\"integer4\" />"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"ddml\" kind=\"string\" />"
               "</type>";}

   //------ QueryValue ------

   void EXPORT pack(MessageData& messageData, const QueryValueType& data)
      {
      pack(messageData, data.ID);
      pack(messageData, data.requestedByID);
      }
   void EXPORT unpack(MessageData& messageData, QueryValueType& data)
      {
      unpack(messageData, data.ID);
      unpack(messageData, data.requestedByID);
      }
   unsigned EXPORT memorySize(const QueryValueType& data)
      {
      return 0
              + ::memorySize(data.ID)
              + ::memorySize(data.requestedByID)
              ;
      }
   std::string EXPORT DDML(const QueryValueType& data)
      {return "<type name=\"QueryValue\">"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"requestedByID\" kind=\"integer4\" />"
               "</type>";}

   //------ QuerySetValue ------

   void EXPORT pack(MessageData& messageData, const QuerySetValueType& data)
      {
      pack(messageData, data.ID);
      pack(messageData, data.ddml);
      }
   void EXPORT unpack(MessageData& messageData, QuerySetValueType& data)
      {
      unpack(messageData, data.ID);
      unpack(messageData, data.ddml);
      }
   unsigned EXPORT memorySize(const QuerySetValueType& data)
      {
      return 0
              + ::memorySize(data.ID)
              + ::memorySize(data.ddml)
              ;
      }
   std::string EXPORT DDML(const QuerySetValueType& data)
      {return "<type name=\"QuerySetValue\">"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"ddml\" kind=\"string\" />"
               "</type>";}

   //------ Layered ------

   void EXPORT pack(MessageData& messageData, const LayeredType& data)
      {
      pack(messageData, data.layer);
      pack(messageData, data.value);
      }
   void EXPORT unpack(MessageData& messageData, LayeredType& data)
      {
      unpack(messageData, data.layer);
      unpack(messageData, data.value);
      }
   unsigned EXPORT memorySize(const LayeredType& data)
      {
      return 0
              + ::memorySize(data.layer)
              + ::memorySize(data.value)
              ;
      }
   std::string EXPORT DDML(const LayeredType& data)
      {return "<type name=\"Layered\" description=\"Layered soil data\">"
               "<field name=\"layer\" kind=\"double\" array=\"T\" />"
               "<field name=\"value\" kind=\"double\" array=\"T\" />"
               "</type>";}

   //------ Time ------

   void EXPORT pack(MessageData& messageData, const TimeType& data)
      {
      pack(messageData, data.startday);
      pack(messageData, data.startsec);
      pack(messageData, data.startsecpart);
      pack(messageData, data.endday);
      pack(messageData, data.endsec);
      pack(messageData, data.endsecpart);
      }
   void EXPORT unpack(MessageData& messageData, TimeType& data)
      {
      unpack(messageData, data.startday);
      unpack(messageData, data.startsec);
      unpack(messageData, data.startsecpart);
      unpack(messageData, data.endday);
      unpack(messageData, data.endsec);
      unpack(messageData, data.endsecpart);
      }
   unsigned EXPORT memorySize(const TimeType& data)
      {
      return 0
              + ::memorySize(data.startday)
              + ::memorySize(data.startsec)
              + ::memorySize(data.startsecpart)
              + ::memorySize(data.endday)
              + ::memorySize(data.endsec)
              + ::memorySize(data.endsecpart)
              ;
      }
   std::string EXPORT DDML(const TimeType& data)
      {return "<type name=\"Time\" description=\"Change in the simulation system time and the duration of the new time step\">"
               "<field name=\"startday\" kind=\"integer4\" description=\"Day number of the start of the timestep\" />"
               "<field name=\"startsec\" kind=\"integer4\" description=\"Seconds past midnight of the start of the timestep (0-86399)\" />"
               "<field name=\"startsecpart\" kind=\"double\" description=\"Fraction of a second of the start of the timestep (0-1)\" />"
               "<field name=\"endday\" kind=\"integer4\" description=\"Day number of the end of the timestep\" />"
               "<field name=\"endsec\" kind=\"integer4\" description=\"Seconds past midnight of the end of the timestep (0-86399)\" />"
               "<field name=\"endsecpart\" kind=\"double\" description=\"Fraction of a second of the end of the timestep (0-1)\" />"
               "</type>";}

   //------ NewMet ------

   void EXPORT pack(MessageData& messageData, const NewMetType& data)
      {
      pack(messageData, data.today);
      pack(messageData, data.radn);
      pack(messageData, data.maxt);
      pack(messageData, data.mint);
      pack(messageData, data.rain);
      pack(messageData, data.vp);
      }
   void EXPORT unpack(MessageData& messageData, NewMetType& data)
      {
      unpack(messageData, data.today);
      unpack(messageData, data.radn);
      unpack(messageData, data.maxt);
      unpack(messageData, data.mint);
      unpack(messageData, data.rain);
      unpack(messageData, data.vp);
      }
   unsigned EXPORT memorySize(const NewMetType& data)
      {
      return 0
              + ::memorySize(data.today)
              + ::memorySize(data.radn)
              + ::memorySize(data.maxt)
              + ::memorySize(data.mint)
              + ::memorySize(data.rain)
              + ::memorySize(data.vp)
              ;
      }
   std::string EXPORT DDML(const NewMetType& data)
      {return "<type name=\"NewMet\">"
               "<field name=\"today\" kind=\"double\" />"
               "<field name=\"radn\" kind=\"single\" lower_bound=\"0.0\" upper_bound=\"50.0\" units=\"MJ/m2/d\" />"
               "<field name=\"maxt\" kind=\"single\" lower_bound=\"-10.0\" upper_bound=\"70.0\" units=\"oC\" />"
               "<field name=\"mint\" kind=\"single\" lower_bound=\"-20.0\" upper_bound=\"50.0\" units=\"oC\" />"
               "<field name=\"rain\" kind=\"single\" lower_bound=\"0.0\" upper_bound=\"1000.0\" units=\"mm/d\" />"
               "<field name=\"vp\" kind=\"single\" units=\"????\" />"
               "</type>";}

   //------ SoilWaterProfileLayer ------

   void EXPORT pack(MessageData& messageData, const SoilWaterProfileLayerType& data)
      {
      pack(messageData, data.thickness);
      pack(messageData, data.BulkDensity);
      pack(messageData, data.SatDepth);
      pack(messageData, data.DULDepth);
      pack(messageData, data.LL15Depth);
      pack(messageData, data.AirDryDepth);
      pack(messageData, data.SWDepth);
      }
   void EXPORT unpack(MessageData& messageData, SoilWaterProfileLayerType& data)
      {
      unpack(messageData, data.thickness);
      unpack(messageData, data.BulkDensity);
      unpack(messageData, data.SatDepth);
      unpack(messageData, data.DULDepth);
      unpack(messageData, data.LL15Depth);
      unpack(messageData, data.AirDryDepth);
      unpack(messageData, data.SWDepth);
      }
   unsigned EXPORT memorySize(const SoilWaterProfileLayerType& data)
      {
      return 0
              + ::memorySize(data.thickness)
              + ::memorySize(data.BulkDensity)
              + ::memorySize(data.SatDepth)
              + ::memorySize(data.DULDepth)
              + ::memorySize(data.LL15Depth)
              + ::memorySize(data.AirDryDepth)
              + ::memorySize(data.SWDepth)
              ;
      }
   std::string EXPORT DDML(const SoilWaterProfileLayerType& data)
      {return "<type name=\"SoilWaterProfileLayer\" array=\"T\">"
               "<element>"
               "<field name=\"thickness\" kind=\"single\" units=\"mm\" />"
               "<field name=\"BulkDensity\" kind=\"single\" units=\"g/cc\" />"
               "<field name=\"SatDepth\" kind=\"single\" units=\"mm\" />"
               "<field name=\"DULDepth\" kind=\"single\" units=\"mm\" />"
               "<field name=\"LL15Depth\" kind=\"single\" units=\"mm\" />"
               "<field name=\"AirDryDepth\" kind=\"single\" units=\"mm\" />"
               "<field name=\"SWDepth\" kind=\"single\" units=\"mm\" />"
               "</element>"
               "</type>";}

   //------ SoilWaterLayer ------

   void EXPORT pack(MessageData& messageData, const SoilWaterLayerType& data)
      {
      pack(messageData, data.thickness);
      pack(messageData, data.amount);
      }
   void EXPORT unpack(MessageData& messageData, SoilWaterLayerType& data)
      {
      unpack(messageData, data.thickness);
      unpack(messageData, data.amount);
      }
   unsigned EXPORT memorySize(const SoilWaterLayerType& data)
      {
      return 0
              + ::memorySize(data.thickness)
              + ::memorySize(data.amount)
              ;
      }
   std::string EXPORT DDML(const SoilWaterLayerType& data)
      {return "<type name=\"SoilWaterLayer\" array=\"T\">"
               "<element>"
               "<field name=\"thickness\" kind=\"single\" units=\"mm\" />"
               "<field name=\"amount\" kind=\"single\" units=\"mm\" />"
               "</element>"
               "</type>";}

   //------ LateralFlowLayer ------

   void EXPORT pack(MessageData& messageData, const LateralFlowLayerType& data)
      {
      pack(messageData, data.thickness);
      pack(messageData, data.amount);
      }
   void EXPORT unpack(MessageData& messageData, LateralFlowLayerType& data)
      {
      unpack(messageData, data.thickness);
      unpack(messageData, data.amount);
      }
   unsigned EXPORT memorySize(const LateralFlowLayerType& data)
      {
      return 0
              + ::memorySize(data.thickness)
              + ::memorySize(data.amount)
              ;
      }
   std::string EXPORT DDML(const LateralFlowLayerType& data)
      {return "<field name=\"LateralFlowLayer\" array=\"T\">"
               "<element>"
               "<field name=\"thickness\" kind=\"single\" />"
               "<field name=\"amount\" kind=\"single\" />"
               "</element>"
               "</field>";}

   //------ SoilWaterBalance ------

   void EXPORT pack(MessageData& messageData, const SoilWaterBalanceType& data)
      {
      pack(messageData, data.infiltration);
      pack(messageData, data.drainage);
      pack(messageData, data.evaporation);
      pack(messageData, data.LateralFlowLayer);
      }
   void EXPORT unpack(MessageData& messageData, SoilWaterBalanceType& data)
      {
      unpack(messageData, data.infiltration);
      unpack(messageData, data.drainage);
      unpack(messageData, data.evaporation);
      unpack(messageData, data.LateralFlowLayer);
      }
   unsigned EXPORT memorySize(const SoilWaterBalanceType& data)
      {
      return 0
              + ::memorySize(data.infiltration)
              + ::memorySize(data.drainage)
              + ::memorySize(data.evaporation)
              + ::memorySize(data.LateralFlowLayer)
              ;
      }
   std::string EXPORT DDML(const SoilWaterBalanceType& data)
      {return "<type name=\"SoilWaterBalance\">"
               "<field name=\"infiltration\" kind=\"single\" />"
               "<field name=\"drainage\" kind=\"single\" />"
               "<field name=\"evaporation\" kind=\"single\" />"
               "<field name=\"LateralFlowLayer\" array=\"T\">"
               "<element>"
               "<field name=\"thickness\" kind=\"single\" />"
               "<field name=\"amount\" kind=\"single\" />"
               "</element>"
               "</field>"
               "</type>";}

   //------ NewSolute ------

   void EXPORT pack(MessageData& messageData, const NewSoluteType& data)
      {
      pack(messageData, data.sender_name);
      pack(messageData, data.sender_numbytes);
      pack(messageData, data.sender_code);
      pack(messageData, data.sender_isarray);
      pack(messageData, data.sender_value);
      pack(messageData, data.sender_id_name);
      pack(messageData, data.sender_id_numbytes);
      pack(messageData, data.sender_id_code);
      pack(messageData, data.sender_id_isarray);
      pack(messageData, data.sender_id_value);
      pack(messageData, data.solute_names_name);
      pack(messageData, data.solute_names_numbytes);
      pack(messageData, data.solute_names_code);
      pack(messageData, data.solute_names_isarray);
      pack(messageData, data.solute_names_value);
      }
   void EXPORT unpack(MessageData& messageData, NewSoluteType& data)
      {
      unpack(messageData, data.sender_name);
      unpack(messageData, data.sender_numbytes);
      unpack(messageData, data.sender_code);
      unpack(messageData, data.sender_isarray);
      unpack(messageData, data.sender_value);
      unpack(messageData, data.sender_id_name);
      unpack(messageData, data.sender_id_numbytes);
      unpack(messageData, data.sender_id_code);
      unpack(messageData, data.sender_id_isarray);
      unpack(messageData, data.sender_id_value);
      unpack(messageData, data.solute_names_name);
      unpack(messageData, data.solute_names_numbytes);
      unpack(messageData, data.solute_names_code);
      unpack(messageData, data.solute_names_isarray);
      unpack(messageData, data.solute_names_value);
      }
   unsigned EXPORT memorySize(const NewSoluteType& data)
      {
      return 0
              + ::memorySize(data.sender_name)
              + ::memorySize(data.sender_numbytes)
              + ::memorySize(data.sender_code)
              + ::memorySize(data.sender_isarray)
              + ::memorySize(data.sender_value)
              + ::memorySize(data.sender_id_name)
              + ::memorySize(data.sender_id_numbytes)
              + ::memorySize(data.sender_id_code)
              + ::memorySize(data.sender_id_isarray)
              + ::memorySize(data.sender_id_value)
              + ::memorySize(data.solute_names_name)
              + ::memorySize(data.solute_names_numbytes)
              + ::memorySize(data.solute_names_code)
              + ::memorySize(data.solute_names_isarray)
              + ::memorySize(data.solute_names_value)
              ;
      }
   std::string EXPORT DDML(const NewSoluteType& data)
      {return "<type name=\"NewSolute\">"
               "<field name=\"sender_name\" kind=\"string\" />"
               "<field name=\"sender_numbytes\" kind=\"integer4\" />"
               "<field name=\"sender_code\" kind=\"integer4\" />"
               "<field name=\"sender_isarray\" kind=\"boolean\" />"
               "<field name=\"sender_value\" kind=\"string\" />"
               "<field name=\"sender_id_name\" kind=\"string\" />"
               "<field name=\"sender_id_numbytes\" kind=\"integer4\" />"
               "<field name=\"sender_id_code\" kind=\"integer4\" />"
               "<field name=\"sender_id_isarray\" kind=\"boolean\" />"
               "<field name=\"sender_id_value\" kind=\"integer4\" />"
               "<field name=\"solute_names_name\" kind=\"string\" />"
               "<field name=\"solute_names_numbytes\" kind=\"integer4\" />"
               "<field name=\"solute_names_code\" kind=\"integer4\" />"
               "<field name=\"solute_names_isarray\" kind=\"boolean\" />"
               "<field name=\"solute_names_value\" kind=\"string\" array=\"T\" />"
               "</type>";}

   //------ layer ------

   void EXPORT pack(MessageData& messageData, const layerType& data)
      {
      pack(messageData, data.thickness);
      pack(messageData, data.amount);
      }
   void EXPORT unpack(MessageData& messageData, layerType& data)
      {
      unpack(messageData, data.thickness);
      unpack(messageData, data.amount);
      }
   unsigned EXPORT memorySize(const layerType& data)
      {
      return 0
              + ::memorySize(data.thickness)
              + ::memorySize(data.amount)
              ;
      }
   std::string EXPORT DDML(const layerType& data)
      {return "<field name=\"layer\" array=\"T\">"
               "<element>"
               "<field name=\"thickness\" kind=\"single\" />"
               "<field name=\"amount\" kind=\"single\" />"
               "</element>"
               "</field>";}

   //------ SoluteProfile ------

   void EXPORT pack(MessageData& messageData, const SoluteProfileType& data)
      {
      pack(messageData, data.name);
      pack(messageData, data.layer);
      }
   void EXPORT unpack(MessageData& messageData, SoluteProfileType& data)
      {
      unpack(messageData, data.name);
      unpack(messageData, data.layer);
      }
   unsigned EXPORT memorySize(const SoluteProfileType& data)
      {
      return 0
              + ::memorySize(data.name)
              + ::memorySize(data.layer)
              ;
      }
   std::string EXPORT DDML(const SoluteProfileType& data)
      {return "<type name=\"SoluteProfile\" array=\"T\">"
               "<element>"
               "<field name=\"name\" kind=\"string\" />"
               "<field name=\"layer\" array=\"T\">"
               "<element>"
               "<field name=\"thickness\" kind=\"single\" />"
               "<field name=\"amount\" kind=\"single\" />"
               "</element>"
               "</field>"
               "</element>"
               "</type>";}

   //------ Irrigated ------

   void EXPORT pack(MessageData& messageData, const IrrigatedType& data)
      {
      pack(messageData, data.sender_name);
      pack(messageData, data.sender_numbytes);
      pack(messageData, data.sender_code);
      pack(messageData, data.sender_isarray);
      pack(messageData, data.sender_value);
      pack(messageData, data.sender_id_name);
      pack(messageData, data.sender_id_numbytes);
      pack(messageData, data.sender_id_code);
      pack(messageData, data.sender_id_isarray);
      pack(messageData, data.sender_id_value);
      pack(messageData, data.amount_name);
      pack(messageData, data.amount_numbytes);
      pack(messageData, data.amount_code);
      pack(messageData, data.amount_isarray);
      pack(messageData, data.amount_value);
      }
   void EXPORT unpack(MessageData& messageData, IrrigatedType& data)
      {
      unpack(messageData, data.sender_name);
      unpack(messageData, data.sender_numbytes);
      unpack(messageData, data.sender_code);
      unpack(messageData, data.sender_isarray);
      unpack(messageData, data.sender_value);
      unpack(messageData, data.sender_id_name);
      unpack(messageData, data.sender_id_numbytes);
      unpack(messageData, data.sender_id_code);
      unpack(messageData, data.sender_id_isarray);
      unpack(messageData, data.sender_id_value);
      unpack(messageData, data.amount_name);
      unpack(messageData, data.amount_numbytes);
      unpack(messageData, data.amount_code);
      unpack(messageData, data.amount_isarray);
      unpack(messageData, data.amount_value);
      }
   unsigned EXPORT memorySize(const IrrigatedType& data)
      {
      return 0
              + ::memorySize(data.sender_name)
              + ::memorySize(data.sender_numbytes)
              + ::memorySize(data.sender_code)
              + ::memorySize(data.sender_isarray)
              + ::memorySize(data.sender_value)
              + ::memorySize(data.sender_id_name)
              + ::memorySize(data.sender_id_numbytes)
              + ::memorySize(data.sender_id_code)
              + ::memorySize(data.sender_id_isarray)
              + ::memorySize(data.sender_id_value)
              + ::memorySize(data.amount_name)
              + ::memorySize(data.amount_numbytes)
              + ::memorySize(data.amount_code)
              + ::memorySize(data.amount_isarray)
              + ::memorySize(data.amount_value)
              ;
      }
   std::string EXPORT DDML(const IrrigatedType& data)
      {return "<type name=\"Irrigated\" description=\"Sent when an irrigation occurs\">"
               "<field name=\"sender_name\" kind=\"string\" />"
               "<field name=\"sender_numbytes\" kind=\"integer4\" />"
               "<field name=\"sender_code\" kind=\"integer4\" />"
               "<field name=\"sender_isarray\" kind=\"boolean\" />"
               "<field name=\"sender_value\" kind=\"string\" />"
               "<field name=\"sender_id_name\" kind=\"string\" />"
               "<field name=\"sender_id_numbytes\" kind=\"integer4\" />"
               "<field name=\"sender_id_code\" kind=\"integer4\" />"
               "<field name=\"sender_id_isarray\" kind=\"boolean\" />"
               "<field name=\"sender_id_value\" kind=\"integer4\" />"
               "<field name=\"amount_name\" kind=\"string\" />"
               "<field name=\"amount_numbytes\" kind=\"integer4\" />"
               "<field name=\"amount_code\" kind=\"integer4\" />"
               "<field name=\"amount_isarray\" kind=\"boolean\" />"
               "<field name=\"amount_value\" kind=\"single\" />"
               "</type>";}

   //------ KillCrop ------

   void EXPORT pack(MessageData& messageData, const KillCropType& data)
      {
      pack(messageData, data.KillFraction);
      }
   void EXPORT unpack(MessageData& messageData, KillCropType& data)
      {
      unpack(messageData, data.KillFraction);
      }
   unsigned EXPORT memorySize(const KillCropType& data)
      {
      return 0
              + ::memorySize(data.KillFraction)
              ;
      }
   std::string EXPORT DDML(const KillCropType& data)
      {return "<type name=\"KillCrop\">"
               "<field name=\"KillFraction\" kind=\"single\" />"
               "</type>";}

   //------ Interception ------

   void EXPORT pack(MessageData& messageData, const InterceptionType& data)
      {
      pack(messageData, data.name);
      pack(messageData, data.CropType);
      pack(messageData, data.layer);
      }
   void EXPORT unpack(MessageData& messageData, InterceptionType& data)
      {
      unpack(messageData, data.name);
      unpack(messageData, data.CropType);
      unpack(messageData, data.layer);
      }
   unsigned EXPORT memorySize(const InterceptionType& data)
      {
      return 0
              + ::memorySize(data.name)
              + ::memorySize(data.CropType)
              + ::memorySize(data.layer)
              ;
      }
   std::string EXPORT DDML(const InterceptionType& data)
      {return "<field name=\"Interception\" array=\"T\">"
               "<element>"
               "<field name=\"name\" kind=\"string\" />"
               "<field name=\"CropType\" kind=\"string\" />"
               "<field name=\"layer\" array=\"T\">"
               "<element>"
               "<field name=\"thickness\" kind=\"single\" />"
               "<field name=\"amount\" kind=\"single\" />"
               "</element>"
               "</field>"
               "</element>"
               "</field>";}

   //------ LightProfile ------

   void EXPORT pack(MessageData& messageData, const LightProfileType& data)
      {
      pack(messageData, data.Interception);
      pack(messageData, data.transmission);
      }
   void EXPORT unpack(MessageData& messageData, LightProfileType& data)
      {
      unpack(messageData, data.Interception);
      unpack(messageData, data.transmission);
      }
   unsigned EXPORT memorySize(const LightProfileType& data)
      {
      return 0
              + ::memorySize(data.Interception)
              + ::memorySize(data.transmission)
              ;
      }
   std::string EXPORT DDML(const LightProfileType& data)
      {return "<type name=\"LightProfile\">"
               "<field name=\"Interception\" array=\"T\">"
               "<element>"
               "<field name=\"name\" kind=\"string\" />"
               "<field name=\"CropType\" kind=\"string\" />"
               "<field name=\"layer\" array=\"T\">"
               "<element>"
               "<field name=\"thickness\" kind=\"single\" />"
               "<field name=\"amount\" kind=\"single\" />"
               "</element>"
               "</field>"
               "</element>"
               "</field>"
               "<field name=\"transmission\" kind=\"single\" />"
               "</type>";}

   //------ Canopy ------

   void EXPORT pack(MessageData& messageData, const CanopyType& data)
      {
      pack(messageData, data.name);
      pack(messageData, data.CropType);
      pack(messageData, data.PotentialEp);
      }
   void EXPORT unpack(MessageData& messageData, CanopyType& data)
      {
      unpack(messageData, data.name);
      unpack(messageData, data.CropType);
      unpack(messageData, data.PotentialEp);
      }
   unsigned EXPORT memorySize(const CanopyType& data)
      {
      return 0
              + ::memorySize(data.name)
              + ::memorySize(data.CropType)
              + ::memorySize(data.PotentialEp)
              ;
      }
   std::string EXPORT DDML(const CanopyType& data)
      {return "<field name=\"Canopy\" array=\"T\">"
               "<element>"
               "<field name=\"name\" kind=\"string\" />"
               "<field name=\"CropType\" kind=\"string\" />"
               "<field name=\"PotentialEp\" kind=\"single\" />"
               "</element>"
               "</field>";}

   //------ CanopyWaterBalance ------

   void EXPORT pack(MessageData& messageData, const CanopyWaterBalanceType& data)
      {
      pack(messageData, data.Canopy);
      pack(messageData, data.eo);
      pack(messageData, data.interception);
      }
   void EXPORT unpack(MessageData& messageData, CanopyWaterBalanceType& data)
      {
      unpack(messageData, data.Canopy);
      unpack(messageData, data.eo);
      unpack(messageData, data.interception);
      }
   unsigned EXPORT memorySize(const CanopyWaterBalanceType& data)
      {
      return 0
              + ::memorySize(data.Canopy)
              + ::memorySize(data.eo)
              + ::memorySize(data.interception)
              ;
      }
   std::string EXPORT DDML(const CanopyWaterBalanceType& data)
      {return "<type name=\"CanopyWaterBalance\">"
               "<field name=\"Canopy\" array=\"T\">"
               "<element>"
               "<field name=\"name\" kind=\"string\" />"
               "<field name=\"CropType\" kind=\"string\" />"
               "<field name=\"PotentialEp\" kind=\"single\" />"
               "</element>"
               "</field>"
               "<field name=\"eo\" kind=\"single\" />"
               "<field name=\"interception\" kind=\"single\" />"
               "</type>";}

   //------ OrganicMatterFraction ------

   void EXPORT pack(MessageData& messageData, const OrganicMatterFractionType& data)
      {
      pack(messageData, data.amount);
      pack(messageData, data.C);
      pack(messageData, data.N);
      pack(messageData, data.P);
      pack(messageData, data.AshAlk);
      }
   void EXPORT unpack(MessageData& messageData, OrganicMatterFractionType& data)
      {
      unpack(messageData, data.amount);
      unpack(messageData, data.C);
      unpack(messageData, data.N);
      unpack(messageData, data.P);
      unpack(messageData, data.AshAlk);
      }
   unsigned EXPORT memorySize(const OrganicMatterFractionType& data)
      {
      return 0
              + ::memorySize(data.amount)
              + ::memorySize(data.C)
              + ::memorySize(data.N)
              + ::memorySize(data.P)
              + ::memorySize(data.AshAlk)
              ;
      }
   std::string EXPORT DDML(const OrganicMatterFractionType& data)
      {return "<field name=\"OrganicMatterFraction\" array=\"T\">"
               "<element>"
               "<field name=\"amount\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"C\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"N\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"P\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"AshAlk\" kind=\"single\" units=\"kg/ha\" />"
               "</element>"
               "</field>";}

   //------ Residue ------

   void EXPORT pack(MessageData& messageData, const ResidueType& data)
      {
      pack(messageData, data.name);
      pack(messageData, data.OrganicMatterType);
      pack(messageData, data.OrganicMatterFraction);
      pack(messageData, data.Cover);
      }
   void EXPORT unpack(MessageData& messageData, ResidueType& data)
      {
      unpack(messageData, data.name);
      unpack(messageData, data.OrganicMatterType);
      unpack(messageData, data.OrganicMatterFraction);
      unpack(messageData, data.Cover);
      }
   unsigned EXPORT memorySize(const ResidueType& data)
      {
      return 0
              + ::memorySize(data.name)
              + ::memorySize(data.OrganicMatterType)
              + ::memorySize(data.OrganicMatterFraction)
              + ::memorySize(data.Cover)
              ;
      }
   std::string EXPORT DDML(const ResidueType& data)
      {return "<type name=\"Residue\" array=\"T\">"
               "<element>"
               "<field name=\"name\" kind=\"string\" />"
               "<field name=\"OrganicMatterType\" kind=\"string\" />"
               "<field name=\"OrganicMatterFraction\" array=\"T\">"
               "<element>"
               "<field name=\"amount\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"C\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"N\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"P\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"AshAlk\" kind=\"single\" units=\"kg/ha\" />"
               "</element>"
               "</field>"
               "<field name=\"Cover\" kind=\"single\" units=\"\" />"
               "</element>"
               "</type>";}

   //------ solute ------

   void EXPORT pack(MessageData& messageData, const soluteType& data)
      {
      pack(messageData, data.name);
      pack(messageData, data.amount);
      }
   void EXPORT unpack(MessageData& messageData, soluteType& data)
      {
      unpack(messageData, data.name);
      unpack(messageData, data.amount);
      }
   unsigned EXPORT memorySize(const soluteType& data)
      {
      return 0
              + ::memorySize(data.name)
              + ::memorySize(data.amount)
              ;
      }
   std::string EXPORT DDML(const soluteType& data)
      {return "<field name=\"solute\" array=\"T\">"
               "<element>"
               "<field name=\"name\" kind=\"string\" />"
               "<field name=\"amount\" kind=\"single\" units=\"kg/ha\" />"
               "</element>"
               "</field>";}

   //------ SurfaceWater ------

   void EXPORT pack(MessageData& messageData, const SurfaceWaterType& data)
      {
      pack(messageData, data.amount);
      pack(messageData, data.solute);
      }
   void EXPORT unpack(MessageData& messageData, SurfaceWaterType& data)
      {
      unpack(messageData, data.amount);
      unpack(messageData, data.solute);
      }
   unsigned EXPORT memorySize(const SurfaceWaterType& data)
      {
      return 0
              + ::memorySize(data.amount)
              + ::memorySize(data.solute)
              ;
      }
   std::string EXPORT DDML(const SurfaceWaterType& data)
      {return "<type name=\"SurfaceWater\">"
               "<field name=\"amount\" kind=\"single\" />"
               "<field name=\"solute\" array=\"T\">"
               "<element>"
               "<field name=\"name\" kind=\"string\" />"
               "<field name=\"amount\" kind=\"single\" units=\"kg/ha\" />"
               "</element>"
               "</field>"
               "</type>";}

   //------ SurfaceWaterBalance ------

   void EXPORT pack(MessageData& messageData, const SurfaceWaterBalanceType& data)
      {
      pack(messageData, data.runoff);
      pack(messageData, data.evaporation);
      pack(messageData, data.runon);
      pack(messageData, data.WaterInput);
      }
   void EXPORT unpack(MessageData& messageData, SurfaceWaterBalanceType& data)
      {
      unpack(messageData, data.runoff);
      unpack(messageData, data.evaporation);
      unpack(messageData, data.runon);
      unpack(messageData, data.WaterInput);
      }
   unsigned EXPORT memorySize(const SurfaceWaterBalanceType& data)
      {
      return 0
              + ::memorySize(data.runoff)
              + ::memorySize(data.evaporation)
              + ::memorySize(data.runon)
              + ::memorySize(data.WaterInput)
              ;
      }
   std::string EXPORT DDML(const SurfaceWaterBalanceType& data)
      {return "<type name=\"SurfaceWaterBalance\">"
               "<field name=\"runoff\" kind=\"single\" />"
               "<field name=\"evaporation\" kind=\"single\" />"
               "<field name=\"runon\" kind=\"single\" />"
               "<field name=\"WaterInput\" kind=\"single\" />"
               "</type>";}

   //------ FertiliserConstituents ------

   void EXPORT pack(MessageData& messageData, const FertiliserConstituentsType& data)
      {
      pack(messageData, data.name);
      pack(messageData, data.SurfaceAmount);
      pack(messageData, data.layer);
      }
   void EXPORT unpack(MessageData& messageData, FertiliserConstituentsType& data)
      {
      unpack(messageData, data.name);
      unpack(messageData, data.SurfaceAmount);
      unpack(messageData, data.layer);
      }
   unsigned EXPORT memorySize(const FertiliserConstituentsType& data)
      {
      return 0
              + ::memorySize(data.name)
              + ::memorySize(data.SurfaceAmount)
              + ::memorySize(data.layer)
              ;
      }
   std::string EXPORT DDML(const FertiliserConstituentsType& data)
      {return "<type name=\"FertiliserConstituents\" array=\"T\">"
               "<element>"
               "<field name=\"name\" kind=\"string\" />"
               "<field name=\"SurfaceAmount\" kind=\"single\" />"
               "<field name=\"layer\" array=\"T\">"
               "<element>"
               "<field name=\"thickness\" kind=\"single\" units=\"mm\" />"
               "<field name=\"amount\" kind=\"single\" units=\"kg/ha\" />"
               "</element>"
               "</field>"
               "</element>"
               "</type>";}

   //------ FPool ------

   void EXPORT pack(MessageData& messageData, const FPoolType& data)
      {
      pack(messageData, data.C);
      pack(messageData, data.N);
      pack(messageData, data.P);
      pack(messageData, data.AshAlk);
      }
   void EXPORT unpack(MessageData& messageData, FPoolType& data)
      {
      unpack(messageData, data.C);
      unpack(messageData, data.N);
      unpack(messageData, data.P);
      unpack(messageData, data.AshAlk);
      }
   unsigned EXPORT memorySize(const FPoolType& data)
      {
      return 0
              + ::memorySize(data.C)
              + ::memorySize(data.N)
              + ::memorySize(data.P)
              + ::memorySize(data.AshAlk)
              ;
      }
   std::string EXPORT DDML(const FPoolType& data)
      {return "<field name=\"FPool\" array=\"T\">"
               "<element>"
               "<field name=\"C\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"N\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"P\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"AshAlk\" kind=\"single\" units=\"kg/ha\" />"
               "</element>"
               "</field>";}

   //------ FPoolProfileLayer ------

   void EXPORT pack(MessageData& messageData, const FPoolProfileLayerType& data)
      {
      pack(messageData, data.thickness);
      pack(messageData, data.no3);
      pack(messageData, data.nh4);
      pack(messageData, data.po4);
      pack(messageData, data.FPool);
      }
   void EXPORT unpack(MessageData& messageData, FPoolProfileLayerType& data)
      {
      unpack(messageData, data.thickness);
      unpack(messageData, data.no3);
      unpack(messageData, data.nh4);
      unpack(messageData, data.po4);
      unpack(messageData, data.FPool);
      }
   unsigned EXPORT memorySize(const FPoolProfileLayerType& data)
      {
      return 0
              + ::memorySize(data.thickness)
              + ::memorySize(data.no3)
              + ::memorySize(data.nh4)
              + ::memorySize(data.po4)
              + ::memorySize(data.FPool)
              ;
      }
   std::string EXPORT DDML(const FPoolProfileLayerType& data)
      {return "<type name=\"FPoolProfileLayer\" array=\"T\">"
               "<element>"
               "<field name=\"thickness\" kind=\"single\" units=\"mm\" />"
               "<field name=\"no3\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"nh4\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"po4\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"FPool\" array=\"T\">"
               "<element>"
               "<field name=\"C\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"N\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"P\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"AshAlk\" kind=\"single\" units=\"kg/ha\" />"
               "</element>"
               "</field>"
               "</element>"
               "</type>";}

   //------ StandingFraction ------

   void EXPORT pack(MessageData& messageData, const StandingFractionType& data)
      {
      pack(messageData, data.amount);
      pack(messageData, data.C);
      pack(messageData, data.N);
      pack(messageData, data.P);
      pack(messageData, data.AshAlk);
      }
   void EXPORT unpack(MessageData& messageData, StandingFractionType& data)
      {
      unpack(messageData, data.amount);
      unpack(messageData, data.C);
      unpack(messageData, data.N);
      unpack(messageData, data.P);
      unpack(messageData, data.AshAlk);
      }
   unsigned EXPORT memorySize(const StandingFractionType& data)
      {
      return 0
              + ::memorySize(data.amount)
              + ::memorySize(data.C)
              + ::memorySize(data.N)
              + ::memorySize(data.P)
              + ::memorySize(data.AshAlk)
              ;
      }
   std::string EXPORT DDML(const StandingFractionType& data)
      {return "<field name=\"StandingFraction\" array=\"T\">"
               "<element>"
               "<field name=\"amount\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"C\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"N\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"P\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"AshAlk\" kind=\"single\" units=\"kg/ha\" />"
               "</element>"
               "</field>";}

   //------ LyingFraction ------

   void EXPORT pack(MessageData& messageData, const LyingFractionType& data)
      {
      pack(messageData, data.amount);
      pack(messageData, data.C);
      pack(messageData, data.N);
      pack(messageData, data.P);
      pack(messageData, data.AshAlk);
      }
   void EXPORT unpack(MessageData& messageData, LyingFractionType& data)
      {
      unpack(messageData, data.amount);
      unpack(messageData, data.C);
      unpack(messageData, data.N);
      unpack(messageData, data.P);
      unpack(messageData, data.AshAlk);
      }
   unsigned EXPORT memorySize(const LyingFractionType& data)
      {
      return 0
              + ::memorySize(data.amount)
              + ::memorySize(data.C)
              + ::memorySize(data.N)
              + ::memorySize(data.P)
              + ::memorySize(data.AshAlk)
              ;
      }
   std::string EXPORT DDML(const LyingFractionType& data)
      {return "<field name=\"LyingFraction\" array=\"T\">"
               "<element>"
               "<field name=\"amount\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"C\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"N\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"P\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"AshAlk\" kind=\"single\" units=\"kg/ha\" />"
               "</element>"
               "</field>";}

   //------ SurfaceOrganicMatter ------

   void EXPORT pack(MessageData& messageData, const SurfaceOrganicMatterType& data)
      {
      pack(messageData, data.Name);
      pack(messageData, data.OrganicMatterType);
      pack(messageData, data.PotDecompRate);
      pack(messageData, data.no3);
      pack(messageData, data.nh4);
      pack(messageData, data.po4);
      pack(messageData, data.StandingFraction);
      pack(messageData, data.LyingFraction);
      }
   void EXPORT unpack(MessageData& messageData, SurfaceOrganicMatterType& data)
      {
      unpack(messageData, data.Name);
      unpack(messageData, data.OrganicMatterType);
      unpack(messageData, data.PotDecompRate);
      unpack(messageData, data.no3);
      unpack(messageData, data.nh4);
      unpack(messageData, data.po4);
      unpack(messageData, data.StandingFraction);
      unpack(messageData, data.LyingFraction);
      }
   unsigned EXPORT memorySize(const SurfaceOrganicMatterType& data)
      {
      return 0
              + ::memorySize(data.Name)
              + ::memorySize(data.OrganicMatterType)
              + ::memorySize(data.PotDecompRate)
              + ::memorySize(data.no3)
              + ::memorySize(data.nh4)
              + ::memorySize(data.po4)
              + ::memorySize(data.StandingFraction)
              + ::memorySize(data.LyingFraction)
              ;
      }
   std::string EXPORT DDML(const SurfaceOrganicMatterType& data)
      {return "<type name=\"SurfaceOrganicMatter\" array=\"T\">"
               "<element>"
               "<field name=\"Name\" kind=\"string\" />"
               "<field name=\"OrganicMatterType\" kind=\"string\" />"
               "<field name=\"PotDecompRate\" kind=\"single\" units=\"day^-1\" />"
               "<field name=\"no3\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"nh4\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"po4\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"StandingFraction\" array=\"T\">"
               "<element>"
               "<field name=\"amount\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"C\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"N\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"P\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"AshAlk\" kind=\"single\" units=\"kg/ha\" />"
               "</element>"
               "</field>"
               "<field name=\"LyingFraction\" array=\"T\">"
               "<element>"
               "<field name=\"amount\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"C\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"N\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"P\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"AshAlk\" kind=\"single\" units=\"kg/ha\" />"
               "</element>"
               "</field>"
               "</element>"
               "</type>";}

   //------ SurfaceOrganicMatterDecomp ------

   void EXPORT pack(MessageData& messageData, const SurfaceOrganicMatterDecompType& data)
      {
      pack(messageData, data.Name);
      pack(messageData, data.OrganicMatterType);
      pack(messageData, data.amount);
      pack(messageData, data.C);
      pack(messageData, data.N);
      pack(messageData, data.P);
      pack(messageData, data.AshAlk);
      }
   void EXPORT unpack(MessageData& messageData, SurfaceOrganicMatterDecompType& data)
      {
      unpack(messageData, data.Name);
      unpack(messageData, data.OrganicMatterType);
      unpack(messageData, data.amount);
      unpack(messageData, data.C);
      unpack(messageData, data.N);
      unpack(messageData, data.P);
      unpack(messageData, data.AshAlk);
      }
   unsigned EXPORT memorySize(const SurfaceOrganicMatterDecompType& data)
      {
      return 0
              + ::memorySize(data.Name)
              + ::memorySize(data.OrganicMatterType)
              + ::memorySize(data.amount)
              + ::memorySize(data.C)
              + ::memorySize(data.N)
              + ::memorySize(data.P)
              + ::memorySize(data.AshAlk)
              ;
      }
   std::string EXPORT DDML(const SurfaceOrganicMatterDecompType& data)
      {return "<type name=\"SurfaceOrganicMatterDecomp\" array=\"T\">"
               "<element>"
               "<field name=\"Name\" kind=\"string\" />"
               "<field name=\"OrganicMatterType\" kind=\"string\" />"
               "<field name=\"amount\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"C\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"N\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"P\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"AshAlk\" kind=\"single\" units=\"kg/ha\" />"
               "</element>"
               "</type>";}

   //------ NBalance ------

   void EXPORT pack(MessageData& messageData, const NBalanceType& data)
      {
      pack(messageData, data.sender_name);
      pack(messageData, data.sender_numbytes);
      pack(messageData, data.sender_code);
      pack(messageData, data.sender_isarray);
      pack(messageData, data.sender_value);
      pack(messageData, data.sender_id_name);
      pack(messageData, data.sender_id_numbytes);
      pack(messageData, data.sender_id_code);
      pack(messageData, data.sender_id_isarray);
      pack(messageData, data.sender_id_value);
      pack(messageData, data.nh4_transform_net_name);
      pack(messageData, data.nh4_transform_net_numbytes);
      pack(messageData, data.nh4_transform_net_code);
      pack(messageData, data.nh4_transform_net_isarray);
      pack(messageData, data.nh4_transform_net_value);
      pack(messageData, data.no3_transform_net_name);
      pack(messageData, data.no3_transform_net_numbytes);
      pack(messageData, data.no3_transform_net_code);
      pack(messageData, data.no3_transform_net_isarray);
      pack(messageData, data.no3_transform_net_value);
      pack(messageData, data.dlt_nh4_net_name);
      pack(messageData, data.dlt_nh4_net_numbytes);
      pack(messageData, data.dlt_nh4_net_code);
      pack(messageData, data.dlt_nh4_net_isarray);
      pack(messageData, data.dlt_nh4_net_value);
      pack(messageData, data.dlt_no3_net_name);
      pack(messageData, data.dlt_no3_net_numbytes);
      pack(messageData, data.dlt_no3_net_code);
      pack(messageData, data.dlt_no3_net_isarray);
      pack(messageData, data.dlt_no3_net_value);
      }
   void EXPORT unpack(MessageData& messageData, NBalanceType& data)
      {
      unpack(messageData, data.sender_name);
      unpack(messageData, data.sender_numbytes);
      unpack(messageData, data.sender_code);
      unpack(messageData, data.sender_isarray);
      unpack(messageData, data.sender_value);
      unpack(messageData, data.sender_id_name);
      unpack(messageData, data.sender_id_numbytes);
      unpack(messageData, data.sender_id_code);
      unpack(messageData, data.sender_id_isarray);
      unpack(messageData, data.sender_id_value);
      unpack(messageData, data.nh4_transform_net_name);
      unpack(messageData, data.nh4_transform_net_numbytes);
      unpack(messageData, data.nh4_transform_net_code);
      unpack(messageData, data.nh4_transform_net_isarray);
      unpack(messageData, data.nh4_transform_net_value);
      unpack(messageData, data.no3_transform_net_name);
      unpack(messageData, data.no3_transform_net_numbytes);
      unpack(messageData, data.no3_transform_net_code);
      unpack(messageData, data.no3_transform_net_isarray);
      unpack(messageData, data.no3_transform_net_value);
      unpack(messageData, data.dlt_nh4_net_name);
      unpack(messageData, data.dlt_nh4_net_numbytes);
      unpack(messageData, data.dlt_nh4_net_code);
      unpack(messageData, data.dlt_nh4_net_isarray);
      unpack(messageData, data.dlt_nh4_net_value);
      unpack(messageData, data.dlt_no3_net_name);
      unpack(messageData, data.dlt_no3_net_numbytes);
      unpack(messageData, data.dlt_no3_net_code);
      unpack(messageData, data.dlt_no3_net_isarray);
      unpack(messageData, data.dlt_no3_net_value);
      }
   unsigned EXPORT memorySize(const NBalanceType& data)
      {
      return 0
              + ::memorySize(data.sender_name)
              + ::memorySize(data.sender_numbytes)
              + ::memorySize(data.sender_code)
              + ::memorySize(data.sender_isarray)
              + ::memorySize(data.sender_value)
              + ::memorySize(data.sender_id_name)
              + ::memorySize(data.sender_id_numbytes)
              + ::memorySize(data.sender_id_code)
              + ::memorySize(data.sender_id_isarray)
              + ::memorySize(data.sender_id_value)
              + ::memorySize(data.nh4_transform_net_name)
              + ::memorySize(data.nh4_transform_net_numbytes)
              + ::memorySize(data.nh4_transform_net_code)
              + ::memorySize(data.nh4_transform_net_isarray)
              + ::memorySize(data.nh4_transform_net_value)
              + ::memorySize(data.no3_transform_net_name)
              + ::memorySize(data.no3_transform_net_numbytes)
              + ::memorySize(data.no3_transform_net_code)
              + ::memorySize(data.no3_transform_net_isarray)
              + ::memorySize(data.no3_transform_net_value)
              + ::memorySize(data.dlt_nh4_net_name)
              + ::memorySize(data.dlt_nh4_net_numbytes)
              + ::memorySize(data.dlt_nh4_net_code)
              + ::memorySize(data.dlt_nh4_net_isarray)
              + ::memorySize(data.dlt_nh4_net_value)
              + ::memorySize(data.dlt_no3_net_name)
              + ::memorySize(data.dlt_no3_net_numbytes)
              + ::memorySize(data.dlt_no3_net_code)
              + ::memorySize(data.dlt_no3_net_isarray)
              + ::memorySize(data.dlt_no3_net_value)
              ;
      }
   std::string EXPORT DDML(const NBalanceType& data)
      {return "<type name=\"NBalance\" description=\"Notification of the completion of the soil N calculations\">"
               "<field name=\"sender_name\" kind=\"string\" />"
               "<field name=\"sender_numbytes\" kind=\"integer4\" />"
               "<field name=\"sender_code\" kind=\"integer4\" />"
               "<field name=\"sender_isarray\" kind=\"boolean\" />"
               "<field name=\"sender_value\" kind=\"string\" />"
               "<field name=\"sender_id_name\" kind=\"string\" />"
               "<field name=\"sender_id_numbytes\" kind=\"integer4\" />"
               "<field name=\"sender_id_code\" kind=\"integer4\" />"
               "<field name=\"sender_id_isarray\" kind=\"boolean\" />"
               "<field name=\"sender_id_value\" kind=\"integer4\" />"
               "<field name=\"nh4_transform_net_name\" kind=\"string\" />"
               "<field name=\"nh4_transform_net_numbytes\" kind=\"integer4\" />"
               "<field name=\"nh4_transform_net_code\" kind=\"integer4\" />"
               "<field name=\"nh4_transform_net_isarray\" kind=\"boolean\" />"
               "<field name=\"nh4_transform_net_value\" kind=\"single\" array=\"T\" />"
               "<field name=\"no3_transform_net_name\" kind=\"string\" />"
               "<field name=\"no3_transform_net_numbytes\" kind=\"integer4\" />"
               "<field name=\"no3_transform_net_code\" kind=\"integer4\" />"
               "<field name=\"no3_transform_net_isarray\" kind=\"boolean\" />"
               "<field name=\"no3_transform_net_value\" kind=\"single\" array=\"T\" />"
               "<field name=\"dlt_nh4_net_name\" kind=\"string\" />"
               "<field name=\"dlt_nh4_net_numbytes\" kind=\"integer4\" />"
               "<field name=\"dlt_nh4_net_code\" kind=\"integer4\" />"
               "<field name=\"dlt_nh4_net_isarray\" kind=\"boolean\" />"
               "<field name=\"dlt_nh4_net_value\" kind=\"single\" array=\"T\" />"
               "<field name=\"dlt_no3_net_name\" kind=\"string\" />"
               "<field name=\"dlt_no3_net_numbytes\" kind=\"integer4\" />"
               "<field name=\"dlt_no3_net_code\" kind=\"integer4\" />"
               "<field name=\"dlt_no3_net_isarray\" kind=\"boolean\" />"
               "<field name=\"dlt_no3_net_value\" kind=\"single\" array=\"T\" />"
               "</type>";}

   //------ CBalance ------

   void EXPORT pack(MessageData& messageData, const CBalanceType& data)
      {
      pack(messageData, data.sender_name);
      pack(messageData, data.sender_numbytes);
      pack(messageData, data.sender_code);
      pack(messageData, data.sender_isarray);
      pack(messageData, data.sender_value);
      pack(messageData, data.sender_id_name);
      pack(messageData, data.sender_id_numbytes);
      pack(messageData, data.sender_id_code);
      pack(messageData, data.sender_id_isarray);
      pack(messageData, data.sender_id_value);
      pack(messageData, data.dlt_oc_name);
      pack(messageData, data.dlt_oc_numbytes);
      pack(messageData, data.dlt_oc_code);
      pack(messageData, data.dlt_oc_isarray);
      pack(messageData, data.dlt_oc_value);
      pack(messageData, data.dlt_om_name);
      pack(messageData, data.dlt_om_numbytes);
      pack(messageData, data.dlt_om_code);
      pack(messageData, data.dlt_om_isarray);
      pack(messageData, data.dlt_om_value);
      }
   void EXPORT unpack(MessageData& messageData, CBalanceType& data)
      {
      unpack(messageData, data.sender_name);
      unpack(messageData, data.sender_numbytes);
      unpack(messageData, data.sender_code);
      unpack(messageData, data.sender_isarray);
      unpack(messageData, data.sender_value);
      unpack(messageData, data.sender_id_name);
      unpack(messageData, data.sender_id_numbytes);
      unpack(messageData, data.sender_id_code);
      unpack(messageData, data.sender_id_isarray);
      unpack(messageData, data.sender_id_value);
      unpack(messageData, data.dlt_oc_name);
      unpack(messageData, data.dlt_oc_numbytes);
      unpack(messageData, data.dlt_oc_code);
      unpack(messageData, data.dlt_oc_isarray);
      unpack(messageData, data.dlt_oc_value);
      unpack(messageData, data.dlt_om_name);
      unpack(messageData, data.dlt_om_numbytes);
      unpack(messageData, data.dlt_om_code);
      unpack(messageData, data.dlt_om_isarray);
      unpack(messageData, data.dlt_om_value);
      }
   unsigned EXPORT memorySize(const CBalanceType& data)
      {
      return 0
              + ::memorySize(data.sender_name)
              + ::memorySize(data.sender_numbytes)
              + ::memorySize(data.sender_code)
              + ::memorySize(data.sender_isarray)
              + ::memorySize(data.sender_value)
              + ::memorySize(data.sender_id_name)
              + ::memorySize(data.sender_id_numbytes)
              + ::memorySize(data.sender_id_code)
              + ::memorySize(data.sender_id_isarray)
              + ::memorySize(data.sender_id_value)
              + ::memorySize(data.dlt_oc_name)
              + ::memorySize(data.dlt_oc_numbytes)
              + ::memorySize(data.dlt_oc_code)
              + ::memorySize(data.dlt_oc_isarray)
              + ::memorySize(data.dlt_oc_value)
              + ::memorySize(data.dlt_om_name)
              + ::memorySize(data.dlt_om_numbytes)
              + ::memorySize(data.dlt_om_code)
              + ::memorySize(data.dlt_om_isarray)
              + ::memorySize(data.dlt_om_value)
              ;
      }
   std::string EXPORT DDML(const CBalanceType& data)
      {return "<type name=\"CBalance\" description=\"Notification of the completion of the soil C calculations\">"
               "<field name=\"sender_name\" kind=\"string\" />"
               "<field name=\"sender_numbytes\" kind=\"integer4\" />"
               "<field name=\"sender_code\" kind=\"integer4\" />"
               "<field name=\"sender_isarray\" kind=\"boolean\" />"
               "<field name=\"sender_value\" kind=\"string\" />"
               "<field name=\"sender_id_name\" kind=\"string\" />"
               "<field name=\"sender_id_numbytes\" kind=\"integer4\" />"
               "<field name=\"sender_id_code\" kind=\"integer4\" />"
               "<field name=\"sender_id_isarray\" kind=\"boolean\" />"
               "<field name=\"sender_id_value\" kind=\"integer4\" />"
               "<field name=\"dlt_oc_name\" kind=\"string\" />"
               "<field name=\"dlt_oc_numbytes\" kind=\"integer4\" />"
               "<field name=\"dlt_oc_code\" kind=\"integer4\" />"
               "<field name=\"dlt_oc_isarray\" kind=\"boolean\" />"
               "<field name=\"dlt_oc_value\" kind=\"single\" array=\"T\" />"
               "<field name=\"dlt_om_name\" kind=\"string\" />"
               "<field name=\"dlt_om_numbytes\" kind=\"integer4\" />"
               "<field name=\"dlt_om_code\" kind=\"integer4\" />"
               "<field name=\"dlt_om_isarray\" kind=\"boolean\" />"
               "<field name=\"dlt_om_value\" kind=\"single\" array=\"T\" />"
               "</type>";}

   //------ IncorpFom ------

   void EXPORT pack(MessageData& messageData, const IncorpFomType& data)
      {
      pack(messageData, data.dlt_fom_type_name);
      pack(messageData, data.dlt_fom_type_numbytes);
      pack(messageData, data.dlt_fom_type_code);
      pack(messageData, data.dlt_fom_type_isarray);
      pack(messageData, data.dlt_fom_type_value);
      pack(messageData, data.dlt_fom_wt_name);
      pack(messageData, data.dlt_fom_wt_numbytes);
      pack(messageData, data.dlt_fom_wt_code);
      pack(messageData, data.dlt_fom_wt_isarray);
      pack(messageData, data.dlt_fom_wt_value);
      pack(messageData, data.dlt_fom_n_name);
      pack(messageData, data.dlt_fom_n_numbytes);
      pack(messageData, data.dlt_fom_n_code);
      pack(messageData, data.dlt_fom_n_isarray);
      pack(messageData, data.dlt_fom_n_value);
      pack(messageData, data.dlt_fom_p_name);
      pack(messageData, data.dlt_fom_p_numbytes);
      pack(messageData, data.dlt_fom_p_code);
      pack(messageData, data.dlt_fom_p_isarray);
      pack(messageData, data.dlt_fom_p_value);
      }
   void EXPORT unpack(MessageData& messageData, IncorpFomType& data)
      {
      unpack(messageData, data.dlt_fom_type_name);
      unpack(messageData, data.dlt_fom_type_numbytes);
      unpack(messageData, data.dlt_fom_type_code);
      unpack(messageData, data.dlt_fom_type_isarray);
      unpack(messageData, data.dlt_fom_type_value);
      unpack(messageData, data.dlt_fom_wt_name);
      unpack(messageData, data.dlt_fom_wt_numbytes);
      unpack(messageData, data.dlt_fom_wt_code);
      unpack(messageData, data.dlt_fom_wt_isarray);
      unpack(messageData, data.dlt_fom_wt_value);
      unpack(messageData, data.dlt_fom_n_name);
      unpack(messageData, data.dlt_fom_n_numbytes);
      unpack(messageData, data.dlt_fom_n_code);
      unpack(messageData, data.dlt_fom_n_isarray);
      unpack(messageData, data.dlt_fom_n_value);
      unpack(messageData, data.dlt_fom_p_name);
      unpack(messageData, data.dlt_fom_p_numbytes);
      unpack(messageData, data.dlt_fom_p_code);
      unpack(messageData, data.dlt_fom_p_isarray);
      unpack(messageData, data.dlt_fom_p_value);
      }
   unsigned EXPORT memorySize(const IncorpFomType& data)
      {
      return 0
              + ::memorySize(data.dlt_fom_type_name)
              + ::memorySize(data.dlt_fom_type_numbytes)
              + ::memorySize(data.dlt_fom_type_code)
              + ::memorySize(data.dlt_fom_type_isarray)
              + ::memorySize(data.dlt_fom_type_value)
              + ::memorySize(data.dlt_fom_wt_name)
              + ::memorySize(data.dlt_fom_wt_numbytes)
              + ::memorySize(data.dlt_fom_wt_code)
              + ::memorySize(data.dlt_fom_wt_isarray)
              + ::memorySize(data.dlt_fom_wt_value)
              + ::memorySize(data.dlt_fom_n_name)
              + ::memorySize(data.dlt_fom_n_numbytes)
              + ::memorySize(data.dlt_fom_n_code)
              + ::memorySize(data.dlt_fom_n_isarray)
              + ::memorySize(data.dlt_fom_n_value)
              + ::memorySize(data.dlt_fom_p_name)
              + ::memorySize(data.dlt_fom_p_numbytes)
              + ::memorySize(data.dlt_fom_p_code)
              + ::memorySize(data.dlt_fom_p_isarray)
              + ::memorySize(data.dlt_fom_p_value)
              ;
      }
   std::string EXPORT DDML(const IncorpFomType& data)
      {return "<type name=\"IncorpFom\">"
               "<field name=\"dlt_fom_type_name\" kind=\"string\" />"
               "<field name=\"dlt_fom_type_numbytes\" kind=\"integer4\" />"
               "<field name=\"dlt_fom_type_code\" kind=\"integer4\" />"
               "<field name=\"dlt_fom_type_isarray\" kind=\"boolean\" />"
               "<field name=\"dlt_fom_type_value\" kind=\"string\" />"
               "<field name=\"dlt_fom_wt_name\" kind=\"string\" />"
               "<field name=\"dlt_fom_wt_numbytes\" kind=\"integer4\" />"
               "<field name=\"dlt_fom_wt_code\" kind=\"integer4\" />"
               "<field name=\"dlt_fom_wt_isarray\" kind=\"boolean\" />"
               "<field name=\"dlt_fom_wt_value\" kind=\"single\" array=\"T\" />"
               "<field name=\"dlt_fom_n_name\" kind=\"string\" />"
               "<field name=\"dlt_fom_n_numbytes\" kind=\"integer4\" />"
               "<field name=\"dlt_fom_n_code\" kind=\"integer4\" />"
               "<field name=\"dlt_fom_n_isarray\" kind=\"boolean\" />"
               "<field name=\"dlt_fom_n_value\" kind=\"single\" array=\"T\" />"
               "<field name=\"dlt_fom_p_name\" kind=\"string\" />"
               "<field name=\"dlt_fom_p_numbytes\" kind=\"integer4\" />"
               "<field name=\"dlt_fom_p_code\" kind=\"integer4\" />"
               "<field name=\"dlt_fom_p_isarray\" kind=\"boolean\" />"
               "<field name=\"dlt_fom_p_value\" kind=\"single\" array=\"T\" />"
               "</type>";}

   //------ SoilOrganicMatter ------

   void EXPORT pack(MessageData& messageData, const SoilOrganicMatterType& data)
      {
      pack(messageData, data.OrganicMatterType);
      pack(messageData, data.layer);
      }
   void EXPORT unpack(MessageData& messageData, SoilOrganicMatterType& data)
      {
      unpack(messageData, data.OrganicMatterType);
      unpack(messageData, data.layer);
      }
   unsigned EXPORT memorySize(const SoilOrganicMatterType& data)
      {
      return 0
              + ::memorySize(data.OrganicMatterType)
              + ::memorySize(data.layer)
              ;
      }
   std::string EXPORT DDML(const SoilOrganicMatterType& data)
      {return "<type name=\"SoilOrganicMatter\">"
               "<field name=\"OrganicMatterType\" kind=\"string\" />"
               "<field name=\"layer\" array=\"T\">"
               "<element>"
               "<field name=\"OrganicMatterFraction\" array=\"T\">"
               "<element>"
               "<field name=\"amount\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"C\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"N\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"P\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"AshAlk\" kind=\"single\" units=\"kg/ha\" />"
               "</element>"
               "</field>"
               "</element>"
               "</field>"
               "</type>";}

   //------ CropChopped ------

   void EXPORT pack(MessageData& messageData, const CropChoppedType& data)
      {
      pack(messageData, data.crop_type);
      pack(messageData, data.dm_type);
      pack(messageData, data.dlt_crop_dm);
      pack(messageData, data.dlt_dm_n);
      pack(messageData, data.fraction_to_residue);
      }
   void EXPORT unpack(MessageData& messageData, CropChoppedType& data)
      {
      unpack(messageData, data.crop_type);
      unpack(messageData, data.dm_type);
      unpack(messageData, data.dlt_crop_dm);
      unpack(messageData, data.dlt_dm_n);
      unpack(messageData, data.fraction_to_residue);
      }
   unsigned EXPORT memorySize(const CropChoppedType& data)
      {
      return 0
              + ::memorySize(data.crop_type)
              + ::memorySize(data.dm_type)
              + ::memorySize(data.dlt_crop_dm)
              + ::memorySize(data.dlt_dm_n)
              + ::memorySize(data.fraction_to_residue)
              ;
      }
   std::string EXPORT DDML(const CropChoppedType& data)
      {return "<type name=\"CropChopped\">"
               "<field name=\"crop_type\" kind=\"string\" />"
               "<field name=\"dm_type\" kind=\"string\" array=\"T\" />"
               "<field name=\"dlt_crop_dm\" kind=\"single\" array=\"T\" />"
               "<field name=\"dlt_dm_n\" kind=\"single\" array=\"T\" />"
               "<field name=\"fraction_to_residue\" kind=\"single\" array=\"T\" />"
               "</type>";}

   //------ NewProfile ------

   void EXPORT pack(MessageData& messageData, const NewProfileType& data)
      {
      pack(messageData, data.dlayer);
      pack(messageData, data.air_dry_dep);
      pack(messageData, data.ll15_dep);
      pack(messageData, data.dul_dep);
      pack(messageData, data.sat_dep);
      pack(messageData, data.sw_dep);
      pack(messageData, data.bd);
      }
   void EXPORT unpack(MessageData& messageData, NewProfileType& data)
      {
      unpack(messageData, data.dlayer);
      unpack(messageData, data.air_dry_dep);
      unpack(messageData, data.ll15_dep);
      unpack(messageData, data.dul_dep);
      unpack(messageData, data.sat_dep);
      unpack(messageData, data.sw_dep);
      unpack(messageData, data.bd);
      }
   unsigned EXPORT memorySize(const NewProfileType& data)
      {
      return 0
              + ::memorySize(data.dlayer)
              + ::memorySize(data.air_dry_dep)
              + ::memorySize(data.ll15_dep)
              + ::memorySize(data.dul_dep)
              + ::memorySize(data.sat_dep)
              + ::memorySize(data.sw_dep)
              + ::memorySize(data.bd)
              ;
      }
   std::string EXPORT DDML(const NewProfileType& data)
      {return "<type name=\"NewProfile\">"
               "<field name=\"dlayer\" kind=\"single\" array=\"T\" />"
               "<field name=\"air_dry_dep\" kind=\"single\" array=\"T\" />"
               "<field name=\"ll15_dep\" kind=\"single\" array=\"T\" />"
               "<field name=\"dul_dep\" kind=\"single\" array=\"T\" />"
               "<field name=\"sat_dep\" kind=\"single\" array=\"T\" />"
               "<field name=\"sw_dep\" kind=\"single\" array=\"T\" />"
               "<field name=\"bd\" kind=\"single\" array=\"T\" />"
               "</type>";}

   //------ NewPotentialGrowth ------

   void EXPORT pack(MessageData& messageData, const NewPotentialGrowthType& data)
      {
      pack(messageData, data.sender);
      pack(messageData, data.frgr);
      }
   void EXPORT unpack(MessageData& messageData, NewPotentialGrowthType& data)
      {
      unpack(messageData, data.sender);
      unpack(messageData, data.frgr);
      }
   unsigned EXPORT memorySize(const NewPotentialGrowthType& data)
      {
      return 0
              + ::memorySize(data.sender)
              + ::memorySize(data.frgr)
              ;
      }
   std::string EXPORT DDML(const NewPotentialGrowthType& data)
      {return "<type name=\"NewPotentialGrowth\">"
               "<field name=\"sender\" kind=\"string\" />"
               "<field name=\"frgr\" kind=\"single\" units=\"\" />"
               "</type>";}

   //------ NewCanopy ------

   void EXPORT pack(MessageData& messageData, const NewCanopyType& data)
      {
      pack(messageData, data.sender);
      pack(messageData, data.height);
      pack(messageData, data.depth);
      pack(messageData, data.lai);
      pack(messageData, data.lai_tot);
      pack(messageData, data.cover);
      pack(messageData, data.cover_tot);
      }
   void EXPORT unpack(MessageData& messageData, NewCanopyType& data)
      {
      unpack(messageData, data.sender);
      unpack(messageData, data.height);
      unpack(messageData, data.depth);
      unpack(messageData, data.lai);
      unpack(messageData, data.lai_tot);
      unpack(messageData, data.cover);
      unpack(messageData, data.cover_tot);
      }
   unsigned EXPORT memorySize(const NewCanopyType& data)
      {
      return 0
              + ::memorySize(data.sender)
              + ::memorySize(data.height)
              + ::memorySize(data.depth)
              + ::memorySize(data.lai)
              + ::memorySize(data.lai_tot)
              + ::memorySize(data.cover)
              + ::memorySize(data.cover_tot)
              ;
      }
   std::string EXPORT DDML(const NewCanopyType& data)
      {return "<type name=\"NewCanopy\">"
               "<field name=\"sender\" kind=\"string\" />"
               "<field name=\"height\" kind=\"single\" />"
               "<field name=\"depth\" kind=\"single\" />"
               "<field name=\"lai\" kind=\"single\" />"
               "<field name=\"lai_tot\" kind=\"single\" />"
               "<field name=\"cover\" kind=\"single\" />"
               "<field name=\"cover_tot\" kind=\"single\" />"
               "</type>";}

   //------ NewCrop ------

   void EXPORT pack(MessageData& messageData, const NewCropType& data)
      {
      pack(messageData, data.sender);
      pack(messageData, data.crop_type);
      }
   void EXPORT unpack(MessageData& messageData, NewCropType& data)
      {
      unpack(messageData, data.sender);
      unpack(messageData, data.crop_type);
      }
   unsigned EXPORT memorySize(const NewCropType& data)
      {
      return 0
              + ::memorySize(data.sender)
              + ::memorySize(data.crop_type)
              ;
      }
   std::string EXPORT DDML(const NewCropType& data)
      {return "<type name=\"NewCrop\">"
               "<field name=\"sender\" kind=\"string\" />"
               "<field name=\"crop_type\" kind=\"string\" />"
               "</type>";}

   //------ NewZone ------

   void EXPORT pack(MessageData& messageData, const NewZoneType& data)
      {
      pack(messageData, data.sender);
      pack(messageData, data.area);
      pack(messageData, data.slope);
      pack(messageData, data.X);
      pack(messageData, data.Y);
      }
   void EXPORT unpack(MessageData& messageData, NewZoneType& data)
      {
      unpack(messageData, data.sender);
      unpack(messageData, data.area);
      unpack(messageData, data.slope);
      unpack(messageData, data.X);
      unpack(messageData, data.Y);
      }
   unsigned EXPORT memorySize(const NewZoneType& data)
      {
      return 0
              + ::memorySize(data.sender)
              + ::memorySize(data.area)
              + ::memorySize(data.slope)
              + ::memorySize(data.X)
              + ::memorySize(data.Y)
              ;
      }
   std::string EXPORT DDML(const NewZoneType& data)
      {return "<type name=\"NewZone\">"
               "<field name=\"sender\" kind=\"string\" />"
               "<field name=\"area\" kind=\"single\" />"
               "<field name=\"slope\" kind=\"single\" />"
               "<field name=\"X\" kind=\"single\" />"
               "<field name=\"Y\" kind=\"single\" />"
               "</type>";}

   //------ SoilLayers ------

   void EXPORT pack(MessageData& messageData, const SoilLayersType& data)
      {
      pack(messageData, data.layers);
      pack(messageData, data.value);
      }
   void EXPORT unpack(MessageData& messageData, SoilLayersType& data)
      {
      unpack(messageData, data.layers);
      unpack(messageData, data.value);
      }
   unsigned EXPORT memorySize(const SoilLayersType& data)
      {
      return 0
              + ::memorySize(data.layers)
              + ::memorySize(data.value)
              ;
      }
   std::string EXPORT DDML(const SoilLayersType& data)
      {return "<type name=\"SoilLayers\">"
               "<field name=\"layers\" kind=\"double\" unit=\"mm\" array=\"T\" />"
               "<field name=\"value\" kind=\"double\" unit=\"mm\" array=\"T\" />"
               "</type>";}

   //------ rlv_layer ------

   void EXPORT pack(MessageData& messageData, const rlv_layerType& data)
      {
      pack(messageData, data.layers);
      pack(messageData, data.rlv);
      }
   void EXPORT unpack(MessageData& messageData, rlv_layerType& data)
      {
      unpack(messageData, data.layers);
      unpack(messageData, data.rlv);
      }
   unsigned EXPORT memorySize(const rlv_layerType& data)
      {
      return 0
              + ::memorySize(data.layers)
              + ::memorySize(data.rlv)
              ;
      }
   std::string EXPORT DDML(const rlv_layerType& data)
      {return "<field name=\"rlv_layer\">"
               "<field name=\"layers\" kind=\"double\" units=\"mm\" array=\"T\" />"
               "<field name=\"rlv\" kind=\"double\" units=\"mmmm^3\" array=\"T\" />"
               "</field>";}

   //------ demands ------

   void EXPORT pack(MessageData& messageData, const demandsType& data)
      {
      pack(messageData, data.crop_ident);
      pack(messageData, data.crop_type);
      pack(messageData, data.rlv_layer);
      pack(messageData, data.demand);
      }
   void EXPORT unpack(MessageData& messageData, demandsType& data)
      {
      unpack(messageData, data.crop_ident);
      unpack(messageData, data.crop_type);
      unpack(messageData, data.rlv_layer);
      unpack(messageData, data.demand);
      }
   unsigned EXPORT memorySize(const demandsType& data)
      {
      return 0
              + ::memorySize(data.crop_ident)
              + ::memorySize(data.crop_type)
              + ::memorySize(data.rlv_layer)
              + ::memorySize(data.demand)
              ;
      }
   std::string EXPORT DDML(const demandsType& data)
      {return "<field name=\"demands\" array=\"T\">"
               "<element>"
               "<field name=\"crop_ident\" kind=\"string\" description=\"Cohort name + instance name\" />"
               "<field name=\"crop_type\" kind=\"string\" />"
               "<field name=\"rlv_layer\">"
               "<field name=\"layers\" kind=\"double\" units=\"mm\" array=\"T\" />"
               "<field name=\"rlv\" kind=\"double\" units=\"mmmm^3\" array=\"T\" />"
               "</field>"
               "<field name=\"demand\" kind=\"double\" unit=\"mm\" />"
               "</element>"
               "</field>";}

   //------ PastureWaterDemand ------

   void EXPORT pack(MessageData& messageData, const PastureWaterDemandType& data)
      {
      pack(messageData, data.demands);
      }
   void EXPORT unpack(MessageData& messageData, PastureWaterDemandType& data)
      {
      unpack(messageData, data.demands);
      }
   unsigned EXPORT memorySize(const PastureWaterDemandType& data)
      {
      return 0
              + ::memorySize(data.demands)
              ;
      }
   std::string EXPORT DDML(const PastureWaterDemandType& data)
      {return "<type name=\"PastureWaterDemand\" description=\"Message sent to add crop water demand terms to water balance calculations.\">"
               "<field name=\"demands\" array=\"T\">"
               "<element>"
               "<field name=\"crop_ident\" kind=\"string\" description=\"Cohort name + instance name\" />"
               "<field name=\"crop_type\" kind=\"string\" />"
               "<field name=\"rlv_layer\">"
               "<field name=\"layers\" kind=\"double\" units=\"mm\" array=\"T\" />"
               "<field name=\"rlv\" kind=\"double\" units=\"mmmm^3\" array=\"T\" />"
               "</field>"
               "<field name=\"demand\" kind=\"double\" unit=\"mm\" />"
               "</element>"
               "</field>"
               "</type>";}

   //------ supplies ------

   void EXPORT pack(MessageData& messageData, const suppliesType& data)
      {
      pack(messageData, data.crop_ident);
      pack(messageData, data.layers);
      pack(messageData, data.supply);
      }
   void EXPORT unpack(MessageData& messageData, suppliesType& data)
      {
      unpack(messageData, data.crop_ident);
      unpack(messageData, data.layers);
      unpack(messageData, data.supply);
      }
   unsigned EXPORT memorySize(const suppliesType& data)
      {
      return 0
              + ::memorySize(data.crop_ident)
              + ::memorySize(data.layers)
              + ::memorySize(data.supply)
              ;
      }
   std::string EXPORT DDML(const suppliesType& data)
      {return "<field name=\"supplies\" array=\"T\">"
               "<element>"
               "<field name=\"crop_ident\" kind=\"string\" description=\"Cohort name + instance name\" />"
               "<field name=\"layers\" kind=\"double\" unit=\"mm\" array=\"T\" />"
               "<field name=\"supply\" kind=\"double\" unit=\"mm\" array=\"T\" />"
               "</element>"
               "</field>";}

   //------ PastureWaterSupply ------

   void EXPORT pack(MessageData& messageData, const PastureWaterSupplyType& data)
      {
      pack(messageData, data.supplies);
      }
   void EXPORT unpack(MessageData& messageData, PastureWaterSupplyType& data)
      {
      unpack(messageData, data.supplies);
      }
   unsigned EXPORT memorySize(const PastureWaterSupplyType& data)
      {
      return 0
              + ::memorySize(data.supplies)
              ;
      }
   std::string EXPORT DDML(const PastureWaterSupplyType& data)
      {return "<type name=\"PastureWaterSupply\">"
               "<field name=\"supplies\" array=\"T\">"
               "<element>"
               "<field name=\"crop_ident\" kind=\"string\" description=\"Cohort name + instance name\" />"
               "<field name=\"layers\" kind=\"double\" unit=\"mm\" array=\"T\" />"
               "<field name=\"supply\" kind=\"double\" unit=\"mm\" array=\"T\" />"
               "</element>"
               "</field>"
               "</type>";}

   //------ water_uptake ------

   void EXPORT pack(MessageData& messageData, const water_uptakeType& data)
      {
      pack(messageData, data.crop_ident);
      pack(messageData, data.layers);
      pack(messageData, data.uptake);
      }
   void EXPORT unpack(MessageData& messageData, water_uptakeType& data)
      {
      unpack(messageData, data.crop_ident);
      unpack(messageData, data.layers);
      unpack(messageData, data.uptake);
      }
   unsigned EXPORT memorySize(const water_uptakeType& data)
      {
      return 0
              + ::memorySize(data.crop_ident)
              + ::memorySize(data.layers)
              + ::memorySize(data.uptake)
              ;
      }
   std::string EXPORT DDML(const water_uptakeType& data)
      {return "<field name=\"water_uptake\" array=\"T\">"
               "<element>"
               "<field name=\"crop_ident\" kind=\"string\" description=\"Cohort name + instance name\" />"
               "<field name=\"layers\" kind=\"double\" unit=\"mm\" array=\"T\" />"
               "<field name=\"uptake\" kind=\"double\" unit=\"mm\" array=\"T\" />"
               "</element>"
               "</field>";}

   //------ PastureWaterUptake ------

   void EXPORT pack(MessageData& messageData, const PastureWaterUptakeType& data)
      {
      pack(messageData, data.water_uptake);
      }
   void EXPORT unpack(MessageData& messageData, PastureWaterUptakeType& data)
      {
      unpack(messageData, data.water_uptake);
      }
   unsigned EXPORT memorySize(const PastureWaterUptakeType& data)
      {
      return 0
              + ::memorySize(data.water_uptake)
              ;
      }
   std::string EXPORT DDML(const PastureWaterUptakeType& data)
      {return "<type name=\"PastureWaterUptake\">"
               "<field name=\"water_uptake\" array=\"T\">"
               "<element>"
               "<field name=\"crop_ident\" kind=\"string\" description=\"Cohort name + instance name\" />"
               "<field name=\"layers\" kind=\"double\" unit=\"mm\" array=\"T\" />"
               "<field name=\"uptake\" kind=\"double\" unit=\"mm\" array=\"T\" />"
               "</element>"
               "</field>"
               "</type>";}

   //------ water_info ------

   void EXPORT pack(MessageData& messageData, const water_infoType& data)
      {
      pack(messageData, data.crop_ident);
      pack(messageData, data.params);
      pack(messageData, data.demand);
      pack(messageData, data.layers);
      pack(messageData, data.rlv);
      pack(messageData, data.root_radius);
      }
   void EXPORT unpack(MessageData& messageData, water_infoType& data)
      {
      unpack(messageData, data.crop_ident);
      unpack(messageData, data.params);
      unpack(messageData, data.demand);
      unpack(messageData, data.layers);
      unpack(messageData, data.rlv);
      unpack(messageData, data.root_radius);
      }
   unsigned EXPORT memorySize(const water_infoType& data)
      {
      return 0
              + ::memorySize(data.crop_ident)
              + ::memorySize(data.params)
              + ::memorySize(data.demand)
              + ::memorySize(data.layers)
              + ::memorySize(data.rlv)
              + ::memorySize(data.root_radius)
              ;
      }
   std::string EXPORT DDML(const water_infoType& data)
      {return "<field name=\"water_info\" array=\"T\">"
               "<element>"
               "<field name=\"crop_ident\" kind=\"string\" description=\"sub-population name\" />"
               "<field name=\"params\" kind=\"double\" unit=\"mm\" array=\"T\" />"
               "<field name=\"demand\" kind=\"double\" unit=\"mm\" />"
               "<field name=\"layers\" kind=\"double\" unit=\"mm\" array=\"T\" />"
               "<field name=\"rlv\" kind=\"double\" unit=\"mm/mm^3\" array=\"T\" />"
               "<field name=\"root_radius\" kind=\"double\" unit=\"mm\" array=\"T\" />"
               "</element>"
               "</field>";}

   //------ WaterInfo ------

   void EXPORT pack(MessageData& messageData, const WaterInfoType& data)
      {
      pack(messageData, data.water_info);
      }
   void EXPORT unpack(MessageData& messageData, WaterInfoType& data)
      {
      unpack(messageData, data.water_info);
      }
   unsigned EXPORT memorySize(const WaterInfoType& data)
      {
      return 0
              + ::memorySize(data.water_info)
              ;
      }
   std::string EXPORT DDML(const WaterInfoType& data)
      {return "<type name=\"WaterInfo\">"
               "<field name=\"water_info\" array=\"T\">"
               "<element>"
               "<field name=\"crop_ident\" kind=\"string\" description=\"sub-population name\" />"
               "<field name=\"params\" kind=\"double\" unit=\"mm\" array=\"T\" />"
               "<field name=\"demand\" kind=\"double\" unit=\"mm\" />"
               "<field name=\"layers\" kind=\"double\" unit=\"mm\" array=\"T\" />"
               "<field name=\"rlv\" kind=\"double\" unit=\"mm/mm^3\" array=\"T\" />"
               "<field name=\"root_radius\" kind=\"double\" unit=\"mm\" array=\"T\" />"
               "</element>"
               "</field>"
               "</type>";}

   //------ fom ------

   void EXPORT pack(MessageData& messageData, const fomType& data)
      {
      pack(messageData, data.weight);
      pack(messageData, data.n);
      pack(messageData, data.p);
      pack(messageData, data.s);
      pack(messageData, data.ash_alk);
      pack(messageData, data.dmd);
      }
   void EXPORT unpack(MessageData& messageData, fomType& data)
      {
      unpack(messageData, data.weight);
      unpack(messageData, data.n);
      unpack(messageData, data.p);
      unpack(messageData, data.s);
      unpack(messageData, data.ash_alk);
      unpack(messageData, data.dmd);
      }
   unsigned EXPORT memorySize(const fomType& data)
      {
      return 0
              + ::memorySize(data.weight)
              + ::memorySize(data.n)
              + ::memorySize(data.p)
              + ::memorySize(data.s)
              + ::memorySize(data.ash_alk)
              + ::memorySize(data.dmd)
              ;
      }
   std::string EXPORT DDML(const fomType& data)
      {return "<field name=\"fom\" array=\"T\">"
               "<element>"
               "<field name=\"weight\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"n\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"p\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"s\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"ash_alk\" unit=\"mol/ha\" kind=\"double\" />"
               "<field name=\"dmd\" unit=\"-\" kind=\"double\" />"
               "</element>"
               "</field>";}

   //------ FomAdded ------

   void EXPORT pack(MessageData& messageData, const FomAddedType& data)
      {
      pack(messageData, data.layers);
      pack(messageData, data.fom);
      }
   void EXPORT unpack(MessageData& messageData, FomAddedType& data)
      {
      unpack(messageData, data.layers);
      unpack(messageData, data.fom);
      }
   unsigned EXPORT memorySize(const FomAddedType& data)
      {
      return 0
              + ::memorySize(data.layers)
              + ::memorySize(data.fom)
              ;
      }
   std::string EXPORT DDML(const FomAddedType& data)
      {return "<type name=\"FomAdded\">"
               "<field name=\"layers\" unit=\"mm\" kind=\"double\" array=\"T\" />"
               "<field name=\"fom\" array=\"T\">"
               "<element>"
               "<field name=\"weight\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"n\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"p\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"s\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"ash_alk\" unit=\"mol/ha\" kind=\"double\" />"
               "<field name=\"dmd\" unit=\"-\" kind=\"double\" />"
               "</element>"
               "</field>"
               "</type>";}

   //------ PastureNutrientUptake ------

   void EXPORT pack(MessageData& messageData, const PastureNutrientUptakeType& data)
      {
      pack(messageData, data.nutrient);
      pack(messageData, data.layers);
      pack(messageData, data.uptake);
      }
   void EXPORT unpack(MessageData& messageData, PastureNutrientUptakeType& data)
      {
      unpack(messageData, data.nutrient);
      unpack(messageData, data.layers);
      unpack(messageData, data.uptake);
      }
   unsigned EXPORT memorySize(const PastureNutrientUptakeType& data)
      {
      return 0
              + ::memorySize(data.nutrient)
              + ::memorySize(data.layers)
              + ::memorySize(data.uptake)
              ;
      }
   std::string EXPORT DDML(const PastureNutrientUptakeType& data)
      {return "<type name=\"PastureNutrientUptake\">"
               "<field name=\"nutrient\" kind=\"string\" />"
               "<field name=\"layers\" unit=\"mm\" kind=\"double\" array=\"T\" />"
               "<field name=\"uptake\" unit=\"kg/ha\" kind=\"double\" array=\"T\" />"
               "</type>";}

   //------ PastureSow ------

   void EXPORT pack(MessageData& messageData, const PastureSowType& data)
      {
      pack(messageData, data.rate);
      }
   void EXPORT unpack(MessageData& messageData, PastureSowType& data)
      {
      unpack(messageData, data.rate);
      }
   unsigned EXPORT memorySize(const PastureSowType& data)
      {
      return 0
              + ::memorySize(data.rate)
              ;
      }
   std::string EXPORT DDML(const PastureSowType& data)
      {return "<type name=\"PastureSow\">"
               "<field name=\"rate\" unit=\"kg/ha\" kind=\"double\" />"
               "</type>";}

   //------ PastureKill ------

   void EXPORT pack(MessageData& messageData, const PastureKillType& data)
      {
      pack(messageData, data.propn_herbage);
      pack(messageData, data.propn_seed);
      }
   void EXPORT unpack(MessageData& messageData, PastureKillType& data)
      {
      unpack(messageData, data.propn_herbage);
      unpack(messageData, data.propn_seed);
      }
   unsigned EXPORT memorySize(const PastureKillType& data)
      {
      return 0
              + ::memorySize(data.propn_herbage)
              + ::memorySize(data.propn_seed)
              ;
      }
   std::string EXPORT DDML(const PastureKillType& data)
      {return "<type name=\"PastureKill\">"
               "<field name=\"propn_herbage\" unit=\"-\" kind=\"double\" />"
               "<field name=\"propn_seed\" unit=\"-\" kind=\"double\" />"
               "</type>";}

   //------ PastureCultivate ------

   void EXPORT pack(MessageData& messageData, const PastureCultivateType& data)
      {
      pack(messageData, data.depth);
      pack(messageData, data.propn_incorp);
      pack(messageData, data.propn_mixed);
      }
   void EXPORT unpack(MessageData& messageData, PastureCultivateType& data)
      {
      unpack(messageData, data.depth);
      unpack(messageData, data.propn_incorp);
      unpack(messageData, data.propn_mixed);
      }
   unsigned EXPORT memorySize(const PastureCultivateType& data)
      {
      return 0
              + ::memorySize(data.depth)
              + ::memorySize(data.propn_incorp)
              + ::memorySize(data.propn_mixed)
              ;
      }
   std::string EXPORT DDML(const PastureCultivateType& data)
      {return "<type name=\"PastureCultivate\">"
               "<field name=\"depth\" unit=\"mm\" kind=\"double\" />"
               "<field name=\"propn_incorp\" unit=\"-\" kind=\"double\" />"
               "<field name=\"propn_mixed\" unit=\"-\" kind=\"double\" />"
               "</type>";}

   //------ PastureCut ------

   void EXPORT pack(MessageData& messageData, const PastureCutType& data)
      {
      pack(messageData, data.cut_height);
      pack(messageData, data.gathered);
      pack(messageData, data.dmd_loss);
      pack(messageData, data.dm_content);
      }
   void EXPORT unpack(MessageData& messageData, PastureCutType& data)
      {
      unpack(messageData, data.cut_height);
      unpack(messageData, data.gathered);
      unpack(messageData, data.dmd_loss);
      unpack(messageData, data.dm_content);
      }
   unsigned EXPORT memorySize(const PastureCutType& data)
      {
      return 0
              + ::memorySize(data.cut_height)
              + ::memorySize(data.gathered)
              + ::memorySize(data.dmd_loss)
              + ::memorySize(data.dm_content)
              ;
      }
   std::string EXPORT DDML(const PastureCutType& data)
      {return "<type name=\"PastureCut\">"
               "<field name=\"cut_height\" unit=\"mm\" kind=\"double\" />"
               "<field name=\"gathered\" unit=\"-\" kind=\"double\" />"
               "<field name=\"dmd_loss\" unit=\"-\" kind=\"double\" />"
               "<field name=\"dm_content\" unit=\"kg/kg\" kind=\"double\" />"
               "</type>";}

   //------ PastureBurn ------

   void EXPORT pack(MessageData& messageData, const PastureBurnType& data)
      {
      pack(messageData, data.kill_plants);
      pack(messageData, data.kill_seed);
      pack(messageData, data.propn_unburnt);
      }
   void EXPORT unpack(MessageData& messageData, PastureBurnType& data)
      {
      unpack(messageData, data.kill_plants);
      unpack(messageData, data.kill_seed);
      unpack(messageData, data.propn_unburnt);
      }
   unsigned EXPORT memorySize(const PastureBurnType& data)
      {
      return 0
              + ::memorySize(data.kill_plants)
              + ::memorySize(data.kill_seed)
              + ::memorySize(data.propn_unburnt)
              ;
      }
   std::string EXPORT DDML(const PastureBurnType& data)
      {return "<type name=\"PastureBurn\">"
               "<field name=\"kill_plants\" unit=\"-\" kind=\"double\" />"
               "<field name=\"kill_seed\" unit=\"-\" kind=\"double\" />"
               "<field name=\"propn_unburnt\" unit=\"-\" kind=\"double\" />"
               "</type>";}

   //------ PastureOnCut ------

   void EXPORT pack(MessageData& messageData, const PastureOnCutType& data)
      {
      pack(messageData, data.fresh_wt);
      pack(messageData, data.dm_content);
      pack(messageData, data.dm);
      pack(messageData, data.cp_conc);
      pack(messageData, data.p_conc);
      pack(messageData, data.s_conc);
      pack(messageData, data.ash_alk);
      }
   void EXPORT unpack(MessageData& messageData, PastureOnCutType& data)
      {
      unpack(messageData, data.fresh_wt);
      unpack(messageData, data.dm_content);
      unpack(messageData, data.dm);
      unpack(messageData, data.cp_conc);
      unpack(messageData, data.p_conc);
      unpack(messageData, data.s_conc);
      unpack(messageData, data.ash_alk);
      }
   unsigned EXPORT memorySize(const PastureOnCutType& data)
      {
      return 0
              + ::memorySize(data.fresh_wt)
              + ::memorySize(data.dm_content)
              + ::memorySize(data.dm)
              + ::memorySize(data.cp_conc)
              + ::memorySize(data.p_conc)
              + ::memorySize(data.s_conc)
              + ::memorySize(data.ash_alk)
              ;
      }
   std::string EXPORT DDML(const PastureOnCutType& data)
      {return "<type name=\"PastureOnCut\">"
               "<field name=\"fresh_wt\" unit=\"kg\" kind=\"double\" />"
               "<field name=\"dm_content\" unit=\"kg/kg\" kind=\"double\" />"
               "<field name=\"dm\" unit=\"-\" kind=\"double\" />"
               "<field name=\"cp_conc\" unit=\"kg/kg\" kind=\"double\" />"
               "<field name=\"p_conc\" unit=\"kg/kg\" kind=\"double\" />"
               "<field name=\"s_conc\" unit=\"kg/kg\" kind=\"double\" />"
               "<field name=\"ash_alk\" unit=\"mol/kg\" kind=\"double\" />"
               "</type>";}

   //------ PastureWeather ------

   void EXPORT pack(MessageData& messageData, const PastureWeatherType& data)
      {
      pack(messageData, data.maxt);
      pack(messageData, data.mint);
      pack(messageData, data.rain);
      pack(messageData, data.snow);
      pack(messageData, data.radn);
      pack(messageData, data.vpd);
      pack(messageData, data.wind);
      }
   void EXPORT unpack(MessageData& messageData, PastureWeatherType& data)
      {
      unpack(messageData, data.maxt);
      unpack(messageData, data.mint);
      unpack(messageData, data.rain);
      unpack(messageData, data.snow);
      unpack(messageData, data.radn);
      unpack(messageData, data.vpd);
      unpack(messageData, data.wind);
      }
   unsigned EXPORT memorySize(const PastureWeatherType& data)
      {
      return 0
              + ::memorySize(data.maxt)
              + ::memorySize(data.mint)
              + ::memorySize(data.rain)
              + ::memorySize(data.snow)
              + ::memorySize(data.radn)
              + ::memorySize(data.vpd)
              + ::memorySize(data.wind)
              ;
      }
   std::string EXPORT DDML(const PastureWeatherType& data)
      {return "<type name=\"PastureWeather\">"
               "<field name=\"maxt\" unit=\"oC\" kind=\"double\" />"
               "<field name=\"mint\" unit=\"oC\" kind=\"double\" />"
               "<field name=\"rain\" unit=\"mm/d\" kind=\"double\" />"
               "<field name=\"snow\" unit=\"mm/d\" kind=\"double\" />"
               "<field name=\"radn\" unit=\"MJ/m2/d\" kind=\"double\" />"
               "<field name=\"vpd\" unit=\"kPa\" kind=\"double\" />"
               "<field name=\"wind\" unit=\"m/s\" kind=\"double\" />"
               "</type>";}

   //------ Faeces ------

   void EXPORT pack(MessageData& messageData, const FaecesType& data)
      {
      pack(messageData, data.weight);
      pack(messageData, data.n);
      pack(messageData, data.p);
      pack(messageData, data.s);
      pack(messageData, data.ash_alk);
      }
   void EXPORT unpack(MessageData& messageData, FaecesType& data)
      {
      unpack(messageData, data.weight);
      unpack(messageData, data.n);
      unpack(messageData, data.p);
      unpack(messageData, data.s);
      unpack(messageData, data.ash_alk);
      }
   unsigned EXPORT memorySize(const FaecesType& data)
      {
      return 0
              + ::memorySize(data.weight)
              + ::memorySize(data.n)
              + ::memorySize(data.p)
              + ::memorySize(data.s)
              + ::memorySize(data.ash_alk)
              ;
      }
   std::string EXPORT DDML(const FaecesType& data)
      {return "<type name=\"Faeces\" array=\"T\">"
               "<element>"
               "<field name=\"weight\" unit=\"kg/d\" kind=\"double\" />"
               "<field name=\"n\" unit=\"kg/d\" kind=\"double\" />"
               "<field name=\"p\" unit=\"kg/d\" kind=\"double\" />"
               "<field name=\"s\" unit=\"kg/d\" kind=\"double\" />"
               "<field name=\"ash_alk\" unit=\"mol/d\" kind=\"double\" />"
               "</element>"
               "</type>";}

   //------ FaecesInorg ------

   void EXPORT pack(MessageData& messageData, const FaecesInorgType& data)
      {
      pack(messageData, data.n);
      pack(messageData, data.p);
      pack(messageData, data.s);
      }
   void EXPORT unpack(MessageData& messageData, FaecesInorgType& data)
      {
      unpack(messageData, data.n);
      unpack(messageData, data.p);
      unpack(messageData, data.s);
      }
   unsigned EXPORT memorySize(const FaecesInorgType& data)
      {
      return 0
              + ::memorySize(data.n)
              + ::memorySize(data.p)
              + ::memorySize(data.s)
              ;
      }
   std::string EXPORT DDML(const FaecesInorgType& data)
      {return "<type name=\"FaecesInorg\" array=\"T\">"
               "<element>"
               "<field name=\"n\" unit=\"kg/d\" kind=\"double\" />"
               "<field name=\"p\" unit=\"kg/d\" kind=\"double\" />"
               "<field name=\"s\" unit=\"kg/d\" kind=\"double\" />"
               "</element>"
               "</type>";}

   //------ Intake ------

   void EXPORT pack(MessageData& messageData, const IntakeType& data)
      {
      pack(messageData, data.weight);
      pack(messageData, data.n);
      pack(messageData, data.p);
      pack(messageData, data.s);
      pack(messageData, data.ash_alk);
      }
   void EXPORT unpack(MessageData& messageData, IntakeType& data)
      {
      unpack(messageData, data.weight);
      unpack(messageData, data.n);
      unpack(messageData, data.p);
      unpack(messageData, data.s);
      unpack(messageData, data.ash_alk);
      }
   unsigned EXPORT memorySize(const IntakeType& data)
      {
      return 0
              + ::memorySize(data.weight)
              + ::memorySize(data.n)
              + ::memorySize(data.p)
              + ::memorySize(data.s)
              + ::memorySize(data.ash_alk)
              ;
      }
   std::string EXPORT DDML(const IntakeType& data)
      {return "<type name=\"Intake\" array=\"T\">"
               "<element>"
               "<field name=\"weight\" unit=\"kg/d\" kind=\"double\" />"
               "<field name=\"n\" unit=\"kg/d\" kind=\"double\" />"
               "<field name=\"p\" unit=\"kg/d\" kind=\"double\" />"
               "<field name=\"s\" unit=\"kg/d\" kind=\"double\" />"
               "<field name=\"ash_alk\" unit=\"mol/d\" kind=\"double\" />"
               "</element>"
               "</type>";}

   //------ PastIntake ------

   void EXPORT pack(MessageData& messageData, const PastIntakeType& data)
      {
      pack(messageData, data.weight);
      pack(messageData, data.n);
      pack(messageData, data.p);
      pack(messageData, data.s);
      pack(messageData, data.ash_alk);
      }
   void EXPORT unpack(MessageData& messageData, PastIntakeType& data)
      {
      unpack(messageData, data.weight);
      unpack(messageData, data.n);
      unpack(messageData, data.p);
      unpack(messageData, data.s);
      unpack(messageData, data.ash_alk);
      }
   unsigned EXPORT memorySize(const PastIntakeType& data)
      {
      return 0
              + ::memorySize(data.weight)
              + ::memorySize(data.n)
              + ::memorySize(data.p)
              + ::memorySize(data.s)
              + ::memorySize(data.ash_alk)
              ;
      }
   std::string EXPORT DDML(const PastIntakeType& data)
      {return "<type name=\"PastIntake\" array=\"T\">"
               "<element>"
               "<field name=\"weight\" unit=\"kg/d\" kind=\"double\" />"
               "<field name=\"n\" unit=\"kg/d\" kind=\"double\" />"
               "<field name=\"p\" unit=\"kg/d\" kind=\"double\" />"
               "<field name=\"s\" unit=\"kg/d\" kind=\"double\" />"
               "<field name=\"ash_alk\" unit=\"mol/d\" kind=\"double\" />"
               "</element>"
               "</type>";}

   //------ SuppIntake ------

   void EXPORT pack(MessageData& messageData, const SuppIntakeType& data)
      {
      pack(messageData, data.weight);
      pack(messageData, data.n);
      pack(messageData, data.p);
      pack(messageData, data.s);
      pack(messageData, data.ash_alk);
      }
   void EXPORT unpack(MessageData& messageData, SuppIntakeType& data)
      {
      unpack(messageData, data.weight);
      unpack(messageData, data.n);
      unpack(messageData, data.p);
      unpack(messageData, data.s);
      unpack(messageData, data.ash_alk);
      }
   unsigned EXPORT memorySize(const SuppIntakeType& data)
      {
      return 0
              + ::memorySize(data.weight)
              + ::memorySize(data.n)
              + ::memorySize(data.p)
              + ::memorySize(data.s)
              + ::memorySize(data.ash_alk)
              ;
      }
   std::string EXPORT DDML(const SuppIntakeType& data)
      {return "<type name=\"SuppIntake\" array=\"T\">"
               "<element>"
               "<field name=\"weight\" unit=\"kg/d\" kind=\"double\" />"
               "<field name=\"n\" unit=\"kg/d\" kind=\"double\" />"
               "<field name=\"p\" unit=\"kg/d\" kind=\"double\" />"
               "<field name=\"s\" unit=\"kg/d\" kind=\"double\" />"
               "<field name=\"ash_alk\" unit=\"mol/d\" kind=\"double\" />"
               "</element>"
               "</type>";}

   //------ faeces_om ------

   void EXPORT pack(MessageData& messageData, const faeces_omType& data)
      {
      pack(messageData, data.weight);
      pack(messageData, data.n);
      pack(messageData, data.p);
      pack(messageData, data.s);
      pack(messageData, data.ash_alk);
      }
   void EXPORT unpack(MessageData& messageData, faeces_omType& data)
      {
      unpack(messageData, data.weight);
      unpack(messageData, data.n);
      unpack(messageData, data.p);
      unpack(messageData, data.s);
      unpack(messageData, data.ash_alk);
      }
   unsigned EXPORT memorySize(const faeces_omType& data)
      {
      return 0
              + ::memorySize(data.weight)
              + ::memorySize(data.n)
              + ::memorySize(data.p)
              + ::memorySize(data.s)
              + ::memorySize(data.ash_alk)
              ;
      }
   std::string EXPORT DDML(const faeces_omType& data)
      {return "<type name=\"faeces_om\">"
               "<field name=\"weight\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"n\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"p\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"s\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"ash_alk\" unit=\"mol/ha\" kind=\"double\" />"
               "</type>";}

   //------ faeces_inorg ------

   void EXPORT pack(MessageData& messageData, const faeces_inorgType& data)
      {
      pack(messageData, data.n);
      pack(messageData, data.p);
      pack(messageData, data.s);
      }
   void EXPORT unpack(MessageData& messageData, faeces_inorgType& data)
      {
      unpack(messageData, data.n);
      unpack(messageData, data.p);
      unpack(messageData, data.s);
      }
   unsigned EXPORT memorySize(const faeces_inorgType& data)
      {
      return 0
              + ::memorySize(data.n)
              + ::memorySize(data.p)
              + ::memorySize(data.s)
              ;
      }
   std::string EXPORT DDML(const faeces_inorgType& data)
      {return "<type name=\"faeces_inorg\">"
               "<field name=\"n\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"p\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"s\" unit=\"kg/ha\" kind=\"double\" />"
               "</type>";}

   //------ urine ------

   void EXPORT pack(MessageData& messageData, const urineType& data)
      {
      pack(messageData, data.volume);
      pack(messageData, data.urea);
      pack(messageData, data.pox);
      pack(messageData, data.so4);
      pack(messageData, data.ash_alk);
      }
   void EXPORT unpack(MessageData& messageData, urineType& data)
      {
      unpack(messageData, data.volume);
      unpack(messageData, data.urea);
      unpack(messageData, data.pox);
      unpack(messageData, data.so4);
      unpack(messageData, data.ash_alk);
      }
   unsigned EXPORT memorySize(const urineType& data)
      {
      return 0
              + ::memorySize(data.volume)
              + ::memorySize(data.urea)
              + ::memorySize(data.pox)
              + ::memorySize(data.so4)
              + ::memorySize(data.ash_alk)
              ;
      }
   std::string EXPORT DDML(const urineType& data)
      {return "<type name=\"urine\">"
               "<field name=\"volume\" unit=\"m3/ha\" kind=\"double\" />"
               "<field name=\"urea\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"pox\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"so4\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"ash_alk\" unit=\"mol/ha\" kind=\"double\" />"
               "</type>";}

   //------ AddExcreta ------

   void EXPORT pack(MessageData& messageData, const AddExcretaType& data)
      {
      pack(messageData, data.faeces_om);
      pack(messageData, data.faeces_inorg);
      pack(messageData, data.urine);
      }
   void EXPORT unpack(MessageData& messageData, AddExcretaType& data)
      {
      unpack(messageData, data.faeces_om);
      unpack(messageData, data.faeces_inorg);
      unpack(messageData, data.urine);
      }
   unsigned EXPORT memorySize(const AddExcretaType& data)
      {
      return 0
              + ::memorySize(data.faeces_om)
              + ::memorySize(data.faeces_inorg)
              + ::memorySize(data.urine)
              ;
      }
   std::string EXPORT DDML(const AddExcretaType& data)
      {return "<type name=\"AddExcreta\">"
               "<type name=\"faeces_om\">"
               "<field name=\"weight\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"n\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"p\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"s\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"ash_alk\" unit=\"mol/ha\" kind=\"double\" />"
               "</type>"
               "<type name=\"faeces_inorg\">"
               "<field name=\"n\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"p\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"s\" unit=\"kg/ha\" kind=\"double\" />"
               "</type>"
               "<type name=\"urine\">"
               "<field name=\"volume\" unit=\"m3/ha\" kind=\"double\" />"
               "<field name=\"urea\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"pox\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"so4\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"ash_alk\" unit=\"mol/ha\" kind=\"double\" />"
               "</type>"
               "</type>";}

   //------ RemoveHerbage ------

   void EXPORT pack(MessageData& messageData, const RemoveHerbageType& data)
      {
      pack(messageData, data.herbage);
      pack(messageData, data.seed);
      }
   void EXPORT unpack(MessageData& messageData, RemoveHerbageType& data)
      {
      unpack(messageData, data.herbage);
      unpack(messageData, data.seed);
      }
   unsigned EXPORT memorySize(const RemoveHerbageType& data)
      {
      return 0
              + ::memorySize(data.herbage)
              + ::memorySize(data.seed)
              ;
      }
   std::string EXPORT DDML(const RemoveHerbageType& data)
      {return "<type name=\"RemoveHerbage\">"
               "<field name=\"herbage\" unit=\"kg/ha\" kind=\"double\" array=\"T\" />"
               "<field name=\"seed\" unit=\"kg/ha\" kind=\"double\" array=\"T\" />"
               "</type>";}

   //------ SuppEaten ------

   void EXPORT pack(MessageData& messageData, const SuppEatenType& data)
      {
      pack(messageData, data.paddock);
      pack(messageData, data.eaten);
      }
   void EXPORT unpack(MessageData& messageData, SuppEatenType& data)
      {
      unpack(messageData, data.paddock);
      unpack(messageData, data.eaten);
      }
   unsigned EXPORT memorySize(const SuppEatenType& data)
      {
      return 0
              + ::memorySize(data.paddock)
              + ::memorySize(data.eaten)
              ;
      }
   std::string EXPORT DDML(const SuppEatenType& data)
      {return "<type name=\"SuppEaten\" array=\"T\">"
               "<element>"
               "<field name=\"paddock\" unit=\"\" kind=\"string\" />"
               "<field name=\"eaten\" unit=\"kg\" kind=\"double\" />"
               "</element>"
               "</type>";}

   //------ herbage ------

   void EXPORT pack(MessageData& messageData, const herbageType& data)
      {
      pack(messageData, data.dm);
      pack(messageData, data.dmd);
      pack(messageData, data.cp_conc);
      pack(messageData, data.p_conc);
      pack(messageData, data.s_conc);
      pack(messageData, data.prot_dg);
      pack(messageData, data.ash_alk);
      pack(messageData, data.height_ratio);
      }
   void EXPORT unpack(MessageData& messageData, herbageType& data)
      {
      unpack(messageData, data.dm);
      unpack(messageData, data.dmd);
      unpack(messageData, data.cp_conc);
      unpack(messageData, data.p_conc);
      unpack(messageData, data.s_conc);
      unpack(messageData, data.prot_dg);
      unpack(messageData, data.ash_alk);
      unpack(messageData, data.height_ratio);
      }
   unsigned EXPORT memorySize(const herbageType& data)
      {
      return 0
              + ::memorySize(data.dm)
              + ::memorySize(data.dmd)
              + ::memorySize(data.cp_conc)
              + ::memorySize(data.p_conc)
              + ::memorySize(data.s_conc)
              + ::memorySize(data.prot_dg)
              + ::memorySize(data.ash_alk)
              + ::memorySize(data.height_ratio)
              ;
      }
   std::string EXPORT DDML(const herbageType& data)
      {return "<field name=\"herbage\" array=\"T\">"
               "<element>"
               "<field name=\"dm\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"dmd\" unit=\"-\" kind=\"double\" />"
               "<field name=\"cp_conc\" unit=\"kg/kg\" kind=\"double\" />"
               "<field name=\"p_conc\" unit=\"kg/kg\" kind=\"double\" />"
               "<field name=\"s_conc\" unit=\"kg/kg\" kind=\"double\" />"
               "<field name=\"prot_dg\" unit=\"kg/kg\" kind=\"double\" />"
               "<field name=\"ash_alk\" unit=\"mol/kg\" kind=\"double\" />"
               "<field name=\"height_ratio\" unit=\"-\" kind=\"double\" />"
               "</element>"
               "</field>";}

   //------ seed ------

   void EXPORT pack(MessageData& messageData, const seedType& data)
      {
      pack(messageData, data.dm);
      pack(messageData, data.dmd);
      pack(messageData, data.cp_conc);
      pack(messageData, data.p_conc);
      pack(messageData, data.s_conc);
      pack(messageData, data.prot_dg);
      pack(messageData, data.ash_alk);
      pack(messageData, data.height_ratio);
      }
   void EXPORT unpack(MessageData& messageData, seedType& data)
      {
      unpack(messageData, data.dm);
      unpack(messageData, data.dmd);
      unpack(messageData, data.cp_conc);
      unpack(messageData, data.p_conc);
      unpack(messageData, data.s_conc);
      unpack(messageData, data.prot_dg);
      unpack(messageData, data.ash_alk);
      unpack(messageData, data.height_ratio);
      }
   unsigned EXPORT memorySize(const seedType& data)
      {
      return 0
              + ::memorySize(data.dm)
              + ::memorySize(data.dmd)
              + ::memorySize(data.cp_conc)
              + ::memorySize(data.p_conc)
              + ::memorySize(data.s_conc)
              + ::memorySize(data.prot_dg)
              + ::memorySize(data.ash_alk)
              + ::memorySize(data.height_ratio)
              ;
      }
   std::string EXPORT DDML(const seedType& data)
      {return "<field name=\"seed\" array=\"T\">"
               "<element>"
               "<field name=\"dm\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"dmd\" unit=\"-\" kind=\"double\" />"
               "<field name=\"cp_conc\" unit=\"kg/kg\" kind=\"double\" />"
               "<field name=\"p_conc\" unit=\"kg/kg\" kind=\"double\" />"
               "<field name=\"s_conc\" unit=\"kg/kg\" kind=\"double\" />"
               "<field name=\"prot_dg\" unit=\"kg/kg\" kind=\"double\" />"
               "<field name=\"ash_alk\" unit=\"mol/kg\" kind=\"double\" />"
               "<field name=\"height_ratio\" unit=\"-\" kind=\"double\" />"
               "</element>"
               "</field>";}

   //------ Plant2Stock ------

   void EXPORT pack(MessageData& messageData, const Plant2StockType& data)
      {
      pack(messageData, data.herbage);
      pack(messageData, data.propn_green);
      pack(messageData, data.legume);
      pack(messageData, data.select_factor);
      pack(messageData, data.seed);
      pack(messageData, data.seed_class);
      }
   void EXPORT unpack(MessageData& messageData, Plant2StockType& data)
      {
      unpack(messageData, data.herbage);
      unpack(messageData, data.propn_green);
      unpack(messageData, data.legume);
      unpack(messageData, data.select_factor);
      unpack(messageData, data.seed);
      unpack(messageData, data.seed_class);
      }
   unsigned EXPORT memorySize(const Plant2StockType& data)
      {
      return 0
              + ::memorySize(data.herbage)
              + ::memorySize(data.propn_green)
              + ::memorySize(data.legume)
              + ::memorySize(data.select_factor)
              + ::memorySize(data.seed)
              + ::memorySize(data.seed_class)
              ;
      }
   std::string EXPORT DDML(const Plant2StockType& data)
      {return "<type name=\"Plant2Stock\">"
               "<field name=\"herbage\" array=\"T\">"
               "<element>"
               "<field name=\"dm\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"dmd\" unit=\"-\" kind=\"double\" />"
               "<field name=\"cp_conc\" unit=\"kg/kg\" kind=\"double\" />"
               "<field name=\"p_conc\" unit=\"kg/kg\" kind=\"double\" />"
               "<field name=\"s_conc\" unit=\"kg/kg\" kind=\"double\" />"
               "<field name=\"prot_dg\" unit=\"kg/kg\" kind=\"double\" />"
               "<field name=\"ash_alk\" unit=\"mol/kg\" kind=\"double\" />"
               "<field name=\"height_ratio\" unit=\"-\" kind=\"double\" />"
               "</element>"
               "</field>"
               "<field name=\"propn_green\" unit=\"-\" kind=\"double\" />"
               "<field name=\"legume\" unit=\"-\" kind=\"double\" />"
               "<field name=\"select_factor\" unit=\"-\" kind=\"double\" />"
               "<field name=\"seed\" array=\"T\">"
               "<element>"
               "<field name=\"dm\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"dmd\" unit=\"-\" kind=\"double\" />"
               "<field name=\"cp_conc\" unit=\"kg/kg\" kind=\"double\" />"
               "<field name=\"p_conc\" unit=\"kg/kg\" kind=\"double\" />"
               "<field name=\"s_conc\" unit=\"kg/kg\" kind=\"double\" />"
               "<field name=\"prot_dg\" unit=\"kg/kg\" kind=\"double\" />"
               "<field name=\"ash_alk\" unit=\"mol/kg\" kind=\"double\" />"
               "<field name=\"height_ratio\" unit=\"-\" kind=\"double\" />"
               "</element>"
               "</field>"
               "<field name=\"seed_class\" unit=\"-\" kind=\"integer4\" array=\"T\" />"
               "</type>";}

   //------ BuyStock ------

   void EXPORT pack(MessageData& messageData, const BuyStockType& data)
      {
      pack(messageData, data.genotype);
      pack(messageData, data.number);
      pack(messageData, data.sex);
      pack(messageData, data.age);
      pack(messageData, data.weight);
      pack(messageData, data.fleece_wt);
      pack(messageData, data.cond_score);
      pack(messageData, data.mated_to);
      pack(messageData, data.pregnant);
      pack(messageData, data.lactating);
      pack(messageData, data.no_young);
      pack(messageData, data.young_wt);
      pack(messageData, data.young_fleece_wt);
      }
   void EXPORT unpack(MessageData& messageData, BuyStockType& data)
      {
      unpack(messageData, data.genotype);
      unpack(messageData, data.number);
      unpack(messageData, data.sex);
      unpack(messageData, data.age);
      unpack(messageData, data.weight);
      unpack(messageData, data.fleece_wt);
      unpack(messageData, data.cond_score);
      unpack(messageData, data.mated_to);
      unpack(messageData, data.pregnant);
      unpack(messageData, data.lactating);
      unpack(messageData, data.no_young);
      unpack(messageData, data.young_wt);
      unpack(messageData, data.young_fleece_wt);
      }
   unsigned EXPORT memorySize(const BuyStockType& data)
      {
      return 0
              + ::memorySize(data.genotype)
              + ::memorySize(data.number)
              + ::memorySize(data.sex)
              + ::memorySize(data.age)
              + ::memorySize(data.weight)
              + ::memorySize(data.fleece_wt)
              + ::memorySize(data.cond_score)
              + ::memorySize(data.mated_to)
              + ::memorySize(data.pregnant)
              + ::memorySize(data.lactating)
              + ::memorySize(data.no_young)
              + ::memorySize(data.young_wt)
              + ::memorySize(data.young_fleece_wt)
              ;
      }
   std::string EXPORT DDML(const BuyStockType& data)
      {return "<type name=\"BuyStock\">"
               "<field name=\"genotype\" unit=\"\" kind=\"string\" />"
               "<field name=\"number\" unit=\"\" kind=\"integer4\" />"
               "<field name=\"sex\" unit=\"\" kind=\"string\" />"
               "<field name=\"age\" unit=\"months\" kind=\"double\" />"
               "<field name=\"weight\" unit=\"kg\" kind=\"double\" />"
               "<field name=\"fleece_wt\" unit=\"kg\" kind=\"double\" />"
               "<field name=\"cond_score\" unit=\"\" kind=\"double\" />"
               "<field name=\"mated_to\" unit=\"\" kind=\"string\" />"
               "<field name=\"pregnant\" unit=\"\" kind=\"integer4\" />"
               "<field name=\"lactating\" unit=\"\" kind=\"integer4\" />"
               "<field name=\"no_young\" unit=\"\" kind=\"integer4\" />"
               "<field name=\"young_wt\" unit=\"kg\" kind=\"double\" />"
               "<field name=\"young_fleece_wt\" unit=\"kg\" kind=\"double\" />"
               "</type>";}

   //------ SellStock ------

   void EXPORT pack(MessageData& messageData, const SellStockType& data)
      {
      pack(messageData, data.group);
      pack(messageData, data.number);
      }
   void EXPORT unpack(MessageData& messageData, SellStockType& data)
      {
      unpack(messageData, data.group);
      unpack(messageData, data.number);
      }
   unsigned EXPORT memorySize(const SellStockType& data)
      {
      return 0
              + ::memorySize(data.group)
              + ::memorySize(data.number)
              ;
      }
   std::string EXPORT DDML(const SellStockType& data)
      {return "<type name=\"SellStock\">"
               "<field name=\"group\" unit=\"\" kind=\"integer4\" />"
               "<field name=\"number\" unit=\"\" kind=\"integer4\" />"
               "</type>";}

   //------ CastrateStock ------

   void EXPORT pack(MessageData& messageData, const CastrateStockType& data)
      {
      pack(messageData, data.group);
      pack(messageData, data.number);
      }
   void EXPORT unpack(MessageData& messageData, CastrateStockType& data)
      {
      unpack(messageData, data.group);
      unpack(messageData, data.number);
      }
   unsigned EXPORT memorySize(const CastrateStockType& data)
      {
      return 0
              + ::memorySize(data.group)
              + ::memorySize(data.number)
              ;
      }
   std::string EXPORT DDML(const CastrateStockType& data)
      {return "<type name=\"CastrateStock\">"
               "<field name=\"group\" unit=\"\" kind=\"integer4\" />"
               "<field name=\"number\" unit=\"\" kind=\"integer4\" />"
               "</type>";}

   //------ DryOffStock ------

   void EXPORT pack(MessageData& messageData, const DryOffStockType& data)
      {
      pack(messageData, data.group);
      pack(messageData, data.number);
      }
   void EXPORT unpack(MessageData& messageData, DryOffStockType& data)
      {
      unpack(messageData, data.group);
      unpack(messageData, data.number);
      }
   unsigned EXPORT memorySize(const DryOffStockType& data)
      {
      return 0
              + ::memorySize(data.group)
              + ::memorySize(data.number)
              ;
      }
   std::string EXPORT DDML(const DryOffStockType& data)
      {return "<type name=\"DryOffStock\">"
               "<field name=\"group\" unit=\"\" kind=\"integer4\" />"
               "<field name=\"number\" unit=\"\" kind=\"integer4\" />"
               "</type>";}

   //------ JoinStock ------

   void EXPORT pack(MessageData& messageData, const JoinStockType& data)
      {
      pack(messageData, data.group);
      pack(messageData, data.mate_to);
      pack(messageData, data.mate_days);
      }
   void EXPORT unpack(MessageData& messageData, JoinStockType& data)
      {
      unpack(messageData, data.group);
      unpack(messageData, data.mate_to);
      unpack(messageData, data.mate_days);
      }
   unsigned EXPORT memorySize(const JoinStockType& data)
      {
      return 0
              + ::memorySize(data.group)
              + ::memorySize(data.mate_to)
              + ::memorySize(data.mate_days)
              ;
      }
   std::string EXPORT DDML(const JoinStockType& data)
      {return "<type name=\"JoinStock\">"
               "<field name=\"group\" unit=\"\" kind=\"integer4\" />"
               "<field name=\"mate_to\" unit=\"\" kind=\"string\" />"
               "<field name=\"mate_days\" unit=\"d\" kind=\"integer4\" />"
               "</type>";}

   //------ MoveStock ------

   void EXPORT pack(MessageData& messageData, const MoveStockType& data)
      {
      pack(messageData, data.group);
      pack(messageData, data.paddock);
      }
   void EXPORT unpack(MessageData& messageData, MoveStockType& data)
      {
      unpack(messageData, data.group);
      unpack(messageData, data.paddock);
      }
   unsigned EXPORT memorySize(const MoveStockType& data)
      {
      return 0
              + ::memorySize(data.group)
              + ::memorySize(data.paddock)
              ;
      }
   std::string EXPORT DDML(const MoveStockType& data)
      {return "<type name=\"MoveStock\">"
               "<field name=\"group\" unit=\"\" kind=\"integer4\" />"
               "<field name=\"paddock\" unit=\"\" kind=\"string\" />"
               "</type>";}

   //------ ShearStock ------

   void EXPORT pack(MessageData& messageData, const ShearStockType& data)
      {
      pack(messageData, data.group);
      pack(messageData, data.sub_group);
      }
   void EXPORT unpack(MessageData& messageData, ShearStockType& data)
      {
      unpack(messageData, data.group);
      unpack(messageData, data.sub_group);
      }
   unsigned EXPORT memorySize(const ShearStockType& data)
      {
      return 0
              + ::memorySize(data.group)
              + ::memorySize(data.sub_group)
              ;
      }
   std::string EXPORT DDML(const ShearStockType& data)
      {return "<type name=\"ShearStock\">"
               "<field name=\"group\" unit=\"\" kind=\"integer4\" />"
               "<field name=\"sub_group\" unit=\"\" kind=\"string\" />"
               "</type>";}

   //------ SplitStock ------

   void EXPORT pack(MessageData& messageData, const SplitStockType& data)
      {
      pack(messageData, data.group);
      pack(messageData, data.type);
      pack(messageData, data.value);
      }
   void EXPORT unpack(MessageData& messageData, SplitStockType& data)
      {
      unpack(messageData, data.group);
      unpack(messageData, data.type);
      unpack(messageData, data.value);
      }
   unsigned EXPORT memorySize(const SplitStockType& data)
      {
      return 0
              + ::memorySize(data.group)
              + ::memorySize(data.type)
              + ::memorySize(data.value)
              ;
      }
   std::string EXPORT DDML(const SplitStockType& data)
      {return "<type name=\"SplitStock\">"
               "<field name=\"group\" unit=\"\" kind=\"integer4\" />"
               "<field name=\"type\" unit=\"\" kind=\"string\" />"
               "<field name=\"value\" unit=\"\" kind=\"double\" />"
               "</type>";}

   //------ TagStock ------

   void EXPORT pack(MessageData& messageData, const TagStockType& data)
      {
      pack(messageData, data.group);
      pack(messageData, data.value);
      }
   void EXPORT unpack(MessageData& messageData, TagStockType& data)
      {
      unpack(messageData, data.group);
      unpack(messageData, data.value);
      }
   unsigned EXPORT memorySize(const TagStockType& data)
      {
      return 0
              + ::memorySize(data.group)
              + ::memorySize(data.value)
              ;
      }
   std::string EXPORT DDML(const TagStockType& data)
      {return "<type name=\"TagStock\">"
               "<field name=\"group\" unit=\"\" kind=\"integer4\" />"
               "<field name=\"value\" unit=\"\" kind=\"integer4\" />"
               "</type>";}

   //------ WeanStock ------

   void EXPORT pack(MessageData& messageData, const WeanStockType& data)
      {
      pack(messageData, data.group);
      pack(messageData, data.sex);
      pack(messageData, data.number);
      }
   void EXPORT unpack(MessageData& messageData, WeanStockType& data)
      {
      unpack(messageData, data.group);
      unpack(messageData, data.sex);
      unpack(messageData, data.number);
      }
   unsigned EXPORT memorySize(const WeanStockType& data)
      {
      return 0
              + ::memorySize(data.group)
              + ::memorySize(data.sex)
              + ::memorySize(data.number)
              ;
      }
   std::string EXPORT DDML(const WeanStockType& data)
      {return "<type name=\"WeanStock\">"
               "<field name=\"group\" unit=\"\" kind=\"integer4\" />"
               "<field name=\"sex\" unit=\"\" kind=\"string\" />"
               "<field name=\"number\" unit=\"\" kind=\"integer4\" />"
               "</type>";}

   //------ dm ------

   void EXPORT pack(MessageData& messageData, const dmType& data)
      {
      pack(messageData, data.pool);
      pack(messageData, data.part);
      pack(messageData, data.dlt);
      }
   void EXPORT unpack(MessageData& messageData, dmType& data)
      {
      unpack(messageData, data.pool);
      unpack(messageData, data.part);
      unpack(messageData, data.dlt);
      }
   unsigned EXPORT memorySize(const dmType& data)
      {
      return 0
              + ::memorySize(data.pool)
              + ::memorySize(data.part)
              + ::memorySize(data.dlt)
              ;
      }
   std::string EXPORT DDML(const dmType& data)
      {return "<field name=\"dm\" array=\"T\">"
               "<element>"
               "<field name=\"pool\" kind=\"string\" />"
               "<field name=\"part\" kind=\"string\" array=\"T\" />"
               "<field name=\"dlt\" kind=\"double\" array=\"T\" />"
               "</element>"
               "</field>";}

   //------ RemoveCropDm ------

   void EXPORT pack(MessageData& messageData, const RemoveCropDmType& data)
      {
      pack(messageData, data.dm);
      }
   void EXPORT unpack(MessageData& messageData, RemoveCropDmType& data)
      {
      unpack(messageData, data.dm);
      }
   unsigned EXPORT memorySize(const RemoveCropDmType& data)
      {
      return 0
              + ::memorySize(data.dm)
              ;
      }
   std::string EXPORT DDML(const RemoveCropDmType& data)
      {return "<type name=\"RemoveCropDm\">"
               "<field name=\"dm\" array=\"T\">"
               "<element>"
               "<field name=\"pool\" kind=\"string\" />"
               "<field name=\"part\" kind=\"string\" array=\"T\" />"
               "<field name=\"dlt\" kind=\"double\" array=\"T\" />"
               "</element>"
               "</field>"
               "</type>";}

   //------ RemoveResidueDm ------

   void EXPORT pack(MessageData& messageData, const RemoveResidueDmType& data)
      {
      pack(messageData, data.dm_type);
      pack(messageData, data.dlt_residue_dm);
      }
   void EXPORT unpack(MessageData& messageData, RemoveResidueDmType& data)
      {
      unpack(messageData, data.dm_type);
      unpack(messageData, data.dlt_residue_dm);
      }
   unsigned EXPORT memorySize(const RemoveResidueDmType& data)
      {
      return 0
              + ::memorySize(data.dm_type)
              + ::memorySize(data.dlt_residue_dm)
              ;
      }
   std::string EXPORT DDML(const RemoveResidueDmType& data)
      {return "<type name=\"RemoveResidueDm\">"
               "<field name=\"dm_type\" kind=\"string\" array=\"T\" />"
               "<field name=\"dlt_residue_dm\" kind=\"single\" array=\"T\" />"
               "</type>";}

   //------ SupplementBuy ------

   void EXPORT pack(MessageData& messageData, const SupplementBuyType& data)
      {
      pack(messageData, data.supplement);
      pack(messageData, data.amount);
      }
   void EXPORT unpack(MessageData& messageData, SupplementBuyType& data)
      {
      unpack(messageData, data.supplement);
      unpack(messageData, data.amount);
      }
   unsigned EXPORT memorySize(const SupplementBuyType& data)
      {
      return 0
              + ::memorySize(data.supplement)
              + ::memorySize(data.amount)
              ;
      }
   std::string EXPORT DDML(const SupplementBuyType& data)
      {return "<type name=\"SupplementBuy\">"
               "<field name=\"supplement\" kind=\"string\" unit=\"-\" />"
               "<field name=\"amount\" kind=\"double\" unit=\"kg\" />"
               "</type>";}

   //------ SupplementFeed ------

   void EXPORT pack(MessageData& messageData, const SupplementFeedType& data)
      {
      pack(messageData, data.supplement);
      pack(messageData, data.amount);
      pack(messageData, data.paddock);
      }
   void EXPORT unpack(MessageData& messageData, SupplementFeedType& data)
      {
      unpack(messageData, data.supplement);
      unpack(messageData, data.amount);
      unpack(messageData, data.paddock);
      }
   unsigned EXPORT memorySize(const SupplementFeedType& data)
      {
      return 0
              + ::memorySize(data.supplement)
              + ::memorySize(data.amount)
              + ::memorySize(data.paddock)
              ;
      }
   std::string EXPORT DDML(const SupplementFeedType& data)
      {return "<type name=\"SupplementFeed\">"
               "<field name=\"supplement\" kind=\"string\" unit=\"-\" />"
               "<field name=\"amount\" kind=\"double\" unit=\"kg\" />"
               "<field name=\"paddock\" kind=\"string\" unit=\"-\" />"
               "</type>";}

   //------ SupplementMix ------

   void EXPORT pack(MessageData& messageData, const SupplementMixType& data)
      {
      pack(messageData, data.src_store);
      pack(messageData, data.amount);
      pack(messageData, data.dest_store);
      }
   void EXPORT unpack(MessageData& messageData, SupplementMixType& data)
      {
      unpack(messageData, data.src_store);
      unpack(messageData, data.amount);
      unpack(messageData, data.dest_store);
      }
   unsigned EXPORT memorySize(const SupplementMixType& data)
      {
      return 0
              + ::memorySize(data.src_store)
              + ::memorySize(data.amount)
              + ::memorySize(data.dest_store)
              ;
      }
   std::string EXPORT DDML(const SupplementMixType& data)
      {return "<type name=\"SupplementMix\">"
               "<field name=\"src_store\" kind=\"string\" unit=\"-\" />"
               "<field name=\"amount\" kind=\"double\" unit=\"kg\" />"
               "<field name=\"dest_store\" kind=\"string\" unit=\"-\" />"
               "</type>";}

   //------ ExternalMassFlow ------

   void EXPORT pack(MessageData& messageData, const ExternalMassFlowType& data)
      {
      pack(messageData, data.PoolClass);
      pack(messageData, data.FlowType);
      pack(messageData, data.C);
      pack(messageData, data.N);
      pack(messageData, data.P);
      pack(messageData, data.DM);
      pack(messageData, data.SW);
      }
   void EXPORT unpack(MessageData& messageData, ExternalMassFlowType& data)
      {
      unpack(messageData, data.PoolClass);
      unpack(messageData, data.FlowType);
      unpack(messageData, data.C);
      unpack(messageData, data.N);
      unpack(messageData, data.P);
      unpack(messageData, data.DM);
      unpack(messageData, data.SW);
      }
   unsigned EXPORT memorySize(const ExternalMassFlowType& data)
      {
      return 0
              + ::memorySize(data.PoolClass)
              + ::memorySize(data.FlowType)
              + ::memorySize(data.C)
              + ::memorySize(data.N)
              + ::memorySize(data.P)
              + ::memorySize(data.DM)
              + ::memorySize(data.SW)
              ;
      }
   std::string EXPORT DDML(const ExternalMassFlowType& data)
      {return "<type name=\"ExternalMassFlow\">"
               "<field name=\"PoolClass\" kind=\"string\" unit=\"-\" />"
               "<field name=\"FlowType\" kind=\"string\" unit=\"-\" />"
               "<field name=\"C\" kind=\"single\" unit=\"kg/ha\" />"
               "<field name=\"N\" kind=\"single\" unit=\"kg/ha\" />"
               "<field name=\"P\" kind=\"single\" unit=\"kg/ha\" />"
               "<field name=\"DM\" kind=\"single\" unit=\"kg/ha\" />"
               "<field name=\"SW\" kind=\"single\" unit=\"mm\" />"
               "</type>";}


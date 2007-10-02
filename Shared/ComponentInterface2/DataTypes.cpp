#include <general/stl_functions.h>
#include <general/TreeNodeIterator.h>
#include <general/xml.h>
#include "DataTypes.h"
#include "MessageData.h"
#include "Interfaces.h"

   //------ Null ------
   void pack(MessageData& messageData, const Null& data) { }
   void unpack(MessageData& messageData, Null& data) { }
   unsigned memorySize(const Null& data) {return 0;}
   std::string DDML(const Null& data) {return "<type/>";}


   //------ Complete ------

   void pack(MessageData& messageData, const CompleteType& data)
      {
      pack(messageData, data.ackID);
      }
   void unpack(MessageData& messageData, CompleteType& data)
      {
      unpack(messageData, data.ackID);
      }
   unsigned memorySize(const CompleteType& data)
      {
      return 0
              + ::memorySize(data.ackID)
              ;
      }
   std::string DDML(const CompleteType& data)
      {return "<type name=\"Complete\">"
               "<field name=\"ackID\" kind=\"integer4\" />"
               "</type>";}

   //------ Error ------

   void pack(MessageData& messageData, const ErrorType& data)
      {
      pack(messageData, data.msg);
      pack(messageData, data.isFatal);
      }
   void unpack(MessageData& messageData, ErrorType& data)
      {
      unpack(messageData, data.msg);
      unpack(messageData, data.isFatal);
      }
   unsigned memorySize(const ErrorType& data)
      {
      return 0
              + ::memorySize(data.msg)
              + ::memorySize(data.isFatal)
              ;
      }
   std::string DDML(const ErrorType& data)
      {return "<type name=\"Error\">"
               "<field name=\"msg\" kind=\"string\" />"
               "<field name=\"isFatal\" kind=\"boolean\" />"
               "</type>";}

   //------ Event ------

   void pack(MessageData& messageData, const EventType& data)
      {
      pack(messageData, data.ID);
      pack(messageData, data.publishedBy);
      pack(messageData, data.ddml);
      }
   void unpack(MessageData& messageData, EventType& data)
      {
      unpack(messageData, data.ID);
      unpack(messageData, data.publishedBy);
      unpack(messageData, data.ddml);
      }
   unsigned memorySize(const EventType& data)
      {
      return 0
              + ::memorySize(data.ID)
              + ::memorySize(data.publishedBy)
              + ::memorySize(data.ddml)
              ;
      }
   std::string DDML(const EventType& data)
      {return "<type name=\"Event\">"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"publishedBy\" kind=\"integer4\" />"
               "<field name=\"ddml\" kind=\"string\" />"
               "</type>";}

   //------ GetValue ------

   void pack(MessageData& messageData, const GetValueType& data)
      {
      pack(messageData, data.ID);
      }
   void unpack(MessageData& messageData, GetValueType& data)
      {
      unpack(messageData, data.ID);
      }
   unsigned memorySize(const GetValueType& data)
      {
      return 0
              + ::memorySize(data.ID)
              ;
      }
   std::string DDML(const GetValueType& data)
      {return "<type name=\"GetValue\">"
               "<field name=\"ID\" kind=\"integer4\" />"
               "</type>";}

   //------ Init1 ------

   void pack(MessageData& messageData, const Init1Type& data)
      {
      pack(messageData, data.sdml);
      pack(messageData, data.fqn);
      pack(messageData, data.inStartup);
      }
   void unpack(MessageData& messageData, Init1Type& data)
      {
      unpack(messageData, data.sdml);
      unpack(messageData, data.fqn);
      unpack(messageData, data.inStartup);
      }
   unsigned memorySize(const Init1Type& data)
      {
      return 0
              + ::memorySize(data.sdml)
              + ::memorySize(data.fqn)
              + ::memorySize(data.inStartup)
              ;
      }
   std::string DDML(const Init1Type& data)
      {return "<type name=\"Init1\">"
               "<field name=\"sdml\" kind=\"string\" />"
               "<field name=\"fqn\" kind=\"string\" />"
               "<field name=\"inStartup\" kind=\"boolean\" />"
               "</type>";}

   //------ NotifySetValueSuccess ------

   void pack(MessageData& messageData, const NotifySetValueSuccessType& data)
      {
      pack(messageData, data.ID);
      pack(messageData, data.success);
      }
   void unpack(MessageData& messageData, NotifySetValueSuccessType& data)
      {
      unpack(messageData, data.ID);
      unpack(messageData, data.success);
      }
   unsigned memorySize(const NotifySetValueSuccessType& data)
      {
      return 0
              + ::memorySize(data.ID)
              + ::memorySize(data.success)
              ;
      }
   std::string DDML(const NotifySetValueSuccessType& data)
      {return "<type name=\"NotifySetValueSuccess\">"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"success\" kind=\"boolean\" />"
               "</type>";}

   //------ PublishEvent ------

   void pack(MessageData& messageData, const PublishEventType& data)
      {
      pack(messageData, data.ID);
      pack(messageData, data.ddml);
      }
   void unpack(MessageData& messageData, PublishEventType& data)
      {
      unpack(messageData, data.ID);
      unpack(messageData, data.ddml);
      }
   unsigned memorySize(const PublishEventType& data)
      {
      return 0
              + ::memorySize(data.ID)
              + ::memorySize(data.ddml)
              ;
      }
   std::string DDML(const PublishEventType& data)
      {return "<type name=\"PublishEvent\">"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"ddml\" kind=\"string\" />"
               "</type>";}

   //------ QueryInfo ------

   void pack(MessageData& messageData, const QueryInfoType& data)
      {
      pack(messageData, data.name);
      pack(messageData, data.kind);
      }
   void unpack(MessageData& messageData, QueryInfoType& data)
      {
      unpack(messageData, data.name);
      unpack(messageData, data.kind);
      }
   unsigned memorySize(const QueryInfoType& data)
      {
      return 0
              + ::memorySize(data.name)
              + ::memorySize(data.kind)
              ;
      }
   std::string DDML(const QueryInfoType& data)
      {return "<type name=\"QueryInfo\">"
               "<field name=\"name\" kind=\"string\" />"
               "<field name=\"kind\" kind=\"integer4\" />"
               "</type>";}

   //------ Register ------

   void pack(MessageData& messageData, const RegisterType& data)
      {
      pack(messageData, data.kind);
      pack(messageData, data.ID);
      pack(messageData, data.destID);
      pack(messageData, data.name);
      pack(messageData, data.ddml);
      }
   void unpack(MessageData& messageData, RegisterType& data)
      {
      unpack(messageData, data.kind);
      unpack(messageData, data.ID);
      unpack(messageData, data.destID);
      unpack(messageData, data.name);
      unpack(messageData, data.ddml);
      }
   unsigned memorySize(const RegisterType& data)
      {
      return 0
              + ::memorySize(data.kind)
              + ::memorySize(data.ID)
              + ::memorySize(data.destID)
              + ::memorySize(data.name)
              + ::memorySize(data.ddml)
              ;
      }
   std::string DDML(const RegisterType& data)
      {return "<type name=\"Register\">"
               "<field name=\"kind\" kind=\"integer4\" />"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"destID\" kind=\"integer4\" />"
               "<field name=\"name\" kind=\"string\" />"
               "<field name=\"ddml\" kind=\"string\" />"
               "</type>";}

   //------ ReplyValue ------

   void pack(MessageData& messageData, const ReplyValueType& data)
      {
      pack(messageData, data.queryID);
      pack(messageData, data.ddml);
      }
   void unpack(MessageData& messageData, ReplyValueType& data)
      {
      unpack(messageData, data.queryID);
      unpack(messageData, data.ddml);
      }
   unsigned memorySize(const ReplyValueType& data)
      {
      return 0
              + ::memorySize(data.queryID)
              + ::memorySize(data.ddml)
              ;
      }
   std::string DDML(const ReplyValueType& data)
      {return "<type name=\"ReplyValue\">"
               "<field name=\"queryID\" kind=\"integer4\" />"
               "<field name=\"ddml\" kind=\"string\" />"
               "</type>";}

   //------ RequestSetValue ------

   void pack(MessageData& messageData, const RequestSetValueType& data)
      {
      pack(messageData, data.ID);
      pack(messageData, data.ddml);
      }
   void unpack(MessageData& messageData, RequestSetValueType& data)
      {
      unpack(messageData, data.ID);
      unpack(messageData, data.ddml);
      }
   unsigned memorySize(const RequestSetValueType& data)
      {
      return 0
              + ::memorySize(data.ID)
              + ::memorySize(data.ddml)
              ;
      }
   std::string DDML(const RequestSetValueType& data)
      {return "<type name=\"RequestSetValue\">"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"ddml\" kind=\"string\" />"
               "</type>";}

   //------ ReturnInfo ------

   void pack(MessageData& messageData, const ReturnInfoType& data)
      {
      pack(messageData, data.queryID);
      pack(messageData, data.compID);
      pack(messageData, data.ID);
      pack(messageData, data.name);
      pack(messageData, data.type);
      pack(messageData, data.kind);
      }
   void unpack(MessageData& messageData, ReturnInfoType& data)
      {
      unpack(messageData, data.queryID);
      unpack(messageData, data.compID);
      unpack(messageData, data.ID);
      unpack(messageData, data.name);
      unpack(messageData, data.type);
      unpack(messageData, data.kind);
      }
   unsigned memorySize(const ReturnInfoType& data)
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
   std::string DDML(const ReturnInfoType& data)
      {return "<type name=\"ReturnInfo\">"
               "<field name=\"queryID\" kind=\"integer4\" />"
               "<field name=\"compID\" kind=\"integer4\" />"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"name\" kind=\"string\" />"
               "<field name=\"type\" kind=\"string\" />"
               "<field name=\"kind\" kind=\"integer4\" />"
               "</type>";}

   //------ ReturnValue ------

   void pack(MessageData& messageData, const ReturnValueType& data)
      {
      pack(messageData, data.compID);
      pack(messageData, data.ID);
      pack(messageData, data.ddml);
      }
   void unpack(MessageData& messageData, ReturnValueType& data)
      {
      unpack(messageData, data.compID);
      unpack(messageData, data.ID);
      unpack(messageData, data.ddml);
      }
   unsigned memorySize(const ReturnValueType& data)
      {
      return 0
              + ::memorySize(data.compID)
              + ::memorySize(data.ID)
              + ::memorySize(data.ddml)
              ;
      }
   std::string DDML(const ReturnValueType& data)
      {return "<type name=\"ReturnValue\">"
               "<field name=\"compID\" kind=\"integer4\" />"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"ddml\" kind=\"string\" />"
               "</type>";}

   //------ QueryValue ------

   void pack(MessageData& messageData, const QueryValueType& data)
      {
      pack(messageData, data.ID);
      pack(messageData, data.requestedByID);
      }
   void unpack(MessageData& messageData, QueryValueType& data)
      {
      unpack(messageData, data.ID);
      unpack(messageData, data.requestedByID);
      }
   unsigned memorySize(const QueryValueType& data)
      {
      return 0
              + ::memorySize(data.ID)
              + ::memorySize(data.requestedByID)
              ;
      }
   std::string DDML(const QueryValueType& data)
      {return "<type name=\"QueryValue\">"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"requestedByID\" kind=\"integer4\" />"
               "</type>";}

   //------ QuerySetValue ------

   void pack(MessageData& messageData, const QuerySetValueType& data)
      {
      pack(messageData, data.ID);
      pack(messageData, data.ddml);
      }
   void unpack(MessageData& messageData, QuerySetValueType& data)
      {
      unpack(messageData, data.ID);
      unpack(messageData, data.ddml);
      }
   unsigned memorySize(const QuerySetValueType& data)
      {
      return 0
              + ::memorySize(data.ID)
              + ::memorySize(data.ddml)
              ;
      }
   std::string DDML(const QuerySetValueType& data)
      {return "<type name=\"QuerySetValue\">"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"ddml\" kind=\"string\" />"
               "</type>";}

   //------ Layered ------

   void pack(MessageData& messageData, const LayeredType& data)
      {
      pack(messageData, data.layer);
      pack(messageData, data.value);
      }
   void unpack(MessageData& messageData, LayeredType& data)
      {
      unpack(messageData, data.layer);
      unpack(messageData, data.value);
      }
   unsigned memorySize(const LayeredType& data)
      {
      return 0
              + ::memorySize(data.layer)
              + ::memorySize(data.value)
              ;
      }
   std::string DDML(const LayeredType& data)
      {return "<type name=\"Layered\" description=\"Layered soil data\">"
               "<field name=\"layer\" kind=\"double\" array=\"T\" />"
               "<field name=\"value\" kind=\"double\" array=\"T\" />"
               "</type>";}

   //------ Time ------

   void pack(MessageData& messageData, const TimeType& data)
      {
      pack(messageData, data.startday);
      pack(messageData, data.startsec);
      pack(messageData, data.startsecpart);
      pack(messageData, data.endday);
      pack(messageData, data.endsec);
      pack(messageData, data.endsecpart);
      }
   void unpack(MessageData& messageData, TimeType& data)
      {
      unpack(messageData, data.startday);
      unpack(messageData, data.startsec);
      unpack(messageData, data.startsecpart);
      unpack(messageData, data.endday);
      unpack(messageData, data.endsec);
      unpack(messageData, data.endsecpart);
      }
   unsigned memorySize(const TimeType& data)
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
   std::string DDML(const TimeType& data)
      {return "<type name=\"Time\" description=\"Change in the simulation system time and the duration of the new time step\">"
               "<field name=\"startday\" kind=\"integer4\" description=\"Day number of the start of the timestep\" />"
               "<field name=\"startsec\" kind=\"integer4\" description=\"Seconds past midnight of the start of the timestep (0-86399)\" />"
               "<field name=\"startsecpart\" kind=\"double\" description=\"Fraction of a second of the start of the timestep (0-1)\" />"
               "<field name=\"endday\" kind=\"integer4\" description=\"Day number of the end of the timestep\" />"
               "<field name=\"endsec\" kind=\"integer4\" description=\"Seconds past midnight of the end of the timestep (0-86399)\" />"
               "<field name=\"endsecpart\" kind=\"double\" description=\"Fraction of a second of the end of the timestep (0-1)\" />"
               "</type>";}

   //------ NewMet ------

   void pack(MessageData& messageData, const NewMetType& data)
      {
      pack(messageData, data.today);
      pack(messageData, data.radn);
      pack(messageData, data.maxt);
      pack(messageData, data.mint);
      pack(messageData, data.rain);
      pack(messageData, data.vp);
      }
   void unpack(MessageData& messageData, NewMetType& data)
      {
      unpack(messageData, data.today);
      unpack(messageData, data.radn);
      unpack(messageData, data.maxt);
      unpack(messageData, data.mint);
      unpack(messageData, data.rain);
      unpack(messageData, data.vp);
      }
   unsigned memorySize(const NewMetType& data)
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
   std::string DDML(const NewMetType& data)
      {return "<type name=\"NewMet\">"
               "<field name=\"today\" kind=\"double\" />"
               "<field name=\"radn\" kind=\"single\" lower_bound=\"0.0\" upper_bound=\"50.0\" units=\"MJ/m2/d\" />"
               "<field name=\"maxt\" kind=\"single\" lower_bound=\"-10.0\" upper_bound=\"70.0\" units=\"oC\" />"
               "<field name=\"mint\" kind=\"single\" lower_bound=\"-20.0\" upper_bound=\"50.0\" units=\"oC\" />"
               "<field name=\"rain\" kind=\"single\" lower_bound=\"0.0\" upper_bound=\"1000.0\" units=\"mm/d\" />"
               "<field name=\"vp\" kind=\"single\" units=\"????\" />"
               "</type>";}

   //------ SoilWaterProfileLayer ------

   void pack(MessageData& messageData, const SoilWaterProfileLayerType& data)
      {
      pack(messageData, data.thickness);
      pack(messageData, data.BulkDensity);
      pack(messageData, data.SatDepth);
      pack(messageData, data.DULDepth);
      pack(messageData, data.LL15Depth);
      pack(messageData, data.AirDryDepth);
      pack(messageData, data.SWDepth);
      }
   void unpack(MessageData& messageData, SoilWaterProfileLayerType& data)
      {
      unpack(messageData, data.thickness);
      unpack(messageData, data.BulkDensity);
      unpack(messageData, data.SatDepth);
      unpack(messageData, data.DULDepth);
      unpack(messageData, data.LL15Depth);
      unpack(messageData, data.AirDryDepth);
      unpack(messageData, data.SWDepth);
      }
   unsigned memorySize(const SoilWaterProfileLayerType& data)
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
   std::string DDML(const SoilWaterProfileLayerType& data)
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

   void pack(MessageData& messageData, const SoilWaterLayerType& data)
      {
      pack(messageData, data.thickness);
      pack(messageData, data.amount);
      }
   void unpack(MessageData& messageData, SoilWaterLayerType& data)
      {
      unpack(messageData, data.thickness);
      unpack(messageData, data.amount);
      }
   unsigned memorySize(const SoilWaterLayerType& data)
      {
      return 0
              + ::memorySize(data.thickness)
              + ::memorySize(data.amount)
              ;
      }
   std::string DDML(const SoilWaterLayerType& data)
      {return "<type name=\"SoilWaterLayer\" array=\"T\">"
               "<element>"
               "<field name=\"thickness\" kind=\"single\" units=\"mm\" />"
               "<field name=\"amount\" kind=\"single\" units=\"mm\" />"
               "</element>"
               "</type>";}

   //------ LateralFlowLayer ------

   void pack(MessageData& messageData, const LateralFlowLayerType& data)
      {
      pack(messageData, data.thickness);
      pack(messageData, data.amount);
      }
   void unpack(MessageData& messageData, LateralFlowLayerType& data)
      {
      unpack(messageData, data.thickness);
      unpack(messageData, data.amount);
      }
   unsigned memorySize(const LateralFlowLayerType& data)
      {
      return 0
              + ::memorySize(data.thickness)
              + ::memorySize(data.amount)
              ;
      }
   std::string DDML(const LateralFlowLayerType& data)
      {return "<field name=\"LateralFlowLayer\" array=\"T\">"
               "<element>"
               "<field name=\"thickness\" kind=\"single\" />"
               "<field name=\"amount\" kind=\"single\" />"
               "</element>"
               "</field>";}

   //------ SoilWaterBalance ------

   void pack(MessageData& messageData, const SoilWaterBalanceType& data)
      {
      pack(messageData, data.infiltration);
      pack(messageData, data.drainage);
      pack(messageData, data.evaporation);
      pack(messageData, data.LateralFlowLayer);
      }
   void unpack(MessageData& messageData, SoilWaterBalanceType& data)
      {
      unpack(messageData, data.infiltration);
      unpack(messageData, data.drainage);
      unpack(messageData, data.evaporation);
      unpack(messageData, data.LateralFlowLayer);
      }
   unsigned memorySize(const SoilWaterBalanceType& data)
      {
      return 0
              + ::memorySize(data.infiltration)
              + ::memorySize(data.drainage)
              + ::memorySize(data.evaporation)
              + ::memorySize(data.LateralFlowLayer)
              ;
      }
   std::string DDML(const SoilWaterBalanceType& data)
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

   void pack(MessageData& messageData, const NewSoluteType& data)
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
   void unpack(MessageData& messageData, NewSoluteType& data)
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
   unsigned memorySize(const NewSoluteType& data)
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
   std::string DDML(const NewSoluteType& data)
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

   void pack(MessageData& messageData, const layerType& data)
      {
      pack(messageData, data.thickness);
      pack(messageData, data.amount);
      }
   void unpack(MessageData& messageData, layerType& data)
      {
      unpack(messageData, data.thickness);
      unpack(messageData, data.amount);
      }
   unsigned memorySize(const layerType& data)
      {
      return 0
              + ::memorySize(data.thickness)
              + ::memorySize(data.amount)
              ;
      }
   std::string DDML(const layerType& data)
      {return "<field name=\"layer\" array=\"T\">"
               "<element>"
               "<field name=\"thickness\" kind=\"single\" />"
               "<field name=\"amount\" kind=\"single\" />"
               "</element>"
               "</field>";}

   //------ SoluteProfile ------

   void pack(MessageData& messageData, const SoluteProfileType& data)
      {
      pack(messageData, data.name);
      pack(messageData, data.layer);
      }
   void unpack(MessageData& messageData, SoluteProfileType& data)
      {
      unpack(messageData, data.name);
      unpack(messageData, data.layer);
      }
   unsigned memorySize(const SoluteProfileType& data)
      {
      return 0
              + ::memorySize(data.name)
              + ::memorySize(data.layer)
              ;
      }
   std::string DDML(const SoluteProfileType& data)
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

   void pack(MessageData& messageData, const IrrigatedType& data)
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
   void unpack(MessageData& messageData, IrrigatedType& data)
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
   unsigned memorySize(const IrrigatedType& data)
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
   std::string DDML(const IrrigatedType& data)
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

   //------ CropWaterSupply ------

   void pack(MessageData& messageData, const CropWaterSupplyType& data)
      {
      pack(messageData, data.name);
      pack(messageData, data.layer);
      }
   void unpack(MessageData& messageData, CropWaterSupplyType& data)
      {
      unpack(messageData, data.name);
      unpack(messageData, data.layer);
      }
   unsigned memorySize(const CropWaterSupplyType& data)
      {
      return 0
              + ::memorySize(data.name)
              + ::memorySize(data.layer)
              ;
      }
   std::string DDML(const CropWaterSupplyType& data)
      {return "<type name=\"CropWaterSupply\" array=\"T\">"
               "<element>"
               "<field name=\"name\" kind=\"string\" />"
               "<field name=\"layer\" array=\"T\">"
               "<element>"
               "<field name=\"thickness\" kind=\"single\" unit=\"mm\" />"
               "<field name=\"amount\" kind=\"single\" unit=\"mm\" />"
               "</element>"
               "</field>"
               "</element>"
               "</type>";}

   //------ RootLayer ------

   void pack(MessageData& messageData, const RootLayerType& data)
      {
      pack(messageData, data.thickness);
      pack(messageData, data.RootLengthDensity);
      pack(messageData, data.PotentialUptake);
      }
   void unpack(MessageData& messageData, RootLayerType& data)
      {
      unpack(messageData, data.thickness);
      unpack(messageData, data.RootLengthDensity);
      unpack(messageData, data.PotentialUptake);
      }
   unsigned memorySize(const RootLayerType& data)
      {
      return 0
              + ::memorySize(data.thickness)
              + ::memorySize(data.RootLengthDensity)
              + ::memorySize(data.PotentialUptake)
              ;
      }
   std::string DDML(const RootLayerType& data)
      {return "<field name=\"RootLayer\" array=\"T\">"
               "<element>"
               "<field name=\"thickness\" kind=\"single\" units=\"mm\" />"
               "<field name=\"RootLengthDensity\" kind=\"single\" units=\"mm/mm3\" />"
               "<field name=\"PotentialUptake\" kind=\"single\" units=\"mm\" />"
               "</element>"
               "</field>";}

   //------ CropWaterDemand ------

   void pack(MessageData& messageData, const CropWaterDemandType& data)
      {
      pack(messageData, data.Name);
      pack(messageData, data.CropType);
      pack(messageData, data.RootLayer);
      pack(messageData, data.amount);
      }
   void unpack(MessageData& messageData, CropWaterDemandType& data)
      {
      unpack(messageData, data.Name);
      unpack(messageData, data.CropType);
      unpack(messageData, data.RootLayer);
      unpack(messageData, data.amount);
      }
   unsigned memorySize(const CropWaterDemandType& data)
      {
      return 0
              + ::memorySize(data.Name)
              + ::memorySize(data.CropType)
              + ::memorySize(data.RootLayer)
              + ::memorySize(data.amount)
              ;
      }
   std::string DDML(const CropWaterDemandType& data)
      {return "<type name=\"CropWaterDemand\" description=\"Message sent to add crop water demand terms to water balance calculations.\" array=\"T\">"
               "<element>"
               "<field name=\"Name\" kind=\"string\" description=\"Cohort name + instance name\" />"
               "<field name=\"CropType\" kind=\"string\" />"
               "<field name=\"RootLayer\" array=\"T\">"
               "<element>"
               "<field name=\"thickness\" kind=\"single\" units=\"mm\" />"
               "<field name=\"RootLengthDensity\" kind=\"single\" units=\"mm/mm3\" />"
               "<field name=\"PotentialUptake\" kind=\"single\" units=\"mm\" />"
               "</element>"
               "</field>"
               "<field name=\"amount\" kind=\"single\" unit=\"mm\" />"
               "</element>"
               "</type>";}

   //------ CropNitrogenDemand ------

   void pack(MessageData& messageData, const CropNitrogenDemandType& data)
      {
      pack(messageData, data.Name);
      pack(messageData, data.CropType);
      pack(messageData, data.RootLayer);
      pack(messageData, data.amount);
      }
   void unpack(MessageData& messageData, CropNitrogenDemandType& data)
      {
      unpack(messageData, data.Name);
      unpack(messageData, data.CropType);
      unpack(messageData, data.RootLayer);
      unpack(messageData, data.amount);
      }
   unsigned memorySize(const CropNitrogenDemandType& data)
      {
      return 0
              + ::memorySize(data.Name)
              + ::memorySize(data.CropType)
              + ::memorySize(data.RootLayer)
              + ::memorySize(data.amount)
              ;
      }
   std::string DDML(const CropNitrogenDemandType& data)
      {return "<type name=\"CropNitrogenDemand\" array=\"T\" description=\"Message sent to add crop N demand terms to N balance calculations.\">"
               "<element>"
               "<field name=\"Name\" kind=\"string\" description=\"Cohort name + instance name\" />"
               "<field name=\"CropType\" kind=\"string\" />"
               "<field name=\"RootLayer\" array=\"T\">"
               "<element>"
               "<field name=\"thickness\" kind=\"single\" units=\"mm\" />"
               "<field name=\"RootLengthDensity\" kind=\"single\" units=\"mm/mm3\" />"
               "<field name=\"PotentialUptakeNO3\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"PotentialUptakeNH4\" kind=\"single\" units=\"kg/ha\" />"
               "</element>"
               "</field>"
               "<field name=\"amount\" kind=\"single\" unit=\"kg/ha\" />"
               "</element>"
               "</type>";}

   //------ CropNitrogenSupply ------

   void pack(MessageData& messageData, const CropNitrogenSupplyType& data)
      {
      pack(messageData, data.name);
      pack(messageData, data.layer);
      }
   void unpack(MessageData& messageData, CropNitrogenSupplyType& data)
      {
      unpack(messageData, data.name);
      unpack(messageData, data.layer);
      }
   unsigned memorySize(const CropNitrogenSupplyType& data)
      {
      return 0
              + ::memorySize(data.name)
              + ::memorySize(data.layer)
              ;
      }
   std::string DDML(const CropNitrogenSupplyType& data)
      {return "<type name=\"CropNitrogenSupply\" array=\"T\">"
               "<element>"
               "<field name=\"name\" kind=\"string\" description=\"Crop type + instance name\" />"
               "<field name=\"layer\" array=\"T\">"
               "<element>"
               "<field name=\"thickness\" kind=\"single\" unit=\"mm\" />"
               "<field name=\"amount\">"
               "<element>"
               "<field name=\"NH4\" kind=\"single\" unit=\"kg/ha\" />"
               "<field name=\"NO3\" kind=\"single\" unit=\"kg/ha\" />"
               "</element>"
               "</field>"
               "</element>"
               "</field>"
               "</element>"
               "</type>";}

   //------ Interception ------

   void pack(MessageData& messageData, const InterceptionType& data)
      {
      pack(messageData, data.name);
      pack(messageData, data.CropType);
      pack(messageData, data.layer);
      }
   void unpack(MessageData& messageData, InterceptionType& data)
      {
      unpack(messageData, data.name);
      unpack(messageData, data.CropType);
      unpack(messageData, data.layer);
      }
   unsigned memorySize(const InterceptionType& data)
      {
      return 0
              + ::memorySize(data.name)
              + ::memorySize(data.CropType)
              + ::memorySize(data.layer)
              ;
      }
   std::string DDML(const InterceptionType& data)
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

   void pack(MessageData& messageData, const LightProfileType& data)
      {
      pack(messageData, data.Interception);
      pack(messageData, data.transmission);
      }
   void unpack(MessageData& messageData, LightProfileType& data)
      {
      unpack(messageData, data.Interception);
      unpack(messageData, data.transmission);
      }
   unsigned memorySize(const LightProfileType& data)
      {
      return 0
              + ::memorySize(data.Interception)
              + ::memorySize(data.transmission)
              ;
      }
   std::string DDML(const LightProfileType& data)
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

   void pack(MessageData& messageData, const CanopyType& data)
      {
      pack(messageData, data.name);
      pack(messageData, data.CropType);
      pack(messageData, data.PotentialEp);
      }
   void unpack(MessageData& messageData, CanopyType& data)
      {
      unpack(messageData, data.name);
      unpack(messageData, data.CropType);
      unpack(messageData, data.PotentialEp);
      }
   unsigned memorySize(const CanopyType& data)
      {
      return 0
              + ::memorySize(data.name)
              + ::memorySize(data.CropType)
              + ::memorySize(data.PotentialEp)
              ;
      }
   std::string DDML(const CanopyType& data)
      {return "<field name=\"Canopy\" array=\"T\">"
               "<element>"
               "<field name=\"name\" kind=\"string\" />"
               "<field name=\"CropType\" kind=\"string\" />"
               "<field name=\"PotentialEp\" kind=\"single\" />"
               "</element>"
               "</field>";}

   //------ CanopyWaterBalance ------

   void pack(MessageData& messageData, const CanopyWaterBalanceType& data)
      {
      pack(messageData, data.Canopy);
      pack(messageData, data.eo);
      pack(messageData, data.interception);
      }
   void unpack(MessageData& messageData, CanopyWaterBalanceType& data)
      {
      unpack(messageData, data.Canopy);
      unpack(messageData, data.eo);
      unpack(messageData, data.interception);
      }
   unsigned memorySize(const CanopyWaterBalanceType& data)
      {
      return 0
              + ::memorySize(data.Canopy)
              + ::memorySize(data.eo)
              + ::memorySize(data.interception)
              ;
      }
   std::string DDML(const CanopyWaterBalanceType& data)
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

   void pack(MessageData& messageData, const OrganicMatterFractionType& data)
      {
      pack(messageData, data.amount);
      pack(messageData, data.C);
      pack(messageData, data.N);
      pack(messageData, data.P);
      pack(messageData, data.AshAlk);
      }
   void unpack(MessageData& messageData, OrganicMatterFractionType& data)
      {
      unpack(messageData, data.amount);
      unpack(messageData, data.C);
      unpack(messageData, data.N);
      unpack(messageData, data.P);
      unpack(messageData, data.AshAlk);
      }
   unsigned memorySize(const OrganicMatterFractionType& data)
      {
      return 0
              + ::memorySize(data.amount)
              + ::memorySize(data.C)
              + ::memorySize(data.N)
              + ::memorySize(data.P)
              + ::memorySize(data.AshAlk)
              ;
      }
   std::string DDML(const OrganicMatterFractionType& data)
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

   void pack(MessageData& messageData, const ResidueType& data)
      {
      pack(messageData, data.name);
      pack(messageData, data.OrganicMatterType);
      pack(messageData, data.OrganicMatterFraction);
      pack(messageData, data.Cover);
      }
   void unpack(MessageData& messageData, ResidueType& data)
      {
      unpack(messageData, data.name);
      unpack(messageData, data.OrganicMatterType);
      unpack(messageData, data.OrganicMatterFraction);
      unpack(messageData, data.Cover);
      }
   unsigned memorySize(const ResidueType& data)
      {
      return 0
              + ::memorySize(data.name)
              + ::memorySize(data.OrganicMatterType)
              + ::memorySize(data.OrganicMatterFraction)
              + ::memorySize(data.Cover)
              ;
      }
   std::string DDML(const ResidueType& data)
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

   void pack(MessageData& messageData, const soluteType& data)
      {
      pack(messageData, data.name);
      pack(messageData, data.amount);
      }
   void unpack(MessageData& messageData, soluteType& data)
      {
      unpack(messageData, data.name);
      unpack(messageData, data.amount);
      }
   unsigned memorySize(const soluteType& data)
      {
      return 0
              + ::memorySize(data.name)
              + ::memorySize(data.amount)
              ;
      }
   std::string DDML(const soluteType& data)
      {return "<field name=\"solute\" array=\"T\">"
               "<element>"
               "<field name=\"name\" kind=\"string\" />"
               "<field name=\"amount\" kind=\"single\" units=\"kg/ha\" />"
               "</element>"
               "</field>";}

   //------ SurfaceWater ------

   void pack(MessageData& messageData, const SurfaceWaterType& data)
      {
      pack(messageData, data.amount);
      pack(messageData, data.solute);
      }
   void unpack(MessageData& messageData, SurfaceWaterType& data)
      {
      unpack(messageData, data.amount);
      unpack(messageData, data.solute);
      }
   unsigned memorySize(const SurfaceWaterType& data)
      {
      return 0
              + ::memorySize(data.amount)
              + ::memorySize(data.solute)
              ;
      }
   std::string DDML(const SurfaceWaterType& data)
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

   void pack(MessageData& messageData, const SurfaceWaterBalanceType& data)
      {
      pack(messageData, data.runoff);
      pack(messageData, data.evaporation);
      pack(messageData, data.runon);
      pack(messageData, data.WaterInput);
      }
   void unpack(MessageData& messageData, SurfaceWaterBalanceType& data)
      {
      unpack(messageData, data.runoff);
      unpack(messageData, data.evaporation);
      unpack(messageData, data.runon);
      unpack(messageData, data.WaterInput);
      }
   unsigned memorySize(const SurfaceWaterBalanceType& data)
      {
      return 0
              + ::memorySize(data.runoff)
              + ::memorySize(data.evaporation)
              + ::memorySize(data.runon)
              + ::memorySize(data.WaterInput)
              ;
      }
   std::string DDML(const SurfaceWaterBalanceType& data)
      {return "<type name=\"SurfaceWaterBalance\">"
               "<field name=\"runoff\" kind=\"single\" />"
               "<field name=\"evaporation\" kind=\"single\" />"
               "<field name=\"runon\" kind=\"single\" />"
               "<field name=\"WaterInput\" kind=\"single\" />"
               "</type>";}

   //------ FertiliserConstituents ------

   void pack(MessageData& messageData, const FertiliserConstituentsType& data)
      {
      pack(messageData, data.name);
      pack(messageData, data.SurfaceAmount);
      pack(messageData, data.layer);
      }
   void unpack(MessageData& messageData, FertiliserConstituentsType& data)
      {
      unpack(messageData, data.name);
      unpack(messageData, data.SurfaceAmount);
      unpack(messageData, data.layer);
      }
   unsigned memorySize(const FertiliserConstituentsType& data)
      {
      return 0
              + ::memorySize(data.name)
              + ::memorySize(data.SurfaceAmount)
              + ::memorySize(data.layer)
              ;
      }
   std::string DDML(const FertiliserConstituentsType& data)
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

   void pack(MessageData& messageData, const FPoolType& data)
      {
      pack(messageData, data.C);
      pack(messageData, data.N);
      pack(messageData, data.P);
      pack(messageData, data.AshAlk);
      }
   void unpack(MessageData& messageData, FPoolType& data)
      {
      unpack(messageData, data.C);
      unpack(messageData, data.N);
      unpack(messageData, data.P);
      unpack(messageData, data.AshAlk);
      }
   unsigned memorySize(const FPoolType& data)
      {
      return 0
              + ::memorySize(data.C)
              + ::memorySize(data.N)
              + ::memorySize(data.P)
              + ::memorySize(data.AshAlk)
              ;
      }
   std::string DDML(const FPoolType& data)
      {return "<field name=\"FPool\" array=\"T\">"
               "<element>"
               "<field name=\"C\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"N\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"P\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"AshAlk\" kind=\"single\" units=\"kg/ha\" />"
               "</element>"
               "</field>";}

   //------ FPoolProfileLayer ------

   void pack(MessageData& messageData, const FPoolProfileLayerType& data)
      {
      pack(messageData, data.thickness);
      pack(messageData, data.no3);
      pack(messageData, data.nh4);
      pack(messageData, data.po4);
      pack(messageData, data.FPool);
      }
   void unpack(MessageData& messageData, FPoolProfileLayerType& data)
      {
      unpack(messageData, data.thickness);
      unpack(messageData, data.no3);
      unpack(messageData, data.nh4);
      unpack(messageData, data.po4);
      unpack(messageData, data.FPool);
      }
   unsigned memorySize(const FPoolProfileLayerType& data)
      {
      return 0
              + ::memorySize(data.thickness)
              + ::memorySize(data.no3)
              + ::memorySize(data.nh4)
              + ::memorySize(data.po4)
              + ::memorySize(data.FPool)
              ;
      }
   std::string DDML(const FPoolProfileLayerType& data)
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

   void pack(MessageData& messageData, const StandingFractionType& data)
      {
      pack(messageData, data.amount);
      pack(messageData, data.C);
      pack(messageData, data.N);
      pack(messageData, data.P);
      pack(messageData, data.AshAlk);
      }
   void unpack(MessageData& messageData, StandingFractionType& data)
      {
      unpack(messageData, data.amount);
      unpack(messageData, data.C);
      unpack(messageData, data.N);
      unpack(messageData, data.P);
      unpack(messageData, data.AshAlk);
      }
   unsigned memorySize(const StandingFractionType& data)
      {
      return 0
              + ::memorySize(data.amount)
              + ::memorySize(data.C)
              + ::memorySize(data.N)
              + ::memorySize(data.P)
              + ::memorySize(data.AshAlk)
              ;
      }
   std::string DDML(const StandingFractionType& data)
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

   void pack(MessageData& messageData, const LyingFractionType& data)
      {
      pack(messageData, data.amount);
      pack(messageData, data.C);
      pack(messageData, data.N);
      pack(messageData, data.P);
      pack(messageData, data.AshAlk);
      }
   void unpack(MessageData& messageData, LyingFractionType& data)
      {
      unpack(messageData, data.amount);
      unpack(messageData, data.C);
      unpack(messageData, data.N);
      unpack(messageData, data.P);
      unpack(messageData, data.AshAlk);
      }
   unsigned memorySize(const LyingFractionType& data)
      {
      return 0
              + ::memorySize(data.amount)
              + ::memorySize(data.C)
              + ::memorySize(data.N)
              + ::memorySize(data.P)
              + ::memorySize(data.AshAlk)
              ;
      }
   std::string DDML(const LyingFractionType& data)
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

   void pack(MessageData& messageData, const SurfaceOrganicMatterType& data)
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
   void unpack(MessageData& messageData, SurfaceOrganicMatterType& data)
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
   unsigned memorySize(const SurfaceOrganicMatterType& data)
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
   std::string DDML(const SurfaceOrganicMatterType& data)
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

   void pack(MessageData& messageData, const SurfaceOrganicMatterDecompType& data)
      {
      pack(messageData, data.Name);
      pack(messageData, data.OrganicMatterType);
      pack(messageData, data.amount);
      pack(messageData, data.C);
      pack(messageData, data.N);
      pack(messageData, data.P);
      pack(messageData, data.AshAlk);
      }
   void unpack(MessageData& messageData, SurfaceOrganicMatterDecompType& data)
      {
      unpack(messageData, data.Name);
      unpack(messageData, data.OrganicMatterType);
      unpack(messageData, data.amount);
      unpack(messageData, data.C);
      unpack(messageData, data.N);
      unpack(messageData, data.P);
      unpack(messageData, data.AshAlk);
      }
   unsigned memorySize(const SurfaceOrganicMatterDecompType& data)
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
   std::string DDML(const SurfaceOrganicMatterDecompType& data)
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

   void pack(MessageData& messageData, const NBalanceType& data)
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
   void unpack(MessageData& messageData, NBalanceType& data)
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
   unsigned memorySize(const NBalanceType& data)
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
   std::string DDML(const NBalanceType& data)
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

   void pack(MessageData& messageData, const CBalanceType& data)
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
   void unpack(MessageData& messageData, CBalanceType& data)
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
   unsigned memorySize(const CBalanceType& data)
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
   std::string DDML(const CBalanceType& data)
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

   void pack(MessageData& messageData, const IncorpFomType& data)
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
   void unpack(MessageData& messageData, IncorpFomType& data)
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
   unsigned memorySize(const IncorpFomType& data)
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
   std::string DDML(const IncorpFomType& data)
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

   void pack(MessageData& messageData, const SoilOrganicMatterType& data)
      {
      pack(messageData, data.OrganicMatterType);
      pack(messageData, data.layer);
      }
   void unpack(MessageData& messageData, SoilOrganicMatterType& data)
      {
      unpack(messageData, data.OrganicMatterType);
      unpack(messageData, data.layer);
      }
   unsigned memorySize(const SoilOrganicMatterType& data)
      {
      return 0
              + ::memorySize(data.OrganicMatterType)
              + ::memorySize(data.layer)
              ;
      }
   std::string DDML(const SoilOrganicMatterType& data)
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

   //------ SoilNitrogenFlowsLayer ------

   void pack(MessageData& messageData, const SoilNitrogenFlowsLayerType& data)
      {
      pack(messageData, data.nh4_transform_net);
      pack(messageData, data.no3_transform_net);
      pack(messageData, data.dlt_nh4_net);
      pack(messageData, data.dlt_no3_net);
      pack(messageData, data.dlt_oc);
      pack(messageData, data.dlt_om);
      pack(messageData, data.dlt_fomc_hum);
      pack(messageData, data.dlt_fomc_biom);
      pack(messageData, data.dlt_fomc_atm);
      pack(messageData, data.dlt_humc_biom);
      pack(messageData, data.dlt_humc_atm);
      pack(messageData, data.dlt_biomc_hum);
      pack(messageData, data.dlt_biomc_atm);
      pack(messageData, data.dlt_fomc_pool);
      }
   void unpack(MessageData& messageData, SoilNitrogenFlowsLayerType& data)
      {
      unpack(messageData, data.nh4_transform_net);
      unpack(messageData, data.no3_transform_net);
      unpack(messageData, data.dlt_nh4_net);
      unpack(messageData, data.dlt_no3_net);
      unpack(messageData, data.dlt_oc);
      unpack(messageData, data.dlt_om);
      unpack(messageData, data.dlt_fomc_hum);
      unpack(messageData, data.dlt_fomc_biom);
      unpack(messageData, data.dlt_fomc_atm);
      unpack(messageData, data.dlt_humc_biom);
      unpack(messageData, data.dlt_humc_atm);
      unpack(messageData, data.dlt_biomc_hum);
      unpack(messageData, data.dlt_biomc_atm);
      unpack(messageData, data.dlt_fomc_pool);
      }
   unsigned memorySize(const SoilNitrogenFlowsLayerType& data)
      {
      return 0
              + ::memorySize(data.nh4_transform_net)
              + ::memorySize(data.no3_transform_net)
              + ::memorySize(data.dlt_nh4_net)
              + ::memorySize(data.dlt_no3_net)
              + ::memorySize(data.dlt_oc)
              + ::memorySize(data.dlt_om)
              + ::memorySize(data.dlt_fomc_hum)
              + ::memorySize(data.dlt_fomc_biom)
              + ::memorySize(data.dlt_fomc_atm)
              + ::memorySize(data.dlt_humc_biom)
              + ::memorySize(data.dlt_humc_atm)
              + ::memorySize(data.dlt_biomc_hum)
              + ::memorySize(data.dlt_biomc_atm)
              + ::memorySize(data.dlt_fomc_pool)
              ;
      }
   std::string DDML(const SoilNitrogenFlowsLayerType& data)
      {return "<type name=\"SoilNitrogenFlowsLayer\" array=\"T\">"
               "<element>"
               "<field name=\"nh4_transform_net\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"no3_transform_net\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"dlt_nh4_net\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"dlt_no3_net\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"dlt_oc\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"dlt_om\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"dlt_fomc_hum\" kind=\"single\" array=\"T\" units=\"kg/ha\" />"
               "<field name=\"dlt_fomc_biom\" kind=\"single\" array=\"T\" units=\"kg/ha\" />"
               "<field name=\"dlt_fomc_atm\" kind=\"single\" array=\"T\" units=\"kg/ha\" />"
               "<field name=\"dlt_humc_biom\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"dlt_humc_atm\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"dlt_biomc_hum\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"dlt_biomc_atm\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"dlt_fomc_pool\" kind=\"single\" array=\"T\" units=\"kg/ha\" />"
               "</element>"
               "</type>";}

   //------ NitrogenBalance ------

   void pack(MessageData& messageData, const NitrogenBalanceType& data)
      {
      pack(messageData, data.Fertilizer);
      pack(messageData, data.ResiduesMineralised);
      pack(messageData, data.Leaching);
      pack(messageData, data.Denitrification);
      pack(messageData, data.Uptake);
      pack(messageData, data.Erosion);
      pack(messageData, data.LateralFlow);
      }
   void unpack(MessageData& messageData, NitrogenBalanceType& data)
      {
      unpack(messageData, data.Fertilizer);
      unpack(messageData, data.ResiduesMineralised);
      unpack(messageData, data.Leaching);
      unpack(messageData, data.Denitrification);
      unpack(messageData, data.Uptake);
      unpack(messageData, data.Erosion);
      unpack(messageData, data.LateralFlow);
      }
   unsigned memorySize(const NitrogenBalanceType& data)
      {
      return 0
              + ::memorySize(data.Fertilizer)
              + ::memorySize(data.ResiduesMineralised)
              + ::memorySize(data.Leaching)
              + ::memorySize(data.Denitrification)
              + ::memorySize(data.Uptake)
              + ::memorySize(data.Erosion)
              + ::memorySize(data.LateralFlow)
              ;
      }
   std::string DDML(const NitrogenBalanceType& data)
      {return "<type name=\"NitrogenBalance\" array=\"T\">"
               "<element>"
               "<field name=\"Fertilizer\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"ResiduesMineralised\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"Leaching\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"Denitrification\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"Uptake\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"Erosion\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"LateralFlow\" kind=\"single\" units=\"kg/ha\" />"
               "</element>"
               "</type>";}

   //------ CropChopped ------

   void pack(MessageData& messageData, const CropChoppedType& data)
      {
      pack(messageData, data.crop_type);
      pack(messageData, data.dm_type);
      pack(messageData, data.dlt_crop_dm);
      pack(messageData, data.dlt_dm_n);
      pack(messageData, data.fraction_to_residue);
      }
   void unpack(MessageData& messageData, CropChoppedType& data)
      {
      unpack(messageData, data.crop_type);
      unpack(messageData, data.dm_type);
      unpack(messageData, data.dlt_crop_dm);
      unpack(messageData, data.dlt_dm_n);
      unpack(messageData, data.fraction_to_residue);
      }
   unsigned memorySize(const CropChoppedType& data)
      {
      return 0
              + ::memorySize(data.crop_type)
              + ::memorySize(data.dm_type)
              + ::memorySize(data.dlt_crop_dm)
              + ::memorySize(data.dlt_dm_n)
              + ::memorySize(data.fraction_to_residue)
              ;
      }
   std::string DDML(const CropChoppedType& data)
      {return "<type name=\"CropChopped\">"
               "<field name=\"crop_type\" kind=\"string\" />"
               "<field name=\"dm_type\" kind=\"string\" array=\"T\" />"
               "<field name=\"dlt_crop_dm\" kind=\"single\" array=\"T\" />"
               "<field name=\"dlt_dm_n\" kind=\"single\" array=\"T\" />"
               "<field name=\"fraction_to_residue\" kind=\"single\" array=\"T\" />"
               "</type>";}

   //------ NewProfile ------

   void pack(MessageData& messageData, const NewProfileType& data)
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
      pack(messageData, data.dlayer_name);
      pack(messageData, data.dlayer_numbytes);
      pack(messageData, data.dlayer_code);
      pack(messageData, data.dlayer_isarray);
      pack(messageData, data.dlayer_value);
      pack(messageData, data.air_dry_dep_name);
      pack(messageData, data.air_dry_dep_numbytes);
      pack(messageData, data.air_dry_dep_code);
      pack(messageData, data.air_dry_dep_isarray);
      pack(messageData, data.air_dry_dep_value);
      pack(messageData, data.ll15_dep_name);
      pack(messageData, data.ll15_dep_numbytes);
      pack(messageData, data.ll15_dep_code);
      pack(messageData, data.ll15_dep_isarray);
      pack(messageData, data.ll15_dep_value);
      pack(messageData, data.dul_dep_name);
      pack(messageData, data.dul_dep_numbytes);
      pack(messageData, data.dul_dep_code);
      pack(messageData, data.dul_dep_isarray);
      pack(messageData, data.dul_dep_value);
      pack(messageData, data.sat_dep_name);
      pack(messageData, data.sat_dep_numbytes);
      pack(messageData, data.sat_dep_code);
      pack(messageData, data.sat_dep_isarray);
      pack(messageData, data.sat_dep_value);
      pack(messageData, data.sw_dep_name);
      pack(messageData, data.sw_dep_numbytes);
      pack(messageData, data.sw_dep_code);
      pack(messageData, data.sw_dep_isarray);
      pack(messageData, data.sw_dep_value);
      pack(messageData, data.bd_name);
      pack(messageData, data.bd_numbytes);
      pack(messageData, data.bd_code);
      pack(messageData, data.bd_isarray);
      pack(messageData, data.bd_value);
      }
   void unpack(MessageData& messageData, NewProfileType& data)
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
      unpack(messageData, data.dlayer_name);
      unpack(messageData, data.dlayer_numbytes);
      unpack(messageData, data.dlayer_code);
      unpack(messageData, data.dlayer_isarray);
      unpack(messageData, data.dlayer_value);
      unpack(messageData, data.air_dry_dep_name);
      unpack(messageData, data.air_dry_dep_numbytes);
      unpack(messageData, data.air_dry_dep_code);
      unpack(messageData, data.air_dry_dep_isarray);
      unpack(messageData, data.air_dry_dep_value);
      unpack(messageData, data.ll15_dep_name);
      unpack(messageData, data.ll15_dep_numbytes);
      unpack(messageData, data.ll15_dep_code);
      unpack(messageData, data.ll15_dep_isarray);
      unpack(messageData, data.ll15_dep_value);
      unpack(messageData, data.dul_dep_name);
      unpack(messageData, data.dul_dep_numbytes);
      unpack(messageData, data.dul_dep_code);
      unpack(messageData, data.dul_dep_isarray);
      unpack(messageData, data.dul_dep_value);
      unpack(messageData, data.sat_dep_name);
      unpack(messageData, data.sat_dep_numbytes);
      unpack(messageData, data.sat_dep_code);
      unpack(messageData, data.sat_dep_isarray);
      unpack(messageData, data.sat_dep_value);
      unpack(messageData, data.sw_dep_name);
      unpack(messageData, data.sw_dep_numbytes);
      unpack(messageData, data.sw_dep_code);
      unpack(messageData, data.sw_dep_isarray);
      unpack(messageData, data.sw_dep_value);
      unpack(messageData, data.bd_name);
      unpack(messageData, data.bd_numbytes);
      unpack(messageData, data.bd_code);
      unpack(messageData, data.bd_isarray);
      unpack(messageData, data.bd_value);
      }
   unsigned memorySize(const NewProfileType& data)
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
              + ::memorySize(data.dlayer_name)
              + ::memorySize(data.dlayer_numbytes)
              + ::memorySize(data.dlayer_code)
              + ::memorySize(data.dlayer_isarray)
              + ::memorySize(data.dlayer_value)
              + ::memorySize(data.air_dry_dep_name)
              + ::memorySize(data.air_dry_dep_numbytes)
              + ::memorySize(data.air_dry_dep_code)
              + ::memorySize(data.air_dry_dep_isarray)
              + ::memorySize(data.air_dry_dep_value)
              + ::memorySize(data.ll15_dep_name)
              + ::memorySize(data.ll15_dep_numbytes)
              + ::memorySize(data.ll15_dep_code)
              + ::memorySize(data.ll15_dep_isarray)
              + ::memorySize(data.ll15_dep_value)
              + ::memorySize(data.dul_dep_name)
              + ::memorySize(data.dul_dep_numbytes)
              + ::memorySize(data.dul_dep_code)
              + ::memorySize(data.dul_dep_isarray)
              + ::memorySize(data.dul_dep_value)
              + ::memorySize(data.sat_dep_name)
              + ::memorySize(data.sat_dep_numbytes)
              + ::memorySize(data.sat_dep_code)
              + ::memorySize(data.sat_dep_isarray)
              + ::memorySize(data.sat_dep_value)
              + ::memorySize(data.sw_dep_name)
              + ::memorySize(data.sw_dep_numbytes)
              + ::memorySize(data.sw_dep_code)
              + ::memorySize(data.sw_dep_isarray)
              + ::memorySize(data.sw_dep_value)
              + ::memorySize(data.bd_name)
              + ::memorySize(data.bd_numbytes)
              + ::memorySize(data.bd_code)
              + ::memorySize(data.bd_isarray)
              + ::memorySize(data.bd_value)
              ;
      }
   std::string DDML(const NewProfileType& data)
      {return "<type name=\"NewProfile\">"
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
               "<field name=\"dlayer_name\" kind=\"string\" />"
               "<field name=\"dlayer_numbytes\" kind=\"integer4\" />"
               "<field name=\"dlayer_code\" kind=\"integer4\" />"
               "<field name=\"dlayer_isarray\" kind=\"boolean\" />"
               "<field name=\"dlayer_value\" kind=\"single\" array=\"T\" />"
               "<field name=\"air_dry_dep_name\" kind=\"string\" />"
               "<field name=\"air_dry_dep_numbytes\" kind=\"integer4\" />"
               "<field name=\"air_dry_dep_code\" kind=\"integer4\" />"
               "<field name=\"air_dry_dep_isarray\" kind=\"boolean\" />"
               "<field name=\"air_dry_dep_value\" kind=\"single\" array=\"T\" />"
               "<field name=\"ll15_dep_name\" kind=\"string\" />"
               "<field name=\"ll15_dep_numbytes\" kind=\"integer4\" />"
               "<field name=\"ll15_dep_code\" kind=\"integer4\" />"
               "<field name=\"ll15_dep_isarray\" kind=\"boolean\" />"
               "<field name=\"ll15_dep_value\" kind=\"single\" array=\"T\" />"
               "<field name=\"dul_dep_name\" kind=\"string\" />"
               "<field name=\"dul_dep_numbytes\" kind=\"integer4\" />"
               "<field name=\"dul_dep_code\" kind=\"integer4\" />"
               "<field name=\"dul_dep_isarray\" kind=\"boolean\" />"
               "<field name=\"dul_dep_value\" kind=\"single\" array=\"T\" />"
               "<field name=\"sat_dep_name\" kind=\"string\" />"
               "<field name=\"sat_dep_numbytes\" kind=\"integer4\" />"
               "<field name=\"sat_dep_code\" kind=\"integer4\" />"
               "<field name=\"sat_dep_isarray\" kind=\"boolean\" />"
               "<field name=\"sat_dep_value\" kind=\"single\" array=\"T\" />"
               "<field name=\"sw_dep_name\" kind=\"string\" />"
               "<field name=\"sw_dep_numbytes\" kind=\"integer4\" />"
               "<field name=\"sw_dep_code\" kind=\"integer4\" />"
               "<field name=\"sw_dep_isarray\" kind=\"boolean\" />"
               "<field name=\"sw_dep_value\" kind=\"single\" array=\"T\" />"
               "<field name=\"bd_name\" kind=\"string\" />"
               "<field name=\"bd_numbytes\" kind=\"integer4\" />"
               "<field name=\"bd_code\" kind=\"integer4\" />"
               "<field name=\"bd_isarray\" kind=\"boolean\" />"
               "<field name=\"bd_value\" kind=\"single\" array=\"T\" />"
               "</type>";}

   //------ NewPotentialGrowth ------

   void pack(MessageData& messageData, const NewPotentialGrowthType& data)
      {
      pack(messageData, data.sender);
      pack(messageData, data.frgr);
      }
   void unpack(MessageData& messageData, NewPotentialGrowthType& data)
      {
      unpack(messageData, data.sender);
      unpack(messageData, data.frgr);
      }
   unsigned memorySize(const NewPotentialGrowthType& data)
      {
      return 0
              + ::memorySize(data.sender)
              + ::memorySize(data.frgr)
              ;
      }
   std::string DDML(const NewPotentialGrowthType& data)
      {return "<type name=\"NewPotentialGrowth\">"
               "<field name=\"sender\" kind=\"string\" />"
               "<field name=\"frgr\" kind=\"single\" units=\"\" />"
               "</type>";}

   //------ NewCanopy ------

   void pack(MessageData& messageData, const NewCanopyType& data)
      {
      pack(messageData, data.sender);
      pack(messageData, data.height);
      pack(messageData, data.depth);
      pack(messageData, data.lai);
      pack(messageData, data.lai_tot);
      pack(messageData, data.cover);
      pack(messageData, data.cover_tot);
      }
   void unpack(MessageData& messageData, NewCanopyType& data)
      {
      unpack(messageData, data.sender);
      unpack(messageData, data.height);
      unpack(messageData, data.depth);
      unpack(messageData, data.lai);
      unpack(messageData, data.lai_tot);
      unpack(messageData, data.cover);
      unpack(messageData, data.cover_tot);
      }
   unsigned memorySize(const NewCanopyType& data)
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
   std::string DDML(const NewCanopyType& data)
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

   void pack(MessageData& messageData, const NewCropType& data)
      {
      pack(messageData, data.sender);
      pack(messageData, data.crop_type);
      }
   void unpack(MessageData& messageData, NewCropType& data)
      {
      unpack(messageData, data.sender);
      unpack(messageData, data.crop_type);
      }
   unsigned memorySize(const NewCropType& data)
      {
      return 0
              + ::memorySize(data.sender)
              + ::memorySize(data.crop_type)
              ;
      }
   std::string DDML(const NewCropType& data)
      {return "<type name=\"NewCrop\">"
               "<field name=\"sender\" kind=\"string\" />"
               "<field name=\"crop_type\" kind=\"string\" />"
               "</type>";}

   //------ NewZone ------

   void pack(MessageData& messageData, const NewZoneType& data)
      {
      pack(messageData, data.sender);
      pack(messageData, data.area);
      pack(messageData, data.slope);
      pack(messageData, data.X);
      pack(messageData, data.Y);
      }
   void unpack(MessageData& messageData, NewZoneType& data)
      {
      unpack(messageData, data.sender);
      unpack(messageData, data.area);
      unpack(messageData, data.slope);
      unpack(messageData, data.X);
      unpack(messageData, data.Y);
      }
   unsigned memorySize(const NewZoneType& data)
      {
      return 0
              + ::memorySize(data.sender)
              + ::memorySize(data.area)
              + ::memorySize(data.slope)
              + ::memorySize(data.X)
              + ::memorySize(data.Y)
              ;
      }
   std::string DDML(const NewZoneType& data)
      {return "<type name=\"NewZone\">"
               "<field name=\"sender\" kind=\"string\" />"
               "<field name=\"area\" kind=\"single\" />"
               "<field name=\"slope\" kind=\"single\" />"
               "<field name=\"X\" kind=\"single\" />"
               "<field name=\"Y\" kind=\"single\" />"
               "</type>";}

   //------ SoilLayers ------

   void pack(MessageData& messageData, const SoilLayersType& data)
      {
      pack(messageData, data.layers);
      pack(messageData, data.value);
      }
   void unpack(MessageData& messageData, SoilLayersType& data)
      {
      unpack(messageData, data.layers);
      unpack(messageData, data.value);
      }
   unsigned memorySize(const SoilLayersType& data)
      {
      return 0
              + ::memorySize(data.layers)
              + ::memorySize(data.value)
              ;
      }
   std::string DDML(const SoilLayersType& data)
      {return "<type name=\"SoilLayers\">"
               "<field name=\"layers\" kind=\"double\" unit=\"mm\" array=\"T\" />"
               "<field name=\"value\" kind=\"double\" unit=\"mm\" array=\"T\" />"
               "</type>";}

   //------ rlv_layer ------

   void pack(MessageData& messageData, const rlv_layerType& data)
      {
      pack(messageData, data.layers);
      pack(messageData, data.rlv);
      }
   void unpack(MessageData& messageData, rlv_layerType& data)
      {
      unpack(messageData, data.layers);
      unpack(messageData, data.rlv);
      }
   unsigned memorySize(const rlv_layerType& data)
      {
      return 0
              + ::memorySize(data.layers)
              + ::memorySize(data.rlv)
              ;
      }
   std::string DDML(const rlv_layerType& data)
      {return "<field name=\"rlv_layer\">"
               "<field name=\"layers\" kind=\"double\" units=\"mm\" array=\"T\" />"
               "<field name=\"rlv\" kind=\"double\" units=\"mmmm^3\" array=\"T\" />"
               "</field>";}

   //------ demands ------

   void pack(MessageData& messageData, const demandsType& data)
      {
      pack(messageData, data.crop_ident);
      pack(messageData, data.crop_type);
      pack(messageData, data.rlv_layer);
      pack(messageData, data.demand);
      }
   void unpack(MessageData& messageData, demandsType& data)
      {
      unpack(messageData, data.crop_ident);
      unpack(messageData, data.crop_type);
      unpack(messageData, data.rlv_layer);
      unpack(messageData, data.demand);
      }
   unsigned memorySize(const demandsType& data)
      {
      return 0
              + ::memorySize(data.crop_ident)
              + ::memorySize(data.crop_type)
              + ::memorySize(data.rlv_layer)
              + ::memorySize(data.demand)
              ;
      }
   std::string DDML(const demandsType& data)
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

   void pack(MessageData& messageData, const PastureWaterDemandType& data)
      {
      pack(messageData, data.demands);
      }
   void unpack(MessageData& messageData, PastureWaterDemandType& data)
      {
      unpack(messageData, data.demands);
      }
   unsigned memorySize(const PastureWaterDemandType& data)
      {
      return 0
              + ::memorySize(data.demands)
              ;
      }
   std::string DDML(const PastureWaterDemandType& data)
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

   void pack(MessageData& messageData, const suppliesType& data)
      {
      pack(messageData, data.crop_ident);
      pack(messageData, data.layers);
      pack(messageData, data.supply);
      }
   void unpack(MessageData& messageData, suppliesType& data)
      {
      unpack(messageData, data.crop_ident);
      unpack(messageData, data.layers);
      unpack(messageData, data.supply);
      }
   unsigned memorySize(const suppliesType& data)
      {
      return 0
              + ::memorySize(data.crop_ident)
              + ::memorySize(data.layers)
              + ::memorySize(data.supply)
              ;
      }
   std::string DDML(const suppliesType& data)
      {return "<field name=\"supplies\" array=\"T\">"
               "<element>"
               "<field name=\"crop_ident\" kind=\"string\" description=\"Cohort name + instance name\" />"
               "<field name=\"layers\" kind=\"double\" unit=\"mm\" array=\"T\" />"
               "<field name=\"supply\" kind=\"double\" unit=\"mm\" array=\"T\" />"
               "</element>"
               "</field>";}

   //------ PastureWaterSupply ------

   void pack(MessageData& messageData, const PastureWaterSupplyType& data)
      {
      pack(messageData, data.supplies);
      }
   void unpack(MessageData& messageData, PastureWaterSupplyType& data)
      {
      unpack(messageData, data.supplies);
      }
   unsigned memorySize(const PastureWaterSupplyType& data)
      {
      return 0
              + ::memorySize(data.supplies)
              ;
      }
   std::string DDML(const PastureWaterSupplyType& data)
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

   void pack(MessageData& messageData, const water_uptakeType& data)
      {
      pack(messageData, data.crop_ident);
      pack(messageData, data.layers);
      pack(messageData, data.uptake);
      }
   void unpack(MessageData& messageData, water_uptakeType& data)
      {
      unpack(messageData, data.crop_ident);
      unpack(messageData, data.layers);
      unpack(messageData, data.uptake);
      }
   unsigned memorySize(const water_uptakeType& data)
      {
      return 0
              + ::memorySize(data.crop_ident)
              + ::memorySize(data.layers)
              + ::memorySize(data.uptake)
              ;
      }
   std::string DDML(const water_uptakeType& data)
      {return "<field name=\"water_uptake\" array=\"T\">"
               "<element>"
               "<field name=\"crop_ident\" kind=\"string\" description=\"Cohort name + instance name\" />"
               "<field name=\"layers\" kind=\"double\" unit=\"mm\" array=\"T\" />"
               "<field name=\"uptake\" kind=\"double\" unit=\"mm\" array=\"T\" />"
               "</element>"
               "</field>";}

   //------ PastureWaterUptake ------

   void pack(MessageData& messageData, const PastureWaterUptakeType& data)
      {
      pack(messageData, data.water_uptake);
      }
   void unpack(MessageData& messageData, PastureWaterUptakeType& data)
      {
      unpack(messageData, data.water_uptake);
      }
   unsigned memorySize(const PastureWaterUptakeType& data)
      {
      return 0
              + ::memorySize(data.water_uptake)
              ;
      }
   std::string DDML(const PastureWaterUptakeType& data)
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

   void pack(MessageData& messageData, const water_infoType& data)
      {
      pack(messageData, data.crop_ident);
      pack(messageData, data.params);
      pack(messageData, data.demand);
      pack(messageData, data.layers);
      pack(messageData, data.rlv);
      pack(messageData, data.root_radius);
      }
   void unpack(MessageData& messageData, water_infoType& data)
      {
      unpack(messageData, data.crop_ident);
      unpack(messageData, data.params);
      unpack(messageData, data.demand);
      unpack(messageData, data.layers);
      unpack(messageData, data.rlv);
      unpack(messageData, data.root_radius);
      }
   unsigned memorySize(const water_infoType& data)
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
   std::string DDML(const water_infoType& data)
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

   void pack(MessageData& messageData, const WaterInfoType& data)
      {
      pack(messageData, data.water_info);
      }
   void unpack(MessageData& messageData, WaterInfoType& data)
      {
      unpack(messageData, data.water_info);
      }
   unsigned memorySize(const WaterInfoType& data)
      {
      return 0
              + ::memorySize(data.water_info)
              ;
      }
   std::string DDML(const WaterInfoType& data)
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

   void pack(MessageData& messageData, const fomType& data)
      {
      pack(messageData, data.weight);
      pack(messageData, data.n);
      pack(messageData, data.p);
      pack(messageData, data.s);
      pack(messageData, data.ash_alk);
      }
   void unpack(MessageData& messageData, fomType& data)
      {
      unpack(messageData, data.weight);
      unpack(messageData, data.n);
      unpack(messageData, data.p);
      unpack(messageData, data.s);
      unpack(messageData, data.ash_alk);
      }
   unsigned memorySize(const fomType& data)
      {
      return 0
              + ::memorySize(data.weight)
              + ::memorySize(data.n)
              + ::memorySize(data.p)
              + ::memorySize(data.s)
              + ::memorySize(data.ash_alk)
              ;
      }
   std::string DDML(const fomType& data)
      {return "<field name=\"fom\" array=\"T\">"
               "<element>"
               "<field name=\"weight\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"n\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"p\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"s\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"ash_alk\" unit=\"mol/ha\" kind=\"double\" />"
               "</element>"
               "</field>";}

   //------ FomAdded ------

   void pack(MessageData& messageData, const FomAddedType& data)
      {
      pack(messageData, data.layers);
      pack(messageData, data.fom);
      }
   void unpack(MessageData& messageData, FomAddedType& data)
      {
      unpack(messageData, data.layers);
      unpack(messageData, data.fom);
      }
   unsigned memorySize(const FomAddedType& data)
      {
      return 0
              + ::memorySize(data.layers)
              + ::memorySize(data.fom)
              ;
      }
   std::string DDML(const FomAddedType& data)
      {return "<type name=\"FomAdded\">"
               "<field name=\"layers\" unit=\"mm\" kind=\"double\" array=\"T\" />"
               "<field name=\"fom\" array=\"T\">"
               "<element>"
               "<field name=\"weight\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"n\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"p\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"s\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"ash_alk\" unit=\"mol/ha\" kind=\"double\" />"
               "</element>"
               "</field>"
               "</type>";}

   //------ PastureNutrientUptake ------

   void pack(MessageData& messageData, const PastureNutrientUptakeType& data)
      {
      pack(messageData, data.nutrient);
      pack(messageData, data.layers);
      pack(messageData, data.uptake);
      }
   void unpack(MessageData& messageData, PastureNutrientUptakeType& data)
      {
      unpack(messageData, data.nutrient);
      unpack(messageData, data.layers);
      unpack(messageData, data.uptake);
      }
   unsigned memorySize(const PastureNutrientUptakeType& data)
      {
      return 0
              + ::memorySize(data.nutrient)
              + ::memorySize(data.layers)
              + ::memorySize(data.uptake)
              ;
      }
   std::string DDML(const PastureNutrientUptakeType& data)
      {return "<type name=\"PastureNutrientUptake\">"
               "<field name=\"nutrient\" kind=\"string\" />"
               "<field name=\"layers\" unit=\"mm\" kind=\"double\" array=\"T\" />"
               "<field name=\"uptake\" unit=\"kg/ha\" kind=\"double\" array=\"T\" />"
               "</type>";}

   //------ PastureSow ------

   void pack(MessageData& messageData, const PastureSowType& data)
      {
      pack(messageData, data.rate);
      }
   void unpack(MessageData& messageData, PastureSowType& data)
      {
      unpack(messageData, data.rate);
      }
   unsigned memorySize(const PastureSowType& data)
      {
      return 0
              + ::memorySize(data.rate)
              ;
      }
   std::string DDML(const PastureSowType& data)
      {return "<type name=\"PastureSow\">"
               "<field name=\"rate\" unit=\"kg/ha\" kind=\"double\" />"
               "</type>";}

   //------ PastureKill ------

   void pack(MessageData& messageData, const PastureKillType& data)
      {
      pack(messageData, data.propn_herbage);
      pack(messageData, data.propn_seeds);
      }
   void unpack(MessageData& messageData, PastureKillType& data)
      {
      unpack(messageData, data.propn_herbage);
      unpack(messageData, data.propn_seeds);
      }
   unsigned memorySize(const PastureKillType& data)
      {
      return 0
              + ::memorySize(data.propn_herbage)
              + ::memorySize(data.propn_seeds)
              ;
      }
   std::string DDML(const PastureKillType& data)
      {return "<type name=\"PastureKill\">"
               "<field name=\"propn_herbage\" unit=\"-\" kind=\"double\" />"
               "<field name=\"propn_seeds\" unit=\"-\" kind=\"double\" />"
               "</type>";}

   //------ PastureCultivate ------

   void pack(MessageData& messageData, const PastureCultivateType& data)
      {
      pack(messageData, data.depth);
      pack(messageData, data.propn_incorp);
      pack(messageData, data.propn_mixed);
      }
   void unpack(MessageData& messageData, PastureCultivateType& data)
      {
      unpack(messageData, data.depth);
      unpack(messageData, data.propn_incorp);
      unpack(messageData, data.propn_mixed);
      }
   unsigned memorySize(const PastureCultivateType& data)
      {
      return 0
              + ::memorySize(data.depth)
              + ::memorySize(data.propn_incorp)
              + ::memorySize(data.propn_mixed)
              ;
      }
   std::string DDML(const PastureCultivateType& data)
      {return "<type name=\"PastureCultivate\">"
               "<field name=\"depth\" unit=\"mm\" kind=\"double\" />"
               "<field name=\"propn_incorp\" unit=\"-\" kind=\"double\" />"
               "<field name=\"propn_mixed\" unit=\"-\" kind=\"double\" />"
               "</type>";}

   //------ PastureCut ------

   void pack(MessageData& messageData, const PastureCutType& data)
      {
      pack(messageData, data.cut_height);
      pack(messageData, data.gathered);
      pack(messageData, data.dmd_loss);
      pack(messageData, data.dm_content);
      }
   void unpack(MessageData& messageData, PastureCutType& data)
      {
      unpack(messageData, data.cut_height);
      unpack(messageData, data.gathered);
      unpack(messageData, data.dmd_loss);
      unpack(messageData, data.dm_content);
      }
   unsigned memorySize(const PastureCutType& data)
      {
      return 0
              + ::memorySize(data.cut_height)
              + ::memorySize(data.gathered)
              + ::memorySize(data.dmd_loss)
              + ::memorySize(data.dm_content)
              ;
      }
   std::string DDML(const PastureCutType& data)
      {return "<type name=\"PastureCut\">"
               "<field name=\"cut_height\" unit=\"mm\" kind=\"double\" />"
               "<field name=\"gathered\" unit=\"-\" kind=\"double\" />"
               "<field name=\"dmd_loss\" unit=\"-\" kind=\"double\" />"
               "<field name=\"dm_content\" unit=\"kg/kg\" kind=\"double\" />"
               "</type>";}

   //------ PastureOnCut ------

   void pack(MessageData& messageData, const PastureOnCutType& data)
      {
      pack(messageData, data.fresh_wt);
      pack(messageData, data.dm_content);
      pack(messageData, data.dm);
      pack(messageData, data.cp_conc);
      pack(messageData, data.p_conc);
      pack(messageData, data.s_conc);
      pack(messageData, data.ash_alk);
      }
   void unpack(MessageData& messageData, PastureOnCutType& data)
      {
      unpack(messageData, data.fresh_wt);
      unpack(messageData, data.dm_content);
      unpack(messageData, data.dm);
      unpack(messageData, data.cp_conc);
      unpack(messageData, data.p_conc);
      unpack(messageData, data.s_conc);
      unpack(messageData, data.ash_alk);
      }
   unsigned memorySize(const PastureOnCutType& data)
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
   std::string DDML(const PastureOnCutType& data)
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

   void pack(MessageData& messageData, const PastureWeatherType& data)
      {
      pack(messageData, data.maxt);
      pack(messageData, data.mint);
      pack(messageData, data.rain);
      pack(messageData, data.snow);
      pack(messageData, data.radn);
      pack(messageData, data.vpd);
      pack(messageData, data.wind);
      }
   void unpack(MessageData& messageData, PastureWeatherType& data)
      {
      unpack(messageData, data.maxt);
      unpack(messageData, data.mint);
      unpack(messageData, data.rain);
      unpack(messageData, data.snow);
      unpack(messageData, data.radn);
      unpack(messageData, data.vpd);
      unpack(messageData, data.wind);
      }
   unsigned memorySize(const PastureWeatherType& data)
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
   std::string DDML(const PastureWeatherType& data)
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

   void pack(MessageData& messageData, const FaecesType& data)
      {
      pack(messageData, data.weight);
      pack(messageData, data.n);
      pack(messageData, data.p);
      pack(messageData, data.s);
      pack(messageData, data.ash_alk);
      }
   void unpack(MessageData& messageData, FaecesType& data)
      {
      unpack(messageData, data.weight);
      unpack(messageData, data.n);
      unpack(messageData, data.p);
      unpack(messageData, data.s);
      unpack(messageData, data.ash_alk);
      }
   unsigned memorySize(const FaecesType& data)
      {
      return 0
              + ::memorySize(data.weight)
              + ::memorySize(data.n)
              + ::memorySize(data.p)
              + ::memorySize(data.s)
              + ::memorySize(data.ash_alk)
              ;
      }
   std::string DDML(const FaecesType& data)
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

   void pack(MessageData& messageData, const FaecesInorgType& data)
      {
      pack(messageData, data.n);
      pack(messageData, data.p);
      pack(messageData, data.s);
      }
   void unpack(MessageData& messageData, FaecesInorgType& data)
      {
      unpack(messageData, data.n);
      unpack(messageData, data.p);
      unpack(messageData, data.s);
      }
   unsigned memorySize(const FaecesInorgType& data)
      {
      return 0
              + ::memorySize(data.n)
              + ::memorySize(data.p)
              + ::memorySize(data.s)
              ;
      }
   std::string DDML(const FaecesInorgType& data)
      {return "<type name=\"FaecesInorg\" array=\"T\">"
               "<element>"
               "<field name=\"n\" unit=\"kg/d\" kind=\"double\" />"
               "<field name=\"p\" unit=\"kg/d\" kind=\"double\" />"
               "<field name=\"s\" unit=\"kg/d\" kind=\"double\" />"
               "</element>"
               "</type>";}

   //------ Intake ------

   void pack(MessageData& messageData, const IntakeType& data)
      {
      pack(messageData, data.weight);
      pack(messageData, data.n);
      pack(messageData, data.p);
      pack(messageData, data.s);
      pack(messageData, data.ash_alk);
      }
   void unpack(MessageData& messageData, IntakeType& data)
      {
      unpack(messageData, data.weight);
      unpack(messageData, data.n);
      unpack(messageData, data.p);
      unpack(messageData, data.s);
      unpack(messageData, data.ash_alk);
      }
   unsigned memorySize(const IntakeType& data)
      {
      return 0
              + ::memorySize(data.weight)
              + ::memorySize(data.n)
              + ::memorySize(data.p)
              + ::memorySize(data.s)
              + ::memorySize(data.ash_alk)
              ;
      }
   std::string DDML(const IntakeType& data)
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

   void pack(MessageData& messageData, const PastIntakeType& data)
      {
      pack(messageData, data.weight);
      pack(messageData, data.n);
      pack(messageData, data.p);
      pack(messageData, data.s);
      pack(messageData, data.ash_alk);
      }
   void unpack(MessageData& messageData, PastIntakeType& data)
      {
      unpack(messageData, data.weight);
      unpack(messageData, data.n);
      unpack(messageData, data.p);
      unpack(messageData, data.s);
      unpack(messageData, data.ash_alk);
      }
   unsigned memorySize(const PastIntakeType& data)
      {
      return 0
              + ::memorySize(data.weight)
              + ::memorySize(data.n)
              + ::memorySize(data.p)
              + ::memorySize(data.s)
              + ::memorySize(data.ash_alk)
              ;
      }
   std::string DDML(const PastIntakeType& data)
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

   void pack(MessageData& messageData, const SuppIntakeType& data)
      {
      pack(messageData, data.weight);
      pack(messageData, data.n);
      pack(messageData, data.p);
      pack(messageData, data.s);
      pack(messageData, data.ash_alk);
      }
   void unpack(MessageData& messageData, SuppIntakeType& data)
      {
      unpack(messageData, data.weight);
      unpack(messageData, data.n);
      unpack(messageData, data.p);
      unpack(messageData, data.s);
      unpack(messageData, data.ash_alk);
      }
   unsigned memorySize(const SuppIntakeType& data)
      {
      return 0
              + ::memorySize(data.weight)
              + ::memorySize(data.n)
              + ::memorySize(data.p)
              + ::memorySize(data.s)
              + ::memorySize(data.ash_alk)
              ;
      }
   std::string DDML(const SuppIntakeType& data)
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

   void pack(MessageData& messageData, const faeces_omType& data)
      {
      pack(messageData, data.weight);
      pack(messageData, data.n);
      pack(messageData, data.p);
      pack(messageData, data.s);
      pack(messageData, data.ash_alk);
      }
   void unpack(MessageData& messageData, faeces_omType& data)
      {
      unpack(messageData, data.weight);
      unpack(messageData, data.n);
      unpack(messageData, data.p);
      unpack(messageData, data.s);
      unpack(messageData, data.ash_alk);
      }
   unsigned memorySize(const faeces_omType& data)
      {
      return 0
              + ::memorySize(data.weight)
              + ::memorySize(data.n)
              + ::memorySize(data.p)
              + ::memorySize(data.s)
              + ::memorySize(data.ash_alk)
              ;
      }
   std::string DDML(const faeces_omType& data)
      {return "<type name=\"faeces_om\">"
               "<field name=\"weight\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"n\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"p\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"s\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"ash_alk\" unit=\"mol/ha\" kind=\"double\" />"
               "</type>";}

   //------ faeces_inorg ------

   void pack(MessageData& messageData, const faeces_inorgType& data)
      {
      pack(messageData, data.n);
      pack(messageData, data.p);
      pack(messageData, data.s);
      }
   void unpack(MessageData& messageData, faeces_inorgType& data)
      {
      unpack(messageData, data.n);
      unpack(messageData, data.p);
      unpack(messageData, data.s);
      }
   unsigned memorySize(const faeces_inorgType& data)
      {
      return 0
              + ::memorySize(data.n)
              + ::memorySize(data.p)
              + ::memorySize(data.s)
              ;
      }
   std::string DDML(const faeces_inorgType& data)
      {return "<type name=\"faeces_inorg\">"
               "<field name=\"n\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"p\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"s\" unit=\"kg/ha\" kind=\"double\" />"
               "</type>";}

   //------ urine ------

   void pack(MessageData& messageData, const urineType& data)
      {
      pack(messageData, data.volume);
      pack(messageData, data.urea);
      pack(messageData, data.pox);
      pack(messageData, data.so4);
      pack(messageData, data.ash_alk);
      }
   void unpack(MessageData& messageData, urineType& data)
      {
      unpack(messageData, data.volume);
      unpack(messageData, data.urea);
      unpack(messageData, data.pox);
      unpack(messageData, data.so4);
      unpack(messageData, data.ash_alk);
      }
   unsigned memorySize(const urineType& data)
      {
      return 0
              + ::memorySize(data.volume)
              + ::memorySize(data.urea)
              + ::memorySize(data.pox)
              + ::memorySize(data.so4)
              + ::memorySize(data.ash_alk)
              ;
      }
   std::string DDML(const urineType& data)
      {return "<type name=\"urine\">"
               "<field name=\"volume\" unit=\"m3/ha\" kind=\"double\" />"
               "<field name=\"urea\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"pox\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"so4\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"ash_alk\" unit=\"mol/ha\" kind=\"double\" />"
               "</type>";}

   //------ AddExcreta ------

   void pack(MessageData& messageData, const AddExcretaType& data)
      {
      pack(messageData, data.faeces_om);
      pack(messageData, data.faeces_inorg);
      pack(messageData, data.urine);
      }
   void unpack(MessageData& messageData, AddExcretaType& data)
      {
      unpack(messageData, data.faeces_om);
      unpack(messageData, data.faeces_inorg);
      unpack(messageData, data.urine);
      }
   unsigned memorySize(const AddExcretaType& data)
      {
      return 0
              + ::memorySize(data.faeces_om)
              + ::memorySize(data.faeces_inorg)
              + ::memorySize(data.urine)
              ;
      }
   std::string DDML(const AddExcretaType& data)
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

   void pack(MessageData& messageData, const RemoveHerbageType& data)
      {
      pack(messageData, data.herbage);
      pack(messageData, data.seed);
      }
   void unpack(MessageData& messageData, RemoveHerbageType& data)
      {
      unpack(messageData, data.herbage);
      unpack(messageData, data.seed);
      }
   unsigned memorySize(const RemoveHerbageType& data)
      {
      return 0
              + ::memorySize(data.herbage)
              + ::memorySize(data.seed)
              ;
      }
   std::string DDML(const RemoveHerbageType& data)
      {return "<type name=\"RemoveHerbage\">"
               "<field name=\"herbage\" unit=\"kg/ha\" kind=\"double\" array=\"T\" />"
               "<field name=\"seed\" unit=\"kg/ha\" kind=\"double\" array=\"T\" />"
               "</type>";}

   //------ SuppEaten ------

   void pack(MessageData& messageData, const SuppEatenType& data)
      {
      pack(messageData, data.paddock);
      pack(messageData, data.eaten);
      }
   void unpack(MessageData& messageData, SuppEatenType& data)
      {
      unpack(messageData, data.paddock);
      unpack(messageData, data.eaten);
      }
   unsigned memorySize(const SuppEatenType& data)
      {
      return 0
              + ::memorySize(data.paddock)
              + ::memorySize(data.eaten)
              ;
      }
   std::string DDML(const SuppEatenType& data)
      {return "<type name=\"SuppEaten\" array=\"T\">"
               "<element>"
               "<field name=\"paddock\" unit=\"\" kind=\"string\" />"
               "<field name=\"eaten\" unit=\"kg\" kind=\"double\" />"
               "</element>"
               "</type>";}

   //------ herbage ------

   void pack(MessageData& messageData, const herbageType& data)
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
   void unpack(MessageData& messageData, herbageType& data)
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
   unsigned memorySize(const herbageType& data)
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
   std::string DDML(const herbageType& data)
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

   void pack(MessageData& messageData, const seedType& data)
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
   void unpack(MessageData& messageData, seedType& data)
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
   unsigned memorySize(const seedType& data)
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
   std::string DDML(const seedType& data)
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

   void pack(MessageData& messageData, const Plant2StockType& data)
      {
      pack(messageData, data.herbage);
      pack(messageData, data.propn_green);
      pack(messageData, data.legume);
      pack(messageData, data.select_factor);
      pack(messageData, data.seed);
      pack(messageData, data.seed_class);
      }
   void unpack(MessageData& messageData, Plant2StockType& data)
      {
      unpack(messageData, data.herbage);
      unpack(messageData, data.propn_green);
      unpack(messageData, data.legume);
      unpack(messageData, data.select_factor);
      unpack(messageData, data.seed);
      unpack(messageData, data.seed_class);
      }
   unsigned memorySize(const Plant2StockType& data)
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
   std::string DDML(const Plant2StockType& data)
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

   void pack(MessageData& messageData, const BuyStockType& data)
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
   void unpack(MessageData& messageData, BuyStockType& data)
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
   unsigned memorySize(const BuyStockType& data)
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
   std::string DDML(const BuyStockType& data)
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

   void pack(MessageData& messageData, const SellStockType& data)
      {
      pack(messageData, data.group);
      pack(messageData, data.number);
      }
   void unpack(MessageData& messageData, SellStockType& data)
      {
      unpack(messageData, data.group);
      unpack(messageData, data.number);
      }
   unsigned memorySize(const SellStockType& data)
      {
      return 0
              + ::memorySize(data.group)
              + ::memorySize(data.number)
              ;
      }
   std::string DDML(const SellStockType& data)
      {return "<type name=\"SellStock\">"
               "<field name=\"group\" unit=\"\" kind=\"integer4\" />"
               "<field name=\"number\" unit=\"\" kind=\"integer4\" />"
               "</type>";}

   //------ CastrateStock ------

   void pack(MessageData& messageData, const CastrateStockType& data)
      {
      pack(messageData, data.group);
      pack(messageData, data.number);
      }
   void unpack(MessageData& messageData, CastrateStockType& data)
      {
      unpack(messageData, data.group);
      unpack(messageData, data.number);
      }
   unsigned memorySize(const CastrateStockType& data)
      {
      return 0
              + ::memorySize(data.group)
              + ::memorySize(data.number)
              ;
      }
   std::string DDML(const CastrateStockType& data)
      {return "<type name=\"CastrateStock\">"
               "<field name=\"group\" unit=\"\" kind=\"integer4\" />"
               "<field name=\"number\" unit=\"\" kind=\"integer4\" />"
               "</type>";}

   //------ DryOffStock ------

   void pack(MessageData& messageData, const DryOffStockType& data)
      {
      pack(messageData, data.group);
      pack(messageData, data.number);
      }
   void unpack(MessageData& messageData, DryOffStockType& data)
      {
      unpack(messageData, data.group);
      unpack(messageData, data.number);
      }
   unsigned memorySize(const DryOffStockType& data)
      {
      return 0
              + ::memorySize(data.group)
              + ::memorySize(data.number)
              ;
      }
   std::string DDML(const DryOffStockType& data)
      {return "<type name=\"DryOffStock\">"
               "<field name=\"group\" unit=\"\" kind=\"integer4\" />"
               "<field name=\"number\" unit=\"\" kind=\"integer4\" />"
               "</type>";}

   //------ JoinStock ------

   void pack(MessageData& messageData, const JoinStockType& data)
      {
      pack(messageData, data.group);
      pack(messageData, data.mate_to);
      pack(messageData, data.mate_days);
      }
   void unpack(MessageData& messageData, JoinStockType& data)
      {
      unpack(messageData, data.group);
      unpack(messageData, data.mate_to);
      unpack(messageData, data.mate_days);
      }
   unsigned memorySize(const JoinStockType& data)
      {
      return 0
              + ::memorySize(data.group)
              + ::memorySize(data.mate_to)
              + ::memorySize(data.mate_days)
              ;
      }
   std::string DDML(const JoinStockType& data)
      {return "<type name=\"JoinStock\">"
               "<field name=\"group\" unit=\"\" kind=\"integer4\" />"
               "<field name=\"mate_to\" unit=\"\" kind=\"string\" />"
               "<field name=\"mate_days\" unit=\"d\" kind=\"integer4\" />"
               "</type>";}

   //------ MoveStock ------

   void pack(MessageData& messageData, const MoveStockType& data)
      {
      pack(messageData, data.group);
      pack(messageData, data.paddock);
      }
   void unpack(MessageData& messageData, MoveStockType& data)
      {
      unpack(messageData, data.group);
      unpack(messageData, data.paddock);
      }
   unsigned memorySize(const MoveStockType& data)
      {
      return 0
              + ::memorySize(data.group)
              + ::memorySize(data.paddock)
              ;
      }
   std::string DDML(const MoveStockType& data)
      {return "<type name=\"MoveStock\">"
               "<field name=\"group\" unit=\"\" kind=\"integer4\" />"
               "<field name=\"paddock\" unit=\"\" kind=\"string\" />"
               "</type>";}

   //------ ShearStock ------

   void pack(MessageData& messageData, const ShearStockType& data)
      {
      pack(messageData, data.group);
      pack(messageData, data.sub_group);
      }
   void unpack(MessageData& messageData, ShearStockType& data)
      {
      unpack(messageData, data.group);
      unpack(messageData, data.sub_group);
      }
   unsigned memorySize(const ShearStockType& data)
      {
      return 0
              + ::memorySize(data.group)
              + ::memorySize(data.sub_group)
              ;
      }
   std::string DDML(const ShearStockType& data)
      {return "<type name=\"ShearStock\">"
               "<field name=\"group\" unit=\"\" kind=\"integer4\" />"
               "<field name=\"sub_group\" unit=\"\" kind=\"string\" />"
               "</type>";}

   //------ SplitStock ------

   void pack(MessageData& messageData, const SplitStockType& data)
      {
      pack(messageData, data.group);
      pack(messageData, data.type);
      pack(messageData, data.value);
      }
   void unpack(MessageData& messageData, SplitStockType& data)
      {
      unpack(messageData, data.group);
      unpack(messageData, data.type);
      unpack(messageData, data.value);
      }
   unsigned memorySize(const SplitStockType& data)
      {
      return 0
              + ::memorySize(data.group)
              + ::memorySize(data.type)
              + ::memorySize(data.value)
              ;
      }
   std::string DDML(const SplitStockType& data)
      {return "<type name=\"SplitStock\">"
               "<field name=\"group\" unit=\"\" kind=\"integer4\" />"
               "<field name=\"type\" unit=\"\" kind=\"string\" />"
               "<field name=\"value\" unit=\"\" kind=\"double\" />"
               "</type>";}

   //------ TagStock ------

   void pack(MessageData& messageData, const TagStockType& data)
      {
      pack(messageData, data.group);
      pack(messageData, data.value);
      }
   void unpack(MessageData& messageData, TagStockType& data)
      {
      unpack(messageData, data.group);
      unpack(messageData, data.value);
      }
   unsigned memorySize(const TagStockType& data)
      {
      return 0
              + ::memorySize(data.group)
              + ::memorySize(data.value)
              ;
      }
   std::string DDML(const TagStockType& data)
      {return "<type name=\"TagStock\">"
               "<field name=\"group\" unit=\"\" kind=\"integer4\" />"
               "<field name=\"value\" unit=\"\" kind=\"integer4\" />"
               "</type>";}

   //------ WeanStock ------

   void pack(MessageData& messageData, const WeanStockType& data)
      {
      pack(messageData, data.group);
      pack(messageData, data.sex);
      pack(messageData, data.number);
      }
   void unpack(MessageData& messageData, WeanStockType& data)
      {
      unpack(messageData, data.group);
      unpack(messageData, data.sex);
      unpack(messageData, data.number);
      }
   unsigned memorySize(const WeanStockType& data)
      {
      return 0
              + ::memorySize(data.group)
              + ::memorySize(data.sex)
              + ::memorySize(data.number)
              ;
      }
   std::string DDML(const WeanStockType& data)
      {return "<type name=\"WeanStock\">"
               "<field name=\"group\" unit=\"\" kind=\"integer4\" />"
               "<field name=\"sex\" unit=\"\" kind=\"string\" />"
               "<field name=\"number\" unit=\"\" kind=\"integer4\" />"
               "</type>";}

   //------ dm ------

   void pack(MessageData& messageData, const dmType& data)
      {
      pack(messageData, data.pool);
      pack(messageData, data.part);
      pack(messageData, data.dlt);
      }
   void unpack(MessageData& messageData, dmType& data)
      {
      unpack(messageData, data.pool);
      unpack(messageData, data.part);
      unpack(messageData, data.dlt);
      }
   unsigned memorySize(const dmType& data)
      {
      return 0
              + ::memorySize(data.pool)
              + ::memorySize(data.part)
              + ::memorySize(data.dlt)
              ;
      }
   std::string DDML(const dmType& data)
      {return "<field name=\"dm\" array=\"T\">"
               "<element>"
               "<field name=\"pool\" kind=\"string\" />"
               "<field name=\"part\" kind=\"string\" array=\"T\" />"
               "<field name=\"dlt\" kind=\"double\" array=\"T\" />"
               "</element>"
               "</field>";}

   //------ RemoveCropDm ------

   void pack(MessageData& messageData, const RemoveCropDmType& data)
      {
      pack(messageData, data.dm);
      }
   void unpack(MessageData& messageData, RemoveCropDmType& data)
      {
      unpack(messageData, data.dm);
      }
   unsigned memorySize(const RemoveCropDmType& data)
      {
      return 0
              + ::memorySize(data.dm)
              ;
      }
   std::string DDML(const RemoveCropDmType& data)
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

   void pack(MessageData& messageData, const RemoveResidueDmType& data)
      {
      pack(messageData, data.dm_type);
      pack(messageData, data.dlt_residue_dm);
      }
   void unpack(MessageData& messageData, RemoveResidueDmType& data)
      {
      unpack(messageData, data.dm_type);
      unpack(messageData, data.dlt_residue_dm);
      }
   unsigned memorySize(const RemoveResidueDmType& data)
      {
      return 0
              + ::memorySize(data.dm_type)
              + ::memorySize(data.dlt_residue_dm)
              ;
      }
   std::string DDML(const RemoveResidueDmType& data)
      {return "<type name=\"RemoveResidueDm\">"
               "<field name=\"dm_type\" kind=\"string\" array=\"T\" />"
               "<field name=\"dlt_residue_dm\" kind=\"single\" array=\"T\" />"
               "</type>";}

   //------ SupplementBuy ------

   void pack(MessageData& messageData, const SupplementBuyType& data)
      {
      pack(messageData, data.supplement);
      pack(messageData, data.amount);
      }
   void unpack(MessageData& messageData, SupplementBuyType& data)
      {
      unpack(messageData, data.supplement);
      unpack(messageData, data.amount);
      }
   unsigned memorySize(const SupplementBuyType& data)
      {
      return 0
              + ::memorySize(data.supplement)
              + ::memorySize(data.amount)
              ;
      }
   std::string DDML(const SupplementBuyType& data)
      {return "<type name=\"SupplementBuy\">"
               "<field name=\"supplement\" kind=\"string\" unit=\"-\" />"
               "<field name=\"amount\" kind=\"double\" unit=\"kg\" />"
               "</type>";}

   //------ SupplementFeed ------

   void pack(MessageData& messageData, const SupplementFeedType& data)
      {
      pack(messageData, data.supplement);
      pack(messageData, data.amount);
      pack(messageData, data.paddock);
      }
   void unpack(MessageData& messageData, SupplementFeedType& data)
      {
      unpack(messageData, data.supplement);
      unpack(messageData, data.amount);
      unpack(messageData, data.paddock);
      }
   unsigned memorySize(const SupplementFeedType& data)
      {
      return 0
              + ::memorySize(data.supplement)
              + ::memorySize(data.amount)
              + ::memorySize(data.paddock)
              ;
      }
   std::string DDML(const SupplementFeedType& data)
      {return "<type name=\"SupplementFeed\">"
               "<field name=\"supplement\" kind=\"string\" unit=\"-\" />"
               "<field name=\"amount\" kind=\"double\" unit=\"kg\" />"
               "<field name=\"paddock\" kind=\"string\" unit=\"-\" />"
               "</type>";}

   //------ SupplementMix ------

   void pack(MessageData& messageData, const SupplementMixType& data)
      {
      pack(messageData, data.src_store);
      pack(messageData, data.amount);
      pack(messageData, data.dest_store);
      }
   void unpack(MessageData& messageData, SupplementMixType& data)
      {
      unpack(messageData, data.src_store);
      unpack(messageData, data.amount);
      unpack(messageData, data.dest_store);
      }
   unsigned memorySize(const SupplementMixType& data)
      {
      return 0
              + ::memorySize(data.src_store)
              + ::memorySize(data.amount)
              + ::memorySize(data.dest_store)
              ;
      }
   std::string DDML(const SupplementMixType& data)
      {return "<type name=\"SupplementMix\">"
               "<field name=\"src_store\" kind=\"string\" unit=\"-\" />"
               "<field name=\"amount\" kind=\"double\" unit=\"kg\" />"
               "<field name=\"dest_store\" kind=\"string\" unit=\"-\" />"
               "</type>";}


#ifndef DataTypesH
#define DataTypesH
#include <general/stl_functions.h>
#include <general/TreeNodeIterator.h>
#include <general/xml.h>
#include <ComponentInterface2/MessageData.h>
#include <ComponentInterface2/Interfaces.h>

   //------ Null ------
   struct Null
      {
      int i;
      };

   inline void pack(MessageData& messageData, const Null& data)
      { }
   inline void unpack(MessageData& messageData, Null& data)
      { }
   inline unsigned memorySize(const Null& data)
      {return 0;}
   std::string DDML(const Null& data)
      {return "<type/>";}


   //------ Complete ------
   struct Complete
      {
      int ackID;
      };

   inline void pack(MessageData& messageData, const Complete& data)
      {
      pack(messageData, data.ackID);
      }
   inline void unpack(MessageData& messageData, Complete& data)
      {
      unpack(messageData, data.ackID);
      }
   inline unsigned memorySize(const Complete& data)
      {
      return 0
              + ::memorySize(data.ackID)
              ;
      }
   std::string DDML(const Complete& data)
      {return "<type name=\"Complete\">"
               "<field name=\"ackID\" kind=\"integer4\" />"
               "</type>";}

   //------ Error ------
   struct Error
      {
      std::string msg;
      bool isFatal;
      };

   inline void pack(MessageData& messageData, const Error& data)
      {
      pack(messageData, data.msg);
      pack(messageData, data.isFatal);
      }
   inline void unpack(MessageData& messageData, Error& data)
      {
      unpack(messageData, data.msg);
      unpack(messageData, data.isFatal);
      }
   inline unsigned memorySize(const Error& data)
      {
      return 0
              + ::memorySize(data.msg)
              + ::memorySize(data.isFatal)
              ;
      }
   std::string DDML(const Error& data)
      {return "<type name=\"Error\">"
               "<field name=\"msg\" kind=\"string\" />"
               "<field name=\"isFatal\" kind=\"boolean\" />"
               "</type>";}

   //------ Event ------
   struct Event
      {
      int ID;
      int publishedBy;
      std::string ddml;
      };

   inline void pack(MessageData& messageData, const Event& data)
      {
      pack(messageData, data.ID);
      pack(messageData, data.publishedBy);
      pack(messageData, data.ddml);
      }
   inline void unpack(MessageData& messageData, Event& data)
      {
      unpack(messageData, data.ID);
      unpack(messageData, data.publishedBy);
      unpack(messageData, data.ddml);
      }
   inline unsigned memorySize(const Event& data)
      {
      return 0
              + ::memorySize(data.ID)
              + ::memorySize(data.publishedBy)
              + ::memorySize(data.ddml)
              ;
      }
   std::string DDML(const Event& data)
      {return "<type name=\"Event\">"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"publishedBy\" kind=\"integer4\" />"
               "<field name=\"ddml\" kind=\"string\" />"
               "</type>";}

   //------ GetValue ------
   struct GetValue
      {
      int ID;
      };

   inline void pack(MessageData& messageData, const GetValue& data)
      {
      pack(messageData, data.ID);
      }
   inline void unpack(MessageData& messageData, GetValue& data)
      {
      unpack(messageData, data.ID);
      }
   inline unsigned memorySize(const GetValue& data)
      {
      return 0
              + ::memorySize(data.ID)
              ;
      }
   std::string DDML(const GetValue& data)
      {return "<type name=\"GetValue\">"
               "<field name=\"ID\" kind=\"integer4\" />"
               "</type>";}

   //------ Init1 ------
   struct Init1
      {
      std::string sdml;
      std::string fqn;
      bool inStartup;
      };

   inline void pack(MessageData& messageData, const Init1& data)
      {
      pack(messageData, data.sdml);
      pack(messageData, data.fqn);
      pack(messageData, data.inStartup);
      }
   inline void unpack(MessageData& messageData, Init1& data)
      {
      unpack(messageData, data.sdml);
      unpack(messageData, data.fqn);
      unpack(messageData, data.inStartup);
      }
   inline unsigned memorySize(const Init1& data)
      {
      return 0
              + ::memorySize(data.sdml)
              + ::memorySize(data.fqn)
              + ::memorySize(data.inStartup)
              ;
      }
   std::string DDML(const Init1& data)
      {return "<type name=\"Init1\">"
               "<field name=\"sdml\" kind=\"string\" />"
               "<field name=\"fqn\" kind=\"string\" />"
               "<field name=\"inStartup\" kind=\"boolean\" />"
               "</type>";}

   //------ NotifySetValueSuccess ------
   struct NotifySetValueSuccess
      {
      int ID;
      bool success;
      };

   inline void pack(MessageData& messageData, const NotifySetValueSuccess& data)
      {
      pack(messageData, data.ID);
      pack(messageData, data.success);
      }
   inline void unpack(MessageData& messageData, NotifySetValueSuccess& data)
      {
      unpack(messageData, data.ID);
      unpack(messageData, data.success);
      }
   inline unsigned memorySize(const NotifySetValueSuccess& data)
      {
      return 0
              + ::memorySize(data.ID)
              + ::memorySize(data.success)
              ;
      }
   std::string DDML(const NotifySetValueSuccess& data)
      {return "<type name=\"NotifySetValueSuccess\">"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"success\" kind=\"boolean\" />"
               "</type>";}

   //------ PublishEvent ------
   struct PublishEvent
      {
      int ID;
      std::string ddml;
      };

   inline void pack(MessageData& messageData, const PublishEvent& data)
      {
      pack(messageData, data.ID);
      pack(messageData, data.ddml);
      }
   inline void unpack(MessageData& messageData, PublishEvent& data)
      {
      unpack(messageData, data.ID);
      unpack(messageData, data.ddml);
      }
   inline unsigned memorySize(const PublishEvent& data)
      {
      return 0
              + ::memorySize(data.ID)
              + ::memorySize(data.ddml)
              ;
      }
   std::string DDML(const PublishEvent& data)
      {return "<type name=\"PublishEvent\">"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"ddml\" kind=\"string\" />"
               "</type>";}

   //------ QueryInfo ------
   struct QueryInfo
      {
      std::string name;
      int kind;
      };

   inline void pack(MessageData& messageData, const QueryInfo& data)
      {
      pack(messageData, data.name);
      pack(messageData, data.kind);
      }
   inline void unpack(MessageData& messageData, QueryInfo& data)
      {
      unpack(messageData, data.name);
      unpack(messageData, data.kind);
      }
   inline unsigned memorySize(const QueryInfo& data)
      {
      return 0
              + ::memorySize(data.name)
              + ::memorySize(data.kind)
              ;
      }
   std::string DDML(const QueryInfo& data)
      {return "<type name=\"QueryInfo\">"
               "<field name=\"name\" kind=\"string\" />"
               "<field name=\"kind\" kind=\"integer4\" />"
               "</type>";}

   //------ Register ------
   struct Register
      {
      int kind;
      int ID;
      int destID;
      std::string name;
      std::string ddml;
      };

   inline void pack(MessageData& messageData, const Register& data)
      {
      pack(messageData, data.kind);
      pack(messageData, data.ID);
      pack(messageData, data.destID);
      pack(messageData, data.name);
      pack(messageData, data.ddml);
      }
   inline void unpack(MessageData& messageData, Register& data)
      {
      unpack(messageData, data.kind);
      unpack(messageData, data.ID);
      unpack(messageData, data.destID);
      unpack(messageData, data.name);
      unpack(messageData, data.ddml);
      }
   inline unsigned memorySize(const Register& data)
      {
      return 0
              + ::memorySize(data.kind)
              + ::memorySize(data.ID)
              + ::memorySize(data.destID)
              + ::memorySize(data.name)
              + ::memorySize(data.ddml)
              ;
      }
   std::string DDML(const Register& data)
      {return "<type name=\"Register\">"
               "<field name=\"kind\" kind=\"integer4\" />"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"destID\" kind=\"integer4\" />"
               "<field name=\"name\" kind=\"string\" />"
               "<field name=\"ddml\" kind=\"string\" />"
               "</type>";}

   //------ ReplyValue ------
   struct ReplyValue
      {
      int queryID;
      std::string ddml;
      };

   inline void pack(MessageData& messageData, const ReplyValue& data)
      {
      pack(messageData, data.queryID);
      pack(messageData, data.ddml);
      }
   inline void unpack(MessageData& messageData, ReplyValue& data)
      {
      unpack(messageData, data.queryID);
      unpack(messageData, data.ddml);
      }
   inline unsigned memorySize(const ReplyValue& data)
      {
      return 0
              + ::memorySize(data.queryID)
              + ::memorySize(data.ddml)
              ;
      }
   std::string DDML(const ReplyValue& data)
      {return "<type name=\"ReplyValue\">"
               "<field name=\"queryID\" kind=\"integer4\" />"
               "<field name=\"ddml\" kind=\"string\" />"
               "</type>";}

   //------ RequestSetValue ------
   struct RequestSetValue
      {
      int ID;
      std::string ddml;
      };

   inline void pack(MessageData& messageData, const RequestSetValue& data)
      {
      pack(messageData, data.ID);
      pack(messageData, data.ddml);
      }
   inline void unpack(MessageData& messageData, RequestSetValue& data)
      {
      unpack(messageData, data.ID);
      unpack(messageData, data.ddml);
      }
   inline unsigned memorySize(const RequestSetValue& data)
      {
      return 0
              + ::memorySize(data.ID)
              + ::memorySize(data.ddml)
              ;
      }
   std::string DDML(const RequestSetValue& data)
      {return "<type name=\"RequestSetValue\">"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"ddml\" kind=\"string\" />"
               "</type>";}

   //------ ReturnInfo ------
   struct ReturnInfo
      {
      int queryID;
      int compID;
      int ID;
      std::string name;
      std::string type;
      int kind;
      };

   inline void pack(MessageData& messageData, const ReturnInfo& data)
      {
      pack(messageData, data.queryID);
      pack(messageData, data.compID);
      pack(messageData, data.ID);
      pack(messageData, data.name);
      pack(messageData, data.type);
      pack(messageData, data.kind);
      }
   inline void unpack(MessageData& messageData, ReturnInfo& data)
      {
      unpack(messageData, data.queryID);
      unpack(messageData, data.compID);
      unpack(messageData, data.ID);
      unpack(messageData, data.name);
      unpack(messageData, data.type);
      unpack(messageData, data.kind);
      }
   inline unsigned memorySize(const ReturnInfo& data)
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
   std::string DDML(const ReturnInfo& data)
      {return "<type name=\"ReturnInfo\">"
               "<field name=\"queryID\" kind=\"integer4\" />"
               "<field name=\"compID\" kind=\"integer4\" />"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"name\" kind=\"string\" />"
               "<field name=\"type\" kind=\"string\" />"
               "<field name=\"kind\" kind=\"integer4\" />"
               "</type>";}

   //------ ReturnValue ------
   struct ReturnValue
      {
      int compID;
      int ID;
      std::string ddml;
      };

   inline void pack(MessageData& messageData, const ReturnValue& data)
      {
      pack(messageData, data.compID);
      pack(messageData, data.ID);
      pack(messageData, data.ddml);
      }
   inline void unpack(MessageData& messageData, ReturnValue& data)
      {
      unpack(messageData, data.compID);
      unpack(messageData, data.ID);
      unpack(messageData, data.ddml);
      }
   inline unsigned memorySize(const ReturnValue& data)
      {
      return 0
              + ::memorySize(data.compID)
              + ::memorySize(data.ID)
              + ::memorySize(data.ddml)
              ;
      }
   std::string DDML(const ReturnValue& data)
      {return "<type name=\"ReturnValue\">"
               "<field name=\"compID\" kind=\"integer4\" />"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"ddml\" kind=\"string\" />"
               "</type>";}

   //------ QueryValue ------
   struct QueryValue
      {
      int ID;
      int requestedByID;
      };

   inline void pack(MessageData& messageData, const QueryValue& data)
      {
      pack(messageData, data.ID);
      pack(messageData, data.requestedByID);
      }
   inline void unpack(MessageData& messageData, QueryValue& data)
      {
      unpack(messageData, data.ID);
      unpack(messageData, data.requestedByID);
      }
   inline unsigned memorySize(const QueryValue& data)
      {
      return 0
              + ::memorySize(data.ID)
              + ::memorySize(data.requestedByID)
              ;
      }
   std::string DDML(const QueryValue& data)
      {return "<type name=\"QueryValue\">"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"requestedByID\" kind=\"integer4\" />"
               "</type>";}

   //------ QuerySetValue ------
   struct QuerySetValue
      {
      int ID;
      std::string ddml;
      };

   inline void pack(MessageData& messageData, const QuerySetValue& data)
      {
      pack(messageData, data.ID);
      pack(messageData, data.ddml);
      }
   inline void unpack(MessageData& messageData, QuerySetValue& data)
      {
      unpack(messageData, data.ID);
      unpack(messageData, data.ddml);
      }
   inline unsigned memorySize(const QuerySetValue& data)
      {
      return 0
              + ::memorySize(data.ID)
              + ::memorySize(data.ddml)
              ;
      }
   std::string DDML(const QuerySetValue& data)
      {return "<type name=\"QuerySetValue\">"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"ddml\" kind=\"string\" />"
               "</type>";}

   //------ Layered ------
   struct Layered
      {
      std::vector<double> layer;
      std::vector<double> value;
      };

   inline void pack(MessageData& messageData, const Layered& data)
      {
      pack(messageData, data.layer);
      pack(messageData, data.value);
      }
   inline void unpack(MessageData& messageData, Layered& data)
      {
      unpack(messageData, data.layer);
      unpack(messageData, data.value);
      }
   inline unsigned memorySize(const Layered& data)
      {
      return 0
              + ::memorySize(data.layer)
              + ::memorySize(data.value)
              ;
      }
   std::string DDML(const Layered& data)
      {return "<type name=\"Layered\" description=\"Layered soil data\">"
               "<field name=\"layer\" kind=\"double\" array=\"T\" />"
               "<field name=\"value\" kind=\"double\" array=\"T\" />"
               "</type>";}

   //------ Time ------
   struct Time
      {
      int startday;
      int startsec;
      double startsecpart;
      int endday;
      int endsec;
      double endsecpart;
      };

   inline void pack(MessageData& messageData, const Time& data)
      {
      pack(messageData, data.startday);
      pack(messageData, data.startsec);
      pack(messageData, data.startsecpart);
      pack(messageData, data.endday);
      pack(messageData, data.endsec);
      pack(messageData, data.endsecpart);
      }
   inline void unpack(MessageData& messageData, Time& data)
      {
      unpack(messageData, data.startday);
      unpack(messageData, data.startsec);
      unpack(messageData, data.startsecpart);
      unpack(messageData, data.endday);
      unpack(messageData, data.endsec);
      unpack(messageData, data.endsecpart);
      }
   inline unsigned memorySize(const Time& data)
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
   std::string DDML(const Time& data)
      {return "<type name=\"Time\" description=\"Change in the simulation system time and the duration of the new time step\">"
               "<field name=\"startday\" kind=\"integer4\" description=\"Day number of the start of the timestep\" />"
               "<field name=\"startsec\" kind=\"integer4\" description=\"Seconds past midnight of the start of the timestep (0-86399)\" />"
               "<field name=\"startsecpart\" kind=\"double\" description=\"Fraction of a second of the start of the timestep (0-1)\" />"
               "<field name=\"endday\" kind=\"integer4\" description=\"Day number of the end of the timestep\" />"
               "<field name=\"endsec\" kind=\"integer4\" description=\"Seconds past midnight of the end of the timestep (0-86399)\" />"
               "<field name=\"endsecpart\" kind=\"double\" description=\"Fraction of a second of the end of the timestep (0-1)\" />"
               "</type>";}

   //------ NewMet ------
   struct NewMet
      {
      double today;
      float radn;
      float maxt;
      float mint;
      float rain;
      float vp;
      };

   inline void pack(MessageData& messageData, const NewMet& data)
      {
      pack(messageData, data.today);
      pack(messageData, data.radn);
      pack(messageData, data.maxt);
      pack(messageData, data.mint);
      pack(messageData, data.rain);
      pack(messageData, data.vp);
      }
   inline void unpack(MessageData& messageData, NewMet& data)
      {
      unpack(messageData, data.today);
      unpack(messageData, data.radn);
      unpack(messageData, data.maxt);
      unpack(messageData, data.mint);
      unpack(messageData, data.rain);
      unpack(messageData, data.vp);
      }
   inline unsigned memorySize(const NewMet& data)
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
   std::string DDML(const NewMet& data)
      {return "<type name=\"NewMet\">"
               "<field name=\"today\" kind=\"double\" />"
               "<field name=\"radn\" kind=\"single\" lower_bound=\"0.0\" upper_bound=\"50.0\" units=\"MJ/m2/d\" />"
               "<field name=\"maxt\" kind=\"single\" lower_bound=\"-10.0\" upper_bound=\"70.0\" units=\"oC\" />"
               "<field name=\"mint\" kind=\"single\" lower_bound=\"-20.0\" upper_bound=\"50.0\" units=\"oC\" />"
               "<field name=\"rain\" kind=\"single\" lower_bound=\"0.0\" upper_bound=\"1000.0\" units=\"mm/d\" />"
               "<field name=\"vp\" kind=\"single\" units=\"????\" />"
               "</type>";}

   //------ SoilWaterProfileLayer ------
   struct SoilWaterProfileLayer
      {
      float thickness;
      float BulkDensity;
      float SatDepth;
      float DULDepth;
      float LL15Depth;
      float AirDryDepth;
      float SWDepth;
      };

   inline void pack(MessageData& messageData, const SoilWaterProfileLayer& data)
      {
      pack(messageData, data.thickness);
      pack(messageData, data.BulkDensity);
      pack(messageData, data.SatDepth);
      pack(messageData, data.DULDepth);
      pack(messageData, data.LL15Depth);
      pack(messageData, data.AirDryDepth);
      pack(messageData, data.SWDepth);
      }
   inline void unpack(MessageData& messageData, SoilWaterProfileLayer& data)
      {
      unpack(messageData, data.thickness);
      unpack(messageData, data.BulkDensity);
      unpack(messageData, data.SatDepth);
      unpack(messageData, data.DULDepth);
      unpack(messageData, data.LL15Depth);
      unpack(messageData, data.AirDryDepth);
      unpack(messageData, data.SWDepth);
      }
   inline unsigned memorySize(const SoilWaterProfileLayer& data)
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
   std::string DDML(const SoilWaterProfileLayer& data)
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
   struct SoilWaterLayer
      {
      float thickness;
      float amount;
      };

   inline void pack(MessageData& messageData, const SoilWaterLayer& data)
      {
      pack(messageData, data.thickness);
      pack(messageData, data.amount);
      }
   inline void unpack(MessageData& messageData, SoilWaterLayer& data)
      {
      unpack(messageData, data.thickness);
      unpack(messageData, data.amount);
      }
   inline unsigned memorySize(const SoilWaterLayer& data)
      {
      return 0
              + ::memorySize(data.thickness)
              + ::memorySize(data.amount)
              ;
      }
   std::string DDML(const SoilWaterLayer& data)
      {return "<type name=\"SoilWaterLayer\" array=\"T\">"
               "<element>"
               "<field name=\"thickness\" kind=\"single\" units=\"mm\" />"
               "<field name=\"amount\" kind=\"single\" units=\"mm\" />"
               "</element>"
               "</type>";}

   //------ LateralFlowLayer ------
   struct LateralFlowLayer
      {
      float thickness;
      float amount;
      };

   inline void pack(MessageData& messageData, const LateralFlowLayer& data)
      {
      pack(messageData, data.thickness);
      pack(messageData, data.amount);
      }
   inline void unpack(MessageData& messageData, LateralFlowLayer& data)
      {
      unpack(messageData, data.thickness);
      unpack(messageData, data.amount);
      }
   inline unsigned memorySize(const LateralFlowLayer& data)
      {
      return 0
              + ::memorySize(data.thickness)
              + ::memorySize(data.amount)
              ;
      }
   std::string DDML(const LateralFlowLayer& data)
      {return "<field name=\"LateralFlowLayer\" array=\"T\">"
               "<element>"
               "<field name=\"thickness\" kind=\"single\" />"
               "<field name=\"amount\" kind=\"single\" />"
               "</element>"
               "</field>";}

   //------ SoilWaterBalance ------
   struct SoilWaterBalance
      {
      float infiltration;
      float drainage;
      float evaporation;
      std::vector<LateralFlowLayer> LateralFlowLayer;
      };

   inline void pack(MessageData& messageData, const SoilWaterBalance& data)
      {
      pack(messageData, data.infiltration);
      pack(messageData, data.drainage);
      pack(messageData, data.evaporation);
      pack(messageData, data.LateralFlowLayer);
      }
   inline void unpack(MessageData& messageData, SoilWaterBalance& data)
      {
      unpack(messageData, data.infiltration);
      unpack(messageData, data.drainage);
      unpack(messageData, data.evaporation);
      unpack(messageData, data.LateralFlowLayer);
      }
   inline unsigned memorySize(const SoilWaterBalance& data)
      {
      return 0
              + ::memorySize(data.infiltration)
              + ::memorySize(data.drainage)
              + ::memorySize(data.evaporation)
              + ::memorySize(data.LateralFlowLayer)
              ;
      }
   std::string DDML(const SoilWaterBalance& data)
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
   struct NewSolute
      {
      std::string sender_name;
      int sender_numbytes;
      int sender_code;
      bool sender_isarray;
      std::string sender_value;
      std::string sender_id_name;
      int sender_id_numbytes;
      int sender_id_code;
      bool sender_id_isarray;
      int sender_id_value;
      std::string solute_names_name;
      int solute_names_numbytes;
      int solute_names_code;
      bool solute_names_isarray;
      std::vector<std::string> solute_names_value;
      };

   inline void pack(MessageData& messageData, const NewSolute& data)
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
   inline void unpack(MessageData& messageData, NewSolute& data)
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
   inline unsigned memorySize(const NewSolute& data)
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
   std::string DDML(const NewSolute& data)
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
   struct layer
      {
      float thickness;
      float amount;
      };

   inline void pack(MessageData& messageData, const layer& data)
      {
      pack(messageData, data.thickness);
      pack(messageData, data.amount);
      }
   inline void unpack(MessageData& messageData, layer& data)
      {
      unpack(messageData, data.thickness);
      unpack(messageData, data.amount);
      }
   inline unsigned memorySize(const layer& data)
      {
      return 0
              + ::memorySize(data.thickness)
              + ::memorySize(data.amount)
              ;
      }
   std::string DDML(const layer& data)
      {return "<field name=\"layer\" array=\"T\">"
               "<element>"
               "<field name=\"thickness\" kind=\"single\" />"
               "<field name=\"amount\" kind=\"single\" />"
               "</element>"
               "</field>";}

   //------ SoluteProfile ------
   struct SoluteProfile
      {
      std::string name;
      std::vector<layer> layer;
      };

   inline void pack(MessageData& messageData, const SoluteProfile& data)
      {
      pack(messageData, data.name);
      pack(messageData, data.layer);
      }
   inline void unpack(MessageData& messageData, SoluteProfile& data)
      {
      unpack(messageData, data.name);
      unpack(messageData, data.layer);
      }
   inline unsigned memorySize(const SoluteProfile& data)
      {
      return 0
              + ::memorySize(data.name)
              + ::memorySize(data.layer)
              ;
      }
   std::string DDML(const SoluteProfile& data)
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

   //------ solute ------
   struct solute
      {
      std::string name;
      float amount;
      };

   inline void pack(MessageData& messageData, const solute& data)
      {
      pack(messageData, data.name);
      pack(messageData, data.amount);
      }
   inline void unpack(MessageData& messageData, solute& data)
      {
      unpack(messageData, data.name);
      unpack(messageData, data.amount);
      }
   inline unsigned memorySize(const solute& data)
      {
      return 0
              + ::memorySize(data.name)
              + ::memorySize(data.amount)
              ;
      }
   std::string DDML(const solute& data)
      {return "<field name=\"solute\" array=\"T\">"
               "<element>"
               "<field name=\"name\" kind=\"string\" />"
               "<field name=\"amount\" kind=\"single\" />"
               "</element>"
               "</field>";}

   //------ Irrigated ------
   struct Irrigated
      {
      double amount;
      double depth;
      std::vector<solute> solute;
      };

   inline void pack(MessageData& messageData, const Irrigated& data)
      {
      pack(messageData, data.amount);
      pack(messageData, data.depth);
      pack(messageData, data.solute);
      }
   inline void unpack(MessageData& messageData, Irrigated& data)
      {
      unpack(messageData, data.amount);
      unpack(messageData, data.depth);
      unpack(messageData, data.solute);
      }
   inline unsigned memorySize(const Irrigated& data)
      {
      return 0
              + ::memorySize(data.amount)
              + ::memorySize(data.depth)
              + ::memorySize(data.solute)
              ;
      }
   std::string DDML(const Irrigated& data)
      {return "<type name=\"Irrigated\" description=\"Sent when an irrigation occurs\">"
               "<field name=\"amount\" kind=\"double\" unit=\"mm\" description=\"Amount of irrigation applied\" />"
               "<field name=\"depth\" kind=\"double\" unit=\"mm\" description=\"Depth of irrigation applied\" />"
               "<field name=\"solute\" array=\"T\">"
               "<element>"
               "<field name=\"name\" kind=\"string\" />"
               "<field name=\"amount\" kind=\"single\" />"
               "</element>"
               "</field>"
               "</type>";}

   //------ CropWaterSupply ------
   struct CropWaterSupply
      {
      std::string name;
      std::vector<layer> layer;
      };

   inline void pack(MessageData& messageData, const CropWaterSupply& data)
      {
      pack(messageData, data.name);
      pack(messageData, data.layer);
      }
   inline void unpack(MessageData& messageData, CropWaterSupply& data)
      {
      unpack(messageData, data.name);
      unpack(messageData, data.layer);
      }
   inline unsigned memorySize(const CropWaterSupply& data)
      {
      return 0
              + ::memorySize(data.name)
              + ::memorySize(data.layer)
              ;
      }
   std::string DDML(const CropWaterSupply& data)
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
   struct RootLayer
      {
      float thickness;
      float RootLengthDensity;
      float PotentialUptake;
      };

   inline void pack(MessageData& messageData, const RootLayer& data)
      {
      pack(messageData, data.thickness);
      pack(messageData, data.RootLengthDensity);
      pack(messageData, data.PotentialUptake);
      }
   inline void unpack(MessageData& messageData, RootLayer& data)
      {
      unpack(messageData, data.thickness);
      unpack(messageData, data.RootLengthDensity);
      unpack(messageData, data.PotentialUptake);
      }
   inline unsigned memorySize(const RootLayer& data)
      {
      return 0
              + ::memorySize(data.thickness)
              + ::memorySize(data.RootLengthDensity)
              + ::memorySize(data.PotentialUptake)
              ;
      }
   std::string DDML(const RootLayer& data)
      {return "<field name=\"RootLayer\" array=\"T\">"
               "<element>"
               "<field name=\"thickness\" kind=\"single\" units=\"mm\" />"
               "<field name=\"RootLengthDensity\" kind=\"single\" units=\"mm/mm3\" />"
               "<field name=\"PotentialUptake\" kind=\"single\" units=\"mm\" />"
               "</element>"
               "</field>";}

   //------ CropWaterDemand ------
   struct CropWaterDemand
      {
      std::string Name;
      std::string CropType;
      std::vector<RootLayer> RootLayer;
      float amount;
      };

   inline void pack(MessageData& messageData, const CropWaterDemand& data)
      {
      pack(messageData, data.Name);
      pack(messageData, data.CropType);
      pack(messageData, data.RootLayer);
      pack(messageData, data.amount);
      }
   inline void unpack(MessageData& messageData, CropWaterDemand& data)
      {
      unpack(messageData, data.Name);
      unpack(messageData, data.CropType);
      unpack(messageData, data.RootLayer);
      unpack(messageData, data.amount);
      }
   inline unsigned memorySize(const CropWaterDemand& data)
      {
      return 0
              + ::memorySize(data.Name)
              + ::memorySize(data.CropType)
              + ::memorySize(data.RootLayer)
              + ::memorySize(data.amount)
              ;
      }
   std::string DDML(const CropWaterDemand& data)
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
   struct CropNitrogenDemand
      {
      std::string Name;
      std::string CropType;
      std::vector<RootLayer> RootLayer;
      float amount;
      };

   inline void pack(MessageData& messageData, const CropNitrogenDemand& data)
      {
      pack(messageData, data.Name);
      pack(messageData, data.CropType);
      pack(messageData, data.RootLayer);
      pack(messageData, data.amount);
      }
   inline void unpack(MessageData& messageData, CropNitrogenDemand& data)
      {
      unpack(messageData, data.Name);
      unpack(messageData, data.CropType);
      unpack(messageData, data.RootLayer);
      unpack(messageData, data.amount);
      }
   inline unsigned memorySize(const CropNitrogenDemand& data)
      {
      return 0
              + ::memorySize(data.Name)
              + ::memorySize(data.CropType)
              + ::memorySize(data.RootLayer)
              + ::memorySize(data.amount)
              ;
      }
   std::string DDML(const CropNitrogenDemand& data)
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
   struct CropNitrogenSupply
      {
      std::string name;
      std::vector<layer> layer;
      };

   inline void pack(MessageData& messageData, const CropNitrogenSupply& data)
      {
      pack(messageData, data.name);
      pack(messageData, data.layer);
      }
   inline void unpack(MessageData& messageData, CropNitrogenSupply& data)
      {
      unpack(messageData, data.name);
      unpack(messageData, data.layer);
      }
   inline unsigned memorySize(const CropNitrogenSupply& data)
      {
      return 0
              + ::memorySize(data.name)
              + ::memorySize(data.layer)
              ;
      }
   std::string DDML(const CropNitrogenSupply& data)
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
   struct Interception
      {
      std::string name;
      std::string CropType;
      std::vector<layer> layer;
      };

   inline void pack(MessageData& messageData, const Interception& data)
      {
      pack(messageData, data.name);
      pack(messageData, data.CropType);
      pack(messageData, data.layer);
      }
   inline void unpack(MessageData& messageData, Interception& data)
      {
      unpack(messageData, data.name);
      unpack(messageData, data.CropType);
      unpack(messageData, data.layer);
      }
   inline unsigned memorySize(const Interception& data)
      {
      return 0
              + ::memorySize(data.name)
              + ::memorySize(data.CropType)
              + ::memorySize(data.layer)
              ;
      }
   std::string DDML(const Interception& data)
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
   struct LightProfile
      {
      std::vector<Interception> Interception;
      float transmission;
      };

   inline void pack(MessageData& messageData, const LightProfile& data)
      {
      pack(messageData, data.Interception);
      pack(messageData, data.transmission);
      }
   inline void unpack(MessageData& messageData, LightProfile& data)
      {
      unpack(messageData, data.Interception);
      unpack(messageData, data.transmission);
      }
   inline unsigned memorySize(const LightProfile& data)
      {
      return 0
              + ::memorySize(data.Interception)
              + ::memorySize(data.transmission)
              ;
      }
   std::string DDML(const LightProfile& data)
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
   struct Canopy
      {
      std::string name;
      std::string CropType;
      float PotentialEp;
      };

   inline void pack(MessageData& messageData, const Canopy& data)
      {
      pack(messageData, data.name);
      pack(messageData, data.CropType);
      pack(messageData, data.PotentialEp);
      }
   inline void unpack(MessageData& messageData, Canopy& data)
      {
      unpack(messageData, data.name);
      unpack(messageData, data.CropType);
      unpack(messageData, data.PotentialEp);
      }
   inline unsigned memorySize(const Canopy& data)
      {
      return 0
              + ::memorySize(data.name)
              + ::memorySize(data.CropType)
              + ::memorySize(data.PotentialEp)
              ;
      }
   std::string DDML(const Canopy& data)
      {return "<field name=\"Canopy\" array=\"T\">"
               "<element>"
               "<field name=\"name\" kind=\"string\" />"
               "<field name=\"CropType\" kind=\"string\" />"
               "<field name=\"PotentialEp\" kind=\"single\" />"
               "</element>"
               "</field>";}

   //------ CanopyWaterBalance ------
   struct CanopyWaterBalance
      {
      std::vector<Canopy> Canopy;
      float eo;
      float interception;
      };

   inline void pack(MessageData& messageData, const CanopyWaterBalance& data)
      {
      pack(messageData, data.Canopy);
      pack(messageData, data.eo);
      pack(messageData, data.interception);
      }
   inline void unpack(MessageData& messageData, CanopyWaterBalance& data)
      {
      unpack(messageData, data.Canopy);
      unpack(messageData, data.eo);
      unpack(messageData, data.interception);
      }
   inline unsigned memorySize(const CanopyWaterBalance& data)
      {
      return 0
              + ::memorySize(data.Canopy)
              + ::memorySize(data.eo)
              + ::memorySize(data.interception)
              ;
      }
   std::string DDML(const CanopyWaterBalance& data)
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
   struct OrganicMatterFraction
      {
      float amount;
      float C;
      float N;
      float P;
      float AshAlk;
      };

   inline void pack(MessageData& messageData, const OrganicMatterFraction& data)
      {
      pack(messageData, data.amount);
      pack(messageData, data.C);
      pack(messageData, data.N);
      pack(messageData, data.P);
      pack(messageData, data.AshAlk);
      }
   inline void unpack(MessageData& messageData, OrganicMatterFraction& data)
      {
      unpack(messageData, data.amount);
      unpack(messageData, data.C);
      unpack(messageData, data.N);
      unpack(messageData, data.P);
      unpack(messageData, data.AshAlk);
      }
   inline unsigned memorySize(const OrganicMatterFraction& data)
      {
      return 0
              + ::memorySize(data.amount)
              + ::memorySize(data.C)
              + ::memorySize(data.N)
              + ::memorySize(data.P)
              + ::memorySize(data.AshAlk)
              ;
      }
   std::string DDML(const OrganicMatterFraction& data)
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
   struct Residue
      {
      std::string name;
      std::string OrganicMatterType;
      std::vector<OrganicMatterFraction> OrganicMatterFraction;
      float Cover;
      };

   inline void pack(MessageData& messageData, const Residue& data)
      {
      pack(messageData, data.name);
      pack(messageData, data.OrganicMatterType);
      pack(messageData, data.OrganicMatterFraction);
      pack(messageData, data.Cover);
      }
   inline void unpack(MessageData& messageData, Residue& data)
      {
      unpack(messageData, data.name);
      unpack(messageData, data.OrganicMatterType);
      unpack(messageData, data.OrganicMatterFraction);
      unpack(messageData, data.Cover);
      }
   inline unsigned memorySize(const Residue& data)
      {
      return 0
              + ::memorySize(data.name)
              + ::memorySize(data.OrganicMatterType)
              + ::memorySize(data.OrganicMatterFraction)
              + ::memorySize(data.Cover)
              ;
      }
   std::string DDML(const Residue& data)
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

   //------ SurfaceWater ------
   struct SurfaceWater
      {
      float amount;
      std::vector<solute> solute;
      };

   inline void pack(MessageData& messageData, const SurfaceWater& data)
      {
      pack(messageData, data.amount);
      pack(messageData, data.solute);
      }
   inline void unpack(MessageData& messageData, SurfaceWater& data)
      {
      unpack(messageData, data.amount);
      unpack(messageData, data.solute);
      }
   inline unsigned memorySize(const SurfaceWater& data)
      {
      return 0
              + ::memorySize(data.amount)
              + ::memorySize(data.solute)
              ;
      }
   std::string DDML(const SurfaceWater& data)
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
   struct SurfaceWaterBalance
      {
      float runoff;
      float evaporation;
      float runon;
      float WaterInput;
      };

   inline void pack(MessageData& messageData, const SurfaceWaterBalance& data)
      {
      pack(messageData, data.runoff);
      pack(messageData, data.evaporation);
      pack(messageData, data.runon);
      pack(messageData, data.WaterInput);
      }
   inline void unpack(MessageData& messageData, SurfaceWaterBalance& data)
      {
      unpack(messageData, data.runoff);
      unpack(messageData, data.evaporation);
      unpack(messageData, data.runon);
      unpack(messageData, data.WaterInput);
      }
   inline unsigned memorySize(const SurfaceWaterBalance& data)
      {
      return 0
              + ::memorySize(data.runoff)
              + ::memorySize(data.evaporation)
              + ::memorySize(data.runon)
              + ::memorySize(data.WaterInput)
              ;
      }
   std::string DDML(const SurfaceWaterBalance& data)
      {return "<type name=\"SurfaceWaterBalance\">"
               "<field name=\"runoff\" kind=\"single\" />"
               "<field name=\"evaporation\" kind=\"single\" />"
               "<field name=\"runon\" kind=\"single\" />"
               "<field name=\"WaterInput\" kind=\"single\" />"
               "</type>";}

   //------ FertiliserConstituents ------
   struct FertiliserConstituents
      {
      std::string name;
      float SurfaceAmount;
      std::vector<layer> layer;
      };

   inline void pack(MessageData& messageData, const FertiliserConstituents& data)
      {
      pack(messageData, data.name);
      pack(messageData, data.SurfaceAmount);
      pack(messageData, data.layer);
      }
   inline void unpack(MessageData& messageData, FertiliserConstituents& data)
      {
      unpack(messageData, data.name);
      unpack(messageData, data.SurfaceAmount);
      unpack(messageData, data.layer);
      }
   inline unsigned memorySize(const FertiliserConstituents& data)
      {
      return 0
              + ::memorySize(data.name)
              + ::memorySize(data.SurfaceAmount)
              + ::memorySize(data.layer)
              ;
      }
   std::string DDML(const FertiliserConstituents& data)
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
   struct FPool
      {
      float C;
      float N;
      float P;
      float AshAlk;
      };

   inline void pack(MessageData& messageData, const FPool& data)
      {
      pack(messageData, data.C);
      pack(messageData, data.N);
      pack(messageData, data.P);
      pack(messageData, data.AshAlk);
      }
   inline void unpack(MessageData& messageData, FPool& data)
      {
      unpack(messageData, data.C);
      unpack(messageData, data.N);
      unpack(messageData, data.P);
      unpack(messageData, data.AshAlk);
      }
   inline unsigned memorySize(const FPool& data)
      {
      return 0
              + ::memorySize(data.C)
              + ::memorySize(data.N)
              + ::memorySize(data.P)
              + ::memorySize(data.AshAlk)
              ;
      }
   std::string DDML(const FPool& data)
      {return "<field name=\"FPool\" array=\"T\">"
               "<element>"
               "<field name=\"C\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"N\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"P\" kind=\"single\" units=\"kg/ha\" />"
               "<field name=\"AshAlk\" kind=\"single\" units=\"kg/ha\" />"
               "</element>"
               "</field>";}

   //------ FPoolProfileLayer ------
   struct FPoolProfileLayer
      {
      float thickness;
      float no3;
      float nh4;
      float po4;
      std::vector<FPool> FPool;
      };

   inline void pack(MessageData& messageData, const FPoolProfileLayer& data)
      {
      pack(messageData, data.thickness);
      pack(messageData, data.no3);
      pack(messageData, data.nh4);
      pack(messageData, data.po4);
      pack(messageData, data.FPool);
      }
   inline void unpack(MessageData& messageData, FPoolProfileLayer& data)
      {
      unpack(messageData, data.thickness);
      unpack(messageData, data.no3);
      unpack(messageData, data.nh4);
      unpack(messageData, data.po4);
      unpack(messageData, data.FPool);
      }
   inline unsigned memorySize(const FPoolProfileLayer& data)
      {
      return 0
              + ::memorySize(data.thickness)
              + ::memorySize(data.no3)
              + ::memorySize(data.nh4)
              + ::memorySize(data.po4)
              + ::memorySize(data.FPool)
              ;
      }
   std::string DDML(const FPoolProfileLayer& data)
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
   struct StandingFraction
      {
      float amount;
      float C;
      float N;
      float P;
      float AshAlk;
      };

   inline void pack(MessageData& messageData, const StandingFraction& data)
      {
      pack(messageData, data.amount);
      pack(messageData, data.C);
      pack(messageData, data.N);
      pack(messageData, data.P);
      pack(messageData, data.AshAlk);
      }
   inline void unpack(MessageData& messageData, StandingFraction& data)
      {
      unpack(messageData, data.amount);
      unpack(messageData, data.C);
      unpack(messageData, data.N);
      unpack(messageData, data.P);
      unpack(messageData, data.AshAlk);
      }
   inline unsigned memorySize(const StandingFraction& data)
      {
      return 0
              + ::memorySize(data.amount)
              + ::memorySize(data.C)
              + ::memorySize(data.N)
              + ::memorySize(data.P)
              + ::memorySize(data.AshAlk)
              ;
      }
   std::string DDML(const StandingFraction& data)
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
   struct LyingFraction
      {
      float amount;
      float C;
      float N;
      float P;
      float AshAlk;
      };

   inline void pack(MessageData& messageData, const LyingFraction& data)
      {
      pack(messageData, data.amount);
      pack(messageData, data.C);
      pack(messageData, data.N);
      pack(messageData, data.P);
      pack(messageData, data.AshAlk);
      }
   inline void unpack(MessageData& messageData, LyingFraction& data)
      {
      unpack(messageData, data.amount);
      unpack(messageData, data.C);
      unpack(messageData, data.N);
      unpack(messageData, data.P);
      unpack(messageData, data.AshAlk);
      }
   inline unsigned memorySize(const LyingFraction& data)
      {
      return 0
              + ::memorySize(data.amount)
              + ::memorySize(data.C)
              + ::memorySize(data.N)
              + ::memorySize(data.P)
              + ::memorySize(data.AshAlk)
              ;
      }
   std::string DDML(const LyingFraction& data)
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
   struct SurfaceOrganicMatter
      {
      std::string Name;
      std::string OrganicMatterType;
      float PotDecompRate;
      float no3;
      float nh4;
      float po4;
      std::vector<StandingFraction> StandingFraction;
      std::vector<LyingFraction> LyingFraction;
      };

   inline void pack(MessageData& messageData, const SurfaceOrganicMatter& data)
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
   inline void unpack(MessageData& messageData, SurfaceOrganicMatter& data)
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
   inline unsigned memorySize(const SurfaceOrganicMatter& data)
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
   std::string DDML(const SurfaceOrganicMatter& data)
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
   struct SurfaceOrganicMatterDecomp
      {
      std::string Name;
      std::string OrganicMatterType;
      float amount;
      float C;
      float N;
      float P;
      float AshAlk;
      };

   inline void pack(MessageData& messageData, const SurfaceOrganicMatterDecomp& data)
      {
      pack(messageData, data.Name);
      pack(messageData, data.OrganicMatterType);
      pack(messageData, data.amount);
      pack(messageData, data.C);
      pack(messageData, data.N);
      pack(messageData, data.P);
      pack(messageData, data.AshAlk);
      }
   inline void unpack(MessageData& messageData, SurfaceOrganicMatterDecomp& data)
      {
      unpack(messageData, data.Name);
      unpack(messageData, data.OrganicMatterType);
      unpack(messageData, data.amount);
      unpack(messageData, data.C);
      unpack(messageData, data.N);
      unpack(messageData, data.P);
      unpack(messageData, data.AshAlk);
      }
   inline unsigned memorySize(const SurfaceOrganicMatterDecomp& data)
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
   std::string DDML(const SurfaceOrganicMatterDecomp& data)
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
   struct NBalance
      {
      std::string sender_name;
      int sender_numbytes;
      int sender_code;
      bool sender_isarray;
      std::string sender_value;
      std::string sender_id_name;
      int sender_id_numbytes;
      int sender_id_code;
      bool sender_id_isarray;
      int sender_id_value;
      std::string nh4_transform_net_name;
      int nh4_transform_net_numbytes;
      int nh4_transform_net_code;
      bool nh4_transform_net_isarray;
      std::vector<float> nh4_transform_net_value;
      std::string no3_transform_net_name;
      int no3_transform_net_numbytes;
      int no3_transform_net_code;
      bool no3_transform_net_isarray;
      std::vector<float> no3_transform_net_value;
      std::string dlt_nh4_net_name;
      int dlt_nh4_net_numbytes;
      int dlt_nh4_net_code;
      bool dlt_nh4_net_isarray;
      std::vector<float> dlt_nh4_net_value;
      std::string dlt_no3_net_name;
      int dlt_no3_net_numbytes;
      int dlt_no3_net_code;
      bool dlt_no3_net_isarray;
      std::vector<float> dlt_no3_net_value;
      };

   inline void pack(MessageData& messageData, const NBalance& data)
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
   inline void unpack(MessageData& messageData, NBalance& data)
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
   inline unsigned memorySize(const NBalance& data)
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
   std::string DDML(const NBalance& data)
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
   struct CBalance
      {
      std::string sender_name;
      int sender_numbytes;
      int sender_code;
      bool sender_isarray;
      std::string sender_value;
      std::string sender_id_name;
      int sender_id_numbytes;
      int sender_id_code;
      bool sender_id_isarray;
      int sender_id_value;
      std::string dlt_oc_name;
      int dlt_oc_numbytes;
      int dlt_oc_code;
      bool dlt_oc_isarray;
      std::vector<float> dlt_oc_value;
      std::string dlt_om_name;
      int dlt_om_numbytes;
      int dlt_om_code;
      bool dlt_om_isarray;
      std::vector<float> dlt_om_value;
      };

   inline void pack(MessageData& messageData, const CBalance& data)
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
   inline void unpack(MessageData& messageData, CBalance& data)
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
   inline unsigned memorySize(const CBalance& data)
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
   std::string DDML(const CBalance& data)
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
   struct IncorpFom
      {
      std::string dlt_fom_type_name;
      int dlt_fom_type_numbytes;
      int dlt_fom_type_code;
      bool dlt_fom_type_isarray;
      std::string dlt_fom_type_value;
      std::string dlt_fom_wt_name;
      int dlt_fom_wt_numbytes;
      int dlt_fom_wt_code;
      bool dlt_fom_wt_isarray;
      std::vector<float> dlt_fom_wt_value;
      std::string dlt_fom_n_name;
      int dlt_fom_n_numbytes;
      int dlt_fom_n_code;
      bool dlt_fom_n_isarray;
      std::vector<float> dlt_fom_n_value;
      std::string dlt_fom_p_name;
      int dlt_fom_p_numbytes;
      int dlt_fom_p_code;
      bool dlt_fom_p_isarray;
      std::vector<float> dlt_fom_p_value;
      };

   inline void pack(MessageData& messageData, const IncorpFom& data)
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
   inline void unpack(MessageData& messageData, IncorpFom& data)
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
   inline unsigned memorySize(const IncorpFom& data)
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
   std::string DDML(const IncorpFom& data)
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
   struct SoilOrganicMatter
      {
      std::string OrganicMatterType;
      std::vector<layer> layer;
      };

   inline void pack(MessageData& messageData, const SoilOrganicMatter& data)
      {
      pack(messageData, data.OrganicMatterType);
      pack(messageData, data.layer);
      }
   inline void unpack(MessageData& messageData, SoilOrganicMatter& data)
      {
      unpack(messageData, data.OrganicMatterType);
      unpack(messageData, data.layer);
      }
   inline unsigned memorySize(const SoilOrganicMatter& data)
      {
      return 0
              + ::memorySize(data.OrganicMatterType)
              + ::memorySize(data.layer)
              ;
      }
   std::string DDML(const SoilOrganicMatter& data)
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
   struct SoilNitrogenFlowsLayer
      {
      float nh4_transform_net;
      float no3_transform_net;
      float dlt_nh4_net;
      float dlt_no3_net;
      float dlt_oc;
      float dlt_om;
      std::vector<float> dlt_fomc_hum;
      std::vector<float> dlt_fomc_biom;
      std::vector<float> dlt_fomc_atm;
      float dlt_humc_biom;
      float dlt_humc_atm;
      float dlt_biomc_hum;
      float dlt_biomc_atm;
      std::vector<float> dlt_fomc_pool;
      };

   inline void pack(MessageData& messageData, const SoilNitrogenFlowsLayer& data)
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
   inline void unpack(MessageData& messageData, SoilNitrogenFlowsLayer& data)
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
   inline unsigned memorySize(const SoilNitrogenFlowsLayer& data)
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
   std::string DDML(const SoilNitrogenFlowsLayer& data)
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
   struct NitrogenBalance
      {
      float Fertilizer;
      float ResiduesMineralised;
      float Leaching;
      float Denitrification;
      float Uptake;
      float Erosion;
      float LateralFlow;
      };

   inline void pack(MessageData& messageData, const NitrogenBalance& data)
      {
      pack(messageData, data.Fertilizer);
      pack(messageData, data.ResiduesMineralised);
      pack(messageData, data.Leaching);
      pack(messageData, data.Denitrification);
      pack(messageData, data.Uptake);
      pack(messageData, data.Erosion);
      pack(messageData, data.LateralFlow);
      }
   inline void unpack(MessageData& messageData, NitrogenBalance& data)
      {
      unpack(messageData, data.Fertilizer);
      unpack(messageData, data.ResiduesMineralised);
      unpack(messageData, data.Leaching);
      unpack(messageData, data.Denitrification);
      unpack(messageData, data.Uptake);
      unpack(messageData, data.Erosion);
      unpack(messageData, data.LateralFlow);
      }
   inline unsigned memorySize(const NitrogenBalance& data)
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
   std::string DDML(const NitrogenBalance& data)
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
   struct CropChopped
      {
      std::string crop_type;
      std::vector<std::string> dm_type;
      std::vector<float> dlt_crop_dm;
      std::vector<float> dlt_dm_n;
      std::vector<float> fraction_to_residue;
      };

   inline void pack(MessageData& messageData, const CropChopped& data)
      {
      pack(messageData, data.crop_type);
      pack(messageData, data.dm_type);
      pack(messageData, data.dlt_crop_dm);
      pack(messageData, data.dlt_dm_n);
      pack(messageData, data.fraction_to_residue);
      }
   inline void unpack(MessageData& messageData, CropChopped& data)
      {
      unpack(messageData, data.crop_type);
      unpack(messageData, data.dm_type);
      unpack(messageData, data.dlt_crop_dm);
      unpack(messageData, data.dlt_dm_n);
      unpack(messageData, data.fraction_to_residue);
      }
   inline unsigned memorySize(const CropChopped& data)
      {
      return 0
              + ::memorySize(data.crop_type)
              + ::memorySize(data.dm_type)
              + ::memorySize(data.dlt_crop_dm)
              + ::memorySize(data.dlt_dm_n)
              + ::memorySize(data.fraction_to_residue)
              ;
      }
   std::string DDML(const CropChopped& data)
      {return "<type name=\"CropChopped\">"
               "<field name=\"crop_type\" kind=\"string\" />"
               "<field name=\"dm_type\" kind=\"string\" array=\"T\" />"
               "<field name=\"dlt_crop_dm\" kind=\"single\" array=\"T\" />"
               "<field name=\"dlt_dm_n\" kind=\"single\" array=\"T\" />"
               "<field name=\"fraction_to_residue\" kind=\"single\" array=\"T\" />"
               "</type>";}

   //------ NewProfile ------
   struct NewProfile
      {
      std::string sender_name;
      int sender_numbytes;
      int sender_code;
      bool sender_isarray;
      std::string sender_value;
      std::string sender_id_name;
      int sender_id_numbytes;
      int sender_id_code;
      bool sender_id_isarray;
      int sender_id_value;
      std::string dlayer_name;
      int dlayer_numbytes;
      int dlayer_code;
      bool dlayer_isarray;
      std::vector<float> dlayer_value;
      std::string air_dry_dep_name;
      int air_dry_dep_numbytes;
      int air_dry_dep_code;
      bool air_dry_dep_isarray;
      std::vector<float> air_dry_dep_value;
      std::string ll15_dep_name;
      int ll15_dep_numbytes;
      int ll15_dep_code;
      bool ll15_dep_isarray;
      std::vector<float> ll15_dep_value;
      std::string dul_dep_name;
      int dul_dep_numbytes;
      int dul_dep_code;
      bool dul_dep_isarray;
      std::vector<float> dul_dep_value;
      std::string sat_dep_name;
      int sat_dep_numbytes;
      int sat_dep_code;
      bool sat_dep_isarray;
      std::vector<float> sat_dep_value;
      std::string sw_dep_name;
      int sw_dep_numbytes;
      int sw_dep_code;
      bool sw_dep_isarray;
      std::vector<float> sw_dep_value;
      std::string bd_name;
      int bd_numbytes;
      int bd_code;
      bool bd_isarray;
      std::vector<float> bd_value;
      };

   inline void pack(MessageData& messageData, const NewProfile& data)
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
   inline void unpack(MessageData& messageData, NewProfile& data)
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
   inline unsigned memorySize(const NewProfile& data)
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
   std::string DDML(const NewProfile& data)
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
   struct NewPotentialGrowth
      {
      std::string sender;
      float frgr;
      };

   inline void pack(MessageData& messageData, const NewPotentialGrowth& data)
      {
      pack(messageData, data.sender);
      pack(messageData, data.frgr);
      }
   inline void unpack(MessageData& messageData, NewPotentialGrowth& data)
      {
      unpack(messageData, data.sender);
      unpack(messageData, data.frgr);
      }
   inline unsigned memorySize(const NewPotentialGrowth& data)
      {
      return 0
              + ::memorySize(data.sender)
              + ::memorySize(data.frgr)
              ;
      }
   std::string DDML(const NewPotentialGrowth& data)
      {return "<type name=\"NewPotentialGrowth\">"
               "<field name=\"sender\" kind=\"string\" />"
               "<field name=\"frgr\" kind=\"single\" units=\"\" />"
               "</type>";}

   //------ NewCanopy ------
   struct NewCanopy
      {
      std::string sender;
      float height;
      float depth;
      float lai;
      float lai_tot;
      float cover;
      float cover_tot;
      };

   inline void pack(MessageData& messageData, const NewCanopy& data)
      {
      pack(messageData, data.sender);
      pack(messageData, data.height);
      pack(messageData, data.depth);
      pack(messageData, data.lai);
      pack(messageData, data.lai_tot);
      pack(messageData, data.cover);
      pack(messageData, data.cover_tot);
      }
   inline void unpack(MessageData& messageData, NewCanopy& data)
      {
      unpack(messageData, data.sender);
      unpack(messageData, data.height);
      unpack(messageData, data.depth);
      unpack(messageData, data.lai);
      unpack(messageData, data.lai_tot);
      unpack(messageData, data.cover);
      unpack(messageData, data.cover_tot);
      }
   inline unsigned memorySize(const NewCanopy& data)
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
   std::string DDML(const NewCanopy& data)
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
   struct NewCrop
      {
      std::string sender;
      std::string crop_type;
      };

   inline void pack(MessageData& messageData, const NewCrop& data)
      {
      pack(messageData, data.sender);
      pack(messageData, data.crop_type);
      }
   inline void unpack(MessageData& messageData, NewCrop& data)
      {
      unpack(messageData, data.sender);
      unpack(messageData, data.crop_type);
      }
   inline unsigned memorySize(const NewCrop& data)
      {
      return 0
              + ::memorySize(data.sender)
              + ::memorySize(data.crop_type)
              ;
      }
   std::string DDML(const NewCrop& data)
      {return "<type name=\"NewCrop\">"
               "<field name=\"sender\" kind=\"string\" />"
               "<field name=\"crop_type\" kind=\"string\" />"
               "</type>";}

   //------ SoilLayers ------
   struct SoilLayers
      {
      std::vector<double> layers;
      std::vector<double> value;
      };

   inline void pack(MessageData& messageData, const SoilLayers& data)
      {
      pack(messageData, data.layers);
      pack(messageData, data.value);
      }
   inline void unpack(MessageData& messageData, SoilLayers& data)
      {
      unpack(messageData, data.layers);
      unpack(messageData, data.value);
      }
   inline unsigned memorySize(const SoilLayers& data)
      {
      return 0
              + ::memorySize(data.layers)
              + ::memorySize(data.value)
              ;
      }
   std::string DDML(const SoilLayers& data)
      {return "<type name=\"SoilLayers\">"
               "<field name=\"layers\" kind=\"double\" unit=\"mm\" array=\"T\" />"
               "<field name=\"value\" kind=\"double\" unit=\"mm\" array=\"T\" />"
               "</type>";}

   //------ rlv_layer ------
   struct rlv_layer
      {
      std::vector<double> layers;
      std::vector<double> rlv;
      };

   inline void pack(MessageData& messageData, const rlv_layer& data)
      {
      pack(messageData, data.layers);
      pack(messageData, data.rlv);
      }
   inline void unpack(MessageData& messageData, rlv_layer& data)
      {
      unpack(messageData, data.layers);
      unpack(messageData, data.rlv);
      }
   inline unsigned memorySize(const rlv_layer& data)
      {
      return 0
              + ::memorySize(data.layers)
              + ::memorySize(data.rlv)
              ;
      }
   std::string DDML(const rlv_layer& data)
      {return "<field name=\"rlv_layer\">"
               "<field name=\"layers\" kind=\"double\" units=\"mm\" array=\"T\" />"
               "<field name=\"rlv\" kind=\"double\" units=\"mmmm^3\" array=\"T\" />"
               "</field>";}

   //------ demands ------
   struct demands
      {
      std::string crop_ident;
      std::string crop_type;
      rlv_layer rlv_layer;
      double demand;
      };

   inline void pack(MessageData& messageData, const demands& data)
      {
      pack(messageData, data.crop_ident);
      pack(messageData, data.crop_type);
      pack(messageData, data.rlv_layer);
      pack(messageData, data.demand);
      }
   inline void unpack(MessageData& messageData, demands& data)
      {
      unpack(messageData, data.crop_ident);
      unpack(messageData, data.crop_type);
      unpack(messageData, data.rlv_layer);
      unpack(messageData, data.demand);
      }
   inline unsigned memorySize(const demands& data)
      {
      return 0
              + ::memorySize(data.crop_ident)
              + ::memorySize(data.crop_type)
              + ::memorySize(data.rlv_layer)
              + ::memorySize(data.demand)
              ;
      }
   std::string DDML(const demands& data)
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
   struct PastureWaterDemand
      {
      std::vector<demands> demands;
      };

   inline void pack(MessageData& messageData, const PastureWaterDemand& data)
      {
      pack(messageData, data.demands);
      }
   inline void unpack(MessageData& messageData, PastureWaterDemand& data)
      {
      unpack(messageData, data.demands);
      }
   inline unsigned memorySize(const PastureWaterDemand& data)
      {
      return 0
              + ::memorySize(data.demands)
              ;
      }
   std::string DDML(const PastureWaterDemand& data)
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
   struct supplies
      {
      std::string crop_ident;
      std::vector<double> layers;
      std::vector<double> supply;
      };

   inline void pack(MessageData& messageData, const supplies& data)
      {
      pack(messageData, data.crop_ident);
      pack(messageData, data.layers);
      pack(messageData, data.supply);
      }
   inline void unpack(MessageData& messageData, supplies& data)
      {
      unpack(messageData, data.crop_ident);
      unpack(messageData, data.layers);
      unpack(messageData, data.supply);
      }
   inline unsigned memorySize(const supplies& data)
      {
      return 0
              + ::memorySize(data.crop_ident)
              + ::memorySize(data.layers)
              + ::memorySize(data.supply)
              ;
      }
   std::string DDML(const supplies& data)
      {return "<field name=\"supplies\" array=\"T\">"
               "<element>"
               "<field name=\"crop_ident\" kind=\"string\" description=\"Cohort name + instance name\" />"
               "<field name=\"layers\" kind=\"double\" unit=\"mm\" array=\"T\" />"
               "<field name=\"supply\" kind=\"double\" unit=\"mm\" array=\"T\" />"
               "</element>"
               "</field>";}

   //------ PastureWaterSupply ------
   struct PastureWaterSupply
      {
      std::vector<supplies> supplies;
      };

   inline void pack(MessageData& messageData, const PastureWaterSupply& data)
      {
      pack(messageData, data.supplies);
      }
   inline void unpack(MessageData& messageData, PastureWaterSupply& data)
      {
      unpack(messageData, data.supplies);
      }
   inline unsigned memorySize(const PastureWaterSupply& data)
      {
      return 0
              + ::memorySize(data.supplies)
              ;
      }
   std::string DDML(const PastureWaterSupply& data)
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
   struct water_uptake
      {
      std::string crop_ident;
      std::vector<double> layers;
      std::vector<double> uptake;
      };

   inline void pack(MessageData& messageData, const water_uptake& data)
      {
      pack(messageData, data.crop_ident);
      pack(messageData, data.layers);
      pack(messageData, data.uptake);
      }
   inline void unpack(MessageData& messageData, water_uptake& data)
      {
      unpack(messageData, data.crop_ident);
      unpack(messageData, data.layers);
      unpack(messageData, data.uptake);
      }
   inline unsigned memorySize(const water_uptake& data)
      {
      return 0
              + ::memorySize(data.crop_ident)
              + ::memorySize(data.layers)
              + ::memorySize(data.uptake)
              ;
      }
   std::string DDML(const water_uptake& data)
      {return "<field name=\"water_uptake\" array=\"T\">"
               "<element>"
               "<field name=\"crop_ident\" kind=\"string\" description=\"Cohort name + instance name\" />"
               "<field name=\"layers\" kind=\"double\" unit=\"mm\" array=\"T\" />"
               "<field name=\"uptake\" kind=\"double\" unit=\"mm\" array=\"T\" />"
               "</element>"
               "</field>";}

   //------ PastureWaterUptake ------
   struct PastureWaterUptake
      {
      std::vector<water_uptake> water_uptake;
      };

   inline void pack(MessageData& messageData, const PastureWaterUptake& data)
      {
      pack(messageData, data.water_uptake);
      }
   inline void unpack(MessageData& messageData, PastureWaterUptake& data)
      {
      unpack(messageData, data.water_uptake);
      }
   inline unsigned memorySize(const PastureWaterUptake& data)
      {
      return 0
              + ::memorySize(data.water_uptake)
              ;
      }
   std::string DDML(const PastureWaterUptake& data)
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
   struct water_info
      {
      std::string crop_ident;
      std::vector<double> params;
      double demand;
      std::vector<double> layers;
      std::vector<double> rlv;
      std::vector<double> root_radius;
      };

   inline void pack(MessageData& messageData, const water_info& data)
      {
      pack(messageData, data.crop_ident);
      pack(messageData, data.params);
      pack(messageData, data.demand);
      pack(messageData, data.layers);
      pack(messageData, data.rlv);
      pack(messageData, data.root_radius);
      }
   inline void unpack(MessageData& messageData, water_info& data)
      {
      unpack(messageData, data.crop_ident);
      unpack(messageData, data.params);
      unpack(messageData, data.demand);
      unpack(messageData, data.layers);
      unpack(messageData, data.rlv);
      unpack(messageData, data.root_radius);
      }
   inline unsigned memorySize(const water_info& data)
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
   std::string DDML(const water_info& data)
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
   struct WaterInfo
      {
      std::vector<water_info> water_info;
      };

   inline void pack(MessageData& messageData, const WaterInfo& data)
      {
      pack(messageData, data.water_info);
      }
   inline void unpack(MessageData& messageData, WaterInfo& data)
      {
      unpack(messageData, data.water_info);
      }
   inline unsigned memorySize(const WaterInfo& data)
      {
      return 0
              + ::memorySize(data.water_info)
              ;
      }
   std::string DDML(const WaterInfo& data)
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
   struct fom
      {
      double weight;
      double n;
      double p;
      double s;
      double ash_alk;
      };

   inline void pack(MessageData& messageData, const fom& data)
      {
      pack(messageData, data.weight);
      pack(messageData, data.n);
      pack(messageData, data.p);
      pack(messageData, data.s);
      pack(messageData, data.ash_alk);
      }
   inline void unpack(MessageData& messageData, fom& data)
      {
      unpack(messageData, data.weight);
      unpack(messageData, data.n);
      unpack(messageData, data.p);
      unpack(messageData, data.s);
      unpack(messageData, data.ash_alk);
      }
   inline unsigned memorySize(const fom& data)
      {
      return 0
              + ::memorySize(data.weight)
              + ::memorySize(data.n)
              + ::memorySize(data.p)
              + ::memorySize(data.s)
              + ::memorySize(data.ash_alk)
              ;
      }
   std::string DDML(const fom& data)
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
   struct FomAdded
      {
      std::vector<double> layers;
      std::vector<fom> fom;
      };

   inline void pack(MessageData& messageData, const FomAdded& data)
      {
      pack(messageData, data.layers);
      pack(messageData, data.fom);
      }
   inline void unpack(MessageData& messageData, FomAdded& data)
      {
      unpack(messageData, data.layers);
      unpack(messageData, data.fom);
      }
   inline unsigned memorySize(const FomAdded& data)
      {
      return 0
              + ::memorySize(data.layers)
              + ::memorySize(data.fom)
              ;
      }
   std::string DDML(const FomAdded& data)
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
   struct PastureNutrientUptake
      {
      std::string nutrient;
      std::vector<double> layers;
      std::vector<double> uptake;
      };

   inline void pack(MessageData& messageData, const PastureNutrientUptake& data)
      {
      pack(messageData, data.nutrient);
      pack(messageData, data.layers);
      pack(messageData, data.uptake);
      }
   inline void unpack(MessageData& messageData, PastureNutrientUptake& data)
      {
      unpack(messageData, data.nutrient);
      unpack(messageData, data.layers);
      unpack(messageData, data.uptake);
      }
   inline unsigned memorySize(const PastureNutrientUptake& data)
      {
      return 0
              + ::memorySize(data.nutrient)
              + ::memorySize(data.layers)
              + ::memorySize(data.uptake)
              ;
      }
   std::string DDML(const PastureNutrientUptake& data)
      {return "<type name=\"PastureNutrientUptake\">"
               "<field name=\"nutrient\" kind=\"string\" />"
               "<field name=\"layers\" unit=\"mm\" kind=\"double\" array=\"T\" />"
               "<field name=\"uptake\" unit=\"kg/ha\" kind=\"double\" array=\"T\" />"
               "</type>";}

   //------ PastureSow ------
   struct PastureSow
      {
      double rate;
      };

   inline void pack(MessageData& messageData, const PastureSow& data)
      {
      pack(messageData, data.rate);
      }
   inline void unpack(MessageData& messageData, PastureSow& data)
      {
      unpack(messageData, data.rate);
      }
   inline unsigned memorySize(const PastureSow& data)
      {
      return 0
              + ::memorySize(data.rate)
              ;
      }
   std::string DDML(const PastureSow& data)
      {return "<type name=\"PastureSow\">"
               "<field name=\"rate\" unit=\"kg/ha\" kind=\"double\" />"
               "</type>";}

   //------ PastureKill ------
   struct PastureKill
      {
      double propn_herbage;
      double propn_seeds;
      };

   inline void pack(MessageData& messageData, const PastureKill& data)
      {
      pack(messageData, data.propn_herbage);
      pack(messageData, data.propn_seeds);
      }
   inline void unpack(MessageData& messageData, PastureKill& data)
      {
      unpack(messageData, data.propn_herbage);
      unpack(messageData, data.propn_seeds);
      }
   inline unsigned memorySize(const PastureKill& data)
      {
      return 0
              + ::memorySize(data.propn_herbage)
              + ::memorySize(data.propn_seeds)
              ;
      }
   std::string DDML(const PastureKill& data)
      {return "<type name=\"PastureKill\">"
               "<field name=\"propn_herbage\" unit=\"-\" kind=\"double\" />"
               "<field name=\"propn_seeds\" unit=\"-\" kind=\"double\" />"
               "</type>";}

   //------ PastureCultivate ------
   struct PastureCultivate
      {
      double depth;
      double propn_incorp;
      double propn_mixed;
      };

   inline void pack(MessageData& messageData, const PastureCultivate& data)
      {
      pack(messageData, data.depth);
      pack(messageData, data.propn_incorp);
      pack(messageData, data.propn_mixed);
      }
   inline void unpack(MessageData& messageData, PastureCultivate& data)
      {
      unpack(messageData, data.depth);
      unpack(messageData, data.propn_incorp);
      unpack(messageData, data.propn_mixed);
      }
   inline unsigned memorySize(const PastureCultivate& data)
      {
      return 0
              + ::memorySize(data.depth)
              + ::memorySize(data.propn_incorp)
              + ::memorySize(data.propn_mixed)
              ;
      }
   std::string DDML(const PastureCultivate& data)
      {return "<type name=\"PastureCultivate\">"
               "<field name=\"depth\" unit=\"mm\" kind=\"double\" />"
               "<field name=\"propn_incorp\" unit=\"-\" kind=\"double\" />"
               "<field name=\"propn_mixed\" unit=\"-\" kind=\"double\" />"
               "</type>";}

   //------ PastureCut ------
   struct PastureCut
      {
      double cut_height;
      double gathered;
      double dmd_loss;
      double dm_content;
      };

   inline void pack(MessageData& messageData, const PastureCut& data)
      {
      pack(messageData, data.cut_height);
      pack(messageData, data.gathered);
      pack(messageData, data.dmd_loss);
      pack(messageData, data.dm_content);
      }
   inline void unpack(MessageData& messageData, PastureCut& data)
      {
      unpack(messageData, data.cut_height);
      unpack(messageData, data.gathered);
      unpack(messageData, data.dmd_loss);
      unpack(messageData, data.dm_content);
      }
   inline unsigned memorySize(const PastureCut& data)
      {
      return 0
              + ::memorySize(data.cut_height)
              + ::memorySize(data.gathered)
              + ::memorySize(data.dmd_loss)
              + ::memorySize(data.dm_content)
              ;
      }
   std::string DDML(const PastureCut& data)
      {return "<type name=\"PastureCut\">"
               "<field name=\"cut_height\" unit=\"mm\" kind=\"double\" />"
               "<field name=\"gathered\" unit=\"-\" kind=\"double\" />"
               "<field name=\"dmd_loss\" unit=\"-\" kind=\"double\" />"
               "<field name=\"dm_content\" unit=\"kg/kg\" kind=\"double\" />"
               "</type>";}

   //------ PastureOnCut ------
   struct PastureOnCut
      {
      double fresh_wt;
      double dm_content;
      double dm;
      double cp_conc;
      double p_conc;
      double s_conc;
      double ash_alk;
      };

   inline void pack(MessageData& messageData, const PastureOnCut& data)
      {
      pack(messageData, data.fresh_wt);
      pack(messageData, data.dm_content);
      pack(messageData, data.dm);
      pack(messageData, data.cp_conc);
      pack(messageData, data.p_conc);
      pack(messageData, data.s_conc);
      pack(messageData, data.ash_alk);
      }
   inline void unpack(MessageData& messageData, PastureOnCut& data)
      {
      unpack(messageData, data.fresh_wt);
      unpack(messageData, data.dm_content);
      unpack(messageData, data.dm);
      unpack(messageData, data.cp_conc);
      unpack(messageData, data.p_conc);
      unpack(messageData, data.s_conc);
      unpack(messageData, data.ash_alk);
      }
   inline unsigned memorySize(const PastureOnCut& data)
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
   std::string DDML(const PastureOnCut& data)
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
   struct PastureWeather
      {
      double maxt;
      double mint;
      double rain;
      double snow;
      double radn;
      double vpd;
      double wind;
      };

   inline void pack(MessageData& messageData, const PastureWeather& data)
      {
      pack(messageData, data.maxt);
      pack(messageData, data.mint);
      pack(messageData, data.rain);
      pack(messageData, data.snow);
      pack(messageData, data.radn);
      pack(messageData, data.vpd);
      pack(messageData, data.wind);
      }
   inline void unpack(MessageData& messageData, PastureWeather& data)
      {
      unpack(messageData, data.maxt);
      unpack(messageData, data.mint);
      unpack(messageData, data.rain);
      unpack(messageData, data.snow);
      unpack(messageData, data.radn);
      unpack(messageData, data.vpd);
      unpack(messageData, data.wind);
      }
   inline unsigned memorySize(const PastureWeather& data)
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
   std::string DDML(const PastureWeather& data)
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
   struct Faeces
      {
      double weight;
      double n;
      double p;
      double s;
      double ash_alk;
      };

   inline void pack(MessageData& messageData, const Faeces& data)
      {
      pack(messageData, data.weight);
      pack(messageData, data.n);
      pack(messageData, data.p);
      pack(messageData, data.s);
      pack(messageData, data.ash_alk);
      }
   inline void unpack(MessageData& messageData, Faeces& data)
      {
      unpack(messageData, data.weight);
      unpack(messageData, data.n);
      unpack(messageData, data.p);
      unpack(messageData, data.s);
      unpack(messageData, data.ash_alk);
      }
   inline unsigned memorySize(const Faeces& data)
      {
      return 0
              + ::memorySize(data.weight)
              + ::memorySize(data.n)
              + ::memorySize(data.p)
              + ::memorySize(data.s)
              + ::memorySize(data.ash_alk)
              ;
      }
   std::string DDML(const Faeces& data)
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
   struct FaecesInorg
      {
      double n;
      double p;
      double s;
      };

   inline void pack(MessageData& messageData, const FaecesInorg& data)
      {
      pack(messageData, data.n);
      pack(messageData, data.p);
      pack(messageData, data.s);
      }
   inline void unpack(MessageData& messageData, FaecesInorg& data)
      {
      unpack(messageData, data.n);
      unpack(messageData, data.p);
      unpack(messageData, data.s);
      }
   inline unsigned memorySize(const FaecesInorg& data)
      {
      return 0
              + ::memorySize(data.n)
              + ::memorySize(data.p)
              + ::memorySize(data.s)
              ;
      }
   std::string DDML(const FaecesInorg& data)
      {return "<type name=\"FaecesInorg\" array=\"T\">"
               "<element>"
               "<field name=\"n\" unit=\"kg/d\" kind=\"double\" />"
               "<field name=\"p\" unit=\"kg/d\" kind=\"double\" />"
               "<field name=\"s\" unit=\"kg/d\" kind=\"double\" />"
               "</element>"
               "</type>";}

   //------ Intake ------
   struct Intake
      {
      double weight;
      double n;
      double p;
      double s;
      double ash_alk;
      };

   inline void pack(MessageData& messageData, const Intake& data)
      {
      pack(messageData, data.weight);
      pack(messageData, data.n);
      pack(messageData, data.p);
      pack(messageData, data.s);
      pack(messageData, data.ash_alk);
      }
   inline void unpack(MessageData& messageData, Intake& data)
      {
      unpack(messageData, data.weight);
      unpack(messageData, data.n);
      unpack(messageData, data.p);
      unpack(messageData, data.s);
      unpack(messageData, data.ash_alk);
      }
   inline unsigned memorySize(const Intake& data)
      {
      return 0
              + ::memorySize(data.weight)
              + ::memorySize(data.n)
              + ::memorySize(data.p)
              + ::memorySize(data.s)
              + ::memorySize(data.ash_alk)
              ;
      }
   std::string DDML(const Intake& data)
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
   struct PastIntake
      {
      double weight;
      double n;
      double p;
      double s;
      double ash_alk;
      };

   inline void pack(MessageData& messageData, const PastIntake& data)
      {
      pack(messageData, data.weight);
      pack(messageData, data.n);
      pack(messageData, data.p);
      pack(messageData, data.s);
      pack(messageData, data.ash_alk);
      }
   inline void unpack(MessageData& messageData, PastIntake& data)
      {
      unpack(messageData, data.weight);
      unpack(messageData, data.n);
      unpack(messageData, data.p);
      unpack(messageData, data.s);
      unpack(messageData, data.ash_alk);
      }
   inline unsigned memorySize(const PastIntake& data)
      {
      return 0
              + ::memorySize(data.weight)
              + ::memorySize(data.n)
              + ::memorySize(data.p)
              + ::memorySize(data.s)
              + ::memorySize(data.ash_alk)
              ;
      }
   std::string DDML(const PastIntake& data)
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
   struct SuppIntake
      {
      double weight;
      double n;
      double p;
      double s;
      double ash_alk;
      };

   inline void pack(MessageData& messageData, const SuppIntake& data)
      {
      pack(messageData, data.weight);
      pack(messageData, data.n);
      pack(messageData, data.p);
      pack(messageData, data.s);
      pack(messageData, data.ash_alk);
      }
   inline void unpack(MessageData& messageData, SuppIntake& data)
      {
      unpack(messageData, data.weight);
      unpack(messageData, data.n);
      unpack(messageData, data.p);
      unpack(messageData, data.s);
      unpack(messageData, data.ash_alk);
      }
   inline unsigned memorySize(const SuppIntake& data)
      {
      return 0
              + ::memorySize(data.weight)
              + ::memorySize(data.n)
              + ::memorySize(data.p)
              + ::memorySize(data.s)
              + ::memorySize(data.ash_alk)
              ;
      }
   std::string DDML(const SuppIntake& data)
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
   struct faeces_om
      {
      double weight;
      double n;
      double p;
      double s;
      double ash_alk;
      };

   inline void pack(MessageData& messageData, const faeces_om& data)
      {
      pack(messageData, data.weight);
      pack(messageData, data.n);
      pack(messageData, data.p);
      pack(messageData, data.s);
      pack(messageData, data.ash_alk);
      }
   inline void unpack(MessageData& messageData, faeces_om& data)
      {
      unpack(messageData, data.weight);
      unpack(messageData, data.n);
      unpack(messageData, data.p);
      unpack(messageData, data.s);
      unpack(messageData, data.ash_alk);
      }
   inline unsigned memorySize(const faeces_om& data)
      {
      return 0
              + ::memorySize(data.weight)
              + ::memorySize(data.n)
              + ::memorySize(data.p)
              + ::memorySize(data.s)
              + ::memorySize(data.ash_alk)
              ;
      }
   std::string DDML(const faeces_om& data)
      {return "<type name=\"faeces_om\">"
               "<field name=\"weight\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"n\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"p\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"s\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"ash_alk\" unit=\"mol/ha\" kind=\"double\" />"
               "</type>";}

   //------ faeces_inorg ------
   struct faeces_inorg
      {
      double n;
      double p;
      double s;
      };

   inline void pack(MessageData& messageData, const faeces_inorg& data)
      {
      pack(messageData, data.n);
      pack(messageData, data.p);
      pack(messageData, data.s);
      }
   inline void unpack(MessageData& messageData, faeces_inorg& data)
      {
      unpack(messageData, data.n);
      unpack(messageData, data.p);
      unpack(messageData, data.s);
      }
   inline unsigned memorySize(const faeces_inorg& data)
      {
      return 0
              + ::memorySize(data.n)
              + ::memorySize(data.p)
              + ::memorySize(data.s)
              ;
      }
   std::string DDML(const faeces_inorg& data)
      {return "<type name=\"faeces_inorg\">"
               "<field name=\"n\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"p\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"s\" unit=\"kg/ha\" kind=\"double\" />"
               "</type>";}

   //------ urine ------
   struct urine
      {
      double volume;
      double urea;
      double pox;
      double so4;
      double ash_alk;
      };

   inline void pack(MessageData& messageData, const urine& data)
      {
      pack(messageData, data.volume);
      pack(messageData, data.urea);
      pack(messageData, data.pox);
      pack(messageData, data.so4);
      pack(messageData, data.ash_alk);
      }
   inline void unpack(MessageData& messageData, urine& data)
      {
      unpack(messageData, data.volume);
      unpack(messageData, data.urea);
      unpack(messageData, data.pox);
      unpack(messageData, data.so4);
      unpack(messageData, data.ash_alk);
      }
   inline unsigned memorySize(const urine& data)
      {
      return 0
              + ::memorySize(data.volume)
              + ::memorySize(data.urea)
              + ::memorySize(data.pox)
              + ::memorySize(data.so4)
              + ::memorySize(data.ash_alk)
              ;
      }
   std::string DDML(const urine& data)
      {return "<type name=\"urine\">"
               "<field name=\"volume\" unit=\"m3/ha\" kind=\"double\" />"
               "<field name=\"urea\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"pox\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"so4\" unit=\"kg/ha\" kind=\"double\" />"
               "<field name=\"ash_alk\" unit=\"mol/ha\" kind=\"double\" />"
               "</type>";}

   //------ AddExcreta ------
   struct AddExcreta
      {
      faeces_om faeces_om;
      faeces_inorg faeces_inorg;
      urine urine;
      };

   inline void pack(MessageData& messageData, const AddExcreta& data)
      {
      pack(messageData, data.faeces_om);
      pack(messageData, data.faeces_inorg);
      pack(messageData, data.urine);
      }
   inline void unpack(MessageData& messageData, AddExcreta& data)
      {
      unpack(messageData, data.faeces_om);
      unpack(messageData, data.faeces_inorg);
      unpack(messageData, data.urine);
      }
   inline unsigned memorySize(const AddExcreta& data)
      {
      return 0
              + ::memorySize(data.faeces_om)
              + ::memorySize(data.faeces_inorg)
              + ::memorySize(data.urine)
              ;
      }
   std::string DDML(const AddExcreta& data)
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
   struct RemoveHerbage
      {
      std::vector<double> herbage;
      std::vector<double> seed;
      };

   inline void pack(MessageData& messageData, const RemoveHerbage& data)
      {
      pack(messageData, data.herbage);
      pack(messageData, data.seed);
      }
   inline void unpack(MessageData& messageData, RemoveHerbage& data)
      {
      unpack(messageData, data.herbage);
      unpack(messageData, data.seed);
      }
   inline unsigned memorySize(const RemoveHerbage& data)
      {
      return 0
              + ::memorySize(data.herbage)
              + ::memorySize(data.seed)
              ;
      }
   std::string DDML(const RemoveHerbage& data)
      {return "<type name=\"RemoveHerbage\">"
               "<field name=\"herbage\" unit=\"kg/ha\" kind=\"double\" array=\"T\" />"
               "<field name=\"seed\" unit=\"kg/ha\" kind=\"double\" array=\"T\" />"
               "</type>";}

   //------ SuppEaten ------
   struct SuppEaten
      {
      std::string paddock;
      double eaten;
      };

   inline void pack(MessageData& messageData, const SuppEaten& data)
      {
      pack(messageData, data.paddock);
      pack(messageData, data.eaten);
      }
   inline void unpack(MessageData& messageData, SuppEaten& data)
      {
      unpack(messageData, data.paddock);
      unpack(messageData, data.eaten);
      }
   inline unsigned memorySize(const SuppEaten& data)
      {
      return 0
              + ::memorySize(data.paddock)
              + ::memorySize(data.eaten)
              ;
      }
   std::string DDML(const SuppEaten& data)
      {return "<type name=\"SuppEaten\" array=\"T\">"
               "<element>"
               "<field name=\"paddock\" unit=\"\" kind=\"string\" />"
               "<field name=\"eaten\" unit=\"kg\" kind=\"double\" />"
               "</element>"
               "</type>";}

   //------ herbage ------
   struct herbage
      {
      double dm;
      double dmd;
      double cp_conc;
      double p_conc;
      double s_conc;
      double prot_dg;
      double ash_alk;
      double height_ratio;
      };

   inline void pack(MessageData& messageData, const herbage& data)
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
   inline void unpack(MessageData& messageData, herbage& data)
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
   inline unsigned memorySize(const herbage& data)
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
   std::string DDML(const herbage& data)
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
   struct seed
      {
      double dm;
      double dmd;
      double cp_conc;
      double p_conc;
      double s_conc;
      double prot_dg;
      double ash_alk;
      double height_ratio;
      };

   inline void pack(MessageData& messageData, const seed& data)
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
   inline void unpack(MessageData& messageData, seed& data)
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
   inline unsigned memorySize(const seed& data)
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
   std::string DDML(const seed& data)
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
   struct Plant2Stock
      {
      std::vector<herbage> herbage;
      double propn_green;
      double legume;
      double select_factor;
      std::vector<seed> seed;
      std::vector<int> seed_class;
      };

   inline void pack(MessageData& messageData, const Plant2Stock& data)
      {
      pack(messageData, data.herbage);
      pack(messageData, data.propn_green);
      pack(messageData, data.legume);
      pack(messageData, data.select_factor);
      pack(messageData, data.seed);
      pack(messageData, data.seed_class);
      }
   inline void unpack(MessageData& messageData, Plant2Stock& data)
      {
      unpack(messageData, data.herbage);
      unpack(messageData, data.propn_green);
      unpack(messageData, data.legume);
      unpack(messageData, data.select_factor);
      unpack(messageData, data.seed);
      unpack(messageData, data.seed_class);
      }
   inline unsigned memorySize(const Plant2Stock& data)
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
   std::string DDML(const Plant2Stock& data)
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
   struct BuyStock
      {
      std::string genotype;
      int number;
      std::string sex;
      double age;
      double weight;
      double fleece_wt;
      double cond_score;
      std::string mated_to;
      int pregnant;
      int lactating;
      int no_young;
      double young_wt;
      double young_fleece_wt;
      };

   inline void pack(MessageData& messageData, const BuyStock& data)
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
   inline void unpack(MessageData& messageData, BuyStock& data)
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
   inline unsigned memorySize(const BuyStock& data)
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
   std::string DDML(const BuyStock& data)
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
   struct SellStock
      {
      int group;
      int number;
      };

   inline void pack(MessageData& messageData, const SellStock& data)
      {
      pack(messageData, data.group);
      pack(messageData, data.number);
      }
   inline void unpack(MessageData& messageData, SellStock& data)
      {
      unpack(messageData, data.group);
      unpack(messageData, data.number);
      }
   inline unsigned memorySize(const SellStock& data)
      {
      return 0
              + ::memorySize(data.group)
              + ::memorySize(data.number)
              ;
      }
   std::string DDML(const SellStock& data)
      {return "<type name=\"SellStock\">"
               "<field name=\"group\" unit=\"\" kind=\"integer4\" />"
               "<field name=\"number\" unit=\"\" kind=\"integer4\" />"
               "</type>";}

   //------ CastrateStock ------
   struct CastrateStock
      {
      int group;
      int number;
      };

   inline void pack(MessageData& messageData, const CastrateStock& data)
      {
      pack(messageData, data.group);
      pack(messageData, data.number);
      }
   inline void unpack(MessageData& messageData, CastrateStock& data)
      {
      unpack(messageData, data.group);
      unpack(messageData, data.number);
      }
   inline unsigned memorySize(const CastrateStock& data)
      {
      return 0
              + ::memorySize(data.group)
              + ::memorySize(data.number)
              ;
      }
   std::string DDML(const CastrateStock& data)
      {return "<type name=\"CastrateStock\">"
               "<field name=\"group\" unit=\"\" kind=\"integer4\" />"
               "<field name=\"number\" unit=\"\" kind=\"integer4\" />"
               "</type>";}

   //------ DryOffStock ------
   struct DryOffStock
      {
      int group;
      int number;
      };

   inline void pack(MessageData& messageData, const DryOffStock& data)
      {
      pack(messageData, data.group);
      pack(messageData, data.number);
      }
   inline void unpack(MessageData& messageData, DryOffStock& data)
      {
      unpack(messageData, data.group);
      unpack(messageData, data.number);
      }
   inline unsigned memorySize(const DryOffStock& data)
      {
      return 0
              + ::memorySize(data.group)
              + ::memorySize(data.number)
              ;
      }
   std::string DDML(const DryOffStock& data)
      {return "<type name=\"DryOffStock\">"
               "<field name=\"group\" unit=\"\" kind=\"integer4\" />"
               "<field name=\"number\" unit=\"\" kind=\"integer4\" />"
               "</type>";}

   //------ JoinStock ------
   struct JoinStock
      {
      int group;
      std::string mate_to;
      int mate_days;
      };

   inline void pack(MessageData& messageData, const JoinStock& data)
      {
      pack(messageData, data.group);
      pack(messageData, data.mate_to);
      pack(messageData, data.mate_days);
      }
   inline void unpack(MessageData& messageData, JoinStock& data)
      {
      unpack(messageData, data.group);
      unpack(messageData, data.mate_to);
      unpack(messageData, data.mate_days);
      }
   inline unsigned memorySize(const JoinStock& data)
      {
      return 0
              + ::memorySize(data.group)
              + ::memorySize(data.mate_to)
              + ::memorySize(data.mate_days)
              ;
      }
   std::string DDML(const JoinStock& data)
      {return "<type name=\"JoinStock\">"
               "<field name=\"group\" unit=\"\" kind=\"integer4\" />"
               "<field name=\"mate_to\" unit=\"\" kind=\"string\" />"
               "<field name=\"mate_days\" unit=\"d\" kind=\"integer4\" />"
               "</type>";}

   //------ MoveStock ------
   struct MoveStock
      {
      int group;
      std::string paddock;
      };

   inline void pack(MessageData& messageData, const MoveStock& data)
      {
      pack(messageData, data.group);
      pack(messageData, data.paddock);
      }
   inline void unpack(MessageData& messageData, MoveStock& data)
      {
      unpack(messageData, data.group);
      unpack(messageData, data.paddock);
      }
   inline unsigned memorySize(const MoveStock& data)
      {
      return 0
              + ::memorySize(data.group)
              + ::memorySize(data.paddock)
              ;
      }
   std::string DDML(const MoveStock& data)
      {return "<type name=\"MoveStock\">"
               "<field name=\"group\" unit=\"\" kind=\"integer4\" />"
               "<field name=\"paddock\" unit=\"\" kind=\"string\" />"
               "</type>";}

   //------ ShearStock ------
   struct ShearStock
      {
      int group;
      std::string sub_group;
      };

   inline void pack(MessageData& messageData, const ShearStock& data)
      {
      pack(messageData, data.group);
      pack(messageData, data.sub_group);
      }
   inline void unpack(MessageData& messageData, ShearStock& data)
      {
      unpack(messageData, data.group);
      unpack(messageData, data.sub_group);
      }
   inline unsigned memorySize(const ShearStock& data)
      {
      return 0
              + ::memorySize(data.group)
              + ::memorySize(data.sub_group)
              ;
      }
   std::string DDML(const ShearStock& data)
      {return "<type name=\"ShearStock\">"
               "<field name=\"group\" unit=\"\" kind=\"integer4\" />"
               "<field name=\"sub_group\" unit=\"\" kind=\"string\" />"
               "</type>";}

   //------ SplitStock ------
   struct SplitStock
      {
      int group;
      std::string type;
      double value;
      };

   inline void pack(MessageData& messageData, const SplitStock& data)
      {
      pack(messageData, data.group);
      pack(messageData, data.type);
      pack(messageData, data.value);
      }
   inline void unpack(MessageData& messageData, SplitStock& data)
      {
      unpack(messageData, data.group);
      unpack(messageData, data.type);
      unpack(messageData, data.value);
      }
   inline unsigned memorySize(const SplitStock& data)
      {
      return 0
              + ::memorySize(data.group)
              + ::memorySize(data.type)
              + ::memorySize(data.value)
              ;
      }
   std::string DDML(const SplitStock& data)
      {return "<type name=\"SplitStock\">"
               "<field name=\"group\" unit=\"\" kind=\"integer4\" />"
               "<field name=\"type\" unit=\"\" kind=\"string\" />"
               "<field name=\"value\" unit=\"\" kind=\"double\" />"
               "</type>";}

   //------ TagStock ------
   struct TagStock
      {
      int group;
      int value;
      };

   inline void pack(MessageData& messageData, const TagStock& data)
      {
      pack(messageData, data.group);
      pack(messageData, data.value);
      }
   inline void unpack(MessageData& messageData, TagStock& data)
      {
      unpack(messageData, data.group);
      unpack(messageData, data.value);
      }
   inline unsigned memorySize(const TagStock& data)
      {
      return 0
              + ::memorySize(data.group)
              + ::memorySize(data.value)
              ;
      }
   std::string DDML(const TagStock& data)
      {return "<type name=\"TagStock\">"
               "<field name=\"group\" unit=\"\" kind=\"integer4\" />"
               "<field name=\"value\" unit=\"\" kind=\"integer4\" />"
               "</type>";}

   //------ WeanStock ------
   struct WeanStock
      {
      int group;
      std::string sex;
      int number;
      };

   inline void pack(MessageData& messageData, const WeanStock& data)
      {
      pack(messageData, data.group);
      pack(messageData, data.sex);
      pack(messageData, data.number);
      }
   inline void unpack(MessageData& messageData, WeanStock& data)
      {
      unpack(messageData, data.group);
      unpack(messageData, data.sex);
      unpack(messageData, data.number);
      }
   inline unsigned memorySize(const WeanStock& data)
      {
      return 0
              + ::memorySize(data.group)
              + ::memorySize(data.sex)
              + ::memorySize(data.number)
              ;
      }
   std::string DDML(const WeanStock& data)
      {return "<type name=\"WeanStock\">"
               "<field name=\"group\" unit=\"\" kind=\"integer4\" />"
               "<field name=\"sex\" unit=\"\" kind=\"string\" />"
               "<field name=\"number\" unit=\"\" kind=\"integer4\" />"
               "</type>";}

   //------ dm ------
   struct dm
      {
      std::string pool;
      std::vector<std::string> part;
      std::vector<double> dlt;
      };

   inline void pack(MessageData& messageData, const dm& data)
      {
      pack(messageData, data.pool);
      pack(messageData, data.part);
      pack(messageData, data.dlt);
      }
   inline void unpack(MessageData& messageData, dm& data)
      {
      unpack(messageData, data.pool);
      unpack(messageData, data.part);
      unpack(messageData, data.dlt);
      }
   inline unsigned memorySize(const dm& data)
      {
      return 0
              + ::memorySize(data.pool)
              + ::memorySize(data.part)
              + ::memorySize(data.dlt)
              ;
      }
   std::string DDML(const dm& data)
      {return "<field name=\"dm\" array=\"T\">"
               "<element>"
               "<field name=\"pool\" kind=\"string\" />"
               "<field name=\"part\" kind=\"string\" array=\"T\" />"
               "<field name=\"dlt\" kind=\"double\" array=\"T\" />"
               "</element>"
               "</field>";}

   //------ RemoveCropDm ------
   struct RemoveCropDm
      {
      std::vector<dm> dm;
      };

   inline void pack(MessageData& messageData, const RemoveCropDm& data)
      {
      pack(messageData, data.dm);
      }
   inline void unpack(MessageData& messageData, RemoveCropDm& data)
      {
      unpack(messageData, data.dm);
      }
   inline unsigned memorySize(const RemoveCropDm& data)
      {
      return 0
              + ::memorySize(data.dm)
              ;
      }
   std::string DDML(const RemoveCropDm& data)
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
   struct RemoveResidueDm
      {
      std::vector<std::string> dm_type;
      std::vector<float> dlt_residue_dm;
      };

   inline void pack(MessageData& messageData, const RemoveResidueDm& data)
      {
      pack(messageData, data.dm_type);
      pack(messageData, data.dlt_residue_dm);
      }
   inline void unpack(MessageData& messageData, RemoveResidueDm& data)
      {
      unpack(messageData, data.dm_type);
      unpack(messageData, data.dlt_residue_dm);
      }
   inline unsigned memorySize(const RemoveResidueDm& data)
      {
      return 0
              + ::memorySize(data.dm_type)
              + ::memorySize(data.dlt_residue_dm)
              ;
      }
   std::string DDML(const RemoveResidueDm& data)
      {return "<type name=\"RemoveResidueDm\">"
               "<field name=\"dm_type\" kind=\"string\" array=\"T\" />"
               "<field name=\"dlt_residue_dm\" kind=\"single\" array=\"T\" />"
               "</type>";}

   //------ SupplementBuy ------
   struct SupplementBuy
      {
      std::string supplement;
      double amount;
      };

   inline void pack(MessageData& messageData, const SupplementBuy& data)
      {
      pack(messageData, data.supplement);
      pack(messageData, data.amount);
      }
   inline void unpack(MessageData& messageData, SupplementBuy& data)
      {
      unpack(messageData, data.supplement);
      unpack(messageData, data.amount);
      }
   inline unsigned memorySize(const SupplementBuy& data)
      {
      return 0
              + ::memorySize(data.supplement)
              + ::memorySize(data.amount)
              ;
      }
   std::string DDML(const SupplementBuy& data)
      {return "<type name=\"SupplementBuy\">"
               "<field name=\"supplement\" kind=\"string\" unit=\"-\" />"
               "<field name=\"amount\" kind=\"double\" unit=\"kg\" />"
               "</type>";}

   //------ SupplementFeed ------
   struct SupplementFeed
      {
      std::string supplement;
      double amount;
      std::string paddock;
      };

   inline void pack(MessageData& messageData, const SupplementFeed& data)
      {
      pack(messageData, data.supplement);
      pack(messageData, data.amount);
      pack(messageData, data.paddock);
      }
   inline void unpack(MessageData& messageData, SupplementFeed& data)
      {
      unpack(messageData, data.supplement);
      unpack(messageData, data.amount);
      unpack(messageData, data.paddock);
      }
   inline unsigned memorySize(const SupplementFeed& data)
      {
      return 0
              + ::memorySize(data.supplement)
              + ::memorySize(data.amount)
              + ::memorySize(data.paddock)
              ;
      }
   std::string DDML(const SupplementFeed& data)
      {return "<type name=\"SupplementFeed\">"
               "<field name=\"supplement\" kind=\"string\" unit=\"-\" />"
               "<field name=\"amount\" kind=\"double\" unit=\"kg\" />"
               "<field name=\"paddock\" kind=\"string\" unit=\"-\" />"
               "</type>";}

   //------ SupplementMix ------
   struct SupplementMix
      {
      std::string src_store;
      double amount;
      std::string dest_store;
      };

   inline void pack(MessageData& messageData, const SupplementMix& data)
      {
      pack(messageData, data.src_store);
      pack(messageData, data.amount);
      pack(messageData, data.dest_store);
      }
   inline void unpack(MessageData& messageData, SupplementMix& data)
      {
      unpack(messageData, data.src_store);
      unpack(messageData, data.amount);
      unpack(messageData, data.dest_store);
      }
   inline unsigned memorySize(const SupplementMix& data)
      {
      return 0
              + ::memorySize(data.src_store)
              + ::memorySize(data.amount)
              + ::memorySize(data.dest_store)
              ;
      }
   std::string DDML(const SupplementMix& data)
      {return "<type name=\"SupplementMix\">"
               "<field name=\"src_store\" kind=\"string\" unit=\"-\" />"
               "<field name=\"amount\" kind=\"double\" unit=\"kg\" />"
               "<field name=\"dest_store\" kind=\"string\" unit=\"-\" />"
               "</type>";}

#endif

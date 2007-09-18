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

   void pack(MessageData& messageData, const Null& data);
   void unpack(MessageData& messageData, Null& data);
   unsigned memorySize(const Null& data);
   std::string DDML(const Null& data);

   //------ Complete ------
   struct CompleteType
      {
      int ackID;
      };

   void pack(MessageData& messageData, const CompleteType& data);
   void unpack(MessageData& messageData, CompleteType& data);
   unsigned memorySize(const CompleteType& data);
   std::string DDML(const CompleteType& data);

   //------ Error ------
   struct ErrorType
      {
      std::string msg;
      bool isFatal;
      };

   void pack(MessageData& messageData, const ErrorType& data);
   void unpack(MessageData& messageData, ErrorType& data);
   unsigned memorySize(const ErrorType& data);
   std::string DDML(const ErrorType& data);

   //------ Event ------
   struct EventType
      {
      int ID;
      int publishedBy;
      std::string ddml;
      };

   void pack(MessageData& messageData, const EventType& data);
   void unpack(MessageData& messageData, EventType& data);
   unsigned memorySize(const EventType& data);
   std::string DDML(const EventType& data);

   //------ GetValue ------
   struct GetValueType
      {
      int ID;
      };

   void pack(MessageData& messageData, const GetValueType& data);
   void unpack(MessageData& messageData, GetValueType& data);
   unsigned memorySize(const GetValueType& data);
   std::string DDML(const GetValueType& data);

   //------ Init1 ------
   struct Init1Type
      {
      std::string sdml;
      std::string fqn;
      bool inStartup;
      };

   void pack(MessageData& messageData, const Init1Type& data);
   void unpack(MessageData& messageData, Init1Type& data);
   unsigned memorySize(const Init1Type& data);
   std::string DDML(const Init1Type& data);

   //------ NotifySetValueSuccess ------
   struct NotifySetValueSuccessType
      {
      int ID;
      bool success;
      };

   void pack(MessageData& messageData, const NotifySetValueSuccessType& data);
   void unpack(MessageData& messageData, NotifySetValueSuccessType& data);
   unsigned memorySize(const NotifySetValueSuccessType& data);
   std::string DDML(const NotifySetValueSuccessType& data);

   //------ PublishEvent ------
   struct PublishEventType
      {
      int ID;
      std::string ddml;
      };

   void pack(MessageData& messageData, const PublishEventType& data);
   void unpack(MessageData& messageData, PublishEventType& data);
   unsigned memorySize(const PublishEventType& data);
   std::string DDML(const PublishEventType& data);

   //------ QueryInfo ------
   struct QueryInfoType
      {
      std::string name;
      int kind;
      };

   void pack(MessageData& messageData, const QueryInfoType& data);
   void unpack(MessageData& messageData, QueryInfoType& data);
   unsigned memorySize(const QueryInfoType& data);
   std::string DDML(const QueryInfoType& data);

   //------ Register ------
   struct RegisterType
      {
      int kind;
      int ID;
      int destID;
      std::string name;
      std::string ddml;
      };

   void pack(MessageData& messageData, const RegisterType& data);
   void unpack(MessageData& messageData, RegisterType& data);
   unsigned memorySize(const RegisterType& data);
   std::string DDML(const RegisterType& data);

   //------ ReplyValue ------
   struct ReplyValueType
      {
      int queryID;
      std::string ddml;
      };

   void pack(MessageData& messageData, const ReplyValueType& data);
   void unpack(MessageData& messageData, ReplyValueType& data);
   unsigned memorySize(const ReplyValueType& data);
   std::string DDML(const ReplyValueType& data);

   //------ RequestSetValue ------
   struct RequestSetValueType
      {
      int ID;
      std::string ddml;
      };

   void pack(MessageData& messageData, const RequestSetValueType& data);
   void unpack(MessageData& messageData, RequestSetValueType& data);
   unsigned memorySize(const RequestSetValueType& data);
   std::string DDML(const RequestSetValueType& data);

   //------ ReturnInfo ------
   struct ReturnInfoType
      {
      int queryID;
      int compID;
      int ID;
      std::string name;
      std::string type;
      int kind;
      };

   void pack(MessageData& messageData, const ReturnInfoType& data);
   void unpack(MessageData& messageData, ReturnInfoType& data);
   unsigned memorySize(const ReturnInfoType& data);
   std::string DDML(const ReturnInfoType& data);

   //------ ReturnValue ------
   struct ReturnValueType
      {
      int compID;
      int ID;
      std::string ddml;
      };

   void pack(MessageData& messageData, const ReturnValueType& data);
   void unpack(MessageData& messageData, ReturnValueType& data);
   unsigned memorySize(const ReturnValueType& data);
   std::string DDML(const ReturnValueType& data);

   //------ QueryValue ------
   struct QueryValueType
      {
      int ID;
      int requestedByID;
      };

   void pack(MessageData& messageData, const QueryValueType& data);
   void unpack(MessageData& messageData, QueryValueType& data);
   unsigned memorySize(const QueryValueType& data);
   std::string DDML(const QueryValueType& data);

   //------ QuerySetValue ------
   struct QuerySetValueType
      {
      int ID;
      std::string ddml;
      };

   void pack(MessageData& messageData, const QuerySetValueType& data);
   void unpack(MessageData& messageData, QuerySetValueType& data);
   unsigned memorySize(const QuerySetValueType& data);
   std::string DDML(const QuerySetValueType& data);

   //------ Layered ------
   struct LayeredType
      {
      std::vector<double> layer;
      std::vector<double> value;
      };

   void pack(MessageData& messageData, const LayeredType& data);
   void unpack(MessageData& messageData, LayeredType& data);
   unsigned memorySize(const LayeredType& data);
   std::string DDML(const LayeredType& data);

   //------ Time ------
   struct TimeType
      {
      int startday;
      int startsec;
      double startsecpart;
      int endday;
      int endsec;
      double endsecpart;
      };

   void pack(MessageData& messageData, const TimeType& data);
   void unpack(MessageData& messageData, TimeType& data);
   unsigned memorySize(const TimeType& data);
   std::string DDML(const TimeType& data);

   //------ NewMet ------
   struct NewMetType
      {
      double today;
      float radn;
      float maxt;
      float mint;
      float rain;
      float vp;
      };

   void pack(MessageData& messageData, const NewMetType& data);
   void unpack(MessageData& messageData, NewMetType& data);
   unsigned memorySize(const NewMetType& data);
   std::string DDML(const NewMetType& data);

   //------ SoilWaterProfileLayer ------
   struct SoilWaterProfileLayerType
      {
      float thickness;
      float BulkDensity;
      float SatDepth;
      float DULDepth;
      float LL15Depth;
      float AirDryDepth;
      float SWDepth;
      };

   void pack(MessageData& messageData, const SoilWaterProfileLayerType& data);
   void unpack(MessageData& messageData, SoilWaterProfileLayerType& data);
   unsigned memorySize(const SoilWaterProfileLayerType& data);
   std::string DDML(const SoilWaterProfileLayerType& data);

   //------ SoilWaterLayer ------
   struct SoilWaterLayerType
      {
      float thickness;
      float amount;
      };

   void pack(MessageData& messageData, const SoilWaterLayerType& data);
   void unpack(MessageData& messageData, SoilWaterLayerType& data);
   unsigned memorySize(const SoilWaterLayerType& data);
   std::string DDML(const SoilWaterLayerType& data);

   //------ LateralFlowLayer ------
   struct LateralFlowLayerType
      {
      float thickness;
      float amount;
      };

   void pack(MessageData& messageData, const LateralFlowLayerType& data);
   void unpack(MessageData& messageData, LateralFlowLayerType& data);
   unsigned memorySize(const LateralFlowLayerType& data);
   std::string DDML(const LateralFlowLayerType& data);

   //------ SoilWaterBalance ------
   struct SoilWaterBalanceType
      {
      float infiltration;
      float drainage;
      float evaporation;
      std::vector<LateralFlowLayerType> LateralFlowLayer;
      };

   void pack(MessageData& messageData, const SoilWaterBalanceType& data);
   void unpack(MessageData& messageData, SoilWaterBalanceType& data);
   unsigned memorySize(const SoilWaterBalanceType& data);
   std::string DDML(const SoilWaterBalanceType& data);

   //------ NewSolute ------
   struct NewSoluteType
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

   void pack(MessageData& messageData, const NewSoluteType& data);
   void unpack(MessageData& messageData, NewSoluteType& data);
   unsigned memorySize(const NewSoluteType& data);
   std::string DDML(const NewSoluteType& data);

   //------ layer ------
   struct layerType
      {
      float thickness;
      float amount;
      };

   void pack(MessageData& messageData, const layerType& data);
   void unpack(MessageData& messageData, layerType& data);
   unsigned memorySize(const layerType& data);
   std::string DDML(const layerType& data);

   //------ SoluteProfile ------
   struct SoluteProfileType
      {
      std::string name;
      std::vector<layerType> layer;
      };

   void pack(MessageData& messageData, const SoluteProfileType& data);
   void unpack(MessageData& messageData, SoluteProfileType& data);
   unsigned memorySize(const SoluteProfileType& data);
   std::string DDML(const SoluteProfileType& data);

   //------ Irrigated ------
   struct IrrigatedType
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
      std::string amount_name;
      int amount_numbytes;
      int amount_code;
      bool amount_isarray;
      float amount_value;
      };

   void pack(MessageData& messageData, const IrrigatedType& data);
   void unpack(MessageData& messageData, IrrigatedType& data);
   unsigned memorySize(const IrrigatedType& data);
   std::string DDML(const IrrigatedType& data);

   //------ CropWaterSupply ------
   struct CropWaterSupplyType
      {
      std::string name;
      std::vector<layerType> layer;
      };

   void pack(MessageData& messageData, const CropWaterSupplyType& data);
   void unpack(MessageData& messageData, CropWaterSupplyType& data);
   unsigned memorySize(const CropWaterSupplyType& data);
   std::string DDML(const CropWaterSupplyType& data);

   //------ RootLayer ------
   struct RootLayerType
      {
      float thickness;
      float RootLengthDensity;
      float PotentialUptake;
      };

   void pack(MessageData& messageData, const RootLayerType& data);
   void unpack(MessageData& messageData, RootLayerType& data);
   unsigned memorySize(const RootLayerType& data);
   std::string DDML(const RootLayerType& data);

   //------ CropWaterDemand ------
   struct CropWaterDemandType
      {
      std::string Name;
      std::string CropType;
      std::vector<RootLayerType> RootLayer;
      float amount;
      };

   void pack(MessageData& messageData, const CropWaterDemandType& data);
   void unpack(MessageData& messageData, CropWaterDemandType& data);
   unsigned memorySize(const CropWaterDemandType& data);
   std::string DDML(const CropWaterDemandType& data);

   //------ CropNitrogenDemand ------
   struct CropNitrogenDemandType
      {
      std::string Name;
      std::string CropType;
      std::vector<RootLayerType> RootLayer;
      float amount;
      };

   void pack(MessageData& messageData, const CropNitrogenDemandType& data);
   void unpack(MessageData& messageData, CropNitrogenDemandType& data);
   unsigned memorySize(const CropNitrogenDemandType& data);
   std::string DDML(const CropNitrogenDemandType& data);

   //------ CropNitrogenSupply ------
   struct CropNitrogenSupplyType
      {
      std::string name;
      std::vector<layerType> layer;
      };

   void pack(MessageData& messageData, const CropNitrogenSupplyType& data);
   void unpack(MessageData& messageData, CropNitrogenSupplyType& data);
   unsigned memorySize(const CropNitrogenSupplyType& data);
   std::string DDML(const CropNitrogenSupplyType& data);

   //------ Interception ------
   struct InterceptionType
      {
      std::string name;
      std::string CropType;
      std::vector<layerType> layer;
      };

   void pack(MessageData& messageData, const InterceptionType& data);
   void unpack(MessageData& messageData, InterceptionType& data);
   unsigned memorySize(const InterceptionType& data);
   std::string DDML(const InterceptionType& data);

   //------ LightProfile ------
   struct LightProfileType
      {
      std::vector<InterceptionType> Interception;
      float transmission;
      };

   void pack(MessageData& messageData, const LightProfileType& data);
   void unpack(MessageData& messageData, LightProfileType& data);
   unsigned memorySize(const LightProfileType& data);
   std::string DDML(const LightProfileType& data);

   //------ Canopy ------
   struct CanopyType
      {
      std::string name;
      std::string CropType;
      float PotentialEp;
      };

   void pack(MessageData& messageData, const CanopyType& data);
   void unpack(MessageData& messageData, CanopyType& data);
   unsigned memorySize(const CanopyType& data);
   std::string DDML(const CanopyType& data);

   //------ CanopyWaterBalance ------
   struct CanopyWaterBalanceType
      {
      std::vector<CanopyType> Canopy;
      float eo;
      float interception;
      };

   void pack(MessageData& messageData, const CanopyWaterBalanceType& data);
   void unpack(MessageData& messageData, CanopyWaterBalanceType& data);
   unsigned memorySize(const CanopyWaterBalanceType& data);
   std::string DDML(const CanopyWaterBalanceType& data);

   //------ OrganicMatterFraction ------
   struct OrganicMatterFractionType
      {
      float amount;
      float C;
      float N;
      float P;
      float AshAlk;
      };

   void pack(MessageData& messageData, const OrganicMatterFractionType& data);
   void unpack(MessageData& messageData, OrganicMatterFractionType& data);
   unsigned memorySize(const OrganicMatterFractionType& data);
   std::string DDML(const OrganicMatterFractionType& data);

   //------ Residue ------
   struct ResidueType
      {
      std::string name;
      std::string OrganicMatterType;
      std::vector<OrganicMatterFractionType> OrganicMatterFraction;
      float Cover;
      };

   void pack(MessageData& messageData, const ResidueType& data);
   void unpack(MessageData& messageData, ResidueType& data);
   unsigned memorySize(const ResidueType& data);
   std::string DDML(const ResidueType& data);

   //------ solute ------
   struct soluteType
      {
      std::string name;
      float amount;
      };

   void pack(MessageData& messageData, const soluteType& data);
   void unpack(MessageData& messageData, soluteType& data);
   unsigned memorySize(const soluteType& data);
   std::string DDML(const soluteType& data);

   //------ SurfaceWater ------
   struct SurfaceWaterType
      {
      float amount;
      std::vector<soluteType> solute;
      };

   void pack(MessageData& messageData, const SurfaceWaterType& data);
   void unpack(MessageData& messageData, SurfaceWaterType& data);
   unsigned memorySize(const SurfaceWaterType& data);
   std::string DDML(const SurfaceWaterType& data);

   //------ SurfaceWaterBalance ------
   struct SurfaceWaterBalanceType
      {
      float runoff;
      float evaporation;
      float runon;
      float WaterInput;
      };

   void pack(MessageData& messageData, const SurfaceWaterBalanceType& data);
   void unpack(MessageData& messageData, SurfaceWaterBalanceType& data);
   unsigned memorySize(const SurfaceWaterBalanceType& data);
   std::string DDML(const SurfaceWaterBalanceType& data);

   //------ FertiliserConstituents ------
   struct FertiliserConstituentsType
      {
      std::string name;
      float SurfaceAmount;
      std::vector<layerType> layer;
      };

   void pack(MessageData& messageData, const FertiliserConstituentsType& data);
   void unpack(MessageData& messageData, FertiliserConstituentsType& data);
   unsigned memorySize(const FertiliserConstituentsType& data);
   std::string DDML(const FertiliserConstituentsType& data);

   //------ FPool ------
   struct FPoolType
      {
      float C;
      float N;
      float P;
      float AshAlk;
      };

   void pack(MessageData& messageData, const FPoolType& data);
   void unpack(MessageData& messageData, FPoolType& data);
   unsigned memorySize(const FPoolType& data);
   std::string DDML(const FPoolType& data);

   //------ FPoolProfileLayer ------
   struct FPoolProfileLayerType
      {
      float thickness;
      float no3;
      float nh4;
      float po4;
      std::vector<FPoolType> FPool;
      };

   void pack(MessageData& messageData, const FPoolProfileLayerType& data);
   void unpack(MessageData& messageData, FPoolProfileLayerType& data);
   unsigned memorySize(const FPoolProfileLayerType& data);
   std::string DDML(const FPoolProfileLayerType& data);

   //------ StandingFraction ------
   struct StandingFractionType
      {
      float amount;
      float C;
      float N;
      float P;
      float AshAlk;
      };

   void pack(MessageData& messageData, const StandingFractionType& data);
   void unpack(MessageData& messageData, StandingFractionType& data);
   unsigned memorySize(const StandingFractionType& data);
   std::string DDML(const StandingFractionType& data);

   //------ LyingFraction ------
   struct LyingFractionType
      {
      float amount;
      float C;
      float N;
      float P;
      float AshAlk;
      };

   void pack(MessageData& messageData, const LyingFractionType& data);
   void unpack(MessageData& messageData, LyingFractionType& data);
   unsigned memorySize(const LyingFractionType& data);
   std::string DDML(const LyingFractionType& data);

   //------ SurfaceOrganicMatter ------
   struct SurfaceOrganicMatterType
      {
      std::string Name;
      std::string OrganicMatterType;
      float PotDecompRate;
      float no3;
      float nh4;
      float po4;
      std::vector<StandingFractionType> StandingFraction;
      std::vector<LyingFractionType> LyingFraction;
      };

   void pack(MessageData& messageData, const SurfaceOrganicMatterType& data);
   void unpack(MessageData& messageData, SurfaceOrganicMatterType& data);
   unsigned memorySize(const SurfaceOrganicMatterType& data);
   std::string DDML(const SurfaceOrganicMatterType& data);

   //------ SurfaceOrganicMatterDecomp ------
   struct SurfaceOrganicMatterDecompType
      {
      std::string Name;
      std::string OrganicMatterType;
      float amount;
      float C;
      float N;
      float P;
      float AshAlk;
      };

   void pack(MessageData& messageData, const SurfaceOrganicMatterDecompType& data);
   void unpack(MessageData& messageData, SurfaceOrganicMatterDecompType& data);
   unsigned memorySize(const SurfaceOrganicMatterDecompType& data);
   std::string DDML(const SurfaceOrganicMatterDecompType& data);

   //------ NBalance ------
   struct NBalanceType
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

   void pack(MessageData& messageData, const NBalanceType& data);
   void unpack(MessageData& messageData, NBalanceType& data);
   unsigned memorySize(const NBalanceType& data);
   std::string DDML(const NBalanceType& data);

   //------ CBalance ------
   struct CBalanceType
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

   void pack(MessageData& messageData, const CBalanceType& data);
   void unpack(MessageData& messageData, CBalanceType& data);
   unsigned memorySize(const CBalanceType& data);
   std::string DDML(const CBalanceType& data);

   //------ IncorpFom ------
   struct IncorpFomType
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

   void pack(MessageData& messageData, const IncorpFomType& data);
   void unpack(MessageData& messageData, IncorpFomType& data);
   unsigned memorySize(const IncorpFomType& data);
   std::string DDML(const IncorpFomType& data);

   //------ SoilOrganicMatter ------
   struct SoilOrganicMatterType
      {
      std::string OrganicMatterType;
      std::vector<layerType> layer;
      };

   void pack(MessageData& messageData, const SoilOrganicMatterType& data);
   void unpack(MessageData& messageData, SoilOrganicMatterType& data);
   unsigned memorySize(const SoilOrganicMatterType& data);
   std::string DDML(const SoilOrganicMatterType& data);

   //------ SoilNitrogenFlowsLayer ------
   struct SoilNitrogenFlowsLayerType
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

   void pack(MessageData& messageData, const SoilNitrogenFlowsLayerType& data);
   void unpack(MessageData& messageData, SoilNitrogenFlowsLayerType& data);
   unsigned memorySize(const SoilNitrogenFlowsLayerType& data);
   std::string DDML(const SoilNitrogenFlowsLayerType& data);

   //------ NitrogenBalance ------
   struct NitrogenBalanceType
      {
      float Fertilizer;
      float ResiduesMineralised;
      float Leaching;
      float Denitrification;
      float Uptake;
      float Erosion;
      float LateralFlow;
      };

   void pack(MessageData& messageData, const NitrogenBalanceType& data);
   void unpack(MessageData& messageData, NitrogenBalanceType& data);
   unsigned memorySize(const NitrogenBalanceType& data);
   std::string DDML(const NitrogenBalanceType& data);

   //------ CropChopped ------
   struct CropChoppedType
      {
      std::string crop_type;
      std::vector<std::string> dm_type;
      std::vector<float> dlt_crop_dm;
      std::vector<float> dlt_dm_n;
      std::vector<float> fraction_to_residue;
      };

   void pack(MessageData& messageData, const CropChoppedType& data);
   void unpack(MessageData& messageData, CropChoppedType& data);
   unsigned memorySize(const CropChoppedType& data);
   std::string DDML(const CropChoppedType& data);

   //------ NewProfile ------
   struct NewProfileType
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

   void pack(MessageData& messageData, const NewProfileType& data);
   void unpack(MessageData& messageData, NewProfileType& data);
   unsigned memorySize(const NewProfileType& data);
   std::string DDML(const NewProfileType& data);

   //------ NewPotentialGrowth ------
   struct NewPotentialGrowthType
      {
      std::string sender;
      float frgr;
      };

   void pack(MessageData& messageData, const NewPotentialGrowthType& data);
   void unpack(MessageData& messageData, NewPotentialGrowthType& data);
   unsigned memorySize(const NewPotentialGrowthType& data);
   std::string DDML(const NewPotentialGrowthType& data);

   //------ NewCanopy ------
   struct NewCanopyType
      {
      std::string sender;
      float height;
      float depth;
      float lai;
      float lai_tot;
      float cover;
      float cover_tot;
      };

   void pack(MessageData& messageData, const NewCanopyType& data);
   void unpack(MessageData& messageData, NewCanopyType& data);
   unsigned memorySize(const NewCanopyType& data);
   std::string DDML(const NewCanopyType& data);

   //------ NewCrop ------
   struct NewCropType
      {
      std::string sender;
      std::string crop_type;
      };

   void pack(MessageData& messageData, const NewCropType& data);
   void unpack(MessageData& messageData, NewCropType& data);
   unsigned memorySize(const NewCropType& data);
   std::string DDML(const NewCropType& data);

   //------ NewZone ------
   struct NewZoneType
      {
      std::string sender;
      float area;
      float slope;
      float X;
      float Y;
      };

   void pack(MessageData& messageData, const NewZoneType& data);
   void unpack(MessageData& messageData, NewZoneType& data);
   unsigned memorySize(const NewZoneType& data);
   std::string DDML(const NewZoneType& data);

   //------ SoilLayers ------
   struct SoilLayersType
      {
      std::vector<double> layers;
      std::vector<double> value;
      };

   void pack(MessageData& messageData, const SoilLayersType& data);
   void unpack(MessageData& messageData, SoilLayersType& data);
   unsigned memorySize(const SoilLayersType& data);
   std::string DDML(const SoilLayersType& data);

   //------ rlv_layer ------
   struct rlv_layerType
      {
      std::vector<double> layers;
      std::vector<double> rlv;
      };

   void pack(MessageData& messageData, const rlv_layerType& data);
   void unpack(MessageData& messageData, rlv_layerType& data);
   unsigned memorySize(const rlv_layerType& data);
   std::string DDML(const rlv_layerType& data);

   //------ demands ------
   struct demandsType
      {
      std::string crop_ident;
      std::string crop_type;
      rlv_layerType rlv_layer;
      double demand;
      };

   void pack(MessageData& messageData, const demandsType& data);
   void unpack(MessageData& messageData, demandsType& data);
   unsigned memorySize(const demandsType& data);
   std::string DDML(const demandsType& data);

   //------ PastureWaterDemand ------
   struct PastureWaterDemandType
      {
      std::vector<demandsType> demands;
      };

   void pack(MessageData& messageData, const PastureWaterDemandType& data);
   void unpack(MessageData& messageData, PastureWaterDemandType& data);
   unsigned memorySize(const PastureWaterDemandType& data);
   std::string DDML(const PastureWaterDemandType& data);

   //------ supplies ------
   struct suppliesType
      {
      std::string crop_ident;
      std::vector<double> layers;
      std::vector<double> supply;
      };

   void pack(MessageData& messageData, const suppliesType& data);
   void unpack(MessageData& messageData, suppliesType& data);
   unsigned memorySize(const suppliesType& data);
   std::string DDML(const suppliesType& data);

   //------ PastureWaterSupply ------
   struct PastureWaterSupplyType
      {
      std::vector<suppliesType> supplies;
      };

   void pack(MessageData& messageData, const PastureWaterSupplyType& data);
   void unpack(MessageData& messageData, PastureWaterSupplyType& data);
   unsigned memorySize(const PastureWaterSupplyType& data);
   std::string DDML(const PastureWaterSupplyType& data);

   //------ water_uptake ------
   struct water_uptakeType
      {
      std::string crop_ident;
      std::vector<double> layers;
      std::vector<double> uptake;
      };

   void pack(MessageData& messageData, const water_uptakeType& data);
   void unpack(MessageData& messageData, water_uptakeType& data);
   unsigned memorySize(const water_uptakeType& data);
   std::string DDML(const water_uptakeType& data);

   //------ PastureWaterUptake ------
   struct PastureWaterUptakeType
      {
      std::vector<water_uptakeType> water_uptake;
      };

   void pack(MessageData& messageData, const PastureWaterUptakeType& data);
   void unpack(MessageData& messageData, PastureWaterUptakeType& data);
   unsigned memorySize(const PastureWaterUptakeType& data);
   std::string DDML(const PastureWaterUptakeType& data);

   //------ water_info ------
   struct water_infoType
      {
      std::string crop_ident;
      std::vector<double> params;
      double demand;
      std::vector<double> layers;
      std::vector<double> rlv;
      std::vector<double> root_radius;
      };

   void pack(MessageData& messageData, const water_infoType& data);
   void unpack(MessageData& messageData, water_infoType& data);
   unsigned memorySize(const water_infoType& data);
   std::string DDML(const water_infoType& data);

   //------ WaterInfo ------
   struct WaterInfoType
      {
      std::vector<water_infoType> water_info;
      };

   void pack(MessageData& messageData, const WaterInfoType& data);
   void unpack(MessageData& messageData, WaterInfoType& data);
   unsigned memorySize(const WaterInfoType& data);
   std::string DDML(const WaterInfoType& data);

   //------ fom ------
   struct fomType
      {
      double weight;
      double n;
      double p;
      double s;
      double ash_alk;
      };

   void pack(MessageData& messageData, const fomType& data);
   void unpack(MessageData& messageData, fomType& data);
   unsigned memorySize(const fomType& data);
   std::string DDML(const fomType& data);

   //------ FomAdded ------
   struct FomAddedType
      {
      std::vector<double> layers;
      std::vector<fomType> fom;
      };

   void pack(MessageData& messageData, const FomAddedType& data);
   void unpack(MessageData& messageData, FomAddedType& data);
   unsigned memorySize(const FomAddedType& data);
   std::string DDML(const FomAddedType& data);

   //------ PastureNutrientUptake ------
   struct PastureNutrientUptakeType
      {
      std::string nutrient;
      std::vector<double> layers;
      std::vector<double> uptake;
      };

   void pack(MessageData& messageData, const PastureNutrientUptakeType& data);
   void unpack(MessageData& messageData, PastureNutrientUptakeType& data);
   unsigned memorySize(const PastureNutrientUptakeType& data);
   std::string DDML(const PastureNutrientUptakeType& data);

   //------ PastureSow ------
   struct PastureSowType
      {
      double rate;
      };

   void pack(MessageData& messageData, const PastureSowType& data);
   void unpack(MessageData& messageData, PastureSowType& data);
   unsigned memorySize(const PastureSowType& data);
   std::string DDML(const PastureSowType& data);

   //------ PastureKill ------
   struct PastureKillType
      {
      double propn_herbage;
      double propn_seeds;
      };

   void pack(MessageData& messageData, const PastureKillType& data);
   void unpack(MessageData& messageData, PastureKillType& data);
   unsigned memorySize(const PastureKillType& data);
   std::string DDML(const PastureKillType& data);

   //------ PastureCultivate ------
   struct PastureCultivateType
      {
      double depth;
      double propn_incorp;
      double propn_mixed;
      };

   void pack(MessageData& messageData, const PastureCultivateType& data);
   void unpack(MessageData& messageData, PastureCultivateType& data);
   unsigned memorySize(const PastureCultivateType& data);
   std::string DDML(const PastureCultivateType& data);

   //------ PastureCut ------
   struct PastureCutType
      {
      double cut_height;
      double gathered;
      double dmd_loss;
      double dm_content;
      };

   void pack(MessageData& messageData, const PastureCutType& data);
   void unpack(MessageData& messageData, PastureCutType& data);
   unsigned memorySize(const PastureCutType& data);
   std::string DDML(const PastureCutType& data);

   //------ PastureOnCut ------
   struct PastureOnCutType
      {
      double fresh_wt;
      double dm_content;
      double dm;
      double cp_conc;
      double p_conc;
      double s_conc;
      double ash_alk;
      };

   void pack(MessageData& messageData, const PastureOnCutType& data);
   void unpack(MessageData& messageData, PastureOnCutType& data);
   unsigned memorySize(const PastureOnCutType& data);
   std::string DDML(const PastureOnCutType& data);

   //------ PastureWeather ------
   struct PastureWeatherType
      {
      double maxt;
      double mint;
      double rain;
      double snow;
      double radn;
      double vpd;
      double wind;
      };

   void pack(MessageData& messageData, const PastureWeatherType& data);
   void unpack(MessageData& messageData, PastureWeatherType& data);
   unsigned memorySize(const PastureWeatherType& data);
   std::string DDML(const PastureWeatherType& data);

   //------ Faeces ------
   struct FaecesType
      {
      double weight;
      double n;
      double p;
      double s;
      double ash_alk;
      };

   void pack(MessageData& messageData, const FaecesType& data);
   void unpack(MessageData& messageData, FaecesType& data);
   unsigned memorySize(const FaecesType& data);
   std::string DDML(const FaecesType& data);

   //------ FaecesInorg ------
   struct FaecesInorgType
      {
      double n;
      double p;
      double s;
      };

   void pack(MessageData& messageData, const FaecesInorgType& data);
   void unpack(MessageData& messageData, FaecesInorgType& data);
   unsigned memorySize(const FaecesInorgType& data);
   std::string DDML(const FaecesInorgType& data);

   //------ Intake ------
   struct IntakeType
      {
      double weight;
      double n;
      double p;
      double s;
      double ash_alk;
      };

   void pack(MessageData& messageData, const IntakeType& data);
   void unpack(MessageData& messageData, IntakeType& data);
   unsigned memorySize(const IntakeType& data);
   std::string DDML(const IntakeType& data);

   //------ PastIntake ------
   struct PastIntakeType
      {
      double weight;
      double n;
      double p;
      double s;
      double ash_alk;
      };

   void pack(MessageData& messageData, const PastIntakeType& data);
   void unpack(MessageData& messageData, PastIntakeType& data);
   unsigned memorySize(const PastIntakeType& data);
   std::string DDML(const PastIntakeType& data);

   //------ SuppIntake ------
   struct SuppIntakeType
      {
      double weight;
      double n;
      double p;
      double s;
      double ash_alk;
      };

   void pack(MessageData& messageData, const SuppIntakeType& data);
   void unpack(MessageData& messageData, SuppIntakeType& data);
   unsigned memorySize(const SuppIntakeType& data);
   std::string DDML(const SuppIntakeType& data);

   //------ faeces_om ------
   struct faeces_omType
      {
      double weight;
      double n;
      double p;
      double s;
      double ash_alk;
      };

   void pack(MessageData& messageData, const faeces_omType& data);
   void unpack(MessageData& messageData, faeces_omType& data);
   unsigned memorySize(const faeces_omType& data);
   std::string DDML(const faeces_omType& data);

   //------ faeces_inorg ------
   struct faeces_inorgType
      {
      double n;
      double p;
      double s;
      };

   void pack(MessageData& messageData, const faeces_inorgType& data);
   void unpack(MessageData& messageData, faeces_inorgType& data);
   unsigned memorySize(const faeces_inorgType& data);
   std::string DDML(const faeces_inorgType& data);

   //------ urine ------
   struct urineType
      {
      double volume;
      double urea;
      double pox;
      double so4;
      double ash_alk;
      };

   void pack(MessageData& messageData, const urineType& data);
   void unpack(MessageData& messageData, urineType& data);
   unsigned memorySize(const urineType& data);
   std::string DDML(const urineType& data);

   //------ AddExcreta ------
   struct AddExcretaType
      {
      faeces_omType faeces_om;
      faeces_inorgType faeces_inorg;
      urineType urine;
      };

   void pack(MessageData& messageData, const AddExcretaType& data);
   void unpack(MessageData& messageData, AddExcretaType& data);
   unsigned memorySize(const AddExcretaType& data);
   std::string DDML(const AddExcretaType& data);

   //------ RemoveHerbage ------
   struct RemoveHerbageType
      {
      std::vector<double> herbage;
      std::vector<double> seed;
      };

   void pack(MessageData& messageData, const RemoveHerbageType& data);
   void unpack(MessageData& messageData, RemoveHerbageType& data);
   unsigned memorySize(const RemoveHerbageType& data);
   std::string DDML(const RemoveHerbageType& data);

   //------ SuppEaten ------
   struct SuppEatenType
      {
      std::string paddock;
      double eaten;
      };

   void pack(MessageData& messageData, const SuppEatenType& data);
   void unpack(MessageData& messageData, SuppEatenType& data);
   unsigned memorySize(const SuppEatenType& data);
   std::string DDML(const SuppEatenType& data);

   //------ herbage ------
   struct herbageType
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

   void pack(MessageData& messageData, const herbageType& data);
   void unpack(MessageData& messageData, herbageType& data);
   unsigned memorySize(const herbageType& data);
   std::string DDML(const herbageType& data);

   //------ seed ------
   struct seedType
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

   void pack(MessageData& messageData, const seedType& data);
   void unpack(MessageData& messageData, seedType& data);
   unsigned memorySize(const seedType& data);
   std::string DDML(const seedType& data);

   //------ Plant2Stock ------
   struct Plant2StockType
      {
      std::vector<herbageType> herbage;
      double propn_green;
      double legume;
      double select_factor;
      std::vector<seedType> seed;
      std::vector<int> seed_class;
      };

   void pack(MessageData& messageData, const Plant2StockType& data);
   void unpack(MessageData& messageData, Plant2StockType& data);
   unsigned memorySize(const Plant2StockType& data);
   std::string DDML(const Plant2StockType& data);

   //------ BuyStock ------
   struct BuyStockType
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

   void pack(MessageData& messageData, const BuyStockType& data);
   void unpack(MessageData& messageData, BuyStockType& data);
   unsigned memorySize(const BuyStockType& data);
   std::string DDML(const BuyStockType& data);

   //------ SellStock ------
   struct SellStockType
      {
      int group;
      int number;
      };

   void pack(MessageData& messageData, const SellStockType& data);
   void unpack(MessageData& messageData, SellStockType& data);
   unsigned memorySize(const SellStockType& data);
   std::string DDML(const SellStockType& data);

   //------ CastrateStock ------
   struct CastrateStockType
      {
      int group;
      int number;
      };

   void pack(MessageData& messageData, const CastrateStockType& data);
   void unpack(MessageData& messageData, CastrateStockType& data);
   unsigned memorySize(const CastrateStockType& data);
   std::string DDML(const CastrateStockType& data);

   //------ DryOffStock ------
   struct DryOffStockType
      {
      int group;
      int number;
      };

   void pack(MessageData& messageData, const DryOffStockType& data);
   void unpack(MessageData& messageData, DryOffStockType& data);
   unsigned memorySize(const DryOffStockType& data);
   std::string DDML(const DryOffStockType& data);

   //------ JoinStock ------
   struct JoinStockType
      {
      int group;
      std::string mate_to;
      int mate_days;
      };

   void pack(MessageData& messageData, const JoinStockType& data);
   void unpack(MessageData& messageData, JoinStockType& data);
   unsigned memorySize(const JoinStockType& data);
   std::string DDML(const JoinStockType& data);

   //------ MoveStock ------
   struct MoveStockType
      {
      int group;
      std::string paddock;
      };

   void pack(MessageData& messageData, const MoveStockType& data);
   void unpack(MessageData& messageData, MoveStockType& data);
   unsigned memorySize(const MoveStockType& data);
   std::string DDML(const MoveStockType& data);

   //------ ShearStock ------
   struct ShearStockType
      {
      int group;
      std::string sub_group;
      };

   void pack(MessageData& messageData, const ShearStockType& data);
   void unpack(MessageData& messageData, ShearStockType& data);
   unsigned memorySize(const ShearStockType& data);
   std::string DDML(const ShearStockType& data);

   //------ SplitStock ------
   struct SplitStockType
      {
      int group;
      std::string type;
      double value;
      };

   void pack(MessageData& messageData, const SplitStockType& data);
   void unpack(MessageData& messageData, SplitStockType& data);
   unsigned memorySize(const SplitStockType& data);
   std::string DDML(const SplitStockType& data);

   //------ TagStock ------
   struct TagStockType
      {
      int group;
      int value;
      };

   void pack(MessageData& messageData, const TagStockType& data);
   void unpack(MessageData& messageData, TagStockType& data);
   unsigned memorySize(const TagStockType& data);
   std::string DDML(const TagStockType& data);

   //------ WeanStock ------
   struct WeanStockType
      {
      int group;
      std::string sex;
      int number;
      };

   void pack(MessageData& messageData, const WeanStockType& data);
   void unpack(MessageData& messageData, WeanStockType& data);
   unsigned memorySize(const WeanStockType& data);
   std::string DDML(const WeanStockType& data);

   //------ dm ------
   struct dmType
      {
      std::string pool;
      std::vector<std::string> part;
      std::vector<double> dlt;
      };

   void pack(MessageData& messageData, const dmType& data);
   void unpack(MessageData& messageData, dmType& data);
   unsigned memorySize(const dmType& data);
   std::string DDML(const dmType& data);

   //------ RemoveCropDm ------
   struct RemoveCropDmType
      {
      std::vector<dmType> dm;
      };

   void pack(MessageData& messageData, const RemoveCropDmType& data);
   void unpack(MessageData& messageData, RemoveCropDmType& data);
   unsigned memorySize(const RemoveCropDmType& data);
   std::string DDML(const RemoveCropDmType& data);

   //------ RemoveResidueDm ------
   struct RemoveResidueDmType
      {
      std::vector<std::string> dm_type;
      std::vector<float> dlt_residue_dm;
      };

   void pack(MessageData& messageData, const RemoveResidueDmType& data);
   void unpack(MessageData& messageData, RemoveResidueDmType& data);
   unsigned memorySize(const RemoveResidueDmType& data);
   std::string DDML(const RemoveResidueDmType& data);

   //------ SupplementBuy ------
   struct SupplementBuyType
      {
      std::string supplement;
      double amount;
      };

   void pack(MessageData& messageData, const SupplementBuyType& data);
   void unpack(MessageData& messageData, SupplementBuyType& data);
   unsigned memorySize(const SupplementBuyType& data);
   std::string DDML(const SupplementBuyType& data);

   //------ SupplementFeed ------
   struct SupplementFeedType
      {
      std::string supplement;
      double amount;
      std::string paddock;
      };

   void pack(MessageData& messageData, const SupplementFeedType& data);
   void unpack(MessageData& messageData, SupplementFeedType& data);
   unsigned memorySize(const SupplementFeedType& data);
   std::string DDML(const SupplementFeedType& data);

   //------ SupplementMix ------
   struct SupplementMixType
      {
      std::string src_store;
      double amount;
      std::string dest_store;
      };

   void pack(MessageData& messageData, const SupplementMixType& data);
   void unpack(MessageData& messageData, SupplementMixType& data);
   unsigned memorySize(const SupplementMixType& data);
   std::string DDML(const SupplementMixType& data);

#endif

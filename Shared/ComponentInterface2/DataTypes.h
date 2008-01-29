#ifndef DataTypesH
#define DataTypesH
#include <general/platform.h>
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

   void EXPORT pack(MessageData& messageData, const Null& data);
   void EXPORT unpack(MessageData& messageData, Null& data);
   unsigned EXPORT memorySize(const Null& data);
   std::string EXPORT DDML(const Null& data);

   //------ Complete ------
   struct CompleteType
      {
      int ackID;
      };

   void EXPORT pack(MessageData& messageData, const CompleteType& data);
   void EXPORT unpack(MessageData& messageData, CompleteType& data);
   unsigned EXPORT memorySize(const CompleteType& data);
   std::string EXPORT DDML(const CompleteType& data);

   //------ Error ------
   struct ErrorType
      {
      bool isFatal;
      std::string msg;
      };

   void EXPORT pack(MessageData& messageData, const ErrorType& data);
   void EXPORT unpack(MessageData& messageData, ErrorType& data);
   unsigned EXPORT memorySize(const ErrorType& data);
   std::string EXPORT DDML(const ErrorType& data);

   //------ Event ------
   struct EventType
      {
      int ID;
      int publishedBy;
      std::string ddml;
      };

   void EXPORT pack(MessageData& messageData, const EventType& data);
   void EXPORT unpack(MessageData& messageData, EventType& data);
   unsigned EXPORT memorySize(const EventType& data);
   std::string EXPORT DDML(const EventType& data);

   //------ GetValue ------
   struct GetValueType
      {
      int ID;
      };

   void EXPORT pack(MessageData& messageData, const GetValueType& data);
   void EXPORT unpack(MessageData& messageData, GetValueType& data);
   unsigned EXPORT memorySize(const GetValueType& data);
   std::string EXPORT DDML(const GetValueType& data);

   //------ Init1 ------
   struct Init1Type
      {
      std::string sdml;
      std::string fqn;
      bool inStartup;
      };

   void EXPORT pack(MessageData& messageData, const Init1Type& data);
   void EXPORT unpack(MessageData& messageData, Init1Type& data);
   unsigned EXPORT memorySize(const Init1Type& data);
   std::string EXPORT DDML(const Init1Type& data);

   //------ NotifySetValueSuccess ------
   struct NotifySetValueSuccessType
      {
      int ID;
      bool success;
      };

   void EXPORT pack(MessageData& messageData, const NotifySetValueSuccessType& data);
   void EXPORT unpack(MessageData& messageData, NotifySetValueSuccessType& data);
   unsigned EXPORT memorySize(const NotifySetValueSuccessType& data);
   std::string EXPORT DDML(const NotifySetValueSuccessType& data);

   //------ PublishEvent ------
   struct PublishEventType
      {
      int ID;
      std::string ddml;
      };

   void EXPORT pack(MessageData& messageData, const PublishEventType& data);
   void EXPORT unpack(MessageData& messageData, PublishEventType& data);
   unsigned EXPORT memorySize(const PublishEventType& data);
   std::string EXPORT DDML(const PublishEventType& data);

   //------ QueryInfo ------
   struct QueryInfoType
      {
      std::string name;
      int kind;
      };

   void EXPORT pack(MessageData& messageData, const QueryInfoType& data);
   void EXPORT unpack(MessageData& messageData, QueryInfoType& data);
   unsigned EXPORT memorySize(const QueryInfoType& data);
   std::string EXPORT DDML(const QueryInfoType& data);

   //------ Register ------
   struct RegisterType
      {
      int kind;
      int ID;
      int destID;
      std::string name;
      std::string ddml;
      };

   void EXPORT pack(MessageData& messageData, const RegisterType& data);
   void EXPORT unpack(MessageData& messageData, RegisterType& data);
   unsigned EXPORT memorySize(const RegisterType& data);
   std::string EXPORT DDML(const RegisterType& data);

   //------ ReplyValue ------
   struct ReplyValueType
      {
      int queryID;
      std::string ddml;
      };

   void EXPORT pack(MessageData& messageData, const ReplyValueType& data);
   void EXPORT unpack(MessageData& messageData, ReplyValueType& data);
   unsigned EXPORT memorySize(const ReplyValueType& data);
   std::string EXPORT DDML(const ReplyValueType& data);

   //------ RequestSetValue ------
   struct RequestSetValueType
      {
      int ID;
      std::string ddml;
      };

   void EXPORT pack(MessageData& messageData, const RequestSetValueType& data);
   void EXPORT unpack(MessageData& messageData, RequestSetValueType& data);
   unsigned EXPORT memorySize(const RequestSetValueType& data);
   std::string EXPORT DDML(const RequestSetValueType& data);

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

   void EXPORT pack(MessageData& messageData, const ReturnInfoType& data);
   void EXPORT unpack(MessageData& messageData, ReturnInfoType& data);
   unsigned EXPORT memorySize(const ReturnInfoType& data);
   std::string EXPORT DDML(const ReturnInfoType& data);

   //------ ReturnValue ------
   struct ReturnValueType
      {
      int compID;
      int ID;
      std::string ddml;
      };

   void EXPORT pack(MessageData& messageData, const ReturnValueType& data);
   void EXPORT unpack(MessageData& messageData, ReturnValueType& data);
   unsigned EXPORT memorySize(const ReturnValueType& data);
   std::string EXPORT DDML(const ReturnValueType& data);

   //------ QueryValue ------
   struct QueryValueType
      {
      int ID;
      int requestedByID;
      };

   void EXPORT pack(MessageData& messageData, const QueryValueType& data);
   void EXPORT unpack(MessageData& messageData, QueryValueType& data);
   unsigned EXPORT memorySize(const QueryValueType& data);
   std::string EXPORT DDML(const QueryValueType& data);

   //------ QuerySetValue ------
   struct QuerySetValueType
      {
      int ID;
      std::string ddml;
      };

   void EXPORT pack(MessageData& messageData, const QuerySetValueType& data);
   void EXPORT unpack(MessageData& messageData, QuerySetValueType& data);
   unsigned EXPORT memorySize(const QuerySetValueType& data);
   std::string EXPORT DDML(const QuerySetValueType& data);

   //------ Layered ------
   struct LayeredType
      {
      std::vector<double> layer;
      std::vector<double> value;
      };

   void EXPORT pack(MessageData& messageData, const LayeredType& data);
   void EXPORT unpack(MessageData& messageData, LayeredType& data);
   unsigned EXPORT memorySize(const LayeredType& data);
   std::string EXPORT DDML(const LayeredType& data);

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

   void EXPORT pack(MessageData& messageData, const TimeType& data);
   void EXPORT unpack(MessageData& messageData, TimeType& data);
   unsigned EXPORT memorySize(const TimeType& data);
   std::string EXPORT DDML(const TimeType& data);

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

   void EXPORT pack(MessageData& messageData, const NewMetType& data);
   void EXPORT unpack(MessageData& messageData, NewMetType& data);
   unsigned EXPORT memorySize(const NewMetType& data);
   std::string EXPORT DDML(const NewMetType& data);

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

   void EXPORT pack(MessageData& messageData, const SoilWaterProfileLayerType& data);
   void EXPORT unpack(MessageData& messageData, SoilWaterProfileLayerType& data);
   unsigned EXPORT memorySize(const SoilWaterProfileLayerType& data);
   std::string EXPORT DDML(const SoilWaterProfileLayerType& data);

   //------ SoilWaterLayer ------
   struct SoilWaterLayerType
      {
      float thickness;
      float amount;
      };

   void EXPORT pack(MessageData& messageData, const SoilWaterLayerType& data);
   void EXPORT unpack(MessageData& messageData, SoilWaterLayerType& data);
   unsigned EXPORT memorySize(const SoilWaterLayerType& data);
   std::string EXPORT DDML(const SoilWaterLayerType& data);

   //------ LateralFlowLayer ------
   struct LateralFlowLayerType
      {
      float thickness;
      float amount;
      };

   void EXPORT pack(MessageData& messageData, const LateralFlowLayerType& data);
   void EXPORT unpack(MessageData& messageData, LateralFlowLayerType& data);
   unsigned EXPORT memorySize(const LateralFlowLayerType& data);
   std::string EXPORT DDML(const LateralFlowLayerType& data);

   //------ SoilWaterBalance ------
   struct SoilWaterBalanceType
      {
      float infiltration;
      float drainage;
      float evaporation;
      std::vector<LateralFlowLayerType> LateralFlowLayer;
      };

   void EXPORT pack(MessageData& messageData, const SoilWaterBalanceType& data);
   void EXPORT unpack(MessageData& messageData, SoilWaterBalanceType& data);
   unsigned EXPORT memorySize(const SoilWaterBalanceType& data);
   std::string EXPORT DDML(const SoilWaterBalanceType& data);

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

   void EXPORT pack(MessageData& messageData, const NewSoluteType& data);
   void EXPORT unpack(MessageData& messageData, NewSoluteType& data);
   unsigned EXPORT memorySize(const NewSoluteType& data);
   std::string EXPORT DDML(const NewSoluteType& data);

   //------ layer ------
   struct layerType
      {
      float thickness;
      float amount;
      };

   void EXPORT pack(MessageData& messageData, const layerType& data);
   void EXPORT unpack(MessageData& messageData, layerType& data);
   unsigned EXPORT memorySize(const layerType& data);
   std::string EXPORT DDML(const layerType& data);

   //------ SoluteProfile ------
   struct SoluteProfileType
      {
      std::string name;
      std::vector<layerType> layer;
      };

   void EXPORT pack(MessageData& messageData, const SoluteProfileType& data);
   void EXPORT unpack(MessageData& messageData, SoluteProfileType& data);
   unsigned EXPORT memorySize(const SoluteProfileType& data);
   std::string EXPORT DDML(const SoluteProfileType& data);

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

   void EXPORT pack(MessageData& messageData, const IrrigatedType& data);
   void EXPORT unpack(MessageData& messageData, IrrigatedType& data);
   unsigned EXPORT memorySize(const IrrigatedType& data);
   std::string EXPORT DDML(const IrrigatedType& data);

   //------ KillCrop ------
   struct KillCropType
      {
      float KillFraction;
      };

   void EXPORT pack(MessageData& messageData, const KillCropType& data);
   void EXPORT unpack(MessageData& messageData, KillCropType& data);
   unsigned EXPORT memorySize(const KillCropType& data);
   std::string EXPORT DDML(const KillCropType& data);

   //------ Interception ------
   struct InterceptionType
      {
      std::string name;
      std::string CropType;
      std::vector<layerType> layer;
      };

   void EXPORT pack(MessageData& messageData, const InterceptionType& data);
   void EXPORT unpack(MessageData& messageData, InterceptionType& data);
   unsigned EXPORT memorySize(const InterceptionType& data);
   std::string EXPORT DDML(const InterceptionType& data);

   //------ LightProfile ------
   struct LightProfileType
      {
      std::vector<InterceptionType> Interception;
      float transmission;
      };

   void EXPORT pack(MessageData& messageData, const LightProfileType& data);
   void EXPORT unpack(MessageData& messageData, LightProfileType& data);
   unsigned EXPORT memorySize(const LightProfileType& data);
   std::string EXPORT DDML(const LightProfileType& data);

   //------ Canopy ------
   struct CanopyType
      {
      std::string name;
      std::string CropType;
      float PotentialEp;
      };

   void EXPORT pack(MessageData& messageData, const CanopyType& data);
   void EXPORT unpack(MessageData& messageData, CanopyType& data);
   unsigned EXPORT memorySize(const CanopyType& data);
   std::string EXPORT DDML(const CanopyType& data);

   //------ CanopyWaterBalance ------
   struct CanopyWaterBalanceType
      {
      std::vector<CanopyType> Canopy;
      float eo;
      float interception;
      };

   void EXPORT pack(MessageData& messageData, const CanopyWaterBalanceType& data);
   void EXPORT unpack(MessageData& messageData, CanopyWaterBalanceType& data);
   unsigned EXPORT memorySize(const CanopyWaterBalanceType& data);
   std::string EXPORT DDML(const CanopyWaterBalanceType& data);

   //------ OrganicMatterFraction ------
   struct OrganicMatterFractionType
      {
      float amount;
      float C;
      float N;
      float P;
      float AshAlk;
      };

   void EXPORT pack(MessageData& messageData, const OrganicMatterFractionType& data);
   void EXPORT unpack(MessageData& messageData, OrganicMatterFractionType& data);
   unsigned EXPORT memorySize(const OrganicMatterFractionType& data);
   std::string EXPORT DDML(const OrganicMatterFractionType& data);

   //------ Residue ------
   struct ResidueType
      {
      std::string name;
      std::string OrganicMatterType;
      std::vector<OrganicMatterFractionType> OrganicMatterFraction;
      float Cover;
      };

   void EXPORT pack(MessageData& messageData, const ResidueType& data);
   void EXPORT unpack(MessageData& messageData, ResidueType& data);
   unsigned EXPORT memorySize(const ResidueType& data);
   std::string EXPORT DDML(const ResidueType& data);

   //------ solute ------
   struct soluteType
      {
      std::string name;
      float amount;
      };

   void EXPORT pack(MessageData& messageData, const soluteType& data);
   void EXPORT unpack(MessageData& messageData, soluteType& data);
   unsigned EXPORT memorySize(const soluteType& data);
   std::string EXPORT DDML(const soluteType& data);

   //------ SurfaceWater ------
   struct SurfaceWaterType
      {
      float amount;
      std::vector<soluteType> solute;
      };

   void EXPORT pack(MessageData& messageData, const SurfaceWaterType& data);
   void EXPORT unpack(MessageData& messageData, SurfaceWaterType& data);
   unsigned EXPORT memorySize(const SurfaceWaterType& data);
   std::string EXPORT DDML(const SurfaceWaterType& data);

   //------ SurfaceWaterBalance ------
   struct SurfaceWaterBalanceType
      {
      float runoff;
      float evaporation;
      float runon;
      float WaterInput;
      };

   void EXPORT pack(MessageData& messageData, const SurfaceWaterBalanceType& data);
   void EXPORT unpack(MessageData& messageData, SurfaceWaterBalanceType& data);
   unsigned EXPORT memorySize(const SurfaceWaterBalanceType& data);
   std::string EXPORT DDML(const SurfaceWaterBalanceType& data);

   //------ FertiliserConstituents ------
   struct FertiliserConstituentsType
      {
      std::string name;
      float SurfaceAmount;
      std::vector<layerType> layer;
      };

   void EXPORT pack(MessageData& messageData, const FertiliserConstituentsType& data);
   void EXPORT unpack(MessageData& messageData, FertiliserConstituentsType& data);
   unsigned EXPORT memorySize(const FertiliserConstituentsType& data);
   std::string EXPORT DDML(const FertiliserConstituentsType& data);

   //------ FPool ------
   struct FPoolType
      {
      float C;
      float N;
      float P;
      float AshAlk;
      };

   void EXPORT pack(MessageData& messageData, const FPoolType& data);
   void EXPORT unpack(MessageData& messageData, FPoolType& data);
   unsigned EXPORT memorySize(const FPoolType& data);
   std::string EXPORT DDML(const FPoolType& data);

   //------ FPoolProfileLayer ------
   struct FPoolProfileLayerType
      {
      float thickness;
      float no3;
      float nh4;
      float po4;
      std::vector<FPoolType> FPool;
      };

   void EXPORT pack(MessageData& messageData, const FPoolProfileLayerType& data);
   void EXPORT unpack(MessageData& messageData, FPoolProfileLayerType& data);
   unsigned EXPORT memorySize(const FPoolProfileLayerType& data);
   std::string EXPORT DDML(const FPoolProfileLayerType& data);

   //------ StandingFraction ------
   struct StandingFractionType
      {
      float amount;
      float C;
      float N;
      float P;
      float AshAlk;
      };

   void EXPORT pack(MessageData& messageData, const StandingFractionType& data);
   void EXPORT unpack(MessageData& messageData, StandingFractionType& data);
   unsigned EXPORT memorySize(const StandingFractionType& data);
   std::string EXPORT DDML(const StandingFractionType& data);

   //------ LyingFraction ------
   struct LyingFractionType
      {
      float amount;
      float C;
      float N;
      float P;
      float AshAlk;
      };

   void EXPORT pack(MessageData& messageData, const LyingFractionType& data);
   void EXPORT unpack(MessageData& messageData, LyingFractionType& data);
   unsigned EXPORT memorySize(const LyingFractionType& data);
   std::string EXPORT DDML(const LyingFractionType& data);

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

   void EXPORT pack(MessageData& messageData, const SurfaceOrganicMatterType& data);
   void EXPORT unpack(MessageData& messageData, SurfaceOrganicMatterType& data);
   unsigned EXPORT memorySize(const SurfaceOrganicMatterType& data);
   std::string EXPORT DDML(const SurfaceOrganicMatterType& data);

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

   void EXPORT pack(MessageData& messageData, const SurfaceOrganicMatterDecompType& data);
   void EXPORT unpack(MessageData& messageData, SurfaceOrganicMatterDecompType& data);
   unsigned EXPORT memorySize(const SurfaceOrganicMatterDecompType& data);
   std::string EXPORT DDML(const SurfaceOrganicMatterDecompType& data);

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

   void EXPORT pack(MessageData& messageData, const NBalanceType& data);
   void EXPORT unpack(MessageData& messageData, NBalanceType& data);
   unsigned EXPORT memorySize(const NBalanceType& data);
   std::string EXPORT DDML(const NBalanceType& data);

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

   void EXPORT pack(MessageData& messageData, const CBalanceType& data);
   void EXPORT unpack(MessageData& messageData, CBalanceType& data);
   unsigned EXPORT memorySize(const CBalanceType& data);
   std::string EXPORT DDML(const CBalanceType& data);

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

   void EXPORT pack(MessageData& messageData, const IncorpFomType& data);
   void EXPORT unpack(MessageData& messageData, IncorpFomType& data);
   unsigned EXPORT memorySize(const IncorpFomType& data);
   std::string EXPORT DDML(const IncorpFomType& data);

   //------ SoilOrganicMatter ------
   struct SoilOrganicMatterType
      {
      std::string OrganicMatterType;
      std::vector<layerType> layer;
      };

   void EXPORT pack(MessageData& messageData, const SoilOrganicMatterType& data);
   void EXPORT unpack(MessageData& messageData, SoilOrganicMatterType& data);
   unsigned EXPORT memorySize(const SoilOrganicMatterType& data);
   std::string EXPORT DDML(const SoilOrganicMatterType& data);

   //------ CropChopped ------
   struct CropChoppedType
      {
      std::string crop_type;
      std::vector<std::string> dm_type;
      std::vector<float> dlt_crop_dm;
      std::vector<float> dlt_dm_n;
      std::vector<float> fraction_to_residue;
      };

   void EXPORT pack(MessageData& messageData, const CropChoppedType& data);
   void EXPORT unpack(MessageData& messageData, CropChoppedType& data);
   unsigned EXPORT memorySize(const CropChoppedType& data);
   std::string EXPORT DDML(const CropChoppedType& data);

   //------ NewProfile ------
   struct NewProfileType
      {
      std::vector<float> dlayer;
      std::vector<float> air_dry_dep;
      std::vector<float> ll15_dep;
      std::vector<float> dul_dep;
      std::vector<float> sat_dep;
      std::vector<float> sw_dep;
      std::vector<float> bd;
      };

   void EXPORT pack(MessageData& messageData, const NewProfileType& data);
   void EXPORT unpack(MessageData& messageData, NewProfileType& data);
   unsigned EXPORT memorySize(const NewProfileType& data);
   std::string EXPORT DDML(const NewProfileType& data);

   //------ NewPotentialGrowth ------
   struct NewPotentialGrowthType
      {
      std::string sender;
      float frgr;
      };

   void EXPORT pack(MessageData& messageData, const NewPotentialGrowthType& data);
   void EXPORT unpack(MessageData& messageData, NewPotentialGrowthType& data);
   unsigned EXPORT memorySize(const NewPotentialGrowthType& data);
   std::string EXPORT DDML(const NewPotentialGrowthType& data);

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

   void EXPORT pack(MessageData& messageData, const NewCanopyType& data);
   void EXPORT unpack(MessageData& messageData, NewCanopyType& data);
   unsigned EXPORT memorySize(const NewCanopyType& data);
   std::string EXPORT DDML(const NewCanopyType& data);

   //------ NewCrop ------
   struct NewCropType
      {
      std::string sender;
      std::string crop_type;
      };

   void EXPORT pack(MessageData& messageData, const NewCropType& data);
   void EXPORT unpack(MessageData& messageData, NewCropType& data);
   unsigned EXPORT memorySize(const NewCropType& data);
   std::string EXPORT DDML(const NewCropType& data);

   //------ NewZone ------
   struct NewZoneType
      {
      std::string sender;
      float area;
      float slope;
      float X;
      float Y;
      };

   void EXPORT pack(MessageData& messageData, const NewZoneType& data);
   void EXPORT unpack(MessageData& messageData, NewZoneType& data);
   unsigned EXPORT memorySize(const NewZoneType& data);
   std::string EXPORT DDML(const NewZoneType& data);

   //------ SoilLayers ------
   struct SoilLayersType
      {
      std::vector<double> layers;
      std::vector<double> value;
      };

   void EXPORT pack(MessageData& messageData, const SoilLayersType& data);
   void EXPORT unpack(MessageData& messageData, SoilLayersType& data);
   unsigned EXPORT memorySize(const SoilLayersType& data);
   std::string EXPORT DDML(const SoilLayersType& data);

   //------ rlv_layer ------
   struct rlv_layerType
      {
      std::vector<double> layers;
      std::vector<double> rlv;
      };

   void EXPORT pack(MessageData& messageData, const rlv_layerType& data);
   void EXPORT unpack(MessageData& messageData, rlv_layerType& data);
   unsigned EXPORT memorySize(const rlv_layerType& data);
   std::string EXPORT DDML(const rlv_layerType& data);

   //------ demands ------
   struct demandsType
      {
      std::string crop_ident;
      std::string crop_type;
      rlv_layerType rlv_layer;
      double demand;
      };

   void EXPORT pack(MessageData& messageData, const demandsType& data);
   void EXPORT unpack(MessageData& messageData, demandsType& data);
   unsigned EXPORT memorySize(const demandsType& data);
   std::string EXPORT DDML(const demandsType& data);

   //------ PastureWaterDemand ------
   struct PastureWaterDemandType
      {
      std::vector<demandsType> demands;
      };

   void EXPORT pack(MessageData& messageData, const PastureWaterDemandType& data);
   void EXPORT unpack(MessageData& messageData, PastureWaterDemandType& data);
   unsigned EXPORT memorySize(const PastureWaterDemandType& data);
   std::string EXPORT DDML(const PastureWaterDemandType& data);

   //------ supplies ------
   struct suppliesType
      {
      std::string crop_ident;
      std::vector<double> layers;
      std::vector<double> supply;
      };

   void EXPORT pack(MessageData& messageData, const suppliesType& data);
   void EXPORT unpack(MessageData& messageData, suppliesType& data);
   unsigned EXPORT memorySize(const suppliesType& data);
   std::string EXPORT DDML(const suppliesType& data);

   //------ PastureWaterSupply ------
   struct PastureWaterSupplyType
      {
      std::vector<suppliesType> supplies;
      };

   void EXPORT pack(MessageData& messageData, const PastureWaterSupplyType& data);
   void EXPORT unpack(MessageData& messageData, PastureWaterSupplyType& data);
   unsigned EXPORT memorySize(const PastureWaterSupplyType& data);
   std::string EXPORT DDML(const PastureWaterSupplyType& data);

   //------ water_uptake ------
   struct water_uptakeType
      {
      std::string crop_ident;
      std::vector<double> layers;
      std::vector<double> uptake;
      };

   void EXPORT pack(MessageData& messageData, const water_uptakeType& data);
   void EXPORT unpack(MessageData& messageData, water_uptakeType& data);
   unsigned EXPORT memorySize(const water_uptakeType& data);
   std::string EXPORT DDML(const water_uptakeType& data);

   //------ PastureWaterUptake ------
   struct PastureWaterUptakeType
      {
      std::vector<water_uptakeType> water_uptake;
      };

   void EXPORT pack(MessageData& messageData, const PastureWaterUptakeType& data);
   void EXPORT unpack(MessageData& messageData, PastureWaterUptakeType& data);
   unsigned EXPORT memorySize(const PastureWaterUptakeType& data);
   std::string EXPORT DDML(const PastureWaterUptakeType& data);

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

   void EXPORT pack(MessageData& messageData, const water_infoType& data);
   void EXPORT unpack(MessageData& messageData, water_infoType& data);
   unsigned EXPORT memorySize(const water_infoType& data);
   std::string EXPORT DDML(const water_infoType& data);

   //------ WaterInfo ------
   struct WaterInfoType
      {
      std::vector<water_infoType> water_info;
      };

   void EXPORT pack(MessageData& messageData, const WaterInfoType& data);
   void EXPORT unpack(MessageData& messageData, WaterInfoType& data);
   unsigned EXPORT memorySize(const WaterInfoType& data);
   std::string EXPORT DDML(const WaterInfoType& data);

   //------ fom ------
   struct fomType
      {
      double weight;
      double n;
      double p;
      double s;
      double ash_alk;
      double dmd;
      };

   void EXPORT pack(MessageData& messageData, const fomType& data);
   void EXPORT unpack(MessageData& messageData, fomType& data);
   unsigned EXPORT memorySize(const fomType& data);
   std::string EXPORT DDML(const fomType& data);

   //------ FomAdded ------
   struct FomAddedType
      {
      std::vector<double> layers;
      std::vector<fomType> fom;
      };

   void EXPORT pack(MessageData& messageData, const FomAddedType& data);
   void EXPORT unpack(MessageData& messageData, FomAddedType& data);
   unsigned EXPORT memorySize(const FomAddedType& data);
   std::string EXPORT DDML(const FomAddedType& data);

   //------ PastureNutrientUptake ------
   struct PastureNutrientUptakeType
      {
      std::string nutrient;
      std::vector<double> layers;
      std::vector<double> uptake;
      };

   void EXPORT pack(MessageData& messageData, const PastureNutrientUptakeType& data);
   void EXPORT unpack(MessageData& messageData, PastureNutrientUptakeType& data);
   unsigned EXPORT memorySize(const PastureNutrientUptakeType& data);
   std::string EXPORT DDML(const PastureNutrientUptakeType& data);

   //------ PastureSow ------
   struct PastureSowType
      {
      double rate;
      };

   void EXPORT pack(MessageData& messageData, const PastureSowType& data);
   void EXPORT unpack(MessageData& messageData, PastureSowType& data);
   unsigned EXPORT memorySize(const PastureSowType& data);
   std::string EXPORT DDML(const PastureSowType& data);

   //------ PastureKill ------
   struct PastureKillType
      {
      double propn_herbage;
      double propn_seed;
      };

   void EXPORT pack(MessageData& messageData, const PastureKillType& data);
   void EXPORT unpack(MessageData& messageData, PastureKillType& data);
   unsigned EXPORT memorySize(const PastureKillType& data);
   std::string EXPORT DDML(const PastureKillType& data);

   //------ PastureCultivate ------
   struct PastureCultivateType
      {
      double depth;
      double propn_incorp;
      double propn_mixed;
      };

   void EXPORT pack(MessageData& messageData, const PastureCultivateType& data);
   void EXPORT unpack(MessageData& messageData, PastureCultivateType& data);
   unsigned EXPORT memorySize(const PastureCultivateType& data);
   std::string EXPORT DDML(const PastureCultivateType& data);

   //------ PastureCut ------
   struct PastureCutType
      {
      double cut_height;
      double gathered;
      double dmd_loss;
      double dm_content;
      };

   void EXPORT pack(MessageData& messageData, const PastureCutType& data);
   void EXPORT unpack(MessageData& messageData, PastureCutType& data);
   unsigned EXPORT memorySize(const PastureCutType& data);
   std::string EXPORT DDML(const PastureCutType& data);

   //------ PastureBurn ------
   struct PastureBurnType
      {
      double kill_plants;
      double kill_seed;
      double propn_unburnt;
      };

   void EXPORT pack(MessageData& messageData, const PastureBurnType& data);
   void EXPORT unpack(MessageData& messageData, PastureBurnType& data);
   unsigned EXPORT memorySize(const PastureBurnType& data);
   std::string EXPORT DDML(const PastureBurnType& data);

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

   void EXPORT pack(MessageData& messageData, const PastureOnCutType& data);
   void EXPORT unpack(MessageData& messageData, PastureOnCutType& data);
   unsigned EXPORT memorySize(const PastureOnCutType& data);
   std::string EXPORT DDML(const PastureOnCutType& data);

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

   void EXPORT pack(MessageData& messageData, const PastureWeatherType& data);
   void EXPORT unpack(MessageData& messageData, PastureWeatherType& data);
   unsigned EXPORT memorySize(const PastureWeatherType& data);
   std::string EXPORT DDML(const PastureWeatherType& data);

   //------ Faeces ------
   struct FaecesType
      {
      double weight;
      double n;
      double p;
      double s;
      double ash_alk;
      };

   void EXPORT pack(MessageData& messageData, const FaecesType& data);
   void EXPORT unpack(MessageData& messageData, FaecesType& data);
   unsigned EXPORT memorySize(const FaecesType& data);
   std::string EXPORT DDML(const FaecesType& data);

   //------ FaecesInorg ------
   struct FaecesInorgType
      {
      double n;
      double p;
      double s;
      };

   void EXPORT pack(MessageData& messageData, const FaecesInorgType& data);
   void EXPORT unpack(MessageData& messageData, FaecesInorgType& data);
   unsigned EXPORT memorySize(const FaecesInorgType& data);
   std::string EXPORT DDML(const FaecesInorgType& data);

   //------ Intake ------
   struct IntakeType
      {
      double weight;
      double n;
      double p;
      double s;
      double ash_alk;
      };

   void EXPORT pack(MessageData& messageData, const IntakeType& data);
   void EXPORT unpack(MessageData& messageData, IntakeType& data);
   unsigned EXPORT memorySize(const IntakeType& data);
   std::string EXPORT DDML(const IntakeType& data);

   //------ PastIntake ------
   struct PastIntakeType
      {
      double weight;
      double n;
      double p;
      double s;
      double ash_alk;
      };

   void EXPORT pack(MessageData& messageData, const PastIntakeType& data);
   void EXPORT unpack(MessageData& messageData, PastIntakeType& data);
   unsigned EXPORT memorySize(const PastIntakeType& data);
   std::string EXPORT DDML(const PastIntakeType& data);

   //------ SuppIntake ------
   struct SuppIntakeType
      {
      double weight;
      double n;
      double p;
      double s;
      double ash_alk;
      };

   void EXPORT pack(MessageData& messageData, const SuppIntakeType& data);
   void EXPORT unpack(MessageData& messageData, SuppIntakeType& data);
   unsigned EXPORT memorySize(const SuppIntakeType& data);
   std::string EXPORT DDML(const SuppIntakeType& data);

   //------ faeces_om ------
   struct faeces_omType
      {
      double weight;
      double n;
      double p;
      double s;
      double ash_alk;
      };

   void EXPORT pack(MessageData& messageData, const faeces_omType& data);
   void EXPORT unpack(MessageData& messageData, faeces_omType& data);
   unsigned EXPORT memorySize(const faeces_omType& data);
   std::string EXPORT DDML(const faeces_omType& data);

   //------ faeces_inorg ------
   struct faeces_inorgType
      {
      double n;
      double p;
      double s;
      };

   void EXPORT pack(MessageData& messageData, const faeces_inorgType& data);
   void EXPORT unpack(MessageData& messageData, faeces_inorgType& data);
   unsigned EXPORT memorySize(const faeces_inorgType& data);
   std::string EXPORT DDML(const faeces_inorgType& data);

   //------ urine ------
   struct urineType
      {
      double volume;
      double urea;
      double pox;
      double so4;
      double ash_alk;
      };

   void EXPORT pack(MessageData& messageData, const urineType& data);
   void EXPORT unpack(MessageData& messageData, urineType& data);
   unsigned EXPORT memorySize(const urineType& data);
   std::string EXPORT DDML(const urineType& data);

   //------ AddExcreta ------
   struct AddExcretaType
      {
      faeces_omType faeces_om;
      faeces_inorgType faeces_inorg;
      urineType urine;
      };

   void EXPORT pack(MessageData& messageData, const AddExcretaType& data);
   void EXPORT unpack(MessageData& messageData, AddExcretaType& data);
   unsigned EXPORT memorySize(const AddExcretaType& data);
   std::string EXPORT DDML(const AddExcretaType& data);

   //------ RemoveHerbage ------
   struct RemoveHerbageType
      {
      std::vector<double> herbage;
      std::vector<double> seed;
      };

   void EXPORT pack(MessageData& messageData, const RemoveHerbageType& data);
   void EXPORT unpack(MessageData& messageData, RemoveHerbageType& data);
   unsigned EXPORT memorySize(const RemoveHerbageType& data);
   std::string EXPORT DDML(const RemoveHerbageType& data);

   //------ SuppEaten ------
   struct SuppEatenType
      {
      std::string paddock;
      double eaten;
      };

   void EXPORT pack(MessageData& messageData, const SuppEatenType& data);
   void EXPORT unpack(MessageData& messageData, SuppEatenType& data);
   unsigned EXPORT memorySize(const SuppEatenType& data);
   std::string EXPORT DDML(const SuppEatenType& data);

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

   void EXPORT pack(MessageData& messageData, const herbageType& data);
   void EXPORT unpack(MessageData& messageData, herbageType& data);
   unsigned EXPORT memorySize(const herbageType& data);
   std::string EXPORT DDML(const herbageType& data);

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

   void EXPORT pack(MessageData& messageData, const seedType& data);
   void EXPORT unpack(MessageData& messageData, seedType& data);
   unsigned EXPORT memorySize(const seedType& data);
   std::string EXPORT DDML(const seedType& data);

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

   void EXPORT pack(MessageData& messageData, const Plant2StockType& data);
   void EXPORT unpack(MessageData& messageData, Plant2StockType& data);
   unsigned EXPORT memorySize(const Plant2StockType& data);
   std::string EXPORT DDML(const Plant2StockType& data);

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

   void EXPORT pack(MessageData& messageData, const BuyStockType& data);
   void EXPORT unpack(MessageData& messageData, BuyStockType& data);
   unsigned EXPORT memorySize(const BuyStockType& data);
   std::string EXPORT DDML(const BuyStockType& data);

   //------ SellStock ------
   struct SellStockType
      {
      int group;
      int number;
      };

   void EXPORT pack(MessageData& messageData, const SellStockType& data);
   void EXPORT unpack(MessageData& messageData, SellStockType& data);
   unsigned EXPORT memorySize(const SellStockType& data);
   std::string EXPORT DDML(const SellStockType& data);

   //------ CastrateStock ------
   struct CastrateStockType
      {
      int group;
      int number;
      };

   void EXPORT pack(MessageData& messageData, const CastrateStockType& data);
   void EXPORT unpack(MessageData& messageData, CastrateStockType& data);
   unsigned EXPORT memorySize(const CastrateStockType& data);
   std::string EXPORT DDML(const CastrateStockType& data);

   //------ DryOffStock ------
   struct DryOffStockType
      {
      int group;
      int number;
      };

   void EXPORT pack(MessageData& messageData, const DryOffStockType& data);
   void EXPORT unpack(MessageData& messageData, DryOffStockType& data);
   unsigned EXPORT memorySize(const DryOffStockType& data);
   std::string EXPORT DDML(const DryOffStockType& data);

   //------ JoinStock ------
   struct JoinStockType
      {
      int group;
      std::string mate_to;
      int mate_days;
      };

   void EXPORT pack(MessageData& messageData, const JoinStockType& data);
   void EXPORT unpack(MessageData& messageData, JoinStockType& data);
   unsigned EXPORT memorySize(const JoinStockType& data);
   std::string EXPORT DDML(const JoinStockType& data);

   //------ MoveStock ------
   struct MoveStockType
      {
      int group;
      std::string paddock;
      };

   void EXPORT pack(MessageData& messageData, const MoveStockType& data);
   void EXPORT unpack(MessageData& messageData, MoveStockType& data);
   unsigned EXPORT memorySize(const MoveStockType& data);
   std::string EXPORT DDML(const MoveStockType& data);

   //------ ShearStock ------
   struct ShearStockType
      {
      int group;
      std::string sub_group;
      };

   void EXPORT pack(MessageData& messageData, const ShearStockType& data);
   void EXPORT unpack(MessageData& messageData, ShearStockType& data);
   unsigned EXPORT memorySize(const ShearStockType& data);
   std::string EXPORT DDML(const ShearStockType& data);

   //------ SplitStock ------
   struct SplitStockType
      {
      int group;
      std::string type;
      double value;
      };

   void EXPORT pack(MessageData& messageData, const SplitStockType& data);
   void EXPORT unpack(MessageData& messageData, SplitStockType& data);
   unsigned EXPORT memorySize(const SplitStockType& data);
   std::string EXPORT DDML(const SplitStockType& data);

   //------ TagStock ------
   struct TagStockType
      {
      int group;
      int value;
      };

   void EXPORT pack(MessageData& messageData, const TagStockType& data);
   void EXPORT unpack(MessageData& messageData, TagStockType& data);
   unsigned EXPORT memorySize(const TagStockType& data);
   std::string EXPORT DDML(const TagStockType& data);

   //------ WeanStock ------
   struct WeanStockType
      {
      int group;
      std::string sex;
      int number;
      };

   void EXPORT pack(MessageData& messageData, const WeanStockType& data);
   void EXPORT unpack(MessageData& messageData, WeanStockType& data);
   unsigned EXPORT memorySize(const WeanStockType& data);
   std::string EXPORT DDML(const WeanStockType& data);

   //------ dm ------
   struct dmType
      {
      std::string pool;
      std::vector<std::string> part;
      std::vector<double> dlt;
      };

   void EXPORT pack(MessageData& messageData, const dmType& data);
   void EXPORT unpack(MessageData& messageData, dmType& data);
   unsigned EXPORT memorySize(const dmType& data);
   std::string EXPORT DDML(const dmType& data);

   //------ RemoveCropDm ------
   struct RemoveCropDmType
      {
      std::vector<dmType> dm;
      };

   void EXPORT pack(MessageData& messageData, const RemoveCropDmType& data);
   void EXPORT unpack(MessageData& messageData, RemoveCropDmType& data);
   unsigned EXPORT memorySize(const RemoveCropDmType& data);
   std::string EXPORT DDML(const RemoveCropDmType& data);

   //------ RemoveResidueDm ------
   struct RemoveResidueDmType
      {
      std::vector<std::string> dm_type;
      std::vector<float> dlt_residue_dm;
      };

   void EXPORT pack(MessageData& messageData, const RemoveResidueDmType& data);
   void EXPORT unpack(MessageData& messageData, RemoveResidueDmType& data);
   unsigned EXPORT memorySize(const RemoveResidueDmType& data);
   std::string EXPORT DDML(const RemoveResidueDmType& data);

   //------ SupplementBuy ------
   struct SupplementBuyType
      {
      std::string supplement;
      double amount;
      };

   void EXPORT pack(MessageData& messageData, const SupplementBuyType& data);
   void EXPORT unpack(MessageData& messageData, SupplementBuyType& data);
   unsigned EXPORT memorySize(const SupplementBuyType& data);
   std::string EXPORT DDML(const SupplementBuyType& data);

   //------ SupplementFeed ------
   struct SupplementFeedType
      {
      std::string supplement;
      double amount;
      std::string paddock;
      };

   void EXPORT pack(MessageData& messageData, const SupplementFeedType& data);
   void EXPORT unpack(MessageData& messageData, SupplementFeedType& data);
   unsigned EXPORT memorySize(const SupplementFeedType& data);
   std::string EXPORT DDML(const SupplementFeedType& data);

   //------ SupplementMix ------
   struct SupplementMixType
      {
      std::string src_store;
      double amount;
      std::string dest_store;
      };

   void EXPORT pack(MessageData& messageData, const SupplementMixType& data);
   void EXPORT unpack(MessageData& messageData, SupplementMixType& data);
   unsigned EXPORT memorySize(const SupplementMixType& data);
   std::string EXPORT DDML(const SupplementMixType& data);

   //------ ExternalMassFlow ------
   struct ExternalMassFlowType
      {
      std::string PoolClass;
      std::string FlowType;
      float C;
      float N;
      float P;
      float DM;
      float SW;
      };

   void EXPORT pack(MessageData& messageData, const ExternalMassFlowType& data);
   void EXPORT unpack(MessageData& messageData, ExternalMassFlowType& data);
   unsigned EXPORT memorySize(const ExternalMassFlowType& data);
   std::string EXPORT DDML(const ExternalMassFlowType& data);

   class Variant;
   void EXPORT pack(MessageData& messageData, const Variant& data);
   void EXPORT unpack(MessageData& messageData, Variant& data);
   unsigned EXPORT memorySize(Variant& data) ;
   std::string EXPORT DDML(const Variant& data);
#endif

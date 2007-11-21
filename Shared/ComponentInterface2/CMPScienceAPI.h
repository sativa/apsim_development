#ifndef CMPScienceAPIH
#define CMPScienceAPIH
#include <general/platform.h>
#include <ComponentInterface2/ScienceAPI.h>

class CMPComponentInterface;
class Variant;
// ------------------------------------------------------------------
// CMP Implementation for interacting with simulation
// NB: Autogenerated. Do not modify manually.
// ------------------------------------------------------------------
class EXPORT CMPScienceAPI : public ScienceAPI
   {
   private:
      CMPComponentInterface& componentInterface;

   public:
      CMPScienceAPI(CMPComponentInterface& componentinterface);
      virtual ~CMPScienceAPI() {};

      virtual void write(const std::string& msg);
      virtual std::string name();
      virtual std::string FQName();

      virtual void setSearchOrder(const std::vector<std::string> &list);
      virtual void getSearchOrder(std::vector<std::string> &list);

      virtual void query(const std::string& pattern, std::vector<QueryMatch>& matches);

      // Methods for reading raw strings
      virtual bool readRaw(const std::string& parName, std::vector<std::string> &values);

      // null
      virtual void subscribe(const std::string& name, boost::function0<void> handler);
      virtual void publish(const std::string& name);

      // bool
      virtual bool read(const std::string& name, const std::string& units, bool optional, bool& data);
      virtual bool get(const std::string& name, const std::string& units, bool optional, bool& data);
      virtual void set(const std::string& name, const std::string& units, bool& data);
      virtual void expose(const std::string& name, const std::string& units, const std::string& description, bool writable, bool& variable);
      virtual void exposeFunction(const std::string& name, const std::string& units, const std::string& description, boost::function1<void, bool&> method);
      virtual void exposeFunction(const std::string& name, const std::string& units, const std::string& description,
                                  boost::function1<void, bool&> getter,
                                  boost::function1<void, bool&> setter);
      virtual void subscribe(const std::string& name, boost::function1<void, bool&> handler);
      virtual void publish(const std::string& name, bool& data);

      // int
      virtual bool read(const std::string& name, const std::string& units, bool optional, int& data);
      virtual bool read(const std::string& name, const std::string& units, bool optional, int& data, int lower, int upper);
      virtual bool get(const std::string& name, const std::string& units, bool optional, int& data);
      virtual bool get(const std::string& name, const std::string& units, bool optional, int& data, int lower, int upper);
      virtual void set(const std::string& name, const std::string& units, int& data);
      virtual void expose(const std::string& name, const std::string& units, const std::string& description, bool writable, int& variable);
      virtual void exposeFunction(const std::string& name, const std::string& units, const std::string& description, boost::function1<void, int&> method);
      virtual void exposeFunction(const std::string& name, const std::string& units, const std::string& description,
                                  boost::function1<void, int&> getter,
                                  boost::function1<void, int&> setter);
      virtual void subscribe(const std::string& name, boost::function1<void, int&> handler);
      virtual void publish(const std::string& name, int& data);

      // float
      virtual bool read(const std::string& name, const std::string& units, bool optional, float& data);
      virtual bool read(const std::string& name, const std::string& units, bool optional, float& data, float lower, float upper);
      virtual bool get(const std::string& name, const std::string& units, bool optional, float& data);
      virtual bool get(const std::string& name, const std::string& units, bool optional, float& data, float lower, float upper);
      virtual void set(const std::string& name, const std::string& units, float& data);
      virtual void expose(const std::string& name, const std::string& units, const std::string& description, bool writable, float& variable);
      virtual void exposeFunction(const std::string& name, const std::string& units, const std::string& description, boost::function1<void, float&> method);
      virtual void exposeFunction(const std::string& name, const std::string& units, const std::string& description,
                                  boost::function1<void, float&> getter,
                                  boost::function1<void, float&> setter);
      virtual void subscribe(const std::string& name, boost::function1<void, float&> handler);
      virtual void publish(const std::string& name, float& data);

      // double
      virtual bool read(const std::string& name, const std::string& units, bool optional, double& data);
      virtual bool read(const std::string& name, const std::string& units, bool optional, double& data, double lower, double upper);
      virtual bool get(const std::string& name, const std::string& units, bool optional, double& data);
      virtual bool get(const std::string& name, const std::string& units, bool optional, double& data, double lower, double upper);
      virtual void set(const std::string& name, const std::string& units, double& data);
      virtual void expose(const std::string& name, const std::string& units, const std::string& description, bool writable, double& variable);
      virtual void exposeFunction(const std::string& name, const std::string& units, const std::string& description, boost::function1<void, double&> method);
      virtual void exposeFunction(const std::string& name, const std::string& units, const std::string& description,
                                  boost::function1<void, double&> getter,
                                  boost::function1<void, double&> setter);
      virtual void subscribe(const std::string& name, boost::function1<void, double&> handler);
      virtual void publish(const std::string& name, double& data);

      // std::string
      virtual bool read(const std::string& name, const std::string& units, bool optional, std::string& data);
      virtual bool get(const std::string& name, const std::string& units, bool optional, std::string& data);
      virtual void set(const std::string& name, const std::string& units, std::string& data);
      virtual void expose(const std::string& name, const std::string& units, const std::string& description, bool writable, std::string& variable);
      virtual void exposeFunction(const std::string& name, const std::string& units, const std::string& description, boost::function1<void, std::string&> method);
      virtual void exposeFunction(const std::string& name, const std::string& units, const std::string& description,
                                  boost::function1<void, std::string&> getter,
                                  boost::function1<void, std::string&> setter);
      virtual void subscribe(const std::string& name, boost::function1<void, std::string&> handler);
      virtual void publish(const std::string& name, std::string& data);

      // std::vector<bool>
      virtual bool read(const std::string& name, const std::string& units, bool optional, std::vector<bool>& data);
      virtual bool get(const std::string& name, const std::string& units, bool optional, std::vector<bool>& data);
      virtual void set(const std::string& name, const std::string& units, std::vector<bool>& data);
      virtual void expose(const std::string& name, const std::string& units, const std::string& description, bool writable, std::vector<bool>& variable);
      virtual void exposeFunction(const std::string& name, const std::string& units, const std::string& description, boost::function1<void, std::vector<bool>&> method);
      virtual void exposeFunction(const std::string& name, const std::string& units, const std::string& description,
                                  boost::function1<void, std::vector<bool>&> getter,
                                  boost::function1<void, std::vector<bool>&> setter);
      virtual void subscribe(const std::string& name, boost::function1<void, std::vector<bool>&> handler);
      virtual void publish(const std::string& name, std::vector<bool>& data);

      // std::vector<int>
      virtual bool read(const std::string& name, const std::string& units, bool optional, std::vector<int>& data);
      virtual bool read(const std::string& name, const std::string& units, bool optional, std::vector<int>& data, int lower, int upper);
      virtual bool get(const std::string& name, const std::string& units, bool optional, std::vector<int>& data);
      virtual bool get(const std::string& name, const std::string& units, bool optional, std::vector<int>& data, int lower, int upper);
      virtual void set(const std::string& name, const std::string& units, std::vector<int>& data);
      virtual void expose(const std::string& name, const std::string& units, const std::string& description, bool writable, std::vector<int>& variable);
      virtual void exposeFunction(const std::string& name, const std::string& units, const std::string& description, boost::function1<void, std::vector<int>&> method);
      virtual void exposeFunction(const std::string& name, const std::string& units, const std::string& description,
                                  boost::function1<void, std::vector<int>&> getter,
                                  boost::function1<void, std::vector<int>&> setter);
      virtual void subscribe(const std::string& name, boost::function1<void, std::vector<int>&> handler);
      virtual void publish(const std::string& name, std::vector<int>& data);

      // std::vector<float>
      virtual bool read(const std::string& name, const std::string& units, bool optional, std::vector<float>& data);
      virtual bool read(const std::string& name, const std::string& units, bool optional, std::vector<float>& data, float lower, float upper);
      virtual bool get(const std::string& name, const std::string& units, bool optional, std::vector<float>& data);
      virtual bool get(const std::string& name, const std::string& units, bool optional, std::vector<float>& data, float lower, float upper);
      virtual void set(const std::string& name, const std::string& units, std::vector<float>& data);
      virtual void expose(const std::string& name, const std::string& units, const std::string& description, bool writable, std::vector<float>& variable);
      virtual void exposeFunction(const std::string& name, const std::string& units, const std::string& description, boost::function1<void, std::vector<float>&> method);
      virtual void exposeFunction(const std::string& name, const std::string& units, const std::string& description,
                                  boost::function1<void, std::vector<float>&> getter,
                                  boost::function1<void, std::vector<float>&> setter);
      virtual void subscribe(const std::string& name, boost::function1<void, std::vector<float>&> handler);
      virtual void publish(const std::string& name, std::vector<float>& data);

      // std::vector<double>
      virtual bool read(const std::string& name, const std::string& units, bool optional, std::vector<double>& data);
      virtual bool read(const std::string& name, const std::string& units, bool optional, std::vector<double>& data, double lower, double upper);
      virtual bool get(const std::string& name, const std::string& units, bool optional, std::vector<double>& data);
      virtual bool get(const std::string& name, const std::string& units, bool optional, std::vector<double>& data, double lower, double upper);
      virtual void set(const std::string& name, const std::string& units, std::vector<double>& data);
      virtual void expose(const std::string& name, const std::string& units, const std::string& description, bool writable, std::vector<double>& variable);
      virtual void exposeFunction(const std::string& name, const std::string& units, const std::string& description, boost::function1<void, std::vector<double>&> method);
      virtual void exposeFunction(const std::string& name, const std::string& units, const std::string& description,
                                  boost::function1<void, std::vector<double>&> getter,
                                  boost::function1<void, std::vector<double>&> setter);
      virtual void subscribe(const std::string& name, boost::function1<void, std::vector<double>&> handler);
      virtual void publish(const std::string& name, std::vector<double>& data);

      // std::vector<std::string>
      virtual bool read(const std::string& name, const std::string& units, bool optional, std::vector<std::string>& data);
      virtual bool get(const std::string& name, const std::string& units, bool optional, std::vector<std::string>& data);
      virtual void set(const std::string& name, const std::string& units, std::vector<std::string>& data);
      virtual void expose(const std::string& name, const std::string& units, const std::string& description, bool writable, std::vector<std::string>& variable);
      virtual void exposeFunction(const std::string& name, const std::string& units, const std::string& description, boost::function1<void, std::vector<std::string>&> method);
      virtual void exposeFunction(const std::string& name, const std::string& units, const std::string& description,
                                  boost::function1<void, std::vector<std::string>&> getter,
                                  boost::function1<void, std::vector<std::string>&> setter);
      virtual void subscribe(const std::string& name, boost::function1<void, std::vector<std::string>&> handler);
      virtual void publish(const std::string& name, std::vector<std::string>& data);

      virtual void subscribe(const std::string& name, boost::function1<void, CompleteType&> handler);
      virtual void publish(const std::string& name, CompleteType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, ErrorType&> handler);
      virtual void publish(const std::string& name, ErrorType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, EventType&> handler);
      virtual void publish(const std::string& name, EventType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, GetValueType&> handler);
      virtual void publish(const std::string& name, GetValueType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, Init1Type&> handler);
      virtual void publish(const std::string& name, Init1Type& data);

      virtual void subscribe(const std::string& name, boost::function1<void, NotifySetValueSuccessType&> handler);
      virtual void publish(const std::string& name, NotifySetValueSuccessType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, PublishEventType&> handler);
      virtual void publish(const std::string& name, PublishEventType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, QueryInfoType&> handler);
      virtual void publish(const std::string& name, QueryInfoType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, RegisterType&> handler);
      virtual void publish(const std::string& name, RegisterType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, ReplyValueType&> handler);
      virtual void publish(const std::string& name, ReplyValueType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, RequestSetValueType&> handler);
      virtual void publish(const std::string& name, RequestSetValueType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, ReturnInfoType&> handler);
      virtual void publish(const std::string& name, ReturnInfoType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, ReturnValueType&> handler);
      virtual void publish(const std::string& name, ReturnValueType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, QueryValueType&> handler);
      virtual void publish(const std::string& name, QueryValueType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, QuerySetValueType&> handler);
      virtual void publish(const std::string& name, QuerySetValueType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, LayeredType&> handler);
      virtual void publish(const std::string& name, LayeredType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, TimeType&> handler);
      virtual void publish(const std::string& name, TimeType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, NewMetType&> handler);
      virtual void publish(const std::string& name, NewMetType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, SoilWaterProfileLayerType&> handler);
      virtual void publish(const std::string& name, SoilWaterProfileLayerType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, SoilWaterLayerType&> handler);
      virtual void publish(const std::string& name, SoilWaterLayerType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, LateralFlowLayerType&> handler);
      virtual void publish(const std::string& name, LateralFlowLayerType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, SoilWaterBalanceType&> handler);
      virtual void publish(const std::string& name, SoilWaterBalanceType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, NewSoluteType&> handler);
      virtual void publish(const std::string& name, NewSoluteType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, layerType&> handler);
      virtual void publish(const std::string& name, layerType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, SoluteProfileType&> handler);
      virtual void publish(const std::string& name, SoluteProfileType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, IrrigatedType&> handler);
      virtual void publish(const std::string& name, IrrigatedType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, InterceptionType&> handler);
      virtual void publish(const std::string& name, InterceptionType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, LightProfileType&> handler);
      virtual void publish(const std::string& name, LightProfileType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, CanopyType&> handler);
      virtual void publish(const std::string& name, CanopyType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, CanopyWaterBalanceType&> handler);
      virtual void publish(const std::string& name, CanopyWaterBalanceType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, OrganicMatterFractionType&> handler);
      virtual void publish(const std::string& name, OrganicMatterFractionType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, ResidueType&> handler);
      virtual void publish(const std::string& name, ResidueType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, soluteType&> handler);
      virtual void publish(const std::string& name, soluteType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, SurfaceWaterType&> handler);
      virtual void publish(const std::string& name, SurfaceWaterType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, SurfaceWaterBalanceType&> handler);
      virtual void publish(const std::string& name, SurfaceWaterBalanceType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, FertiliserConstituentsType&> handler);
      virtual void publish(const std::string& name, FertiliserConstituentsType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, FPoolType&> handler);
      virtual void publish(const std::string& name, FPoolType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, FPoolProfileLayerType&> handler);
      virtual void publish(const std::string& name, FPoolProfileLayerType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, StandingFractionType&> handler);
      virtual void publish(const std::string& name, StandingFractionType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, LyingFractionType&> handler);
      virtual void publish(const std::string& name, LyingFractionType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, SurfaceOrganicMatterType&> handler);
      virtual void publish(const std::string& name, SurfaceOrganicMatterType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, SurfaceOrganicMatterDecompType&> handler);
      virtual void publish(const std::string& name, SurfaceOrganicMatterDecompType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, NBalanceType&> handler);
      virtual void publish(const std::string& name, NBalanceType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, CBalanceType&> handler);
      virtual void publish(const std::string& name, CBalanceType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, IncorpFomType&> handler);
      virtual void publish(const std::string& name, IncorpFomType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, SoilOrganicMatterType&> handler);
      virtual void publish(const std::string& name, SoilOrganicMatterType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, CropChoppedType&> handler);
      virtual void publish(const std::string& name, CropChoppedType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, NewProfileType&> handler);
      virtual void publish(const std::string& name, NewProfileType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, NewCanopyType&> handler);
      virtual void publish(const std::string& name, NewCanopyType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, NewCropType&> handler);
      virtual void publish(const std::string& name, NewCropType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, NewZoneType&> handler);
      virtual void publish(const std::string& name, NewZoneType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, SoilLayersType&> handler);
      virtual void publish(const std::string& name, SoilLayersType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, rlv_layerType&> handler);
      virtual void publish(const std::string& name, rlv_layerType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, demandsType&> handler);
      virtual void publish(const std::string& name, demandsType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, PastureWaterDemandType&> handler);
      virtual void publish(const std::string& name, PastureWaterDemandType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, suppliesType&> handler);
      virtual void publish(const std::string& name, suppliesType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, PastureWaterSupplyType&> handler);
      virtual void publish(const std::string& name, PastureWaterSupplyType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, water_uptakeType&> handler);
      virtual void publish(const std::string& name, water_uptakeType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, PastureWaterUptakeType&> handler);
      virtual void publish(const std::string& name, PastureWaterUptakeType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, water_infoType&> handler);
      virtual void publish(const std::string& name, water_infoType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, WaterInfoType&> handler);
      virtual void publish(const std::string& name, WaterInfoType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, fomType&> handler);
      virtual void publish(const std::string& name, fomType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, FomAddedType&> handler);
      virtual void publish(const std::string& name, FomAddedType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, PastureNutrientUptakeType&> handler);
      virtual void publish(const std::string& name, PastureNutrientUptakeType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, PastureSowType&> handler);
      virtual void publish(const std::string& name, PastureSowType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, PastureKillType&> handler);
      virtual void publish(const std::string& name, PastureKillType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, PastureCultivateType&> handler);
      virtual void publish(const std::string& name, PastureCultivateType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, PastureCutType&> handler);
      virtual void publish(const std::string& name, PastureCutType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, PastureOnCutType&> handler);
      virtual void publish(const std::string& name, PastureOnCutType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, PastureWeatherType&> handler);
      virtual void publish(const std::string& name, PastureWeatherType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, FaecesType&> handler);
      virtual void publish(const std::string& name, FaecesType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, FaecesInorgType&> handler);
      virtual void publish(const std::string& name, FaecesInorgType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, IntakeType&> handler);
      virtual void publish(const std::string& name, IntakeType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, PastIntakeType&> handler);
      virtual void publish(const std::string& name, PastIntakeType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, SuppIntakeType&> handler);
      virtual void publish(const std::string& name, SuppIntakeType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, faeces_omType&> handler);
      virtual void publish(const std::string& name, faeces_omType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, faeces_inorgType&> handler);
      virtual void publish(const std::string& name, faeces_inorgType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, urineType&> handler);
      virtual void publish(const std::string& name, urineType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, AddExcretaType&> handler);
      virtual void publish(const std::string& name, AddExcretaType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, RemoveHerbageType&> handler);
      virtual void publish(const std::string& name, RemoveHerbageType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, SuppEatenType&> handler);
      virtual void publish(const std::string& name, SuppEatenType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, herbageType&> handler);
      virtual void publish(const std::string& name, herbageType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, seedType&> handler);
      virtual void publish(const std::string& name, seedType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, Plant2StockType&> handler);
      virtual void publish(const std::string& name, Plant2StockType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, BuyStockType&> handler);
      virtual void publish(const std::string& name, BuyStockType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, SellStockType&> handler);
      virtual void publish(const std::string& name, SellStockType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, CastrateStockType&> handler);
      virtual void publish(const std::string& name, CastrateStockType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, DryOffStockType&> handler);
      virtual void publish(const std::string& name, DryOffStockType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, JoinStockType&> handler);
      virtual void publish(const std::string& name, JoinStockType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, MoveStockType&> handler);
      virtual void publish(const std::string& name, MoveStockType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, ShearStockType&> handler);
      virtual void publish(const std::string& name, ShearStockType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, SplitStockType&> handler);
      virtual void publish(const std::string& name, SplitStockType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, TagStockType&> handler);
      virtual void publish(const std::string& name, TagStockType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, WeanStockType&> handler);
      virtual void publish(const std::string& name, WeanStockType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, dmType&> handler);
      virtual void publish(const std::string& name, dmType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, RemoveCropDmType&> handler);
      virtual void publish(const std::string& name, RemoveCropDmType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, RemoveResidueDmType&> handler);
      virtual void publish(const std::string& name, RemoveResidueDmType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, SupplementBuyType&> handler);
      virtual void publish(const std::string& name, SupplementBuyType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, SupplementFeedType&> handler);
      virtual void publish(const std::string& name, SupplementFeedType& data);

      virtual void subscribe(const std::string& name, boost::function1<void, SupplementMixType&> handler);
      virtual void publish(const std::string& name, SupplementMixType& data);


      virtual void subscribe(const std::string& name, boost::function1<void, Variant&> handler);
      virtual void publish(const std::string& name, Variant& data);

   };
#endif

#ifndef PlantPartH
#define PlantPartH

#include <stdio.h>
#include <math.h>
#include <map>
#include <string>
#include <stdexcept>
#include <iomanip>

#include <boost/function.hpp>
#include <boost/bind.hpp>

#include <ComponentInterface/Type.h>
#include <ComponentInterface/ApsimVariant.h>
#include <ComponentInterface/Component.h>
#include <ComponentInterface/datatypes.h>
#include <ComponentInterface/Messages.h>
#include <ComponentInterface/MessageDataExt.h>
#include <ApsimShared/ApsimComponentData.h>
#include <ApsimShared/FStringExt.h>
#include <general/string_functions.h>
#include <ComponentInterface/ScienceAPI.h>

#include "PlantLibrary.h"
#include "PlantComponent.h"
#include "PlantInterface.h"
#include "PlantSpatial.h"
#include "CompositePool.h"
#include "Delta.h"
#include "PlantStress.h"
#include "Utility/PlantUtility.h"

class plantPart : public plantThing
   {
 protected:
   plantInterface *plant;                 // The plant we are attached to
   string myName;                        // What we call ourselves


 private:
//1) Need to make Senesced() method in PlantPart
//2) Make a Total() method = Green+Senesced
//3) Make Grain and GrainTotal methods which return Green or Total if it is a grain part.

   // state variables
   struct {
      float n_conc_crit;                  // critical N concentration (g N/g biomass)
      float n_conc_min;                   // minimum N concentration (g N/g biomass)
   } g;

   // "Constants"
public:

   plantPart(ScienceAPI& scienceAPI, plantInterface *p, const string &name);
   virtual ~plantPart() {};

   // deltas
   Delta Senescing;
   Delta Growth;
   Delta Detaching;
   Delta Retranslocation;

   // pools
   Pool& Green;
   Pool& Senesced;

   CompositePool Grain;

   // summary pools
   CompositePool Total;
   CompositePool GrainTotal;
   CompositePool Vegetative;
   CompositePool VegetativeTotal;

      // plantPart
   virtual float nCrit(void);
   virtual float nMin(void);

      // GrainPart
   virtual float dmGreenNew(void);

      // compositePart public
   virtual void  onInit1(protocol::Component *system);
   virtual void  zeroDeltas(void);
   virtual void  zeroAllGlobals(void);

      // plant
   bool tempFlagToShortCircuitInit1;

   virtual void  prepare(void) = 0;
   virtual void  checkBounds(void);
   virtual void  doNFixRetranslocate(float NFix, float NDemandDifferentialTotal);   //???
   virtual void  doNPartition(float nSupply, float n_demand_sum, float n_capacity_sum);  //???
   virtual float dlt_dm_green_retrans_hack(float);
   const string &name(void);

      //PURE
   virtual void  process(void) = 0;
   virtual float availableRetranslocateN(void) = 0;
   virtual void  calcDlt_pod_area (void) = 0;   //FIXME

   virtual float dltDmGreen(void)  = 0;
   virtual float dltDmUptake(void)  = 0;
   virtual float dltDmGreenRemoved(void) = 0;
   virtual float dltDmRemoved(void) = 0;
   virtual float dltNGreen(void)  = 0;
   virtual float dltNRemoved(void) = 0;
   virtual float dltNRetransOut(void) = 0;
   virtual float dltNSenescedRetrans(void) = 0;

   virtual float dmGrainWetTotal(void) = 0;
   virtual float dmGreenDemand(void) = 0;
   virtual float dmRetransSupply(void) = 0;

   virtual void  doBioActual (void) = 0;
   virtual void  doCover (PlantSpatial &spatial) = 0;
   virtual void  doDmDemand (float dlt_dm_supply_by_veg) = 0;
   virtual void  doDmPotRUE (void ) = 0;                      // (OUTPUT) potential dry matter (carbohydrate) production (g/m^2)
   virtual void  doDmRetranslocate(float DMAvail, float DMDemandDifferentialTotal) = 0;
   virtual void  doNDemand1(float, float) = 0;
   virtual void  doNDemand1Pot(float, float) = 0;
   virtual void  doNDemand2(float, float) = 0;
   virtual void  doNDemandGrain(float nfact_grain_conc, float swdef_expansion) = 0;
   virtual void  doNRetranslocate( float N_supply, float g_grain_n_demand) = 0;
   virtual void  doNSenescedRetrans(float navail, float n_demand_tot) = 0;
   virtual void  doNSenescence(void) = 0;
   virtual void  doRemoveBiomass(protocol::RemoveCropDmType dmRemoved, string &c_remove_biomass_report) = 0;
   virtual void  doSoilNDemand(void) = 0;
   virtual void  doSWDemand(float SWDemandMaxFactor) = 0;

   virtual void  get_dlt_dm_green_retrans(vector<float> &) = 0;
   virtual void  get_dlt_p_retrans(vector<float> &dlt_p_retrans) = 0;
   virtual void  get_dm_green(vector<float> &) = 0;
   virtual void  get_dm_plant_min(vector<float> &) = 0;
   virtual void  get_dm_senesced(vector<float> &) = 0;
   virtual void  get_n_demanded(vector<float> &) = 0;
   virtual void  get_name(vector<string> &names) = 0;
   virtual void  get_p_demand(vector<float> &p_demand) = 0;

   virtual float grainNo(void) = 0;
   virtual float grainWaterContent(void) = 0;
   virtual float grainWt(void) = 0;

   virtual float interceptRadiationGreen(float radiation) = 0;
   virtual float interceptRadiationTotal(float radiation) = 0;
   virtual void  morphology(void) = 0;

   virtual float nDemandGrain(void) = 0;
   virtual float nDemandGrain2(void) = 0;
   virtual float nMax(void) = 0;

   virtual void  onDayOf(const string &) = 0;
   virtual void  onKillStem(void) = 0;
   virtual void  onPlantEvent(const string &) = 0;

   virtual float pDemand(void) = 0;
   virtual void  removeBiomass(void) = 0;
   virtual float soilNDemand(void) = 0;
   virtual void  writeCultivarInfo (protocol::Component *) = 0;
   virtual void  zeroDltNSenescedTrans(void) = 0;

   virtual void onHarvest(float height, float remove_fr,
                          vector<string> &dm_type,
                          vector<float> &dlt_crop_dm,
                          vector<float> &dlt_dm_n,
                          vector<float> &dlt_dm_p,
                          vector<float> &fraction_to_residue) = 0;


   virtual void  onEndCrop(vector<string> &dm_type,
                  vector<float> &dlt_crop_dm,
                  vector<float> &dlt_dm_n,
                  vector<float> &dlt_dm_p,
                  vector<float> &fraction_to_residue) = 0;

   virtual void  collectDetachedForResidue(vector<string> &part_name
                                          , vector<float> &dm_residue
                                          , vector<float> &dm_n
                                          , vector<float> &dm_p
                                          , vector<float> &fract) = 0;


      // PlantP
   virtual float dmGreenStressDeterminant(void) = 0;
   virtual void  doPPartition(float p_uptake, float total_p_demand) = 0;
   virtual void  doPRetranslocate(float total_p_supply, float total_p_demand) = 0;
   virtual float pGreenStressDeterminant(void) = 0;
   virtual float pMaxPotStressDeterminant(void) = 0;
   virtual float pMinPotStressDeterminant(void) = 0;
   virtual float pRetransDemand(void) = 0;
   virtual float pRetransSupply(void) = 0;

      // compositePart public
   virtual float coverGreen(void)  = 0;
   virtual float coverSen(void)  = 0;
   virtual float coverTotal(void)  = 0;

   virtual void  Detachment(void) = 0;

   virtual float dltDm(void) = 0;
   virtual float dltDmDetached(void) = 0;
   virtual float dltDmGrainDemand(void) = 0;
   virtual float dltDmGreenRetrans(void) = 0;
   virtual float dltDmGreenRetransUptake(void) = 0;
   virtual float dltDmPotRue(void) = 0;
   virtual float dltDmRetranslocate(void) = 0;
   virtual float dltDmRetranslocateSupply(float DemandDifferential)  = 0;
   virtual float dltDmSenesced(void) = 0;
   virtual float dltDmSenescedRemoved(void) = 0;
   virtual float dltNDetached(void) = 0;
   virtual float dltNRetrans(void) = 0;
   virtual float dltNSenesced(void) = 0;
   virtual float dltNSenescedTrans(void) = 0;
   virtual float dltPDetached(void) = 0;
   virtual float dltPGreen(void)  = 0;
   virtual float dltPSenesced(void) = 0;

   virtual float dmDemandDifferential(void)  = 0;
   virtual float dmRetransDemand(void)  = 0;

   virtual void  doDmMin(void) = 0;
   virtual void  doGrainNumber (void) = 0;
   virtual void  doNConccentrationLimits(float) = 0;
   virtual void  doPDemand(void) = 0;
   virtual void  doProcessBioDemand(void) = 0;
   virtual void  doPSenescence(void) = 0;
   virtual void  doSenescence(float) = 0;

   virtual void  get_dlt_dm_detached(vector<float> &) = 0;
   virtual void  get_dlt_dm_green(vector<float> &) = 0;
   virtual void  get_dlt_dm_senesced(vector<float> &) = 0;

   virtual float giveDmGreen(float)  = 0;           // Arbitrator gives this part dm = 0; return amount used

   virtual float n_conc_crit(void) = 0;
   virtual float n_conc_min(void) = 0;
   virtual float nCapacity(void) = 0;
   virtual float nConcCrit() = 0;
   virtual float nConcMin() = 0;
   virtual float nDemand(void) = 0;
   virtual float nDemandDifferential(void) = 0;
   virtual float nRetransDemand(void) = 0;
   virtual float nRetransSupply(void) = 0;

   virtual void  onEmergence(void) = 0;

   virtual float pMaxPot(void) = 0;
   virtual float pMinPot(void) = 0;

   virtual void  readConstants (protocol::Component *, const string &) = 0;
   virtual void  readCultivarParameters (protocol::Component *, const string &) = 0;
   virtual void  readSpeciesParameters (protocol::Component *, vector<string> &) = 0;
   virtual void  removeBiomass2(float) = 0;

   virtual float SWDemand(void) = 0;
   virtual float SWDemandTE(void) = 0;
   virtual float transpirationEfficiency(void) = 0;

   virtual void  update(void) = 0;
   virtual void  zeroDltDmGreen(void) = 0;

   virtual bool isRetransPart(void) = 0;
   virtual bool isYieldPart(void) = 0;

protected:

      // protected constructor called by CompositePart only.
      plantPart(ScienceAPI& api, plantInterface *p, const string &name,
                Pool& green, Pool& senesced);

    void doInit1(protocol::Component *system);

//++++++ NOT IMPLEMENTED IN COMPOSITEPART YET

//      virtual void onSowing(void);
//      virtual void onGermination(void);

//      virtual void onTransplanting(void);
//      virtual void onFlowering(void);
//      virtual void onStartGrainFill(void);


//   virtual float dltNGreenRemoved(void);
//   virtual float dltNSenescedRemoved(void);
//   virtual float dltPGreenRemoved(void);
//   virtual float dltPSenescedRemoved(void);
//   virtual float dltPRemoved(void);

//   virtual float giveDmSenesced(float);
//   virtual float giveNGreen(float);

//   virtual float giveDmGreenRemoved(float);           //
//   virtual float giveDmSenescedRemoved(float);

//   virtual float dltDmGreenNew(void);

//   virtual float height(void);
//   virtual float width(void);

//   virtual void onHarvest_GenericAboveGroundPart(float remove_fr,
//                          vector<string> &dm_type,
//                          vector<float> &dlt_crop_dm,
//                          vector<float> &dlt_dm_n,
//                          vector<float> &dlt_dm_p,
//                          vector<float> &fraction_to_residue);

   //needed to standardise interface for composite subclass

//   virtual float dltLeafAreaPot(void);

//   virtual void write();
//   virtual void onRemoveBiomass(float);

   private:
      void get_dm_green_demand(protocol::Component *system, protocol::QueryValueData &qd);
      std::string addPartToVar(const std::string& variableName);
      std::string addPartToDesc(const std::string& description);

      void Initialise(bool isVegetative);
};


#endif /* PlantPartsH */
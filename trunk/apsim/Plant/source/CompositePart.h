#ifndef CompositePartH
#define CompositePartH

#include "PlantPart.h"

class CompositePart : public plantPart
{
   friend ostream &operator<<(ostream &, const CompositePart &);
public:                                             // member functions
   CompositePart(ScienceAPI& scienceAPI, plantInterface *p, const string &name);

   const CompositePart &operator=(const CompositePart &other);      // Assigment operator

   void add(plantPart* part);

   virtual Pool& Green(void);
   virtual Pool& Senesced(void);

   virtual void onInit1(protocol::Component *);
   virtual void doNewMet(protocol::NewMetType &newmet) ;
   virtual void readConstants (protocol::Component *, const string &);
   virtual void readSpeciesParameters (protocol::Component *, vector<string> &);
   virtual void readCultivarParameters (protocol::Component *, const string &);
   virtual void writeCultivarInfo (protocol::Component *);
   virtual void doProcessBioDemand(void);
   virtual void onDayOf(const string &);
   virtual float availableRetranslocateN(void);
   virtual void onEmergence();
   virtual void get_name(vector<string> &name);
   virtual void get_p_demand(vector<float> &p_demand);
   virtual void get_dlt_p_retrans(vector<float> &dlt_p_retrans);
   virtual void get_dlt_dm_senesced(vector<float> &);
   virtual void get_dlt_dm_detached(vector<float> &);
   virtual void get_dlt_dm_green_retrans(vector<float> &);
   virtual void get_dlt_dm_green(vector<float> &);
   virtual void get_dm_senesced(vector<float> &);
   virtual void get_dm_green(vector<float> &);
   virtual void get_n_demanded(vector<float> &);
   virtual void get_dm_plant_min(vector<float> &);

   virtual void morphology(void);

   virtual void doNDemand1(float, float);
   virtual void doNDemand1Pot(float, float);
   virtual void doNDemand2(float, float);
   virtual void doSoilNDemand(void);
   virtual void doNSenescence(void);
   virtual void doPDemand(void);
   virtual void Detachment(void);
   virtual void doPSenescence(void);
   virtual void  doRemoveBiomass(protocol::RemoveCropDmType dmRemoved, string &c_remove_biomass_report);
   virtual void  removeBiomass(void);
   virtual void  removeBiomass2(float);

   virtual void doGrainNumber (void);

   virtual void onHarvest(float height, float remove_fr,
                          vector<string> &dm_type,
                          vector<float> &dlt_crop_dm,
                          vector<float> &dlt_dm_n,
                          vector<float> &dlt_dm_p,
                          vector<float> &fraction_to_residue);

   virtual void onKillStem(void);

   virtual void onEndCrop(vector<string> &dm_type,
                          vector<float> &dlt_crop_dm,
                          vector<float> &dlt_dm_n,
                          vector<float> &dlt_dm_p,
                          vector<float> &fraction_to_residue);

   virtual void zeroAllGlobals(void);
   virtual void zeroDeltas(void);
   virtual void zeroDltNSenescedTrans(void);
   virtual void doNSenescedRetrans(float navail, float n_demand_tot);
   virtual void collectDetachedForResidue(vector<string> &part_name
                                          , vector<float> &dm_residue
                                          , vector<float> &dm_n
                                          , vector<float> &dm_p
                                          , vector<float> &fract);

   virtual void update(void);
   void fixPools();
   virtual void doNConccentrationLimits(float);
   virtual void zeroDltDmGreen(void);


   virtual float coverTotal(void) ;
   virtual float coverGreen(void) ;
   virtual float coverSen(void) ;
   virtual float interceptRadiationGreen(float radiation);
   virtual float interceptRadiationTotal(float radiation);
   virtual float grainNo(void);
   virtual float grainNConcPercent(void);
   virtual float nDemandGrain(void);

   virtual float dltDmGrainDemand(void);
   virtual float dltDmRetranslocate(void);
   virtual float dltDmRetranslocateSupply(float demand_differential) ;
   virtual float dltDmGreenRetransUptake(void);
   virtual float transpirationEfficiency(void);
   virtual float dltDmPotTe(void);
   virtual float dltDmPotRue(void);
   virtual float dltDm(void);

   virtual float dltDmGreen(void)  ;
   virtual float dltDmSenesced(void);
   virtual float dltDmDetached(void);
   virtual float dltDmGreenRetrans(void);

   virtual float dltDmGreenRemoved(void);
   virtual float dltDmSenescedRemoved(void);
   virtual float dltDmRemoved(void);
   virtual float dltNRemoved(void);

   virtual float grainWaterContent(void);
   virtual float dmGreenDemand(void);
   virtual float dmGrainTotal(void);
   virtual float dmGrainWetTotal(void);
   virtual float dmGreenGrainTotal(void);

   virtual float dmGreen(void);
   virtual float dmGreenVeg(void);
   virtual float dmSenescedVeg(void);
   virtual float dmSenesced(void);
   virtual float dmTotal(void);
   virtual float dmTotalVeg(void);
   virtual float grainWt(void);
   virtual float dmRetransSupply(void);
   virtual float dmRetransDemand(void) ;

   virtual float nTotal(void);
   virtual float nTotalVeg(void);
   virtual float nGreen(void);
   virtual float nGreenVeg(void);
   virtual float nSenesced(void);
   virtual float nSenescedVeg(void);
   virtual float nConcGrain(void);
   virtual float nDemandGrain2(void);
   virtual float nRetransSupply(void);
   virtual float nRetransDemand(void);

   virtual float nGrainTotal(void);
   virtual float nGreenGrainTotal(void);

   virtual float dltNRetransOut(void);
   virtual float dltNSenescedRetrans(void);
   virtual float dltNGreen(void);
   virtual float nDemand(void);
   virtual float soilNDemand(void);
   virtual float nCapacity(void);
   virtual void  doNPartition(float nSupply, float n_demand_sum, float n_capacity_sum);

   virtual float nMaxPot(void);
   virtual float nMax(void);
   virtual float nMinPot(void);
   virtual float pTotal(void);
   virtual float pTotalVeg(void);
   virtual float pGrainTotal(void);
   virtual float pGreenGrainTotal(void);
   virtual float pGreenVeg(void);
   virtual float pGreen(void);
   virtual float pSenescedGrainTotal(void);
   virtual float pSenescedVeg(void);
   virtual float pSenesced(void);
   virtual float pConcGrainTotal(void);
   virtual float pMaxPot(void);
   virtual float pMinPot(void);

   virtual float pDemand(void);
   virtual float pRetransSupply(void);
   virtual float pRetransDemand(void);

   virtual void display(ostream &os = cout) ;  // display function
   virtual void doCover (PlantSpatial &spatial);

   virtual void calcDlt_pod_area (void);   //FIXME

   virtual void doDmPotRUE (void );                      // (OUTPUT) potential dry matter (carbohydrate) production (g/m^2)
   virtual void doTECO2(void);                           // (OUTPUT) transpiration coefficient                         //FIXME
   virtual void doSWDemand(float SWDemandMaxFactor);     //(OUTPUT) crop water demand (mm)               //FIXME
   virtual float SWDemand(void);                         //(OUTPUT) crop water demand (mm)               //FIXME
   virtual float SWDemandTE(void);                       //(OUTPUT) crop water demand (mm)               //FIXME
   virtual void doDmPotTE(float swSupply);                         //(OUTPUT) potential dry matter production by transpiration (g/m^2)//FIXME

   virtual void doNDemandGrain(float nfact_grain_conc, float swdef_expansion);
   virtual void doDmDemand (float dlt_dm_supply_by_veg);
   virtual float giveDmGreen(float dmSupplied);
   virtual void doDmRetranslocate(float DMAvail, float DMDemandDifferentialTotal);
   virtual float dmDemandDifferential(void) ;
   virtual float nDemandDifferential(void);
   virtual void doNFixRetranslocate(float NFix, float NDemandDifferentialTotal);
   virtual void doBioActual (void);
   virtual void doSenescence (float sen_fr);
   virtual void doDmMin(void);

   virtual void doNInit (void);
   virtual void doNRetranslocate( float N_avail_rep, float grain_n_demand);

   virtual void doPPartition(float p_uptake, float total_p_demand);
   virtual void doPRetranslocate(float total_p_supply, float total_p_demand);

   virtual float dltPGreen(void);
   virtual float dltNSenesced(void);
   virtual float dltPSenesced(void);
   virtual float dltNDetached(void);
   virtual float dltPDetached(void);
   virtual float n_conc_crit(void);
   virtual float n_conc_min(void);
   virtual float dltNRetrans(void);
   virtual float dltNSenescedTrans(void);

   virtual bool isYieldPart(void);
   virtual bool isRetransPart(void);

#if TEST_CompositePart
   virtual ~CompositePart();                            // destructor
#else
   ~CompositePart();
#endif

protected:
   float dmGreenStressDeterminant(void);
   float pGreenStressDeterminant(void);
   float pMaxPotStressDeterminant(void);
   float pMinPotStressDeterminant(void);

   vector <plantPart *> myParts;

};

#endif

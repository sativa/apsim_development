
// Modification log
// 2 Feb 05 J. Hargreaves  Implementation

#ifndef CompositePart_H
#define CompositePart_H

#include "PlantPart.h"

class CompositePart : public plantPart
{
   friend ostream &operator<<(ostream &, const CompositePart &);
public:                                             // member functions
   CompositePart();    //default constructor
   CompositePart(plantInterface *p, const string &name);

   CompositePart(const CompositePart &CompositePart);           // copy constructor
   const CompositePart &operator=(const CompositePart &other);      // Assigment operator

   virtual void doInit1(protocol::Component *system);

   virtual void doRegistrations(protocol::Component *);
   virtual void doTick(protocol::TimeType &tick) ;
   virtual void doNewMet(protocol::NewMetType &newmet) ;
   virtual void readConstants (protocol::Component *, const string &);
   virtual void readSpeciesParameters (protocol::Component *, vector<string> &);
   virtual void readCultivarParameters (protocol::Component *, const string &);
   virtual void writeCultivarInfo (protocol::Component *);
   virtual void doProcessBioDemand(void);
   virtual void onDayOf(const string &);
   virtual float availableRetranslocateN(void);

   virtual void get_name(vector<string> &name);
   virtual void get_p_demand(vector<float> &p_demand);
   virtual void get_dlt_p_green(vector<float> &dlt_p_green);
   virtual void get_dlt_p_retrans(vector<float> &dlt_p_retrans);
   virtual void get_p_green(vector<float> &p_green);
   virtual void get_dlt_p_sen(vector<float> &);
   virtual void get_dlt_p_dead(vector<float> &);
   virtual void get_dlt_p_detached(vector<float> &);
   virtual void get_p_sen(vector<float> &);
   virtual void get_p_dead(vector<float> &);
   virtual void get_dlt_n_dead_detached(vector<float> &);
   virtual void get_dlt_n_detached(vector<float> &);
   virtual void get_dlt_n_senesced_trans(vector<float> &);
   virtual void get_dlt_n_senesced_retrans(vector<float> &);
   virtual void get_dlt_n_senesced_dead(vector<float> &);
   virtual void get_dlt_n_senesced(vector<float> &);
   virtual void get_dlt_n_retrans(vector<float> &);
   virtual void get_dlt_n_dead(vector<float> &);
   virtual void get_dlt_n_green(vector<float> &);
   virtual void get_n_dead(vector<float> &);
   virtual void get_n_senesced(vector<float> &);
   virtual void get_n_green(vector<float> &);
   virtual void get_dlt_dm_senesced_dead(vector<float> &);
   virtual void get_dlt_dm_green_dead(vector<float> &);
   virtual void get_dlt_dm_dead_detached(vector<float> &);
   virtual void get_dlt_dm_senesced(vector<float> &);
   virtual void get_dlt_dm_detached(vector<float> &);
   virtual void get_dlt_dm_green_retrans(vector<float> &);
   virtual void get_dlt_dm_green(vector<float> &);
   virtual void get_dm_senesced(vector<float> &);
   virtual void get_dm_dead(vector<float> &);
   virtual void get_dm_green(vector<float> &);
   virtual void get_n_demanded(vector<float> &);
   virtual void get_dm_plant_min(vector<float> &);

   virtual void morphology(void);

   virtual void doNDemand1(float, float);
   virtual void doNDemand1Pot(float, float);
   virtual void doNDemand2(float, float);
   virtual void doSoilNDemand(void);
   virtual void doNSenescence(void);
   virtual void doDmDetachment(void);
   virtual void doNDetachment(void);
   virtual void doPDetachment(void);
   virtual void doPDemand(void);
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
   virtual void collectDeadDetachedForResidue(vector<string> &part_name
                                              , vector<float> &dm_dead_detached
                                              , vector<float> &n_dead_detached
                                              , vector<float> &p_dead_detached
                                              , vector<float> &fract);

   virtual void update(void);
   virtual void doNConccentrationLimits(float);
   virtual void zeroDltDmGreen(void);


   virtual float coverTotal(void) ;
   virtual float coverGreen(void) ;
   virtual float coverSen(void) ;
   virtual float coverDead(void) ;
   virtual float interceptRadiationGreen(float radiation);
   virtual float interceptRadiationTotal(float radiation);
   virtual float grainNo(void) const;
   virtual float grainNConcPercent(void) const;
   virtual float nDemandGrain(void) const;

   virtual float dltDmGrainDemand(void) const;
   virtual float dltDmRetranslocate(void) const;
   virtual float dltDmRetranslocateSupply(float demand_differential) ;
   virtual float dltDmGreenRetransUptake(void) const ;
   virtual float transpirationEfficiency(void) const;
   virtual float dltDmPotTe(void) const;
   virtual float dltDmPotRue(void) const;
   virtual float dltDm(void) const;

   virtual float dltDmGreen(void) const ;
   virtual float dltDmDead(void) const;
   virtual float dltDmSenesced(void) const;
   virtual float dltDmDetached(void) const;
   virtual float dltDmGreenRetrans(void) const ;

   virtual float dltDmGreenRemoved(void) const;
   virtual float dltDmSenescedRemoved(void) const;
   virtual float dltDmDeadRemoved(void) const;
   virtual float dltDmRemoved(void) const;
   virtual float dltNRemoved(void) const;

   virtual float grainWaterContent(void) const;
   virtual float dmGreenDemand(void) const;
   virtual float dmTotal(void) const;
   virtual float dmGrainTotal(void) const;
   virtual float dmGrainWetTotal(void) const;
   virtual float dmVegTotal(void) const;
   virtual float dmGreenGrainTotal(void) const;
   virtual float dmGreen(void) const;
   virtual float dmGreenVegTotal(void) const;
   virtual float dmSenescedVegTotal(void) const;
   virtual float dmSenesced(void) const;
   virtual float dmDeadVegTotal(void) const;
   virtual float dmDead(void) const;
   virtual float grainWt(void) const;
   virtual float dmRetransSupply(void) const;
   virtual float dmRetransDemand(void) ;

   virtual float nTotal(void) const;
   virtual float nGrainTotal(void) const;
   virtual float nVegTotal(void) const;
   virtual float nGreenGrainTotal(void) const;
   virtual float nGreenVegTotal(void) const;
   virtual float nGreen(void) const;
   virtual float nSenescedVegTotal(void) const;
   virtual float nSenesced(void) const;
   virtual float nDeadVegTotal(void) const;
   virtual float nDead(void) const;
   virtual float nConcGrain(void) const;
   virtual float nDemandGrain2(void);
   virtual float nRetransSupply(void);
   virtual float nRetransDemand(void);
   virtual float dltNRetransOut(void);
   virtual float dltNSenescedRetrans(void) const;
   virtual float dltNGreen(void)const;
   virtual float nDemand(void) const;
   virtual float soilNDemand(void);
   virtual float nCapacity(void);
   virtual void  doNPartition(float nSupply, float n_demand_sum, float n_capacity_sum);

   virtual float nMaxPot(void) const;
   virtual float nMax(void) const;
   virtual float nMinPot(void) const;
   virtual float pTotal(void) const;
   virtual float pGrainTotal(void) const;
   virtual float pVegTotal(void) const;
   virtual float pGreenGrainTotal(void) const;
   virtual float pDeadGrainTotal(void) const;
   virtual float pGreenVegTotal(void) const;
   virtual float pGreen(void) const;
   virtual float pSenescedGrainTotal(void) const;
   virtual float pSenescedVegTotal(void) const;
   virtual float pSenesced(void) const;
   virtual float pDeadVegTotal(void) const;
   virtual float pDead(void) const;
   virtual float pConcGrain(void) const;
   virtual float pConcGrainTotal(void) const;
   virtual float pMaxPot(void) const;
   virtual float pMinPot(void) const;

   virtual float pDemand(void);
   virtual float pRetransSupply(void);
   virtual float pRetransDemand(void);

   virtual void doPInit(void);

   virtual void display(ostream &os = cout) const;  // display function
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
   virtual float dmDemandDifferential(void) const;
   virtual float nDemandDifferential(void);
   virtual void doNFixRetranslocate(float NFix, float NDemandDifferentialTotal);
   virtual void doBioActual (void);
   virtual void doSenescence1 (float sen_fr);
   virtual void doSenescence2 (float sen_fr);
   virtual void doDmMin(void);

   virtual void doNInit (void);
   virtual void doNRetranslocate( float N_avail_rep, float grain_n_demand);

   virtual void doPPartition(float p_uptake, float total_p_demand);
   virtual void doPRetranslocate(float total_p_supply, float total_p_demand);

   virtual float dltPGreen(void) const;
   virtual float dltNDead(void) const;
   virtual float dltPDead(void) const;
   virtual float dltNSenesced(void) const;
   virtual float dltPSenesced(void) const;
   virtual float dltNDetached(void) const;
   virtual float dltPDetached(void) const;
   virtual float nConc(void)const;
   virtual float nConcPercent(void)const;
   virtual float pConc(void)const;
   virtual float pConcPercent(void) const;
   virtual float n_conc_crit(void) const;
   virtual float n_conc_min(void) const;
   virtual float dltNRetrans(void) const;
   virtual float dltNSenescedTrans(void) const;

   virtual bool isYieldPart(void) const;
   virtual bool isRetransPart(void) const;

#if TEST_CompositePart
   virtual ~CompositePart();                            // destructor
#else
   ~CompositePart();
#endif


   //      protected:
   //         plantInterface *plant;                 // The plant we are attached to

protected:
   float dmGreenStressDeterminant(void);
   float pGreenStressDeterminant(void);
   float pMaxPotStressDeterminant(void);
   float pMinPotStressDeterminant(void);

   virtual void get_DMGreen(protocol::Component *, protocol::QueryValueData &);
   virtual void get_NGreen(protocol::Component *, protocol::QueryValueData &);
   virtual void get_PGreen(protocol::Component *, protocol::QueryValueData &);
   virtual void get_DMDead(protocol::Component *, protocol::QueryValueData &);
   virtual void get_NDead(protocol::Component *, protocol::QueryValueData &);
   virtual void get_PDead(protocol::Component *, protocol::QueryValueData &);
   virtual void get_DMSenesced(protocol::Component *, protocol::QueryValueData &);
   virtual void get_NSenesced(protocol::Component *, protocol::QueryValueData &);
   virtual void get_PSen(protocol::Component *, protocol::QueryValueData &);
   virtual void get_Dlt_dm_green(protocol::Component *, protocol::QueryValueData &);
   virtual void get_Dlt_n_green(protocol::Component *, protocol::QueryValueData &);
   virtual void get_Dlt_p_green(protocol::Component *, protocol::QueryValueData &);
   virtual void get_Dlt_dm_dead(protocol::Component *, protocol::QueryValueData &);
   virtual void get_Dlt_n_dead(protocol::Component *, protocol::QueryValueData &);
   virtual void get_Dlt_p_dead(protocol::Component *, protocol::QueryValueData &);
   virtual void get_Dlt_dm_senesced(protocol::Component *, protocol::QueryValueData &);
   virtual void get_Dlt_n_senesced(protocol::Component *, protocol::QueryValueData &);
   virtual void get_Dlt_p_sen(protocol::Component *, protocol::QueryValueData &);
   virtual void get_Dlt_dm_detached(protocol::Component *, protocol::QueryValueData &);
   virtual void get_Dlt_n_detached(protocol::Component *, protocol::QueryValueData &);
   virtual void get_Dlt_p_det(protocol::Component *, protocol::QueryValueData &);
   virtual void get_n_conc(protocol::Component *, protocol::QueryValueData &);
   virtual void get_p_conc(protocol::Component *, protocol::QueryValueData &);
   virtual void get_n_conc_crit(protocol::Component *, protocol::QueryValueData &);
   virtual void get_n_conc_min(protocol::Component *, protocol::QueryValueData &);
   virtual void get_NDemand(protocol::Component *, protocol::QueryValueData &);
   virtual void get_Dlt_n_retrans(protocol::Component *, protocol::QueryValueData &);
   virtual void get_Dlt_n_senesced_retrans(protocol::Component *, protocol::QueryValueData &);
   virtual void get_Dlt_n_senesced_trans(protocol::Component *, protocol::QueryValueData &);
   virtual void get_Dlt_dm_green_retrans(protocol::Component *, protocol::QueryValueData &);



   /* system interface: */
////   UInt2SetFnMap   IDtoSetFn;    /* setVariable */

   vector <plantPart *> myParts;
////   vector <plantPart *>::iterator part;

};

#endif

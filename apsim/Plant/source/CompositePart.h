
// Modification log
// 2 Feb 05 J. Hargreaves  Implementation

#ifndef CompositePart_H
#define CompositePart_H

#include "PlantPart.h"

class CompositePart : public plantPart
{
   friend ostream &operator<<(ostream &, const CompositePart &);
public:												// member functions
   CompositePart(plantInterface *p, const string &name);

   const CompositePart &operator=(const CompositePart &other);		// Assigment operator

   virtual void doInit1();

   virtual void doRegistrations(protocol::Component *);
   virtual void doTick(protocol::timeType &tick) ;
   virtual void doNewMet(protocol::newmetType &newmet) ;
   virtual void readConstants (protocol::Component *, const string &);
   virtual void readSpeciesParameters (protocol::Component *, vector<string> &);
   virtual void readCultivarParameters (protocol::Component *, const string &);
   virtual void writeCultivarInfo (protocol::Component *);
   virtual void doProcessBioDemand(void);
   virtual void onDayOf(const string &);
   virtual float availableRetranslocateN(void);


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
   virtual float interceptRadiation(float radiation);        //FIXME
   virtual float grainNo(void) ;
   virtual float grainNConcPercent(void);
   virtual float nDemandGrain(void) ;

   virtual float dltDmGrainDemand(void) ;
   virtual float dltDmRetranslocate(void) ;
   virtual float dltDmRetranslocateSupply(float demand_differential) ;
   virtual float dltDmGreenRetransUptake(void) ;
   virtual float dltDmPotTe(void);            //FIXME
   virtual float dltDmPotRue(void);           //FIXME

   virtual float dltDmGreen(void) ;
   virtual float dltDmDead(void) ;
   virtual float dltDmSenesced(void) ;
   virtual float dltDmDetached(void) ;
   virtual float dltDmGreenRetrans(void) ;

   virtual float dmGreenDemand(void) ;     
   virtual float dmTotal(void) ;
   virtual float dmGrainTotal(void) ;
   virtual float dmVegTotal(void);
   virtual float dmGreenGrainTotal(void);
   virtual float dmGreen(void);
   virtual float dmGreenVegTotal(void);
   virtual float dmSenescedVegTotal(void);
   virtual float dmSenesced(void) ;
   virtual float dmDeadVegTotal(void);
   virtual float dmDead(void) ;
   virtual float grainWt(void);
   virtual float dmRetransSupply(void) ;
   virtual float dmRetransDemand(void) ;

   virtual float nTotal(void);
   virtual float nGrainTotal(void);
   virtual float nVegTotal(void);
   virtual float nGreenGrainTotal(void);
   virtual float nGreenVegTotal(void);
   virtual float nGreen(void) ;
   virtual float nSenescedVegTotal(void);
   virtual float nSenesced(void);
   virtual float nDeadVegTotal(void);
   virtual float nDead(void);
   virtual float nConcGrain(void);
   virtual float nDemandGrain2(void);
   virtual float nRetransSupply(void);
   virtual float nRetransDemand(void);
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
   virtual float pGrainTotal(void);
   virtual float pVegTotal(void);
   virtual float pGreenGrainTotal(void);
   virtual float pDeadGrainTotal(void);
   virtual float pGreenVegTotal(void);
   virtual float pGreen(void);
   virtual float pSenescedGrainTotal(void);
   virtual float pSenescedVegTotal(void);
   virtual float pSenesced(void);
   virtual float pDeadVegTotal(void);
   virtual float pDead(void);
   virtual float pConcGrain(void);
   virtual float pConcGrainTotal(void);
   virtual float pMaxPot(void);
   virtual float pMinPot(void);

   virtual float pDemand(void);
   virtual float pRetransSupply(void);
   virtual float pRetransDemand(void);

   virtual void doPInit(void);

   virtual void display(ostream &os = cout) const;	// display function
   virtual float calcCover (float canopy_fac);                  // return pod cover   //FIXME

   virtual void calcDlt_pod_area (void);   //FIXME

   virtual void doDmPotRUE (double  radn_int_pod);                      //FIXME   // (OUTPUT) potential dry matter (carbohydrate) production (g/m^2)
   virtual void doTECO2(void);                                       // (OUTPUT) transpiration coefficient                         //FIXME
   virtual float SWDemand(void);                           //(OUTPUT) crop water demand (mm)               //FIXME
   virtual void doDmPotTE(void);                                       //(OUTPUT) potential dry matter production by transpiration (g/m^2)//FIXME

   virtual void doNDemandGrain(float nfact_grain_conc, float swdef_expansion);
   virtual void doDmDemand (float dlt_dm_supply_by_veg);
   virtual float giveDmGreen(float dmSupplied);
   virtual void doDmRetranslocate(float DMAvail, float DMDemandDifferentialTotal);
   virtual float dmDemandDifferential(void) ;
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

   virtual float dltPGreen(void) ;
   virtual float dltNDead(void) ;
   virtual float dltPDead(void) ;
   virtual float dltNSenesced(void) ;
   virtual float dltPSenesced(void) ;
   virtual float dltNDetached(void) ;
   virtual float dltPDetached(void) ;
   virtual float nConc(void);
   virtual float nConcPercent(void);
   virtual float pConc(void);
   virtual float pConcPercent(void);
   virtual float n_conc_crit(void) ;
   virtual float n_conc_min(void) ;
   virtual float dltNRetrans(void) ;
   virtual float dltNSenescedTrans(void) ;

   virtual bool isYieldPart(void);
   virtual bool isRetransPart(void);

#if TEST_CompositePart
   virtual ~CompositePart();							// destructor
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

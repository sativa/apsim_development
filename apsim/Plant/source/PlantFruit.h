
// Modification log
// 2 Feb 05 J. Hargreaves  Implementation

#ifndef PLANTFRUIT_H
#define PLANTFRUIT_H

#ifndef __CSTRING_H
#include <cstring.h>
#endif

#ifndef __IOSTREAM_H
#include <iostream.h>
#endif

#include "PlantParts.h"
#include "GrainPart.h"
#include "PodPart.h"



class PlantFruit : public plantPart
{
   friend ostream &operator<<(ostream &, const PlantFruit &);
public:												// member functions
   PlantFruit(plantInterface *p, const string &name);

   //		PlantFruit(const PlantFruit &PlantFruit); 			// copy constructor
   const PlantFruit &operator=(const PlantFruit &other);		// Assigment operator

   void doInit1();

   void doRegistrations(protocol::Component *);
   void doTick(protocol::timeType &tick) ;
   void doNewMet(protocol::newmetType &newmet) ;
   void readConstants (protocol::Component *, const string &);
   void readSpeciesParameters (protocol::Component *, vector<string> &);
   void readCultivarParameters (protocol::Component *, const string &);
   void writeCultivarInfo (protocol::Component *);
   void doProcessBioDemand(void);
   void onDayOf(const string &);
   float availableRetranslocateN(void);

   void get_head_wt(protocol::Component *, protocol::QueryValueData &);
   void get_head_n(protocol::Component *, protocol::QueryValueData &);

   void get_pod_n(protocol::Component *, protocol::QueryValueData &);
   void get_pod_p(protocol::Component *, protocol::QueryValueData &qd);
   void get_head_p(protocol::Component *, protocol::QueryValueData &qd);

   void get_p_demand(vector<float> &p_demand);
   void get_dlt_p_green(vector<float> &dlt_p_green);
   void get_dlt_p_retrans(vector<float> &dlt_p_retrans);
   void get_p_green(vector<float> &p_green);
   void get_dlt_p_sen(vector<float> &);
   void get_dlt_p_dead(vector<float> &);
   void get_dlt_p_detached(vector<float> &);
   void get_p_sen(vector<float> &);
   void get_p_dead(vector<float> &);
   void get_dlt_n_dead_detached(vector<float> &);
   void get_dlt_n_detached(vector<float> &);
   void get_dlt_n_senesced_trans(vector<float> &);
   void get_dlt_n_senesced_retrans(vector<float> &);
   void get_dlt_n_senesced_dead(vector<float> &);
   void get_dlt_n_senesced(vector<float> &);
   void get_dlt_n_retrans(vector<float> &);
   void get_dlt_n_dead(vector<float> &);
   void get_dlt_n_green(vector<float> &);
   void get_n_dead(vector<float> &);
   void get_n_senesced(vector<float> &);
   void get_n_green(vector<float> &);
   void get_dlt_dm_senesced_dead(vector<float> &);
   void get_dlt_dm_green_dead(vector<float> &);
   void get_dlt_dm_dead_detached(vector<float> &);
   void get_dlt_dm_senesced(vector<float> &);
   void get_dlt_dm_detached(vector<float> &);
   void get_dlt_dm_green_retrans(vector<float> &);
   void get_dlt_dm_green(vector<float> &);
   void get_dm_senesced(vector<float> &);
   void get_dm_dead(vector<float> &);
   void get_dm_green(vector<float> &);
   void get_n_demanded(vector<float> &);
   void get_dm_plant_min(vector<float> &);

   void morphology(void);

   void doNDemand1(float, float);
   void doNDemand1Pot(float, float);
   void doNDemand2(float, float);
   void doSoilNDemand(void);
   void doNSenescence(void);
   void doDmDetachment(void);
   void doNDetachment(void);
   void doPDetachment(void);
   void doPDemand(void);
   void doPSenescence(void);

   void doGrainNumber (void);

   void onHarvest(float height, float remove_fr,
                  vector<string> &dm_type,
                  vector<float> &dlt_crop_dm,
                  vector<float> &dlt_dm_n,
                  vector<float> &dlt_dm_p,
                  vector<float> &fraction_to_residue);

   void onKillStem(void);

   void onEndCrop(vector<string> &dm_type,
                  vector<float> &dlt_crop_dm,
                  vector<float> &dlt_dm_n,
                  vector<float> &dlt_dm_p,
                  vector<float> &fraction_to_residue);

   void zeroAllGlobals(void);
   void zeroDeltas(void);
   void zeroDltNSenescedTrans(void);
   void doNSenescedRetrans(float navail, float n_demand_tot);
   void collectDetachedForResidue(vector<string> &part_name
                                  , vector<float> &dm_residue
                                  , vector<float> &dm_n
                                  , vector<float> &dm_p
                                  , vector<float> &fraction_to_residue);
   void collectDeadDetachedForResidue(vector<string> &part_name
                                      , vector<float> &dm_dead_detached
                                      , vector<float> &n_dead_detached
                                      , vector<float> &p_dead_detached
                                      , vector<float> &fraction_to_residue);

   void update(void);
   void doNConccentrationLimits(void);
   void zeroDltDmGreen(void);


   float coverTotal(void) const;
   float coverGreen(void) const;
   float coverSen(void) const;
   float coverDead(void) const;
   float interceptRadiation(float radiation);        //FIXME
   float grainNo(void) const;
   float grainNConcPercent(void);
   float nDemandGrain(void) const;

   float dltDmGrainDemand(void) const;
   float dltDmRetranslocate(void);
   float dltDmGreen(void);
   float dltDmPotTe(void);            //FIXME
   float dltDmPotRue(void);        //FIXME
   float dltDmGreenUptake(void);
   float dltDmGreenRetransUptake(void);
   float dltDmDetached(void);

   float dmGreenDemand(void);
   float dmTotal(void);
   float dmGrainTotal(void);
   float dmVegTotal(void);
   float dmGreenGrainTotal(void);
   float dmGreen(void);
   float dmGreenVegTotal(void);
   float dmSenescedVegTotal(void);
   float dmSenesced(void);
   float dmDeadVegTotal(void);
   float dmDead(void);
   float grainWt(void);
   float dmRetransSupply(void);
   float dmRetransDemand(void);

   float nTotal(void);
   float nGrainTotal(void);
   float nVegTotal(void);
   float nGreenGrainTotal(void);
   float nGreenVegTotal(void);
   float nGreen(void);
   float nSenescedVegTotal(void);
   float nSenesced(void);
   float nDeadVegTotal(void);
   float nDead(void);
   float nConcGrain(void);
   float nDemandGrain2(void);
   float nRetransSupply(void);
   float nRetransDemand(void);
   float dltNRetransOut(void);
   float dltNSenescedRetrans(void);
   float dltNGreen(void);
   float nDemand(void);
   float soilNDemand(void);
   float nCapacity(void);
   void  doNPartition(float nSupply, float n_demand_sum, float n_capacity_sum);

   float nMaxPot(void);
   float nMax(void);
   float nMinPot(void);
   float pTotal(void);
   float pGrainTotal(void);
   float pVegTotal(void);
   float pGreenGrainTotal(void);
   float pDeadGrainTotal(void);
   float pGreenVegTotal(void);
   float pGreen(void);
   float pSenescedGrainTotal(void);
   float pSenescedVegTotal(void);
   float pSenesced(void);
   float pDeadVegTotal(void);
   float pDead(void);
   float pConcGrain(void);
   float pConcGrainTotal(void);
   float pMaxPot(void);
   float pMinPot(void);
   void  updatePDet(void);

   float pDemand(void);
   float pRetransSupply(void);
   float pRetransDemand(void);

   void doPInit(void);

   virtual void display(ostream &os = cout) const;	// display function
   float calcCover (float canopy_fac);                  // return pod cover   //FIXME

   void calcDlt_pod_area (void);   //FIXME

   void doDmPotRUE (double  radn_int_pod);                      //FIXME   // (OUTPUT) potential dry matter (carbohydrate) production (g/m^2)
   void doTECO2();                                       // (OUTPUT) transpiration coefficient                         //FIXME
   float SWDemand(void);                           //(OUTPUT) crop water demand (mm)               //FIXME
   void doDmPotTE(void);                                       //(OUTPUT) potential dry matter production by transpiration (g/m^2)//FIXME

   void doNDemandGrain(float nfact_grain_conc, float swdef_expansion);
   void doDmDemand (float dlt_dm_supply_by_veg);
   void doDmPartition(float DMAvail, float DMDemandTotal);
   void doDmRetranslocate(float DMAvail, float DMDemandDifferentialTotal);
   float dmDemandDifferential(void);
   float nDemandDifferential(void);
   void doNFixRetranslocate(float NFix, float NDemandDifferentialTotal);
   void doBioActual (void);
   void doSenescence1 (float sen_fr);
   void doSenescence2 (float sen_fr);
   void doDmMin(void);

   void doNInit (void);
   void doNRetranslocate( float N_avail_rep, float grain_n_demand);

   void doPPartition(float p_uptake, float total_p_demand);
   void doPRetranslocate(float total_p_supply, float total_p_demand);

#if TEST_PlantFruit
   virtual ~PlantFruit();							// destructor
#else
   ~PlantFruit();
#endif

private:
   void refreshStates(void);
   float dmGreenStressDeterminant(void);
   float pGreenStressDeterminant(void);
   float pMaxPotStressDeterminant(void);
   float pMinPotStressDeterminant(void);



   /* system interface: */
   UInt2SetFnMap   IDtoSetFn;    /* setVariable */

   vector <plantPart *> myParts;
   vector <plantPart *> myGrainParts;
   vector <plantPart *> myVegParts;
   vector<plantPart *> supplyPools;

   fruitPodPart  *podPart;
   fruitGrainPart  *grainPart;

   bool  gHasreadconstants;
   float dmRetranslocate;
   float gDlt_dm;


   // The plant we hook into
   PlantComponent *parentPlant;
};

#endif

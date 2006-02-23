
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

class PlantComponent;
class ApsimVariant;
class plantThing;
class eventObserver;
class Plant;
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

   void doInit(PlantComponent *systemInterface, PlantPhenology *plantPhenology);
   void doInit1();


   void doRegistrations(protocol::Component *);
   void doTick(protocol::timeType &tick) ;
   void doNewMet(protocol::newmetType &newmet) ;
   void readConstants (protocol::Component *, const string &);
   void readSpeciesParameters (protocol::Component *, vector<string> &);
   void readCultivarParameters (protocol::Component *, const string &);
   void writeCultivarInfo (protocol::Component *);
   void processBioDemand(void);
   void onDayOf(const string &);
   float availableRetranslocateN(void);

   void get_head_wt(protocol::Component *, protocol::QueryValueData &);
   void get_head_n(protocol::Component *, protocol::QueryValueData &);

   void get_pod_n(protocol::Component *, protocol::QueryValueData &);           //remove to pod
   void get_pod_p(protocol::Component *, protocol::QueryValueData &qd);         //remove to pod
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
   void dm_detachment1(void);
   void n_detachment1(void);
   void p_detachment1(void);
   void doPDemand(void);
   void doPSenescence(void);

   void grain_number (void);

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
   void n_conc_limits(void);
   void zeroDltDmGreen(void);


   float coverTotal(void) const;
   float coverGreen(void) const;
   float coverSen(void) const;
   float coverDead(void) const;
   float interceptRadiation(float radiation);        //FIXME
   float grainNo(void) const;
   float grainNConcPercent(void);
   float grainNDemand(void) const;

   float dltDmGrainDemand(void) const;
   float dltDmRetranslocate(void);
   float dltDmGreen(void);
   float dltDmPotTe(void);            //FIXME
   float dltDmPotRuePod(void);        //FIXME

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
   float nGrainDemand2(void);
   float nRetransSupply(void);
   float nRetransDemand(void);
   float dltNRetransOut(void);
   float dltNGreen(void);
   float nDemand(void);
   float soilNDemand(void);
   float nCapacity(void);
   void  nPartition(float nSupply);
   void  nFix(float nSupply);

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

   void pInit(void);

   virtual void display(ostream &os = cout) const;	// display function
   float calcCover (float canopy_fac);                  // return pod cover   //FIXME

   void calcDlt_pod_area (void);   //FIXME

   void dm_pot_rue (double  radn_int_pod);                      //FIXME   // (OUTPUT) potential dry matter (carbohydrate) production (g/m^2)
   void transp_eff_co2();                                       // (OUTPUT) transpiration coefficient                         //FIXME
   void sw_demand1(float *sw_demand);                           //(OUTPUT) crop water demand (mm)               //FIXME
   void bio_water1(void);                                       //(OUTPUT) potential dry matter production by transpiration (g/m^2)//FIXME

   void grain_n_demand1(float G_nfact_grain_conc
                        , float G_swdef_expansion);

   void grain_n_demand2(void);

   float dm_yield_demand ( float  g_dlt_dm_veg);

   float dm_yield_demand2 ( float  g_dlt_dm_veg);

   void yieldpart_demand_stress1(void);               //remove
   void dm_partition1 (double g_dlt_dm);
   void dm_retranslocate1(float  g_dlt_dm_retrans_to_fruit) ;
   void bio_actual (void);
   void doSenescence1 (float sen_fr);
   void doSenescence2 (float sen_fr);
   void doDmMin(void);

   void n_conc_grain_limits(void) ;

   void nit_init (void);
   void n_retranslocate(void);
   void doNRetranslocate( float N_avail_rep, float g_grain_n_demand);

#if TEST_PlantFruit
   virtual ~PlantFruit();							// destructor
#else
   ~PlantFruit();
#endif


   //      protected:
   //         plantInterface *plant;                 // The plant we are attached to

private:
   void refreshStates(void);
   void grain_number (float stem_dm
                      ,float pGrains_per_gram_stem
                      ,float *gGrain_no);
   float dmGreenStressDeterminant(void);
   float pGreenStressDeterminant(void);
   float pMaxPotStressDeterminant(void);
   float pMinPotStressDeterminant(void);
   void distributeDltPGreen(float p_uptake, float total_p_demand);
   void distributeDltPRetrans(float total_p_supply, float total_p_demand);



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
   float gDlt_dm_stress_max;                          // maximum daily stress on dm production (0-1)


   // The plant we hook into
   PlantComponent *parentPlant;
   PlantPhenology *phenology;

};

#endif

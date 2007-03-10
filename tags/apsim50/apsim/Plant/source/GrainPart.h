
// Modification log
// 2 Feb 05 J. Hargreaves  Implementation

#ifndef GrainPart_H
#define GrainPart_H

#ifndef __CSTRING_H
#include <cstring.h>
#endif

#ifndef __IOSTREAM_H
#include <iostream.h>
#endif
#include "PlantParts.h"
#include "OilPart.h"
#include "MealPart.h"

class fruitGrainPart : public plantPart
{
   friend ostream &operator<<(ostream &, const fruitGrainPart &);
public:												// member functions
   fruitGrainPart(plantInterface *p, const string &name);

   //		fruitGrainPart(const fruitGrainPart &fruitGrainPart); 			// copy constructor
   const fruitGrainPart &operator=(const fruitGrainPart &other);		// Assigment operator

   void doInit(PlantComponent *systemInterface, PlantPhenology *plantPhenology);
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

   //             bool set_plant_grain_oil_conc(protocol::QuerySetValueData&v);

   void get_grain_size(protocol::Component *system, protocol::QueryValueData &qd);
   //  void get_dlt_dm_grain_demand(protocol::Component *, protocol::QueryValueData &);
   void get_grain_wt(protocol::Component *, protocol::QueryValueData &);
   void get_yield(protocol::Component *, protocol::QueryValueData &);

   void get_n_conc_grain(protocol::Component *, protocol::QueryValueData &);
   void get_grain_n(protocol::Component *, protocol::QueryValueData &);
   void get_grain_n_demand(protocol::Component *, protocol::QueryValueData &);
   void get_grain_protein(protocol::Component *, protocol::QueryValueData &);
   //  void get_p_grain_pcnt(protocol::Component *, protocol::QueryValueData &qd);
   void get_n_conc_meal(protocol::Component *, protocol::QueryValueData &);

   //  void get_grain_oil_conc(protocol::Component *, protocol::QueryValueData &);

   void get_grain_p(protocol::Component *, protocol::QueryValueData &qd);
   void get_p_conc_grain(protocol::Component *, protocol::QueryValueData &qd);
   //  void get_grain_p_demand(protocol::Component *, protocol::QueryValueData &qd);

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
   //            void onEmergence(void);
   //            void onFlowering(void);
   //            void onStartGrainFill(void);

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
   void zeroDltDmGreenRetrans(void);


   float grainEnergy(void) const;        //remove
   float grainNo(void) const;
   float nConcPercent(void);
   float nDemandGrain(void) const;

   float dltDmDemand(void) const;
   float dltDmRetranslocateSupply(float demand_differential);
   float dltDmRetranslocate(void);
   float dltDmGreen(void);
   float dltDmGreenUptake(void);
   float dltDmGreenRetransUptake(void);
   float dltDmDetached(void);

   float dmTotal(void);
   float dmGreen(void);
   float dmSenesced(void);
   float dmDead(void);
   float grainWt(void);
   float dmRetransSupply(void);
   float dmRetransDemand(void);

   float nTotal(void);
   float nGreen(void);
   float nSenesced(void);
   float nDead(void);
   float nConc(void);
   float nDemand2(void);
   float nRetransSupply(void);
   float nRetransDemand(void);
   float dltNRetransOut(void);
   float dltNSenescedRetrans(void);
   float dltNGreen(void);
   float nDemand(void);
   float soilNDemand(void);
   void  doNPartition(float nSupply, float n_demand_sum, float n_capacity_sum);

   float pTotal(void);
   float pGreen(void);
   float pSenesced(void);
   float pDead(void);
   float pConc(void);
   float pConcTotal(void);
   void  updatePDet(void);

   float pDemand(void);
   float pRetransSupply(void);
   float pRetransDemand(void);

   void doPPartition(float p_uptake, float total_p_demand);
   void doPRetranslocate(float total_p_supply, float total_p_demand);
   void doPInit(void);

   float dmGreenStressDeterminant(void);
   float pGreenStressDeterminant(void);
   float pMaxPotStressDeterminant(void);
   float pMinPotStressDeterminant(void);

   virtual void display(ostream &os = cout) const;	// display function
   float calcCover (float canopy_fac);                  // return pod cover   //FIXME



   //FIXME
   void doNDemandGrain(float nfact_grain_conc, float swdef_expansion);
   void doNDemandGrain1(float nfact_grain_conc, float swdef_expansion);
   void doNDemandGrain2(void);

   float dmGreenDemand (void);
   float dmDemandDifferential(void);
   float doDmDemand (void);

   void doDmPartition(float DMAvail, float DMDemandTotal);
   void doDmRetranslocate(float DMAvail, float DMDemandDifferentialTotal);
   void doBioActual (void);
   void doSenescence1 (float sen_fr);
   void doSenescence2 (float sen_fr);
   void doDmMin(void);

   float nDemandDifferential(void);
   void doNFixRetranslocate(float NFix, float NDemandDifferentialTotal);

   void doNInit (void);
   void doNRetranslocate( float N_avail_rep, float g_grain_n_demand);

#if TEST_fruitGrainPart
   virtual ~fruitGrainPart();							// destructor
#else
   ~fruitGrainPart();
#endif


   //      protected:
   //         plantInterface *plant;                 // The plant we are attached to

private:
   void doDMDemandGrain1 (void) ;
   void doDMDemandGrain2(void) ;

   void refreshStates(void);
   void doDMDemandGrain (void);
   void doDMDemandStress(void);
   float meanT (void);

   float grainNumber (float stem_dm
                      ,float pGrains_per_gram_stem);

   float dltNGrainConc(plantPart *fruitGrainPart
                       , float sfac_slope      //(INPUT)  soil water stress factor slope
                       , float sw_fac_max      //(INPUT)  soil water stress factor maximum
                       , float temp_fac_min    //(INPUT)  temperature stress factor minimum optimum temp
                       , float tfac_slope      //(INPUT)  temperature stress factor slope
                       , float ave_temp        //(INPUT)  average air temperature (oC)
                       , float nfact_grain_conc// (INPUT)
                       , float swdef_expansion); // (INPUT)

   void doNConcGrainLimits(void) ;


   /* system interface: */
   UInt2SetFnMap   IDtoSetFn;    /* setVariable */

   vector <plantPart *> myParts;

   fruitOilPart  *oilPart;
   fruitMealPart  *mealPart;

   unsigned int idLatitude;

   float gDlt_dm_grain_demand;
   float gN_grain_demand;
   float gP_grain_demand;

   float gGrain_no;                 // multiplier of grain weight to account for seed energy content

   bool  gDelayGrnFill;
   int   gDaysDelayedGrnFill;

   bool  gHasreadconstants;
   float gMaxt;
   float gMint;
   float gLatitude;
   int gDay_of_year;
   int gYear;

   float dmRetranslocate;
   float gDlt_dm;

   stateObserver gDm_stress_max;                      // sum of maximum daily stress on dm production per phase
   float gDlt_dm_stress_max;                          // maximum daily stress on dm production (0-1)

   float cGrain_fill_option;
   float cX_temp_grainfill[max_table];
   int   cNum_temp_grainfill;
   float cY_rel_grainfill[max_table];
   float cGrain_no_option;
   float cGrain_n_option;
   float cSw_fac_max;
   float cTemp_fac_min;
   float cSfac_slope;
   float cTfac_slope;
   float cPotential_grain_n_filling_rate ;
   float cCrit_grainfill_rate;
   float cX_temp_grain_n_fill[max_table];
   int   cNum_temp_grain_n_fill;
   float cY_rel_grain_n_fill[max_table];
   float cGrn_water_cont;
   int   cNum_n_conc_stage;
   float cX_stage_code[max_table];
   float cN_conc_crit_grain;
   float cN_conc_max_grain;
   float cN_conc_min_grain;
   float cTwilight;                                   // twilight in angular distance between
                                                      // sunset and end of twilight - altitude
                                                      // of sun. (deg)
                                                      // to grain
   float pGrains_per_gram_stem;
   float pPotential_grain_filling_rate;

   float pX_pp_hi_incr[max_table];
   float pY_hi_incr[max_table];                       // harvest index increment per day ()
   int   pNum_pp_hi_incr;
   int   pNum_hi_max_pot;
   float pX_hi_max_pot_stress[max_table];             // maximum harvest index (g grain/g biomass)
   float pY_hi_max_pot[max_table];                    // maximum harvest index (g grain/g biomass)
   float pMinTempGrnFill;
   int   pDaysDelayGrnFill;



   // The plant we hook into
   //      Plant *plant;
   PlantComponent *parentPlant;
   PlantPhenology *phenology;
   stageSubject   otherObservers;            // Another collection of state variable observers

};

#endif
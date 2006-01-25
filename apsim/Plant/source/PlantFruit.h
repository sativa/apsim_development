
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
class PlantPhenology;
class ApsimVariant;
class plantPart;
class plantThing;
class eventObserver;
class Plant;
class fruitPodPart;
class fruitOilPart;
class fruitMealPart;

//typedef bool (PlantFruit::*ptr2setFn) (protocol::QuerySetValueData&);


      //*****        FIXME when this becomes proper class
//   Short description:
//      indices of plant part names
//const int  pod  = 0 ;
//const int  meal = 1 ; // excludes oil component
//const int  oil  = 2 ; // seed oil
// number of plant parts
//const int  max_part = 3 ; // NB. implies for (i=0; i < max_part; max_part++) usage

class PlantFruit : public plantPart
{
   typedef enum {pw_C3, pw_C4, pw_UNDEF} photosynthetic_pathway_t;    //  FIXME temporary until proper fruit class
   friend ostream &operator<<(ostream &, const PlantFruit &);
	public:												// member functions
            PlantFruit(plantInterface *p, const string &name);

//		PlantFruit(Plant *P);			// default constructor
//            PlantFruit(float greenLeaf, float greenStem, float senescedLeaf, float senescedStem, float deadLeaf, float deadStem);
//		PlantFruit(const PlantFruit &PlantFruit); 			// copy constructor
		const PlantFruit &operator=(const PlantFruit &other);		// Assigment operator

            void doInit(PlantComponent *systemInterface, PlantPhenology *plantPhenology);
            void doInit1();

//            void setValue(float greenLeaf, float greenStem, float senescedLeaf, float senescedStem, float deadLeaf, float deadStem);
//doRegistrations
//zeroAllGlobals
//zeroDeltas
//readCultivarParameters
//onDayOf
//readConstants
//readSpeciesParameters
//             bool setVariable(unsigned id, protocol::QuerySetValueData& qd) ;

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

//             bool set_plant_grain_oil_conc(protocol::QuerySetValueData&v);

             void get_grain_size(protocol::Component *system, protocol::QueryValueData &qd);
            //  void get_dlt_dm_grain_demand(protocol::Component *, protocol::QueryValueData &);
             void get_grain_wt(protocol::Component *, protocol::QueryValueData &);
             void get_head_wt(protocol::Component *, protocol::QueryValueData &);
             void get_yield(protocol::Component *, protocol::QueryValueData &);
              void get_head_n(protocol::Component *, protocol::QueryValueData &);

             void get_n_conc_grain(protocol::Component *, protocol::QueryValueData &);
              void get_grain_n(protocol::Component *, protocol::QueryValueData &);
              void get_grain_n_demand(protocol::Component *, protocol::QueryValueData &);
              void get_grain_protein(protocol::Component *, protocol::QueryValueData &);
            //  void get_p_grain_pcnt(protocol::Component *, protocol::QueryValueData &qd);
              void get_pod_n(protocol::Component *, protocol::QueryValueData &);
             void get_n_conc_meal(protocol::Component *, protocol::QueryValueData &);

            //  void get_grain_oil_conc(protocol::Component *, protocol::QueryValueData &);

              void get_grain_p(protocol::Component *, protocol::QueryValueData &qd);
              void get_p_conc_grain(protocol::Component *, protocol::QueryValueData &qd);
            //  void get_grain_p_demand(protocol::Component *, protocol::QueryValueData &qd);
              void get_pod_p(protocol::Component *, protocol::QueryValueData &qd);
              void get_head_p(protocol::Component *, protocol::QueryValueData &qd);

             void get_p_demand(vector<float> &p_demand);
             void get_dlt_p_green(vector<float> &dlt_p_green);
             void get_dlt_p_retrans(vector<float> &dlt_p_retrans);
             void get_p_green(vector<float> &p_green);

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
              void grain_number (float stem_dm
                                      ,float pGrains_per_gram_stem
                                      ,float *gGrain_no);

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
            void putStates(vector<plantPart *> fruitParts);
            void getDltNGreen(vector<plantPart *> fruitParts);
            void putDltNGreen(vector<plantPart *> fruitParts);
            void getDltDmGrainDemand(void) const;                 //??
            void getDltDmGreen(vector<plantPart *> fruitParts);
            void putDltDmGreen(vector<plantPart *> fruitParts);
            void getDltDmGreenRetrans(vector<plantPart *> fruitParts);
            void putDltDmGreenRetrans(vector<plantPart *> fruitParts);
            void putDltDmGreenSenesced(vector<plantPart *> fruitParts);
            void putDltNRetrans(vector<plantPart *> fruitParts);
            void doNSenescedRetrans(float navail, float n_demand_tot);
            void putNConcLimits(vector<plantPart *> fruitParts);

            void getPDemand(vector<plantPart *> fruitParts);
            void update(void);
            void refreshStates(void);
            void n_conc_limits(void);
            void zeroDltDmGreen(void);


            float coverTotal(void) const;
            float coverGreen(void) const;
            float coverSen(void) const;
            float coverDead(void) const;
            float interceptRadiation(float radiation);
            float grainEnergy(void) const;
            float grainNConcPercent(void);
            float grainNDemand(void) const;

            float dltDmGrainDemand(void) const;
            float dltDmRetranslocate(void);
            float dltDmGreen(void);

            float dmTotal(void);
            float dmGrainTotal(void);
            float dmVegTotal(void);
            float dmGreenGrainTotal(void);
            float dmGreenVegTotal(void);
            float dmSenescedVegTotal(void);
            float dmDeadVegTotal(void);
            float grainWt(void);
            float dmRetransSupply(void);
            float dmRetransDemand(void);

            float nTotal(void);
            float nGrainTotal(void);
            float nVegTotal(void);
            float nGreenGrainTotal(void);
            float nGreenVegTotal(void);
            float nSenescedVegTotal(void);
            float nDeadVegTotal(void);
            float nConcGrain(void);
            float nGrainDemand2(void);
            float nRetransSupply(void);
            float nRetransDemand(void);
            float dltNRetransOut(void);
            float dltNGreen(void);
            float nDemand(void);
            float nCapacity(void);
            void  nPartition(float nSupply);
            void  nFix(float nSupply);

            float nMaxPot(void);
            float nMinPot(void);
            float pTotal(void);
            float pGrainTotal(void);
            float pVegTotal(void);
            float pGreenGrainTotal(void);
            float pDeadGrainTotal(void);
            float pGreenVegTotal(void);
            float pSenescedGrainTotal(void);
            float pSenescedVegTotal(void);
            float pDeadVegTotal(void);
            float pConcGrain(void);
            float pConcGrainTotal(void);
            float pMaxPot(void);
            float pMinPot(void);
            void  updatePDet(void);

            float pDemand(void);
            float pRetransSupply(void);
            float pRetransDemand(void);

            void distributeDltPGreen(float p_uptake, float total_p_demand);
            void distributeDltPRetrans(float total_p_supply, float total_p_demand);
            void pInit(void);

           float dmGreenStressDeterminant(void);
           float pGreenStressDeterminant(void);
           float pMaxPotStressDeterminant(void);
           float pMinPotStressDeterminant(void);

		virtual void display(ostream &os = cout) const;	// display function
//            float calcCover (float pai);  // calc pod cover
            float calcCover (float canopy_fac);  // return pod cover

            float divide (float dividend, float divisor, float default_value) const;  // Command

            void calcDlt_pod_area (void);
            float meanT (void);

            void dm_pot_rue (double  radn_int_pod
                           , photosynthetic_pathway_t c_photosynthetic_pathway);                         // (OUTPUT) potential dry matter (carbohydrate) production (g/m^2)

            void rue_co2_modifier(photosynthetic_pathway_t photosyntheticType //!please use 'C3' or 'C4' for photosyntheticType
                                , float co2                                   // CO2 level (ppm)
                                , float meanT                                 // daily mean temp (oC)
                                , float *modifier);                           // modifier (-)

            void transp_eff_co2();      // (OUTPUT) transpiration coefficient
            void sw_demand1(float *sw_demand);        //(OUTPUT) crop water demand (mm)
            void bio_water1(void); //(OUTPUT) potential dry matter production by transpiration (g/m^2)
            void bio_grain_oil (void);
            void bio_grain_demand (void);
            void bio_yieldpart_demand1 (void) ;
            void bio_yieldpart_demand2(void) ;

            void grain_n_demand1(float G_nfact_grain_conc
                               , float G_swdef_expansion);

            void grain_n_demand2(void);

            float n_dlt_grain_conc(plantPart *grainPart
                                       , float sfac_slope      //(INPUT)  soil water stress factor slope
                                       , float sw_fac_max      //(INPUT)  soil water stress factor maximum
                                       , float temp_fac_min    //(INPUT)  temperature stress factor minimum optimum temp
                                       , float tfac_slope      //(INPUT)  temperature stress factor slope
                                       , float ave_temp        //(INPUT)  average air temperature (oC)
                                       , float nfact_grain_conc// (INPUT)
                                       , float swdef_expansion); // (INPUT)

            float dm_yield_demand ( float  g_dlt_dm_veg);

            float dm_yield_demand2 ( float  g_dlt_dm_veg);

            void yieldpart_demand_stress1(void);
            void dm_partition1 (double g_dlt_dm);
            void dm_partition2 (double g_dlt_dm);
            void dm_retranslocate1(float  g_dlt_dm_retrans_to_fruit) ;
            void dm_retranslocate2(float  g_dlt_dm_retrans_to_fruit) ;
            void bio_actual (void);
            void doSenescence1 (float sen_fr);
            void doSenescence2 (float sen_fr);
            void doDmMin(void);
               void retrans_init(float *dm_plant_min);

               void n_conc_grain_limits(void) ;

               void nit_init (void);
               void n_retranslocate(void);
               void doNRetranslocate( float N_avail_rep, float g_grain_n_demand);

#if TEST_PlantFruit
		virtual ~PlantFruit();							// destructor
#else
	     ~PlantFruit();
#endif

//      float cover_green
//      float cover_sen
//      float cover_dead

      struct Cover
      {
         float green;
         float sen;
         float dead;
      };

      Cover coverPod;

//      struct PlantPartType
//         {
//         float leaf;
//         float stem;
//         };
//
//         PlantPartType green;
//         PlantPartType senesced;
//         PlantPartType dead;

//      protected:
//         plantInterface *plant;                 // The plant we are attached to

      float cGrain_fill_option;
      float cX_temp_grainfill[max_table];
      int   cNum_temp_grainfill;
      float cY_rel_grainfill[max_table];

      float cGrain_oil_conc;                            // fractional oil content of grain (0-1)
      float gDlt_dm_grain_demand;
      float gDlt_dm_pot_rue_pod;
      float gDlt_dm_pot_te;
      float gN_grain_demand;
      float gP_grain_demand;
      float gDlt_dm_oil_conv;
      float dmOil_conv_retranslocate;

      float gGrain_no;                 // multiplier of grain weight to account for seed energy content

	private:
        /* system interface: */
        UInt2SetFnMap   IDtoSetFn;    /* setVariable */

//      vector <plantThing *> myThings;
      vector <plantPart *> myParts;
      vector <plantPart *> myGrainParts;
      vector <plantPart *> myVegParts;
      vector<plantPart *> supplyPools;

      fruitPodPart  *podPart;
      fruitOilPart  *oilPart;
      fruitMealPart  *mealPart;

      unsigned int idLatitude;

      float cExtinctionCoeffPod;
      float cSpec_pod_area;
      float cRue_pod;

      float gPai;
      float gDlt_pai;
      bool  gDelayGrnFill;
      int   gDaysDelayedGrnFill;
      float gTranspEff;                             // transpiration efficiency of fruit (g dm/m^2/mm water)
      bool  gHasreadconstants;
      float gMaxt;
      float gMint;
      float gLatitude;
      int gDay_of_year;
      int gYear;

      float gGrain_energy;                 // multiplier of grain weight to account for seed energy content
      float dmRetranslocate;
      float gDlt_dm;

      stateObserver gDm_stress_max;                      // sum of maximum daily stress on dm production per phase
      float gDlt_dm_stress_max;                          // maximum daily stress on dm production (0-1)

      float cSvp_fract;
      float cFrac_pod[max_table];                        // fraction of dm or grain weight allocated to pod
      float cX_stage_no_partition[max_table];
      float cY_frac_pod[max_table];                      // fraction of dm or grain weight allocated to pod
      int   cNum_stage_no_partition;
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
      float cCarbo_oil_conv_ratio;
      int   cNum_n_conc_stage;
      float cX_stage_code[max_table];
      float cY_n_conc_crit_pod[max_table];
      float cY_n_conc_max_pod[max_table];
      float cY_n_conc_min_pod[max_table];
      float cN_conc_crit_grain;
      float cN_conc_max_grain;
      float cN_conc_min_grain;
      float cTwilight;                                   // twilight in angular distance between
                                                        // sunset and end of twilight - altitude
                                                        // of sun. (deg)
      float cPod_trans_frac;                             // fraction of pod used in translocat
                                                        // to grain
      float cX_co2_te_modifier[max_table];
      float cY_co2_te_modifier[max_table];
      int   cNum_co2_te_modifier;
      float cTransp_eff_cf[max_table];                   // transpiration efficiency coefficient
                                                        // to convert vpd to
                                                        // transpiration efficiency (kpa)
                                                        // although this is expressed as a
                                                        // pressure it is really in the form
                                                        // kpa*g carbo per m^2 / g water per m^2
                                                        // and this can be converted to
                                                        // kpa*g carbo per m^2 / mm water
                                                        // because 1g water = 1 cm^3 water

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

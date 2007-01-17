
// Modification log
// 2 Feb 05 J. Hargreaves  Implementation

#ifndef GrainPart_H
#define GrainPart_H

#include "CompositePart.h"
#include "OilPart.h"
#include "MealPart.h"

class fruitGrainPart : public CompositePart
{
//   friend ostream &operator<<(ostream &, const fruitGrainPart &);
public:                                             // member functions
   fruitGrainPart(plantInterface *p, const string &name);
   fruitGrainPart();
   virtual ~fruitGrainPart() = 0;

   void doInit1(protocol::Component *system);

   void doRegistrations(protocol::Component *);
   void doTick(protocol::TimeType &tick) ;
   void doNewMet(protocol::NewMetType &newmet) ;
   void readConstants (protocol::Component *, const string &);
   void readSpeciesParameters (protocol::Component *, vector<string> &);
   void readCultivarParameters (protocol::Component *, const string &);
   void writeCultivarInfo (protocol::Component *);
   void doProcessBioDemand(void);

   //             bool set_plant_grain_oil_conc(protocol::QuerySetValueData&v);

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

   void morphology(void);

   void doNDemand1(float, float);
   void doNDemand1Pot(float, float);
   void doNDemand2(float, float);


   void onKillStem(void);
   //            void onEmergence(void);
   //            void onFlowering(void);
   //            void onStartGrainFill(void);


   void zeroAllGlobals(void);
   void zeroDeltas(void);

   void update(void);

   float grainEnergy(void);        //remove
   float nConcPercent(void)const;         //remove
   float grainNConcPercent(void) const;
   float nDemandGrain(void) const;
   float nDemandGrain2(void);

   float dltDmDemand(void);
   float dltDmGrainDemand(void) const;

   float nDemand2(void);

   float pConcGrain(void)const;
   float pConcGrainTotal(void) const;
   float pSenescedGrainTotal(void)const;     //remove

   virtual void display(ostream &os = cout) const;  // display function



   void doNDemandGrain(float nfact_grain_conc, float swdef_expansion);

   float calcDmDemand (void);

   void doNInit (void);
   void doNRetranslocate( float N_avail_rep, float g_grain_n_demand);



   //      protected:
   //         plantInterface *plant;                 // The plant we are attached to

protected:
   void doDMDemandGrain (void) ;

   void doDMDemandStress(void);
   float meanT (void);

   float dltNGrainConc(float sfac_slope      //(INPUT)  soil water stress factor slope
                       , float sw_fac_max      //(INPUT)  soil water stress factor maximum
                       , float temp_fac_min    //(INPUT)  temperature stress factor minimum optimum temp
                       , float tfac_slope      //(INPUT)  temperature stress factor slope
                       , float ave_temp        //(INPUT)  average air temperature (oC)
                       , float nfact_grain_conc// (INPUT)
                       , float swdef_expansion); // (INPUT)

   void doNConcGrainLimits(void) ;


   /* system interface: */
////   UInt2SetFnMap   IDtoSetFn;    /* setVariable */

   fruitOilPart  *oilPart;
   fruitMealPart  *mealPart;

   unsigned int idLatitude;

   float gDlt_dm_grain_demand;
   float gN_grain_demand;
   float gP_grain_demand;

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

   float cX_temp_grainfill[max_table];
   int   cNum_temp_grainfill;
   float cY_rel_grainfill[max_table];
   float cSw_fac_max;
   float cTemp_fac_min;
   float cSfac_slope;
   float cTfac_slope;
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
   float pMinTempGrnFill;
   int   pDaysDelayGrnFill;

   stageSubject   otherObservers;            // Another collection of state variable observers
};

#endif

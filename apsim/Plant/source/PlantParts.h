#ifndef PlantPartsH
#define PlantPartsH

#include <stdio.h>
#include <math.h>
#include <map>
#include <string>
#include <stdexcept>

#include <boost/function.hpp>
#include <boost/bind.hpp>

#include <ComponentInterface/Type.h>
#include <ComponentInterface/ApsimVariant.h>
#include <ComponentInterface/Component.h>
#include <ComponentInterface/dataTypes.h>
#include <ComponentInterface/Messages.h>
#include <ComponentInterface/MessageDataExt.h>
#include <ApsimShared/ApsimComponentData.h>
#include <ApsimShared/FStringExt.h>
#include <general/string_functions.h>

#include "PlantLibrary.h"
#include "PlantComponent.h"
#include "PlantInterface.h"
#include "Plant.h"


class plantPart : public plantThing
   {
   friend class Plant;
   public:
      // (global) state variables
      struct {

         float n_conc_crit;                  // critical N concentration (g N/g biomass)
         float n_conc_max;                   // maximum N concentration (g N/g biomass)
         float n_conc_min;                   // minimum N concentration (g N/g biomass)
         float p_conc_sen;                  // critical P concentration (g N/g biomass)
         float p_conc_max;                   // maximum P concentration (g N/g biomass)
         float p_conc_min;                   // minimum P concentration (g N/g biomass)

      } g;
         float DMPlantMin;                 // minimum weight of each plant part (g/plant)
         float Height;                     // The height of this part (mm)
         float Width;                      // The width of this part (mm)
         float PGreen;
         float PSen;
         float PDead;
         float NGreen;                      // plant nitrogen content (g N/m^2)
         float NDead;                       // plant N content of dead plants (g N/m^2)
         float NSenesced;                   // plant N content of senesced plant (g N/m^2)
         float DMGreen;                     // live plant dry weight (biomass) (g/m^2)
         float DMSenesced;                  // senesced plant dry wt (g/m^2)
         float DMDead;                      // dry wt of dead plants (g/m^2)


      // deltas
      struct {
         float dm_green;                     // biomass growth (g/m^2)
         float dm_senesced;                  // biomass senescence (g/m^2)
         float dm_dead;                      // biomass dead (g/m^2)

         float dm_detached;                  // biomass detached from senesced part (g/m^2)
         float dm_dead_detached;             // biomass detached from dead plant (g/m^2)

         float dm_green_retrans;             // biomass retranslocated to/from (+/-) green part to/from <<somewhere else??>> (g/m^2)

         float dm_green_dead;                // plant biomass from green part to dead population(g/m^2)
         float dm_senesced_dead;             // plant biomass from green part to dead population(g/m^2)

         float n_green;                      // actual N uptake into plant (g/m^2)
         float n_senesced;                   // actual N loss with senesced plant (g/m^2)
         float n_dead;                       // actual N loss with dead plant (g/m^2)

         float n_detached;                   // actual N loss with detached senesced part (g/m^2)
         float n_dead_detached;              // actual N loss with detached dead plant (g/m^2)

         float n_retrans;                    // nitrogen retranslocated to/from (+/-) green part to/from <<somewhere else??>> (g/m^2)

         float n_green_dead;                 // plant N from green part to dead population(g/m^2)
         float n_senesced_dead;              // plant N from senesced part to dead population(g/m^2)

         float n_senesced_retrans;           // plant N retranslocated to/from (+/-) senesced part to/from <<somewhere else??>> (g/m^2)
         float n_senesced_trans;

         float p_green;
         float p_sen;
         float p_dead;
         float p_det;

         float p_dead_det;
         float p_retrans;

         float p_green_dead;
         float p_senesced_dead;

         float height;                       // growth upwards (mm)
         float width;                        // growth outwards (mm)
      } dlt;

      // "Variables"
      float DMGreenDemand;              // biomass demand (g/m^2)
      float NDemand ;                   // critical plant nitrogen demand (g/m^2)
      float PDemand;

      float SoilNDemand;
      float NCapacity;                  // amount of nitrogen this part can take(g/m^2)
      float NMax ;                      // maximum plant nitrogen demand (g/m^2)


      // "Constants"
      struct {
         float dm_init;                      // Initial value
         float n_init_conc;                  // Initial N value
         float p_init_conc;                  // Initial P value
         float n_sen_conc;                   // N concentration of senesced material (gN/gdm)

         float trans_frac;                   // fraction of part used in translocation to grain
         float n_retrans_fraction;           // fraction of N in paret availale for retranslocation

         float dead_detach_frac;             // fraction of dead plant parts detaching each day (0-1)
         float sen_detach_frac;              // fraction of dead plant parts detaching each day (0-1)

         bool  p_stress_determinant;         // is a P stress_determinant
         bool  p_yield_part;                 // is a P yield_part
         bool  p_retrans_part;               // is a P retrans_part

         bool  stress_determinant;           // is a stress_determinant
         bool  yield_part;                   // is a yield_part
         bool  retrans_part;                // is a retrans_part

         float n_deficit_uptake_fraction;    // xxxxxxxxxx

         interpolationFunction n_conc_min;
         interpolationFunction n_conc_crit;
         interpolationFunction n_conc_max;

         interpolationFunction p_conc_min;
         interpolationFunction p_conc_sen;
         interpolationFunction p_conc_max;
         int   num_x_p_stage_code;
         float x_p_stage_code [max_table];
         float y_p_conc_min[max_table];
         float y_p_conc_sen[max_table];
         float y_p_conc_max[max_table];

         interpolationFunction height;
         interpolationFunction width;

         interpolationFunction dm_sen_frac;
         interpolationFunction fr_remain;
         string name;                        // What we call ourselves
      } c;

      PlantComponent *parentPlant;
      PlantPhenology *phenology;


   plantPart(plantInterface *p, const string &name)
     {
     zeroAllGlobals();
     plant = p;
     c.name = name;
     c.trans_frac = 0.0;
     c.n_retrans_fraction = 1.0;
     c.p_stress_determinant = false;
     c.p_yield_part = false;
     c.p_retrans_part = false;
     };
   virtual ~plantPart() {};

   virtual void doInit(PlantComponent *systemInterface, PlantPhenology *plantPhenology);
   virtual void zeroAllGlobals(void);
   virtual void zeroDeltas(void);
   virtual void zeroDltNSenescedTrans(void);
   virtual void checkBounds(void);

   virtual void doRegistrations(protocol::Component *);
   virtual void readConstants (protocol::Component *, const string &);
   virtual void readSpeciesParameters (protocol::Component *, vector<string> &);
   virtual void readCultivarParameters (protocol::Component *, const string &);

   virtual void onPlantEvent(const string &);
   virtual void onRemoveBiomass(float) {};

   virtual void prepare(void);
   //void process(void);
   virtual void update(void);
   virtual void updateDm(void);
   virtual void updateN(void);
   virtual void updateP(void);

   virtual void morphology(void);
   virtual void doNConccentrationLimits(void);

   virtual void doDmPartition(float DMAvail, float DMDemandTotal);
   virtual void doDmRetranslocate(float DMAvail, float DMDemandDifferentialTotal);
   virtual void doDmMin(void);
   virtual void doNDemand1(float, float);
   virtual void doNDemand1Pot(float, float);
   virtual void doNDemand2(float, float);
   virtual void doSoilNDemand(void);
   virtual void doSenescence1(float);
   virtual void doSenescence2(float);
   virtual void doNSenescence(void);
   virtual void doNSenescedRetrans(float navail, float n_demand_tot);
   virtual void doNRetranslocate( float N_supply, float g_grain_n_demand);
   virtual void doNFixRetranslocate(float NFix, float NDemandDifferentialTotal);
   virtual void doDmDetachment(void);
   virtual void doNDetachment(void);
   virtual void doProcessBioDemand(void);

   virtual void doPDetachment(void);
   virtual void doPDemand(void);
   virtual void doPSenescence(void);
   virtual void zeroDltDmGreen(void);
   virtual void zeroDltDmGreenRetrans(void);

   virtual void collectDetachedForResidue(vector<string> &part_name
                                          , vector<float> &dm_residue
                                          , vector<float> &dm_n
                                          , vector<float> &dm_p
                                          , vector<float> &fraction_to_residue);
   virtual void collectDeadDetachedForResidue(vector<string> &part_name
                                          , vector<float> &dm_dead_detached
                                          , vector<float> &n_dead_detached
                                          , vector<float> &p_dead_detached
                                          , vector<float> &fraction_to_residue);
   virtual float dmTotal(void);
   virtual float dmGreenDemand(void);
   virtual float dmDemandDifferential(void);
   virtual float dltDmRetranslocateSupply(float DemandDifferential);
   virtual float dmGreen(void);
   virtual float dltDmGreen(void);
   virtual float dltDmGreenRetrans(void);
   virtual float dmGreenNew(void);
   virtual float dltDmGreenNew(void);
   virtual float dltDmGreenUptake(void);
   virtual float dltDmGreenRetransUptake(void);
   virtual float dltDmDetached(void);
   virtual float dmSenesced(void);
   virtual float dmDead(void);
   virtual float dmRetransSupply(void);
   virtual float dmRetransDemand(void);

   virtual float dmGreenStressDeterminant(void);
   virtual float pGreenStressDeterminant(void);
   virtual float pMaxPotStressDeterminant(void);
   virtual float pMinPotStressDeterminant(void);
   virtual float nTotal(void);
   virtual float nGreen(void);
   virtual float nSenesced(void);
   virtual float dltNSenescedRetrans(void);
   virtual float nDead(void);
   virtual float nConc(void);
   virtual float dltNRetransOut(void);
   virtual float dltNGreen(void);

   virtual float nMaxPot(void) ;
   virtual float nMinPot(void) ;
   virtual float pTotal(void);
   virtual float pGreen(void);
   virtual float pSenesced(void);
   virtual float pDead(void);
   virtual float pConc(void);

   virtual float pMaxPot(void) ;
   virtual float pMinPot(void) ;
   virtual void updatePDet(void) ;

   virtual float soilNDemand(void);
   virtual float nDemand(void);
   virtual float nMax(void);
   virtual float nCapacity(void);
   virtual void  doNPartition(float nSupply, float n_demand_sum, float n_capacity_sum);
   virtual float nRetransSupply(void);
   virtual float nRetransDemand(void);
   virtual float nDemandDifferential(void);

   virtual float pDemand(void);
   virtual float pRetransSupply(void);
   virtual float pRetransDemand(void);

   virtual void doPPartition(float p_uptake, float total_p_demand);
   virtual void doPRetranslocate(float total_p_supply, float total_p_demand);
   virtual void doPInit(void);

   virtual void onHarvest(float height, float remove_fr,
                          vector<string> &dm_type,
                          vector<float> &dlt_crop_dm,
                          vector<float> &dlt_dm_n,
                          vector<float> &dlt_dm_p,
                          vector<float> &fraction_to_residue) = 0;

   virtual void onEndCrop(vector<string> &dm_type,
                  vector<float> &dlt_crop_dm,
                  vector<float> &dlt_dm_n,
                  vector<float> &dlt_dm_p,
                  vector<float> &fraction_to_residue);

   virtual void onKillStem(void);
   virtual void onDayOf(const string &);

   virtual float availableRetranslocateN(void);
   const string &name(void) {return c.name;};

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

   protected:
      plantInterface *plant;                 // The plant we are attached to

      virtual void onEmergence(void);
      virtual void onFlowering(void);
      virtual void onStartGrainFill(void);

   private:
      void get_n_conc(protocol::Component *, protocol::QueryValueData &);
      void get_n_conc_crit(protocol::Component *, protocol::QueryValueData &);
      void get_n_conc_min(protocol::Component *, protocol::QueryValueData &);
      void get_p_conc(protocol::Component *, protocol::QueryValueData &);

};


float critNFactor(vector<const plantPart *> &, float );

class plantPartHack : public plantPart {
  private:
   Plant *myplant;
   int   part;
   void get(void);
   void put(void);
 public:
   plantPartHack(Plant *p, int ipart, const string &name) : plantPart((plantInterface *)p, name) {
      part = ipart;
      myplant = p;
      get();
   };
   ~plantPartHack(){put();};
   void onHarvest(float height, float remove_fr,
                  vector<string> &dm_type,
                  vector<float> &dlt_crop_dm,
                  vector<float> &dlt_dm_n,
                  vector<float> &dlt_dm_p,
                  vector<float> &fraction_to_residue) {};
};

#endif /* PlantPartsH */

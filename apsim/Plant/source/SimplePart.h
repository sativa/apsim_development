#ifndef SimplePartH
#define SimplePartH


#include "PlantPart.h"

class SimplePart : public plantPart
   {
 protected:
//1) Need to make Senesced() method in SimplePart
//2) Make a Total() method = Green+Senesced
//3) Make Grain and GrainTotal methods which return Green or Total if it is a grain part.

   // state variables
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
   float relativeGrowthRate;
   float radiationInterceptedGreen;
   float radiationInterceptedTotal;
   float transpEff;
   float pEoCropFactor;                             // Crop factor for sw demand applied to Eo

   // deltas
   struct {
      float dm_pot_te;
      float dm_pot_rue;
      float dm;
      float dm_green_removed;                     // green biomass removed (g/m^2)
      float dm_senesced_removed;                  // senesced biomass removed (g/m^2)
      float n_senesced_retrans;           // plant N retranslocated to/from (+/-) senesced part to/from <<somewhere else??>> (g/m^2)
      float n_senesced_trans;
      float height;                       // growth upwards (mm)
      float width;                        // growth outwards (mm)
   } dlt;

   // "Variables"
   float DMGreenDemand;              // biomass demand (g/m^2)
   float NDemand ;                   // critical plant nitrogen demand (g/m^2)
   float PDemand;
   float sw_demand_te;
   float sw_demand;

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
      int   trans_frac_option;            // flag to say how trans_frac is to be used.
      float n_retrans_fraction;           // fraction of N in paret availale for retranslocation

      float sen_detach_frac;              // fraction of dead plant parts detaching each day (0-1)

      bool  p_stress_determinant;         // is a P stress_determinant
      bool  p_retrans_part;               // is a P retrans_part

      bool  yield_part;                   // is a yield_part
      bool  retrans_part;                // is a retrans_part

      float n_deficit_uptake_fraction;    // xxxxxxxxxx

      interpolationFunction n_conc_min;
      interpolationFunction n_conc_crit;
      interpolationFunction n_conc_max;

      int   num_x_p_stage_code;
      float x_p_stage_code [max_table];
      float y_p_conc_min[max_table];
      float y_p_conc_sen[max_table];
      float y_p_conc_max[max_table];

      interpolationFunction height;
      interpolationFunction width;

      interpolationFunction dm_sen_frac;
      interpolationFunction fr_remain;
      float transpEffCf[max_table];                  // transpiration efficiency coefficient
                                                        // to convert vpd to
                                                        // transpiration efficiency (kpa)
                                                        // although this is expressed as a
                                                        // pressure it is really in the form
                                                        // kpa*g carbo per m^2 / g water per m^2
                                                        // and this can be converted to
                                                        // kpa*g carbo per m^2 / mm water
                                                        // because 1g water = 1 cm^3 water
   } c;

   plantInterface *plant;                 // The plant we are attached to

public:

   SimplePart(ScienceAPI& scienceAPI, plantInterface *p, const string &name);
   virtual ~SimplePart() {};

public:
      // plant
   virtual float dltDmGreenRemoved(void);
   virtual float dltDmPotRue(void);
   virtual float dltDmSenesced(void);
   virtual float dltNDetached(void);
   virtual float dltNSenesced(void);
   virtual float dltNSenescedTrans(void);
   virtual void  doDmPotTE(float swSupply);                         //(OUTPUT) potential dry matter production by transpiration (g/m^2)
   virtual float giveNGreen(float) ;
   virtual float interceptRadiationGreen(float radiation);
   virtual float n_conc_crit(void);
   virtual float n_conc_min(void);
   virtual float SWDemand(void);
   virtual float transpirationEfficiency(void);
   virtual float height(void);
   virtual float width(void);

      //grainpart
   virtual float dltDmGreenNew(void) ;
   virtual float nDemand(void);

      //grainpartHI
   virtual float dmGreenDemand(void);

      //allometricArbitrator
   virtual void zeroDltDmGreen(void);
   virtual float dltLeafAreaPot(void) {throw std::runtime_error("SimplePart::dltLeafAreaPot() called");};
   virtual float giveDmGreen(float) ;           // Arbitrator gives this part dm; return amount used
   virtual float dltDmGreen(void) ;

protected:
   virtual void zeroDltNSenescedTrans(void);
   virtual void zeroAllGlobals(void);
   virtual void zeroDeltas(void);
   virtual void checkBounds(void) {};

   bool tempFlagToShortCircuitInit1;
   virtual void Detachment(void);
   virtual void doDmMin(void){};
   virtual void doDmRetranslocate(float DMAvail, float DMDemandDifferentialTotal);
   virtual void doNConccentrationLimits(float);
   virtual void doNDemand1(float, float);
   virtual void doNDemand1Pot(float, float);
   virtual void doNDemand2(float, float);
   virtual void doNFixRetranslocate(float NFix, float NDemandDifferentialTotal);
   virtual void doNRetranslocate( float N_supply, float g_grain_n_demand);
   virtual void doNSenescedRetrans(float navail, float n_demand_tot);
   virtual void doNSenescence(void);
   virtual void doProcessBioDemand(void){};
   virtual void doSenescence(float);
   virtual void doSoilNDemand(void);
   virtual void morphology(void);
   virtual void onInit1(protocol::Component *);
   virtual void onPlantEvent(const string &);
   virtual void onRemoveBiomass(float) {};
   virtual void prepare(void);
   virtual void readConstants (protocol::Component *, const string &);
   virtual void readCultivarParameters (protocol::Component *, const string &);
   virtual void readSpeciesParameters (protocol::Component *, vector<string> &);
   virtual void update(void);
   virtual void write() {};

   virtual void doPDemand(void);
   virtual void doPSenescence(void);


   virtual void collectDetachedForResidue(vector<string> &part_name
                                          , vector<float> &dm_residue
                                          , vector<float> &dm_n
                                          , vector<float> &dm_p
                                          , vector<float> &fract);

   virtual float dlt_dm_green_retrans_hack(float);
   virtual float dltDmRetranslocateSupply(float DemandDifferential) ;
   virtual float dltNRetransOut(void);
   virtual float dltDmGreenRetransUptake(void);
   virtual float dltDmGreenRetrans(void);
   virtual float dltDmRetranslocate(void);
   virtual float dltDmDetached(void);
   virtual float dltNGreen(void) ;
   virtual float dltPGreen(void) ;
   virtual float dltPSenesced(void);
   virtual float dltPDetached(void);
   virtual float dltNRetrans(void);
   virtual float dltNSenescedRetrans(void);
   virtual float dltDmSenescedRemoved(void);
   virtual float dltDmRemoved(void);
   virtual float dltNRemoved(void);

   virtual float dmDemandDifferential(void) ;

   virtual float giveDmSenesced(float) ;

   virtual float giveDmGreenRemoved(float) ;           //
   virtual float giveDmSenescedRemoved(float) ;

   virtual float dmGreenNew(void) ;
   virtual float dmRetransSupply(void);
   virtual float dmRetransDemand(void){return 0.0;} ;
   virtual float dmGreenStressDeterminant(void);

   virtual float nMax(void);
   virtual float nCapacity(void);

   virtual float pGreenStressDeterminant(void);
   virtual float pMaxPotStressDeterminant(void);
   virtual float pMinPotStressDeterminant(void);
   virtual float pMaxPot(void);
   virtual float pMinPot(void);

   virtual float soilNDemand(void);
   virtual void  doNPartition(float nSupply, float n_demand_sum, float n_capacity_sum);
   virtual float nRetransSupply(void);
   virtual float nRetransDemand(void){return 0.0;};
   virtual float nDemandDifferential(void);
   virtual float nMin(void)  {return g.n_conc_min * Green.DM();};
   virtual float nCrit(void)  {return g.n_conc_crit * Green.DM();};
   virtual float pDemand(void);
   virtual float pRetransSupply(void);
   virtual float pRetransDemand(void);

   virtual void  doRemoveBiomass(protocol::RemoveCropDmType dmRemoved, string &c_remove_biomass_report);
   virtual void  removeBiomass(void);
   virtual void  removeBiomass2(float);

   virtual void doPPartition(float p_uptake, float total_p_demand);
   virtual void doPRetranslocate(float total_p_supply, float total_p_demand);


   virtual void onHarvest(float height, float remove_fr,
                          vector<string> &dm_type,
                          vector<float> &dlt_crop_dm,
                          vector<float> &dlt_dm_n,
                          vector<float> &dlt_dm_p,
                          vector<float> &fraction_to_residue) = 0;

   virtual void onHarvest_GenericAboveGroundPart(float remove_fr,
                          vector<string> &dm_type,
                          vector<float> &dlt_crop_dm,
                          vector<float> &dlt_dm_n,
                          vector<float> &dlt_dm_p,
                          vector<float> &fraction_to_residue);

   virtual void onEndCrop(vector<string> &dm_type,
                  vector<float> &dlt_crop_dm,
                  vector<float> &dlt_dm_n,
                  vector<float> &dlt_dm_p,
                  vector<float> &fraction_to_residue);

   virtual void onKillStem(void);
   virtual void onDayOf(const string &);

   const string &name(void) {return myName;};

   virtual void get_name(vector<string> &names);
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

   //needed to standardise interface for composite subclass

   virtual float availableRetranslocateN(void);
   virtual void  doCover (PlantSpatial &spatial);
   virtual float coverGreen(void) ;
   virtual float coverSen(void) ;
   virtual float coverTotal(void) ;
   virtual float dltDmGrainDemand(void);
   virtual float dltDmPotTe(void);
   virtual float dltDm(void);
   virtual float grainWaterContent(void);
   virtual float dmGrainWetTotal(void);
   virtual float grainNo(void);
   virtual float grainWt(void);
   virtual float interceptRadiationTotal(float radiation);
   virtual float nDemandGrain(void);
   virtual float nDemandGrain2(void);

   virtual void doSWDemand(float SWDemandMaxFactor);
   virtual float SWDemandTE(void);
   virtual void calcDlt_pod_area (void);   //FIXME
   virtual void doBioActual (void);
   virtual void doDmDemand (float dlt_dm_supply_by_veg);
   virtual void doDmPotRUE (void );                      // (OUTPUT) potential dry matter (carbohydrate) production (g/m^2)
   virtual void doGrainNumber (void);
   virtual void doNDemandGrain(float nfact_grain_conc, float swdef_expansion);
   virtual void doNInit (void);
   virtual void doTECO2(void);                                       // (OUTPUT) transpiration coefficient                         //FIXME
   virtual void writeCultivarInfo (protocol::Component *);

   virtual bool isYieldPart(void)  {return false;};
   virtual bool isRetransPart(void)  {return c.retrans_part;};

   virtual void onEmergence(void);

   virtual void onSowing(void){};
   virtual void onGermination(void){};

   virtual void onTransplanting(void) {};
   virtual void onFlowering(void);
   virtual void onStartGrainFill(void);

   // protected constructor called by CompositePart only.
   SimplePart(ScienceAPI& api, plantInterface *p, const string &name,
             Pool& green, Pool& senesced);

private:
   virtual float dltNGreenRemoved(void);
   virtual float dltNSenescedRemoved(void);
   virtual float dltPGreenRemoved(void);
   virtual float dltPSenescedRemoved(void);
   virtual float dltPRemoved(void);

   float nConcCrit();
   float nConcMin();
   void get_dm_green_demand(protocol::Component *system, protocol::QueryValueData &qd);
   std::string addPartToVar(const std::string& variableName);
   std::string addPartToDesc(const std::string& description);

   void Initialise();

};

#endif /* SimplePartsH */
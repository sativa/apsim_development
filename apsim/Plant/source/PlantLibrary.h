#ifndef PlantLibaryH
#define PlantLibaryH

#include <list>

using namespace std;

class commsInterface;
class PlantComponent;

#define min(A,B) ((A)<(B)?(A):(B))
#define max(A,B) ((A)>(B)?(A):(B))

extern const char *Blank;

// Size of a message"s address char. string
const int Mes_address_size = 8;

// Size of action part of message
const int Mes_Action_size = 20;

// Size of data part of message
const int Mes_Data_size = 50;

// Message delimiter
extern const char *Mes_delimiter;

// Len. of string returned by char. fun.
const int Function_string_len = 600;

// minimum year for APSIM simulations.
const int min_year = 1800;

// maximum year for APSIM simulations.
const int max_year = 2200;

extern const char *All_active_modules;
const int Unknown_module = 0;
extern const char *First_active_module;

/*   ================================================================
 *    Conversion constants
 *   ================================================================
 *
 *   Short description:
 *     Globally used conversion constants
 */

 // ----------------------- Declaration section ------------------------

 //  Constant values

      // WEIGHT conversion
      const float gm2kg = 1.0/1000.0;           // constant to convert g to kg
      const float kg2gm = 1000.0;               // conversion of kilograms to grams
      const float mg2gm = 1.0/1000.0;           // conversion of mg to grams
      const float t2g = 1000.0*1000.0;          // tonnes to grams
      const float g2t = 1.0/ t2g;               // grams to tonnes
      const float t2kg = 1000.0;                // tonnes to kilograms
      const float kg2t = 1.0/ t2kg;             // kilograms to tonnes


      // AREA conversion
      const float ha2scm = 10000.0*10000.0;     // ha to sq cm
      const float ha2sm = 10000.0;              // conversion of hectares to sq metres
      const float sm2ha = 1.0/10000.0;          // constant to convert m^2 to hectares
      const float sm2smm = 1000000.0;           // conversion of square metres to square mm
      const float smm2sm = 1.0/1000000.0;       // conversion factor of mm^2 to m^2
      const float scm2smm = 100.0;              // conversion factor of cm^2 to mm^2


      // PRESSURE and related conversion
      const float g2mm = 1.0e3/1.0e6;           // convert g water/m^2 to mm water
                                                // 1 g water = 1,000 cubic mm and
                                                // 1 sq m = 1,000,000 sq mm
      const float mb2kpa = 100.0/1000.0;        // convert pressure mbar to kpa
                                                // 1000 mbar = 100 kpa

      // LENGTH conversion
      const float cm2mm = 10.0;                 // cm to mm
      const float mm2cm = 1.0/10.0;             // conversion of mm to cm
      const float mm2m = 1.0/1000.0;            // conversion of mm to m
      const float km2m  = 1000.0;               // conversion of km to metres


      // VOLUME conversion
      const float cmm2cc = 1.0/1000.0;          // conversion of cubic mm to cubic cm
      const float conv_gmsm2kgha = 100.0;       // convert g/sm -> kg/ha
      const float conv_pc2fr = 0.01;            // convert %age to fraction
      const float pcnt2fract = 1.0/100.0;       // convert percent to fraction
      const float fract2pcnt = 100.0;           // convert fraction to percent
      const float mm2lpsm = 1.0;                // mm depth to litres per sq m
      const float lpsm2mm = 1.0;                // litres per sq m to mm depth
      const float day2hr  = 24.0;               // days to hours
      const float hr2s    = 60.0*60.0;          // hours to seconds
      const float s2hr    = 1.0/hr2s;           // seconds to hours

// An "external" function.
class externalFunction {
 protected:
     std::string xName, yName, xUnits, yUnits;
 public:
   externalFunction() {};
   ~externalFunction() {};

   void read(PlantComponent *P, const string &section,
                     const char *xname, const char * xunits, float x0, float x1,
                     const char *yname, const char * yunits, float y0, float y1)
      {
      vector<string> t;
      t.push_back(section);
      search(P, t, xname, xunits, x0, x1, yname, yunits, y0, y1);
      };
   virtual void search(PlantComponent *P, vector<string> &sections,
                       const char *xname, const char * xunits, float x0, float x1,
                       const char *yname, const char * yunits, float y0, float y1)
      {
      xName = string(xname); yName = string(yname);
      xUnits = string(xunits); yUnits = string(yunits);
      };
   virtual std::string description(void);

   virtual float value(float v) = 0;
   float operator [] (float arg) {return value(arg);};
};


// Implement stick (linear interpolation) functions
class interpolationFunction : public externalFunction
{
 private:
   vector<float> x;
   vector<float> y;
 public:
   void search(PlantComponent *P, vector<string> &sections,
             const char *xName, const char * xunits, float x0, float x1,
             const char *yName, const char * yunits, float y0, float y1);
   float value(float v);
   vector<float> xVal() const {
   	return(x);
   };
   vector<float> yVal() const {
   	return(y);
   };
   std::string description(void);
};

// Implement table lookup functions
class lookupFunction : public externalFunction
{
 private:
   vector<float> x;
   vector<float> y;
 public:
   void search(PlantComponent *P, vector<string> &sections,
             const char *xName, const char * xunits, float x0, float x1,
             const char *yName, const char * yunits, float y0, float y1);
   float value(float v);
   vector<float> xVal() const {
   	return(x);
   };
   vector<float> yVal() const {
   	return(y);
   };
   std::string description(void);
};


// A class that observes things. Mostly report-related.
class observer {
	protected:
        int n;
	  float *myLocation, mySum;
   public:
     observer() {};
     virtual ~observer() {};
     void setup(float *loc) {
     	  myLocation = loc;
     	  mySum = 0.0; n= 0;
     };
     virtual void update(void) = 0;
     void reset(void) {mySum = 0.0; n = 0;};
     float getSum(void) {return mySum;};
     float getN(void) {return n;};
     float getAverage(void) {return (n > 0 ? mySum/(float)n : 1.0);};
};

// Keep track of state variable
class stateObserver : public observer {
	public:
     void update(void) {mySum += *myLocation; n++;};
};

// Factors are "1 - state" style variables
class factorObserver : public observer {
	public:
     void update(void) {mySum += (1.0 - *myLocation); n++;};
};

// A subject (observer manager)
class stageSubject {
	private:
	  std::list<observer *> observers;
	public:
	  stageSubject() {};
	  void addObserver(observer *s) {observers.push_back(s);};
     void update() {
     	  for (std::list<observer*>::iterator o = observers.begin();
     	       o !=  observers.end();
     	       o++)
     	      (*o)->update();
     };
     void reset(){
     	  for (std::list<observer*>::iterator o = observers.begin();
     	       o !=  observers.end();
     	       o++)
     	      (*o)->reset();
     };
};


float legnew_vernal_days(float  g_maxt
                               ,float  g_mint
                               ,float  *c_x_vernal_temp
                               ,float  *c_y_vernal_days
                               ,int    c_num_vernal_temp) ;



void crop_dm_pot_rue (float current_stage,
                float *rue, float radn_int, float temp_stress_photo,
                float nfact_photo, float *dlt_dm_pot);

void plant_dm_senescence (int num_part               // (INPUT) number of plant parts
                               , float max_Table            // (INPUT) max lookup length
                               , float independant_variable // (INPUT) independant variable which is said to drive senescence.
                               , float **c_x_dm_sen_frac    // (INPUT)  lookup for independant variabl
                               , float **c_y_dm_sen_frac    // (INPUT)  fraction of  material senescin
                               , int *c_num_dm_sen_frac     // (INPUT)  fraction of  material sene
                               , float *g_dm_green          // (INPUT)  live plant dry weight (biomass
                               , float *dlt_dm_senesced);    // (OUTPUT) actual biomass senesced
                                                            //          from plant parts (g/m^2)

void crop_dm_senescence0(const int num_part,              //(INPUT)  number of plant parts
                         const int root,                  //(INPUT)  number of plant root part
                         const int leaf,                  //(INPUT)  number for plant leaf part
                         const int stem,                  //(INPUT)  number for plant stem part
                         float dm_leaf_sen_frac,         //(INPUT)  fraction of senescing leaf dry
                         float dm_root_sen_frac,         //(INPUT)  fraction of root dry matter se
                         float *dlt_dm_green,             //(INPUT)  plant biomass growth (g/m^2)
                         float *dlt_dm_green_retrans,     //(INPUT)  plant biomass retranslocated
                         float dlt_lai,                  //(INPUT)  actual change in live plant la
                         float dlt_slai,                 //(INPUT)  area of leaf that senesces fro
                         float *dm_green,                 //(INPUT)  live plant dry weight (biomass
                         float lai,                      //(INPUT)  live plant green lai
                         float *dlt_dm_senesced,          //(OUTPUT) actual biomass senesced from plant parts (g/m^2)
                         float *dlt_dm_sen_retrans);       //(OUTPUT) reduction in senesced biomass as a result of retranslocation

void crop_dm_dead_detachment(const int num_part,
                             float *dead_detach_frac,         //(INPUT)  fraction of dead plant parts detached
                             float *dm_dead,                  //(INPUT)  dry wt of dead plants (g/m^2)
                             float *dlt_dm_dead_detached);     //(OUTPUT) change in dm of dead plant

void cproc_dm_senescence1 (const int num_part,           //(INPUT)  number of plant parts
                           const int max_table,          //(INPUT)  max lookup length
                           float independant_variable,   //(INPUT)  independant variable which
                           float **c_x_dm_sen_frac,        //(INPUT)  lookup for independant variabl   is said to drive senescence.
                           float **c_y_dm_sen_frac,        // (INPUT)  fraction of  material senescin
                           int   *c_num_dm_sen_frac,         // (INPUT)  fraction of  material sene
                           float *g_dm_green,              // (INPUT)  live plant dry weight (biomass
                           float *g_dlt_dm_green,          // (INPUT)  plant biomass growth (g/m^2)
                           float *g_dlt_dm_green_retrans,  // (INPUT)  plant biomass retranslocat
                           float *dlt_dm_senesced);         // (OUTPUT) actual biomass senesced from plant parts (g/m^2)

void cproc_dm_retranslocate1 (commsInterface *iface,
                              float g_current_stage,         //(INPUT)  current phenological stage
                              int start_grnfil,              //(INPUT)
                              int end_grnfil,                //(INPUT)
                              int grain_part_no,             //(INPUT)
                              int max_part,                  //(INPUT)
                              int *supply_pools,             //(INPUT)
                              int num_supply_pools,          //(INPUT)
                              float g_dlt_dm_grain_demand,   //(INPUT)  grain dm demand (g/m^2)
                              float *g_dlt_dm_green,         //(INPUT)  plant biomass growth (g/m^2)
                              float *g_dm_green,             //(INPUT)  live plant dry weight (biomass
                              float *g_dm_plant_min,         //(INPUT)  minimum weight of each plant p
                              float g_plants,                //(INPUT)  Plant density (plants/m^2)
                              float *dm_retranslocate);       //(OUTPUT) actual change in plant part weights due to translocation (g/m^2)

void cproc_dm_detachment1( const int max_part,
                           float *c_sen_detach_frac,
                           float *g_dm_senesced,
                           float *g_dlt_dm_detached,
                           float *c_dead_detach_frac,
                           float *g_dm_dead,
                           float *g_dlt_dm_dead_detached);

void cproc_bio_yieldpart_demand1(float G_current_stage,             // (INPUT)  current phenological stage
                                 int Start_stress_stage,            // (INPUT)
                                 int Start_grainfill_stage,         // (INPUT)
                                 int End_grainfill_stage,           // (INPUT)
                                 int Yield_part,                    // (INPUT)
                                 int Root_part,                     // (INPUT)
                                 int Max_part,                      // (INPUT)
                                 float G_dlt_dm,                    // (INPUT)  the daily biomass production (
                                 float *G_dm_green,                 // (INPUT)  live plant dry weight (biomass
                                 float *G_dm_senesced,              // (INPUT)  senesced plant dry wt (g/m^2)
                                 float *G_days_tot,                 // (INPUT)  duration of each phase (days)
                                 float *G_dm_stress_max,            // (INPUT)  sum of maximum daily stress on
                                 float P_hi_incr,                  // (INPUT)  harvest index increment per da
                                 float *P_x_hi_max_pot_stress,      // (INPUT) Potential Max HI Stress dete
                                 float *P_y_hi_max_pot,             // (INPUT) Potential Max HI
                                 int P_num_hi_max_pot,             // (INPUT) Number of lookup pairs
                                 float *Dlt_dm_yieldpart_demand);    //(OUTPUT) grain dry matter potential (g/m^2)

void cproc_yieldpart_demand_stress1(float G_nfact_photo,
                                    float G_swdef_photo,
                                    float G_temp_stress_photo,
                                    float *Dlt_dm_stress_max);

void cproc_bio_init1(float *C_dm_init,         //(INPUT)
                     int   Init_stage,         //(INPUT)
                     float G_current_stage,    //(INPUT)  current phenological stage
                     float G_plants,           //(INPUT)  Plant density (plants/m^2)
                     float Max_part,           //(INPUT)
                     float *G_dm_green);        //(INPUT/OUTPUT) plant part weights  (g/m^2)

void cproc_rue_n_gradients(int   day,                  // !day of the year
                           float latitude,           //latitude in degree
                           float radiation,          //daily global radiation (MJ/m2/d)
                           float tempmax,            //daily maximum tempeature (C)
                           float tempmin,            //daily minimum tempeature (C)
                           float lai_green,          //leaf area index (-)
                           float sln_gradient,      //SLN gradients in canopy (g N/m2 leaf)
                           float pmaxmax,            //potential assimilation rate (SLN ASYMPTOTE) (mg CO2/m2.s)
                           float shadow_projection,  //shadow projection (=0.5)
                           float biomass_conversion, //biomass coversion for biochemical coversion and maintenance respiration (mg DM / mgCO2)
                           float scatter_coeff,      //scattering coefficients (=0.15)
                           float *rue_sln);            //rue based on SLN gradients in the canopy (g DM / MJ)

void crop_dm_pot_rue_co2 (float current_stage,
                          float *rue,
                          float radn_int,
                          float temp_stress_photo,
                          float nfact_photo,
                          float co2_modifier,
                          float *dlt_dm_pot);     //(OUTPUT) potential dry matter  (carbohydrate) production (g/m^2

  void legnew_bio_yieldpart_demand2(
    float g_grain_no
    ,float p_potential_grain_filling_rate
    ,float g_maxt
    ,float g_mint
    ,float *c_x_temp_grainfill
    ,float *c_y_rel_grainfill
    ,int   c_num_temp_grainfill
    ,float *g_dlt_dm_grain_demand
    ) ;

//---------------------------------------------------------------------------
  float legopt_leaf_area_sen_age1
    (float  *g_leaf_no
    ,float  *g_leaf_no_dead
    ,float  g_dlt_leaf_no_dead
    ,int    max_node
    ,float  g_lai
    ,float  g_slai
    ,float  c_min_tpla
    ,float  *g_leaf_area
    ,float  g_plants);

  void legopt_leaf_no_init1
    (
     float  c_leaf_no_at_emerg
    ,float  *leaf_no
    ,float  *node_no
    ) ;
  void legopt_leaf_area_init1
    (
     float  c_initial_tpla
    ,float  c_leaf_no_at_emerg
    ,float  g_plants
    ,float  *lai
    ,float  *leaf_area
    ) ;
  void legopt_leaf_area_sen1
    (float  g_dlt_lai_stressed
    ,float  g_dlt_leaf_no
    ,float  g_dlt_leaf_no_dead
    ,float  g_lai
    ,float  *g_leaf_area
    ,float  *g_leaf_no
    ,float  *g_leaf_no_dead
    ,int    max_node
    ,float  g_plants
    ,float  g_slai
    ,float  c_min_tpla
    ,float  *g_dlt_slai_age
    ,float  c_lai_sen_light
    ,float  c_sen_light_slope
    ,float  *g_dlt_slai_light
    ,float  c_sen_rate_water
    ,float  g_swdef_photo
    ,float  *g_dlt_slai_water
    ,float  *c_x_temp_senescence
    ,float  *c_y_senescence_fac
    ,int    c_num_temp_senescence
    ,float  g_mint
    ,float  *g_dlt_slai_frost
    ,float  *g_dlt_slai
    ) ;
void cproc_leaf_no_pot3(
     float  *c_x_node_no_app                  //(INPUT)
    ,float  *c_y_node_app_rate                //(INPUT)
    ,int    c_num_node_no_app                 // (INPUT)
    ,float  *c_x_node_no_leaf                 // (INPUT)
    ,float  *c_y_leaves_per_node              // (INPUT)
    ,int    c_num_node_no_leaf                // (INPUT)
    ,bool   inNodeFormationPhase
    ,bool   inEmergenceDay
    ,float  node_no_now                       // (INPUT) current number of nodes
    ,float  g_dlt_tt                          // (INPUT)  daily thermal time (growing de
    ,float  g_nfact_expansion
    ,float  g_swdef_expansion
    ,float  *g_leaves_per_node                 // OUTPUT
    ,float  *dlt_leaf_no_pot                   // (OUTPUT) new fraction of oldest expanding leaf
    ,float  *dlt_node_no_pot                   // (OUTPUT) new fraction of oldest expanding node on main stem
    ) ;
  void plant_canopy_width
    (
     float  g_canopy_width
    ,float  *p_x_stem_wt
    ,float  *p_y_width
    ,int    p_num_stem_wt
    ,float  *g_dm_green
    ,float  g_plants
    ,int    stem
    ,float  *dlt_canopy_width
    ) ;

void legnew_canopy_fac (
     float  g_row_spacing
    ,float  g_plants
    ,float  g_skip_row_fac
    ,float  g_skip_plant_fac
    ,float  g_canopy_width
    ,float  *g_canopy_fac) ;

void crop_lai_equilib_water (int    day_of_year,         // (INPUT) day of year
                             int    year,                // (INPUT) year
                             float  extinction_coef,     // (INPUT) radiation extinction coefficient
                             float *rue,                 // (INPUT) radiation use efficiency (g dm/mj)
                             float  cover_green,          //(INPUT) fraction of radiation reaching the
                             float  current_stage,       // (INPUT) current phenological stage
                             const int    num_layer,     // (INPUT) number of layers in profile
                             float *dlayer,              // (INPUT) thickness of soil layer I (mm)
                             float  lai,                 // (INPUT) live plant green lai
                             float  nfact_photo,         // (INPUT)
                             float  radn,                // (INPUT) solar radiation (Mj/m^2/day)
                             float  radn_int,            // (INPUT) radn intercepted by leaves (mj/m^2)
                             float  root_depth,          // (INPUT) depth of roots (mm)
                             float *sw_supply,           // (INPUT) potential water to take up
                             float  temp_stress_photo,   // (INPUT)
                             float  transp_eff,          // (INPUT) transpiration efficiency (g dm/m^2)
                             float *lai_equilib_water);   // (INPUT/OUTPUT) lai threshold for water sene

void crop_lai_equilib_light ( float radn_int,
                              float cover_green,
                              float sen_radn_crit,
                              float extinction_coef,
                              float lai,
                              int   day_of_year,
                              int   year,
                              float *lai_eqlb_light);     //(INPUT/OUTPUT) lai threshold for light senescence

void crop_leaf_area_sen_frost1(float *frost_temp,           //(INPUT)
                               float *frost_fraction,       //(INPUT)
                               int   num_frost_temp,        //(INPUT)
                               float lai,                   //(INPUT)  live plant green lai
                               float mint,                  //(INPUT)  minimum air temperature (o_c)
                               float plants,                //(INPUT)
                               float min_tpla,              //(INPUT)
                               float *dlt_slai_frost);       //(OUTPUT) lai frosted today

void crop_leaf_area_sen_water2(int   day_of_year,                //(INPUT)  day of year
                               int   year,                       //(INPUT)  year
                               float sen_threshold,              //(INPUT)  supply:demand ratio for onset
                               float sen_water_time_const,       //(INPUT)  delay factor for water senesce
                               int   num_layer,                  //(INPUT)  number of layers in profile
                               float *dlayer,                     //(INPUT)  thickness of soil layer I (mm)
                               float lai,                        //(INPUT)  live plant green lai
                               float *lai_equilib_water,          //(INPUT)  lai threshold for water senesc
                               float root_depth,                 //(INPUT)  depth of roots (mm)
                               float sw_demand,                  //(INPUT)  total crop demand for water
                               float *sw_supply,                  //(INPUT)  potential water to take up
                               float *dlt_slai_water);            // (OUTPUT) water stress senescense

void crop_leaf_area_sen_light2 (float radn_int,                //(INPUT)
                                float radn,                    //(INPUT)
                                float sen_radn_crit,           //(INPUT)
                                int   year,                    //(INPUT)
                                int   day_of_year,             //(INPUT)
                                float *lai_equilib_light,      //(INPUT)
                                float lai,                     //(INPUT)
                                float sen_light_time_const,    //(INPUT)
                                float *dlt_slai_light);         //(OUTPUT) lai senesced by low light

void crop_leaf_area_sen_age1 (int emergence,                   //(INPUT)  emergence stage no.
                              int this_stage,                  //(INPUT)  This current stage
                              float g_dlt_lai_stressed,        //(INPUT)  potential change in live
                              float g_dlt_leaf_no,             //(INPUT)  actual fraction of oldest leaf
                              float g_dlt_leaf_no_dead,        //(INPUT)  fraction of oldest green leaf
                              float g_lai,                     //(INPUT)  live plant green lai
                              float *g_leaf_area,              //(INPUT)  leaf area of each leaf (mm^2)
                              float *g_leaf_no_dead,           //(INPUT)  no of dead leaves ()
                              float g_plants,                  //(INPUT)  Plant density (plants/m^2)
                              float g_slai,                    //(INPUT)  area of leaf that senesces fro
                              float c_min_tpla,                //(INPUT)
                              float *dlt_slai_age);             //(OUTPUT) new senesced lai from phasic devel.

void crop_leaf_area_sen_light1 (float lai_sen_light,
                                float sen_light_slope,
                                float lai,
                                float plants,
                                float min_tpla,
                                float *dlt_slai_light);   //(OUTPUT) lai senesced by low light

void crop_leaf_area_sen_water1 (float sen_rate_water,    //(INPUT)  slope in linear eqn relating soil wat
                                float lai,               //(INPUT)  live plant green lai
                                float swdef_photo,       //(INPUT)
                                float plants,            //(INPUT)
                                float min_tpla,          //(INPUT)
                                float *dlt_slai_water);    //(OUTPUT) water stress senescense

void cproc_leaf_area_sen1 (int emergence,                 // (INPUT)  emergence stage no.
                           int this_stage,                // (INPUT)  This current stage
                           float g_dlt_lai_stressed,      // (INPUT)  potential change in live
                           float g_dlt_leaf_no,           // (INPUT)  actual fraction of oldest leaf
                           float g_dlt_leaf_no_dead,      // (INPUT)  fraction of oldest green leaf
                           float g_lai,                   // (INPUT)  live plant green lai
                           float *g_leaf_area,            // (INPUT)  leaf area of each leaf (mm^2)
                           float *g_leaf_no_dead,         // (INPUT)  no of dead leaves ()
                           float g_plants,                // (INPUT)  Plant density (plants/m^2)
                           float g_slai,                  // (INPUT)  area of leaf that senesces fro
                           float c_min_tpla,              // (INPUT)
                           float *g_dlt_slai_age,         // (OUTPUT) new senesced lai from phasic devel.
                           float c_lai_sen_light,         // (INPUT)
                           float c_sen_light_slope,       // (INPUT)
                           float *g_dlt_slai_light,       // (OUTPUT)
                           float c_sen_rate_water,        // (INPUT)
                           float g_swdef_photo,           // (INPUT)
                           float *g_dlt_slai_water,       // (OUTPUT)
                           float *cXTempSenescence,       // (INPUT)
                           float *cYSenescenceFac,        // (INPUT)
                           int c_num_temp_senescence,     // (INPUT)
                           float g_mint,                  // (INPUT)
                           float *g_dlt_slai_frost,       // (OUTPUT)
                           float *g_dlt_slai);            // (OUTPUT)

void cproc_leaf_area_init1 (float c_initial_tpla,     //(INPUT)  initial plant leaf area (mm^2)
                            int   init_stage,         //(INPUT)  initialisation stage
                            float g_current_stage,    //(INPUT)  current phenological stage
                            float *g_days_tot,        // (INPUT)  duration of each phase (days)
                            float  g_plants,          // (INPUT)  Plant density (plants/m^2)
                            float *lai);               //(OUTPUT) total plant leaf area

void cproc_lai_detachment1 (int leaf,                           //(INPUT)
                            float *c_sen_detach_frac,           //(INPUT)
                            float g_slai,                       //(INPUT)
                            float *g_dlt_slai_detached,         //(OUTPUT)
                            float *c_dead_detach_frac,          //(INPUT)
                            float g_tlai_dead,                  //(INPUT)
                            float *g_dlt_tlai_dead_detached);    //(OUTPUT)

void cproc_canopy_height (float g_canopy_height,          // (INPUT)  canopy height (mm)
                          float *pXStemWt,                //(INPUT)
                          float *pYHeight,                // (INPUT)
                          int   p_num_stem_wt,            // (INPUT)
                          float *g_dm_green,              //(INPUT)  live plant dry weight (biomass
                          float g_plants,                 // (INPUT)  Plant density (plants/m^2)
                          int   stem,                     // (INPUT)  plant part no for stem
                          float *dlt_canopy_height);       //(OUTPUT) canopy height change (mm)

void cproc_leaf_no_init1 (float c_leaf_no_at_emerg,       //(INPUT)  leaf number at emergence ()
                          float g_current_stage,          //(INPUT)  current phenological stage
                          int   emerg,                    //(INPUT)  emergence stage no
                          float *g_days_tot,               //(INPUT)  duration of each phase (days)
                          float *leaf_no,                  //(OUTPUT) initial leaf number
                          float *node_no);                  //(OUTPUT) initial node number

void cproc_leaf_no_pot1 (float *c_x_node_no_app,            // (INPUT)
                         float *c_y_node_app_rate,          // (INPUT)
                         int    c_num_node_no_app,            //  (INPUT)
                         float *c_x_node_no_leaf,           //  (INPUT)
                         float *c_y_leaves_per_node,        //  (INPUT)
                         int    c_num_node_no_leaf,           // (INPUT)
                         bool   inNodeFormationPhase,
                         bool   inEmergenceDay,
                         float  node_no_now,                // (INPUT) current number of nodes
                         float  g_dlt_tt,                   // (input)
                         float *dlt_leaf_no_pot,            // (OUTPUT) new fraction of oldest expanding leaf
                         float *dlt_node_no_pot);            // (OUTPUT) new fraction of oldest expanding node on main stem

void cproc_leaf_area_pot1 (float *c_x_node_no,                  //(INPUT)  node number for lookup
                           float *c_y_leaf_size,                //(INPUT)  leaf size for lookup
                           int    c_num_node_no,                //(INPUT)  lookup table size
                           float g_node_no,                    //(INPUT)  node number
                           float c_node_no_correction,         //(INPUT)  corrects for other growing lea
                           float  g_dlt_leaf_no_pot,            //(INPUT)  potential fraction of oldest l
                           float  g_plants,                     //(INPUT)  Plant density (plants/m^2)
                           float *dlt_lai_pot);                  //(OUTPUT) change in leaf area

void cproc_leaf_area_stressed1 (float  g_dlt_lai_pot,         //(INPUT)
                                float  g_swdef_expansion,     //(INPUT)
                                float  g_nfact_expansion,     //(INPUT)
                                float *g_dlt_lai_stressed);    //(OUTPUT)

void cproc_leaf_area_actual1 (float *c_x_lai,
                              float *c_y_sla_max,
                              int    c_num_lai,
                              float  dlt_dm_leaf,        //(INPUT)  leaf biomass growth (g/m^2)
                              float *g_dlt_lai,          //(OUTPUT)  actual change in live plant la
                              float  g_dlt_lai_stressed, //(INPUT)  potential change in live
                              float  g_lai);              //(INPUT)  live plant green lai

void cproc_leaf_no_actual1 (int   c_num_lai_ratio,           //(INPUT)  number of ratios in table ()
                            float *c_x_lai_ratio,            //(INPUT)  ratio table for critical leaf
                            float *c_y_leaf_no_frac,         //(INPUT)  reduction in leaf appearance (
                            float g_dlt_lai,                 //(INPUT)  actual change in live plant la
                            float g_dlt_lai_stressed,        //(INPUT)  potential change in live
                            float *g_dlt_leaf_no,            //(OUTPUT) actual fraction of oldest leaf
                            float g_dlt_leaf_no_pot,         //(INPUT)  potential fraction of oldest l
                            float *g_dlt_node_no,            //(OUTPUT) actual fraction of oldest node
                            float g_dlt_node_no_pot);         //(INPUT)  pot fraction of oldest node

void cproc_leaf_no_pot2 (float *c_x_node_no_app,       //(INPUT)
                         float *c_y_node_app_rate,     //(INPUT)
                         float *c_y_leaves_per_node,   //(INPUT)
                         int c_num_node_no_app,       //(INPUT)
                         float g_current_stage,       //(INPUT)  current phenological stage
                         int start_node_app,          //(INPUT)  stage of start of leaf appeara
                         int end_node_app,            //(INPUT)  stage of end of leaf appearanc
                         int emerg,                   //(INPUT)  emergence stage
                         float *g_days_tot,            //(INPUT)  duration of each phase (days)
                         float g_dlt_tt,              //(INPUT)  daily thermal time (growing de
                         float *g_node_no,             //(INPUT)  number of fully expanded nodes
                         float *dlt_leaf_no_pot,       //(OUTPUT) new fraction of oldest expanding leaf
                         float *dlt_node_no_pot);       //(OUTPUT) new fraction of oldest expanding node on main stem

void cproc_tpla_max (float g_leaf_no_final,            //(INPUT)final leaf number
                     float g_tiller_no_fertile,        //(INPUT)number of fertile tillers
                     float c_tiller_coef,              //(INPUT)tiller coefficient on TPLAmax
                     float p_main_stem_coef,           //(INPUT)main stem coefficient on TPLAmax
                     float *tpla_max);                  //(OUTPUT) maximum total plant leaf area mm^2

void cproc_leaf_area_pot_tpla (int  begin_stage,                 // (INPUT)  stage number of start leaf area growth
                               int  end_stageTPLAPlateau,        // (INPUT)  stage number to stop TPLA growth
                               int  now,                         // (INPUT)  stage number now = max_stage + 1
                               float *g_phase_tt,                // (INPUT)  required thermal time between stages
                               float *g_tt_tot,                  // (INPUT)  elapsed thermal time between stages
                               float *g_days_tot,                // (INPUT)  elapsed days between stages
                               float  g_current_stage,           // (INPUT)  current stage
                               float  c_initial_tpla,            // (INPUT)  initial total plant area (mm2)
                               float  tpla_max,                  // (INPUT)  maximum total plant area (mm2)
                               float  c_tpla_inflection_ratio,   // (INPUT)  fraction of thermal time from begin to
                                                                 //          end leaf area growth where inflexion on
                                                                 //          TPLA occurs
                               float  *g_tpla_today,              // (OUTPUT)  today's total leaf area per plant (mm2)
                               float  g_tpla_yesterday,          // (INPUT)  yesterday's TPLA (mm2)
                               float  p_tpla_prod_coef,          // (INPUT)  TPLA production coefficient (initial slope)
                               float  g_plants,                  // (INPUT)  Plant density (plants/m2)
                               float  *g_lai,                     // (OUTPUT)  current leaf area index()
                               float *g_dlt_lai_pot);             // (OUTPUT) change in leaf area

float leaf_size_bellshapecurve (float c_x0_const,
                                float c_x0_slope,
                                float g_leaf_no_final,
                                float c_y0_const,
                                float c_y0_slope,
                                float cAConst,
                                float cASlope1,
                                float cASlope2,
                                float cBConst,
                                float cBSlope1,
                                float cBSlope2,
                                float leaf_no);         //(INPUT) nominated leaf number

void cproc_leaf_area_pot_bellshapecurve (int  begin_stage,             //
                                         int  now,                     //
                                         float *g_leaf_no,             //
                                         float  c_leaf_no_correction,  //
                                         float  c_x0_const,            //
                                         float  c_x0_slope,            //
                                         float  g_leaf_no_final,       //
                                         float  c_y0_const,            //
                                         float  c_y0_slope,            //
                                         float  cAConst,               //
                                         float  cASlope1,              //
                                         float  cASlope2,              //
                                         float  cBConst,               //
                                         float  cBSlope1,              //
                                         float  cBSlope2,              //
                                         float  g_dlt_leaf_no,         //
                                         float  g_plants,              //
                                         float  g_swdef_expansion,     //
                                         float *dlt_lai_pot);           // (OUTPUT) change in leaf area

void plant_leaf_detachment (float *leaf_area
			        , float dlt_slai_detached
			        , float plants
			        , int max_node);

void plant_leaf_removal_top (float *leaf_area
                           , float dlt_lai_removed
                           , float plants
                           , int last_node);
//---------------------------------------------------------------------------
void legnew_cover_leaf_pod (
     float g_row_spacing
    ,float *c_x_row_spacing
    ,float *c_y_extinct_coef
    ,int   c_num_row_spacing
    ,float c_extinct_coef_pod
    ,float  canopy_fac
    ,float g_lai
    ,float g_pai
    ,float *lai_canopy
    ,float *g_cover_green
    ,float *g_cover_pod
    ) ;
  void legnew_cover (
    float g_row_spacing
    ,float *c_x_row_spacing
    ,float *c_y_extinct_coef
    ,int   c_num_row_spacing
    ,float canopy_fac
    ,float g_lai
    ,float *g_cover_green
    ) ;
  void legnew_extinct_coef
    (
     float g_row_spacing
    ,float *c_x_row_spacing
    ,float *c_y_extinct_coef
    ,int   c_num_row_spacing
    ,float *extinct_coef
    ) ;

  float legnew_node_no_from_area
    (
     float *g_leaf_area
    ,int   num_nodes
    ,float pla
    ) ;
  float legnew_leaf_no_from_area (
     float  *g_leaf_area
    ,float  *g_leaf_no
    ,int    num_nodes
    ,float  pla
    ) ;


//---------------------------------------------------------------------------


float crop_failure_germination(commsInterface *,
                              int days_germ_limit,   // (INPUT)  maximum days allowed after sowing for germination to take place (days)
                              int daysInStage,        // (Input) days we have spent in current stage
                              float plants);         // (INPUT)  Plant density (plants/m^2)

float crop_failure_emergence(commsInterface *,
                            float tt_emerg_limit,    // (INPUT)  maximum degree days allowed for emergence to take place (deg day)
                             float ttInStage,         // (INPUT)  the sum of growing degree days for a phenological stage (oC d)
                             float plants);           // (INPUT)  Plant density (plants/m^2)

float crop_failure_leaf_sen (commsInterface *
                            ,float g_lai              // (INPUT)  live plant green lai
                            ,float g_plants);           // (INPUT)  Plant density (plants/m^2)

float crop_failure_phen_delay (commsInterface *
                              ,float c_swdf_pheno_limit          // (INPUT)  critical cumulative phenology water stress above which the crop fails (unitless)
                              ,float cswd_pheno                 // (INPUT)  cumulative water stress type 3
                              ,float g_plants);                    // (INPUT)  Plant density (plants/m^2)

void crop_death_drought (commsInterface *,
                         int  emerg,                // (INPUT) emergence stage
                         int  flag_leaf,            // (INPUT) flag leaf stage
                         int  plant_end,            // (INPUT) maximum plant stage
                         float *g_cswd_photo,       // (INPUT) cumulative water stress photosynthesis
                         float *g_leaf_no,          // (INPUT) leaf no in different phases
                         float  c_leaf_no_crit,     // (INPUT) critical leaf no below which drought death may occur
                         float  c_swdf_photo_limit, // (INPUT) critical cumulative photosynthesis water stress, above which the crop partly fails (unitless)
                         float  g_swdef_photo,      // (INPUT) daily water stress for photosynthesis
                         float  c_swdf_photo_rate,  // (INPUT) rate of plant reduction with photosynthesis water stress,above which the crop fails (unitless)
                         float  g_plants,           // (INPUT) plant density (plants/m2)
                         float *dlt_plants);         // (OUTPUT)daily plant death (plants/m2)

void crop_death_seedling_hightemp (commsInterface *,
                                   int days_after_emerg,           // (INPUT) days after emergence
                                   int g_year,                     // (INPUT) year
                                   int g_day_of_year,              // (INPUT) day of year
                                   float *g_soil_temp,              // (INPUT) soil surface temperature (C)
                                   float *c_x_weighted_temp,        // (INPUT) soil temperature (C) in lookup table
                                   float *c_y_plant_death,          // (INPUT) fraction of plants killed
                                   int   c_num_weighted_temp,        // (INPUT) no of table elements
                                   float g_plants,                 // (INPUT) plant density (plants/m2)
                                   float *dlt_plants);               // (OUPUT) daily plant death (plants/m2)

void soil_temp_weighted_3days (int  g_year,                  //   (INPUT) year
                               int  g_day_of_year,           //   (INPUT) day of year
                               float *g_soil_temp,            //   (INPUT) soil surface temperature (C)
                               float *c_x_weighted_temp,     //   (INPUT) soil temperature (C) in lookup table
                               float *c_y_plant_death,       //   (INPUT) fraction of plants killed
                               int   c_num_weighted_temp,    //   (INPUT) no of table elements
                               float *killfr);                //    (OUTPUT) fraction of plants killed

void crop_death_actual (float g_dlt_plants_failure_germ,
                        float g_dlt_plants_failure_emergence,
                        float g_dlt_plants_failure_leaf_sen,
                        float g_dlt_plants_failure_phen_delay,
                        float g_dlt_plants_death_seedling,
                        float g_dlt_plants_death_drought,
                        float g_dlt_plants_death_barrenness,
                        float *dlt_plants);
  void crop_crown_temp_nwheat
   ( float tempmx
    ,float tempmn
    ,float snow
    ,float *tempcx
    ,float *tempcn
    ) ;

//---------------------------------------------------------------------------
  void cproc_n_uptake3
    (
     float  *g_dlayer
    ,int    max_layer
    ,float  *g_no3gsm_uptake_pot
    ,float  *g_nh4gsm_uptake_pot
    ,float  g_n_fix_pot
    ,const char   *c_n_supply_preference
    ,float  *g_n_demand
    ,float  *g_n_max
    ,int    max_part
    ,float  g_root_depth
    ,float  *dlt_no3gsm
    ,float  *dlt_nh4gsm
    ) ;
  void cproc_n_supply3 (
     float  g_dlayer[]
    ,int    max_layer
    ,float  g_no3gsm[]
    ,float  g_no3gsm_min[]
    ,float  *g_no3gsm_uptake_pot
    ,float  g_root_depth
    ,float  *g_root_length
    ,float  g_bd[]
    ,float  c_n_stress_start_stage
    ,float  c_total_n_uptake_max
    ,float  c_no3_uptake_max
    ,float  c_no3_conc_half_max
    ,float  *g_sw_avail_pot
    ,float  *g_sw_avail
    ,float  g_current_stage
    ,float  *c_n_fix_rate
    ,float  fixation_determinant
    ,float  g_swdef_fixation
    ,float  *g_n_fix_pot
    ) ;
void cproc_n_supply4 (float* g_dlayer  //  ! (INPUT)
               ,float* g_bd                   //
               ,int max_layer                 //  ! (INPUT)
               ,float* g_NO3gsm               //  ! (INPUT)
               ,float* g_NO3gsm_min           //  ! (INPUT)
               ,float* G_no3gsm_uptake_pot    //
               ,float* g_NH4gsm               //  ! (INPUT)
               ,float* g_NH4gsm_min           //  ! (INPUT)
               ,float* G_nH4gsm_uptake_pot    //
               ,float g_root_depth            //  ! (INPUT)
               ,float c_n_stress_start_stage  //
               ,float c_kno3                   //
               ,float c_no3ppm_min
               ,float c_knh4                   //
               ,float c_nh4ppm_min
               ,float c_total_n_uptake_max    //
               ,float* g_sw_avail_pot         //  ! (INPUT)
               ,float* g_sw_avail             //  ! (INPUT)
               ,float G_current_stage         //  ! (INPUT)
               ,float *C_n_fix_rate            //  ! (INPUT)
               ,float fixation_determinant    //  ! (INPUT)
               ,float G_swdef_fixation        //  ! (INPUT)
               ,float *g_n_fix_pot);           //   ! (outPUT)
void legnew_n_senescence1
    (
     int    num_part
    ,float  *c_n_sen_conc
    ,float  *g_dlt_dm_senesced
    ,float  *g_n_green
    ,float  *g_dm_green
    ,float  *dlt_n_senesced_trans
    ,float  *dlt_n_senesced) ;


void crop_n_conc_ratio(const int leaf,       //IN
                       const int stem,       //IN
                       float *dm_green,      //(INPUT)  live plant dry weight (biomass)
                       float *N_conc_crit,   //(INPUT)  critical N concentration (g N/g
                       float *N_conc_min,    //(INPUT)  minimum N concentration (g N/g b
                       float *N_green,       //(INPUT)  plant nitrogen content (g N/m^2)
                       float *N_conc_ratio); //(OUTPUT) available N as fraction of N c

void crop_nfact_photo(const int leaf,
                      const int stem,
                      float *dm_green,       //(INPUT)  live plant dry weight (biomass)
                      float *n_conc_crit,    //(INPUT)  critical N concentration (g N/g
                      float *n_conc_min,     //(INPUT)  minimum N concentration (g N/g b
                      float *n_green,        //(INPUT)  plant nitrogen content (g N/m^2)
                      float n_fact_photo,    //(INPUT)  multipler for N deficit effect o
                      float *nfact);         //(OUTPUT) N stress factor

void crop_nfact_pheno(const int leaf,        //IN
                      const int stem,        //IN
                      float *dm_green,       //(INPUT)  live plant dry weight (biomass)
                      float *n_conc_crit,    //(INPUT)  critical N concentration (g N/g
                      float *n_conc_min,     //(INPUT)  minimum N concentration (g N/g b
                      float *n_green,        //(INPUT)  plant nitrogen content (g N/m^2)
                      float n_fact_pheno,    //(INPUT)  multipler for N deficit effect on phenology
                      float *nfact);          //(OUTPUT) N stress factor

void crop_nfact_grain_conc(const int leaf,
                           const int stem,
                           float *dm_green,         //(INPUT)  live plant dry weight (biomass)
                           float *n_conc_crit,      //(INPUT)  critical N concentration (g N/g
                           float *n_conc_min,       //(INPUT)  minimum N concentration (g N/g b
                           float *n_green,          //(INPUT)  plant nitrogen content (g N/m^2)
                           float *nfact);            //(OUTPUT) N stress factor

void crop_nfact_expansion(const int leaf,
                          float *dm_green,         //(INPUT) live plant dry weight (biomass)  (g/m^2)
                          float *N_conc_crit,      //(INPUT)  critical N concentration (g N/ biomass)
                          float *N_conc_min,       // (INPUT) minimum N concentration (g N/g biomass)
                          float *N_green,          // (INPUT) plant nitrogen content (g N/m^2
                          float N_fact_expansion, //(INPUT) multipler for N deficit effect leaf expansion
                          float *nfact);            //(OUTPUT) stress factor

void crop_n_detachment(const int num_part,
                       const int root,
                       const int leaf,
                       float dm_leaf_detach_frac,  // (INPUT)  fraction of senesced leaf dry matter detaching from live plant each day (0-1)
                       float *dlt_N_senesced,      //  (INPUT)  actual N loss with senesced plant (g/m^2)
                       float *dlt_N_detached);      // (OUTPUT) actual nitrogen senesced  from plant parts (g/m^2)

void cproc_n_detachment1(const int max_part,
                         float *c_sen_detach_frac,
                         float *g_n_senesced,
                         float *g_dlt_n_detached,
                         float *c_dead_detach_frac,
                         float *g_n_dead,
                         float *g_dlt_n_dead_detached);

float crop_n_dlt_grain_conc(const int grain,
                            float sfac_slope,      //(INPUT)  soil water stress factor slope
                            float sw_fac_max,      //(INPUT)  soil water stress factor maximum
                            float temp_fac_min,    //(INPUT)  temperature stress factor minimum optimum temp
                            float tfac_slope,      //(INPUT)  temperature stress factor slope
                            float maxt,            //(INPUT)  maximum air temperature (oC)
                            float mint,            //(INPUT)  maximum air temperature (oC)
                            float nfact_grain_conc,// (INPUT)
                            float *n_conc_crit,     //(INPUT)  critical N concentration (g N/g biomass)
                            float *n_conc_min,      //(INPUT)  minimum N concentration (g N/g biomass)
                            float swdef_expansion); // (INPUT)

void crop_n_retrans_avail(const int num_part,
                          const int root,
                          const int grain,
                          float *g_N_conc_min,
                          float *g_dm_green,
                          float *g_N_green,
                          float *N_avail);

void cproc_n_senescence1 (const int num_part,         //(INPUT) number of plant part
                          float *c_n_sen_conc,         //(INPUT)  N concentration of senesced materia (g/m^2)
                          float *g_dlt_dm_senesced,   //(INPUT)  plant biomass senescence (g/m^2)
                          float *g_n_green,           //(INPUT) nitrogen in plant material (g/m^2)
                          float *g_dm_green,          //(INPUT) plant material (g/m^2)
                          float *dlt_N_senesced);      // (OUTPUT) actual nitrogen senesced from plant parts (g/m^2)

void cproc_n_uptake1(float C_no3_diffn_const,   //(INPUT)  time constant for uptake by di
                     float *G_dlayer,            //(INPUT)  thickness of soil layer I (mm)//
                     const int max_layer,        // INPUT)  max number of soil layers
                     float *G_no3gsm_diffn_pot,  //(INPUT)  potential NO3 (supply) from so
                     float *G_no3gsm_mflow_avail,// (INPUT)  potential NO3 (supply) from
                     float G_n_fix_pot,         //(INPUT) potential N fixation (g/m2)
                     const char *C_n_supply_preference, //(INPUT)
                     float *G_n_demand,                //(INPUT)  critical plant nitrogen demand
                     float *G_n_max,                   //(INPUT)  maximum plant nitrogen demand
                     const int max_part,              //(INPUT)  number of plant parts
                     float G_root_depth,              // (INPUT)  depth of roots (mm)
                     float *dlt_no3gsm);                // (OUTPUT) actual plant N uptake from NO3 in each layer (g/m^2

void cproc_n_supply1 (float *G_dlayer,                   // (INPUT)
                      int    max_layer,               // (INPUT)
                      float *G_dlt_sw_dep,               // (INPUT)
                      float *G_NO3gsm,                   // (INPUT)
                      float *G_NO3gsm_min,               // (INPUT)
                      float G_root_depth,                // (INPUT)
                      float *G_sw_dep,                   // (INPUT)
                      float *G_NO3gsm_mflow_avail,       // (OUTPUT)
                      float *G_sw_avail,                 // (INPUT)
                      float *G_NO3gsm_diffn_pot,         // (OUTPUT)
                      float G_current_stage,             // (INPUT)
                      float *C_n_fix_rate,               // (INPUT)
                      float fixation_determinant,       // (INPUT)
                      float G_swdef_fixation,           // (INPUT)
                      float *G_N_fix_pot);                // (INPUT)

void crop_n_mass_flow1(const int num_layer,          // (INPUT)  number of layers in profile
                       float *dlayer,                // (INPUT)  thickness of soil layer I (mm)
                       float *dlt_sw_dep,            // (INPUT)  water uptake in each layer (mm water)
                       float *no3gsm,                // (INPUT)  nitrate nitrogen in layer L (g N/m^2)
                       float *no3gsm_min,            // (INPUT)  minimum allowable NO3 in soil (g/m^2)
                       float root_depth,             // (INPUT)  depth of roots (mm)
                       float *sw_dep,                // (INPUT)  soil water content of layer L (mm)
                       float *NO3gsm_mflow_pot);      // (OUTPUT) potential plant NO3 uptake (supply) g/m^2 by mass flow

void crop_n_diffusion1 (const int num_layer,      // (INPUT)  number of layers in profile
                        float *dlayer,            // (INPUT)  thickness of soil layer I (mm)
                        float *no3gsm,            // (INPUT)  nitrate nitrogen in layer L (g N/m^
                        float *no3gsm_min,        // (INPUT)  minimum allowable NO3 in soil (g/m^
                        float root_depth,         // (INPUT)  depth of roots (mm)
                        float *sw_avail,          // (INPUT)  actual extractable soil water (mm)
                        float *sw_avail_pot,      // (INPUT)  potential extractable soil water (m
                        float *NO3gsm_diffn_pot);  // (OUTPUT) potential plant NO3 by diffusion

void crop_n_fixation_pot1( float G_current_stage,              // (INPUT) Current stage
                           float *C_n_fix_rate,                // (INPUT)  potential rate of N fixation (
                           float fixation_determinant,         // (INPUT)
                           float G_swdef_fixation,             // (INPUT)
                           float *N_fix_pot);                   // (OUTPUT) N fixation potential (g/

void cproc_n_demand1(const int max_part,          // (INPUT)
                     int   *demand_parts,         // (INPUT)
                     const int num_demand_parts,  // (INPUT)
                     float G_dlt_dm,              // (INPUT)  the daily biomass production (
                     float *G_dlt_dm_green,        // (INPUT)  plant biomass growth (g/m^2)
                     float G_dlt_dm_pot_rue,      // (INPUT)  potential dry matter productio
                     float *G_dlt_n_retrans,       // (INPUT)  nitrogen retranslocated out fr
                     float *G_dm_green,            // (INPUT)  live plant dry weight (biomass
                     float *G_n_conc_crit,         // (INPUT)  critical N concentration (g N/
                     float *G_n_conc_max,          // (INPUT)  maximum N concentration (g N/g
                     float *G_n_green,             // (INPUT)  plant nitrogen content (g N/m^
                     float *N_demand,             // (OUTPUT) critical plant nitrogen demand g/m^2)
                     float *N_max);                // (OUTPUT) max plant nitrogen demand  (g/m^2)

void cproc_n_init1(float *C_n_init_conc,  // (INPUT)  initial N concentration (
                   int    max_part,
                   float *G_dm_green,     // (INPUT)  live plant dry weight (biomass
                   float *N_green);        // plant nitrogen (g/m^2)

void crop_n_dead_detachment(int num_part,
                            float *dead_detach_frac, //(INPUT)  fraction of dead plant parts detaching each day (0-1)
                            float *n_dead,           //(INPUT)  plant N content of dead plants (g N/m^2)
                            float *dlt_N_dead_detached);  //(OUTPUT) change in dm of dead plants (g/m^2)

void cproc_n_supply2 (float *g_dlayer,                // (INPUT)
                      const int max_layer,            // (INPUT)
                      float *g_dlt_sw_dep,            // (INPUT)
                      float *g_NO3gsm,                // (INPUT)
                      float *g_NO3gsm_min,            // (INPUT)
                      float g_root_depth,             // (INPUT)
                      float *g_sw_dep,                // (INPUT)
                      float *g_NO3gsm_mflow_avail,    // (OUTPUT)
                      float *g_sw_avail,              // (INPUT)
                      float *g_sw_avail_pot,          // (INPUT)
                      float *g_NO3gsm_diffn_pot,      // (OUTPUT)
                      float g_current_stage,          // (INPUT)
                      float *c_n_fix_rate,             // (INPUT)
                      float fixation_determinant,     // (INPUT)
                      float g_swdef_fixation,        // (INPUT)
                      float *g_N_fix_pot);             // (INPUT)

  void crop_grain_number (
     float g_current_stage
    ,float *g_days_tot
    ,int   emerg
    ,int   flowering
    ,float *dm_green
    ,int   stem
    ,float p_grains_per_gram_stem
    ,float *g_grain_no
    ) ;

//---------------------------------------------------------------------------
  void cproc_phenology_nw (
     float *g_previous_stage
    ,float *g_current_stage
    ,int   sowing_stage
    ,int   germ_stage
    ,int   end_development_stage
    ,int   start_stress_stage
    ,int   end_stress_stage
    ,int   end_flower_stage
    ,int   max_stage
    ,int   c_num_temp
    ,float *c_x_temp
    ,float *c_y_tt
    ,float g_maxt
    ,float g_mint
    ,float g_nfact_pheno
    ,float g_swdef_pheno
    ,float g_swdef_pheno_flower
    ,float g_swdef_pheno_grainfill
    ,float g_vern_eff
    ,float g_photop_eff
    ,float c_pesw_germ
    ,float *c_fasw_emerg            // (INPUT)
    ,float *c_rel_emerg_rate        // (INPUT)
    ,int   c_num_fasw_emerg         // (INPUT)
    ,float *g_dlayer
    ,int   max_layer
    ,float g_sowing_depth
    ,float *g_sw_dep
    ,float *g_dul_dep
    ,float *p_ll_dep
    ,float *g_dlt_tt
    ,float *g_phase_tt
    ,float *g_phase_devel
    ,float *g_dlt_stage
    ,float *g_tt_tot
    ,float *g_days_tot) ;

float legume_stage_code( float *C_stage_code_list //(INPUT)  list of stage numbers
                   , float *G_phase_tt        //(INPUT)  Cumulative growing degree days required for each stage (deg days)
                   , float *G_tt_tot          //(INPUT)  Cumulative growing degree days required for each stage (deg days)
                   , float stage_no           //(INPUT) stage number to convert
                   , float *stage_table       //(INPUT) table of stage codes
                   , int numvals, int max_stage);             //(INPUT) size_of of table

float plant_stage_code (float  *c_stage_code_list
			  ,float  *g_phase_tt
			  ,float  *g_tt_tot
			  ,float  stage_no
			  ,float  *stage_table
			  ,int    numvals, int max_stage);
float crop_stage_code (float *c_stage_code_list,
                       float *g_tt_tot,
                       float *g_phase_tt,
                       float stage_no,              // (INPUT) stage number to convert
                       float *stage_table,           // (INPUT) table of stage codes
                       int   numvals,                 // (INPUT) size_of of table
                       int   max_stage);               // (INPUT) max stage number

void crop_thermal_time (int    C_num_temp,          //(INPUT)  size_of table
                        float *C_x_temp,            //(INPUT)  temperature table for photosyn
                        float *C_y_tt,              //(INPUT)  degree days
                        float  G_current_stage,     //(INPUT)  current phenological stage
                        float  G_maxt,              //(INPUT)  maximum air temperature (oC)
                        float  G_mint,              //(INPUT)  minimum air temperature (oC)
                        int    start_stress_stage,  //(INPUT)
                        int    end_stress_stage,    //(INPUT)
                        float  G_nfact_pheno,       //(INPUT)
                        float  G_swdef_pheno,       //(INPUT)
                        float *G_dlt_tt);           //(OUTPUT) daily thermal time (oC)

float crop_phase_tt(float G_dlt_tt,          //(INPUT)  daily thermal time (growing de
                    float *G_phase_tt,       //(INPUT)  Cumulative growing degree days
                    float *G_tt_tot,         //(INPUT)  the sum of growing degree days
                    float stage_no);         //(INPUT) stage number

void crop_devel(float G_phase_devel,        //(INPUT)  development of current phase (
                float *g_dlt_stage,           //(OUTPUT) change in growth stage
                float *g_current_stage);      //(INPUT/OUTPUT) new stage no.

float crop_germination(int   sowing_stage,             //(INPUT)
                       int   germ_stage,               //(INPUT)
                       float C_pesw_germ,              //(INPUT)  plant extractable soil water i
                       float G_current_stage,          //(INPUT)  current phenological stage
                       float *G_days_tot,              //(INPUT)  duration of each phase (days)
                       float *G_dlayer,                //(INPUT)  thickness of soil layer I (mm)
                       int   max_layer,                //(INPUT)
                       float G_sowing_depth,           //(INPUT)  sowing depth (mm)
                       float *G_sw_dep,                //(INPUT)  soil water content of layer L
                       float *P_ll_dep);                //(INPUT)  lower limit of plant-extractab


void crop_phase_devel(int   sowing_stage,              //(INPUT)
                      int   germ_stage,                //(INPUT)
                      int   end_development_stage,     //(INPUT)
                      float C_pesw_germ,               //(INPUT)  plant extractable soil water i
                      float *C_fasw_emerg,             // (INPUT)
                      float *C_rel_emerg_rate,         // (INPUT)
                      int   C_num_fasw_emerg,          //(INPUT)
                      float G_current_stage,           //(INPUT)  current phenological stage
                      float *G_days_tot,               // (INPUT)  duration of each phase (days)
                      float *G_dlayer,                 // (INPUT)  thickness of soil layer I (mm)
                      int   max_layer,                 //(INPUT)
                      float G_sowing_depth,            //(INPUT)  sowing depth (mm)
                      float *G_sw_dep,                 // (INPUT)  soil water content of layer L
                      float *G_dul_dep,                // (INPUT)
                      float *P_ll_dep,                 // (INPUT)  lower limit of plant-extractab
                      float G_dlt_tt,                  // (INPUT)  Cumulative growing degree days
                      float *G_phase_tt,               //
                      float *G_tt_tot,                 // (INPUT)  the sum of growing degree days
                      float *phase_devel);              // (OUTPUT) fraction of current phase
                                                       //          elapsed ()
void plant_phenology3 (float *g_previous_stage
                             ,float *g_current_stage
                             ,int   sowing_stage
                             ,int   germ_stage
                             ,int   end_flowering_stage
                             ,int   end_development_stage
                             ,int   start_stress_stage
                             ,int   end_stress_stage
                             ,int   max_stage
                             ,int   c_num_temp
                             ,float *c_x_temp
                             ,float *c_y_tt
                             ,float g_maxt
                             ,float g_mint
                             ,float g_nfact_pheno
                             ,float g_swdef_pheno
                             ,float g_swdef_pheno_flower
                             ,float g_swdef_pheno_grainfill
                             ,float c_pesw_germ
                             ,float *c_fasw_emerg
                             ,float *c_rel_emerg_rate
                             ,int   c_num_fasw_emerg
                             ,float *g_dlayer
                             ,int   max_layer
                             ,float g_sowing_depth
                             ,float *g_sw_dep
                             ,float *g_dul_dep
                             ,float *p_ll_dep
                             ,float *g_dlt_tt
                             ,float *g_phase_tt
                             ,float *g_phase_devel // OUT
                             ,float *g_dlt_stage
                             ,float *g_tt_tot
                             ,float *g_days_tot);

void cproc_phenology1 (float  *G_previous_stage,         //   OUTPUT
                       float  *G_current_stage,          //   OUTPUT
                       int    sowing_stage,              //   IN
                       int    germ_stage,                //   IN
                       int    end_development_stage,     //   IN
                       int    start_stress_stage,        //   IN
                       int    end_stress_stage,          //   IN
                       int    max_stage,                 //   IN
                       int    C_num_temp,                //   IN
                       float *C_x_temp,                  //   IN
                       float *C_y_tt,                   //    IN
                       float  G_maxt,                    //   IN
                       float  G_mint,                    //   IN
                       float  G_nfact_pheno,             //   IN
                       float  G_swdef_pheno,             //   IN
                       float  C_pesw_germ,               //   IN
                       float *C_fasw_emerg,             //    (INPUT)
                       float *C_rel_emerg_rate,         //    (INPUT)
                       int    C_num_fasw_emerg,          //   (INPUT)
                       float *G_dlayer,                 //    IN
                       int    max_layer,                 //   IN
                       float  G_sowing_depth,            //   IN
                       float *G_sw_dep,                 //    IN
                       float *G_dul_dep,                //    IN
                       float *P_ll_dep,                 //    IN
                       float *G_dlt_tt,                 //    OUT
                       float *G_phase_tt,               //    OUT
                       float *G_phase_devel,             //   OUT
                       float *G_dlt_stage,              //    OUT
                       float *G_tt_tot,                 //    OUT
                       float *G_days_tot);               //    OUT

void crop_germ_dlt_tt(float *C_fasw_emerg,        //(INPUT)  plant extractable soil water i
                      float *C_rel_emerg_rate,    //(INPUT)
                      int    C_num_fasw_emerg,    //(INPUT)
                      float  G_current_stage,     //(INPUT)  current phenological stage
                      int    germ_phase,          //(INPUT)
                      float *G_dlayer,            //(INPUT)  thickness of soil layer I (mm)
                      int    max_layer,           //(INPUT)
                      float  G_sowing_depth,      //(INPUT)  sowing depth (mm)
                      float *G_sw_dep,            //(INPUT)  soil water content of layer L
                      float *P_ll_dep,            //(INPUT)  lower limit of plant-extractab
                      float *G_dul_dep,           //(INPUT)  drained upper limit(mm)
                      float *G_dlt_tt);            //(IN/OUTPUT)


float crop_sw_avail_fac(int   num_sw_ratio,             //(INPUT)
                        float *x_sw_ratio,              //(INPUT)
                        float *y_sw_fac_root,           //(INPUT)
                        float *dul_dep,                 //(INPUT) drained upper limit for layer L (mm water)
                        float *sw_dep,                  //(INPUT) soil water content of layer L (mm)
                        float *ll_dep,                  //(INPUT) lower limit of plant-extractable soil
                        int   layer);                    //(INPUT) soil profile layer number
void cproc_root_length_growth_new (
     float  c_specific_root_length                 // (INPUT) length of root per unit wt (mm
    ,float  p_root_distribution_pattern            // (INPUT) patter with depth
    ,float *g_dlayer                               // (INPUT)  thickness of soil layer I (mm)
    ,float  g_dlt_root_wt                          // (INPUT)  plant root biomass growth (g/m
    ,float *g_dlt_root_length                      // (OUTPUT) increase in root length (mm/mm
    ,float  g_dlt_root_depth                       // (INPUT)  increase in root depth (mm)
    ,float  g_root_depth                           // (INPUT)  depth of roots (mm)
    ,float *g_root_length                          // (INPUT)
    ,float  g_plants                               // (INPUT)
    ,float *p_xf                                   // (INPUT)  eXtension rate Factor (0-1)
    ,int    c_num_sw_ratio                         // (INPUT)
    ,float *c_x_sw_ratio                           // (INPUT)
    ,float *c_y_sw_fac_root                        // (INPUT)
    ,float *c_x_plant_rld                          // (INPUT)
    ,float *c_y_rel_root_rate                      // (INPUT)
    ,int    c_num_plant_rld                        // (INPUT)
    ,float *g_dul_dep                              // (INPUT)  drained upper limit soil water
    ,float *g_sw_dep                               // (INPUT)  soil water content of layer L
    ,float *p_ll_dep                               // (INPUT)  lower limit of plant-extractab
    ,int    max_layer);                             // (INPUT)  maximum number of soil laye


void cproc_root_length_senescence1(float  C_specific_root_length,    //(INPUT)  length of root per unit wt (m
                                   float *G_dlayer,                  //(INPUT)  thickness of soil layer I (mm)
                                   float  G_dlt_root_dm_senesced,    //(INPUT)  plant biomass senescence  (g/m^2)
                                   float *G_root_length,             //(INPUT)
                                   float  G_root_depth,              //(INPUT)  depth of roots (mm)
                                   float *G_dlt_root_length_senesced,//(OUTPUT) root length lost from each layer (mm/mm^2)
                                    int   max_layer);                 // (INPUT)  maximum layer number

void cproc_root_length_init1 (float root_wt,
                              float c_specific_root_length,
                              float g_root_depth,
                              float *g_dlayer,
                              float *g_root_length,
                              int   max_layer);

void cproc_root_length_growth1(float  C_specific_root_length,      //   (INPUT) length of root per unit wt (mm
                               float *G_dlayer,                    //   (INPUT)  thickness of soil layer I (mm)
                               float  G_dlt_root_wt,               //   (INPUT)  plant root biomass growth (g/m
                               float *G_dlt_root_length,           //   (OUTPUT) increase in root length (mm/mm
                               float  G_dlt_root_depth,            //   (INPUT)  increase in root depth (mm)
                               float  G_root_depth,                //   (INPUT)  depth of roots (mm)
                               float *G_root_length,               //   (INPUT)
                               float  G_plants,                    //   (INPUT)
                               float *P_xf,                        //   (INPUT)  eXtension rate Factor (0-1)
                               int    C_num_sw_ratio,              //   (INPUT)
                               float *C_x_sw_ratio,                //   (INPUT)
                               float *C_y_sw_fac_root,             //   (INPUT)
                               float *C_x_plant_rld,               //   (INPUT)
                               float *C_y_rel_root_rate,           //   (INPUT)
                               int    C_num_plant_rld,             //   (INPUT)
                               float *G_dul_dep,                   //   (INPUT)  drained upper limit soil water
                               float *G_sw_dep,                    //   (INPUT)  soil water content of layer L
                               float *P_ll_dep,                    //   (INPUT)  lower limit of plant-extractab
                               int    max_layer);                   //   (INPUT)  maximum number of soil laye

void crop_root_dist(float *G_dlayer,          //(INPUT)  thickness of soil layer I (mm)
                    float *G_root_length,     //(INPUT)
                    float G_root_depth,       //(INPUT)  depth of roots (mm)
                    float *root_array,        //(OUTPUT) array to contain distributed material
                    float root_sum,           //(INPUT) Material to be distributed
                    int   max_layer);          // (INPUT) max number of soil layers

void crop_root_redistribute (float *root_length,       //  root length (mm/mm^2)
                             float  root_depth_old,    //  old root depth (mm)
                             float *dlayer_old,        //  old soil profile layers (mm)
                             int    nlayr_old,         //  number of old soil profile layers
                             float root_depth_new,     //  new root depth (mm)
                             float *dlayer_new,        //  new soil profile layers (mm)
                             int   nlayr_new);          //  number of new soil profile layers

void crop_root_sw_avail_factor(int  num_sw_ratio,                 // (INPUT)
                               float *x_sw_ratio,                 // (INPUT)
                               float *y_sw_fac_root,              // (INPUT)
                               float *dlayer,                     // (INPUT) layer depth (mm)
                               float *dul_dep,                    // (INPUT) drained upper limit for layer L (mm water)
                               float *sw_dep,                     // (INPUT) soil water content of layer L (mm)
                               float *ll_dep,                     // (INPUT) lower limit of plant-extractable soil water for soil layer L (mm)
                               float  root_depth,                 //
                               float *sw_avail_factor);            // (OUTPUT) sw availability factor for root depth growth
void legopt_root_depth1
    (
     float *g_dlayer
    ,int   c_num_sw_ratio
    ,float *c_x_sw_ratio
    ,float *c_y_sw_fac_root
    ,float *g_dul_dep
    ,float *g_sw_dep
    ,float *p_ll_dep
    ,float *c_root_depth_rate
    ,float g_current_stage
    ,float g_mint
    ,float g_maxt
    ,float *c_x_temp_root_advance
    ,float *c_y_rel_root_advance
    ,int   c_num_temp_root_advance
    ,float *p_xf
    ,float g_root_depth
    ,float *g_dlt_root_depth
    ) ;

void cproc_root_depth2 (float G_current_stage,             //  (INPUT) current growth stage
                        float G_maxt,                     //
                        float G_mint,                     //
                        float G_swdef_photo,              //
                        float G_root_depth,               //  (OUTPUT) root depth (mm)
                        int   C_num_temp_root,              //
                        float *C_x_temp_root,              //
                        float *C_y_temp_root_fac,          //
                        int   C_num_ws_root,                //
                        float *C_x_ws_root,                //
                        float *C_y_ws_root_fac,            //
                        int   C_num_sw_ratio,               //! (INPUT) number of sw lookup pairs
                        float *C_x_sw_ratio,               //  (INPUT) sw factor lookup x
                        float *C_y_sw_fac_root,            //  (INPUT) sw factor lookup y
                        float *G_dlayer,                   //  (INPUT)  layer thicknesses (mm)
                        float *G_dul_dep,                  //  (INPUT) DUL (mm)
                        float *G_sw_dep,                   //  (INPUT) SW (mm)
                        float *P_ll_dep,                   //  (INPUT) LL (mm)
                        float *C_root_depth_rate,          //  (INPUT) root front velocity (mm)
                        float *P_xf,                       //  (INPUT) exploration factor
                        float *G_dlt_root_depth);           //  (OUTPUT) increase in rooting depth (mm

void crop_root_depth_increase2(float current_stage,             //(INPUT)  current phenological stage
                               float *c_root_depth_rate,         //(INPUT)  root growth rate potential (mm
                               float *dlayer,                    //(INPUT)  thickness of soil layer I (mm)
                               float root_depth,                //(INPUT)  depth of roots (mm)
                               float temp_factor,               //(INPUT)  depth of roots (mm)
                               float ws_factor,                 //(INPUT)  depth of roots (mm)
                               float sw_avail_factor,           //(INPUT)
                               float *p_xf,                      //(INPUT) eXploration Factor (0-1)
                               float *dlt_root_depth);            //(OUTPUT) increase in root depth (mm)
  void legopt_root_depth_increase
    (
     float  c_root_depth_rate[]
    ,float  g_current_stage
    ,float  *g_dlayer
    ,float  g_root_depth
    ,float  g_sw_avail_fac_deepest_layer
    ,float  *p_xf
    ,float  temp_factor
    ,float  *dlt_root_depth
    ) ;

//---------------------------------------------------------------------------

void crop_temperature_stress_photo (int   num_ave_temp ,     // (INPUT)  size_of critical temperature table
                                    float *x_ave_temp,        // (INPUT)  critical temperatures for photosynthesis (oC)
                                    float *y_stress_photo,    // (INPUT)  Factors for critical temperatures (0-1)
                                    float maxt,              // (INPUT)  maximum air temperature (oC)
                                    float mint,              // (INPUT)  minimum air temperature (oC)
                                    float *temp_stress_photo); // (OUTPUT) photosynthetic reduction factor for  temperature stress (0-1)

void crop_store_value(int day_of_year,
                               int year,
                               float *array,
                               float value);

float crop_running_ave(int day_of_year,
                       int year,
                       float *array,
                       int number_of_days);

void crop_part_fraction_delta (int    part_no,
                               float *fraction,         // (INPUT)  fraction for each part
                               float part,              // (INPUT)  part value to use
                               float *dlt_part);        // (OUTPUT) change in part

void crop_pool_fraction_delta (int num_part,         // (INPUT)  number of plant parts
                               float *fraction,      // (INPUT)  fraction of pools to detach
                               float *pool,          // (INPUT)  plant pool for detachment (g/m^2)
                               float *dlt_pool);      // (OUTPUT) change in plant pool

void crop_sw_avail(int   num_layer,     // (INPUT)  number of layers in profile
                   float *dlayer,            // (INPUT)  thickness of soil layer I (mm)
                   float root_depth,        // (INPUT)  depth of roots (mm)
                   float *sw_dep,            // (INPUT)  soil water content of layer L (mm)
                   float *ll_dep,            // (INPUT)  lower limit of plant-extractable  soil water for soil layer L (mm)
                   float *sw_avail);          // (OUTPUT) crop water potential uptake  for each full layer (mm)

void crop_sw_supply(int   num_layer,        // (INPUT)  number of layers in profile
                    float *dlayer,          // (INPUT)  thickness of soil layer I (mm)
                    float root_depth,       // (INPUT)  depth of roots (mm)
                    float *sw_dep,          // (INPUT)  soil water content of layer L (mm)
                    float *kl,              // (INPUT)  root length density factor for water
                    float *ll_dep,          // (INPUT)  lower limit of plant-extractable soi
                    float *sw_supply);       // (OUTPUT) potential crop water uptake from each layer (mm) (supply to roots)

void crop_sw_uptake0(int   num_layer,      // (INPUT)  number of layers in profile
                     float *dlayer,             // (INPUT)  thickness of soil layer I (mm)
                     float root_depth,         // (INPUT)  depth of roots (mm)
                     float sw_demand,          // (INPUT)  total crop demand for water (mm)
                     float *sw_supply,          // (INPUT)  potential water to take up (supply) from current soil water (mm)
                     float *dlt_sw_dep);         // (OUTPUT) root water uptake (mm)

void crop_swdef_expansion(int   num_sw_demand_ratio,   //  (INPUT)
                          float *x_sw_demand_ratio,    //  (INPUT)
                          float *y_swdef_leaf,         //  (INPUT)
                          int   num_layer,             //  (INPUT)  number of layers in profile
                          float *dlayer,               //  (INPUT)  thickness of soil layer I (mm)
                          float root_depth,            //  (INPUT)  depth of roots (mm)
                          float sw_demand,            //  (INPUT)  total crop demand for water (mm)
                          float *sw_supply,            //  (INPUT)  potential water to take up (supply) from current soil water (mm)
                          float *swdef);                //  (OUTPUT) sw stress factor (0-1)

void crop_swdef_photo(int   num_layer,    //(INPUT)  number of layers in profile
                      float *dlayer,      //(INPUT)  thickness of soil layer I (mm)
                      float root_depth,   //(INPUT)  depth of roots (mm)
                      float sw_demand,   //(INPUT)  total crop demand for water (mm)
                      float *sw_supply,   //(INPUT)  potential water to take up (supply) from current soil water (mm)
                      float *swdef);       //(OUTPUT) sw stress factor (0-1)

void crop_swdef_pheno(int    num_sw_avail_ratio,        // (INPUT)
                      float *x_sw_avail_ratio,          // (INPUT)
                      float *y_swdef_pheno,             // (INPUT)
                      int    num_layer,                 // (INPUT)  number of layers in profile
                      float *dlayer,                    // (INPUT)  thickness of soil layer I (mm)
                      float  root_depth,                // (INPUT)  depth of roots (mm)
                      float *sw_avail,                  // (INPUT)  actual extractable soil water (mm)
                      float *sw_avail_pot,              // (INPUT)  potential extractable soil water (mm)
                      float *swdef);                     // (OUTPUT) sw stress factor (0-1)

void crop_swdef_fixation(int  num_sw_avail_fix,      // (INPUT)
                         float *x_sw_avail_fix,      // (INPUT)
                         float *y_swdef_fix,         // (INPUT)
                         int   num_layer,       // (INPUT)  number of layers in profile
                         float *dlayer,              // (INPUT)  thickness of soil layer I (mm)
                         float  root_depth,          // (INPUT)  depth of roots (mm)
                         float *sw_avail,            // (INPUT)  actual extractable soil water (mm)
                         float *sw_avail_pot,        // (INPUT)  potential extractable soil water (mm)
                         float *swdef);               // (OUTPUT) sw stress factor (0-1)

void crop_oxdef_photo1(int   C_num_oxdef_photo,    //  (INPUT)
                       float *C_oxdef_photo,      //  (INPUT)
                       float *C_oxdef_photo_rtfr, //  (INPUT)
                       float *G_ll15_dep,         //  (INPUT)
                       float *G_sat_dep,          //  (INPUT)
                       float *G_sw_dep,           //  (INPUT)  soil water content of layer L
                       float *G_dlayer,           //  (INPUT)  thickness of soil layer I (mm)
                       float *G_root_length,      //  (INPUT)
                       float G_root_depth,        //  (INPUT)  depth of roots (mm)
                       int   max_layer,           //  (INPUT)
                       float *oxdef_photo);        //  (OUTPUT)

void cproc_sw_supply1 (commsInterface *,
                       float C_sw_lb,              //(INPUT)
                       float *G_dlayer,            //(INPUT)
                       float *P_ll_dep,            //(INPUT)
                       float *G_dul_dep,           //(INPUT)
                       float *G_sw_dep,            //(INPUT)
                       int   max_layer,            //(INPUT)
                       float G_root_depth,        //(INPUT)
                       float *P_kl,                //(INPUT)
                       float *G_sw_avail,          //(OUTPUT)
                       float *G_sw_avail_pot,      //(OUTPUT)
                       float *G_sw_supply);         //(OUTPUT)

void crop_sw_avail_pot(int   num_layer,       //   (INPUT)  number of layers in profile
                       float *dlayer,         //   (INPUT)  thickness of soil layer I (mm)
                       float *dul_dep,        //   (INPUT)  drained upper limit soil water content for soil layer L (mm water)
                       float root_depth,      //   (INPUT)  depth of roots (mm)
                       float *ll_dep,         //   (INPUT)  lower limit of plant-extractable soil water for soil layer L (mm)
                       float *sw_avail_pot);   //   (OUTPUT) crop water potential uptake for each full layer (mm)

void cproc_sw_uptake1(int   num_layer,        //  (INPUT)  number of layers in profile
                      float *dlayer,          //  (INPUT)  thickness of soil layer I (mm)
                      float root_depth,       //  (INPUT)  depth of roots (mm)
                      float sw_demand,       //  (INPUT)  total crop demand for water (mm)
                      float *sw_supply,       //  (INPUT)  potential water to take up (supply)
                      float *dlt_sw_dep);      //  (OUTPUT) root water uptake (mm)

void cproc_sw_demand1(float dlt_dm_pot_rue,      //(INPUT)  potential dry matter production with opt
                      float transp_eff,          //(INPUT)  transpiration efficiency (g dm/m^2/mm wa
                      float *sw_demand);          //(OUTPUT) crop water demand (mm)

void cproc_sw_demand_bound (float sw_demand_unbounded,  //(INPUT)  Unbounded sw demand (mm)
                            float eo_crop_factor,       //(INPUT) crop factor for eo   (-)
                            float eo,                   //(INPUT) eo                  (mm)
                            float cover_green,          //(INPUT) green crop cover    (-)
                            float *sw_demand_bounded);    //(OUTPUT) bounded sw demand (mm)

float svp(float temp); //(INPUT)  fraction of distance between svp at mi

void cproc_transp_eff1(float svp_fract,          // (INPUT)  fraction of distance between svp at mi
                       float transp_eff_cf,     //  (INPUT)  transpiration efficiency coefficien
                       float maxt,               // (INPUT)  maximum air temperature (oC)
                       float mint,               // (INPUT)  minimum air temperature (oC)
                       float *transp_eff);       //   (OUTPUT)

void cproc_bio_water1(int   num_layer,      //(INPUT)  number of layers in profile
                      float *dlayer,        //(INPUT)  thickness of soil layer I (mm)
                      float root_depth,    //(INPUT)  depth of roots (mm)
                      float *sw_supply,     //(INPUT)  potential water to take up (supply)
                      float transp_eff,    //(INPUT)  transpiration efficiency (g dm/m^2/m
                      float *dlt_dm_pot_te); //(OUTPUT) potential dry matter production
                                            //         by transpiration (g/m^2)

void crop_check_sw(commsInterface *,
                   float minsw,    // (INPUT)  lowest acceptable value for ll
                   float *dlayer,   // (INPUT)  thickness of soil layer I (mm)
                   float *dul_dep,  // (INPUT)  drained upper limit soil water content for soil layer L (mm water)
                   int   max_layer,// (INPUT)  number of layers in profile ()
                   float *sw_dep,   // (INPUT)  soil water content of layer L (mm)
                   float *ll_dep);   // (INPUT)  lower limit of plant-extractable soil water
                                    //          for soil layer L (mm)

void cproc_transp_eff_co2(float svp_fract,        // (INPUT)  fraction of distance between svp at mi
                          float transp_eff_cf,    // (INPUT)  transpiration efficiency coefficien
                          float maxt,             // (INPUT)  maximum air temperature (oC)
                          float mint,             // (INPUT)  minimum air temperature (oC)
                          float co2level,         // (INPUT)  current co2 level (ppm)
                          float *co2_level_te,     // (INPUT)  co2 levels (ppm)
                          float *te_co2_modifier,  // (INPUT)  te modifiers of co2 levels (0-1)
                          int   num_co2_level_te,   // (INPUT)  number of table elements in co2-te modifier table
                          float *transp_eff);       // (OUTPUT) transpiration coefficient

double divide (double dividend, double divisor, double default_value);
float l_bound (float var, float lower);
float u_bound (float var, float upper);

void bound_check_real_array (commsInterface *,
                             float *array,// (INPUT) array to be checked
                             int    array_size,    // (INPUT) array size_of
                             float  lower_bound,// (INPUT) lower bound of values
                             float  upper_bound,// (INPUT) upper bound of values
                             const char *array_name);// (INPUT) key string of array
void bound_check_integer_array (commsInterface *,
                             int *array,// (INPUT) array to be checked
                             int    array_size,    // (INPUT) array size_of
                             int  lower_bound,// (INPUT) lower bound of values
                             int  upper_bound,// (INPUT) upper bound of values
                             const char *array_name);// (INPUT) key string of array

void bound_check_real_var (commsInterface *,float value, float lower, float upper, const char *vname);
void bound_check_integer_var (commsInterface *, int value, int lower, int upper, const char *vname);

float bound(float var, float lower, float upper);

float error_margin(const float *value);

void fill_real_array (float *var, float value, int limit);
void fill_integer_array (int *var, int value, int limit);

int get_cumulative_index_real(float cum_sum, const float *array, int size_of);

float sum_real_array (float *var, int nelem);
float sum_integer_array (int *var, int limit);
int position_in_real_array(float Number,      //(INPUT) Number to search for
                           float *Array,      //(INPUT) Array to search
                           int Array_size);    //(INPUT) Number of elements in array

//int position_in_char_array(char *Value, char *Array, int *Array_size);

float sum_between (int start, int finish, const float *array);
float sum_part_of_real(float *array,     // array to be summed
                       int start,        // index for starting element
                       int stop,         // index for stopping element
                       int size_of);      //  size of array

int count_of_real_vals (float *array, int limit);


int offset_day_of_year (int iyr,     //(INPUT) day of year number
                        int doy,     //(INPUT) year
                        int ndays);   //(INPUT) number of days to adjust by
bool leap_year(int *year);
bool check_date (int day, int month, int year);
double date_to_jday (int day, int month, int year);
void   jday_to_date (int *day, int *month, int *year, double jday);

//---------------------------------------------------------------------------

float linear_interp_real (float x, float *x_cord, float *y_cord, int num_cord);

bool stage_is_between (int start, int finish, float current_stage);

int find_layer_no(float depth, float *dlayr, int num_layers);

bool on_day_of (int stage_no, float current_stage);

float root_proportion (int    layer,              // (INPUT) layer to look at
                       float *dlayr,              // (INPUT) array of layer depths
                       float  root_depth);         // (INPUT) depth of roots

float stage_no_of (float stage_code,           //(INPUT) stage code to look up
                   float *stage_code_list,     //(INPUT) list of stage codes
                   int   list_size);            //(INPUT) size_of of stage code list

void accumulate (float value,             //  (INPUT) value to add to array
                 float *array,            //  (INPUT/OUTPUT) array to split
                 float p_index,           //  (INPUT) current p_index no
                 float dlt_index);         //  (INPUT) increment in p_index no

void subtract_real_array (const float *amount, float *store, int count);

void add_real_array (const float *amount, float *store, int count);

void crop_radn_int0(float cover_green,
                    float fr_intc_radn,
                    float radn,
                    float *radn_int);

float day_length (int day_of_year, float latitude, float twilight);

void jday_to_day_of_year(double*, int*, int*);

inline bool leap_year(int year)
   {return leap_year(&year);}

inline bool reals_are_equal(float A, float B, float C) {return(fabs(A-B)<C);}
inline bool isEqual(float A, float B, float C) {return(fabs(A-B)<C);}

inline bool reals_are_equal(float A, float B) {return(fabs(A-B)<1.0E-6);}
inline bool isEqual(float A, float B) {return(fabs(A-B)<1.0E-6);}


std::string ftoa(double Float, char *fmtwidth=".2");
std::string itoa(int value, int width);

inline bool char2any(const char *str, int &value) {
   return (sscanf(str, "%d", &value) == 1);
}
inline bool char2any(const char *str, float &value) {
   return (sscanf(str, "%f", &value) == 1);
}
inline std::string any2string(float value) {
   return(ftoa(value, ".2"));
}
inline std::string any2string(int value) {
   return(itoa(value, 5));
}

#endif

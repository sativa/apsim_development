// status:
// 13/11/2003
// Merged neils changes from plant_main.for:1.6  OK
//                           plant.for      1.4  OK
//                           plant_opt.for  1.2  OK
//  7/2/2004
// Merged John's fruit cohorts from plant_main.for:1.17  OK
//                                  plant.for      1.15  OK
//                                  plant_opt.for  1.10  OK

#include <stdio.h>
#include <math.h>
#include <map>
#include <string>
#include <stdexcept>
#include <strstream>

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
#include "Plant.h"
#include "PlantP.h"

using namespace std;

Plant *currentInstance = NULL;

static const char* nullType =         "<type\>";
static const char* integerType =      "<type kind=\"integer4\"/>";
static const char* integerArrayType = "<type kind=\"integer4\" array=\"T\"/>";
static const char* floatType =        "<type kind=\"single\"/>";
static const char* floatArrayType =   "<type kind=\"single\" array=\"T\"/>";
static const char* doubleType =       "<type kind=\"double\"/>";
static const char* doubleArrayType =  "<type kind=\"double\" array=\"T\"/>";
static const char* stringType =       "<type kind=\"string\"/>";
static const char* stringArrayType =  "<type kind=\"string\" array=\"T\"/>";
static const char* logicalType =      "<type kind=\"boolean\"/>";



/////////////These might be redundancies??//////////
void push_routine (const char *) {};
void pop_routine (const char *) {};

/////////////////////////
Plant::Plant(PlantComponent *P)
    {
      c.x_dm_sen_frac = new float* [max_part];
      c.y_dm_sen_frac = new float* [max_part];
      c.num_dm_sen_frac = new int [max_part];
      for (int part = 0; part < max_part; part++)
         {
         c.x_dm_sen_frac[part] = new float [max_table];
         c.y_dm_sen_frac[part] = new float [max_table];
         }
      g.dlt_dm_fruit_green = new float* [max_fruit_cohorts];
      g.dlt_dm_fruit_senesced = new float* [max_fruit_cohorts];
      g.dm_fruit_green = new float* [max_fruit_cohorts];
      g.fruit_days_tot = new float* [max_fruit_cohorts];
      g.fruit_phase_tt = new float* [max_fruit_cohorts];
      g.fruit_tt_tot = new float* [max_fruit_cohorts];
      g.dm_fruit_dead = new float* [max_fruit_cohorts];
      g.dm_fruit_senesced = new float* [max_fruit_cohorts];
      g.dlt_dm_fruit_green_retrans = new float* [max_fruit_cohorts];
      g.dlt_dm_fruit_abort = new float* [max_fruit_cohorts];
      g.fruit_sdr_daily = new float* [max_fruit_cohorts];

      for (int cohort = 0; cohort < max_fruit_cohorts; cohort++)
         {
         g.dlt_dm_fruit_green[cohort] = new float [max_part];
         g.dlt_dm_fruit_senesced[cohort] = new float [max_part];
         g.dm_fruit_green[cohort] = new float [max_part];
         g.fruit_days_tot[cohort]= new float [max_fruit_stage];
         g.fruit_phase_tt[cohort]= new float [max_fruit_stage];
         g.fruit_tt_tot[cohort]= new float [max_fruit_stage];
         g.dm_fruit_dead[cohort]= new float [max_part];
         g.dm_fruit_senesced[cohort]= new float [max_part];
         g.dlt_dm_fruit_green_retrans[cohort]= new float [max_part];
         g.dlt_dm_fruit_abort[cohort]= new float [max_part];
         g.fruit_sdr_daily[cohort]=new float [366];
         }
      phosphorus = new PlantP(this);
      plant_zero_all_globals();
      parent = P;
    }
  Plant::~Plant()
    {
    for (int part = 0; part < max_part; part++)
         {
         delete [] c.x_dm_sen_frac[part];
         delete [] c.y_dm_sen_frac[part];
         }
    delete [] c.x_dm_sen_frac; delete [] c.y_dm_sen_frac; delete [] c.num_dm_sen_frac;

    for (int cohort = 0; cohort < max_fruit_cohorts; cohort++)
       {
       delete [] g.dlt_dm_fruit_green[cohort];
       delete [] g.dlt_dm_fruit_senesced[cohort];
       delete [] g.dm_fruit_green[cohort];
       delete [] g.fruit_days_tot[cohort];
       delete [] g.fruit_phase_tt[cohort];
       delete [] g.fruit_tt_tot[cohort];
       delete [] g.dm_fruit_dead[cohort];
       delete [] g.dm_fruit_senesced[cohort];
       delete [] g.dlt_dm_fruit_green_retrans[cohort];
       delete [] g.dlt_dm_fruit_abort[cohort];
       delete [] g.fruit_sdr_daily[cohort];
    }
    delete [] g.dlt_dm_fruit_green;
    delete [] g.dlt_dm_fruit_senesced;
    delete [] g.dm_fruit_green;
    delete [] g.fruit_days_tot;
    delete [] g.fruit_phase_tt;
    delete [] g.fruit_tt_tot;
    delete [] g.dm_fruit_dead;
    delete [] g.dm_fruit_senesced;
    delete [] g.dlt_dm_fruit_green_retrans;
    delete [] g.dlt_dm_fruit_abort;
    delete [] g.fruit_sdr_daily;
    delete phosphorus;
    }

void Plant::initialise(void)
  {
  doIDs();                 // Gather IDs for getVariable requests
  plant_read_constants (); // Read constants
  plant_zero_variables (); // Zero global states
  plant_init ();           // Site specific init
  plant_get_other_variables (); // sw etc..
  }
void Plant::doIDs(void)
   {
       // gets
       id.eo = parent->addRegistration(RegistrationType::get,
                                       "eo", floatType,
                                       "", "");
       id.sw_dep= parent->addRegistration(RegistrationType::get,
                                       "sw_dep", floatArrayType,
                                       "", "");
       id.no3 = parent->addRegistration(RegistrationType::get,
                                       "no3", floatArrayType,
                                       "", "");
       id.no3_min= parent->addRegistration(RegistrationType::get,
                                       "no3_min", floatArrayType,
                                       "", "");
       id.nh4 = parent->addRegistration(RegistrationType::get,
                                       "nh4", floatArrayType,
                                       "", "");
       id.nh4_min= parent->addRegistration(RegistrationType::get,
                                       "nh4_min", floatArrayType,
                                       "", "");
       id.latitude = parent->addRegistration(RegistrationType::get,
                                       "latitude", floatType,
                                       "", "");
       id.parasite_c_demand = parent->addRegistration(RegistrationType::get,
                                       "parasite_dm_demand", floatType,
                                       "", "");
       id.parasite_sw_demand = parent->addRegistration(RegistrationType::get,
                                       "parasite_sw_demand", floatType,
                                       "", "");
       id.maxt_soil_surface = parent->addRegistration(RegistrationType::get,
                                       "maxt_soil_surface", floatType,
                                       "", "");
       id.co2 = parent->addRegistration(RegistrationType::get,
                                        "co2", floatType,
                                        "", "");

       string canopyName = string("fr_intc_radn_") + string(parent->getName());
       id.fr_intc_radn = parent->addRegistration(RegistrationType::get,
                                       canopyName.c_str(),
                                       floatType,
                                       "", "");
       // sets
       id.dlt_no3 = parent->addRegistration(RegistrationType::set,
                                       "dlt_no3", floatArrayType,
                                       "", "");
       id.dlt_nh4 = parent->addRegistration(RegistrationType::set,
                                       "dlt_nh4", floatArrayType,
                                       "", "");
       id.dlt_sw_dep = parent->addRegistration(RegistrationType::set,
                                       "dlt_sw_dep", floatArrayType,
                                       "", "");

       // events.
       id.crop_chopped = parent->addRegistration(RegistrationType::event,
                                       "crop_chopped", "",
                                       "", "");
       id.incorp_fom = parent->addRegistration(RegistrationType::event,
                                       "incorp_fom", "",
                                       "", "");

       phosphorus->doIDs(parent);
  }

// Register Methods, Events,
void Plant::doRegistrations(void)
   {
   // Events
#define setupEvent(name,type,address) {\
   boost::function3<void, unsigned &, unsigned &, protocol::Variant &> fn;\
   fn = boost::bind(address, this, _1, _2, _3); \
   parent->addEvent(name, type, fn);\
   }

   setupEvent("prepare",     RegistrationType::respondToEvent, &Plant::doPrepare);
   setupEvent("process",     RegistrationType::respondToEvent, &Plant::doProcess);
   setupEvent("tick",        RegistrationType::respondToEvent, &Plant::doTick);
   setupEvent("newmet",      RegistrationType::respondToEvent, &Plant::doNewMet);
   setupEvent("new_profile", RegistrationType::respondToEvent, &Plant::doNewProfile);
   setupEvent("sow",         RegistrationType::respondToEvent, &Plant::doSow);
   setupEvent("harvest",     RegistrationType::respondToEvent, &Plant::doHarvest);
   setupEvent("end_crop",    RegistrationType::respondToEvent, &Plant::doEndCrop);
   setupEvent("kill_crop",   RegistrationType::respondToEvent, &Plant::doKillCrop);
   setupEvent("end_run",     RegistrationType::respondToEvent, &Plant::doEndRun);
   setupEvent("kill_stem",   RegistrationType::respondToEvent, &Plant::doKillStem);
   setupEvent("remove_crop_biomass",   RegistrationType::respondToEvent, &Plant::doRemoveCropBiomass);
#undef setupEvent

   // Send My Variable
#define setupGetVar parent->addGettableVar
#define setupGetFunction(name,type,length,address,units,desc) {\
   boost::function2<void, protocol::Component *, protocol::QueryValueData &> fn;\
   fn = boost::bind(address, this, _1, _2); \
   parent->addGettableVar(name, type, length, fn, units, desc);\
   }

   setupGetFunction("plant_status", protocol::DTstring, false,
                     &Plant::get_plant_status, "", "Plant Status");

   setupGetVar("dlt_stage",
               g.dlt_stage, "", "Change in plant stage");

   setupGetVar("stage",
               g.current_stage,"", "Plant stage");

   setupGetFunction("stage_code", protocol::DTsingle, false,
                    &Plant::get_stage_code, "", "Plant stage code");

   setupGetFunction("stage_name", protocol::DTstring, false,
                    &Plant::get_stage_name, "", "Plant stage name");

// XXXX UGLY HACK workaround for broken coordinator in 3.4
//   id = parent->addGettableVar("crop_type", protocol::DTstring, false, "");
//   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_crop_type));

   setupGetVar("crop_class",
               g.crop_class, "", "Plant crop class");

   setupGetVar("dlt_tt",
               g.dlt_tt, "dd", "Change in cumulative thermal time");

   setupGetVar("phase_tt", max_stage,
               g.phase_tt, "dd", "Thermal time spent in each crop phase");

   setupGetVar("tt_tot", max_stage,
               g.tt_tot, "dd", "Thermal time spent in each crop stage");

   setupGetVar("days_tot", max_stage,
               g.days_tot, "days", "Days spent in each crop stage");

   setupGetFunction("das", protocol::DTint4, false,
                    &Plant::get_das, "days", "Days after sowing");

   setupGetVar("cum_vernal_days",
               g.cum_vernal_days, "vd", "Days in vernalisation");

   setupGetVar("flowering_date",
               g.flowering_date, "doy", "Day of flowering");

   setupGetVar("maturity_date",
               g.maturity_date, "doy", "Day of maturity");

   setupGetVar("flowering_das",
               g.flowering_das, "days", "Days from sowing to flowering");

   setupGetVar("maturity_das",
               g.maturity_das, "days", "Days from sowing to maturity");

   setupGetFunction("leaf_no", protocol::DTsingle, false,
                    &Plant::get_leaf_no, "leaves/m2", "number of leaves per square meter");

   setupGetFunction("node_no", protocol::DTsingle, false,
                    &Plant::get_node_no, "nodes/m2", "number of nodes per square meter");

   setupGetVar("dlt_leaf_no",
               g.dlt_leaf_no, "leaves/m2", "Change in number of leaves");

   setupGetVar("dlt_node_no",
               g.dlt_node_no, "nodes/m2", "Change in number of nodes");

   setupGetFunction("leaf_no_dead", protocol::DTsingle, false,
                     &Plant::get_leaf_no_dead, "leaves/m2", "number of dead leaves per square meter");

   setupGetFunction("leaf_area", protocol::DTsingle, true,
                    &Plant::get_leaf_area, "mm^2", "Leaf area for each node");

//   id = parent->addGettableVar("height", protocol::DTsingle, false, "mm");
//   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_height));

   setupGetVar("width",
               g.canopy_width, "mm", "canopy row width");

   setupGetVar("root_depth",
               g.root_depth, "mm", "depth of roots");

   setupGetVar("plants",
               g.plants, "plants/m^2", "Plant desnity");

//   id = parent->addGettableVar("cover_green", protocol::DTsingle, false, "");
//   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_cover_green));

//   id = parent->addGettableVar("cover_tot", protocol::DTsingle, false, "");
//   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_cover_tot));

   setupGetFunction("lai_sum", protocol::DTsingle, false,
                    &Plant::get_lai_sum, "m^2/m^2", "LAI of all leaf parts");

   setupGetFunction("tlai", protocol::DTsingle, false,
                    &Plant::get_tlai, "m^2/m^2", "tlai");

   setupGetVar("slai",
               g.slai, "m^2/m^2", "Senesced lai");

   setupGetVar("lai",
               g.lai, "m^2/m^2", "Leaf area index");

   setupGetVar("lai_canopy_green",
               g.lai_canopy_green, "m^2/m^2", "Green lai");

   setupGetVar("tlai_dead",
               g.tlai_dead, "m^2/m^2", "tlai dead");

   setupGetVar("pai",
               g.pai, "m^2/m^2", "Pod area index");

   setupGetVar("dlt_slai_age",
               g.dlt_slai_age, "m^2/m^2", "Change in lai via age");

   setupGetVar("dlt_slai_light",
               g.dlt_slai_light, "m^2/m^2", "Change in lai via light");

   setupGetVar("dlt_slai_water",
               g.dlt_slai_water, "m^2/m^2", "Change in lai via water stress");

   setupGetVar("dlt_slai_frost",
               g.dlt_slai_frost, "m^2/m^2", "Change in lai via low temperature");

   setupGetVar("grain_no",
               g.grain_no, "/m^2", "Grain number");

   setupGetFunction("grain_size", protocol::DTsingle, false,
                    &Plant::get_grain_size, "g", "Size of each grain");

   setupGetVar("root_wt",
               g.dm_green[root], "g/m^2", "Weight of roots");

   setupGetVar("leaf_wt",
               g.dm_green[leaf], "g/m^2", "Weight of leaf");

   setupGetVar("stem_wt",
               g.dm_green[stem], "g/m^2", "Weight of stems");

   setupGetFunction("head_wt", protocol::DTsingle, false,
                    &Plant::get_head_wt, "g/m^2", "Weight of heads");

   setupGetVar("pod_wt",
               g.dm_green[pod],"g/m^2", "Weight of pods");

   setupGetFunction("grain_wt", protocol::DTsingle, false,
                    &Plant::get_grain_wt, "g/m^2", "Weight of grain");

   setupGetVar("meal_wt",
               g.dm_green[meal], "g/m^2", "Weight of meal");

   setupGetVar("oil_wt",
               g.dm_green[oil], "g/m^2", "Weight of oil");

   setupGetVar("dm_green", max_part,
               g.dm_green, "g/m^2", "Weight of green material");

   setupGetVar("dm_senesced", max_part,
               g.dm_senesced, "g/m^2", "Weight of senesced material");

   setupGetVar("dm_dead", max_part,
                g.dm_dead, "g/m^2","Weight of dead material");

   setupGetFunction("yield", protocol::DTsingle, false,
                    &Plant::get_yield,  "kg/ha", "Yield");

   setupGetFunction("biomass", protocol::DTsingle, false,
                    &Plant::get_biomass, "kg/ha", "Biomass");

   setupGetFunction("biomass_wt", protocol::DTsingle, false,
                    &Plant::get_biomass_wt, "g/m^2", "Biomass weight");

   setupGetFunction("green_biomass", protocol::DTsingle, false,
                    &Plant::get_green_biomass, "kg/ha", "Green Biomass weight");

   setupGetFunction("green_biomass_wt", protocol::DTsingle, false,
                    &Plant::get_green_biomass_wt, "g/m^2", "Green Biomass weight");

   setupGetVar("dlt_dm",
               g.dlt_dm, "g/m^2", "Change in dry matter");

   setupGetVar("dlt_dm_pot_rue",
               g.dlt_dm_pot_rue, "g/m^2", "Potential dry matter production via photosynthesis");

   setupGetVar("dlt_dm_pot_te",
               g.dlt_dm_pot_te, "g/m^2", "Potential dry matter production via transpiration");

   setupGetVar("dlt_dm_grain_demand",
               g.dlt_dm_grain_demand, "g/m^2", "??");

   setupGetVar("dlt_dm_green", max_part,
               g.dlt_dm_green,  "g/m^2", "change in green pool");

   setupGetVar("dlt_dm_green_retrans", max_part,
               g.dlt_dm_green_retrans, "g/m^2", "change in green pool from retranslocation");

   setupGetVar("dlt_dm_detached", max_part,
               g.dlt_dm_detached,  "g/m^2", "change in dry matter via detachment");

   setupGetVar("dlt_dm_senesced", max_part,
               g.dlt_dm_senesced,"g/m^2", "change in dry matter via senescence");

   setupGetVar("dlt_dm_dead_detached", max_part,
               g.dlt_dm_dead_detached,  "g/m^2", "change in dry matter via detachment");

   setupGetVar("grain_oil_conc",
               c.grain_oil_conc, "%", "??");

   setupGetVar("dlt_dm_oil_conv",
               g.dlt_dm_oil_conv,"g/m^2", "change in oil via ??");

   setupGetVar("dlt_dm_oil_conv_retrans",
               g.dlt_dm_oil_conv_retranslocate, "g/m^2", "change in oil via retranslocation");

   setupGetFunction("biomass_n", protocol::DTsingle, false,
                    &Plant::get_biomass_n,  "g/m^2", "N in total biomass");

   setupGetFunction("n_uptake", protocol::DTsingle, false,
                    &Plant::get_n_uptake, "g/m^2", "N uptake");

   setupGetFunction("green_biomass_n", protocol::DTsingle, false,
                    &Plant::get_green_biomass_n, "g/m^2", "N in green biomass");

   setupGetFunction("grain_n", protocol::DTsingle, false,
                    &Plant::get_grain_n, "g/m^2", "N in grain");

   setupGetVar("leaf_n",
               g.n_green[leaf],"g/m^2", "N in leaves");

   setupGetVar("stem_n",
               g.n_green[stem], "g/m^2", "N in stems");

   setupGetVar("root_n",
               g.n_green[root], "g/m^2", "N in roots");

   setupGetVar("pod_n",
               g.n_green[pod], "g/m^2", "N in pods");

   setupGetVar("deadleaf_n",
               g.n_senesced[leaf], "g/m^2", "N in dead leaves");

   setupGetFunction("head_n", protocol::DTsingle, false,
                    &Plant::get_head_n, "g/m^2", "N in heads");

   setupGetVar("n_green", max_part,
               g.n_green,  "g/m^2", "N in green");

   setupGetVar("n_senesced", max_part,
               g.n_senesced,  "g/m^2", "N in senesced");

   setupGetVar("n_dead", max_part,
               g.n_dead,"g/m^2", "N in dead");

   setupGetVar("dlt_n_green", max_part,
               g.dlt_n_green, "g/m^2", "N in delta green");

   setupGetVar("dlt_n_dead", max_part,
               g.dlt_n_dead, "g/m^2", "N in delta dead");

   setupGetVar("dlt_n_retrans", max_part,
               g.dlt_n_retrans, "g/m^2", "N in retranslocate");

   setupGetVar("dlt_n_senesced_trans", max_part,
               g.dlt_n_senesced_trans, "g/m^2", "N in translocate");

   setupGetVar("dlt_n_senesced_retrans", max_part,
               g.dlt_n_senesced_retrans, "g/m^2", "N in retranslocate");

   setupGetVar("dlt_n_senesced", max_part,
               g.dlt_n_senesced, "g/m^2", "N in delta senesced");

   setupGetVar("dlt_n_detached", max_part,
               g.dlt_n_detached, "g/m^2", "N in detached");

   setupGetVar("dlt_n_dead_detached", max_part,
               g.dlt_n_dead_detached,  "g/m^2", "N in dead detached");

   setupGetVar("temp_stress_photo",
               g.temp_stress_photo, "", "Temperature Stress in photosynthesis");

   setupGetVar("swdef_pheno",
               g.swdef_pheno, "", "Soil water deficit in phenological development");

   setupGetVar("swdef_photo",
               g.swdef_photo, "", "Soil water deficit in photosynthesis");

   setupGetVar("swdef_expan",
               g.swdef_expansion, "", "Soil water deficit in leaf expansion");

   setupGetVar("swdef_fixation",
               g.swdef_fixation, "", "Soil water deficit in N fixation");

   setupGetVar("oxdef_photo",
               g.oxdef_photo, "", "Oxygen deficit in photosynthesis");

   setupGetFunction("sw_stress_pheno", protocol::DTsingle, false,
                    &Plant::get_swstress_pheno,
                          "","Soil water stress for phenological development");

   setupGetFunction("sw_stress_photo", protocol::DTsingle, false,
                    &Plant::get_swstress_photo,
                    "","Soil water stress for photosynthesis");

   setupGetFunction("sw_stress_expan", protocol::DTsingle, false,
                    &Plant::get_swstress_expan,
                    "","Soil water stress for leaf expansion");

   setupGetFunction("sw_stress_fixation", protocol::DTsingle, false,
                    &Plant::get_swstress_fixation,
                    "","Soil water stress for N fixation");

   setupGetVar("transp_eff",
               g.transp_eff, "g/m2/mm", "Transpiration Efficiency");

   setupGetFunction("ep", protocol::DTsingle, false,
                    &Plant::get_ep, "mm", "Plant water uptake");

   setupGetFunction("sw_uptake", protocol::DTsingle, true,
                    &Plant::get_sw_uptake, "mm", "Plant water uptake per layer");

   setupGetFunction("cep", protocol::DTsingle, false,
                    &Plant::get_cep,
                    "mm", "Cumulative plant water uptake");

   setupGetFunction("sw_supply", protocol::DTsingle, false,
                    &Plant::get_sw_supply, "mm", "Soil water supply");


   setupGetFunction("sw_supply_layr", protocol::DTsingle, true,
                    &Plant::get_sw_supply_layr, "mm", "Soil water supply");

   setupGetFunction("esw_layr", protocol::DTsingle, true,
                    &Plant::get_esw_layr, "mm", "Extractable soil water");

   setupGetFunction("n_conc_stover", protocol::DTsingle, false,
                    &Plant::get_n_conc_stover, "%", "N concentration in stover");

   setupGetFunction("n_conc_root", protocol::DTsingle, false,
                    &Plant::get_n_conc_root, "%", "N concentration in root");

   setupGetFunction("n_conc_leaf", protocol::DTsingle, false,
                    &Plant::get_n_conc_leaf, "%", "N concentration in leaf");

   setupGetFunction("n_conc_stem", protocol::DTsingle, false,
                    &Plant::get_n_conc_stem, "%", "N concentration in stem");

   setupGetFunction("n_conc_grain", protocol::DTsingle, false,
                    &Plant::get_n_conc_grain, "%", "N concentration in grain");
   setupGetFunction("n_grain_pcnt", protocol::DTsingle, false,
                    &Plant::get_n_conc_grain,
                    "%", "N concentration in grain");

   setupGetFunction("grain_protein", protocol::DTsingle, false,
                    &Plant::get_grain_protein,
                    "%", "grain protein content");

   setupGetFunction("n_conc_meal", protocol::DTsingle, false,
                    &Plant::get_n_conc_meal, "%", "meal N content");

   setupGetFunction("n_conc_crit", protocol::DTsingle, false,
                    &Plant::get_n_conc_crit, "%", "critical N content");

   setupGetFunction("n_conc_min", protocol::DTsingle, false,
                    &Plant::get_n_conc_min, "%", "minimum N content");

   setupGetFunction("n_conc_crit_leaf", protocol::DTsingle, false,
                    &Plant::get_n_conc_crit_leaf, "%", "critical N content in leaves");

   setupGetFunction("n_conc_min_leaf", protocol::DTsingle, false,
                    &Plant::get_n_conc_min_leaf, "%", "mininmum N content in leaves");

   setupGetFunction("n_conc_crit_stem", protocol::DTsingle, false,
                    &Plant::get_n_conc_crit_stem,
                    "%", "critical N content in stem");

   setupGetFunction("n_conc_min_stem", protocol::DTsingle, false,
                    &Plant::get_n_conc_min_stem,
                    "%", "mininmum N content in stem");

   setupGetFunction("n_uptake_stover", protocol::DTsingle, false,
                    &Plant::get_n_uptake_stover,
                    "g/m^2", "N taken up by agp");

   setupGetFunction("no3_tot", protocol::DTsingle, false,
                    &Plant::get_no3_tot,
                    "g/m^2", "NO3 available to plants");

   setupGetFunction("n_demand", protocol::DTsingle, false,
                    &Plant::get_n_demand,
                    "g/m^2", "N demand");

   setupGetVar("grain_n_demand",
               g.grain_n_demand, "g/m^2", "N demand of grain");

   setupGetFunction("n_supply_soil", protocol::DTsingle, false,
                    &Plant::get_n_supply_soil,
                    "g/m^2", "N supply");

   setupGetVar("dlt_n_fixed_pot",
               g.n_fix_pot, "g/m^2", "potential N fixation");

   setupGetVar("dlt_n_fixed",
               g.n_fix_uptake, "g/m^2", "N fixation");

   setupGetVar("n_fixed_tops",
               g.n_fixed_tops, "g/m^2", "N fixation");

   setupGetVar("nfact_photo",
               g.nfact_photo, "", "N factor for photosynthesis");

   setupGetVar("nfact_pheno",
               g.nfact_pheno, "", "N factor for phenology");

   setupGetVar("nfact_expan",
               g.nfact_expansion, "", "N factor for leaf expansion");

   setupGetVar("nfact_grain",
               g.nfact_grain_conc, "", "N factor for ??");

   setupGetVar("nfact_grain_tot", max_stage,
               g.cnd_grain_conc,
               "", "N factor for ??");

   setupGetFunction("n_stress_photo", protocol::DTsingle, false,
                    &Plant::get_nstress_photo,
                    "","N stress for photosyntesis");

   setupGetFunction("n_stress_pheno", protocol::DTsingle, false,
                    &Plant::get_nstress_pheno,
                    "","N stress for phenology");

   setupGetFunction("n_stress_expan", protocol::DTsingle, false,
                    &Plant::get_nstress_expan,
                    "","N stress for leaf expansion");

   setupGetFunction("n_stress_grain", protocol::DTsingle, false,
                    &Plant::get_nstress_grain,
                    "","N stress for grain filling");

   setupGetFunction("rlv", protocol::DTsingle, true,
                    &Plant::get_rlv, "mm/mm^3", "Root length density");

   setupGetFunction("rld", protocol::DTsingle, true,
                    &Plant::get_rlv, "mm/mm^3", "Root length density");

   setupGetFunction("no3_demand", protocol::DTsingle, false,
                    &Plant::get_no3_demand,
                    "kg/ha", "Demand for NO3");

   setupGetVar("sw_demand",
               g.sw_demand, "mm", "Demand for sw");

   setupGetVar("sw_demand_te",
               g.sw_demand_te, "mm", "Demand for sw");

   setupGetFunction("root_length", protocol::DTsingle, true,
                    &Plant::get_root_length,
                    "mm/mm^2", "Root length");

   setupGetFunction("root_length_dead", protocol::DTsingle, true,
                    &Plant::get_root_length_dead,
                          "mm/mm^2", "Dead root length");

   setupGetFunction("no3gsm_uptake_pot", protocol::DTsingle, true,
                    &Plant::get_no3gsm_uptake_pot,
                    "g/m2", "Pot NO3 uptake");

   setupGetFunction("nh4gsm_uptake_pot", protocol::DTsingle, true,
                    &Plant::get_nh4gsm_uptake_pot,
                    "g/m2", "Pot NH4 uptake");

   setupGetFunction("no3_swfac", protocol::DTsingle, false,
                    &Plant::get_no3_swfac,
                    "", "Work this out...>>");

   setupGetVar("leaves_per_node",
               g.leaves_per_node, "","");

   setupGetFunction("no3_uptake", protocol::DTsingle, false,
                    &Plant::get_no3_uptake,
                    "","NO3 uptake");

   setupGetFunction("nh4_uptake", protocol::DTsingle, false,
                    &Plant::get_nh4_uptake,
                    "","NH4 uptake");

   setupGetFunction("parasite_dm_supply", protocol::DTsingle, false,
                     &Plant::get_parasite_c_gain,
                     "g/m^2", "Assimilate to parasite");

   setupGetFunction("leaf_area_tot", protocol::DTsingle, false,
                    &Plant::get_leaf_area_tot,
                    "m^2", "Total plant leaf area");

   setupGetFunction("dlt_fruit_no", protocol::DTsingle, false,
                    &Plant::get_dlt_fruit_no,
                    "fruit/m2","Change in fruit number");

   setupGetFunction("dlt_fruit_flower_no", protocol::DTsingle, false,
                    &Plant::get_dlt_fruit_flower_no,
                    "flower/m2","Change in flower number");

   setupGetFunction("dlt_fruit_site_no", protocol::DTsingle, false,
                    &Plant::get_dlt_fruit_site_no,
                    "site/m2","New sites");

   setupGetFunction("fruit_site_no", protocol::DTsingle, false,
                    &Plant::get_fruit_site_no,
                    "site/m2","");

   setupGetFunction("fruit_no", protocol::DTsingle, false,
                    &Plant::get_fruit_no,
                    "fruit/m2","");

   setupGetFunction("fruit_no_stage", protocol::DTsingle, true,
                    &Plant::get_fruit_no_stage,
                    "fruit/m2","");

   setupGetFunction("dm_fruit_green_cohort", protocol::DTsingle, false,
                    &Plant::get_dm_fruit_green_cohort,
                    "g/m2","");

   setupGetFunction("dm_fruit_green_part", protocol::DTsingle, true,
                    &Plant::get_dm_fruit_green_part,
                    "g/m2","");

   setupGetFunction("dlt_dm_fruit_demand", protocol::DTsingle, false,
                    &Plant::get_dlt_dm_fruit_demand,
                    "g/m2","");

   setupGetFunction("dlt_dm_fruit_green_part", protocol::DTsingle, true,
                    &Plant::get_dlt_dm_fruit_green_part,
                    "g/m2","");

   setupGetFunction("dm_fruit_green_stage", protocol::DTsingle, true,
                    &Plant::get_dm_fruit_green_stage,
                    "g/m2","");

   setupGetFunction("dlt_dm_fruit_green_retrans_part", protocol::DTsingle, true,
                    &Plant::get_dlt_dm_fruit_green_retrans_part,
                    "g/m2","");

   setupGetVar("num_fruit_cohorts",
               g.num_fruit_cohorts, "cohorts","");

   setupGetVar("dm_parasite_retranslocate",
              g.dm_parasite_retranslocate, "g/m2","");

   setupGetFunction("count_fruit_cohorts", protocol::DTint4, false,
                    &Plant::get_count_fruit_cohorts,
                    "cohorts","");

   setupGetFunction("dlt_dm_fruit_abort_cohort", protocol::DTsingle, false,
                    &Plant::get_dlt_dm_fruit_abort_cohort,
                    "g/m2","");

   setupGetFunction("dlt_dm_fruit_abort", protocol::DTsingle, true,
                    &Plant::get_dlt_dm_fruit_abort,
                    "g/m2","");

   setupGetFunction("dlt_dm_fruit_abort_part", protocol::DTsingle, true,
                    &Plant::get_dlt_dm_fruit_abort_part,
                    "g/m2","");

   setupGetFunction("dlt_fruit_no_abort", protocol::DTsingle, false,
                    &Plant::get_dlt_fruit_no_abort,
                    "fruit/m2", "");

   setupGetFunction("zadok_stage", protocol::DTsingle, false,
                    &Plant::get_zadok_stage,
                    "0-100", "Zadok's growth developmental stage");

#undef setupGetVar
#undef setupGetFunction

   unsigned int id;
   // Set My Variable
   id = parent->addRegistration(RegistrationType::respondToSet, "crop_class",           stringType);
   IDtoSetFn.insert(UInt2SetFnMap::value_type(id,&Plant::set_plant_crop_class));

   id = parent->addRegistration(RegistrationType::respondToSet, "grain_oil_conc",       floatType);
   IDtoSetFn.insert(UInt2SetFnMap::value_type(id,&Plant::set_plant_grain_oil_conc));

   phosphorus->doRegistrations(parent);
   }


void Plant::onApsimGetQuery(protocol::ApsimGetQueryData& apsimQueryData)
{
   boost::function2<void, protocol::Component *, protocol::QueryValueData &> fn;

   // XXXX UGLY HACK workaround for broken coordinator in 3.4 Get rid of this ASAP
   string name = string(apsimQueryData.name.f_str(),apsimQueryData.name.length());
   if (name == string("crop_type")) {
      fn = boost::bind(&Plant::get_crop_type, this, _1, _2);
      parent->addGettableVar("crop_type", protocol::DTstring, false,
                             fn, "",  "");
   } else if (name == string("cover_green")) {
      fn = boost::bind(&Plant::get_cover_green, this, _1, _2);
      parent->addGettableVar("cover_green", protocol::DTsingle, false,
                             fn, "",  "");
   } else if (name == string("cover_tot")) {
      fn = boost::bind(&Plant::get_cover_tot, this, _1, _2);
      parent->addGettableVar("cover_tot", protocol::DTsingle, false,
                             fn, "",  "");
   } else if (name == string("height")) {
      fn = boost::bind(&Plant::get_height, this, _1, _2);
      parent->addGettableVar("height", protocol::DTsingle, false,
                             fn, "mm",  "");
   }
}


// Set a variable from the system.
bool Plant::setVariable(unsigned id, protocol::QuerySetValueData& qd)
  {
    ptr2setFn pf = IDtoSetFn[id];
    if (pf) {return((this->*pf)(qd));}
    return false;
  }
void Plant::sendStageMessage(const char *what)
  {
  unsigned int id = parent->addRegistration(RegistrationType::event,
                                            what, "",
                                            "", "");
  protocol::ApsimVariant outgoingApsimVariant(parent);
  parent->publish (id, outgoingApsimVariant);
  }
/////////////////////////These routines are portions of the fortran "main" routine.

// Field a Prepare message
void Plant::doPrepare(unsigned &, unsigned &, protocol::Variant &)
  {
    plant_zero_daily_variables ();
    phosphorus->zero_daily_variables();

    plant_get_other_variables ();     // request and receive variables from owner-modules
    if (g.plant_status == out)
     {
     plant_zero_variables ();          /// XXX UGLY HACK
     }
    else
     {
     plant_get_other_variables ();     // request and receive variables from owner-modules
     plant_prepare ();                 // do crop preparation
     }
  }

// Field a Process message
void Plant::doProcess(unsigned &, unsigned &, protocol::Variant &)
  {
    if (g.plant_status != out)
     {
     plant_get_other_variables ();   // request and receive variables from owner-modules
     plant_process ();               // do crop processes
     plant_set_other_variables ();   // send changes to owner-modules
     }
     else
     {} // plant is out
  }

// Field a Sow event
void Plant::doSow(unsigned &, unsigned &, protocol::Variant &v)
  {
  plant_get_other_variables (); // request and receive variables from owner-modules
  plant_start_crop (v);          // start crop and do  more initialisations
  }

// Field a Harvest event
void Plant::doHarvest(unsigned &, unsigned &, protocol::Variant &v)
  {
  plant_harvest (v);             // harvest crop - turn into residue
  }

// Field a End crop event
void Plant::doEndCrop(unsigned &, unsigned &, protocol::Variant &v)
  {
  plant_end_crop ();            //end crop - turn into residue
  plant_zero_variables ();
  }

// Field a Kill crop event
void Plant::doKillCrop(unsigned &, unsigned &, protocol::Variant &v)
   {
   plant_kill_crop_action (v);  //kill crop - turn into dead population
   }

// Field a Kill Stem event
void Plant::doKillStem(unsigned &, unsigned &, protocol::Variant &v)
   {
   plant_kill_stem (v);            //die
   }

// Field a Remove Crop Biomass event
void Plant::doRemoveCropBiomass(unsigned &, unsigned &, protocol::Variant &v)
   {
   plant_remove_crop_biomass (v);
   }

// Field a end run event
void Plant::doEndRun(unsigned &, unsigned &,protocol::Variant &/*v*/)
  {
  plant_zero_variables ();
  }

// Field a class change event
void Plant::doAutoClassChange(unsigned &/*fromId*/, unsigned &eventId, protocol::Variant &v)
  {
  string ps = IDtoAction[eventId];
  //printf("got '%s' from %d\n",ps.c_str(),eventId);

  plant_auto_class_change(ps.c_str());
  }

// Field a Tick event
void Plant::doTick(unsigned &, unsigned &, protocol::Variant &v)
  {
  struct protocol::timeType tick;
  v.unpack(tick);
  double sd = (double)tick.startday;
  jday_to_day_of_year(&sd, &g.day_of_year, &g.year);
  }

// Field a NewMet event
void Plant::doNewMet(unsigned &, unsigned &, protocol::Variant &v)
  {
  if (g.hasreadconstants)
     {
     struct protocol::newmetType newmet;
     v.unpack(newmet);
     g.radn = newmet.radn;
     g.maxt = newmet.maxt;
     g.mint = newmet.mint;
     }
  }


//////////////////////////////Translated code starts here///////////////////
//+  Purpose
//       Takes the minimum of biomass production limited by radiation and
//       biomass production limited by water.

//+  Mission Statement
//     Takes the minimum of biomass production limited by radiation and
//     biomass production limited by water.

//+  Changes
//      250894 jngh specified and programmed
void Plant::plant_bio_actual (int option /* (INPUT) option number*/)
    {
    const char*  my_name = "plant_bio_actual" ;
    push_routine (my_name);

    if (option == 1)
        {
        // use whichever is limiting
        g.dlt_dm = min (g.dlt_dm_pot_rue, g.dlt_dm_pot_te);
        }
    else
        {
        throw std::invalid_argument("invalid template option in plant_bio_actual");
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Simulate crop grain biomass demand.

//+  Mission Statement
//     Calculate grain biomass demand

//+  Changes
//      250894 jngh specified and programmed
void Plant::plant_bio_grain_demand (int option /* (INPUT) option number */)
    {
    const char*  my_name = "plant_bio_grain_demand" ;
    const int  num_yield_parts = 2 ;
    int   yield_parts[num_yield_parts] = {meal, oil};

//- Implementation Section ----------------------------------
    push_routine (my_name);

    if (option == 1)
        {
        legnew_bio_yieldpart_demand1(
            g.current_stage
            , c.twilight
            , g.day_of_year
            , g.latitude
            , flowering
            , start_grain_fill
            , maturity
            , yield_parts
            , num_yield_parts
            , root
            , max_part
            , g.dlt_dm
            , g.dm_green
            , g.dm_senesced
            , g.days_tot
            , g.dm_stress_max
            , p.x_pp_hi_incr
            , p.y_hi_incr
            , p.num_pp_hi_incr
            , p.x_hi_max_pot_stress
            , p.y_hi_max_pot
            , p.num_hi_max_pot
            , g.grain_energy
            , &g.dlt_dm_grain_demand
            );                                    // Start Stress_stage
        }
    else if (option == 2)
        {
        legnew_bio_yieldpart_demand2(
            g.current_stage,
            start_grain_fill,
            end_grain_fill,
            g.grain_no,
            p.potential_grain_filling_rate,
            g.maxt,
            g.mint,
            c.x_temp_grainfill,
            c.y_rel_grainfill,
            c.num_temp_grainfill,
            &g.dlt_dm_grain_demand);
        }
    else if (option == 3)
        {
        legnew_bio_yieldpart_demand3(
               max_fruit_cohorts,
               max_part,
               max_fruit_stage,
               g.num_fruit_cohorts,
               g.current_fruit_stage,
               flowering,
               start_grain_fill,
               end_grain_fill,
               p.y_tt_flower_to_start_grain,
               p.y_tt_fruit_start_to_end_grain,
               g.dlt_fruit_tt,
               g.fruit_tt_tot,
               g.fruit_no,
               p.potential_fruit_filling_rate,
               p.dm_fruit_max,
               g.dm_fruit_green,
               g.maxt,
               g.mint,
               c.x_temp_grainfill,
               c.y_rel_grainfill,
               c.num_temp_grainfill,
               c.x_stage_no_partition,
               c.y_frac_pod,
               c.num_stage_no_partition,
               c.tt_flower_to_start_pod,
               g.fruit_phase_tt,
               g.grain_energy,
               g.dlt_dm_fruit_demand,
               g.dlt_dm_fruit_grain_demand);

        g.dlt_dm_grain_demand = sum_real_array(g.dlt_dm_fruit_grain_demand, max_part);
        }
    else
        {
        throw std::invalid_argument("invalid template option in plant_bio_grain_demand");
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Calculate grain oil factors.

//+  Mission Statement
//       Calculate grain oil factors

//+  Changes
//      141100 jngh specified and programmed
void Plant::plant_bio_grain_oil (int option /* (INPUT) option number */)
    {
    const char*  my_name = "plant_bio_grain_oil" ;
    push_routine (my_name);

    if (option == 1)
        {
        legnew_bio_grain_oil (
            c.grain_oil_conc
            , c.carbo_oil_conv_ratio
            , &g.grain_energy);
        }
    else
        {
        throw std::invalid_argument( "invalid template option in plant_bio_grain_oil");
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Partition biomass.

//+  Mission Statement
//     Calculate biomass partitioning

//+  Changes
//      250894 jngh specified and programmed
void Plant::plant_bio_partition (int option /* (INPUT) option number */)
    {
    const char*  my_name = "plant_bio_partition" ;
    push_routine (my_name);

    if (option == 1)
        {

        legnew_dm_partition1
            (
            c.frac_leaf[(int)g.current_stage-1]
            , c.frac_pod[(int)g.current_stage-1]
            , g.grain_energy
            , c.grain_oil_conc
            , c.ratio_root_shoot[(int)g.current_stage-1]
            , c.sla_min
            , g.dlt_dm
            , g.dlt_dm_grain_demand
            , g.dlt_lai_stressed
            , &g.dlt_dm_oil_conv
            , g.dlt_dm_green
            );

        }
    else if (option == 2)
        {

        legnew_dm_partition2
            (
            g.current_stage
            , c.x_stage_no_partition
            , c.y_frac_leaf
            , c.y_frac_pod
            , c.num_stage_no_partition
            , g.grain_energy
            , c.grain_oil_conc
            , c.y_ratio_root_shoot
            , c.sla_min
            , g.dlt_dm
            , g.dlt_dm_grain_demand
            , g.dlt_lai_stressed
            , &g.dlt_dm_oil_conv
            , g.dlt_dm_green
            );
        }
    else if (option == 3)
        {
        legnew_dm_partition3(
                     g.current_stage
                   , g.current_fruit_stage
                   , c.x_stage_no_partition
                   , c.y_frac_leaf
                   , c.y_frac_pod
                   , c.num_stage_no_partition
                   , g.grain_energy
                   , c.grain_oil_conc
                   , c.y_ratio_root_shoot
                   , c.sla_min
                   , max_fruit_cohorts
                   , max_part
                   , g.num_fruit_cohorts
                   , g.fruit_no
                   , g.dlt_dm
                   , g.dlt_dm_fruit_grain_demand
                   , g.dlt_dm_fruit_demand
                   , g.dlt_dm_parasite_demand
                   , g.dlt_lai_stressed
                   , g.dlt_dm_fruit_oil_conv
                   , &g.dlt_dm_parasite
                   , g.dlt_dm_fruit_green);

         g.dlt_dm_oil_conv     = sum_real_array(g.dlt_dm_fruit_oil_conv, max_fruit_cohorts);
         for (int part=0; part < max_part; part++)
            {
            g.dlt_dm_green[part] = 0.0;
            int cohort = 0;
            do {
               g.dlt_dm_green[part] += g.dlt_dm_fruit_green[cohort][part];
               cohort++;
               } while (cohort < g.num_fruit_cohorts);
            }
        }
    else
        {
        throw std::invalid_argument("invalid template option in plant_bio_partition");
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Retranslocate biomass.

//+  Mission Statement
//     Retranslocate biomass

//+  Changes
//      250894 jngh specified and programmed
void Plant::plant_bio_retrans (int option /* (INPUT) option number */)
    {
    const char*  my_name = "plant_bio_retrans" ;
    const int  num_supply_pools = 3 ;
    const int  num_fruit_supply_pools = 2 ;

    int   supply_pools[num_supply_pools] = {stem, leaf, pod};
    int   fruit_supply_pools[num_fruit_supply_pools] = {stem, leaf};

    push_routine (my_name);
    if (option == 1)
        {

        legnew_dm_retranslocate1
            (
            c.frac_pod[(int) g.current_stage-1]
            , g.grain_energy
            , c.grain_oil_conc
            , pod
            , meal
            , oil
            , max_part
            , supply_pools
            , num_supply_pools
            , g.dlt_dm_grain_demand
            , g.dlt_dm_oil_conv
            , g.dlt_dm_green
            , g.dm_green
            , g.dm_plant_min
            , g.plants
            , &g.dlt_dm_oil_conv_retranslocate
            , g.dlt_dm_green_retrans
            );

        }
    else if (option == 2)
        {

        legnew_dm_retranslocate2
            (
            g.current_stage
            , c.x_stage_no_partition
            , c.y_frac_pod
            , c.num_stage_no_partition
            , g.grain_energy
            , c.grain_oil_conc
            , pod
            , meal
            , oil
            , max_part
            , supply_pools
            , num_supply_pools
            , g.dlt_dm_grain_demand
            , g.dlt_dm_oil_conv
            , g.dlt_dm_green
            , g.dm_green
            , g.dm_plant_min
            , g.plants
            , &g.dlt_dm_oil_conv_retranslocate
            , g.dlt_dm_green_retrans
            );

        }
    else if (option == 3)
        {
        legnew_dm_retranslocate3(
                     start_grain_fill
                   , end_grain_fill
                   , g.current_stage
                   , g.current_fruit_stage
                   , c.x_stage_no_partition
                   , c.y_frac_pod
                   , c.num_stage_no_partition
                   , g.grain_energy
                   , c.grain_oil_conc
                   , pod
                   , meal
                   , oil
                   , max_part
                   , fruit_supply_pools
                   , num_fruit_supply_pools
                   , max_fruit_cohorts
                   , g.dlt_dm_fruit_grain_demand
                   , g.dlt_dm_fruit_demand
                   , g.dlt_dm_parasite_demand
                   , g.dlt_dm_fruit_oil_conv
                   , g.dlt_dm_parasite
                   , g.dlt_dm_fruit_green
                   , g.dm_green
                   , g.dm_fruit_green
                   , g.dm_plant_min
                   , g.dm_fruit_pod_min
                   , g.plants
                   , g.fruit_no
                   , g.num_fruit_cohorts
                   , g.dlt_dm_fruit_oil_conv_retranslocate
                   , &g.dm_parasite_retranslocate
                   , g.dlt_dm_fruit_green_retrans);
         g.dlt_dm_oil_conv_retranslocate =
                    sum_real_array(g.dlt_dm_fruit_oil_conv_retranslocate,max_fruit_cohorts);
         for (int part=0; part < max_part; part++)
            {
            g.dlt_dm_green_retrans[part] = 0.0;
            for (int cohort = 0; cohort < max_fruit_cohorts; cohort++)
                {
                g.dlt_dm_green_retrans[part] += g.dlt_dm_fruit_green_retrans[cohort][part];
               }
            }
        }
    else
        {
        throw std::invalid_argument("invalid template option in plant_bio_retrans");
        }

    pop_routine (my_name);
    return;
    }



//+  Purpose
//         Get current water stress factors (0-1)

//+  Mission Statement
//     Calulates the current water stress factors

//+  Changes
//     010994 jngh specified and programmed
//     250297 slw split up into separate stress factors
void Plant::plant_water_stress (int option /* (INPUT) option number */)
    {
//+  Constant Values
    const char*  my_name = "plant_water_stress" ;

//+  Local Variables
    float ext_sw_supply [max_layer];              // external sw supply (mm)

//- Implementation Section ----------------------------------

    push_routine (my_name);

    if (option == 1)
        {

        if (Str_i_Eq(p.uptake_source,"apsim"))
            {
            // this would have been avoided if we have
            // each stress factor in its own routine! - NIH
            // photo requires (really) actually water uptake
            // but expansion requires pot water uptake.
            // we only have one supply variable.
            plant_get_ext_uptakes(p.uptake_source.c_str()
                                 ,c.crop_type.c_str()
                                 ,"water"
                                 ,1.0
                                 ,0.0
                                 ,100.0
                                 ,ext_sw_supply
                                 ,max_layer);
            crop_swdef_photo(max_layer, g.dlayer, g.root_depth,
                             g.sw_demand, ext_sw_supply, &g.swdef_photo);
            }
        else
            {
            crop_swdef_photo(max_layer, g.dlayer, g.root_depth,
                             g.sw_demand, g.sw_supply, &g.swdef_photo);
            }

        crop_swdef_pheno(c.num_sw_avail_ratio,
                         c.x_sw_avail_ratio, c.y_swdef_pheno, max_layer, g.dlayer,
                         g.root_depth, g.sw_avail, g.sw_avail_pot, &g.swdef_pheno);
        crop_swdef_expansion(c.num_sw_demand_ratio,
                             c.x_sw_demand_ratio, c.y_swdef_leaf, max_layer, g.dlayer,
                             g.root_depth, g.sw_demand, g.sw_supply, &g.swdef_expansion);
        crop_swdef_fixation(c.num_sw_avail_fix,
                            c.x_sw_avail_fix, c.y_swdef_fix, max_layer, g.dlayer,
                            g.root_depth, g.sw_avail, g.sw_avail_pot,
                            &g.swdef_fixation);

        }
    else if (option == 2)
        {
        if (Str_i_Eq(p.uptake_source,"apsim"))
            {
            plant_get_ext_uptakes(p.uptake_source.c_str()
                                 ,c.crop_type.c_str()
                                 ,"water"
                                 ,1.0
                                 ,0.0
                                 ,100.0
                                 ,ext_sw_supply
                                 ,max_layer);
            crop_swdef_photo(max_layer, g.dlayer, g.root_depth,
                             g.sw_demand, ext_sw_supply, &g.swdef_photo);
            }
        else
            {
            crop_swdef_photo(max_layer, g.dlayer, g.root_depth,
                             g.sw_demand, g.sw_supply, &g.swdef_photo);
            }

        crop_swdef_pheno(c.num_sw_avail_ratio,
                         c.x_sw_avail_ratio, c.y_swdef_pheno, max_layer, g.dlayer,
                         g.root_depth, g.sw_avail, g.sw_avail_pot, &g.swdef_pheno);

        crop_swdef_pheno(c.num_sw_avail_ratio_flower,
                         c.x_sw_avail_ratio_flower, c.y_swdef_pheno_flower, max_layer, g.dlayer,
                         g.root_depth, g.sw_avail, g.sw_avail_pot,
                         &g.swdef_pheno_flower);

        crop_swdef_pheno(c.num_sw_avail_ratio_grainfill,
                         c.x_sw_avail_ratio_grainfill, c.y_swdef_pheno_grainfill, max_layer, g.dlayer,
                         g.root_depth, g.sw_avail, g.sw_avail_pot,
                         &g.swdef_pheno_grainfill);

        crop_swdef_expansion(c.num_sw_demand_ratio,
                             c.x_sw_demand_ratio, c.y_swdef_leaf, max_layer, g.dlayer,
                             g.root_depth, g.sw_demand, g.sw_supply, &g.swdef_expansion);
        crop_swdef_fixation(c.num_sw_avail_fix,
                            c.x_sw_avail_fix, c.y_swdef_fix, max_layer, g.dlayer,
                            g.root_depth, g.sw_avail, g.sw_avail_pot,
                            &g.swdef_fixation);

        }
    else
        {
        throw std::invalid_argument ("invalid template option plant_water_stress");
        }

    pop_routine (my_name);
    }


//+  Purpose
//         Get current temperature stress factors (0-1)

//+  Mission Statement
//         Gets the current Temperature stress factors

//+  Changes
//     010994 jngh specified and programmed
void Plant::plant_temp_stress (int option/* (INPUT) option number*/)
    {
    const char*  my_name = "plant_temp_stress" ;

    push_routine (my_name);

    if (option == 1)
        {
        crop_temperature_stress_photo(c.num_ave_temp,
                                      c.x_ave_temp, c.y_stress_photo,
                                      g.maxt, g.mint, &g.temp_stress_photo);
        }
    else
        {
        throw std::invalid_argument ("invalid template option in temp_stress");
        }

    pop_routine (my_name);
    return;
    }




//+  Purpose
//         Get current oxygen deficit stress factors (0-1)

//+  Mission Statement
//     Calculate today's oxygen deficit stress factor

//+  Changes
//     181197 nih specified and programmed
void Plant::plant_oxdef_stress (int option /* (INPUT) option number */)
    {


//+  Constant Values
    const char*  my_name = "plant_oxdef_stress" ;

//- Implementation Section ----------------------------------

    push_routine (my_name);

    if (option == 1)
        {
        crop_oxdef_photo1(  c.num_oxdef_photo
                          , c.oxdef_photo
                          , c.oxdef_photo_rtfr
                          , g.ll15_dep
                          , g.sat_dep
                          , g.sw_dep
                          , g.dlayer
                          , g.root_length
                          , g.root_depth
                          , max_layer
                          , &g.oxdef_photo);
        }
    else
        {
        throw std::invalid_argument ("invalid template option in oxdef_stress");
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       bio transpiration efficiency

//+  Mission Statement
//     Calculate biomass transpiration efficiency

//+  Changes
//      5/9/96 dph
void Plant::plant_bio_water (int option /* (INPUT) option number */)
    {
    const char*  my_name = "plant_bio_water" ;
    push_routine (my_name);

    if (option == 1)
        {
        cproc_bio_water1 (max_layer, g.dlayer, g.root_depth,
                          g.sw_supply, g.transp_eff, &g.dlt_dm_pot_te);
        }
    else
        {
        throw std::invalid_argument ("invalid template option in bio_water");
        }
    pop_routine (my_name);
    return;
    }


//+  Purpose
//     Initialise biomass calculation

//+  Mission Statement
//     Initialise biomass calculation

//+  Changes
//     21-04-1998 - unknown - Programmed and Specified
void Plant::plant_bio_init (int option)
    {
    const char*  myname = "plant_bio_init" ;
    push_routine (myname);

    if (option==1)
        {
        plant_dm_init (c.dm_init[leaf]
                       , c.dm_init[root]
                       , c.dm_init[stem]
                       , c.leaf_trans_frac
                       , c.stem_trans_frac
                       , c.pod_trans_frac
                       , g.current_stage
                       , g.plants
                       , g.dm_green
                       , g.dm_plant_min);
        }
    else if (option==2)
        {
        plant_dm_init(c.dm_init[leaf]
                    , c.dm_init[root]
                    , c.dm_init[stem]
                    , c.leaf_trans_frac
                    , c.stem_trans_frac
                    , c.pod_trans_frac
                    , g.current_stage
                    , g.plants
                    , g.dm_green
                    , g.dm_plant_min);

        plant_fruit_dm_init(c.dm_init[leaf]
                          , c.dm_init[root]
                          , c.dm_init[stem]
                          , c.pod_trans_frac
                          , max_part
                          , max_fruit_cohorts
                          , g.current_stage
                          , g.current_fruit_stage
                          , g.num_fruit_cohorts
                          , g.fruit_no
                          , g.plants
                          , g.fruit_sdr_daily
                          , g.dm_fruit_green
                          , g.dm_fruit_pod_min);
        }
    else
        {
        throw std::invalid_argument ( "invalid template option in bio_init");
        }

    pop_routine (myname);
    return;
    }


//+  Purpose
//       Simulate crop grain biomass demand stress factor

//+  Mission Statement
//     Calculate grain biomass demand stress factor

//+  Changes
//      280598 nih specified and programmed
void Plant::plant_bio_grain_demand_stress (int option /* (INPUT) option number */)
    {
    const char*  my_name = "plant_bio_grain_demand_stress" ;
    push_routine (my_name);

    if (option == 1)
        {
        cproc_yieldpart_demand_stress1 (min(g.nfact_photo, phosphorus->fact_photo())
                                        ,g.swdef_photo
                                        ,g.temp_stress_photo
                                        ,&g.dlt_dm_stress_max);
        }
    else
        {
        throw std::invalid_argument ( "invalid template option in bio_grain_demand_stress");
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Initialise plant weights and plant weight minimums
//       at required instances.

//+  Mission Statement
//     Initialise plant weights and plant weight minimums at required instances.

//+  Changes
//     21-04-1998 - unknown - Programmed and Specified
void Plant::plant_retrans_init (int option)
    {
    const char*  myname = "plant_retrans_init" ;

//- Implementation Section ----------------------------------
    push_routine (myname);

    if (option==1)
        {
        legnew_retrans_init
            (
            c.leaf_trans_frac
            , c.stem_trans_frac
            , c.pod_trans_frac
            , g.current_stage
            , g.plants
            , g.dm_green, g.dm_plant_min
            );
        }
    else
        {
        throw std::invalid_argument ("invalid template option in  retrans_init");
        }

    pop_routine (myname);
    return;
    }


//+  Purpose
//       Simulate plant detachment.

//+  Mission Statement
//     Simulate plant detatchment

//+  Changes
//      091294 jngh specified and programmed
void Plant::plant_detachment (int option /* (INPUT) option number */)
    {
    const char*  my_name = "plant_detachment" ;

//- Implementation Section ----------------------------------
    push_routine (my_name);

    if (option == 1)
        {
        cproc_lai_detachment1 (leaf
                               , c.sen_detach_frac
                               , g.slai
                               , &g.dlt_slai_detached
                               , c.dead_detach_frac
                               , g.tlai_dead
                               , &g.dlt_tlai_dead_detached);

        plant_leaf_detachment (g.leaf_area
                               , g.dlt_slai_detached
                               , g.plants, max_node);

        cproc_dm_detachment1 ( max_part
                              , c.sen_detach_frac
                              , g.dm_senesced
                              , g.dlt_dm_detached
                              , c.dead_detach_frac
                              , g.dm_dead
                              , g.dlt_dm_dead_detached);

        cproc_n_detachment1 ( max_part
                             , c.sen_detach_frac
                             , g.n_senesced
                             , g.dlt_n_detached
                             , c.dead_detach_frac
                             , g.n_dead
                             , g.dlt_n_dead_detached);
        }
    else
        {
        throw std::invalid_argument ("invalid template option in detachment");
        }

    pop_routine (my_name);
    return;
    }



//+  Purpose
//      Determine plant death in crop

//+  Mission Statement
//     Determine plant death of crop

//+  Changes
//       290994 jngh specified and programmed
void Plant::plant_plant_death (int option /* (INPUT) option number*/)
    {
//+  Constant Values
    const char*  my_name = "plant_plant_death" ;

//- Implementation Section ----------------------------------

    push_routine (my_name);

    if (option == 1)
        {
        crop_failure_germination (parent, sowing, germ, now,
                                  c.days_germ_limit,
                                  g.current_stage,
                                  g.days_tot,
                                  g.plants,
                                  &g.dlt_plants_failure_germ);

        crop_failure_emergence (parent, germ, emerg, now,
                                c.tt_emerg_limit,
                                g.current_stage,
                                g.plants,
                                g.tt_tot,
                                &g.dlt_plants_failure_emergence);

//         print*, germ, emerg, now,
//     .          c%tt_emerg_limit,
//     .          g%current_stage,
//     .          g%plants,
//     .          g%tt_tot,
//     :          g%dlt_plants_failure_emergence

        plant_failure_leaf_sen(g.current_stage,
                               g.lai,
                               g.plants,
                               &g.dlt_plants_failure_leaf_sen);

        plant_failure_phen_delay(c.swdf_pheno_limit
            , g.cswd_pheno
            , g.current_stage
            , g.plants
            , &g.dlt_plants_failure_phen_delay
            );

        plant_death_seedling
            (
            c.num_weighted_temp
            , c.x_weighted_temp
            , c.y_plant_death
            , g.day_of_year
            , g.soil_temp
            , g.year
            , g.days_tot
            , g.plants
            , &g.dlt_plants_death_seedling);

        plant_death_drought
            (
            c.leaf_no_crit
            , c.swdf_photo_limit
            , c.swdf_photo_rate
            , g.cswd_photo
            , g.leaf_no
            , g.plants
            , g.swdef_photo
            , &g.dlt_plants_death_drought
            );

        plant_death_actual(g.dlt_plants_death_drought
                           , &g.dlt_plants_death_external
                           , g.dlt_plants_death_seedling
                           , g.dlt_plants_failure_emergence
                           , g.dlt_plants_failure_germ
                           , g.dlt_plants_failure_leaf_sen
                           , g.dlt_plants_failure_phen_delay
                           , &g.dlt_plants);
        if (reals_are_equal (g.dlt_plants + g.plants, 0.0))
            {
            plant_kill_crop(g.dm_dead
                            , g.dm_green
                            , g.dm_senesced
                            , &g.plant_status);
            // XX Needs to signal a need to call zero_variables here...
            // Present method is to rely on calling zero_xx at tomorrow's prepare() event.. :(
            }
        else
            {
            }

        }
    else
        {
        throw std::invalid_argument ("invalid template option in plant_death");
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//      Determine plant death from all leaf area senescing.

//+  Mission Statement
//     Determine plant death from leaf area senescing

//+  Changes
//       290994 jngh specified and programmed
//       110695 psc  added plant death from high soil temp
//       100795 jngh moved plant_kill crop to end of routine
void Plant::plant_failure_leaf_sen (float g_current_stage    // (INPUT)  current phenological stage
                                   ,float g_lai              // (INPUT)  live plant green lai
                                   ,float g_plants           // (INPUT)  Plant density (plants/m^2)
                                   ,float *dlt_plants        // (OUTPUT) change in plant number
                                   ) {
//+  Constant Values
    const char*  my_name = "plant_failure_leaf_sen" ;

//+  Local Variables
    float leaf_area;                              // leaf area per plant

//- Implementation Section ----------------------------------

    push_routine (my_name);

    leaf_area = divide (g_lai, g_plants, 0.0);

    if (reals_are_equal (leaf_area, 0.0, 1.0e-6)
        && stage_is_between (floral_init, plant_end, g_current_stage))
        {
        *dlt_plants = - g_plants;
        parent->writeString ("Crop failure because of total leaf senescence.");
        }
    else
        {
        *dlt_plants = 0.0;
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//      Determine plant death from prolonged phenology delay.

//+  Mission Statement
//     Determine plant death from prolonged phenology delay

//+  Changes
//       290994 jngh specified and programmed
//       110695 psc  added plant death from high soil temp
//       100795 jngh moved plant_kill crop to end of routine
void Plant::plant_failure_phen_delay (
     float c_swdf_pheno_limit          // (INPUT)  critical cumulative phenology water stress above which the crop fails (unitless)
    ,float *g_cswd_pheno               // (INPUT)  cumulative water stress type 3
    ,float g_current_stage             // (INPUT)  current phenological stage
    ,float g_plants                    // (INPUT)  Plant density (plants/m^2)
    ,float *dlt_plants                  // (OUTPUT) change in plant number
    ) {

//+  Constant Values
    const char*  my_name = "plant_failure_phen_delay" ;

//+  Local Variables
    float cswd_pheno;                             // cumulative water stress for phenology

//- Implementation Section ----------------------------------

    push_routine (my_name);

    cswd_pheno = sum_between (emerg-1, flowering-1, g_cswd_pheno);

    if (stage_is_between (emerg, flowering, g_current_stage)
        && cswd_pheno>=c_swdf_pheno_limit)
        {
        *dlt_plants = - g_plants;
        parent->writeString ("Crop failure because of prolonged");
        parent->writeString ("phenology delay through water stress.");
        }
    else
        {
        *dlt_plants = 0.0;
        }

   pop_routine (my_name);
   return;
   }


//+  Purpose
//      Determine plant seedling death.

//+  Mission Statement
//     Determine plant seeding death

//+  Changes
//       290994 jngh specified and programmed
//       110695 psc  added plant death from high soil temp
//       100795 jngh moved plant_kill crop to end of routine
void Plant::plant_death_seedling
    (
     int    c_num_weighted_temp      // (INPUT)  size of table
    ,float  *c_x_weighted_temp        // (INPUT)  temperature table for poor est
    ,float  *c_y_plant_death          // (INPUT)  index of plant death
    ,int    g_day_of_year            // (INPUT)  day of year
    ,float  *g_soil_temp              // (INPUT)  soil surface temperature (oC)
    ,int    g_year                   // (INPUT)  year
    ,float  *g_days_tot               // (INPUT)  duration of each phase (days)
    ,float  g_plants                 // (INPUT)  Plant density (plants/m^2)
    ,float  *dlt_plants               // (OUTPUT) change in plant number
    ) {
    const char*  my_name = "plant_death_seedling" ;

    int   days_after_emerg;                       // days after emergence (days)
    float killfr;                                 // fraction of crop population to kill


    push_routine (my_name);

//cpsc  add code to kill plants for high soil surface temperatures

    days_after_emerg = (int) (sum_between (emerg-1, now-1, g_days_tot)) - 1;
    if (days_after_emerg == 1)
        {
        plant_plants_temp(c_num_weighted_temp
                          , c_x_weighted_temp
                          , c_y_plant_death
                          , g_day_of_year
                          , g_soil_temp
                          , g_year
                          , &killfr);
        *dlt_plants = - g_plants*killfr;

        if (killfr > 0.0)
            {
            string msg= "Plant kill. ";
              msg = msg + ftoa(killfr*100.0, 2).c_str();
              msg = msg + "% failure because of high soil surface temperatures.";
            parent->writeString (msg.c_str());
            }
        else
            {
            // do nothing
            }

        }
    else
        {
        *dlt_plants = 0.0;
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//      Determine plant death from drought.

//+  Mission Statement
//     Determine plant death from drought

//+  Changes
//       290994 jngh specified and programmed
//       110695 psc  added plant death from high soil temp
//       100795 jngh moved plant_kill crop to end of routine
void Plant::plant_death_drought
    (
     float  c_leaf_no_crit              // (INPUT)  critical number of leaves belo
    ,float  c_swdf_photo_limit          // (INPUT)  critical cumulative photosynth
    ,float  c_swdf_photo_rate           // (INPUT)  rate of plant reduction with p
    ,float  *g_cswd_photo               // (INPUT)  cumulative water stress type 1
    ,float  *g_leaf_no                  // (INPUT)  number of fully expanded leave
    ,float  g_plants                    // (INPUT)  Plant density (plants/m^2)
    ,float  g_swdef_photo               // (INPUT)
    ,float  *dlt_plants                  // (OUTPUT) change in plant number
    ) {
//+  Constant Values
    const char*  my_name = "plant_death_drought" ;

//+  Local Variables
    float cswd_photo;                             // cumulative water stress for photoperiod
    float leaf_no;                                // number of leaves
    float killfr;                                 // fraction of crop population to kill

//- Implementation Section ----------------------------------

    push_routine (my_name);

    cswd_photo = sum_between (emerg-1, flowering-1, g_cswd_photo);
    leaf_no = sum_real_array (g_leaf_no, max_node);

    if (leaf_no<c_leaf_no_crit
        && cswd_photo>c_swdf_photo_limit
        && g_swdef_photo<1.0)
        {

        killfr = c_swdf_photo_rate * (cswd_photo - c_swdf_photo_limit); //XX This is wrong??
        killfr = bound (killfr, 0.0, 1.0);
        *dlt_plants = - g_plants*killfr;

        string msg= "Plant kill. ";
          msg = msg + ftoa(killfr*100.0, 2).c_str();
          msg = msg + "% failure because of water stress.";
        parent->writeString (msg.c_str());
        }


    else
        {
        *dlt_plants = 0.0;
        }
    pop_routine (my_name);
    return;
    }


//+  Purpose
//      Determine plant seedling death.

//+  Mission Statement
//     Determine plant seeding death

//+  Changes
//       290902 jngh specified and programmed
void Plant::plant_death_external_action(protocol::Variant &v         // (INPUT) message variant
                                        ,float g_plants              // (INPUT) Plant density (plants/m^2)
                                        ,float *dlt_plants           // (OUTPUT) change in plant number
                                        ) {
    const char*  my_name = "plant_death_external_action" ;

    float killfr;                                 // fraction of crop population to kill

    push_routine (my_name);

    protocol::ApsimVariant incomingApsimVariant(parent);
    incomingApsimVariant.aliasTo(v.getMessageData());

    // Determine kill fraction
    if (incomingApsimVariant.get("plants_kill_fraction", protocol::DTsingle, false, killfr) == false)
       {
       killfr = 1.0;
       *dlt_plants = - g_plants   ;               // default to whole crop
       }
    else
       {
       bound_check_real_var(parent, killfr, 0.0, 1.0, "killfr");
       *dlt_plants = *dlt_plants - g_plants*killfr;
       }

    if (killfr > 0.0)
        {
        string msg= "Plant kill. ";
          msg = msg + ftoa(killfr*100.0, 2).c_str();
          msg = msg + "% crop killed because of external action.";
        parent->writeString (msg.c_str());
        }
    else
        {
        // do nothing - no fraction
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//      Determine plant death from external action.

//+  Mission Statement
//     Determine plant death from external action

//+  Changes
//       290994 jngh specified and programmed
//       110695 psc  added plant death from high soil temp
//       100795 jngh moved plant_kill crop to end of routine
void Plant::plant_death_crop_killed
    (
      float    g_plants                           // (INPUT)  Plant density (plants/m^2)
    , status_t g_plant_status                     // (INPUT)
    , float    *dlt_plants                        // (OUTPUT) change in plant number
    ) {
    const char*  my_name = "plant_death_crop_killed" ;

    push_routine (my_name);

    if (g_plant_status == dead)
        {
        *dlt_plants = - g_plants;
        parent->writeString ("Crop killed because of external action.");
        }
    else
        {
        *dlt_plants = 0.0;
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//      Determine actual plant death.

//+  Mission Statement
//     Determine actual plant death

//+  Changes
//       290994 jngh specified and programmed
//       110695 psc  added plant death from high soil temp
//       100795 jngh moved plant_kill crop to end of routine
void Plant::plant_death_actual
    (
     float g_dlt_plants_death_drought                 // (INPUT)
    ,float *g_dlt_plants_death_external              // (INPUT)
    ,float g_dlt_plants_death_seedling              // (INPUT)
    ,float g_dlt_plants_failure_emergence           // (INPUT)
    ,float g_dlt_plants_failure_germ                // (INPUT)
    ,float g_dlt_plants_failure_leaf_sen            // (INPUT)
    ,float g_dlt_plants_failure_phen_delay          // (INPUT)
    ,float *dlt_plants                               // (OUTPUT) change in plant number
    ) {
    const char*  my_name = "plant_death_actual" ;


    push_routine (my_name);

    // dlt's are negative so take minimum.
    float pmin = g_dlt_plants_failure_germ;             // Progressive minimum
    pmin = min(pmin, g_dlt_plants_failure_emergence);
    pmin = min(pmin, g_dlt_plants_failure_leaf_sen);
    pmin = min(pmin, g_dlt_plants_failure_phen_delay);
    pmin = min(pmin, g_dlt_plants_death_drought);
    pmin = min(pmin, g_dlt_plants_death_seedling);
    pmin = min(pmin, *g_dlt_plants_death_external);

    *dlt_plants = pmin;

    *g_dlt_plants_death_external = 0.0;                //Ugly hack here??
    pop_routine (my_name);
    return;
    }


//+  Purpose
//        Calculate fraction of plants killed by high temperature during
//        emergence (0-1).

//+  Mission Statement
//     Calculate fraction of plants killed by high temperature during emergence

//+  Changes
//     230695 jngh specified and programmed
void Plant::plant_plants_temp
    (
     int    c_num_weighted_temp                          // (INPUT)  size of table
    ,float  *c_x_weighted_temp                           // (INPUT)  temperature table for poor est
    ,float  *c_y_plant_death                             // (INPUT)  index of plant death
    ,int    g_day_of_year                                // (INPUT)  day of year
    ,float  *g_soil_temp                                 // (INPUT)  soil surface temperature (oC)
    ,int    g_year                                       // (INPUT)  year
    ,float  *killfr                                      // (OUTPUT) fraction of plants killed  (plants/m^2)
    ) {

//+  Constant Values
    const char*  my_name = "plant_plants_temp" ;

//+  Local Variables
    int   day_before;                             // day of year number of day before
                                                  // yesterday ()
    float weighted_temp;                          // 3 day weighted soil temperature (oC)
    int   yesterday;                              // day of year number of yesterday

//- Implementation Section ----------------------------------
    push_routine (my_name);

    yesterday = offset_day_of_year (g_year, g_day_of_year, - 1);
    day_before = offset_day_of_year (g_year, g_day_of_year, - 2);

    weighted_temp = 0.25 * g_soil_temp[day_before]
      + 0.50 * g_soil_temp[yesterday]
      + 0.25 * g_soil_temp[g_day_of_year];

    *killfr = linear_interp_real (weighted_temp
          , c_x_weighted_temp
          , c_y_plant_death
          , c_num_weighted_temp);

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Kill crop

//+  Mission Statement
//     Kill the crop

//+  Changes
//       290994 jngh specified and programmed
void Plant::plant_kill_crop
    (
     float  *g_dm_dead                                  // (INPUT)  dry wt of dead plants (g/m^2)
    ,float  *g_dm_green                                 // (INPUT)  live plant dry weight (biomass) (g/m^2)
    ,float  *g_dm_senesced                              // (INPUT)  senesced plant dry wt (g/m^2)
    ,status_t *g_plant_status                            // (OUTPUT)
    ) {
//+  Constant Values
    const char*  my_name = "plant_kill_crop" ;

//+  Local Variables
    float biomass;                                // above ground dm (kg/ha)

//- Implementation Section ----------------------------------

//!!!!! fix problem with deltas in update when change from alive to dead ?zero deltas
    push_routine (my_name);

    if (*g_plant_status == alive)
        {
        *g_plant_status = dead;

        biomass = (sum_real_array (g_dm_green, max_part)
                             - g_dm_green[root]) * gm2kg /sm2ha
                             + (sum_real_array (g_dm_senesced, max_part)
                             - g_dm_senesced[root]) * gm2kg /sm2ha
                             + (sum_real_array (g_dm_dead, max_part)
                             - g_dm_dead[root]) * gm2kg /sm2ha;

        // report
        char msg[80];
        sprintf(msg, "Plant death. standing above-ground dm = %.2f (kg/ha)", biomass);
        parent->writeString (msg);
        }
    else
        {
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Simulate potential crop leaf area development - may be limited by
//       DM production in subsequent routine

//+  Mission Statement
//     Get the potential leaf area development

//+  Changes
//      250894 jngh specified and programmed
void Plant::plant_leaf_area_potential (int option /* (INPUT) option number */)
    {
//+  Constant Values
    const char*  my_name = "plant_leaf_area_potential" ;

//- Implementation Section ----------------------------------
    push_routine (my_name);

// Plant leaf development
    if (option == 1)
        {
        cproc_leaf_area_pot1 (c.x_node_no
                              , c.y_leaf_size
                              , c.num_node_no
                              , g.node_no
                              , c.node_no_correction
                              , emerg
                              , now
                              , g.dlt_leaf_no_pot
                              , g.plants
                              , &g.dlt_lai_pot);
        }
    else
        {
        throw std::invalid_argument ( "invalid template option inplant_leaf_area_potential");
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Simulate potential stressed crop leaf area development - may
//       be limited by DM production in subsequent routine

//+  Mission Statement
//     Get potential stressed leaf area development

//+  Changes
//      250894 jngh specified and programmed
//      250297 slw changed to sugar template structure
void Plant::plant_leaf_area_stressed (int option /* (INPUT) option number*/)
    {

//+  Constant Values
    const char*  my_name = "plant_leaf_area_stressed" ;

//- Implementation Section ----------------------------------
    push_routine (my_name);

// Plant leaf development
    if (option == 1)
        {
        cproc_leaf_area_stressed1 (g.dlt_lai_pot
                                   ,g.swdef_expansion
                                   ,min(g.nfact_expansion, phosphorus->fact_expansion())
                                   ,&g.dlt_lai_stressed);
        }
    else
        {
        throw std::invalid_argument ( "invalid template option in leaf_area_stresses");
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//      Initialise plant leaf area

//+  Mission Statement
//     Initialise plant leaf area

//+  Changes
//     21-04-1998 - neilh - Programmed and Specified
void Plant::plant_leaf_area_init (int option)
    {

//+  Constant Values
    const char*  myname = "plant_leaf_area_init" ;

//- Implementation Section ----------------------------------
    push_routine (myname);

    if (option==1)
        {
// initialise total leaf number
        legopt_leaf_area_init1 (
            c.initial_tpla
            , c.leaf_no_at_emerg
            , emerg
            , g.current_stage
            , g.days_tot
            , g.plants
            , &g.lai
            , g.leaf_area);
        }
    else
        {
        throw std::invalid_argument ( "invalid template option in leaf_area_init");
        }

    pop_routine (myname);
    return;
    }


//+  Purpose
//       Leaf number initialisation

//+  Mission Statement
//     Initialise leaf number development

//+  Changes
//      270598 nih specified and programmed
void Plant::plant_leaf_no_init (int option)
    {
//+  Constant Values
    const char*  my_name = "plant_leaf_no_init" ;

//- Implementation Section ----------------------------------
    push_routine (my_name);

    // Plant leaf development
    if (option == 1)
        {
        // initialise total leaf number
        legopt_leaf_no_init1  (
            c.leaf_no_at_emerg
            , g.current_stage
            , emerg
            , g.days_tot
            , g.leaf_no
            , g.node_no);
        }
    else
        {
        throw std::invalid_argument ("invalid template option");
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Simulate actual crop leaf area development - checks that leaf area
//       development matches DM production.

//+  Mission Statement
//     Caculate actual crop leaf area development

//+  Changes
//      250894 jngh specified and programmed
void Plant::plant_leaf_area_actual (int option /* (INPUT) option number*/)
    {
//+  Constant Values
    const char*  my_name = "plant_leaf_area_actual" ;

//- Implementation Section ----------------------------------
    push_routine (my_name);

    if (option == 1)
        {
        // limit the delta leaf area by carbon supply
        cproc_leaf_area_actual1 (c.x_lai
                                 , c.y_sla_max
                                 , c.num_lai
                                 , g.dlt_dm_green[leaf]
                                 , &g.dlt_lai
                                 , g.dlt_lai_stressed
                                 , g.lai);
        }
    else
        {
        throw std::invalid_argument ("invalid template option");
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//         Calculate increase in pod area

//+  Mission Statement
//     Calculate increase in pod area

//+  Changes
//     280199 nih specified and programmed
void Plant::plant_pod_area (int option /* (INPUT) option number*/)
    {
//+  Constant Values
    const char*  my_name = "plant_pod_area" ;

//- Implementation Section ----------------------------------

    push_routine (my_name);

    if (option == 1)
        {
        g.dlt_pai = g.dlt_dm_green[pod] * c.spec_pod_area * smm2sm;
        }
    else
        {
        throw std::invalid_argument ("invalid template option");
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Simulate actual crop leaf area development - checks that leaf area
//       development matches DM production.

//+  Mission Statement
//     Simulate the actual leaf number development

//+  Changes
//      250894 jngh specified and programmed
void Plant::plant_leaf_no_actual (int option /* (INPUT) option number*/)
    {
//+  Constant Values
    const char*  my_name = "plant_leaf_no_actual" ;

//- Implementation Section ----------------------------------
    push_routine (my_name);

    if (option == 1)
        {
        // limit the delta leaf area by carbon supply
        cproc_leaf_no_actual1(c.num_lai_ratio
                             , c.x_lai_ratio
                             , c.y_leaf_no_frac
                             , g.dlt_lai
                             , g.dlt_lai_stressed
                             , &g.dlt_leaf_no
                             , g.dlt_leaf_no_pot
                             , &g.dlt_node_no
                             , g.dlt_node_no_pot);
        }
    else
        {
        throw std::invalid_argument ("invalid template option");
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Leaf number development

//+  Mission Statement
//     Calculate leaf number development

//+  Changes
//      250894 jngh specified and programmed
void Plant::plant_leaf_no_pot (int option /* (INPUT) option number*/)
    {
//+  Constant Values
    const char*  my_name = "plant_leaf_no_pot" ;

//- Implementation Section ----------------------------------
    push_routine (my_name);

    // Plant leaf development
    if (option == 1)
        {
        cproc_leaf_no_pot1(c.x_node_no_app
                           , c.y_node_app_rate
                           , c.num_node_no_app
                           , c.x_node_no_leaf
                           , c.y_leaves_per_node
                           ,  c.num_node_no_leaf
                           ,  g.current_stage
                           ,  emerg
                           ,  maturity
                           ,  emerg
                           , g.days_tot
                           , g.dlt_tt
                           , g.node_no
                           , &g.dlt_leaf_no_pot
                           , &g.dlt_node_no_pot);
        }
    else if (option == 2)
        {
        cproc_leaf_no_pot3  (c.x_node_no_app
                             , c.y_node_app_rate
                             , c.num_node_no_app
                             , c.x_node_no_leaf
                             , c.y_leaves_per_node
                             , c.num_node_no_leaf
                             , g.current_stage
                             , emerg
                             , maturity
                             , emerg
                             , g.dlt_tt
                             , g.node_no
                             , min(g.nfact_expansion, phosphorus->fact_expansion())
                             , g.swdef_expansion
                             , &g.leaves_per_node
                             , &g.dlt_leaf_no_pot
                             , &g.dlt_node_no_pot);
        }
    else
        {
        throw std::invalid_argument ("invalid template option");
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Initialise plant nitrogen.

//+  Mission Statement
//     Initialise plant nitrogen

//+  Changes
//      250894 jngh specified and programmed
void Plant::plant_nit_init (int option /* (INPUT) option number*/)
    {
//+  Constant Values
    const char*  my_name = "plant_nit_init" ;

//- Implementation Section ----------------------------------
    push_routine (my_name);

    if (option == 1)
        {
        plant_n_conc_grain_limits(c.n_conc_crit_grain
                                  , c.n_conc_max_grain
                                  , c.n_conc_min_grain
                                  , g.current_stage
                                  , g.dlt_dm_green_retrans
                                  , g.dlt_dm_green
                                  , g.dm_green
                                  , g.n_conc_crit
                                  , g.n_conc_max
                                  , g.n_conc_min);
        cproc_n_init1(c.n_init_conc
                     , max_part
                     , emerg
                     , g.current_stage
                     , g.days_tot
                     , g.dm_green
                     , g.n_green);
        }
    else
        {
        throw std::invalid_argument ("invalid template option");
        }

    pop_routine (my_name);
    return;
    }



//+  Purpose
//       Find nitrogen supply.

//+  Mission Statement
//     Get the nitrogen supply for plant

//+  Changes
//      250894 jngh specified and programmed
void Plant::plant_nit_supply (int option /* (INPUT) option number*/)
    {

//+  Constant Values
    const char*  my_name = "plant_nit_supply" ;

//+  Local Variables
    float biomass;

//- Implementation Section ----------------------------------
    push_routine (my_name);

// find potential N uptake (supply, available N)
    if (option == 1)
        {
        biomass = (sum_real_array (g.dm_green, max_part)- g.dm_green[root]) + g.dlt_dm;

        cproc_n_supply1 (g.dlayer
                         , max_layer
                         , g.dlt_sw_dep
                         , g.no3gsm
                         , g.no3gsm_min
                         , g.root_depth
                         , g.sw_dep
                         , g.no3gsm_mflow_avail
                         , g.sw_avail
                         , g.no3gsm_diffn_pot
                         , g.current_stage
                         , c.n_fix_rate
                         , biomass
                         , g.swdef_fixation
                         , &g.n_fix_pot);

        }
    else if (option == 2)
        {
        biomass = (sum_real_array (g.dm_green, max_part) - g.dm_green[root]) + g.dlt_dm;

        cproc_n_supply3 (g.dlayer
                         , max_layer
                         , g.no3gsm
                         , g.no3gsm_min
                         , g.no3gsm_uptake_pot
                         , g.root_depth
                         , g.root_length
                         , g.bd
                         , c.n_stress_start_stage
                         , c.total_n_uptake_max
                         , c.no3_uptake_max
                         , c.no3_conc_half_max
                         , g.sw_avail_pot
                         , g.sw_avail
                         , g.current_stage
                         , c.n_fix_rate
                         , biomass
                         , g.swdef_fixation
                         , &g.n_fix_pot);
        }
     else if (option == 3)
        {
        biomass = (sum_real_array (g.dm_green, max_part)- g.dm_green[root])
                    + g.dlt_dm;

        cproc_n_supply4 (g.dlayer
                             , g.bd
                             , max_layer
                             , g.no3gsm
                             , g.no3gsm_min
                             , g.no3gsm_uptake_pot
                             , g.nh4gsm
                             , g.nh4gsm_min
                             , g.nh4gsm_uptake_pot
                             , g.root_depth
                             , c.n_stress_start_stage
                             , c.kno3
                             , c.no3ppm_min
                             , c.knh4
                             , c.nh4ppm_min
                             , c.total_n_uptake_max
                             , g.sw_avail_pot
                             , g.sw_avail
                             , g.current_stage
                             , c.n_fix_rate
                             , biomass
                             , g.swdef_fixation
                             , &g.n_fix_pot);
        }
    else
        {
        throw std::invalid_argument ("invalid template N uptake option");
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Do nitrogen retranslocation.

//+  Mission Statement
//     Calculate nitrogen retranslocation

//+  Changes
//      250894 jngh specified and programmed
void Plant::plant_nit_retrans (int option/* (INPUT) option number*/)
    {
//+  Constant Values
    const char*  my_name = "plant_nit_retrans" ;

//- Implementation Section ----------------------------------
    push_routine (my_name);

    if (option == 1)
        {
        legnew_n_retranslocate(g.n_conc_crit
                               , g.n_conc_min
                               , g.dlt_dm_green
                               , g.dlt_dm_green_retrans
                               , g.dm_green
                               , g.n_conc_max
                               , g.n_green
                               , g.grain_n_demand
                               , g.dlt_n_retrans);
        }
     else if (option == 2)
        {
         plant_n_retranslocate( g.n_conc_crit
                               , g.n_conc_min
                               , c.n_retrans_fraction
                               , g.dm_green
                               , g.n_green
                               , g.soil_n_demand[meal]
                               , g.dlt_n_green
                               , g.dlt_n_retrans);

        }
    else
        {
        throw std::invalid_argument ("invalid n retrans option");
        }

    pop_routine (my_name);
    return;
    }

//  Purpose
//      Find grain nitrogen demand.
//
//  Mission Statement
//    Get the grain nitrogen demand
void Plant::plant_nit_grain_demand (int Option)
   {
   if (Option == 1)
      {
      plant_grain_n_demand1(c.sfac_slope
                            , c.sw_fac_max
                            , c.temp_fac_min
                            , c.tfac_slope
                            , g.maxt
                            , g.mint
                            , g.nfact_grain_conc
                            , g.n_conc_crit
                            , g.swdef_expansion
                            , g.n_conc_min
                            , g.dlt_dm_green
                            , g.dlt_dm_green_retrans
                            , g.dm_green
                            , g.n_conc_max
                            , g.n_green
                            , &g.grain_n_demand);
      }
   else if (Option == 2)
      {
       // start grain n filling immediately after flowering
      plant_grain_n_demand2(g.current_stage,
                            flowering,
                            start_grain_fill,
                            end_grain_fill,
                            g.grain_no,
                            c.potential_grain_n_filling_rate,
                            g.maxt,
                            g.mint,
                            c.x_temp_grain_n_fill,
                            c.y_rel_grain_n_fill,
                            c.num_temp_grain_n_fill,
                            g.n_conc_min,
                            g.n_conc_max,
                            g.dlt_dm_green,
                            g.dlt_dm_green_retrans,
                            g.dm_green,
                            g.n_green,
                            c.crit_grainfill_rate,
                            &g.grain_n_demand);
      }
    else
      {
      throw std::invalid_argument ("Invalid n demand option");
      }
}

//+  Purpose
//       Find nitrogen demand.

//+  Mission Statement
//     Get the plant nitrogen demand

//+  Changes
//      250894 jngh specified and programmed
void Plant::plant_nit_demand (int option /* (INPUT) option number*/)
    {
//+  Constant Values
    const int  num_demand_parts = 4 ;
    const char*  my_name = "plant_nit_demand" ;

//+  Local Variables
    int   demand_parts[num_demand_parts] = {root,leaf,stem,pod};

//- Implementation Section ----------------------------------
    push_routine (my_name);

    if (option == 1)
        {
        cproc_n_demand1(max_part
                        , demand_parts
                        , num_demand_parts
                        , g.dlt_dm
                        , g.dlt_dm_green
                        , g.dlt_dm_pot_rue
                        , g.dlt_n_retrans
                        , g.dm_green
                        , g.n_conc_crit
                        , g.n_conc_max
                        , g.n_green
                        , g.n_demand
                        , g.n_max);
         }
     else if (option == 2)
         {
         plant_n_demand(max_part
                        , demand_parts
                        , num_demand_parts
                        , meal
                        , g.dlt_dm
                        , g.dlt_dm_green
                        , g.dlt_dm_pot_rue
                        , g.dm_green
                        , g.n_conc_crit
                        , g.n_conc_max
                        , g.n_green
                        , g.grain_n_demand
                        , c.n_deficit_uptake_fraction
                        , g.n_demand
                        , g.n_max);
        }
    else
        {
        throw std::invalid_argument ("invalid n demand option");
        }

    pop_routine (my_name);
    return;
    }

//  Purpose
//      Find nitrogen demand.
//
//  Mission Statement
//    Get the plant nitrogen demand
void Plant::plant_soil_nit_demand (int Option)
{
   if (Option == 1) {
      plant_soil_n_demand1(g.soil_n_demand);
   } else {
      throw std::invalid_argument ("Invalid template option");
   }
}

//  Purpose
//      Find soil nitrogen demand.
//
//  Mission Statement
//    Get the plant nitrogen demand
void Plant::plant_soil_n_demand1 (float *g_soil_n_demand)
{
   for (int part = 0; part < max_part; part++)
       {
       g_soil_n_demand[part] = g.n_demand[part] - g.dlt_n_senesced_retrans[part];
       g_soil_n_demand[part] = l_bound(g_soil_n_demand[part],0.0);
       }
}

//+  Purpose
//       Find nitrogen uptake.

//+  Mission Statement
//     Get the plant nitrogen uptake

//+  Changes
//      250894 jngh specified and programmed
void Plant::plant_nit_uptake (int option/* (INPUT) option number*/)
    {
//+  Constant Values
    const char*  my_name = "plant_nit_uptake" ;

//- Implementation Section ----------------------------------
    push_routine (my_name);

    if (Str_i_Eq(p.uptake_source, "apsim"))
        {
        // NIH - note that I use a -ve conversion
        // factor FOR NOW to make it a delta.
        plant_get_ext_uptakes(p.uptake_source.c_str()
                             ,c.crop_type.c_str()
                             ,"no3"
                             ,-kg2gm/ha2sm
                             ,0.0
                             ,100.0
                             ,g.dlt_no3gsm
                             ,max_layer);   // uptake flag// crop type// uptake name// unit conversion factor// uptake lbound// uptake ubound// uptake array// array dim

        }
    else if (option == 1)
        {
        cproc_n_uptake1(c.no3_diffn_const
                       , g.dlayer
                       , max_layer
                       , g.no3gsm_diffn_pot
                       , g.no3gsm_mflow_avail
                       , g.n_fix_pot
                       , c.n_supply_preference.c_str()
                       , g.n_demand
                       , g.n_max
                       , max_part
                       , g.root_depth
                       , g.dlt_no3gsm);
        }
    else if ((option == 2) || (option == 3))
        {
        cproc_n_uptake3(g.dlayer
                        , max_layer
                        , g.no3gsm_uptake_pot
                        , g.nh4gsm_uptake_pot
                        , g.n_fix_pot
                        , c.n_supply_preference.c_str()
                        , g.soil_n_demand
                        , g.n_max
                        , max_part
                        , g.root_depth
                        , g.dlt_no3gsm
                        , g.dlt_nh4gsm);
        }
    else
        {
        throw std::invalid_argument ("invalid template option");
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Find nitrogen partitioning.

//+  Mission Statement
//     Calculate the nitrogen partitioning in the plant

//+  Changes
//      250894 jngh specified and programmed
void Plant::plant_nit_partition (int option /* (INPUT) option number*/)
    {
//+  Constant Values
    const char*  my_name = "plant_nit_partition" ;

//- Implementation Section ----------------------------------
    push_routine (my_name);

    if (option == 1)
        {
        legnew_n_partition(g.dlayer
                           , g.dlt_no3gsm
                           , g.dlt_nh4gsm
                           , g.soil_n_demand
                           , g.n_fix_pot
                           , g.n_max
                           , g.root_depth
                           , g.dlt_n_green
                           , &g.n_fix_uptake);

        }
    else
        {
        throw std::invalid_argument ("invalid template option");
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//         Get current Nitrogen stress factors (0-1)

//+  Mission Statement
//         Gets the current Nitrogen stress factors

//+  Changes
//     010994 jngh specified and programmed
//     250297 slw modified to split stress factors

void Plant::plant_nit_stress (int option /* (INPUT) option number*/)
    {
//+  Constant Values
    const char*  my_name = "plant_nit_stress" ;

//- Implementation Section ----------------------------------
    push_routine (my_name);

    if (option == 1)
        {

        crop_nfact_pheno(leaf,
                         stem,
                         g.dm_green,
                         g.n_conc_crit,
                         g.n_conc_min,
                         g.n_green,
                         c.n_fact_pheno,
                         &g.nfact_pheno);

        crop_nfact_photo(leaf, stem,
                         g.dm_green,
                         g.n_conc_crit,
                         g.n_conc_min,
                         g.n_green,
                         c.n_fact_photo, &g.nfact_photo);

        crop_nfact_expansion(leaf, g.dm_green,
                             g.n_conc_crit,
                             g.n_conc_min,
                             g.n_green,
                             c.n_fact_expansion,
                             &g.nfact_expansion);

        crop_nfact_grain_conc(leaf, stem,
                              g.dm_green,
                              g.n_conc_crit,
                              g.n_conc_min,
                              g.n_green, &g.nfact_grain_conc);
        }
    else if (option == 2)
        {
        crop_nfact_pheno(leaf, stem, g.dm_green,
                         g.n_conc_crit,
                         g.n_conc_min,
                         g.n_green,
                         c.n_fact_pheno,
                         &g.nfact_pheno);
///! Make option for n stress photo to be based on leaf n only
///!         call crop_nfact_photo(leaf, stem,
///!     .                     g%dm_green,
///!     .                     g%N_conc_crit,
///!     .                     g%N_conc_min,
///!     .                     g%N_green,
///!     .                     c%N_fact_photo, g%nfact_photo)

        // This call is really to calculate photo stress!!!!!
        crop_nfact_expansion(leaf, g.dm_green,
                              g.n_conc_crit,
                              g.n_conc_min,
                              g.n_green,
                              c.n_fact_photo,
                              &g.nfact_photo);
        crop_nfact_expansion(leaf, g.dm_green,
                             g.n_conc_crit,
                             g.n_conc_min,
                             g.n_green,
                             c.n_fact_expansion,
                             &g.nfact_expansion);
        crop_nfact_grain_conc(leaf, stem,
                              g.dm_green,
                              g.n_conc_crit,
                              g.n_conc_min,
                              g.n_green,
                              &g.nfact_grain_conc);
        }
    else
        {
        throw std::invalid_argument ("invalid template option");
        }

    pop_routine (my_name);
    return;
    }

//+  Purpose
//      Calculate an approximate nitrogen demand for today's growth.
//      The estimate basically = n to fill the plant up to maximum
//      nitrogen concentration.

//+  Mission Statement
//     Calculate nitrogen demand for growth

//+  Changes
//     14-05-1997 - huth - Programmed and Specified
void Plant::plant_nit_demand_est (int option)
    {
//+  Constant Values
    const int  num_demand_parts = 4 ;
//
    const char*  my_name = "plant_nit_demand_est" ;

//+  Local Variables
    float biomass;
    float dlt_dm_green_pot [max_part];            // potential (est) dlt dm green
    float dlt_n_retrans[max_part];                // retranslocated N
    float dm_green_tot;                           // total dm green
    int   part;                                   // simple plant part counter
    float n_demand[max_part];
    float n_max[max_part];
    float n_fix_pot;
    int   demand_parts[num_demand_parts]={root,leaf,stem,pod};

//- Implementation Section ----------------------------------
    push_routine (my_name);

    if (option == 1)
        {
// Option 1 is to assume that the distribution of plant
// C will be similar after today and so N demand is that
// required to raise all plant parts to max N conc.

// calculate potential new shoot and root growth
        dm_green_tot = sum_real_array (g.dm_green, max_part);

        for (part = 0; part < max_part; part++)
           {
           dlt_dm_green_pot[part] = g.dlt_dm_pot_rue
                                        * divide (g.dm_green[part], dm_green_tot, 0.0);
           dlt_n_retrans[part] = 0.0;
           }

        ///nh note g.dlt_dm_pot_rue becomes two of the subroutine arguments.;
        cproc_n_demand1 (max_part
                        , demand_parts
                        , num_demand_parts
                        , g.dlt_dm_pot_rue
                        , dlt_dm_green_pot
                        , g.dlt_dm_pot_rue
                        , dlt_n_retrans
                        , g.dm_green
                        , g.n_conc_crit
                        , g.n_conc_max
                        , g.n_green
                        , n_demand
                        , n_max);

        g.ext_n_demand = sum_real_array (n_demand,max_part);
        //nh  use zero growth value here so that estimated n fix is always <= actual;
        biomass = (sum_real_array (g.dm_green, max_part) - g.dm_green[root]);
        crop_n_fixation_pot1(g.current_stage
                             , c.n_fix_rate
                             , biomass
                             , g.swdef_fixation
                             , &n_fix_pot);

        if (Str_i_Eq(c.n_supply_preference,"active"))
            {
            // Nothing extra to do here
            }
        else if (Str_i_Eq(c.n_supply_preference,"fixation"))
            {
            // Remove potential fixation from demand term
            g.ext_n_demand = g.ext_n_demand - n_fix_pot;
            g.ext_n_demand = l_bound(g.ext_n_demand, 0.0);
            }
        else
            {
            throw std::invalid_argument ("bad n supply preference");
            }
        }
    else
        {
        throw std::invalid_argument ("invalid template option");
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//     Canopy height.

//+  Mission Statement
//     Calculate canopy height

//+  Changes
//     010994 jngh specified and programmed
void Plant::plant_height (int   option/*(INPUT) option number*/)
    {
//+  Constant Values
    const char*  my_name = "plant_height" ;

//- Implementation Section ----------------------------------

    push_routine (my_name);

    if (option == 1)
        {
        cproc_canopy_height(g.canopy_height
                            , p.x_stem_wt
                            , p.y_height
                            , p.num_stem_wt
                            , g.dm_green
                            , g.plants
                            , stem
                            , &g.dlt_canopy_height);
        }
    else
        {
        throw std::invalid_argument ("invalid template option");
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//     Canopy height.

//+  Mission Statement
//     Calculate canopy height

//+  Changes
//     010994 jngh specified and programmed
void Plant::plant_width (int   option/*(INPUT) option number*/)
    {
//+  Constant Values
    const char*  my_name = "plant_width" ;

//- Implementation Section ----------------------------------

    push_routine (my_name);

    if (option == 1)
        {
        plant_canopy_width (
            g.canopy_width
            , p.x_stem_wt
            , p.y_width
            , p.num_stem_wt
            , g.dm_green
            , g.plants
            , stem
            , &g.dlt_canopy_width
            );
        }
    else
        {
        throw std::invalid_argument ("invalid template option");
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Initialise Phenological Growth Stage Targets

//+  Mission Statement
//     Initialise plant growth phases

//+  Changes
//     240498 nih specified and programmed
void Plant::plant_phenology_init (int   option/*(INPUT) option number*/)
    {
    //+  Constant Values
    const char*  my_name = "plant_phenology_init" ;

    //- Implementation Section ----------------------------------

    push_routine (my_name);

    if (option == 1)
        {
        // initialise phenology phase targets
        legnew_phenology_init(c.shoot_lag
                              , c.shoot_rate
                              , g.maxt
                              , g.mint
                              , c.x_vernal_temp
                              , c.y_vernal_days
                              , c.num_vernal_temp
                              , &g.cum_vernal_days
                              , p.cum_vernal_days
                              , p.tt_emerg_to_endjuv
                              , p.num_cum_vernal_days
                              , c.twilight
                              , g.current_stage
                              , g.days_tot
                              , g.day_of_year
                              , g.year
                              , g.latitude
                              , g.sowing_depth
                              , p.x_pp_endjuv_to_init
                              , p.y_tt_endjuv_to_init
                              , p.num_pp_endjuv_to_init
                              , p.x_pp_init_to_flower
                              , p.y_tt_init_to_flower
                              , p.num_pp_init_to_flower
                              , p.x_pp_flower_to_start_grain
                              , p.y_tt_flower_to_start_grain
                              , p.num_pp_flower_to_start_grain
                              , p.x_pp_start_to_end_grain
                              , p.y_tt_start_to_end_grain
                              , p.num_pp_start_to_end_grain
                              , p.tt_end_grain_to_maturity
                              , p.tt_maturity_to_ripe
                              , p.est_days_emerg_to_init
                              , g.phase_tt);
        }
    else if (option == 2)
        {
        wheat_phenology_init_nwheat(c.shoot_lag
                                    , c.shoot_rate
                                    , g.current_stage
                                    , g.days_tot
                                    , g.sowing_depth
                                    , g.phase_tt
                                    , p.startgf_to_mat
                                    , p.phyllochron);
        }
    else if (option == 3)
        {
        // initialise phenology phase targets
        legnew_phenology_init(c.shoot_lag
                              , c.shoot_rate
                              , g.maxt
                              , g.mint
                              , c.x_vernal_temp
                              , c.y_vernal_days
                              , c.num_vernal_temp
                              , &g.cum_vernal_days
                              , p.cum_vernal_days
                              , p.tt_emerg_to_endjuv
                              , p.num_cum_vernal_days
                              , c.twilight
                              , g.current_stage
                              , g.days_tot
                              , g.day_of_year
                              , g.year
                              , g.latitude
                              , g.sowing_depth
                              , p.x_pp_endjuv_to_init
                              , p.y_tt_endjuv_to_init
                              , p.num_pp_endjuv_to_init
                              , p.x_pp_init_to_flower
                              , p.y_tt_init_to_flower
                              , p.num_pp_init_to_flower
                              , p.x_pp_flower_to_start_grain
                              , p.y_tt_flower_to_start_grain
                              , p.num_pp_flower_to_start_grain
                              , p.x_pp_start_to_end_grain
                              , p.y_tt_start_to_end_grain
                              , p.num_pp_start_to_end_grain
                              , p.tt_end_grain_to_maturity
                              , p.tt_maturity_to_ripe
                              , p.est_days_emerg_to_init
                              , g.phase_tt);

        plant_fruit_phenology_init(c.twilight
                               , g.current_fruit_stage
                               , g.fruit_days_tot
                               , max_fruit_stage
                               , max_fruit_cohorts
                               , g.num_fruit_cohorts
                               , g.day_of_year
                               , g.latitude
                               , p.x_pp_flower_to_start_grain
                               , p.y_tt_flower_to_start_grain
                               , p.num_pp_flower_to_start_grain
                               , p.x_pp_fruit_start_to_end_grain
                               , p.y_tt_fruit_start_to_end_grain
                               , p.num_pp_fruit_start_to_end_grain
                               , p.tt_end_grain_to_maturity
                               , p.tt_maturity_to_ripe
                               , g.fruit_phase_tt);
        }
    else
        {
        throw std::invalid_argument ("invalid template option");
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//     Use temperature, photoperiod and genetic characteristics
//     to determine when the crop begins a new growth phase.
//     The initial daily thermal time and height are also set.

//+  Mission Statement
//     Calculate the plant growth stages

//+  Changes
//     010994 jngh specified and programmed
void Plant::plant_phenology (int   option/*(INPUT) option number*/)
    {

//+  Local Variables
    float photoperiod;

//+  Constant Values
    const char*  my_name = "plant_phenology" ;

//- Implementation Section ----------------------------------

    push_routine (my_name);

    if (option == 1)
        {
        plant_phenology3 (&g.previous_stage
                          ,&g.current_stage
                          ,sowing
                          ,germ
                          ,start_grain_fill
                          ,harvest_ripe
                          ,emerg
                          ,flowering
                          ,max_stage
                          ,c.num_temp
                          ,c.x_temp
                          ,c.y_tt
                          ,g.maxt
                          ,g.mint
                          ,min(g.nfact_pheno, phosphorus->fact_pheno())
                          ,g.swdef_pheno
                          ,g.swdef_pheno_flower
                          ,g.swdef_pheno_grainfill
                          ,c.pesw_germ
                          ,c.fasw_emerg
                          ,c.rel_emerg_rate
                          ,c.num_fasw_emerg
                          ,g.dlayer
                          ,max_layer
                          ,g.sowing_depth
                          ,g.sw_dep
                          ,g.dul_dep
                          ,p.ll_dep
                          ,&g.dlt_tt
                          ,g.phase_tt
                          ,&g.phase_devel
                          ,&g.dlt_stage
                          ,g.tt_tot
                          ,g.days_tot);
        // update thermal time states and day count
        accumulate (g.dlt_tt, g.tt_tot
                   ,g.previous_stage-1.0, g.dlt_stage);

        accumulate (1.0, g.days_tot
                   ,g.previous_stage-1.0, g.dlt_stage);
        }
    else if (option == 2)
        {
        wheat_vernaliz_days_nwheat (g.current_stage
                                   ,germ
                                   ,floral_init
                                   ,g.maxt
                                   ,g.mint
                                   ,0.0
                                   ,&g.dlt_cumvd
                                   ,g.cumvd);

        wheat_vernaliz_effect_nwheat(g.current_stage
                                     ,emerg
                                     ,floral_init
                                     ,p.vern_sens
                                     ,g.cumvd
                                     ,g.dlt_cumvd
                                     ,50.0
                                     ,&g.vern_eff);  //maximum vernalisation requirement is 50 days

        g.cumvd = g.cumvd + g.dlt_cumvd;

        photoperiod = day_length (g.day_of_year,g.latitude,c.twilight);

        wheat_photoperiod_effect(g.current_stage,
                                 emerg,
                                 floral_init,
                                 photoperiod,
                                 p.photop_sens,
                                 &g.photop_eff);

        cproc_phenology_nw (&g.previous_stage
                            ,&g.current_stage
                            ,sowing
                            ,germ
                            ,harvest_ripe
                            ,emerg
                            ,flowering
                            ,start_grain_fill
                            ,max_stage
                            ,c.num_temp
                            ,c.x_temp
                            ,c.y_tt
                            ,g.maxt
                            ,g.mint
                            ,min(g.nfact_pheno, phosphorus->fact_pheno())
                            ,g.swdef_pheno
                            ,g.swdef_pheno_flower
                            ,g.swdef_pheno_grainfill
                            ,g.vern_eff
                            ,g.photop_eff
                            ,c.pesw_germ
                            ,c.fasw_emerg
                            ,c.rel_emerg_rate
                            ,c.num_fasw_emerg
                            ,g.dlayer
                            ,max_layer
                            ,g.sowing_depth
                            ,g.sw_dep
                            ,g.dul_dep
                            ,p.ll_dep
                            ,&g.dlt_tt
                            ,g.phase_tt
                            ,&g.phase_devel
                            ,&g.dlt_stage
                            ,g.tt_tot
                            ,g.days_tot);
        //fprintf(stdout, "%d, %f, %f, %f\n", g.day_of_year, g.maxt, g.mint, g.dlt_stage);
        }
      else if (option == 3)
         {
         plant_phenology3 (&g.previous_stage
                          ,&g.current_stage
                          ,sowing
                          ,germ
                          ,start_grain_fill
                          ,harvest_ripe
                          ,emerg
                          ,flowering
                          ,max_stage
                          ,c.num_temp
                          ,c.x_temp
                          ,c.y_tt
                          ,g.maxt
                          ,g.mint
                          ,min(g.nfact_pheno, phosphorus->fact_pheno())
                          ,g.swdef_pheno
                          ,g.swdef_pheno_flower
                          ,g.swdef_pheno_grainfill
                          ,c.pesw_germ
                          ,c.fasw_emerg
                          ,c.rel_emerg_rate
                          ,c.num_fasw_emerg
                          ,g.dlayer
                          ,max_layer
                          ,g.sowing_depth
                          ,g.sw_dep
                          ,g.dul_dep
                          ,p.ll_dep
                          ,&g.dlt_tt
                          ,g.phase_tt
                          ,&g.phase_devel
                          ,&g.dlt_stage
                          ,g.tt_tot
                          ,g.days_tot);

         plant_fruit_cohort_init (
                      initial_fruit_stage
                    , g.current_stage
                    , g.days_tot
                    , g.current_fruit_stage
                    , &g.num_fruit_cohorts );

         plant_fruit_phenology (g.previous_fruit_stage
                               ,g.current_fruit_stage
                               ,initial_fruit_stage
                               ,harvest_ripe
                               ,start_grain_fill
                               ,end_grain_fill
                               ,max_fruit_stage
                               ,max_fruit_cohorts
                               ,g.num_fruit_cohorts
                               ,g.dm_fruit_green
                               ,p.dm_fruit_max
                               ,g.fruit_no
                               ,c.num_temp
                               ,c.x_temp
                               ,c.y_tt
                               ,g.maxt
                               ,g.mint
                               ,g.swdef_pheno_flower
                               ,g.swdef_pheno_grainfill
                               ,g.fruit_phase_tt
                               ,g.fruit_phase_devel
                               ,g.dlt_fruit_tt
                               ,g.dlt_fruit_stage
                               ,g.fruit_tt_tot
                               ,g.fruit_days_tot);

         plant_fruit_phenology_update (g.previous_stage
                                      ,&g.current_stage
                                      ,g.current_fruit_stage
                                      ,initial_fruit_stage
                                      ,plant_end
                                      ,start_grain_fill
                                      ,max_fruit_cohorts
                                      ,g.num_fruit_cohorts
                                      ,c.fruit_phen_end
                                      ,g.fruit_no
                                      ,&g.dlt_stage);

         // update thermal time states and day count
         accumulate (g.dlt_tt, g.tt_tot, g.previous_stage-1.0, g.dlt_stage);
         accumulate (1.0, g.days_tot, g.previous_stage-1.0, g.dlt_stage);
        }
    else
        {
        throw std::invalid_argument ("invalid template option");
        }

    if (sum_real_array(g.days_tot, max_stage) > 0)
    {
       g.das += 1;
    }
    else
    {
       g.das = 0;
    }

    pop_routine (my_name);
    return;
    }

//+  Purpose
//     Use temperature, photoperiod and genetic characteristics
//     to determine when the crop begins a new growth phase.
//     The initial daily thermal time and height are also set.

//+  Changes
//      091294 jngh specified and programmed
void Plant::plant_fruit_cohort_number (int option)
    {
    if (option == 1)
       {
       g.num_fruit_cohorts = 0.0;
       }
    else if (option == 2)
       {
       plant_fruit_cohort_init (
                     initial_fruit_stage
                   , g.current_stage
                   , g.days_tot
                   , g.current_fruit_stage
                   , &g.num_fruit_cohorts );
       }
    else
       {
       throw std::invalid_argument ("Invalid template option");
       }
    }


//+  Purpose
//       Simulate plant senescence.

//+  Mission Statement
//     Calculate plant senescence

//+  Changes
//      091294 jngh specified and programmed
void Plant::plant_sen_bio (int dm_senescence_option)
    {

//+  Constant Values
    const char*  my_name = "plant_sen_bio" ;

//+  Local Variables
    float canopy_sen_fr;                          // fraction of canopy senescing

//- Implementation Section ----------------------------------
    push_routine (my_name);

    if (dm_senescence_option == 1)
        {
        canopy_sen_fr = divide (g.dlt_slai, g.lai + g.dlt_lai, 0.0);

        cproc_dm_senescence1 (max_part
                              , max_table
                              , canopy_sen_fr
                              , c.x_dm_sen_frac
                              , c.y_dm_sen_frac
                              , c.num_dm_sen_frac
                              , g.dm_green
                              , g.dlt_dm_green
                              , g.dlt_dm_green_retrans
                              , g.dlt_dm_senesced);
         }
    else if (dm_senescence_option == 2)
         {
         canopy_sen_fr = divide (g.dlt_slai, g.lai + g.dlt_lai, 0.0);
         plant_dm_senescence (max_part
                              , max_table
                              , canopy_sen_fr
                              , c.x_dm_sen_frac
                              , c.y_dm_sen_frac
                              , c.num_dm_sen_frac
                              , g.dm_green
                              , g.dlt_dm_senesced);
        }
      else if (dm_senescence_option == 3)
        {
        canopy_sen_fr = divide (g.dlt_slai, g.lai + g.dlt_lai, 0.0);

        cproc_dm_senescence1 (max_part
                              , max_table
                              , canopy_sen_fr
                              , c.x_dm_sen_frac
                              , c.y_dm_sen_frac
                              , c.num_dm_sen_frac
                              , g.dm_green
                              , g.dlt_dm_green
                              , g.dlt_dm_green_retrans
                              , g.dlt_dm_senesced);
        for (int part = 0; part < max_part; part++)
          g.dlt_dm_fruit_senesced[0][part] = g.dlt_dm_senesced[part];
        }
    else
        {
        throw std::invalid_argument ("invalid template option in plant_sen_bio");
        }
    pop_routine (my_name);
    }



//+  Purpose
//       Simulate plant nitrogen senescence.

//+  Mission Statement
//     Calculate plant nitrogen senescence

//+  Changes
//      091294 jngh specified and programmed
void Plant::plant_sen_nit (int   option/*(INPUT) option number*/)
    {

//+  Constant Values
    const char*  my_name = "plant_sen_nit" ;

//- Implementation Section ----------------------------------
    push_routine (my_name);

    if (option == 1)
        {
        legnew_n_senescence1 (max_part
          , c.n_sen_conc
          , g.dlt_dm_senesced
          , g.n_green
          , g.dm_green
          , g.dlt_n_senesced_trans
          , g.dlt_n_senesced);
        }
    else if (option == 2)
        {
        plant_N_senescence (max_part
                             , c.n_sen_conc
                             , g.n_conc_max
                             , g.dlt_dm_senesced
                             , g.n_green
                             , g.dm_green
                             , g.n_demand
                             , g.dlt_n_senesced_trans
                             , g.dlt_n_senesced_retrans
                             , g.dlt_n_senesced);
        }
    else
        {
        throw std::invalid_argument ("invalid sen nit option");
        }

    pop_routine (my_name);
    return;
    }



//+  Purpose
//       Return the fractional death of oldest green leaf.

//+  Mission Statement
//     Get the fractional death of oldest green leaf

//+  Changes
//     010994 jngh specified and programmed

void Plant::plant_leaf_death (int   option/*(INPUT) option number*/)
    {
//+  Constant Values
    const char*  my_name = "plant_leaf_death" ;

//- Implementation Section ----------------------------------

    push_routine (my_name);

    if (option == 1)
        {
        legnew_leaf_death_leg(c.sen_start_stage
                              , c.fr_lf_sen_rate
                              , c.node_sen_rate
                              , min(g.nfact_expansion, phosphorus->fact_expansion())
                              , c.n_fact_lf_sen_rate
                              , g.current_stage
                              , g.dlt_tt
                              , g.leaf_no
                              , g.leaf_no_dead
                              , g.leaf_area
                              , c.min_tpla
                              , &g.dlt_leaf_no_dead);
        }
    else
        {
        throw std::invalid_argument ("invalid template option");
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Return the lai that senesces on the current day.

//+  Mission Statement
//     Calculate today's senesced leaf area index

//+  Changes
//     200498 nih specified and programmed
void Plant::plant_leaf_area_sen (int   option/*(INPUT) option number*/)
    {
//+  Constant Values
    const char*  my_name = "plant_leaf_area_sen" ;

//- Implementation Section ----------------------------------

    push_routine (my_name);

// get senescense from age

    if (option == 1)
        {
        legopt_leaf_area_sen1  ( emerg
                                , now
                                , g.dlt_lai_stressed
                                , g.dlt_leaf_no
                                , g.dlt_leaf_no_dead
                                , g.lai
                                , g.leaf_area
                                , g.leaf_no
                                , g.leaf_no_dead
                                , max_node
                                , g.plants
                                , g.slai
                                , c.min_tpla
                                , &g.dlt_slai_age
                                , c.lai_sen_light
                                , c.sen_light_slope
                                , &g.dlt_slai_light
                                , c.sen_rate_water
                                , g.swdef_photo
                                , &g.dlt_slai_water
                                , c.x_temp_senescence
                                , c.y_senescence_fac
                                , c.num_temp_senescence
                                , g.mint
                                , &g.dlt_slai_frost
                                , &g.dlt_slai );
        }
    else
        {
        throw std::invalid_argument ("invalid template option");
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       cleanup after crop processes

//+  Mission Statement
//     Cleanup the variables after crop processes

//+  Changes
//      250894 jngh specified and programmed
void Plant::plant_cleanup ()
    {
    const char*  my_name = "plant_cleanup" ;

    push_routine (my_name);
    plant_update(c.stage_code_list
                , g.phase_tt
                , g.tt_tot
                , c.n_conc_crit_grain
                , c.n_conc_crit_root
                , c.n_conc_max_grain
                , c.n_conc_max_root
                , c.n_conc_min_grain
                , c.n_conc_min_root
                , c.x_stage_code
                , c.y_n_conc_crit_leaf
                , c.y_n_conc_crit_pod
                , c.y_n_conc_crit_stem
                , c.y_n_conc_max_leaf
                , c.y_n_conc_max_pod
                , c.y_n_conc_max_stem
                , c.y_n_conc_min_leaf
                , c.y_n_conc_min_pod
                , c.y_n_conc_min_stem
                , c.x_co2_nconc_modifier
                , c.y_co2_nconc_modifier
                , c.num_co2_nconc_modifier
                , g.co2
                , g.row_spacing
                , g.skip_row_fac
                , g.skip_plant_fac
                , c.x_row_spacing
                , c.y_extinct_coef
                , c.y_extinct_coef_dead
                , c.num_row_spacing
                , &g.canopy_height
                , &g.canopy_width
                , g.cnd_grain_conc
                , g.cnd_photo
                , &g.cover_dead
                , &g.cover_green
                , &g.cover_sen
                , g.cswd_expansion
                , g.cswd_pheno
                , g.cswd_photo
                , g.current_stage
                , g.dlt_canopy_height
                , g.dlt_canopy_width
                , g.dlt_dm
                , g.dlt_dm_dead_detached
                , g.dlt_dm_detached
                , g.dlt_dm_green
                , g.dlt_dm_green_retrans
                , g.dlt_dm_senesced
                , g.dlt_dm_stress_max
                , g.dlt_heat_stress_tt
                , g.dlt_lai
                , g.dlt_leaf_no
                , g.dlt_node_no
                , g.dlt_leaf_no_dead
                , g.dlt_n_dead_detached
                , g.dlt_n_detached
                , g.dlt_n_green
                , g.dlt_n_retrans
                , g.dlt_n_senesced
                , g.dlt_n_senesced_trans
                , g.dlt_n_senesced_retrans
                , g.dlt_plants
                , g.dlt_root_depth
                , g.dlt_slai
                , g.dlt_slai_detached
                , g.dlt_stage
                , g.dlt_tlai_dead_detached
                , g.dm_dead
                , g.dm_green
                , g.dm_plant_top_tot
                , g.dm_senesced
                , g.dm_stress_max
                , &g.grain_no
                , g.heat_stress_tt
                , &g.lai
                , &g.lai_canopy_green
                , g.leaf_area
                , g.leaf_no
                , g.node_no
                , g.leaf_no_dead
                , g.nfact_grain_conc
                , g.nfact_photo
                , g.n_conc_crit
                , g.n_conc_max
                , g.n_conc_min
                , g.n_dead
                , g.n_green
                , g.n_senesced
                , &g.plants
                , g.previous_stage
                , &g.root_depth
                , &g.slai
                , g.swdef_expansion
                , g.swdef_pheno
                , g.swdef_photo
                , &g.tlai_dead
                , g.dlt_root_length_dead
                , g.root_length_dead
                , g.root_length
                , g.dlt_root_length
                , g.dlt_root_length_senesced
                , &g.pai
                , g.dlt_pai
                , c.extinct_coef_pod
                , &g.cover_pod
                , p.num_canopy_widths);

    plant_check_bounds(g.canopy_height
                       , g.cover_dead
                       , g.cover_green
                       , g.cover_sen
                       , g.current_stage
                       , g.days_tot
                       , g.dlayer
                       , g.dm_dead
                       , g.dm_green
                       , g.dm_senesced
                       , g.dm_stress_max
                       , g.grain_no
                       , g.heat_stress_tt
                       , g.lai
                       , g.leaf_area
                       , g.leaf_no
                       , g.node_no
                       , g.leaf_no_dead
                       , g.n_conc_crit
                       , g.n_conc_max
                       , g.n_conc_min
                       , g.n_dead
                       , g.n_green
                       , g.n_senesced
                       , g.phase_tt
                       , g.plants
                       , g.root_depth
                       , g.slai
                       , g.tlai_dead
                       , g.tt_tot
                       );
    plant_totals( g.current_stage
                , g.days_tot
                , g.day_of_year
                , g.dlayer
                , g.dlt_n_retrans
                , g.dlt_sw_dep
                , g.dm_green
                , &g.flowering_date
                , &g.flowering_das
                , &g.lai
                , &g.lai_max
                , &g.maturity_date
                , &g.maturity_das
                , &g.n_conc_act_stover_tot
                , g.n_conc_crit
                , &g.n_conc_crit_stover_tot
                , g.n_dead
                , g.n_demand
                , &g.n_demand_tot
                , g.n_green
                , g.n_senesced
                , &g.n_uptake_grain_tot
                , &g.n_uptake_stover_tot
                , &g.n_uptake_tot
                , g.dlt_n_green
                , &g.n_fix_uptake
                , &g.n_fixed_tops
                , &g.root_depth
                , &g.transpiration_tot );
    if (g.plant_status == alive &&
        g.current_stage != g.previous_stage) {
        plant_event(g.current_stage
                    , g.dlayer
                    , g.dm_dead
                    , g.dm_green
                    , g.dm_senesced
                    , g.lai
                    , g.n_green
                    , g.root_depth
                    , g.sw_dep
                    , p.ll_dep);
        }
    else
        {
        }
    plant_check_leaf_record();

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Check that leaf records agree

//+  Mission Statement
//     Check that leaf records agree

//+  Changes
//      050199 nih specified and programmed
void Plant::plant_check_leaf_record ()
    {
    float leaf_area_tot;
    int   node;

    const char*  my_name = "plant_check_leaf_record" ;

    push_routine (my_name);

    leaf_area_tot = sum_real_array (g.leaf_area, max_node)
                       * g.plants * smm2sm;
//      print*, 'leaf_area_tot, g%lai, g%slai,g%lai+g%slai'
//      print*, leaf_area_tot, g%lai, g%slai,g%lai+g%slai
//      if (leaf_area_tot .ne.(g%lai+g%slai)) then
//      print*, '*** leaf_area_tot-(g%lai+g%slai)'
//      print*, leaf_area_tot-(g%lai+g%slai)
//      else
//      endif
    if (! reals_are_equal (leaf_area_tot, g.lai + g.slai, tolerance_lai))
      {
      ostrstream msg;
      msg << "Bad record for total leaf area. Leaf area total = ";
      msg <<  leaf_area_tot << ". Lai total = " <<  g.lai + g.slai << ends;
      throw std::runtime_error (msg.str());
      }

    leaf_area_tot = 0.0;
    for (node = 0; node < max_node; node++)
      {
      leaf_area_tot = leaf_area_tot +
                  divide (g.leaf_no_dead[node],g.leaf_no[node],0.0)
                     * g.leaf_area[node]
                     * g.plants * smm2sm;
      }
    //nh invalid comparison as senescence may exceed leaf death by age.;
    //c      if (.not.reals_are_equal(leaf_area_tot,g.slai))then;
    //c         call fatal_error (&err_internal,;
    //c     :      "bad record for senesced leaf area");
    //c         print*,"slai=",g.slai;
    //c         print*,"sen_leaf_area_tot=",leaf_area_tot;
    //c         pause;
    //c      else;
    //c         print*,"senesced leaf record ok";
    //c      endif;

    if (sum_real_array(g.leaf_no_dead, max_node) >
        sum_real_array(g.leaf_no, max_node))
        {
        throw std::runtime_error ("bad record for dead leaf number");
        }


    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Update states

//+  Mission Statement
//     Update states of variables

//+  Changes
//      250894 jngh specified and programmed
void Plant::plant_update
    (
     float *c_stage_code_list                      // (INPUT)  list of stage numbers
    ,float *g_phase_tt                             // (INPUT)  Cumulative growing degree days
    ,float *g_tt_tot                               // (INPUT)  the sum of growing degree days
    ,float  c_n_conc_crit_meal                     // (INPUT)  critical N concentration of gr
    ,float  c_n_conc_crit_root                     // (INPUT)  critical N concentration of ro
    ,float  c_n_conc_max_meal                      // (INPUT)  maximum N concentration of gra
    ,float  c_n_conc_max_root                      // (INPUT)  maximum N concentration of roo
    ,float  c_n_conc_min_meal                      // (INPUT)  minimum N concentration of gra
    ,float  c_n_conc_min_root                      // (INPUT)  minimum N concentration of roo
    ,float *c_x_stage_code                         // (INPUT)  stage table for N concentratio
    ,float *c_y_n_conc_crit_leaf                   // (INPUT)  critical N concentration of
    ,float *c_y_n_conc_crit_pod                    // (INPUT)  critical N concentration of p
    ,float *c_y_n_conc_crit_stem                   // (INPUT)  critical N concentration of
    ,float *c_y_n_conc_max_leaf                    // (INPUT)  maximum N concentration of le
    ,float *c_y_n_conc_max_pod                     // (INPUT)  maximum N concentration of pod
    ,float *c_y_n_conc_max_stem                    // (INPUT)  maximum N concentration of st
    ,float *c_y_n_conc_min_leaf                    // (INPUT)  minimum N concentration of le
    ,float *c_y_n_conc_min_pod                     // (INPUT)  minimum N concentration of pod
    ,float *c_y_n_conc_min_stem                    // (INPUT)  minimum N concentration of st
    ,float *c_x_co2_nconc_modifier
    ,float *c_y_co2_nconc_modifier
    ,int   c_num_co2_nconc_modifier
    ,float g_co2
    ,float  g_row_spacing                          // (INPUT)  row spacing (m) [optional]
    ,float  g_skip_row_fac                         // skip row factor
    ,float  g_skip_plant_fac                       // skip plant factor
    ,float *c_x_row_spacing
    ,float *c_y_extinct_coef
    ,float *c_y_extinct_coef_dead
    ,int    c_num_row_spacing
    ,float  *g_canopy_height                              // (out/INPUT)  canopy height (mm)
    ,float  *g_canopy_width                               // (INPUT)  canopy width (mm)
    ,float *g_cnd_grain_conc                             // (INPUT)  cumulative nitrogen stress typ
    ,float *g_cnd_photo                                  // (INPUT)  cumulative nitrogen stress typ
    ,float  *g_cover_dead                                 // (out/INPUT)  fraction of radiation reaching
    ,float  *g_cover_green                                // (out/INPUT)  fraction of radiation reaching
    ,float  *g_cover_sen                                  // (out/INPUT)  fraction of radiation reaching
    ,float *g_cswd_expansion                             // (INPUT)  cumulative water stress type 2
    ,float *g_cswd_pheno                                 // (INPUT)  cumulative water stress type 3
    ,float *g_cswd_photo                                 // (INPUT)  cumulative water stress type 1
    ,float  g_current_stage                              // (INPUT)  current phenological stage
    ,float  g_dlt_canopy_height                                // (INPUT)  change in canopy height (mm)
    ,float  g_dlt_canopy_width                                 // (INPUT)  change in canopy width (mm)
    ,float  g_dlt_dm                                           // (INPUT)  the daily biomass production (
    ,float *g_dlt_dm_dead_detached                             // (INPUT)  plant biomass detached fro
    ,float *g_dlt_dm_detached                                  // (INPUT)  plant biomass detached (g/m^2)
    ,float *g_dlt_dm_green                                     // (INPUT)  plant biomass growth (g/m^2)
    ,float *g_dlt_dm_green_retrans                             // (INPUT)  plant biomass retranslocat
    ,float *g_dlt_dm_senesced                                  // (INPUT)  plant biomass senescence (g/m^
    ,float  g_dlt_dm_stress_max                                // (INPUT)  maximum daily stress on dm pro
    ,float  g_dlt_heat_stress_tt                               // (INPUT)  change in heat stress accumula
    ,float  g_dlt_lai                                          // (INPUT)  actual change in live plant la
    ,float  g_dlt_leaf_no                                      // (INPUT)  actual fraction of oldest leaf
    ,float  g_dlt_node_no                                      // (INPUT)  actual fraction of oldest node
    ,float  g_dlt_leaf_no_dead                                 // (INPUT)  fraction of oldest green leaf
    ,float *g_dlt_n_dead_detached                              // (INPUT)  actual N loss with detached
    ,float *g_dlt_n_detached                                   // (INPUT)  actual N loss with detached pl
    ,float *g_dlt_n_green                                      // (INPUT)  actual N uptake into plant (g/
    ,float *g_dlt_n_retrans                                    // (INPUT)  nitrogen retranslocated out fr
    ,float *g_dlt_n_senesced                                   // (INPUT)  actual N loss with senesced pl
    ,float *g_dlt_n_senesced_trans                             //  ??
    ,float *g_dlt_n_senesced_retrans                           //  ??
    ,float  g_dlt_plants                                       // (INPUT)  change in Plant density (plant
    ,float  g_dlt_root_depth                                   // (INPUT)  increase in root depth (mm)
    ,float  g_dlt_slai                                         // (INPUT)  area of leaf that senesces fro
    ,float  g_dlt_slai_detached                                // (INPUT)  plant senesced lai detached
    ,float  g_dlt_stage                                        // (INPUT)  change in stage number
    ,float  g_dlt_tlai_dead_detached                           // (INPUT)  plant lai detached from dea
    ,float *g_dm_dead                                          // (INPUT)  dry wt of dead plants (g/m^2)
    ,float *g_dm_green                                         // (INPUT)  live plant dry weight (biomass
    ,float *g_dm_plant_top_tot                                 // (INPUT)  total carbohydrate production
    ,float *g_dm_senesced                                      // (INPUT)  senesced plant dry wt (g/m^2)
    ,float *g_dm_stress_max                                    // (INPUT)  sum of maximum daily stress on
    ,float  *g_grain_no                                         // (out/INPUT)  grain number (grains/plant)
    ,float *g_heat_stress_tt                                   // (INPUT)  heat stress cumulation in each
    ,float  *g_lai                                              // (out/INPUT)  live plant green lai
    ,float *g_lai_canopy_green                                 // (out/INPUT)  live plant green lai in canopy
    ,float *g_leaf_area                                        // (INPUT)  leaf area of each leaf (mm^2)
    ,float *g_leaf_no                                          // (INPUT)  number of fully expanded leave
    ,float *g_node_no                                          // (INPUT)  number of fully expanded nodes
    ,float *g_leaf_no_dead                                     // (INPUT)  no of dead leaves ()
    ,float  g_nfact_grain_conc                                 // (INPUT)
    ,float  g_nfact_photo                                      // (INPUT)
    ,float *g_n_conc_crit                                      // (out/INPUT)  critical N concentration (g N/
    ,float *g_n_conc_max                                       // (out/INPUT)  maximum N concentration (g N/g
    ,float *g_n_conc_min                                       // (out/INPUT)  minimum N concentration (g N/g
    ,float *g_n_dead                                           // (INPUT)  plant N content of dead plants
    ,float *g_n_green                                          // (INPUT)  plant nitrogen content (g N/m^
    ,float *g_n_senesced                                       // (INPUT)  plant N content of senesced pl
    ,float *g_plants                                           // (out/INPUT)  Plant density (plants/m^2)
    ,float g_previous_stage                                     // (INPUT)  previous phenological stage
    ,float *g_root_depth                                         // (out/INPUT)  depth of roots (mm)
    ,float *g_slai                                               // (INPUT)  area of leaf that senesces fro
    ,float g_swdef_expansion                                    // (INPUT)
    ,float g_swdef_pheno                                        // (INPUT)
    ,float g_swdef_photo                                        // (INPUT)
    ,float *g_tlai_dead                                          // (INPUT)  total lai of dead plants
    ,float *g_dlt_root_length_dead                                  // (INPUT)  Change in Root length of dead population in each layer
    ,float *g_root_length_dead                                  // (INPUT)  Root length of dead population in each layer
    ,float *g_root_length                                       // (INPUT)  Root length in each layer
    ,float *g_dlt_root_length                                   // (INPUT)  Root growth in each layer
    ,float *g_dlt_root_length_senesced
    ,float *g_pai
    ,float g_dlt_pai
    ,float c_extinct_coef_pod
    ,float *g_cover_pod
    ,int   p_num_canopy_widths
    ) {

//+  Constant Values
    const char*  my_name = "plant_update" ;

//+  Local Variables
    double dlt_dm_plant;                           // dry matter increase (g/plant)
    float dlt_leaf_area;                          // leaf area increase (mm^2/plant)
    float dlt_leaf_dm;                            // leaf dm increase (g/plant)
    double dlt_dm_green_dead;                      // dry matter of green plant part dying
                                                   // (g/m^2)
    double dlt_dm_senesced_dead;                   // dry matter of senesced plant part
                                                   // dying (g/m^2)
    double dlt_n_green_dead;                       // N content of green plant part dying
                                                   // (g/m^2)
    double dlt_n_senesced_dead;                    // N content of senesced plant part
                                                   // dying (g/m^2)
    float dlt_grain_no_lost;                      // grain no lost from barrenness
                                                  // (grains/m^2)
    float dlt_lai_dead;                           // lai of green leaf of plants dying ()
    float dlt_slai_dead;                          // lai of senesced leaf of plant dying ()
    float dlt_root_length_dead;                   // root length of plant dying ()
    double dying_fract_plants;                    // fraction op population dying (0-1)
    float node_no;                                // currently expanding node no.
    int   part;                                   // plant part index
    int   layer;                                  // layer index number
    int   leaf_rec;                               // leaf record number
    int   num_leaves;
    int   node;
    float leaf_no_dead_tot;
    float canopy_fac;

//- Implementation Section ----------------------------------
    push_routine (my_name);

// Note.
// Accumulate is used to add a value into a specified array element.
// If a specified increment of the element indicates a new element
// is started, the value is distributed proportionately between the
// two elements of the array

// Add is used to add the elements of one array into the corresponding
// elements of another array.

// now update with deltas

// The following table describes the transfer of material that should
// take place
//                        POOLS
//                 green senesced  dead
// dlt_green         +                     (incoming only)
// dlt_retrans       +-
// dlt_senesced      -      +
// dlt_dead          -      -       +
// dlt_detached             -       -      (outgoing only)

    // transfer N
    subtract_real_array (g_dlt_n_dead_detached, g_n_dead, max_part);

    add_real_array (g_dlt_n_green, g_n_green, max_part);
    add_real_array (g_dlt_n_retrans, g_n_green, max_part);
    subtract_real_array (g_dlt_n_senesced, g_n_green, max_part);

    add_real_array (g_dlt_n_senesced, g_n_senesced, max_part);

    // Let me register my surprise at how this is done on the next few lines
    // - why intrinsically limit processes to leaf etc right here!!! - NIH
    g_n_green[leaf] = g_n_green[leaf] - g_dlt_n_senesced_trans[leaf];
    g_n_green[stem] = g_n_green[stem] + g_dlt_n_senesced_trans[leaf];
    g_n_green[leaf] = g_n_green[leaf] - sum_real_array(g_dlt_n_senesced_retrans, max_part);
    add_real_array (g_dlt_n_senesced_retrans, g_n_green, max_part);

    subtract_real_array (g_dlt_n_detached, g_n_senesced, max_part);

    dying_fract_plants = divide (-g_dlt_plants, *g_plants, 0.0);
    dying_fract_plants = bound (dying_fract_plants, 0.0, 1.0);

    for (part = 0; part < max_part; part++)
       {
       dlt_n_green_dead = g_n_green[part] * dying_fract_plants;
       g_n_green[part] = g_n_green[part] - dlt_n_green_dead;
       g_n_dead[part] = g_n_dead[part] + dlt_n_green_dead;

       dlt_n_senesced_dead = g_n_senesced[part] * dying_fract_plants;
       g_n_senesced[part] = g_n_senesced[part] - dlt_n_senesced_dead;
       g_n_dead[part] = g_n_dead[part] + dlt_n_senesced_dead;
       }

    // Transfer plant dry matter

    dlt_dm_plant = divide (g_dlt_dm, *g_plants, 0.0);

    accumulate (dlt_dm_plant, g_dm_plant_top_tot, g_previous_stage-1, g_dlt_stage);

    subtract_real_array (g_dlt_dm_dead_detached, g_dm_dead, max_part);

    add_real_array (g_dlt_dm_green, g_dm_green, max_part);
    add_real_array (g_dlt_dm_green_retrans, g_dm_green, max_part);
    subtract_real_array (g_dlt_dm_senesced, g_dm_green, max_part);

    add_real_array (g_dlt_dm_senesced, g_dm_senesced, max_part);
    subtract_real_array (g_dlt_dm_detached, g_dm_senesced, max_part);

    for (part = 0; part < max_part; part++)
         {
         dlt_dm_green_dead = g_dm_green[part] * dying_fract_plants;
         g_dm_green[part] = g_dm_green[part] - dlt_dm_green_dead;
         g_dm_dead[part] = g_dm_dead[part] + dlt_dm_green_dead;

         dlt_dm_senesced_dead = g_dm_senesced[part] * dying_fract_plants;
         g_dm_senesced[part] = g_dm_senesced[part] - dlt_dm_senesced_dead;
         g_dm_dead[part] = g_dm_dead[part] + dlt_dm_senesced_dead;
         }
//         OutputDebugString("Update");
         //    fprintf(stdout, "%d,%.9f,%.9f,%.9f,%.9f\n", g.day_of_year,
//            g_dm_green[root] + g_dm_green[leaf] + g_dm_green[stem],
//            g_dm_green[root], g_dm_green[leaf],g_dm_green[stem]);
    // transfer plant grain no.
    dlt_grain_no_lost  = *g_grain_no * dying_fract_plants;
    *g_grain_no = *g_grain_no - dlt_grain_no_lost;

    // transfer plant leaf area
    *g_lai = *g_lai + g_dlt_lai - g_dlt_slai;
    *g_slai = *g_slai + g_dlt_slai - g_dlt_slai_detached;
    dlt_lai_dead  = *g_lai  * dying_fract_plants;
    dlt_slai_dead = *g_slai * dying_fract_plants;

    *g_lai = *g_lai - dlt_lai_dead;
    *g_slai = *g_slai - dlt_slai_dead;
    *g_tlai_dead = *g_tlai_dead + dlt_lai_dead + dlt_slai_dead - g_dlt_tlai_dead_detached;

    *g_canopy_width = *g_canopy_width + g_dlt_canopy_width;

    if (p_num_canopy_widths > 0)
        {
        legnew_canopy_fac (g_row_spacing
                           , *g_plants
                           , g_skip_row_fac
                           , g_skip_plant_fac
                           , *g_canopy_width
                           , &canopy_fac);
        }
    else
        {
        canopy_fac = g_skip_row_fac;
        }

// now update new canopy covers

    legnew_cover_leaf_pod(g_row_spacing
                          ,c_x_row_spacing
                          ,c_y_extinct_coef
                          ,c_num_row_spacing
                          ,c_extinct_coef_pod
                          , canopy_fac
                          ,*g_lai
                          ,*g_pai
                          ,g_lai_canopy_green
                          ,g_cover_green
                          ,g_cover_pod);
    legnew_cover(g_row_spacing
                ,c_x_row_spacing
                ,c_y_extinct_coef_dead
                ,c_num_row_spacing
                , canopy_fac
                ,*g_slai
                ,g_cover_sen);
    legnew_cover(g_row_spacing
                 ,c_x_row_spacing
                 ,c_y_extinct_coef_dead
                 ,c_num_row_spacing
                 , canopy_fac
                 ,*g_tlai_dead
                 ,g_cover_dead);

// plant leaf development
// need to account for truncation of partially developed leaf (add 1)
    node_no = 1.0 + sum_between (emerg-1, now-1, g_node_no);
    //c      print*,"node_no=",node_no;
    //c      print*,"g_leaf_area=",(g_leaf_area(i), i=1,10);
    //c      print*,"g_leaf_no=",(g_leaf_no(i), i=1,10);
    //c      print*,"g_leaf_no_dead=",(g_leaf_no_dead(i), i=1,10);

    dlt_leaf_area = divide (g_dlt_lai, *g_plants, 0.0) * sm2smm;
    accumulate (dlt_leaf_area, g_leaf_area, node_no-1.0, g_dlt_node_no);
// Area senescence is calculated apart from plant number death
// so any decrease in plant number will mean an increase in average
// plant size as far as the leaf size record is concerned.

    if ((*g_plants + g_dlt_plants)>0)
        {
// Not total failure
//         do 5 node = 1, max_node
//            g_leaf_area(node) = g_leaf_area(node)
//     :                  * g_plants/(g_plants+g_dlt_plants)
//    5    continue
        }
    else
        {
        fill_real_array(g_leaf_area, 0.0, max_node);
        }

// cnh =====================================================
//      ! detached leaf area needs to be accounted for
//
//      dlt_leaf_area = divide (g_dlt_slai_detached, g_plants, 0.0)
//     :              * sm2smm
//      num_leaves = count_of_Real_vals(g_leaf_area,max_leaf)
//      num_leaves = max_leaf
//      dlt_leaf_dm = g_dlt_dm_detached[leaf]/g_plants
//
//      empty_leaves = -1
//      do 111 leaf_rec = 1,num_leaves
//        if (g_leaf_area(leaf_rec).le.dlt_leaf_area) then
//           dlt_leaf_area = dlt_leaf_area - g_leaf_area(leaf_rec)
//           g_leaf_area(leaf_rec) = 0.0
//        else
//           g_leaf_area(leaf_rec) = g_leaf_area(leaf_rec) - dlt_leaf_area
//           dlt_leaf_area = 0.0
//        endif
//        if (g_leaf_dm(leaf_rec).le.dlt_leaf_dm) then
//           dlt_leaf_dm = dlt_leaf_dm - g_leaf_dm(leaf_rec)
//           g_leaf_dm(leaf_rec) = 0.0
//        else
//           g_leaf_dm(leaf_rec) = g_leaf_dm(leaf_rec) - dlt_leaf_dm
//           dlt_leaf_dm = 0.0
//        endif
//        if ((g_leaf_dm(leaf_rec).gt.0.0).and.(empty_leaves.eq.-1)) then
//           empty_leaves = leaf_rec - 1
//        else
//        endif
//  111 continue
//
//      if (empty_leaves.gt.0) then
//         g_leaf_no_detached = g_leaf_no_detached + empty_leaves
//         !kludgy solution for now
//         do 112 leaf_rec=empty_leaves+1, num_leaves
//            leaf_rec_new = leaf_rec - empty_leaves
//            g_leaf_dm(leaf_rec_new)=g_leaf_dm(leaf_rec)
//            g_leaf_area(leaf_rec_new)=g_leaf_area(leaf_rec)
//            g_leaf_dm(leaf_rec) = 0.0
//            g_leaf_area(leaf_rec) = 0.0
//  112    continue
//      else
//      endif

// cnh =====================================================
//    fprintf(stdout, "A%d, %.3f, %.3f, %.3f, %.3f, %.3f, %.3f\n",
//             g.day_of_year, g_dlt_leaf_no,
//             g_leaf_no[0], g_leaf_no[1], g_leaf_no[2],
//             node_no, g_dlt_node_no);
    accumulate (g_dlt_leaf_no, g_leaf_no, node_no-1.0, g_dlt_node_no);
//    fprintf(stdout, "B%d, %.3f, %.3f, %.3f, %.3f, %.3f, %.3f\n",
//             g.day_of_year, g_dlt_leaf_no,
//             g_leaf_no[0], g_leaf_no[1], g_leaf_no[2],
//             node_no, g_dlt_node_no);
//    fprintf(stdout, "X%d,%.6f,%.6f\n",
//             g.day_of_year,g_dlt_leaf_no, g_dlt_node_no);


    leaf_no_dead_tot = sum_real_array(g_leaf_no_dead,max_node) + g_dlt_leaf_no_dead;

    for (node = 0; node < max_node; node++)
        {
        if (leaf_no_dead_tot>g_leaf_no[node])
            {
            leaf_no_dead_tot = leaf_no_dead_tot - g_leaf_no[node];
            g_leaf_no_dead[node] = g_leaf_no[node];
            }
        else
            {
            g_leaf_no_dead[node] = leaf_no_dead_tot;
            leaf_no_dead_tot = 0.0;
            }
        }

    accumulate (g_dlt_node_no, g_node_no, g_previous_stage-1, g_dlt_stage);                       //ok

// plant stress

    accumulate (g_dlt_heat_stress_tt, g_heat_stress_tt, g_previous_stage-1, g_dlt_stage);         //ok
    accumulate (g_dlt_heat_stress_tt, g_heat_stress_tt, g_previous_stage-1, g_dlt_stage);         //ok

    accumulate (g_dlt_dm_stress_max, g_dm_stress_max, g_current_stage-1, g_dlt_stage);            //ok

    accumulate (1.0 - g_swdef_photo, g_cswd_photo, g_previous_stage-1, g_dlt_stage);              //ok
    accumulate (1.0 - g_swdef_expansion, g_cswd_expansion, g_previous_stage-1, g_dlt_stage);      //ok
    accumulate (1.0 - g_swdef_pheno, g_cswd_pheno, g_previous_stage-1, g_dlt_stage);              //ok

    accumulate (1.0 - g_nfact_photo, g_cnd_photo, g_previous_stage-1, g_dlt_stage);               //ok
    accumulate (1.0 - g_nfact_grain_conc, g_cnd_grain_conc, g_previous_stage-1, g_dlt_stage);     //ok

// other plant states

    *g_canopy_height = *g_canopy_height + g_dlt_canopy_height;
    *g_plants = *g_plants + g_dlt_plants;
    *g_root_depth = *g_root_depth + g_dlt_root_depth;
    add_real_array (g_dlt_root_length, g_root_length, max_layer);
    subtract_real_array (g_dlt_root_length_senesced, g_root_length, max_layer);

// Note that movement and detachment of C is already done, just
// need to maintain relationship between length and mass
// Note that this is not entirely accurate.  It links live root
// weight with root length and so thereafter dead(and detaching)
// root is assumed to have the same distribution as live roots.
    for (layer = 0; layer < max_layer; layer++)
        {
        g_dlt_root_length_dead[layer] = g_root_length[layer] * dying_fract_plants;
        g_root_length[layer] = g_root_length[layer] - g_dlt_root_length_dead[layer];
        g_root_length_dead[layer] = g_root_length_dead[layer] + g_dlt_root_length_dead[layer];
        }

    *g_pai = *g_pai + g_dlt_pai;

    plant_n_conc_limits( c_stage_code_list
                        , g_phase_tt
                        , g_tt_tot
                        , c_n_conc_crit_meal
                        , c_n_conc_crit_root
                        , c_n_conc_max_meal
                        , c_n_conc_max_root
                        , c_n_conc_min_meal
                        , c_n_conc_min_root
                        , c_x_stage_code
                        , c_y_n_conc_crit_leaf
                        , c_y_n_conc_crit_pod
                        , c_y_n_conc_crit_stem
                        , c_y_n_conc_max_leaf
                        , c_y_n_conc_max_pod
                        , c_y_n_conc_max_stem
                        , c_y_n_conc_min_leaf
                        , c_y_n_conc_min_pod
                        , c_y_n_conc_min_stem
                        , c_x_co2_nconc_modifier, c_y_co2_nconc_modifier, c_num_co2_nconc_modifier
                        , g_co2
                        , g_current_stage
                        , g_n_conc_crit
                        , g_n_conc_max
                        , g_n_conc_min);                                          // plant N concentr

    pop_routine (my_name);
    return;
    }


//+  Purpose
//         Check bounds of internal pools

//+  Mission Statement
//     Check bounds of internal pools

//+  Changes
//     010994 jngh specified and programmed
void Plant::plant_check_bounds
    (
     float  g_canopy_height                     // (INPUT)  canopy height (mm)
    ,float  g_cover_dead                        // (INPUT)  fraction of radiation reaching
    ,float  g_cover_green                       // (INPUT)  fraction of radiation reaching
    ,float  g_cover_sen                         // (INPUT)  fraction of radiation reaching
    ,float  g_current_stage                     // (INPUT)  current phenological stage
    ,float *g_days_tot                          // (INPUT)  duration of each phase (days)
    ,float *g_dlayer                            // (INPUT)  thickness of soil layer I (mm)
    ,float *g_dm_dead                           // (INPUT)  dry wt of dead plants (g/m^2)
    ,float *g_dm_green                          // (INPUT)  live plant dry weight (biomass
    ,float *g_dm_senesced                       // (INPUT)  senesced plant dry wt (g/m^2)
    ,float *g_dm_stress_max                     // (INPUT)  sum of maximum daily stress on
    ,float  g_grain_no                          // (INPUT)  grain number (grains/plant)
    ,float *g_heat_stress_tt                    // (INPUT)  heat stress cumulation in each
    ,float  g_lai                               // (INPUT)  live plant green lai
    ,float *g_leaf_area                         // (INPUT)  leaf area of each leaf (mm^2)
    ,float *g_leaf_no                           // (INPUT)  number of fully expanded leave
    ,float *g_node_no                           // (INPUT)  number of fully expanded nodes
    ,float *g_leaf_no_dead                      // (INPUT)  no of dead leaves ()
    ,float *g_n_conc_crit                       // (INPUT)  critical N concentration (g N/
    ,float *g_n_conc_max                        // (INPUT)  maximum N concentration (g N/g
    ,float *g_n_conc_min                        // (INPUT)  minimum N concentration (g N/g
    ,float *g_n_dead                            // (INPUT)  plant N content of dead plants
    ,float *g_n_green                           // (INPUT)  plant nitrogen content (g N/m^
    ,float *g_n_senesced                        // (INPUT)  plant N content of senesced pl
    ,float *g_phase_tt                          // (INPUT)  Cumulative growing degree days
    ,float  g_plants                            // (INPUT)  Plant density (plants/m^2)
    ,float  g_root_depth                        // (INPUT)  depth of roots (mm)
    ,float  g_slai                              // (INPUT)  area of leaf that senesces fro
    ,float  g_tlai_dead                         // (INPUT)  total lai of dead plants
    ,float *g_tt_tot                            // (INPUT)  the sum of growing degree days
    ) {

//+  Constant Values
    const char*  my_name = "plant_check_bounds" ;

//+  Local Variables

//- Implementation Section ----------------------------------

    push_routine (my_name);

    bound_check_real_var(parent,g_root_depth
                         , 0.0
                         , sum_real_array (g_dlayer, max_layer)
                         , "root_depth");

    bound_check_real_var(parent,g_current_stage
                         , 1.0
                         , max_stage
                         , "current_stage");

    bound_check_real_var(parent,g_plants
                         , 0.0
                         , 10000.0
                         , "plants");

    bound_check_real_var(parent,g_cover_green
                         , 0.0
                         , 1.0
                         , "cover_green");

    bound_check_real_var(parent,g_cover_sen
                         , 0.0
                         , 1.0
                         , "cover_sen");

    bound_check_real_var(parent,g_cover_dead
                         , 0.0
                         , 1.0
                         , "cover_dead");

    bound_check_real_var(parent,sum_real_array (g_heat_stress_tt, max_stage)
                         , 0.0
                         , 1000000.0
                         , "heat_stress_tt");
    bound_check_real_var(parent,sum_real_array (g_dm_stress_max, max_stage)
                         , 0.0
                         , 1000000.0
                         , "dm_stress_max");

    pop_routine (my_name);
    return;
    }


//+  Purpose
//         Collect totals of crop variables for output

//+  Mission Statement
//     Collect totals of crop variables for output

//+  Changes
//     010994 jngh specified and programmed
void Plant::plant_totals
    (
     float g_current_stage              // (INPUT)  current phenological stage
    ,float *g_days_tot                   // (INPUT)  duration of each phase (days)
    ,int   g_day_of_year                // (INPUT)  day of year
    ,float *g_dlayer                     // (INPUT)  thickness of soil layer I (mm)
    ,float *g_dlt_n_retrans              // (INPUT)  nitrogen retranslocated out from parts to grain (g/m^2)
    ,float *g_dlt_sw_dep                 // (INPUT)  water uptake in each layer (mm water)
    ,float *g_dm_green                   // (INPUT)  live plant dry weight (biomass) (g/m^2)
    ,int   *g_flowering_date             // (INPUT)  flowering day number
    ,int   *g_flowering_das              // (INPUT)  flowering day number
    ,float *g_lai                        // (INPUT)  live plant green lai
    ,float *g_lai_max                    // (INPUT)  maximum lai - occurs at flowering
    ,int   *g_maturity_date              // (INPUT)  maturity day number
    ,int   *g_maturity_das               // (INPUT)  maturity day number
    ,float  *g_n_conc_act_stover_tot           // (INPUT)  sum of tops actual N concentration (g N/g biomass)
    ,float  *g_n_conc_crit                     // (INPUT)  critical N concentration (g N/g biomass)
    ,float  *g_n_conc_crit_stover_tot          // (INPUT)  sum of tops critical N concentration (g N/g biomass)
    ,float  *g_n_dead                          // (INPUT)  plant N content of dead plants (g N/m^2)
    ,float  *g_n_demand                        // (INPUT)  critical plant nitrogen demand (g/m^2)
    ,float  *g_n_demand_tot                    // (out/INPUT)  sum of N demand since last output (g/m^2)
    ,float  *g_n_green                         // (INPUT)  plant nitrogen content (g N/m^2)
    ,float  *g_n_senesced                      // (INPUT)  plant N content of senesced plant (g N/m^2)
    ,float  *g_n_uptake_grain_tot              // (out/INPUT)  sum of grain N uptake (g N/m^2)
    ,float  *g_n_uptake_stover_tot             // (out/INPUT)  sum of tops N uptake (g N/m^2)
    ,float  *g_n_uptake_tot                    // (out/INPUT)  cumulative total N uptake (g/m^2)
    ,float  *g_dlt_n_green                     // (INPUT)  daily N uptake (g/m^2)
    ,float  *g_n_fix_uptake                    // (INPUT)  daily fixed N (g/m^2)
    ,float  *g_n_fixed_tops                    // (out/INPUT)  fixed N in tops (g/m2)
    ,float  *g_root_depth                      // (INPUT)  depth of roots (mm)
    ,float  *g_transpiration_tot               // (out/INPUT)  cumulative transpiration (mm)
    )  {

//+  Constant Values
    const char*  my_name = "plant_totals" ;

//+  Local Variables
    float n_conc_stover;                          // tops actual N concentration (g N/g part)
    int   deepest_layer;                          // deepest layer in which the roots are growing
    float n_conc_stover_crit;                     // tops critical N concentration (g N/g part)
    float n_green_demand;                         // plant N demand (g/m^2)
    float n_uptake;                               // nitrogen uptake from soil (g/m^2)
    float n_uptake_stover;                        // nitrogen uptake from soil by veg. top (g/m^2)
    float n_grain;                                // total grain N uptake
    float n_dead;                                 // above ground dead plant N
    float n_green;                                // above ground green plant N
    float n_senesced;                             // above ground senesced plant N
    float n_stover;                               // nitrogen content of stover
    float n_uptake_soil;                          // daily N taken up by roots (mineral + fixation)
    float n_uptake_soil_tops;                     // daily N taken up by roots going into tops

//- Implementation Section ----------------------------------

    push_routine (my_name);

// get totals
    n_conc_stover = divide ((g_n_green[leaf]+ g_n_green[stem]+ g_n_green[pod])
    , (g_dm_green[leaf] + g_dm_green[stem] + g_dm_green[pod])
    , 0.0);

    n_uptake = sum_real_array (g_dlt_n_retrans, max_part);
    n_uptake_stover =  g_dlt_n_retrans[leaf] + g_dlt_n_retrans[stem];

// note - g_n_conc_crit should be done before the stages change

    n_conc_stover_crit = (g_n_conc_crit[leaf] + g_n_conc_crit[stem])
    * 0.5;
    n_green_demand = sum_real_array (g_n_demand, max_part);

    deepest_layer = find_layer_no (*g_root_depth, g_dlayer, max_layer);

    if (on_day_of (sowing, g_current_stage))
        {
        *g_n_uptake_tot = n_uptake;
        *g_transpiration_tot = - sum_real_array (g_dlt_sw_dep, deepest_layer+1);
        *g_n_conc_act_stover_tot = n_conc_stover;
        *g_n_conc_crit_stover_tot = n_conc_stover_crit;
        *g_n_demand_tot = n_green_demand;
        *g_n_uptake_stover_tot = n_uptake_stover;
        *g_n_uptake_grain_tot = sum_real_array (g_dlt_n_retrans, max_part);

        n_uptake_soil = sum_real_array(g_dlt_n_green,max_part);
        n_uptake_soil_tops = n_uptake_soil - g_dlt_n_green[root];
        *g_n_fixed_tops = n_uptake_soil_tops
                              * divide (*g_n_fix_uptake
                                        ,n_uptake_soil
                                        ,0.0);

        }
    else
        {
        *g_n_uptake_tot = (*g_n_uptake_tot) + n_uptake;
        *g_transpiration_tot = (*g_transpiration_tot) + (-sum_real_array (g_dlt_sw_dep, deepest_layer+1));
        *g_n_conc_act_stover_tot = n_conc_stover;
        *g_n_conc_crit_stover_tot = n_conc_stover_crit;
        *g_n_demand_tot = (*g_n_demand_tot) + n_green_demand;
        *g_n_uptake_stover_tot = (*g_n_uptake_stover_tot) + n_uptake_stover;
        *g_n_uptake_grain_tot = *g_n_uptake_grain_tot + sum_real_array (g_dlt_n_retrans, max_part);

        n_uptake_soil = sum_real_array(g_dlt_n_green,max_part);
        n_uptake_soil_tops = n_uptake_soil - g_dlt_n_green[root];
        *g_n_fixed_tops = *g_n_fixed_tops + n_uptake_soil_tops * divide (*g_n_fix_uptake ,n_uptake_soil ,0.0);

        }

    *g_lai_max = max (*g_lai_max, *g_lai);
    if (on_day_of (flowering, g_current_stage))
        {
        *g_flowering_date = g_day_of_year;
        *g_flowering_das = sum_real_array (g_days_tot, flowering);

        }
    else if (on_day_of (maturity, g_current_stage))
        {
        *g_maturity_date = g_day_of_year;
        *g_maturity_das = sum_real_array (g_days_tot, maturity);
        }
    else
        {
        }

// note - oil has no N, thus it is not included in calculations

    n_grain = g_n_green[meal] + g_n_dead[meal];

    n_green = (sum_real_array (g_n_green, max_part)
    - g_n_green[root]
    - g_n_green[meal]);

    n_senesced = (sum_real_array (g_n_senesced, max_part)
    - g_n_senesced[root]
    - g_n_senesced[meal]);

    n_dead = (sum_real_array (g_n_dead, max_part)
    - g_n_dead[root]
    - g_n_dead[meal]);

    n_stover = n_green + n_senesced + n_dead;

    *g_n_uptake_grain_tot = n_grain;
    *g_n_uptake_stover_tot = n_stover;
    *g_n_uptake_tot = n_grain + n_stover;

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Report occurence of event and the current status of specific
//       variables.

//+  Mission Statement
//     Report occurence of event and the current status of specific variables

//+  Changes
//     010994 jngh specified and programmed
void Plant::plant_event
    (float  g_current_stage               // (INPUT)  current phenological stage
    ,float *g_dlayer                      // (INPUT)  thickness of soil layer I (mm)
    ,float *g_dm_dead                     // (INPUT)  dry wt of dead plants (g/m^2)
    ,float *g_dm_green                    // (INPUT)  live plant dry weight (biomass
    ,float *g_dm_senesced                 // (INPUT)  senesced plant dry wt (g/m^2)
    ,float  g_lai                         // live plant green lai
    ,float *g_n_green                     // (INPUT)  plant nitrogen content (g N/m^
    ,float  g_root_depth                  // (INPUT)  depth of roots (mm)
    ,float *g_sw_dep                      // (INPUT)  soil water content of layer L
    ,float *p_ll_dep                      // (INPUT)  lower limit of plant-extractab
    ) {

//+  Constant Values
    const char*  my_name = "plant_event" ;

//+  Local Variables
    float biomass;                                // total above ground plant wt (g/m^2)
    int   deepest_layer;                          // deepest layer in which the roots are growing
    int   layer;                                  // profile layer number
    float pesw_tot;                               // total plant extractable sw (mm)
    float pesw[max_layer];                        // plant extractable soil water (mm)
    float n_green;                                // plant nitrogen of tops (g/m^2) less pod
    float dm_green;                               // plant wt of tops (g/m^2) less pod
    int   stage_no;                               // stage number at beginning of phase
    float n_green_conc_percent;                   // n% of tops less pod (incl grain)

//- Implementation Section ----------------------------------

    push_routine (my_name);

    stage_no = (int) g_current_stage;

    if (on_day_of (stage_no, g_current_stage))
        {
        // new phase has begun.
        sendStageMessage(c.stage_names[stage_no-1].c_str());

        char msg[80];
        sprintf(msg, " stage %.1f %s"
                      , c.stage_code_list[stage_no-1]
                      , c.stage_names[stage_no-1].c_str());
        parent->writeString(msg);

        biomass = sum_real_array (g_dm_green, max_part)
                     - g_dm_green[root]
                     + sum_real_array (g_dm_senesced, max_part)
                     - g_dm_senesced[root]
                     + sum_real_array (g_dm_dead, max_part)
                     - g_dm_dead[root];

// note - oil has no N, thus is not included in calculations
        dm_green = sum_real_array (g_dm_green, max_part)
                     - g_dm_green[root]
                     - g_dm_green[meal];

        n_green = sum_real_array (g_n_green, max_part)
                     - g_n_green[root]
                     - g_n_green[meal];

        n_green_conc_percent = divide (n_green, dm_green, 0.0) * fract2pcnt;

        deepest_layer = find_layer_no (g_root_depth, g_dlayer, max_layer);
        for (layer = 0; layer <= deepest_layer; layer++)
           {
           pesw[layer] = g_sw_dep[layer] - p_ll_dep[layer];
           pesw[layer] = l_bound (pesw[layer], 0.0);
           }
        pesw_tot = sum_real_array (pesw, deepest_layer+1);

        if (stage_is_between (emerg, plant_end, g_current_stage))
            {
            char msg[256];
            sprintf(msg,
"                biomass =       %8.2f (g/m^2)   lai          = %7.3f (m^2/m^2)\n\
                stover N conc = %8.2f (%%)     extractable sw = %7.2f (mm)"
                ,biomass
                ,g_lai
                ,n_green_conc_percent
                ,pesw_tot);
            parent->writeString (msg);
            }
        else
            {
            }

        }
    else
        {
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Add root residue to root residue pool

//+  Mission Statement
//     Add root residue to root residue pool

//+  Changes
//       220794 jngh specified and programmed
//       170895 jngh changed message send to message pass to module
//       220696 jngh changed to post_ construct
//       081100 dph  added eventInterface parameter to call to crop_root_incorp
void Plant::plant_root_incorp (
     float  dlt_dm_root                  // (INPUT) new root residue dm (g/m^2)
    ,float  dlt_n_root                   // (INPUT) new root residue N (g/m^2)
    ,float  *root_length) {              // (INPUT) root length of root residue (mm/mm^2)

//+  Constant Values
    const char*  my_name = "plant_root_incorp" ;

//+  Local Variables

//- Implementation Section ----------------------------------

    push_routine (my_name);

    if (dlt_dm_root>0.0)
        {
        plant_root_incorp (dlt_dm_root
                          ,dlt_n_root
                          ,g.dlayer
                          ,root_length
                          ,g.root_depth
                          ,c.crop_type.c_str()
                          ,max_layer);

        }
    else
        {
// no roots to incorporate
        }

    pop_routine (my_name);
    return;
    }

//+  Purpose
//       Plant root distribution in the soil

//+  Mission Statement
//     Calculates the plant root depth

//+  Changes
//      250894 jngh specified and programmed
void Plant::plant_root_depth (int option /* (INPUT) option number*/)
    {

//+  Constant Values
    const char*  my_name = "plant_root_depth" ;

//- Implementation Section ----------------------------------
    //!!!!!!!! check order dependency of deltas
    push_routine (my_name);

    if (option == 1)
        {
        legopt_root_depth1 (
        g.dlayer
        ,c.num_sw_ratio
        ,c.x_sw_ratio
        ,c.y_sw_fac_root
        ,g.dul_dep
        ,g.sw_dep
        ,p.ll_dep
        ,c.root_depth_rate
        ,g.current_stage
        ,g.mint
        ,g.maxt
        ,c.x_temp_root_advance
        ,c.y_rel_root_advance
        ,c.num_temp_root_advance
        ,p.xf
        ,g.root_depth
        ,&g.dlt_root_depth);

        }
    else if (option==2)
        {

        cproc_root_depth2 ( g.current_stage
                           ,g.maxt
                           ,g.mint
                           ,g.swdef_photo
                           ,g.root_depth
                           ,c.num_temp_root_advance
                           ,c.x_temp_root_advance
                           ,c.y_rel_root_advance
                           ,c.num_ws_root
                           ,c.x_ws_root
                           ,c.y_ws_root_fac
                           ,c.num_sw_ratio
                           ,c.x_sw_ratio
                           ,c.y_sw_fac_root
                           ,g.dlayer
                           ,g.dul_dep
                           ,g.sw_dep
                           ,p.ll_dep
                           ,c.root_depth_rate
                           ,p.xf
                           ,&g.dlt_root_depth);

        }
    else
        {
        throw std::invalid_argument ("invalid template option");
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Plant water supply

//+  Mission Statement
//     Plant water supply

//+  Changes
//      250894 jngh specified and programmed
void Plant::plant_water_supply (int option /* (INPUT) option number*/)
    {
    const char*  my_name = "plant_water_supply" ;

    push_routine (my_name);

    if (option == 1)
        {
        cproc_sw_supply1 (parent
                          ,c.sw_lb
                          ,g.dlayer
                          ,p.ll_dep
                          ,g.dul_dep
                          ,g.sw_dep
                          ,max_layer
                          ,g.root_depth
                          ,p.kl
                          ,g.sw_avail
                          ,g.sw_avail_pot
                          ,g.sw_supply);
        }
    else
        {
        throw std::invalid_argument ("invalid template option");
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Plant water demand

//+  Mission Statement
//     Calculate the plant water demand

//+  Changes
//      250894 jngh specified and programmed
void Plant::plant_water_demand (int option /* (INPUT) option number*/)
    {

//+  Constant Values
    const char*  my_name = "plant_water_demand" ;

//- Implementation Section ----------------------------------
    //!!!!!!!! check order dependency of deltas
    push_routine (my_name);

    if (option == 1)
        {

        cproc_sw_demand1 (g.dlt_dm_pot_rue,
                          g.transp_eff,
                          &g.sw_demand_te);

        //g.sw_demand_te = g.sw_demand_te + g.dlt_dm_parasite_demand

        cproc_sw_demand_bound(g.sw_demand_te
                              ,p.eo_crop_factor
                              ,g.eo
                              ,g.cover_green
                              ,&g.sw_demand);
        }
    else
        {
        throw std::invalid_argument ("invalid template option");
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Plant transpiration and soil water extraction

//+  Mission Statement
//     Get the plant water uptake

//+  Changes
//      250894 jngh specified and programmed

void Plant::plant_water_uptake (int option /*(INPUT) option number*/)
    {
    int   layer;                                  // layer number of profile ()
    float ext_sw_supply[max_layer];
    const char*  my_name = "plant_water_uptake" ;

    push_routine (my_name);

    if (Str_i_Eq(p.uptake_source,"apsim"))
        {
        plant_get_ext_uptakes(p.uptake_source.c_str()
                             ,c.crop_type.c_str()
                             ,"water"
                             ,1.0
                             ,0.0
                             ,100.0
                             ,ext_sw_supply
                             ,max_layer);

        for (layer = 0; layer < g.num_layers; layer++)
           {
           g.dlt_sw_dep[layer] = -ext_sw_supply[layer];
           }
        }
    else if (option == 1)
        {
        cproc_sw_uptake1(max_layer, g.dlayer, g.root_depth, g.sw_demand, g.sw_supply, g.dlt_sw_dep);
        }
    else
        {
        throw std::invalid_argument ("invalid template option");
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       light supply

//+  Mission Statement
//     Seek the light intercepted by the leaves

//+  Changes
//      5/9/96 dph
void Plant::plant_light_supply (int option /*(INPUT) option number*/)
    {

//+  Constant Values
    const char*  my_name = "plant_light_supply" ;

//- Implementation Section ----------------------------------
    push_routine (my_name);

    if (option == 1)
        {
        crop_radn_int0(g.cover_green, g.fr_intc_radn, g.radn, &g.radn_int);
        //cradn
        //fprintf(stdout, "%d %f\n", g.day_of_year, g.cover_green);
        }
    else
        {
        throw std::invalid_argument ("invalid template option");
        }

    pop_routine (my_name);
    }


//+  Purpose
//       biomass light

//+  Mission Statement
//     Biomass radiation use efficiency

//+  Changes
//      5/9/96 dph
void Plant::plant_bio_rue (int option /*(INPUT) option number*/)
    {
    const char*  my_name = "plant_bio_rue" ;

    push_routine (my_name);

    if (option == 1)
        {
        legnew_dm_pot_rue(g.current_stage, max_stage
                         , g.tt_tot
                         , c.x_stage_rue
                         , g.phase_tt
                         , c.y_rue
                         , c.rue_pod
                         , g.cover_green
                         , g.cover_pod
                         , g.radn_int
                         , min(min(min(g.temp_stress_photo, g.nfact_photo),
                             g.oxdef_photo), phosphorus->fact_photo())
                         , g.co2, g.maxt, g.mint
                         , c.photosynthetic_pathway
                         , &g.dlt_dm_pot_rue);
        }
    else
        {
        throw std::invalid_argument ("invalid template option");
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Calculate today's transpiration efficiency from min and max
//       temperatures and converting mm water to g dry matter
//       (g dm/m^2/mm water)

//+  Mission Statement
//     Get today's transpiration efficiency calculations

//+  Changes
//      5/9/96 dph
void Plant::plant_transpiration_eff (int option /*(INPUT) option number*/)
    {

//+  Constant Values
    const char*  my_name = "plant_transpiration_efficiency" ;

//- Implementation Section ----------------------------------
    push_routine (my_name);

    if (option == 1)
        {
        int current_phase = int(g.current_stage);

//        cproc_transp_eff1(c.svp_fract, c.transp_eff_cf[current_phase-1],
//                          g.maxt, g.mint,
//                          &g.transp_eff);
        cproc_transp_eff_co2(c.svp_fract, c.transp_eff_cf[current_phase-1],
                             g.maxt, g.mint, g.co2,
                             c.x_co2_te_modifier, c.y_co2_te_modifier,
                             c.num_co2_te_modifier,
                             &g.transp_eff);
        }
    else
        {
        throw std::invalid_argument ("invalid template option");
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Simulate plant root senescence.

//+  Mission Statement
//     Calculate plant root senescence

//+  Changes
//      200897 nih specified and programmed
void Plant::plant_sen_root_length (int option /*(INPUT) option number*/)
    {
//+  Constant Values
    const char*  my_name = "plant_sen_root_length" ;

//- Implementation Section ----------------------------------
    push_routine (my_name);

    if (option == 1)
        {
        cproc_root_length_senescence1(  c.specific_root_length
                                      , g.dlayer
                                      , g.dlt_dm_senesced[root]
                                      , g.root_length
                                      , g.root_depth
                                      , g.dlt_root_length_senesced
                                      , max_layer);
        }
    else
        {
        throw std::invalid_argument ("invalid template option");
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//      Initialise plant root depth

//+  Mission Statement
//     Initialises the plant root depth

//+  Changes
//      220498 nih specified and programmed
void Plant::plant_root_depth_init (int option /*(INPUT) option number*/)
    {

//+  Constant Values
    const char*  my_name = "plant_root_depth_init" ;

//- Implementation Section ----------------------------------
    push_routine (my_name);

    if (option == 1)
        {
        cproc_root_depth_init1(c.initial_root_depth
                               , g.current_stage
                               , germ
                               , g.days_tot
                               , &g.root_depth);
        }
    else
        {
        throw std::invalid_argument ("invalid template option");
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Plant root distribution calculations

//+  Mission Statement
//     Calculate the plant root distribution during growth

//+  Changes
//      200897 nih specified and programmed
void Plant::plant_root_length_growth (int option /*(INPUT) option number*/)
    {

//+  Constant Values
    const char*  my_name = "plant_root_length_growth" ;

//- Implementation Section ----------------------------------

    push_routine (my_name);

    if (option == 1)
        {
        cproc_root_length_growth1(  c.specific_root_length
                                  , g.dlayer
                                  , g.dlt_dm_green[root]
                                  , g.dlt_root_length
                                  , g.dlt_root_depth
                                  , g.root_depth
                                  , g.root_length
                                  , g.plants
                                  , p.xf
                                  , c.num_sw_ratio
                                  , c.x_sw_ratio
                                  , c.y_sw_fac_root
                                  , c.x_plant_rld
                                  , c.y_rel_root_rate
                                  , c.num_plant_rld
                                  , g.dul_dep
                                  , g.sw_dep
                                  , p.ll_dep
                                  , max_layer);
        }
    else if (option == 2)
       {
       cproc_root_length_growth_new (c.specific_root_length
                                   , p.root_distribution_pattern
                                   , g.dlayer
                                   , g.dlt_dm_green[root]
                                   , g.dlt_root_length
                                   , g.dlt_root_depth
                                   , g.root_depth
                                   , g.root_length
                                   , g.plants
                                   , p.xf
                                   , c.num_sw_ratio
                                   , c.x_sw_ratio
                                   , c.y_sw_fac_root
                                   , c.x_plant_rld
                                   , c.y_rel_root_rate
                                   , c.num_plant_rld
                                   , g.dul_dep
                                   , g.sw_dep
                                   , p.ll_dep
                                   , max_layer);
        }
    else
        {
        throw std::invalid_argument ("invalid template option");
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Plant root distribution calculations

//+  Mission Statement
//     Calculate the plant root distribution initialisation

//+  Changes
//      200897 nih specified and programmed
void Plant::plant_root_length_init (int option /*(INPUT) option number*/)
    {

//+  Constant Values
    const char*  my_name = "plant_root_length_init" ;

//- Implementation Section ----------------------------------

    push_routine (my_name);

    if (option == 1)
        {
        cproc_root_length_init1( emerg
                                ,g.current_stage
                                ,g.days_tot
                                ,g.dm_green[root]
                                ,c.specific_root_length
                                ,g.root_depth
                                ,g.dlayer
                                ,g.root_length
                                ,max_layer);
        }
    else
        {
        throw std::invalid_argument ("invalid template option");
        }

    pop_routine (my_name);
    return;
    }
//+  Purpose
//       Initialise plant weights and plant weight minimums
//       at required instances.

//+  Mission Statement
//     Initialise plant weights and plant weight minimums at required instances.

//+  Changes
//     010994 jngh specified and programmed
void Plant::plant_dm_init (
         float  c_dm_leaf_init                            // (INPUT)  leaf growth before emergence (
        ,float  c_dm_root_init                            // (INPUT)  root growth before emergence (
        ,float  c_dm_stem_init                            // (INPUT)  stem growth before emergence (
        ,float  c_leaf_trans_frac                         // (INPUT)  fraction of leaf used in trans
        ,float  c_stem_trans_frac                         // (INPUT)  fraction of stem used in trans
        ,float  c_pod_trans_frac                          // (INPUT)  fraction of pod used in trans
        ,float  g_current_stage                           // (INPUT)  current phenological stage
        ,float  g_plants                                  // (INPUT)  Plant density (plants/m^2)
        ,float  *dm_green                                  // (INPUT/OUTPUT) plant part weights  (g/m^2)
        ,float  *dm_plant_min                              // (OUTPUT) minimum weight of each plant part (g/plant)
        ) {

//+  Local Variables
    float dm_plant_leaf;                          // dry matter in leaves (g/plant)
    float dm_plant_stem;                          // dry matter in stems (g/plant)
    float dm_plant_pod;                           // dry matter in pods (g/plant)

//- Implementation Section ----------------------------------

// initialise plant weight
// initialisations - set up dry matter for leaf, stem, pod, grain
// and root

    if (on_day_of (emerg, g_current_stage))
        {
        // seedling has just emerged.
        // initialise root, stem and leaf.
        dm_green[root] = c_dm_root_init * g_plants;
        dm_green[stem] = c_dm_stem_init * g_plants;
        dm_green[leaf] = c_dm_leaf_init * g_plants;
        dm_green[meal] = 0.0;
        dm_green[oil] = 0.0;
        dm_green[pod] = 0.0;
        }
    else if (on_day_of (flowering, g_current_stage))
        {
        dm_plant_stem = divide (dm_green[stem], g_plants, 0.0);
        dm_plant_min[stem] = dm_plant_stem;
        dm_plant_leaf = divide (dm_green[leaf], g_plants, 0.0);
        dm_plant_min[leaf] = dm_plant_leaf;
        dm_plant_min[pod] = 0.0;
        }
    else if (on_day_of (start_grain_fill, g_current_stage))
        {

// we are at first day of grainfill.
// set the minimum weight of stem; used for retranslocation to grain
// set the minimum weight of leaf; used for translocation to grain

        dm_plant_stem = divide (dm_green[stem], g_plants, 0.0);
        dm_plant_min[stem] = dm_plant_stem * (1.0 - c_stem_trans_frac);
        dm_plant_leaf = divide (dm_green[leaf], g_plants, 0.0);
        dm_plant_min[leaf] = dm_plant_leaf * (1.0 - c_leaf_trans_frac);
        dm_plant_min[pod] = 0.0;
        }
   else
        {
        // no changes
        }


   dm_plant_pod = divide (dm_green[pod], g_plants, 0.0);
   dm_plant_min[pod] = max (dm_plant_pod * (1.0 - c_pod_trans_frac), dm_plant_min[pod]);

   return;
   }

//+  Purpose
//       Calculate the critical N concentration below which plant growth
//       is affected.  Also minimum and maximum N concentrations below
//       and above which it is not allowed to fall or rise.
//       These are analogous to the water concentrations
//       of sat, dul and ll.

//+  Mission Statement
//     Calculate the critical N concentration below which plant growth is affected.

//+  Changes
//       080994 jngh specified and programmed
void Plant::plant_n_conc_limits
    (
     float  *c_stage_code_list                  // (INPUT)  list of stage numbers
    ,float  *g_phase_tt                         // (INPUT)  Cumulative growing degree days
    ,float  *g_tt_tot                           // (INPUT)  the sum of growing degree days
    ,float  c_n_conc_crit_meal                 // (INPUT)  critical N concentration of gr
    ,float  c_n_conc_crit_root                 // (INPUT)  critical N concentration of ro
    ,float  c_n_conc_max_meal                  // (INPUT)  maximum N concentration of gra
    ,float  c_n_conc_max_root                  // (INPUT)  maximum N concentration of roo
    ,float  c_n_conc_min_meal                  // (INPUT)  minimum N concentration of gra
    ,float  c_n_conc_min_root                  // (INPUT)  minimum N concentration of roo
    ,float  *c_x_stage_code                     // (INPUT)  stage table for N concentratio
    ,float  *c_y_n_conc_crit_leaf               // (INPUT)  critical N concentration of
    ,float  *c_y_n_conc_crit_pod                // (INPUT)  critical N concentration of p
    ,float  *c_y_n_conc_crit_stem               // (INPUT)  critical N concentration of
    ,float  *c_y_n_conc_max_leaf                // (INPUT)  maximum N concentration of le
    ,float  *c_y_n_conc_max_pod                 // (INPUT)  maximum N concentration of pod
    ,float  *c_y_n_conc_max_stem                // (INPUT)  maximum N concentration of st
    ,float  *c_y_n_conc_min_leaf                // (INPUT)  minimum N concentration of le
    ,float  *c_y_n_conc_min_pod                 // (INPUT)  minimum N concentration of pod
    ,float  *c_y_n_conc_min_stem                // (INPUT)  minimum N concentration of st
    ,float  *c_x_co2_nconc_modifier
    ,float  *c_y_co2_nconc_modifier
    ,int    c_num_co2_nconc_modifier
    ,float  g_co2
    ,float  g_current_stage                    // (INPUT)  current phenological stage
    ,float  *n_conc_crit                        // (OUTPUT) critical N concentration  (g N/g part)
    ,float  *n_conc_max                         // (OUTPUT) maximum N concentration   (g N/g part)
    ,float  *n_conc_min                         // (OUTPUT) minimum N concentration    g N/g part)
    ) {

//+  Constant Values
//+  Local Variables
    int   numvals;                                // number of values in stage code table
    float stage_code;                             // interpolated current stage code

//- Implementation Section ----------------------------------


    fill_real_array (n_conc_crit, 0.0, max_part);
    fill_real_array (n_conc_min, 0.0, max_part);

//    if (stage_is_between (emerg, maturity, g_current_stage))
//        {

//jh set elsewhere   N_conc_crit[meal] = c_n_conc_crit_meal
//jh set elsewhere   N_conc_max[meal] = c_n_conc_max_meal
//jh set elsewhere   N_conc_min[meal] = c_n_conc_min_meal

        n_conc_crit[oil] = 0.0;
        n_conc_max[oil] = 0.0;
        n_conc_min[oil] = 0.0;

        n_conc_crit[root] = c_n_conc_crit_root;
        n_conc_max[root] = c_n_conc_max_root;
        n_conc_min[root] = c_n_conc_min_root;

// the tops critical N percentage concentration is the stover
// (non-grain shoot) concentration below which N concentration
// begins to affect plant growth.

        numvals = 1+count_of_real_vals (c_x_stage_code, max_stage); //not an index
        stage_code = plant_stage_code(c_stage_code_list
                                    , g_phase_tt
                                    , g_tt_tot
                                    , g_current_stage
                                    , c_x_stage_code
                                    , numvals, max_stage);
//       stage_code = crop_stage_code(c_stage_code_list
//                                    , g_tt_tot
//                                    , g_phase_tt
//                                    , g_current_stage
//                                    , c_x_stage_code
//                                    , numvals, max_stage);
        n_conc_crit[stem] = linear_interp_real (stage_code
                                                , c_x_stage_code
                                                , c_y_n_conc_crit_stem
                                                , numvals);
        n_conc_crit[leaf] = linear_interp_real (stage_code
                                                , c_x_stage_code
                                                , c_y_n_conc_crit_leaf
                                                , numvals);
        n_conc_crit[pod] = linear_interp_real (stage_code
                                               , c_x_stage_code
                                               , c_y_n_conc_crit_pod
                                               , numvals);

// the  minimum N concentration is the N concentration
// below which N does not fall.

        n_conc_min[stem] = linear_interp_real (stage_code
        , c_x_stage_code
        , c_y_n_conc_min_stem
        , numvals);

        n_conc_min[leaf] = linear_interp_real (stage_code
        , c_x_stage_code
        , c_y_n_conc_min_leaf
        , numvals);

        n_conc_min[pod] = linear_interp_real (stage_code
        , c_x_stage_code
        , c_y_n_conc_min_pod
        , numvals);

// the  maximum N concentration is the N concentration
// above which N does not rise.

        n_conc_max[stem] = linear_interp_real (stage_code
        , c_x_stage_code
        , c_y_n_conc_max_stem
        , numvals);

        n_conc_max[leaf] = linear_interp_real (stage_code
        , c_x_stage_code
        , c_y_n_conc_max_leaf
        , numvals);

        n_conc_max[pod] = linear_interp_real (stage_code
        , c_x_stage_code
        , c_y_n_conc_max_pod
        , numvals);

        float co2_modifier = linear_interp_real(g_co2,
                                                c_x_co2_nconc_modifier,
                                                c_y_co2_nconc_modifier,
                                                c_num_co2_nconc_modifier);
        n_conc_crit[leaf] *= co2_modifier;
        if (n_conc_crit[leaf] <= n_conc_min[leaf])
           {
           throw std::runtime_error("Aiieeee nconc_crit < nconc_min!");
           }
//        }
    }


//+  Purpose
//       Calculate the critical N concentration for grain below which plant growth
//       is affected.  Also minimum and maximum N concentrations below
//       and above which it is not allowed to fall or rise.
//       These are analogous to the water concentrations
//       of sat, dul and ll.

//+  Mission statement
//       Calculate the critical N concentration for grain

//+  Changes
//       241100 jngh specified and programmed
void Plant::plant_n_conc_grain_limits
    (
     float  c_n_conc_crit_grain             // (INPUT)  critical N concentration of gr
    ,float  c_n_conc_max_grain              // (INPUT)  maximum N concentration of gra
    ,float  c_n_conc_min_grain              // (INPUT)  minimum N concentration of gra
    ,float  g_current_stage                 // (INPUT)  current phenological stage
    ,float  *g_dlt_dm_green_retrans         // (INPUT)  plant biomass growth (g/m^2)
    ,float  *g_dlt_dm_green                 // (INPUT)  plant biomass growth (g/m^2)
    ,float  *g_dm_green                     // (INPUT)  plant biomass (g/m^2)
    ,float  *n_conc_crit                    // (OUTPUT) critical N concentration (g N/g part)
    ,float  *n_conc_max                     // (OUTPUT) maximum N concentration (g N/g part)
    ,float  *n_conc_min                     // (OUTPUT) minimum N concentration (g N/g part)
    ) {
//+  Local Variables
    float dm_oil;                                 // oil mass (g/m2)
    float dm_meal;                                // meal mass (g/m2)
    float dm_grain;                               // grain mass (g/m2)
    float n_crit_grain;                           // critial mass of grain N (g/m2)
    float n_max_grain;                            // maximum mass of grain N (g/m2)
    float n_min_grain;                            // minimum mass of grain N (g/m2)

//- Implementation Section ----------------------------------

    if (stage_is_between (start_grain_fill, maturity, g_current_stage))
        {
        dm_oil = g_dm_green[oil]
                     + g_dlt_dm_green[oil]
                     + g_dlt_dm_green_retrans[oil];
        dm_meal = g_dm_green[meal]
                     + g_dlt_dm_green[meal]
                     + g_dlt_dm_green_retrans[meal];
        dm_grain = dm_oil + dm_meal;

        n_crit_grain = c_n_conc_crit_grain * dm_grain;
        n_max_grain = c_n_conc_max_grain * dm_grain;
        n_min_grain = c_n_conc_min_grain * dm_grain;

        n_conc_crit[meal] = divide (n_crit_grain, dm_meal, 0.0);
        n_conc_max[meal] = divide (n_max_grain, dm_meal, 0.0);
        n_conc_min[meal] = divide (n_min_grain, dm_meal, 0.0);
        }
    }


//+  Purpose
//       Set plant nitrogen

//+  Mission Statement
//     Set plant nitrogen

//+  Changes
//     010994 jngh specified and programmed
void Plant::plant_n_init
    (
     float  c_n_leaf_init_conc                  // (INPUT)  initial leaf N concentration (
    ,float  c_n_root_init_conc                  // (INPUT)  initial root N concentration (
    ,float  c_n_stem_init_conc                  // (INPUT)  initial stem N concentration (
    ,float  g_current_stage                     // (INPUT)  current phenological stage
    ,float  *g_dm_green                         // (INPUT)  live plant dry weight (biomass
    ,float  *n_green                            // (OUTPUT) plant nitrogen (g/m^2)
    ) {

    if (on_day_of (emerg, g_current_stage))
        {
        n_green[root] = c_n_root_init_conc*g_dm_green[root];
        n_green[stem] = c_n_stem_init_conc*g_dm_green[stem];
        n_green[leaf] = c_n_leaf_init_conc*g_dm_green[leaf];
        n_green[pod] = 0.0;
        n_green[meal] = 0.0;
        n_green[oil] = 0.0;

        }
    }



//+  Purpose
//       Return actual plant nitrogen uptake to each plant part.

//+  Mission Statement
//     Calculate actual plant nitrogen uptake to each plant part

//+  Changes
//       080994 jngh specified and programmed
//       150995 psc  mungbpea + fixation
void Plant::legnew_n_partition
    (float  *g_dlayer            // (INPUT)  thickness of soil layer I (mm)
    ,float  *g_dlt_no3gsm        // (INPUT)  actual NO3 uptake from soil (g
    ,float  *g_dlt_nh4gsm        // (INPUT)  actual NO3 uptake from soil (g
    ,float  *g_n_demand          // (INPUT)  critical plant nitrogen demand
    ,float  g_n_fix_pot         // (INPUT)  N fixation potential (g/m^2)
    ,float  *g_n_max             // (INPUT)  maximum plant nitrogen demand
    ,float  g_root_depth        // (INPUT)  depth of roots (mm)
    ,float  *dlt_n_green         // (OUTPUT) actual plant N uptake into each plant part (g/m^2)
    ,float  *n_fix_uptake        // (OUTPUT) actual N fixation (g/m^2)
    ) {

//+  Constant Values
    const char*  my_name = "legnew_n_partition" ;

//+  Local Variables
    int   deepest_layer;                          // deepest layer in which the roots are growing
    float plant_part_fract;                       // fraction of nitrogen to use (0-1) for plant part
    int   part;                                   // plant part number
    float n_uptake_sum;                           // total plant N uptake (g/m^2)
    float n_excess;                               // N uptake above N crit (g/m^2)
    float n_capacity[max_part];                   // amount of N that can be stored in plant part above Ncrit (g/m^2)
    float n_capacity_sum;                         // total excess N storage (g/m^2)
    float n_demand;                               // total nitrogen demand (g/m^2)
    float n_fix_demand_tot;                       // total demand for N fixation (g/m^2)
    float fix_demand;                             // demand for fixed N per plant part (g/m^
    float fix_part_fract;                         // fraction of fixed N per plant part (g/m

//- Implementation Section ----------------------------------

    push_routine (my_name);

// find proportion of uptake to be
// distributed to to each plant part and distribute it.

    deepest_layer = find_layer_no (g_root_depth, g_dlayer, max_layer);
    n_uptake_sum = - sum_real_array (g_dlt_no3gsm, deepest_layer+1)
                   - sum_real_array (g_dlt_nh4gsm, deepest_layer+1);
    n_demand = sum_real_array (g_n_demand, max_part);

    n_excess = n_uptake_sum - n_demand;
    n_excess = l_bound (n_excess, 0.0);

    if (n_excess>0.0)
        {
        for (part = 0; part < max_part; part++)
          {
          n_capacity[part] = g_n_max[part] - g_n_demand[part];
          }
        n_capacity[meal] = 0.0;
        n_capacity[oil] = 0.0;
        }
    else
        {
        fill_real_array (n_capacity, 0.0, max_part);
        }

    n_capacity_sum = sum_real_array (n_capacity, max_part);

    for (part = 0; part < max_part; part++)
        {
        if (n_excess>0.0)
            {
            plant_part_fract = divide (n_capacity[part], n_capacity_sum, 0.0);
            dlt_n_green[part] = g_n_demand[part] + n_excess * plant_part_fract;
            }
        else
            {
            plant_part_fract = divide (g_n_demand[part], n_demand, 0.0);
            dlt_n_green[part] = n_uptake_sum * plant_part_fract;
            }
        }
//cnh    dlt_n_green[meal] = 0.0;
    dlt_n_green[oil] = 0.0;

    if (!reals_are_equal(sum_real_array (dlt_n_green, max_part) - n_uptake_sum, 0.0))
        {
        string msg ="dlt_n_green mass balance is off: "
              + ftoa(sum_real_array (dlt_n_green, max_part), ".6")
              + " vs "
              + ftoa(n_uptake_sum, ".6");
        parent->warningError(msg.c_str());
        }

    n_fix_demand_tot = l_bound (n_demand - n_uptake_sum, 0.0);

    *n_fix_uptake = bound (g_n_fix_pot, 0.0, n_fix_demand_tot);

    for (part = 0; part < max_part; part++ )
         {
         fix_demand = l_bound (g_n_demand[part] - dlt_n_green[part], 0.0);
         fix_part_fract = divide (fix_demand, n_fix_demand_tot, 0.0);
         dlt_n_green[part] = dlt_n_green[part] + fix_part_fract * (*n_fix_uptake);
         }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Calculate grain oil factors

//+  Mission statement
//        Calculate grain oil factors

//+  Changes
//      141100 jngh specified and programmed
void Plant::legnew_bio_grain_oil (
     float  c_grain_oil_conc          // (INPUT) fractional oil content of grain (0-1)
    ,float  c_carbo_oil_conv_ratio    // (INPUT) Carbohydrate:oil conversion ratio (>= 1.0)
    ,float  *grain_energy              // (OUTPUT) multiplier of grain weight to account
    ) {                               // for seed energy content (>= 1.0)

//+  Constant Values
    const char*  my_name = "legnew_bio_grain_oil" ;

//- Implementation Section ----------------------------------
    push_routine (my_name);

    *grain_energy = 1.0 + c_grain_oil_conc * (c_carbo_oil_conv_ratio - 1.0);

    bound_check_real_var (parent,*grain_energy
                          , 1.0
                          , 2.0
                          , "grain_energy");

    pop_routine (my_name);
    return;
    }


//+  Purpose
//        Find grain demand for carbohydrate using harvest index (g/m^2)

//+  Mission Statement
//   Calculate yield component biomass demand using harvest index increments

//+  Changes
//     010994 jngh specified and programmed
void Plant::legnew_bio_yieldpart_demand1
    (float g_current_stage                     // (INPUT)  current phenological stage
    ,float c_twilight                          // (INPUT)  twilight in angular distance b
    ,int   g_day_of_year                       // (INPUT)  day of year
    ,float g_latitude                          // (INPUT)  latitude (degrees, negative fo
    ,int   start_stress_stage                  // (INPUT)
    ,int   start_grainfill_stage               // (INPUT)
    ,int   end_grainfill_stage                 // (INPUT)
    ,int  *yield_parts                         // (INPUT)
    ,int   num_yield_parts                     // (INPUT)
    ,int   root_part                           // (INPUT)
    ,int   max_part                            // (INPUT)
    ,float g_dlt_dm                            // (INPUT)  the daily biomass production (
    ,float *g_dm_green                         // (INPUT)  live plant dry weight (biomass
    ,float *g_dm_senesced                      // (INPUT)  senesced plant dry wt (g/m^2)
    ,float *g_days_tot                         // (INPUT)  duration of each phase (days)
    ,float *g_dm_stress_max                    // (INPUT)  sum of maximum daily stress on
    ,float *p_x_pp_hi_incr                     // (INPUT)
    ,float *p_y_hi_incr                        // (INPUT)  harvest index increment per da
    ,int   p_num_pp_hi_incr                    // (INPUT)
    ,float *p_x_hi_max_pot_stress              // (INPUT) Potential Max HI Stress dete
    ,float *p_y_hi_max_pot                     // (INPUT) Potential Max HI
    ,int   p_num_hi_max_pot                    // (INPUT) Number of lookup pairs
    ,float g_grain_energy                      // (INPUT)
    ,float *dlt_dm_yieldpart_demand             // (OUTPUT) grain dry matter potential (g/m^2)
    ) {

//+  Constant Values
    const char*  my_name = "legnew_bio_yieldpart_demand1" ;

//+  Local Variables
    float ave_stress;                             // average dm_stress from flowering to gra
    float stress_sum;                             // total    "          "     "      "    "
    float days_sum;                               // total    days       "     "      "    "
    float dlt_dm_yield;                           // grain demand for carbohydrate (g/m^2)
    float dlt_dm_yield_unadj;                     // grain demand for carbohydrate, unadjusted
// for energy conversion (g/m^2)
    float dm_tops;                                // drymatter of tops (g/m^2)
    float harvest_index;                          // last harvest index (g grain/g biomass)
    float hi_max_pot;                             // max potential HI due to stress
    float dm_tops_new;                            // new drymatter  tops (g/m^2)
    float harvest_index_new;                      // next harvest index (g grain/g biomass)
    float dm_grain_new;                           // new drymatter grain (g/m^2)
    float dm_green_yield_parts;                   // dry matter of yield parts (g/m^2)
    float energy_adjust;                          // adjustment for energy used in oil conversion.
    int   indx;                                   // loop index
    float hi_incr;                                // harvest index increment per day
    float photoperiod;                            // hours of photosynthetic light (hours)

//- Implementation Section ----------------------------------

    push_routine (my_name);

    if (stage_is_between (start_grainfill_stage
                          , end_grainfill_stage
                          , g_current_stage))
        {
        stress_sum = sum_between (start_stress_stage-1
                                  ,start_grainfill_stage-1
                                  ,g_dm_stress_max);
        days_sum = sum_between (start_stress_stage-1
                                ,start_grainfill_stage-1
                                ,g_days_tot);
        ave_stress = divide (stress_sum, days_sum, 1.0);
        hi_max_pot = linear_interp_real(ave_stress
                                        ,p_x_hi_max_pot_stress
                                        ,p_y_hi_max_pot
                                        ,p_num_hi_max_pot);

        photoperiod = day_length (g_day_of_year, g_latitude, c_twilight);

        hi_incr = linear_interp_real(photoperiod
                                    ,p_x_pp_hi_incr
                                    ,p_y_hi_incr
                                    ,p_num_pp_hi_incr);

    // effective grain filling period

        dm_tops = sum_real_array (g_dm_green, max_part)
                         - g_dm_green[root_part]
                         + sum_real_array (g_dm_senesced, max_part)
                         - g_dm_senesced[root_part];
        dm_green_yield_parts = 0.0;

        for (indx = 0; indx < num_yield_parts; indx++)
            {
            dm_green_yield_parts = dm_green_yield_parts
                         + g_dm_green[yield_parts[indx]];
            }
        harvest_index = divide (dm_green_yield_parts, dm_tops, 0.0);
        dm_tops_new = dm_tops + g_dlt_dm;

        harvest_index_new = u_bound (harvest_index + hi_incr, hi_max_pot);

        dm_grain_new = dm_tops_new * harvest_index_new;
        dlt_dm_yield_unadj = dm_grain_new - dm_green_yield_parts;

    // adjust for grain energy

        dlt_dm_yield_unadj = bound (dlt_dm_yield_unadj, 0.0, dm_grain_new);

        energy_adjust = divide (g_grain_energy
                                , 1.0 + harvest_index_new*(g_grain_energy - 1.0)
                                , 0.0);

        dlt_dm_yield = dlt_dm_yield_unadj * energy_adjust;
    //jh         dlt_dm_yield = dlt_dm_yield_unadj
        }
    else
        {
        // we are out of grain fill period
        dlt_dm_yield = 0.0;
        }


    *dlt_dm_yieldpart_demand = dlt_dm_yield;

    pop_routine (my_name);
    return;
    }



//+  Purpose
//       Partitions new dm (assimilate) between plant components (g/m^2)

//+  Mission Statement
//     Partitions new biomass between plant components

//+  Changes
//       010994 jngh specified and programmed
//       250495 psc  modified dlt_dm_green(grain) to account for barren heads
//       180597 mjr  modified to account for partitioning to leaf during grainfil
//                     and partitioning to energy pool
void Plant::legnew_dm_partition1
    (
     float  c_frac_leaf                   // (INPUT)  fraction of remaining dm allocated to leaf
    ,float  c_frac_pod                    // (INPUT)  fraction of remaining dm allocated to pod
    ,float  g_grain_energy                // multiplier of grain weight to account f
    ,float  c_grain_oil_conc              // multiplier of grain weight to account f
    ,float  c_ratio_root_shoot            // (INPUT)  root:shoot ratio of new dm ()
    ,float  c_sla_min                     // (INPUT)  minimum specific leaf area for
    ,double g_dlt_dm                      // (INPUT)  the daily biomass production (
    ,float  g_dlt_dm_grain_demand         // (INPUT)  grain dm demand (g/m^2)
    ,float  g_dlt_lai_stressed            // (INPUT)  potential change in live
    ,float  *dlt_dm_oil_conv               // (OUTPUT) actual biomass used in conversion to oil (g/m2)
    ,float  *dlt_dm_green                  // (OUTPUT) actual biomass partitioned to plant parts (g/m^2)
    ) {

//+  Constant Values
    const char*  my_name = "legnew_dm_partition1" ;

//+  Local Variables
    double dlt_dm_green_tot;                       // total of partitioned dm (g/m^2)
    double dlt_dm_leaf_max;                        // max increase in leaf dm (g/m^2)
//jh redundant      real       partition_grain       ! fraction of dm partitioned to grain
//jh redundant                                       ! versus pod
    double dm_remaining;                           // interim dm pool for partitioning
    double yield_demand;                           // sum of grain, energy & pod
    double dm_grain_demand;                        // assimilate demand for grain (g/m^2)
    double dm_meal_demand;                         // assimilate demand for meal (g/m^2)
    double dm_oil_demand;                          // assimilate demand for oil (g/m^2)
    double dm_oil_conv_demand;                     // assimilate demand for conversion to oil (g/m^2)
    double dm_pod_demand;                          // assimilate demand for pod (g/m^2)

//- Implementation Section ----------------------------------
    push_routine (my_name);

// Root must be satisfied. The roots don't take any of the
// carbohydrate produced - that is for tops only.  Here we assume
// that enough extra was produced to meet demand. Thus the root
// growth is not removed from the carbo produced by the model.

    // first we zero all plant component deltas
    fill_real_array (dlt_dm_green, 0.0, max_part);

    // now we get the root delta for all stages - partition scheme
    // specified in coeff file
    dlt_dm_green[root] = c_ratio_root_shoot * g_dlt_dm;

    // calculate demands of reproductive parts
    dm_grain_demand = divide (g_dlt_dm_grain_demand, g_grain_energy, 0.0);

    dm_meal_demand = dm_grain_demand * (1.0 - c_grain_oil_conc);
    dm_oil_demand = dm_grain_demand - dm_meal_demand;
    dm_oil_conv_demand = g_dlt_dm_grain_demand - dm_grain_demand;

    if (dm_grain_demand > 0.0)
        {
        dm_pod_demand = dm_grain_demand * c_frac_pod;
        }
    else
        {
        dm_pod_demand = g_dlt_dm * c_frac_pod;
        }
    yield_demand = dm_pod_demand
      + dm_meal_demand
      + dm_oil_demand
      + dm_oil_conv_demand;

    // now distribute the assimilate to plant parts
    if (yield_demand >= g_dlt_dm)
        {
        // reproductive demand exceeds supply - distribute assimilate to those parts only
        dlt_dm_green[meal] = g_dlt_dm
             * divide (dm_meal_demand, yield_demand, 0.0);
        dlt_dm_green[oil] = g_dlt_dm
             * divide (dm_oil_demand, yield_demand, 0.0);
        *dlt_dm_oil_conv = g_dlt_dm
             * divide (dm_oil_conv_demand, yield_demand, 0.0);
        dlt_dm_green[pod] = g_dlt_dm
          - dlt_dm_green[meal]
          - dlt_dm_green[oil]
          - (*dlt_dm_oil_conv);
        dlt_dm_green[stem] = 0.0;
        dlt_dm_green[leaf] = 0.0;
        }
    else
        {
        // more assimilate than needed for reproductive parts
        // distribute to all parts

        // satisfy reproductive demands
        dlt_dm_green[meal]   = dm_meal_demand;
        dlt_dm_green[oil]    = dm_oil_demand;
        *dlt_dm_oil_conv      = dm_oil_conv_demand;
        dlt_dm_green[pod]    = dm_pod_demand;

        // distribute remainder to vegetative parts
        dm_remaining = g_dlt_dm - yield_demand;
        dlt_dm_green[leaf] = c_frac_leaf * dm_remaining;

        // limit the delta leaf area to maximum
        dlt_dm_leaf_max = divide (g_dlt_lai_stressed, c_sla_min * smm2sm, 0.0);
        dlt_dm_green[leaf] = u_bound (dlt_dm_green[leaf], dlt_dm_leaf_max);

        dm_remaining = dm_remaining - dlt_dm_green[leaf];
        dlt_dm_green[stem] = dm_remaining;
        }

    // do mass balance check - roots are not included
    dlt_dm_green_tot = sum_real_array (dlt_dm_green, max_part)
                          - dlt_dm_green[root]
                          + *dlt_dm_oil_conv;

    if (!reals_are_equal(dlt_dm_green_tot, g_dlt_dm, 1.0E-4))  // XX this is probably too much slop - try doubles XX
      {
      string msg = "dlt_dm_green_tot mass balance is off: "
          + ftoa(dlt_dm_green_tot, ".6")
          + " vs "
          + ftoa(g_dlt_dm, ".6");
      parent->warningError(msg.c_str());
      }

    // check that deltas are in legal range
    bound_check_real_array (parent,dlt_dm_green, max_part, 0.0, g_dlt_dm, "dlt_dm_green");

//    fprintf(stdout, "%d,%.9f,%.9f,%.9f,%.9f\n", g.day_of_year,
//            g.dm_green[root] + g.dm_green[leaf] + g.dm_green[stem],
//            g.dm_green[root], g.dm_green[leaf],g.dm_green[stem]);

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Partitions new dm (assimilate) between plant components (g/m^2)

//+  Mission Statement
//     Partitions new biomass between plant components

//+  Changes
//       010994 jngh specified and programmed
//       250495 psc  modified dlt_dm_green(grain) to account for barren heads
//       180597 mjr  modified to account for partitioning to leaf during grainfil
//                     and partitioning to energy pool
void Plant::legnew_dm_partition2
    (
     float  g_current_stage
    ,float  *c_x_stage_no_partition
    ,float  *c_y_frac_leaf
    ,float  *c_y_frac_pod
    ,int    c_num_stage_no_partition
    ,float  g_grain_energy                      // multiplier of grain weight to account f
    ,float  c_grain_oil_conc                    // multiplier of grain weight to account f
    ,float  *c_y_ratio_root_shoot               // (INPUT)  root:shoot ratio of new dm ()
    ,float  c_sla_min                           // (INPUT)  minimum specific leaf area for
    ,double  g_dlt_dm                            // (INPUT)  the daily biomass production (
    ,float  g_dlt_dm_grain_demand               // (INPUT)  grain dm demand (g/m^2)
    ,float  g_dlt_lai_stressed                  // (INPUT)  potential change in live
    ,float  *dlt_dm_oil_conv                     // (OUTPUT) actual biomass used in conversion to oil (g/m2)
    ,float  *dlt_dm_green                       // (OUTPUT) actual biomass partitioned to plant parts (g/m^2)
    ) {
//XX//    float c_frac_leaf;                            // (INPUT)  fraction of remaining dm allocated to leaf
//XX//    float c_frac_pod;                             // (INPUT)  fraction of remaining dm allocated to pod

//+  Constant Values
    const char*  my_name = "legnew_dm_partition2" ;

//+  Local Variables
    float dlt_dm_green_tot;                       // total of partitioned dm (g/m^2)
    float dlt_dm_leaf_max;                        // max increase in leaf dm (g/m^2)
//jh redundant      real       partition_grain       ! fraction of dm partitioned to grain
//jh redundant                                       ! versus pod
    float dm_remaining;                           // interim dm pool for partitioning
    float yield_demand;                           // sum of grain, energy & pod
    float dm_grain_demand;                        // assimilate demand for grain (g/m^2)
    float dm_meal_demand;                         // assimilate demand for meal (g/m^2)
    float dm_oil_demand;                          // assimilate demand for oil (g/m^2)
    float dm_oil_conv_demand;                     // assimilate demand for conversion to oil (g/m^2)
    float dm_pod_demand;                          // assimilate demand for pod (g/m^2)
    float frac_leaf;
    float frac_pod;
    float ratio_root_shoot;
//- Implementation Section ----------------------------------
    push_routine (my_name);

// Interpolate leaf and pod fractions
    frac_leaf = linear_interp_real(g_current_stage
                                   ,c_x_stage_no_partition
                                   ,c_y_frac_leaf
                                   ,c_num_stage_no_partition);
    frac_pod = linear_interp_real(g_current_stage
                                  ,c_x_stage_no_partition
                                  ,c_y_frac_pod
                                  ,c_num_stage_no_partition);
    ratio_root_shoot = linear_interp_real(g_current_stage
                                          ,c_x_stage_no_partition
                                          ,c_y_ratio_root_shoot
                                          ,c_num_stage_no_partition);
   // Root must be satisfied. The roots don't take any of the
   // carbohydrate produced - that is for tops only.  Here we assume
   // that enough extra was produced to meet demand. Thus the root
   // growth is not removed from the carbo produced by the model.

   // first we zero all plant component deltas

    fill_real_array (dlt_dm_green, 0.0, max_part);

    // now we get the root delta for all stages - partition scheme
    // specified in coeff file

    dlt_dm_green[root] = ratio_root_shoot *g_dlt_dm;

    // calculate demands of reproductive parts

    dm_grain_demand = divide (g_dlt_dm_grain_demand, g_grain_energy, 0.0);

    dm_meal_demand = dm_grain_demand * (1.0 - c_grain_oil_conc);
    dm_oil_demand = dm_grain_demand - dm_meal_demand;
    dm_oil_conv_demand = g_dlt_dm_grain_demand - dm_grain_demand;
    if (dm_grain_demand > 0.0)
        {
        dm_pod_demand = dm_grain_demand * frac_pod;
        }
    else
        {
        dm_pod_demand = g_dlt_dm * frac_pod;
        }
    yield_demand = dm_pod_demand + dm_meal_demand + dm_oil_demand  + dm_oil_conv_demand;

    // now distribute the assimilate to plant parts

    if (yield_demand >= g_dlt_dm)
        {
        // reproductive demand exceeds supply - distribute assimilate to those parts only
        dlt_dm_green[meal] = g_dlt_dm * divide (dm_meal_demand, yield_demand, 0.0);
        dlt_dm_green[oil] = g_dlt_dm * divide (dm_oil_demand, yield_demand, 0.0);
        *dlt_dm_oil_conv = g_dlt_dm * divide (dm_oil_conv_demand, yield_demand, 0.0);
        dlt_dm_green[pod] = g_dlt_dm - dlt_dm_green[meal] - dlt_dm_green[oil] - (*dlt_dm_oil_conv);
        dlt_dm_green[stem] = 0.0;
        dlt_dm_green[leaf] = 0.0;
        }
    else
        {
        // more assimilate than needed for reproductive parts
        // distribute to all parts

        // satisfy reproductive demands

        dlt_dm_green[meal]   = dm_meal_demand;
        dlt_dm_green[oil]    = dm_oil_demand;
        *dlt_dm_oil_conv      = dm_oil_conv_demand;
        dlt_dm_green[pod]    = dm_pod_demand;

        // distribute remainder to vegetative parts

        dm_remaining = g_dlt_dm - yield_demand;

        dlt_dm_green[leaf] = frac_leaf * dm_remaining;

        // limit the delta leaf area to maximum
        dlt_dm_leaf_max = divide (g_dlt_lai_stressed, c_sla_min * smm2sm, 0.0);
        dlt_dm_green[leaf] = u_bound (dlt_dm_green[leaf], dlt_dm_leaf_max);

        dm_remaining = dm_remaining - dlt_dm_green[leaf];
        dlt_dm_green[stem] = dm_remaining;
        }

    // do mass balance check - roots are not included
    dlt_dm_green_tot = sum_real_array (dlt_dm_green, max_part) - dlt_dm_green[root] + *dlt_dm_oil_conv;

    if (!reals_are_equal(dlt_dm_green_tot, g_dlt_dm, 1.0E-4))  //XX too much slop here?
      {
      string msg = "dlt_dm_green_tot mass balance is off: "
          + ftoa(dlt_dm_green_tot, ".6")
          + " vs "
          + ftoa(g_dlt_dm, ".6");
      parent->warningError(msg.c_str());
      }

    // check that deltas are in legal range
    bound_check_real_array (parent,dlt_dm_green, max_part, 0.0, g_dlt_dm, "dlt_dm_green");

    pop_routine (my_name);
    return;
    }

//  Purpose
//      Partitions new dm (assimilate) between plant components (g/m^2)
//
//  Mission Statement
//    Partitions new biomass between plant components
void Plant::legnew_dm_partition3(float  g_current_stage              //
                               , float *g_current_fruit_stage        //
                               , float *c_x_stage_no_partition
                               , float *c_y_frac_leaf                  // (INPUT)  fraction of remaining dm allocated to leaf
                               , float *c_y_frac_pod                   // (INPUT)  fraction of remaining dm allocated to pod
                               , int   c_num_stage_no_partition
                               , float  g_grain_energy               //          multiplier of grain weight to account f
                               , float  c_grain_oil_conc             //          multiplier of grain weight to account f
                               , float *c_y_ratio_root_shoot           // (INPUT)  root:shoot ratio of new dm ()
                               , float  c_sla_min                    // (INPUT)  minimum specific leaf area for
                               , const int   max_fruit_cohorts       //
                               , const int   max_part                //
                               , int   g_num_fruit_cohorts           //
                               , float *g_fruit_no
                               , float  g_dlt_dm                     // (INPUT)  the daily biomass production
                               , float  *g_dlt_dm_grain_demand       // (INPUT)  grain dm demand (g/m^2)
                               , float  *g_dlt_dm_fruit_demand       // (INPUT)  grain dm demand (g/m^2)
                               , float  g_dlt_dm_parasite_demand     //          assimilate demand for parasite (g/m^2)
                               , float  g_dlt_lai_stressed           // (INPUT)  potential change in live
                               , float  *dlt_dm_oil_conv             // (OUTPUT) actual biomass used in conversion to oil (g/m2)
                               , float  *dlt_dm_parasite             // (OUTPUT) actual biomass partitioned to parasite (g/m^2)
                               , float  **dlt_dm_green)              // (OUTPUT) actual biomass partitioned
{
      const char *my_name  = "legnew_dm_partition3";

      //  Local Variables
      float dlt_dm_green_tot;     // total of partitioned dm (g/m^2)
      float dlt_dm_leaf_max;      // max increase in leaf dm (g/m^2)
      float **dlt_dm_demand;// assimilate demand for reproductive parts (g/m^2)
      float dm_remaining;         // interim dm pool for partitioning
      float yield_demand;         // sum of grain, energy & pod
      float dm_grain_demand;      // assimilate demand for grain (g/m^2)
      float *dm_oil_conv_demand; // assimilate demand for conversion to oil (g/m^2)
      int   cohort, part;
//      int   current_phase;
//      int   current_fruit_phase;
      float frac_leaf, frac_pod, ratio_root_shoot;

      push_routine (my_name);
      dlt_dm_demand = new float* [max_fruit_cohorts];
      dm_oil_conv_demand = new float [max_fruit_cohorts];
      for (int cohort = 0; cohort < max_fruit_cohorts; cohort++) {
         dlt_dm_demand[cohort] = new float [max_part];
      }

      // Root must be satisfied. The roots don't take any of the
      // carbohydrate produced - that is for tops only.  Here we assume
      // that enough extra was produced to meet demand. Thus the root
      // growth is not removed from the carbo produced by the model.
      //
      // first we zero all plant component deltas

      for (cohort = 0; cohort < max_fruit_cohorts; cohort++)
         {
         dm_oil_conv_demand[cohort] = 0.0;
         for (part = 0; part < max_part; part++)
             {
             dlt_dm_green[cohort][part] = 0.0;
             dlt_dm_demand[cohort][part] = 0.0;
            }
         }
      *dlt_dm_parasite = 0.0;

//      current_phase = (int)g_current_stage;

      // Interpolate leaf and pod fractions
      frac_leaf = linear_interp_real(g_current_stage
                                   ,c_x_stage_no_partition
                                   ,c_y_frac_leaf
                                   ,c_num_stage_no_partition);

      ratio_root_shoot = linear_interp_real(g_current_stage
                                   ,c_x_stage_no_partition
                                   ,c_y_ratio_root_shoot
                                   ,c_num_stage_no_partition);

      // now we get the root delta for all stages - partition scheme
      // specified in coeff file
      dlt_dm_green[0][root] = ratio_root_shoot * g_dlt_dm;

      dm_remaining = g_dlt_dm;

      // NB. The old fortran implmenetation assumed there's always at least 1 cohort..
      //     I'd suggest we wrap this a with a conditional on (whole plant) fruit filling phase

      // partition to older, filling fruit.
      cohort = 0;
      do {
         if (g_fruit_no[cohort] > 0.0)
      	    {
//      	    current_fruit_phase = (int)g_current_fruit_stage[cohort];
             if (stage_is_between (start_grain_fill, end_grain_fill, g_current_fruit_stage[cohort]))
                {
                // we are in grain filling phase
                // calculate demands of reproductive parts
                frac_pod = linear_interp_real(g_current_fruit_stage[cohort]
                                             ,c_x_stage_no_partition
                                             ,c_y_frac_pod
                                             ,c_num_stage_no_partition);

                legnew_dm_part_demands(
                          frac_pod
                        , g_grain_energy
                        , c_grain_oil_conc
                        , g_dlt_dm_grain_demand[cohort]
                        , &dm_oil_conv_demand[cohort]
                        , &dlt_dm_demand[cohort][meal]
                        , &dlt_dm_demand[cohort][oil]
                        , &dlt_dm_demand[cohort][pod]);

                legnew_dm_distribute(
                          max_part
                        , &dm_remaining
                        , dlt_dm_demand[cohort][meal]
                        , dlt_dm_demand[cohort][oil]
                        , dlt_dm_demand[cohort][pod]
                        , dm_oil_conv_demand[cohort]
                        , &dlt_dm_oil_conv[cohort]
                        , dlt_dm_green[cohort] );
                }
             else
                {
                // not in grain filling phase
                dlt_dm_green[cohort][meal] = 0.0;
                dlt_dm_green[cohort][oil]  = 0.0;
                dlt_dm_oil_conv[cohort]   = 0.0;
                dlt_dm_green[cohort][pod]  = 0.0;
                }
             }
         else
            {
            // no fruit in this cohort
            dlt_dm_green[cohort][meal] = 0.0;
            dlt_dm_green[cohort][oil]  = 0.0;
            dlt_dm_oil_conv[cohort]   = 0.0;
            dlt_dm_green[cohort][pod]  = 0.0;
            }

         cohort++;
         } while (cohort < g_num_fruit_cohorts);

      // partition to parasite
      if (g_dlt_dm_parasite_demand > dm_remaining)
         {
         *dlt_dm_parasite = dm_remaining;
         dm_remaining = 0.0;
         }
      else
         {
         *dlt_dm_parasite = g_dlt_dm_parasite_demand;
         dm_remaining = dm_remaining - *dlt_dm_parasite;
         }

      // partition to young fruit
      cohort = 0;
      do {
      	if (g_fruit_no[cohort] > 0.0)
      	    {
             if (stage_is_between (flowering, start_grain_fill,
                                   g_current_fruit_stage[cohort]))
                {
                // in flowering phase
                dlt_dm_green[cohort][meal] = 0.0;
                dlt_dm_green[cohort][oil]  = 0.0;
                dlt_dm_oil_conv[cohort]   = 0.0;
                dlt_dm_demand[cohort][pod] = g_dlt_dm_fruit_demand[cohort];
                legnew_dm_distribute ( max_part
                                      , &dm_remaining
                                      , dlt_dm_demand[cohort][meal]
                                      , dlt_dm_demand[cohort][oil]
                                      , dlt_dm_demand[cohort][pod]
                                      , dm_oil_conv_demand[cohort]
                                      , &dlt_dm_oil_conv[cohort]
                                      , dlt_dm_green[cohort]);
                }
             }
          else
             {
             // No fruit in this cohort
             dlt_dm_green[cohort][meal] = 0.0;
             dlt_dm_green[cohort][oil]  = 0.0;
             dlt_dm_oil_conv[cohort]   = 0.0;
             dlt_dm_green[cohort][pod]  = 0.0;
             }
         cohort++;
         } while (cohort < g_num_fruit_cohorts);

      // distribute remainder to vegetative parts
      if (dm_remaining > 0.0)
         {
         // we have some assimilate left to partition
         dlt_dm_green[0][leaf] = frac_leaf * dm_remaining;

         // limit the delta leaf area to maximum
         dlt_dm_leaf_max = divide (g_dlt_lai_stressed, c_sla_min * smm2sm, 0.0);
         dlt_dm_green[0][leaf] = u_bound (dlt_dm_green[0][leaf], dlt_dm_leaf_max);
         dm_remaining = dm_remaining - dlt_dm_green[0][leaf];
         dlt_dm_green[0][stem] = dm_remaining;
         }
      else
         {
         // no assimilate left
         dlt_dm_green[0][leaf] = 0.0;
         dlt_dm_green[0][stem] = 0.0;
         }

      // do mass balance check - roots are not included
      dlt_dm_green_tot  = 0.0;
      cohort = 0;
      do {
         dlt_dm_green_tot += sum_real_array (dlt_dm_green[cohort], max_part)
                                 - dlt_dm_green[cohort][root];
         cohort++;
         } while (cohort < g_num_fruit_cohorts);

      dlt_dm_green_tot = dlt_dm_green_tot
                             + sum_real_array(dlt_dm_oil_conv, max_fruit_cohorts)
                             + *dlt_dm_parasite;

      if (!reals_are_equal(dlt_dm_green_tot, g_dlt_dm, 1.0E-4))    // Slop?
         {
         string msg = "dlt_dm_green_tot mass balance is off: "
             + ftoa(dlt_dm_green_tot, ".6")
             + " vs "
             + ftoa(g_dlt_dm, ".6");
         parent->warningError(msg.c_str());
         }

      // check that deltas are in legal range
      float *sum = new float[max_part];
      for (part=0; part < max_part; part++)
        {
        sum[part] = 0.0;
        cohort = 0;
        do {
           sum[part] += dlt_dm_green[cohort][part];
           cohort++;
           } while (cohort < g_num_fruit_cohorts);
        }
      bound_check_real_array(parent,sum, max_part, 0.0, g_dlt_dm, "dlt_dm_green");
      bound_check_real_var (parent,*dlt_dm_parasite, 0.0, g_dlt_dm, "dlt_dm_parasite");

      delete [] sum;
      for (int cohort = 0; cohort < max_fruit_cohorts; cohort++) {
         delete [] dlt_dm_demand [cohort];
      }
      delete [] dlt_dm_demand;
      delete [] dm_oil_conv_demand;
      pop_routine (my_name);
}


//+  Purpose
//     Calculate plant dry matter delta's due to retranslocation
//     to grain, pod and energy (g/m^2)

//+  Mission Statement
//   Calculate biomass retranslocation to the yield component

//+  Changes
//       150900 jngh specified and programmed
void Plant::legnew_dm_retranslocate1
    (
     float  c_frac_pod                    // (INPUT) fraction of remaining dm allocated to pod
    ,float  g_grain_energy                // (INPUT) multiplier of grain weight to account for energy used in oil conversion.
    ,float  c_grain_oil_conc              // (INPUT) fraction of grain that is oil
    ,int    pod                           // (INPUT)
    ,int    meal                          // (INPUT)
    ,int    oil                           // (INPUT)
    ,int    max_part                      // (INPUT)
    ,int    *supply_pools                 // (INPUT)
    ,int    num_supply_pools              // (INPUT)
    ,float  g_dlt_dm_grain_demand         // (INPUT)  grain dm demand (g/m^2)
    ,float  g_dlt_dm_oil_conv             // (INPUT)  dm used in oil conversion (g/m^2)
    ,float  *g_dlt_dm_green               // (INPUT)  plant biomass growth (g/m^2)
    ,float  *g_dm_green                   // (INPUT)  live plant dry weight (biomass
    ,float  *g_dm_plant_min               // (INPUT)  minimum weight of each plant p
    ,float  g_plants                      // (INPUT)  Plant density (plants/m^2)
    ,float  *dm_oil_conv_retranslocate    // (OUTPUT) assimilate used for oil conversion - energy (g/m^2)
    ,float  *dm_retranslocate             // (OUTPUT) actual change in plant part weights due to translocation (g/m^2)
    ) {

//+  Constant Values
    const char*  my_name = "legnew_dm_retranslocate1" ;

//+  Local Variables
    int   part;                                   // plant part no.
    float dlt_dm_retrans_part;                    // carbohydrate removed from part (g/m^2)
    float dlt_dm_retrans_total;                   // total carbohydrate removed from parts (g/m^2)
    float yield_demand_differential;              // demand in excess of available supply (g/m^2)
    float demand_differential;                    // demand in excess of available supply (g/m^2)
    int   counter;
    float dm_part_avail;                          // carbohydrate avail from part(g/m^2)
    float dm_part_pot;                            // potential part weight (g/m^2)
    float dm_demand_differential;                 // assimilate demand by grain - meal + oil + energy (g/m^2)
    float dm_grain_demand_differential;           // assimilate demand for grain - meal + oil (g/m^2)
    float dm_oil_demand_differential;             // assimilate demand for oil (g/m^2)
    float dm_meal_demand_differential;            // assimilate demand for meal (g/m^2)
    float dm_pod_demand_differential;             // assimilate demand for pod (g/m^2)
    float dm_oil_conv_demand_differential;        // assimilate demand for oil conversion - energy (g/m^2)
    float dlt_dm_grain;                           // assimilate used to produce grain and oil in partitioning (g/m^2)

//- Implementation Section ----------------------------------

    push_routine (my_name);

// now translocate carbohydrate between plant components
// this is different for each stage

    fill_real_array (dm_retranslocate, 0.0, max_part);

    dlt_dm_grain = g_dlt_dm_green[meal]
    + g_dlt_dm_green[oil]
    + g_dlt_dm_oil_conv;

    if (g_dlt_dm_grain_demand > dlt_dm_grain)
        {
// we can translocate source carbohydrate
// to reproductive parts if needed

// calculate demands for each reproductive part

        dm_demand_differential = g_dlt_dm_grain_demand
        - dlt_dm_grain;

        dm_grain_demand_differential = divide (dm_demand_differential
        , g_grain_energy, 0.0);

        dm_meal_demand_differential = dm_grain_demand_differential
        * (1.0 - c_grain_oil_conc);
        dm_oil_demand_differential = dm_grain_demand_differential
        - dm_meal_demand_differential;

        dm_oil_conv_demand_differential = dm_demand_differential
        - dm_grain_demand_differential;

        dm_pod_demand_differential = dm_grain_demand_differential
        * c_frac_pod;

        yield_demand_differential = dm_pod_demand_differential
        + dm_meal_demand_differential
        + dm_oil_demand_differential
        + dm_oil_conv_demand_differential;
        demand_differential = yield_demand_differential;

// get available carbohydrate from supply pools
        for (counter = 0; counter < num_supply_pools; counter++ )
           {
           part = supply_pools[counter];
           dm_part_pot = g_dm_green[part]
           + dm_retranslocate[part];
           dm_part_avail = dm_part_pot
           - g_dm_plant_min[part]
           * g_plants;
           dm_part_avail = l_bound (dm_part_avail, 0.0);

           dlt_dm_retrans_part = min (demand_differential, dm_part_avail);
           dm_retranslocate[part] = - dlt_dm_retrans_part;

           demand_differential = demand_differential - dlt_dm_retrans_part;
           }

        dlt_dm_retrans_total = - (sum_real_array (dm_retranslocate, max_part));

// now distribute retranslocate to demand sinks.

        if (yield_demand_differential > dlt_dm_retrans_total)
            {
            dm_retranslocate[meal] = dlt_dm_retrans_total
            * divide (dm_meal_demand_differential
            , yield_demand_differential
            , 0.0);
            dm_retranslocate[oil] = dlt_dm_retrans_total
            * divide (dm_oil_demand_differential
            , yield_demand_differential
            , 0.0);
            *dm_oil_conv_retranslocate = dlt_dm_retrans_total
              * divide (dm_oil_conv_demand_differential
              , yield_demand_differential
              , 0.0);
            dm_retranslocate[pod] = dlt_dm_retrans_total
            * divide (dm_pod_demand_differential
            , yield_demand_differential
            , 0.0)
            + dm_retranslocate[pod];
            }
        else
            {

            dm_retranslocate[meal] = dm_meal_demand_differential;
            dm_retranslocate[oil] = dm_oil_demand_differential;
            *dm_oil_conv_retranslocate = dm_oil_conv_demand_differential;
            dm_retranslocate[pod] = dm_pod_demand_differential
            + dm_retranslocate[pod];
            }

// ??? check that stem and leaf are >= min wts
        }
    else
        {
// we have no retranslocation
        fill_real_array (dm_retranslocate, 0.0, max_part);
        *dm_oil_conv_retranslocate = 0.0;
        }

    // now check that we have mass balance
    if (!reals_are_equal(-1.0*sum_real_array (dm_retranslocate, max_part),
                         *dm_oil_conv_retranslocate))
      {
      string msg = "dm_retranslocate mass balance is off: "
          + ftoa(sum_real_array (dm_retranslocate, max_part), ".6")
          + " vs "
          + ftoa(*dm_oil_conv_retranslocate, ".6");
      parent->warningError(msg.c_str());
      }
    pop_routine (my_name);
//    fprintf(stdout, "%d,%.9f,%.9f,%.9f,%.9f\n", g.day_of_year,
//            g.dm_green[root] + g.dm_green[leaf] + g.dm_green[stem],
//            g.dm_green[root], g.dm_green[leaf],g.dm_green[stem]);
    return;
    }


//+  Purpose
//     Calculate plant dry matter delta's due to retranslocation
//     to grain, pod and energy (g/m^2)

//+  Mission Statement
//   Calculate biomass retranslocation to the yield component

//+  Changes
//       150900 jngh specified and programmed
void Plant::legnew_dm_retranslocate2
    (
     float  g_current_stage
    ,float  *c_x_stage_no_partition
    ,float  *c_y_frac_pod
    ,int    c_num_stage_no_partition
    ,float  g_grain_energy               // (INPUT) multiplier of grain weight to account for energy used in oil conversion.
    ,float  c_grain_oil_conc             // (INPUT) fraction of grain that is oil
    ,int    pod                          // (INPUT)
    ,int    meal                         // (INPUT)
    ,int    oil                          // (INPUT)
    ,int    max_part                     // (INPUT)
    ,int    *supply_pools                // (INPUT)
    ,int    num_supply_pools             // (INPUT)
    ,float  g_dlt_dm_grain_demand        // (INPUT)  grain dm demand (g/m^2)
    ,float  g_dlt_dm_oil_conv            // (INPUT)  dm used in oil conversion (g/m^2)
    ,float  *g_dlt_dm_green              // (INPUT)  plant biomass growth (g/m^2)
    ,float  *g_dm_green                  // (INPUT)  live plant dry weight (biomass
    ,float  *g_dm_plant_min              // (INPUT)  minimum weight of each plant p
    ,float  g_plants                     // (INPUT)  Plant density (plants/m^2)
    ,float  *dm_oil_conv_retranslocate    // (OUTPUT) assimilate used for oil conversion - energy (g/m^2)
    ,float  *dm_retranslocate            // (OUTPUT) actual change in plant part weights due to translocation (g/m^2)
    ) {

//+  Constant Values
    const char*  my_name = "legnew_dm_retranslocate2" ;

//+  Local Variables
    int   part;                                   // plant part no.
    float dlt_dm_retrans_part;                    // carbohydrate removed from part (g/m^2)
    float dlt_dm_retrans_total;                   // total carbohydrate removed from parts (g/m^2)
    float yield_demand_differential;              // demand in excess of available supply (g/m^2)
    float demand_differential;                    // demand in excess of available supply (g/m^2)
    int   counter;
    float dm_part_avail;                          // carbohydrate avail from part(g/m^2)
    float dm_part_pot;                            // potential part weight (g/m^2)
    float mass_balance;                           // sum of translocated carbo (g/m^2)
    float dm_demand_differential;                 // assimilate demand by grain - meal + oil + energy (g/m^2)
    float dm_grain_demand_differential;           // assimilate demand for grain - meal + oil (g/m^2)
    float dm_oil_demand_differential;             // assimilate demand for oil (g/m^2)
    float dm_meal_demand_differential;            // assimilate demand for meal (g/m^2)
    float dm_pod_demand_differential;             // assimilate demand for pod (g/m^2)
    float dm_oil_conv_demand_differential;        // assimilate demand for oil conversion - energy (g/m^2)
    float dlt_dm_grain;                           // assimilate used to produce grain and oil in partitioning (g/m^2)
    float frac_pod;

//- Implementation Section ----------------------------------

    push_routine (my_name);

    frac_pod = linear_interp_real(g_current_stage
    ,c_x_stage_no_partition
    ,c_y_frac_pod
    ,c_num_stage_no_partition);

// now translocate carbohydrate between plant components
// this is different for each stage

    fill_real_array (dm_retranslocate, 0.0, max_part);

    dlt_dm_grain = g_dlt_dm_green[meal]
    + g_dlt_dm_green[oil]
    + g_dlt_dm_oil_conv;

    if (g_dlt_dm_grain_demand > dlt_dm_grain)
        {
// we can translocate source carbohydrate
// to reproductive parts if needed

// calculate demands for each reproductive part

        dm_demand_differential = g_dlt_dm_grain_demand
        - dlt_dm_grain;

        dm_grain_demand_differential = divide (dm_demand_differential
        , g_grain_energy, 0.0);

        dm_meal_demand_differential = dm_grain_demand_differential
        * (1.0 - c_grain_oil_conc);
        dm_oil_demand_differential = dm_grain_demand_differential
        - dm_meal_demand_differential;

        dm_oil_conv_demand_differential = dm_demand_differential
        - dm_grain_demand_differential;

        dm_pod_demand_differential = dm_grain_demand_differential
        * frac_pod;

        yield_demand_differential = dm_pod_demand_differential
        + dm_meal_demand_differential
        + dm_oil_demand_differential
        + dm_oil_conv_demand_differential;
        demand_differential = yield_demand_differential;

// get available carbohydrate from supply pools
        for (counter = 0; counter < num_supply_pools; counter++)
            {
            part = supply_pools[counter];
            dm_part_pot = g_dm_green[part]
            + dm_retranslocate[part];
            dm_part_avail = dm_part_pot
            - g_dm_plant_min[part]
            * g_plants;
            dm_part_avail = l_bound (dm_part_avail, 0.0);

            dlt_dm_retrans_part = min (demand_differential
            ,dm_part_avail);
            dm_retranslocate[part] = - dlt_dm_retrans_part;

            demand_differential = demand_differential
            - dlt_dm_retrans_part;

            }

        dlt_dm_retrans_total = - (sum_real_array (dm_retranslocate, max_part));

// now distribute retranslocate to demand sinks.

        if (yield_demand_differential > dlt_dm_retrans_total)
            {
            dm_retranslocate[meal] = dlt_dm_retrans_total
            * divide (dm_meal_demand_differential
            , yield_demand_differential
            , 0.0);
            dm_retranslocate[oil] = dlt_dm_retrans_total
            * divide (dm_oil_demand_differential
            , yield_demand_differential
            , 0.0);
            *dm_oil_conv_retranslocate = dlt_dm_retrans_total
               * divide (dm_oil_conv_demand_differential
               , yield_demand_differential
               , 0.0);
            dm_retranslocate[pod] = dlt_dm_retrans_total
            * divide (dm_pod_demand_differential
            , yield_demand_differential
            , 0.0)
            + dm_retranslocate[pod];
            }
        else
            {

            dm_retranslocate[meal] = dm_meal_demand_differential;
            dm_retranslocate[oil] = dm_oil_demand_differential;
            *dm_oil_conv_retranslocate = dm_oil_conv_demand_differential;
            dm_retranslocate[pod] = dm_pod_demand_differential
            + dm_retranslocate[pod];
            }

// ??? check that stem and leaf are >= min wts
        }
    else
        {
// we have no retranslocation
        fill_real_array (dm_retranslocate, 0.0, max_part);
        *dm_oil_conv_retranslocate = 0.0;
        }

// now check that we have mass balance

    mass_balance = sum_real_array (dm_retranslocate, max_part)
    + *dm_oil_conv_retranslocate;
    bound_check_real_var (parent,mass_balance, -1.0e-5, 1.0e-5
    , "dm_retranslocate mass balance");

    pop_routine (my_name);
    return;
    }

//  Purpose
//    Calculate plant dry matter delta's due to retranslocation
//    to grain, pod and energy (g/m^2)
//
//  Mission Statement
//  Calculate biomass retranslocation to the yield component
void Plant::legnew_dm_retranslocate3(
                    int   start_grain_fill           //
                   ,int   end_grain_fill             //
                   ,float g_current_stage            //
                   ,float *g_current_fruit_stage     //
                   ,float *c_x_stage_no_partition   // (INPUT) fraction of remaining dm allocated to pod
                   ,float *c_y_frac_pod                // (INPUT) fraction of remaining dm allocated to pod
                   ,int   c_num_stage_no_partition   // (INPUT) fraction of remaining dm allocated to pod
                   ,float g_grain_energy             // (INPUT) multiplier of grain weight to account for energy used in oil conversion.
                   ,float c_grain_oil_conc           // (INPUT) fraction of grain that is oil
                   ,int   pod                        // (INPUT)
                   ,int   meal                       // (INPUT)
                   ,int   oil                        // (INPUT)
                   ,int   max_part                   // (INPUT)
                   ,int   *supply_pools              // (INPUT)
                   ,int   num_supply_pools           // (INPUT)
                   ,int   max_fruit_cohorts          //
                   ,float *g_dlt_dm_grain_demand     //  (INPUT)  grain dm demand (g/m^2)
                   ,float *g_dlt_dm_fruit_demand     //  (INPUT)  grain dm demand (g/m^2)
                   ,float g_dlt_dm_parasite_demand   //  (INPUT)  parasite dm demand (g/m^2)
                   ,float *g_dlt_dm_oil_conv         //  (INPUT)  dm used in oil conversion (g/m^2
                   ,float g_dlt_dm_parasite          //  (INPUT)  parasite biomass growth (g/m^2)
                   ,float **g_dlt_dm_green           //  (INPUT)  plant biomass growth (g/m^2)
                   ,float *g_dm_green                //  (INPUT)  live plant dry weight (biomass
                   ,float **g_dm_fruit_green         //  (INPUT)  plant biomass growth (g/m^2)
                   ,float *g_dm_plant_min            //  (INPUT)  minimum weight of each plant p
                   ,float *g_dm_fruit_pod_min        //  (INPUT)  minimum weight of each fruit pod
                   ,float g_plants                   //  (INPUT)  Plant density (plants/m^2)
                   ,float *g_fruit_no                //  (INPUT)  Plant density (plants/m^2)
                   ,int   g_num_fruit_cohorts        //
                   ,float *dm_oil_conv_retranslocate //  (OUTPUT) assimilate used for oil conversion - energy (g/m^2)
                   ,float *dm_parasite_retranslocate //  (OUTPUT) assimilate retranslocated to parasite (g/m^2)
                   ,float **dm_retranslocate)         //  (OUTPUT) actual change in plant part weights due to translocation (g/m^2)
   {
     const char *my_name  = "legnew_dm_retranslocate3";

     int        part;                  // plant part no.
     float      dlt_dm_retrans_part;   // carbohydrate removed from part
     // (g/m^2)
     float      dlt_dm_retrans_total;  // total carbohydrate removed from parts
     // (g/m^2)
     float      yield_demand_differential; // demand in excess of available supply
     // (g/m^2)
     float      grain_demand_differential; // demand in excess of available supply
     // (g/m^2)
     int        counter;
     float      dm_part_avail;         // carbohydrate avail from part(g/m^2)
     float      dm_part_pot;           // potential part weight (g/m^2)
     float      mass_balance;          // sum of translocated carbo (g/m^2)
     float      dm_demand_differential;             // assimilate demand by grain - meal + oil + energy (g/m^2)
     float      dm_grain_demand_differential;       // assimilate demand for grain - meal + oil (g/m^2)
     float      dm_oil_demand_differential;         // assimilate demand for oil (g/m^2)
     float      dm_meal_demand_differential;        // assimilate demand for meal (g/m^2)
     float      dm_pod_demand_differential;         // assimilate demand for pod (g/m^2)
     float      dm_pod_demand_dummy;                // assimilate demand for pod (g/m^2)
     float      dm_oil_conv_demand_differential;    // assimilate demand for oil conversion - energy (g/m^2)
     float      dlt_dm_grain;                       // assimilate used to produce grain and oil in partitioning (g/m^2)
     float      dm_retrans_avail;
     float      dm_retrans_avail_pod;
     float      dm_retrans_remaining;
     float      frac_pod;
     int        cohort;
     int        current_phase;

     push_routine (my_name);

   // now translocate carbohydrate between plant components
   // this is different for each stage
   for (cohort = 0; cohort < max_fruit_cohorts; cohort++) {
   	for (part = 0; part < max_part; part++) {
         dm_retranslocate[cohort][part] = 0.0;
      }
   }


   // get available carbohydrate from supply pools
   dm_retrans_avail = 0.0;
   for (counter = 0; counter < num_supply_pools; counter++)
     {
     part = supply_pools[counter];
     dm_part_pot = g_dm_green[part];
     dm_part_avail = dm_part_pot
                       - g_dm_plant_min[part]
                       * g_plants;
     dm_part_avail = l_bound (dm_part_avail, 0.0);
     dm_retrans_avail = dm_part_avail + dm_retrans_avail;
     }

   dm_retrans_remaining = dm_retrans_avail;
   current_phase = (int) g_current_stage;

   if (stage_is_between (start_grain_fill, end_grain_fill
			               , g_current_fruit_stage[cohort]))
      {
      for (cohort = 0; cohort < g_num_fruit_cohorts; cohort++)
         {
         if (g_fruit_no[cohort] > 0.0)
            {
            // we are in grain filling phase
            dm_retrans_avail_pod = g_dm_fruit_green[cohort][pod]
                                     - g_dm_fruit_pod_min[cohort]
                                     * g_fruit_no[cohort];
            dm_retrans_avail_pod = l_bound (dm_retrans_avail_pod, 0.0);

            // calculate demands of reproductive parts
            dlt_dm_grain = g_dlt_dm_green[cohort][meal]
                                     + g_dlt_dm_green[cohort][oil]
                                     + g_dlt_dm_oil_conv[cohort];
            if (g_dlt_dm_grain_demand[cohort] > dlt_dm_grain)
                {
                // we can translocate source carbohydrate
                // to reproductive parts if needed
                // calculate demands for each reproductive part

                dm_demand_differential = g_dlt_dm_grain_demand[cohort]
                                         - dlt_dm_grain;

                dm_grain_demand_differential = divide (dm_demand_differential
                                                 , g_grain_energy, 0.0);

                dm_meal_demand_differential = dm_grain_demand_differential
                                                  * (1.0 - c_grain_oil_conc);
                dm_oil_demand_differential = dm_grain_demand_differential
                                                  - dm_meal_demand_differential;

                dm_oil_conv_demand_differential = dm_demand_differential
                                                  - dm_grain_demand_differential;

                dm_pod_demand_differential = dm_grain_demand_differential
                                                   * c_y_frac_pod[current_phase];

                frac_pod = linear_interp_real(g_current_fruit_stage[cohort]
                                             ,c_x_stage_no_partition
                                             ,c_y_frac_pod
                                             ,c_num_stage_no_partition);

                legnew_dm_part_demands ( frac_pod
                                       , g_grain_energy
                                       , c_grain_oil_conc
                                       , dm_demand_differential
                                       , &dm_oil_conv_demand_differential
                                       , &dm_meal_demand_differential
                                       , &dm_oil_demand_differential
                                       , &dm_pod_demand_differential);


                // retranslocate from pod
                dm_pod_demand_dummy = 0.0;
                legnew_dm_distribute (max_part
                                    , &dm_retrans_avail_pod
                                    , dm_meal_demand_differential
                                    , dm_oil_demand_differential
                                    , dm_pod_demand_dummy
                                    , dm_oil_conv_demand_differential
                                    , &dm_oil_conv_retranslocate[cohort]
                                    , dm_retranslocate[cohort]);

                dm_retranslocate[cohort][pod] =
                                 - dm_retranslocate[cohort][meal]
                                 - dm_retranslocate[cohort][oil]
                                 - dm_oil_conv_retranslocate[cohort];

                dm_meal_demand_differential =
                         dm_meal_demand_differential
                         - dm_retranslocate[cohort][meal];
                dm_oil_demand_differential =
                         dm_oil_demand_differential
                         - dm_retranslocate[cohort][oil];
                dm_oil_conv_demand_differential =
                         dm_oil_conv_demand_differential
                         - dm_oil_conv_retranslocate[cohort];
                dm_pod_demand_differential =
                         dm_pod_demand_differential
                         - dm_retranslocate[cohort][pod];

                // retranslocate from leaf and stem
                legnew_dm_distribute (max_part
                                    , &dm_retrans_remaining
                                    , dm_meal_demand_differential
                                    , dm_oil_demand_differential
                                    , dm_pod_demand_differential
                                    , dm_oil_conv_demand_differential
                                    , &dm_oil_conv_retranslocate[cohort]
                                    , dm_retranslocate[cohort]);

              }
            else
              {
                // the cohort's demand is satisfied - no need to retranslocate
                dm_retranslocate[cohort][meal] = 0.0;
                dm_retranslocate[cohort][oil]  = 0.0;
                dm_oil_conv_retranslocate[cohort]   = 0.0;
                dm_retranslocate[cohort][pod]  = 0.0;
              }
		      } /// if g_fruit_no[cohort] > 0.0
	      else
		      {
            // no fruit in this cohort
            dm_retranslocate[cohort][meal] = 0.0;
            dm_retranslocate[cohort][oil]  = 0.0;
            dm_oil_conv_retranslocate[cohort]   = 0.0;
            dm_retranslocate[cohort][pod]  = 0.0;
            }
	      }  ///for (cohort = 0; coh...
      } ///if (stage_is_between

   // retranslocate to parasite
   if (g_dlt_dm_parasite_demand > g_dlt_dm_parasite)
      {
      dm_demand_differential = g_dlt_dm_parasite_demand
                             - g_dlt_dm_parasite;
      if (dm_retrans_remaining > 0.0)
         {
         if (dm_demand_differential > dm_retrans_remaining)
            {
            *dm_parasite_retranslocate = dm_retrans_remaining;
            dm_retrans_remaining = 0.0;
            }
         else
            {
            *dm_parasite_retranslocate = dm_demand_differential;
            dm_retrans_remaining = dm_retrans_remaining
                                 - *dm_parasite_retranslocate;
            }
         }
      else
         {
         *dm_parasite_retranslocate = 0.0;
         }
      }
   else
      {
      *dm_parasite_retranslocate = 0.0;
      }

   dlt_dm_retrans_total = dm_retrans_avail - dm_retrans_remaining;
   dm_retrans_remaining = dlt_dm_retrans_total;

   // remove retranslocated carbohydrate from supply pools
   for (counter = 0; counter < num_supply_pools; counter++)
      {
      part = supply_pools[counter];
      dm_part_avail = g_dm_green[part]
                    - g_dm_plant_min[part]
                      * g_plants;
      dm_part_avail = l_bound (dm_part_avail, 0.0);

      dlt_dm_retrans_part = min (dm_retrans_remaining
                                ,dm_part_avail);
      dm_retranslocate[0][part] = dm_retranslocate[0][part]
                                - dlt_dm_retrans_part;

      dm_retrans_remaining = dm_retrans_remaining
                           - dlt_dm_retrans_part;
      }

      // now check that we have mass balance
   mass_balance = 0.0;
   for (cohort = 0; cohort < g_num_fruit_cohorts; cohort++)
      {
      mass_balance += sum_real_array(dm_retranslocate[cohort], max_part);
      }
   mass_balance = mass_balance
                + sum_real_array(dm_oil_conv_retranslocate, max_part)
                + *dm_parasite_retranslocate;
   bound_check_real_var (parent,mass_balance, -5.0e-5, 5.0e-5
                            , "dm_retranslocate mass balance");

   pop_routine (my_name);
   }

//  Purpose
//      Partitions new dm (assimilate) between plant components (g/m^2)
//
//  Mission Statement
//    Partitions new biomass between plant components
//
//  Changes
//      010994 jngh specified and programmed
void Plant::legnew_dm_part_demands(float c_frac_pod              // (INPUT)  fraction of remaining dm allocated to pod
                                 , float g_grain_energy          // multiplier of grain weight to account f
                                 , float c_grain_oil_conc        // (INPUT)  grain dm demand (g/m^2)
                                 , float g_dlt_dm_grain_demand   // multiplier of grain weight to account f
                                 , float *dm_oil_conv_demand      // assimilate demand for reproductive parts (g/m^2)
                                 , float *dlt_dm_demand_meal      // assimilate demand for reproductive parts (g/m^2)
                                 , float *dlt_dm_demand_oil       // assimilate demand for reproductive parts (g/m^2)
                                 , float *dlt_dm_demand_pod )     // assimilate demand for conversion to oil (g/m^2)
   {
   float       dm_grain_demand;       // assimilate demand for grain (g/m^2)

   // calculate demands of reproductive parts
   dm_grain_demand = divide (g_dlt_dm_grain_demand
                             , g_grain_energy, 0.0);

   *dlt_dm_demand_meal = dm_grain_demand
                              * (1.0 - c_grain_oil_conc);
   *dlt_dm_demand_oil = dm_grain_demand
                             - *dlt_dm_demand_meal;
   *dm_oil_conv_demand= g_dlt_dm_grain_demand
                             - dm_grain_demand;

   *dlt_dm_demand_pod = dm_grain_demand * c_frac_pod;
   }

//  Purpose
//      Partitions new dm (assimilate) between plant components (g/m^2)
//
//  Mission Statement
//    Partitions new biomass between plant components
//
//  Changes
//      010994 jngh specified and programmed
void Plant::legnew_dm_distribute(int max_part
                               , float *dm_remaining          // interim dm pool for partitioning
                               , float dlt_dm_demand_meal    // assimilate demand for reproductive parts (g/m^2)
                               , float dlt_dm_demand_oil     // assimilate demand for reproductive parts (g/m^2)
                               , float dlt_dm_demand_pod     // assimilate demand for reproductive parts (g/m^2)
                               , float dm_oil_conv_demand    // assimilate demand for conversion to oil (g/m^2)
                               , float *dlt_dm_oil_conv       // (OUTPUT) actual biomass used in conversion to oil (g/m2)
                               , float *dlt_dm_green          // (OUTPUT) actual biomass partitioned
                                )
    {
    float  yield_demand;   // sum of grain, energy & pod

    if (*dm_remaining > 0.0)
        {
        // still have some assimilate to partition

        yield_demand = dlt_dm_demand_pod
                        + dlt_dm_demand_meal
                        + dlt_dm_demand_oil
                        + dm_oil_conv_demand;

        if (yield_demand >= *dm_remaining)
            {
            // reproductive demand exceeds supply - distribute assimilate to those parts only
            dlt_dm_green[meal] = *dm_remaining
                           * divide (dlt_dm_demand_meal
                                   , yield_demand, 0.0);
            dlt_dm_green[oil] = *dm_remaining
                            * divide (dlt_dm_demand_oil
                                    , yield_demand, 0.0);
            *dlt_dm_oil_conv = *dm_remaining
                           * divide (dm_oil_conv_demand
                                   , yield_demand, 0.0);
            dlt_dm_green[pod] = *dm_remaining
                                    - dlt_dm_green[meal]
                                    - dlt_dm_green[oil]
                                    - *dlt_dm_oil_conv;
            *dm_remaining = 0.0;
            }
        else
            {
            // more than enough assimilate to go around
            dlt_dm_green[meal] =  dlt_dm_green[meal]
                               +  dlt_dm_demand_meal;
            dlt_dm_green[oil]  =  dlt_dm_green[oil]
                               +  dlt_dm_demand_oil;
            *dlt_dm_oil_conv   =  *dlt_dm_oil_conv
                              +  dm_oil_conv_demand;
            dlt_dm_green[pod]  = dlt_dm_green[pod]
                               + dlt_dm_demand_pod;

            *dm_remaining = *dm_remaining - yield_demand;
            }
        }
     else
         {
         // no assimilate left to partition
         dlt_dm_green[meal] = 0.0;
         dlt_dm_green[oil]  = 0.0;
         *dlt_dm_oil_conv   = 0.0;
         dlt_dm_green[pod]  = 0.0;
         }
   }

//+  Purpose
//       Returns cumulative thermal time targets required for the
//       individual growth stages.

//+  Mission Statement
//     Get the cumulative thermal time targets for growth phases

//+  Changes
//     <insert here>
void Plant::legnew_phenology_init(
     float  c_shoot_lag                  // (INPUT)  minimum growing degree days fo
    ,float  c_shoot_rate                 // (INPUT)  growing deg day increase with
    ,float  g_maxt
    ,float  g_mint
    ,float  *c_x_vernal_temp
    ,float  *c_y_vernal_days
    ,int    c_num_vernal_temp
    ,float  *g_cum_vernal_days             // in/out
    ,float  *p_cum_vernal_days
    ,float  *p_tt_emerg_to_endjuv
    ,int    p_num_cum_vernal_days
    ,float  c_twilight                     // (INPUT)  twilight in angular distance b
    ,float  g_current_stage                // (INPUT)  current phenological stage
    ,float  *g_days_tot                    // (INPUT)  duration of each phase (days)
    ,int    g_day_of_year                  // (INPUT)  day of year
    ,int    g_year                         // (INPUT)  year
    ,float  g_latitude                     // (INPUT)  latitude (degrees, negative fo
    ,float  g_sowing_depth                 // (INPUT)  sowing depth (mm)
    ,float  *p_x_pp_endjuv_to_init         // (INPUT)
    ,float  *p_y_tt_endjuv_to_init         // (INPUT)
    ,int    p_num_pp_endjuv_to_init        // (INPUT)
    ,float  *p_x_pp_init_to_flower         // (INPUT)
    ,float  *p_y_tt_init_to_flower         // (INPUT)
    ,int    p_num_pp_init_to_flower        // (INPUT)
    ,float  *p_x_pp_flower_to_start_grain  // (INPUT)
    ,float  *p_y_tt_flower_to_start_grain  // (INPUT)
    ,int    p_num_pp_flower_to_start_grain // (INPUT)
    ,float  *p_x_pp_start_to_end_grain     // (INPUT)
    ,float  *p_y_tt_start_to_end_grain     // (INPUT)
    ,int    p_num_pp_start_to_end_grain    // (INPUT)
    ,float  p_tt_end_grain_to_maturity    // (INPUT)
    ,float  p_tt_maturity_to_ripe         // (INPUT)  growing deg day required to fo
    ,int    p_est_days_emerg_to_init       // (INPUT) estimate of days to init
    ,float  *phase_tt)                      // (INPUT/OUTPUT) cumulative growing degree days required for each stage (deg days)
  {

//+  Constant Values
    const char*  my_name = "legnew_phenology_init" ;

//+  Local Variables
    float photoperiod;                            // hours of photosynthetic light (hours)
    int   est_day_of_floral_init;

//- Implementation Section ----------------------------------

    push_routine (my_name);

    photoperiod = day_length (g_day_of_year
                              , g_latitude
                              , c_twilight);

    if (stage_is_between (germ, endjuv, g_current_stage))
        {
        phase_tt[germ_to_emerg-1] = c_shoot_lag + g_sowing_depth*c_shoot_rate;

        *g_cum_vernal_days = *g_cum_vernal_days
           + legnew_vernal_days (g_maxt
                                 ,g_mint
                                 ,c_x_vernal_temp
                                 ,c_y_vernal_days
                                 ,c_num_vernal_temp);

        phase_tt[emerg_to_endjuv-1] = linear_interp_real(*g_cum_vernal_days
                                     ,p_cum_vernal_days
                                     ,p_tt_emerg_to_endjuv
                                     ,p_num_cum_vernal_days);

        if (on_day_of(emerg,g_current_stage))
            {
    // This If Test added to reproduce old results. - NIH
    // Only minor differences really - should consider
    // whether this is necessary.
            est_day_of_floral_init = offset_day_of_year (g_year
                                         , g_day_of_year
                                         , p_est_days_emerg_to_init);

            photoperiod = day_length (est_day_of_floral_init, g_latitude, c_twilight);

            phase_tt[endjuv_to_init-1] = linear_interp_real (photoperiod
                                         ,p_x_pp_endjuv_to_init
                                         ,p_y_tt_endjuv_to_init
                                         ,p_num_pp_endjuv_to_init);
//            fprintf(stdout,"%d,%f,%f\n",est_day_of_floral_init,photoperiod,phase_tt[endjuv_to_init-1]);
            }

        phase_tt[init_to_flower-1] = linear_interp_real
                                         (photoperiod
                                         ,p_x_pp_init_to_flower
                                         ,p_y_tt_init_to_flower
                                         ,p_num_pp_init_to_flower);
        phase_tt[flower_to_start_grain-1] = linear_interp_real
                                         (photoperiod
                                         ,p_x_pp_flower_to_start_grain
                                         ,p_y_tt_flower_to_start_grain
                                         ,p_num_pp_flower_to_start_grain);
        phase_tt[start_to_end_grain-1] = linear_interp_real
                                         (photoperiod
                                         ,p_x_pp_start_to_end_grain
                                         ,p_y_tt_start_to_end_grain
                                         ,p_num_pp_start_to_end_grain);

        phase_tt[end_grain_to_maturity-1] = p_tt_end_grain_to_maturity;
        phase_tt[maturity_to_ripe-1] = p_tt_maturity_to_ripe;
//        fprintf(stdout,"%d,%f,%f\n",g_day_of_year,photoperiod,phase_tt[2]);
        }
    else if (stage_is_between (endjuv, floral_init, g_current_stage))
        {
        phase_tt[endjuv_to_init-1] = linear_interp_real(photoperiod
                                           ,p_x_pp_endjuv_to_init
                                           ,p_y_tt_endjuv_to_init
                                           ,p_num_pp_endjuv_to_init);
        phase_tt[init_to_flower-1] = linear_interp_real(photoperiod
                                           ,p_x_pp_init_to_flower
                                           ,p_y_tt_init_to_flower
                                           ,p_num_pp_init_to_flower);
        phase_tt[flower_to_start_grain-1] = linear_interp_real(photoperiod
                                           ,p_x_pp_flower_to_start_grain
                                           ,p_y_tt_flower_to_start_grain
                                           ,p_num_pp_flower_to_start_grain);
        phase_tt[start_to_end_grain-1] = linear_interp_real (photoperiod
                                           ,p_x_pp_start_to_end_grain
                                           ,p_y_tt_start_to_end_grain
                                           ,p_num_pp_start_to_end_grain);

        }
    else if (stage_is_between (floral_init,flowering, g_current_stage))
        {
        phase_tt[init_to_flower-1] = linear_interp_real(photoperiod
                                           ,p_x_pp_init_to_flower
                                           ,p_y_tt_init_to_flower
                                           ,p_num_pp_init_to_flower);
        phase_tt[flower_to_start_grain-1] = linear_interp_real  (photoperiod
                                                ,p_x_pp_flower_to_start_grain
                                                ,p_y_tt_flower_to_start_grain
                                                ,p_num_pp_flower_to_start_grain);
        phase_tt[start_to_end_grain-1] = linear_interp_real(photoperiod
                                                ,p_x_pp_start_to_end_grain
                                                ,p_y_tt_start_to_end_grain
                                                ,p_num_pp_start_to_end_grain);

        }
    else if (stage_is_between (flowering, start_grain_fill, g_current_stage))
        {
        phase_tt[flower_to_start_grain-1] = linear_interp_real (photoperiod
                                                ,p_x_pp_flower_to_start_grain
                                                ,p_y_tt_flower_to_start_grain
                                                ,p_num_pp_flower_to_start_grain);
        phase_tt[start_to_end_grain-1] = linear_interp_real(photoperiod
                                                ,p_x_pp_start_to_end_grain
                                                ,p_y_tt_start_to_end_grain
                                                ,p_num_pp_start_to_end_grain);

         }
    else if (stage_is_between (start_grain_fill, end_grain_fill, g_current_stage))
        {
        phase_tt[start_to_end_grain-1] = linear_interp_real (photoperiod
                                                ,p_x_pp_start_to_end_grain
                                                ,p_y_tt_start_to_end_grain
                                                ,p_num_pp_start_to_end_grain);

        }
    else
        {
        }


    pop_routine (my_name);
    return;
    }


//+  Purpose
//     Calculate the nitrogen retranslocation from the various plant parts
//     to the grain.

//+  Mission Statement
//     Calculate N retranslocation from various plant parts to grain

//+  Changes
//       080994 jngh specified and programmed
void Plant::legnew_n_retranslocate
    (float  *g_n_conc_crit
    ,float  *g_n_conc_min               // (INPUT)  minimum N concentration (g N/g
    ,float  *g_dlt_dm_green             // (INPUT)  plant biomass growth (g/m^2)
    ,float  *g_dlt_dm_green_retrans     // (INPUT)  plant biomass growth (g/m^2)
    ,float  *g_dm_green                 // (INPUT)  live plant dry weight (biomass
    ,float  *g_n_conc_max               // (INPUT)  maximum N concentration (g N/g
    ,float  *g_n_green                  // (INPUT)  plant nitrogen content (g N/m^
    ,float  g_grain_n_demand            //  INPUT
    ,float  *dlt_n_retrans              // (OUTPUT) plant N taken out from plant parts (g N/m^2)
    ) {

//+  Constant Values
    const char*  my_name = "legnew_n_retranslocate" ;
//
    const float  tolerence = 0.001 ;

//+  Local Variables
    float n_avail[max_part];                      // N available for transfer to grain (g/m^2)
    float n_avail_stover;                         // total N available in stover (g/m^2)
    int   part;                                   // plant part number

//- Implementation Section ----------------------------------

    push_routine (my_name);

    crop_n_retrans_avail (max_part
                         , root
                         , meal
                         , g_n_conc_min
                         , g_dm_green
                         , g_n_green
                         , n_avail);  // grain N potential (supply)

    //! available N does not include roots or grain
    //! this should not presume roots and grain are 0.
    n_avail_stover  =  sum_real_array (n_avail, max_part);

    //! get actual grain N uptake
    //! limit retranslocation to total available N
    fill_real_array (dlt_n_retrans, 0.0, max_part);

    if (g_grain_n_demand >= n_avail_stover)
       {
       //    ! demand greater than or equal to supply
       //    ! retranslocate all available N
       dlt_n_retrans[leaf] = - n_avail[leaf];
       dlt_n_retrans[stem] = - n_avail[stem];
       dlt_n_retrans[pod] = -  n_avail[pod];
       dlt_n_retrans[meal] =   n_avail_stover;
       }
    else
       {
       //    ! supply greater than demand.
       //    ! Retranslocate what is needed

       dlt_n_retrans[leaf] = - g_grain_n_demand
                             * divide (n_avail[leaf]
                                     , n_avail_stover, 0.0);

       dlt_n_retrans[pod] = - g_grain_n_demand
                             * divide (n_avail[pod]
                                     , n_avail_stover, 0.0);

       dlt_n_retrans[stem] = - g_grain_n_demand
                             - dlt_n_retrans[leaf] //   ! note - these are
                             - dlt_n_retrans[pod]; //! -ve values.

       dlt_n_retrans[meal] = g_grain_n_demand;
      }

    // just check that we got the maths right.
    for (int part = root; part <= pod; part++)
        {
        bound_check_real_var (parent,fabs (dlt_n_retrans[part])
                              , 0.0, n_avail[part] + tolerence
                              , "dlt_N_retrans(part)");
        }

    pop_routine (my_name);
    }

//  Purpose
//    Calculate the nitrogen retranslocation from the various plant parts
//    to the grain.
//
//  Mission Statement
//    Calculate N retranslocation from various plant parts to grain
void Plant::plant_n_retranslocate(float *g_n_conc_crit //! (INPUT)  critical N concentration (g N/
                                  , float *g_n_conc_min                     //(INPUT)  minimum N concentration (g N/
                                  , float c_n_retrans_fraction
                                  , float *g_dm_green                       //(INPUT)  live plant dry weight (biomas
                                  , float *g_n_green                        //(INPUT)  plant nitrogen content (g N/m2)
                                  , float g_grain_n_demand                 //
                                  , float *g_dlt_n_green                    //
                                  , float *dlt_n_retrans)                   //(OUTPUT) plant N taken out from
                                                       // plant parts (g N/m^2)
   {
      const char *my_name = "plant_N_retranslocate";
      const float tolerence = 0.001;  // tolerence for bound check (g/m^2)

      float      n_avail[max_part];     // N available for transfer to grain
                                        // (g/m^2)
      float      n_avail_stover;        // total N available in stover
                                        // (g/m^2)
      float      n_potential;           // maximum grain N demand (g/m^2)
      int        part;                  // plant part number
      float      leaf_n_crit;
      float      grain_retrans_demand;


      push_routine (my_name);

      // What about fixation here????
      grain_retrans_demand = g_grain_n_demand
                           - g_dlt_n_green[meal];

      grain_retrans_demand = max(0.0,grain_retrans_demand);


      crop_n_retrans_avail (max_part
                               , root
                               , meal
                               , g_n_conc_min
                               , g_dm_green
                               , g_n_green
                               , n_avail);  // grain N potential (supply)


      // only allow a fraction of available move on any day
      // (using simple first order decay)
      for (int part = 0; part < max_part; part++)
         n_avail[part] = n_avail[part]*c_n_retrans_fraction;



      // available N does not include roots or grain
      // this should not presume roots and grain are 0.

      n_avail_stover  =  sum_real_array (n_avail, max_part);

      //    ! get actual grain N uptake
      //    ! limit retranslocation to total available N

      fill_real_array (dlt_n_retrans, 0.0, max_part);

      if (grain_retrans_demand>=n_avail_stover)
          {
          // demand greater than or equal to supply
          // retranslocate all available N

          dlt_n_retrans[leaf] = - n_avail[leaf];
          dlt_n_retrans[stem] = - n_avail[stem];
          dlt_n_retrans[pod ] = - n_avail[pod];
          dlt_n_retrans[meal] =   n_avail_stover;
          }
      else
          {
          // supply greater than demand.
          // Retranslocate what is needed

         dlt_n_retrans[leaf] = - grain_retrans_demand
                               * divide (n_avail[leaf]
                                       , n_avail_stover, 0.0);

         dlt_n_retrans[pod] = - grain_retrans_demand
                               * divide (n_avail[pod]
                                       , n_avail_stover, 0.0);

         dlt_n_retrans[stem] = - grain_retrans_demand
                               - dlt_n_retrans[leaf]   // note - these are
                               - dlt_n_retrans[pod];    // -ve values.

         dlt_n_retrans[meal] = grain_retrans_demand;
         }

      // just check that we got the maths right.
     for (int part = root; part <= pod; part++)
        {
        bound_check_real_var (parent,fabs (dlt_n_retrans[part])
                              , 0.0, n_avail[part] + tolerence
                              , "dlt_N_retrans(part)");
        }

     pop_routine (my_name);
  }

//+  Purpose
//       Return the fractional death of oldest green leaf.

//+  Mission Statement
//     Get the fractional death of oldest green leaf

//+  Changes
//     061097 nh specified and programmed
//     260500 dsg floating point .eq. comparisons modified
void Plant::legnew_leaf_death_leg
    (
     float  c_sen_start_stage          // (INPUT)  stage for onset of senescence
    ,float  c_fr_lf_sen_rate           // (INPUT)  fraction of total leaf no sene  per senescing node (/node)
    ,float  c_node_sen_rate            // (INPUT)  node senescence rate (deg day/
    ,float  g_nfact_expansion
    ,float  c_n_fact_lf_sen_rate
    ,float  g_current_stage            // (INPUT)  current phenological stage
    ,float  g_dlt_tt                   // (INPUT)  daily thermal time (growing de
    ,float  *g_leaf_no                 // (INPUT)  number of fully expanded leave
    ,float  *g_leaf_no_dead            // (INPUT)  no of dead leaves ()
    ,float  *g_leaf_area               // (INPUT)  area of leaves
    ,float  c_min_tpla                 // (INPUT)  minimum plant leaf area
    ,float  *dlt_leaf_no_dead           // (OUTPUT) new fraction of oldest green leaf
    ) {

//+  Constant Values
    const char*  my_name = "legnew_leaf_death_leg" ;

//+  Local Variables
    float leaf_no_now;                            // total number of leaves yesterday
    float leaf_no_dead_now;                       // total number of dead leaves yesterday
    float leaf_death_rate;                        // thermal time for senescence of another leaf (oCd)
    float leaf_per_node;                          // no. of leaves senescing per node
    float tpla_now;                               //
    float max_sleaf_no_now;                       // max number of senesced leaves allowable
    float max_sen_area;                           // max area that can be senesced
    float node_sen_rate;

//- Implementation Section ----------------------------------

    push_routine (my_name);

    leaf_no_now = sum_real_array (g_leaf_no, max_node);

    leaf_per_node = leaf_no_now * c_fr_lf_sen_rate;

    node_sen_rate = divide( c_node_sen_rate
                          , 1.0 + c_n_fact_lf_sen_rate * (1.0 - g_nfact_expansion)
                          , 0.0);

    leaf_death_rate = divide (node_sen_rate, leaf_per_node, 0.0);

    if (reals_are_equal(g_current_stage, (float)harvest_ripe))
        {
        // Constrain leaf death to remaining leaves
        //cnh do we really want to do this?;
        leaf_no_dead_now = sum_real_array (g_leaf_no_dead,max_node);
        *dlt_leaf_no_dead = l_bound (leaf_no_now - leaf_no_dead_now, 0.0);

        }
    else if (g_current_stage>=c_sen_start_stage)
        {
        *dlt_leaf_no_dead = divide (g_dlt_tt, leaf_death_rate, 0.0);

        // Ensure minimum leaf area remains
        tpla_now = sum_real_array (g_leaf_area, max_node);
        max_sen_area = l_bound (tpla_now - c_min_tpla, 0.0);
        max_sleaf_no_now = legnew_leaf_no_from_area (g_leaf_area
                                                     , g_leaf_no
                                                     , max_node
                                                     , max_sen_area);

        // Constrain leaf death to remaining leaves
        leaf_no_dead_now = sum_real_array (g_leaf_no_dead, max_node);
        *dlt_leaf_no_dead = u_bound (*dlt_leaf_no_dead, max_sleaf_no_now - leaf_no_dead_now);
//     fprintf(stdout, "%d,%.9f,%.9f,%.9f,%.9f\n",
//                g.day_of_year, *dlt_leaf_no_dead, max_sleaf_no_now, leaf_no_dead_now, g_dlt_tt);
       }
    else
        {
        *dlt_leaf_no_dead = 0.0;
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Initialise plant weights and plant weight minimums
//       at required instances.

//+  Mission Statement
//     Initialise plant weights and plant weight minimums at required instances.

//+  Changes
//     010994 jngh specified and programmed
void Plant::legnew_retrans_init
    (
     float c_leaf_trans_frac                      // (INPUT)  fraction of leaf used in trans
    ,float c_stem_trans_frac                      // (INPUT)  fraction of stem used in trans
    ,float  c_pod_trans_frac                      // (INPUT)  fraction of pod used in trans
    ,float  g_current_stage                       // (INPUT)  current phenological stage
    ,float  g_plants                              // (INPUT)  Plant density (plants/m^2)
    ,float *dm_green                              // (INPUT/OUTPUT) plant part weights (g/m^2)
    ,float *dm_plant_min                          // (OUTPUT) minimum weight of each plant part (g/plant)
    ) {
//+  Constant Values
    const char*  my_name = "legnew_retrans_init" ;

//+  Local Variables
    float dm_plant_leaf;                          // dry matter in leaves (g/plant)
    float dm_plant_stem;                          // dry matter in stems (g/plant)
    float dm_plant_pod;                           // dry matter in pods (g/plant)

//- Implementation Section ----------------------------------

    push_routine (my_name);

    // initialise plant weight
    // initialisations - set up dry matter for leaf, stem, pod, grain
    // and root

    if (on_day_of (flowering, g_current_stage)) {

        // we are at first day of grainfill.
        // set the minimum weight of stem; used for retranslocation to grain
        // set the minimum weight of leaf; used for translocation to grain

        dm_plant_stem = divide (dm_green[stem], g_plants, 0.0);
        dm_plant_min[stem] = dm_plant_stem * (1.0 - c_stem_trans_frac);
        dm_plant_leaf = divide (dm_green[leaf], g_plants, 0.0);
        dm_plant_min[leaf] = dm_plant_leaf * (1.0 - c_leaf_trans_frac);
        dm_plant_min[pod] = 0.0;
        }                                             // no changes
    else
        {
        }


    dm_plant_pod = divide (dm_green[pod], g_plants, 0.0);
    dm_plant_min[pod] = max (dm_plant_pod * (1.0 - c_pod_trans_frac), dm_plant_min[pod]);

    pop_routine (my_name);
    return;
    }



//  Purpose
//      Derives seneseced plant nitrogen (g N/m^2)
//
//  Mission Statement
//  Calculate change in senesced plant Nitrogen
void Plant::plant_N_senescence (int num_part                  //(INPUT) number of plant part
                               ,float *c_n_sen_conc           //(INPUT)  N concentration of senesced materia  (g/m^2)
                               ,float *g_n_conc_max           //(INPUT) critical N conc
                               ,float* g_dlt_dm_senesced      // (INPUT)  plant biomass senescence (g/m^2)
                               ,float* g_n_green              //(INPUT) nitrogen in plant material (g/m^2)
                               ,float* g_dm_green             // (INPUT) plant material (g/m^2)
                               ,float* g_n_demand             //
                               ,float* dlt_n_senesced_trans   // (OUTPUT)  plant N senescence (g/m^2)
                               ,float* dlt_n_senesced_retrans //
                               ,float* dlt_n_senesced)        //  (OUTPUT) actual nitrogen senesced
     {                                                        //    from plant parts (g/m^2)


      const char *my_name  = "plant_N_senescence";

      int part;               //! plant part counter variable
      float    green_n_conc;  //! N conc of green material (g/g)
      float    sen_n_conc;    //! N conc of senescing material (g/g)
      //float    dlt_n_in_senescing_part;
      float    dlt_n_in_senescing_leaf;
      float    new_n_max;
      float    new_n;
      float    left_over_n;
      float    trans_tot;
      float    trans_grain;
      float    grain_fraction;
      float    navail;
      float    n_demand_tot;

      push_routine (my_name);

      // Don't use this translocation variable - seems a bit dodgy
      fill_real_array(dlt_n_senesced_trans,0.0, num_part);


      // first we zero all plant component deltas

      for (part = 0; part < num_part; part++)
         {
         green_n_conc = divide (g_n_green[part]
                               ,g_dm_green[part]
                               ,0.0);

         //dlt_n_in_senescing_part = g_dlt_dm_senesced[part]
         //                        * green_n_conc;  //XX unused calc???? ask neil

         sen_n_conc = min (c_n_sen_conc[part], green_n_conc);

         dlt_n_senesced[part] = g_dlt_dm_senesced[part]
                              * sen_n_conc;

         dlt_n_senesced[part] = u_bound (dlt_n_senesced[part]
                                           , g_n_green[part]);

         }

      //! now get N to retranslocate out of senescing leaves
      fill_real_array(dlt_n_senesced_trans, 0.0, num_part);

      green_n_conc = divide (g_n_green[leaf]
                            ,g_dm_green[leaf]
                            ,0.0);

      dlt_n_in_senescing_leaf = g_dlt_dm_senesced[leaf]
                              * green_n_conc;

      navail = dlt_n_in_senescing_leaf - dlt_n_senesced[leaf];
      navail = l_bound(navail, 0.0);

      n_demand_tot = sum_real_array(g_n_demand, num_part);

      for (part=0; part < num_part; part++)
         {
         dlt_n_senesced_retrans[part] = navail
                           * divide (g_n_demand[part]
                                    ,n_demand_tot
                                    ,0.0);
         }

      pop_routine (my_name);
  }

//  Purpose
//    Calculate plant n demand
//
//  Mission Statement
//    Calculate plant n demand
void Plant::plant_grain_n_demand1(float c_sfac_slope            //   (INPUT)  soil water stress factor slope
                                , float c_sw_fac_max            //   (INPUT)  soil water stress factor maxim
                                , float c_temp_fac_min          //   (INPUT)  temperature stress factor mini
                                , float c_tfac_slope            //   (INPUT)  temperature stress factor slop
                                , float g_maxt                  //   (INPUT)  maximum air temperature (oC)
                                , float g_mint                  //   (INPUT)  minimum air temperature (oC)
                                , float g_nfact_grain_conc      //   (INPUT)
                                , float *g_n_conc_crit          //   (INPUT)  critical N concentration (g N/
                                , float g_swdef_expansion       //   (INPUT)
                                , float *g_n_conc_min           //   (INPUT)  minimum N concentration (g N/g
                                , float *g_dlt_dm_green         //   (INPUT)  plant biomass growth (g/m^2)
                                , float *g_dlt_dm_green_retrans //   (INPUT)  plant biomass growth (g/m^2)
                                , float *g_dm_green             //   (INPUT)  live plant dry weight (biomass
                                , float *g_n_conc_max           //   (INPUT)  maximum N concentration (g N/g
                                , float *g_n_green              //   (INPUT)  plant nitrogen content (g N/m^
                                , float *grain_n_demand)        //   grain N demand (g/m^2)
  {
      const char *my_name = "plant_grain_n_demand1";

      float   n_avail[max_part];     // N available for transfer to grain
                                     // (g/m^2)
      float   n_avail_stover;        // total N available in stover
                                     // (g/m^2)
      float   n_potential;           // maximum grain N demand (g/m^2)
      int     part;                  // plant part number


      push_routine (my_name);


      *grain_n_demand = (g_dlt_dm_green[meal] + g_dlt_dm_green_retrans[meal])
             * crop_n_dlt_grain_conc(meal,
                            c_sfac_slope
                          , c_sw_fac_max
                          , c_temp_fac_min
                          , c_tfac_slope
                          , g_maxt
                          , g_mint
                          , g_nfact_grain_conc
                          , g_n_conc_crit
                          , g_n_conc_min
                          , g_swdef_expansion);


      n_potential  = (g_dm_green[meal]
                     + g_dlt_dm_green[meal]
                     + g_dlt_dm_green_retrans[meal])
               * g_n_conc_max[meal];


      *grain_n_demand = u_bound (*grain_n_demand
                                , n_potential - g_n_green[meal]);

      pop_routine (my_name);
   }






//+  Purpose
//       Calculate Grain Numer

//+  Mission Statement
//       Calculate Grain Numer

//+  Changes

void Plant::plant_grain_number (int option /*(INPUT) option number*/)
    {
    const char*  my_name = "plant_grain_number" ;

    push_routine (my_name);

    if (option == 1)
        {
        // do not use grain number
        g.grain_no = 0;
        }
    else if (option == 2)
        {
        crop_grain_number ( g.current_stage,
                            g.days_tot,
                            emerg,
//                            start_grain_fill,
                            flowering,
                            g.dm_green,
                            stem,
                            p.grains_per_gram_stem,
                            &g.grain_no);
        }
    else
        {
        throw std::invalid_argument ("invalid template option");
        }

    pop_routine (my_name);
    return;
    }

//  Purpose
//      Calculate Fruit Site Number
//
//  Mission Statement
//      Calculate Fruit Numer
void Plant::plant_fruit_site_number (int option)
   {
   const char *my_name = "plant_fruit_site_number";

   push_routine (my_name);

   if (option == 1)
      {
      // do not use fruit number
      g.fruit_site_no = 0;
      g.dlt_fruit_site_no = 0.0;
      }
   else if (option == 2)
      {
      crop_fruit_site_number (g.current_stage
                       , g.days_tot
                       , initial_fruit_stage
                       , end_grain_fill
                       , g.fruit_tt_tot
                       , g.fruit_phase_tt
                       , p.cutout_fract
                       , g.plants
                       , max_fruit_cohorts
                       , max_stage
                       , p.x_node_no_fruit_sites
                       , p.y_fruit_sites_per_node
                       , p.num_node_no_fruit_sites
                       , &g.node_no_first_flower
                       , emerg
                       , maturity
                       , g.node_no
                       , g.maxt
                       , g.mint
                       , c.x_temp_fruit_site
                       , c.y_rel_fruit_site
                       , c.num_temp_fruit_site
                       , g.dlt_node_no
                       , &g.dlt_fruit_site_no );
      }
   else
      throw std::invalid_argument ("Invalid template option for plant fruit number");

   pop_routine (my_name);
   }

//  Purpose
//      Calculate Fruit Site Number
//
//  Mission Statement
//      Calculate Fruit Numer
void Plant::plant_fruit_number (int option)
   {
   const char *my_name = "plant_fruit_number";

   push_routine (my_name);

   if (option == 1)
      {
      // do not use fruit number
      fill_real_array(g.fruit_no,0, max_fruit_cohorts);
      fill_real_array(g.dlt_fruit_no,0, max_fruit_cohorts);
      fill_real_array(g.fruit_flower_no,0, max_fruit_cohorts);
      }
   else if (option == 2)
      {
      crop_fruit_flower_number (p.dm_fruit_set_crit
                       , p.dm_fruit_set_min
                       , g.dlt_dm
                       , g.dlt_dm_daily
                       , c.days_assimilate_ave
                       , g.day_of_year
                       , g.year
                       , g.fruit_flower_no
                       , max_fruit_cohorts
                       , g.dlt_fruit_site_no
                       , g.setting_fruit
                       , &g.dlt_fruit_flower_no);
      if (g.num_fruit_cohorts > 0)
         {
         g.fruit_flower_no[g.num_fruit_cohorts-1] = g.dlt_fruit_flower_no;
         }
      crop_fruit_number(flowering
                      , max_stage
                      , max_fruit_cohorts
                      , g.num_fruit_cohorts
                      , c.tt_flower_to_start_pod
                      , g.fruit_tt_tot
                      , g.fruit_flower_no
                      , g.dlt_fruit_no);

       // update fruit and flower numbers now
      for (int cohort = 0; cohort < g.num_fruit_cohorts; cohort++)
         {
         g.fruit_no[cohort] +=  g.dlt_fruit_no[cohort];
         g.fruit_flower_no[cohort] -= g.dlt_fruit_no[cohort];
         }
      }
   else
      throw std::invalid_argument ("Invalid template option for plant fruit number");

   pop_routine (my_name);
   }






//+  Purpose
//       Returns cumulative thermal time targets required for the
//       individual growth stages.

//+  Changes
//     <insert here>
void Plant::wheat_phenology_init_nwheat
    (
     float  c_shoot_lag           // (INPUT)  minimum growing degree days fo
    ,float  c_shoot_rate          // (INPUT)  growing deg day increase with
    ,float  g_current_stage       // (INPUT)  current phenological stage
    ,float  *g_days_tot           // (INPUT)  duration of each phase (days)
    ,float  g_sowing_depth        // (INPUT)  sowing depth (mm)
    ,float  *phase_tt             // (INPUT/OUTPUT) cumulative growing degree days required for each stage (deg days)
    ,float  p_startgf_to_mat
    ,float  p_phyllochron
    ) {
//+  Constant Values
    const char*  my_name = "wheat_phenology_init_nwheat" ;

//+  Local Variables

//- Implementation Section ----------------------------------

    push_routine (my_name);

// On the germination day, calculate the tt for emergence
    //      if (on_day_of (sowing, g_current_stage, g_days_tot)||;
    //     :    stage_is_between(sowing, emerg, g_current_stage)) then;

    //        if (p_tt_germ_to_emerg > 1.0) then;
    //           phase_tt(germ_to_emerg) = p_tt_germ_to_emerg;
    //        else;
    phase_tt[germ_to_emerg-1] = c_shoot_lag + g_sowing_depth*c_shoot_rate;
    //        end if;

//This is to avoid a varning in leaf number final
    //         phase_tt(emerg_to_endjuv) = max(1.0, p_tt_emerg_to_endjuv);
    phase_tt[emerg_to_endjuv-1] = 1.0;
    //       if (p_tt_endjuv_to_init > 1.0) then;
    //         phase_tt(endjuv_to_init)  = p_tt_endjuv_to_init;
    //       else;
    phase_tt[endjuv_to_init-1]  = 400.0;
    //c       end if;

    //       if (p_tt_init_to_flag > 1.0) then;
    //         phase_tt(init_to_flag)    = p_tt_init_to_flag;
    //       else;
    //         phase_tt(init_to_flag)    = 3.0 * p_phyllochron;
    //       endif;
    //c;
    //c       if (p_tt_flag_to_flower > 1.0) then;
    //         phase_tt(flag_to_flower)  = p_tt_flag_to_flower;
    //c       else;
    //c         phase_tt(flag_to_flower)  = 2.0 * p_phyllochron + 80.0;
    //c       endif;

    phase_tt[init_to_flower-1] = 5.0 * p_phyllochron + 80.0;

    //c       if (p_tt_flower_to_start_grain > 1.0) then;
    //c         phase_tt(flower_to_start_grain) = p_tt_flower_to_start_grain;
    //c       else;
    phase_tt[flower_to_start_grain-1] = 200.0 - 80.0;
    //c       endif;

    //c       if (p_tt_end_grain_to_maturity > 1.0) then;
    //c         phase_tt(end_grain_to_maturity) = p_tt_end_grain_to_maturity;
    //c       else;
    phase_tt[end_grain_to_maturity-1] = 0.05*(  phase_tt[flower_to_start_grain-1]  + p_startgf_to_mat);
    //c       endif;

    //c       if (p_tt_start_to_end_grain > 1.0) then;
    //c         phase_tt(start_to_end_grain)    = p_tt_start_to_end_grain;
    //c       else;
    phase_tt[start_to_end_grain-1]    = p_startgf_to_mat - phase_tt[end_grain_to_maturity-1];
    //c       endif;

    //c       if (p_tt_maturity_to_ripe > 1.0) then;
    //c         phase_tt(maturity_to_ripe) = p_tt_maturity_to_ripe;
    //c       else;
    phase_tt[maturity_to_ripe-1] = 1.0;
    //c       endif;

    //c       if (p_tt_ripe_to_harvest > 1.0) then;
    //c         phase_tt(ripe_to_harvest)  = p_tt_ripe_to_harvest;
    //c       else;
    phase_tt[ripe_to_harvest-1]  = 1000.0 ;         // keep it from dying????
    //c       endif;
    //c      endif;
    //c      print *, "p_startgf_to_mat=", p_startgf_to_mat;

// Between germ and floral initiation, the target should be set every day based on
// photoperiod and vernalisation
    //c      if (stage_is_between (emerg, floral_init;
    //c     :                      , g_current_stage)) then;
    //c        ;                                    //Use the smaller one of vernalization and photoperiod effect
    //c        vern_php_eff = min(g_vern_eff, g_photop_eff);
    //c;
    //c        ;                                    //Change the thermal time target for endjuv to init
    //c;
    //c        phase_tt(endjuv_to_init)= phase_tt(endjuv_to_init);
    //c     :                          + g_dlt_tt *(1.0 - vern_php_eff);
    //c;
    //c      endif;

    pop_routine (my_name);
    return;
    }



//+  Purpose
//       Simulate crop processes.  These include biomass production,
//       phenological stages, plant component development,
//       water uptake and nitrogen uptake, and plant senescense.

//+  Mission Statement
//     Performs actions for the current day

//+  Changes
//      250894 jngh specified and programmed
void Plant::plant_process ( void )
    {
//+  Constant Values
    const char*  my_name = "plant_process" ;

//- Implementation Section ----------------------------------
    //!!!!!!!! check order dependency of deltas
    push_routine (my_name);

//      call plant_root_depth (1)  !1 = xf added
    plant_root_depth (2);                         //sw stress factor
    plant_root_depth_init(1);
    plant_water_supply (1);

    if (g.plant_status == alive)
        {
        //c         call plant_nit_stress (1);
        //c         call plant_temp_stress (1);

        //c         call plant_light_supply (1);
        //c         call plant_bio_rue (1);
        //c         call plant_transpiration_eff (1);
        //c         call plant_water_demand (1);

        plant_water_stress (2);
        plant_oxdef_stress (1);

        plant_phenology_init (c.phenology_option);
        plant_phenology (c.phenology_option);

        plant_grain_number(c.grain_no_option);

        plant_water_uptake (1);
        plant_height (1);                         // variety specific stem wt/plant approach
        plant_width (1);                          // variety specific stem wt/plant approach
        plant_leaf_no_init(1);
        plant_leaf_no_pot (c.leaf_no_pot_option); // plant node/leaf approach

        plant_leaf_area_init (1);
        plant_leaf_area_potential (1);            // linear interp leaf size
        plant_leaf_area_stressed (1);

        plant_bio_water (1);
        plant_bio_rue (1);
        //fprintf(stdout, "%d,%.9f,%.9f,%.9f\n", g.day_of_year,g.dlt_dm, g.dlt_dm_pot_rue, g.dlt_dm_pot_te);
        plant_bio_init(c.fruit_no_option);
        plant_bio_actual (1);
        // c1
        //fprintf(stdout, "%d,%.9f,%.9f,%.9f\n", g.day_of_year,g.dlt_dm, g.dlt_dm_pot_rue, g.dlt_dm_pot_te);

        plant_fruit_site_number(c.fruit_no_option);
        plant_fruit_number(c.fruit_no_option);

        plant_bio_grain_demand_stress(1);

        plant_bio_grain_demand (c.grain_fill_option);
        plant_bio_grain_oil (1);

        plant_bio_partition (c.partition_option);

        //plant_retrans_init(1);

        plant_bio_retrans (c.partition_option);

        plant_leaf_area_actual (1);               // plant node/leaf approach with
                                                  // sla_max = f(lai)

        plant_pod_area (1);

        plant_leaf_no_actual(1);
        plant_root_length_init(1);                //added NIH
        plant_root_length_growth(c.root_growth_option);              //added NIH

        plant_leaf_death (1);                     // 1 = fract leaf death rate
        plant_leaf_area_sen (1);

        plant_sen_bio (c.dm_senescence_option);
        plant_sen_root_length(1);                 // added NIH

        plant_nit_init (1);
        plant_nit_grain_demand (c.grain_n_option);

        plant_nit_supply (c.n_uptake_option);
        if (c.n_retrans_option==1)
           {
           // this option requires retrans to happen before working out
           // n demand from soil
           // NOTE: two processes are linked.
           plant_nit_retrans (c.n_retrans_option);
           plant_nit_demand (c.n_retrans_option);
           }
         else
           {
           plant_nit_demand (c.n_retrans_option);
           }


        plant_sen_nit (c.n_senescence_option);
        plant_soil_nit_demand (1);
        plant_nit_uptake (c.n_uptake_option);     // allows preference of N source
        plant_nit_partition (1);                  // allows output of n fixed
        if (c.n_retrans_option==2)
           {
           // this option requires soil uptake to satisfy grain n before
           // retranslocation
           plant_nit_retrans (c.n_retrans_option);
           }
        phosphorus->process(g.current_stage
                            , g.dm_green
                            , g.dlt_dm_senesced);

        plant_fruit_abort(c.fruit_no_option);
        plant_plant_death (1);
        }
    else
        {
        // crop is dead
        }

    if (g.plant_status == dead)
        {
        // crop is dead
        //cjngh         call plant_zero_variables ()
        plant_dead ();
        }
    else
        {
        // crop is alive
        }

    plant_detachment (1);
    phosphorus->detachment(g.dm_senesced
                          , g.dm_dead
                          , g.dlt_dm_detached
                          , g.dlt_dm_dead_detached);

    phosphorus->death(g.plants, g.dlt_plants);

    plant_fruit_cleanup(c.fruit_no_option);
    plant_cleanup();

    //plant_fruit_cohort_number(c.fruit_no_option);

    plant_water_stress (2);
    plant_nit_stress (c.n_stress_option);

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Set up states for dead crop

//+  Mission Statement
//     Sets up states for dead crop

//+  Changes
//      091095 jngh specified and programmed
void Plant::plant_dead (void)
    {
    const char*  my_name = "plant_dead" ;

    push_routine (my_name);

    g.current_stage   = (float) plant_end;

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Report occurence of harvest and the current status of specific
//       variables.

//+  Mission Statement
//     Carry out all the harvest routines

//+  Changes
//     010994 jngh specified and programmed

void Plant::plant_harvest (protocol::Variant &v/*(INPUT) message variant*/)
    {
    const char*  my_name = "plant_harvest" ;
    FString  report_flag;
    push_routine (my_name);

    protocol::ApsimVariant incomingApsimVariant(parent);
    incomingApsimVariant.aliasTo(v.getMessageData());

    if (g.plant_status != out)
        {
        // crop harvested. Report status
        if (incomingApsimVariant.get("report", protocol::DTstring, false, report_flag) == false ||
            report_flag == "yes")
            {
            plant_harvest_report();
            }
        else
            {
            }
        plant_auto_class_change("harvest");
        plant_harvest_update(v);
        }
    else
        {
        char msg[80];
        sprintf(msg, "%s%s"
                , g.module_name.c_str()
                , " is not in the ground - unable to harvest.");
        parent->warningError (msg);
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Report occurence of harvest and the current status of specific
//       variables.

//+  Mission Statement
//     Carry out all the harvest routines

//+  Changes
//     010994 jngh specified and programmed
void Plant::plant_kill_stem (protocol::Variant &v/*(INPUT) incoming message variant*/)
    {
//+  Constant Values
    const char*  my_name = "plant_kill_stem" ;

//+  Local Variables

//- Implementation Section ----------------------------------
    push_routine (my_name);

    plant_auto_class_change("kill_stem");

    plant_kill_stem_update(v);

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Remove crop biomass.

//+  Mission Statement
//     Remove crop biomass

//+  Changes
//     200904 jngh specified and programmed
void Plant::plant_remove_crop_biomass (protocol::Variant &v/*(INPUT) incoming message variant*/)
    {
//+  Constant Values
    const char*  my_name = "plant_remove_crop_biomass" ;

//+  Local Variables

//- Implementation Section ----------------------------------
    push_routine (my_name);

    //plant_auto_class_change("remove_biomass");

    plant_remove_biomass_update(v);

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Report occurence of harvest and the current status of specific
//       variables.

//+  Mission Statement
//     Carry out all the harvest routines

//+  Changes
//     010994 jngh specified and programmed
void Plant::plant_dormancy (protocol::Variant &v/*(INPUT) incoming message variant*/)
    {
//+  Constant Values
    const char*  my_name = "plant_dormancy" ;

//+  Local Variables
    FString  dormancy_flag;
    FString  previous_class;

//- Implementation Section ----------------------------------

    push_routine (my_name);

    protocol::ApsimVariant incomingApsimVariant(parent);
    incomingApsimVariant.aliasTo(v.getMessageData());

// crop harvested. Report status
    if (incomingApsimVariant.get("state", protocol::DTstring, false, dormancy_flag) == false)
         {
         throw std::invalid_argument("dormancy state must be specified");
         }

    if (dormancy_flag == "on")
        {
        previous_class = g.crop_class.c_str();
        plant_auto_class_change("dormancy");
        if (previous_class.f_str() != g.crop_class)
            {
            g.pre_dormancy_crop_class = previous_class.f_str();
            }
        else
            {
            // still dormant
            }
        }
    else if (dormancy_flag == "off")
        {
        g.crop_class = g.pre_dormancy_crop_class;
        plant_read_species_const();
        }
    else
        {
        // unknown dormancy_flag
        throw std::invalid_argument ("dormancy state is unknown - neither on nor off");
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Update states after a harvest

//+  Mission Statement
//     Update the states of variables after a harvest

//+  Changes
//      171297 nih specified and programmed
//      160798 nih fixed bug in n_init calculation
//      191099 jngh changed to plant_Send_Crop_Chopped_Event
//      261099 jngh removed energy from residue components
//      131100 jngh removed energy
//      210201 dsg replaced unprotected divides with 'divide' function
void Plant::plant_harvest_update (protocol::Variant &v/*(INPUT)message arguments*/)
    {

//+  Constant Values
    const char*  my_name = "plant_harvest_update" ;

//+  Local Variables
    //c      real       dlt_leaf_area         ;     // leaf area increase (mm^2/plant)
    float dm_residue;                             // dry matter added to residue (kg/ha)
    float n_residue;                              // nitrogen added to residue (kg/ha)
    float dm_root_residue;                             // dry matter added to residue (kg/ha)
    float n_root_residue;                              // nitrogen added to residue (kg/ha)
    float dm_tops_residue;                             // dry matter added to residue (kg/ha)
    float n_tops_residue;                              // nitrogen added to residue (kg/ha)
    float P_residue;                              // phosphorus added to residue (g/m^2)
    float dm_removed;                             // dry matter removed from system (kg/ha)
    float n_removed;                              // nitrogen removed from system (kg/ha)
    float dm_root;
    float n_root;
//integer    leaf_no               ! currently expanding leaf no.
    float fract;
    int   numvals;
    int   part;
    float part_current;
    float part_previous;
    float remove_fr;
    int   stage_no;
    int   stage_no_current;
    int   stage_no_previous;
    float dm_init;
    float n_init;
    float height;                                 // cutting height
    float fr_height;                              // fractional cutting height
    float retain_fr_green;
    float retain_fr_sen;
    float retain_fr_dead;
    float canopy_fac;
    float temp;
    float chop_fr_green[max_part];                      // fraction chopped (0-1)
    float chop_fr_sen[max_part];                      // fraction chopped (0-1)
    float chop_fr_dead[max_part];                      // fraction chopped (0-1)
    float chop_fr;                                 // fraction chopped (0-1)
    float fraction_to_residue[max_part];          // fraction sent to residue (0-1)
    float dlt_dm_crop[max_part];                  // change in dry matter of crop (kg/ha)
    float dlt_dm_n[max_part];                     // change in N content of dry matter (kg/ha)
    float dlt_dm_p[max_part];                     // change in P content of dry matter (kg/ha)
    float dlt_dm_harvest;                         // dry matter harvested (g/m^2)
    float dlt_n_harvest;                          // N content of dm harvested (g/m^2)
    float dlt_dm_die;                             // dry matter in dieback of roots (g/m^2)
    float dlt_n_die;                              // N content of drymatter in dieback (g/m^2)
    float avg_leaf_area;
    float P_tops;

//- Implementation Section ----------------------------------
    push_routine (my_name);

    // Tell the rest of the system we are about to harvest
    sendStageMessage("harvesting");

    protocol::ApsimVariant incomingApsimVariant(parent);
    incomingApsimVariant.aliasTo(v.getMessageData());

    // determine the cutting height
    if (incomingApsimVariant.get("height", protocol::DTsingle, false, height) == false)
       {
       height = 0.0;
       }
    bound_check_real_var(parent,height, 0.0, 1000.0, "height");

    g.previous_stage = g.current_stage;

    stage_no_current = int (g.current_stage);
    g.current_stage = c.stage_stem_reduction_harvest[stage_no_current-1];

    // determine the new stem density
    // ==============================
    if (incomingApsimVariant.get("plants", protocol::DTsingle, false, temp) == true)
        {
        bound_check_real_var(parent,temp, 0.0, 10000.0, "plants");
        g.plants = temp;
        }

    if (incomingApsimVariant.get("remove", protocol::DTsingle, false, remove_fr) == false)
        {
        remove_fr = 0.0;
        }
    bound_check_real_var(parent,remove_fr, 0.0, 1.0, "remove");

    for (part=0; part < max_part; part++)
        {
        fraction_to_residue[part] = 0.0;
        dlt_dm_crop[part] = 0.0;
        dlt_dm_n[part] = 0.0;
        }

    //dm_root = 0.0;
    //n_root = 0.0;

    // Update biomass and N pools.  Different types of plant pools are
    // ===============================================================
    // affected differently.
    // =====================
    for (part=0; part<max_part;part++)
        {
        if (part==root)
            {
            // Calculate Root Die Back
            retain_fr_green = 1.0;
            retain_fr_sen = 1.0;
            retain_fr_dead = 1.0;
            chop_fr_green[part] = 1.0 - retain_fr_green;
            chop_fr_dead[part]  = 1.0 - retain_fr_sen;
            chop_fr_sen[part]   = 1.0 - retain_fr_dead;

            fraction_to_residue[part] = 0.0;

            dlt_dm_die = g.dm_green[part] * c.root_die_back_fr;
            g.dm_senesced[part] = g.dm_senesced[part] + dlt_dm_die;
            g.dm_green[part] = g.dm_green[part] - dlt_dm_die;

            dlt_n_die =  dlt_dm_die * c.n_sen_conc[part];
            g.n_senesced[part] = g.n_senesced[part] + dlt_n_die;
            g.n_green[part]= g.n_green[part] - dlt_n_die;

             dlt_n_harvest = g.n_dead[part]*chop_fr_dead[part] + g.n_green[part]*chop_fr_green[part] + g.n_senesced[part]*chop_fr_sen[part];
             dlt_dm_harvest = g.dm_dead[part]*chop_fr_dead[part] + g.dm_green[part]*chop_fr_green[part] + g.dm_senesced[part]*chop_fr_sen[part];

             dlt_dm_crop[part] = dlt_dm_harvest * gm2kg/sm2ha;
             dlt_dm_n[part] = dlt_n_harvest * gm2kg/sm2ha;

             fraction_to_residue[part] = 0.0;

             g.dm_dead[part] = retain_fr_dead * g.dm_dead[part];
             g.dm_senesced[part] = retain_fr_sen * g.dm_senesced[part];
             g.dm_green[part] = retain_fr_green * g.dm_green[part];

             g.dm_fruit_dead[0][part] = retain_fr_dead * g.dm_dead[part];
             g.dm_fruit_senesced[0][part] = retain_fr_sen * g.dm_senesced[part];
             g.dm_fruit_green[0][part] = retain_fr_green * g.dm_green[part];

             g.n_dead[part] = retain_fr_dead * g.n_dead[part];
             g.n_senesced[part] = retain_fr_sen * g.n_senesced[part];
             g.n_green[part] = retain_fr_green * g.n_green[part];

            }
        else
            {
           // we are accounting for tops
           // Calculate return of biomass to surface residues
            if (part==meal || part==oil)
                {
                // biomass is removed
                retain_fr_green = 0.0;
                retain_fr_sen = 0.0;
                retain_fr_dead = 0.0;
                chop_fr_green[part] = 1.0 - retain_fr_green;
                chop_fr_dead[part]  = 1.0 - retain_fr_dead;
                chop_fr_sen[part]   = 1.0 - retain_fr_sen;
                fraction_to_residue[part] = 0.0;

                dlt_n_harvest = g.n_dead[part]*chop_fr_dead[part] + g.n_green[part]*chop_fr_green[part] + g.n_senesced[part]*chop_fr_sen[part];
                dlt_dm_harvest = g.dm_dead[part]*chop_fr_dead[part] + g.dm_green[part]*chop_fr_green[part] + g.dm_senesced[part]*chop_fr_sen[part];

                dlt_dm_crop[part] = dlt_dm_harvest * gm2kg/sm2ha;
                dlt_dm_n[part] = dlt_n_harvest * gm2kg/sm2ha;

                g.dm_dead[part] = retain_fr_dead * g.dm_dead[part];
                g.dm_senesced[part] = retain_fr_sen * g.dm_senesced[part];
                g.dm_green[part] = retain_fr_green * g.dm_green[part];

                for (int cohort = 0; cohort < max_fruit_cohorts; cohort++)
                   {
                   g.dm_fruit_dead[cohort][part]     = g.dm_fruit_dead[cohort][part] * retain_fr_dead;
                   g.dm_fruit_senesced[cohort][part] = g.dm_fruit_senesced[cohort][part] * retain_fr_sen;
                   g.dm_fruit_green[cohort][part]    = g.dm_fruit_green[cohort][part] * retain_fr_green;
                   }

                g.n_dead[part] = retain_fr_dead * g.n_dead[part];
                g.n_senesced[part] = retain_fr_sen * g.n_senesced[part];
                g.n_green[part] = retain_fr_green * g.n_green[part];

                }
            else if (part==stem)
                {
                // Some biomass is removed according to harvest height

                fr_height = divide (height,g.canopy_height, 0.0);
                retain_fr_green = linear_interp_real (fr_height
                                                     ,c.fr_height_cut
                                                     ,c.fr_stem_remain
                                                     ,c.num_fr_height_cut);

                retain_fr_sen = retain_fr_green;
                retain_fr_dead = retain_fr_green;
                chop_fr_green[part] = 1.0 -retain_fr_green;
                chop_fr_dead[part]  = 1.0 -retain_fr_dead;
                chop_fr_sen[part]   = 1.0 -retain_fr_sen;
                fraction_to_residue[part] = (1.0 - remove_fr);

                dlt_n_harvest = g.n_dead[part]*chop_fr_dead[part] + g.n_green[part]*chop_fr_green[part] + g.n_senesced[part]*chop_fr_sen[part];
                dlt_dm_harvest = g.dm_dead[part]*chop_fr_dead[part] + g.dm_green[part]*chop_fr_green[part] + g.dm_senesced[part]*chop_fr_sen[part];

                dlt_dm_crop[part] = dlt_dm_harvest * gm2kg/sm2ha;
                dlt_dm_n[part] = dlt_n_harvest * gm2kg/sm2ha;


                g.dm_dead[part] = retain_fr_dead * g.dm_dead[part];
                g.dm_senesced[part] = retain_fr_sen * g.dm_senesced[part];
                g.dm_green[part] = retain_fr_green * g.dm_green[part];

                g.dm_fruit_dead[0][part] = retain_fr_dead * g.dm_dead[part];
                g.dm_fruit_senesced[0][part] = retain_fr_sen * g.dm_senesced[part];
                g.dm_fruit_green[0][part] = retain_fr_green * g.dm_green[part];

                g.n_dead[part] = retain_fr_dead * g.n_dead[part];
                g.n_senesced[part] = retain_fr_sen * g.n_senesced[part];
                g.n_green[part] = retain_fr_green * g.n_green[part];
                }
            else
                {
                // this includes leaf, pod
                dm_init = u_bound(c.dm_init [part] * g.plants, g.dm_green[part]);
                n_init = u_bound(dm_init * c.n_init_conc[part],g.n_green[part]);

                retain_fr_green = divide(dm_init, g.dm_green[part], 0.0);
                retain_fr_dead = 0.0;
                retain_fr_sen = 0.0;

                chop_fr_green[part] = 1.0 - retain_fr_green;
                chop_fr_dead[part]  = 1.0 - retain_fr_dead;
                chop_fr_sen[part]   = 1.0 - retain_fr_sen;
                fraction_to_residue[part] = (1.0 - remove_fr);

                dlt_n_harvest  = g.n_dead[part]  + g.n_green[part]  + g.n_senesced[part] - n_init;
                dlt_dm_harvest = g.dm_dead[part] + g.dm_green[part] + g.dm_senesced[part] - dm_init;

                dlt_dm_crop[part] = dlt_dm_harvest * gm2kg/sm2ha;
                dlt_dm_n[part] = dlt_n_harvest * gm2kg/sm2ha;

                g.dm_dead[part] = retain_fr_dead * g.dm_dead[part];
                g.dm_senesced[part] = retain_fr_sen * g.dm_senesced[part];
                g.dm_green[part] = retain_fr_green * g.dm_green[part];

                for (int cohort = 0; cohort < max_fruit_cohorts; cohort++)
                   {
                   g.dm_fruit_dead[cohort][part]     = g.dm_fruit_dead[cohort][part] * retain_fr_dead;
                   g.dm_fruit_senesced[cohort][part] = g.dm_fruit_senesced[cohort][part] * retain_fr_sen;
                   g.dm_fruit_green[cohort][part]    = g.dm_fruit_green[cohort][part] * retain_fr_green;
                   }

                g.n_dead[part] = retain_fr_dead * g.n_dead[part];
                g.n_senesced[part] = retain_fr_sen * g.n_senesced[part];
                g.n_green[part] = n_init;

                }
            }
        }

    //     call crop_top_residue (c%crop_type, dm_residue, N_residue)

    if (sum_real_array(dlt_dm_crop, max_part) > 0.0)
        {
       vector<string> part_name;
       part_name.push_back("root");
       part_name.push_back("leaf");
       part_name.push_back("stem");
       part_name.push_back("pod");
       part_name.push_back("meal");
       part_name.push_back("oil");

         // Call plant P module so that it can add
         // its P to the message - green,senesced and dead
         // all removed using same fractions
       phosphorus->crop_chopped(chop_fr_green  // green
                                  , chop_fr_sen    // senesced
                                  , chop_fr_dead   // dead
                                  , &P_tops
                                  , dlt_dm_p); // dlt_dm_p should be P harvested from green, sen and dead

        plant_send_crop_chopped_event (c.crop_type
                                     , part_name
                                     , dlt_dm_crop
                                     , dlt_dm_n
                                     , dlt_dm_p
                                     , fraction_to_residue
                                     , max_part);
        }
    else
        {
        // no surface residue
        }



    dm_residue = 0.0;
    for (part=0; part < max_part; part++)
     dm_residue = dm_residue + (dlt_dm_crop[part] * fraction_to_residue[part]);

    n_residue = 0.0;
    for (part=0; part < max_part; part++)
      n_residue = n_residue + (dlt_dm_n[part] * fraction_to_residue[part]);

    dm_root_residue = dlt_dm_crop[root] * fraction_to_residue[root];
    n_root_residue = dlt_dm_n[root] * fraction_to_residue[root];

    dm_tops_residue = dm_residue - dm_root_residue;
    n_tops_residue = n_residue - n_root_residue;

    parent->writeString ("\nCrop harvested.");

    char  msg[400];

        parent->writeString ("    Organic matter from crop:-      Tops to surface residue      Roots to soil FOM");

        sprintf (msg, "%48s%7.2f%24.2f", "DM (kg/ha) =               ", dm_tops_residue, dm_root_residue);
        parent->writeString (msg);

        sprintf (msg, "%48s%7.2f%24.2f", "N  (kg/ha) =               ", n_tops_residue, n_root_residue);
        parent->writeString (msg);

        parent->writeString (" ");


    float dm_chopped_tops = sum_real_array(dlt_dm_crop, max_part) - dlt_dm_crop[root];
    float dm_chopped_root = dlt_dm_crop[root];
    float dm_removed_tops = dm_chopped_tops - dm_tops_residue;
    float dm_removed_root = dm_chopped_root - dm_root_residue;

    float n_chopped_tops = sum_real_array(dlt_dm_n, max_part) - dlt_dm_n[root];
    float n_chopped_root = dlt_dm_n[root];
    float n_removed_tops = n_chopped_tops - n_tops_residue;
    float n_removed_root = n_chopped_root - n_root_residue;

        parent->writeString ("    Organic matter removed from system:-      From Tops               From Roots");

        sprintf (msg, "%48s%7.2f%24.2f", "DM (kg/ha) =               ", dm_removed_tops, dm_removed_root);
        parent->writeString (msg);

        sprintf (msg, "%48s%7.2f%24.2f", "N  (kg/ha) =               ", n_removed_tops, n_removed_root);
        parent->writeString (msg);

        parent->writeString (" ");

    // put roots into root residue

    // call plant_root_incorp (dm_root, n_root)

    // Initialise plant leaf area

    g.lai = c.initial_tpla * smm2sm * g.plants;
    g.slai = 0.0;
    g.tlai_dead = 0.0;

    g.canopy_width = 10.0;
    if (p.num_canopy_widths > 0)
        {
        legnew_canopy_fac (g.row_spacing
                           , g.plants
                           , g.skip_row_fac
                           , g.skip_plant_fac
                           , g.canopy_width
                           , &canopy_fac);
        }
    else
        {
        canopy_fac = g.skip_row_fac;
        }

// now update new canopy covers

    legnew_cover_leaf_pod(g.row_spacing
                          ,c.x_row_spacing
                          ,c.y_extinct_coef
                          ,c.num_row_spacing
                          ,c.extinct_coef_pod
                          , canopy_fac
                          ,g.lai
                          ,g.pai
                          ,&g.lai_canopy_green
                          ,&g.cover_green
                          ,&g.cover_pod);

    legnew_cover (g.row_spacing
                  ,c.x_row_spacing
                  ,c.y_extinct_coef_dead
                  ,c.num_row_spacing
                  , canopy_fac
                  ,g.slai
                  ,&g.cover_sen);
    legnew_cover (g.row_spacing
                  ,c.x_row_spacing
                  ,c.y_extinct_coef_dead
                  ,c.num_row_spacing
                  , canopy_fac
                  ,g.tlai_dead
                  ,&g.cover_dead);

    fill_real_array (g.leaf_no, 0.0, max_node);
    fill_real_array (g.leaf_no_dead, 0.0, max_node);
    fill_real_array (g.node_no, 0.0, max_stage);
    fill_real_array (g.leaf_area, 0.0, max_node);

    stage_no_current = (int) g.current_stage;

    g.node_no[stage_no_current-1] = c.leaf_no_at_emerg;

    fill_real_array (g.leaf_no, 1.0, (int)c.leaf_no_at_emerg);
    g.leaf_no[(int)c.leaf_no_at_emerg] = fmod(c.leaf_no_at_emerg,1.0);

    avg_leaf_area = divide(c.initial_tpla,c.leaf_no_at_emerg,0.0);

    fill_real_array (g.leaf_area,avg_leaf_area,(int)c.leaf_no_at_emerg);
    g.leaf_area[(int)c.leaf_no_at_emerg] = fmod(c.leaf_no_at_emerg,1.0) * avg_leaf_area;

    //cnh the following has not been changed though may not be necessary;
    //cnh start;
    stage_no_current = (int) g.current_stage;
    stage_no_previous = (int) g.previous_stage;
    part_current = fmod(g.current_stage, 1.0);
    part_previous = fmod(g.previous_stage, 1.0);

    if (stage_no_current==stage_no_previous)
        {
        fract = divide (part_current, part_previous, 0.0);
        }
    else
        {
        fract = part_current;
        for (stage_no = stage_no_current+1; stage_no<=stage_no_previous;stage_no++)
            {
            g.days_tot[stage_no-1]       = 0.0;
            g.tt_tot[stage_no-1]         = 0.0;
            g.heat_stress_tt[stage_no-1] = 0.0;
            g.dm_stress_max[stage_no-1]  = 0.0;
            g.cswd_photo[stage_no-1]     = 0.0;
            g.cswd_expansion[stage_no-1] = 0.0;
            g.cnd_photo[stage_no-1]      = 0.0;
            g.cnd_grain_conc[stage_no-1] = 0.0;
            }
        }
    g.days_tot[stage_no_current-1]       = fract * g.days_tot[stage_no_current-1];
    g.tt_tot[stage_no_current-1]         = fract * g.tt_tot[stage_no_current-1];
    g.heat_stress_tt[stage_no_current-1] = 0.0;
    g.dm_stress_max[stage_no_current-1]  = 0.0;
    g.cswd_photo[stage_no_current-1]     = 0.0;
    g.cswd_expansion[stage_no_current-1] = 0.0;
    g.cnd_photo[stage_no_current-1]      = 0.0;
    g.cnd_grain_conc[stage_no_current-1] = 0.0;

// other plant states

    g.canopy_height = 1.0;

    plant_n_conc_limits (c.stage_code_list
    , g.phase_tt
    , g.tt_tot
    , c.n_conc_crit_grain
    , c.n_conc_crit_root
    , c.n_conc_max_grain
    , c.n_conc_max_root
    , c.n_conc_min_grain
    , c.n_conc_min_root
    , c.x_stage_code
    , c.y_n_conc_crit_leaf
    , c.y_n_conc_crit_pod
    , c.y_n_conc_crit_stem
    , c.y_n_conc_max_leaf
    , c.y_n_conc_max_pod
    , c.y_n_conc_max_stem
    , c.y_n_conc_min_leaf
    , c.y_n_conc_min_pod
    , c.y_n_conc_min_stem
    , c.x_co2_nconc_modifier
    , c.y_co2_nconc_modifier
    , c.num_co2_nconc_modifier
    , g.co2
    , g.current_stage
    , g.n_conc_crit
    , g.n_conc_max
    , g.n_conc_min
    )  ;                                          // plant N concentr

    if (g.plant_status == alive &&
        g.current_stage < g.previous_stage)
        {
        plant_event (
          g.current_stage
        , g.dlayer
        , g.dm_dead
        , g.dm_green
        , g.dm_senesced
        , g.lai
        , g.n_green
        , g.root_depth
        , g.sw_dep
        , p.ll_dep);
        }
    else
        {
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Update states after a sen

//+  Mission Statement
//     Update the states of variables after a sen

//+  Changes
//      171297 nih specified and programmed
//      160798 nih fixed bug in n_init calculation
//      191099 jngh changed to plant_Send_Crop_Chopped_Event
//      261099 jngh removed energy from residue components
//      131100 jngh removed energy
//      210201 dsg replaced unprotected divides with 'divide' function
void Plant::plant_kill_stem_update (protocol::Variant &v/*(INPUT) message arguments*/)
    {

//+  Constant Values
    const char*  my_name = "plant_kill_stem_update" ;

//+  Local Variables
    float stage_fract;
    int   part;
    float stage_part_current;
    float stage_part_previous;
    int   stage_no;
    int   stage_no_current;
    int   stage_no_previous;
    float dm_init;
    float n_init;
    float temp;
    float dlt_dm_sen;                             // dry matter sened (g/m^2)
    float dlt_n_sen;                              // N content of dm sened (g/m^2)
    float avg_leaf_area;
    int   numvals;
    int   leaf_no_emerged;
    float leaf_emerging_fract;
    float canopy_fac;

//- Implementation Section ----------------------------------
    push_routine (my_name);

    protocol::ApsimVariant aV(parent);
    aV.aliasTo(v.getMessageData());


    g.previous_stage = g.current_stage;

    stage_no_current = (int) g.current_stage;
    g.current_stage =  c.stage_stem_reduction_kill_stem[stage_no_current-1];

    // determine the new stem density
    // ==============================
    if (aV.get("plants", protocol::DTsingle, false, temp) == true)
        {
        bound_check_real_var(parent,temp, 0.0, 10000.0, "plants");
        g.plants = temp;
        }

    // if annual, then modify root and leaf so all goes to dead.
    // Update biomass and N pools.

    for (part = 0; part <max_part; part++)
       {
       if (part == root)
           {
           // Calculate Root Die Back
           dlt_n_sen =  g.dm_green[part]
           * c.root_die_back_fr
           * c.n_sen_conc[part];
           g.n_senesced[part] = g.n_senesced[part]
           + dlt_n_sen;
           g.n_green[part]= g.n_green[part]
           - dlt_n_sen;
           }
       else if (part == leaf)
           {
           dm_init = c.dm_init [part] * g.plants;
           n_init = dm_init * c.n_init_conc[part];

           g.n_dead[part] = g.n_dead[part]
           + g.n_green[part]
           + g.n_senesced[part]
           - n_init;
           g.n_dead[part] = l_bound (g.n_dead[part], 0.0);
           g.n_green[part] = n_init;
           g.n_senesced[part] = 0.0;
           }
       else
           {
           g.n_dead[part] = g.n_dead[part]
           + g.n_green[part]
           + g.n_senesced[part];
           g.n_green[part] = 0.0;
           g.n_senesced[part] = 0.0;
           }
       }

// Transfer plant dry matter
    for (part = 0; part <max_part;part++)
        {
        if (part == root)
            {
            // Calculate Root Die Back
            dlt_dm_sen = g.dm_green[part] * c.root_die_back_fr;
            g.dm_senesced[part] = g.dm_senesced[part] + dlt_dm_sen;
            g.dm_green[part] = g.dm_green[part] - dlt_dm_sen;
            }
        else if (part == leaf)
            {
            dm_init = c.dm_init [part] * g.plants;
            g.dm_dead[part] = g.dm_dead[part]
                                 + g.dm_green[part]
                                 + g.dm_senesced[part]
                                 - dm_init;
            g.dm_dead[part] = l_bound (g.dm_dead[part], 0.0);
            g.dm_green[part] = dm_init;
            g.dm_senesced[part] = 0.0;
            }
        else
            {
            g.dm_dead[part] = g.dm_dead[part]
                                 + g.dm_green[part]
                                 + g.dm_senesced[part];
            g.dm_green[part] = 0.0;
            g.dm_senesced[part] = 0.0;
            }
        g.dm_fruit_dead[1][part] = g.dm_dead[part];
        g.dm_fruit_green[1][part] = g.dm_green[part];
        g.dm_fruit_senesced[1][part] = g.dm_senesced[part];
        }

    // transfer plant leaf area
    g.slai = 0.0;
    g.tlai_dead = g.tlai_dead + g.lai;
    // Initialise plant leaf area
    g.lai = c.initial_tpla * smm2sm * g.plants;
    g.pai = 0.0;
    // JNGH need to account for dead pai

    // transfer plant grain no.
    g.grain_no = 0.0;
    fill_real_array(g.fruit_no, 0.0, max_fruit_cohorts);
    g.canopy_width = 10.0;
    if (p.num_canopy_widths > 0)
        {
        legnew_canopy_fac (g.row_spacing
                           , g.plants
                           , g.skip_row_fac
                           , g.skip_plant_fac
                           , g.canopy_width
                           , &canopy_fac);
        }
    else
        {
        canopy_fac = g.skip_row_fac;
        }

    // now update new canopy covers

    legnew_cover_leaf_pod (g.row_spacing
                           ,c.x_row_spacing
                           ,c.y_extinct_coef
                           ,c.num_row_spacing
                           ,c.extinct_coef_pod
                           ,canopy_fac
                           ,g.lai
                           ,g.pai
                           ,&g.lai_canopy_green
                           ,&g.cover_green
                           ,&g.cover_pod);

    legnew_cover (g.row_spacing
                  ,c.x_row_spacing
                  ,c.y_extinct_coef_dead
                  ,c.num_row_spacing
                  ,canopy_fac
                  ,g.slai
                  ,&g.cover_sen);
    legnew_cover (g.row_spacing
                  ,c.x_row_spacing
                  ,c.y_extinct_coef_dead
                  ,c.num_row_spacing
                  ,canopy_fac
                  ,g.tlai_dead
                  ,&g.cover_dead);
    for (int node =0; node < max_node; node++)
        {
        g.leaf_no[node] = 0.0;
        g.leaf_no_dead[node] = 0.0;
        g.leaf_area[node] = 0.0;
        }
    for (int node = 0; node < max_stage; node++)
        {
        g.node_no[node] = 0.0;
        }

    stage_no_current = (int) g.current_stage;
    g.node_no[stage_no_current-1] = c.leaf_no_at_emerg;

    leaf_no_emerged = (int) c.leaf_no_at_emerg;
    leaf_emerging_fract = fmod(c.leaf_no_at_emerg, 1.0);

    for (int leaf = 0; leaf < leaf_no_emerged; leaf++)
        {
        g.leaf_no[leaf] = 1.0;
        }
    g.leaf_no[leaf_no_emerged] = leaf_emerging_fract;

    avg_leaf_area = divide (c.initial_tpla, c.leaf_no_at_emerg, 0.0);

    for (int leaf = 0; leaf < leaf_no_emerged; leaf++)
        {
        g.leaf_area[leaf] = avg_leaf_area;
        }
    g.leaf_area[leaf_no_emerged] = leaf_emerging_fract * avg_leaf_area;

    //cnh the following has not been changed though may not be necessary;
    //cnh start;
    stage_no_current = (int) (g.current_stage);
    stage_no_previous = (int) (g.previous_stage);
    stage_part_current = fmod (g.current_stage, 1.0);
    stage_part_previous = fmod (g.previous_stage, 1.0);

    if (stage_no_current==stage_no_previous)
        {
        stage_fract = divide (stage_part_current, stage_part_previous, 0.0);
        }
    else
        {
        stage_fract = stage_part_current;
        for (stage_no = stage_no_current; stage_no< stage_no_previous; stage_no++)
            {
            g.days_tot[stage_no-1]       = 0.0;
            g.tt_tot[stage_no-1]         = 0.0;
            g.heat_stress_tt[stage_no-1] = 0.0;
            g.dm_stress_max[stage_no-1]  = 0.0;
            g.cswd_photo[stage_no-1]     = 0.0;
            g.cswd_expansion[stage_no-1] = 0.0;
            g.cnd_photo[stage_no-1]      = 0.0;
            g.cnd_grain_conc[stage_no-1] = 0.0;
            }
        }
    g.days_tot[stage_no_current-1] =
       stage_fract * g.days_tot[stage_no_current-1];
    g.tt_tot[stage_no_current-1]         =
       stage_fract * g.tt_tot[stage_no_current-1];
    g.heat_stress_tt[stage_no_current-1] = 0.0;
    g.dm_stress_max[stage_no_current-1]  = 0.0;
    g.cswd_photo[stage_no_current-1]     = 0.0;
    g.cswd_expansion[stage_no_current-1] = 0.0;
    g.cnd_photo[stage_no_current-1]      = 0.0;
    g.cnd_grain_conc[stage_no_current-1] = 0.0;
    //cnh end subroutine;
    // other plant states

    g.canopy_height = 1.0;

    plant_n_conc_limits (
           c.stage_code_list
         , g.phase_tt
         , g.tt_tot
         , c.n_conc_crit_grain
         , c.n_conc_crit_root
         , c.n_conc_max_grain
         , c.n_conc_max_root
         , c.n_conc_min_grain
         , c.n_conc_min_root
         , c.x_stage_code
         , c.y_n_conc_crit_leaf
         , c.y_n_conc_crit_pod
         , c.y_n_conc_crit_stem
         , c.y_n_conc_max_leaf
         , c.y_n_conc_max_pod
         , c.y_n_conc_max_stem
         , c.y_n_conc_min_leaf
         , c.y_n_conc_min_pod
         , c.y_n_conc_min_stem
         , c.x_co2_nconc_modifier
         , c.y_co2_nconc_modifier
         , c.num_co2_nconc_modifier
         , g.co2
         , g.current_stage
         , g.n_conc_crit
         , g.n_conc_max
         , g.n_conc_min )  ;                                          // plant N concentr

    if (g.plant_status == alive &&
        g.current_stage < g.previous_stage)
        {
        plant_event (g.current_stage
            , g.dlayer
            , g.dm_dead
            , g.dm_green
            , g.dm_senesced
            , g.lai
            , g.n_green
            , g.root_depth
            , g.sw_dep
            , p.ll_dep );
        }
    else
        {
        }


    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Update states after a harvest

//+  Mission Statement
//     Update the states of variables after a harvest

//+  Changes
//      171297 nih specified and programmed
//      160798 nih fixed bug in n_init calculation
//      191099 jngh changed to plant_Send_Crop_Chopped_Event
//      261099 jngh removed energy from residue components
//      131100 jngh removed energy
//      210201 dsg replaced unprotected divides with 'divide' function
void Plant::plant_remove_biomass_update (protocol::Variant &v/*(INPUT)message arguments*/)
    {

//+  Constant Values
    const char*  my_name = "plant_remove_biomass_update" ;

//+  Local Variables
    //c      real       dlt_leaf_area         ;     // leaf area increase (mm^2/plant)
    float dm_residue;                             // dry matter added to residue (kg/ha)
    float n_residue;                              // nitrogen added to residue (kg/ha)
    float dm_root_residue;                             // dry matter added to residue (kg/ha)
    float n_root_residue;                              // nitrogen added to residue (kg/ha)
    float dm_tops_residue;                             // dry matter added to residue (kg/ha)
    float n_tops_residue;                              // nitrogen added to residue (kg/ha)
    float P_residue;                              // phosphorus added to residue (g/m^2)
    float dm_removed;                             // dry matter removed from system (kg/ha)
    float n_removed;                              // nitrogen removed from system (kg/ha)
    float dm_root;
    float n_root;
//integer    leaf_no               ! currently expanding leaf no.
    float fract;
    int   numvals;
    int   part;
    float part_current;
    float part_previous;
    float remove_fr;
    int   stage_no;
    int   stage_no_current;
    int   stage_no_previous;
    float dm_init;
    float n_init;
    float height;                                 // cutting height
    float fr_height;                              // fractional cutting height
    float retain_fr_green;
    float retain_fr_sen;
    float retain_fr_dead;
    float canopy_fac;
    float temp;
    float chop_fr_green[max_part];                      // fraction chopped (0-1)
    float chop_fr_sen[max_part];                      // fraction chopped (0-1)
    float chop_fr_dead[max_part];                      // fraction chopped (0-1)
    float chop_fr;                                 // fraction chopped (0-1)
    float fraction_to_residue[max_part];          // fraction sent to residue (0-1)
    float dlt_dm_crop[max_part];                  // change in dry matter of crop (kg/ha)
    float dlt_dm_n[max_part];                     // change in N content of dry matter (kg/ha)
    float dlt_dm_p[max_part];                     // change in P content of dry matter (kg/ha)
    float dlt_dm_removed;                         // dry matter harvested (g/m^2)
    float dlt_n_removed;                          // N content of dm harvested (g/m^2)
    float dlt_dm_die;                             // dry matter in dieback of roots (g/m^2)
    float dlt_n_die;                              // N content of drymatter in dieback (g/m^2)
    float avg_leaf_area;
    float P_tops;

    float error_margin = 1.0e-6 ;

//- Implementation Section ----------------------------------
    push_routine (my_name);

     protocol::removeCropDmType dmRemoved;
     v.unpack(dmRemoved);


         ostrstream msg;
         msg << "Remove Crop Biomass:-" << endl;
         float dmTotal = 0.0;

         for (unsigned int pool=0; pool < dmRemoved.dm.size(); pool++)
         {
            for (unsigned int part = 0; part < dmRemoved.dm[pool].part.size(); part++)
            {
               msg << "   dm " << dmRemoved.dm[pool].pool << " " << dmRemoved.dm[pool].part[part] << " = " << dmRemoved.dm[pool].dlt[part] << " (g/m2)" << endl;
               dmTotal +=  dmRemoved.dm[pool].dlt[part];
            }
         }

         msg << endl << "   dm total = " << dmTotal << " (kg/ha)" << endl << ends;

         parent->writeString (msg.str());

      float dltDmGreen[max_part];
      float dltDmSenesced[max_part];
      float dltDmDead[max_part];

      for (int part = 0; part < max_part; part++)
      {
         dltDmGreen[part] = 0.0;
         dltDmSenesced[part] = 0.0;
         dltDmDead[part] = 0.0;
      }

      for (unsigned int pool = 0; pool < dmRemoved.dm.size(); pool++)
      {
         for (unsigned int part = 0; part < dmRemoved.dm[pool].part.size(); part++)
         {
            if (dmRemoved.dm[pool].pool == "green")
            {
               if (dmRemoved.dm[pool].part[part] == "stem")       {dltDmGreen[stem] = dmRemoved.dm[pool].dlt[part]; }
               else if (dmRemoved.dm[pool].part[part] ==  "leaf") {dltDmGreen[leaf] = dmRemoved.dm[pool].dlt[part]; }
               else {  /* unknown part */ }
            }

            else if (dmRemoved.dm[pool].pool == "senesced")
            {
               if (dmRemoved.dm[pool].part[part] == "stem")       {dltDmSenesced[stem] = dmRemoved.dm[pool].dlt[part]; }
               else if (dmRemoved.dm[pool].part[part] ==  "leaf") {dltDmSenesced[leaf] = dmRemoved.dm[pool].dlt[part]; }
               else { /* unknown part */ }
            }

            else if (dmRemoved.dm[pool].pool == "dead")
            {
               if (dmRemoved.dm[pool].part[part] == "stem")       {dltDmDead[stem] = dmRemoved.dm[pool].dlt[part]; }
               else if (dmRemoved.dm[pool].part[part] ==  "leaf") {dltDmDead[leaf] = dmRemoved.dm[pool].dlt[part]; }
               else { /* unknown part */ }
            }
         }
      }
         ostrstream msg1;
         msg1 << "Remove Crop Biomass 2:-" << endl;
         float dmTotal1 = 0.0;

         msg1 << "   dm green leaf = " << dltDmGreen[leaf] << " (g/m2)" << endl;
        msg1 << "   dm green stem = " << dltDmGreen[stem] << " (g/m2)" << endl;
         dmTotal1 +=  dltDmGreen[leaf] + dltDmGreen[stem];

         msg1 << "   dm senesced leaf = " << dltDmSenesced[leaf] << " (g/m2)" << endl;
         msg1 << "   dm senesced stem = " << dltDmSenesced[stem] << " (g/m2)" << endl;
         dmTotal1 +=  dltDmSenesced[leaf] + dltDmSenesced[stem];

         msg1 << "   dm dead leaf = " << dltDmDead[leaf] << " (g/m2)" << endl;
         msg1 << "   dm dead stem = " << dltDmDead[stem] << " (g/m2)" << endl;
         dmTotal1 +=  dltDmDead[leaf] + dltDmDead[stem];

         msg1 << endl << "   dm total = " << dmTotal1 << " (g/m2)" << endl << ends;

         parent->writeString (msg1.str());

         ostrstream msg2;
         msg2 << "Crop Biomass Available:-" << endl;
         float dmTotal2 = 0.0;

         msg2 << "   dm green leaf = " << g.dm_green[leaf] << " (g/m2)" << endl;
         msg2 << "   dm green stem = " << g.dm_green[stem] << " (g/m2)" << endl;
         dmTotal2 +=  g.dm_green[leaf] + g.dm_green[stem];

         msg2 << "   dm senesced leaf = " << g.dm_senesced[leaf] << " (g/m2)" << endl;
         msg2 << "   dm senesced stem = " << g.dm_senesced[stem] << " (g/m2)" << endl;
         dmTotal2 +=  g.dm_senesced[leaf] + g.dm_senesced[stem];

         msg2 << "   dm dead leaf = " << g.dm_dead[leaf] << " (g/m2)" << endl;
         msg2 << "   dm dead stem = " << g.dm_dead[stem] << " (g/m2)" << endl;
         dmTotal2 +=  g.dm_dead[leaf] + g.dm_dead[stem];

         msg2 << endl << "   dm total = " << dmTotal2 << " (g/m2)" << endl << ends;

         parent->writeString (msg2.str());

    g.previous_stage = g.current_stage;

    stage_no_current = int (g.current_stage);
//    g.current_stage = c.stage_stem_reduction_harvest[stage_no_current-1];

//    // determine the new stem density
//    // ==============================
//    if (incomingApsimVariant.get("plants", protocol::DTsingle, false, temp) == true)
//        {
//        bound_check_real_var(parent,temp, 0.0, 10000.0, "plants");
//        g.plants = temp;
//        }
//
//    if (incomingApsimVariant.get("remove", protocol::DTsingle, false, remove_fr) == false)
//        {
//        remove_fr = 0.0;
//        }
//    bound_check_real_var(parent,remove_fr, 0.0, 1.0, "remove");

    for (part=0; part < max_part; part++)
        {
        dlt_dm_crop[part] = 0.0;
        dlt_dm_n[part] = 0.0;
        }

    // Update biomass and N pools.  Different types of plant pools are
    // ===============================================================
    // affected differently.
    // =====================
    for (part=0; part<max_part;part++)
        {
        if (part==root)
            {
            // Calculate Root Die Back

            float chop_fr_green_leaf = divide(dltDmGreen[part], g.dm_green[part], 0.0);
            dlt_dm_die = g.dm_green[part] * c.root_die_back_fr * chop_fr_green_leaf;
            g.dm_senesced[part] = g.dm_senesced[part] + dlt_dm_die;
            g.dm_green[part] = g.dm_green[part] - dlt_dm_die;

            dlt_n_die =  dlt_dm_die * c.n_sen_conc[part];
            g.n_senesced[part] = g.n_senesced[part] + dlt_n_die;
            g.n_green[part]= g.n_green[part] - dlt_n_die;

//             g.dm_fruit_dead[0][part] = retain_fr_dead * g.dm_dead[part];
//             g.dm_fruit_senesced[0][part] = retain_fr_sen * g.dm_senesced[part];
//             g.dm_fruit_green[0][part] = retain_fr_green * g.dm_green[part];

            }
        else
            {
           // we are accounting for tops
//           // Calculate return of biomass to surface residues
            if (part==meal || part==oil)
                {
//                // biomass is removed
//                retain_fr_green = 0.0;
//                retain_fr_sen = 0.0;
//                retain_fr_dead = 0.0;
//                chop_fr_green[part] = 1.0 - retain_fr_green;
//                chop_fr_dead[part]  = 1.0 - retain_fr_dead;
//                chop_fr_sen[part]   = 1.0 - retain_fr_sen;
//                fraction_to_residue[part] = 0.0;
//
//                dlt_n_removed = g.n_dead[part]*chop_fr_dead[part] + g.n_green[part]*chop_fr_green[part] + g.n_senesced[part]*chop_fr_sen[part];
//                dlt_dm_removed = g.dm_dead[part]*chop_fr_dead[part] + g.dm_green[part]*chop_fr_green[part] + g.dm_senesced[part]*chop_fr_sen[part];
//
//                dlt_dm_crop[part] = dlt_dm_removed * gm2kg/sm2ha;
//                dlt_dm_n[part] = dlt_n_removed * gm2kg/sm2ha;
//
//                g.dm_dead[part] = retain_fr_dead * g.dm_dead[part];
//                g.dm_senesced[part] = retain_fr_sen * g.dm_senesced[part];
//                g.dm_green[part] = retain_fr_green * g.dm_green[part];
//
//                for (int cohort = 0; cohort < max_fruit_cohorts; cohort++)
//                   {
//                   g.dm_fruit_dead[cohort][part]     = g.dm_fruit_dead[cohort][part] * retain_fr_dead;
//                   g.dm_fruit_senesced[cohort][part] = g.dm_fruit_senesced[cohort][part] * retain_fr_sen;
//                   g.dm_fruit_green[cohort][part]    = g.dm_fruit_green[cohort][part] * retain_fr_green;
//                   }
//
//                g.n_dead[part] = retain_fr_dead * g.n_dead[part];
//                g.n_senesced[part] = retain_fr_sen * g.n_senesced[part];
//                g.n_green[part] = retain_fr_green * g.n_green[part];
//
            }
            else if (part==stem)
            {
                if (dltDmGreen[part] > g.dm_green[part] + error_margin)
                {
                     ostrstream msg;
                     msg << "Attempting to remove more green stem biomass than available:-" << endl;
                     msg << "Removing " << dltDmGreen[part] << " (g/m2) from " << g.dm_green[part] << " (g/m2) available." << ends;
                     throw std::runtime_error (msg.str());
                }
                else if (dltDmSenesced[part] > g.dm_senesced[part] + error_margin)
                {
                     ostrstream msg;
                     msg << "Attempting to remove more senesced stem biomass than available:-" << endl;
                     msg << "Removing " << dltDmSenesced[part] << " (g/m2) from " << g.dm_senesced[part] << " (g/m2) available." << ends;
                     throw std::runtime_error (msg.str());
                }
                else if (dltDmDead[part] > g.dm_dead[part] + error_margin)
                {
                     ostrstream msg;
                     msg << "Attempting to remove more dead stem biomass than available:-" << endl;
                     msg << "Removing " << dltDmDead[part] << " (g/m2) from " << g.dm_dead[part] << " (g/m2) available." << ends;
                     throw std::runtime_error (msg.str());
                }
                else
                { // no more pools
                }

//                // Some biomass is removed according to harvest height
//
//                fr_height = divide (height,g.canopy_height, 0.0);
//                retain_fr_green = linear_interp_real (fr_height
//                                                     ,c.fr_height_cut
//                                                     ,c.fr_stem_remain
//                                                     ,c.num_fr_height_cut);
//
                chop_fr_green[part] = divide(dltDmGreen[part], g.dm_green[part], 0.0);
                chop_fr_sen[part]   = divide(dltDmSenesced[part], g.dm_senesced[part], 0.0);
                chop_fr_dead[part]  = divide(dltDmDead[part], g.dm_dead[part], 0.0);

//                chop_fr_green[part] = 1.0 -retain_fr_green;
//                chop_fr_dead[part]  = 1.0 -retain_fr_dead;
//                chop_fr_sen[part]   = 1.0 -retain_fr_sen;
//                fraction_to_residue[part] = (1.0 - remove_fr);
//
                dlt_n_removed = g.n_dead[part]*chop_fr_dead[part] + g.n_green[part]*chop_fr_green[part] + g.n_senesced[part]*chop_fr_sen[part];
                dlt_dm_removed = dltDmDead[part] + dltDmGreen[part] + dltDmSenesced[part];

                dlt_dm_crop[part] = dlt_dm_removed * gm2kg/sm2ha;
                dlt_dm_n[part] = dlt_n_removed * gm2kg/sm2ha;


                g.dm_dead[part] = g.dm_dead[part] - dltDmDead[part];
                g.dm_senesced[part] =  g.dm_senesced[part] - dltDmSenesced[part];
                g.dm_green[part] = g.dm_green[part] - dltDmGreen[part];

//                g.dm_fruit_dead[0][part] = retain_fr_dead * g.dm_dead[part];
//                g.dm_fruit_senesced[0][part] = retain_fr_sen * g.dm_senesced[part];
//                g.dm_fruit_green[0][part] = retain_fr_green * g.dm_green[part];
//
                g.n_dead[part] = g.n_dead[part] - g.n_dead[part]*chop_fr_dead[part];
                g.n_senesced[part] = g.n_senesced[part] - g.n_senesced[part]*chop_fr_sen[part];
                g.n_green[part] = g.n_green[part] - g.n_green[part]*chop_fr_green[part];
            }
            else
            {
//                // this includes leaf, pod
                if (dltDmGreen[part] > g.dm_green[part] + error_margin)
                {
                     ostrstream msg;
                     msg << "Attempting to remove more green leaf biomass than available:-" << endl;
                     msg << "Removing " << dltDmGreen[part] << " (g/m2) from " << g.dm_green[part] << " (g/m2) available." << ends;
                     throw std::runtime_error (msg.str());
                }
                else if (dltDmSenesced[part] > g.dm_senesced[part] + error_margin)
                {
                     ostrstream msg;
                     msg << "Attempting to remove more senesced leaf biomass than available:-" << endl;
                     msg << "Removing " << dltDmSenesced[part] << " (g/m2) from " << g.dm_senesced[part] << " (g/m2) available." << ends;
                     throw std::runtime_error (msg.str());
                }
                else if (dltDmDead[part] > g.dm_dead[part] + error_margin)
                {
                     ostrstream msg;
                     msg << "Attempting to remove more dead leaf biomass than available:-" << endl;
                     msg << "Removing " << dltDmDead[part] << " (g/m2) from " << g.dm_dead[part] << " (g/m2) available." << ends;
                     throw std::runtime_error (msg.str());
                }
                else
                { // no more pools
                }

//                dm_init = u_bound(c.dm_init [part] * g.plants, g.dm_green[part]);
//                n_init = u_bound(dm_init * c.n_init_conc[part],g.n_green[part]);


                chop_fr_green[part] = divide(dltDmGreen[part], g.dm_green[part], 0.0);
                chop_fr_sen[part]   = divide(dltDmSenesced[part], g.dm_senesced[part], 0.0);
                chop_fr_dead[part]  = divide(dltDmDead[part], g.dm_dead[part], 0.0);

                dlt_n_removed = g.n_dead[part]*chop_fr_dead[part] + g.n_green[part]*chop_fr_green[part] + g.n_senesced[part]*chop_fr_sen[part];
                dlt_dm_removed = dltDmDead[part] + dltDmGreen[part] + dltDmSenesced[part];

                dlt_dm_crop[part] = dlt_dm_removed * gm2kg/sm2ha;
                dlt_dm_n[part] = dlt_n_removed * gm2kg/sm2ha;


                g.dm_dead[part] = g.dm_dead[part] - dltDmDead[part];
                g.dm_senesced[part] =  g.dm_senesced[part] - dltDmSenesced[part];
                g.dm_green[part] = g.dm_green[part] - dltDmGreen[part];
//
//                for (int cohort = 0; cohort < max_fruit_cohorts; cohort++)
//                   {
//                   g.dm_fruit_dead[cohort][part]     = g.dm_fruit_dead[cohort][part] * retain_fr_dead;
//                   g.dm_fruit_senesced[cohort][part] = g.dm_fruit_senesced[cohort][part] * retain_fr_sen;
//                   g.dm_fruit_green[cohort][part]    = g.dm_fruit_green[cohort][part] * retain_fr_green;
//                   }
//
                g.n_dead[part] = g.n_dead[part] - g.n_dead[part]*chop_fr_dead[part];
                g.n_senesced[part] = g.n_senesced[part] - g.n_senesced[part]*chop_fr_sen[part];
                g.n_green[part] = g.n_green[part] - g.n_green[part]*chop_fr_green[part];
//
                }
            }
        }


//    if (sum_real_array(dlt_dm_crop, max_part) > 0.0)
//        {
//       vector<string> part_name;
//       part_name.push_back("root");
//       part_name.push_back("leaf");
//       part_name.push_back("stem");
//       part_name.push_back("pod");
//       part_name.push_back("meal");
//       part_name.push_back("oil");
//
//         // Call plant P module so that it can add
//         // its P to the message - green,senesced and dead
//         // all removed using same fractions
//       phosphorus->crop_chopped(chop_fr_green  // green
//                                  , chop_fr_sen    // senesced
//                                  , chop_fr_dead   // dead
//                                  , &P_tops
//                                  , dlt_dm_p); // dlt_dm_p should be P harvested from green, sen and dead
//
//        plant_send_crop_chopped_event (c.crop_type
//                                     , part_name
//                                     , dlt_dm_crop
//                                     , dlt_dm_n
//                                     , dlt_dm_p
//                                     , fraction_to_residue
//                                     , max_part);
//        }
//    else
//        {
//        // no surface residue
//        }
//
//
//
//    dm_residue = 0.0;
//    for (part=0; part < max_part; part++)
//     dm_residue = dm_residue + (dlt_dm_crop[part] * fraction_to_residue[part]);
//
//    n_residue = 0.0;
//    for (part=0; part < max_part; part++)
//      n_residue = n_residue + (dlt_dm_n[part] * fraction_to_residue[part]);
//
//    dm_root_residue = dlt_dm_crop[root] * fraction_to_residue[root];
//    n_root_residue = dlt_dm_n[root] * fraction_to_residue[root];
//
//    dm_tops_residue = dm_residue - dm_root_residue;
//    n_tops_residue = n_residue - n_root_residue;
//
    parent->writeString ("\nCrop biomass removed.");

    char  msgrmv[400];


    float dm_removed_tops = sum_real_array(dlt_dm_crop, max_part) - dlt_dm_crop[root];
    float dm_removed_root = dlt_dm_crop[root];

    float n_removed_tops = sum_real_array(dlt_dm_n, max_part) - dlt_dm_n[root];
    float n_removed_root = dlt_dm_n[root];

        parent->writeString ("    Organic matter removed from system:-      From Tops               From Roots");

        sprintf (msgrmv, "%48s%7.2f%24.2f", "DM (kg/ha) =               ", dm_removed_tops, dm_removed_root);
        parent->writeString (msgrmv);

        sprintf (msgrmv, "%48s%7.2f%24.2f", "N  (kg/ha) =               ", n_removed_tops, n_removed_root);
        parent->writeString (msgrmv);

        parent->writeString (" ");
//
//    // put roots into root residue
//
//    // call plant_root_incorp (dm_root, n_root)

    // Initialise plant leaf area

//    g.lai = c.initial_tpla * smm2sm * g.plants;
//    g.slai = 0.0;
//    g.tlai_dead = 0.0;

    float dlt_lai = g.lai * chop_fr_green[leaf];
    float dlt_slai = g.slai * chop_fr_sen[leaf];
    float dlt_tlai_dead = g.tlai_dead * chop_fr_dead[leaf];

    g.lai = g.lai - dlt_lai;
    g.slai = g.slai - dlt_slai;
    g.tlai_dead = g.tlai_dead - dlt_tlai_dead;

    float dlt_lai_tot = dlt_lai + dlt_slai;

     plant_leaf_detachment (g.leaf_area
                            , dlt_lai_tot
                            , g.plants, max_node);


    g.canopy_width = g.canopy_width * (1.0 - chop_fr_green[stem]);

    if (p.num_canopy_widths > 0)
        {
        legnew_canopy_fac (g.row_spacing
                           , g.plants
                           , g.skip_row_fac
                           , g.skip_plant_fac
                           , g.canopy_width
                           , &canopy_fac);
        }
    else
        {
        canopy_fac = g.skip_row_fac;
        }

// now update new canopy covers

    legnew_cover_leaf_pod(g.row_spacing
                          ,c.x_row_spacing
                          ,c.y_extinct_coef
                          ,c.num_row_spacing
                          ,c.extinct_coef_pod
                          , canopy_fac
                          ,g.lai
                          ,g.pai
                          ,&g.lai_canopy_green
                          ,&g.cover_green
                          ,&g.cover_pod);

    legnew_cover (g.row_spacing
                  ,c.x_row_spacing
                  ,c.y_extinct_coef_dead
                  ,c.num_row_spacing
                  , canopy_fac
                  ,g.slai
                  ,&g.cover_sen);
    legnew_cover (g.row_spacing
                  ,c.x_row_spacing
                  ,c.y_extinct_coef_dead
                  ,c.num_row_spacing
                  , canopy_fac
                  ,g.tlai_dead
                  ,&g.cover_dead);

//    fill_real_array (g.leaf_no, 0.0, max_node);
//    fill_real_array (g.leaf_no_dead, 0.0, max_node);
//    fill_real_array (g.node_no, 0.0, max_stage);
//    fill_real_array (g.leaf_area, 0.0, max_node);
//
//    stage_no_current = (int) g.current_stage;
//
//    g.node_no[stage_no_current-1] = c.leaf_no_at_emerg;
//
//    fill_real_array (g.leaf_no, 1.0, (int)c.leaf_no_at_emerg);
//    g.leaf_no[(int)c.leaf_no_at_emerg] = fmod(c.leaf_no_at_emerg,1.0);
//
//    avg_leaf_area = divide(c.initial_tpla,c.leaf_no_at_emerg,0.0);
//
//    fill_real_array (g.leaf_area,avg_leaf_area,(int)c.leaf_no_at_emerg);
//    g.leaf_area[(int)c.leaf_no_at_emerg] = fmod(c.leaf_no_at_emerg,1.0) * avg_leaf_area;
//
    //cnh the following has not been changed though may not be necessary;
    //cnh start;
    stage_no_current = (int) g.current_stage;
    stage_no_previous = (int) g.previous_stage;
    part_current = fmod(g.current_stage, 1.0);
    part_previous = fmod(g.previous_stage, 1.0);

    if (stage_no_current==stage_no_previous)
        {
        fract = divide (part_current, part_previous, 0.0);
        }
    else
        {
        fract = part_current;
        for (stage_no = stage_no_current+1; stage_no<=stage_no_previous;stage_no++)
            {
            g.days_tot[stage_no-1]       = 0.0;
            g.tt_tot[stage_no-1]         = 0.0;
            g.heat_stress_tt[stage_no-1] = 0.0;
            g.dm_stress_max[stage_no-1]  = 0.0;
            g.cswd_photo[stage_no-1]     = 0.0;
            g.cswd_expansion[stage_no-1] = 0.0;
            g.cnd_photo[stage_no-1]      = 0.0;
            g.cnd_grain_conc[stage_no-1] = 0.0;
            }
        }
    g.days_tot[stage_no_current-1]       = fract * g.days_tot[stage_no_current-1];
    g.tt_tot[stage_no_current-1]         = fract * g.tt_tot[stage_no_current-1];
    g.heat_stress_tt[stage_no_current-1] = 0.0;
    g.dm_stress_max[stage_no_current-1]  = 0.0;
    g.cswd_photo[stage_no_current-1]     = 0.0;
    g.cswd_expansion[stage_no_current-1] = 0.0;
    g.cnd_photo[stage_no_current-1]      = 0.0;
    g.cnd_grain_conc[stage_no_current-1] = 0.0;

// other plant states

    g.canopy_height = g.canopy_height * (1.0 - chop_fr_green[stem]);

    plant_n_conc_limits (c.stage_code_list
    , g.phase_tt
    , g.tt_tot
    , c.n_conc_crit_grain
    , c.n_conc_crit_root
    , c.n_conc_max_grain
    , c.n_conc_max_root
    , c.n_conc_min_grain
    , c.n_conc_min_root
    , c.x_stage_code
    , c.y_n_conc_crit_leaf
    , c.y_n_conc_crit_pod
    , c.y_n_conc_crit_stem
    , c.y_n_conc_max_leaf
    , c.y_n_conc_max_pod
    , c.y_n_conc_max_stem
    , c.y_n_conc_min_leaf
    , c.y_n_conc_min_pod
    , c.y_n_conc_min_stem
    , c.x_co2_nconc_modifier
    , c.y_co2_nconc_modifier
    , c.num_co2_nconc_modifier
    , g.co2
    , g.current_stage
    , g.n_conc_crit
    , g.n_conc_max
    , g.n_conc_min
    )  ;                                          // plant N concentr

    if (g.plant_status == alive &&
        g.current_stage < g.previous_stage)
        {
        plant_event (
          g.current_stage
        , g.dlayer
        , g.dm_dead
        , g.dm_green
        , g.dm_senesced
        , g.lai
        , g.n_green
        , g.root_depth
        , g.sw_dep
        , p.ll_dep);
        }
    else
        {
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Zero crop variables & arrays

//+  Mission Statement
//     Zero all the global variables and arrays

//+  Changes
//     060495 nih taken from template

void Plant::plant_zero_all_globals (void)
    {
//+  Constant Values
    const char*  my_name = "plant_zero_all_globals" ;

//- Implementation Section ----------------------------------

    push_routine (my_name);

#if 0
  memset (&g, 0xdeadbeef, sizeof(g));
  memset (&p, 0xdeadbeef, sizeof(p));
  memset (&c, 0xdeadbeef, sizeof(c)); //not for <x>_dm_sen_frac
#endif
      g.hasreadconstants = false;
      g.module_name="";
      g.crop_class="";
      g.plant_status=out;
      g.cultivar="";
      g.pre_dormancy_crop_class="";
      g.swdef_expansion=1.0;
      g.swdef_photo=1.0;
      g.swdef_pheno=1.0;
      g.swdef_fixation=1.0;
      g.sw_avail_fac_deepest_layer=0;
      g.nfact_expansion=1.0;
      g.nfact_photo=1.0;
      g.nfact_grain_conc=1.0;
      g.nfact_pheno=1.0;
      g.temp_stress_photo=1.0;
      g.oxdef_photo=1.0;
      g.row_spacing=0;
      g.skip_row=0;
      g.skip_plant=0;
      g.skip_row_fac=0;
      g.skip_plant_fac=0;
      g.sowing_depth=0;
      g.year=0;
      g.day_of_year=0;
      g.fr_intc_radn=0;
      g.latitude=0;
      g.radn=0;
      g.mint=0;
      g.maxt=0;
      fill_real_array (g.soil_temp, 0, 366+1);
      g.eo=0;
      fill_real_array (g.cnd_photo , 0, max_stage);
      fill_real_array (g.cnd_grain_conc , 0, max_stage);
      fill_real_array (g.cswd_photo , 0, max_stage);
      fill_real_array (g.cswd_expansion , 0, max_stage);
      fill_real_array (g.cswd_pheno , 0, max_stage);
      g.cum_vernal_days=0;
      g.dlt_tt=0;
      fill_real_array (g.tt_tot, 0, max_stage);
      fill_real_array (g.phase_tt, 0, max_stage);
      g.dlt_tt_curv=0;
      fill_real_array (g.tt_curv_tot, 0, max_stage);
      fill_real_array (g.phase_tt_curv, 0, max_stage);
      g.dlt_tt_other=0;
      fill_real_array (g.tt_other_tot, 0, max_stage);
      fill_real_array (g.phase_tt_other, 0, max_stage);
      fill_real_array (g.heat_stress_tt, 0, max_stage);
      g.dlt_heat_stress_tt=0;
      g.dlt_stage=0;
      g.current_stage=0;
      g.previous_stage=0;
      fill_real_array (g.days_tot , 0, max_stage);
      g.das=0;
      g.dlt_canopy_height=0;
      g.canopy_height=0;
      g.dlt_canopy_width=0;
      g.canopy_width=0;
      g.phase_devel=0;
      g.plants=0;
      g.dlt_plants=0;
      g.grain_no=0;
      g.dlt_root_depth=0;
      g.root_depth=0;
      g.cover_green=0;
      g.cover_sen=0;
      g.cover_dead=0;
      g.dlt_plants_death_seedling=0;
      g.dlt_plants_death_drought=0;
      g.dlt_plants_failure_phen_delay=0;
      g.dlt_plants_failure_leaf_sen=0;
      g.dlt_plants_failure_emergence=0;
      g.dlt_plants_failure_germ=0;
      g.dlt_plants_death_external=0;
      g.dlt_dm=0;
      g.dlt_dm_pot_rue=0;
      g.dlt_dm_pot_te=0;
      g.dlt_dm_oil_conv=0;
      g.dlt_dm_parasite = 0.0;
      fill_real_array (g.dlt_dm_green, 0, max_part);
      fill_real_array (g.dlt_dm_senesced, 0, max_part);
      fill_real_array (g.dlt_dm_detached, 0, max_part);
      fill_real_array (g.dlt_dm_dead, 0, max_part);
      fill_real_array (g.dlt_dm_dead_detached, 0, max_part);
      g.dlt_dm_oil_conv_retranslocate=0;
      fill_real_array (g.dlt_dm_green_retrans, 0, max_part);
      fill_real_array (g.dm_stress_max, 0, max_stage);
      g.dlt_dm_stress_max=0.0;
      g.dlt_dm_grain_demand=0.0;
      g.dlt_dm_parasite_demand = 0.0;
      g.dlt_sw_parasite_demand = 0.0;
      fill_real_array (g.dm_green_demand, 0, max_part);
      fill_real_array (g.dm_dead, 0, max_part);
      fill_real_array (g.dm_green, 0, max_part);
      fill_real_array (g.dm_senesced, 0, max_part);
      fill_real_array (g.dm_plant_top_tot, 0, max_stage);
      g.radn_int=0;
      g.transp_eff=0;
      g.slai=0;
      g.dlt_slai=0;
      g.dlt_lai=0;
      g.dlt_lai_pot=0;
      g.dlt_lai_stressed=0;
      g.lai=0;
      g.lai_canopy_green=0;
      g.tlai_dead=0;
      g.dlt_slai_detached=0;
      g.dlt_tlai_dead=0;
      g.dlt_tlai_dead_detached=0;
      g.dlt_slai_age=0;
      g.dlt_slai_light=0;
      g.dlt_slai_water=0;
      g.dlt_slai_frost=0;
      g.pai=0;
      g.dlt_pai=0;
      fill_real_array (g.leaf_no, 0, max_node);
      fill_real_array (g.node_no, 0, max_stage);
      fill_real_array (g.leaf_no_dead, 0, max_node);
      g.dlt_leaf_no=0;
      g.dlt_node_no=0;
      g.dlt_leaf_no_pot=0;
      g.dlt_node_no_pot=0;
      g.dlt_leaf_no_dead=0;
      g.leaf_no_final=0;
      fill_real_array (g.leaf_area, 0, max_node);
      fill_real_array (g.lai_equilib_light, 0, 366+1);
      fill_real_array (g.lai_equilib_water, 0, 366+1);
      fill_real_array (g.n_demand , 0, max_part);
      fill_real_array (g.n_max , 0, max_part);
      fill_real_array (g.dlt_n_green, 0, max_part);
      fill_real_array (g.dlt_n_senesced, 0, max_part);
      fill_real_array (g.dlt_n_senesced_trans, 0, max_part);
      fill_real_array (g.dlt_n_senesced_retrans, 0, max_part);
      fill_real_array (g.dlt_n_detached, 0, max_part);
      fill_real_array (g.dlt_n_dead, 0, max_part);
      fill_real_array (g.dlt_n_dead_detached, 0, max_part);
      fill_real_array (g.n_dead, 0, max_part);
      fill_real_array (g.n_green, 0, max_part);
      fill_real_array (g.n_senesced, 0, max_part);
      fill_real_array (g.dlt_n_retrans, 0, max_part);
      fill_real_array (g.dlt_no3gsm, 0, max_layer);
      fill_real_array (g.dlt_nh4gsm, 0, max_layer);
      fill_real_array (g.no3gsm , 0, max_layer);
      fill_real_array (g.no3gsm_min, 0, max_layer);
      fill_real_array (g.nh4gsm , 0, max_layer);
      fill_real_array (g.nh4gsm_min, 0, max_layer);

      fill_real_array (g.no3gsm_diffn_pot, 0, max_layer);
      fill_real_array (g.no3gsm_mflow_avail, 0, max_layer);
      fill_real_array (g.soil_n_demand, 0, max_part);
      g.grain_n_demand = 0.0;
      g.n_fix_pot=0;
      fill_real_array (g.no3gsm_uptake_pot, 0, max_layer);
      fill_real_array (g.nh4gsm_uptake_pot, 0, max_layer);
      g.n_fix_uptake=0;
      g.n_fixed_tops=0;
      fill_real_array (g.n_conc_crit, 0, max_part);
      fill_real_array (g.n_conc_max, 0, max_part);
      fill_real_array (g.n_conc_min, 0, max_part);
      fill_real_array (g.dm_plant_min, 0, max_part);
      g.cover_pod=0;
      fill_real_array (g.dlayer , 0, max_layer);
      fill_real_array (g.dlt_sw_dep, 0, max_layer);
      fill_real_array (g.ll15_dep, 0, max_layer);
      fill_real_array (g.dul_dep , 0, max_layer);
      fill_real_array (g.sat_dep, 0, max_layer);
      fill_real_array (g.bd, 0, max_layer);
      fill_real_array (g.sw_dep , 0, max_layer);
      g.sw_demand=0;
      g.sw_demand_te=0;
      fill_real_array (g.sw_avail_pot, 0, max_layer);
      fill_real_array (g.sw_avail, 0, max_layer);
      fill_real_array (g.sw_supply , 0, max_layer);

      g.num_layers=0;
      g.transpiration_tot=0;
      g.n_uptake_tot=0;
      g.n_demand_tot=0;
      g.n_conc_act_stover_tot=0;
      g.n_conc_crit_stover_tot=0;
      g.n_uptake_grain_tot=0;
      g.n_uptake_stover_tot=0;
      g.lai_max=0;
      g.flowering_date=0;
      g.maturity_date=0;
      g.flowering_das=0;
      g.maturity_das=0;
      fill_real_array (g.root_length, 0, max_layer);
      fill_real_array (g.root_length_dead, 0, max_layer);
      fill_real_array (g.dlt_root_length_dead, 0, max_layer);
      fill_real_array (g.dlt_root_length, 0, max_layer);
      fill_real_array (g.dlt_root_length_senesced, 0, max_layer);
      g.ext_n_demand=0;
      g.ext_sw_demand=0;
      g.grain_energy=0;
      g.dlt_cumvd=0;
      g.cumvd=0;
      g.vern_eff=0;
      g.photop_eff=0;
      g.leaves_per_node=0;

      p.grains_per_gram_stem=0;
      p.potential_grain_filling_rate=0;
      p.tt_maturity_to_ripe=0;
      p.tt_end_grain_to_maturity=0;
      fill_real_array (p.cum_vernal_days, 0, max_table);
      fill_real_array (p.tt_emerg_to_endjuv, 0, max_table);
      p.num_cum_vernal_days=0;
      p.tt_flower_to_maturity=0;
      fill_real_array (p.x_pp_endjuv_to_init, 0, max_table);
      fill_real_array (p.y_tt_endjuv_to_init, 0, max_table);
      fill_real_array (p.x_pp_init_to_flower, 0, max_table);
      fill_real_array (p.y_tt_init_to_flower, 0, max_table);
      fill_real_array (p.x_pp_flower_to_start_grain, 0, max_table);
      fill_real_array (p.y_tt_flower_to_start_grain, 0, max_table);
      fill_real_array (p.x_pp_start_to_end_grain, 0, max_table);
      fill_real_array (p.y_tt_start_to_end_grain, 0, max_table);

      p.num_pp_endjuv_to_init=0;
      p.num_pp_init_to_flower=0;
      p.num_pp_flower_to_start_grain=0;
      p.num_pp_start_to_end_grain=0;

      p.est_days_emerg_to_init=0;
      fill_real_array (p.x_pp_hi_incr, 0, max_table);
      fill_real_array (p.y_hi_incr, 0, max_table);
      p.num_pp_hi_incr=0;
      p.num_hi_max_pot=0;
      fill_real_array (p.x_hi_max_pot_stress, 0, max_table);
      fill_real_array (p.y_hi_max_pot, 0, max_table);
      fill_real_array (p.kl, 0, max_layer);
      fill_real_array (p.ll_dep, 0, max_layer);

      fill_real_array (p.x_stem_wt, 0, max_table);
      fill_real_array (p.y_height , 0, max_table);
      fill_real_array (p.y_width , 0, max_table);
      p.num_stem_wt=0;
      p.num_canopy_widths=0;
      fill_real_array (p.xf, 0, max_layer);
      p.uptake_source = "";
      p.eo_crop_factor=0;
      p.startgf_to_mat=0;
      p.phyllochron=0;
      p.vern_sens=0;
      p.photop_sens=0;

      //       plant Constants
	   c.grain_fill_option=0;
      c.n_uptake_option=0;
      c.phenology_option=0;
      c.leaf_no_pot_option=0;
      c.partition_option=0;
      c.grain_no_option=0;

      c.sen_start_stage=0;
      fill_real_array (c.x_temp_grainfill, 0, max_table);
      fill_real_array (c.y_rel_grainfill, 0, max_table);
      c.num_temp_grainfill=0;

      c.no3_uptake_max=0;
      c.no3_conc_half_max=0;

      c.crop_type="";
      c.default_crop_class="";
      c.stage_names.clear();
      ///////////c.part_names.empty(); in constructor!
      c.n_supply_preference="";
      fill_real_array (c.x_sw_ratio , 0, max_table);
      fill_real_array (c.y_sw_fac_root , 0, max_table);
      fill_real_array (c.x_ws_root , 0, max_table);
      fill_real_array (c.y_ws_root_fac , 0, max_table);
      fill_real_array (c.x_sw_demand_ratio , 0, max_table);
      fill_real_array (c.y_swdef_leaf , 0, max_table);
      fill_real_array (c.x_sw_avail_ratio , 0, max_table);
      fill_real_array (c.y_swdef_pheno , 0, max_table);
      fill_real_array (c.x_sw_avail_fix , 0, max_table);
      fill_real_array (c.y_swdef_fix , 0, max_table);
      fill_real_array (c.oxdef_photo , 0, max_table);
      fill_real_array (c.oxdef_photo_rtfr, 0, max_table);
      c.num_oxdef_photo=0;
      c.num_sw_ratio=0;
      c.num_ws_root=0;
      c.num_sw_demand_ratio=0;
      c.num_sw_avail_ratio=0;
      c.num_sw_avail_fix=0;
      fill_real_array (c.stage_code_list, 0, max_stage);
      c.twilight=0;


      fill_real_array (c.x_lai_ratio, 0, max_table);

      fill_real_array (c.y_leaf_no_frac, 0, max_table);
      c.num_lai_ratio=0;
      c.n_conc_crit_grain=0;
      c.n_conc_max_grain=0;
      c.n_conc_min_grain=0;
      c.n_conc_crit_root=0;
      c.n_conc_max_root=0;
      c.n_conc_min_root=0;
      fill_real_array (c.x_stage_code, 0, max_stage);
      fill_real_array (c.y_n_conc_crit_leaf, 0, max_stage);
      fill_real_array (c.y_n_conc_max_leaf, 0, max_stage);
      fill_real_array (c.y_n_conc_min_leaf, 0, max_stage);
      fill_real_array (c.y_n_conc_crit_stem, 0, max_stage);
      fill_real_array (c.y_n_conc_max_stem, 0, max_stage);
      fill_real_array (c.y_n_conc_min_stem, 0, max_stage);
      fill_real_array (c.y_n_conc_crit_pod, 0, max_stage);
      fill_real_array (c.y_n_conc_max_pod, 0, max_stage);
      fill_real_array (c.y_n_conc_min_pod, 0, max_stage);
      c.n_fact_photo=0;
      c.n_fact_pheno=0;
      c.n_fact_expansion=0;
      fill_real_array (c.n_init_conc, 0, max_part);
      fill_real_array (c.n_sen_conc, 0, max_part);
      c.num_n_conc_stage=0;
      fill_real_array (c.x_row_spacing, 0, max_table);
      fill_real_array (c.y_extinct_coef, 0, max_table);
      fill_real_array (c.y_extinct_coef_dead, 0, max_table);
      fill_real_array (c.x_stage_rue, 0, max_stage);
      fill_real_array (c.y_rue, 0, max_stage);
      fill_real_array (c.stage_stem_reduction_harvest, 0, max_stage);
      fill_real_array (c.stage_stem_reduction_kill_stem, 0, max_stage);
      fill_real_array (c.root_depth_rate, 0, max_stage);
      c.extinct_coef_pod=0;
      c.spec_pod_area=0;
      c.rue_pod=0;
      c.num_row_spacing=0;
      c.leaf_no_crit=0;
      c.tt_emerg_limit=0;
      c.days_germ_limit=0;
      c.swdf_pheno_limit=0;
      c.swdf_photo_limit=0;
      c.swdf_photo_rate=0;
      c.initial_root_depth=0;
      fill_real_array (c.x_lai , 0, max_table);
      fill_real_array (c.y_sla_max, 0, max_table);
      c.sla_min=0;
      c.initial_tpla=0;
      c.min_tpla=0;
      c.svp_fract=0;
      fill_real_array (c.transp_eff_cf, 0, max_stage);
      c.num_lai=0;
      c.pesw_germ=0;
      c.grain_n_conc_min=0;
      c.seed_wt_min=0;
      c.leaf_no_at_emerg=0;
      fill_real_array (c.fasw_emerg, 0, max_table);
      fill_real_array (c.rel_emerg_rate, 0, max_table);
      c.num_fasw_emerg=0;
      c.no3_diffn_const=0;
      fill_real_array (c.n_fix_rate, 0,max_stage);
      c.shoot_lag=0;
      c.shoot_rate=0;
      fill_real_array (c.x_node_no_app, 0, max_table);
      fill_real_array (c.y_node_app_rate, 0, max_table);
      fill_real_array (c.x_node_no_leaf, 0, max_table);
      fill_real_array (c.y_leaves_per_node, 0, max_table);
      fill_real_array (c.dm_init, 0, max_part);
      c.leaf_init_rate=0;
      c.leaf_no_seed=0;
      ////c.x_dm_sen_frac=NULL; done in constructor
      ////c.y_dm_sen_frac=NULL;
      ////c.num_dm_sen_frac=NULL;
      fill_real_array (c.dead_detach_frac,0,max_part);
      fill_real_array (c.sen_detach_frac,0,max_part);
      c.num_node_no_app=0;
      c.num_node_no_leaf=0;
      c.swdf_grain_min=0;
      c.hi_min=0;
      c.sfac_slope=0;
      c.tfac_slope=0;
      c.lai_sen_light=0;
      c.sw_fac_max=0;
      fill_real_array (c.x_temp_senescence, 0, max_table);
      fill_real_array (c.y_senescence_fac, 0, max_table);
      c.temp_fac_min=0;
      c.spla_slope=0;
      c.sen_threshold=0;
      c.sen_rate_water=0;
      c.sen_light_slope=0;
      c.num_temp_senescence=0;
      c.grn_water_cont=0;
      c.partition_rate_leaf=0;
      fill_real_array (c.frac_leaf,0,max_stage);
      fill_real_array (c.frac_pod,0,max_stage);
      fill_real_array (c.ratio_root_shoot, 0, max_table);
      fill_real_array (c.x_stage_no_partition, 0, max_table);
      fill_real_array (c.y_frac_leaf, 0, max_table);
      fill_real_array (c.y_frac_pod, 0, max_table);
      fill_real_array (c.y_ratio_root_shoot, 0, max_table);
      c.num_stage_no_partition=0;
      c.stem_trans_frac=0;
      c.leaf_trans_frac=0;
      c.pod_trans_frac=0;
      c.htstress_coeff=0;
      c.temp_grain_crit_stress=0;
      c.node_sen_rate=0;
      c.fr_lf_sen_rate=0;
      c.n_fact_lf_sen_rate = 0.0;
      c.carbo_oil_conv_ratio=0;
      c.grain_oil_conc=0;
      c.node_no_correction=0;
      fill_real_array (c.x_node_no, 0, max_table);
      fill_real_array (c.y_leaf_size, 0, max_table);
      c.num_node_no=0;
      fill_real_array (c.x_ave_temp, 0, max_table);
      fill_real_array (c.y_stress_photo, 0, max_table);
      fill_real_array (c.x_temp, 0, max_table);
      fill_real_array (c.y_tt, 0, max_table);
      fill_real_array (c.x_weighted_temp, 0, max_table);
      fill_real_array (c.y_plant_death, 0, max_table);
      fill_real_array (c.y_grain_rate, 0, max_table);
      c.num_temp=0;
      c.num_ave_temp=0;
      c.num_temp_grain=0;
      c.num_factors=0;
      c.num_temp_other=0;
      c.num_weighted_temp=0;
      c.tt_emerg_to_endjuv_ub=0;
      c.tt_maturity_to_ripe_ub=0;
      c.kl_ub=0;
      c.sw_dep_ub=0;
      c.sw_dep_lb=0;
      c.sw_ub=0;
      c.sw_lb=0;
      c.no3_ub=0;
      c.no3_lb=0;
      c.no3_min_ub=0;
      c.no3_min_lb=0;
      c.nh4_ub=0;
      c.nh4_lb=0;
      c.nh4_min_ub=0;
      c.nh4_min_lb=0;
      c.leaf_no_min=0;
      c.leaf_no_max=0;
      c.latitude_ub=0;
      c.latitude_lb=0;
      c.maxt_ub=0;
      c.maxt_lb=0;
      c.mint_ub=0;
      c.mint_lb=0;
      c.radn_ub=0;
      c.radn_lb=0;
      c.dlayer_ub=0;
      c.dlayer_lb=0;
      c.row_spacing_default=0;
      c.skip_row_default=0;
      c.skip_plant_default=0;
      fill_real_array (c.fr_height_cut , 0, max_table);
      fill_real_array (c.fr_stem_remain, 0, max_table);
      c.num_fr_height_cut=0;
      c.specific_root_length=0;
      c.root_die_back_fr=0;
      fill_real_array (c.x_plant_rld , 0, max_table);
      fill_real_array (c.y_rel_root_rate , 0, max_table);
      c.num_plant_rld=0;
      c.class_action.clear();
      c.class_change.clear();
      fill_real_array (c.x_vernal_temp, 0, max_table);
      fill_real_array (c.y_vernal_days, 0, max_table);
      c.num_vernal_temp=0;
      fill_real_array (c.x_temp_root_advance, 0, max_table);
      fill_real_array (c.y_rel_root_advance, 0, max_table);
      c.num_temp_root_advance=0;
      c.eo_crop_factor_default=0;

      // parasite
      g.dlt_dm_parasite_demand= 0.0;
      g.dlt_sw_parasite_demand = 0.0;
      g.dm_parasite_retranslocate = 0.0;
      g.dlt_dm_parasite = 0.0;

      // fruit cohorts
      g.setting_fruit = false;
      g.num_fruit_cohorts = 0;
      fill_real_array (g.previous_fruit_stage, 0.0, max_fruit_cohorts);
      fill_real_array (g.current_fruit_stage, 0.0, max_fruit_cohorts);
      fill_real_array (g.dlt_dm_fruit_grain_demand, 0.0, max_fruit_cohorts);
      fill_real_array (g.fruit_no, 0.0, max_fruit_cohorts);
      fill_real_array (g.dlt_dm_fruit_oil_conv, 0.0, max_fruit_cohorts);
      fill_real_array (g.dlt_dm_fruit_demand, 0.0, max_fruit_cohorts);
      g.node_no_first_flower= 0.0;
      fill_real_array (g.dlt_dm_daily, 0.0, 366);
      g.fruit_site_no = 0.0;
      g.dlt_fruit_flower_no= 0.0;
      g.dlt_fruit_site_no = 0.0;
      g.swdef_pheno_flower = 0.0;
      g.swdef_pheno_grainfill = 0.0;
      for (int i=0; i < max_fruit_cohorts; i++) {
         for (int j=0; j < max_fruit_stage; j++) {
           g.fruit_days_tot[i][j]= 0.0;
           g.fruit_phase_tt[i][j]= 0.0;
           g.fruit_tt_tot[i][j]= 0.0;
         }
         for (int j=0; j < max_part; j++) {
           g.dm_fruit_green[i][j]= 0.0;
           g.dlt_dm_fruit_green[i][j]= 0.0;
           g.dlt_dm_fruit_senesced[i][j] = 0.0;
           g.dlt_dm_fruit_abort[i][j] = 0.0;
           g.dm_fruit_dead[i][j] = 0.0;
           g.dm_fruit_senesced[i][j] = 0.0;
           g.dlt_dm_fruit_green_retrans[i][j]= 0.0;
         }
         for (int j = 0; j < 366; j++) {
           g.fruit_sdr_daily[i][j] = 0.0;
         }
      }

      fill_real_array(g.dlt_fruit_tt, 0.0, max_fruit_cohorts);
      fill_real_array(g.dlt_fruit_no, 0.0, max_fruit_cohorts);
      fill_real_array(g.dlt_fruit_stage, 0.0, max_fruit_cohorts);
      fill_real_array (g.fruit_phase_devel, 0.0, max_fruit_cohorts);

      fill_real_array (g.dlt_dm_fruit_oil_conv_retranslocate, 0.0, max_fruit_cohorts);
      fill_real_array (g.fruit_sdr, 0.0, max_fruit_cohorts);
      fill_real_array (g.dlt_fruit_no_abort, 0.0, max_fruit_cohorts);
      fill_real_array (g.dm_fruit_pod_min, 0.0, max_fruit_cohorts);
      fill_real_array (g.dlt_dm_green_abort, 0.0, max_part);

       // fruit cohorts
      fill_real_array (p.x_node_no_fruit_sites, 0.0,max_table);
      fill_real_array (p.y_fruit_sites_per_node, 0.0,max_table);
      p.dm_fruit_set_min= 0.0;
      p.num_node_no_fruit_sites = 0;
      fill_real_array (p.fruit_frac_pod, 0.0,max_stage);
      fill_real_array (p.x_pp_fruit_start_to_end_grain, 0.0 ,max_table);
      fill_real_array (p.y_tt_fruit_start_to_end_grain, 0.0 ,max_table);
      fill_real_array (p.x_stage_sdr_min, 0.0 ,max_table);
      fill_real_array (p.y_sdr_min, 0.0 ,max_table);
      p.num_sdr_min = 0;

      p.num_pp_fruit_start_to_end_grain = 0;
      fill_integer_array (p.fruit_stage_no_partition, 0,max_stage);

      p.num_fruit_stage_no_partition= 0;
      p.dm_fruit_max= 0.0;
      p.dm_fruit_set_crit = 0.0;
      p.potential_fruit_filling_rate= 0.0;
      p.cutout_fract = 0.0;

      fill_real_array (c.x_temp_fruit_site,0.0, max_table);
      fill_real_array (c.y_rel_fruit_site,0.0, max_table);
      c.num_temp_fruit_site  = 0;

      fill_real_array (c.x_sw_avail_ratio_flower, 0.0, max_table);
      fill_real_array (c.y_swdef_pheno_flower, 0.0, max_table);
      c.num_sw_avail_ratio_flower = 0;

      fill_real_array (c.x_sw_avail_ratio_grainfill, 0.0, max_table);
      fill_real_array (c.y_swdef_pheno_grainfill, 0.0, max_table);
      c.num_sw_avail_ratio_grainfill = 0;

      p.root_distribution_pattern = 0.0;

      c.fruit_no_option = 0;
      c.days_assimilate_ave = 0;
      c.dm_abort_fract = 0;
      c.fruit_phen_end = 0;
      c.tt_flower_to_start_pod= 0.0;
      c.fract_dm_fruit_abort_crit = 0.0;
      c.root_growth_option= 0;

      c.swdef_pheno_flower = 0.0;
      c.swdef_pheno_grainfill = 0.0;

      fill_real_array (c.x_co2_te_modifier, 0.0, max_table);
      fill_real_array (c.y_co2_te_modifier, 0.0, max_table);
      c.num_co2_te_modifier=0;

      fill_real_array (c.x_co2_nconc_modifier, 0.0, max_table);
      fill_real_array (c.y_co2_nconc_modifier, 0.0, max_table);
      c.num_co2_nconc_modifier=0;

      c.photosynthetic_pathway = pw_UNDEF;

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Zero crop variables & arrays

//+  Mission Statement
//     Set the crop variables and arrays to zero

//+  Changes
//     010994 jngh specified and programmed
//     090695 psc  add row spacing = 0
void Plant::plant_zero_variables (void)
    {
//+  Constant Values
    const char*  my_name = "plant_zero_variables" ;

//- Implementation Section ----------------------------------

    push_routine (my_name);

// zero pools etc.

    plant_zero_daily_variables ();

    //c      call fill_char_array (c.part_names, " ", max_part);

    fill_real_array (g.cnd_grain_conc , 0.0, max_stage);
    fill_real_array (g.cnd_photo , 0.0, max_stage);
    fill_real_array (g.cswd_expansion , 0.0, max_stage);
    fill_real_array (g.cswd_pheno , 0.0, max_stage);
    fill_real_array (g.cswd_photo , 0.0, max_stage);

    fill_real_array (g.leaf_no , 0.0, max_node);
    fill_real_array (g.node_no , 0.0, max_stage);
    fill_real_array (g.leaf_no_dead , 0.0, max_node);

    fill_real_array (g.days_tot , 0.0, max_stage);
    fill_real_array (g.phase_tt , 0.0, max_stage);
    fill_real_array (g.heat_stress_tt , 0.0, max_stage);
    fill_real_array (g.tt_tot , 0.0, max_stage);
    fill_real_array (g.phase_tt_curv , 0.0, max_stage);
    fill_real_array (g.tt_curv_tot , 0.0, max_stage);
    fill_real_array (g.phase_tt_other , 0.0, max_stage);
    fill_real_array (g.tt_other_tot , 0.0, max_stage);

    fill_real_array (g.dm_plant_top_tot, 0.0, max_stage);
    fill_real_array (g.dm_stress_max , 0.0, max_stage);

    fill_real_array (g.lai_equilib_light, 0.0, 366);
    fill_real_array (g.lai_equilib_water, 0.0, 366);
    fill_real_array (g.soil_temp , 0.0, 366);

    fill_real_array (g.dm_green , 0.0, max_part);
    fill_real_array (g.dm_dead , 0.0, max_part);
    fill_real_array (g.dm_plant_min , 0.0, max_part);
    fill_real_array (g.dm_senesced , 0.0, max_part);

    fill_real_array (g.n_conc_crit , 0.0, max_part);
    fill_real_array (g.n_conc_min , 0.0, max_part);
    fill_real_array (g.n_green , 0.0, max_part);
    fill_real_array (g.n_dead , 0.0, max_part);
    fill_real_array (g.n_senesced , 0.0, max_part);
    fill_real_array (g.dlt_n_senesced_trans , 0.0, max_part);
    fill_real_array (g.dlt_n_senesced_retrans , 0.0, max_part);

    fill_real_array (g.leaf_area , 0.0, max_node);
//    fill_real_array (p.ll_dep , 0.0, max_layer);
    fill_real_array (p.xf , 0.0, max_layer);
    fill_real_array (g.root_length , 0.0, max_layer);
    fill_real_array (g.root_length_dead, 0.0, max_layer);
    fill_real_array (g.dlt_root_length_dead, 0.0, max_layer);

    g.grain_no = 0.0;
    g.cumvd = 0.0;
    g.leaves_per_node = 0.0;

    g.dlt_plants_death_external     = 0.0;
    g.flowering_date        = 0;
    g.flowering_das         = 0;
    g.maturity_date         = 0;
    g.maturity_das          = 0;
    g.das                   = 0;
    g.lai_max               = 0.0;

    g.previous_stage        = 0.0;
    g.cum_vernal_days       = 0.0;
    g.plants                = 0.0;
    g.root_depth            = 0.0;
    g.canopy_height         = 0.0;
    g.canopy_width         = 0.0;
    g.grain_no              = 0.0;
    g.leaf_no_final         = 0.0;
    g.n_conc_act_stover_tot = 0.0;
    g.n_conc_crit_stover_tot = 0.0;
    g.n_demand_tot          = 0.0;
    g.n_uptake_grain_tot    = 0.0;
    g.n_uptake_stover_tot   = 0.0;
    g.n_uptake_tot          = 0.0;
    g.transpiration_tot     = 0.0;

    g.sowing_depth          = 0.0;
    g.skip_row              = 0.0;
    g.skip_row_fac          = 0.0;
    g.skip_plant            = 0.0;
    g.skip_plant_fac        = 0.0;
    g.row_spacing           = 0.0;

    g.cover_green           = 0.0;
    g.cover_pod             = 0.0;
    g.cover_sen             = 0.0;
    g.cover_dead            = 0.0;
    g.slai                  = 0.0;
    g.lai                   = 0.0;
    g.tlai_dead             = 0.0;
    g.pai                   = 0.0;

    g.swdef_pheno = 1.0;
    g.swdef_photo = 1.0;
    g.swdef_expansion = 1.0;
    g.swdef_fixation = 1.0;
    g.nfact_pheno = 1.0;
    g.nfact_photo = 1.0;
    g.nfact_grain_conc = 1.0;

    g.n_fix_pot = 0.0;
    g.n_fix_uptake = 0.0;
    g.n_fixed_tops = 0.0;

    // fruit cohort number
    g.setting_fruit               =  false;
    g.num_fruit_cohorts           = 0;
    g.node_no_first_flower        = 0.0;
    g.fruit_site_no               = 0.0;
    g.dm_parasite_retranslocate   = 0.0;

    for (int i = 0; i < max_fruit_cohorts; i++) {
      g.previous_fruit_stage[i]     = 0.0;
      g.current_fruit_stage[i]      = 0.0;
      g.fruit_no[i]                 = 0.0;
      g.fruit_sdr[i]                = 0.0;
      g.dm_fruit_pod_min[i]         = 0.0;
      g.fruit_phase_devel[i]        = 0.0;
      for (int j =0; j < max_fruit_stage; j++) {
         g.fruit_days_tot[i][j]        = 0.0;
         g.fruit_phase_tt[i][j]        = 0.0;
         g.fruit_tt_tot[i][j]          = 0.0;
      }
      for (int j =0; j < max_part; j++) {
         g.dm_fruit_green[i][j]        = 0.0;
         g.dm_fruit_dead[i][j]         = 0.0;
         g.dm_fruit_senesced[i][j]     = 0.0;
      }
    }
    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Zero crop daily variables & arrays

//+  Mission Statement
//     Set crop daily variables & arrays to zero

//+  Changes
//     010994 jngh specified and programmed
void Plant::plant_zero_daily_variables ()
    {
//+  Constant Values
    const char*  my_name = "plant_zero_daily_variables" ;

//- Implementation Section ----------------------------------

    push_routine (my_name);

// zero pools etc.

    fill_real_array (g.dlt_dm_green , 0.0, max_part);
    fill_real_array (g.dlt_dm_green_retrans, 0.0, max_part);
    fill_real_array (g.dlt_n_green , 0.0, max_part);
    fill_real_array (g.dlt_n_retrans , 0.0, max_part);
    fill_real_array (g.dlt_no3gsm , 0.0, max_layer);
    fill_real_array (g.dlt_nh4gsm , 0.0, max_layer);
    fill_real_array (g.dlt_sw_dep , 0.0, max_layer);
    fill_real_array (g.dm_green_demand , 0.0, max_part);
    fill_real_array (g.n_demand , 0.0, max_part);
    g.grain_n_demand = 0.0;
    fill_real_array (g.soil_n_demand , 0.0, max_part);

    fill_real_array (g.dlt_dm_dead_detached, 0.0, max_part);
    fill_real_array (g.dlt_dm_detached , 0.0, max_part);
    fill_real_array (g.dlt_dm_senesced , 0.0, max_part);
    fill_real_array (g.dlt_n_dead_detached, 0.0, max_part);
    fill_real_array (g.dlt_n_detached , 0.0, max_part);
    fill_real_array (g.dlt_n_senesced , 0.0, max_part);
    fill_real_array (g.dlt_n_senesced_trans , 0.0, max_part);
    fill_real_array (g.dlt_n_senesced_retrans , 0.0, max_part);

    fill_real_array (g.sw_avail , 0.0, max_layer);
    fill_real_array (g.sw_avail_pot , 0.0, max_layer);
    fill_real_array (g.sw_supply , 0.0, max_layer);

    fill_real_array (g.dlt_root_length , 0.0, max_layer);
    fill_real_array (g.dlt_root_length_senesced, 0.0, max_layer);

    fill_real_array (g.no3gsm_uptake_pot, 0.0, max_layer);
    fill_real_array (g.nh4gsm_uptake_pot, 0.0, max_layer);

    g.vern_eff = 0.0;
    g.dlt_cumvd = 0.0;
    g.photop_eff = 0.0;

    g.dlt_tlai_dead_detached   = 0.0;
    g.dlt_slai_detached        = 0.0;
    g.dlt_canopy_height        = 0.0;
    g.dlt_canopy_width        = 0.0;
    g.dlt_dm                   = 0.0;
    g.dlt_dm_grain_demand      = 0.0;
    g.dlt_dm_stress_max        = 0.0;
    g.dlt_heat_stress_tt       = 0.0;
    g.dlt_leaf_no              = 0.0;
//    g.dlt_node_no              = 0.0;JNGH - need to carry this through for site no next day.
    g.dlt_leaf_no_pot          = 0.0;
    g.dlt_node_no_pot          = 0.0;
    g.dlt_leaf_no_dead         = 0.0;
    g.dlt_plants               = 0.0;
    g.dlt_root_depth           = 0.0;
    g.dlt_slai                 = 0.0;
    g.dlt_stage                = 0.0;
    g.dlt_lai                  = 0.0;
    g.dlt_pai                  = 0.0;
    g.dlt_tt                   = 0.0;
    g.dlt_tt_curv              = 0.0;
    g.dlt_tt_other             = 0.0;
    g.sw_demand                = 0.0;
    g.sw_demand_te             = 0.0;
    g.ext_n_demand             = 0.0;

//      g%dlt_plants_death_barrenness     = 0.0
    g.dlt_plants_death_seedling       = 0.0;
    g.dlt_plants_death_drought        = 0.0;
    g.dlt_plants_failure_phen_delay   = 0.0;
    g.dlt_plants_failure_leaf_sen     = 0.0;
    g.dlt_plants_failure_emergence    = 0.0;
    g.dlt_plants_failure_germ         = 0.0;
//      g%dlt_plants_death_external       = 0.0

//      g%dlt_dm_pot_rue = 0.0
//      g%dlt_dm_pot_te  = 0.0
//      g%radn_int = 0.0

//      g%dlt_slai_age    = 0.0
//      g%dlt_slai_light  = 0.0
//      g%dlt_slai_water  = 0.0
//      g%dlt_slai_frost  = 0.0

      // fruit cohort number
    g.dlt_fruit_flower_no                            = 0.0;
//    g.dlt_fruit_site_no                       = 0.0;
    g.dm_parasite_retranslocate               = 0.0;
    for (int i = 0; i < max_fruit_cohorts; i++) {
      g.dlt_dm_fruit_grain_demand[i]            = 0.0;
      g.dlt_dm_fruit_oil_conv[i]                = 0.0;
      g.dlt_dm_fruit_demand[i]                  = 0.0;
      g.dlt_fruit_stage[i]                      = 0.0;
      g.dlt_dm_fruit_oil_conv_retranslocate[i]  = 0.0;
      g.dlt_fruit_no_abort[i]                   = 0.0;
      g.dlt_fruit_tt[i]                         = 0.0;
      g.dlt_fruit_no[i]                         = 0.0;
      for (int j = 0; j < max_part; j++) {
         g.dlt_dm_fruit_green[i][j]                = 0.0;
         g.dlt_dm_fruit_senesced[i][j]             = 0.0;
         g.dlt_dm_fruit_green_retrans[i][j]        = 0.0;
         g.dlt_dm_fruit_abort[i][j]                = 0.0;
      }
    }
    for (int j = 0; j < max_part; j++) {
      g.dlt_dm_green_abort[j]                   = 0.0;
    }
    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Crop initialisation

//+  Mission Statement
//     Crop initialisation

//+  Changes
//     010994 jngh specified and programmed
//     050599 sdb removed version reference
//     011099 dph added code to get type of module.
void Plant::plant_init (void)
    {
//+  Constant Values
    const char*  my_name = "plant_init" ;

//+  Calls

//- Implementation Section ----------------------------------

    push_routine (my_name);

    parent->writeString (" initialising");

    // initialize crop variables
    plant_get_site_characteristics();

    g.current_stage = (float)plant_end;
    g.plant_status = out;
    g.module_name = parent->getName();

    float ll[max_layer]; int num_layers;
    parent->readParameter ("parameters"
                         , "ll"/*, "()"*/
                         , ll, num_layers
                         , 0.0, 1.0);
    fill_real_array (p.ll_dep, 0.0, max_layer);
    for (int layer = 0; layer < num_layers; layer++)
       p.ll_dep[layer] = ll[layer]*g.dlayer[layer];

    phosphorus->doInit(parent, c.crop_type, c.part_names);

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Start crop using parameters specified in passed record

//+  Mission Statement
//     Start the crop using passed parameters

//+  Changes
//     010994 jngh specified and programmed
//     090695 psc  add row spacing read
//     220696 jngh changed extract to collect
void Plant::plant_start_crop (protocol::Variant &v/*(INPUT) message arguments*/)
    {

//+  Constant Values
    const char*  my_name = "plant_start_crop" ;

//+  Local Variables
    int   numvals;                                // number of values found in array
    char  msg[200];                             // output string
    FString  dummy;                               // dummy variable

//- Implementation Section ----------------------------------

    push_routine (my_name);

    if (g.plant_status == out)
        {
        protocol::ApsimVariant incomingApsimVariant(parent);
        incomingApsimVariant.aliasTo(v.getMessageData());
        parent->writeString ( "Crop Sow");

        // Check anachronisms
        if (incomingApsimVariant.get("crop_type", protocol::DTstring, false, dummy) != false)
            {
            parent->warningError ("crop type no longer used in sowing command");
            }

        // get species parameters
        if (incomingApsimVariant.get("crop_class", protocol::DTstring, false, dummy) == false)
            {
            // crop class was not specified
            g.crop_class = c.default_crop_class;
            }
        else
            {
            g.crop_class = dummy.f_str();
            g.crop_class = g.crop_class.substr(0,dummy.length());
            }
        plant_read_species_const ();

        // get cultivar parameters
        if (incomingApsimVariant.get("cultivar", protocol::DTstring, false, dummy) == false)
            {
            throw std::invalid_argument("Cultivar not specified");
            }
        else
            {
            g.cultivar = dummy.substr(0,dummy.length()).f_str();
            g.cultivar = g.cultivar.substr(0,dummy.length());
            }
        plant_read_cultivar_params ();

        // get root profile parameters
        plant_read_root_params ();

        // get other sowing criteria
        if (incomingApsimVariant.get("plants", protocol::DTsingle, false, g.plants) == false)
            {
            throw std::invalid_argument("plant density ('plants') not specified");
            }
        bound_check_real_var(parent,g.plants, 0.0, 1000.0, "plants");

        if (incomingApsimVariant.get("sowing_depth", protocol::DTsingle, false, g.sowing_depth) == false)
            {
            throw std::invalid_argument("sowing_depth not specified");
            }
        bound_check_real_var(parent,g.sowing_depth, 0.0, 100.0, "sowing_depth");

        if (incomingApsimVariant.get("row_spacing", protocol::DTsingle, false, g.row_spacing) == false)
            {
            g.row_spacing = c.row_spacing_default;
            }
        bound_check_real_var(parent,g.row_spacing, 0.0, 2000.0, "row_spacing");


        if (incomingApsimVariant.get("skipplant", protocol::DTsingle, false, g.skip_plant) == false)
            {
            g.skip_plant = c.skip_plant_default;
            }
        bound_check_real_var(parent,g.skip_plant, 0.0, 2.0, "skipplant");
        g.skip_plant_fac = (2.0 + g.skip_plant)/2.0;

        if (incomingApsimVariant.get("skiprow", protocol::DTsingle, false, g.skip_row) == false)
            {
            g.skip_row = c.skip_row_default;
            }
        bound_check_real_var(parent,g.skip_row, 0.0, 2.0, "skiprow");
        g.skip_row_fac = (2.0 + g.skip_row)/2.0;

        // Bang.
        g.current_stage = (float) sowing;
        g.plant_status = alive;
        sendStageMessage("sowing");

        parent->writeString ("");
        parent->writeString ("                 Crop Sowing Data");
        parent->writeString ("    ------------------------------------------------");
        parent->writeString ("    Sowing  Depth Plants Spacing Skip  Skip  Cultivar");
        parent->writeString ("    Day no   mm     m^2     mm   row   plant name");
        parent->writeString ("    ------------------------------------------------");

        sprintf(msg, "   %7d%7.1f%7.1f%7.1f%6.1f%6.1f %s"
               , g.day_of_year, g.sowing_depth
               , g.plants, g.row_spacing
               , g.skip_row, g.skip_plant, g.cultivar.c_str());
        parent->writeString (msg);

        parent->writeString ("    ------------------------------------------------\n");

        }
    else
        {
        string m = string(g.module_name + " is still in the ground -\n unable to sow until it is\n taken out by \"end_crop\" action.");
        throw std::runtime_error (m.c_str());
        }

    pop_routine (my_name);
    return;
    }


/////////////////////////////////////////////////////////////////
//+  Purpose
//       Get cultivar parameters for named cultivar, from crop parameter file.

//+  Mission Statement
//     Get cultivar parameters for named cultivar

//+  Changes
//       090994 jngh specified and programmed
//       270801 jngh changed hi_incr to pp effect
void Plant::plant_read_cultivar_params ()
    {
//+  Constant Values
    const char*  my_name = "plant_read_cultivar_params" ;

//+  Local Variables
    string s;
    char  msg[200];                             // output string
    int   numvals;                                // number of values read

//- Implementation Section ----------------------------------

    push_routine (my_name);

    parent->writeString (" - reading cultivar parameters");

// TEMPLATE OPTION
//       plant_dm_grain_hi
    if (c.fruit_no_option == 1)
       {
       parent->readParameter (g.cultivar.c_str()
       , "x_pp_hi_incr"/*,  "(h)"*/
       , p.x_pp_hi_incr
       , p.num_pp_hi_incr
       , 0.0, 24.0);

       parent->readParameter (g.cultivar.c_str()
       , "y_hi_incr"/*,  "()"*/
       , p.y_hi_incr
       , p.num_pp_hi_incr
       , 0.0, 1.0);

       parent->readParameter (g.cultivar.c_str()
       , "x_hi_max_pot_stress"/*,  "(0-1)"*/
       , p.x_hi_max_pot_stress, p.num_hi_max_pot
       , 0.0, 1.0);

       parent->readParameter (g.cultivar.c_str()
       , "y_hi_max_pot"//, "(0-1)"
       , p.y_hi_max_pot, p.num_hi_max_pot
       , 0.0, 1.0);
      }
    if (c.grain_no_option==2)
        {
        parent->readParameter (g.cultivar.c_str()
        , "grains_per_gram_stem"//, "(/g)"
        , p.grains_per_gram_stem
        , 0.0, 10000.0);
        }
    if (c.grain_fill_option==2)
        {
        parent->readParameter (g.cultivar.c_str()
        , "potential_grain_filling_rate"//, "(g/grain/day)"
        , p.potential_grain_filling_rate
        , 0.0, 1.0);
        }

//   plant_phenology_init

    if (c.phenology_option==1)
        {

        parent->readParameter (g.cultivar.c_str()
        , "cum_vernal_days"//, "(vd)"
        , p.cum_vernal_days, p.num_cum_vernal_days
        , 0.0, 100.0);

        parent->readParameter (g.cultivar.c_str()
        , "tt_emerg_to_endjuv"//, "(degday)"
        , p.tt_emerg_to_endjuv, p.num_cum_vernal_days
        , 0.0, c.tt_emerg_to_endjuv_ub);

        parent->readParameter (g.cultivar.c_str()
        , "est_days_emerg_to_init"//, "()"
        , p.est_days_emerg_to_init
        , 0, 100);

        parent->readParameter (g.cultivar.c_str()
        , "x_pp_endjuv_to_init"//, "(h)"
        , p.x_pp_endjuv_to_init
        , p.num_pp_endjuv_to_init
        , 0.0, 24.0);

        parent->readParameter (g.cultivar.c_str()
        , "y_tt_endjuv_to_init"//, "(degday)"
        , p.y_tt_endjuv_to_init
        , p.num_pp_endjuv_to_init
        , 0.0, 1e6);

        parent->readParameter (g.cultivar.c_str()
        , "x_pp_init_to_flower"//, "(h)"
        , p.x_pp_init_to_flower
        , p.num_pp_init_to_flower
        , 0.0, 24.0);

        parent->readParameter (g.cultivar.c_str()
        , "y_tt_init_to_flower"//, "(degday)"
        , p.y_tt_init_to_flower
        , p.num_pp_init_to_flower
        , 0.0, 1e6);

        parent->readParameter (g.cultivar.c_str()
        , "x_pp_flower_to_start_grain"
//        , "(h)"
        , p.x_pp_flower_to_start_grain
        , p.num_pp_flower_to_start_grain
        , 0.0, 24.0);

        parent->readParameter (g.cultivar.c_str()
        , "y_tt_flower_to_start_grain"
//        , "(degday)"
        , p.y_tt_flower_to_start_grain
        , p.num_pp_flower_to_start_grain
        , 0.0, 1e6);

        parent->readParameter (g.cultivar.c_str()
        , "x_pp_start_to_end_grain"
//        , "(h)"
        , p.x_pp_start_to_end_grain
        , p.num_pp_start_to_end_grain
        , 0.0, 24.0);

        parent->readParameter (g.cultivar.c_str()
        , "y_tt_start_to_end_grain"
//        , "(degday)"
        , p.y_tt_start_to_end_grain
        , p.num_pp_start_to_end_grain
        , 0.0, 1e6);

        parent->readParameter (g.cultivar.c_str()
        , "tt_end_grain_to_maturity"//, "()"
        , p.tt_end_grain_to_maturity
        , 0.0, 1e6);

        parent->readParameter (g.cultivar.c_str()
        , "tt_maturity_to_ripe"//, "()"
        , p.tt_maturity_to_ripe
        , 0.0, c.tt_maturity_to_ripe_ub);

// Nwheat Phenology
//=========================
        }
    else if (c.phenology_option==2)
        {
        parent->readParameter (g.cultivar.c_str()
        , "phyllochron"//, "()"
        , p.phyllochron
        , 0.0, 300.);

        parent->readParameter (g.cultivar.c_str()
        , "startgf_to_mat"//, "()"
        , p.startgf_to_mat
        , 0.0, 3000.);

        parent->readParameter (g.cultivar.c_str()
        , "vern_sens"//, "()"
        , p.vern_sens
        , 0.0, 10.0);

        parent->readParameter (g.cultivar.c_str()
        , "photop_sens"//, "()"
        , p.photop_sens
        , 0.0, 10.0);
        }

    // Fruit number Phenology
    //=========================
    else if (c.phenology_option == 3)
       {
         parent->readParameter (g.cultivar.c_str()
                       , "x_pp_fruit_start_to_end_grain"
//                       , "(h)"
                       , p.x_pp_fruit_start_to_end_grain
                       , p.num_pp_fruit_start_to_end_grain
                       , 0.0, 24.0);

         parent->readParameter (g.cultivar.c_str()
                       , "y_tt_fruit_start_to_end_grain"
//                       , "(degday)"
                       , p.y_tt_fruit_start_to_end_grain
                       , p.num_pp_fruit_start_to_end_grain
                       , 0.0, 1.0e6);


         parent->readParameter (g.cultivar.c_str()
                        , "tt_end_grain_to_maturity"//, "()"
                        , p.tt_end_grain_to_maturity
                        , 0.0, 1e6);

         parent->readParameter (g.cultivar.c_str()
                        , "tt_maturity_to_ripe"//, "()"
                        , p.tt_maturity_to_ripe
                        , 0.0, c.tt_maturity_to_ripe_ub);

         parent->readParameter (g.cultivar.c_str()
                    , "cum_vernal_days"//, "(vd)"
                    , p.cum_vernal_days, p.num_cum_vernal_days
                    , 0.0, 100.0);

         parent->readParameter (g.cultivar.c_str()
                    , "tt_emerg_to_endjuv"//, "(degday)"
                    , p.tt_emerg_to_endjuv, p.num_cum_vernal_days
                    , 0.0, c.tt_emerg_to_endjuv_ub);

         parent->readParameter (g.cultivar.c_str()
                     , "est_days_emerg_to_init"//, "()"
                     , p.est_days_emerg_to_init
                     , 0, 100);

         parent->readParameter (g.cultivar.c_str()
                    , "x_pp_endjuv_to_init"//, "(h)"
                    , p.x_pp_endjuv_to_init
                    , p.num_pp_endjuv_to_init
                    , 0.0, 24.0);

         parent->readParameter (g.cultivar.c_str()
                    , "y_tt_endjuv_to_init"//, "(degday)"
                    , p.y_tt_endjuv_to_init
                    , p.num_pp_endjuv_to_init
                    , 0.0, 1e6);

         parent->readParameter (g.cultivar.c_str()
                    , "x_pp_init_to_flower"//, "(h)"
                    , p.x_pp_init_to_flower
                    , p.num_pp_init_to_flower
                    , 0.0, 24.0);

         parent->readParameter (g.cultivar.c_str()
                    , "y_tt_init_to_flower"//, "(degday)"
                    , p.y_tt_init_to_flower
                    , p.num_pp_init_to_flower
                    , 0.0, 1e6);

         parent->readParameter (g.cultivar.c_str()
                    , "x_pp_flower_to_start_grain"
//                    , "(h)"
                    , p.x_pp_flower_to_start_grain
                    , p.num_pp_flower_to_start_grain
                    , 0.0, 24.0);

         parent->readParameter (g.cultivar.c_str()
                    , "y_tt_flower_to_start_grain"
//                    , "(degday)"
                    , p.y_tt_flower_to_start_grain
                    , p.num_pp_flower_to_start_grain
                    , 0.0, 1e6);

         parent->readParameter (g.cultivar.c_str()
                    , "x_pp_start_to_end_grain"
//                    , "(h)"
                    , p.x_pp_start_to_end_grain
                    , p.num_pp_start_to_end_grain
                    , 0.0, 24.0);

         parent->readParameter (g.cultivar.c_str()
                    , "y_tt_start_to_end_grain"
//                    , "(degday)"
                    , p.y_tt_start_to_end_grain
                    , p.num_pp_start_to_end_grain
                    , 0.0, 1e6);

         parent->readParameter (g.cultivar.c_str()
                     , "tt_end_grain_to_maturity"//, "()"
                     , p.tt_end_grain_to_maturity
                     , 0.0, 1e6);

         parent->readParameter (g.cultivar.c_str()
                     , "tt_maturity_to_ripe"//, "()"
                     , p.tt_maturity_to_ripe
                     , 0.0, c.tt_maturity_to_ripe_ub);
         }

     if (c.grain_fill_option==3)
         {
         parent->readParameter (g.cultivar.c_str()
                , "potential_fruit_filling_rate"//, "(g/fruit/degday)"
                , p.potential_fruit_filling_rate
                , 0.0, 1.0);
         }

      if (c.partition_option == 3)
         {
	      parent->readParameter (g.cultivar.c_str()
                     , "x_fruit_stage_no_partition"//, "()"
                     , p.fruit_stage_no_partition
                     , p.num_fruit_stage_no_partition
                     , 0, 20);

	      parent->readParameter (g.cultivar.c_str()
                     , "y_fruit_frac_pod"//, "()"
                     , p.fruit_frac_pod, numvals
                     , 0.0, 2.0);

         for (int stage = 0; stage < p.num_fruit_stage_no_partition; stage++)
            {
            c.y_frac_pod[p.fruit_stage_no_partition[stage]] =
                                            p.fruit_frac_pod[stage];
            }
         }

      if (c.fruit_no_option == 2)
         {
         parent->readParameter (g.cultivar.c_str()
                , "x_stage_supply_demand_ratio"//, "()"
                , p.x_stage_sdr_min, p.num_sdr_min
                , 1.0, max_stage);

         parent->readParameter (g.cultivar.c_str()
                , "y_supply_demand_ratio_min"//, "()"
                , p.y_sdr_min, p.num_sdr_min
                , 0.0, 1.0);

         parent->readParameter (g.cultivar.c_str()
                , "dm_fruit_max"//, "(g/fruit)"
                , p.dm_fruit_max
                , 0.0, 100.0);

         parent->readParameter (g.cultivar.c_str()
                , "potential_fruit_filling_rate"//, "(g/fruit/degday)"
                , p.potential_fruit_filling_rate
                , 0.0, 10.0);

         parent->readParameter (g.cultivar.c_str()
                , "cutout_fract"//, "(0-1)"
                , p.cutout_fract
                , 0.0, 10.0);

         parent->readParameter (g.cultivar.c_str()
               , "x_node_no_fruit_sites"//, "(sites/node)"
               , p.x_node_no_fruit_sites, p.num_node_no_fruit_sites
               , 0.0, 100.0);

         parent->readParameter (g.cultivar.c_str()
              , "y_fruit_sites_per_node"//, "(sites/node)"
              , p.y_fruit_sites_per_node, numvals
              , 0.0, 100.0);

         parent->readParameter (g.cultivar.c_str()
                , "dm_fruit_set_min"//, "(g/fruit/day)"
                , p.dm_fruit_set_min
                , 0.0, 10.0);

         parent->readParameter (g.cultivar.c_str()
                , "dm_fruit_set_crit"//, "(g/fruit/day)"
                , p.dm_fruit_set_crit
                , 0.0, 10.0);
        }

    parent->readParameter (g.cultivar.c_str()
    , "x_stem_wt"//, "(g/plant)"
    , p.x_stem_wt, p.num_stem_wt
    , 0.0, 1000.0);

    parent->readParameter (g.cultivar.c_str()
    , "y_height"//, "(mm)"
    , p.y_height, p.num_stem_wt
    , 0.0, 5000.0);

    parent->readParameter (g.cultivar.c_str()
    , "y_width"//, "(mm)"
    , p.y_width, p.num_canopy_widths
    , 0.0, 5000.0, true);
    if (p.num_canopy_widths ==0)
        {
        fill_real_array(p.y_width, 0.0, max_table);
        }

// report

    parent->writeString ("    ------------------------------------------------");

    sprintf (msg, "   %s%s",  "cultivar                   = ", g.cultivar.c_str());
    parent->writeString (msg);

    sprintf (msg, "   %s%8d", "est_days_emerg_to_init     = " , p.est_days_emerg_to_init);
    parent->writeString (msg);

    s = string("   cum_vernal_days            = ");
    for (int i = 0; i < p.num_cum_vernal_days; i++)
      {
      s = s + ftoa(p.cum_vernal_days[i], "10.2") + " ";
      }
    parent->writeString (s.c_str());

    s = string("   tt_emerg_to_endjuv         = ");
    for (int i = 0; i < p.num_cum_vernal_days; i++)
      {
      s = s + ftoa(p.tt_emerg_to_endjuv[i], "10.2") + " ";
      }
    parent->writeString (s.c_str());

    s = string("   x_pp_endjuv_to_init        = ");
    for (int i = 0; i < p.num_pp_endjuv_to_init; i++)
      {
      s = s + ftoa(p.x_pp_endjuv_to_init[i], "10.2") + " ";
      }
    parent->writeString (s.c_str());

    s = string("   y_tt_endjuv_to_init        = ");
    for (int i = 0; i < p.num_pp_endjuv_to_init; i++)
      {
      s = s + ftoa(p.y_tt_endjuv_to_init[i], "10.2") + " ";
      }
    parent->writeString (s.c_str());

    s = string("   x_pp_init_to_flower        = ");
    for (int i = 0; i < p.num_pp_init_to_flower; i++)
      {
      s = s + ftoa(p.x_pp_init_to_flower[i], "10.2") + " ";
      }
    parent->writeString (s.c_str());

    s = string("   y_tt_init_to_flower        = ");
    for (int i = 0; i < p.num_pp_init_to_flower; i++)
      {
      s = s + ftoa(p.y_tt_init_to_flower[i],"10.2") + " ";
      }
    parent->writeString (s.c_str());

    s = string("   x_pp_flower_to_start_grain = ");
    for (int i = 0; i < p.num_pp_flower_to_start_grain; i++)
      {
      s = s + ftoa(p.x_pp_flower_to_start_grain[i], "10.2") + " ";
      }
    parent->writeString (s.c_str());
    s = string("   y_tt_flower_to_start_grain = ");
    for (int i = 0; i < p.num_pp_flower_to_start_grain; i++)
      {
      s = s + ftoa(p.y_tt_flower_to_start_grain[i], "10.2") + " ";
      }
    parent->writeString (s.c_str());

    if (c.phenology_option == 3)
       {
       s = string("         Fruit No Phenology");
       parent->writeString (s.c_str());

       s = string("   x_pp_fruit_start_to_end_grain = ");
       for (int i = 0; i < p.num_pp_fruit_start_to_end_grain; i++)
         {
         s = s + ftoa(p.x_pp_fruit_start_to_end_grain[i], "10.2") + " ";
         }
       parent->writeString (s.c_str());

       s = string("   y_tt_fruit_start_to_end_grain = ");
       for (int i = 0; i < p.num_pp_fruit_start_to_end_grain; i++)
         {
         s = s + ftoa(p.y_tt_fruit_start_to_end_grain[i],"10.2") + " ";
         }
       parent->writeString (s.c_str());
       }
    else
      {
      s = string("   x_pp_start_to_end_grain    = ");
      for (int i = 0; i < p.num_pp_start_to_end_grain; i++)
        {
        s = s + ftoa(p.x_pp_start_to_end_grain[i], "10.2") + " ";
        }
      parent->writeString (s.c_str());

      s = string("   y_tt_start_to_end_grain    = ");
      for (int i = 0; i < p.num_pp_start_to_end_grain; i++)
        {
        s = s + ftoa(p.y_tt_start_to_end_grain[i], "10.2") + " ";
        }
      parent->writeString (s.c_str());
      }

    s = string("   tt_end_grain_to_maturity   = ");
    s = s + ftoa(p.tt_end_grain_to_maturity, "10.2");
    parent->writeString (s.c_str());

    s = string("   tt_maturity_to_ripe        = ");
    s = s + ftoa(p.tt_maturity_to_ripe, "10.2");
    parent->writeString (s.c_str());

    // TEMPLATE OPTION
    if (c.fruit_no_option == 1)
       {
       s = string("   x_pp_hi_incr               = ");
       for (int i = 0; i < p.num_pp_hi_incr; i++)
         {
         s = s + ftoa(p.x_pp_hi_incr[i], "10.2") + " ";
         }
       parent->writeString (s.c_str());

       s = string("   y_hi_incr                  = ");
       for (int i = 0; i < p.num_pp_hi_incr; i++)
         {
         s = s + ftoa(p.y_hi_incr[i], "10.4") + " ";
         }
       parent->writeString (s.c_str());

       s = string("   x_hi_max_pot_stress        = ");
       for (int i = 0; i < p.num_hi_max_pot; i++)
         {
         s = s + ftoa(p.x_hi_max_pot_stress[i], "10.2") + " ";
         }
       parent->writeString (s.c_str());

       s = string("   y_hi_max_pot               = ");
       for (int i = 0; i < p.num_hi_max_pot; i++)
         {
         s = s + ftoa(p.y_hi_max_pot[i], "10.2") + " ";
         }
       parent->writeString (s.c_str());
       }
     else if(c.fruit_no_option == 2)
       {
       s = string("         Fruit No Parameters");
       parent->writeString (s.c_str());

       s = string("   x_fruit_stage_no_partition   = ");
       for (int i = 0; i < p.num_fruit_stage_no_partition; i++)
         s = s + itoa(p.fruit_stage_no_partition[i],5) + " ";
       parent->writeString (s.c_str());

       s = string("   y_fruit_frac_pod             = ");
       for (int i = 0; i < p.num_fruit_stage_no_partition; i++)
         s = s + ftoa(p.fruit_frac_pod[i], "8.2") + " ";
       parent->writeString (s.c_str());

       s = string("   x_node_no_fruit_sites        = ");
       for (int i = 0; i < p.num_node_no_fruit_sites; i++)
         s = s + ftoa(p.x_node_no_fruit_sites[i], "8.2") + " ";
       parent->writeString (s.c_str());

       s = string("   y_fruit_sites_per_node       = ");
       for (int i = 0; i < p.num_node_no_fruit_sites; i++)
         s = s + ftoa(p.y_fruit_sites_per_node[i], "8.2") + " ";
       parent->writeString (s.c_str());

       s = string("   x_stage_supply_demand_ratio  = ");
       for (int i = 0; i < p.num_sdr_min; i++)
          s = s + ftoa(p.x_stage_sdr_min[i], "8.2");
       parent->writeString (s.c_str());

       s = string("   y_supply_demand_ratio_min    = ");
       for (int i = 0; i < p.num_sdr_min; i++)
          s = s + ftoa(p.y_sdr_min[i], "8.2");
       parent->writeString (s.c_str());

       s = string("   dm_fruit_max                 = ");
       s = s + ftoa(p.dm_fruit_max, "8.2");
       parent->writeString (s.c_str());

       s = string("   dm_fruit_set_min             = ");
       s = s + ftoa(p.dm_fruit_set_min, "8.2");
       parent->writeString (s.c_str());

       s = string("   dm_fruit_set_crit            = ");
       s = s + ftoa(p.dm_fruit_set_crit, "8.2");
       parent->writeString (s.c_str());

       s = string("   potential_fruit_filling_rate = ");
       s = s + ftoa(p.potential_fruit_filling_rate, "8.2");
       parent->writeString (s.c_str());

       s = string("   cutout_fract                 = ");
       s = s + ftoa(p.cutout_fract, "8.2");
       parent->writeString (s.c_str());
       }
    s = string("   x_stem_wt                  = ");
    for (int i = 0; i < p.num_stem_wt; i++)
      {
      s = s + ftoa(p.x_stem_wt[i], "10.2") + " ";
      }
    parent->writeString (s.c_str());

    s = string("   y_height                   = ");
    for (int i = 0; i < p.num_stem_wt; i++)
      {
      s = s + ftoa(p.y_height[i], "10.2") + " ";
      }
    parent->writeString (s.c_str());

    if (p.num_canopy_widths >0)
        {
        s = string("   y_width                   = ");
        for (int i = 0; i < p.num_canopy_widths; i++)
           {
           s = s + ftoa(p.y_width[i], "10.2") + " ";
           }
        parent->writeString (s.c_str());
        }

    parent->writeString ("    ------------------------------------------------\n\n");

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Get root profile parameters

//+  Mission Statement
//     Get root profile parameters

//+  Changes
//       090994 jngh specified and programmed
//     210395 jngh changed from plant_section to a parameters section
void Plant::plant_read_root_params ()
    {

//+  Constant Values
    const char*  my_name = "plant_read_root_params" ;
    const char*  section_name = "parameters" ;

//+  Local Variables
    int   layer;                                  // layer number
    float ll [max_layer];                         // lower limit of plant-extractable
                                                  // soil water for soil layer l
                                                  // (mm water/mm soil)
    float dep_tot, esw_tot;                         // total depth of soil & ll
    int   num_layers=0;                             // number of layers in profile
    char  msg[200];

//- Implementation Section ----------------------------------

    push_routine (my_name);

    parent->writeString (" - reading root profile parameters");

//       cproc_sw_demand_bound

    if (parent->readParameter (section_name
                , "eo_crop_factor"//, "()"
                , p.eo_crop_factor
                , 0.0, 100., true) == false)

        {
        p.eo_crop_factor = c.eo_crop_factor_default;
        }
    else
        {
        }

//       plant_sw_supply
    p.uptake_source = parent->readParameter (section_name, "uptake_source");
    if (p.uptake_source == "")

        {
        p.uptake_source = "calc";
        }
    else
        {
        }

    parent->readParameter (section_name
              , "ll"//, "()"
              , ll, num_layers
              , 0.0, c.sw_ub);

    fill_real_array (p.ll_dep, 0.0, max_layer);
    for (int layer = 0; layer < num_layers; layer++)
       p.ll_dep[layer] = ll[layer]*g.dlayer[layer];

    parent->readParameter (section_name
               , "kl"//, "()"
               , p.kl, num_layers
               , 0.0, c.kl_ub);

    parent->readParameter (section_name
              , "xf"//, "()"
              , p.xf, num_layers
              , 0.0, 1.0);

    if (c.root_growth_option == 2)
        {
        parent->readParameter (section_name
                , "root_distribution_pattern"//, "()"
                , p.root_distribution_pattern
                , 0.0, 100.0);
        }

    // report
    parent->writeString ("                   Root Profile");
    parent->writeString ("    -----------------------------------------------");
    parent->writeString ("     Layer       Kl           Lower    Exploration");
    parent->writeString ("     Depth     Factor         Limit      Factor  ");
    parent->writeString ("     (mm)         ()        (mm/mm)       (0-1)");
    parent->writeString ("    -----------------------------------------------");

    dep_tot = esw_tot = 0.0;
    for (layer = 0; layer < num_layers; layer++)
       {
       sprintf (msg, "%9.1f%10.3f%15.3f%12.3f"
          , g.dlayer[layer]
          , p.kl[layer]
          , ll[layer]
          , p.xf[layer]);
       parent->writeString (msg);
       dep_tot += g.dlayer[layer];
       esw_tot += g.dul_dep[layer] - p.ll_dep[layer];
       }
    parent->writeString ("    -----------------------------------------------");
    sprintf (msg
          , "    Extractable SW: %5.0fmm in %5.0fmm total depth (%3.0f%%)."
          , esw_tot
          , dep_tot
          , 100.0 * divide(esw_tot, dep_tot, 0.0));
    parent->writeString (msg);

    sprintf (msg, "%s%5.1f%s"
        ,"    Crop factor for bounding water use is set to "
        , p.eo_crop_factor
          , " times eo.");
    parent->writeString (msg);

    if (c.root_growth_option == 2)
        {
        sprintf (msg, "(%s%5.1f)"
                 , "Root_distribution_pattern ="
                 , p.root_distribution_pattern);
        parent->writeString (msg);
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       End crop

//+  Mission Statement
//     End the crop

//+  Changes
//       290994 jngh specified and programmed
//      191099 jngh changed to plant_Send_Crop_Chopped_Event
void Plant::plant_end_crop ()
    {
    const char*  my_name = "plant_end_crop" ;

    float dm_residue;                             // dry matter added to residue (g/m^2)
    float n_residue;                              // nitrogen added to residue (g/m^2)
    float P_residue;                              // phosphorus added to residue (g/m^2)
    float dm_root;                                // dry matter added to soil (g/m^2)
    float n_root;                                 // nitrogen added to soil (g/m^2)
    float P_root;                                 // phosphorus added to soil (g/m^2)
    char  msg[400];
    float yield;                                  // grain wt (kg/ha)
    float fraction_to_residue[max_part];          // fraction sent to residue (0-1)
    float dlt_dm_crop[max_part];                  // change in dry matter of crop (kg/ha)
    float dlt_dm_n[max_part];                     // N content of changeed dry matter (kg/ha)
    float dlt_dm_p[max_part];                     // P content of changeed dry matter (kg/ha)
    int part;                                     // part
    float  incorp_fr[max_part];
    float  P_tops;                // Phosphorus added to residue (g/m^2)

    push_routine (my_name);

    if (g.plant_status != out)
        {
        g.plant_status = out;
        g.current_stage = (float) plant_end;

        // report
        yield = (g.dm_green[meal] + g.dm_dead[meal]
          + g.dm_green[oil] + g.dm_dead[oil] )
          * gm2kg /sm2ha;
        sprintf (msg, "Crop ended. Yield (dw) = %7.1f  (kg/ha)", yield);
        parent->writeString (msg);

        // now do post harvest processes
        dm_root = g.dm_green[root] + g.dm_senesced[root];

        n_root  = g.n_green[root] + g.n_senesced[root];

        plant_root_incorp (dm_root, n_root, g.root_length);

        plant_root_incorp (g.dm_dead[root], g.n_dead[root], g.root_length_dead);

        P_root = 0.0;
        for (int part = 0; part < max_part; part++)
            {
            incorp_fr[part] = 0.0;
            }
        incorp_fr[root] = 1.0;
        P_root += phosphorus->incorp_fom(incorp_fr // green
                              , incorp_fr // senesced
                              , g.dlayer
                              , g.root_length
                              , g.root_depth);  //  should be all root P all from green, sen and dead

        P_root += phosphorus->incorp_fom_dead(incorp_fr // dead
                                    , g.dlayer
                                    , g.root_length_dead
                                    , g.root_depth);  //  should be all root P all from green, sen and dead

        // put stover and any remaining grain into surface residue
        dm_residue =
             sum_real_array (g.dm_green, max_part) - g.dm_green[root]
           + sum_real_array (g.dm_senesced, max_part) - g.dm_senesced[root]
           + sum_real_array (g.dm_dead, max_part) - g.dm_dead[root];

        for ( part = 0; part < max_part; part++)
           {
           dlt_dm_crop[part] = (g.dm_green[part]
               + g.dm_senesced[part]
               + g.dm_dead[part])
               * gm2kg/sm2ha;
           }
        n_residue =
             sum_real_array (g.n_green, max_part) - g.n_green[root]
           + sum_real_array (g.n_senesced, max_part) - g.n_senesced[root]
           + sum_real_array (g.n_dead, max_part) - g.n_dead[root];

        for ( part = 0; part < max_part; part++)
           {
           dlt_dm_n[part] = (g.n_green[part]
              + g.n_senesced[part]
              + g.n_dead[part])
              * gm2kg/sm2ha;
           }

        // call crop_top_residue (c%crop_type, dm_residue, N_residue)

        for ( part = 0; part < max_part; part++) { fraction_to_residue[part] = 1.0; }
        fraction_to_residue[root] = 0.0;

        if (sum_real_array(dlt_dm_crop, max_part) > 0.0)
            {
            vector<string> part_name;
            part_name.push_back("root");
            part_name.push_back("leaf");
            part_name.push_back("stem");
            part_name.push_back("pod");
            part_name.push_back("meal");
            part_name.push_back("oil");

           // Call plant P module so that it can add
           // its P to the message - green,senesced and dead
           // all removed using same fractions
           float chop_fr[max_part];
           for ( part = 0; part < max_part; part++)
           {
               chop_fr[part] = 1.0;
           }
           chop_fr[root] = 0.0;
//
//           phosphorus->residue_chopped(chop_fr  // green
//                                      , chop_fr  // senesced
//                                      , chop_fr  // dead
//                                      , fraction_to_residue
//                                      , &P_residue
//                                      , dlt_dm_p);  // dlt_dm_p should be all P all from green, sen and dead

            phosphorus->end_crop(chop_fr, &P_residue, dlt_dm_p);  // dlt_dm_p should be all P all from green, sen and dead

            plant_send_crop_chopped_event ( c.crop_type
                  , part_name
                  , dlt_dm_crop
                  , dlt_dm_n
                  , dlt_dm_p
                  , fraction_to_residue
                  , max_part);
            }
        else
            {
            // no surface residue
            }

        dm_root = g.dm_green[root] + g.dm_dead[root] + g.dm_senesced[root];
        n_root  = g.n_green[root] + g.n_dead[root] + g.n_senesced[root];

        parent->writeString ("    Organic matter from crop:-      Tops to surface residue      Roots to soil FOM");

        sprintf (msg, "%48s%7.2f%24.2f"
                           , "DM (kg/ha) =               ", dm_residue * gm2kg /sm2ha, dm_root * gm2kg /sm2ha);
        parent->writeString (msg);

        sprintf (msg, "%48s%7.2f%24.2f"
                           , "N  (kg/ha) =               ", n_residue * gm2kg /sm2ha, n_root * gm2kg /sm2ha);
        parent->writeString (msg);

        sprintf (msg, "%48s%7s%24.2f"
                           , "P  (kg/ha) =               ", " ", P_root * gm2kg /sm2ha);
        parent->writeString (msg);

        parent->writeString (" ");
        }
    else
        {
        sprintf(msg, "%s%s%s", g.module_name.c_str(), " is not in the ground -", " unable to end crop.");

        parent->warningError (msg);
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Kill crop

//+  Mission Statement
//     Kill the crop

//+  Changes
//       151102 jngh specified and programmed

void Plant::plant_kill_crop_action (protocol::Variant &mVar)
    {
//+  Constant Values
    const char*  my_name = "plant_kill_crop_action" ;

//+  Local Variables

//- Implementation Section ----------------------------------

    push_routine (my_name);

    if (g.plant_status != out)
        {
        // kill crop - die
        plant_death_external_action (mVar
                                     , g.plants
                                     , &g.dlt_plants_death_external );

//         if (reals_are_equal (g%dlt_plants_death_external
//     :                       + g%plants, 0.0)) then
//            call plant_kill_crop
//     :               (
//     :                g%dm_dead
//     :              , g%dm_green
//     :              , g%dm_senesced
//     :              , g%plant_status
//     :               )
//         else
//         endif

        }
    else
        {
        char msg[500];
        sprintf(msg, "%s%s%s"
         ,g.module_name.c_str()
         , " is not in the ground -"
         , " unable to kill crop.");
        parent->warningError (msg);
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Stores a value in an annual circular array

//+  Mission Statement
//     Stores a value in an array

//+  Changes
//     230695 jngh specified and programmed
void Plant::plant_store_value (
     int    g_day_of_year        // (INPUT)  day of year
    ,int    g_year               // (INPUT)  year
    ,float  array[]                // (OUTPUT) storage array
    ,float  value                // (INPUT) value to be stored
    ) {

//+  Constant Values
    const char*  my_name = "plant_store_value" ;

//- Implementation Section ----------------------------------

    push_routine (my_name);

    array[g_day_of_year] = value;

    if (g_day_of_year==365 && leap_year (g_year - 1))
        {
        array[366] = 0.0;
        }
    else
        {
        }
    pop_routine (my_name);
    return;
    }


//+  Purpose
//      Get the values of variables/arrays from other modules.

//+  Mission Statement
//     Gets the values of variables/arrays from other modules

//+  Assumptions
//      assumes variable has the following format
//         <variable_name> = <variable_value/s> (<units>)

//+  Changes
//     010994 jngh specified and programmed
//     220696 jngh optimised order of gets
//     140896 jngh modified fr_intc_radn name to inclued a suffix of module name
//     010998 sb Used min_year and max_year instead of consts from ini file.
//     191200 jngh changed soil_temp to maxt_soil_surface
void Plant::plant_get_other_variables ()
    {
    const char*  my_name = "plant_get_other_variables" ;
    std::vector<float> values;               // Scratch area

    int   numvals;                                // number of values put into array
    float soil_temp;                              // soil surface temperature (oC)

//- Implementation Section ----------------------------------

    push_routine (my_name);

    // Parasite assimilate demand
    if (id.parasite_c_demand != 0)
       {
       parent->getVariable(id.parasite_c_demand, g.dlt_dm_parasite_demand, 0.0, 10000.0, true);
       }
    else
       {
       g.dlt_dm_parasite_demand = 0.0;
       }

    if (id.parasite_sw_demand != 0)
       {
       parent->getVariable(id.parasite_sw_demand, g.dlt_sw_parasite_demand, 0.0, 10000.0, true);
       }
    else
       {
       g.dlt_sw_parasite_demand = 0.0;
       }

    // Soil temperature at surface
    if (id.maxt_soil_surface != 0)
       {
       parent->getVariable(id.maxt_soil_surface, soil_temp, 0.0, 80.0, true);
       plant_store_value (g.day_of_year
                        , g.year
                        , g.soil_temp
                        , soil_temp);
       }

    // Canopy XX this is wrong if our 'module name' changes during a simulation
    parent->getVariable(id.fr_intc_radn, g.fr_intc_radn, 0.0, 1.0, true);

    // Soilwat2
    parent->getVariable(id.eo, g.eo, 0.0, 20.0);

    values.empty();
    parent->getVariable(id.sw_dep, values, c.sw_dep_lb, c.sw_dep_ub);
    for (unsigned int i=0; i< values.size(); i++) {
    	g.sw_dep[i] = values[i];
    }
    //assert (values.size() == g.num_layers);

    values.empty();
    if (!parent->getVariable(id.no3, values, c.no3_lb, c.no3_ub, true))
        {
        // we have no N supply - make non-limiting.
        for (int i = 0; i < g.num_layers; i++)
           values.push_back(10000.0);
        }
    for (int i = 0; i < g.num_layers; i++)
       {
       g.no3gsm[i] = values[i] * kg2gm /ha2sm;
       }

    values.empty();
    parent->getVariable(id.no3_min, values, c.no3_min_lb, c.no3_min_ub, true);
    for (int i = 0; i < g.num_layers; i++)
       {
       g.no3gsm_min[i] = values[i] * kg2gm /ha2sm;
       }

    values.empty();
    if (!parent->getVariable(id.nh4, values, c.nh4_lb, c.nh4_ub, true))
        {
        // we have no N supply - make non-limiting.
        for (int i = 0; i < g.num_layers; i++)
           values.push_back(10000.0);
        }
    for (int i = 0; i < g.num_layers; i++)
       {
       g.nh4gsm[i] = values[i] * kg2gm /ha2sm;
       }

    values.empty();
    parent->getVariable(id.nh4_min, values, c.nh4_min_lb, c.nh4_min_ub, true);
    for (int i = 0; i < g.num_layers; i++)
       {
       g.nh4gsm_min[i] = values[i] * kg2gm /ha2sm;
       }

    if (!parent->getVariable(id.co2, g.co2, 0.0, 1500.0, true))
       {
       g.co2 = c.co2_default;
       }
    pop_routine (my_name);
    }

// SWIM
void Plant::plant_get_ext_uptakes (const char *uptake_source,        //(INPUT) uptake flag
                           const char *crop_type,            //(INPUT) crop type name
                           const char *uptake_type,          //(INPUT) uptake name
                           float unit_conversion_factor,     //(INPUT) unit conversion factor
                           float uptake_lbound,              //(INPUT) uptake lower limit
                           float uptake_ubound,              //(INPUT) uptake upper limit
                           float *uptake_array,              //(OUTPUT) crop uptake array
                           int max_layer)                    //(INPUT) max layer number

/*  Purpose
*     Ask swim for uptakes of water or solute
*
*  Mission Statement
*   Get the soil uptake for %3 from another module
*
*  Notes
*      Bounds should probably be passed in when crops decide what
*      these should be (ie when ini files have limits for uptake
*      in them)
*
*  Changes
*     08-05-1997 - huth - Programmed and Specified
*     20/5/2003 ad converted to BC++
*/
   {
   char uptake_name[80];             // Uptake variable name
   unsigned int id, layer;
   std::vector<float> values;   // Scratch area

   if (strcmp(uptake_source, "apsim") == 0 && *crop_type != '\0')
      {
      // NB - if crop type is blank then swim will know nothing
      // about this crop (eg if not initialised yet)

      sprintf(uptake_name, "uptake_%s_%s", uptake_type, crop_type);

      id = parent->addRegistration(RegistrationType::get,
                                   uptake_name, floatArrayType,
                                   "", "");

      parent->getVariable(id, values, uptake_lbound, uptake_ubound);

      for (layer=0; layer< values.size(); layer++)
        {
        uptake_array[layer] = values[layer] * unit_conversion_factor;
        }
      }
   }

//+  Purpose
//      Set the value of a variable or array in other module/s.

//+  Mission Statement
//     Set the value of a variable or array in other modules

//+  Notes
//      a flag is set if any of the totals is requested.  The totals are
//      reset during the next process phase when this happens.

//+  Changes
//     010994 jngh specified and programmed
//     240696 jngh changed set_ to post_ construct
//     011100 dph  change post_ back to set_ construct
void Plant::plant_set_other_variables ()
    {
//+  Constant Values
    const char*  my_name = "plant_set_other_variables" ;

//+  Local Variables
    float scratch[max_layer];                     // soil NO3 change (kg/ha)
    int   layer;                                  // soil layer no.
    int   num_layers;                             // number of layers

//- Implementation Section ----------------------------------

    push_routine (my_name);

    plant_update_other_variables ();

    if (Str_i_Eq(p.uptake_source, "calc"))
        {
        //!!! perhaps we should get number of layers at init and keep it
        num_layers = 1+count_of_real_vals (g.dlayer, max_layer);

        for (layer = 0; layer< num_layers;layer++) {scratch[layer] = g.dlt_no3gsm[layer] * gm2kg /sm2ha;}
        protocol::vector<float> dlt_no3_values(scratch, scratch+num_layers);
        parent->setVariable(id.dlt_no3, dlt_no3_values);

        for (layer = 0; layer< num_layers;layer++) {scratch[layer] = g.dlt_nh4gsm[layer] * gm2kg /sm2ha;}
        protocol::vector<float> dlt_nh4_values(scratch, scratch+num_layers);
        parent->setVariable(id.dlt_nh4, dlt_nh4_values);

        for (layer = 0; layer< num_layers;layer++) {scratch[layer] = g.dlt_sw_dep[layer];}
        protocol::vector<float> dlt_sw_dep_values(scratch, scratch+num_layers);
        parent->setVariable(id.dlt_sw_dep, dlt_sw_dep_values);
        }
    else
        {
        // no need to send updates
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Update other modules states

//+  Mission Statement
//     Update other modules states

//+  Changes
//      250894 jngh specified and programmed
//      191099 jngh changed to plant_Send_Crop_Chopped_Event
void Plant::plant_update_other_variables (void)
    {

//+  Constant Values
    const char*  my_name = "plant_update_other_variables" ;

//+  Local Variables
    float dm_residue[max_part+1];                   // dry matter added to residue (kg/ha)
    float n_residue[max_part+1];                    // nitrogen added to residue (kg/ha)
    float P_residue[max_part+1];                    // phosphorus added to residue (kg/ha)
    float fraction_to_residue[max_part+1];          // fraction sent to residue (0-1)
    float chop_fr[max_part+1];
    float incorp_fr[max_part+1];
    float P_tops;                // Phosphorus added to residue (g/m^2)
//    float P_root;                // Phosphorus added to soil (g/m^2)
    int   part;
    int   layer;
    float root_length[max_layer];

//- Implementation Section ----------------------------------
    push_routine (my_name);

    // dispose of detached material from senesced parts in
    // live population
    for (int part =0; part < max_part; part++)
       {
       dm_residue[part] = g.dlt_dm_detached[part] * gm2kg/sm2ha;
       n_residue[part] = g.dlt_n_detached[part] * gm2kg/sm2ha;
       fraction_to_residue[part] = 1.0;
       }
    fraction_to_residue[root] = 0.0;

    //  call crop_top_residue (g%crop_type, dm_residue, N_residue)
    if (sum_real_array(dm_residue, max_part) > 0.0)
    {
        vector<string> part_name;
        part_name.push_back("root");
        part_name.push_back("leaf");
        part_name.push_back("stem");
        part_name.push_back("pod");
        part_name.push_back("meal");
        part_name.push_back("oil");

             // Call plant P module so that it can add
             // its P to the message
             // calculate chop fractions for this biomass flow
        for (part = 0; part < max_part; part++)
        {
              chop_fr[part] = 1.0;
        }
        chop_fr[root] = 0.0;
//        phosphorus->residue_chopped(chop_fr_green
//                                   , chop_fr_sen
//                                   , chop_fr_dead
//                                   , fraction_to_residue
//                                   , &P_tops
//                                   , P_residue); // P residue should be just dlt_p_det here

        phosphorus->detached (chop_fr, &P_tops, P_residue); // P residue should be just dlt_p_det here

        plant_send_crop_chopped_event (c.crop_type
                                     , part_name
                                     , dm_residue
                                     , n_residue
                                     , P_residue
                                     , fraction_to_residue
                                     , max_part);
    }
    else
    {
        // no surface residue
    }

    // put live population roots into root residue

    // correct root length for change in root length in update
    for (layer = 0; layer < max_layer; layer++)
        {
        root_length[layer] = g.root_length[layer] + g.dlt_root_length_dead[layer];
        }

    plant_root_incorp (g.dlt_dm_detached[root]
                       , g.dlt_n_detached[root]
                       , root_length);

    for (part =0; part < max_part; part++)
    {
         incorp_fr[part] = 0.0;
    }
    incorp_fr[root] = 1.0;

    phosphorus->incorp_fom_detached(incorp_fr
                                    , g.dlayer
                                    , root_length
                                    , g.root_depth);    //  should be detached root P

    // now dispose of dead population detachments
    for (part =0; part < max_part; part++)
    {
       dm_residue[part] = g.dlt_dm_dead_detached[part] * gm2kg/sm2ha;
       n_residue[part] = g.dlt_n_dead_detached[part] * gm2kg/sm2ha;
       fraction_to_residue[part] = 1.0;
    }
    fraction_to_residue[root] = 0.0;

    //      call crop_top_residue (c%crop_type, dm_residue, N_residue)
    if (sum_real_array(dm_residue, max_part) > 0.0)
    {
        vector<string> part_name;
        part_name.push_back("root");
        part_name.push_back("leaf");
        part_name.push_back("stem");
        part_name.push_back("pod");
        part_name.push_back("meal");
        part_name.push_back("oil");

          // calculate chop fractions for this biomass flow
        for (part =0; part < max_part; part++)
        {
           chop_fr[part] = 1.0;
        }
        chop_fr[root] = 0.0;
//             // Call plant P module so that it can add
//             // its P to the message
//        phosphorus->residue_chopped(chop_fr_green
//                                    , chop_fr_sen
//                                    , chop_fr_dead
//                                    , fraction_to_residue
//                                    , &P_tops
//                                    , P_residue); // P residue should be just dlt_p_dead_det here

        phosphorus->dead_detached (chop_fr, &P_tops, P_residue); // P residue should be just dlt_p_dead_det here

        plant_send_crop_chopped_event(c.crop_type
                                    , part_name
                                    , dm_residue
                                    , n_residue
                                    , P_residue
                                    , fraction_to_residue
                                    , max_part);
    }
    else
    {
        // no surface residue
    }

    // put dead population roots into root residue

    // correct root length for change in root length in update
    for (layer = 0; layer < max_layer; layer++)
        {
        root_length[layer] = g.root_length_dead[layer] - g.dlt_root_length_dead[layer];
        }

    plant_root_incorp (g.dlt_dm_dead_detached[root]
                       , g.dlt_n_dead_detached[root]
                       , root_length);

    for (part =0; part < max_part; part++)
    {
         incorp_fr[part] = 0.0;
    }
    incorp_fr[root] = 1.0;

    phosphorus->incorp_fom_dead_detached( incorp_fr
                                          , g.dlayer
                                          , root_length
                                          , g.root_depth);      //  should be dead detached root P

    pop_routine (my_name);
    return;
    }




//+  Purpose
//       Crop initialisation - reads constants from constants file

//+  Mission Statement
//     Read in the constants for plant

//+  Changes
//     010994 jngh specified and programmed
//     070495 psc added extra constants (leaf_app etc.)
//     110695 psc added soil temp effects on plant establishment
//     250996 jngh corrected type of lower limit of parent->readParameter
//     010998 sb removed year upper and lower bounds.
void Plant::plant_read_constants ( void )
    {

//+  Constant Values
    const char*  my_name = "plant_read_constants" ;
    const char*  section_name = "constants" ;

//+  Local Variables
    int   numvals;                                // number of values returned

//- Implementation Section ----------------------------------

    push_routine (my_name);

    // call write_string (new_line            //"    - reading constants");

    c.crop_type = parent->readParameter (section_name, "crop_type");

    c.default_crop_class = parent->readParameter (section_name, "default_crop_class");

    string scratch = parent->readParameter (section_name, "part_names");
    Split_string(scratch, " ", c.part_names);

    parent->readParameter (section_name
    , "sw_ub"//, "(mm/mm)"
    , c.sw_ub
    , 0.0, 1.0);

    parent->readParameter (section_name
    , "sw_lb"//, "(mm/mm)"
    , c.sw_lb
    , 0.0, 1.0);

//    plant_get_other_variables

// checking the bounds of the bounds..
    parent->readParameter (section_name
    , "latitude_ub"//, "(ol)"
    , c.latitude_ub
    , -90.0, 90.0);

    parent->readParameter (section_name
    , "latitude_lb"//, "(ol)"
    , c.latitude_lb
    , -90.0, 90.0);

    parent->readParameter (section_name
    , "maxt_ub"//, "(oc)"
    , c.maxt_ub
    , 0.0, 60.0);

    parent->readParameter (section_name
    , "maxt_lb"//, "(oc)"
    , c.maxt_lb
    , 0.0, 60.0);

    parent->readParameter (section_name
    , "mint_ub"//, "(oc)"
    , c.mint_ub
    , 0.0, 40.0);

    parent->readParameter (section_name
    , "mint_lb"//, "(oc)"
    , c.mint_lb
    , -100.0, 100.0);

    parent->readParameter (section_name
    , "radn_ub"//, "(mj/m^2)"
    , c.radn_ub
    , 0.0, 100.0);

    parent->readParameter (section_name
    , "radn_lb"//, "(mj/m^2)"
    , c.radn_lb
    , 0.0, 100.0);

    parent->readParameter (section_name
    , "dlayer_ub"//, "(mm)"
    , c.dlayer_ub
    , 0.0, 10000.0);

    parent->readParameter (section_name
    , "dlayer_lb"//, "(mm)"
    , c.dlayer_lb
    , 0.0, 10000.0);

// 8th block
    parent->readParameter (section_name
    , "sw_dep_ub"//, "(mm)"
    , c.sw_dep_ub
    , 0.0, 10000.0);

    parent->readParameter (section_name
    , "sw_dep_lb"//, "(mm)"
    , c.sw_dep_lb
    , 0.0, 10000.0);

    parent->readParameter (section_name
    , "no3_ub"//, "(kg/ha)"
    , c.no3_ub
    , 0.0, 100000.0);

    parent->readParameter (section_name
    , "no3_lb"//, "(kg/ha)"
    , c.no3_lb
    , 0.0, 100000.0);

    parent->readParameter (section_name
    , "no3_min_ub"//, "(kg/ha)"
    , c.no3_min_ub
    , 0.0, 100000.0);

    parent->readParameter (section_name
    , "no3_min_lb"//, "(kg/ha)"
    , c.no3_min_lb
    , 0.0, 100000.0);

    parent->readParameter (section_name
    , "nh4_ub"//, "(kg/ha)"
    , c.nh4_ub
    , 0.0, 100000.0);

    parent->readParameter (section_name
    , "nh4_lb"//, "(kg/ha)"
    , c.nh4_lb
    , 0.0, 100000.0);

    parent->readParameter (section_name
    , "nh4_min_ub"//, "(kg/ha)"
    , c.nh4_min_ub
    , 0.0, 100000.0);

    parent->readParameter (section_name
    , "nh4_min_lb"//, "(kg/ha)"
    , c.nh4_min_lb
    , 0.0, 100000.0);

    g.hasreadconstants = true;

    pop_routine (my_name);
    return;
    }


//+  Purpose
//     APSim allows modules to perform calculations in preparation for
//     the standard APSim timestep.  This model uses this opportunity
//     to calculate potential growth variables for the coming day
//     and phenological development.

//+  Mission Statement
//     Perform preparatory calculations for the next timestep

//+  Changes
//     21-08-1997 - huth - Programmed and Specified
void Plant::plant_prepare (void)
    {
//+  Constant Values
    const char*  myname = "plant_prepare" ;

//- Implementation Section ----------------------------------
    push_routine (myname);

    plant_nit_stress (c.n_stress_option);
    plant_temp_stress (1);
    plant_light_supply (1);
    plant_bio_rue (1);
    plant_transpiration_eff (1);
    plant_water_demand (1);
    plant_nit_demand_est(1);

    // Note actually should send total plant
    // potential growth rather than just tops - NIH
    phosphorus->prepare(g.current_stage, g.dm_green, g.dlt_dm_pot_rue);

    pop_routine (myname);
    return;
    }

void Plant::registerClassActions(void)
   {
#ifdef NEED_TO_CALL_DELETEREG_ONCE_ONLY_OR_IT_CRASHES_XXXX
   // Remove old registrations from the system
   for (UInt2StringMap::const_iterator i = IDtoAction.begin();
        i != IDtoAction.end();
        i++)
        {
        parent->deleteRegistration(RegistrationType::respondToEvent,i->first);
        parent->deleteRegistration(RegistrationType::respondToEvent,i->first);
        }
#endif
   IDtoAction.clear();

   // Add the new class actions we're interested in
   for (vector<string>::const_iterator i = c.class_action.begin();
        i != c.class_action.end();
        i++)
      {
      unsigned int id;
      boost::function3<void, unsigned &, unsigned &, protocol::Variant &> fn;
      fn = boost::bind(&Plant::doAutoClassChange, this, _1, _2, _3);
      id = parent->addEvent(i->c_str(), RegistrationType::respondToEvent, fn);

      IDtoAction.insert(UInt2StringMap::value_type(id,i->c_str()));
      //printf("registered '%s' as %d\n",i->c_str(),id);
      }
   }

//+  Purpose
//       Species initialisation - reads constants from constants file

//+  Mission Statement
//     Species initialisation - reads constants from constants file

//+  Changes
//     25-11-1997 neilh adapted from old template approach
void Plant::plant_read_species_const ()
    {

//+  Constant Values
    const char*  my_name = "plant_read_species_const" ;

//+  Local Variables
    int   numvals;                                // number of values returned
    int   num_sections;                           // number of sections to search
    int   part;                                   // plant part counter
    vector<string> search_order;                  // sections to search
    char  name[200];                              // scratch area
//- Implementation Section ----------------------------------

    push_routine (my_name);

    string scratch = parent->readParameter (c.crop_type.c_str(), g.crop_class.c_str());
    Split_string(scratch, " ", search_order);

    parent->writeString (string(" - reading constants for " +
                                g.crop_class + "(" + c.crop_type +")").c_str());

    scratch = parent->searchParameter (search_order, "class_action");
    Split_string(scratch, " ", c.class_action);
    registerClassActions();

    scratch = parent->searchParameter (search_order, "class_change");
    Split_string(scratch, " ", c.class_change);

    parent->searchParameter (search_order, "phenology_option"//, "()"
                      , c.phenology_option
                      , 1, 3);

    parent->searchParameter (search_order, "root_growth_option"//, "()"
                      , c.root_growth_option
                      , 1, 2);

    scratch = parent->searchParameter (search_order, "stage_names");
    Split_string(scratch, " ", c.stage_names);

    parent->searchParameter (search_order, "stage_code"//, "()"
                     , c.stage_code_list, numvals
                     , 0.0, 1000.0);

    parent->searchParameter (search_order,
                      "stage_stem_reduction_harvest"
                     //, "()"
                     , c.stage_stem_reduction_harvest, numvals
                     , 1.0, 11.0);

    parent->searchParameter (search_order,
                      "stage_stem_reduction_kill_stem"
                     //, "()"
                     , c.stage_stem_reduction_kill_stem, numvals
                     , 1.0, 11.0);

//      call search_parent->searchParameter (search_order, num_sections
//     :                     , 'rue', max_stage, '(g dm/mj)'
//     :                     , c%rue, numvals
//     :                     , 0.0, 1000.0)

    parent->searchParameter (search_order,
                      "x_stage_rue"//, "()"
                     , c.x_stage_rue, numvals
                     , 0.0, 1000.0);

    parent->searchParameter (search_order,
                      "y_rue"//, "(g dm/mj)"
                     , c.y_rue, numvals
                     , 0.0, 1000.0);

    parent->searchParameter (search_order,
                      "root_depth_rate"//, "(mm)"
                     , c.root_depth_rate, numvals
                     , 0.0, 1000.0);

    parent->searchParameter (search_order,
                      "n_fix_rate"//, "()"
                     , c.n_fix_rate, numvals
                     , 0.0, 1.0);

    parent->searchParameter (search_order,
                      "transp_eff_cf"//, "(kpa)"
                     , c.transp_eff_cf, numvals
                     , 0.0, 1.0);

    parent->searchParameter (search_order,
                       "partition_option"//, "()"
                      , c.partition_option
                      , 1, 3);

    if (c.partition_option==1 )
        {
        parent->searchParameter (search_order
                         ,"frac_leaf"//,  "()"
                         , c.frac_leaf, numvals
                         , 0.0, 1.0);

        parent->searchParameter (search_order
                         ,"frac_pod"//, "()"
                         , c.frac_pod, numvals
                         , 0.0, 2.0);
        parent->searchParameter (search_order
                         ,"ratio_root_shoot"//, "()"
                         , c.ratio_root_shoot, numvals
                         , 0.0, 1000.0);

        }
    else if (c.partition_option==2)
        {
        parent->searchParameter (search_order
                         ,"x_stage_no_partition"//, "()"
                         , c.x_stage_no_partition
                         , c.num_stage_no_partition
                         , 0.0, 20.0);

        parent->searchParameter (search_order
                         ,"y_frac_leaf"//,  "()"
                         , c.y_frac_leaf, numvals
                         , 0.0, 1.0);

        parent->searchParameter (search_order
                         ,"y_frac_pod"//, "()"
                         , c.y_frac_pod, numvals
                         , 0.0, 2.0);

        parent->searchParameter (search_order
                         ,"y_ratio_root_shoot"//, "()"
                         , c.y_ratio_root_shoot, numvals
                         , 0.0, 1000.0);
        }
    else if (c.partition_option==3)
        {
        parent->searchParameter (search_order
                         ,"x_stage_no_partition"//,  "()"
                         , c.x_stage_no_partition
                         , c.num_stage_no_partition
                         , 0.0, 20.0);

        parent->searchParameter (search_order
                         ,"y_frac_leaf"//, "()"
                         , c.y_frac_leaf, numvals
                         , 0.0, 1.0);
        parent->searchParameter (search_order
                         ,"y_ratio_root_shoot"//, "()"
                         , c.y_ratio_root_shoot, numvals
                         , 0.0, 1000.0);

        }

    parent->searchParameter (search_order
                   ,"row_spacing_default"//, "(mm)"
                   , c.row_spacing_default
                   , 0.0, 2000.);

    parent->searchParameter (search_order
                   ,"skiprow_default"//, "()"
                   , c.skip_row_default
                   , 0.0, 2.0);

    parent->searchParameter (search_order
                     ,"x_row_spacing"//,  "(mm)"
                     , c.x_row_spacing, c.num_row_spacing
                     , 0.0, 2000.);

    parent->searchParameter (search_order
                     ,"y_extinct_coef"//, "()"
                     , c.y_extinct_coef, c.num_row_spacing
                     , 0.0, 1.0);

    parent->searchParameter (search_order
                     ,"y_extinct_coef_dead"//, "()"
                     , c.y_extinct_coef_dead, c.num_row_spacing
                     , 0.0, 1.0);

    parent->searchParameter (search_order
                   ,"extinct_coef_pod"//, "()"
                   , c.extinct_coef_pod
                   , 0.0, 1.0);

    parent->searchParameter (search_order
                   ,"spec_pod_area"//, "()"
                   , c.spec_pod_area
                   , 0.0, 100000.0);

    parent->searchParameter (search_order
                   ,"rue_pod"//, "()"
                   , c.rue_pod
                   , 0.0, 3.0);

// crop failure

    parent->searchParameter (search_order
                   ,"leaf_no_crit"//, "()"
                   , c.leaf_no_crit
                   , 0.0, 100.0);

    parent->searchParameter (search_order
                   ,"tt_emerg_limit"//, "(oc)"
                   , c.tt_emerg_limit
                   , 0.0, 1000.0);

    parent->searchParameter (search_order
                   ,"days_germ_limit"//, "(days)"
                   , c.days_germ_limit
                   , 0.0, 365.0);

    parent->searchParameter (search_order
                   ,"swdf_pheno_limit"//, "()"
                   , c.swdf_pheno_limit
                   , 0.0, 1000.0);

    parent->searchParameter (search_order
                   ,"swdf_photo_limit"//, "()"
                   , c.swdf_photo_limit
                   , 0.0, 1000.0);

    parent->searchParameter (search_order
                   ,"swdf_photo_rate"//, "()"
                   , c.swdf_photo_rate
                   , 0.0, 1.0);

//    plant_root_depth

    parent->searchParameter (search_order
                   ,"initial_root_depth"//, "(mm)"
                   , c.initial_root_depth
                   , 0.0, 1000.0);

//    plant_root_length

    parent->searchParameter (search_order
                   ,"specific_root_length"//, "(mm/g)"
                   , c.specific_root_length
                   , 0.0, 1.0e6);

    parent->searchParameter (search_order
                   ,"root_die_back_fr"//, "(0-1)"
                   , c.root_die_back_fr
                   , 0.0, 0.99);

    parent->searchParameter (search_order
                     ,"x_plant_rld"//,  "()"
                     , c.x_plant_rld, c.num_plant_rld
                     , 0.0, 0.1);

    parent->searchParameter (search_order
                     ,"y_rel_root_rate"//, "()"
                     , c.y_rel_root_rate, c.num_plant_rld
                     , 0.001, 1.0);

//    plant_leaf_area_init
    parent->searchParameter (search_order
                   ,"initial_tpla"//, "(mm^2)"
                   , c.initial_tpla
                   , 0.0, 100000.0);

    parent->searchParameter (search_order
                   ,"min_tpla"//, "(mm^2)"
                   , c.min_tpla
                   , 0.0, 100000.0);

// TEMPLATE OPTION
//    plant_leaf_area

    parent->searchParameter (search_order
                     ,"x_lai"//, "(mm2/mm2)"
                     , c.x_lai, c.num_lai
                     , 0.0, 15.0);

    parent->searchParameter (search_order
                     ,"y_sla_max"//, "(mm2/g)"
                     , c.y_sla_max, c.num_lai
                     , 0.0, 2.e5);

    parent->searchParameter (search_order
                     ,"x_lai_ratio"//, "()"
                     , c.x_lai_ratio, c.num_lai_ratio
                     , 0.0, 1.0);

    parent->searchParameter (search_order
                     ,"y_leaf_no_frac"//, "()"
                     , c.y_leaf_no_frac, c.num_lai_ratio
                     , 0.0, 1.0);

//    plant_get_cultivar_params

    parent->searchParameter (search_order
                   ,"tt_emerg_to_endjuv_ub"//, "()"
                   , c.tt_emerg_to_endjuv_ub
                   , 0.0, 1.e6);

    parent->searchParameter (search_order
                   ,"tt_maturity_to_ripe_ub"//, "()"
                   , c.tt_maturity_to_ripe_ub
                   , 0.0, 1.e6);

//    plant_transp_eff

    parent->searchParameter (search_order
                   ,"svp_fract"//, "()"
                   , c.svp_fract
                   , 0.0, 1.0);

//    cproc_sw_demand_bound

    parent->searchParameter (search_order
                   ,"eo_crop_factor_default"//, "()"
                   , c.eo_crop_factor_default
                   , 0.0, 100.);

//    plant_germination

    parent->searchParameter (search_order
                   ,"pesw_germ"//, "(mm/mm)"
                   , c.pesw_germ
                   , 0.0, 1.0);

    parent->searchParameter (search_order
                     ,"fasw_emerg"//,  "()"
                     , c.fasw_emerg, c.num_fasw_emerg
                     , 0.0, 1.0);

    parent->searchParameter (search_order
                     ,"rel_emerg_rate"//,  "()"
                     , c.rel_emerg_rate, c.num_fasw_emerg
                     , 0.0, 1.0);

//    plant_leaf_appearance

    parent->searchParameter (search_order
                   ,"leaf_no_at_emerg"//, "()"
                   , c.leaf_no_at_emerg
                   , 0.0, 100.0);

//    plant_n_uptake

    parent->searchParameter (search_order
                      ,"n_uptake_option"//, "()"
                      , c.n_uptake_option
                      , 1, 3);

    if (c.n_uptake_option==1)
        {
        parent->searchParameter (search_order
                       ,"no3_diffn_const"//, "(days)"
                       , c.no3_diffn_const
                       , 0.0, 100.0);
        }
    else if (c.n_uptake_option==2)
        {
        parent->searchParameter (search_order
                       ,"no3_uptake_max"//, "(g/mm)"
                       , c.no3_uptake_max
                       , 0.0, 1.0);

        parent->searchParameter (search_order
                       ,"no3_conc_half_max"//, "(ppm)"
                       , c.no3_conc_half_max
                       , 0.0, 100.0);

        parent->searchParameter (search_order
                       , "total_n_uptake_max"//, "(g/m2)"
                       , c.total_n_uptake_max
                       , 0.0, 100.0);
        }
     else if (c.n_uptake_option==3)
         {
         parent->searchParameter (search_order
                       , "kno3"//, "(/day)"
                       , c.kno3
                       , 0.0, 1.0);

         parent->searchParameter (search_order
                       , "no3ppm_min"//, "(ppm)"
                       , c.no3ppm_min
                       , 0.0, 10.0);

         parent->searchParameter (search_order
                       , "knh4"//, "(/day)"
                       , c.knh4
                       , 0.0, 1.0);

         parent->searchParameter (search_order
                       , "nh4ppm_min"//, "(ppm)"
                       , c.nh4ppm_min
                       , 0.0, 10.0);

         parent->searchParameter (search_order
                        , "total_n_uptake_max"//, "(g/m2)"
                        , c.total_n_uptake_max
                        , 0.0, 100.0);
         }
     else
         {
         	// Unknown N uptake option
         }
    c.n_supply_preference = parent->searchParameter (search_order, "n_supply_preference");

    //    plant_phenology_init
    parent->searchParameter (search_order,
                    "shoot_lag"//, "(oc)"
                   , c.shoot_lag
                   , 0.0, 100.0);

    parent->searchParameter (search_order,
                    "shoot_rate"//, "(oc/mm)"
                   , c.shoot_rate
                   , 0.0, 100.0);

    parent->searchParameter (search_order,
                       "leaf_no_pot_option"//, "()"
                      , c.leaf_no_pot_option
                      , 1, 2);

    parent->searchParameter (search_order,
                      "x_node_no_app"//, "()"
                     , c.x_node_no_app, c.num_node_no_app
                     , 0.0, 200.);
    parent->searchParameter (search_order,
                      "y_node_app_rate"//,  "()"
                     , c.y_node_app_rate, c.num_node_no_app
                     , 0.0, 400.);

    parent->searchParameter (search_order,
                      "x_node_no_leaf"//,  "()"
                     , c.x_node_no_leaf, c.num_node_no_leaf
                     , 0.0, 200.);
    parent->searchParameter (search_order,
                      "y_leaves_per_node"//,  "()"
                     , c.y_leaves_per_node, c.num_node_no_leaf
                     , 0.0, 50.);

    //    plant_dm_init
    parent->searchParameter (search_order
                     ,"dm_init"//,  "(g/plant)"
                     , c.dm_init, numvals
                     , 0.0, 1.0);

     //    plant_get_root_params
    parent->searchParameter (search_order
                   ,"kl_ub"//, "()"
                   , c.kl_ub
                   , 0.0, 1000.0);

    //    plant_retranslocate
    parent->searchParameter (search_order
                   ,"stem_trans_frac"//, "()"
                   , c.stem_trans_frac
                   , 0.0, 1.0);

    parent->searchParameter (search_order
                   ,"pod_trans_frac"//, "()"
                   , c.pod_trans_frac
                   , 0.0, 1.0);

    parent->searchParameter (search_order
                   ,"leaf_trans_frac"//, "()"
                   , c.leaf_trans_frac
                   , 0.0, 1.0);

    //    grain number
    parent->searchParameter (search_order
                      ,"grain_no_option"//, "()"
                      , c.grain_no_option
                      , 1, 2);

    //    legume grain filling
    parent->searchParameter (search_order
                      ,"grain_fill_option"//,/ "()"
                      , c.grain_fill_option
                      , 1, 3);

    if (c.grain_fill_option==2 || c.grain_fill_option==3)
        {
        parent->searchParameter (search_order
                         , "x_temp_grainfill"
                         //, "(oc)"
                         , c.x_temp_grainfill
                         , c.num_temp_grainfill
                         , 0.0
                         , 40.0);

        parent->searchParameter (search_order
                         ,"y_rel_grainfill"
                         //, "(-)"
                         , c.y_rel_grainfill
                         , c.num_temp_grainfill
                         , 0.0
                         , 1.0);
        }

     //    plant_n_dlt_grain_conc
     parent->searchParameter (search_order
                        , "grain_n_option"//, "()"
                        , c.grain_n_option
                        , 1, 2);

     if (c.grain_n_option==1)
         {
         parent->searchParameter (search_order
                        ,"sw_fac_max"//, "()"
                        , c.sw_fac_max
                        , 0.0, 100.0);

         parent->searchParameter (search_order
                        ,"temp_fac_min"//, "()"
                        , c.temp_fac_min
                        , 0.0, 100.0);

         parent->searchParameter (search_order
                        ,"sfac_slope"//, "()"
                        , c.sfac_slope
                        , -10.0, 0.0);

         parent->searchParameter (search_order
                        ,"tfac_slope"//, "()"
                        , c.tfac_slope
                        , 0.0, 100.0);
         }
     else
         {
         parent->searchParameter (search_order
                        , "potential_grain_n_filling_rate"//, "()"
                        , c.potential_grain_n_filling_rate
                        , 0.0, 1.0);

         parent->searchParameter (search_order
                        , "crit_grainfill_rate"//, "(mg/grain/d)"
                        , c.crit_grainfill_rate
                        , 0.0, 1.0);

         parent->searchParameter (search_order
                          , "x_temp_grain_n_fill"//,  "(oC)"
                          , c.x_temp_grain_n_fill
                          , c.num_temp_grain_n_fill
                          , 0.0
                          , 40.0);

         parent->searchParameter (search_order
                          , "y_rel_grain_n_fill"
                          //, "(-)"
                          , c.y_rel_grain_n_fill
                          , c.num_temp_grain_n_fill
                          , 0.0
                          , 1.0);
         }

     parent->searchParameter (search_order
                        , "n_retrans_option"//, "()"
                        , c.n_retrans_option
                        , 1, 2);
     if (c.n_retrans_option==1)
         {
         //Nothing..
         }
     else
         {
         parent->searchParameter (search_order
                        , "n_retrans_fraction"//, "()"
                        , c.n_retrans_fraction
                        , 0.0, 1.0);

         parent->searchParameter (search_order
                        , "n_deficit_uptake_fraction"//, "()"
                        , c.n_deficit_uptake_fraction
                        , 0.0, 1.0);
         }

     parent->searchParameter (search_order
                     ,"sen_start_stage"//, "()"
                     , c.sen_start_stage
                     , 0.0, (float)max_stage);

     parent->searchParameter (search_order
                    ,"fr_lf_sen_rate"//, "(/degday)"
                    , c.fr_lf_sen_rate
                    , 0.0, 1.0);

     parent->searchParameter (search_order
                    ,"node_sen_rate"//, "(degday)"
                    , c.node_sen_rate
                    , 0.0, 1000.0);

     parent->searchParameter (search_order
                   , "n_fact_lf_sen_rate"//, "(/degday)"
                   , c.n_fact_lf_sen_rate
                   , 0.0, 5.0);

    //    plant_event
    parent->searchParameter (search_order
                   ,"grn_water_cont"//, "(g/g)"
                   , c.grn_water_cont
                   , 0.0, 1.0);

    //    plant_dm_partition
    parent->searchParameter (search_order
                   ,"sla_min"//, "(mm^2/g)"
                   , c.sla_min
                   , 0.0, 100000.0);

    parent->searchParameter (search_order
                   ,"carbo_oil_conv_ratio"//, "()"
                   , c.carbo_oil_conv_ratio
                   , 0.0, 20.0);

    parent->searchParameter (search_order
                   ,"grain_oil_conc"//, "()"
                   , c.grain_oil_conc
                   , 0.0, 1.0);

    //    plant_dm_senescence
    parent->searchParameter (search_order
                      , "dm_senescence_option"//, "()"
                      , c.dm_senescence_option
                      , 1, 3);

    for (part=0; part<max_part;part++)
       {
       sprintf(name, "x_dm_sen_frac_%s", c.part_names[part].c_str());
       parent->searchParameter (search_order
                        , name
                        //, "()"
                        , c.x_dm_sen_frac[part]
                        , c.num_dm_sen_frac[part]
                        , 0.0
                        , 100.0);

       sprintf(name, "y_dm_sen_frac_%s", c.part_names[part].c_str());
       parent->searchParameter (search_order
                        , name
                        //, "()"
                        , c.y_dm_sen_frac[part]
                        , c.num_dm_sen_frac[part]
                        , 0.0
                        , 1.0);
       }

    //    plant_dm_dead_detachment
    parent->searchParameter (search_order
                     , "dead_detach_frac"//, "()"
                     , c.dead_detach_frac, numvals
                     , 0.0, 1.0);

    parent->searchParameter (search_order
                     , "sen_detach_frac"//, "()"
                     , c.sen_detach_frac, numvals
                     , 0.0, 1.0);

    //    plant_leaf_area_devel
    parent->searchParameter (search_order
                   , "node_no_correction"//, "()"
                   , c.node_no_correction
                   , 0.0, 10.0);

    parent->searchParameter (search_order
                     , "x_node_no"//, "()"
                     , c.x_node_no, c.num_node_no
                     , 0.0, 100.0);

    parent->searchParameter (search_order
                     , "y_leaf_size"//, "(mm2)"
                     , c.y_leaf_size, c.num_node_no
                     , 0.0, 60000.0);

    //    plant_leaf_area_sen_light
    parent->searchParameter (search_order
                   , "lai_sen_light"//, "(m^2/m^2)"
                   , c.lai_sen_light
                   , 3.0, 20.0);

    parent->searchParameter (search_order
                   , "sen_light_slope"//, "()"
                   , c.sen_light_slope
                   , 0.0, 100.0);

    // TEMPLATE OPTION
    //    plant_leaf_area_sen_frost
    parent->searchParameter (search_order
                     , "x_temp_senescence"//, "(oc)"
                     , c.x_temp_senescence, c.num_temp_senescence
                     , -20.0, 20.0);

    parent->searchParameter (search_order
                     , "y_senescence_fac"//, "()"
                     , c.y_senescence_fac, c.num_temp_senescence
                     , 0.0, 1.0);

    // TEMPLATE OPTION
    //    plant_leaf_area_sen_water
    parent->searchParameter (search_order
                   , "sen_rate_water"//, "()"
                   , c.sen_rate_water
                   , 0.0, 100.0);

    //    plant_phenology_init
    parent->searchParameter (search_order
                   , "twilight"//, "(o)"
                   , c.twilight
                   , -90.0, 90.0);

    //    plant_n_conc_limits
    parent->searchParameter (search_order
                     , "x_stage_code"//, "()"
                     , c.x_stage_code, c.num_n_conc_stage
                     , 0.0, 100.0);

    parent->searchParameter (search_order
                     , "y_n_conc_crit_leaf"//, "()"
                     , c.y_n_conc_crit_leaf, c.num_n_conc_stage
                     , 0.0, 100.0);

    parent->searchParameter (search_order
                     , "y_n_conc_max_leaf"//, "()"
                     , c.y_n_conc_max_leaf, c.num_n_conc_stage
                     , 0.0, 100.0);

    parent->searchParameter (search_order
                     , "y_n_conc_min_leaf"//, "()"
                     , c.y_n_conc_min_leaf, c.num_n_conc_stage
                     , 0.0, 100.0);

    parent->searchParameter (search_order
                     , "y_n_conc_crit_stem"//, "()"
                     , c.y_n_conc_crit_stem, c.num_n_conc_stage
                     , 0.0, 100.0);

    parent->searchParameter (search_order
                     , "y_n_conc_max_stem"//, "()"
                     , c.y_n_conc_max_stem, c.num_n_conc_stage
                     , 0.0, 100.0);

    parent->searchParameter (search_order
                     , "y_n_conc_min_stem"//, "()"
                     , c.y_n_conc_min_stem, c.num_n_conc_stage
                     , 0.0, 100.0);

    parent->searchParameter (search_order
                     , "y_n_conc_crit_pod"//, "()"
                     , c.y_n_conc_crit_pod, c.num_n_conc_stage
                     , 0.0, 100.0);

    parent->searchParameter (search_order
                     , "y_n_conc_max_pod"//, "()"
                     , c.y_n_conc_max_pod, c.num_n_conc_stage
                     , 0.0, 100.0);

    parent->searchParameter (search_order
                     , "y_n_conc_min_pod"//, "()"
                     , c.y_n_conc_min_pod, c.num_n_conc_stage
                     , 0.0, 100.0);

    parent->searchParameter (search_order
                   , "n_conc_crit_grain"//, "()"
                   , c.n_conc_crit_grain
                   , 0.0, 100.0);

    parent->searchParameter (search_order
                   , "n_conc_max_grain"//, "()"
                   , c.n_conc_max_grain
                   , 0.0, 100.0);

    parent->searchParameter (search_order
                   , "n_conc_min_grain"//, "()"
                   , c.n_conc_min_grain
                   , 0.0, 100.0);

    parent->searchParameter (search_order
                   , "n_conc_crit_root"//, "()"
                   , c.n_conc_crit_root
                   , 0.0, 100.0);

    parent->searchParameter (search_order
                   , "n_conc_max_root"//, "()"
                   , c.n_conc_max_root
                   , 0.0, 100.0);

    parent->searchParameter (search_order
                   , "n_conc_min_root"//, "()"
                   , c.n_conc_min_root
                   , 0.0, 100.0);

    //    plant_n_init
    parent->searchParameter (search_order
                     , "n_init_conc"//,  "(g/g)"
                     , c.n_init_conc, numvals
                     , 0.0, 1.0);

    //    plant_n_senescence
    parent->searchParameter (search_order
                     , "n_senescence_option"//, "()"
                     , c.n_senescence_option
                     , 1, 2);
    parent->searchParameter (search_order
                     , "n_sen_conc"//, "()"
                     , c.n_sen_conc, numvals
                     , 0.0, 1.0);

    //    plant_nfact
    parent->searchParameter (search_order
                      , "n_stress_option"//, "()"
                      , c.n_stress_option
                      , 1, 2);

    parent->searchParameter (search_order
                  , "N_stress_start_stage"//, "()"
                  , c.n_stress_start_stage
                  , 0.0, 100.0);

    parent->searchParameter (search_order
                   , "n_fact_photo"//, "()"
                   , c.n_fact_photo
                   , 0.0, 100.0);

    parent->searchParameter (search_order
                   , "n_fact_pheno"//, "()"
                   , c.n_fact_pheno
                   , 0.0, 100.0);

    parent->searchParameter (search_order
                   , "n_fact_expansion"//, "()"
                   , c.n_fact_expansion
                   , 0.0, 100.0);

    //    plant_rue_reduction
    parent->searchParameter (search_order
                     , "x_ave_temp"//,  "(oc)"
                     , c.x_ave_temp, c.num_ave_temp
                     , 0.0, 100.0);

    parent->searchParameter (search_order
                     , "y_stress_photo"//,  "()"
                     , c.y_stress_photo, c.num_factors
                     , 0.0, 1.0);

    //    plant_thermal_time
    parent->searchParameter (search_order
                     , "x_temp"//, "(oc)"
                     , c.x_temp, c.num_temp
                     , 0.0, 100.0);

    parent->searchParameter (search_order
                     , "y_tt"//, "(oc days)"
                     , c.y_tt, c.num_temp
                     , 0.0, 100.0);

    parent->searchParameter (search_order
                     , "x_vernal_temp"//, "(oc)"
                     , c.x_vernal_temp, c.num_vernal_temp
                     , -10., 60.0);

    parent->searchParameter (search_order
                     , "y_vernal_days"//, "(days)"
                     , c.y_vernal_days, c.num_vernal_temp
                     , 0.0, 1.0);

    parent->searchParameter (search_order
                     , "x_temp_root_advance"//, "(oc)"
                     , c.x_temp_root_advance
                     , c.num_temp_root_advance
                     , -10., 60.0);

    parent->searchParameter (search_order
                     , "y_rel_root_advance"//, "(0-1)"
                     , c.y_rel_root_advance
                     , c.num_temp_root_advance
                     , 0.0, 1.0);

    parent->searchParameter (search_order
                     , "x_weighted_temp"//, "(oc)"
                     , c.x_weighted_temp, c.num_weighted_temp
                     , 0.0, 100.0);

    parent->searchParameter (search_order
                     , "y_plant_death"//, "(oc)"
                     , c.y_plant_death, c.num_weighted_temp
                     , 0.0, 100.0);

    //    plant_swdef
    parent->searchParameter (search_order
                     , "x_sw_demand_ratio"//, "()"
                     , c.x_sw_demand_ratio, c.num_sw_demand_ratio
                     , 0.0, 100.0);

    parent->searchParameter (search_order
                     , "y_swdef_leaf"//, "()"
                     , c.y_swdef_leaf, c.num_sw_demand_ratio
                     , 0.0, 100.0);

    parent->searchParameter (search_order
                     , "x_sw_avail_ratio"//, "()"
                     , c.x_sw_avail_ratio, c.num_sw_avail_ratio
                     , 0.0, 100.0);

    parent->searchParameter (search_order
                     , "y_swdef_pheno"//, "()"
                     , c.y_swdef_pheno, c.num_sw_avail_ratio
                     , 0.0, 100.0);

    parent->searchParameter (search_order
                     , "x_sw_ratio"//, "()"
                     , c.x_sw_ratio, c.num_sw_ratio
                     , 0.0, 100.0);

    parent->searchParameter (search_order
                     , "y_sw_fac_root"//, "()"
                     , c.y_sw_fac_root, c.num_sw_ratio
                     , 0.0, 100.0);

    parent->searchParameter (search_order
                     , "x_ws_root"//,  "()"
                     , c.x_ws_root, c.num_ws_root
                     , 0.0, 1.0);

    parent->searchParameter (search_order
                     , "y_ws_root_fac"//, "()"
                     , c.y_ws_root_fac, c.num_ws_root
                     , 0.0, 1.0);

    parent->searchParameter (search_order
                     , "x_sw_avail_fix"//,  "()"
                     , c.x_sw_avail_fix, c.num_sw_avail_fix
                     , 0.0, 100.0);

    parent->searchParameter (search_order
                     , "y_swdef_fix"//, "()"
                     , c.y_swdef_fix, c.num_sw_avail_fix
                     , 0.0, 100.0);

    parent->searchParameter (search_order
                     , "oxdef_photo_rtfr"//, "()"
                     , c.oxdef_photo_rtfr, c.num_oxdef_photo
                     , 0.0, 1.0);

    parent->searchParameter (search_order
                     , "oxdef_photo"//, "()"
                     , c.oxdef_photo, c.num_oxdef_photo
                     , 0.0, 1.0);

    parent->searchParameter (search_order
                     , "fr_height_cut"//,  "(0-1)"
                     , c.fr_height_cut, c.num_fr_height_cut
                     , 0.0, 1.0);

    parent->searchParameter (search_order
                     , "fr_stem_remain"//, "()"
                     , c.fr_stem_remain, c.num_fr_height_cut
                     , 0.0, 1.0);

    parent->searchParameter (search_order
                     , "x_sw_avail_ratio_flower"//, "()"
                     , c.x_sw_avail_ratio_flower, c.num_sw_avail_ratio_flower
                     , 0.0, 1.0);

    parent->searchParameter (search_order
                     , "y_swdef_pheno_flower"//, "()"
                     , c.y_swdef_pheno_flower, c.num_sw_avail_ratio_flower
                     , 0.0, 5.0);

    parent->searchParameter (search_order
                     , "x_sw_avail_ratio_grainfill"//, "()"
                     , c.x_sw_avail_ratio_grainfill, c.num_sw_avail_ratio_grainfill
                     , 0.0, 1.0);

    parent->searchParameter (search_order
                     , "y_swdef_pheno_grainfill"//, "()"
                     , c.y_swdef_pheno_grainfill, c.num_sw_avail_ratio_grainfill
                     , 0.0, 5.0);

    //fruit cohort number
    parent->searchParameter (search_order
                    , "fruit_no_option"//, "()"
                    , c.fruit_no_option
                    , 1, 2);

    if (c.fruit_no_option == 2)
        {
        parent->searchParameter (search_order
                        , "days_assimilate_ave"//, "()"
                        , c.days_assimilate_ave
                        , 0, 100);

        parent->searchParameter (search_order
                     , "dm_abort_fract"//, "()"
                     , c.dm_abort_fract
                     , 0.0, 1.0);

        parent->searchParameter (search_order
                     , "fract_dm_fruit_abort_crit"//, "()"
                     , c.fract_dm_fruit_abort_crit
                     , 0.0, 1.0);

        parent->searchParameter (search_order
                     , "fruit_phen_end"//, "()"
                     , c.fruit_phen_end
                     , 0.0, 1.0);

        parent->searchParameter (search_order
                     , "tt_flower_to_start_pod"//, "()"
                     , c.tt_flower_to_start_pod
                     , 0.0, 1000.0);

        parent->searchParameter (search_order
                          , "x_temp_fruit_site"
                          //, "(oC)"
                          , c.x_temp_fruit_site
                          , c.num_temp_fruit_site
                          , 0.0
                          , 50.0);

        parent->searchParameter (search_order
                          , "y_rel_fruit_site"
                          //, "(-)"
                          , c.y_rel_fruit_site
                          , c.num_temp_fruit_site
                          , 0.0
                          , 1.0);
        }

    if (c.grain_fill_option == 3)
        {
        parent->searchParameter (search_order
                          , "x_temp_grainfill"
                          //, "(oC)"
                          , c.x_temp_grainfill
                          , c.num_temp_grainfill
                          , 0.0
                          , 40.0);

        parent->searchParameter (search_order
                          , "y_rel_grainfill"
                          //, "(-)"
                          , c.y_rel_grainfill
                          , c.num_temp_grainfill
                          , 0.0
                          , 1.0);
        }

    parent->searchParameter (search_order
                     , "co2_default"//, "()"
                     , c.co2_default
                     , 0.0, 1000.0);

    parent->searchParameter (search_order
                     , "x_co2_te_modifier"//, "()"
                     , c.x_co2_te_modifier, c.num_co2_te_modifier
                     , 0.0, 1000.0);
    parent->searchParameter (search_order
                     , "y_co2_te_modifier"//, "()"
                     , c.y_co2_te_modifier, c.num_co2_te_modifier
                     , 0.0, 10.0);

    parent->searchParameter (search_order
                     , "x_co2_nconc_modifier"//, "()"
                     , c.x_co2_nconc_modifier, c.num_co2_nconc_modifier
                     , 0.0, 1000.0);
    parent->searchParameter (search_order
                     , "y_co2_nconc_modifier"//, "()"
                     , c.y_co2_nconc_modifier, c.num_co2_nconc_modifier
                     , 0.0, 10.0);

    string pathway = parent->searchParameter (search_order, "photosynthetic_pathway");
    if (stricmp(pathway.c_str(), "C3")==0) {
      c.photosynthetic_pathway = pw_C3;
    } else if(stricmp(pathway.c_str(), "C4")==0) {
      c.photosynthetic_pathway = pw_C4;
    } else {
      c.photosynthetic_pathway = pw_UNDEF;
    }

    pop_routine (my_name);
    }


//+  Purpose
//       Report occurence of harvest and the current status of specific
//       variables.

//+  Mission Statement
//     Report occurance of harvest and the current status of specific variables

//+  Changes
//     1712997 nih specified and programmed
void Plant::plant_harvest_report ()
    {
//+  Constant Values
    const char*  my_name = "plant_harvest_report" ;
    const float  plant_c_frac = 0.4;    // fraction of c in resiudes


//+  Local Variables
    float biomass_dead;                           // above ground dead plant wt (kg/ha)
    float biomass_green;                          // above ground green plant wt (kg/ha)
    float biomass_senesced;                       // above ground senesced plant wt (kg/ha)
    float dm;                                     // above ground total dry matter (kg/ha)
    float grain_wt;                               // grain dry weight (g/kernel)
    float plant_grain_no;                          // final grains /head
    float leaf_no;                                // total leaf number
    float n_grain;                                // total grain N uptake (kg/ha)
    float n_dead;                                 // above ground dead plant N (kg/ha)
    float n_green;                                // above ground green plant N (kg/ha)
    float n_senesced;                             // above ground senesced plant N (kg/ha)
    float n_stover;                               // nitrogen content of stover (kg\ha)
    float n_total;                                // total gross nitrogen content (kg/ha)
    float n_grain_conc_percent;                   // grain nitrogen %
    int   phase;                                  // phenological phase number
    float si1;                                    // mean water stress type 1
    float si2;                                    // mean water stress type 2
    float si4;                                    // mean nitrogen stress type 1
    float si5;                                    // mean nitrogen stress type 2
    float stover;                                 // above ground dry weight less grain (kg/ha)
    char  msg[200];                               // message
    float yield;                                  // grain yield dry wt (kg/ha)
    float yield_wet;                              // grain yield including moisture (kg/ha)
    float fruit_yield;                 // fruit yield dry wt (kg/ha)
    float fruit_yield_wet;             // fruit yield including moisture
                                       // (kg/ha)
    //- Implementation Section ----------------------------------
    push_routine (my_name);

    // crop harvested. Report status
    yield = (g.dm_green[meal] + g.dm_dead[meal] + g.dm_green[oil] + g.dm_dead[oil])
          * gm2kg / sm2ha;

    // include the grain water content
    yield_wet = yield / (1.0 - c.grn_water_cont);

    fruit_yield = 0.0;
    for (int cohort =0; cohort < g.num_fruit_cohorts; cohort++)
        {
        fruit_yield += g.dm_fruit_green [cohort][meal]
                 + g.dm_fruit_dead [cohort][meal]
                 + g.dm_fruit_green[cohort][oil]
                 + g.dm_fruit_dead [cohort][oil]
                 + g.dm_fruit_green[cohort][pod]
                 + g.dm_fruit_dead [cohort][pod];
        }
    fruit_yield *= gm2kg / sm2ha;

    // include the grain water content
    fruit_yield_wet = fruit_yield / (1.0 - c.grn_water_cont);

    grain_wt = divide (g.dm_green[meal] + g.dm_dead[meal] + g.dm_green[oil] + g.dm_dead[oil]
                     , g.grain_no, 0.0);

    plant_grain_no = divide (g.grain_no, g.plants, 0.0);

    biomass_green = (sum_real_array (g.dm_green, max_part) - g.dm_green[root]) * gm2kg / sm2ha;
    biomass_senesced = (sum_real_array (g.dm_senesced, max_part) - g.dm_senesced[root]) * gm2kg / sm2ha;
    biomass_dead = (sum_real_array (g.dm_dead, max_part) - g.dm_dead[root]) * gm2kg / sm2ha;
    dm = (biomass_green + biomass_senesced + biomass_dead);

    stover = dm - yield;

    float dmRoot = (g.dm_green[root] + g.dm_dead[root] + g.dm_senesced[root]) * gm2kg / sm2ha;
    float nRoot = (g.n_green[root] + g.n_dead[root] + g.n_senesced[root]) * gm2kg / sm2ha;

    leaf_no = sum_real_array (g.leaf_no, max_node);

    n_grain_conc_percent = divide (
        g.n_green[meal] + g.n_dead[meal] + g.n_green[oil] + g.n_dead[oil]
      , g.dm_green[meal] + g.dm_dead[meal] + g.dm_green[oil] + g.dm_dead[oil]
      , 0.0) * fract2pcnt;

    n_grain = (g.n_green[meal] + g.n_dead[meal]+ g.n_green[oil] + g.n_dead[oil]) * gm2kg/sm2ha;
    n_green = (sum_real_array (g.n_green, max_part) - g.n_green[root] - g.n_green[meal] - g.n_green[oil]) * gm2kg / sm2ha;
    n_senesced = (sum_real_array (g.n_senesced, max_part) - g.n_senesced[root] - g.n_senesced[meal]- g.n_senesced[oil]) * gm2kg / sm2ha;
    n_dead = (sum_real_array (g.n_dead, max_part) - g.n_dead[root] - g.n_dead[meal] - g.n_dead[oil]) * gm2kg / sm2ha;

    n_stover = n_green + n_senesced + n_dead;
    n_total = n_grain + n_stover;

    float DMRrootShootRatio = divide(dmRoot, dm, 0.0);
    float HarvestIndex      = divide(yield, dm, 0.0);
    float StoverCNRatio     = divide(stover*plant_c_frac, n_stover, 0.0);
    float RootCNRatio       = divide(dmRoot*plant_c_frac, nRoot, 0.0);

    parent->writeString ("");

    sprintf (msg, "%s%4d%26s%s%10.1f"
             , " flowering day          = ",g.flowering_date, " "
             , " stover (kg/ha)         = ",stover);
    parent->writeString (msg);

    if (c.fruit_no_option == 2)
       {
       sprintf (msg, "%s%6.1f%24s%s%10.1f"
             , " fruit yield (kg/ha)    = ", fruit_yield, " "
             , " fruit yield wet (kg/ha)= ", fruit_yield_wet);
       parent->writeString (msg);
       }

    sprintf (msg, "%s%4d%26s%s%10.1f"
             , " maturity day           = ", g.maturity_date, " "
             , " grain yield (kg/ha)    = ", yield);
    parent->writeString (msg);

    sprintf (msg, "%s%6.1f%24s%s%10.1f"
             , " grain % water content  = ", c.grn_water_cont * fract2pcnt, " "
             , " grain yield wet (kg/ha)= ", yield_wet);
    parent->writeString (msg);

    sprintf (msg, "%s%8.3f%22s%s%10.3f"
             , " grain wt (g)           = ", grain_wt, " "
             , " grains/m^2             = ", g.grain_no);
    parent->writeString (msg);

    sprintf (msg, "%s%6.1f%24s%s%10.3f"
             , " grains/plant           = ", plant_grain_no, " "
             , " maximum lai            = ", g.lai_max);
    parent->writeString (msg);

    sprintf (msg, "%s%10.1f"
             , " total above ground biomass (kg/ha)    = ", dm);
    parent->writeString (msg);

    sprintf (msg, "%s%10.1f",
               " live above ground biomass (kg/ha)     = "
              , biomass_green + biomass_senesced);
    parent->writeString (msg);

    sprintf (msg, "%s%10.1f"
             , " green above ground biomass (kg/ha)    = ", biomass_green);
    parent->writeString (msg);

    sprintf (msg, "%s%10.1f"
             , " senesced above ground biomass (kg/ha) = ", biomass_senesced);
    parent->writeString (msg);

    sprintf (msg, "%s%10.1f"
             , " dead above ground biomass (kg/ha)     = ", biomass_dead);
    parent->writeString (msg);

    sprintf (msg, "%s%6.1f"
             , " number of leaves       = ", leaf_no);
    parent->writeString (msg);

    sprintf (msg, "%s%8.3f%22s%s%10.3f"
             , " DM Root:Shoot ratio    = ", DMRrootShootRatio, " "
             , " Harvest Index          = ", HarvestIndex);
    parent->writeString (msg);

    sprintf (msg, "%s%8.3f%22s%s%10.3f"
             , " Stover C:N ratio       = ", StoverCNRatio, " "
             , " Root C:N ratio         = ", RootCNRatio);
    parent->writeString (msg);

    sprintf (msg, "%s%10.2f%20s%s%10.2f"
             , " grain N percent        = ", n_grain_conc_percent, " "
             , " total N content (kg/ha)= ", n_total);
    parent->writeString (msg);

    sprintf (msg, "%s%10.2f%20s%s%8.2f"
             , " grain N uptake (kg/ha) = ", n_grain, " "
             , " senesced N content (kg/ha)=", n_senesced);
    parent->writeString (msg);

    sprintf (msg, "%s%10.2f%20s%s%10.2f"
             , " green N content (kg/ha)= ", n_green, " "
             , " dead N content (kg/ha) = ", n_dead);
    parent->writeString (msg);

    phosphorus->summary (g.dm_green, g.dm_dead);

    parent->writeString ("");

    sprintf (msg,"%s", " Average Stress Indices:                          Water Photo  Water Expan  N Photo      N grain conc");
    parent->writeString (msg);

    for (phase = emerg_to_endjuv; phase <= start_to_end_grain; phase++)
         {
         si1 = divide (g.cswd_photo[phase-1], g.days_tot[phase-1], 0.0);
         si2 = divide (g.cswd_expansion[phase-1], g.days_tot[phase-1], 0.0);
         si4 = divide (g.cnd_photo[phase-1], g.days_tot[phase-1], 0.0);
         si5 = divide (g.cnd_grain_conc[phase-1], g.days_tot[phase-1], 0.0);

         sprintf (msg,"%4s%-20s%s%-23s%6.3f%13.3f%13.3f%13.3f", " ", c.stage_names[phase-1].c_str(), " to ", c.stage_names[phase].c_str(), si1, si2, si4, si5);
         parent->writeString (msg);

         }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//     Changes crop class automatically

//+  Mission Statement
//     Changes crop class automatically

//+  Changes
//     10-02-1998 - unknown - Programmed and Specified
bool  Plant::plant_auto_class_change (const char *action)
   {
    const char*  myname = "plant_auto_class_change" ;

    //- Implementation Section ----------------------------------
    push_routine (myname);

    vector<string>::iterator i = find(c.class_action.begin(), c.class_action.end(),
                                      action);
    if (i == c.class_action.end()
        || Str_i_Eq(c.class_change[i-c.class_action.begin()],"unchanged"))
        {
        // then do nothing
        return false;
        }
    else
        {
        g.crop_class = c.class_change[i-c.class_action.begin()];
        plant_read_species_const();
        return true;
        }
    }



//+  Purpose
//     Notify other modules of crop chopped.

//+  Mission Statement
//     Notify other modules of crop chopped.

//+  Changes
//   070999 jngh - Programmed and Specified
//   190901 jngh - corrected dm_type to array
void Plant::plant_send_crop_chopped_event (
    const string&  crop_type                      // (INPUT) crop type
    ,vector<string>& dm_type                        // (INPUT) residue type
    ,float * dlt_crop_dm                    // (INPUT) residue weight (kg/ha)
    ,float * dlt_dm_n                       // (INPUT) residue N weight (kg/ha)
    ,float * dlt_dm_p                       // (INPUT) residue P weight (kg/ha)
    ,float * fraction_to_residue            // (INPUT) residue fraction to residue (0-1)
    ,int     max_part                       // (INPUT) number of residue types
    ) {

//+  Constant Values
    const char*  myname = "plant_send_crop_chopped_event" ;
//- Implementation Section ----------------------------------
    push_routine (myname);

#ifdef PROTOCOL_WORKS_PROPERLY
    protocol::crop_choppedType chopped;
    chopped.crop_type = crop_type.c_str();
    for (int i = 0; i < max_part; i++)
       {
       chopped.dm_type.push_back(dm_type[i].c_str());
       chopped.dlt_crop_dm.push_back(dlt_crop_dm[i]);
       chopped.dlt_dm_n.push_back(dlt_dm_n[i]);
       chopped.dlt_dm_p.push_back(dlt_dm_p[i]);
       chopped.fraction_to_residue.push_back(fraction_to_residue[i]);
       }
    parent->publish (id.crop_chopped, chopped);
#else
    protocol::ApsimVariant outgoingApsimVariant(parent);
    outgoingApsimVariant.store("crop_type", protocol::DTstring, false, FString(crop_type.c_str()));
#if 1
    // Other end of this assumes all strings have equal length
    unsigned int maxlen = 0;
    for (unsigned int i=0; i <  dm_type.size();i++)
        {
        maxlen = max(maxlen, dm_type[i].size());
        }
    char *buf = new char [maxlen*dm_type.size()];
    memset(buf, 0,maxlen*dm_type.size());
    for (unsigned int i=0; i <  dm_type.size();i++)
        {
        strcpy(buf+i*maxlen, dm_type[i].c_str());
        }
    outgoingApsimVariant.store("dm_type", protocol::DTstring, true,
              FStrings(buf, maxlen, dm_type.size(), dm_type.size()));
    delete [] buf;
#else
    protocol::vector<FString> dmtypes;
    for (unsigned int i=0; i <  dm_type.size();i++)
        {
        FString dm = dm_type[i].c_str();
        dmtypes.push_back(dm);
        }
    outgoingApsimVariant.store("dm_type", protocol::DTstring, true, dmtypes);
#endif
    outgoingApsimVariant.store("dlt_crop_dm", protocol::DTsingle, true,
             protocol::vector<float>(dlt_crop_dm, dlt_crop_dm+max_part));
    outgoingApsimVariant.store("dlt_dm_n", protocol::DTsingle, true,
             protocol::vector<float>(dlt_dm_n, dlt_dm_n+max_part));
    outgoingApsimVariant.store("dlt_dm_p", protocol::DTsingle, true,
             protocol::vector<float>(dlt_dm_p, dlt_dm_p+max_part));
    outgoingApsimVariant.store("fraction_to_residue", protocol::DTsingle, true,
             protocol::vector<float>(fraction_to_residue, fraction_to_residue+max_part));
    parent->publish (id.crop_chopped, outgoingApsimVariant);
#endif
    pop_routine (myname);
    return;
    }


//+  Purpose
//     Update internal soil layer structure with new data

//+  Mission Statement
//     Update internal soil layer structure with new data

//+  Changes
//        150600 nih
void Plant::doNewProfile(unsigned &, unsigned &, protocol::Variant &v /* (INPUT) message arguments*/)
    {

//+  Local Variables
    int   numvals;
    float profile_depth;                          // depth of soil profile (mm)
    float root_depth_new;                         // new root depth (mm)
    float dlayer[max_layer];                      // soil layer depths (mm)
    int   layer;

//+  Constant Values
    const char*  myname = "plant_onnew_profile" ;

//- Implementation Section ----------------------------------
    push_routine (myname);

#ifdef PROTOCOL_WORKS_PROPERLY
    protocol::new_profileType profile;
    v.unpack(profile);
    for (unsigned layer = 0; layer != profile.dlayer.size(); layer++)
       {
       dlayer[layer] = profile.dlayer[layer];
       g.ll15_dep[layer] = profile.ll15_dep[layer];
       g.dul_dep[layer] = profile.dul_dep[layer];
       g.sat_dep[layer] = profile.sat_dep[layer];
       g.sw_dep[layer] = profile.sw_dep[layer];
       g.bd[layer] = profile.bd[layer];
       }
#else
    protocol::ApsimVariant av(parent);
    av.aliasTo(v.getMessageData());
    protocol::vector<float> scratch;
    av.get("dlayer", protocol::DTsingle, true, scratch);
    for (unsigned layer = 0; layer != scratch.size(); layer++) { dlayer[layer] = scratch[layer]; }
    numvals = scratch.size();

    av.get("ll15_dep", protocol::DTsingle, true, scratch);
    for (unsigned layer = 0; layer != scratch.size(); layer++) { g.ll15_dep[layer] = scratch[layer]; }
    av.get("dul_dep", protocol::DTsingle, true, scratch);
    for (unsigned layer = 0; layer != scratch.size(); layer++) { g.dul_dep[layer] = scratch[layer]; }
    av.get("sat_dep", protocol::DTsingle, true, scratch);
    for (unsigned layer = 0; layer != scratch.size(); layer++) { g.sat_dep[layer] = scratch[layer]; }
    av.get("sw_dep", protocol::DTsingle, true, scratch);
    for (unsigned layer = 0; layer != scratch.size(); layer++) { g.sw_dep[layer] = scratch[layer]; }
    av.get("bd", protocol::DTsingle, true, scratch);
    for (unsigned layer = 0; layer != scratch.size(); layer++) { g.bd[layer] = scratch[layer]; }

#endif
// dlayer may be changed from its last setting
// due to erosion

    profile_depth = sum_real_array (dlayer, numvals);

    if (g.root_depth>profile_depth)
        {
        root_depth_new = profile_depth;
        crop_root_redistribute(g.root_length
                             , g.root_depth
                             , g.dlayer
                             , g.num_layers
                             , root_depth_new
                             , dlayer
                             , numvals);

        g.root_depth = root_depth_new;
        }
    else
        {
        // roots are ok.
        }

    // NIH - don't like changing parameter value here.
    for (layer = 0; layer < numvals; layer++)
       {
       p.ll_dep[layer] = divide (p.ll_dep[layer], g.dlayer[layer], 0.0)
                              * dlayer[layer];

       g.dlayer[layer] = dlayer[layer];
       }
    g.num_layers = numvals;

    pop_routine (myname);
    return;
    }


//+  Purpose
//      Get the values of site characteristics (that will not change
//      during the simulation.

//+  Mission Statement
//     Get the geographic attributes of the site

//+  Changes
//     200600 nih specified and programmed
void Plant::plant_get_site_characteristics ()
    {
//+  Constant Values
    const char*  my_name = "plant_get_site_characteristics" ;

//+  Local Variables
    int   numvals;                                // number of values put into array

//- Implementation Section ----------------------------------

    push_routine (my_name);

    parent->getVariable(id.latitude, g.latitude, c.latitude_lb, c.latitude_ub);

    pop_routine (my_name);
    return;
    }



/////////////////////////////Get&Set Interface code
bool Plant::set_plant_crop_class(protocol::QuerySetValueData&v)
    {
    FString crop_class;
    v.variant.unpack(crop_class);
    g.crop_class = crop_class.f_str();
    plant_read_species_const ();
    return true;
    }
bool Plant::set_plant_grain_oil_conc(protocol::QuerySetValueData&v)
    {
    v.variant.unpack(c.grain_oil_conc);
    bound_check_real_var(parent,c.grain_oil_conc, 0.0, 1.0, "grain_oil_conc");
    plant_read_species_const ();
    return true;
    }

void Plant::get_plant_status(protocol::Component *system, protocol::QueryValueData &qd) const
{
    switch (g.plant_status) {
    	case out: system->sendVariable(qd, FString("out")); break;
    	case dead: system->sendVariable(qd, FString("dead")); break;
    	case alive: system->sendVariable(qd, FString("alive")); break;
    }
}


void Plant::get_stage_code(protocol::Component *system, protocol::QueryValueData &qd)
{
    if (g.plant_status != out)
    {
        int stage_no = (int) g.current_stage;
        system->sendVariable(qd, c.stage_code_list[stage_no-1]);
    }
    else
    {
        system->sendVariable(qd, (float) 0.0);
    }
}


void Plant::get_stage_name(protocol::Component *system, protocol::QueryValueData &qd)
{
    if (g.plant_status != out)
    {
        unsigned int stage_no = (unsigned int) g.current_stage;
        if (stage_no > 0 && stage_no <= c.stage_names.size())
           {
           system->sendVariable(qd,FString(c.stage_names[stage_no-1].c_str()));
           return;
           }
    }
    system->sendVariable(qd,FString("out"));
}


void Plant::get_crop_type(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, FString(c.crop_type.c_str()));
}

void Plant::get_das(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, g.das);
}


void Plant::get_leaf_no(protocol::Component *system, protocol::QueryValueData &qd)
{
    float temp = sum_real_array(g.leaf_no, max_node);
    system->sendVariable(qd, temp);
}


void Plant::get_node_no(protocol::Component *system, protocol::QueryValueData &qd)
{
    float temp = sum_real_array(g.node_no, max_stage);
    system->sendVariable(qd, temp);
}

void Plant::get_leaf_no_dead(protocol::Component *system, protocol::QueryValueData &qd)
{
    float temp = sum_real_array(g.leaf_no_dead, max_node);
    system->sendVariable(qd, temp);
}


void Plant::get_leaf_area(protocol::Component *system, protocol::QueryValueData &qd)
{
    // XX can't handle big arrays..
    system->sendVariable(qd, protocol::vector<float>(g.leaf_area, g.leaf_area+20/*max_node*/));
}


void Plant::get_height(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, g.canopy_height);
}

void Plant::get_plants(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, g.plants);
}


void Plant::get_cover_green(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, g.cover_green);
}


void Plant::get_cover_tot(protocol::Component *system, protocol::QueryValueData &qd)
{
    float cover_tot = 1.0
        - (1.0 - g.cover_green)
        * (1.0 - g.cover_sen)
        * (1.0 - g.cover_dead);

    system->sendVariable(qd, cover_tot);
}


void Plant::get_lai_sum(protocol::Component *system, protocol::QueryValueData &qd)
{
    float lai_sum = g.lai + g.slai + g.tlai_dead;
    system->sendVariable(qd, lai_sum);
}


void Plant::get_tlai(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, g.lai + g.slai);
}


void Plant::get_grain_size(protocol::Component *system, protocol::QueryValueData &qd)
{
    float grain_size = divide ( g.dm_green[meal]
                                  +g.dm_dead[meal]
                                  +g.dm_green[oil]
                                  +g.dm_dead[oil]
                               , g.grain_no, 0.0);

    system->sendVariable(qd, grain_size);
}

void Plant::get_head_wt(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, g.dm_green[meal]+g.dm_green[oil]+g.dm_green[pod]);
}


void Plant::get_yield(protocol::Component *system, protocol::QueryValueData &qd)
{
    float yield = (g.dm_green[meal] + g.dm_dead[meal]
        + g.dm_green[oil] + g.dm_dead[oil])
        * gm2kg / sm2ha;
    system->sendVariable(qd, yield);
}


void Plant::get_biomass(protocol::Component *system, protocol::QueryValueData &qd)
{
    float biomass = (sum_real_array (g.dm_green, max_part)
        - g.dm_green[root]
        + sum_real_array (g.dm_senesced, max_part)
        - g.dm_senesced[root]
        + sum_real_array (g.dm_dead, max_part)
        - g.dm_dead[root])
        * gm2kg / sm2ha;

    system->sendVariable(qd, biomass);
}


void Plant::get_green_biomass(protocol::Component *system, protocol::QueryValueData &qd)
{
    float biomass = (sum_real_array (g.dm_green, max_part)
        - g.dm_green[root])
        * gm2kg / sm2ha;

    system->sendVariable(qd, biomass);
}


void Plant::get_biomass_wt(protocol::Component *system, protocol::QueryValueData &qd)
{
    float biomass = (sum_real_array (g.dm_green, max_part)
        - g.dm_green[root]
        + sum_real_array (g.dm_senesced, max_part)
        - g.dm_senesced[root]
        + sum_real_array (g.dm_dead, max_part)
        - g.dm_dead[root]);

    system->sendVariable(qd, biomass);
}


void Plant::get_green_biomass_wt(protocol::Component *system, protocol::QueryValueData &qd)
{
    float biomass = sum_real_array (g.dm_green, max_part)
        - g.dm_green[root];

    system->sendVariable(qd, biomass);
}


void Plant::get_biomass_n(protocol::Component *system, protocol::QueryValueData &qd)
{
    float biomass_n = (sum_real_array (g.n_green, max_part)
        - g.n_green[root]
        + sum_real_array (g.n_senesced, max_part)
        - g.n_senesced[root]
        + sum_real_array (g.n_dead, max_part)
        - g.n_dead[root]);
    //ih     :             * gm2kg / sm2ha;

    system->sendVariable(qd, biomass_n);
}


void Plant::get_n_uptake(protocol::Component *system, protocol::QueryValueData &qd)
{
    float biomass_n = (sum_real_array (g.n_green, max_part)
        - g.n_green[root]
        + sum_real_array (g.n_senesced, max_part)
        - g.n_senesced[root]
        + sum_real_array (g.n_dead, max_part)
        - g.n_dead[root]);
    //cih     :             * gm2kg / sm2ha;

    system->sendVariable(qd, biomass_n);
}


void Plant::get_green_biomass_n(protocol::Component *system, protocol::QueryValueData &qd)
{
    float biomass_n = (sum_real_array (g.n_green, max_part)
        - g.n_green[root]);
    //cih     :             * gm2kg / sm2ha;

    system->sendVariable(qd, biomass_n);
}


void Plant::get_grain_n(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, g.n_green[meal] + g.n_green[oil]);
}


void Plant::get_head_n(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, g.n_green[pod]+g.n_green[meal]);
}

void Plant::get_grain_wt(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, g.dm_green[oil]+g.dm_green[meal]);
}

void Plant::get_ep(protocol::Component *system, protocol::QueryValueData &qd)
{
    float sum = 0.0;
    int num_layers = 1+count_of_real_vals (g.dlayer, max_layer);
    for (int layer = 0; layer < num_layers; layer++)
        {
        sum = sum + fabs(-1.0 * g.dlt_sw_dep[layer]);
        }
    system->sendVariable(qd, sum);
}

void Plant::get_sw_uptake(protocol::Component *system, protocol::QueryValueData &qd)
{
    float rwu[max_layer];
    int num_layers = 1+count_of_real_vals (g.dlayer, max_layer);
    for (int layer = 0; layer < num_layers; layer++)
        {
        rwu[layer] = - g.dlt_sw_dep[layer];
        }
    system->sendVariable(qd, protocol::vector<float>(rwu, rwu+num_layers));
}



void Plant::get_cep(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, - g.transpiration_tot);
}


void Plant::get_sw_supply(protocol::Component *system, protocol::QueryValueData &qd)
{
    int deepest_layer = find_layer_no (g.root_depth, g.dlayer, max_layer);
    float sw_supply_sum = sum_real_array (g.sw_supply, deepest_layer+1);
    system->sendVariable(qd, sw_supply_sum);
}

void Plant::get_sw_supply_layr(protocol::Component *system, protocol::QueryValueData &qd)
{
    int num_layers = 1+count_of_real_vals (g.dlayer, max_layer);
    system->sendVariable(qd, protocol::vector<float>(g.sw_supply, g.sw_supply+num_layers));
}

void Plant::get_esw_layr(protocol::Component *system, protocol::QueryValueData &qd)
{
    float esw_layr[max_layer];
    int num_layers = 1+count_of_real_vals (g.dlayer, max_layer);
    for (int layer = 0; layer < num_layers; layer++)
       {
       esw_layr[layer] = l_bound (g.sw_dep[layer] - p.ll_dep[layer], 0.0);
       }
    system->sendVariable(qd, protocol::vector<float>(esw_layr,esw_layr+num_layers));
}

// plant nitrogen
void Plant::get_n_conc_stover(protocol::Component *system, protocol::QueryValueData &qd)
{
    float n_conc = divide ((g.n_green[leaf]
        + g.n_green[stem]
        + g.n_green[pod])
        , (g.dm_green[leaf]
        + g.dm_green[stem]
        + g.dm_green[pod])
        , 0.0) * 100.0;

    system->sendVariable(qd, n_conc);
}


void Plant::get_n_conc_root(protocol::Component *system, protocol::QueryValueData &qd)
{
    float n_conc = divide (g.n_green[root]
        , g.dm_green[root]
        , 0.0) * 100.0;

    system->sendVariable(qd, n_conc);
}
void Plant::get_n_conc_leaf(protocol::Component *system, protocol::QueryValueData &qd)
{
    float n_conc = divide (g.n_green[leaf]
        , g.dm_green[leaf]
        , 0.0) * 100.0;

    system->sendVariable(qd, n_conc);
}


void Plant::get_n_conc_stem(protocol::Component *system, protocol::QueryValueData &qd)
{
    float n_conc = divide (g.n_green[stem]
        , g.dm_green[stem]
        , 0.0) * 100.0;

    system->sendVariable(qd, n_conc);
}


void Plant::get_n_conc_grain(protocol::Component *system, protocol::QueryValueData &qd)
{
    float n_conc = divide (g.n_green[meal] + g.n_green[oil]
        , g.dm_green[meal] + g.dm_green[oil]
        , 0.0) * 100.0;

    system->sendVariable(qd, n_conc);
}

void Plant::get_grain_protein(protocol::Component *system, protocol::QueryValueData &qd)
{
    float gp = divide (g.n_green[meal] + g.n_green[oil]
                       , g.dm_green[meal] + g.dm_green[oil]
                       , 0.0) * 100.0 * 5.71;

    system->sendVariable(qd, gp);
}


void Plant::get_n_conc_meal(protocol::Component *system, protocol::QueryValueData &qd)
{
    float n_conc = divide (g.n_green[meal]
        , g.dm_green[meal]
        , 0.0) * 100.0;

    system->sendVariable(qd, n_conc);
}


void Plant::get_n_conc_crit(protocol::Component *system, protocol::QueryValueData &qd)
{
    float n_conc = divide ((g.n_conc_crit[leaf]*g.dm_green[leaf]
        + g.n_conc_crit[stem]*g.dm_green[stem])
        , (g.dm_green[leaf] + g.dm_green[stem])
        , 0.0) * 100.0;

    system->sendVariable(qd, n_conc);
}

void Plant::get_n_conc_crit_leaf(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, (float) (g.n_conc_crit[leaf] * 100.0));
}
void Plant::get_n_conc_crit_stem(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, (float) (g.n_conc_crit[stem] * 100.0));
}
void Plant::get_n_conc_min_leaf(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, (float) (g.n_conc_min[leaf] * 100.0));
}
void Plant::get_n_conc_min_stem(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, (float) (g.n_conc_min[stem] * 100.0));
}

void Plant::get_n_conc_min(protocol::Component *system, protocol::QueryValueData &qd)
{
    float n_conc = divide ((g.n_conc_min[leaf]*g.dm_green[leaf]
        + g.n_conc_min[stem]*g.dm_green[stem])
        , (g.dm_green[leaf]
        + g.dm_green[stem])
        , 0.0) * 100.0;

    system->sendVariable(qd, n_conc);
}


void Plant::get_n_uptake_stover(protocol::Component *system, protocol::QueryValueData &qd)
{
    float apt_n_up = (g.n_green[leaf]+g.n_green[stem]+g.n_green[pod]);
    //cih     :            *gm2kg /sm2ha;
    system->sendVariable(qd, apt_n_up);
}


void Plant::get_no3_tot(protocol::Component *system, protocol::QueryValueData &qd)
{
    int deepest_layer = find_layer_no (g.root_depth, g.dlayer, max_layer);
    float no3gsm_tot = sum_real_array (g.no3gsm, deepest_layer+1);
    system->sendVariable(qd, no3gsm_tot);
}


void Plant::get_n_demand(protocol::Component *system, protocol::QueryValueData &qd)
{
    float n_demand = sum_real_array (g.n_demand, max_part);
    system->sendVariable(qd, n_demand);
}

void Plant::get_grain_n_demand(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, g.grain_n_demand);
}


void Plant::get_n_supply_soil(protocol::Component *system, protocol::QueryValueData &qd)
{
    int deepest_layer = find_layer_no (g.root_depth, g.dlayer, max_layer);
    float n_uptake_sum = sum_real_array (g.dlt_no3gsm, deepest_layer+1)
                       +  sum_real_array (g.dlt_nh4gsm, deepest_layer+1);
    if (n_uptake_sum > 0)
       n_uptake_sum = - n_uptake_sum;
    else if (n_uptake_sum < 0)
       n_uptake_sum = - n_uptake_sum ;
    else
      n_uptake_sum = 0.0 ;
    system->sendVariable(qd, n_uptake_sum);
}



void Plant::get_n_fixed_tops(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, g.n_fixed_tops);
}

void Plant::get_nfact_grain_tot(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, protocol::vector<float>(g.cnd_grain_conc,g.cnd_grain_conc+max_stage));
}


void Plant::get_rlv(protocol::Component *system, protocol::QueryValueData &qd)
{
	 float rlv[max_layer];
    int num_layers = 1+count_of_real_vals (g.dlayer, max_layer);
    for (int layer = 0; layer < num_layers; layer++)
       {
       rlv[layer] = divide (g.root_length[layer]
          ,g.dlayer[layer], 0.0);
       }
    system->sendVariable(qd, protocol::vector<float>(rlv,rlv+num_layers));
}


void Plant::get_no3_demand(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, (float)g.ext_n_demand*gm2kg/sm2ha);
}

void Plant::get_sw_demand_te(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, g.sw_demand_te);
}


void Plant::get_root_length(protocol::Component *system, protocol::QueryValueData &qd)
{
    int num_layers = 1+count_of_real_vals (g.dlayer, max_layer);
    system->sendVariable(qd, protocol::vector<float>(g.root_length,g.root_length+num_layers));
}


void Plant::get_root_length_dead(protocol::Component *system, protocol::QueryValueData &qd)
{
    int num_layers = 1+count_of_real_vals (g.dlayer, max_layer);
    system->sendVariable(qd, protocol::vector<float>(g.root_length_dead, g.root_length_dead+num_layers));
}


void Plant::get_no3gsm_uptake_pot(protocol::Component *system, protocol::QueryValueData &qd)
{
    int num_layers = 1+count_of_real_vals (g.dlayer, max_layer);
    system->sendVariable(qd, protocol::vector<float>(g.no3gsm_uptake_pot, g.no3gsm_uptake_pot+num_layers));
}

void Plant::get_nh4gsm_uptake_pot(protocol::Component *system, protocol::QueryValueData &qd)
{
    int num_layers = 1+count_of_real_vals (g.dlayer, max_layer);
    system->sendVariable(qd, protocol::vector<float>(g.nh4gsm_uptake_pot, g.nh4gsm_uptake_pot+num_layers));
}

void Plant::get_no3_swfac(protocol::Component *system, protocol::QueryValueData &qd)
{
    float swfac[max_layer];
    int num_layers = 1+count_of_real_vals (g.dlayer, max_layer);
    for (int layer=0; layer < num_layers; layer++)
        {
        swfac[layer] = pow(divide(g.sw_avail[layer],g.sw_avail_pot[layer],0.0), 2);
        swfac[layer] = bound(swfac[layer],0.0,1.0);
        }

    system->sendVariable(qd, protocol::vector<float>(swfac, swfac+num_layers));
}


void Plant::get_leaves_per_node(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, g.leaves_per_node);
}

void Plant::get_no3_uptake(protocol::Component *system, protocol::QueryValueData &qd)
{
    float no3_uptake[max_layer];
    fill_real_array(no3_uptake,0.0, max_layer);
    int num_layers = count_of_real_vals (g.dlayer, max_layer);
    for (int layer = 0; layer <= num_layers; layer++) {
       no3_uptake[layer] =  g.dlt_no3gsm[layer] * gm2kg/sm2ha;
    }
    system->sendVariable(qd, protocol::vector<float>(no3_uptake, no3_uptake+num_layers));
}

void Plant::get_nh4_uptake(protocol::Component *system, protocol::QueryValueData &qd)
{
    float nh4_uptake[max_layer];
    fill_real_array(nh4_uptake,0.0, max_layer);
    int num_layers = count_of_real_vals (g.dlayer, max_layer);
    for (int layer = 0; layer <= num_layers; layer++) {
       nh4_uptake[layer] =  g.dlt_nh4gsm[layer] * gm2kg/sm2ha;
    }
    system->sendVariable(qd, protocol::vector<float>(nh4_uptake, nh4_uptake+num_layers));
}

void Plant::get_swstress_pheno(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    float swstress_pheno;
    if (g.swdef_pheno > 0.0)
       swstress_pheno = 1.0 - g.swdef_pheno;
    else
       swstress_pheno = 0.0;
    systemInterface->sendVariable(qd, swstress_pheno);  //()
}

void Plant::get_swstress_photo(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    float swstress_photo;
    if (g.swdef_photo > 0.0)
       swstress_photo = 1.0 - g.swdef_photo;
    else
       swstress_photo = 0.0;
    systemInterface->sendVariable(qd, swstress_photo);  //()
}

void Plant::get_swstress_expan(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    float swstress_expan;
    if (g.swdef_expansion > 0.0)
       swstress_expan = 1.0 - g.swdef_expansion;
    else
       swstress_expan = 0.0;
    systemInterface->sendVariable(qd, swstress_expan);  //()
}

void Plant::get_swstress_fixation(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    float swstress_fixation;
    if (g.swdef_fixation > 0.0)
       swstress_fixation = 1.0 - g.swdef_fixation;
    else
       swstress_fixation = 0.0;
    systemInterface->sendVariable(qd, swstress_fixation);  //()
}

void Plant::get_nstress_pheno(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    float nstress_pheno;
    if (g.nfact_pheno > 0.0)
       nstress_pheno = 1.0 - g.nfact_pheno;
    else
       nstress_pheno = 0.0;
    systemInterface->sendVariable(qd, nstress_pheno);  //()
}

void Plant::get_nstress_photo(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    float nstress_photo;
    if (g.nfact_photo > 0.0)
       nstress_photo = 1.0 - g.nfact_photo;
    else
       nstress_photo = 0.0;
    systemInterface->sendVariable(qd, nstress_photo);  //()
}

void Plant::get_nstress_expan(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    float nstress_expan;
    if (g.nfact_expansion > 0.0)
       nstress_expan = 1.0 - g.nfact_expansion;
    else
       nstress_expan = 0.0;
    systemInterface->sendVariable(qd, nstress_expan);  //()
}

void Plant::get_nstress_grain(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    float nstress_grain;
    if (g.nfact_grain_conc > 0.0)
       nstress_grain = 1.0 - g.nfact_grain_conc;
    else
       nstress_grain = 0.0;
    systemInterface->sendVariable(qd, nstress_grain);  //()
}


void Plant::get_parasite_c_gain(protocol::Component *system, protocol::QueryValueData &qd)
{
  float dlt_wt = g.dlt_dm_parasite + g.dm_parasite_retranslocate;
  system->sendVariable(qd, dlt_wt);
}
void Plant::get_leaf_area_tot(protocol::Component *system, protocol::QueryValueData &qd)
{
  system->sendVariable(qd, (float)sum_real_array(g.leaf_area, max_node));
}

void Plant::get_dlt_fruit_no(protocol::Component *system, protocol::QueryValueData &qd)
{
  system->sendVariable(qd, (float)sum_real_array(g.dlt_fruit_no, max_fruit_cohorts));
}
void Plant::get_dlt_fruit_flower_no(protocol::Component *system, protocol::QueryValueData &qd)
{
  system->sendVariable(qd, g.dlt_fruit_flower_no);
}
void Plant::get_dlt_fruit_site_no(protocol::Component *system, protocol::QueryValueData &qd)
{
  system->sendVariable(qd, g.dlt_fruit_site_no);
}
void Plant::get_fruit_site_no(protocol::Component *system, protocol::QueryValueData &qd)
{
  system->sendVariable(qd, g.fruit_site_no);
}
void Plant::get_fruit_no(protocol::Component *system, protocol::QueryValueData &qd)
{
  system->sendVariable(qd, (float)sum_real_array(g.fruit_no, max_fruit_cohorts));
}
void Plant::get_fruit_no_stage(protocol::Component *system, protocol::QueryValueData &qd)
{
   int cohort;
   float temp_array[max_stage];
   for (cohort = 0;cohort < max_stage; cohort++) temp_array[cohort] = 0.0;
   cohort = 0;
   do {
     int stage_no = (int)g.current_fruit_stage[cohort];
     stage_no = min(stage_no, end_grain_fill);
     temp_array[stage_no] += g.fruit_no[cohort];
     cohort++;
     } while (cohort < g.num_fruit_cohorts);
   system->sendVariable(qd, protocol::vector<float>(temp_array+initial_fruit_stage, temp_array+end_grain_fill+1));
}
void Plant::get_dm_fruit_green_cohort(protocol::Component *system, protocol::QueryValueData &qd)
{
  float sum = 0.0;
  int cohort = 0;
  do {
     sum += sum_real_array(g.dm_fruit_green[cohort], max_part);
     cohort++;
     } while (cohort < g.num_fruit_cohorts);
  system->sendVariable(qd, sum);
}
void Plant::get_dm_fruit_green_part(protocol::Component *system, protocol::QueryValueData &qd)
{
  float sum[max_part];
  for (int part = 0; part < max_part; part++)
    {
    sum[part] = 0.0;
    int cohort = 0;
    do {
       sum[part] += g.dm_fruit_green[cohort][part];
       cohort++;
       } while (cohort < g.num_fruit_cohorts);
    }
  system->sendVariable(qd, protocol::vector<float>(sum, sum+max_part));
}
void Plant::get_dlt_dm_fruit_demand(protocol::Component *system, protocol::QueryValueData &qd)
{
  system->sendVariable(qd, (float)sum_real_array(g.dlt_dm_fruit_demand, max_fruit_cohorts));
}
void Plant::get_dlt_dm_fruit_green_part(protocol::Component *system, protocol::QueryValueData &qd)
{
  float sum[max_part];
  for (int part = 0; part < max_part; part++)
    {
    sum[part] = 0.0;
    int cohort = 0;
    do {
       sum[part] += g.dlt_dm_fruit_green[cohort][part];
       cohort++;
       } while (cohort < g.num_fruit_cohorts);
    }
  system->sendVariable(qd, protocol::vector<float>(sum, sum+max_part));
}
void Plant::get_dm_fruit_green_stage(protocol::Component *system, protocol::QueryValueData &qd)
{
   int cohort;
   float temp_array[max_fruit_cohorts];
   for (cohort = 0;cohort < max_fruit_cohorts; cohort++) temp_array[cohort] = 0.0;
   cohort = 0;
   do {
       int stage_no = (int)g.current_fruit_stage[cohort];
       stage_no = min(stage_no, end_grain_fill);
       temp_array[stage_no] += sum_real_array(&g.dm_fruit_green[cohort][pod],max_part-pod);
       cohort++;
       } while (cohort < g.num_fruit_cohorts);

   system->sendVariable(qd, protocol::vector<float>(temp_array+initial_fruit_stage,
                                                    temp_array+end_grain_fill+1));
}
void Plant::get_dlt_dm_fruit_green_retrans_part(protocol::Component *system, protocol::QueryValueData &qd)
{
  float sum[max_part];
  for (int part = 0; part < max_part; part++)
    {
    sum[part] = 0.0;
    int cohort = 0;
    do {
       sum[part] += g.dlt_dm_fruit_green_retrans[cohort][part];
       cohort++;
       } while (cohort < g.num_fruit_cohorts);
    }
  system->sendVariable(qd, protocol::vector<float>(sum, sum+max_part));
}
void Plant::get_num_fruit_cohorts(protocol::Component *system, protocol::QueryValueData &qd)
{
  system->sendVariable(qd, g.num_fruit_cohorts);
}
void Plant::get_dm_parasite_retranslocate(protocol::Component *system, protocol::QueryValueData &qd)
{
  system->sendVariable(qd, g.dm_parasite_retranslocate);
}
void Plant::get_count_fruit_cohorts(protocol::Component *system, protocol::QueryValueData &qd)
{
  int count = 0;
  for (int cohort = 0; cohort < max_fruit_cohorts; cohort++)
  	  count += (g.fruit_no[cohort] > 0.0);
  system->sendVariable(qd, count);
}
void Plant::get_dlt_dm_fruit_abort_cohort(protocol::Component *system, protocol::QueryValueData &qd)
{
  float sum = 0.0;
  for (int cohort = 0; cohort < max_fruit_cohorts; cohort++)
     {
     sum += sum_real_array(g.dlt_dm_fruit_abort[cohort], max_part);
     }
  system->sendVariable(qd, sum);
}
void Plant::get_dlt_dm_fruit_abort(protocol::Component *system, protocol::QueryValueData &qd)
{
   system->sendVariable(qd, protocol::vector<float>(g.dlt_dm_green_abort, g.dlt_dm_green_abort+max_part));
}

void Plant::get_dlt_dm_fruit_abort_part(protocol::Component *system, protocol::QueryValueData &qd)
{
  int part, cohort;
  float sum[max_part];
  for (part = 0; part < max_part; part++)
    {
    sum[part] = 0.0;
    for (cohort = 0; cohort < max_fruit_cohorts; cohort++)
      sum[part] += g.dlt_dm_fruit_abort[cohort][part];
    }
  system->sendVariable(qd, protocol::vector<float>(sum, sum+max_part));
}
void Plant::get_dlt_fruit_no_abort(protocol::Component *system, protocol::QueryValueData &qd)
{
  system->sendVariable(qd, (float)sum_real_array(g.dlt_fruit_no_abort, max_fruit_cohorts));
}

///////////////
/*  Purpose
*       Calculate and provide root matter incorporation information
*       to the APSIM messaging system.
*
*  Mission Statement
*   Pass root material to the soil modules (based on root length distribution)
*/
void Plant::plant_root_incorp (float dlt_dm_root,        //(INPUT) new root residue dm (g/m^2)
                               float dlt_N_root,         //(INPUT) new root residue N (g/m^2)
                               float *g_dlayer,           //(INPUT) layer thicknesses (mm)
                               float *g_root_length,      //(INPUT) layered root length (mm)
                               float g_root_depth,       //(INPUT) root depth (mm)
                               const char *c_crop_type,   //(INPUT) crop type
                               const int max_layer)      //(INPUT) maximum no of soil layers
   {
   int deepest_layer;         // deepest layer in which the roots are growing
   float *dlt_dm_incorp = new float[max_layer]; // root residue (kg/ha)
   float *dlt_N_incorp = new float[max_layer];  // root residue N (kg/ha)
   if (dlt_dm_root > 0.0)
         {
         crop_root_dist(g_dlayer, g_root_length, g_root_depth, dlt_dm_incorp,
                        dlt_dm_root * gm2kg /sm2ha, max_layer);

         bound_check_real_array(parent,dlt_dm_incorp, max_layer, 0.0, dlt_dm_root * gm2kg / sm2ha,
                                "dlt_dm_incorp");

         crop_root_dist(g_dlayer, g_root_length, g_root_depth, dlt_N_incorp,
                        dlt_N_root * gm2kg /sm2ha, max_layer);

         bound_check_real_array(parent,dlt_N_incorp,  max_layer, 0.0, dlt_N_root * gm2kg / sm2ha,
                                "dlt_N_incorp");

         deepest_layer = find_layer_no(g_root_depth, g_dlayer, max_layer);
#ifdef PROTOCOL_WORKS_PROPERLY
         struct incorp_fom_type;
         ...
#else
         protocol::ApsimVariant outgoingApsimVariant(parent);
         outgoingApsimVariant.store("dlt_fom_type", protocol::DTstring, false,
                                    FString(c_crop_type));
         outgoingApsimVariant.store("dlt_fom_wt", protocol::DTsingle, true,
                                    protocol::vector<float>(dlt_dm_incorp, dlt_dm_incorp+deepest_layer+1));
         outgoingApsimVariant.store("dlt_fom_n", protocol::DTsingle, true,
                                    protocol::vector<float>(dlt_N_incorp, dlt_N_incorp+deepest_layer+1));
         parent->publish (id.incorp_fom, outgoingApsimVariant);
#endif
         }
    delete []  dlt_dm_incorp;
    delete []  dlt_N_incorp;
}


//+  Purpose
//       Return plant nitrogen demand for each plant component
//
//+  Mission Statement
//   Calculate the Nitrogen demand and maximum uptake for each plant pool
//
//+  Notes
//           Nitrogen required for grain growth has already been removed
//           from the stover.  Thus the total N demand is the sum of the
//           demands of the stover and roots.  Stover N demand consists of
//           two components:
//           Firstly, the demand for nitrogen by the potential new growth.
//           Secondly, the demand due to the difference between
//           the actual N concentration and the critical N concentration
//           of the tops (stover), which can be positive or negative
//
//           NOTE that this routine will not work if the root:shoot ratio
//           is broken. - NIH
//
//+  Changes
//     27-6-2003 nih taken from cproc_n_demand1
void Plant::plant_n_demand(int max_part     // (INPUT)
       , int *demand_parts                  // (INPUT)
       , int num_demand_parts               // (INPUT)
       , int grain_part_no                  //
       , float g_dlt_dm                     // (INPUT)  the daily biomass production (
       , float *g_dlt_dm_green              // (INPUT)  plant biomass growth (g/m^2)
       , float g_dlt_dm_pot_rue             // (INPUT)  potential dry matter productio
       , float *g_dm_green                  // (INPUT)  live plant dry weight (biomass
       , float *g_n_conc_crit               // (INPUT)  critical N concentration (g N/
       , float *g_n_conc_max                // (INPUT)  maximum N concentration (g N/g
       , float *g_n_green                   // (INPUT)  plant nitrogen content (g N/m^
       , float g_grain_n_demand             //
       , float c_n_deficit_uptake_fraction  //
       , float *n_demand                    // (OUTPUT) critical plant nitrogen demand  (g/m^2)
       , float *n_max)                      // (OUTPUT) max plant nitrogen demand  (g/m^2)
  {
      const char *my_name = "plant_N_demand";

      int         counter;
      float       n_crit;               // critical n amount (g/m^2)
      float       n_demand_new;         // demand for n by new growth
                                        // (g/m^2)
      float       n_demand_old;         // demand for n by old biomass
                                        // (g/m^2)
      float       n_potential;          // maximum n uptake potential (g/m^2)
      float       n_max_new;            // n required by new growth to reach
                                        // n_conc_max  (g/m^2)
      float       n_max_old;            // n required by old biomass to reach
                                        // n_conc_max  (g/m^2)
      int         part;                 // plant part
      float       dlt_dm_pot;           // potential dry weight increase
                                        // (g/m^2)
      float       part_fract;           // plant part fraction of dm  (0-1)
//- Implementation Section ----------------------------------
      push_routine (my_name);


      fill_real_array (n_demand, 0.0, max_part);
      fill_real_array (n_max, 0.0, max_part);

      for ( counter = 0; counter < num_demand_parts; counter++)
         {
         part = demand_parts[counter];

         //// need to calculate dm using potential rue not affected by
         //// n and temperature

         part_fract = divide (g_dlt_dm_green[part], g_dlt_dm, 0.0);
         dlt_dm_pot = g_dlt_dm_pot_rue * part_fract ;
         dlt_dm_pot = bound (dlt_dm_pot, 0.0, g_dlt_dm_pot_rue);

         if (g_dm_green[part]>0.0)
            {
            // get n demands due to difference between actual n concentrations
            // and critical n concentrations of tops (stover) and roots.

            n_crit       = g_dm_green[part] * g_n_conc_crit[part];
            n_potential  = g_dm_green[part] * g_n_conc_max[part];


            n_demand_old = (n_crit  - g_n_green[part]);
            if (n_demand_old>0.0)
               {
               // Don't allow demand to satisfy all deficit
               n_demand_old = n_demand_old
                        * c_n_deficit_uptake_fraction;
               }
            else
               {
               // let all extra n offset uptake
               }

            n_max_old    = (n_potential  - g_n_green[part]);
            if (n_max_old>0.0)
               {
               // Don't allow demand to satisfy all deficit
               n_max_old = n_max_old
                        * c_n_deficit_uptake_fraction;
               }
            else
               {
               // let all extra n offset uptake
               }

             // get potential n demand (critical n) of potential growth

            n_demand_new = dlt_dm_pot * g_n_conc_crit[part];
            n_max_new    = dlt_dm_pot * g_n_conc_max[part];

            n_demand[part] = n_demand_old + n_demand_new;
            n_max[part]    = n_max_old    + n_max_new;

            n_demand[part] = l_bound (n_demand[part], 0.0);
            n_max[part]    = l_bound (n_max[part], 0.0);
            }
         else
            {
            // g_dm_green[part]<=0.0
            n_demand[part] = 0.0;
            n_max[part]    = 0.0;
            }
      } // for
      n_demand[grain_part_no] = g_grain_n_demand;
      pop_routine (my_name);
   }


//  Purpose
//      Perform grain filling calculations
void Plant::plant_grain_n_demand2(
      float     g_current_stage,
      int       flowering,
      int       start_grain_fill,
      int       end_grain_fill,
      float     g_grain_no,
      float     c_potential_grain_n_filling_rate,
      float     g_maxt,
      float     g_mint,
      float     *c_x_temp_grain_n_fill,
      float     *c_y_rel_grain_n_fill,
      int       c_num_temp_grain_n_fill,
      float     *g_n_conc_min,               // (INPUT)  minimum N concentration (g N/g
      float     *g_n_conc_max,               // (INPUT)  maximum N concentration (g N/g
      float     *g_dlt_dm_green,             // (INPUT)  plant biomass growth (g/m^2)
      float     *g_dlt_dm_green_retrans,     // (INPUT)  plant biomass growth (g/m^2)
      float     *g_dm_green,                 // (INPUT)  live plant dry weight (biomass
      float     *g_n_green,                  // (INPUT)  plant nitrogen content (g N/m^
      float     c_crit_grainfill_rate,
      float     *grain_n_demand)
    {
      const char *my_name = "plant_grain_n_demand2";

      float Tav ;
      float N_potential;
      float grain_growth;
      float max_grain_n;

      push_routine (my_name);

      // default case
      *grain_n_demand = 0.0;

      if (stage_is_between(flowering, end_grain_fill, g_current_stage))
         {
         // we are in grain filling stage
         Tav = (g_maxt+g_mint)/2.0;

         *grain_n_demand = g_grain_no
                               * c_potential_grain_n_filling_rate
                               * linear_interp_real
                                      (Tav
                                      ,c_x_temp_grain_n_fill
                                      ,c_y_rel_grain_n_fill
                                      ,c_num_temp_grain_n_fill);
         }

      if (stage_is_between(start_grain_fill, end_grain_fill, g_current_stage))
         {
         // during grain C filling period so make sure that C filling is still
         // going on otherwise stop putting N in now

         grain_growth = divide(g_dlt_dm_green[meal] + g_dlt_dm_green_retrans[meal]
                              , g_grain_no
                              , 0.0);
         if (grain_growth < c_crit_grainfill_rate)
            {
            //! grain filling has stopped - stop n flow as well
            *grain_n_demand = 0.0;
            }
         }

      pop_routine (my_name);
   }

//  Purpose
//      Initialise plant weights and plant weight minimums
//      at required instances.
//
//  Mission Statement
//    Initialise plant weights and plant weight minimums at required instances.
void Plant::plant_fruit_dm_init(
                    float c_dm_leaf_init            // (INPUT)  leaf growth before emergence (
                   ,float c_dm_root_init            // (INPUT)  root growth before emergence (
                   ,float c_dm_stem_init            // (INPUT)  stem growth before emergence (
                   ,float c_pod_trans_frac          // (INPUT)  fraction of pod used in trans
                   ,int   max_part                  // (INPUT)
                   ,int   max_fruit_cohorts         // (INPUT)
                   ,float g_current_stage           // (INPUT)  current phenological stage
                   ,float*g_current_fruit_stage     // (INPUT)  current cohort phenological stage
                   ,int   g_num_fruit_cohorts       // (INPUT)
                   ,float*g_fruit_no                // (INPUT)  Fruit density (Fruit/m^2)
                   ,float g_plants                  // (INPUT/OUTPUT) plant part weights (g/m^2)
                   ,float**g_fruit_sdr_daily         //
                   ,float**dm_fruit_green           // (INPUT)  Plant density (plants/m^2)
                   ,float*dm_fruit_pod_min)         // (OUTPUT) minimum weight of each plant part (g/plant)
{
   const char *my_name = "plant_fruit_dm_init";

   //  Local Variables
   float      dm_fruit_pod;          // dry matter in pods (g/pod)

   //*- Implementation Section ----------------------------------

   push_routine (my_name);

   // initialise plant weight
   // initialisations - set up dry matter for pod etc
   if (on_day_of (emerg, g_current_stage))
      {
      // seedling has just emerged.
      // initialise fruit.
    	for (int cohort = 0; cohort < max_fruit_cohorts; cohort++)
    	   {
         dm_fruit_pod_min[cohort] = 0.0;
         for (int part=0; part< max_part; part++)
            {
            dm_fruit_green[cohort][part] = 0.0;
            }
         for (int day=0; day< 366; day++)
            {
            g_fruit_sdr_daily[cohort][day] = 0.0;
            }
         }
      dm_fruit_green[0][root] = c_dm_root_init * g_plants;
      dm_fruit_green[0][stem] = c_dm_stem_init * g_plants;
      dm_fruit_green[0][leaf] = c_dm_leaf_init * g_plants;
      }
   else if (stage_is_between(start_grain_fill, end_grain_fill, g_current_stage))
      {
      for (int cohort = 0; cohort < g_num_fruit_cohorts; cohort++)
         {
         if (on_day_of (start_grain_fill, g_current_fruit_stage[cohort]))
             {
             // we are at first day of grainfill for this cohort.
             dm_fruit_pod = divide (dm_fruit_green[cohort][pod], g_fruit_no[cohort], 0.0);
             dm_fruit_pod_min[cohort] = dm_fruit_pod * (1.0 - c_pod_trans_frac);
            }
         }
      }
   pop_routine (my_name);
}

//  Purpose
//    Use temperature, photoperiod and genetic characteristics
//    to determine when the crop begins a new growth phase.
//    The initial daily thermal time and height are also set.
//
//  Mission Statement
//  Calculate crop phenological development using thermal time targets.
void Plant::plant_fruit_phenology_update (float g_previous_stage
                                         ,float *g_current_stage             // output
                                         ,float *g_current_fruit_stage
                                         ,int   initial_stage
                                         ,int   end_development_stage
                                         ,int   start_grain_fill
                                         ,int   max_fruit_cohorts
                                         ,int   g_num_fruit_cohorts
                                         ,float c_fruit_phen_end
                                         ,float *g_fruit_no
                                         ,float *g_dlt_stage)                // output
{
   float fruit_no_cum ;
   float fruit_no_crit ;
   int  cohort;

   const char *my_name = "plant_fruit_phenology_update";
   push_routine (my_name);

   if (stage_is_between (initial_stage, end_development_stage, *g_current_stage))
      {
      fruit_no_crit = 0.0 ;
      if (stage_is_between (start_grain_fill, end_development_stage, *g_current_stage))
         {
         cohort = 0;
         do {
            if (g_current_fruit_stage[cohort] >= initial_stage)
               {
               fruit_no_crit = fruit_no_crit + g_fruit_no[cohort];
               }
            cohort++;
            } while (cohort < g_num_fruit_cohorts);
         // C1
         // fprintf(stdout, "%d %d %f\n", g.day_of_year, g_num_fruit_cohorts, fruit_no_crit);
         fruit_no_crit = fruit_no_crit * c_fruit_phen_end;
         if (fruit_no_crit > 0.0)
            {
            fruit_no_cum = 0.0;
            cohort = 0;
            do {
               fruit_no_cum = fruit_no_cum + g_fruit_no[cohort];
               if (fruit_no_cum >= fruit_no_crit)
                  {
                  if (floor(*g_current_stage) <
                      floor(g_current_fruit_stage[cohort]))
                     {
                     *g_current_stage = floor(*g_current_stage + 1.0);
                     }
                  else
                     {
                     *g_current_stage = max(*g_current_stage,
                                            g_current_fruit_stage[cohort]);
                     }
                  break;
                  }
               cohort++;
               } while (cohort < g_num_fruit_cohorts);
            }
         else
            {
            // no fruit
            *g_current_stage = g_current_fruit_stage[0];
            }
         }
      else  // sgf->ed
         {
         // we haven't reached grain fill yet
         *g_current_stage = g_current_fruit_stage[0];
         }
      if (((int)*g_current_stage) != ((int)g_previous_stage))
         {
         *g_current_stage = (int)(*g_current_stage);
         }
      *g_dlt_stage = *g_current_stage - g_previous_stage;
      } // initial-> end devel
   else
      {
      // dlt_stage is calculated for us in phenology3 - >implies
      //*g_dlt_stage = *g_dlt_stage;
      }
   if (*g_dlt_stage < 0.0)
      {
      throw std::runtime_error ("negative dlt_stage in plant_fruit_phenology_update");
      }
   // C2
   //fprintf(stdout, "%d %f %f %d %d\n", g.day_of_year, *g_dlt_stage, *g_current_stage,g_num_fruit_cohorts, cohort);

   pop_routine (my_name);
}

void Plant::plant_fruit_abort (int option)
    {
    const char*  my_name = "plant_fruit_abort" ;

    push_routine (my_name);

    if (option == 1)
        {
        // not active
        }
    else if (option == 2)
        {
        plant_fruit_no_abort(initial_fruit_stage
                           , start_grain_fill
                           , g.current_fruit_stage
                           , max_fruit_stage
                           , max_part
                           , max_fruit_cohorts
                           , c.days_assimilate_ave
                           , g.day_of_year
                           , g.year
                           , g.fruit_no
                           , g.num_fruit_cohorts
                           , p.x_stage_sdr_min
                           , p.y_sdr_min
                           , p.num_sdr_min
                           , p.dm_fruit_max
                           , c.fract_dm_fruit_abort_crit
                           , g.fruit_days_tot
                           , g.dlt_dm_fruit_demand
                           , g.dm_fruit_green
                           , g.dlt_dm_fruit_green
                           , g.dlt_dm_fruit_green_retrans
                           , g.fruit_sdr_daily
                           , g.fruit_sdr
                           , g.dlt_fruit_no_abort);

        plant_fruit_dm_abort(max_part
                           , c.dm_abort_fract
                           , max_fruit_cohorts
                           , g.num_fruit_cohorts
                           , g.fruit_no
                           , g.dm_fruit_green
                           , g.dlt_dm_fruit_green
                           , g.dlt_dm_fruit_green_retrans
                           , g.dlt_fruit_no_abort
                           , g.dlt_dm_fruit_abort);
        for (int part = 0; part < max_part; part++)
           {
           g.dlt_dm_green_retrans[part] = 0.0;
           g.dlt_dm_green_abort[part] = 0.0;
           g.dlt_dm_senesced[part] = g.dlt_dm_senesced[part]; // explicit..
           for (int cohort=0; cohort<max_fruit_cohorts; cohort++)
              {
              g.dlt_dm_green_retrans[part] += g.dlt_dm_fruit_green_retrans[cohort][part];
              g.dlt_dm_green_abort[part] += g.dlt_dm_fruit_abort[cohort][part];
              g.dlt_dm_senesced[part] += g.dlt_dm_green_abort[part];
              }
           }
         plant_fruit_n_retranslocate( max_part
                                  , c.dm_abort_fract
                                  , max_fruit_cohorts
                                  , g.num_fruit_cohorts
                                  , g.dm_fruit_green
                                  , g.n_green
                                  , g.dlt_dm_fruit_abort
                                  , g.dlt_n_retrans );
        }
    else
        {
        throw std::invalid_argument ("invalid template option");
        }

    pop_routine (my_name);
    }


void Plant::plant_fruit_cleanup (int option)
    {
    const char*  my_name = "plant_fruit_cleanup" ;

    push_routine (my_name);

    if (option == 1)
        {
        // not active
        }
    else if (option == 2)
        {
        plant_fruit_update(g.plants
                         , g.dlt_plants
                         , g.dlt_fruit_flower_no
                         , g.dlt_fruit_site_no
                         , g.dlt_fruit_no_abort
                         , g.dlt_dm_fruit_senesced
                         , g.dlt_dm_fruit_green
                         , g.dlt_dm_fruit_green_retrans
                         , g.dlt_dm_fruit_abort
                         , g.dlt_dm_parasite
                         , g.dm_fruit_green
                         , g.dm_fruit_dead
                         , g.dm_fruit_senesced
                         , g.num_fruit_cohorts
                         , g.fruit_no
                         , &g.fruit_site_no
                         , g.dlt_dm
                         , g.fruit_sdr
                         , g.day_of_year
                         , g.year
                         , g.dlt_dm_daily
                         , g.fruit_sdr_daily
                         , &g.setting_fruit);
        }
    else
        {
        throw std::invalid_argument ("invalid template option");
        }

    pop_routine (my_name);
    }


void Plant::plant_fruit_cohort_init( int   init_stage_cohort
                                    ,float g_current_stage        // stage of crop
                                    ,float *g_days_tot
                                    ,float *g_current_fruit_stage // stage of each fruit cohort
                                    ,int   *g_num_fruit_cohorts)  // current count of fruit cohorts
    {
    const char*  my_name = "plant_fruit_cohort_init" ;

    push_routine (my_name);

    if (on_day_of (init_stage_cohort, g_current_stage))
       {
       *g_num_fruit_cohorts = 1;
       g_current_fruit_stage[*g_num_fruit_cohorts-1] = (float) init_stage_cohort;
       }
    else if (*g_num_fruit_cohorts > 0)
       {
       *g_num_fruit_cohorts = *g_num_fruit_cohorts + 1;
       if (*g_num_fruit_cohorts <= max_fruit_cohorts)
           {
           g_current_fruit_stage[*g_num_fruit_cohorts-1] = (float) init_stage_cohort;
           }
       else
           {
           throw std::runtime_error ("number of fruit cohorts exceeded maximum allowed");
           }
       }
    pop_routine (my_name);
    }

//+  Purpose
//       Get change in plant fruit number

//+  Mission Statement
//       Get change in plant fruit number
void Plant::crop_fruit_site_number(float g_current_stage
                             ,float *g_days_tot
                             ,int   initial_fruit_stage
                             , int final_fruit_stage
                             , float **g_fruit_tt_tot
                             , float **g_fruit_phase_tt
                             , float p_cutout_fract
                             ,float g_plants
                             , int max_fruit_cohorts
                             , int max_stage
                             ,float *p_x_node_no_fruit_sites
                             ,float *p_y_fruit_sites_per_node
                             ,int   p_num_node_no_fruit_sites
                             ,float *g_node_no_first_flower      //IN & OUT
                             ,int   start_node_app               // (INPUT)  stage of start of fruit appeara
                             ,int   end_node_app                 // (INPUT)  stage of end of fruit appearanc
                             ,float *g_node_no                   // (INPUT)  number of fully expanded nodes
                             ,float g_maxt
                             ,float g_mint
                             ,float *c_x_temp_fruit_site
                             ,float *c_y_rel_fruit_site
                             ,int   c_num_temp_fruit_site
                             ,float g_dlt_node_no
                             ,float *dlt_fruit_site_no )         // OUT
{
    const char*  my_name = "crop_fruit_site_number" ;

    //+  Local Variables
    float fruit_sites_per_node;
    float dlt_fruit_site_no_pot;
    float node_no_now;
    float metabolic_fact;
    float fruit_tt_target,
          fruit_tt_cum,
          temp_fac,
          tav,
          dlt_node_no;

    //- Implementation Section ----------------------------------
    push_routine (my_name);

    node_no_now = sum_between (start_node_app-1, end_node_app-1, g_node_no);

    if (on_day_of (initial_fruit_stage, g_current_stage))
        {
        *g_node_no_first_flower = node_no_now;
        dlt_node_no = g_dlt_node_no;
        }
    else if (*g_node_no_first_flower > 0.0)
        {
        dlt_node_no = g_dlt_node_no;
        }
    else
        {
        dlt_node_no = 0.0;
        }

    // C3
    //fprintf(stdout, "%d %f\n", g_day_of_year, dlt_dm_average);
    fruit_sites_per_node = linear_interp_real(node_no_now
                                             ,p_x_node_no_fruit_sites
                                             ,p_y_fruit_sites_per_node
                                             ,p_num_node_no_fruit_sites);

    dlt_fruit_site_no_pot = dlt_node_no
                  * g_plants
                  * fruit_sites_per_node;

    fruit_tt_target = sum_between(initial_fruit_stage-1,
                                  final_fruit_stage-1,
                                  g_fruit_phase_tt [0])
                  * p_cutout_fract;
    fruit_tt_cum = sum_between(initial_fruit_stage-1,
                               final_fruit_stage-1,
                               g_fruit_tt_tot[0]);

    metabolic_fact = divide (fruit_tt_cum, fruit_tt_target, 0.0);
    metabolic_fact = bound (metabolic_fact, 0.0, 1.0);

    tav = (g_maxt + g_mint) / 2.0;

    temp_fac = linear_interp_real(tav
                                  ,c_x_temp_fruit_site
                                  ,c_y_rel_fruit_site
                                  ,c_num_temp_fruit_site);

    *dlt_fruit_site_no = dlt_fruit_site_no_pot
                          * (1.0 - metabolic_fact)
                          * temp_fac;
 }

// Purpose
//     Get change in plant flower number
//
// Mission Statement
//     Get change in plant flower number
//
// Changes
//     101003 jngh specified and programmed
void Plant::crop_fruit_flower_number (
      float p_dm_fruit_set_crit
     ,float p_dm_fruit_set_min
     ,float g_dlt_dm
     ,float *g_dlt_dm_daily
     ,int   c_days_assimilate_ave
     ,int   g_day_of_year
     ,int   g_year
     ,float *g_fruit_flower_no
     ,int   max_fruit_cohorts
     ,float g_dlt_fruit_site_no
     ,bool  g_setting_fruit
     ,float *dlt_flower_no  // OUTPUT
      ) {
    //  Local Variables
    float dlt_dm_average;
    float flower_no_potential;
    float dlt_flower_no_potential;
    int start_day;
    int start_year;

    start_day = offset_day_of_year(g_year, g_day_of_year, -1);
    if (g_day_of_year == 1)
         start_year = g_year - 1;
    else
         start_year = g_year;


    dlt_dm_average = crop_running_ave(start_day
                                     , start_year
                                     , g_dlt_dm_daily
                                     , c_days_assimilate_ave);

    if (g_setting_fruit)
       {
       flower_no_potential = divide (dlt_dm_average, p_dm_fruit_set_min, 0.0);
       }
    else
       {
       flower_no_potential = divide (dlt_dm_average, p_dm_fruit_set_crit, 0.0);
       }
    dlt_flower_no_potential = flower_no_potential -
                 sum_real_array(g_fruit_flower_no, max_fruit_cohorts);
    dlt_flower_no_potential = l_bound(dlt_flower_no_potential, 0.0);

    *dlt_flower_no = dlt_flower_no_potential;
    *dlt_flower_no = u_bound (*dlt_flower_no, g_dlt_fruit_site_no);

    }

// Purpose
//     Get change in plant fruit number
//
// Mission Statement
//     Get change in plant fruit number
//
// Changes
//     101003 jngh specified and programmed
void Plant::crop_fruit_number (int flowering
                             , int max_stage
                             , int max_fruit_cohorts
                             , int g_num_fruit_cohorts
                             , float c_tt_flower_to_start_pod
                             , float **g_tt_tot
                             , float *g_flower_no
                             , float *dlt_fruit_no)
    {
    //  Local Variables
    int   cohort = 0;
    do {
       if (g_flower_no[cohort] > 0.0)
            {
            if (g_tt_tot[cohort][flowering-1] >= c_tt_flower_to_start_pod)
                {
                // flowers become fruit
                dlt_fruit_no[cohort] = g_flower_no[cohort];
                }
            }
       cohort++;
       } while (cohort < g_num_fruit_cohorts);
  }
//+  Purpose
//       Get plant fruit number shed

//+  Mission Statement
//       Get plant fruit number shed
void Plant::plant_fruit_no_abort(
     int   initial_stage
    ,int   start_grain_fill
    ,float*g_current_fruit_stage
    ,int   max_fruit_stage
    ,int   max_part
    ,int   max_fruit_cohorts
    ,int   c_days_assimilate_ave
    ,int   g_day_of_year
    ,int   g_year
    ,float*g_fruit_no
    ,int   g_num_fruit_cohorts
    ,float *p_x_stage_sdr_min
    ,float *p_y_sdr_min
    ,int   p_num_sdr_min
    ,float p_dm_fruit_max
    ,float c_fract_dm_fruit_abort_crit
    ,float **g_fruit_days_tot
    ,float *g_dm_fruit_demand
    ,float **g_dm_fruit_green
    ,float **g_dlt_dm_fruit
    ,float **g_dlt_dm_fruit_retrans
    ,float **g_fruit_sdr_daily
    ,float *g_fruit_sdr
    ,float *dlt_fruit_no_abort
    )
{
    const char*  my_name = "plant_fruit_no_abort" ;


    float supply_demand_ratio;
    float survive_fract;
    float dlt_fruit_no_survive;
    int   cohort;
    int   current_phase;
    float dlt_dm_fruit;
    float dm_fruit;
    float dm_fruit_crit;
    float dlt_dm_fruit_retrans;
    float fruit_sdr_average;
    float dm_supply;
    float sdr_days_tot;
    int   days_assimilate_ave;
    int   start_day;
    int   start_year;
    float sdr_min;

    //- Implementation Section ----------------------------------
    push_routine (my_name);

    cohort = 0;
    do {
       if (g_fruit_no[cohort]> 0.0)
          {
          //slight positional difference form john for loop ordering
          dm_fruit_crit = p_dm_fruit_max
                         * c_fract_dm_fruit_abort_crit
                         * g_fruit_no[cohort];

          dm_fruit = 0.0;
          dlt_dm_fruit = 0.0;
          dlt_dm_fruit_retrans = 0.0;
          for (int part=pod; part< max_part;  part++)
             {
             dm_fruit += g_dm_fruit_green[cohort][part];
             dlt_dm_fruit += g_dlt_dm_fruit[cohort][part];
             dlt_dm_fruit_retrans += g_dlt_dm_fruit_retrans[cohort][part];
             }
          // can only abort fruit that is not in grain filling phase
          dm_supply = dlt_dm_fruit + dlt_dm_fruit_retrans;
          supply_demand_ratio = divide(dm_supply
                                     , g_dm_fruit_demand[cohort]
                                     , 1.0);

          g_fruit_sdr[cohort] = supply_demand_ratio;

          current_phase = (int)g_current_fruit_stage[cohort];

          sdr_days_tot = sum_between(initial_stage-1, current_phase-1, g_fruit_days_tot[cohort]);
          days_assimilate_ave = min(c_days_assimilate_ave, sdr_days_tot);

          start_day = offset_day_of_year(g_year, g_day_of_year, -1);

          if (g_day_of_year == 1)
               start_year = g_year - 1;
          else
               start_year = g_year;

          fruit_sdr_average = crop_running_ave(start_day
                                             , start_year
                                             , g_fruit_sdr_daily[cohort]
                                             , days_assimilate_ave-1);

//          fprintf(stdout, "%3d %3d %10.4f\n", g.day_of_year, days_assimilate_ave,
//                  fruit_sdr_average);

          fruit_sdr_average = divide(
                      fruit_sdr_average * (float)(days_assimilate_ave-1)
                      + g_fruit_sdr[cohort]
                     , days_assimilate_ave, 0.0);

          if (dm_fruit <= dm_fruit_crit)
              {
              sdr_min = linear_interp_real (g_current_fruit_stage[cohort]
                                          , p_x_stage_sdr_min
                                          , p_y_sdr_min
                                          , p_num_sdr_min);


              survive_fract = divide(fruit_sdr_average, sdr_min, 0.0);
              survive_fract = u_bound (survive_fract, 1.0);

              dlt_fruit_no_survive = g_fruit_no[cohort] * survive_fract;
              if (dlt_fruit_no_survive > tolerance_fruit_no)
                 dlt_fruit_no_abort[cohort] = g_fruit_no[cohort] - dlt_fruit_no_survive;
              else
                 dlt_fruit_no_abort[cohort] = g_fruit_no[cohort];
              }
          else
              {
              dlt_fruit_no_abort[cohort] = 0.0;
              }
          }
      else
          {
          //no fruit in this cohort
          dlt_fruit_no_abort[cohort] = 0.0;
          }
      cohort++;
      } while (cohort < g_num_fruit_cohorts);
   pop_routine (my_name);
   }


//+  Purpose
//       Get plant fruit number shed

//+  Mission Statement
//       Get plant fruit number shed
void Plant::plant_fruit_dm_abort(int   max_part
                               ,float c_dm_abort_fract
                               ,int   max_fruit_cohorts
                               ,int   g_num_fruit_cohorts
                               ,float *g_fruit_no
                               ,float **g_dm_fruit_green
                               ,float **g_dlt_dm_fruit
                               ,float **g_dm_fruit_retranslocate
                               ,float *g_dlt_fruit_no_abort
                               ,float **dlt_dm_fruit_abort)
    {
    const char*  my_name = "plant_fruit_dm_abort" ;

    //+  Local Variables
    float abort_fract;
    int   cohort;
    int   part;
    float dm_fruit_abort_retrans;
    float dm_tot_fruit;
    float dm_tot_fruit_abort;

    //- Implementation Section ----------------------------------
    push_routine (my_name);

    for (cohort=0; cohort < max_fruit_cohorts; cohort++)
       for (part = pod; part < max_part; part++)
           dlt_dm_fruit_abort[cohort][part] = 0.0;

    cohort = 0;
    do {
       if (g_fruit_no[cohort] > 0.0)
           {
           abort_fract = divide (g_dlt_fruit_no_abort[cohort]
                                , g_fruit_no[cohort], 0.0)
                                * c_dm_abort_fract;

           for (part = pod; part < max_part; part++)
              {
              dm_tot_fruit =
                     ( g_dm_fruit_green[cohort][part]
                     + g_dlt_dm_fruit[cohort][part]
                     + g_dm_fruit_retranslocate[cohort][part]);

              if (dm_tot_fruit > tolerance_dm)
                 dm_tot_fruit_abort = dm_tot_fruit * abort_fract;
              else
                 dm_tot_fruit_abort = dm_tot_fruit;

              dlt_dm_fruit_abort[cohort][part] = dm_tot_fruit_abort
                                                 * c_dm_abort_fract;

              dm_fruit_abort_retrans = dm_tot_fruit_abort
                                     - dlt_dm_fruit_abort[cohort][part];

              g_dm_fruit_retranslocate[cohort][part] =
                           g_dm_fruit_retranslocate[cohort][part]
                           - dm_fruit_abort_retrans;

              //retranslocate fractiion of aborted fruit dm to stem
              g_dm_fruit_retranslocate[0][stem] =
                           g_dm_fruit_retranslocate[0][stem]
                           + dm_fruit_abort_retrans;
              }
           }
       else
           {
           //no fruit in this cohort
           for (part = pod; part < max_part; part++)
              {
              dlt_dm_fruit_abort[cohort][part] = 0.0;
              }
           }
       cohort++;
    } while (cohort < g_num_fruit_cohorts);

    pop_routine (my_name);
    }

void Plant::plant_fruit_n_retranslocate (
           int   max_part
          ,float c_dm_abort_fract
          ,int   max_fruit_cohorts
          ,int   g_num_fruit_cohorts
          ,float **g_dm_fruit_green
          ,float *g_n_green
          ,float **g_dlt_dm_fruit_abort
          ,float *g_n_fruit_retranslocate )
    {
    int part, cohort;
    float dlt_dm_abort;
    float dm_fruit_abort_retrans;
    float dm_tot_fruit_abort;
    float dm_green;
    float green_n_conc;
    float n_fruit_abort_retrans;


    for(part = pod; part < max_part; part++)
       {
       dm_green = 0.0;
       for (cohort = 0; cohort < g_num_fruit_cohorts; cohort++)
       	 dm_green += g_dm_fruit_green[cohort][part];
       if (dm_green > tolerance_dm)
          {
          green_n_conc = divide (g_n_green[part]
                                 , dm_green
                                 , 0.0);
          dlt_dm_abort = 0.0;
          for (cohort = 0; cohort < g_num_fruit_cohorts; cohort++)
              dlt_dm_abort += g_dlt_dm_fruit_abort[cohort][part];

          dm_tot_fruit_abort = divide (dlt_dm_abort
                                      , c_dm_abort_fract
                                      , 0.0);
          dm_fruit_abort_retrans = dm_tot_fruit_abort - dlt_dm_abort;

          n_fruit_abort_retrans = dm_fruit_abort_retrans * green_n_conc;
          }
       else
          {
          n_fruit_abort_retrans = g_n_green[part];
          }
       g_n_fruit_retranslocate[part] = g_n_fruit_retranslocate[part]
                                      - n_fruit_abort_retrans;

       // retranslocate fractiion of aborted fruit N to stem
       g_n_fruit_retranslocate[stem] = g_n_fruit_retranslocate[stem]
                                      + n_fruit_abort_retrans;
       }
    }

//+  Purpose
//       Update plant fruit

//+  Mission Statement
//       Update plant fruit
void Plant::plant_fruit_update(
     float  g_plants
    ,float  g_dlt_plants
    ,float  g_dlt_fruit_flower_no
    ,float  g_dlt_fruit_site_no
    ,float  *g_dlt_fruit_no_abort
    ,float  **g_dlt_dm_fruit_senesced
    ,float  **g_dlt_dm_fruit_green
    ,float  **g_dlt_dm_fruit_retrans
    ,float  **g_dlt_dm_fruit_abort
    ,float  g_dlt_dm_parasite
    ,float  **g_dm_fruit_green
    ,float  **g_dm_fruit_dead
    ,float  **g_dm_fruit_senesced
    ,int    g_num_fruit_cohorts
    ,float  *g_fruit_no
    ,float  *g_fruit_site_no      // IN/OUT
    ,float  g_dlt_dm
    ,float  *g_fruit_sdr
    ,int    g_day_of_year
    ,int    g_year
    ,float  *g_dlt_dm_daily
    ,float  **g_fruit_sdr_daily
    ,bool   *g_setting_fruit)     //IN/OUT
{
    //+  Constant Values
    const char*  my_name = "plant_fruit_update" ;

    //+  Local Variables
    int   part;
    int   cohort;
    float dying_fract_plants;
    float dlt_dm_green_dead;
    float dlt_dm_senesced_dead;
    float dlt_fruit_no_lost;
    float dlt_dm_veg;

//- Implementation Section ----------------------------------

    push_routine (my_name);

    *g_fruit_site_no  = *g_fruit_site_no  + g_dlt_fruit_site_no;
    //fprintf(stdout, "%3d %10f\n", g.day_of_year, *g_fruit_site_no);
    if (g_dlt_fruit_flower_no <= 0.0 && g_setting_fruit)
       {
       *g_setting_fruit = false;
       }
    else if (g_dlt_fruit_flower_no > 0.0 && !g_setting_fruit)
       {
       *g_setting_fruit = true;
       }
    else
       {
       // carry on with current fruit setting status
       }

    dlt_dm_veg = 0.0;
    cohort = 0;
    do {
       for (part = leaf; part <= stem; part++)
          dlt_dm_veg += g_dlt_dm_fruit_green[cohort][part];
       cohort++;
       } while (cohort < g_num_fruit_cohorts);
    crop_store_value(g_day_of_year, g_year, g_dlt_dm_daily,
                     dlt_dm_veg + g_dlt_dm_parasite);

    // C4
    //fprintf(stdout, "%d %f\n", g_day_of_year,   g_dlt_dm);
    dying_fract_plants = divide (-g_dlt_plants, g_plants, 0.0);
    dying_fract_plants = bound (dying_fract_plants, 0.0, 1.0);

    if (g_num_fruit_cohorts > 0)
        {
        cohort = 0;
        do {
           if (g_fruit_no[cohort] > 0.0)
              crop_store_value(g_day_of_year, g_year
                          , g_fruit_sdr_daily[cohort]
                          , g_fruit_sdr[cohort]);

           // transfer plant grain no.
           g_fruit_no[cohort] = g_fruit_no[cohort] - g_dlt_fruit_no_abort[cohort];
           dlt_fruit_no_lost  = g_fruit_no[cohort] * dying_fract_plants;
           g_fruit_no[cohort] = g_fruit_no[cohort] - dlt_fruit_no_lost;

           for (part = 0; part < max_part; part++)
              {
              g_dm_fruit_green[cohort][part] =
                  g_dm_fruit_green[cohort][part]
                  + g_dlt_dm_fruit_green[cohort][part]
                  + g_dlt_dm_fruit_retrans[cohort][part]
                  - g_dlt_dm_fruit_senesced[cohort][part]
                  - g_dlt_dm_fruit_abort[cohort][part];

              dlt_dm_green_dead = g_dm_fruit_green[cohort][part] * dying_fract_plants;
              g_dm_fruit_green[cohort][part] = g_dm_fruit_green[cohort][part] - dlt_dm_green_dead;
              g_dm_fruit_dead[cohort][part] = g_dm_fruit_dead[cohort][part] + dlt_dm_green_dead;

              dlt_dm_senesced_dead = g_dm_fruit_senesced[cohort][part] * dying_fract_plants;
              g_dm_fruit_senesced[cohort][part] = g_dm_fruit_senesced[cohort][part] - dlt_dm_senesced_dead;
              g_dm_fruit_dead[cohort][part] = g_dm_fruit_dead[cohort][part] + dlt_dm_senesced_dead;
              }
            cohort++;
            } while (cohort < g_num_fruit_cohorts);

        }
    else
        {
        for (part = 0; part < max_part; part++)
           {
           g_dm_fruit_green[0][part] =
                 g_dm_fruit_green[0][part]
                 + g_dlt_dm_fruit_green[0][part]
                 + g_dlt_dm_fruit_retrans[0][part]
                 - g_dlt_dm_fruit_senesced[0][part];

           dlt_dm_green_dead = g_dm_fruit_green[0][part] * dying_fract_plants;
           g_dm_fruit_green[0][part] =
                g_dm_fruit_green[0][part]
                - dlt_dm_green_dead;
           g_dm_fruit_dead[0][part] =
                g_dm_fruit_dead[0][part]
                + dlt_dm_green_dead;

           dlt_dm_senesced_dead = g_dm_fruit_senesced[0][part] * dying_fract_plants;
           g_dm_fruit_senesced[0][part] =
                g_dm_fruit_senesced[0][part]
                - dlt_dm_senesced_dead;
           g_dm_fruit_dead[0][part] =
                g_dm_fruit_dead[0][part]
                + dlt_dm_senesced_dead;
           }
       }
    pop_routine (my_name);
    }


//+  Purpose
//       Returns cumulative thermal time targets required for the
//       individual growth stages.

//+  Mission Statement
//     Get the cumulative thermal time targets for growth phases
void Plant::plant_fruit_phenology_init(
     float c_twilight                          // (INPUT)  twilight in angular distance b
    ,float *g_current_stage                    // (INPUT)  current phenological stage
    ,float **g_days_tot                        // (INPUT)  duration of each phase (days)
    ,int   max_fruit_stage
    ,int   max_fruit_cohorts
    ,float g_num_fruit_cohorts
    ,float g_day_of_year                       // (INPUT)  day of year
    ,float g_latitude                          // (INPUT)  latitude (degrees, negative fo
    ,float *p_x_pp_flower_to_start_grain       // (INPUT)
    ,float *p_y_tt_flower_to_start_grain       // (INPUT)
    ,float p_num_pp_flower_to_start_grain      // (INPUT)
    ,float *p_x_pp_start_to_end_grain          // (INPUT)
    ,float *p_y_tt_start_to_end_grain          // (INPUT)
    ,float p_num_pp_start_to_end_grain         // (INPUT)
    ,float p_tt_end_grain_to_maturity          // (INPUT)
    ,float p_tt_maturity_to_ripe               // (INPUT)  growing deg day required to fo
    ,float **phase_tt)                         // (INPUT/OUTPUT) cumulative growing
                                               // degree days required for
                                               // each stage (deg days)
{
    const char*  my_name = "plant_fruit_phenology_init" ;

    float photoperiod;                            // hours of photosynthetic light (hours)
    int   cohort;

    //- Implementation Section ----------------------------------

    push_routine (my_name);

    photoperiod = day_length (g_day_of_year
                            , g_latitude
                            , c_twilight);

    phase_tt[g.num_fruit_cohorts][flower_to_start_grain-1] =
          linear_interp_real(photoperiod
                            ,p_x_pp_flower_to_start_grain
                            ,p_y_tt_flower_to_start_grain
                            ,p_num_pp_flower_to_start_grain);

    cohort = 0;
    do {
       if (stage_is_between (flowering, start_grain_fill, g_current_stage[cohort]))
           {
           if (on_day_of(flowering, g_current_stage[cohort]))
               {
               phase_tt[cohort][end_grain_to_maturity-1] = p_tt_end_grain_to_maturity;
               phase_tt[cohort][maturity_to_ripe-1] = p_tt_maturity_to_ripe;
               }
           phase_tt[cohort][flower_to_start_grain-1] = linear_interp_real
                                                       (photoperiod
                                                       ,p_x_pp_flower_to_start_grain
                                                       ,p_y_tt_flower_to_start_grain
                                                       ,p_num_pp_flower_to_start_grain);
           phase_tt[cohort][start_to_end_grain-1] = linear_interp_real
                                                   (photoperiod
                                                   ,p_x_pp_start_to_end_grain
                                                   ,p_y_tt_start_to_end_grain
                                                   ,p_num_pp_start_to_end_grain);
           }
      else if (stage_is_between (start_grain_fill, end_grain_fill, g_current_stage[cohort]))
           {
           phase_tt[cohort][start_to_end_grain-1] = linear_interp_real
                                                    (photoperiod
                                                    ,p_x_pp_start_to_end_grain
                                                    ,p_y_tt_start_to_end_grain
                                                    ,p_num_pp_start_to_end_grain);
           }
      cohort++;
      } while (cohort < g_num_fruit_cohorts);
   pop_routine (my_name);
   }


//+  Purpose
//     Use temperature, photoperiod and genetic characteristics
//     to determine when the crop begins a new growth phase.
//     The initial daily thermal time and height are also set.

//+  Mission Statement
//   Calculate crop phenological development using thermal time targets.
void Plant::plant_fruit_phenology (
     float *g_previous_stage
    ,float *g_current_stage
    ,int   initial_stage
    ,int   end_development_stage
    ,int   start_grain_fill
    ,int   end_grain_fill
    ,int   max_stage
    ,int   max_fruit_cohorts
    ,int   g_num_fruit_cohorts
    ,float **g_dm_fruit_green
    ,int   p_dm_fruit_max
    ,float *g_fruit_no
    , int   c_num_temp
    , float *c_x_temp
    , float *c_y_tt
    , float g_maxt
    , float g_mint
    , float g_swdef_pheno_flower
    , float g_swdef_pheno_grainfill
    ,float **g_phase_tt
    ,float *g_phase_devel
    ,float *g_dlt_tt
    ,float *g_dlt_stage
    ,float **g_tt_tot
    ,float **g_days_tot)
{
    int   cohort;
    const char*  my_name = "plant_fruit_phenology" ;

    //- Implementation Section ----------------------------------

    push_routine (my_name);

    cohort = 0;
    do {
       g_previous_stage[cohort] = g_current_stage[cohort];
       if (stage_is_between(initial_stage
                           , start_grain_fill
                           ,g_current_stage[cohort]))
          {
          crop_thermal_time(c_num_temp
                          , c_x_temp
                          , c_y_tt
                          , g_current_stage[cohort]
                          , g_maxt
                          , g_mint
                          , initial_stage
                          , start_grain_fill
                          , g_swdef_pheno_flower    // no nstress, so just repeat swdef stress here
                          , g_swdef_pheno_flower
                          , &g_dlt_tt[cohort]);
          }
       else if (stage_is_between(start_grain_fill
                                , end_development_stage
                                , g_current_stage[cohort]))
          {
          crop_thermal_time (c_num_temp
                           , c_x_temp
                           , c_y_tt
                           , g_current_stage[cohort]
                           , g_maxt
                           , g_mint
                           , start_grain_fill
                           , end_development_stage
                           , g_swdef_pheno_grainfill    // no nstress, so just repeat swdef stress here
                           , g_swdef_pheno_grainfill
                           , &g_dlt_tt[cohort]);
          }

       plant_fruit_phase_devel(initial_stage
                               , end_development_stage
                               , start_grain_fill
                               , end_grain_fill
                               , g_current_stage[cohort]
                               , g_dm_fruit_green[cohort]
                               , p_dm_fruit_max
                               , g_fruit_no[cohort]
                               , g_dlt_tt[cohort]
                               , g_phase_tt[cohort]
                               , g_tt_tot[cohort]
                               , &g_phase_devel[cohort]);

       crop_devel(max_stage
                   , g_phase_devel[cohort]
                   , &g_dlt_stage[cohort]
                   , &g_current_stage[cohort]);

       // update thermal time states and day count
       accumulate (g_dlt_tt[cohort], g_tt_tot[cohort]
                    , g_previous_stage[cohort]-1.0, g_dlt_stage[cohort]);

       accumulate (1.0, g_days_tot[cohort]
                    , g_previous_stage[cohort]-1.0, g_dlt_stage[cohort]);
       cohort++;
    } while (cohort < g_num_fruit_cohorts);

    pop_routine (my_name);
  }

//+  Purpose
//     Determine the fraction of current phase elapsed ().

//+  Mission statement
//   Determine the progress through the current growth stage.

void Plant::plant_fruit_phase_devel( int    initial_stage                  // (INPUT)
                              ,int    end_development_stage          // (INPUT)
                              ,int    start_grain_fill               // (INPUT)
                              ,int    end_grain_fill                 // (INPUT)
                              ,float  g_current_stage                // (INPUT)  current phenological stage
                              ,float  *g_dm_fruit_green              // (INPUT)
                              ,float  p_dm_fruit_max                 // (INPUT)
                              ,float  g_fruit_no                     // (INPUT)
                              ,float  g_dlt_tt                       // (INPUT)  daily thermal time (growing de
                              ,float  *g_phase_tt                    // (INPUT)  Cumulative growing degree days
                              ,float  *g_tt_tot                      // (INPUT)  the sum of growing degree days
                              ,float  *phase_devel)                   // (OUTPUT) fraction of current phase elapsed
    {
    //+  Local variables
    float phase_devel_grain;
    const char*  my_name = "fruit_phase_devel" ;

    //- Implementation Section ----------------------------------
    push_routine (my_name);

    if (stage_is_between (initial_stage, end_development_stage, g_current_stage))
        {
        *phase_devel =  crop_phase_tt( g_dlt_tt
                                    , g_phase_tt
                                    , g_tt_tot
                                    , g_current_stage);
        }
    else
        {
        *phase_devel = fmod(g_current_stage, 1.0);
        }


   if (stage_is_between (start_grain_fill, end_grain_fill, g_current_stage))
       {
       phase_devel_grain = divide (sum_real_array(&g_dm_fruit_green[pod]
                                                  ,oil-pod)
                                  ,p_dm_fruit_max*g_fruit_no, 0.0);
       }
   else
       {
       phase_devel_grain = 0.0;
       }


   *phase_devel = max(*phase_devel, phase_devel_grain);
   *phase_devel = bound(*phase_devel, 0.0, 1.0);

   pop_routine (my_name);
   }


//+  Purpose
//       Zero crop variables & arrays

//+  Mission Statement
//     Zero all the global variables and arrays
void Plant::fruit_zero_all_globals ()
    {
    const char*  my_name = "fruit_zero_all_globals" ;

    //- Implementation Section ----------------------------------

    push_routine (my_name);

    // Fruit Globals
    for (int i=0; i < max_fruit_cohorts; i++) {
      g.current_fruit_stage[i]    = 0.0;
    	for (int j = 0; j < max_fruit_stage; j++) {
        g.fruit_phase_tt[i][j]         = 0.0;
        g.fruit_days_tot[i][j]         = 0.0;
      }
    }
    pop_routine (my_name);
    }



//+  Purpose
//       Perform grain filling calculations
void Plant::legnew_bio_yieldpart_demand3(
        int   max_fruit_cohorts,                           // (INPUT)
        int   max_part,                                    // (INPUT)
        int   max_fruit_stage,                             // (INPUT)
        int   g_num_fruit_cohorts,                         // (INPUT)
        float *g_current_stage,                            // (INPUT)
        int   flowering,                                   // (INPUT)
        int   start_grain_fill,                            // (INPUT)
        int   end_grain_fill,                              // (INPUT)
        float *y_tt_flower_to_start_grain,                 // (INPUT)
        float *y_tt_fruit_start_to_end_grain,              // (INPUT)
        float *g_dlt_tt,                                    // (INPUT)
        float **g_tt_tot,                                  // (INPUT)
        float *g_fruit_no,                                 // (INPUT)
        float p_potential_fruit_filling_rate,              // (INPUT)
        float p_dm_fruit_max,                              // (INPUT)
        float **g_dm_fruit_green,                          // (INPUT)
        float g_maxt,                                      // (INPUT)
        float g_mint,                                      // (INPUT)
        float *c_x_temp_grainfill,                         // (INPUT)
        float *c_y_rel_grainfill,                          // (INPUT)
        int   c_num_temp_grainfill,                        // (INPUT)
        float *c_x_stage_no_partition,                     // (INPUT)
        float *c_y_frac_pod,                               // (INPUT)
        int   c_num_stage_no_partition,
        float c_tt_flower_to_start_pod,
        float **g_fruit_phase_tt,                          // INPUT
        float g_grain_energy,                              // (INPUT)
        float *dlt_dm_fruit_demand,                         //(OUTPUT)
        float *dlt_dm_grain_demand)                         //(OUTPUT)
{
    const char*  my_name = "legnew_bio_yieldpart_demand3" ;

    //+  Local Variables
    float tav;
    float dlt_dm_yield_unadj;                     // grain demand for carbohydrate, unadjusted
                                                  // for energy conversion (g/m^2)
    float potential_grain_filling_rate;
    float temp_fac;
    float dlt_dm_pod_demand;
    float dm_max;
    float dm_pod_max;
    float tt_fruit_age, tt_fruit_age_max, m1, m2;
    float dlt_fruit_age;
//    int   current_phase;
    int   cohort;
    float dummy[1];
    float dlt_dm_fruit_max;
    float pod_age_fract;
    float frac_pod;
    //- Implementation Section ----------------------------------
    push_routine (my_name);

    m1 = y_tt_flower_to_start_grain[0];
    for (int i = 1; i < max_table; i++)
       m1 = max(y_tt_flower_to_start_grain[i], m1);

    m2 =y_tt_fruit_start_to_end_grain[0];
    for (int i = 1; i < max_table; i++)
       m2 = max(y_tt_fruit_start_to_end_grain[i], m2);

    tt_fruit_age_max = m1 + m2;

    tav = (g_maxt + g_mint) / 2.0;
    temp_fac = linear_interp_real(tav
                                  ,c_x_temp_grainfill
                                  ,c_y_rel_grainfill
                                  ,c_num_temp_grainfill);

    cohort = 0;
    do {
       if (g_fruit_no[cohort] > 0.0)
           {
           if (stage_is_between(flowering, start_grain_fill, g_current_stage[cohort]))
               {
               // we are in flowering stage
               dlt_fruit_age = divide(g_dlt_tt[cohort],
                                      tt_fruit_age_max, 0.0);
               frac_pod = linear_interp_real(g_current_stage[cohort]
                                            ,c_x_stage_no_partition
                                            ,c_y_frac_pod
                                            ,c_num_stage_no_partition);
               dm_max = p_dm_fruit_max
                      * temp_fac
                      * g_fruit_no[cohort]
                      * frac_pod;

               dlt_dm_pod_demand = dm_max * dlt_fruit_age;

               dlt_dm_fruit_demand[cohort] = dlt_dm_pod_demand;
               dlt_dm_grain_demand[cohort] = 0.0;
               }
           else if (stage_is_between(start_grain_fill,end_grain_fill,g_current_stage[cohort]))
               {
               // we are in grain filling stage
               frac_pod = linear_interp_real(g_current_stage[cohort]
                                            ,c_x_stage_no_partition
                                            ,c_y_frac_pod
                                            ,c_num_stage_no_partition);

               potential_grain_filling_rate = divide(p_potential_fruit_filling_rate
                                                     , 1.0 + frac_pod
                                                     , 0.0);

               dlt_dm_yield_unadj = g_fruit_no[cohort]
                                       * potential_grain_filling_rate
                                       * temp_fac
                                       * g_dlt_tt[cohort];

              // adjust for grain energy
              dlt_dm_grain_demand[cohort] = dlt_dm_yield_unadj * g_grain_energy;
              dlt_dm_pod_demand = dlt_dm_yield_unadj * frac_pod;
              dlt_dm_fruit_demand[cohort] = dlt_dm_pod_demand + dlt_dm_grain_demand[cohort];
              dlt_dm_fruit_max = g_fruit_no[cohort] * p_dm_fruit_max
                      - sum_real_array(&g_dm_fruit_green[cohort][pod], max_part-pod);
              dlt_dm_fruit_max = l_bound (dlt_dm_fruit_max, 0.0);

              // adjust the demands
              if (dlt_dm_fruit_demand[cohort] > dlt_dm_fruit_max)
                  {
                  dlt_dm_grain_demand[cohort] = dlt_dm_grain_demand[cohort]
                                                 * divide (dlt_dm_fruit_max
                                                           , dlt_dm_fruit_demand[cohort]
                                                           , 0.0);
                  dlt_dm_fruit_demand[cohort] = dlt_dm_fruit_max;
                  }
              }
           else
              {
              // no changes
              dlt_dm_grain_demand[cohort] = 0.0;
              dlt_dm_fruit_demand[cohort] = 0.0;
              }
           }
       else
           {
           // no fruit
           dlt_dm_grain_demand[cohort] = 0.0;
           dlt_dm_fruit_demand[cohort] = 0.0;
           }
       cohort++;
       } while (cohort < g_num_fruit_cohorts);
       pop_routine (my_name);
   }

// NB. "5.2" is half way between FI and flag leaf in wheat
void Plant::get_zadok_stage(protocol::Component *system, protocol::QueryValueData &qd)
{
    float zadok_stage = 0.0;

    if (g.current_stage >= sowing &&
        g.current_stage <= emerg)
       {
       zadok_stage = 5.0 * (g.current_stage - sowing);
       }
    else if (g.current_stage > emerg &&
             g.current_stage <= 4.9)
       {
       float leaf_no_now = sum_between (emerg-1, now-1, g.leaf_no);

       static float tillerno_y[] =   // tiller no.
           {0, 0, 5};
       static float tillerno_x[] =   // lf. no.
           {0, 5, 8};
       float tiller_no_now = linear_interp_real (leaf_no_now
                                               , tillerno_x
                                               , tillerno_y
                                               , sizeof(tillerno_x)/sizeof(float));
       if (tiller_no_now <= 0.0)
           {
           zadok_stage = 10.0 + leaf_no_now;
           }
        else
           {
           zadok_stage = 20.0 + tiller_no_now;
           }
       }
    else if (g.current_stage > 4.9 &&
             g.current_stage < plant_end )
       {
// from senthold's archive:
//1    2    3         4.5     5       6
//eme  ej   eveg(fl)  anth    sgf   mat
//10   30   43 	     65      70     9

// from CropMod
//                 sow ger eme  juv    fi   flag    fl st_gf end_gf mat hv_rpe end_crop
//stage_code       = 1   2   3    4      5     6     7    8    9    10   11   12
//Zadok_stage      = 0   5  10   10     15    40    60   71   87    90   93  100

// Plant:
//                 sow    ger   eme  juv    fi       fl   st_gf end_gf  mat hv_rpe  end
//stage_code      = 1      2    3     4     5   4.9 5.4   6     7      8     9    10     11  ()     ! numeric code for phenological stages
//                  na     na   na    na    na   30  43   65    71     87    90    100

       static float zadok_code_y[] =
           {30,   40, 65, 71, 87, 90, 100};
       static float zadok_code_x[] =
           {4.9, 5.4,  6,  7,  8,  9,  10};

      zadok_stage = linear_interp_real (g.current_stage
                                       , zadok_code_x
                                       , zadok_code_y
                                       , sizeof(zadok_code_x)/sizeof(float));
       }
    system->sendVariable(qd, zadok_stage);
}


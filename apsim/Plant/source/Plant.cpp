#include <general/pch.h>
#include <vcl.h>
#include <math.h>
#pragma hdrstop

#include <ComponentInterface/Type.h>
#include <ComponentInterface/ApsimVariant.h>
#include <ComponentInterface/Component.h>
#include <ComponentInterface/dataTypes.h>
#include <ApsimShared/ApsimComponentData.h>
#include <ApsimShared/FStringExt.h>
#include <general/string_functions.h>

#include <map>
#include <string>

using namespace std;

#include "PlantLibrary.h"
#include "PlantComponent.h"
#include "Plant.h"

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
vector<string> part_name;


/////////////Thes might be redundancies??//////////
void push_routine (const char *) {};
void pop_routine (const char *) {};

// Called from fortran world
extern "C" int fatal_error(const int */*code*/, char *str, int str_length) {
    str[str_length] = '\0';
    if (currentInstance) {currentInstance->error(str,1);}
    return 0;
}

extern "C" int warning_error(const int */*code*/, char *str, int str_length) {
    str[str_length] = '\0';
    if (currentInstance) {currentInstance->error(str, 0);}
    return 0;
}

void Write_string (char *line) {
    if (currentInstance) {currentInstance->write_string(line);}
}
/////////////////////////

// Floating point to ascii (string). 
string Ftoa(double Float, int Num_decplaces, int Width)
   {
   char fbuf[80], buf[80];
   sprintf(fbuf, "%%%d.%df", Width, Num_decplaces);
   sprintf(buf, fbuf, Float);
   return(string(buf));
   }
std::string Ftoa(double Float, int Num_decplaces)
  {
  return(Ftoa(Float, Num_decplaces, 12));
  }

//////////////////////
void Plant::doInit(void)
  {
  currentInstance = this;
  doIDs();                 // Gather IDs for getVariable requests
  plant_read_constants (); // Read constants
  plant_zero_variables (); // Zero global states
  plant_init ();           // Site specific init
  plant_get_other_variables (); // sw etc..
  }
void Plant::doIDs(void)
   {
       // gets
       id.eo = parent->addRegistration(protocol::getVariableReg,
                                       "eo", floatType,
                                       "", "");
       id.fr_intc_radn = parent->addRegistration(protocol::getVariableReg,
                                       "fr_intc_radn", floatType,
                                       "", "");
       id.sw_dep= parent->addRegistration(protocol::getVariableReg,
                                       "sw_dep", floatArrayType,
                                       "", "");
       id.no3 = parent->addRegistration(protocol::getVariableReg,
                                       "no3", floatArrayType,
                                       "", "");
       id.no3_min= parent->addRegistration(protocol::getVariableReg,
                                       "no3_min", floatArrayType,
                                       "", "");
       id.latitude = parent->addRegistration(protocol::getVariableReg,
                                       "latitude", floatType,
                                       "", "");

       // sets
       id.dlt_no3 = parent->addRegistration(protocol::setVariableReg,
                                       "dlt_no3", floatArrayType,
                                       "", "");
       id.dlt_sw_dep = parent->addRegistration(protocol::setVariableReg,
                                       "dlt_sw_dep", floatArrayType,
                                       "", "");

       // events.
       id.crop_chopped = parent->addRegistration(protocol::eventReg,
                                       "crop_chopped", "",
                                       "", "");
       id.incorp_fom = parent->addRegistration(protocol::eventReg,
                                       "incorp_fom", "",
                                       "", "");
  }

// Register Methods, Events,
void Plant::doRegistrations(void)
   {
   unsigned id;

   //id = parent->addRegistration(protocol::respondToMethodCallReg, "create", "");
   //IDtoEventFn.insert(UInt2EventFnMap::value_type(id,&Plant::doCreate)); /* unused */

   //id = parent->addRegistration(protocol::respondToMethodCallReg, "sysinit", "");
   //IDtoEventFn.insert(UInt2EventFnMap::value_type(id,&Plant::doSysInit)); /* unused */

   id = parent->addRegistration(protocol::respondToEventReg, "prepare", "");
   IDtoEventFn.insert(UInt2EventFnMap::value_type(id,&Plant::doPrepare));

   id = parent->addRegistration(protocol::respondToEventReg, "process", "");
   IDtoEventFn.insert(UInt2EventFnMap::value_type(id,&Plant::doProcess));

   id = parent->addRegistration(protocol::respondToEventReg, "tick", "");
   IDtoEventFn.insert(UInt2EventFnMap::value_type(id,&Plant::doTick));

   id = parent->addRegistration(protocol::respondToEventReg, "newmet", "");
   IDtoEventFn.insert(UInt2EventFnMap::value_type(id,&Plant::doNewMet));

   id = parent->addRegistration(protocol::respondToEventReg, "new_profile", "");
   IDtoEventFn.insert(UInt2EventFnMap::value_type(id,&Plant::doNewProfile));

   id = parent->addRegistration(protocol::respondToEventReg, "sow", "");
   IDtoEventFn.insert(UInt2EventFnMap::value_type(id,&Plant::doSow));
   id = parent->addRegistration(protocol::respondToMethodCallReg, "sow", "");
   IDtoEventFn.insert(UInt2EventFnMap::value_type(id,&Plant::doSow));

   id = parent->addRegistration(protocol::respondToEventReg, "harvest", "");
   IDtoEventFn.insert(UInt2EventFnMap::value_type(id,&Plant::doHarvest));
   id = parent->addRegistration(protocol::respondToMethodCallReg, "harvest", "");
   IDtoEventFn.insert(UInt2EventFnMap::value_type(id,&Plant::doHarvest));

   id = parent->addRegistration(protocol::respondToEventReg, "end_crop", "");
   IDtoEventFn.insert(UInt2EventFnMap::value_type(id,&Plant::doEndCrop));
   id = parent->addRegistration(protocol::respondToMethodCallReg, "end_crop", "");
   IDtoEventFn.insert(UInt2EventFnMap::value_type(id,&Plant::doEndCrop));

   id = parent->addRegistration(protocol::respondToEventReg, "end_crop", "");
   IDtoEventFn.insert(UInt2EventFnMap::value_type(id,&Plant::doEndCrop));
   id = parent->addRegistration(protocol::respondToMethodCallReg, "end_crop", "");
   IDtoEventFn.insert(UInt2EventFnMap::value_type(id,&Plant::doEndCrop));

   id = parent->addRegistration(protocol::respondToEventReg, "end_run", "");
   IDtoEventFn.insert(UInt2EventFnMap::value_type(id,&Plant::doEndRun));
   id = parent->addRegistration(protocol::respondToMethodCallReg, "end_run", "");
   IDtoEventFn.insert(UInt2EventFnMap::value_type(id,&Plant::doEndRun));

   id = parent->addRegistration(protocol::respondToEventReg, "kill_stem", "");
   IDtoEventFn.insert(UInt2EventFnMap::value_type(id,&Plant::doKillStem));
   id = parent->addRegistration(protocol::respondToMethodCallReg, "kill_stem", "");
   IDtoEventFn.insert(UInt2EventFnMap::value_type(id,&Plant::doKillStem));

#if 0
//replaced - this is done when reading ini file
   id = parent->addRegistration(protocol::respondToEventReg, "reduce", "");
   IDtoEventFn.insert(UInt2EventFnMap::value_type(id,&Plant::doAutoClassChange));
   id = parent->addRegistration(protocol::respondToMethodCallReg, "reduce", "");
   IDtoEventFn.insert(UInt2EventFnMap::value_type(id,&Plant::doAutoClassChange));

   id = parent->addRegistration(protocol::respondToMethodCallReg, "spray", "");
   IDtoEventFn.insert(UInt2EventFnMap::value_type(id,&Plant::doAutoClassChange));

   id = parent->addRegistration(protocol::respondToMethodCallReg, "delay", "");
   IDtoEventFn.insert(UInt2EventFnMap::value_type(id,&Plant::doAutoClassChange));

   id = parent->addRegistration(protocol::respondToMethodCallReg, "spring", "");
   IDtoEventFn.insert(UInt2EventFnMap::value_type(id,&Plant::doAutoClassChange));

   id = parent->addRegistration(protocol::respondToMethodCallReg, "dormancy", "");
   IDtoEventFn.insert(UInt2EventFnMap::value_type(id,&Plant::doAutoClassChange));
#endif

   // Send My Variable
   id = parent->addRegistration(protocol::respondToGetReg, "plant_status", stringType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_plant_status));
   id = parent->addRegistration(protocol::respondToGetReg, "dlt_stage", floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_dlt_stage));
   id = parent->addRegistration(protocol::respondToGetReg, "stage"    , floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_stage));
   id = parent->addRegistration(protocol::respondToGetReg, "stage_code", floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_stage_code));
   id = parent->addRegistration(protocol::respondToGetReg, "stage_name", stringType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_stage_name));
   id = parent->addRegistration(protocol::respondToGetReg, "crop_type" , stringType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_crop_type));
   id = parent->addRegistration(protocol::respondToGetReg, "crop_class", stringType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_crop_class));
   id = parent->addRegistration(protocol::respondToGetReg, "dlt_tt",     floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_dlt_tt));
   id = parent->addRegistration(protocol::respondToGetReg, "phase_tt",   floatArrayType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_phase_tt));
   id = parent->addRegistration(protocol::respondToGetReg, "tt_tot",     floatArrayType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_tt_tot));
   id = parent->addRegistration(protocol::respondToGetReg, "days_tot",   floatArrayType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_days_tot));
   id = parent->addRegistration(protocol::respondToGetReg, "das",        integerType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_das));
   id = parent->addRegistration(protocol::respondToGetReg, "cum_vernal_days", floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_cum_vernal_days));
   id = parent->addRegistration(protocol::respondToGetReg, "flowering_date",  integerType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_flowering_date));
   id = parent->addRegistration(protocol::respondToGetReg, "maturity_date",   integerType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_maturity_date));
   id = parent->addRegistration(protocol::respondToGetReg, "flowering_das",   integerType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_flowering_das));
   id = parent->addRegistration(protocol::respondToGetReg, "maturity_das",    integerType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_maturity_das));
   id = parent->addRegistration(protocol::respondToGetReg, "leaf_no",         floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_leaf_no));
   id = parent->addRegistration(protocol::respondToGetReg, "node_no",         floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_node_no));
   id = parent->addRegistration(protocol::respondToGetReg, "dlt_leaf_no",     floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_dlt_leaf_no));
   id = parent->addRegistration(protocol::respondToGetReg, "dlt_node_no",     floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_dlt_node_no));
   id = parent->addRegistration(protocol::respondToGetReg, "leaf_no_dead",    floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_leaf_no_dead));
   id = parent->addRegistration(protocol::respondToGetReg, "leaf_area",       floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_leaf_area));
   id = parent->addRegistration(protocol::respondToGetReg, "height",          floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_height));
   id = parent->addRegistration(protocol::respondToGetReg, "width",           floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_width));
   id = parent->addRegistration(protocol::respondToGetReg, "root_depth",      floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_root_depth));
   id = parent->addRegistration(protocol::respondToGetReg, "plants",          floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_plants));
   id = parent->addRegistration(protocol::respondToGetReg, "cover_green",     floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_cover_green));
   id = parent->addRegistration(protocol::respondToGetReg, "cover_tot",       floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_cover_tot));
   id = parent->addRegistration(protocol::respondToGetReg, "lai_sum",         floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_lai_sum));
   id = parent->addRegistration(protocol::respondToGetReg, "tlai",            floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_tlai));
   id = parent->addRegistration(protocol::respondToGetReg, "slai",            floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_slai));
   id = parent->addRegistration(protocol::respondToGetReg, "lai",             floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_lai));
   id = parent->addRegistration(protocol::respondToGetReg, "lai_canopy_green", floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_lai_canopy_green));
   id = parent->addRegistration(protocol::respondToGetReg, "tlai_dead",        floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_tlai_dead));
   id = parent->addRegistration(protocol::respondToGetReg, "dlt_slai_age",     floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_dlt_slai_age));
   id = parent->addRegistration(protocol::respondToGetReg, "dlt_slai_light",        floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_dlt_slai_light));
   id = parent->addRegistration(protocol::respondToGetReg, "dlt_slai_water",        floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_dlt_slai_water));
   id = parent->addRegistration(protocol::respondToGetReg, "dlt_slai_frost",        floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_dlt_slai_frost));

   id = parent->addRegistration(protocol::respondToGetReg, "pai",              floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_pai));
   id = parent->addRegistration(protocol::respondToGetReg, "grain_no",         floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_grain_no));
   id = parent->addRegistration(protocol::respondToGetReg, "root_wt",          floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_root_wt));
   id = parent->addRegistration(protocol::respondToGetReg, "leaf_wt",          floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_leaf_wt));
   id = parent->addRegistration(protocol::respondToGetReg, "stem_wt",          floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_stem_wt));
   id = parent->addRegistration(protocol::respondToGetReg, "pod_wt",           floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_pod_wt));
   id = parent->addRegistration(protocol::respondToGetReg, "grain_wt",         floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_grain_wt));
   id = parent->addRegistration(protocol::respondToGetReg, "meal_wt",          floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_meal_wt));
   id = parent->addRegistration(protocol::respondToGetReg, "oil_wt",           floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_oil_wt));
   id = parent->addRegistration(protocol::respondToGetReg, "dm_green",         floatArrayType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_dm_green));
   id = parent->addRegistration(protocol::respondToGetReg, "dm_senesced",      floatArrayType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_dm_senesced));
   id = parent->addRegistration(protocol::respondToGetReg, "dm_dead",          floatArrayType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_dm_dead));
   id = parent->addRegistration(protocol::respondToGetReg, "yield",            floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_yield));
   id = parent->addRegistration(protocol::respondToGetReg, "biomass",          floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_biomass));
   id = parent->addRegistration(protocol::respondToGetReg, "green_biomass",    floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_green_biomass));
   id = parent->addRegistration(protocol::respondToGetReg, "biomass_wt",       floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_biomass_wt));
   id = parent->addRegistration(protocol::respondToGetReg, "green_biomass_wt", floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_green_biomass_wt));
   id = parent->addRegistration(protocol::respondToGetReg, "dlt_dm",           floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_dlt_dm));
   id = parent->addRegistration(protocol::respondToGetReg, "dlt_dm_pot_rue",   floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_dlt_dm_pot_rue));
   id = parent->addRegistration(protocol::respondToGetReg, "dlt_dm_pot_te",    floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_dlt_dm_pot_te));
   id = parent->addRegistration(protocol::respondToGetReg, "dlt_dm_grain_demand", floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_dlt_dm_grain_demand));
   id = parent->addRegistration(protocol::respondToGetReg, "dlt_dm_green",     floatArrayType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_dlt_dm_green));
   id = parent->addRegistration(protocol::respondToGetReg, "dlt_dm_green_retrans", floatArrayType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_dlt_dm_green_retrans));
   id = parent->addRegistration(protocol::respondToGetReg, "dlt_dm_detached",      floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_dlt_dm_detached));
   id = parent->addRegistration(protocol::respondToGetReg, "dlt_dm_senesced",      floatArrayType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_dlt_dm_senesced));
   id = parent->addRegistration(protocol::respondToGetReg, "dlt_dm_dead_detached", floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_dlt_dm_dead_detached));
   id = parent->addRegistration(protocol::respondToGetReg, "grain_oil_conc",       floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_grain_oil_conc));
   id = parent->addRegistration(protocol::respondToGetReg, "dlt_dm_oil_conv",      floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_dlt_dm_oil_conv));
   id = parent->addRegistration(protocol::respondToGetReg, "dlt_dm_oil_conv_retrans", floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_dlt_dm_oil_conv_retrans));
   id = parent->addRegistration(protocol::respondToGetReg, "biomass_n",            floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_biomass_n));
   id = parent->addRegistration(protocol::respondToGetReg, "n_uptake",             floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_n_uptake));
   id = parent->addRegistration(protocol::respondToGetReg, "green_biomass_n",      floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_green_biomass_n));
   id = parent->addRegistration(protocol::respondToGetReg, "grain_n",              floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_grain_n));
   id = parent->addRegistration(protocol::respondToGetReg, "leaf_n",               floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_leaf_n));
   id = parent->addRegistration(protocol::respondToGetReg, "stem_n",               floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_stem_n));
   id = parent->addRegistration(protocol::respondToGetReg, "root_n",               floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_root_n));
   id = parent->addRegistration(protocol::respondToGetReg, "pod_n",                floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_pod_n));
   id = parent->addRegistration(protocol::respondToGetReg, "n_senesced",           floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_n_senesced));
   id = parent->addRegistration(protocol::respondToGetReg, "n_dead",               floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_n_dead));
   id = parent->addRegistration(protocol::respondToGetReg, "dlt_n_green",          floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_dlt_n_green));
   id = parent->addRegistration(protocol::respondToGetReg, "dlt_n_retrans",        floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_dlt_n_retrans));
   id = parent->addRegistration(protocol::respondToGetReg, "dlt_n_detached",       floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_dlt_n_detached));
   id = parent->addRegistration(protocol::respondToGetReg, "dlt_n_dead_detached",  floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_dlt_n_dead_detached));
   id = parent->addRegistration(protocol::respondToGetReg, "temp_stress_photo",    floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_temp_stress_photo));
   id = parent->addRegistration(protocol::respondToGetReg, "swdef_pheno",          floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_swdef_pheno));
   id = parent->addRegistration(protocol::respondToGetReg, "swdef_photo",          floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_swdef_photo));
   id = parent->addRegistration(protocol::respondToGetReg, "swdef_expan",          floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_swdef_expan));
   id = parent->addRegistration(protocol::respondToGetReg, "swdef_fixation",       floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_swdef_fixation));
   id = parent->addRegistration(protocol::respondToGetReg, "oxdef_photo",          floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_oxdef_photo));
   id = parent->addRegistration(protocol::respondToGetReg, "transp_eff",           floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_transp_eff));
   id = parent->addRegistration(protocol::respondToGetReg, "ep",                   floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_ep));
   id = parent->addRegistration(protocol::respondToGetReg, "cep",                  floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_cep));
   id = parent->addRegistration(protocol::respondToGetReg, "sw_supply",            floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_sw_supply));
   id = parent->addRegistration(protocol::respondToGetReg, "esw_layr",             floatArrayType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_esw_layr));
   id = parent->addRegistration(protocol::respondToGetReg, "n_conc_stover",        floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_n_conc_stover));
   id = parent->addRegistration(protocol::respondToGetReg, "n_conc_leaf",          floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_n_conc_leaf));
   id = parent->addRegistration(protocol::respondToGetReg, "n_conc_stem",          floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_n_conc_stem));
   id = parent->addRegistration(protocol::respondToGetReg, "n_conc_grain",         floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_n_conc_grain));
   id = parent->addRegistration(protocol::respondToGetReg, "n_conc_meal",          floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_n_conc_meal));
   id = parent->addRegistration(protocol::respondToGetReg, "n_conc_crit",          floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_n_conc_crit));
   id = parent->addRegistration(protocol::respondToGetReg, "n_conc_min",           floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_n_conc_min));
   id = parent->addRegistration(protocol::respondToGetReg, "n_uptake_stover",      floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_n_uptake_stover));
   id = parent->addRegistration(protocol::respondToGetReg, "no3_tot",              floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_no3_tot));
   id = parent->addRegistration(protocol::respondToGetReg, "n_demand",             floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_n_demand));
   id = parent->addRegistration(protocol::respondToGetReg, "n_supply_soil",        floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_n_supply_soil));
   id = parent->addRegistration(protocol::respondToGetReg, "dlt_n_fixed_pot",      floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_dlt_n_fixed_pot));
   id = parent->addRegistration(protocol::respondToGetReg, "dlt_n_fixed",          floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_dlt_n_fixed));
   id = parent->addRegistration(protocol::respondToGetReg, "n_fixed_tops",         floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_n_fixed_tops));
   id = parent->addRegistration(protocol::respondToGetReg, "nfact_photo",          floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_nfact_photo));
   id = parent->addRegistration(protocol::respondToGetReg, "nfact_pheno",          floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_nfact_pheno));
   id = parent->addRegistration(protocol::respondToGetReg, "nfact_expan",          floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_nfact_expan));
   id = parent->addRegistration(protocol::respondToGetReg, "nfact_grain",          floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_nfact_grain));
   id = parent->addRegistration(protocol::respondToGetReg, "nfact_grain_tot",      floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_nfact_grain_tot));
   id = parent->addRegistration(protocol::respondToGetReg, "rlv",                  floatArrayType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_rlv));
   id = parent->addRegistration(protocol::respondToGetReg, "no3_demand",           floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_no3_demand));
   id = parent->addRegistration(protocol::respondToGetReg, "sw_demand",            floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_sw_demand));
   id = parent->addRegistration(protocol::respondToGetReg, "sw_demand_te",         floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_sw_demand_te));
   id = parent->addRegistration(protocol::respondToGetReg, "root_length",          floatArrayType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_root_length));
   id = parent->addRegistration(protocol::respondToGetReg, "root_length_dead",     floatArrayType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_root_length_dead));
   id = parent->addRegistration(protocol::respondToGetReg, "no3gsm_uptake_pot",    floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_no3gsm_uptake_pot));
   id = parent->addRegistration(protocol::respondToGetReg, "no3_swfac",            floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_no3_swfac));
   id = parent->addRegistration(protocol::respondToGetReg, "leaves_per_node",      floatType);
   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_leaves_per_node));


   // Set My Variable
   id = parent->addRegistration(protocol::respondToSetReg, "crop_class",           stringType);
   IDtoSetFn.insert(UInt2SetFnMap::value_type(id,&Plant::set_plant_crop_class));

   id = parent->addRegistration(protocol::respondToSetReg, "grain_oil_conc",       floatType);
   IDtoSetFn.insert(UInt2SetFnMap::value_type(id,&Plant::set_plant_grain_oil_conc));
   }

void Plant::doEvent(unsigned int &id, protocol::Variant &v)
  {
  currentInstance = this;
  ptr2EventFn pf = IDtoEventFn[id];
  if (pf) {(this->*pf)(id,v);}
  }

// Return a variable to the system.
void Plant::getVariable(unsigned id, protocol::QueryValueData& qd)
  {
    currentInstance = this;
    ptr2getFn pf = IDtoGetFn[id];
    if (pf) {(this->*pf)(qd);}
  }

// Set a variable from the system.
bool Plant::setVariable(unsigned id, protocol::QuerySetValueData& qd)
  {
    currentInstance = this;
    ptr2setFn pf = IDtoSetFn[id];
    if (pf) {return((this->*pf)(qd));}
    return false;
  }
/////////////////////////These routines are portions of the fortran "main" routine.

// Field a Prepare message
void Plant::doPrepare(unsigned &, protocol::Variant &)
  {
    if (g.plant_status == alive)
     {
     plant_zero_daily_variables ();
     plant_get_other_variables ();     // request and receive variables from owner-modules
     plant_prepare ();                 // do crop preparation
     }
  else
     {
     plant_zero_variables ();          // Is this really necessary?
     //nh plant_get_other_variables ();
     }
  }

// Field a Process message
void Plant::doProcess(unsigned &, protocol::Variant &)
  {
    if (g.plant_status == alive)
     {
     plant_get_other_variables ();   // request and receive variables from owner-modules
     plant_process ();               // do crop processes
     plant_set_other_variables ();   // send changes to owner-modules
     }
  else
     {
     //cnh plant_zero_variables ();
     }
  }

// Field a Sow event
void Plant::doSow(unsigned &, protocol::Variant &v)
  {
  plant_get_other_variables (); // request and receive variables from owner-modules
  plant_start_crop (v);          // start crop and do  more initialisations 
  }

// Field a Harvest event
void Plant::doHarvest(unsigned &, protocol::Variant &v)
  {
  plant_harvest (v);             // harvest crop - turn into residue
  }

// Field a End crop event
void Plant::doEndCrop(unsigned &, protocol::Variant &v)
  {
  plant_end_crop ();            //end crop - turn into residue
  }

// Field a Kill crop event
void Plant::doKillCrop(unsigned &, protocol::Variant &v) 
   {
   plant_kill_crop_action (v);  //kill crop - turn into residue
   }

// Field a Kill Stem event
void Plant::doKillStem(unsigned &, protocol::Variant &v) 
   {
   plant_kill_stem (v);            //die
   }

// Field a end run event
void Plant::doEndRun(unsigned &, protocol::Variant &/*v*/)
  {
  plant_zero_variables ();
  }

// Field a class change event
void Plant::doAutoClassChange(unsigned &id, protocol::Variant &v)
  {
  string ps = IDtoAction[id];
  plant_auto_class_change(ps.c_str());
  }

// Field a Tick event
void Plant::doTick(unsigned &, protocol::Variant &v)
  {
  struct protocol::timeType tick;
  v.unpack(tick);
  double sd = (double)tick.startday;
  jday_to_day_of_year(&sd, &g.day_of_year, &g.year);
  }

// Field a NewMet event
void Plant::doNewMet(unsigned &, protocol::Variant &v)
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
        fatal_error (&err_internal, "invalid template option",-1);
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
    else
        {
        fatal_error (&err_internal, "invalid template option",-1);
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
        fatal_error (&err_internal, "invalid template option",-1);
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
    else
        {
        fatal_error (&err_internal, "invalid template option",-1);
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
    int   supply_pools[num_supply_pools] = {stem, leaf, pod};

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
    else
        {
        fatal_error (&err_internal, "invalid template option",-1);
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
            crop_get_ext_uptakes(p.uptake_source.c_str()
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
    else
        {
        fatal_error (&err_internal, "invalid template option",-1);
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
        fatal_error (&err_internal, "invalid template option",-1);
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
//+  Constant Values
    const char*  my_name = "plant_dm_init" ;

//+  Local Variables
    float dm_plant_leaf;                          // dry matter in leaves (g/plant)
    float dm_plant_stem;                          // dry matter in stems (g/plant)
    float dm_plant_pod;                           // dry matter in pods (g/plant)

//- Implementation Section ----------------------------------

    push_routine (my_name);

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

//mungb      else if (on_day_of (flowering, g_current_stage, g_days_tot)) then
//mungb             ! we are at first day of flowering
//mungb             ! set the minimum weight of stem; used for retranslocation to

//mungb!         dm_plant_stem = divide (dm_green[stem], g_plants, 0.0)
//mungb!         dm_plant_min[stem] = dm_plant_stem * (1.0 - c_stem_trans_frac)

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
        fatal_error (&err_internal, "invalid template option",-1);
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
        fatal_error (&err_internal, "invalid template option");
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
    else
        {
        fatal_error (&err_internal, "invalid template option");
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
        cproc_yieldpart_demand_stress1 (g.nfact_photo
                                        ,g.swdef_photo
                                        ,g.temp_stress_photo
                                        ,&g.dlt_dm_stress_max);
        }
    else
        {
        fatal_error (&err_internal, "invalid template option");
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
        fatal_error (&err_internal, "invalid template option");
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
                               , g.plants);

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
        fatal_error (&err_internal, "invalid template option");
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//      Remove detachment from leaf area record

//+  Mission Statement
//     Remove detachment from leaf area record

//+  Changes
//       050199 nih specified and programmed
void Plant::plant_leaf_detachment (float *leaf_area           // OUT
                                   ,float dlt_slai_detached   // IN
                                   ,float plants)             // IN
    {
//+  Local Variables
    float area_detached;                          // (mm2/plant)
    int   node;

//+  Constant Values
    const char*  my_name = "plant_leaf_detachment" ;

//- Implementation Section ----------------------------------

    push_routine (my_name);

    area_detached = dlt_slai_detached / plants * sm2smm;

    for (node = 0; node < max_node; node++) 
      {
      if(area_detached>leaf_area[node])
        {
        area_detached = area_detached - leaf_area[node];
        leaf_area[node] = 0.0;
        }
      else
        {
        leaf_area[node] = leaf_area[node] - area_detached;
        area_detached = 0.0;
        }
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
        crop_failure_germination (sowing, germ, now,
                                  c.days_germ_limit,
                                  g.current_stage,
                                  g.days_tot,
                                  g.plants,
                                  &g.dlt_plants_failure_germ);

        crop_failure_emergence (germ, emerg, now,
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
            }
        else
            {
            }

        }
    else
        {
        fatal_error (&err_internal, "invalid template option");
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
        parent->writeString (" crop failure because of total leaf senescence.");
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
        parent->writeString ("         crop failure because of prolonged");
        parent->writeString ("         phenology delay through water stress.");
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
            string msg= "plant_kill. ";
              msg = msg + Ftoa(killfr*100.0, 0).c_str();
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

        string msg= "plant_kill. ";
          msg = msg + Ftoa(killfr*100.0, 0).c_str();
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
       bound_check_real_var(killfr, 0.0, 1.0, "killfr");
       *dlt_plants = *dlt_plants - g_plants*killfr;
       }

    if (killfr > 0.0)
        {
        string msg= "plant_kill. ";
          msg = msg + Ftoa(killfr*100.0, 0).c_str();
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
        parent->writeString (" crop killed because of external action.");
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
        sprintf(msg, " plant death. standing above-ground dm = %f (kg/ha)", biomass);
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
        fatal_error (&err_internal, "invalid template option");
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
                                   ,g.nfact_expansion
                                   ,&g.dlt_lai_stressed);
        }
    else
        {
        fatal_error (&err_internal, "invalid template option");
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
        fatal_error (&err_internal, "invalid template option");
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
        fatal_error (&err_internal, "invalid template option");
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
        fatal_error (&err_internal, "invalid template option");
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
        fatal_error (&err_internal, "invalid template option");
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
        fatal_error (&err_internal, "invalid template option");
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
                             , g.nfact_expansion
                             , g.swdef_expansion
                             , &g.leaves_per_node
                             , &g.dlt_leaf_no_pot
                             , &g.dlt_node_no_pot);
        }
    else
        {
        fatal_error (&err_internal, "invalid template option");
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
        fatal_error (&err_internal, "invalid template option");
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
                         , c.no3_uptake_max
                         , c.no3_conc_half_max
                         , g.sw_avail_pot
                         , g.sw_avail
                         , g.current_stage
                         , c.n_fix_rate
                         , biomass
                         , g.swdef_fixation
                         , g.n_fix_pot
                         );
        }
    else
        {
        fatal_error (&err_internal, "invalid template option");
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

        legnew_n_retranslocate
        (
        c.sfac_slope
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
        , g.dlt_n_retrans
        );

        }
    else
        {
        fatal_error (&err_internal, "invalid template option");
        }

    pop_routine (my_name);
    return;
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
    else
        {
        fatal_error (&err_internal, "invalid template option");
        }

    pop_routine (my_name);
    return;
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
        crop_get_ext_uptakes(p.uptake_source.c_str()
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
    else if (option == 2)
        {
        cproc_n_uptake3(g.dlayer
                        , max_layer
                        , g.no3gsm_uptake_pot
                        , g.n_fix_pot
                        , c.n_supply_preference.c_str()
                        , g.n_demand
                        , g.n_max
                        , max_part
                        , g.root_depth
                        , g.dlt_no3gsm);
        }
    else
        {
        fatal_error (&err_internal, "invalid template option");
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
                           , g.n_demand
                           , g.n_fix_pot
                           , g.n_max
                           , g.root_depth
                           , g.dlt_n_green
                           , &g.n_fix_uptake);

        }
    else
        {
        fatal_error (&err_internal, "invalid template option");
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
    else
        {
        fatal_error (&err_internal, "invalid template option");
        }

    pop_routine (my_name);
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
    ,float  g_current_stage                    // (INPUT)  current phenological stage       
    ,float  *n_conc_crit                        // (OUTPUT) critical N concentration  (g N/g part)          
    ,float  *n_conc_max                         // (OUTPUT) maximum N concentration   (g N/g part)  
    ,float  *n_conc_min                         // (OUTPUT) minimum N concentration    g N/g part)
    ) {

//+  Constant Values
    const char*  my_name = "plant_n_conc_limits" ;

//+  Local Variables
    int   numvals;                                // number of values in stage code table
    float stage_code;                             // interpolated current stage code

//- Implementation Section ----------------------------------

    push_routine (my_name);

    fill_real_array (n_conc_crit, 0.0, max_part);
    fill_real_array (n_conc_min, 0.0, max_part);

    if (stage_is_between (emerg, maturity, g_current_stage))
        {

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
                                    , numvals);
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

        }
    else
        {
        }

    pop_routine (my_name);
    return;
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

//+  Constant Values
    const char*  my_name = "plant_n_conc_grain_limits" ;

//+  Local Variables
    float dm_oil;                                 // oil mass (g/m2)
    float dm_meal;                                // meal mass (g/m2)
    float dm_grain;                               // grain mass (g/m2)
    float n_crit_grain;                           // critial mass of grain N (g/m2)
    float n_max_grain;                            // maximum mass of grain N (g/m2)
    float n_min_grain;                            // minimum mass of grain N (g/m2)

//- Implementation Section ----------------------------------

    push_routine (my_name);

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
    else
        {
        }


    pop_routine (my_name);
    return;
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
//+  Constant Values
    const char*  my_name = "plant_n_init" ;

//- Implementation Section ----------------------------------

    push_routine (my_name);

    if (on_day_of (emerg, g_current_stage))
        {
        n_green[root] = c_n_root_init_conc*g_dm_green[root];
        n_green[stem] = c_n_stem_init_conc*g_dm_green[stem];
        n_green[leaf] = c_n_leaf_init_conc*g_dm_green[leaf];
        n_green[pod] = 0.0;
        n_green[meal] = 0.0;
        n_green[oil] = 0.0;

        }
    else
        {
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

        for (part = 1; part <= max_part; part++) 
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
            fatal_error (&err_user, "bad n supply preference");
            }
        }
    else
        {
        fatal_error (&err_internal, "invalid template option");
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
        fatal_error (&err_internal, "invalid template option");
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
        fatal_error (&err_internal, "invalid template option");
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
    else
        {
        fatal_error (&err_internal, "invalid template option");
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
        cproc_phenology1 (&g.previous_stage
                          ,&g.current_stage
                          ,sowing
                          ,germ
                          ,harvest_ripe
                          ,emerg
                          ,flowering
                          ,max_stage
                          ,c.num_temp
                          ,c.x_temp
                          ,c.y_tt
                          ,g.maxt
                          ,g.mint
                          ,g.nfact_pheno
                          ,g.swdef_pheno
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
        //fprintf(stdout, "%d, %f, %f, %f\n", g.day_of_year, g.maxt, g.mint, g.dlt_tt);
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
                                     ,&g.vern_eff);                             //maximum vernalisation requirement is 50 days

        g.cumvd = g.cumvd + g.dlt_cumvd;

        photoperiod = day_length (g.day_of_year,g.latitude,c.twilight);

        wheat_photoperiod_effect(g.current_stage,
                                 emerg,
                                 floral_init,
                                 photoperiod,
                                 p.photop_sens,
                                 &g.photop_eff);

        cproc_phenology_nw (g.previous_stage
                            ,g.current_stage
                            ,sowing
                            ,germ
                            ,harvest_ripe
                            ,emerg
                            ,flowering
                            ,max_stage
                            ,c.num_temp
                            ,c.x_temp
                            ,c.y_tt
                            ,g.maxt
                            ,g.mint
                            ,g.nfact_pheno
                            ,g.swdef_pheno
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
                            ,g.dlt_tt
                            ,g.phase_tt 
                            ,g.phase_devel
                            ,g.dlt_stage
                            ,g.tt_tot
                            ,g.days_tot);
        }
    else
        {
        fatal_error (&err_internal, "invalid template option");
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Simulate plant senescence.

//+  Mission Statement
//     Calculate plant senescence

//+  Changes
//      091294 jngh specified and programmed
void Plant::plant_sen_bio (int   option/*(INPUT) option number*/)
    {

//+  Constant Values
    const char*  my_name = "plant_sen_bio" ;

//+  Local Variables
    float canopy_sen_fr;                          // fraction of canopy senescing

//- Implementation Section ----------------------------------
    push_routine (my_name);

    if (option == 1)
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
         //fprintf(stdout,"%d,%f,%f\n", g.day_of_year, canopy_sen_fr, g.dlt_dm_senesced[1]);
        }
    else
        {
        fatal_error (&err_internal, "invalid template option");
        }

    pop_routine (my_name);
    return;
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
    else
        {
        fatal_error (&err_internal, "invalid template option");
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
                              , g.nfact_expansion
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
        fatal_error (&err_internal, "invalid template option");
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
        fatal_error (&err_internal, "invalid template option");
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
      fatal_error (&err_internal, "bad record for total leaf area");
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
        fatal_error (&err_internal,"bad record for dead leaf number");
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
    ,float *g_dlt_n_senesced_trans                             // (INPUT)  actual N loss with senesced pl
    ,float *g_dlt_n_senesced                                   //  ??
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

    g_n_green[leaf] = g_n_green[leaf] - g_dlt_n_senesced_trans[leaf];
    g_n_green[stem] = g_n_green[stem] + g_dlt_n_senesced_trans[leaf];

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
        dlt_root_length_dead = g_root_length[layer] * dying_fract_plants;
        g_root_length[layer] = g_root_length[layer] - dlt_root_length_dead;
        g_root_length_dead[layer] = g_root_length_dead[layer] + dlt_root_length_dead;
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

    bound_check_real_var(g_root_depth
                         , 0.0
                         , sum_real_array (g_dlayer, max_layer)
                         , "root_depth");

    bound_check_real_var(g_current_stage
                         , 1.0
                         , max_stage
                         , "current_stage");

    bound_check_real_var(g_plants
                         , 0.0
                         , 10000.0
                         , "plants");

    bound_check_real_var(g_cover_green
                         , 0.0
                         , 1.0
                         , "cover_green");

    bound_check_real_var(g_cover_sen
                         , 0.0
                         , 1.0
                         , "cover_sen");

    bound_check_real_var(g_cover_dead
                         , 0.0
                         , 1.0
                         , "cover_dead");

    bound_check_real_var(sum_real_array (g_heat_stress_tt, max_stage)
                         , 0.0
                         , 1000000.0
                         , "heat_stress_tt");
    bound_check_real_var(sum_real_array (g_dm_stress_max, max_stage)
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
"                     biomass =       %.6f   lai            = %.6f\n\
                     stover n conc = %.6f   extractable sw = %.6f"
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
//       Return an interpolated stage code from a table of stage_codes
//       and a nominated stage number. Returns 0 if the stage number is not
//       found. Interpolation is done on thermal time.

//+  Mission Statement
//     Get the stage code from a table of stage codes

//+  Changes
//       080994 jngh specified and programmed
float Plant::plant_stage_code (float  *c_stage_code_list  // (INPUT)  list of stage numbers
                              ,float  *g_phase_tt         // (INPUT)  Cumulative growing degree days required for each stage (deg days)
                              ,float  *g_tt_tot           // (INPUT)  the sum of growing degree days for a phenological stage (oC d)
                              ,float  stage_no            // (INPUT) stage number to convert
                              ,float  *stage_table        // (INPUT) table of stage codes
                              ,int    numvals) {          // (INPUT) size_of of table

     //+  Constant Values
     const char*  my_name = "plant_stage_code" ;

     //+  Local Variables
     float phase_tt;                                   // required thermal time between stages (oC)
     float fraction_of;                                //
     int   i;                                          // array index - counter
     int   next_stage;                                 // next stage number to use
     float tt_tot;                                     // elapsed thermal time between stages (oC)
     int   this_stage;                                 // this stage to use
     float x_stage_code = 0.0;                         // interpolated stage code

     //- Implementation Section ----------------------------------

     push_routine (my_name);

     if (numvals>=2)
         {
         // we have a valid table
         this_stage = stage_no_of (stage_table[0] , c_stage_code_list, max_stage);

         for (i = 1; i < numvals; i++)
           {
           next_stage = stage_no_of (stage_table[i], c_stage_code_list, max_stage);
           if (stage_is_between (this_stage, next_stage, stage_no))
              {
              // we have found its place
              tt_tot = sum_between (this_stage-1, next_stage-1, g_tt_tot);
              phase_tt = sum_between (this_stage-1, next_stage-1, g_phase_tt);
              fraction_of = divide (tt_tot, phase_tt, 0.0);
              fraction_of = bound(fraction_of, 0.0, 0.999);
              x_stage_code = stage_table[i-1] + (stage_table[i] - stage_table[i-1])
                                  * fraction_of;
              break;
              }
           else
              {
              x_stage_code = 0.0;
              this_stage = next_stage;
              }
           }
        }
     else
        {
        // we have no valid table
        x_stage_code = 0.0;

        char msg[80];
        sprintf(msg, "invalid stage code lookup table - number of values = %d", numvals);
        warning_error (&err_user, msg);
        }
    pop_routine (my_name);
    return (x_stage_code);
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
        fatal_error (&err_internal, "invalid template option");
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
        cproc_sw_supply1 ( c.sw_lb
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
        fatal_error (&err_internal, "invalid template option");
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

        cproc_sw_demand_bound(g.sw_demand_te
                              ,p.eo_crop_factor
                              ,g.eo
                              ,g.cover_green
                              ,&g.sw_demand);

        }
    else
        {
        fatal_error (&err_internal, "invalid template option");
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
        crop_get_ext_uptakes(p.uptake_source.c_str()
                             ,c.crop_type.c_str()
                             ,"water"
                             ,1.0
                             ,0.0
                             ,100.0
                             ,ext_sw_supply
                             ,max_layer);    // uptake flag// crop type// uptake name// unit conversion factor// uptake lbound// uptake ubound// uptake array// array dim

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
        fatal_error (&err_internal, "invalid template option");
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
        }
    else
        {
        fatal_error (&err_internal, "invalid template option");
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
        legnew_dm_pot_rue(g.current_stage
                         , g.tt_tot
                         , c.x_stage_rue
                         , g.phase_tt
                         , c.y_rue
                         , c.rue_pod
                         , g.cover_green
                         , g.cover_pod
                         , g.radn_int
                         , min(min(g.temp_stress_photo, g.nfact_photo), g.oxdef_photo)
                         , &g.dlt_dm_pot_rue);
        }
    else
        {
        fatal_error (&err_internal, "invalid template option");
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

        cproc_transp_eff1(c.svp_fract, c.transp_eff_cf[current_phase-1],
                          g.maxt, g.mint,
                          &g.transp_eff);
        }
    else
        {
        fatal_error (&err_internal, "invalid template option");
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
        fatal_error (&err_internal, "invalid template option");
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
        fatal_error (&err_internal, "invalid template option");
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
    else
        {
        fatal_error (&err_internal, "invalid template option");
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
        fatal_error (&err_internal, "invalid template option");
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//     Calculate extinction coefficient as a function of row spacing

//+  Mission Statement
//     Calculate extinction coefficient as a function of row spacing

//+  Changes
//     15-08-1997 - huth - Programmed and Specified
void Plant::legnew_extinct_coef 
    (
     float g_row_spacing
    ,float *c_x_row_spacing
    ,float *c_y_extinct_coef
    ,int   c_num_row_spacing
    ,float *extinct_coef      //OUTPUT
    ) {

//+  Constant Values
    const char*  myname = "legnew_extinct_coef" ;

//- Implementation Section ----------------------------------
    push_routine (myname);

    *extinct_coef = linear_interp_real (g_row_spacing
                                        ,c_x_row_spacing
                                        ,c_y_extinct_coef
                                        ,c_num_row_spacing);

    pop_routine (myname);
    return;
    }


//+  Purpose
//     Calculate crop cover

//+  Mission Statement
//     Calculate crop cover

//+  Changes
//     15-08-1997 - huth - Programmed and Specified
//     10-02-1999 - huth - added pod cover component
void Plant::legnew_cover (
    float g_row_spacing
    ,float *c_x_row_spacing
    ,float *c_y_extinct_coef
    ,int   c_num_row_spacing
    ,float canopy_fac                        // skip row factor   
    ,float g_lai
    ,float *g_cover_green                    //OUTPUT
    ) {

//+  Constant Values
    const char*  myname = "legnew_cover" ;

//+  Local Variables
    float extinct_coef;
    float lai_canopy;                             // lai transformed to solid canopy
    float cover_green_leaf_canopy;                // green leaf cover in canopy

//- Implementation Section ----------------------------------
    push_routine (myname);

    if (g_lai > 0.0)
        {
        extinct_coef = linear_interp_real (g_row_spacing
                                           ,c_x_row_spacing
                                           ,c_y_extinct_coef
                                           ,c_num_row_spacing);

//-----light interception modified to give hedgerow effect with skip row

        lai_canopy = g_lai * canopy_fac;          // lai in hedgerow
                                                  // interception on row area basis
        cover_green_leaf_canopy = 1.0 - exp(-extinct_coef*lai_canopy) ;
        *g_cover_green = divide (cover_green_leaf_canopy, canopy_fac
        , 0.0)             ;                      // interception on ground area basis

        }
    else
        {
        *g_cover_green = 0.0;
        }

    pop_routine (myname);
    return;
    }


//+  Purpose
//     Calculate crop cover due to leaf and pod light interception

//+  Mission Statement
//     Calculate crop cover due to leaf and pod light interception

//+  Changes
//     15-08-1997 - huth - Programmed and Specified
//     10-02-1999 - huth - added pod cover component
void Plant::legnew_cover_leaf_pod (
     float g_row_spacing
    ,float *c_x_row_spacing
    ,float *c_y_extinct_coef
    ,int   c_num_row_spacing
    ,float c_extinct_coef_pod
    ,float  canopy_fac
    ,float g_lai
    ,float g_pai
    ,float *g_lai_canopy     // OUTPUT lai transformed to solid rows
    ,float *g_cover_green  // OUTPUT
    ,float *g_cover_pod    // OUTPUT
    ) {

//+  Constant Values
    const char*  myname = "legnew_cover_leaf_pod" ;

//+  Local Variables
    double extinct_coef;
    double lai_canopy;                             // lai transformed to solid rows
    double pai_canopy;                             // pai transformed to solid rows
    float cover_green_canopy;                     // green cover in row
    float cover_green_pod_canopy;                 // green pod cover in row

//- Implementation Section ----------------------------------
    push_routine (myname);
    extinct_coef = linear_interp_real (g_row_spacing
                                       ,c_x_row_spacing
                                       ,c_y_extinct_coef
                                       ,c_num_row_spacing);

    //-----light interception modified to give hedgerow effect with skip row
    lai_canopy = g_lai * canopy_fac;     // lai in hedgerow
    pai_canopy = g_pai * canopy_fac;

    if (g_lai > 0.0 || g_pai > 0.0)
        {
        cover_green_canopy = 1.0 - exp(-(c_extinct_coef_pod*pai_canopy
                                         + extinct_coef*lai_canopy));
                                                  // interception on ground area basis
        *g_cover_green = divide (cover_green_canopy, canopy_fac, 0.0);
        }
    else
        {
        *g_cover_green = 0.0;
        }

    if (g_pai > 0.0)
        {
        cover_green_pod_canopy = 1.0
        - exp(-c_extinct_coef_pod*pai_canopy) ;   // interception on row area basis
                                                  // interception on ground area basis
        *g_cover_pod = divide (cover_green_pod_canopy, canopy_fac, 0.0);

        }
    else
        {
        *g_cover_pod = 0.0;
        }
    *g_lai_canopy = lai_canopy;

    pop_routine (myname);
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
    n_uptake_sum = - sum_real_array (g_dlt_no3gsm, deepest_layer+1);
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
    dlt_n_green[meal] = 0.0;
    dlt_n_green[oil] = 0.0;

    if (!reals_are_equal(sum_real_array (dlt_n_green, max_part) - n_uptake_sum, 0.0))
        {
        string msg ="dlt_n_green mass balance is off: "
              + Ftoa(sum_real_array (dlt_n_green, max_part), 4)
              + " vs "
              + Ftoa(n_uptake_sum, 4);
        error(msg.c_str(), false);
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

    bound_check_real_var (*grain_energy
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
          + Ftoa(dlt_dm_green_tot, 6)
          + " vs "
          + Ftoa(g_dlt_dm, 6);
      error(msg.c_str(), 0);
      }

    // check that deltas are in legal range
    bound_check_real_array (dlt_dm_green, max_part, 0.0, g_dlt_dm, "dlt_dm_green");

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

    if (!reals_are_equal(dlt_dm_green_tot, g_dlt_dm))
      {
      string msg = "dlt_dm_green_tot mass balance is off: "
          + Ftoa(dlt_dm_green_tot, 6)
          + " vs "
          + Ftoa(g_dlt_dm, 6);
      error(msg.c_str(), 0);
      }

    // check that deltas are in legal range
    bound_check_real_array (dlt_dm_green, max_part, 0.0, g_dlt_dm, "dlt_dm_green");

    pop_routine (my_name);
    return;
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
          + Ftoa(sum_real_array (dm_retranslocate, max_part), 6)
          + " vs "
          + Ftoa(*dm_oil_conv_retranslocate, 6);
      error(msg.c_str(), 0);
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
    bound_check_real_var (mass_balance, -1.0e-5, 1.0e-5
    , "dm_retranslocate mass balance");

    pop_routine (my_name);
    return;
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
    ,float  *phase_tt                      // (INPUT/OUTPUT) cumulative growing degree days required for each stage (deg days)
    ) {

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
    (
     float  c_sfac_slope                // (INPUT)  soil water stress factor slope               
    ,float  c_sw_fac_max                // (INPUT)  soil water stress factor maxim               
    ,float  c_temp_fac_min              // (INPUT)  temperature stress factor mini               
    ,float  c_tfac_slope                // (INPUT)  temperature stress factor slop               
    ,float  g_maxt                      // (INPUT)  maximum air temperature (oC)                 
    ,float  g_mint                      // (INPUT)  minimum air temperature (oC)                 
    ,float  g_nfact_grain_conc          // (INPUT)                                               
    ,float  *g_n_conc_crit              // (INPUT)  critical N concentration (g N/               
    ,float  g_swdef_expansion           // (INPUT)                                               
    ,float  *g_n_conc_min               // (INPUT)  minimum N concentration (g N/g               
    ,float  *g_dlt_dm_green             // (INPUT)  plant biomass growth (g/m^2)                 
    ,float  *g_dlt_dm_green_retrans     // (INPUT)  plant biomass growth (g/m^2)                 
    ,float  *g_dm_green                 // (INPUT)  live plant dry weight (biomass               
    ,float  *g_n_conc_max               // (INPUT)  maximum N concentration (g N/g               
    ,float  *g_n_green                  // (INPUT)  plant nitrogen content (g N/m^               
    ,float  *dlt_n_retrans              // (OUTPUT) plant N taken out from plant parts (g N/m^2) 
    ) {

//+  Constant Values
    const char*  my_name = "legnew_n_retranslocate" ;
//
    const float  tolerence = 0.001 ;

//+  Local Variables
    float grain_n_demand;                         // grain N demand (g/m^2)
    float n_avail[max_part];                      // N available for transfer to grain (g/m^2)
    float n_avail_stover;                         // total N available in stover (g/m^2)
    float n_potential;                            // maximum grain N demand (g/m^2)
    int   part;                                   // plant part number

//- Implementation Section ----------------------------------

    push_routine (my_name);

    grain_n_demand = (g_dlt_dm_green[meal] + g_dlt_dm_green_retrans[meal])
                     * crop_n_dlt_grain_conc (meal
                                              , c_sfac_slope
                                              , c_sw_fac_max
                                              , c_temp_fac_min
                                              , c_tfac_slope
                                              , g_maxt
                                              , g_mint
                                              , g_nfact_grain_conc
                                              , g_n_conc_crit
                                              , g_n_conc_min
                                              , g_swdef_expansion );

    n_potential  = (g_dm_green[meal]
                     + g_dlt_dm_green[meal]
                     + g_dlt_dm_green_retrans[meal])
                   * g_n_conc_max[meal];

    grain_n_demand = u_bound (grain_n_demand, n_potential - g_n_green[meal]);

    crop_n_retrans_avail (max_part
                          , root
                          , meal
                          , g_n_conc_min
                          , g_dm_green
                          , g_n_green
                          , n_avail)  ;   // grain N potential (supply)

// available N does not include roots or grain
// this should not presume roots and grain are 0.

    n_avail_stover  =  sum_real_array (n_avail, max_part);

// get actual grain N uptake

// limit retranslocation to total available N

    fill_real_array (dlt_n_retrans, 0.0, max_part);

    if (grain_n_demand>=n_avail_stover)
        {

// demand greater than or equal to supply
// retranslocate all available N

        dlt_n_retrans[leaf] = - n_avail[leaf];
        dlt_n_retrans[stem] = - n_avail[stem];
        dlt_n_retrans[pod] = - n_avail[pod];
        dlt_n_retrans[meal] = n_avail_stover;

        }
    else
        {

// supply greater than demand.
// Retranslocate what is needed

        dlt_n_retrans[leaf] = - grain_n_demand
        * divide (n_avail[leaf]
        , n_avail_stover, 0.0);

        dlt_n_retrans[pod] = - grain_n_demand
        * divide (n_avail[pod]
        , n_avail_stover, 0.0);

        dlt_n_retrans[stem] = - grain_n_demand
        - dlt_n_retrans[leaf]
        - dlt_n_retrans[pod] ;                    // note - these are// -ve values.

        dlt_n_retrans[meal] = grain_n_demand;

        }

// just check that we got the maths right.

    for (part = root; part < pod; part++) 
      {
      bound_check_real_var (abs (dlt_n_retrans[part])
                            , 0.0, n_avail[part] + tolerence
                            , "dlt_n_retrans[part]");
      }

    pop_routine (my_name);
    return;
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

// NIH - if we accept this approach we'll
// need to make this configurable via a parameter
    node_sen_rate = c_node_sen_rate;
    ///c     :       * (1.0 + (1.0 - g_nfact_expansion));

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
//       Potential biomass (carbohydrate) production from
//       photosynthesis (g/m^2).  The effect of factors such
//       temperature and nutritional status of the plant are
//       taken into account in the radiation use efficiency.

//+  Mission Statement
//     Get the potential biomass production - limited by stress factors

//+  Changes
//       181197 nih specified and programmed
void Plant::legnew_dm_pot_rue (
     float  current_stage
    ,float  *g_tt_tot
    ,float  *c_x_stage_rue
    ,float  *g_phase_tt
    ,float  *c_y_rue
    ,float  rue_pod
    ,float  cover_green
    ,float  cover_pod
    ,double  radn_int
    ,double  stress_factor
    ,float  *dlt_dm_pot                    // (OUTPUT) potential dry matter (carbohydrate) production (g/m^2)
    ) {

//+  Constant Values
    const char*  my_name = "legnew_dm_pot_rue" ;

//+  Local Variables
    double podfr;                                  // fraction of intercepted light intercepted by pods
    double rue_leaf;
    int   numvals;

//- Implementation Section ----------------------------------

    push_routine (my_name);

    // Assume pods are above the rest of the canopy
    podfr = bound(divide(cover_pod,cover_green,0.0), 0.0, 1.0);

    if (cover_green<cover_pod)
        {
        fatal_error (&err_internal,"error in pod light interception");
        *dlt_dm_pot = 0.0;
        }
    else
        {
        numvals = 1+count_of_real_vals (c_x_stage_rue, max_stage);
        rue_leaf = linear_interp_real (current_stage
                                       , c_x_stage_rue
                                       , c_y_rue
                                       , numvals);
        *dlt_dm_pot = (radn_int * podfr * rue_pod +
                       radn_int * (1.0 - podfr) * rue_leaf) * stress_factor;
        // c0
        //fprintf(stdout, "%d,%.9f,%d,%.9f,%.9f,%.9f,%.9f\n",
        //        g.day_of_year, rue_leaf, numvals, c_y_rue[0], c_y_rue[1], c_y_rue[2], c_y_rue[3]);
        }
    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Derives number of nodes to result in given cumulative area

//+  Mission Statement
//     Get the number of nodes for a given cumulative area

//+  Changes
//       110298 nih specified and programmed
float Plant::legnew_node_no_from_area
    (
     float *g_leaf_area         // (INPUT)  leaf area of each leaf (mm^2)     
    ,int   num_nodes            // (INPUT)  number of nodes                   
    ,float pla                  // (INPUT)  plant leaf area                   
    ) {

    //+  Constant Values
    const char*  my_name = "legnew_node_no_from_area" ;
    
    //+  Local Variables
    int   node_no;                                    // number of nodes containing leaf area (0-max_node)
    float node_area_whole;                            // number of complete nodes ()
    float node_area_part;                             // area from last node (mm^2)
    float node_fract;                                 // fraction of last node (0-1)
    float result;
    //- Implementation Section ----------------------------------
    
    push_routine (my_name);
    
    node_no = get_cumulative_index_real (pla, g_leaf_area, num_nodes);
    
    node_area_whole = sum_real_array (g_leaf_area, node_no);
    node_area_part = pla - node_area_whole;
    node_fract = divide (node_area_part, g_leaf_area[node_no], 0.0);
    
    result = (float) (node_no - 1) + node_fract;
    pop_routine (my_name);
    return result;
    }


//+  Purpose
//       Derives number of leaves to result in given cumulative area

//+  Mission Statement
//     Gets the number of leaves for given area

//+  Changes
//       110298 nih specified and programmed
float Plant::legnew_leaf_no_from_area (
     float  *g_leaf_area                // (INPUT)  leaf area of each leaf (mm^2)
    ,float  *g_leaf_no
    ,int    num_nodes                   // (INPUT)  number of nodes
    ,float  pla                         // (INPUT)  plant leaf area
    ) {

    //+  Constant Values
    const char*  my_name = "legnew_leaf_no_from_area" ;

    //+  Local Variables
    int   node_no;                                    // number of nodes containing
    // leaf area (0-max_node)
    float node_area_whole;                            // number of complete nodes ()
    float node_area_part;                             // area from last node (mm^2)
    float node_fract;                                 // fraction of last node (0-1)
    float result;

    //- Implementation Section ----------------------------------

    push_routine (my_name);

    node_no = 1+get_cumulative_index_real (pla, g_leaf_area, num_nodes);

    node_area_whole = sum_real_array (g_leaf_area, node_no-1);
    node_area_part = pla - node_area_whole;
    node_fract = divide (node_area_part, g_leaf_area[node_no-1], 0.0);

    result = sum_real_array (g_leaf_no,node_no) + node_fract * g_leaf_no[node_no-1];
//     fprintf(stdout, "%d,%.9f,%.9f,%.9f,%.9f,%.9f\n",
//             g.day_of_year, (float)node_no,
//             node_area_whole, node_fract, sum_real_array (g_leaf_no,node_no), result);
//     fprintf(stdout, "%d,%.9f,%.9f,%.9f\n",
//             g.day_of_year,g_leaf_no[0], g_leaf_no[1], g_leaf_no[2]);

    pop_routine (my_name);
    return result;
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

    if (on_day_of (start_grain_fill, g_current_stage)) {

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


//+  Purpose
//       Calculate number of vernal days from daily temperature

//+  Mission Statement
//     Calculate number of vernal days from daily temperature

//+  Changes
//       101299 nih specified and programmed
float Plant::legnew_vernal_days(float  g_maxt
                               ,float  g_mint
                               ,float  *c_x_vernal_temp
                               ,float  *c_y_vernal_days
                               ,int    c_num_vernal_temp) {

    //+  Constant Values
    const char*  my_name = "legnew_vernal_days" ;
    
    //+  Local Variables
    float av_temp;
    float result;
    //- Implementation Section ----------------------------------
    
    push_routine (my_name);
    
    av_temp = (g_maxt + g_mint)/2.0;
    
    result = linear_interp_real(av_temp
                                ,c_x_vernal_temp
                                ,c_y_vernal_days
                                ,c_num_vernal_temp);
                                          
    pop_routine (my_name);
    return result;
    }


//+  Purpose
//       Derives seneseced plant nitrogen (g N/m^2)

//+  Mission Statement
//   Calculate change in senesced plant Nitrogen

//+  Changes
//       121297 nih specified and programmed
void Plant::legnew_n_senescence1 
    (
     int    num_part               // (INPUT) number of plant part                               
    ,float  *c_n_sen_conc          // (INPUT)  N concentration of senesced materia  (g/m^2)      
    ,float  *g_dlt_dm_senesced     // (INPUT)  plant biomass senescence (g/m^2)                  
    ,float  *g_n_green             // (INPUT) nitrogen in plant material (g/m^2)                 
    ,float  *g_dm_green            // (INPUT) plant material (g/m^2)                             
    ,float  *dlt_n_senesced_trans  // (OUTPUT)  plant N senescence (g/m^2)                       
    ,float  *dlt_n_senesced        // (OUTPUT) actual nitrogen senesced from plant parts (g/m^2) 
    ) {

//+  Constant Values
    const char*  my_name = "legnew_n_senescence1" ;

//+  Local Variables
    int   part;                                   // plant part counter variable
    float green_n_conc;                           // N conc of green material (g/g)
    float sen_n_conc;                             // N conc of senescing material (g/g)
    float dlt_n_in_senescing_part;

//- Implementation Section ----------------------------------

    push_routine (my_name);

// first we zero all plant component deltas

    for (part = 0; part < num_part; part++) 
       {
       green_n_conc = divide (g_n_green[part]
       ,g_dm_green[part]
       ,0.0);
   
       dlt_n_in_senescing_part = g_dlt_dm_senesced[part]
       * green_n_conc;
   
       sen_n_conc = min (c_n_sen_conc[part], green_n_conc);
   
       dlt_n_senesced[part] = g_dlt_dm_senesced[part]
       * sen_n_conc;
   
       dlt_n_senesced[part] = u_bound (dlt_n_senesced[part]
       , g_n_green[part]);
   
       dlt_n_senesced_trans[part] = dlt_n_in_senescing_part
       - dlt_n_senesced[part];
   
       dlt_n_senesced_trans[part] = l_bound(dlt_n_senesced_trans[part]
       , 0.0);
   
       }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//     Calculate crop cover

//+  Mission Statement
//     Calculate crop cover

//+  Changes
//     15-08-1997 - huth - Programmed and Specified
//     10-02-1999 - huth - added pod cover component
void Plant::legnew_canopy_fac (
     float  g_row_spacing
    ,float  g_plants
    ,float  g_skip_row_fac              // skip row factor  
    ,float  g_skip_plant_fac            // skip plant factor
    ,float  g_canopy_width
    ,float  *g_canopy_fac
    ) {

//+  Constant Values
    const char*  myname = "legnew_canopy_fac" ;

//+  Local Variables
    float area_actual;
    float area_avail;
    float plant_space;
    float radius_intra_row_solid;
    float radius_intra_row_skip;
    float width_intra_row;
    float radius_inter_row_solid;
    float radius_inter_row_skip;
    float width_inter_row;
    float row_spacing_effective;
    float plant_spacing_effective;

//- Implementation Section ----------------------------------
    push_routine (myname);

    row_spacing_effective = g_row_spacing * g_skip_row_fac;
    plant_space = divide (sm2smm
       , row_spacing_effective * g_plants * g_skip_plant_fac
       , 0.0);
    plant_spacing_effective = plant_space * g_skip_plant_fac;
    radius_intra_row_solid = min(plant_space * 0.5
      , g_canopy_width * 0.5);
    radius_intra_row_skip = min(plant_space * (g_skip_plant_fac - 0.5)
      , g_canopy_width * 0.5);

    width_intra_row = radius_intra_row_skip + radius_intra_row_solid;

    radius_inter_row_solid = min(g_row_spacing * 0.5, g_canopy_width * 0.5);
    radius_inter_row_skip = min(g_row_spacing * (g_skip_row_fac - 0.5), g_canopy_width * 0.5);
    width_inter_row = radius_inter_row_solid + radius_inter_row_skip;

    area_avail = plant_spacing_effective * row_spacing_effective;
    area_actual = width_inter_row * width_intra_row;
    *g_canopy_fac = divide (area_avail, area_actual, 0.0);

    pop_routine (myname);
    return;
    }


//+  Purpose
//       Get change in plant canopy width from stem dry matter per plant

//+  Mission Statement
//   Calculate change in crop canopy width (based upon weight of %7).

//+  Changes
//       230498 nih specified and programmed
void Plant::plant_canopy_width
    (
     float  g_canopy_width                   // (INPUT)  canopy height (mm)
    ,float  *p_x_stem_wt
    ,float  *p_y_width
    ,int    p_num_stem_wt
    ,float  *g_dm_green                      // (INPUT)  live plant dry weight (biomass
    ,float  g_plants                         // (INPUT)  Plant density (plants/m^2)
    ,int    stem                             // (INPUT)  plant part no for stem
    ,float  *dlt_canopy_width                 // (OUTPUT) canopy width change (mm)
    ) {
    const char*  my_name = "plant_canopy_width" ;
    float dm_stem_plant;                          // dry matter of stem (g/plant)
    float new_width;                              // new plant width (mm)

    push_routine (my_name);

    dm_stem_plant = divide (g_dm_green[stem], g_plants, 0.0);
    new_width = linear_interp_real(dm_stem_plant
                                  ,p_x_stem_wt
                                  ,p_y_width
                                  ,p_num_stem_wt);

    *dlt_canopy_width = new_width - g_canopy_width;
    *dlt_canopy_width = l_bound (*dlt_canopy_width, 0.0);

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Return the fractional increase in emergence of the oldest
//       expanding leaf and nodes.  Nodes can initiate from a user-defined
//       starting stage and leaves from emergence.  The initiation of both
//       leaves and nodes finishes at a user-defined end stage.
//       Note ! this does not take account of the other younger leaves
//       that are currently expanding

//+  Mission Statement
//   Calculate the potential increase in plant leaf and node number

//+  Changes
//       270598 nih specified and programmed
void Plant::cproc_leaf_no_pot3
    (
     float  *c_x_node_no_app                  //(INPUT)                                                     
    ,float  *c_y_node_app_rate                //(INPUT)                                                     
    ,int    c_num_node_no_app                 // (INPUT)                                                    
    ,float  *c_x_node_no_leaf                 // (INPUT)                                                    
    ,float  *c_y_leaves_per_node              // (INPUT)
    ,int    c_num_node_no_leaf                // (INPUT)
    ,float  g_current_stage                   // (INPUT)  current phenological stage
    ,int    start_node_app                    // (INPUT)  stage of start of leaf appeara
    ,int    end_node_app                      // (INPUT)  stage of end of leaf appearanc
    ,int    emerg                             // (INPUT)  emergence stage
    ,float  g_dlt_tt                          // (INPUT)  daily thermal time (growing de
    ,float  *g_node_no                        // (INPUT)  number of fully expanded nodes
    ,float  g_nfact_expansion                                                                               
    ,float  g_swdef_expansion                                                                               
    ,float  *g_leaves_per_node                 // OUTPUT                                                               
    ,float  *dlt_leaf_no_pot                   // (OUTPUT) new fraction of oldest expanding leaf             
    ,float  *dlt_node_no_pot                   // (OUTPUT) new fraction of oldest expanding node on main stem
    ) {

//+  Constant Values
    const char*  my_name = "cproc_leaf_no_pot3" ;

//+  Local Variables
    float node_no_now;                            // number of fully expanded nodes
    float dlt_leaves_per_node;
    float node_app_rate;
    float leaves_per_node_now;

//- Implementation Section ----------------------------------

    push_routine (my_name);

    node_no_now = sum_between (start_node_app-1
                              ,end_node_app-1
                              ,g_node_no);
                          
    node_app_rate = linear_interp_real(node_no_now
                                       ,c_x_node_no_app
                                       ,c_y_node_app_rate
                                       ,c_num_node_no_app);

    if (stage_is_between (start_node_app, end_node_app, g_current_stage)) 
        {
        *dlt_node_no_pot = divide (g_dlt_tt, node_app_rate, 0.0);
        }
    else
        {
        *dlt_node_no_pot = 0.0;
        }

    if (on_day_of (emerg, g_current_stage))
        {
        // no leaf growth on first day because initialised elsewhere ???
        *dlt_leaf_no_pot = 0.0;
    
        *g_leaves_per_node = linear_interp_real(node_no_now
                                              ,c_x_node_no_leaf
                                              ,c_y_leaves_per_node
                                              ,c_num_node_no_leaf);
        }       
    else if (stage_is_between (emerg, end_node_app,g_current_stage)) 
        {
        // NIH - bit scary using dlt_node_no_POT
        // so make sure that we don't get ahead of ourselves
        // of node number does not increase at potential rate
        
        leaves_per_node_now = linear_interp_real(node_no_now
                                                 ,c_x_node_no_leaf
                                                 ,c_y_leaves_per_node
                                                 ,c_num_node_no_leaf);
        
        *g_leaves_per_node = min(*g_leaves_per_node,leaves_per_node_now);
        
        dlt_leaves_per_node = linear_interp_real( node_no_now+(*dlt_node_no_pot)
                                                ,c_x_node_no_leaf
                                                ,c_y_leaves_per_node
                                                ,c_num_node_no_leaf)
                                 - leaves_per_node_now;
        
        *g_leaves_per_node = (*g_leaves_per_node) + dlt_leaves_per_node * min(pow(g_nfact_expansion,2),g_swdef_expansion);
        
        *dlt_leaf_no_pot = (*dlt_node_no_pot) * (*g_leaves_per_node);
        }
    else
        {
        *dlt_leaf_no_pot = 0.0;
        }
    
    
    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Return the lai that would senesce on the
//       current day.

//+  Mission Statement
//   Calculate today's leaf area senescence

//+  Changes
//     200498 nih specified and programmed
void Plant::legopt_leaf_area_sen1
    (
     int    emergence                        // (INPUT) emergence stage no.                       
    ,int    this_stage                       // (INPUT) This current stage                        
    ,float  g_dlt_lai_stressed               // (INPUT)  potential change in live                 
    ,float  g_dlt_leaf_no                    // (INPUT)  actual fraction of oldest leaf           
    ,float  g_dlt_leaf_no_dead               // (INPUT)  fraction of oldest green leaf            
    ,float  g_lai                            // (INPUT)  live plant green lai                     
    ,float  *g_leaf_area                     // (INPUT)  leaf area of each leaf (mm^2)            
    ,float  *g_leaf_no                                                                            
    ,float  *g_leaf_no_dead                  // (INPUT)  no of dead leaves ()                     
    ,int    max_node                                                                              
    ,float  g_plants                         // (INPUT)  Plant density (plants/m^2)               
    ,float  g_slai                           // (INPUT)  area of leaf that senesces fro           
    ,float  c_min_tpla                       // (INPUT)                                           
    ,float  *g_dlt_slai_age                   // (OUTPUT) new senesced lai from phasic devel.      
    ,float  c_lai_sen_light                  // (INPUT)                                           
    ,float  c_sen_light_slope                // (INPUT)                                           
    ,float  *g_dlt_slai_light                 // (OUTPUT)                                          
    ,float  c_sen_rate_water                 // (INPUT)                                           
    ,float  g_swdef_photo                    // (INPUT)                                           
    ,float  *g_dlt_slai_water                 // (OUTPUT)                                          
    ,float  *c_x_temp_senescence             // (INPUT)                                           
    ,float  *c_y_senescence_fac              // (INPUT)                                           
    ,int    c_num_temp_senescence            // (INPUT)                                           
    ,float  g_mint                           // (INPUT)                                           
    ,float  *g_dlt_slai_frost                 // (OUTPUT)                                          
    ,float  *g_dlt_slai                       // (OUTPUT)                                          
    ) {

//+  Constant Values
    const char*  my_name = "legopt_leaf_area_sen" ;

//- Implementation Section ----------------------------------
    push_routine (my_name);
    legopt_leaf_area_sen_age1(emergence
                              , this_stage
                              , g_leaf_no
                              , g_leaf_no_dead
                              , g_dlt_leaf_no_dead
                              , max_node
                              , g_lai
                              , g_slai
                              , c_min_tpla
                              , g_leaf_area
                              , g_plants
                              , g_dlt_slai_age );

    crop_leaf_area_sen_light1(c_lai_sen_light
                              , c_sen_light_slope
                              , g_lai
                              , g_plants
                              , c_min_tpla
                              , g_dlt_slai_light);

    crop_leaf_area_sen_water1 (c_sen_rate_water,
                               g_lai,
                               g_swdef_photo,
                               g_plants,
                               c_min_tpla,
                               g_dlt_slai_water);

    crop_leaf_area_sen_frost1(c_x_temp_senescence,
                              c_y_senescence_fac,
                              c_num_temp_senescence,
                              g_lai,
                              g_mint,
                              g_plants,
                              c_min_tpla,
                              g_dlt_slai_frost);
//    fprintf(stdout, "%d,%.9f,%.9f,%.9f,%.9f\n",
//                g.day_of_year,*g_dlt_slai_age, *g_dlt_slai_light, *g_dlt_slai_water, *g_dlt_slai_frost);

    *g_dlt_slai = max(max(max(*g_dlt_slai_age, *g_dlt_slai_light), *g_dlt_slai_water), *g_dlt_slai_frost);

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Initialise leaf area.

//+  Mission Statement
//   Initialise plant leaf area (on first day of %2)

//+  Changes
//     210498 nih specified and programmed
void Plant::legopt_leaf_area_init1
    (
     float  c_initial_tpla              // (INPUT)  initial plant leaf area (mm^2)     
    ,float  c_leaf_no_at_emerg          //                                             
    ,int    init_stage                  // (INPUT)  initialisation stage               
    ,float  g_current_stage             // (INPUT)  current phenological stage         
    ,float  *g_days_tot                 // (INPUT)  duration of each phase (days)
    ,float  g_plants                    // (INPUT)  Plant density (plants/m^2)
    ,float  *lai                        // (OUTPUT) total plant leaf area              
    ,float  *leaf_area                  // (OUTPUT)                                    
    ) {

//+  Constant Values
    const char*  my_name = "legopt_leaf_area_init1" ;

//- Implementation Section ----------------------------------

    push_routine (my_name);

    if (on_day_of (init_stage, g_current_stage))
        {
        *lai = c_initial_tpla * smm2sm * g_plants;

        fill_real_array (leaf_area,
                         divide(c_initial_tpla,c_leaf_no_at_emerg,0.0),
                         (int)c_leaf_no_at_emerg);
        leaf_area[(int)c_leaf_no_at_emerg] =                      //ok
        divide(fmod(c_leaf_no_at_emerg, 1.0) * c_initial_tpla
               ,c_leaf_no_at_emerg, 0.0);
        }
    else
        {
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Return the the initial number of leaves.

//+  Mission Statement
//   Initialise leaf number (on first day of %3)

//+  Notes
//    NIH - I would prefer to use leaf_no_at_init and init_stage
//          for routine parameters for generalisation

//+  Changes
//       250598 nih specified and programmed
void Plant::legopt_leaf_no_init1
    (
     float  c_leaf_no_at_emerg          // (INPUT)  leaf number at emergence () 
    ,float  g_current_stage             // (INPUT)  current phenological stage  
    ,int    emerg                       // (INPUT)  emergence stage no
    ,float  *g_days_tot                 // (INPUT)  duration of each phase (days)
    ,float  *leaf_no                    // (OUTPUT) initial leaf number          
    ,float  *node_no                    // (OUTPUT) initial node number          
    ) {

//+  Constant Values
    const char*  my_name = "legopt_leaf_no_init1" ;

//- Implementation Section ----------------------------------

    push_routine (my_name);

    if (on_day_of (emerg, g_current_stage))
        {
        // initialise first leaves
        node_no[emerg-1] = c_leaf_no_at_emerg;
        fill_real_array (leaf_no, 1.0, int(c_leaf_no_at_emerg));
        leaf_no[(int)c_leaf_no_at_emerg]= fmod(c_leaf_no_at_emerg,1.0);
        }
    else
        {
        // no inital leaf no
        }

    pop_routine (my_name);
    return;
    }



//---------------------------------------------------------------------------
// Return the lai that would senesce on the
// current day due to ageing
//---------------------------------------------------------------------------
void Plant::legopt_leaf_area_sen_age1
    (
     int    emergence                           // (INPUT) emergence stage no.
    ,int    this_stage                          // (INPUT) This current stage
    ,float  *g_leaf_no
    ,float  *g_leaf_no_dead                     // (INPUT)  no of dead leaves ()
    ,float  g_dlt_leaf_no_dead                  // (INPUT)  fraction of oldest green leaf
    ,int    max_node
    ,float  g_lai                               // (INPUT)  live plant green lai
    ,float  g_slai                              // (INPUT)  area of leaf that senesces fro
    ,float  c_min_tpla                          // (INPUT)
    ,float  *g_leaf_area                        // (INPUT)  leaf area of each leaf (mm^2)
    ,float  g_plants                            // (INPUT)  Plant density (plants/m^2)
    ,float  *dlt_slai_age                        // (OUTPUT) new senesced lai from phasic devel.
    )
    {

    const char*  my_name = "legopt_leaf_area_sen_age1" ;
    float area_sen_dying_node;                    // senesced leaf area from current node dying (mm^2)
    int   dying_node;                             // current node number dying ()
    float leaf_no_dead;                           // today's number of dead leaves ()
    float slai_age;                               // lai senesced by natural ageing
    float min_lai;                                // min allowable LAI
    float max_sen;

// now calculate the leaf senescence
// due to normal phenological (phasic) development

// get highest leaf no. senescing today

    leaf_no_dead = sum_real_array (g_leaf_no_dead, max_node) + g_dlt_leaf_no_dead;
    dying_node = get_cumulative_index_real (leaf_no_dead, g_leaf_no, max_node);

// get area senesced from highest leaf no.
    if (dying_node >= 0)
        {
        area_sen_dying_node = divide ( leaf_no_dead - sum_real_array(g_leaf_no, dying_node)
                                      , g_leaf_no[dying_node]
                                      , 0.0) * g_leaf_area[dying_node];

        slai_age = (sum_real_array (g_leaf_area, dying_node)
                      + area_sen_dying_node)
                      * smm2sm * g_plants;

        min_lai = c_min_tpla * g_plants * smm2sm;
        max_sen = l_bound (g_lai - min_lai, 0.0);
        *dlt_slai_age = bound (slai_age - g_slai, 0.0, max_sen);
        }
    else
        {
        *dlt_slai_age = 0.0;
        }
//    fprintf(stdout, "%d,%.9f,%.9f,%.9f,%.9f\n",
//                g.day_of_year, slai_age, area_sen_dying_node, (float) dying_node, leaf_no_dead);

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Calculate plant rooting depth through time

//+  Mission Statement
//   Calculate today's rooting depth

//+  Changes
//     170498 nih specified and programmed
void Plant::legopt_root_depth1
    (
     float *g_dlayer                     // (INPUT)  layer thicknesses (mm)
    ,int   c_num_sw_ratio                // (INPUT) number of sw lookup pairs
    ,float *c_x_sw_ratio                 // (INPUT) sw factor lookup x
    ,float *c_y_sw_fac_root              // (INPUT) sw factor lookup y
    ,float *g_dul_dep                    // (INPUT) DUL (mm)
    ,float *g_sw_dep                     // (INPUT) SW (mm)
    ,float *p_ll_dep                     // (INPUT) LL (mm)
    ,float *c_root_depth_rate            // (INPUT) root front velocity (mm)
    ,float g_current_stage               // (INPUT) current growth stage
    ,float g_mint
    ,float g_maxt
    ,float *c_x_temp_root_advance
    ,float *c_y_rel_root_advance
    ,int   c_num_temp_root_advance
    ,float *p_xf                         // (INPUT) exploration factor
    ,float g_root_depth                  // (input) root depth (mm)
    ,float *g_dlt_root_depth              // (OUTPUT) increase in rooting depth (mm)
    ) {

//+  Constant Values
    const char*  my_name = "legopt_root_depth1" ;

//+  Local Variables
    int   deepest_layer;                          // deepest layer in which the roots are growing
    float sw_avail_fac_deepest_layer;             //
    float temp_factor;
    float av_temp;

//- Implementation Section ----------------------------------
    push_routine (my_name);

    deepest_layer = find_layer_no (g_root_depth, g_dlayer, crop_max_layer);

    sw_avail_fac_deepest_layer = crop_sw_avail_fac ( c_num_sw_ratio
                                                   ,c_x_sw_ratio
                                                   ,c_y_sw_fac_root
                                                   ,g_dul_dep
                                                   ,g_sw_dep
                                                   ,p_ll_dep
                                                   ,deepest_layer );

    av_temp = (g_mint + g_maxt)/2.0;

    temp_factor = linear_interp_real(av_temp
                                     ,c_x_temp_root_advance
                                     ,c_y_rel_root_advance
                                     ,c_num_temp_root_advance);

    legopt_root_depth_increase(c_root_depth_rate
                               , g_current_stage
                               , g_dlayer
                               , g_root_depth
                               , sw_avail_fac_deepest_layer
                               , p_xf
                               , temp_factor
                               , g_dlt_root_depth);

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Return the increase in root depth (mm)

//+  Mission Statement
//   Calculate the increase in rooting depth.

//+  Notes
//         there is a discrepency when the root crosses into another
//         layer. - cr380

//+  Changes
//      031097 nih specified and programmed
void Plant::legopt_root_depth_increase
    (
     float  c_root_depth_rate[]                 // (INPUT)  root growth rate potential (mm
    ,float  g_current_stage                    // (INPUT)  current phenological stage
    ,float  *g_dlayer                          // (INPUT)  thickness of soil layer I (mm)
    ,float  g_root_depth                       // (INPUT)  depth of roots (mm)
    ,float  g_sw_avail_fac_deepest_layer       // (INPUT)
    ,float  *p_xf                              // (INPUT) eXploration Factor (0-1)
    ,float  temp_factor
    ,float  *dlt_root_depth                     // (OUTPUT) increase in root depth (mm)
    ) {

//+  Constant Values
    const char*  my_name = "legopt_root_depth_increase" ;

//+  Local Variables
    int   current_phase;                          // current phase number
    float root_depth_max;                         // maximum depth to which roots can go (mm)
    int   current_layer;                          // layer of root front
    int   deepest_layer;                          // deepest layer for rooting

//- Implementation Section ----------------------------------

    push_routine (my_name);

    current_layer = find_layer_no(g_root_depth, g_dlayer, crop_max_layer);
    current_phase = (int) g_current_stage;

// this equation allows soil water in the deepest
// layer in which roots are growing
// to affect the daily increase in rooting depth.

    *dlt_root_depth  = c_root_depth_rate[current_phase-1]
                              * g_sw_avail_fac_deepest_layer
                              * p_xf[current_layer]
                              * temp_factor;

// constrain it by the maximum
// depth that roots are allowed to grow.

    deepest_layer = count_of_real_vals (p_xf, crop_max_layer);
    root_depth_max = sum_real_array (g_dlayer, deepest_layer+1);
    *dlt_root_depth = u_bound (*dlt_root_depth, root_depth_max - g_root_depth);

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Return actual plant nitrogen uptake from each soil layer.

//+  Mission Statement
//   Calculate crop Nitrogen Uptake

//+  Changes
void Plant::cproc_n_uptake3
    (
     float  *g_dlayer                            // (INPUT)  thickness of soil layer I (mm)                      
    ,int    max_layer                            // (INPUT)  max number of soil layers                           
    ,float  *g_no3gsm_uptake_pot                 // (INPUT)  potential NO3 (supply)                              
    ,float  g_n_fix_pot                          // (INPUT) potential N fixation (g/m2)                          
    ,const char   *c_n_supply_preference         // (INPUT)c_n_supply_preference*(*)
    ,float  *g_n_demand                          // (INPUT)  critical plant nitrogen demand
    ,float  *g_n_max                             // (INPUT)  maximum plant nitrogen demand
    ,int    max_part                             // (INPUT)  number of plant parts
    ,float  g_root_depth                         // (INPUT)  depth of roots (mm)
    ,float  *dlt_no3gsm                          // (OUTPUT) actual plant N uptake from NO3 in each layer (g/m^2)
    ) {

//+  Constant Values
    const char*  my_name = "cproc_n_uptake3" ;

//+  Local Variables
    int   deepest_layer;                          // deepest layer in which the roots are growing
    int   layer;                                  // soil layer number of profile
    float n_demand;                               // total nitrogen demand (g/m^2)
    float no3gsm_uptake;                          // plant NO3 uptake from layer (g/m^2)
    float n_max;                                  // potential N uptake per plant (g/m^2)
    float no3gsm_supply;
    float scalef;

//- Implementation Section ----------------------------------

    push_routine (my_name);

    deepest_layer = find_layer_no (g_root_depth, g_dlayer, max_layer);

    no3gsm_supply = sum_real_array (g_no3gsm_uptake_pot, deepest_layer+1);

    n_demand = sum_real_array (g_n_demand, max_part);

    if (Str_i_Eq(c_n_supply_preference,"fixation"))
        {
        n_demand = l_bound (n_demand - g_n_fix_pot, 0.0);
        }

    //n_max    = sum_real_array (g_n_max, max_part);  //XX unused? check NH

    // get actual change in N contents

    fill_real_array (dlt_no3gsm, 0.0, max_layer);

    if (n_demand > no3gsm_supply)
        {
        scalef = 0.99999  ;                       // avoid taking it all up as it can
                                                  // cause rounding errors to take
                                                  // no3 below zero.
        }
    else
        {
        scalef = divide (n_demand
                         ,no3gsm_supply
                         ,0.0);
        }

    for (layer = 0; layer <= deepest_layer; layer++) 
        {
         // allocate nitrate
        no3gsm_uptake = g_no3gsm_uptake_pot[layer] * scalef;
        dlt_no3gsm[layer] = - no3gsm_uptake;
        }
    }


//+  Purpose
//      Calculate nitrogen supplies from soil and fixation

//+  Mission Statement
//   Calculate crop Nitrogen supplies (soil + fixation)

//+  Changes
//     21-04-1998 - neilh - Programmed and Specified
void Plant::cproc_n_supply3 (
     float  g_dlayer[]                        // (INPUT)
    ,int    max_layer                        // (INPUT)
    ,float  g_no3gsm[]                        // (INPUT)
    ,float  g_no3gsm_min[]                    // (INPUT)
    ,float  *g_no3gsm_uptake_pot
    ,float  g_root_depth                     // (INPUT)
    ,float  *g_root_length
    ,float  g_bd[]
    ,float  c_no3_uptake_max
    ,float  c_no3_conc_half_max
    ,float  *g_sw_avail_pot                  // (INPUT)
    ,float  *g_sw_avail                      // (INPUT)
    ,float  g_current_stage                  // (INPUT)
    ,float  *c_n_fix_rate                    // (INPUT)
    ,float  fixation_determinant             // (INPUT)
    ,float  g_swdef_fixation                 // (INPUT)
    ,float  g_n_fix_pot                      // (INPUT)
    ) {

//+  Local variables
    float no3ppm;
    int   deepest_layer;
    int layer;
    float umax;
    float swfac;

//+  Constant Values
    const char*  myname = "cproc_n_supply3" ;

//- Implementation Section ----------------------------------
    push_routine (myname);

    deepest_layer = find_layer_no (g_root_depth, g_dlayer, max_layer);

    for (layer = 0; layer<= deepest_layer; layer++)
        {
        no3ppm = (g_no3gsm[layer] - g_no3gsm_min[layer])
           * divide (1000.0, g_bd[layer]*g_dlayer[layer], 0.0);
        umax = c_no3_uptake_max * divide(no3ppm
        ,no3ppm + c_no3_conc_half_max
        ,0.0);
        umax=l_bound(umax,0.0);
    
                                                      //**2
        swfac = divide(g_sw_avail[layer],g_sw_avail_pot[layer],0.0) ;
        swfac = bound(swfac,0.0,1.0);
    
        g_no3gsm_uptake_pot[layer] = umax
        * g_root_length[layer] * 1e6
        * swfac;                                      //mm2 to m2
    
        g_no3gsm_uptake_pot[layer]
        = u_bound(g_no3gsm_uptake_pot[layer]
        ,g_no3gsm[layer]-g_no3gsm_min[layer]);
    
        }

    // determine N from fixation
    crop_n_fixation_pot1 (g_current_stage
                         , c_n_fix_rate
                         , fixation_determinant
                         , g_swdef_fixation
                         , &g_n_fix_pot);

    pop_routine (myname);
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
                            start_grain_fill,
                            g.dm_green,
                            stem,
                            p.grains_per_gram_stem,
                            &g.grain_no);
        }
    else
        {
        fatal_error (&err_internal, "invalid template option");
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Perform grain number calculations

//+  Changes
void Plant::crop_grain_number (
     float g_current_stage
    ,float *g_days_tot
    ,int   emerg
    ,int   start_grain_fill
    ,float *dm_green
    ,int   stem
    ,float p_grains_per_gram_stem
    ,float *g_grain_no    // OUTPUT
    ) {

//+  Constant Values
    const char*  my_name = "crop grain number" ;

//+  Local Variables

//- Implementation Section ----------------------------------

    push_routine (my_name);

    if (on_day_of (emerg, g_current_stage))
        {
        // seedling has just emerged.
        *g_grain_no = 0.0;
        }
    else if (on_day_of (start_grain_fill, g_current_stage))
        {
        // we are at first day of grainfill.
        *g_grain_no = p_grains_per_gram_stem * dm_green[stem];
        }
    else
        {
        // no changes
        }

    pop_routine (my_name);
    return;
    }


//+  Purpose
//       Perform grain filling calculations

//+  Changes
void Plant::legnew_bio_yieldpart_demand2(
     float g_current_stage
    ,int   start_grain_fill
    ,int   end_grain_fill
    ,float g_grain_no
    ,float p_potential_grain_filling_rate
    ,float g_maxt
    ,float g_mint
    ,float *c_x_temp_grainfill
    ,float *c_y_rel_grainfill
    ,int   c_num_temp_grainfill
    ,float *g_dlt_dm_grain_demand
    ) {


//+  Constant Values
    const char*  my_name = "legnew_bio_yieldpart_demand2" ;

//+  Local Variables
    float tav;

//- Implementation Section ----------------------------------

    push_routine (my_name);

    if (stage_is_between(start_grain_fill, end_grain_fill, g_current_stage))
        {
        // we are in grain filling stage
        tav = (g_maxt+g_mint)/2.0;
    
        *g_dlt_dm_grain_demand = g_grain_no
                    * p_potential_grain_filling_rate
                    * linear_interp_real(tav
                                         ,c_x_temp_grainfill
                                         ,c_y_rel_grainfill
                                         ,c_num_temp_grainfill);
    
        }
    else
        {
        // no changes
        *g_dlt_dm_grain_demand = 0.0;
        }
    
    
    pop_routine (my_name);
    return;
    }


// ==================================================================
// Nwheat Phenology model taken from CROPMOD module
// ==================================================================

//+  Purpose
//     Use temperature, photoperiod and genetic characteristics
//     to determine when the crop begins a new growth phase.
//     The initial daily thermal time and height are also set.

//+  Mission Statement
//   Calculate crop phenological development using thermal time targets.

//+  Changes
//     240498 nih specified and programmed
//     240599 ew reprogrammed to take out the stress in thermal time
void Plant::cproc_phenology_nw (
     float g_previous_stage
    ,float g_current_stage
    ,int   sowing_stage
    ,int   germ_stage
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
    ,float g_dlt_tt
    ,float *g_phase_tt
    ,float g_phase_devel
    ,float g_dlt_stage
    ,float *g_tt_tot
    ,float *g_days_tot
    ) {

//+  Constant Values
    const char*  my_name = "cproc_phenology_nw" ;

//+  Local variables
    float fstress;
    float g_dlt_tt_phenol;

    float tempcx;                                 //maximum crown temp
    float tempcn;                                 //minimum crown temp

//- Implementation Section ----------------------------------

    push_routine (my_name);

    g_previous_stage = g_current_stage;

// get thermal times
    //c==============================================================================;
    //c        call crop_thermal_time_nw (;
    //c     :                             g_maxt,;
    //c     :                             g_mint,;
    //c     :                             0.0,;
    //c     :                             26.0,;
    //c     :                             34.0,;
    //c     :                             g_dlt_tt);

    //c==============================================================================;
//USE CROWN TEMPERATURE AND THREE HOURS THERMAL TIME

    crop_crown_temp_nwheat (g_maxt,g_mint,0.0,&tempcx,&tempcn);

    g_dlt_tt = linear_interp_real((tempcx+tempcn)/2.0
                                  ,c_x_temp
                                  ,c_y_tt
                                  ,c_num_temp);

    //c         call crop_thermal_time;
    //c     :               (;
    //c     :                c_num_temp;
    //c     :              , c_x_temp;
    //c     :              , c_y_tt;
    //c     :              , g_current_stage;
    //c     :              , tempcx           ;     //G_maxt
    //c     :              , tempcn           ;     //G_mint
    //c     :              , start_stress_stage;
    //c     :              , end_stress_stage;
    //c     :              , 1.0              ;     //G_nfact_pheno
    //c     :              , 1.0              ;     //G_swdef_pheno
    //c     :              , g_dlt_tt;
    //c     :               );

    //c==============================================================================;

    if (stage_is_between (start_stress_stage,end_stress_stage, g_current_stage))
        {
        fstress = min (g_swdef_pheno, g_nfact_pheno);
        }
    else
        {
        fstress = 1.0;
        }


    g_dlt_tt        = g_dlt_tt ;                      //*fstress Enli deleted the stress

    g_dlt_tt_phenol = g_dlt_tt*fstress*min(g_vern_eff,g_photop_eff);

    crop_phase_devel (sowing_stage
                     , germ_stage
                     , end_development_stage
                     , c_pesw_germ
                     , c_fasw_emerg
                     , c_rel_emerg_rate
                     , c_num_fasw_emerg
                     , g_current_stage
                     , g_days_tot
                     , g_dlayer
                     , max_layer
                     , g_sowing_depth
                     , g_sw_dep
                     , g_dul_dep
                     , p_ll_dep
                     , g_dlt_tt_phenol
                     , g_phase_tt
                     , g_tt_tot
                     , &g_phase_devel);

    crop_devel(max_stage
               , g_phase_devel
               , &g_dlt_stage
               , &g_current_stage);

    // update thermal time states and day count

    accumulate (g_dlt_tt_phenol, g_tt_tot,g_previous_stage-1, g_dlt_stage);

    accumulate (1.0, g_days_tot, g_previous_stage-1, g_dlt_stage);

    pop_routine (my_name);
    return;
    }


//+  Purpose
//     Calculate min and max crown temperatures.

//+  Changes
//       280394 nih - programmed and specified
//       030399 ew  - regprogrammed
void Plant::crop_crown_temp_nwheat
   ( float tempmx        //Daily maximum temperature of the air (C)              
    ,float tempmn        //Daily minimum temperature of the air (C)              
    ,float snow          //Snow depth on the current day (mm)                    
    ,float *tempcx        //Daily maximum of crown temperature (C)     - OUTPUT   
    ,float *tempcn        //Daily minimum of crown temperature (C)     - OUTPUT   
    ) {

//+  Constant Values
    const char*  myname = "crop_crown_temp_nwheat" ;

//- Implementation Section ----------------------------------

    push_routine (myname);

// Calculate max crown temperature
    if (tempmx < 0.)
        {
        *tempcx = 2.0 + tempmx * (0.4 + 0.0018 * pow(snow - 15., 2));
        }
    else
        {
        *tempcx = tempmx;
        }

// Calculate min crown temperature
    if (tempmn < 0.)
        {
        *tempcn = 2.0 + tempmn * (0.4 + 0.0018 * pow(snow - 15., 2));
        }
    else
        {
        *tempcn = tempmn;
        }

    pop_routine (myname);
    return;
    }


//+  Purpose
//     Calculate daily vernalisation and accumulate to g_cumvd

//+  Mission Statement
//     Calculate todays vernalization (used to affect phenology)

//+  Notes
//   Nwheat originally had the following if logic for determining whether
//   vernalisation is calculated for today
//     if     (          cumvd .lt. reqvd
//     :                        .and.
//     :       (istage .eq.emerg .or. istage .eq. germ)   )
//     :then
//
//   In order to remove the explicit value 'reqvd' and make the stages
//   more flexibile this logic was replaced. - NIH 14/07/98

//+  Changes
//     14/07/98 nih taken from Nwheat
void Plant::wheat_vernaliz_days_nwheat
   ( float g_current_stage     //The current development stage                                                              
    ,int   start_stage         //Stage vernalisation begins                   
    ,int   end_stage           //Stage vernalisation ends                     
    ,float g_maxt              //Daily maximum Temperature                    
    ,float g_mint              //Daily minimum temperature                    
    ,float g_snow              //Snow depth of the day (mm)                   
    ,float *g_dlt_cumvd         //vernalisation day today                      OUTPUT
    ,float g_cumvd             //cumulative vernalisation days till yesterday 
   )  {                         

//+  Constant Values
    const char*  myname = "wheat_vernalization" ;

//+  Local Variables
    float tempcn;
    float tempcx;
    float tempcr;
    float vd,vd1,vd2;

//- Implementation Section ----------------------------------
    push_routine (myname);

    if (stage_is_between(start_stage,end_stage
        ,g_current_stage)) 
        {
       // The cumulative vernalization has not reached the required level of vernalization
   
       crop_crown_temp_nwheat (g_maxt,g_mint,g_snow,&tempcx,&tempcn);
   
       tempcr = (tempcn+tempcx)/2.0;
   
       if (g_mint < 15.0 && g_maxt > 0.0)
           {
           vd1 = 1.4 - 0.0778 * tempcr;
           vd2 = 0.5 + 13.44 / pow(g_maxt-g_mint + 3., 2) * tempcr;
           vd = min (vd1, vd2);
           vd = l_bound (vd, 0.0);
           *g_dlt_cumvd = vd;
           }
       else
           {
           // too cold or too warm - no vernalization
           }
       if (g_maxt > 30. && g_cumvd+(*g_dlt_cumvd) < 10.)
           {
           // high temperature will reduce vernalization
           *g_dlt_cumvd = - 0.5*(g_maxt - 30.);
           *g_dlt_cumvd = - min(-(*g_dlt_cumvd), g_cumvd);
           }
       else
           {
           }
       }
   else
       {
       *g_dlt_cumvd = 0.0;
       }


   pop_routine (myname);
   return;
   }

//+  Purpose
//     <insert here>

//+  Mission Statement
//     Vernalisation factor

//+  Changes
//     <insert here>
void Plant::wheat_vernaliz_effect_nwheat
   (
     float current_stage
    ,int   start_stage
    ,int   end_stage
    ,float p_vern_sens
    ,float cumvd
    ,float dlt_cumvd
    ,float reqvd
    ,float *vern_effect        //OUTPUT
    ) {

//+  Constant Values
    const char*  my_name = "wheat_vernaliz_effect" ;

//+  Local Variables
    float vfac;                                   // vernalization factor
    float vern_sens_fac;

//- Implementation Section ----------------------------------
    push_routine (my_name);

    if (stage_is_between(start_stage,end_stage,current_stage))
        {

        if (reqvd < 0.0) { reqvd = 50.0; }
        vern_sens_fac =  p_vern_sens* 0.0054545 + 0.0003;
        vfac = 1. - vern_sens_fac * (reqvd - (cumvd+dlt_cumvd));
        *vern_effect = bound (vfac, 0.0, 1.0);
        }
    else
        {
        *vern_effect = 1.0;
        }

    pop_routine (my_name);
    return;
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
    phase_tt[end_grain_to_maturity-1] = 0.05*(  phase_tt[flower_to_start_grain]  + p_startgf_to_mat);
    //c       endif;

    //c       if (p_tt_start_to_end_grain > 1.0) then;
    //c         phase_tt(start_to_end_grain)    = p_tt_start_to_end_grain;
    //c       else;
    phase_tt[start_to_end_grain-1]    = p_startgf_to_mat - phase_tt[end_grain_to_maturity];
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
//     <insert here>

//+  Mission Statement
//     Photoperiod factor

//+  Changes
//     <insert here>
void Plant::wheat_photoperiod_effect(
     float current_stage
    ,int   start_stage
    ,int   end_stage
    ,float photoperiod
    ,float p_photop_sen
    ,float *photop_eff
    ) {

//+  Constant Values
    const char*  my_name = "wheat_photoperiod_effect" ;

//+  Local Variables
    float photop_sen_factor;

//- Implementation Section ----------------------------------
    push_routine (my_name);

    if (stage_is_between(start_stage,end_stage,current_stage))
        {

        photop_sen_factor = p_photop_sen * 0.002;

        *photop_eff = 1. - photop_sen_factor * pow(20. - photoperiod, 2);
        *photop_eff = bound (*photop_eff, 0.0, 1.0);

        }
    else
        {
        *photop_eff = 1.0;
        }

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

        plant_water_stress (1);
        plant_oxdef_stress (1);

        plant_phenology_init (c.phenology_option);
        plant_phenology (c.phenology_option);

        plant_grain_number(c.grain_no_option);

//      if (g%plant_status.eq.status_alive) then

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
        plant_bio_init(1);
        plant_bio_actual (1);
        // c1
        //fprintf(stdout, "%d,%.9f,%.9f,%.9f\n", g.day_of_year,g.dlt_dm, g.dlt_dm_pot_rue, g.dlt_dm_pot_te);

        plant_bio_grain_demand_stress(1);

        plant_bio_grain_demand (c.grain_fill_option);
        plant_bio_grain_oil (1);

        plant_bio_partition (c.partition_option);

        plant_retrans_init(1);

        plant_bio_retrans (c.partition_option);
        // c2
        //fprintf(stdout, "%d,%.9f,%.9f,%.9f\n", g.day_of_year,
        //        g.dlt_dm_green[root],g.dlt_dm_green[leaf],g.dlt_dm_green[stem]);
        plant_leaf_area_actual (1);               // plant node/leaf approach with
                                                  // sla_max = f(lai)

        plant_pod_area (1);

        plant_leaf_no_actual(1);
        plant_root_length_init(1);                //added NIH
        plant_root_length_growth(1);              //added NIH

        plant_leaf_death (1);                     // 1 = fract leaf death rate
        plant_leaf_area_sen (1);
        //c2
//            fprintf(stdout, "%d,%.9f,%.9f,%.9f,%.9f\n", g.day_of_year,
//            g.dm_green[root] + g.dm_green[leaf] + g.dm_green[stem],
//            g.dm_green[root], g.dm_green[leaf],g.dm_green[stem]);

        plant_sen_bio (1);
        plant_sen_root_length(1);                 // added NIH
        plant_sen_nit (1);
        //c3
//            fprintf(stdout, "%d,%.9f,%.9f,%.9f,%.9f\n", g.day_of_year,
//            g.dm_green[root] + g.dm_green[leaf] + g.dm_green[stem],
//            g.dm_green[root], g.dm_green[leaf],g.dm_green[stem]);

        plant_nit_supply (c.n_uptake_option);
        plant_nit_init (1);
        plant_nit_retrans (1);                    // 1 accounts for dlt_dm_green_retrans
        plant_nit_demand (1);
        plant_nit_uptake (c.n_uptake_option);     // allows preference of N source
        plant_nit_partition (1);                  // allows output of n fixed

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
    plant_cleanup();

    plant_water_stress (1);
    plant_nit_stress (1);

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
        warning_error (&err_user, " is not in the ground - unable to harvest.");
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
         fatal_error(&err_user, "dormancy state must be specified");
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
        fatal_error (&err_user, "dormancy state is unknown - neither on nor off");
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
    float retain_fr;
    float canopy_fac;
    float temp;
    float fraction_to_residue[max_part];          // fraction sent to residue (0-1)
    float dlt_dm_crop[max_part];                  // change in dry matter of crop (kg/ha)
    float dlt_dm_n[max_part];                     // change in N content of dry matter (kg/ha)
    float dlt_dm_harvest;                         // dry matter harvested (g/m^2)
    float dlt_n_harvest;                          // N content of dm harvested (g/m^2)
    float dlt_dm_die;                             // dry matter in dieback of roots (g/m^2)
    float dlt_n_die;                              // N content of drymatter in dieback (g/m^2)
    float avg_leaf_area;

//- Implementation Section ----------------------------------
    push_routine (my_name);

    protocol::ApsimVariant incomingApsimVariant(parent);
    incomingApsimVariant.aliasTo(v.getMessageData());

    // determine the cutting height
    if (incomingApsimVariant.get("height", protocol::DTsingle, false, height) == false)
       {
       height = 0.0;
       }
    bound_check_real_var(height, 0.0, 1000.0, "height");

    g.previous_stage = g.current_stage;

    stage_no_current = int (g.current_stage);
    g.current_stage = c.stage_stem_reduction_harvest[stage_no_current-1];

    // determine the new stem density
    // ==============================
    if (incomingApsimVariant.get("plants", protocol::DTsingle, false, temp) == true)
        {
        bound_check_real_var(height, 0.0, 10000.0, "plants");
        g.plants = temp;        	
        }

    if (incomingApsimVariant.get("remove", protocol::DTsingle, false, remove_fr) == false)
        {
        remove_fr = 0.0;
        }
    bound_check_real_var(remove_fr, 0.0, 1.0, "remove");

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
            dlt_dm_die = g.dm_green[part] * c.root_die_back_fr;
            g.dm_senesced[part] = g.dm_senesced[part] + dlt_dm_die;
            dlt_n_die =  g.dm_green[part] * c.root_die_back_fr * c.n_sen_conc[part];
            g.n_senesced[part] = g.n_senesced[part] + dlt_n_die;
            g.n_green[part]= g.n_green[part] - dlt_n_die;

            g.dm_green[part] = g.dm_green[part] * (1.0 - c.root_die_back_fr);

            dlt_dm_crop[part] = dlt_dm_die * gm2kg/sm2ha;
            dlt_dm_n[part] = dlt_n_die * gm2kg/sm2ha;
            fraction_to_residue[part] = 0.0;
            }
        else
            {
           // we are accounting for tops
           // Calculate return of biomass to surface residues
            if (part==meal || part==oil)
                {
                // biomass is removed
                dlt_dm_n[part] = (g.n_dead[part] + g.n_green[part] + g.n_senesced[part])
                                     * gm2kg/sm2ha;

                dlt_dm_crop[part] = (g.dm_dead[part] + g.dm_green[part] + g.dm_senesced[part])
                                       * gm2kg/sm2ha;

                fraction_to_residue[part] = 0.0;

                g.dm_dead[part] = 0.0;
                g.dm_senesced[part] = 0.0;
                g.dm_green[part] = 0.0;

                g.n_dead[part] = 0.0;
                g.n_senesced[part] = 0.0;
                g.n_green[part] = 0.0;
                }
            else if (part==stem)
                {
                // Some biomass is removed according to harvest height
                fr_height = divide (height,g.canopy_height, 0.0);
                retain_fr = linear_interp_real (fr_height
                   ,c.fr_height_cut
                   ,c.fr_stem_remain
                   ,c.num_fr_height_cut);
                dlt_n_harvest = (g.n_dead[part] + g.n_green[part] + g.n_senesced[part])
                                 * (1.0-retain_fr);

                dlt_dm_harvest = (g.dm_dead[part] + g.dm_green[part] + g.dm_senesced[part])
                                  * (1.0-retain_fr);

                dlt_dm_crop[part] = dlt_dm_harvest * gm2kg/sm2ha;

                dlt_dm_n[part] = dlt_n_harvest * gm2kg/sm2ha;
                fraction_to_residue[part] = (1.0 - remove_fr);

                g.dm_dead[part] = retain_fr * g.dm_dead[part];
                g.dm_senesced[part] = retain_fr * g.dm_senesced[part];
                g.dm_green[part] = retain_fr * g.dm_green[part];

                g.n_dead[part] = retain_fr * g.n_dead[part];
                g.n_senesced[part] = retain_fr * g.n_senesced[part];
                g.n_green[part] = retain_fr * g.n_green[part];
                }
            else
                {
                // this includes leaf, pod
                dm_init = u_bound(c.dm_init [part] * g.plants, g.dm_green[part]);
                n_init = u_bound(dm_init * c.n_init_conc[part],g.n_green[part]);

                dlt_n_harvest = g.n_dead[part] + (g.n_green[part] - n_init) + g.n_senesced[part];
                dlt_dm_harvest = g.dm_dead[part] + (g.dm_green[part] - dm_init) + g.dm_senesced[part];

                dlt_dm_crop[part] = dlt_dm_harvest * gm2kg/sm2ha;

                dlt_dm_n[part] = dlt_n_harvest * gm2kg/sm2ha;
                fraction_to_residue[part] = (1.0 - remove_fr);

                g.dm_dead[part] = 0.0;
                g.dm_senesced[part] = 0.0;
                g.dm_green[part] = dm_init;

                g.n_dead[part] = 0.0;
                g.n_senesced[part] = 0.0;
                g.n_green[part] = n_init;

                }
            }
        }

    //     call crop_top_residue (c%crop_type, dm_residue, N_residue)

    if (sum_real_array(dlt_dm_crop, max_part) > 0.0)
        {
        plant_send_crop_chopped_event (c.crop_type
                                       , part_name
                                       , dlt_dm_crop
                                       , dlt_dm_n
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

    dm_root = dlt_dm_crop[root];
    n_root = dlt_dm_n[root];

    parent->writeString (" crop harvested.");
    parent->writeString (string("  tops residue =  " + Ftoa(dm_residue, 2) + " kg/ha").c_str());
    parent->writeString (string("  tops n       =  " + Ftoa(n_residue, 2) + " kg/ha").c_str());
    parent->writeString (string("  root residue =  " + Ftoa(dm_root, 2) + " kg/ha").c_str());
    parent->writeString (string("  root n       =  " + Ftoa(n_root, 2) + " kg/ha").c_str());

    dm_removed = 0.0;
    for (part=0; part < max_part; part++)
      dm_removed = dm_removed + dlt_dm_crop[part];
    dm_removed = dm_removed - dm_residue - dm_root;

    n_removed = 0.0;
    for (part=0; part < max_part; part++)
      n_removed = n_removed + dlt_dm_n[part];
    n_removed = n_removed - n_residue - n_root;

    dm_root = 0.0;
    n_root  = 0.0;

    parent->writeString (string("  tops removed =  "+ Ftoa(dm_removed, 2) + " kg/ha").c_str());
    parent->writeString (string("  tops n removed= "+ Ftoa(n_removed, 2) + " kg/ha").c_str());
    parent->writeString (string("  root removed =  "+ Ftoa(dm_root, 2) + " kg/ha").c_str());
    parent->writeString (string("  root n removed= "+ Ftoa(n_root, 2) + " kg/ha").c_str());

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
        bound_check_real_var(temp, 0.0, 10000.0, "plants");
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
      g.swdef_expansion=0;
      g.swdef_photo=0;
      g.swdef_pheno=0;
      g.swdef_fixation=0;
      g.sw_avail_fac_deepest_layer=0;
      g.nfact_expansion=0;
      g.nfact_photo=0;
      g.nfact_grain_conc=0;
      g.nfact_pheno=0;
      g.temp_stress_photo=0;
      g.oxdef_photo=0;
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
      fill_real_array (g.dlt_n_detached, 0, max_part);               
      fill_real_array (g.dlt_n_dead, 0, max_part);
      fill_real_array (g.dlt_n_dead_detached, 0, max_part);          
      fill_real_array (g.n_dead, 0, max_part);                  
      fill_real_array (g.n_green, 0, max_part);                 
      fill_real_array (g.n_senesced, 0, max_part);              
      fill_real_array (g.dlt_n_retrans, 0, max_part);           
      fill_real_array (g.dlt_no3gsm, 0, max_layer);             
      fill_real_array (g.no3gsm , 0, max_layer);                
      fill_real_array (g.no3gsm_min, 0, max_layer);             
      fill_real_array (g.no3gsm_diffn_pot, 0, max_layer);       
      fill_real_array (g.no3gsm_mflow_avail, 0, max_layer);     
      g.n_fix_pot=0;                         
      fill_real_array (g.no3gsm_uptake_pot, 0, max_layer);
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

    fill_real_array (g.leaf_area , 0.0, max_node);

    fill_real_array (p.ll_dep , 0.0, max_layer);
    fill_real_array (p.xf , 0.0, max_layer);
    fill_real_array (g.root_length , 0.0, max_layer);
    fill_real_array (g.root_length_dead, 0.0, max_layer);

    g.grain_no = 0.0;
    g.cumvd = 0.0;
    g.leaves_per_node = 0.0;

    g.dlt_plants_death_external     = 0.0;
    g.flowering_date        = 0;
    g.flowering_das         = 0;
    g.maturity_date         = 0;
    g.maturity_das          = 0;
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

    g.swdef_pheno = 0.0;
    g.swdef_photo = 0.0;
    g.swdef_expansion = 0.0;
    g.swdef_fixation = 0.0;
    g.nfact_pheno = 0.0;
    g.nfact_photo = 0.0;
    g.nfact_grain_conc = 0.0;

    g.n_fix_pot = 0.0;
    g.n_fix_uptake = 0.0;
    g.n_fixed_tops = 0.0;

//    p.uptake_source = " ";

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
    fill_real_array (g.dlt_sw_dep , 0.0, max_layer);
    fill_real_array (g.dm_green_demand , 0.0, max_part);
    fill_real_array (g.n_demand , 0.0, max_part);

    fill_real_array (g.dlt_dm_dead_detached, 0.0, max_part);
    fill_real_array (g.dlt_dm_detached , 0.0, max_part);
    fill_real_array (g.dlt_dm_senesced , 0.0, max_part);
    fill_real_array (g.dlt_n_dead_detached, 0.0, max_part);
    fill_real_array (g.dlt_n_detached , 0.0, max_part);
    fill_real_array (g.dlt_n_senesced , 0.0, max_part);
    fill_real_array (g.dlt_n_senesced_trans , 0.0, max_part);

    fill_real_array (g.sw_avail , 0.0, max_layer);
    fill_real_array (g.sw_avail_pot , 0.0, max_layer);
    fill_real_array (g.sw_supply , 0.0, max_layer);

    fill_real_array (g.dlt_root_length , 0.0, max_layer);
    fill_real_array (g.dlt_root_length_senesced, 0.0, max_layer);

    fill_real_array (g.no3gsm_uptake_pot, 0.0, max_layer);

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
    g.dlt_node_no              = 0.0;
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
        parent->writeString ( "sow");

        // Check anachronisms
        if (incomingApsimVariant.get("crop_type", protocol::DTstring, false, dummy) != false)
            {
            warning_error (&err_user, "crop type no longer used in sowing command");
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
            fatal_error(&err_user, "Cultivar not specified");
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
            fatal_error(&err_user, "plant density ('plants') not specified");
            }
        bound_check_real_var(g.plants, 0.0, 1000.0, "plants");

        if (incomingApsimVariant.get("sowing_depth", protocol::DTsingle, false, g.sowing_depth) == false)
            {
            fatal_error(&err_user, "sowing_depth not specified");
            }
        bound_check_real_var(g.sowing_depth, 0.0, 100.0, "sowing_depth");

        if (incomingApsimVariant.get("row_spacing", protocol::DTsingle, false, g.row_spacing) == false)
            {
            g.row_spacing = c.row_spacing_default;
            }
        bound_check_real_var(g.row_spacing, 0.0, 2000.0, "row_spacing");


        if (incomingApsimVariant.get("skipplant", protocol::DTsingle, false, g.skip_plant) == false)
            {
            g.skip_plant = c.skip_plant_default;
            }
        bound_check_real_var(g.skip_plant, 0.0, 2.0, "skipplant");
        g.skip_plant_fac = (2.0 + g.skip_plant)/2.0;

        if (incomingApsimVariant.get("skiprow", protocol::DTsingle, false, g.skip_row) == false)
            {
            g.skip_row = c.skip_row_default;
            }
        bound_check_real_var(g.skip_row, 0.0, 2.0, "skiprow");
        g.skip_row_fac = (2.0 + g.skip_row)/2.0;

        // Bang.
        g.current_stage = (float) sowing;
        g.plant_status = alive;

        parent->writeString ("");
        parent->writeString ("                 crop sowing data");
        parent->writeString ("    ------------------------------------------------");
        parent->writeString ("    sowing  depth plants spacing skip  skip  cultivar");
        parent->writeString ("    day no   mm     m^2     mm   row   plant name");
        parent->writeString ("    ------------------------------------------------");

        sprintf(msg, "   %7d%7.1f%7.1f%7.1f%6.1f%6.1f%20s"
               , g.day_of_year, g.sowing_depth
               , g.plants, g.row_spacing
               , g.skip_row, g.skip_plant, g.cultivar.c_str());
        parent->writeString (msg);

        parent->writeString ("    ------------------------------------------------\n");

        }
    else
        {
        string m = string(g.module_name + " is still in the ground -\n unable to sow until it is\n taken out by \"end_crop\" action.");
        fatal_error (&err_user, (char*)m.c_str());
        }

    pop_routine (my_name);
    return;
    }
////////////////////////////////
/// All these read_xxx need replacement: use templates
void read_real_array(protocol::Component* parent,
                     const string& sectionName,
                     const string& variableName,
                     int arraySize,
                     const string& units,
                     float values[],
                     int& numValues,
                     float lower,
                     float upper,
                     bool optional = false)
   {
   char buf[200];
   FString valueString(buf, 200, CString);
   numValues = 0;
   if (parent->readParameter(sectionName.c_str(),
                             variableName.c_str(),
                             valueString,
                             optional))
      {
      vector<string> vals;
      Split_string(asString(valueString), " ", vals);
      numValues = min(vals.size(), (unsigned)arraySize);
      for (int v = 0; v < numValues; v++)
         values[v] = atof(vals[v].c_str());

      bound_check_real_array(values, numValues, lower, upper, variableName.c_str());
      }
   }
void Plant::read_real_array(const vector<string>& search_order,
                     const string& variableName,
                     int arraySize,
                     const string& units,
                     float values[],
                     int& numValues,
                     float lower,
                     float upper)
{
   for (vector<string>::const_iterator i = search_order.begin();
        i != search_order.end();
        i++)
     {
     numValues = 0;
     ::read_real_array(this->parent, *i,
                       variableName, arraySize,
                       units, values, numValues,
                       lower, upper, true);
     if (numValues > 0)  return;
     }
}
void Plant::read_real_array(const string& sectionName,
                     const string& variableName,
                     int arraySize,
                     const string& units,
                     float values[],
                     int& numValues,
                     float lower,
                     float upper,
                     bool optional)
{
   ::read_real_array(this->parent, sectionName,
                     variableName, arraySize,
                     units, values, numValues,
                     lower, upper, optional);
}
/////////////
bool read_real_var(protocol::Component* parent,
                     const string &sectionName,
                     const string &variableName,
                     const string &units,
                     float& value,
                     float lower,
                     float upper,
                     bool optional=false)
   {
   char buf[200];
   FString valueString(buf, 200, CString);
   if (parent->readParameter(sectionName.c_str(),
                             variableName.c_str(),
                             valueString,
                             optional))
      {
      if (sscanf(valueString.f_str(),"%f", &value) != 1)
         {
         char msg[200];
         strcpy(msg, "Cannot read a real value from string\n"
                  "Parameter name = ");
         strcat(msg, variableName.c_str());
         strcat(msg, "\n");
         strcat(msg, "value = '");
         strncat(msg, valueString.f_str(),20);
         strcat(msg, "'");
         fatal_error(&err_user, msg, strlen(msg));
         return false;
         }
      bound_check_real_var(value, lower, upper, variableName.c_str());
      return true;
      }
   return false;
   }
bool Plant::read_real_var(const vector<string>& search_order,
                     const string& variableName,
                     const string& units,
                     float& value, float lower, float upper)
{
   for (vector<string>::const_iterator i = search_order.begin();
        i != search_order.end();
        i++)
     {
     if (::read_real_var(this->parent, *i,
                         variableName, units, value, lower, upper, true) == true)
         {
         return true;
         }
     }
   return false;
}
bool Plant::read_real_var(const string& sectionName,
                     const string& variableName,
                     const string& units,
                     float& value, float lower, float upper,
                     bool optional)
{
   return (::read_real_var(this->parent, sectionName,
                         variableName, units, value, lower, upper, optional));
}

////////
bool read_integer_var(protocol::Component* parent,
                     const string& sectionName,
                     const string& variableName,
                     const string& units,
                     int& value,
                     int lower,
                     int upper, bool optional = false)
   {
   char buf[200];
   FString valueString(buf, 200, CString);
   if (parent->readParameter(sectionName.c_str(),
                             variableName.c_str(),
                             valueString,
                             optional))
      {
      if (sscanf(valueString.f_str(),"%d", &value) != 1)
         {
         char msg[200];
         strcpy(msg, "Cannot read a real value from string\n"
                  "Parameter name = ");
         strcat(msg, variableName.c_str());
         strcat(msg, "\n");
         strcat(msg, "value = '");
         strncat(msg, valueString.f_str(),20);
         strcat(msg, "'");
         fatal_error(&err_user, msg, strlen(msg));
         return false;
         }
      bound_check_integer_var(value, lower, upper, (char*)variableName.c_str());
      return true;
      }
   return false;
   }
bool Plant::read_integer_var(const vector<string>& search_order,
                     const string& variableName,
                     const string& units,
                     int& value, int lower, int upper)
{
   for (vector<string>::const_iterator i = search_order.begin();
        i != search_order.end();
        i++)
     {
     if (::read_integer_var(this->parent, *i,
                           variableName, units, value, lower, upper, true) == true)
         {
         return true;
         }
     }
   return false;
}
bool Plant::read_integer_var(const string& sectionName,
                     const string& variableName,
                     const string& units,
                     int& value, int lower, int upper,
                     bool optional)
{
   return (::read_integer_var(this->parent, sectionName,
                              variableName, units, value, lower, upper, optional));
}

/////////////// basic version
bool read_char_var(protocol::Component* parent,
                     const string& sectionName,
                     const string& variableName,
                     const string& units,
                     string& value,
                     bool optional)
   {
   char buf[200];
   FString valueString(buf, 200, CString);
   if (parent->readParameter(sectionName.c_str(),
                             variableName.c_str(),
                             valueString,
                             optional))
      {
      value = asString(valueString);
      return true;
      }
   return false;
   }
bool Plant::read_char_var(const vector<string>& search_order,
                     const string& variableName,
                     const string& units,
                     string& value)
{
   for (vector<string>::const_iterator i = search_order.begin();
        i != search_order.end();
        i++)
     {
     if (::read_char_var(this->parent, *i,
                           variableName, units, value, true) == true)
         {
         	return true;
         }
     }
   return false;
}
bool Plant::read_char_var(const string& sectionName,
                     const string& variableName,
                     const string& units,
                     string& value,
                     bool optional)
{
   return (::read_char_var(this->parent, sectionName,
                           variableName, units, value, optional));
}
/////////////

// basic version..
bool read_char_array(protocol::Component* parent,
                     const string& sectionName,
                     const string& variableName,
                     const string& units,
                     vector<string>& values,
                     bool optional)
   {
   char buf[200];
   FString valueString(buf, 200, CString);
   values.clear();
   if (parent->readParameter(sectionName.c_str(),
                             variableName.c_str(),
                             valueString,
                             optional))
      {
      string value = asString(valueString);
      Split_string(value, " ", values);
      return true;
      }
   return false;
   }

void Plant::read_char_array(const vector<string>& search_order,
                     const string& variableName,
                     const string& units,
                     vector<string>& values)
{
   for (vector<string>::const_iterator i = search_order.begin();
        i != search_order.end();
        i++)
     {
     if (::read_char_array(this->parent, *i,
                           variableName, units, values, true) == true)
         {
         	return;
         }
     }
}
void Plant::read_char_array(const string& sectionName,
                     const string& variableName,
                     const string& units,
                     vector<string>& values,
                     bool optional)
{
   ::read_char_array(this->parent, sectionName,
                     variableName, units, values, optional);
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

    read_real_array (g.cultivar
    , "x_pp_hi_incr", max_table, "(h)"
    , p.x_pp_hi_incr
    , p.num_pp_hi_incr
    , 0.0, 24.0);

    read_real_array (g.cultivar
    , "y_hi_incr", max_table, "()"
    , p.y_hi_incr
    , p.num_pp_hi_incr
    , 0.0, 1.0);

    read_real_array (g.cultivar
    , "x_hi_max_pot_stress", max_table, "(0-1)"
    , p.x_hi_max_pot_stress, p.num_hi_max_pot
    , 0.0, 1.0);

    read_real_array (g.cultivar
    , "y_hi_max_pot", max_table, "(0-1)"
    , p.y_hi_max_pot, p.num_hi_max_pot
    , 0.0, 1.0);

    if (c.grain_no_option==2)
        {
        read_real_var (g.cultivar
        , "grains_per_gram_stem", "(/g)"
        , p.grains_per_gram_stem
        , 0.0, 10000.0);
        }
    if (c.grain_fill_option==2)
        {
        read_real_var (g.cultivar
        , "potential_grain_filling_rate", "(g/grain/day)"
        , p.potential_grain_filling_rate
        , 0.0, 1.0);
        }

//   plant_phenology_init

    if (c.phenology_option==1)
        {

        read_real_array (g.cultivar
        , "cum_vernal_days", max_table, "(vd)"
        , p.cum_vernal_days, p.num_cum_vernal_days
        , 0.0, 100.0);

        read_real_array (g.cultivar
        , "tt_emerg_to_endjuv", max_table, "(degday)"
        , p.tt_emerg_to_endjuv, p.num_cum_vernal_days
        , 0.0, c.tt_emerg_to_endjuv_ub);

        read_integer_var (g.cultivar
        , "est_days_emerg_to_init", "()"
        , p.est_days_emerg_to_init
        , 0, 100);

        read_real_array (g.cultivar
        , "x_pp_endjuv_to_init", max_table, "(h)"
        , p.x_pp_endjuv_to_init
        , p.num_pp_endjuv_to_init
        , 0.0, 24.0);

        read_real_array (g.cultivar
        , "y_tt_endjuv_to_init", max_table, "(degday)"
        , p.y_tt_endjuv_to_init
        , p.num_pp_endjuv_to_init
        , 0.0, 1e6);

        read_real_array (g.cultivar
        , "x_pp_init_to_flower", max_table, "(h)"
        , p.x_pp_init_to_flower
        , p.num_pp_init_to_flower
        , 0.0, 24.0);

        read_real_array (g.cultivar
        , "y_tt_init_to_flower", max_table, "(degday)"
        , p.y_tt_init_to_flower
        , p.num_pp_init_to_flower
        , 0.0, 1e6);

        read_real_array (g.cultivar
        , "x_pp_flower_to_start_grain"
        , max_table, "(h)"
        , p.x_pp_flower_to_start_grain
        , p.num_pp_flower_to_start_grain
        , 0.0, 24.0);

        read_real_array (g.cultivar
        , "y_tt_flower_to_start_grain"
        , max_table, "(degday)"
        , p.y_tt_flower_to_start_grain
        , p.num_pp_flower_to_start_grain
        , 0.0, 1e6);

        read_real_array (g.cultivar
        , "x_pp_start_to_end_grain"
        , max_table, "(h)"
        , p.x_pp_start_to_end_grain
        , p.num_pp_start_to_end_grain
        , 0.0, 24.0);

        read_real_array (g.cultivar
        , "y_tt_start_to_end_grain"
        , max_table, "(degday)"
        , p.y_tt_start_to_end_grain
        , p.num_pp_start_to_end_grain
        , 0.0, 1e6);

        read_real_var (g.cultivar
        , "tt_end_grain_to_maturity", "()"
        , p.tt_end_grain_to_maturity
        , 0.0, 1e6);

        read_real_var (g.cultivar
        , "tt_maturity_to_ripe", "()"
        , p.tt_maturity_to_ripe
        , 0.0, c.tt_maturity_to_ripe_ub);

// Nwheat Phenology
//=========================
        }
    else if (c.phenology_option==2)
        {
        read_real_var (g.cultivar
        , "phyllochron", "()"
        , p.phyllochron
        , 0.0, 300.);

        read_real_var (g.cultivar
        , "startgf_to_mat", "()"
        , p.startgf_to_mat
        , 0.0, 3000.);

        read_real_var (g.cultivar
        , "vern_sens", "()"
        , p.vern_sens
        , 0.0, 10.0);

        read_real_var (g.cultivar
        , "photop_sens", "()"
        , p.photop_sens
        , 0.0, 10.0);
        }

//=========================

    read_real_array (g.cultivar
    , "x_stem_wt", max_table, "(g/plant)"
    , p.x_stem_wt, p.num_stem_wt
    , 0.0, 1000.0);

    read_real_array (g.cultivar
    , "y_height", max_table, "(mm)"
    , p.y_height, p.num_stem_wt
    , 0.0, 5000.0);

    read_real_array (g.cultivar
    , "y_width", max_table, "(mm)"
    , p.y_width, p.num_canopy_widths
    , 0.0, 5000.0, true);
    if (p.num_canopy_widths ==0)
        {
        fill_real_array(p.y_width, 0.0, max_table);
        }
    else
        {
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
      s = s + Ftoa(p.cum_vernal_days[i], 2,10) + " ";
      }
    parent->writeString (s.c_str());

    s = string("   tt_emerg_to_endjuv         = ");
    for (int i = 0; i < p.num_cum_vernal_days; i++)
      {
      s = s + Ftoa(p.tt_emerg_to_endjuv[i], 2,10) + " ";
      }
    parent->writeString (s.c_str());

    s = string("   x_pp_endjuv_to_init        = ");
    for (int i = 0; i < p.num_pp_endjuv_to_init; i++)
      {
      s = s + Ftoa(p.x_pp_endjuv_to_init[i], 2,10) + " ";
      }
    parent->writeString (s.c_str());

    s = string("   y_tt_endjuv_to_init        = ");
    for (int i = 0; i < p.num_pp_endjuv_to_init; i++)
      {
      s = s + Ftoa(p.y_tt_endjuv_to_init[i], 2,10) + " ";
      }
    parent->writeString (s.c_str());

    s = string("   x_pp_init_to_flower        = ");
    for (int i = 0; i < p.num_pp_init_to_flower; i++)
      {
      s = s + Ftoa(p.x_pp_init_to_flower[i], 2,10) + " ";
      }
    parent->writeString (s.c_str());

    s = string("   y_tt_init_to_flower        = ");
    for (int i = 0; i < p.num_pp_init_to_flower; i++)
      {
      s = s + Ftoa(p.y_tt_init_to_flower[i], 2,10) + " ";
      }
    parent->writeString (s.c_str());

    s = string("   x_pp_flower_to_start_grain = ");
    for (int i = 0; i < p.num_pp_flower_to_start_grain; i++)
      {
      s = s + Ftoa(p.x_pp_flower_to_start_grain[i], 2,10) + " ";
      }
    parent->writeString (s.c_str());

    s = string("   y_tt_flower_to_start_grain = ");
    for (int i = 0; i < p.num_pp_flower_to_start_grain; i++)
      {
      s = s + Ftoa(p.y_tt_flower_to_start_grain[i], 2,10) + " ";
      }
    parent->writeString (s.c_str());

    s = string("   x_pp_start_to_end_grain    = ");
    for (int i = 0; i < p.num_pp_start_to_end_grain; i++)
      {
      s = s + Ftoa(p.x_pp_start_to_end_grain[i], 2,10) + " ";
      }
    parent->writeString (s.c_str());

    s = string("   y_tt_start_to_end_grain    = ");
    for (int i = 0; i < p.num_pp_start_to_end_grain; i++)
      {
      s = s + Ftoa(p.y_tt_start_to_end_grain[i], 2,10) + " ";
      }
    parent->writeString (s.c_str());

    s = string("   tt_end_grain_to_maturity   = ");
    s = s + Ftoa(p.tt_end_grain_to_maturity, 2,10);
    parent->writeString (s.c_str());

    s = string("   tt_maturity_to_ripe        = ");
    s = s + Ftoa(p.tt_maturity_to_ripe, 2,10);
    parent->writeString (s.c_str());

// TEMPLATE OPTION
    s = string("   x_pp_hi_incr               = ");
    for (int i = 0; i < p.num_pp_hi_incr; i++)
      {
      s = s + Ftoa(p.x_pp_hi_incr[i], 2,10) + " ";
      }
    parent->writeString (s.c_str());

    s = string("   y_hi_incr                  = ");
    for (int i = 0; i < p.num_pp_hi_incr; i++)
      {
      s = s + Ftoa(p.y_hi_incr[i], 4,10) + " ";
      }
    parent->writeString (s.c_str());

// TEMPLATE OPTION
    s = string("   x_hi_max_pot_stress        = ");
    for (int i = 0; i < p.num_hi_max_pot; i++)
      {
      s = s + Ftoa(p.x_hi_max_pot_stress[i], 2,10) + " ";
      }
    parent->writeString (s.c_str());

    s = string("   y_hi_max_pot               = ");
    for (int i = 0; i < p.num_hi_max_pot; i++)
      {
      s = s + Ftoa(p.y_hi_max_pot[i], 2,10) + " ";
      }
    parent->writeString (s.c_str());

    s = string("   x_stem_wt                  = ");
    for (int i = 0; i < p.num_stem_wt; i++)
      {
      s = s + Ftoa(p.x_stem_wt[i], 2,10) + " ";
      }
    parent->writeString (s.c_str());

    s = string("   y_height                   = ");
    for (int i = 0; i < p.num_stem_wt; i++)
      {
      s = s + Ftoa(p.y_height[i], 2,10) + " ";
      }
    parent->writeString (s.c_str());

    if (p.num_canopy_widths >0)
        {
        s = string("   y_width                   = ");
        for (int i = 0; i < p.num_canopy_widths; i++)
           {
           s = s + Ftoa(p.y_width[i], 2,10) + " ";
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
    int   num_layers;                             // number of layers in profile
    char  msg[200];

//- Implementation Section ----------------------------------

    push_routine (my_name);

    parent->writeString ("\n   - reading root profile parameters");

//       cproc_sw_demand_bound

    if (read_real_var (section_name
                       , "eo_crop_factor", "()"
                       , p.eo_crop_factor
                       , 0.0, 100., true) == false)

        {
        p.eo_crop_factor = c.eo_crop_factor_default;
        }
    else
        {
        }

//       plant_sw_supply

    if (read_char_var (section_name
                       , "uptake_source", "()"
                       , p.uptake_source, true) == false)

        {
        p.uptake_source = "calc";
        }
    else
        {
        }

    read_real_array (section_name
    , "ll", max_layer, "()"
    , ll, num_layers
    , 0.0, c.sw_ub);

    fill_real_array (p.ll_dep, 0.0, max_layer);
    for (int layer = 0; layer < num_layers; layer++)
       p.ll_dep[layer] = ll[layer]*g.dlayer[layer];

    read_real_array (section_name
    , "kl", max_layer, "()"
    , p.kl, num_layers
    , 0.0, c.kl_ub);

    read_real_array (section_name
    , "xf", max_layer, "()"
    , p.xf, num_layers
    , 0.0, 1.0);

// report

    parent->writeString ("                   root profile");
    parent->writeString ("---------------------------------------------------");
    parent->writeString ("     layer       kl           lower    exploration");
    parent->writeString ("     depth     factor         limit      factor  ");
    parent->writeString ("     (mm)         ()        (mm/mm)       (0-1)");
    parent->writeString ("---------------------------------------------------");

    for (layer = 0; layer < num_layers; layer++)
       {
       sprintf (msg, "%9.1f%10.3f%15.3f%12.3f"
          , g.dlayer[layer]
          , p.kl[layer]
          , ll[layer]
          , p.xf[layer]);
       parent->writeString (msg);
       }
    parent->writeString ("---------------------------------------------------\n");

    sprintf (msg, "(%s%5.1f%s)"
        ,"crop factor for bounding water use is set to "
        , p.eo_crop_factor
        , " times eo");
    parent->writeString (msg);

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
    float dm_root;                                // dry matter added to soil (g/m^2)
    float n_root;                                 // nitrogen added to soil (g/m^2)
    char  msg[400];
    float yield;                                  // grain wt (kg/ha)
    float fraction_to_residue[max_part];          // fraction sent to residue (0-1)
    float dlt_dm_crop[max_part];                  // change in dry matter of crop (kg/ha)
    float dlt_dm_n[max_part];                     // N content of changeed dry matter (kg/ha)
    int part;                                     // part

    push_routine (my_name);

    if (g.plant_status != out)
        {
        g.plant_status = out;
        g.current_stage = (float) plant_end;

        // report
        yield = (g.dm_green[meal] + g.dm_dead[meal]
          + g.dm_green[oil] + g.dm_dead[oil] )
          * gm2kg /sm2ha;
        sprintf (msg, "   crop ended. yield (dw) = %7.1f  (kg/ha)", yield);
        parent->writeString (msg);

        // now do post harvest processes
        dm_root = g.dm_green[root] + g.dm_senesced[root];

        n_root  = g.n_green[root] + g.n_senesced[root];

        plant_root_incorp (dm_root, n_root, g.root_length);

        plant_root_incorp (g.dm_dead[root], g.n_dead[root], g.root_length_dead);

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
            plant_send_crop_chopped_event ( c.crop_type
                  , part_name
                  , dlt_dm_crop
                  , dlt_dm_n
                  , fraction_to_residue
                  , max_part);
            }
        else
            {
            // no surface residue
            }

        dm_root = g.dm_green[root] + g.dm_dead[root] + g.dm_senesced[root];
        n_root  = g.n_green[root] + g.n_dead[root] + g.n_senesced[root];

        sprintf (msg, "%s%7.1f%s"
        , "  straw residue = "
        , dm_residue * gm2kg /sm2ha, " kg/ha");
        parent->writeString (msg);

        sprintf (msg, "%s%7.1f%s"
        , "  straw n =       "
        , n_residue * gm2kg /sm2ha, " kg/ha");
        parent->writeString (msg);

        sprintf (msg, "%s%7.1f%s"
        , "  root residue =  "
        , dm_root * gm2kg /sm2ha, " kg/ha");
        parent->writeString (msg);

        sprintf (msg, "%s%7.1f%s"
        , "  root n =        "
        , n_root * gm2kg /sm2ha, " kg/ha");
        parent->writeString (msg);

        }
    else
        {
        sprintf(msg, "%s%s%s"
         ,g.module_name.c_str()
         , " is not in the ground -"
         , " unable to end crop.");

        warning_error (&err_user, msg);
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
        warning_error (&err_user, msg);
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
    protocol::vector<float> values;               // Scratch area

    int   numvals;                                // number of values put into array
    float soil_temp;                              // soil surface temperature (oC)

//- Implementation Section ----------------------------------

    push_routine (my_name);
    //XXX this is fucked up here - soil_temp is always 0.0 => frozen seeds can't emerge?????
    //cnh disable this until a proper implementation exists.;
    //cnh this sort of approach uses up too much processing time;
    //cnh      call get_real_var_optional (unknown_module;
    //cnh     :                                  , "maxt_soil_surface";
    //cnh     :                                  , "(oc)";
    //cnh     :                                  , soil_temp, numvals;
    //cnh     :                                  , 0.0, 80.0);
    numvals = 0;
    soil_temp = 0.0;
    //cnh end subroutine;

    if (numvals==0)
        {
        // soil temp not supplied
        }
    else
        {
        plant_store_value (
               g.day_of_year
             , g.year
             , g.soil_temp
             , soil_temp);
        }

    // Canopy
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

    pop_routine (my_name);
    return;
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
    float fraction_to_residue[max_part+1];          // fraction sent to residue (0-1)
    int   part;
//- Implementation Section ----------------------------------
    push_routine (my_name);

    // dispose of detached material from senesced parts in
    // live population
    for (part =0; part < max_part; part++) 
       {
       dm_residue[part] = g.dlt_dm_detached[part] * gm2kg/sm2ha;
       n_residue[part] = g.dlt_n_detached[part] * gm2kg/sm2ha;
       fraction_to_residue[part] = 1.0;
       }
    fraction_to_residue[root] = 0.0;

    //  call crop_top_residue (g%crop_type, dm_residue, N_residue)
    if (sum_real_array(dm_residue, max_part) > 0.0)
        {
        plant_send_crop_chopped_event
        (c.crop_type
        , part_name
        , dm_residue
        , n_residue
        , fraction_to_residue
        , max_part);
        }
    else
        {
        // no surface residue
        }

    // put live population roots into root residue
    plant_root_incorp (g.dlt_dm_detached[root]
                       , g.dlt_n_detached[root]
                       , g.root_length);

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
        plant_send_crop_chopped_event(c.crop_type
          , part_name
          , dm_residue
          , n_residue
          , fraction_to_residue
          , max_part);
        }
    else
        {
        // no surface residue
        }

    // put dead population roots into root residue
    plant_root_incorp (g.dlt_dm_dead_detached[root]
                       , g.dlt_n_dead_detached[root]
                       , g.root_length_dead);

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
//     250996 jngh corrected type of lower limit of read_integer_var
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

    read_char_var (section_name
    , "crop_type", "()"
    , c.crop_type);

    read_char_var (section_name
    , "default_crop_class"
    , "()"
    , c.default_crop_class);

    read_char_array (section_name
    , "part_names", "()"
    , c.part_names);

    read_real_var (section_name
    , "sw_ub", "(mm/mm)"
    , c.sw_ub
    , 0.0, 1.0);

    read_real_var (section_name
    , "sw_lb", "(mm/mm)"
    , c.sw_lb
    , 0.0, 1.0);

//    plant_get_other_variables

// checking the bounds of the bounds..
    read_real_var (section_name
    , "latitude_ub", "(ol)"
    , c.latitude_ub
    , -90.0, 90.0);

    read_real_var (section_name
    , "latitude_lb", "(ol)"
    , c.latitude_lb
    , -90.0, 90.0);

    read_real_var (section_name
    , "maxt_ub", "(oc)"
    , c.maxt_ub
    , 0.0, 60.0);

    read_real_var (section_name
    , "maxt_lb", "(oc)"
    , c.maxt_lb
    , 0.0, 60.0);

    read_real_var (section_name
    , "mint_ub", "(oc)"
    , c.mint_ub
    , 0.0, 40.0);

    read_real_var (section_name
    , "mint_lb", "(oc)"
    , c.mint_lb
    , -100.0, 100.0);

    read_real_var (section_name
    , "radn_ub", "(mj/m^2)"
    , c.radn_ub
    , 0.0, 100.0);

    read_real_var (section_name
    , "radn_lb", "(mj/m^2)"
    , c.radn_lb
    , 0.0, 100.0);

    read_real_var (section_name
    , "dlayer_ub", "(mm)"
    , c.dlayer_ub
    , 0.0, 10000.0);

    read_real_var (section_name
    , "dlayer_lb", "(mm)"
    , c.dlayer_lb
    , 0.0, 10000.0);

// 8th block
    read_real_var (section_name
    , "sw_dep_ub", "(mm)"
    , c.sw_dep_ub
    , 0.0, 10000.0);

    read_real_var (section_name
    , "sw_dep_lb", "(mm)"
    , c.sw_dep_lb
    , 0.0, 10000.0);

    read_real_var (section_name
    , "no3_ub", "(kg/ha)"
    , c.no3_ub
    , 0.0, 100000.0);

    read_real_var (section_name
    , "no3_lb", "(kg/ha)"
    , c.no3_lb
    , 0.0, 100000.0);

    read_real_var (section_name
    , "no3_min_ub", "(kg/ha)"
    , c.no3_min_ub
    , 0.0, 100000.0);

    read_real_var (section_name
    , "no3_min_lb", "(kg/ha)"
    , c.no3_min_lb
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

    if (g.plant_status == alive)
        {
        plant_nit_stress (1);
        plant_temp_stress (1);
        plant_light_supply (1);
//        fprintf(stdout, "%d,%.9f,%.9f,%.9f,%.9f\n",
//                g.day_of_year, g.radn_int, g.radn, g.cover_green, g.lai);

        plant_bio_rue (1);
        plant_transpiration_eff (1);
        plant_water_demand (1);
        plant_nit_demand_est(1);
        }
    else
        {
        }

    pop_routine (myname);
    return;
    }

void Plant::registerClassActions(void)
   {
   // Remove old registrations from the system
   for (UInt2StringMap::const_iterator i = IDtoAction.begin();
        i != IDtoAction.end();
        i++)
        {
        parent->deleteRegistration(protocol::respondToMethodCallReg,i->first);
        parent->deleteRegistration(protocol::respondToEventReg,i->first);
        }
   IDtoAction.clear();

   // Add the new class actions we're interested in
   for (vector<string>::const_iterator i = c.class_action.begin();
        i != c.class_action.end();
        i++)
      {
      // Methods & events haven't been sorted out yet?
      unsigned int id = parent->addRegistration(protocol::respondToMethodCallReg,
                                                  i->c_str(), "");
      IDtoEventFn.insert(UInt2EventFnMap::value_type(id,&Plant::doAutoClassChange));

      id = parent->addRegistration(protocol::respondToEventReg,
                                                  i->c_str(), "");
      IDtoEventFn.insert(UInt2EventFnMap::value_type(id,&Plant::doAutoClassChange)); //XX to be sure, to be sure.....

      IDtoAction.insert(UInt2StringMap::value_type(id,i->c_str()));
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

    read_char_array (c.crop_type, g.crop_class, "()", search_order);

    parent->writeString (string(" - reading constants for " +
                                g.crop_class + "(" + c.crop_type +")").c_str());

    read_char_array (search_order, "class_action", "()", c.class_action);
    registerClassActions();

    read_char_array (search_order, "class_change", "()", c.class_change);

    read_integer_var (search_order, "phenology_option", "()"
    , c.phenology_option
    , 1, 2);

    read_char_array (search_order, "stage_names", "()", c.stage_names);

    read_real_array (search_order, "stage_code", max_stage, "()"
    , c.stage_code_list, numvals
    , 0.0, 1000.0);

    read_real_array (search_order,
     "stage_stem_reduction_harvest"
    , max_stage, "()"
    , c.stage_stem_reduction_harvest, numvals
    , 1.0, 11.0);

    read_real_array (search_order,
     "stage_stem_reduction_kill_stem"
    , max_stage, "()"
    , c.stage_stem_reduction_kill_stem, numvals
    , 1.0, 11.0);

//      call search_read_real_array (search_order, num_sections
//     :                     , 'rue', max_stage, '(g dm/mj)'
//     :                     , c%rue, numvals
//     :                     , 0.0, 1000.0)

    read_real_array (search_order,
     "x_stage_rue", max_table, "()"
    , c.x_stage_rue, numvals
    , 0.0, 1000.0);

    read_real_array (search_order,
     "y_rue", max_table, "(g dm/mj)"
    , c.y_rue, numvals
    , 0.0, 1000.0);

    read_real_array (search_order,
     "root_depth_rate", max_stage, "(mm)"
    , c.root_depth_rate, numvals
    , 0.0, 1000.0);

    read_real_array (search_order,
     "n_fix_rate", max_stage, "()"
    , c.n_fix_rate, numvals
    , 0.0, 1.0);

    read_real_array (search_order,
     "transp_eff_cf", max_stage, "(kpa)"
    , c.transp_eff_cf, numvals
    , 0.0, 1.0);

    read_integer_var (search_order,
     "partition_option", "()"
    , c.partition_option
    , 1, 2);

    if (c.partition_option==1)
        {
        read_real_array (search_order,
         "frac_leaf", max_stage, "()"
        , c.frac_leaf, numvals
        , 0.0, 1.0);

        read_real_array (search_order,
         "frac_pod", max_stage, "()"
        , c.frac_pod, numvals
        , 0.0, 2.0);
        read_real_array (search_order,
         "ratio_root_shoot", max_stage, "()"
        , c.ratio_root_shoot, numvals
        , 0.0, 1000.0);

        }
    else if (c.partition_option==2)
        {
        read_real_array (search_order,
         "x_stage_no_partition", max_stage, "()"
        , c.x_stage_no_partition
        , c.num_stage_no_partition
        , 0.0, 20.0);

        read_real_array (search_order,
         "y_frac_leaf", max_stage, "()"
        , c.y_frac_leaf, numvals
        , 0.0, 1.0);

        read_real_array (search_order,
         "y_frac_pod", max_stage, "()"
        , c.y_frac_pod, numvals
        , 0.0, 2.0);

        read_real_array (search_order,
         "y_ratio_root_shoot", max_stage, "()"
        , c.y_ratio_root_shoot, numvals
        , 0.0, 1000.0);

        }

    read_real_var (search_order,
     "row_spacing_default", "(mm)"
    , c.row_spacing_default
    , 0.0, 2000.);

    read_real_var (search_order,
     "skiprow_default", "()"
    , c.skip_row_default
    , 0.0, 2.0);

    read_real_array (search_order,
     "x_row_spacing", max_table, "(mm)"
    , c.x_row_spacing, c.num_row_spacing
    , 0.0, 2000.);

    read_real_array (search_order,
     "y_extinct_coef", max_table, "()"
    , c.y_extinct_coef, c.num_row_spacing
    , 0.0, 1.0);

    read_real_array (search_order,
     "y_extinct_coef_dead", max_table, "()"
    , c.y_extinct_coef_dead, c.num_row_spacing
    , 0.0, 1.0);

    read_real_var (search_order,
     "extinct_coef_pod", "()"
    , c.extinct_coef_pod
    , 0.0, 1.0);

    read_real_var (search_order,
     "spec_pod_area", "()"
    , c.spec_pod_area
    , 0.0, 100000.0);

    read_real_var (search_order,
     "rue_pod", "()"
    , c.rue_pod
    , 0.0, 3.0);

// crop failure

    read_real_var (search_order,
     "leaf_no_crit", "()"
    , c.leaf_no_crit
    , 0.0, 100.0);

    read_real_var (search_order,
     "tt_emerg_limit", "(oc)"
    , c.tt_emerg_limit
    , 0.0, 1000.0);

    read_real_var (search_order,
     "days_germ_limit", "(days)"
    , c.days_germ_limit
    , 0.0, 365.0);

    read_real_var (search_order,
     "swdf_pheno_limit", "()"
    , c.swdf_pheno_limit
    , 0.0, 1000.0);

    read_real_var (search_order,
     "swdf_photo_limit", "()"
    , c.swdf_photo_limit
    , 0.0, 1000.0);

    read_real_var (search_order,
     "swdf_photo_rate", "()"
    , c.swdf_photo_rate
    , 0.0, 1.0);

//    plant_root_depth

    read_real_var (search_order,
     "initial_root_depth", "(mm)"
    , c.initial_root_depth
    , 0.0, 1000.0);

//    plant_root_length

    read_real_var (search_order,
     "specific_root_length", "(mm/g)"
    , c.specific_root_length
    , 0.0, 1.0e6);

    read_real_var (search_order,
     "root_die_back_fr", "(0-1)"
    , c.root_die_back_fr
    , 0.0, 0.99);

    read_real_array (search_order,
     "x_plant_rld", max_table, "()"
    , c.x_plant_rld, c.num_plant_rld
    , 0.0, 0.1);

    read_real_array (search_order,
     "y_rel_root_rate", max_table, "()"
    , c.y_rel_root_rate, c.num_plant_rld
    , 0.001, 1.0);

//    plant_leaf_area_init

    read_real_var (search_order,
     "initial_tpla", "(mm^2)"
    , c.initial_tpla
    , 0.0, 100000.0);

    read_real_var (search_order,
     "min_tpla", "(mm^2)"
    , c.min_tpla
    , 0.0, 100000.0);

// TEMPLATE OPTION
//    plant_leaf_area

    read_real_array (search_order,
     "x_lai", max_table, "(mm2/mm2)"
    , c.x_lai, c.num_lai
    , 0.0, 15.0);

    read_real_array (search_order,
     "y_sla_max", max_table, "(mm2/g)"
    , c.y_sla_max, c.num_lai
    , 0.0, 2.e5);

    read_real_array (search_order,
     "x_lai_ratio", max_table, "()"
    , c.x_lai_ratio, c.num_lai_ratio
    , 0.0, 1.0);

    read_real_array (search_order,
     "y_leaf_no_frac", max_table, "()"
    , c.y_leaf_no_frac, c.num_lai_ratio
    , 0.0, 1.0);

//    plant_get_cultivar_params

    read_real_var (search_order,
     "tt_emerg_to_endjuv_ub", "()"
    , c.tt_emerg_to_endjuv_ub
    , 0.0, 1.e6);

    read_real_var (search_order,
     "tt_maturity_to_ripe_ub", "()"
    , c.tt_maturity_to_ripe_ub
    , 0.0, 1.e6);

//    plant_transp_eff

    read_real_var (search_order,
     "svp_fract", "()"
    , c.svp_fract
    , 0.0, 1.0);

//    cproc_sw_demand_bound

    read_real_var (search_order,
     "eo_crop_factor_default", "()"
    , c.eo_crop_factor_default
    , 0.0, 100.);

//    plant_germination

    read_real_var (search_order,
     "pesw_germ", "(mm/mm)"
    , c.pesw_germ
    , 0.0, 1.0);

    read_real_array (search_order,
     "fasw_emerg", max_table, "()"
    , c.fasw_emerg, c.num_fasw_emerg
    , 0.0, 1.0);

    read_real_array (search_order,
     "rel_emerg_rate", max_table, "()"
    , c.rel_emerg_rate, c.num_fasw_emerg
    , 0.0, 1.0);

//    plant_leaf_appearance

    read_real_var (search_order,
     "leaf_no_at_emerg", "()"
    , c.leaf_no_at_emerg
    , 0.0, 100.0);

//    plant_n_uptake

    read_integer_var (search_order,
     "n_uptake_option", "()"
    , c.n_uptake_option
    , 1, 2);

    if (c.n_uptake_option==1)
        {
        read_real_var (search_order,
         "no3_diffn_const", "(days)"
        , c.no3_diffn_const
        , 0.0, 100.0);
        }
    else if (c.n_uptake_option==2)
        {
        read_real_var (search_order,
         "no3_uptake_max", "(g/mm)"
        , c.no3_uptake_max
        , 0.0, 1.0);

        read_real_var (search_order,
         "no3_conc_half_max", "(ppm)"
        , c.no3_conc_half_max
        , 0.0, 100.0);
        }

    read_char_var (search_order,
     "n_supply_preference", "()"
    , c.n_supply_preference);

//    plant_phenology_init

    read_real_var (search_order,
     "shoot_lag", "(oc)"
    , c.shoot_lag
    , 0.0, 100.0);

    read_real_var (search_order,
     "shoot_rate", "(oc/mm)"
    , c.shoot_rate
    , 0.0, 100.0);

    read_integer_var (search_order,
     "leaf_no_pot_option", "()"
    , c.leaf_no_pot_option
    , 1, 2);

    read_real_array (search_order,
     "x_node_no_app", max_table, "()"
    , c.x_node_no_app, c.num_node_no_app
    , 0.0, 200.);
    read_real_array (search_order,
     "y_node_app_rate", max_table, "()"
    , c.y_node_app_rate, c.num_node_no_app
    , 0.0, 400.);
    read_real_array (search_order,
     "x_node_no_leaf", max_table, "()"
    , c.x_node_no_leaf, c.num_node_no_leaf
    , 0.0, 200.);
    read_real_array (search_order,
     "y_leaves_per_node", max_table, "()"
    , c.y_leaves_per_node, c.num_node_no_leaf
    , 0.0, 50.);

//    plant_dm_init

    read_real_array (search_order,
     "dm_init", max_table, "(g/plant)"
    , c.dm_init, numvals
    , 0.0, 1.0);

//    plant_get_root_params

    read_real_var (search_order,
     "kl_ub", "()"
    , c.kl_ub
    , 0.0, 1000.0);    ///XX oh yeah??

//    plant_retranslocate

    read_real_var (search_order,
     "stem_trans_frac", "()"
    , c.stem_trans_frac
    , 0.0, 1.0);

    read_real_var (search_order,
     "pod_trans_frac", "()"
    , c.pod_trans_frac
    , 0.0, 1.0);

    read_real_var (search_order,
     "leaf_trans_frac", "()"
    , c.leaf_trans_frac
    , 0.0, 1.0);

//    grain number

    read_integer_var (search_order,
     "grain_no_option", "()"
    , c.grain_no_option
    , 1, 2);

//    legume grain filling

    read_integer_var (search_order,
     "grain_fill_option", "()"
    , c.grain_fill_option
    , 1, 2);

    if (c.grain_fill_option==2)
        {
        read_real_array (search_order,
         "x_temp_grainfill"
        , max_table, "(oc)"
        , c.x_temp_grainfill
        , c.num_temp_grainfill
        , 0.0
        , 40.0);

        read_real_array (search_order,
         "y_rel_grainfill"
        , max_table, "(-)"
        , c.y_rel_grainfill
        , c.num_temp_grainfill
        , 0.0
        , 1.0);
        }

//    plant_n_dlt_grain_conc

    read_real_var (search_order,
     "sw_fac_max", "()"
    , c.sw_fac_max
    , 0.0, 100.0);

    read_real_var (search_order,
     "temp_fac_min", "()"
    , c.temp_fac_min
    , 0.0, 100.0);

    read_real_var (search_order,
     "sfac_slope", "()"
    , c.sfac_slope
    , -10.0, 0.0);

    read_real_var (search_order,
     "tfac_slope", "()"
    , c.tfac_slope
    , 0.0, 100.0);

    read_real_var (search_order,
     "sen_start_stage", "()"
    , c.sen_start_stage
    , 0.0, max_stage);

    read_real_var (search_order,
     "fr_lf_sen_rate", "(/degday)"
    , c.fr_lf_sen_rate
    , 0.0, 1.0);

    read_real_var (search_order,
     "node_sen_rate", "(degday)"
    , c.node_sen_rate
    , 0.0, 1000.0);

//    plant_event

    read_real_var (search_order,
     "grn_water_cont", "(g/g)"
    , c.grn_water_cont
    , 0.0, 1.0);

//    plant_dm_partition

    read_real_var (search_order,
     "sla_min", "(mm^2/g)"
    , c.sla_min
    , 0.0, 100000.0);

    read_real_var (search_order,
     "carbo_oil_conv_ratio", "()"
    , c.carbo_oil_conv_ratio
    , 0.0, 20.0);

    read_real_var (search_order,
     "grain_oil_conc", "()"
    , c.grain_oil_conc
    , 0.0, 1.0);

//    plant_dm_senescence

    for (part=0; part<max_part;part++)
       {
       sprintf(name, "x_dm_sen_frac_%s", c.part_names[part].c_str());
       read_real_array (search_order
          , name
          , max_part, "()"
          , c.x_dm_sen_frac[part]
          , c.num_dm_sen_frac[part]
          , 0.0
          , 100.0);

       sprintf(name, "y_dm_sen_frac_%s", c.part_names[part].c_str());
       read_real_array (search_order
          , name
          , max_part, "()"
          , c.y_dm_sen_frac[part]
          , c.num_dm_sen_frac[part]
          , 0.0
          , 1.0);

       }

//    plant_dm_dead_detachment

    read_real_array (search_order
    , "dead_detach_frac", max_part, "()"
    , c.dead_detach_frac, numvals
    , 0.0, 1.0);

    read_real_array (search_order
    , "sen_detach_frac", max_part, "()"
    , c.sen_detach_frac, numvals
    , 0.0, 1.0);

//    plant_leaf_area_devel

    read_real_var (search_order
    , "node_no_correction", "()"
    , c.node_no_correction
    , 0.0, 10.0);

    read_real_array (search_order
    , "x_node_no", max_table, "()"
    , c.x_node_no, c.num_node_no
    , 0.0, 100.0);

    read_real_array (search_order
    , "y_leaf_size", max_table, "(mm2)"
    , c.y_leaf_size, c.num_node_no
    , 0.0, 60000.0);

//    plant_leaf_area_sen_light

    read_real_var (search_order
    , "lai_sen_light", "(m^2/m^2)"
    , c.lai_sen_light
    , 3.0, 20.0);

    read_real_var (search_order
    , "sen_light_slope", "()"
    , c.sen_light_slope
    , 0.0, 100.0);

// TEMPLATE OPTION
//    plant_leaf_area_sen_frost

    read_real_array (search_order
    , "x_temp_senescence", max_table, "(oc)"
    , c.x_temp_senescence, c.num_temp_senescence
    , -20.0, 20.0);

    read_real_array (search_order
    , "y_senescence_fac", max_table, "()"
    , c.y_senescence_fac, c.num_temp_senescence
    , 0.0, 1.0);

// TEMPLATE OPTION
//    plant_leaf_area_sen_water

    read_real_var (search_order
    , "sen_rate_water", "()"
    , c.sen_rate_water
    , 0.0, 100.0);

//    plant_phenology_init

    read_real_var (search_order
    , "twilight", "(o)"
    , c.twilight
    , -90.0, 90.0);

//    plant_n_conc_limits

    read_real_array (search_order
    , "x_stage_code", max_stage, "()"
    , c.x_stage_code, c.num_n_conc_stage
    , 0.0, 100.0);

    read_real_array (search_order
    , "y_n_conc_crit_leaf", max_stage, "()"
    , c.y_n_conc_crit_leaf, c.num_n_conc_stage
    , 0.0, 100.0);

    read_real_array (search_order
    , "y_n_conc_max_leaf", max_stage, "()"
    , c.y_n_conc_max_leaf, c.num_n_conc_stage
    , 0.0, 100.0);

    read_real_array (search_order
    , "y_n_conc_min_leaf", max_stage, "()"
    , c.y_n_conc_min_leaf, c.num_n_conc_stage
    , 0.0, 100.0);

    read_real_array (search_order
    , "y_n_conc_crit_stem", max_stage, "()"
    , c.y_n_conc_crit_stem, c.num_n_conc_stage
    , 0.0, 100.0);

    read_real_array (search_order
    , "y_n_conc_max_stem", max_stage, "()"
    , c.y_n_conc_max_stem, c.num_n_conc_stage
    , 0.0, 100.0);

    read_real_array (search_order
    , "y_n_conc_min_stem", max_stage, "()"
    , c.y_n_conc_min_stem, c.num_n_conc_stage
    , 0.0, 100.0);

    read_real_array (search_order
    , "y_n_conc_crit_pod", max_stage, "()"
    , c.y_n_conc_crit_pod, c.num_n_conc_stage
    , 0.0, 100.0);

    read_real_array (search_order
    , "y_n_conc_max_pod", max_stage, "()"
    , c.y_n_conc_max_pod, c.num_n_conc_stage
    , 0.0, 100.0);

    read_real_array (search_order
    , "y_n_conc_min_pod", max_stage, "()"
    , c.y_n_conc_min_pod, c.num_n_conc_stage
    , 0.0, 100.0);

    read_real_var (search_order
    , "n_conc_crit_grain", "()"
    , c.n_conc_crit_grain
    , 0.0, 100.0);

    read_real_var (search_order
    , "n_conc_max_grain", "()"
    , c.n_conc_max_grain
    , 0.0, 100.0);

    read_real_var (search_order
    , "n_conc_min_grain", "()"
    , c.n_conc_min_grain
    , 0.0, 100.0);

    read_real_var (search_order
    , "n_conc_crit_root", "()"
    , c.n_conc_crit_root
    , 0.0, 100.0);

    read_real_var (search_order
    , "n_conc_max_root", "()"
    , c.n_conc_max_root
    , 0.0, 100.0);

    read_real_var (search_order
    , "n_conc_min_root", "()"
    , c.n_conc_min_root
    , 0.0, 100.0);

//    plant_n_init

    read_real_array (search_order
    , "n_init_conc", max_stage, "(g/g)"
    , c.n_init_conc, numvals
    , 0.0, 1.0);

//    plant_n_senescence

    read_real_array (search_order
    , "n_sen_conc", max_part, "()"
    , c.n_sen_conc, numvals
    , 0.0, 1.0);

//    plant_nfact

    read_real_var (search_order
    , "n_fact_photo", "()"
    , c.n_fact_photo
    , 0.0, 100.0);

    read_real_var (search_order
    , "n_fact_pheno", "()"
    , c.n_fact_pheno
    , 0.0, 100.0);

    read_real_var (search_order
    , "n_fact_expansion", "()"
    , c.n_fact_expansion
    , 0.0, 100.0);

//    plant_rue_reduction

    read_real_array (search_order
    , "x_ave_temp", max_table, "(oc)"
    , c.x_ave_temp, c.num_ave_temp
    , 0.0, 100.0);

    read_real_array (search_order
    , "y_stress_photo", max_table, "()"
    , c.y_stress_photo, c.num_factors
    , 0.0, 1.0);

//    plant_thermal_time

    read_real_array (search_order
    , "x_temp", max_table, "(oc)"
    , c.x_temp, c.num_temp
    , 0.0, 100.0);

    read_real_array (search_order
    , "y_tt", max_table, "(oc days)"
    , c.y_tt, c.num_temp
    , 0.0, 100.0);

    read_real_array (search_order
    , "x_vernal_temp", max_table, "(oc)"
    , c.x_vernal_temp, c.num_vernal_temp
    , -10., 60.0);

    read_real_array (search_order
    , "y_vernal_days", max_table, "(days)"
    , c.y_vernal_days, c.num_vernal_temp
    , 0.0, 1.0);

    read_real_array (search_order
    , "x_temp_root_advance", max_table, "(oc)"
    , c.x_temp_root_advance
    , c.num_temp_root_advance
    , -10., 60.0);

    read_real_array (search_order
    , "y_rel_root_advance", max_table, "(0-1)"
    , c.y_rel_root_advance
    , c.num_temp_root_advance
    , 0.0, 1.0);

    read_real_array (search_order
    , "x_weighted_temp", max_table, "(oc)"
    , c.x_weighted_temp, c.num_weighted_temp
    , 0.0, 100.0);

    read_real_array (search_order
    , "y_plant_death", max_table, "(oc)"
    , c.y_plant_death, c.num_weighted_temp
    , 0.0, 100.0);

//    plant_swdef

    read_real_array (search_order
    , "x_sw_demand_ratio", max_table, "()"
    , c.x_sw_demand_ratio, c.num_sw_demand_ratio
    , 0.0, 100.0);

    read_real_array (search_order
    , "y_swdef_leaf", max_table, "()"
    , c.y_swdef_leaf, c.num_sw_demand_ratio
    , 0.0, 100.0);

    read_real_array (search_order
    , "x_sw_avail_ratio", max_table, "()"
    , c.x_sw_avail_ratio, c.num_sw_avail_ratio
    , 0.0, 100.0);

    read_real_array (search_order
    , "y_swdef_pheno", max_table, "()"
    , c.y_swdef_pheno, c.num_sw_avail_ratio
    , 0.0, 100.0);

    read_real_array (search_order
    , "x_sw_ratio", max_table, "()"
    , c.x_sw_ratio, c.num_sw_ratio
    , 0.0, 100.0);

    read_real_array (search_order
    , "y_sw_fac_root", max_table, "()"
    , c.y_sw_fac_root, c.num_sw_ratio
    , 0.0, 100.0);

    read_real_array (search_order
    , "x_ws_root", max_table, "()"
    , c.x_ws_root, c.num_ws_root
    , 0.0, 1.0);

    read_real_array (search_order
    , "y_ws_root_fac", max_table, "()"
    , c.y_ws_root_fac, c.num_ws_root
    , 0.0, 1.0);

    read_real_array (search_order
    , "x_sw_avail_fix", max_table, "()"
    , c.x_sw_avail_fix, c.num_sw_avail_fix
    , 0.0, 100.0);

    read_real_array (search_order
    , "y_swdef_fix", max_table, "()"
    , c.y_swdef_fix, c.num_sw_avail_fix
    , 0.0, 100.0);

    read_real_array (search_order
    , "oxdef_photo_rtfr", max_table, "()"
    , c.oxdef_photo_rtfr, c.num_oxdef_photo
    , 0.0, 1.0);

    read_real_array (search_order
    , "oxdef_photo", max_table, "()"
    , c.oxdef_photo, c.num_oxdef_photo
    , 0.0, 1.0);

    read_real_array (search_order
    , "fr_height_cut", max_table, "(0-1)"
    , c.fr_height_cut, c.num_fr_height_cut
    , 0.0, 1.0);

    read_real_array (search_order
    , "fr_stem_remain", max_table, "()"
    , c.fr_stem_remain, c.num_fr_height_cut
    , 0.0, 1.0);

    pop_routine (my_name);
    return;
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

//+  Local Variables
    float biomass_dead;                           // above ground dead plant wt (kg/ha)
    float biomass_green;                          // above ground green plant wt (kg/ha)
    float biomass_senesced;                       // above ground senesced plant wt (kg/ha)
    float dm;                                     // above ground total dry matter (kg/ha)
    float grain_wt;                               // grain dry weight (g/kernel)
    float head_grain_no;                          // final grains /head
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

//- Implementation Section ----------------------------------

    push_routine (my_name);

// crop harvested. Report status

    yield = (g.dm_green[meal] + g.dm_dead[meal]
    + g.dm_green[oil] + g.dm_dead[oil])
    * gm2kg / sm2ha;

// include the grain water content
    yield_wet = yield / (1.0 - c.grn_water_cont);

    grain_wt = divide (g.dm_green[meal] + g.dm_dead[meal]
       + g.dm_green[oil] + g.dm_dead[oil], g.grain_no, 0.0);

    head_grain_no = divide (g.grain_no, g.plants, 0.0);

    biomass_green = (sum_real_array (g.dm_green, max_part)
      - g.dm_green[root]) * gm2kg / sm2ha;

    biomass_senesced = (sum_real_array (g.dm_senesced, max_part)
      - g.dm_senesced[root]) * gm2kg / sm2ha;

    biomass_dead = (sum_real_array (g.dm_dead, max_part)
      - g.dm_dead[root]) * gm2kg / sm2ha;

    dm = (biomass_green + biomass_senesced + biomass_dead);

    stover = dm - yield;

    leaf_no = sum_real_array (g.leaf_no, max_node);

    n_grain_conc_percent = divide (
        g.n_green[meal] + g.n_dead[meal] + g.n_green[oil] + g.n_dead[oil]
      , g.dm_green[meal] + g.dm_dead[meal] + g.dm_green[oil] + g.dm_dead[oil]
      , 0.0) * fract2pcnt;

    n_grain = (g.n_green[meal] + g.n_dead[meal]
        + g.n_green[oil] + g.n_dead[oil]) * gm2kg/sm2ha;

    n_green = (sum_real_array (g.n_green, max_part)
        - g.n_green[root] - g.n_green[meal] - g.n_green[oil]) * gm2kg / sm2ha;

    n_senesced = (sum_real_array (g.n_senesced, max_part)
        - g.n_senesced[root] - g.n_senesced[meal]- g.n_senesced[oil]) * gm2kg / sm2ha;

    n_dead = (sum_real_array (g.n_dead, max_part)
        - g.n_dead[root] - g.n_dead[meal] - g.n_dead[oil]) * gm2kg / sm2ha;

    n_stover = n_green + n_senesced + n_dead;
    n_total = n_grain + n_stover;

    parent->writeString ("");

    sprintf (msg, "%s%4d%20s%s%10.1f"
             , " flowering day          = ",g.flowering_date, " "
             , " stover (kg/ha)         = ",stover);
    parent->writeString (msg);

    sprintf (msg, "%s%4d%20s%s%10.1f"
             , " maturity day           = ", g.maturity_date, " "
             , " grain yield (kg/ha)    = ", yield);
    parent->writeString (msg);

    sprintf (msg, "%s%6.1f%18s%s%10.1f"
             , " grain % water content  = ", c.grn_water_cont * fract2pcnt, " "
             , " grain yield wet (kg/ha)= ", yield_wet);
    parent->writeString (msg);

    sprintf (msg, "%s%8.3f%16s%s%10.3f"
             , " grain wt (g)           = ", grain_wt, " "
             , " grains/m^2             = ", g.grain_no);
    parent->writeString (msg);

    sprintf (msg, "%s%6.1f%18s%s%10.3f"
             , " grains/head            = ", head_grain_no, " "
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

    sprintf (msg, "%s%10.2f%14s%s%10.2f"
             , " grain n percent        = ", n_grain_conc_percent, " "
             , " total n content (kg/ha)= ", n_total);
    parent->writeString (msg);

    sprintf (msg, "%s%10.2f%14s%s%8.2f"
             , " grain n uptake (kg/ha) = ", n_grain, " "
             , " senesced n content (kg/ha) =", n_senesced);
    parent->writeString (msg);

    sprintf (msg, "%s%10.2f%14s%s%10.2f"
             , " green n content (kg/ha)= ", n_green, " "
             , " dead n content (kg/ha) = ", n_dead);
    parent->writeString (msg);

    for (phase = emerg_to_endjuv; phase <= start_to_end_grain; phase++)
         {
         si1 = divide (g.cswd_photo[phase-1], g.days_tot[phase-1], 0.0); 
         si2 = divide (g.cswd_expansion[phase-1], g.days_tot[phase-1], 0.0);
         si4 = divide (g.cnd_photo[phase-1], g.days_tot[phase-1], 0.0);
         si5 = divide (g.cnd_grain_conc[phase-1], g.days_tot[phase-1], 0.0);

         parent->writeString ("");

         sprintf (msg,"%s%s"
                  , " stress indices for ", c.stage_names[phase-1].c_str());
         parent->writeString (msg);

         sprintf (msg,"%s%16.7f%s%16.7f"
                  , " water stress 1 =", si1
                  , "   nitrogen stress 1 =", si4);
         parent->writeString (msg);

         sprintf (msg,"%s%16.7f%s%16.7f"
                  , " water stress 2 =", si2
                  , "   nitrogen stress 2 =", si5);
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
       chopped.fraction_to_residue.push_back(fraction_to_residue[i]);
       }
    parent->publish (id.crop_chopped, chopped);
#else
    protocol::ApsimVariant outgoingApsimVariant(parent);
    outgoingApsimVariant.store("crop_type", protocol::DTstring, false, FString(crop_type.c_str()));
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

    outgoingApsimVariant.store("dlt_crop_dm", protocol::DTsingle, true,
             protocol::vector<float>(dlt_crop_dm, dlt_crop_dm+max_part));
    outgoingApsimVariant.store("dlt_dm_n", protocol::DTsingle, true,
             protocol::vector<float>(dlt_dm_n, dlt_dm_n+max_part));
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
void Plant::doNewProfile(unsigned &, protocol::Variant &v /* (INPUT) message arguments*/)
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
    bound_check_real_var(c.grain_oil_conc, 0.0, 1.0, "grain_oil_conc");
    plant_read_species_const ();
    return true;
    }

void Plant::get_plant_status(protocol::QueryValueData &qd)
{
    switch (g.plant_status) {
    	case out: parent->sendVariable(qd, FString("out")); break;
    	case dead: parent->sendVariable(qd, FString("dead")); break;
    	case alive: parent->sendVariable(qd, FString("alive")); break;
    }
}


void Plant::get_dlt_stage(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.dlt_stage);
}


void Plant::get_stage(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.current_stage);
}


void Plant::get_stage_code(protocol::QueryValueData &qd)
{
    if (g.plant_status != out)
    {
        int stage_no = (int) g.current_stage;
        parent->sendVariable(qd, c.stage_code_list[stage_no-1]);
    }
    else
    {
        parent->sendVariable(qd, 0.0);
    }
}


void Plant::get_stage_name(protocol::QueryValueData &qd)
{
    if (g.plant_status != out)
    {
        unsigned int stage_no = (unsigned int) g.current_stage;
        if (stage_no > 0 && stage_no <= c.stage_names.size())
           {
           parent->sendVariable(qd,FString(c.stage_names[stage_no-1].c_str()));
           return;
           }
    }
    parent->sendVariable(qd,FString("out"));
}


void Plant::get_crop_type(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, FString(c.crop_type.c_str()));
}


void Plant::get_crop_class(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, FString(g.crop_class.c_str()));
}


void Plant::get_dlt_tt(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.dlt_tt);
}


void Plant::get_phase_tt(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, protocol::vector<float>(g.phase_tt, g.phase_tt+max_stage));
}


void Plant::get_tt_tot(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, protocol::vector<float>(g.tt_tot, g.tt_tot+max_stage));
}


void Plant::get_days_tot(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, protocol::vector<float>(g.days_tot, g.days_tot+max_stage));
}


void Plant::get_das(protocol::QueryValueData &qd)
{
    int das = floor(sum_real_array(g.days_tot, max_stage)+0.5);  // Nearest int
    parent->sendVariable(qd, das);
}


void Plant::get_cum_vernal_days(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.cum_vernal_days);
}


void Plant::get_flowering_date(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.flowering_date);
}


void Plant::get_maturity_date(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.maturity_date);
}


void Plant::get_flowering_das(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.flowering_das);
}


void Plant::get_maturity_das(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.maturity_das);
}


void Plant::get_leaf_no(protocol::QueryValueData &qd)
{
    float temp = sum_real_array(g.leaf_no, max_node);
    parent->sendVariable(qd, temp);
}


void Plant::get_node_no(protocol::QueryValueData &qd)
{
    float temp = sum_real_array(g.node_no, max_stage);
    parent->sendVariable(qd, temp);
}


void Plant::get_dlt_leaf_no(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.dlt_leaf_no);
}


void Plant::get_dlt_node_no(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.dlt_node_no);
}


void Plant::get_leaf_no_dead(protocol::QueryValueData &qd)
{
    float temp = sum_real_array(g.leaf_no_dead, max_node);
    parent->sendVariable(qd, temp);
}


void Plant::get_leaf_area(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, protocol::vector<float>(g.leaf_area, g.leaf_area+max_node));
}


void Plant::get_height(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.canopy_height);
}


void Plant::get_width(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.canopy_width);
}


void Plant::get_root_depth(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.root_depth);
}


void Plant::get_plants(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.plants);
}


void Plant::get_cover_green(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.cover_green);
}


void Plant::get_cover_tot(protocol::QueryValueData &qd)
{
    float cover_tot = 1.0
        - (1.0 - g.cover_green)
        * (1.0 - g.cover_sen)
        * (1.0 - g.cover_dead);

    parent->sendVariable(qd, cover_tot);
}


void Plant::get_lai_sum(protocol::QueryValueData &qd)
{
    float lai_sum = g.lai + g.slai + g.tlai_dead;
    parent->sendVariable(qd, lai_sum);
}


void Plant::get_tlai(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.lai + g.slai);
}


void Plant::get_slai(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.slai);
}


void Plant::get_lai(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.lai);
}


void Plant::get_lai_canopy_green(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.lai_canopy_green);
}


void Plant::get_tlai_dead(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.tlai_dead);
}


void Plant::get_pai(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.pai);
// plant biomass
}


void Plant::get_grain_no(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.grain_no);
}


void Plant::get_root_wt(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.dm_green[root]);
}


void Plant::get_leaf_wt(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.dm_green[leaf]);
}


void Plant::get_stem_wt(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.dm_green[stem]);
}


void Plant::get_pod_wt(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.dm_green[pod]);
}


void Plant::get_grain_wt(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.dm_green[meal] + g.dm_green[oil]);
}


void Plant::get_meal_wt(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.dm_green[meal]);
}


void Plant::get_oil_wt(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.dm_green[oil]);
}


void Plant::get_dm_green(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, protocol::vector<float>(g.dm_green, g.dm_green+max_part));
}


void Plant::get_dm_senesced(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, protocol::vector<float>(g.dm_senesced, g.dm_senesced+max_part));
}


void Plant::get_dm_dead(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, protocol::vector<float>(g.dm_dead, g.dm_dead+max_part));
}


void Plant::get_yield(protocol::QueryValueData &qd)
{
    float yield = (g.dm_green[meal] + g.dm_dead[meal]
        + g.dm_green[oil] + g.dm_dead[oil])
        * gm2kg / sm2ha;
    parent->sendVariable(qd, yield);
}


void Plant::get_biomass(protocol::QueryValueData &qd)
{
    float biomass = (sum_real_array (g.dm_green, max_part)
        - g.dm_green[root]
        + sum_real_array (g.dm_senesced, max_part)
        - g.dm_senesced[root]
        + sum_real_array (g.dm_dead, max_part)
        - g.dm_dead[root])
        * gm2kg / sm2ha;

    parent->sendVariable(qd, biomass);
}


void Plant::get_green_biomass(protocol::QueryValueData &qd)
{
    float biomass = (sum_real_array (g.dm_green, max_part)
        - g.dm_green[root])
        * gm2kg / sm2ha;

    parent->sendVariable(qd, biomass);
}


void Plant::get_biomass_wt(protocol::QueryValueData &qd)
{
    float biomass = (sum_real_array (g.dm_green, max_part)
        - g.dm_green[root]
        + sum_real_array (g.dm_senesced, max_part)
        - g.dm_senesced[root]
        + sum_real_array (g.dm_dead, max_part)
        - g.dm_dead[root]);

    parent->sendVariable(qd, biomass);
}


void Plant::get_green_biomass_wt(protocol::QueryValueData &qd)
{
    float biomass = sum_real_array (g.dm_green, max_part)
        - g.dm_green[root];

    parent->sendVariable(qd, biomass);
}


void Plant::get_dlt_dm(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.dlt_dm);
}


void Plant::get_dlt_dm_pot_rue(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.dlt_dm_pot_rue);
}


void Plant::get_dlt_dm_pot_te(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.dlt_dm_pot_te);
}


void Plant::get_dlt_dm_grain_demand(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.dlt_dm_grain_demand);
}


void Plant::get_dlt_dm_green(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, protocol::vector<float>(g.dlt_dm_green,g.dlt_dm_green+max_part));
}


void Plant::get_dlt_dm_green_retrans(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, protocol::vector<float>(g.dlt_dm_green_retrans,g.dlt_dm_green_retrans+max_part));
}


void Plant::get_dlt_dm_detached(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, protocol::vector<float>(g.dlt_dm_detached, g.dlt_dm_detached+max_part));
}


void Plant::get_dlt_dm_senesced(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, protocol::vector<float>(g.dlt_dm_senesced, g.dlt_dm_senesced+max_part));
}


void Plant::get_dlt_dm_dead_detached(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, protocol::vector<float>(g.dlt_dm_dead_detached, g.dlt_dm_dead_detached+max_part));
}


void Plant::get_grain_oil_conc(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, c.grain_oil_conc);
}


void Plant::get_dlt_dm_oil_conv(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.dlt_dm_oil_conv);
}


void Plant::get_dlt_dm_oil_conv_retrans(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.dlt_dm_oil_conv_retranslocate);
}


void Plant::get_biomass_n(protocol::QueryValueData &qd)
{
    float biomass_n = (sum_real_array (g.n_green, max_part)
        - g.n_green[root]
        + sum_real_array (g.n_senesced, max_part)
        - g.n_senesced[root]
        + sum_real_array (g.n_dead, max_part)
        - g.n_dead[root]);
    //ih     :             * gm2kg / sm2ha;

    parent->sendVariable(qd, biomass_n);
}


void Plant::get_n_uptake(protocol::QueryValueData &qd)
{
    float biomass_n = (sum_real_array (g.n_green, max_part)
        - g.n_green[root]
        + sum_real_array (g.n_senesced, max_part)
        - g.n_senesced[root]
        + sum_real_array (g.n_dead, max_part)
        - g.n_dead[root]);
    //cih     :             * gm2kg / sm2ha;

    parent->sendVariable(qd, biomass_n);
}


void Plant::get_green_biomass_n(protocol::QueryValueData &qd)
{
    float biomass_n = (sum_real_array (g.n_green, max_part)
        - g.n_green[root]);
    //cih     :             * gm2kg / sm2ha;

    parent->sendVariable(qd, biomass_n);
}


void Plant::get_grain_n(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.n_green[meal] + g.n_green[oil]);
}


void Plant::get_leaf_n(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.n_green[leaf]);
}


void Plant::get_stem_n(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.n_green[stem]);
}


void Plant::get_root_n(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.n_green[root]);
}


void Plant::get_pod_n(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.n_green[pod]);
}


void Plant::get_n_senesced(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, protocol::vector<float>(g.n_senesced,g.n_senesced+max_part));
}


void Plant::get_n_dead(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, protocol::vector<float>(g.n_dead,g.n_dead+max_part));
}


void Plant::get_dlt_n_green(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, protocol::vector<float>(g.dlt_n_green, g.dlt_n_green+max_part));
}


void Plant::get_dlt_n_retrans(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, protocol::vector<float>(g.dlt_n_retrans, g.dlt_n_retrans+max_part));
}


void Plant::get_dlt_n_detached(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, protocol::vector<float>(g.dlt_n_detached, g.dlt_n_detached+max_part));
}


void Plant::get_dlt_n_dead_detached(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, protocol::vector<float>(g.dlt_n_dead_detached, g.dlt_n_dead_detached+max_part));
}


void Plant::get_temp_stress_photo(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.temp_stress_photo);
}


void Plant::get_swdef_pheno(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.swdef_pheno);
}


void Plant::get_swdef_photo(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.swdef_photo);
}


void Plant::get_swdef_expan(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.swdef_expansion);
}


void Plant::get_swdef_fixation(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.swdef_fixation);
}


void Plant::get_oxdef_photo(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.oxdef_photo);
}


void Plant::get_transp_eff(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.transp_eff);
}


void Plant::get_ep(protocol::QueryValueData &qd)
{
#if 0
    float rwu[max_layer];
    int num_layers = 1+count_of_real_vals (g.dlayer, max_layer);
    for (int layer = 0; layer < num_layers; layer++)
        {
        rwu[layer] = - g.dlt_sw_dep[layer];
        }
    parent->sendVariable(qd, protocol::vector<float>(rwu, rwu+num_layers));
#else
    float sum = 0.0;
    int num_layers = 1+count_of_real_vals (g.dlayer, max_layer);
    for (int layer = 0; layer < num_layers; layer++)
        {
        sum = sum + fabs(-1.0 * g.dlt_sw_dep[layer]);
        }
    parent->sendVariable(qd, sum);
#endif    
}


void Plant::get_cep(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, - g.transpiration_tot);
}


void Plant::get_sw_supply(protocol::QueryValueData &qd)
{
    int deepest_layer = find_layer_no (g.root_depth, g.dlayer, max_layer);
    float sw_supply_sum = sum_real_array (g.sw_supply, deepest_layer+1);
    parent->sendVariable(qd, sw_supply_sum);
}


void Plant::get_esw_layr(protocol::QueryValueData &qd)
{
    float esw_layr[max_layer];
    int num_layers = 1+count_of_real_vals (g.dlayer, max_layer);
    for (int layer = 0; layer < num_layers; layer++)
       {
       esw_layr[layer] = l_bound (g.sw_dep[layer] - p.ll_dep[layer], 0.0);
       }
    parent->sendVariable(qd, protocol::vector<float>(esw_layr,esw_layr+num_layers));
}

// plant nitrogen
void Plant::get_n_conc_stover(protocol::QueryValueData &qd)
{
    float n_conc = divide ((g.n_green[leaf]
        + g.n_green[stem]
        + g.n_green[pod])
        , (g.dm_green[leaf]
        + g.dm_green[stem]
        + g.dm_green[pod])
        , 0.0) * 100.0;

    parent->sendVariable(qd, n_conc);
}


void Plant::get_n_conc_leaf(protocol::QueryValueData &qd)
{
    float n_conc = divide (g.n_green[leaf]
        , g.dm_green[leaf]
        , 0.0) * 100.0;

    parent->sendVariable(qd, n_conc);
}


void Plant::get_n_conc_stem(protocol::QueryValueData &qd)
{
    float n_conc = divide (g.n_green[stem]
        , g.dm_green[stem]
        , 0.0) * 100.0;

    parent->sendVariable(qd, n_conc);
}


void Plant::get_n_conc_grain(protocol::QueryValueData &qd)
{
    float n_conc = divide (g.n_green[meal] + g.n_green[oil]
        , g.dm_green[meal] + g.dm_green[oil]
        , 0.0) * 100.0;

    parent->sendVariable(qd, n_conc);
}


void Plant::get_n_conc_meal(protocol::QueryValueData &qd)
{
    float n_conc = divide (g.n_green[meal]
        , g.dm_green[meal]
        , 0.0) * 100.0;

    parent->sendVariable(qd, n_conc);
}


void Plant::get_n_conc_crit(protocol::QueryValueData &qd)
{
    float n_conc = divide ((g.n_conc_crit[leaf]*g.dm_green[leaf]
        + g.n_conc_crit[stem]*g.dm_green[stem])
        , (g.dm_green[leaf] + g.dm_green[stem])
        , 0.0) * 100.0;

    parent->sendVariable(qd, n_conc);
}


void Plant::get_n_conc_min(protocol::QueryValueData &qd)
{
    float n_conc = divide ((g.n_conc_min[leaf]*g.dm_green[leaf]
        + g.n_conc_min[stem]*g.dm_green[stem])
        , (g.dm_green[leaf]
        + g.dm_green[stem])
        , 0.0) * 100.0;

    parent->sendVariable(qd, n_conc);
}


void Plant::get_n_uptake_stover(protocol::QueryValueData &qd)
{
    float apt_n_up = (g.n_green[leaf]+g.n_green[stem]+g.n_green[pod]);
    //cih     :            *gm2kg /sm2ha;
    parent->sendVariable(qd, apt_n_up);
}


void Plant::get_no3_tot(protocol::QueryValueData &qd)
{
    int deepest_layer = find_layer_no (g.root_depth, g.dlayer, max_layer);
    float no3gsm_tot = sum_real_array (g.no3gsm, deepest_layer+1);
    parent->sendVariable(qd, no3gsm_tot);
}


void Plant::get_n_demand(protocol::QueryValueData &qd)
{
    float n_demand = sum_real_array (g.n_demand, max_part);
    parent->sendVariable(qd, n_demand);
}


void Plant::get_n_supply_soil(protocol::QueryValueData &qd)
{
    int deepest_layer = find_layer_no (g.root_depth, g.dlayer, max_layer);
    float n_uptake_sum = - sum_real_array (g.dlt_no3gsm, deepest_layer+1);
    parent->sendVariable(qd, n_uptake_sum);
}


void Plant::get_dlt_n_fixed_pot(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.n_fix_pot);
}


void Plant::get_dlt_n_fixed(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.n_fix_uptake);
}


void Plant::get_n_fixed_tops(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.n_fixed_tops);
}


void Plant::get_nfact_photo(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.nfact_photo);
}


void Plant::get_nfact_expan(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.nfact_expansion);
}


void Plant::get_nfact_grain(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.nfact_grain_conc);
}

void Plant::get_nfact_pheno(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.nfact_pheno);
}


void Plant::get_nfact_grain_tot(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, protocol::vector<float>(g.cnd_grain_conc,g.cnd_grain_conc+max_stage));
}


void Plant::get_rlv(protocol::QueryValueData &qd)
{
	 float rlv[max_layer];
    int num_layers = 1+count_of_real_vals (g.dlayer, max_layer);
    for (int layer = 0; layer < num_layers; layer++)
       {
       rlv[layer] = divide (g.root_length[layer]
          ,g.dlayer[layer], 0.0);
       }
    parent->sendVariable(qd, protocol::vector<float>(rlv,rlv+num_layers));
}


void Plant::get_no3_demand(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.ext_n_demand*gm2kg/sm2ha);
}


void Plant::get_sw_demand(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.sw_demand);
}


void Plant::get_sw_demand_te(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.sw_demand_te);
}


void Plant::get_root_length(protocol::QueryValueData &qd)
{
    int num_layers = 1+count_of_real_vals (g.dlayer, max_layer);
    parent->sendVariable(qd, protocol::vector<float>(g.root_length,g.root_length+num_layers));
}


void Plant::get_root_length_dead(protocol::QueryValueData &qd)
{
    int num_layers = 1+count_of_real_vals (g.dlayer, max_layer);
    parent->sendVariable(qd, protocol::vector<float>(g.root_length_dead, g.root_length_dead+num_layers));
}


void Plant::get_no3gsm_uptake_pot(protocol::QueryValueData &qd)
{
    int num_layers = 1+count_of_real_vals (g.dlayer, max_layer);
    parent->sendVariable(qd, protocol::vector<float>(g.no3gsm_uptake_pot, g.no3gsm_uptake_pot+num_layers));
}


void Plant::get_no3_swfac(protocol::QueryValueData &qd)
{
    float swfac[100];
    int num_layers = 1+count_of_real_vals (g.dlayer, max_layer);
    for (int layer=0; layer < num_layers; layer++)
        {
        swfac[layer] = pow(divide(g.sw_avail[layer],g.sw_avail_pot[layer],0.0), 2);
        swfac[layer] = bound(swfac[layer],0.0,1.0);
        }

    parent->sendVariable(qd, protocol::vector<float>(swfac, swfac+num_layers));
}


void Plant::get_leaves_per_node(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.leaves_per_node);
}

void Plant::get_dlt_slai_age(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.dlt_slai_age);
}
void Plant::get_dlt_slai_light(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.dlt_slai_light);
}
void Plant::get_dlt_slai_water(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.dlt_slai_water);
}
void Plant::get_dlt_slai_frost(protocol::QueryValueData &qd)
{
    parent->sendVariable(qd, g.dlt_slai_frost);
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

         bound_check_real_array(dlt_dm_incorp, max_layer, 0.0, dlt_dm_root * gm2kg / sm2ha,
                                "dlt_dm_incorp");

         crop_root_dist(g_dlayer, g_root_length, g_root_depth, dlt_N_incorp,
                        dlt_N_root * gm2kg /sm2ha, max_layer);

         bound_check_real_array(dlt_N_incorp,  max_layer, 0.0, dlt_N_root * gm2kg / sm2ha,
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

float Plant::legume_stage_code(
                     float *c_stage_code_list //(INPUT)  list of stage numbers
                   , float *g_phase_tt        //(INPUT)  Cumulative growing degree days required for each stage (deg days)
                   , float *g_tt_tot          //(INPUT)  Cumulative growing degree days required for each stage (deg days)
                   , float stage_no           //(INPUT) stage number to convert
                   , float *stage_table       //(INPUT) table of stage codes
                   , int numvals)             //(INPUT) size_of of table
  {
//*+  Purpose
//*       Return an interpolated stage code from a table of stage_codes
//*       and a nominated stage number. Returns 0 if the stage number is not
//*       found. Interpolation is done on thermal time.

//*+  Mission Statement
//*     Get the stage code from a table of stage codes

   float   phase_tt;              // required thermal time between stages
                                 //      ! (oC)
   float   fraction_of;           //!
   int     next_stage;            //! next stage number to use
   float   tt_tot;                //! elapsed thermal time between stages
                                 ///      ! (oC)
   int     this_stage;            //! this stage to use
   float   x_stage_code;          //! interpolated stage code


      if (numvals >= 2)
         {
         // we have a valid table
         this_stage = stage_no_of (stage_table[0]
                                      , c_stage_code_list
                                      , max_stage);

         for (int i = 1; i < numvals; i++)
            {
            next_stage = stage_no_of (stage_table[i]
                                         , c_stage_code_list
                                         , max_stage);

            if (stage_is_between (this_stage, next_stage, stage_no))
               {
               // we have found its place
               tt_tot = sum_between (this_stage-1, next_stage-1, g_tt_tot);
               phase_tt = sum_between (this_stage-1, next_stage-1, g_phase_tt);
               fraction_of = divide (tt_tot, phase_tt, 0.0);
               fraction_of = bound(fraction_of, 0.0, 0.999);
               x_stage_code = stage_table[i-1]
                           + (stage_table[i] - stage_table[i-1])
                           * fraction_of;
//Cx
//fprintf(stdout, "%d,%d,%.9f,%.9f,%.9f,%.9f\n", g.day_of_year,
//this_stage, tt_tot,phase_tt,fraction_of,x_stage_code);
               break;
               }
            else
               {
               x_stage_code = 0.0;
               this_stage = next_stage;
               }
            }
         }
      else
         {
         // we have no valid table
         x_stage_code = 0.0;

//         write (error_message,'(a, i10)')
//     :               'Invalid lookup table - number of values ='
//     :              , numvals
         warning_error (&err_user, "Bad stage lookup table");
         }

   return x_stage_code;
   }



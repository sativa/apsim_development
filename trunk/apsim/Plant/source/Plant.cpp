#include <stdio.h>
#include <math.h>
#include <string>

#include <map>
#include <list>
#include <vector>

#include <stdexcept>

#include <boost/function.hpp>
#include <boost/bind.hpp>

#include <ComponentInterface/Component.h>
#include <ComponentInterface/Type.h>

#include "PlantComponent.h"
#include "PlantLibrary.h"
#include "PlantPhenology.h"
#include "WheatPhenology.h"
#include "TTTPhenology.h"
#include "TTTRatePhenology.h"
#include "Plant.h"
#include "PlantParts.h"
#include "PlantFruit.h"
#include "Observers.h"
#include "ReproStruct.h"

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

//      plant_tolerances
const float  tolerance_lai = 1.0e-4 ;
const float  tolerance_dm = 1.0e-4 ;

/////////////These might be redundancies??//////////
void push_routine (const char *) {};
void pop_routine (const char *) {};

/////////////////////////
Plant::Plant(PlantComponent *P)
    {
    parent = P;
    c.x_dm_sen_frac = new float* [max_part];
    c.y_dm_sen_frac = new float* [max_part];
    c.num_dm_sen_frac = new int [max_part];
    for (int part = 0; part < max_part; part++)
         {
         c.x_dm_sen_frac[part] = new float [max_table];
         c.y_dm_sen_frac[part] = new float [max_table];
         }

    c.part_names.resize(max_part);
    c.part_names[root] = "root";
    phenology = NULL;

    g.cswd_pheno.setup(&g.swdef_pheno);
    g.cswd_photo.setup(&g.swdef_photo);
    g.cnd_grain_conc.setup(&g.nfact_grain_conc);
    g.cnd_photo.setup(&g.nfact_photo);
    g.cswd_expansion.setup(&g.swdef_expansion);

    stageObservers.addObserver(&g.cswd_photo);
    stageObservers.addObserver(&g.cswd_expansion);
    stageObservers.addObserver(&g.cnd_grain_conc);
    stageObservers.addObserver(&g.cnd_photo);
    otherObservers.addObserver(&g.cswd_pheno);
    }

Plant::~Plant()
    {
    for (int part = 0; part < max_part; part++)
         {
         delete [] c.x_dm_sen_frac[part];
         delete [] c.y_dm_sen_frac[part];
         }
    delete [] c.x_dm_sen_frac;
    delete [] c.y_dm_sen_frac;
    delete [] c.num_dm_sen_frac;
    if (phenology) delete phenology;
    if (fruitPart) delete fruitPart;
    if (stemPart) delete stemPart;
    if (leafPart) delete leafPart;
    if (reproStruct) delete reproStruct;
    if (floweringEventObserver) delete floweringEventObserver;
    if (maturityEventObserver) delete maturityEventObserver;
    }


// Init1. Set up plant structure
void Plant::doInit1(protocol::Component *s)
    {
    string scratch = s->readParameter ("constants", "phenology_model");
    if (scratch == "")
       throw std::invalid_argument("The parameter 'phenology_model'\nisn't in your ini file.\n\nGet one.\n");
    else if (scratch == "legume")
       phenology = new TTTPhenology(parent, this);
    else if (scratch == "tttrate")
       phenology = new TTTRatePhenology(parent, this);
    else if (scratch == "wheat")
       phenology = new WheatPhenology(parent, this);
    else
       throw std::invalid_argument("Unknown phenology model '" + scratch + "'");
    myThings.push_back(phenology);

    // NB. These must work in conjunction with setupHacks()..
    stemPart = new plantStemPart(this, "stem");
    myThings.push_back(stemPart);
    myParts.push_back(stemPart);
    myStoverParts.push_back(stemPart);

    leafPart = new plantLeafPart(this, "leaf");
    myThings.push_back(leafPart);
    myParts.push_back(leafPart);
    myStoverParts.push_back(leafPart);

    fruitPart = new PlantFruit(this, "fruit");
    myThings.push_back(fruitPart);
    myParts.push_back(fruitPart);
    myStoverParts.push_back(fruitPart);

    fruitPart->doInit1();


    reproStruct = new ReproStruct(this, "bruce");
    //myThings.push_back(reproStruct);
    //myParts.push_back(reproStruct);

    floweringEventObserver = new eventObserver("flowering", this);
    myThings.push_back(floweringEventObserver);

    maturityEventObserver = new eventObserver("maturity", this);
    myThings.push_back(maturityEventObserver);

    plant_zero_all_globals();
    zero_p_variables();

    }

void Plant::initialise(void)
   {
   doIDs();                 // Gather IDs for getVariable requests
   PlantP_set_phosphorus_aware(parent); // See whether a P module is plugged in
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

   // we want to send these events out
   id.add_residue_p = parent->addRegistration(RegistrationType::event,
                                    "add_residue_p", "",
                                    "", "");
   id.incorp_fom_p = parent->addRegistration(RegistrationType::event,
                                    "incorp_fom_p", "",
                                    "", "");
   // we will want this variable
   // ids.layered_p_uptake = systemInterface->addRegistration(RegistrationType::get,
   //                                "layered_p_uptake", floatType,
   //                                "", "");
   }

// Register Methods, Events,
void Plant::doRegistrations(protocol::Component *system)
   {
   // Events
   setupEvent(parent, "prepare",     RegistrationType::respondToEvent, &Plant::doPrepare);
   setupEvent(parent, "process",     RegistrationType::respondToEvent, &Plant::doProcess);
   setupEvent(parent, "tick",        RegistrationType::respondToEvent, &Plant::doTick);
   setupEvent(parent, "newmet",      RegistrationType::respondToEvent, &Plant::doNewMet);
   setupEvent(parent, "new_profile", RegistrationType::respondToEvent, &Plant::doNewProfile);
   setupEvent(parent, "sow",         RegistrationType::respondToEvent, &Plant::doSow);
   setupEvent(parent, "harvest",     RegistrationType::respondToEvent, &Plant::doHarvest);
   setupEvent(parent, "end_crop",    RegistrationType::respondToEvent, &Plant::doEndCrop);
   setupEvent(parent, "kill_crop",   RegistrationType::respondToEvent, &Plant::doKillCrop);
   setupEvent(parent, "end_run",     RegistrationType::respondToEvent, &Plant::doEndRun);
   setupEvent(parent, "kill_stem",   RegistrationType::respondToEvent, &Plant::doKillStem);
   setupEvent(parent, "remove_crop_biomass",   RegistrationType::respondToEvent, &Plant::doRemoveCropBiomass);


   // Send My Variable

   setupGetFunction(parent, "plant_status", protocol::DTstring, false,
                     &Plant::get_plant_status, "", "Plant Status");

// XXXX UGLY HACK workaround for broken coordinator in 3.4
//   id = parent->addGettableVar("crop_type", protocol::DTstring, false, "");
//   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_crop_type));

   parent->addGettableVar("crop_class",
               g.crop_class, "", "Plant crop class");

   setupGetFunction(parent, "leaf_no", protocol::DTsingle, false,
                    &Plant::get_leaf_no, "leaves/plant", "number of leaves per plant");

   parent->addGettableVar("node_no",
               g.node_no, "nodes/plant", "number of mainstem nodes per plant");

   parent->addGettableVar("dlt_leaf_no",
               g.dlt_leaf_no, "leaves/m2", "Change in number of leaves");

   parent->addGettableVar("dlt_node_no",
               g.dlt_node_no, "nodes/m2", "Change in number of nodes");

   setupGetFunction(parent, "leaf_no_dead", protocol::DTsingle, false,
                     &Plant::get_leaf_no_dead, "leaves/m2", "number of dead leaves per square meter");

   setupGetFunction(parent, "leaf_area", protocol::DTsingle, true,
                    &Plant::get_leaf_area, "mm^2", "Leaf area for each node");

//   id = parent->addGettableVar("height", protocol::DTsingle, false, "mm");
//   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_height));

   parent->addGettableVar("width",
               g.canopy_width, "mm", "canopy row width");

   parent->addGettableVar("root_depth",
               g.root_depth, "mm", "depth of roots");

   parent->addGettableVar("plants",
               g.plants, "plants/m^2", "Plant desnity");

//   id = parent->addGettableVar("cover_green", protocol::DTsingle, false, "");
//   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_cover_green));

//   id = parent->addGettableVar("cover_tot", protocol::DTsingle, false, "");
//   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_cover_tot));

   parent->addGettableVar("dlt_leaf_no_pot",
               g.dlt_leaf_no_pot, "m^2/m^2", "Leaf no");

   parent->addGettableVar("lai_canopy_green",
               g.lai_canopy_green, "m^2/m^2", "Green lai");

//   parent->addGettableVar("tlai_dead",
//               g.tlai_dead, "m^2/m^2", "tlai dead");

   parent->addGettableVar("dlt_slai_age",
               g.dlt_slai_age, "m^2/m^2", "Change in lai via age");

   parent->addGettableVar("dlt_slai_light",
               g.dlt_slai_light, "m^2/m^2", "Change in lai via light");

   parent->addGettableVar("dlt_slai_water",
               g.dlt_slai_water, "m^2/m^2", "Change in lai via water stress");

   parent->addGettableVar("dlt_slai_frost",
               g.dlt_slai_frost, "m^2/m^2", "Change in lai via low temperature");

   parent->addGettableVar("root_wt",
               g.dm_green[root], "g/m^2", "Weight of roots");

   setupGetFunction(parent, "dm_green", protocol::DTsingle, true,
                    &Plant::get_dm_green, "g/m^2", "Weight of green material");

   setupGetFunction(parent, "dm_senesced", protocol::DTsingle, true,
                    &Plant::get_dm_senesced, "g/m^2", "Weight of senesced material");

   setupGetFunction(parent, "dm_dead", protocol::DTsingle, true,
                     &Plant::get_dm_dead, "g/m^2","Weight of dead material");

   setupGetFunction(parent, "biomass", protocol::DTsingle, false,
                    &Plant::get_biomass, "kg/ha", "Biomass");

   setupGetFunction(parent, "biomass_wt", protocol::DTsingle, false,
                    &Plant::get_biomass_wt, "g/m^2", "Biomass weight");

   setupGetFunction(parent, "green_biomass", protocol::DTsingle, false,
                    &Plant::get_green_biomass, "kg/ha", "Green Biomass weight");

   setupGetFunction(parent, "green_biomass_wt", protocol::DTsingle, false,
                    &Plant::get_green_biomass_wt, "g/m^2", "Green Biomass weight");

   setupGetFunction(parent, "stover_wt", protocol::DTsingle, false,
                    &Plant::get_stover_biomass_wt, "g/m^2", "Stover Biomass weight");

   setupGetFunction(parent, "dm_plant_min", protocol::DTsingle, true,
                    &Plant::get_dm_plant_min, "g/m^2", "Minimum weights");


   parent->addGettableVar("dlt_dm",
               g.dlt_dm, "g/m^2", "Change in dry matter");

   parent->addGettableVar("dlt_dm_pot_rue",
               g.dlt_dm_pot_rue, "g/m^2", "Potential dry matter production via photosynthesis");

   parent->addGettableVar("dlt_dm_pot_te",
               g.dlt_dm_pot_te, "g/m^2", "Potential dry matter production via transpiration");


   setupGetFunction(parent, "dlt_dm_green", protocol::DTsingle, true,
                    &Plant::get_dlt_dm_green,  "g/m^2", "change in green pool");

   setupGetFunction(parent, "dlt_dm_green_retrans", protocol::DTsingle, true,
                    &Plant::get_dlt_dm_green_retrans, "g/m^2", "change in green pool from retranslocation");

   setupGetFunction(parent, "dlt_dm_detached", protocol::DTsingle, true,
                    &Plant::get_dlt_dm_detached,  "g/m^2", "change in dry matter via detachment");

   setupGetFunction(parent, "dlt_dm_senesced", protocol::DTsingle, true,
                    &Plant::get_dlt_dm_senesced,"g/m^2", "change in dry matter via senescence");

   setupGetFunction(parent, "dlt_dm_dead_detached", protocol::DTsingle, true,
                    &Plant::get_dlt_dm_dead_detached,"g/m^2", "change in dead dry matter via detachment");

   setupGetFunction(parent, "dlt_dm_green_dead", protocol::DTsingle, true,
                    &Plant::get_dlt_dm_green_dead,  "g/m^2", "change in green dry matter via plant death");

   setupGetFunction(parent, "dlt_dm_senesced_dead", protocol::DTsingle, true,
                    &Plant::get_dlt_dm_senesced_dead,  "g/m^2", "change in green dry matter via plant death");

   setupGetFunction(parent, "biomass_n", protocol::DTsingle, false,
                    &Plant::get_biomass_n,  "g/m^2", "N in total biomass");

   setupGetFunction(parent, "n_uptake", protocol::DTsingle, false,
                    &Plant::get_n_uptake, "g/m^2", "N uptake");

   setupGetFunction(parent, "green_biomass_n", protocol::DTsingle, false,
                    &Plant::get_green_biomass_n, "g/m^2", "N in green biomass");

   parent->addGettableVar("root_n",
               g.n_green[root], "g/m^2", "N in roots");

   setupGetFunction(parent, "n_green", protocol::DTsingle, true,
                    &Plant::get_n_green,  "g/m^2", "N in green");

   setupGetFunction(parent, "n_senesced", protocol::DTsingle, true,
                    &Plant::get_n_senesced,  "g/m^2", "N in senesced");

   setupGetFunction(parent, "n_dead", protocol::DTsingle, true,
                    &Plant::get_n_dead,"g/m^2", "N in dead");

   setupGetFunction(parent, "dlt_n_green", protocol::DTsingle, true,
                    &Plant::get_dlt_n_green, "g/m^2", "N in delta green");

   setupGetFunction(parent, "dlt_n_dead", protocol::DTsingle, true,
                    &Plant::get_dlt_n_dead, "g/m^2", "N in delta dead");

   setupGetFunction(parent, "dlt_n_retrans", protocol::DTsingle, true,
                    &Plant::get_dlt_n_retrans, "g/m^2", "N in retranslocate");

   setupGetFunction(parent, "dlt_n_senesced_trans", protocol::DTsingle, true,
                    &Plant::get_dlt_n_senesced_trans, "g/m^2", "N in translocate");

   setupGetFunction(parent, "dlt_n_senesced_retrans", protocol::DTsingle, true,
                    &Plant::get_dlt_n_senesced_retrans, "g/m^2", "N in retranslocate");

   setupGetFunction(parent, "dlt_n_senesced", protocol::DTsingle, true,
                    &Plant::get_dlt_n_senesced, "g/m^2", "N in delta senesced");

   setupGetFunction(parent, "dlt_n_senesced_dead", protocol::DTsingle, true,
                    &Plant::get_dlt_n_senesced_dead, "g/m^2", "N in delta senesced dead");

   setupGetFunction(parent, "dlt_n_detached", protocol::DTsingle, true,
                    &Plant::get_dlt_n_detached, "g/m^2", "N in detached");

   setupGetFunction(parent, "dlt_n_dead_detached", protocol::DTsingle, true,
                    &Plant::get_dlt_n_dead_detached,  "g/m^2", "N in dead detached");

   parent->addGettableVar("temp_stress_photo",
               g.temp_stress_photo, "", "Temperature Stress in photosynthesis");

   parent->addGettableVar("swdef_pheno",
               g.swdef_pheno, "", "Soil water deficit in phenological development");

   parent->addGettableVar("swdef_photo",
               g.swdef_photo, "", "Soil water deficit in photosynthesis");

   parent->addGettableVar("swdef_expan",
               g.swdef_expansion, "", "Soil water deficit in leaf expansion");

   parent->addGettableVar("swdef_fixation",
               g.swdef_fixation, "", "Soil water deficit in N fixation");

   parent->addGettableVar("oxdef_photo",
               g.oxdef_photo, "", "Oxygen deficit in photosynthesis");

   setupGetFunction(parent, "sw_stress_pheno", protocol::DTsingle, false,
                    &Plant::get_swstress_pheno,
                          "","Soil water stress for phenological development");

   setupGetFunction(parent, "sw_stress_photo", protocol::DTsingle, false,
                    &Plant::get_swstress_photo,
                    "","Soil water stress for photosynthesis");

   setupGetFunction(parent, "sw_stress_expan", protocol::DTsingle, false,
                    &Plant::get_swstress_expan,
                    "","Soil water stress for leaf expansion");

   setupGetFunction(parent, "sw_stress_fixation", protocol::DTsingle, false,
                    &Plant::get_swstress_fixation,
                    "","Soil water stress for N fixation");

   parent->addGettableVar("transp_eff",
               g.transp_eff, "g/m2/mm", "Transpiration Efficiency");

   setupGetFunction(parent, "ep", protocol::DTsingle, false,
                    &Plant::get_ep, "mm", "Plant water uptake");

   setupGetFunction(parent, "sw_uptake", protocol::DTsingle, true,
                    &Plant::get_sw_uptake, "mm", "Plant water uptake per layer");

   setupGetFunction(parent, "cep", protocol::DTsingle, false,
                    &Plant::get_cep,
                    "mm", "Cumulative plant water uptake");

   setupGetFunction(parent, "sw_supply", protocol::DTsingle, false,
                    &Plant::get_sw_supply, "mm", "Soil water supply");

   setupGetFunction(parent, "sw_supply_layr", protocol::DTsingle, true,
                    &Plant::get_sw_supply_layr, "mm", "Soil water supply");

   setupGetFunction(parent, "esw_layr", protocol::DTsingle, true,
                    &Plant::get_esw_layr, "mm", "Extractable soil water");

   setupGetFunction(parent, "n_conc_stover", protocol::DTsingle, false,
                    &Plant::get_n_conc_stover, "%", "N concentration in stover");

   setupGetFunction(parent, "n_conc_root", protocol::DTsingle, false,
                    &Plant::get_n_conc_root, "%", "N concentration in root");

   setupGetFunction(parent, "n_conc_crit", protocol::DTsingle, false,
                    &Plant::get_n_conc_crit, "%", "critical N content");

   setupGetFunction(parent, "n_conc_min", protocol::DTsingle, false,
                    &Plant::get_n_conc_min, "%", "minimum N content");

   setupGetFunction(parent, "n_uptake_stover", protocol::DTsingle, false,
                    &Plant::get_n_uptake_stover,
                    "g/m^2", "N taken up by agp");

   setupGetFunction(parent, "no3_tot", protocol::DTsingle, false,
                    &Plant::get_no3_tot,
                    "g/m^2", "NO3 available to plants");

   setupGetFunction(parent, "n_demand", protocol::DTsingle, false,
                    &Plant::get_n_demand,
                    "g/m^2", "N demand");

   setupGetFunction(parent, "n_demanded", protocol::DTsingle, true,
                    &Plant::get_n_demanded,
                    "g/m^2", "N demanded");

   setupGetFunction(parent, "n_supply_soil", protocol::DTsingle, false,
                    &Plant::get_n_supply_soil,
                    "g/m^2", "N supply");

   parent->addGettableVar("dlt_n_fixed_pot",
               g.n_fix_pot, "g/m^2", "potential N fixation");

   parent->addGettableVar("dlt_n_fixed",
               g.n_fix_uptake, "g/m^2", "N fixation");

   parent->addGettableVar("n_fixed_tops",
               g.n_fixed_tops, "g/m^2", "N fixation");

   parent->addGettableVar("nfact_photo",
               g.nfact_photo, "", "N factor for photosynthesis");

   parent->addGettableVar("nfact_pheno",
               g.nfact_pheno, "", "N factor for phenology");

   parent->addGettableVar("nfact_expan",
               g.nfact_expansion, "", "N factor for leaf expansion");

   parent->addGettableVar("nfact_grain",
               g.nfact_grain_conc, "", "N factor for ??");

   parent->addGettableVar("remove_biom_pheno",
               g.remove_biom_pheno, "", "biomass removal factor for phenology");

   setupGetFunction(parent, "nfact_grain_tot", protocol::DTsingle, false,
                    &Plant::get_nfact_grain_tot,
                    "", "Summed grain N factor for current stage");

   setupGetFunction(parent, "n_stress_photo", protocol::DTsingle, false,
                    &Plant::get_nstress_photo,
                    "","N stress for photosyntesis");

   setupGetFunction(parent, "n_stress_pheno", protocol::DTsingle, false,
                    &Plant::get_nstress_pheno,
                    "","N stress for phenology");

   setupGetFunction(parent, "n_stress_expan", protocol::DTsingle, false,
                    &Plant::get_nstress_expan,
                    "","N stress for leaf expansion");

   setupGetFunction(parent, "n_stress_grain", protocol::DTsingle, false,
                    &Plant::get_nstress_grain,
                    "","N stress for grain filling");

   setupGetFunction(parent, "rlv", protocol::DTsingle, true,
                    &Plant::get_rlv, "mm/mm^3", "Root length density");

   setupGetFunction(parent, "rld", protocol::DTsingle, true,
                    &Plant::get_rlv, "mm/mm^3", "Root length density");

   setupGetFunction(parent, "no3_demand", protocol::DTsingle, false,
                    &Plant::get_no3_demand,
                    "kg/ha", "Demand for NO3");

   parent->addGettableVar("sw_demand",
               g.sw_demand, "mm", "Demand for sw");

   parent->addGettableVar("sw_demand_te",
               g.sw_demand_te, "mm", "Demand for sw");

   setupGetFunction(parent, "root_length", protocol::DTsingle, true,
                    &Plant::get_root_length,
                    "mm/mm^2", "Root length");

   setupGetFunction(parent, "root_length_dead", protocol::DTsingle, true,
                    &Plant::get_root_length_dead,
                          "mm/mm^2", "Dead root length");

   setupGetFunction(parent, "no3gsm_uptake_pot", protocol::DTsingle, true,
                    &Plant::get_no3gsm_uptake_pot,
                    "g/m2", "Pot NO3 uptake");

   setupGetFunction(parent, "nh4gsm_uptake_pot", protocol::DTsingle, true,
                    &Plant::get_nh4gsm_uptake_pot,
                    "g/m2", "Pot NH4 uptake");

   setupGetFunction(parent, "no3_swfac", protocol::DTsingle, false,
                    &Plant::get_no3_swfac,
                    "", "Work this out...>>");

   parent->addGettableVar("leaves_per_node",
               g.leaves_per_node, "","");

   setupGetFunction(parent, "no3_uptake", protocol::DTsingle, false,
                    &Plant::get_no3_uptake,
                    "","NO3 uptake");

   setupGetFunction(parent, "nh4_uptake", protocol::DTsingle, false,
                    &Plant::get_nh4_uptake,
                    "","NH4 uptake");

   setupGetFunction(parent, "parasite_dm_supply", protocol::DTsingle, false,
                     &Plant::get_parasite_c_gain,
                     "g/m^2", "Assimilate to parasite");

   setupGetFunction(parent, "leaf_area_tot", protocol::DTsingle, false,
                    &Plant::get_leaf_area_tot,
                    "m^2", "Total plant leaf area");

   setupGetFunction(parent, "p_green", protocol::DTsingle, true,
                    &Plant::get_p_green,
                    "g/m^2", "P in green plant parts");

   setupGetFunction(parent, "p_dead", protocol::DTsingle, true,
                    &Plant::get_p_dead,
                    "g/m^2", "P in dead plant parts");

   setupGetFunction(parent, "p_senesced", protocol::DTsingle, true,
                    &Plant::get_p_sen,
                    "g/m^2","P in senesced plant parts");

   setupGetFunction(parent, "p_sen", protocol::DTsingle, true,
                    &Plant::get_p_sen,
                    "g/m^2","P in senesced plant parts");

   setupGetFunction(parent, "p_demand", protocol::DTsingle, false,
                    &Plant::get_p_demand,
                    "g/m^2","");

   setupGetFunction(parent, "p_demand_parts", protocol::DTsingle, true,
                    &Plant::get_p_demand_parts,
                    "g/m^2","");

   parent->addGettableVar("pfact_photo",
               g.pfact_photo,
               "", "P factor in photosynthesis");

   parent->addGettableVar("pfact_pheno",
               g.pfact_pheno,
               "", "P factor in phenology");

   parent->addGettableVar("pfact_expansion",
               g.pfact_expansion,
               "", "P factor in leaf expansion");

   parent->addGettableVar("pfact_grain",
               g.pfact_grain,
               "", "P factor in grain");

   setupGetFunction(parent, "p_stress_photo", protocol::DTsingle, false,
                    &Plant::get_pstress_photo,
                    "", "P stress in photosynthesis");

   setupGetFunction(parent, "p_stress_pheno", protocol::DTsingle, false,
                    &Plant::get_pstress_pheno,
                    "", "P stress in phenology");

   setupGetFunction(parent, "p_stress_expansion", protocol::DTsingle, false,
                    &Plant::get_pstress_expansion,
                    "", "P stress in leaf expansion");

   setupGetFunction(parent, "p_stress_expan", protocol::DTsingle, false,
                    &Plant::get_pstress_expansion,
                    "", "P stress in leaf expansion");

   setupGetFunction(parent, "p_stress_grain", protocol::DTsingle, false,
                    &Plant::get_pstress_grain,
                    "", "P stress in grain");

   setupGetFunction(parent, "biomass_p", protocol::DTsingle, false,
                    &Plant::get_biomass_p,
                    "g/m^2","P in biomass");

   setupGetFunction(parent, "p_uptake", protocol::DTsingle, false,
                    &Plant::get_biomass_p,
                    "g/m^2","P  uptake");

   setupGetFunction(parent, "green_biomass_p", protocol::DTsingle, false,
                    &Plant::get_green_biomass_p,
                    "g/m^2","P in green biomass");

   parent->addGettableVar("root_p",
               g.p_green[root],
               "g/m^2","P in roots");

   setupGetFunction(parent, "dlt_p_green", protocol::DTsingle, true,
                    &Plant::get_dlt_p_green,
                    "g/m^2","dlt P parts");

   setupGetFunction(parent, "dlt_p_retrans", protocol::DTsingle, true,
                    &Plant::get_dlt_p_retrans,
                    "g/m^2","dlt P parts");

   setupGetFunction(parent, "dlt_p_detached", protocol::DTsingle, true,
                    &Plant::get_dlt_p_detached,
                    "g/m^2","dlt P detached");

   setupGetFunction(parent, "dlt_p_dead", protocol::DTsingle, true,
                    &Plant::get_dlt_p_dead,
                    "g/m^2","dlt P in dead");

   setupGetFunction(parent, "dlt_p_sen", protocol::DTsingle, true,
                    &Plant::get_dlt_p_sen,
                    "g/m^2","dlt P in senesced");

   setupGetFunction(parent, "p_conc_stover", protocol::DTsingle, false,
                    &Plant::get_p_conc_stover,
                    "%","P in stover");

   setupGetFunction(parent, "p_uptake_stover", protocol::DTsingle, false,
                    &Plant::get_p_uptake_stover,
                    "%","P in stover");

   setupGetFunction(parent, "ll_dep", protocol::DTsingle, true,
                    &Plant::get_ll,
                    "mm","Crop lower limit");

#undef setupGetVar
#undef setupGetFunction

   unsigned int id;
   // Set My Variable
   id = system->addRegistration(RegistrationType::respondToSet, "crop_class", stringType);
   IDtoSetFn.insert(UInt2SetFnMap::value_type(id,&Plant::set_plant_crop_class));

   system->addRegistration(RegistrationType::event, "sowing", "", "", "");
   system->addRegistration(RegistrationType::event, "harvesting", "", "", "");

   for (vector<plantThing *>::iterator t = myThings.begin();
        t != myThings.end();
        t++)
      (*t)->doRegistrations(parent);
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

void Plant::doPlantEvent(const string &e)
   {
   for (vector<plantThing *>::iterator t = myThings.begin();
        t != myThings.end();
        t++)
      (*t)->onPlantEvent(e);
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
  zero_daily_p_variables();

  plant_get_other_variables ();     // request and receive variables from owner-modules
  if (g.plant_status == out)
     {
     plant_zero_variables ();
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
//  plant_zero_variables ();
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

// Field a end run event
void Plant::doEndRun(unsigned &, unsigned &,protocol::Variant &/*v*/)
   {
   plant_zero_variables ();
   }

// Field a Remove Crop Biomass event
void Plant::doRemoveCropBiomass(unsigned &, unsigned &, protocol::Variant &v)
   {
   plant_remove_crop_biomass (v);
   }

// Field a class change event
void Plant::doAutoClassChange(unsigned &/*fromId*/, unsigned &eventId, protocol::Variant &v)
  {
  string ps = IDtoAction[eventId];
  plant_auto_class_change(ps.c_str());
  }

// Field a Tick event
void Plant::doTick(unsigned &, unsigned &, protocol::Variant &v)
  {
  struct protocol::timeType tick;
  v.unpack(tick);
  double sd = (double)tick.startday;
  jday_to_day_of_year(&sd, &g.day_of_year, &g.year);
  fruitPart->doTick(tick);
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
     fruitPart->doNewMet(newmet);

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
        double dlt_dm_supply_by_veg = g.dlt_dm;

        g.dlt_dm_yield_demand_fruit = fruitPart->dm_yield_demand (dlt_dm_supply_by_veg);

        legnew_dm_partition1 (c.frac_leaf[(int)phenology->stageNumber()-1]
                              , c.ratio_root_shoot[(int)phenology->stageNumber()-1]
                              , c.sla_min
                              , dlt_dm_supply_by_veg
                              , g.dlt_dm_yield_demand_fruit
                              , &g.dlt_dm_supply_to_fruit
                              , g.dlt_dm_green);

       }
    else if (option == 2)
       {
        double dlt_dm_supply_by_veg = g.dlt_dm;

        g.dlt_dm_yield_demand_fruit = fruitPart->dm_yield_demand2 (dlt_dm_supply_by_veg);

        legnew_dm_partition2 (phenology->stageNumber()
                               , c.x_stage_no_partition
                               , c.y_frac_leaf
                               , c.num_stage_no_partition
                               , c.y_ratio_root_shoot
                               , c.sla_min
                               , dlt_dm_supply_by_veg
                               , g.dlt_dm_yield_demand_fruit
                               , &g.dlt_dm_supply_to_fruit
                               , g.dlt_dm_green);

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

    plantPartHack *rootPart = new plantPartHack(this, root, "root");

    vector<plantPart *> supply_pools_by_veg;
    supply_pools_by_veg.push_back(stemPart);
    supply_pools_by_veg.push_back(leafPart);

    vector<plantPart *> fruit_supply_pools;
    fruit_supply_pools.push_back(stemPart);
    fruit_supply_pools.push_back(leafPart);

    vector<plantPart *> allParts;
    allParts.push_back(rootPart);
    allParts.push_back(leafPart);
    allParts.push_back(stemPart);
    allParts.push_back(fruitPart);

    push_routine (my_name);

    if (option == 1)
        {
         float dm_demand_differential = g.dlt_dm_yield_demand_fruit
                                      - g.dlt_dm_supply_to_fruit;
         legnew_dm_retranslocate(allParts
                                    , supply_pools_by_veg
                                    , dm_demand_differential
                                    , g.plants
                                    , &g.dlt_dm_retrans_to_fruit);
//         g.dlt_dm_supply_to_fruit += g.dlt_dm_retrans_to_fruit;                 //FIXMME? when fruit made into proper class

        }
    else if (option == 2)
        {
         float dm_demand_differential = g.dlt_dm_yield_demand_fruit
                                      - g.dlt_dm_supply_to_fruit;
         legnew_dm_retranslocate(allParts
                                    , supply_pools_by_veg
                                    , dm_demand_differential
                                    , g.plants
                                    , &g.dlt_dm_retrans_to_fruit);
        }
    else
        {
        throw std::invalid_argument("invalid template option in plant_bio_retrans");
        }

    pop_routine (my_name);
    delete rootPart;
    }

//     ===========================================================
void Plant::plant_bio_distribute (int option /* (INPUT) option number */)
//     ===========================================================
{
//+  Purpose
//       distribute biomass to fruit parts.


//- Implementation Section ----------------------------------
    if (option == 1)
    {
          fruitPart->dm_partition1 ( g.dlt_dm_supply_to_fruit);    // this may need to be redone when fruit becomes true class
          fruitPart->dm_retranslocate1( g.dlt_dm_retrans_to_fruit);    // this may need to be redone when fruit becomes true class
    }
    else if (option == 2)
    {          // do nothing                //FIXME do we need this code?
        float dlt_dm_green_fruit[max_part];
        fruitPart->dm_partition2 ( g.dlt_dm_supply_to_fruit);    // this may need to be redone when fruit becomes true class
        fruitPart->dm_retranslocate2( g.dlt_dm_retrans_to_fruit);    // this may need to be redone when fruit becomes true class

    }
    else
    {
        throw std::invalid_argument("invalid template option in plant_bio_distribute");
    }

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

//- Implementation Section ----------------------------------

    push_routine (my_name);

    if (option == 1)
        {

        crop_swdef_photo( max_layer, g.dlayer, g.root_depth,
                             g.sw_demand, g.dlt_sw_dep, &g.swdef_photo);
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
        crop_swdef_photo(max_layer, g.dlayer, g.root_depth,
                             g.sw_demand, g.dlt_sw_dep, &g.swdef_photo);

        crop_swdef_pheno(c.num_sw_avail_ratio,
                         c.x_sw_avail_ratio, c.y_swdef_pheno, max_layer,g.dlayer,
                         g.root_depth, g.sw_avail, g.sw_avail_pot, &g.swdef_pheno);

        crop_swdef_pheno(c.num_sw_avail_ratio_flower,
                         c.x_sw_avail_ratio_flower, c.y_swdef_pheno_flower, max_layer,g.dlayer,
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
          float dltDmPotTeVeg = 0.0;
        fruitPart->bio_water1 ();
        plant_bio_water1 (g.swSupplyVeg, g.transp_eff, &dltDmPotTeVeg);
        g.dlt_dm_pot_te = dltDmPotTeVeg + fruitPart->dltDmPotTe();

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
        plant_dm_init ( c.dm_init[root]
                       , g.plants
                       , g.dm_green
                       , g.dm_plant_min);
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
        fruitPart->yieldpart_demand_stress1 ();
        }
    else
        {
        throw std::invalid_argument ( "invalid template option in bio_grain_demand_stress");
        }

    pop_routine (my_name);
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
        cproc_lai_detachment1 (leafPart->c.sen_detach_frac
                               , leafPart->gSLAI
                               , &g.dlt_slai_detached
                               , leafPart->c.dead_detach_frac
                               , leafPart->gTLAI_dead
                               , &leafPart->dltTLAI_dead_detached);

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

        for (vector<plantPart *>::iterator t = myParts.begin();
             t != myParts.end();
             t++)
           (*t)->dm_detachment1();

        cproc_n_detachment1 ( max_part
                             , c.sen_detach_frac
                             , g.n_senesced
                             , g.dlt_n_detached
                             , c.dead_detach_frac
                             , g.n_dead
                             , g.dlt_n_dead_detached);
        for (vector<plantPart *>::iterator t = myParts.begin();
             t != myParts.end();
             t++)
           {
           (*t)->n_detachment1();
           //(*t)->p_detachment1();
           }
        }
    else
        {
        throw std::invalid_argument ("invalid template option in detachment");
        }

    detachment_p();
    pop_routine (my_name);
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
        if (phenology->inPhase("sowing"))
           g.dlt_plants_failure_germ =
                  crop_failure_germination (parent,
                                            c.days_germ_limit,
                                            phenology->daysInCurrentPhase(),
                                            g.plants);
        else
           g.dlt_plants_failure_germ = 0.0;

        if (phenology->inPhase("germination"))
           g.dlt_plants_failure_emergence =
                  crop_failure_emergence (parent,
                                          c.tt_emerg_limit,
                                          phenology->ttInCurrentPhase(),
                                          g.plants);
        else
           g.dlt_plants_failure_emergence = 0.0;

        g.dlt_plants_death_seedling = 0.0;
        if (phenology->inPhase("emergence"))
           {
           int days_after_emerg = phenology->daysInCurrentPhase();//XZZZZ Can do this on emergence day?
           if (days_after_emerg == 1)
              {
              g.dlt_plants_death_seedling =
                  plant_death_seedling(c.num_weighted_temp
                                     , c.x_weighted_temp
                                     , c.y_plant_death
                                     , g.day_of_year
                                     , g.soil_temp
                                     , g.year
                                     , g.plants);
              }
           }
        /*XXXX this needs tou be coupled with dlt_leaf_area_sen, c_sen_start_stage  FIXME*/
//        if (phenology->inPhase("leaf_senescence"))
        if (phenology->inPhase("above_ground"))
           g.dlt_plants_failure_leaf_sen =
                  crop_failure_leaf_sen(parent, leafPart->gLAI, g.plants);
        else
           g.dlt_plants_failure_leaf_sen = 0.0;

        if (phenology->inPhase("preflowering"))
           g.dlt_plants_failure_phen_delay =
                  crop_failure_phen_delay(parent
                                         , c.swdf_pheno_limit
                                         , g.cswd_pheno.getSum()
                                         , g.plants);
        else
           g.dlt_plants_failure_phen_delay = 0.0;

        if (phenology->inPhase("preflowering"))   // XX NEW - check pls.
           g.dlt_plants_death_drought =
              plant_death_drought(c.leaf_no_crit
                                 , c.swdf_photo_limit
                                 , c.swdf_photo_rate
                                 , g.cswd_photo.getSum()
                                 , g.leaf_no
                                 , g.plants
                                 , g.swdef_photo);
        else
           g.dlt_plants_death_drought = 0.0;

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
//      Determine plant seedling death.

//+  Mission Statement
//     Determine plant seeding death
float Plant::plant_death_seedling
    (
     int    c_num_weighted_temp      // (INPUT)  size of table
    ,float  *c_x_weighted_temp        // (INPUT)  temperature table for poor est
    ,float  *c_y_plant_death          // (INPUT)  index of plant death
    ,int    g_day_of_year            // (INPUT)  day of year
    ,float  *g_soil_temp              // (INPUT)  soil surface temperature (oC)
    ,int    g_year                   // (INPUT)  year
    ,float  g_plants                 // (INPUT)  Plant density (plants/m^2)
    ) {
    float killfr;                                 // fraction of crop population to kill

    // code to kill plants for high soil surface temperatures
    plant_plants_temp(c_num_weighted_temp
                          , c_x_weighted_temp
                          , c_y_plant_death
                          , g_day_of_year
                          , g_soil_temp
                          , g_year
                          , &killfr);

    float dlt_plants = - g_plants*killfr;

    if (killfr > 0.0)
       {
       string msg= "Plant kill. ";
         msg = msg + ftoa(killfr*fract2pcnt, ".2").c_str();
         msg = msg + "% failure because of high soil surface temperatures.";
       parent->writeString (msg.c_str());
       }
    return dlt_plants;
    }


//+  Purpose
//      Determine plant death from drought.

//+  Mission Statement
//     Determine plant death from drought

//+  Changes
//       290994 jngh specified and programmed
//       110695 psc  added plant death from high soil temp
//       100795 jngh moved plant_kill crop to end of routine
float Plant::plant_death_drought
    (
     float  c_leaf_no_crit              // (INPUT)  critical number of leaves belo
    ,float  c_swdf_photo_limit          // (INPUT)  critical cumulative photosynth
    ,float  c_swdf_photo_rate           // (INPUT)  rate of plant reduction with p
    ,float  cswd_photo                 // (INPUT)  cumulative water stress type 1
    ,float  *g_leaf_no                  // (INPUT)  number of fully expanded leave
    ,float  g_plants                    // (INPUT)  Plant density (plants/m^2)
    ,float  g_swdef_photo               // (INPUT)
    ) {
    float leaf_no;                                // number of leaves
    float killfr;                                 // fraction of crop population to kill
    float dlt_plants = 0.0;                       // population to kill

    leaf_no = sum_real_array (g_leaf_no, max_node);

    if (leaf_no<c_leaf_no_crit
        && cswd_photo>c_swdf_photo_limit
        && g_swdef_photo<1.0)
        {
        killfr = c_swdf_photo_rate * (cswd_photo - c_swdf_photo_limit);
        killfr = bound (killfr, 0.0, 1.0);
        dlt_plants = - g_plants*killfr;

        string msg= "Plant kill. ";
          msg = msg + ftoa(killfr*fract2pcnt, ".2").c_str();
          msg = msg + "% failure because of water stress.";
        parent->writeString (msg.c_str());
        }
    return dlt_plants;
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
          msg = msg + ftoa(killfr*fract2pcnt, ".2").c_str();
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

        biomass = (topsGreen()+topsSenesced()+topsDead()) * gm2kg /sm2ha;

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
        leafPart->dltLAI_pot =
           cproc_leaf_area_pot1 (c.x_node_no
                              , c.y_leaf_size
                              , c.num_node_no
                              , g.node_no
                              , c.node_no_correction
                              , g.dlt_leaf_no_pot
                              , g.plants);
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
        leafPart->dltLAI_stressed =
           cproc_leaf_area_stressed1 (leafPart->dltLAI_pot ,g.swdef_expansion
                                     ,min(g.nfact_expansion, g.pfact_expansion));
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
        if (phenology->on_day_of("emergence"))
           legopt_leaf_area_init1 (c.initial_tpla
                                 , c.leaf_no_at_emerg
                                 , g.plants
                                 , &leafPart->gLAI
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
        if (phenology->on_day_of("emergence"))
            legopt_leaf_no_init1(c.leaf_no_at_emerg
                                 , g.leaf_no
                                 , &g.node_no);
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
                                 , leafPart->dlt.dm_green
                                 , &leafPart->dltLAI
                                 , leafPart->dltLAI_stressed
                                 , leafPart->gLAI);
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
void Plant::plant_pod_area (int option /* (INPUT) option number*/)     // FIXME - remove this when fruit becomes proper class
    {
//+  Constant Values
    const char*  my_name = "plant_pod_area" ;

//- Implementation Section ----------------------------------

    push_routine (my_name);

    if (option == 1)
           {
        fruitPart->calcDlt_pod_area ();
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
                             , leafPart->dltLAI
                             , leafPart->dltLAI_stressed
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
                           , phenology->inPhase("node_formation")
                           , phenology->on_day_of("emergence")
                           , g.node_no
                           , phenology->get_dlt_tt()
                           , &g.dlt_leaf_no_pot
                           , &g.dlt_node_no_pot);
        }
    else if (option == 2)
        {
        //wheat
        float tiller_no_now =  g.node_no;
        cproc_leaf_no_pot3  (c.x_node_no_app
                             , c.y_node_app_rate
                             , c.num_node_no_app
                             , c.x_node_no_leaf
                             , c.y_leaves_per_node
                             , c.num_node_no_leaf
                             , phenology->inPhase("tiller_formation")
                             , phenology->on_day_of("emergence")
                             , tiller_no_now
                             , phenology->get_dlt_tt()
                             , min(g.nfact_expansion, g.pfact_expansion)
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
        if (phenology->inPhase("grainfill"))
            fruitPart->n_conc_grain_limits();

        if (phenology->on_day_of("emergence"))
           {
           cproc_n_init1(c.n_init_conc
                        , max_part
                        , g.dm_green
                        , g.n_green);
           if (g.phosphorus_aware)
              cproc_n_init1(c.p_conc_init
                            , max_part
                            , g.dm_green
                            , g.p_green);
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
        biomass = topsGreen() + g.dlt_dm;

        cproc_n_supply1 (g.dlayer
                         , g.dlt_sw_dep
                         , g.no3gsm
                         , g.no3gsm_min
                         , g.root_depth
                         , g.sw_dep
                         , g.no3gsm_mflow_avail
                         , g.sw_avail
                         , g.no3gsm_diffn_pot
                         , phenology->stageNumber()
                         , c.n_fix_rate
                         , biomass
                         , g.swdef_fixation
                         , &g.n_fix_pot);

        }
    else if (option == 2)
        {
        biomass = topsGreen() + g.dlt_dm;

        cproc_n_supply3 (g.dlayer
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
                         , phenology->stageNumber()
                         , c.n_fix_rate
                         , biomass
                         , g.swdef_fixation
                         , &g.n_fix_pot);
        }
     else if (option == 3)
        {
        biomass = topsGreen()  + g.dlt_dm;

        cproc_n_supply4 (g.dlayer
                             , g.bd
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
                             , phenology->stageNumber()
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
        legnew_n_retranslocate(fruitPart->grainNDemand());
//         float dlt_n_retrans_fruit[max_part];
//         fruitPart->n_retranslocate (g.n_conc_min
//                               , g.dm_green
//                               , g.n_green
//                               , g.grain_n_demand
//                               , dlt_n_retrans_fruit        //FIXME change to g. when proper fruit class
//                               );
//
//         legnew_n_retranslocate_test(supply_pools_by_veg
//                                   , num_supply_pools_by_veg
//                                   , g.n_conc_min
//                                   , g.dm_green
//                                   , g.n_green
//                                   , g.grain_n_demand
//                                   , g.grain_n_supply
//                                   , g.dlt_n_retrans
//                                   ) ;

        }
    else if (option == 2)
        {
        legnew_n_retranslocate( fruitPart->nGrainDemand2());  //FIXME
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
        if (phenology->inPhase("grainfill"))
            fruitPart->grain_n_demand1(g.nfact_grain_conc
                                       , g.swdef_expansion);
      }
   else if (Option == 2)
      {
       // start grain n filling immediately after flowering
      fruitPart->grain_n_demand2();
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
    const int  num_demand_parts = 1 ;
    const char*  my_name = "plant_nit_demand" ;

//+  Local Variables
    int   demand_parts[num_demand_parts] = {root};

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
        for (vector<plantPart *>::iterator t = myParts.begin();
             t != myParts.end();
             t++)
           (*t)->doNDemand1(g.dlt_dm, g.dlt_dm_pot_rue);
        }
     else if (option == 2)
         {
         plant_n_demand(max_part
                        , demand_parts
                        , num_demand_parts
                        , g.dlt_dm
                        , g.dlt_dm_green
                        , g.dlt_dm_pot_rue
                        , g.dm_green
                        , g.n_conc_crit
                        , g.n_conc_max
                        , g.n_green
                        , fruitPart->grainNDemand()
                        , c.n_deficit_uptake_fraction
                        , g.n_demand
                        , g.n_max);
         for (vector<plantPart *>::iterator t = myParts.begin();
              t != myParts.end();
              t++)
            (*t)->doNDemand2(g.dlt_dm, g.dlt_dm_pot_rue);
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
   for (vector<plantPart *>::iterator t = myParts.begin();
        t != myParts.end();
        t++)
      (*t)->doSoilNDemand();
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
                             ,g.dlt_no3gsm);


        }
    else if (option == 1)
        {
        cproc_n_uptake1(c.no3_diffn_const
                       , g.dlayer
                       , g.no3gsm_diffn_pot
                       , g.no3gsm_mflow_avail
                       , g.n_fix_pot
                       , c.n_supply_preference.c_str()
                       , sumNDemand()
                       , sumNMax()
                       , g.root_depth
                       , g.dlt_no3gsm);
        }
    else if ((option == 2) || (option == 3))
        {
        cproc_n_uptake3(g.dlayer
                        , g.no3gsm_uptake_pot
                        , g.nh4gsm_uptake_pot
                        , g.n_fix_pot
                        , c.n_supply_preference.c_str()
                        , sumSoilNDemand()
                        , sumNMax()
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
//     Calculate the nitrogen and phosporous partitioning in the plant

//+  Changes
//      250894 jngh specified and programmed
void Plant::plant_nit_partition (int option /* (INPUT) option number*/)
    {
    if (option == 1)
        {
    vector<plantPart *> allParts;
    setupHacks(allParts);

        legnew_n_partition(g.dlayer
                           , g.dlt_no3gsm
                           , g.dlt_nh4gsm
                           , g.soil_n_demand
                           , g.n_fix_pot
                           , g.n_max
                           , g.root_depth
                           , g.dlt_n_green
                           , &g.n_fix_uptake
                           , allParts);

         deleteHacks(allParts);
        }
    else
        {
        throw std::invalid_argument ("invalid template option");
        }

    if (g.phosphorus_aware)
    {
       vector<plantPart *> allParts;
       setupHacks(allParts);
       PlantP_partition(allParts);

    deleteHacks(allParts);
    }


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
        vector<const plantPart *> parts;

        // Expansion uses leaves only
        parts.push_back(leafPart);
        g.nfact_expansion = critNFactor(parts, c.n_fact_expansion);

        // Rest have leaf & stem
        parts.push_back(stemPart);
        g.nfact_pheno = critNFactor(parts, c.n_fact_pheno);
        g.nfact_photo = critNFactor(parts, c.n_fact_photo);
        g.nfact_grain_conc = critNFactor(parts, 1.0);
        }
    else if (option == 2)
        {
        vector<const plantPart *> parts;

        // Expansion & photosynthesis from leaves only
        parts.push_back(leafPart);
        g.nfact_expansion = critNFactor(parts, c.n_fact_expansion);
        g.nfact_photo = critNFactor(parts, c.n_fact_photo);

        // leaf & stem
        parts.push_back(stemPart);
        g.nfact_pheno = critNFactor(parts, c.n_fact_pheno);
        g.nfact_grain_conc = critNFactor(parts, 1.0);
        }
    else
        {
        throw std::invalid_argument ("invalid template option in plant_nit_stress");
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
    const int  num_demand_parts = 1 ;
    int   demand_parts[num_demand_parts]={root};

//- Implementation Section ----------------------------------
    push_routine (my_name);

    if (option == 1)
        {
// Option 1 is to assume that the distribution of plant
// C will be similar after today and so N demand is that
// required to raise all plant parts to max N conc.

        // calculate potential new shoot and root growth
        dm_green_tot = plantGreen();

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
                        , g.n_demand
                        , g.n_max);

        for (vector<plantPart *>::iterator t = myParts.begin();
             t != myParts.end();
             t++)
           {
           (*t)->doNDemand1Pot(g.dlt_dm_pot_rue, g.dlt_dm_pot_rue);
           }

        g.ext_n_demand = sumNDemand();

        //nh  use zero growth value here so that estimated n fix is always <= actual;
        biomass = topsGreen();
        crop_n_fixation_pot1(phenology->stageNumber()
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
        canopy_sen_fr = divide (leafPart->dltSLAI, leafPart->gLAI + leafPart->dltLAI, 0.0);

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
        for (vector<plantPart *>::iterator t = myParts.begin();
             t != myParts.end();
             t++)
           (*t)->doSenescence1(canopy_sen_fr);

         }
    else if (dm_senescence_option == 2)
         {
         //XX this arm is redundant - not used by any ini files..????
         canopy_sen_fr = divide (leafPart->dltSLAI, leafPart->gLAI +leafPart->dltLAI, 0.0);
         plant_dm_senescence (max_part
                              , max_table
                              , canopy_sen_fr
                              , c.x_dm_sen_frac
                              , c.y_dm_sen_frac
                              , c.num_dm_sen_frac
                              , g.dm_green
                              , g.dlt_dm_senesced);
        for (vector<plantPart *>::iterator t = myParts.begin();
             t != myParts.end();
             t++)
           (*t)->doSenescence2(canopy_sen_fr);

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

        for (vector<plantPart *>::iterator t = myParts.begin();
             t != myParts.end();
             t++)
           (*t)->doNSenescence();

        }
    else if (option == 2)
        {
        plant_N_senescence (max_part       // ok: N senescence is called later
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

    if (g.phosphorus_aware == true)
       {
       vector<plantPart*> allParts;
       setupHacks(allParts);
       PlantP_senescence(allParts);
       deleteHacks(allParts);
       }

    pop_routine (my_name);
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
                              , min(g.nfact_expansion, g.pfact_expansion)
                              , c.n_fact_lf_sen_rate
                              , phenology->get_dlt_tt()
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
        legopt_leaf_area_sen1  ( leafPart->dltLAI_stressed
                                , g.dlt_leaf_no
                                , g.dlt_leaf_no_dead
                                , leafPart->gLAI
                                , g.leaf_area
                                , g.leaf_no
                                , g.leaf_no_dead
                                , max_node
                                , g.plants
                                , leafPart->gSLAI
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
                                , &leafPart->dltSLAI );
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
    g.remove_biom_pheno = 1.0;

    plant_update(c.n_conc_crit_root
                , c.n_conc_max_root
                , c.n_conc_min_root
                , c.x_stage_code
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
                , &g.canopy_width
                , &g.cover_dead
                , &g.cover_green
                , &g.cover_sen
                , g.dlt_dm
                , g.dlt_dm_dead_detached
                , g.dlt_dm_detached
                , g.dlt_dm_green
                , g.dlt_dm_green_retrans
                , g.dlt_dm_senesced
                , g.dlt_dm_green_dead
                , g.dlt_dm_senesced_dead
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
                , g.dlt_n_green_dead
                , g.dlt_n_senesced_dead
                , g.dlt_plants
                , g.dlt_root_depth
                , g.dlt_slai_detached
                , g.dm_dead
                , g.dm_green
                , g.dm_senesced
                , &g.lai_canopy_green
                , g.leaf_area
                , g.leaf_no
                , &g.node_no
                , g.leaf_no_dead
                , g.n_conc_crit
                , g.n_conc_max
                , g.n_conc_min
                , g.n_dead
                , g.n_green
                , g.n_senesced
                , &g.plants
                , &g.root_depth
                , g.swdef_pheno
                , g.dlt_root_length_dead
                , g.root_length_dead
                , g.root_length
                , g.dlt_root_length
                , g.dlt_root_length_senesced);

    plant_check_bounds(g.cover_dead
                       , g.cover_green
                       , g.cover_sen
                       , g.dlayer
                       , g.dm_dead
                       , g.dm_green
                       , g.dm_senesced
                       , g.leaf_area
                       , g.leaf_no
                       , g.leaf_no_dead
                       , g.n_conc_crit
                       , g.n_conc_max
                       , g.n_conc_min
                       , g.n_dead
                       , g.n_green
                       , g.n_senesced
                       , g.plants
                       , g.root_depth);
    plant_totals(g.day_of_year
                , g.dlayer
                , g.dlt_n_retrans
                , g.dlt_sw_dep
                , g.dm_green
                , &g.lai_max
                , &g.n_conc_act_stover_tot
                , g.n_conc_crit
                , &g.n_conc_crit_stover_tot
                , g.n_dead
                , g.n_demand
                , &g.n_demand_tot
                , g.n_green
                , g.n_senesced
                , &g.n_uptake_stover_tot
                , &g.n_uptake_tot
                , g.dlt_n_green
                , &g.n_fix_uptake
                , &g.n_fixed_tops
                , &g.root_depth
                , &g.transpiration_tot );

    phenology->update();

    if (g.plant_status == alive &&
        phenology->previousStageName() != phenology->stageName())
        {
        plant_event (g.dlayer
                    , g.dm_dead
                    , g.dm_green
                    , g.dm_senesced
                    , g.n_green
                    , g.root_depth
                    , g.sw_dep
                    , p.ll_dep);
        if (phenology->inPhase("stress_reporting")) {
            float si1 = divide (g.cswd_photo.getSum(),
                                phenology->daysInPhase(phenology->previousStageName()), 0.0);
            float si2 = divide (g.cswd_expansion.getSum(),
                                phenology->daysInPhase(phenology->previousStageName()), 0.0);
            float si4 = divide (g.cnd_photo.getSum(),
                                phenology->daysInPhase(phenology->previousStageName()), 0.0);
            float si5 = divide (g.cnd_grain_conc.getSum(),
                                phenology->daysInPhase(phenology->previousStageName()), 0.0);
            char msg[1024];
            sprintf (msg,"%4s%-20s%s%-23s%6.3f%13.3f%13.3f%13.3f\n", " ",
                      phenology->previousStageName().c_str(),
                      " to ",
                      phenology->stageName().c_str(), si1, si2, si4, si5);
            g.averageStressMessage += msg;
        }
        stageObservers.reset();

        }

    plant_check_leaf_record();

    pop_routine (my_name);
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

    if (! reals_are_equal (leaf_area_tot, leafPart->gLAI + leafPart->gSLAI, tolerance_lai))
      {
      ostrstream msg;
      msg << "Bad record for total leaf area. Leaf area total = ";
      msg <<  leaf_area_tot << ". Lai total = " <<  (leafPart->gLAI + leafPart->gSLAI) << ends;
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
void Plant::plant_update(
                          float  c_n_conc_crit_root                     // (INPUT)  critical N concentration of ro
                         ,float  c_n_conc_max_root                      // (INPUT)  maximum N concentration of roo
                         ,float  c_n_conc_min_root                      // (INPUT)  minimum N concentration of roo
                         ,float *c_x_stage_code                         // (INPUT)  stage table for N concentratio
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
                         ,float  *g_canopy_width                               // (INPUT)  canopy width (mm)
                         ,float  *g_cover_dead                                 // (out/INPUT)  fraction of radiation reaching
                         ,float  *g_cover_green                                // (out/INPUT)  fraction of radiation reaching
                         ,float  *g_cover_sen                                  // (out/INPUT)  fraction of radiation reaching
                         ,float  g_dlt_dm                                           // (INPUT)  the daily biomass production (
                         ,float *g_dlt_dm_dead_detached                             // (INPUT)  plant biomass detached fro
                         ,float *g_dlt_dm_detached                                  // (INPUT)  plant biomass detached (g/m^2)
                         ,float *g_dlt_dm_green                                     // (INPUT)  plant biomass growth (g/m^2)
                         ,float *g_dlt_dm_green_retrans                             // (INPUT)  plant biomass retranslocat
                         ,float *g_dlt_dm_senesced                                  // (INPUT)  plant biomass senescence (g/m^
                         ,float *g_dlt_dm_green_dead                                  // (INPUT)  plant biomass senescence (g/m^
                         ,float *g_dlt_dm_senesced_dead                                  // (INPUT)  plant biomass senescence (g/m^
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
                         ,float *g_dlt_n_green_dead                                 // (INPUT)  plant N death (g/m^2)
                         ,float *g_dlt_n_senesced_dead                              // (INPUT)  plant N death (g/m^2)
                         ,float  g_dlt_plants                                       // (INPUT)  change in Plant density (plant
                         ,float  g_dlt_root_depth                                   // (INPUT)  increase in root depth (mm)
                         ,float  g_dlt_slai_detached                                // (INPUT)  plant senesced lai detached
                         ,float *g_dm_dead                                          // (INPUT)  dry wt of dead plants (g/m^2)
                         ,float *g_dm_green                                         // (INPUT)  live plant dry weight (biomass
                         ,float *g_dm_senesced                                      // (INPUT)  senesced plant dry wt (g/m^2)
                         ,float *g_lai_canopy_green                                 // (out/INPUT)  live plant green lai in canopy
                         ,float *g_leaf_area                                        // (INPUT)  leaf area of each leaf (mm^2)
                         ,float *g_leaf_no                                          // (INPUT)  number of fully expanded leave
                         ,float *g_node_no                                          // (INPUT)  number of fully expanded nodes
                         ,float *g_leaf_no_dead                                     // (INPUT)  no of dead leaves ()
                         ,float *g_n_conc_crit                                      // (out/INPUT)  critical N concentration (g N/
                         ,float *g_n_conc_max                                       // (out/INPUT)  maximum N concentration (g N/g
                         ,float *g_n_conc_min                                       // (out/INPUT)  minimum N concentration (g N/g
                         ,float *g_n_dead                                           // (INPUT)  plant N content of dead plants
                         ,float *g_n_green                                          // (INPUT)  plant nitrogen content (g N/m^
                         ,float *g_n_senesced                                       // (INPUT)  plant N content of senesced pl
                         ,float *g_plants                                           // (out/INPUT)  Plant density (plants/m^2)
                         ,float *g_root_depth                                         // (out/INPUT)  depth of roots (mm)
                         ,float g_swdef_pheno                                        // (INPUT)
                         ,float *g_dlt_root_length_dead                                  // (INPUT)  Change in Root length of dead population in each layer
                         ,float *g_root_length_dead                                  // (INPUT)  Root length of dead population in each layer
                         ,float *g_root_length                                       // (INPUT)  Root length in each layer
                         ,float *g_dlt_root_length                                   // (INPUT)  Root growth in each layer
                         ,float *g_dlt_root_length_senesced)
{

//+  Constant Values
    const char*  my_name = "plant_update" ;

//+  Local Variables
    float dlt_leaf_area;                          // leaf area increase (mm^2/plant)
    float dlt_leaf_dm;                            // leaf dm increase (g/plant)
                                                  // (grains/m^2)
    float dlt_lai_dead;                           // lai of green leaf of plants dying ()
    float dlt_slai_dead;                          // lai of senesced leaf of plant dying ()
    float dlt_root_length_dead;                   // root length of plant dying ()
    double dying_fract_plants;                    // fraction op population dying (0-1)
    float node_no;                                // currently expanding node no.
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

    plantPartHack *rootPart = new plantPartHack(this, root, "root");

    vector<plantPart *> allParts;
    allParts.push_back(rootPart);
    allParts.push_back(leafPart);
    allParts.push_back(stemPart);
    allParts.push_back(fruitPart);

    vector<plantPart *> someParts;
    someParts.push_back(rootPart);
    someParts.push_back(leafPart);
    someParts.push_back(stemPart);

    vector<plantPart *> rootParts;
    rootParts.push_back(rootPart);

    vector<plantPart *>::iterator part;


// The following table describes the transfer of material that should
// take place
//                        POOLS
//                 green senesced  dead
// dlt_green         +                     (incoming only)
// dlt_retrans       +-
// dlt_senesced      -      +
// dlt_dead          -      -       +
// dlt_detached             -       -      (outgoing only)

    dying_fract_plants = getDyingFractionPlants();

    //Hmmm. Don't quite know where this should be.. For now, it doesn't do much (height)..
    for (vector<plantPart *>::iterator t = myParts.begin();
         t != myParts.end();
         t++)
       (*t)->update();

    // transfer N & P
    for (part = someParts.begin(); part != someParts.end(); part++)
       {
       (*part)->g.n_dead -= (*part)->dlt.n_dead_detached;
       (*part)->g.n_green += (*part)->dlt.n_green;
       (*part)->g.n_green += (*part)->dlt.n_retrans;
       (*part)->g.n_green -= (*part)->dlt.n_senesced;
       (*part)->g.n_senesced += (*part)->dlt.n_senesced;
       if (g.phosphorus_aware)
           {
           (*part)->g.p_green += (*part)->dlt.p_green;
           (*part)->g.p_green += (*part)->dlt.p_retrans;
           (*part)->g.p_green -= (*part)->dlt.p_sen;
           (*part)->g.p_green = l_bound((*part)->g.p_green, 0.0);  // Can occur at total leaf senescence. FIXME! XXXX
           }
       }

    // Let me register my surprise at how this is done on the next few lines
    // - why intrinsically limit processes to leaf etc right here!!! - NIH
    leafPart->g.n_green -= leafPart->dlt.n_senesced_trans;
    stemPart->g.n_green += leafPart->dlt.n_senesced_trans;

    float s = 0.0;
    for (part = allParts.begin(); part != allParts.end(); part++)
       s += (*part)->dlt.n_senesced_retrans;

    leafPart->g.n_green -= s;                               // xxpdev ?? i dont understand this??? all retranslocate to leaf?

    for (part = someParts.begin(); part != someParts.end(); part++)
       {
       (*part)->g.n_green += (*part)->dlt.n_senesced_retrans;
       (*part)->g.n_senesced -= (*part)->dlt.n_detached;
       (*part)->g.n_green = l_bound((*part)->g.n_green, 0.0);   // Can occur at total leaf senescence. FIXME! XXXX
       }

    for (part = someParts.begin(); part != someParts.end(); part++)
       {
       (*part)->dlt.n_green_dead = (*part)->g.n_green * dying_fract_plants;
       (*part)->g.n_green -= (*part)->dlt.n_green_dead;
       (*part)->g.n_dead += (*part)->dlt.n_green_dead;

       (*part)->dlt.n_senesced_dead = (*part)->g.n_senesced * dying_fract_plants;
       (*part)->g.n_senesced -= (*part)->dlt.n_senesced_dead;
       (*part)->g.n_dead += (*part)->dlt.n_senesced_dead;

       (*part)->g.dm_dead -= (*part)->dlt.dm_dead_detached;

       (*part)->g.dm_green += (*part)->dlt.dm_green;
       (*part)->g.dm_green += (*part)->dlt.dm_green_retrans;
       (*part)->g.dm_green -= (*part)->dlt.dm_senesced;

       (*part)->g.dm_senesced += (*part)->dlt.dm_senesced;
       (*part)->g.dm_senesced -= (*part)->dlt.dm_detached;

       (*part)->dlt.dm_green_dead = (*part)->g.dm_green * dying_fract_plants;
       (*part)->g.dm_green -=  (*part)->dlt.dm_green_dead;
       (*part)->g.dm_dead += (*part)->dlt.dm_green_dead;

       (*part)->dlt.dm_senesced_dead = (*part)->g.dm_senesced * dying_fract_plants;
       (*part)->g.dm_senesced -= (*part)->dlt.dm_senesced_dead;
       (*part)->g.dm_dead += (*part)->dlt.dm_senesced_dead;

       if (g.phosphorus_aware)
           {
           float dlt_p_green_dead = (*part)->g.p_green * dying_fract_plants;
           (*part)->g.p_green -= dlt_p_green_dead;
           (*part)->g.p_dead += dlt_p_green_dead;

           float dlt_p_senesced_dead = (*part)->g.p_sen * dying_fract_plants;
           (*part)->g.p_sen  -= dlt_p_senesced_dead;
           (*part)->g.p_dead += dlt_p_senesced_dead;
           }
       }

    delete rootPart;

    // transfer plant leaf area
    leafPart->gLAI +=  leafPart->dltLAI - leafPart->dltSLAI;
    leafPart->gSLAI += leafPart->dltSLAI - g_dlt_slai_detached;

    dlt_lai_dead  = leafPart->gLAI  * dying_fract_plants;
    dlt_slai_dead = leafPart->gSLAI * dying_fract_plants;
    leafPart->gLAI -=  dlt_lai_dead;
    leafPart->gSLAI -=  dlt_slai_dead;
    leafPart->gTLAI_dead +=  dlt_lai_dead + dlt_slai_dead - leafPart->dltTLAI_dead_detached;


    if (*g_canopy_width > 0.0)
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
    float cover_green_leaf;
    legnew_cover(g_row_spacing
                          ,c_x_row_spacing
                          ,c_y_extinct_coef
                          ,c_num_row_spacing
                          , canopy_fac
                          ,leafPart->gLAI
                          ,&cover_green_leaf);

    float cover_pod = fruitPart->calcCover(canopy_fac);
    *g_cover_green = add_covers (cover_green_leaf, cover_pod);

    legnew_cover(g_row_spacing
                ,c_x_row_spacing
                ,c_y_extinct_coef_dead
                ,c_num_row_spacing
                , canopy_fac
                ,leafPart->gSLAI
                ,g_cover_sen);

    legnew_cover(g_row_spacing
                 ,c_x_row_spacing
                 ,c_y_extinct_coef_dead
                 ,c_num_row_spacing
                 , canopy_fac
                 ,leafPart->gTLAI_dead
                 ,g_cover_dead);

    // plant leaf development
    // need to account for truncation of partially developed leaf (add 1)
    node_no = 1.0 + *g_node_no;

    dlt_leaf_area = divide (leafPart->dltLAI, *g_plants, 0.0) * sm2smm;
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

    accumulate (g_dlt_leaf_no, g_leaf_no, node_no-1.0, g_dlt_node_no);

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
    *g_node_no += g_dlt_node_no;

    // plant stress observers
    stageObservers.update();
    if (phenology->inPhase("preflowering")) g.cswd_pheno.update();

    // other plant states
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

    plant_n_conc_limits(c_n_conc_crit_root
                        , c_n_conc_max_root
                        , c_n_conc_min_root
                        , g.co2_modifier_n_conc
                        , g_n_conc_crit
                        , g_n_conc_max
                        , g_n_conc_min);

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
    (float  g_cover_dead                        // (INPUT)  fraction of radiation reaching
    ,float  g_cover_green                       // (INPUT)  fraction of radiation reaching
    ,float  g_cover_sen                         // (INPUT)  fraction of radiation reaching
    ,float *g_dlayer                            // (INPUT)  thickness of soil layer I (mm)
    ,float *g_dm_dead                           // (INPUT)  dry wt of dead plants (g/m^2)
    ,float *g_dm_green                          // (INPUT)  live plant dry weight (biomass
    ,float *g_dm_senesced                       // (INPUT)  senesced plant dry wt (g/m^2)
    ,float *g_leaf_area                         // (INPUT)  leaf area of each leaf (mm^2)
    ,float *g_leaf_no                           // (INPUT)  number of fully expanded leave
    ,float *g_leaf_no_dead                      // (INPUT)  no of dead leaves ()
    ,float *g_n_conc_crit                       // (INPUT)  critical N concentration (g N/
    ,float *g_n_conc_max                        // (INPUT)  maximum N concentration (g N/g
    ,float *g_n_conc_min                        // (INPUT)  minimum N concentration (g N/g
    ,float *g_n_dead                            // (INPUT)  plant N content of dead plants
    ,float *g_n_green                           // (INPUT)  plant nitrogen content (g N/m^
    ,float *g_n_senesced                        // (INPUT)  plant N content of senesced pl
    ,float  g_plants                            // (INPUT)  Plant density (plants/m^2)
    ,float  g_root_depth                        // (INPUT)  depth of roots (mm)
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

    vector<plantPart *> allParts;
    setupHacks(allParts);
    for (vector<plantPart *>::iterator t = myParts.begin(); t != myParts.end(); t++)
       {
       (*t)->checkBounds();
       }
    deleteHacks(allParts);

    pop_routine (my_name);
    }


//+  Purpose
//         Collect totals of crop variables for output

//+  Mission Statement
//     Collect totals of crop variables for output

//+  Changes
//     010994 jngh specified and programmed
void Plant::plant_totals
    (int   g_day_of_year                // (INPUT)  day of year
    ,float *g_dlayer                     // (INPUT)  thickness of soil layer I (mm)
    ,float *g_dlt_n_retrans              // (INPUT)  nitrogen retranslocated out from parts to grain (g/m^2)
    ,float *g_dlt_sw_dep                 // (INPUT)  water uptake in each layer (mm water)
    ,float *g_dm_green                   // (INPUT)  live plant dry weight (biomass) (g/m^2)
    ,float *g_lai_max                    // (INPUT)  maximum lai - occurs at flowering
    ,float  *g_n_conc_act_stover_tot           // (INPUT)  sum of tops actual N concentration (g N/g biomass)
    ,float  *g_n_conc_crit                     // (INPUT)  critical N concentration (g N/g biomass)
    ,float  *g_n_conc_crit_stover_tot          // (INPUT)  sum of tops critical N concentration (g N/g biomass)
    ,float  *g_n_dead                          // (INPUT)  plant N content of dead plants (g N/m^2)
    ,float  *g_n_demand                        // (INPUT)  critical plant nitrogen demand (g/m^2)
    ,float  *g_n_demand_tot                    // (out/INPUT)  sum of N demand since last output (g/m^2)
    ,float  *g_n_green                         // (INPUT)  plant nitrogen content (g N/m^2)
    ,float  *g_n_senesced                      // (INPUT)  plant N content of senesced plant (g N/m^2)
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
    float n_uptake_soil;                          // daily N taken up by roots (mineral + fixation)
    float n_uptake_soil_tops;                     // daily N taken up by roots going into tops

//- Implementation Section ----------------------------------

    push_routine (my_name);

// get totals
    n_conc_stover = divide (stoverNGreen(),stoverGreen() , 0.0);

    n_uptake = plantDltNRetrans();
    n_uptake_stover =  leafPart->dlt.n_retrans + stemPart->dlt.n_retrans;

// note - g_n_conc_crit should be done before the stages change

    n_conc_stover_crit = (leafPart->g.n_conc_crit + stemPart->g.n_conc_crit) * 0.5;
    n_green_demand = sumNDemand();

    deepest_layer = find_layer_no (*g_root_depth, g_dlayer, max_layer);

    if (phenology->on_day_of ("sowing"))
        {
        *g_n_uptake_tot = n_uptake;
        *g_transpiration_tot = - sum_real_array (g_dlt_sw_dep, deepest_layer+1);
        *g_n_conc_act_stover_tot = n_conc_stover;
        *g_n_conc_crit_stover_tot = n_conc_stover_crit;
        *g_n_demand_tot = n_green_demand;
        *g_n_uptake_stover_tot = n_uptake_stover;

        n_uptake_soil = plantDltNGreen();
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
//        n_uptake_soil = sum_real_array(g_dlt_n_green,max_part) + reproStruct->dlt.n_green + stemPart->dlt.n_green + leafPart->dlt.n_green;
        n_uptake_soil = plantDltNGreen();
        n_uptake_soil_tops = n_uptake_soil - g_dlt_n_green[root];
        *g_n_fixed_tops = *g_n_fixed_tops + n_uptake_soil_tops * divide (*g_n_fix_uptake ,n_uptake_soil ,0.0);

        }

    *g_lai_max = max (*g_lai_max, leafPart->gLAI);
// note - oil has no N, thus it is not included in calculations

    n_grain = fruitPart->nGrainTotal();


    *g_n_uptake_stover_tot = stoverNTot();
    *g_n_uptake_tot = n_grain + stoverNTot();

    pop_routine (my_name);
    return;
    }

//+  Purpose
//       Report occurence of event and the current status of specific
//       variables.
//       Called when a new phase has begun.

//+  Mission Statement
//     Report occurence of event and the current status of specific variables

//+  Changes
//     010994 jngh specified and programmed
void Plant::plant_event(float *g_dlayer           // (INPUT)  thickness of soil layer I (mm)
    ,float *g_dm_dead                     // (INPUT)  dry wt of dead plants (g/m^2)
    ,float *g_dm_green                    // (INPUT)  live plant dry weight (biomass
    ,float *g_dm_senesced                 // (INPUT)  senesced plant dry wt (g/m^2)
    ,float *g_n_green                     // (INPUT)  plant nitrogen content (g N/m^
    ,float  g_root_depth                  // (INPUT)  depth of roots (mm)
    ,float *g_sw_dep                      // (INPUT)  soil water content of layer L
    ,float *p_ll_dep)          // (INPUT)  lower limit of plant-extractab
    {
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

    // Don't send an end crop to the system - otherwise all the other crops will stop too!
    if (phenology->stageName() != "end_crop")
       sendStageMessage(phenology->stageName().c_str());

    // Tell all the plant things what has happened too (should do this one day ??xx??)
//    doPlantEvent(phenology->stageName());

    char msg[80];
    sprintf(msg, " stage %.1f %s"
              , phenology->stageNumber()
              , phenology->stageName().c_str());
    parent->writeString(msg);

    biomass = topsTot();

    // note - oil has no N, thus is not included in calculations
    dm_green = stoverGreen();
    n_green = stoverNGreen();

    n_green_conc_percent = divide (n_green, dm_green, 0.0) * fract2pcnt;

    deepest_layer = find_layer_no (g_root_depth, g_dlayer, max_layer);
    for (layer = 0; layer <= deepest_layer; layer++)
       {
       pesw[layer] = g_sw_dep[layer] - p_ll_dep[layer];
       pesw[layer] = l_bound (pesw[layer], 0.0);
       }
    pesw_tot = sum_real_array (pesw, deepest_layer+1);

    if (phenology->inPhase ("above_ground"))
            {
            char msg[256];
            sprintf(msg,
"                biomass =       %8.2f (g/m^2)   lai          = %7.3f (m^2/m^2)\n"
"                stover N conc = %8.2f (%%)    extractable sw = %7.2f (mm)",
                biomass, leafPart->gLAI, n_green_conc_percent, pesw_tot);
            parent->writeString (msg);
            }
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
    ,float  dlt_p_root                   // (INPUT) new root residue P (g/m^2)
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
                          ,dlt_p_root
                          ,g.dlayer
                          ,root_length
                          ,g.root_depth
                          ,c.crop_type.c_str());

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
        legopt_root_depth1 (g.dlayer
                            ,c.num_sw_ratio
                            ,c.x_sw_ratio
                            ,c.y_sw_fac_root
                            ,g.dul_dep
                            ,g.sw_dep
                            ,p.ll_dep
                            ,c.root_depth_rate
                            ,phenology->stageNumber()
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

        cproc_root_depth2 ( phenology->stageNumber()
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

        fruitPart->sw_demand1 (&g.swDemandTEFruit);

        float swDemandTEVeg = 0.0;                                        //
        float dltDmPotRueveg = g.dlt_dm_pot_rue - fruitPart->dltDmPotRuePod();     // FIXME when fruit is proper class
        cproc_sw_demand1 (dltDmPotRueveg,                                 //
                          g.transp_eff,                                   //
                          &swDemandTEVeg);                                //

        g.sw_demand_te = swDemandTEVeg + g.swDemandTEFruit;


        //g.sw_demand_te = g.sw_demand_te + g.dlt_dm_parasite_demand

        cproc_sw_demand_bound(g.sw_demand_te
                             ,p.eo_crop_factor
                              ,g.eo
                              ,g.cover_green
                              ,&g.sw_demand);

       // Capping of sw demand will create an effective TE- recalculate it here
       // In an ideal world this should NOT be changed here - NIH
       g.transp_eff = g.transp_eff * divide(g.sw_demand_te,g.sw_demand, 1.0);
       g.swDemandTEFruit = g.swDemandTEFruit * divide(g.sw_demand,g.sw_demand_te, 1.0);  // Hack to correct TE for fruit

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
                             ,ext_sw_supply);

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

//===========================================================================
void Plant::plant_water_distribute (int option /*(INPUT) option number*/)
//===========================================================================
{
//+  Purpose
//       light supply

//+  Mission Statement
//     Seek the light intercepted by the leaves

//+  Changes
///      250894 jngh specified and programmed

//+  Constant Values
    const char*  my_name = "plant_water_distribute" ;

//- Implementation Section ----------------------------------
    push_routine (my_name);

    if (option == 1)
    {
      float swDemandVeg = g.sw_demand - g.swDemandTEFruit;
      float swSupply = - sum_real_array(g.dlt_sw_dep, max_layer);

      plant_water_supply_partition (g.sw_demand
                                  , swDemandVeg
                                  , swSupply
                                  , &g.swSupplyVeg
                                  , &g.swSupplyFruit);

    }
    else
    {
        throw std::invalid_argument ("invalid template option");
    }

    pop_routine (my_name);
}

//===========================================================================
void Plant::plant_water_supply_partition(float gSwDemand
                                       , float gSwDemandVeg
                                       , float gSwSupply
                                       , float *gSwSupplyVeg
                                       , float *gSwSupplyFruit)
//===========================================================================
{

//+  Purpose
//       Partitions water uptake between plant components (g/m^2)

//+  Mission Statement
//     Partitions water between plant components

//+  Changes
//       010994 jngh specified and programmed

//+  Constant Values
    const char*  my_name = "plant_water_supply_partition" ;

//- Implementation Section ----------------------------------
    push_routine (my_name);

    *gSwSupplyVeg = gSwSupply * divide (gSwDemandVeg, gSwDemand, 0.0);
    *gSwSupplyFruit = gSwSupply - *gSwSupplyVeg;

    pop_routine (my_name);
}


//===========================================================================
void Plant::plant_light_supply_partition (int option /*(INPUT) option number*/)
//===========================================================================
{
//+  Purpose
//       light supply

//+  Mission Statement
//     Seek the light intercepted by the leaves

//+  Changes
//      5/9/96 dph

//+  Constant Values
    const char*  my_name = "plant_light_supply_partition" ;

//- Implementation Section ----------------------------------
    push_routine (my_name);

    if (option == 1)
    {
          float canopy_fac;
          if (g.canopy_width > 0.0)
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

             // calc the green fruit interception
          float frIntcRadnGreenFruit = g.fr_intc_radn * divide (fruitPart->calcCover(canopy_fac), g.cover_green, 0.0);
          crop_radn_int0(fruitPart->calcCover(canopy_fac)
                       , frIntcRadnGreenFruit
                       , g.radn
                       , &g.radnIntGreenFruit);

             // calc the total fruit interception - what is left is transmitted to the vegetative parts)
             // fruit is considered to be at top of canopy
          float radnIntTotFruit = 0.0;
          float frIntcRadnTotFruit = g.fr_intc_radn * divide (fruitPart->calcCover(canopy_fac), g.cover_green, 0.0);
          crop_radn_int0(fruitPart->calcCover(canopy_fac)
                       , frIntcRadnTotFruit
                       , g.radn
                       , &radnIntTotFruit);

             // calc the total interception
          float radnIntTot = 0.0;
          crop_radn_int0(g.cover_green
                       , g.fr_intc_radn
                       , g.radn
                       , &radnIntTot);

             // calc the green veg interception
          float radnIntGreenVeg = radnIntTot - radnIntTotFruit;

               // for now, put both interceptions into radn_int
          g.radn_int =  radnIntGreenVeg + g.radnIntGreenFruit;  // FIXME when turned into proper fruit class

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

        fruitPart->dm_pot_rue( g.radnIntGreenFruit);


        float radnIntGreenVeg = g.radn_int - g.radnIntGreenFruit;  //  FIXME temporary until proper fruit class

        float dlt_dm_pot_rue_veg = 0.0;
        plant_dm_pot_rue_veg(&c.rue
                           , radnIntGreenVeg
                           , min(min(min(g.temp_stress_photo, g.nfact_photo),
                               g.oxdef_photo), g.pfact_photo)
                           , &dlt_dm_pot_rue_veg);

        g.dlt_dm_pot_rue = dlt_dm_pot_rue_veg + fruitPart->dltDmPotRuePod();  // FIXME when fruit is made proper class
        }
    else
        {
        throw std::invalid_argument ("invalid template option");
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
void Plant::plant_dm_pot_rue_veg (externalFunction *c_rue
                                , double  radn_int
                                , double  stress_factor
                                , float  *dlt_dm_pot)                    // (OUTPUT) potential dry matter (carbohydrate) production (g/m^2)
  {
  //+  Local Variables
  double rue_leaf;

  rue_leaf = c_rue->value(phenology->stageNumber());

  *dlt_dm_pot = (radn_int * rue_leaf) * stress_factor * g.co2_modifier_rue;

//  fprintf(stdout, "%f,%f,%f\n", phenology->stageNumber(), stress_factor, *dlt_dm_pot);
  }

void Plant::plant_co2_modifier_rue(void)
{
  plant_rue_co2_modifier(c.photosynthetic_pathway,
                         g.co2,
                         g.maxt,
                         g.mint,
                         &g.co2_modifier_rue);
}

void Plant::plant_co2_modifier_te(void)
{
   g.co2_modifier_te = linear_interp_real (g.co2
                                         , c.x_co2_te_modifier
                                         , c.y_co2_te_modifier
                                         , c.num_co2_te_modifier);
}

void Plant::plant_co2_modifier_n_conc(void)
{
   g.co2_modifier_n_conc = linear_interp_real (g.co2
                                         , c.x_co2_nconc_modifier
                                         , c.y_co2_nconc_modifier
                                         , c.num_co2_nconc_modifier);
}

void Plant::plant_vpd (float c_svp_fract, float g_maxt, float g_mint)
{
   g.vpd = vpd(c_svp_fract, g_maxt, g_mint);
}


//==========================================================================
void Plant::plant_rue_co2_modifier(photosynthetic_pathway_t croptype, // Photosynthetic pathway
                                   float co2,                 //!CO2 level (ppm)
                                   float maxt,                //!daily max temp (C)
                                   float mint,                //!daily min temp (C)
                                   float *modifier)           //!modifier (-)
//==========================================================================
/*  Purpose
*     Calculation of the CO2 modification on rue
*
*     References
*     Reyenga, Howden, Meinke, Mckeon (1999), Modelling global change impact on wheat cropping in
*              south-east Queensland, Australia. Enivironmental Modelling & Software 14:297-306
*
*
*  Purpose
*     Calculation of the CO2 modification on rue
*
*  Changes
*     20000717   ew programmed
*/
   {
   //  Local Variables
      float temp;  //daily average temperature (C)
      float TT;    //co2 compensation point (ppm)
      float first;            // Temp vars for passing composite arg to a func
      float second;           // expecting a pointer

   // Implementation Section ----------------------------------

   switch (croptype)
      {
      pw_C3:
        {
        temp = 0.5*( maxt + mint);
        TT  = divide(163.0 - temp, 5.0 - 0.1 * temp, 0.0);

        first = (co2 - TT) * (350.0 + 2.0 * TT);
        second = (co2 + 2.0 * TT)*(350.0 - TT);
        *modifier = divide( first, second, 1.0);
        break;
        }
      pw_C4:
        {
        *modifier = 0.000143 * co2 + 0.95; //Mark Howden, personal communication
        break;
        }
      default:
        {
        throw std::invalid_argument ("Unknown photosynthetic pathway in cproc_rue_co2_modifier()");
        }
      }
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
        float te_coeff = c.transp_eff_cf[(int)phenology->stageNumber()-1];

        fruitPart->transp_eff_co2();

        cproc_transp_eff_co2(c.svp_fract, te_coeff,
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
                                      , g.dlt_root_length_senesced);
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
    if (option == 1)
        {
        if (phenology->on_day_of ("germination"))
          g.root_depth = c.initial_root_depth;
        }
    else
        throw std::invalid_argument ("invalid template option");

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
                                  , p.ll_dep);
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
                                   , p.ll_dep);
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
        if (phenology->on_day_of("emergence"))
        	  {
           cproc_root_length_init1(g.dm_green[root]
                                ,c.specific_root_length
                                ,g.root_depth
                                ,g.dlayer
                                ,g.root_length);
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
//       Initialise plant weights and plant weight minimums
//       at required instances.

//+  Mission Statement
//     Initialise plant weights and plant weight minimums at required instances.

//+  Changes
//     010994 jngh specified and programmed
void Plant::plant_dm_init (
        float  c_dm_root_init                            // (INPUT)  root growth before emergence (
        ,float  g_plants                                  // (INPUT)  Plant density (plants/m^2)
        ,float  *dm_green                                  // (INPUT/OUTPUT) plant part weights  (g/m^2)
        ,float  *dm_plant_min                              // (OUTPUT) minimum weight of each plant part (g/plant)
        ) {

//+  Local Variables
    float dm_plant_pod;                           // dry matter in pods (g/plant)

//- Implementation Section ----------------------------------

// initialise plant weight
// initialisations - set up dry matter for leaf, pod, grain
// and root

//    doPlantEvent(phenology->stageName());
   if(phenology->on_day_of(phenology->stageName()))
       fruitPart->onDayOf(phenology->stageName());

    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->doDmMin();


    if (phenology->on_day_of("emergence"))
        {
        // seedling has just emerged.
        // initialise root and leaf.
        dm_green[root] = c_dm_root_init * g_plants;
        }
   }                                                                                     // Should plant event be call for all parts?

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
void Plant::plant_n_conc_limits(float  c_n_conc_crit_root                 // (INPUT)  critical N concentration of ro
                               ,float  c_n_conc_max_root                  // (INPUT)  maximum N concentration of roo
                               ,float  c_n_conc_min_root                  // (INPUT)  minimum N concentration of roo
                               ,float  g_co2_modifier_n_conc
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

        n_conc_crit[root] = c_n_conc_crit_root;
        n_conc_max[root] = c_n_conc_max_root;
        n_conc_min[root] = c_n_conc_min_root;

// the tops critical N percentage concentration is the stover
// (non-grain shoot) concentration below which N concentration
// begins to affect plant growth.

        for (vector<plantPart *>::iterator t = myParts.begin();
             t != myParts.end();
             t++)
           (*t)->n_conc_limits();

        leafPart->g.n_conc_crit *= g_co2_modifier_n_conc;
        if (leafPart->g.n_conc_crit <= leafPart->g.n_conc_min)
           {
           throw std::runtime_error("Aiieeee nconc_crit < nconc_min!");
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
    ,vector<plantPart *> &allParts        // (INPUT) vector of plant parts
    ) {

//+  Local Variables
    int   deepest_layer;                          // deepest layer in which the roots are growing
    float plant_part_fract;                       // fraction of nitrogen to use (0-1) for plant part
    vector<plantPart *>::iterator part;           // iterator
    int   ipart;
    float n_uptake_sum;                           // total plant N uptake (g/m^2)
    float n_excess;                               // N uptake above N crit (g/m^2)
    vector<float> n_capacity(allParts.size());    // amount of N that can be stored in plant part above Ncrit (g/m^2)
    float n_capacity_sum;                         // total excess N storage (g/m^2)
    float n_demand_sum;                               // total nitrogen demand (g/m^2)
    float n_fix_demand_tot;                       // total demand for N fixation (g/m^2)
    float fix_demand;                             // demand for fixed N per plant part (g/m^
    float fix_part_fract;                         // fraction of fixed N per plant part (g/m
//- Implementation Section ----------------------------------

    float dlt_n_green_part = 0.0;

    // find the proportion of uptake to be distributed to
    // each plant part and distribute it.
    deepest_layer = find_layer_no (g_root_depth, g_dlayer, max_layer);
    n_uptake_sum = - sum_real_array (g_dlt_no3gsm, deepest_layer+1)
                   - sum_real_array (g_dlt_nh4gsm, deepest_layer+1);

    n_demand_sum = 0.0;
    for (part = allParts.begin(); part != allParts.end(); part++)
       n_demand_sum += (*part)->nDemand();

    n_excess = n_uptake_sum - n_demand_sum;
    n_excess = l_bound (n_excess, 0.0);

    n_capacity_sum = 0.0;
    for (part = allParts.begin(); part != allParts.end(); part++)
       n_capacity_sum += (*part)->nCapacity();

    for (part = allParts.begin(); part != allParts.end(); part++)
        {
        if (n_excess>0.0)
            {
            plant_part_fract = divide ((*part)->nCapacity(), n_capacity_sum, 0.0);
            dlt_n_green_part = (*part)->nDemand() + n_excess * plant_part_fract;
            }
        else
            {
            plant_part_fract = divide ((*part)->nDemand(), n_demand_sum, 0.0);
            dlt_n_green_part = n_uptake_sum * plant_part_fract;
            }
        (*part)->nPartition(dlt_n_green_part);
        }

    float dlt_n_green_sum = 0.0;
    for (part = allParts.begin(); part != allParts.end(); part++)
         dlt_n_green_sum += (*part)->dltNGreen();

    if (!reals_are_equal(dlt_n_green_sum - n_uptake_sum, 0.0))
        {
        string msg ="Crop dlt_n_green mass balance is off: dlt_n_green_sum ="
              + ftoa(dlt_n_green_sum, ".6")
              + " vs n_uptake_sum ="
              + ftoa(n_uptake_sum, ".6");
        parent->warningError(msg.c_str());
        }

    n_fix_demand_tot = l_bound (n_demand_sum - n_uptake_sum, 0.0);

    *n_fix_uptake = bound (g_n_fix_pot, 0.0, n_fix_demand_tot);

    for (part = allParts.begin(); part != allParts.end(); part++)
         {
         fix_demand = l_bound ((*part)->nDemand() - (*part)->dltNGreen(), 0.0);
         fix_part_fract = divide (fix_demand, n_fix_demand_tot, 0.0);
         dlt_n_green_part = fix_part_fract * (*n_fix_uptake);
         (*part)->nFix(dlt_n_green_part);
         }
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
void Plant::legnew_dm_partition1( float  c_frac_leaf                   // (INPUT)  fraction of remaining dm allocated to leaf
                                     , float  c_ratio_root_shoot            // (INPUT)  root:shoot ratio of new dm ()
                                     , float  c_sla_min                     // (INPUT)  minimum specific leaf area for
                                     , double  g_dlt_dm                      // (INPUT)  the daily biomass production (
                                     , float g_dlt_dm_yield_demand
                                     , double *dlt_dm_fruit
                                     , float *dlt_dm_green)                  // (OUTPUT) actual biomass partitioned to plant parts (g/m^2)
{

//+  Local Variables
    double dlt_dm_green_tot;                       // total of partitioned dm (g/m^2)
    double dlt_dm_leaf_max;                        // max increase in leaf dm (g/m^2)
    double dm_remaining;                           // interim dm pool for partitioning

//- Implementation Section ----------------------------------

         // Root must be satisfied. The roots don't take any of the
         // carbohydrate produced - that is for tops only.  Here we assume
         // that enough extra was produced to meet demand. Thus the root
         // growth is not removed from the carbo produced by the model.

          // first we zero all plant component deltas
    fill_real_array (dlt_dm_green, 0.0, max_part);
    for (vector<plantPart *>::iterator t = myParts.begin();
         t != myParts.end();
         t++)
       (*t)->zeroDltDmGreen();

    // now we get the root delta for all stages - partition scheme
    // specified in coeff file
    dlt_dm_green[root] = c_ratio_root_shoot * g_dlt_dm;

         // now distribute the assimilate to plant parts
    if (g_dlt_dm_yield_demand >= g_dlt_dm)
        {
        // reproductive demand exceeds supply - distribute assimilate to those parts only
        *dlt_dm_fruit = g_dlt_dm;
        }
    else
        {
        // more assimilate than needed for reproductive parts
        // distribute to all parts

        // satisfy reproductive demands
        // distribute remainder to vegetative parts
        *dlt_dm_fruit = g_dlt_dm_yield_demand;
        dm_remaining = g_dlt_dm - g_dlt_dm_yield_demand;
        leafPart->dlt.dm_green = c_frac_leaf * dm_remaining;

            // limit the delta leaf area to maximum
        dlt_dm_leaf_max = divide (leafPart->dltLAI_stressed, c_sla_min * smm2sm, 0.0);
        leafPart->dlt.dm_green = u_bound (leafPart->dlt.dm_green, dlt_dm_leaf_max);

        dm_remaining -= leafPart->dlt.dm_green;
        // everything else to stem
        stemPart->dlt.dm_green = dm_remaining;
        }

         // do mass balance check - roots are not included
    dlt_dm_green_tot = topsDltDmGreen() + *dlt_dm_fruit;

    if (!reals_are_equal(dlt_dm_green_tot, g_dlt_dm, 1.0E-4))  // XX this is probably too much slop - try doubles XX
    {
         string msg = "dlt_dm_green_tot mass balance is off: "
                    + ftoa(dlt_dm_green_tot, ".6")
                    + " vs "
                    + ftoa(g_dlt_dm, ".6");
      parent->warningError(msg.c_str());
    }

    // check that deltas are in legal range
    bound_check_real_array (parent, dlt_dm_green, max_part, 0.0, g_dlt_dm, "dlt_dm_green");
}

  void Plant::legnew_dm_partition2 (float  g_current_stage
                                        , float  *c_x_stage_no_partition
                                        , float  *c_y_frac_leaf
                                        , int    c_num_stage_no_partition
                                        , float *c_y_ratio_root_shoot
                                        , float c_sla_min
                                        , double g_dlt_dm
                                        , float g_dlt_dm_yield_demand
                                        , double *dlt_dm_fruit          // (OUTPUT)
                                        , float *dlt_dm_green)       // (OUTPUT)
{

//+  Local Variables
    double dlt_dm_green_tot;                       // total of partitioned dm (g/m^2)
    double dlt_dm_leaf_max;                        // max increase in leaf dm (g/m^2)
    double dm_remaining;                           // interim dm pool for partitioning

//- Implementation Section ----------------------------------

// Interpolate leaf and pod fractions
    float frac_leaf = linear_interp_real(g_current_stage
                                   ,c_x_stage_no_partition
                                   ,c_y_frac_leaf
                                   ,c_num_stage_no_partition);

    float ratio_root_shoot = linear_interp_real(g_current_stage
                                          ,c_x_stage_no_partition
                                          ,c_y_ratio_root_shoot
                                          ,c_num_stage_no_partition);

         // Root must be satisfied. The roots don't take any of the
         // carbohydrate produced - that is for tops only.  Here we assume
         // that enough extra was produced to meet demand. Thus the root
         // growth is not removed from the carbo produced by the model.

          // first we zero all plant component deltas
    fill_real_array (dlt_dm_green, 0.0, max_part);
    for (vector<plantPart *>::iterator t = myParts.begin();
         t != myParts.end();
         t++)
       (*t)->dlt.dm_green = 0.0;
       fruitPart->zeroDltDmGreen();

    // now we get the root delta for all stages - partition scheme
    // specified in coeff file
    dlt_dm_green[root] = ratio_root_shoot * g_dlt_dm;

         // now distribute the assimilate to plant parts
    if (g_dlt_dm_yield_demand >= g_dlt_dm)
        {
        // reproductive demand exceeds supply - distribute assimilate to those parts only
        *dlt_dm_fruit = g_dlt_dm;
        }
    else
        {
        // more assimilate than needed for reproductive parts
        // distribute to all parts

        // satisfy reproductive demands
        *dlt_dm_fruit = g_dlt_dm_yield_demand;

        // distribute remainder to vegetative parts
        dm_remaining = g_dlt_dm - g_dlt_dm_yield_demand;
        leafPart->dlt.dm_green = frac_leaf * dm_remaining;

            // limit the delta leaf area to maximum
        dlt_dm_leaf_max = divide (leafPart->dltLAI_stressed, c_sla_min * smm2sm, 0.0);
        leafPart->dlt.dm_green = u_bound (leafPart->dlt.dm_green, dlt_dm_leaf_max);

        dm_remaining -= leafPart->dlt.dm_green;
        // everything else to stem
        stemPart->dlt.dm_green = dm_remaining;
        }

         // do mass balance check - roots are not included
    dlt_dm_green_tot = topsDltDmGreen() + *dlt_dm_fruit;

    if (!reals_are_equal(dlt_dm_green_tot, g_dlt_dm, 1.0E-4))  // XX this is probably too much slop - try doubles XX
    {
         string msg = "dlt_dm_green_tot mass balance is off: "
                    + ftoa(dlt_dm_green_tot, ".6")
                    + " vs "
                    + ftoa(g_dlt_dm, ".6");
      parent->warningError(msg.c_str());
    }

    // check that deltas are in legal range
    bound_check_real_array (parent, dlt_dm_green, max_part, 0.0, g_dlt_dm, "dlt_dm_green");

}


//     ===========================================================
void Plant::legnew_dm_retranslocate
    (vector<plantPart *> &allParts         // (INPUT) all parts of plant
    ,vector<plantPart *> &supply_pools     // (INPUT) parts that can supply retranslocate
    ,float  g_dm_demand_differential      // (INPUT)  grain dm demand (g/m^2)
    ,float  g_plants                      // (INPUT)  Plant density (plants/m^2)
    ,float  *dlt_dm_retrans_to_fruit)      // (OUTPUT) dm retranslocated to fruit (g/m^2)
{

//+  Purpose
//     Calculate plant dry matter delta's due to retranslocation
//     to grain, pod and energy (g/m^2)

//+  Mission Statement
//   Calculate biomass retranslocation to the yield component

//+  Changes
//       150900 jngh specified and programmed

//+  Local Variables
    vector<plantPart *>::iterator part;

    float dlt_dm_retrans_part;                    // carbohydrate removed from part (g/m^2)
    float demand_differential;                    // demand in excess of available supply (g/m^2)
    int   counter;
    float dm_part_avail;                          // carbohydrate avail from part(g/m^2)
    float mass_balance;                           // sum of translocated carbo (g/m^2)
    float dm_retranslocate = 0.0;

//- Implementation Section ----------------------------------

// now translocate carbohydrate between plant components
// this is different for each stage

    for (part = allParts.begin(); part != allParts.end(); part++)
        (*part)->dlt.dm_green_retrans = 0.0;

    demand_differential = g_dm_demand_differential;

    // get available carbohydrate from supply pools
    for (part = supply_pools.begin(); part != supply_pools.end(); part++)
        {
           dm_part_avail = (*part)->g.dm_green - (*part)->g.dm_plant_min * g_plants;
           dm_part_avail = l_bound (dm_part_avail, 0.0);

           dlt_dm_retrans_part = min (demand_differential, dm_part_avail);

           //assign and accumulate
           dm_retranslocate += (*part)->dlt.dm_green_retrans = - dlt_dm_retrans_part;

           demand_differential = demand_differential - dlt_dm_retrans_part;
        }

    *dlt_dm_retrans_to_fruit = - dm_retranslocate;
}


//  Purpose
//      Partitions new dm (assimilate) between plant components (g/m^2)
//
//  Mission Statement
//    Partitions new biomass between plant components
//
//  Changes
//      010994 jngh specified and programmed     //FIXME need to be called
//JNGH Implement?void Plant::legnew_dm_part_demands(float c_frac_pod              // (INPUT)  fraction of remaining dm allocated to pod
//JNGH Implement?                                 , float g_grain_energy          // multiplier of grain weight to account f
//JNGH Implement?                                 , float c_grain_oil_conc        // (INPUT)  grain dm demand (g/m^2)
//JNGH Implement?                                 , float g_dlt_dm_grain_demand   // multiplier of grain weight to account f
//JNGH Implement?                                 , float *dm_oil_conv_demand      // assimilate demand for reproductive parts (g/m^2)
//JNGH Implement?                                 , float *dlt_dm_demand_meal      // assimilate demand for reproductive parts (g/m^2)
//JNGH Implement?                                 , float *dlt_dm_demand_oil       // assimilate demand for reproductive parts (g/m^2)
//JNGH Implement?                                 , float *dlt_dm_demand_pod )     // assimilate demand for conversion to oil (g/m^2)
//JNGH Implement?   {
//JNGH Implement?   float       dm_grain_demand;       // assimilate demand for grain (g/m^2)
//JNGH Implement?
//JNGH Implement?   // calculate demands of reproductive parts
//JNGH Implement?   dm_grain_demand = divide (g_dlt_dm_grain_demand, g_grain_energy, 0.0);
//JNGH Implement?   *dlt_dm_demand_meal = dm_grain_demand * (1.0 - c_grain_oil_conc);
//JNGH Implement?   *dlt_dm_demand_oil = dm_grain_demand - *dlt_dm_demand_meal;
//JNGH Implement?   *dm_oil_conv_demand= g_dlt_dm_grain_demand - dm_grain_demand;
//JNGH Implement?   *dlt_dm_demand_pod = dm_grain_demand * c_frac_pod;
//JNGH Implement?   }
//JNGH Implement?
//JNGH Implement?//  Purpose
//      Partitions new dm (assimilate) between plant components (g/m^2)
//
//  Mission Statement
//    Partitions new biomass between plant components
//
//  Changes
//      010994 jngh specified and programmed      //FIXME not used yet - will need for arbitrating dm to plant parts based on demands
//JNGH Implement? void Plant::legnew_dm_distribute(int max_part
//JNGH Implement?                                , float *dm_remaining          // interim dm pool for partitioning
//JNGH Implement?                                , float dlt_dm_demand_meal    // assimilate demand for reproductive parts (g/m^2)
//JNGH Implement?                                , float dlt_dm_demand_oil     // assimilate demand for reproductive parts (g/m^2)
//JNGH Implement?                                , float dlt_dm_demand_pod     // assimilate demand for reproductive parts (g/m^2)
//JNGH Implement?                                , float dm_oil_conv_demand    // assimilate demand for conversion to oil (g/m^2)
//JNGH Implement?                                , float dlt_dm_oil_conv       // (OUTPUT) actual biomass used in conversion to oil (g/m2)
//JNGH Implement?                                , float *dlt_dm_green          // (OUTPUT) actual biomass partitioned
//JNGH Implement?                                 )
//JNGH Implement?     {
//JNGH Implement?     float  yield_demand;   // sum of grain, energy & pod
//JNGH Implement?
//JNGH Implement?     if (*dm_remaining > 0.0)
//JNGH Implement?         {
//JNGH Implement?         // still have some assimilate to partition
//JNGH Implement?
//JNGH Implement?         yield_demand = dlt_dm_demand_pod
//JNGH Implement?                         + dlt_dm_demand_meal
//JNGH Implement?                         + dlt_dm_demand_oil
//JNGH Implement?                         + dm_oil_conv_demand;
//JNGH Implement?
//JNGH Implement?         if (yield_demand >= *dm_remaining)
//JNGH Implement?             {
//JNGH Implement?             // reproductive demand exceeds supply - distribute assimilate to those parts only
//JNGH Implement?             dlt_dm_green[meal] = *dm_remaining
//JNGH Implement?                            * divide (dlt_dm_demand_meal
//JNGH Implement?                                    , yield_demand, 0.0);
//JNGH Implement?             dlt_dm_green[oil] = *dm_remaining
//JNGH Implement?                             * divide (dlt_dm_demand_oil
//JNGH Implement?                                     , yield_demand, 0.0);
//JNGH Implement?             dlt_dm_oil_conv = *dm_remaining
//JNGH Implement?                            * divide (dm_oil_conv_demand
//JNGH Implement?                                    , yield_demand, 0.0);
//JNGH Implement?             dlt_dm_green[pod] = *dm_remaining
//JNGH Implement?                                     - dlt_dm_green[meal]
//JNGH Implement?                                     - dlt_dm_green[oil]
//JNGH Implement?                                     - dlt_dm_oil_conv;
//JNGH Implement?             *dm_remaining = 0.0;
//JNGH Implement?             }
//JNGH Implement?         else
//JNGH Implement?             {
//JNGH Implement?             // more than enough assimilate to go around
//JNGH Implement?             dlt_dm_green[meal] =  dlt_dm_green[meal]
//JNGH Implement?                                +  dlt_dm_demand_meal;
//JNGH Implement?             dlt_dm_green[oil]  =  dlt_dm_green[oil]
//JNGH Implement?                                +  dlt_dm_demand_oil;
//JNGH Implement?             dlt_dm_oil_conv   =  dlt_dm_oil_conv
//JNGH Implement?                               +  dm_oil_conv_demand;
//JNGH Implement?             dlt_dm_green[pod]  = dlt_dm_green[pod]
//JNGH Implement?                                + dlt_dm_demand_pod;
//JNGH Implement?
//JNGH Implement?             *dm_remaining = *dm_remaining - yield_demand;
//JNGH Implement?             }
//JNGH Implement?         }
//JNGH Implement?      else
//JNGH Implement?          {
//JNGH Implement?          // no assimilate left to partition
//JNGH Implement?          dlt_dm_green[meal] = 0.0;
//JNGH Implement?          dlt_dm_green[oil]  = 0.0;
//JNGH Implement?          dlt_dm_oil_conv   = 0.0;
//JNGH Implement?          dlt_dm_green[pod]  = 0.0;
//JNGH Implement?          }
//JNGH Implement?    }
//JNGH Implement?
//JNGH Implement?
//+  Purpose
//     Calculate the nitrogen retranslocation from the various plant parts
//     to the grain.

//+  Mission Statement
//     Calculate N retranslocation from various plant parts to grain

//+  Changes
//       080994 jngh specified and programmed
void Plant::legnew_n_retranslocate (float g_grain_n_demand)
{
//+  Constant Values
    const char*  my_name = "legnew_n_retranslocate" ;
    const float  tolerence = 0.001 ;

//+  Local Variables
    plantPartHack *rootPart = new plantPartHack(this, root, "root"); rootPart->c.n_retrans_fraction = 0.0;

    vector<plantPart *> allParts;
    allParts.push_back(rootPart);
    allParts.push_back(leafPart);
    allParts.push_back(stemPart);
    allParts.push_back(fruitPart);

    vector<plantPart *> mbCheckParts1;
    mbCheckParts1.push_back(rootPart);
    mbCheckParts1.push_back(leafPart);
    mbCheckParts1.push_back(stemPart);
    mbCheckParts1.push_back(fruitPart);

    vector<plantPart *> stoverParts1;
    stoverParts1.push_back(leafPart);
    stoverParts1.push_back(stemPart);
    stoverParts1.push_back(fruitPart);

    vector<plantPart*>::iterator part;            // plant part

//- Implementation Section ----------------------------------
    push_routine (my_name);

          //! available N does not include roots or grain
          //! this should not presume roots and grain are 0.
          // grain N potential (supply)
    float n_avail_stover = 0.0;
    for (part = stoverParts1.begin(); part != stoverParts1.end(); part++)
        n_avail_stover += (*part)->availableRetranslocateN();


    for (part = stoverParts1.begin(); part != stoverParts1.end(); part++)
        (*part)->doNRetranslocate(n_avail_stover, g_grain_n_demand);

          // check that we got (some of) the maths right.
    for (part = mbCheckParts1.begin(); part != mbCheckParts1.end(); part++)
        {
        bound_check_real_var (parent,fabs((*part)->dltNRetransOut())
                              , 0.0, (*part)->availableRetranslocateN() + tolerence
                              , (string("dlt_N_retrans(") + (*part)->name() + string(")")).c_str() );
        }

    pop_routine (my_name);
    delete rootPart;
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

    if (phenology->on_day_of("harvest_ripe"))
        {
        // Constrain leaf death to remaining leaves
        //cnh do we really want to do this?;
        leaf_no_dead_now = sum_real_array (g_leaf_no_dead,max_node);
        *dlt_leaf_no_dead = l_bound (leaf_no_now - leaf_no_dead_now, 0.0);
        }
    else if (phenology->stageNumber() > c_sen_start_stage
             /*XXXX should be phenology->inPhase("leaf_senescence") !!!!!*/)
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
       }
    else
        {
        *dlt_leaf_no_dead = 0.0;
        }

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

      // Find N in senesced material
      for (part = 0; part < num_part; part++)
         {
         green_n_conc = divide (g_n_green[part]
                               ,g_dm_green[part]
                               ,0.0);

         sen_n_conc = min (c_n_sen_conc[part], green_n_conc);

         dlt_n_senesced[part] = g_dlt_dm_senesced[part]
                              * sen_n_conc;

         dlt_n_senesced[part] = u_bound (dlt_n_senesced[part]
                                           , g_n_green[part]);
         }
      for (vector<plantPart *>::iterator t = myParts.begin();
           t != myParts.end();
           t++)
         (*t)->doNSenescence();

      //! now get N to retranslocate out of senescing leaves
      fill_real_array(dlt_n_senesced_trans, 0.0, num_part);
      for (vector<plantPart *>::iterator t = myParts.begin();
           t != myParts.end();
           t++)
         (*t)->zeroDltNSenescedTrans();

      green_n_conc = divide (leafPart->g.n_green, leafPart->g.dm_green, 0.0);
      dlt_n_in_senescing_leaf = leafPart->dlt.dm_senesced * green_n_conc;

      navail = dlt_n_in_senescing_leaf - leafPart->dlt.n_senesced;
      navail = l_bound(navail, 0.0);

      n_demand_tot = sumNDemand();
      for (part=0; part < num_part; part++)
         {
         dlt_n_senesced_retrans[part] = navail
                           * divide (g_n_demand[part]
                                    ,n_demand_tot
                                    ,0.0);
         }
      for (vector<plantPart *>::iterator t = myParts.begin();
           t != myParts.end();
           t++)
         (*t)->doNSenescedRetrans(navail, n_demand_tot);

      pop_routine (my_name);
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

    plant_co2_modifier_rue ();
    plant_co2_modifier_te ();
    plant_co2_modifier_n_conc ();
    plant_vpd (c.svp_fract, g.maxt, g.mint);


//      call plant_root_depth (1)  !1 = xf added
    plant_root_depth (2);
    plant_water_supply (1);

    if (g.plant_status == alive)
        {
        plant_water_uptake (1);
        plant_water_stress (2);
        plant_oxdef_stress (1);

        environment_t e;
        e.dlayer = vector<float>(g.dlayer, g.dlayer + g.num_layers);
        e.sw_dep = vector<float>(g.sw_dep, g.sw_dep + g.num_layers);
        e.ll_dep = vector<float>(p.ll_dep, p.ll_dep + g.num_layers);
        e.dul_dep = vector<float>(g.dul_dep, g.dul_dep + g.num_layers);
        e.maxt = g.maxt;
        e.mint = g.mint;
        e.latitude = g.latitude;
        e.day_of_year = g.day_of_year;
        phenology->prepare (e);

        pheno_stress_t ps;
        ps.swdef = g.swdef_pheno;
        ps.nfact = min(g.nfact_pheno, g.pfact_pheno);
        ps.swdef_flower = g.swdef_pheno_flower;
        ps.swdef_grainfill = g.swdef_pheno_grainfill;
        ps.remove_biom_pheno = g.remove_biom_pheno;

        phenology->process (e,ps);

        plant_root_depth_init(1);

//        plant_grain_number(c.grain_no_option);
        fruitPart->grain_number();    // Calculate grain no

        for (vector<plantPart *>::iterator t = myParts.begin();
             t != myParts.end();
             t++)
           (*t)->morphology();

        plant_leaf_no_init(1);
        plant_leaf_no_pot (c.leaf_no_pot_option); // plant node/leaf approach

        plant_leaf_area_init (1);
        plant_leaf_area_potential (1);            // linear interp leaf size
        plant_leaf_area_stressed (1);

        plant_water_distribute (1);
        plant_bio_water (1);
        plant_bio_rue (1);
        //fprintf(stdout, "%d,%.9f,%.9f,%.9f\n", g.day_of_year,g.dlt_dm, g.dlt_dm_pot_rue, g.dlt_dm_pot_te);
        plant_bio_init(1);
        plant_bio_actual (1);
        // c1
        //fprintf(stdout, "%d,%.9f,%.9f,%.9f\n", g.day_of_year,g.dlt_dm, g.dlt_dm_pot_rue, g.dlt_dm_pot_te);

        fruitPart->processBioDemand();

        plant_bio_partition (c.partition_option);

//        plant_retrans_init(1);

        plant_bio_retrans (c.partition_option);

        plant_bio_distribute (c.partition_option);  // for fruit class - process bio distribute

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

        plant_p_retrans();

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

    plant_water_stress (2);
    plant_nit_stress (c.n_stress_option);

    pop_routine (my_name);
    }


//+  Purpose
//       Set up states for dead crop

//+  Mission Statement
//     Sets up states for dead crop

//+  Changes
//      091095 jngh specified and programmed
void Plant::plant_dead (void)
    {

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

    if (g.plant_status != out)
        {
          plant_auto_class_change("kill_stem");

          plant_kill_stem_update(v);
        }
    else
        {
        char msg[500];
        sprintf(msg, "%s%s%s"
         ,g.module_name.c_str()
         , " is not in the ground -"
         , " unable to kill stem.");
        parent->warningError (msg);
        }


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
    float dm_residue;                             // dry matter added to residue (kg/ha)
    float n_residue;                              // nitrogen added to residue (kg/ha)
    float dm_root_residue;                             // dry matter added to residue (kg/ha)
    float n_root_residue;                              // nitrogen added to residue (kg/ha)
    float p_root_residue;                              // phosp added to residue (kg/ha)
    float dm_tops_residue;                             // dry matter added to residue (kg/ha)
    float n_tops_residue;                              // nitrogen added to residue (kg/ha)
    float p_tops_residue;                              // phosp added to residue (kg/ha)
    float p_residue;                              // phosphorus added to residue (g/m^2)
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
    float n_init, p_init;
    float height;                                 // cutting height
    float retain_fr_green;
    float retain_fr_sen;
    float retain_fr_dead;
    float canopy_fac;
    float temp;
    float chop_fr_green;                      // fraction chopped (0-1)
    float chop_fr_sen;                      // fraction chopped (0-1)
    float chop_fr_dead;                      // fraction chopped (0-1)
    float chop_fr;                                 // fraction chopped (0-1)
    float dlt_dm_harvest;                         // dry matter harvested (g/m^2)
    float dlt_n_harvest;                          // N content of dm harvested (g/m^2)
    float dlt_p_harvest;                          // N content of dm harvested (kg/ha)
    float dlt_dm_die;                             // dry matter in dieback of roots (g/m^2)
    float dlt_n_die;                              // N content of drymatter in dieback (g/m^2)
    float dlt_p_die;                              // P content of drymatter in dieback (g/m^2)
    float avg_leaf_area;
    float P_tops;
    float cover_pod;

//- Implementation Section ----------------------------------
    push_routine (my_name);

    // Tell the rest of the system we are about to harvest
    sendStageMessage("harvesting");

    protocol::ApsimVariant incomingApsimVariant(parent);
    incomingApsimVariant.aliasTo(v.getMessageData());

    unsigned int junk = 0L;
    phenology->onHarvest(junk,junk,v);

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

    // determine the cutting height
    if (incomingApsimVariant.get("height", protocol::DTsingle, false, height) == false)
       {
       height = 0.0;
       }
    bound_check_real_var(parent,height, 0.0, 1000.0, "height");

    vector<string> dm_type;
    vector<float>  fraction_to_residue;
    vector<float>  dlt_crop_dm;
    vector<float>  dlt_dm_n;
    vector<float>  dlt_dm_p;

    // Update biomass and N pools.  Different types of plant pools are affected differently.
    // Calculate Root Die Back
    retain_fr_green = 1.0;
    retain_fr_sen = 1.0;
    retain_fr_dead = 1.0;
    chop_fr_green = 1.0 - retain_fr_green;
    chop_fr_dead  = 1.0 - retain_fr_sen;
    chop_fr_sen   = 1.0 - retain_fr_dead;

    dlt_dm_die = g.dm_green[root] * c.root_die_back_fr;
    g.dm_senesced[root] = g.dm_senesced[root] + dlt_dm_die;
    g.dm_green[root] = g.dm_green[root] - dlt_dm_die;

    dlt_n_die =  dlt_dm_die * c.n_sen_conc[root];
    g.n_senesced[root] = g.n_senesced[root] + dlt_n_die;
    g.n_green[root]= g.n_green[root] - dlt_n_die;

    dlt_p_die =  g.p_green[root] * c.root_die_back_fr;
    g.p_sen[root] = g.p_sen[root] + dlt_p_die;
    g.p_green[root]= g.p_green[root] - dlt_p_die;

    dlt_dm_harvest = g.dm_dead[root]*chop_fr_dead
                    + g.dm_green[root]*chop_fr_green
                    + g.dm_senesced[root]*chop_fr_sen;

    dlt_n_harvest = g.n_dead[root]*chop_fr_dead
                    + g.n_green[root]*chop_fr_green
                    + g.n_senesced[root]*chop_fr_sen;

    dlt_p_harvest = g.p_dead[root]*chop_fr_dead
                    + g.p_green[root]*chop_fr_green
                    + g.p_sen[root]*chop_fr_sen;

    g.dm_dead[root] *= retain_fr_dead;
    g.dm_senesced[root] *= retain_fr_sen;
    g.dm_green[root] *= retain_fr_green;
    g.n_dead[root] *= retain_fr_dead;
    g.n_senesced[root] *= retain_fr_sen;
    g.n_green[root] *= retain_fr_green;
    g.p_dead[root] *= retain_fr_dead;
    g.p_sen[root] *= retain_fr_sen;
    g.p_green[root] *= retain_fr_green;

    dm_type.push_back("root");
    fraction_to_residue.push_back(0.0);
    dlt_crop_dm.push_back(dlt_dm_harvest * gm2kg/sm2ha);
    dlt_dm_n.push_back(dlt_n_harvest * gm2kg/sm2ha);
    dlt_dm_p.push_back(dlt_p_harvest* gm2kg/sm2ha);

    // Accounting for tops
    // Calculate return of biomass to surface residues

    for (vector<plantPart *>::iterator t = myParts.begin();
         t != myParts.end();
         t++)
       (*t)->onHarvest(height, remove_fr,
                        dm_type,
                        dlt_crop_dm,
                        dlt_dm_n,
                        dlt_dm_p,
                        fraction_to_residue);

    if (sum(dlt_crop_dm) > 0.0)
        {
        plant_send_crop_chopped_event (c.crop_type
                                     , dm_type
                                     , dlt_crop_dm
                                     , dlt_dm_n
                                     , dlt_dm_p
                                     , fraction_to_residue);
        }

    dm_residue = 0.0;
    for (unsigned int part=0; part < dm_type.size(); part++)
     dm_residue += (dlt_crop_dm[part] * fraction_to_residue[part]);

    n_residue = 0.0;
    for (unsigned int part=0; part < dm_type.size(); part++)
      n_residue += (dlt_dm_n[part] * fraction_to_residue[part]);

    p_residue = 0.0;
    for (unsigned int part=0; part < dm_type.size(); part++)
      p_residue += (dlt_dm_p[part] * fraction_to_residue[part]);

    dm_root_residue = dlt_crop_dm[0] * fraction_to_residue[0];
    n_root_residue = dlt_dm_n[0] * fraction_to_residue[0];
    p_root_residue = dlt_dm_p[0] * fraction_to_residue[0];

    dm_tops_residue = dm_residue - dm_root_residue;
    n_tops_residue = n_residue - n_root_residue;
    p_tops_residue = p_residue - p_root_residue;

    parent->writeString ("\nCrop harvested.");

    char  msg[400];

    parent->writeString ("    Organic matter from crop:-      Tops to surface residue      Roots to soil FOM");

    sprintf (msg, "%48s%7.2f%24.2f", "DM (kg/ha) =               ", dm_tops_residue, dm_root_residue);
    parent->writeString (msg);

    sprintf (msg, "%48s%7.2f%24.2f", "N  (kg/ha) =               ", n_tops_residue, n_root_residue);
    parent->writeString (msg);
    if (g.phosphorus_aware)
       {
       sprintf (msg, "%48s%7.2f%24.2f",
                                     "P  (kg/ha) =               ", p_tops_residue, p_root_residue);
       parent->writeString (msg);
       }
    parent->writeString (" ");

    float dm_chopped_tops = sum(dlt_crop_dm) - dlt_crop_dm[0];
    float dm_chopped_root = dlt_crop_dm[0];
    float dm_removed_tops = dm_chopped_tops - dm_tops_residue;
    float dm_removed_root = dm_chopped_root - dm_root_residue;

    float n_chopped_tops = sum(dlt_dm_n) - dlt_dm_n[0];
    float n_chopped_root = dlt_dm_n[0];
    float n_removed_tops = n_chopped_tops - n_tops_residue;
    float n_removed_root = n_chopped_root - n_root_residue;

    float p_chopped_tops = sum(dlt_dm_p) - dlt_dm_p[0];
    float p_chopped_root = dlt_dm_p[0];
    float p_removed_tops = p_chopped_tops - p_tops_residue;
    float p_removed_root = p_chopped_root - p_root_residue;

    parent->writeString ("    Organic matter removed from system:-      From Tops               From Roots");

    sprintf (msg, "%48s%7.2f%24.2f", "DM (kg/ha) =               ", dm_removed_tops, dm_removed_root);
    parent->writeString (msg);

    sprintf (msg, "%48s%7.2f%24.2f", "N  (kg/ha) =               ", n_removed_tops, n_removed_root);
    parent->writeString (msg);
    if (g.phosphorus_aware)
       {
       sprintf (msg, "%48s%7.2f%24.2f",
                                     "P  (kg/ha) =               ", p_removed_tops, p_removed_root);
       parent->writeString (msg);
       }
    parent->writeString (" ");


    // Initialise plant leaf area
    leafPart->gLAI = c.initial_tpla * smm2sm * g.plants;
    leafPart->gSLAI = 0.0;
    leafPart->gTLAI_dead = 0.0;

    if (g.canopy_width > 0.0)
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

    float cover_green_leaf;
    legnew_cover(g.row_spacing
                          ,c.x_row_spacing
                          ,c.y_extinct_coef
                          ,c.num_row_spacing
                          , canopy_fac
                          ,leafPart->gLAI
                          ,&cover_green_leaf);

    cover_pod = fruitPart->calcCover(canopy_fac);
    g.cover_green = add_covers (cover_green_leaf, cover_pod);


    legnew_cover (g.row_spacing
                  ,c.x_row_spacing
                  ,c.y_extinct_coef_dead
                  ,c.num_row_spacing
                  , canopy_fac
                  ,leafPart->gSLAI
                  ,&g.cover_sen);
    legnew_cover (g.row_spacing
                  ,c.x_row_spacing
                  ,c.y_extinct_coef_dead
                  ,c.num_row_spacing
                  , canopy_fac
                  ,leafPart->gTLAI_dead
                  ,&g.cover_dead);

    fill_real_array (g.leaf_no, 0.0, max_node);
    fill_real_array (g.leaf_no_dead, 0.0, max_node);
    fill_real_array (g.leaf_area, 0.0, max_node);

    g.node_no = c.leaf_no_at_emerg;

    fill_real_array (g.leaf_no, 1.0, (int)c.leaf_no_at_emerg);
    g.leaf_no[(int)c.leaf_no_at_emerg] = fmod(c.leaf_no_at_emerg,1.0);

    avg_leaf_area = divide(c.initial_tpla,c.leaf_no_at_emerg,0.0);

    fill_real_array (g.leaf_area,avg_leaf_area,(int)c.leaf_no_at_emerg);
    g.leaf_area[(int)c.leaf_no_at_emerg] = fmod(c.leaf_no_at_emerg,1.0) * avg_leaf_area;

// other plant states
    plant_n_conc_limits (c.n_conc_crit_root
                       , c.n_conc_max_root
                       , c.n_conc_min_root
                       , g.co2_modifier_n_conc
                       , g.n_conc_crit
                       , g.n_conc_max
                       , g.n_conc_min);

    if (g.plant_status == alive &&
        phenology->previousStageName() != phenology->stageName())
        {
        plant_event (g.dlayer
                   , g.dm_dead
                   , g.dm_green
                   , g.dm_senesced
                   , g.n_green
                   , g.root_depth
                   , g.sw_dep
                   , p.ll_dep);
        stageObservers.reset();
        otherObservers.reset();
        }

    pop_routine (my_name);
    }


//+  Purpose
//       Update states after a kill stem event

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
    float stage_part_current;
    float stage_part_previous;
    int   stage_no;
    int   stage_no_current;
    int   stage_no_previous;
    float dm_init;
    float n_init, p_init;
    float temp;
    float dlt_dm_sen;                             // dry matter sened (g/m^2)
    float dlt_n_sen;                              // N content of dm senesced (g/m^2)
    float dlt_p_sen;                              // P content of dm senesced (g/m^2)
    float avg_leaf_area;
    int   numvals;
    int   leaf_no_emerged;
    float leaf_emerging_fract;
    float canopy_fac;
    float cover_pod;

//- Implementation Section ----------------------------------
    push_routine (my_name);

    protocol::ApsimVariant aV(parent);
    aV.aliasTo(v.getMessageData());

    unsigned int junk = 0L;
    phenology->onKillStem(junk,junk,v);

    // determine the new stem density
    // ==============================
    if (aV.get("plants", protocol::DTsingle, false, temp) == true)
        {
        bound_check_real_var(parent,temp, 0.0, 10000.0, "plants");
        g.plants = temp;
        }

    plantPartHack *rootPart = new plantPartHack(this, root, "root");

    vector<plantPart *> someParts1;
    //someParts.push_back(stemPart);
    someParts1.push_back(fruitPart);
    vector<plantPart *>::iterator part;

    // Update biomass and N pools.

    // XX should be part->onKillStem(...); FIXME!
    // Calculate Root Die Back
    dlt_dm_sen = rootPart->g.dm_green * c.root_die_back_fr;
    rootPart->g.dm_senesced += dlt_dm_sen;
    rootPart->g.dm_green -= dlt_dm_sen;
    dlt_n_sen =  rootPart->g.dm_green * c.root_die_back_fr * c.n_sen_conc[root];
    rootPart->g.n_senesced += dlt_n_sen;
    rootPart->g.n_green -= dlt_n_sen;
    dlt_p_sen =  rootPart->g.p_green * c.root_die_back_fr;
    rootPart->g.p_sen += dlt_p_sen;
    rootPart->g.p_green -= dlt_p_sen;

    for (part = myParts.begin(); part != myParts.end(); part++)
        {
        dm_init = (*part)->c.dm_init * g.plants;
        n_init = dm_init * (*part)->c.n_init_conc;
        p_init = dm_init * (*part)->c.p_init_conc;
        (*part)->g.dm_dead += (*part)->g.dm_green + (*part)->g.dm_senesced - dm_init;
        (*part)->g.dm_dead = l_bound ((*part)->g.dm_dead, 0.0);
        (*part)->g.dm_green = dm_init;
        (*part)->g.dm_senesced = 0.0;

        (*part)->g.n_dead += (*part)->g.n_green + (*part)->g.n_senesced - n_init;
        (*part)->g.n_dead = l_bound ((*part)->g.n_dead, 0.0);
        (*part)->g.n_green = n_init;
        (*part)->g.n_senesced = 0.0;

        (*part)->g.p_dead += (*part)->g.p_green + (*part)->g.p_sen - p_init;
        (*part)->g.p_dead = l_bound ((*part)->g.p_dead, 0.0);
        (*part)->g.p_green = p_init;
        (*part)->g.p_sen = 0.0;
        }

    fruitPart->onKillStem();

    for (part = someParts1.begin(); part != someParts1.end(); part++)        // somPaarts are pod, meal and oil - only place it is used.
       {
       (*part)->g.dm_dead += (*part)->g.dm_green + (*part)->g.dm_senesced;
       (*part)->g.dm_green = 0.0;
       (*part)->g.dm_senesced = 0.0;

       (*part)->g.n_dead += (*part)->g.n_green + (*part)->g.n_senesced;
       (*part)->g.n_green = 0.0;
       (*part)->g.n_senesced = 0.0;

       (*part)->g.p_dead += (*part)->g.p_green + (*part)->g.p_sen;
       (*part)->g.p_green = 0.0;
       (*part)->g.p_sen = 0.0;
       }
    delete rootPart;

    // transfer plant leaf area
    leafPart->gSLAI = 0.0;
    leafPart->gTLAI_dead += leafPart->gLAI;
    // Initialise plant leaf area
    leafPart->gLAI = c.initial_tpla * smm2sm * g.plants;
    g.pai = 0.0;
    // JNGH need to account for dead pai

    // transfer plant grain no.

    if (g.canopy_width > 0.0)
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

    float cover_green_leaf;
    legnew_cover(g.row_spacing
                          ,c.x_row_spacing
                          ,c.y_extinct_coef
                          ,c.num_row_spacing
                          , canopy_fac
                          ,leafPart->gLAI
                          ,&cover_green_leaf);

    cover_pod = fruitPart->calcCover(canopy_fac);
    g.cover_green = add_covers (cover_green_leaf, cover_pod);


    legnew_cover (g.row_spacing
                  , c.x_row_spacing
                  , c.y_extinct_coef_dead
                  , c.num_row_spacing
                  , canopy_fac
                  , leafPart->gSLAI
                  , &g.cover_sen);
    legnew_cover (g.row_spacing
                  , c.x_row_spacing
                  , c.y_extinct_coef_dead
                  , c.num_row_spacing
                  , canopy_fac
                  , leafPart->gTLAI_dead
                  , &g.cover_dead);
    for (int node =0; node < max_node; node++)
        {
        g.leaf_no[node] = 0.0;
        g.leaf_no_dead[node] = 0.0;
        g.leaf_area[node] = 0.0;
        }

    g.node_no = c.leaf_no_at_emerg;
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

    plant_n_conc_limits (c.n_conc_crit_root
                        , c.n_conc_max_root
                        , c.n_conc_min_root
                        , g.co2_modifier_n_conc
                        , g.n_conc_crit
                        , g.n_conc_max
                        , g.n_conc_min )  ;                                          // plant N concentr

    if (g.plant_status == alive &&
        phenology->previousStageName() != phenology->stageName())
        {
        plant_event (g.dlayer
            , g.dm_dead
            , g.dm_green
            , g.dm_senesced
            , g.n_green
            , g.root_depth
            , g.sw_dep
            , p.ll_dep );
        stageObservers.reset();
        otherObservers.reset();
        }


    pop_routine (my_name);
    return;
    }

//NIH up to here

//+  Purpose
//       Zero crop variables & arrays

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
    float fract;
    int   numvals;
    vector<plantPart *>::iterator part;

    float remove_fr;
    float cover_pod;

    float dm_init;
    float n_init;
    float height;                                 // cutting height
    float fr_height;                              // fractional cutting height
    float retain_fr_green;
    float retain_fr_sen;
    float retain_fr_dead;
    float canopy_fac;
    float temp;
    float chop_fr;                                 // fraction chopped (0-1)
//    float fraction_to_residue[max_part];          // fraction sent to residue (0-1)
    float dlt_dm_removed;                         // dry matter harvested (g/m^2)
    float dlt_n_removed;                          // N content of dm harvested (g/m^2)
    float avg_leaf_area;
    float P_tops;

    plantPartHack *rootPart = new plantPartHack(this, root, "root");

    vector<plantPart *> allParts;
    allParts.push_back(rootPart);
    allParts.push_back(leafPart);
    allParts.push_back(stemPart);
    allParts.push_back(fruitPart);

    vector<plantPart *> topsParts;
    topsParts.push_back(leafPart);
    topsParts.push_back(stemPart);
    //topsParts.push_back(podPart);  //??
    //topsParts.push_back(mealPart); //??
    //topsParts.push_back(oilPart);  //??

    float error_margin = 1.0e-6 ;

//- Implementation Section ----------------------------------
    push_routine (my_name);

    protocol::removeCropDmType dmRemoved;
    v.unpack(dmRemoved);

    if (c.remove_biomass_report == "on")
    {
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
    }

    // Unpack the DmRemoved structure
    for (unsigned int pool = 0; pool < dmRemoved.dm.size(); pool++)
    {
       for (unsigned int part = 0; part < dmRemoved.dm[pool].part.size(); part++)
       {
          if (dmRemoved.dm[pool].pool == "green")
          {
             if (dmRemoved.dm[pool].part[part] == "stem")       {stemPart->dlt.dm_green = dmRemoved.dm[pool].dlt[part]; }
             else if (dmRemoved.dm[pool].part[part] ==  "leaf") {leafPart->dlt.dm_green = dmRemoved.dm[pool].dlt[part]; }
             else {  /* unknown part */ }
          }

          else if (dmRemoved.dm[pool].pool == "senesced")
          {
             if (dmRemoved.dm[pool].part[part] == "stem")       {stemPart->dlt.dm_senesced = dmRemoved.dm[pool].dlt[part]; }
             else if (dmRemoved.dm[pool].part[part] ==  "leaf") {leafPart->dlt.dm_senesced = dmRemoved.dm[pool].dlt[part]; }
             else { /* unknown part */ }
          }

          else if (dmRemoved.dm[pool].pool == "dead")
          {
             if (dmRemoved.dm[pool].part[part] == "stem")       {stemPart->dlt.dm_dead = dmRemoved.dm[pool].dlt[part]; }
             else if (dmRemoved.dm[pool].part[part] ==  "leaf") {leafPart->dlt.dm_dead = dmRemoved.dm[pool].dlt[part]; }
             else { /* unknown part */ }
          }
       }
    }

    float dltBiomassGreen = leafPart->dlt.dm_green + stemPart->dlt.dm_green;
    float biomassGreen =  leafPart->g.dm_green + stemPart->g.dm_green;
    g.remove_biom_pheno = divide (dltBiomassGreen, biomassGreen, 0.0);

    if (c.remove_biomass_report == "on")
    {
       ostrstream msg1;
       msg1 << "Remove Crop Biomass 2:-" << endl;
       float dmTotal1 = 0.0;

       msg1 << "   dm green leaf = " << leafPart->dlt.dm_green << " (g/m2)" << endl;
       msg1 << "   dm green stem = " << stemPart->dlt.dm_green << " (g/m2)" << endl;
       dmTotal1 +=  leafPart->dlt.dm_green + stemPart->dlt.dm_green;

       msg1 << "   dm senesced leaf = " << leafPart->dlt.dm_senesced << " (g/m2)" << endl;
       msg1 << "   dm senesced stem = " << stemPart->dlt.dm_senesced << " (g/m2)" << endl;
       dmTotal1 +=  leafPart->dlt.dm_senesced + stemPart->dlt.dm_senesced;

       msg1 << "   dm dead leaf = " << leafPart->dlt.dm_dead << " (g/m2)" << endl;
       msg1 << "   dm dead stem = " << stemPart->dlt.dm_dead << " (g/m2)" << endl;
       dmTotal1 +=  leafPart->dlt.dm_dead + stemPart->dlt.dm_dead;

       msg1 << endl << "   dm total = " << dmTotal1 << " (g/m2)" << endl << ends;

       parent->writeString (msg1.str());

       ostrstream msg2;
       msg2 << "Crop Biomass Available:-" << endl;
       float dmTotal2 = 0.0;

       msg2 << "   dm green leaf = " << leafPart->g.dm_green << " (g/m2)" << endl;
       msg2 << "   dm green stem = " << stemPart->g.dm_green << " (g/m2)" << endl;
       dmTotal2 +=  leafPart->g.dm_green + stemPart->g.dm_green;

       msg2 << "   dm senesced leaf = " << leafPart->g.dm_senesced << " (g/m2)" << endl;
       msg2 << "   dm senesced stem = " << stemPart->g.dm_senesced << " (g/m2)" << endl;
       dmTotal2 +=  leafPart->g.dm_senesced + stemPart->g.dm_senesced;

       msg2 << "   dm dead leaf = " << leafPart->g.dm_dead << " (g/m2)" << endl;
       msg2 << "   dm dead stem = " << stemPart->g.dm_dead << " (g/m2)" << endl;
       dmTotal2 +=  leafPart->g.dm_dead + stemPart->g.dm_dead;

       msg2 << endl << "   dm total = " << dmTotal2 << " (g/m2)" << endl << ends;

       parent->writeString (msg2.str());
    }

    // Check sensibility of part deltas
    for (part = allParts.begin(); part != allParts.end(); part++)
    {
        if ((*part)->dlt.dm_green > (*part)->g.dm_green + error_margin)
        {
             ostrstream msg;
             msg << "Attempting to remove more green " << (*part)->name() << " biomass than available:-" << endl;
             msg << "Removing " << (*part)->dlt.dm_green << " (g/m2) from " << (*part)->g.dm_green << " (g/m2) available." << ends;
             throw std::runtime_error (msg.str());
        }
        else if ((*part)->dlt.dm_senesced > (*part)->g.dm_senesced + error_margin)
        {
             ostrstream msg;
             msg << "Attempting to remove more senesced " << (*part)->name() << " biomass than available:-" << endl;
             msg << "Removing " << (*part)->dlt.dm_senesced << " (g/m2) from " << (*part)->g.dm_senesced << " (g/m2) available." << ends;
             throw std::runtime_error (msg.str());
        }
        else if ((*part)->dlt.dm_dead > (*part)->g.dm_dead + error_margin)
        {
             ostrstream msg;
             msg << "Attempting to remove more dead " << (*part)->name() << " biomass than available:-" << endl;
             msg << "Removing " << (*part)->dlt.dm_dead << " (g/m2) from " <<(*part)->g.dm_dead << " (g/m2) available." << ends;
             throw std::runtime_error (msg.str());
        }
        else
        { // no more checks
        }
    }

    // Update biomass and N pools.  Different types of plant pools are affected in different ways.
    // Calculate Root Die Back
    float dm_removed_root = 0.0, n_removed_root = 0.0;
    float chop_fr_green_leaf = divide(leafPart->dlt.dm_green, leafPart->g.dm_green, 0.0);
    float dlt_dm_die = rootPart->g.dm_green * c.root_die_back_fr * chop_fr_green_leaf;
    rootPart->g.dm_senesced = rootPart->g.dm_senesced + dlt_dm_die;
    rootPart->g.dm_green = rootPart->g.dm_green - dlt_dm_die;
    dm_removed_root = 0.0 /*dlt_dm_die??xx*/;

    float dlt_n_die = dlt_dm_die * c.n_sen_conc[root];
    rootPart->g.n_senesced = rootPart->g.n_senesced + dlt_n_die;
    rootPart->g.n_green= rootPart->g.n_green - dlt_n_die;
    n_removed_root = 0.0 /*dlt_n_die??xx*/;

    float dm_removed_tops = 0.0;
    float n_removed_tops = 0.0;
    for (part = topsParts.begin(); part != topsParts.end(); part++)
        {
        float chop_fr_green = divide((*part)->dlt.dm_green, (*part)->g.dm_green, 0.0);
        float chop_fr_sen   = divide((*part)->dlt.dm_senesced, (*part)->g.dm_senesced, 0.0);
        float chop_fr_dead  = divide((*part)->dlt.dm_dead, (*part)->g.dm_dead, 0.0);

        dm_removed_tops += ((*part)->dlt.dm_green + (*part)->dlt.dm_senesced + (*part)->dlt.dm_dead) * gm2kg/sm2ha;
        n_removed_tops += ((*part)->g.n_green*chop_fr_green +
                      (*part)->g.n_senesced*chop_fr_sen +
                      (*part)->g.n_dead*chop_fr_dead) * gm2kg/sm2ha;

        (*part)->g.dm_green -= (*part)->dlt.dm_green;
        (*part)->g.dm_senesced -= (*part)->dlt.dm_senesced;
        (*part)->g.dm_dead -= (*part)->dlt.dm_dead;

        (*part)->g.n_green -= (*part)->g.n_green * chop_fr_green;
        (*part)->g.n_senesced -= (*part)->g.n_senesced * chop_fr_sen;
        (*part)->g.n_dead -= (*part)->g.n_dead * chop_fr_dead;

        (*part)->g.p_green -= (*part)->g.p_green * chop_fr_green;
        (*part)->g.p_sen -= (*part)->g.p_sen * chop_fr_sen;
        (*part)->g.p_dead -= (*part)->g.p_dead * chop_fr_dead;
        }

    if (c.remove_biomass_report == "on")
    {
       parent->writeString ("\nCrop biomass removed.");
       char  msgrmv[400];

       parent->writeString ("    Organic matter removed from system:-      From Tops               From Roots");

       sprintf (msgrmv, "%48s%7.2f%24.2f", "DM (kg/ha) =               ", dm_removed_tops, dm_removed_root);
       parent->writeString (msgrmv);

       sprintf (msgrmv, "%48s%7.2f%24.2f", "N  (kg/ha) =               ", n_removed_tops, n_removed_root);
       parent->writeString (msgrmv);

       sprintf (msgrmv, "%30s%7.2f", "Remove biomass phenology factor = ", g.remove_biom_pheno);
       parent->writeString (msgrmv);

       parent->writeString (" ");
    }

    // Initialise plant leaf area
//    g.lai = c.initial_tpla * smm2sm * g.plants;
//    g.slai = 0.0;
//    g.tlai_dead = 0.0;

    float chop_fr_green = divide(leafPart->dlt.dm_green, leafPart->g.dm_green, 0.0);
    float chop_fr_sen   = divide(leafPart->dlt.dm_senesced, leafPart->g.dm_senesced, 0.0);
    float chop_fr_dead  = divide(leafPart->dlt.dm_dead, leafPart->g.dm_dead, 0.0);

    delete rootPart;

    float dlt_lai = leafPart->gLAI * chop_fr_green;
    leafPart->dltSLAI = leafPart->gSLAI * chop_fr_sen;
    float dlt_tlai_dead = leafPart->gTLAI_dead * chop_fr_dead;

    // keep leaf area above a minimum
    float lai_init = c.initial_tpla * smm2sm * g.plants;
    float dlt_lai_max = leafPart->gLAI - lai_init;
    dlt_lai = u_bound (dlt_lai, dlt_lai_max);

    leafPart->gLAI -= dlt_lai;
    leafPart->gSLAI -= leafPart->dltSLAI;
    leafPart->gTLAI_dead -= dlt_tlai_dead;

    // keep dm above a minimum
    dm_init = leafPart->c.dm_init * g.plants;
    leafPart->g.dm_green = l_bound (leafPart->g.dm_green, dm_init);

    n_init = dm_init * leafPart->c.n_init_conc;
    leafPart->g.n_green = l_bound (leafPart->g.n_green, n_init);

    plant_leaf_detachment (g.leaf_area
                            , leafPart->dltSLAI
                            , g.plants, max_node);


    plant_leaf_removal_top (g.leaf_area
                            , dlt_lai
                            , g.plants, &g.node_no);

    stemPart->g.width *= (1.0 - divide(stemPart->dlt.dm_green, stemPart->g.dm_green, 0.0));
    reproStruct->g.width *= (1.0 - divide(reproStruct->dlt.dm_green, reproStruct->g.dm_green, 0.0));

    if (g.canopy_width > 0.0)
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

    float cover_green_leaf;
    legnew_cover(g.row_spacing
                          ,c.x_row_spacing
                          ,c.y_extinct_coef
                          ,c.num_row_spacing
                          , canopy_fac
                          ,leafPart->gLAI
                          ,&cover_green_leaf);

    cover_pod = fruitPart->calcCover(canopy_fac);
    g.cover_green = add_covers (cover_green_leaf, cover_pod);


    legnew_cover (g.row_spacing
                  ,c.x_row_spacing
                  ,c.y_extinct_coef_dead
                  ,c.num_row_spacing
                  , canopy_fac
                  ,leafPart->gSLAI
                  ,&g.cover_sen);
    legnew_cover (g.row_spacing
                  ,c.x_row_spacing
                  ,c.y_extinct_coef_dead
                  ,c.num_row_spacing
                  , canopy_fac
                  ,leafPart->gTLAI_dead
                  ,&g.cover_dead);

         // calc new node number
     for (int node = max_node - 1; node >= 0; node--)
      {
         if (!reals_are_equal(g.leaf_area[node], 0.0, 1.0E-4))    // Slop?
         {
            g.node_no = (float)node;  //FIXME - need adjustment for leafs remaining in for this node
            break;
         }
      }


//     float oldDmStem = divide (g.dm_senesced[stem], (1.0 - chop_fr_sen[part]), 0.0)
//                       + divide (g.dm_green[stem], (1.0 - chop_fr_green[part]), 0.0);
//     float newDmStem = g.dm_senesced[stem] + g.dm_green[stem];
//     float dltNodeNo = divide (newDmStem, oldDmStem, 0.0);
//     g.node_no -= dltNodeNo;

         // calc new leaf number
     int newNodeNo = 1.0 + g.node_no;

     for (int node = newNodeNo - 1; node < max_node; node++)
      {
         g.leaf_no[node] = 0.0;
         g.leaf_no_dead[node] = 0.0;
      }

//         // clac new leaf area
//      float newLeafAreaPlant = divide (g.lai, g.plants, 0.0);
//      float leafAreaRemovedWithNodes = 0.0
//      float oldLeafAreaPlant = sum_real_array (g.leaf_area, max_node);
//
//      for (int node = newNodeNo - 1; node < max_node; node++)
//      {
//         leafAreaRemovedWithNodes += g.leaf_area[node];
//         g.leaf_area[node] = 0.0;
//      }
//
//      leafDmRemovedWithNodes = divide (leafAreaRemovedWithNodes,



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

    phenology->onRemoveBiomass(g.remove_biom_pheno);

    // other plant states
    stemPart->g.height *= (1.0 - divide(stemPart->dlt.dm_green, stemPart->g.dm_green, 0.0));
    reproStruct->g.height *= (1.0 - divide(reproStruct->dlt.dm_green, reproStruct->g.dm_green, 0.0));

    plant_n_conc_limits (c.n_conc_crit_root
                       , c.n_conc_max_root
                       , c.n_conc_min_root
                       , g.co2_modifier_n_conc
                       , g.n_conc_crit
                       , g.n_conc_max
                       , g.n_conc_min);

    if (g.plant_status == alive &&
        phenology->previousStageName() != phenology->stageName())
        {
        plant_event (g.dlayer
            , g.dm_dead
            , g.dm_green
            , g.dm_senesced
            , g.n_green
            , g.root_depth
            , g.sw_dep
            , p.ll_dep );
        stageObservers.reset();
        otherObservers.reset();
        }

    pop_routine (my_name);
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
      g.vpd = 0.0;
      g.co2_modifier_te = 0.0;
      g.co2_modifier_n_conc = 0.0;
      g.co2_modifier_rue = 0.0;
      g.hasreadconstants = false;
      g.plant_status_out_today = false;
      g.module_name = "";
      g.crop_class = "";
      g.plant_status = out;
      g.cultivar = "";
      g.pre_dormancy_crop_class = "";
      g.swdef_expansion = 1.0;
      g.swdef_photo = 1.0;
      g.swdef_pheno = 1.0;
      g.swdef_fixation = 1.0;
      g.sw_avail_fac_deepest_layer = 0;
      g.nfact_expansion = 1.0;
      g.nfact_photo = 1.0;
      g.nfact_grain_conc = 1.0;
      g.nfact_pheno = 1.0;
      g.remove_biom_pheno = 1.0;
      g.temp_stress_photo = 1.0;
      g.oxdef_photo = 1.0;
      g.row_spacing = 0.0;
      g.skip_row = 0.0;
      g.skip_plant = 0.0;
      g.skip_row_fac = 0.0;
      g.skip_plant_fac = 0.0;
      g.year = 0.0;
      g.day_of_year = 0.0;
      g.fr_intc_radn = 0.0;
      g.latitude = 0.0;
      g.radn = 0.0;
      g.mint = 0.0;
      g.maxt = 0.0;
      fill_real_array (g.soil_temp, 0.0, 366+1);
      g.eo = 0.0;
      g.canopy_width = 0.0;
      g.plants = 0.0;
      g.dlt_plants = 0.0;
      g.dlt_root_depth = 0.0;
      g.root_depth = 0.0;
      g.cover_green = 0.0;
      g.cover_sen = 0.0;
      g.cover_dead = 0.0;
      g.dlt_plants_death_seedling = 0.0;
      g.dlt_plants_death_drought = 0.0;
      g.dlt_plants_failure_phen_delay = 0.0;
      g.dlt_plants_failure_leaf_sen = 0.0;
      g.dlt_plants_failure_emergence = 0.0;
      g.dlt_plants_failure_germ = 0.0;
      g.dlt_plants_death_external = 0.0;
      g.dlt_dm = 0.0;
      g.dlt_dm_pot_rue = 0.0;
      g.dlt_dm_pot_te = 0.0;
      g.dltDmPotRueFruit = 0.0;
      g.dlt_dm_supply_to_fruit = 0.0;
      g.dlt_dm_yield_demand_fruit = 0.0;
      g.dlt_dm_retrans_to_fruit = 0.0;
      g.dlt_dm_parasite  =  0.0;
      fill_real_array (g.dlt_dm_green, 0.0, max_part);
      fill_real_array (g.dlt_dm_senesced, 0.0, max_part);
      fill_real_array (g.dlt_dm_detached, 0.0, max_part);
      fill_real_array (g.dlt_dm_green_dead, 0.0, max_part);
      fill_real_array (g.dlt_dm_senesced_dead, 0.0, max_part);
      fill_real_array (g.dlt_dm_dead_detached, 0.0, max_part);
      fill_real_array (g.dlt_dm_green_retrans, 0.0, max_part);
      g.dlt_dm_parasite_demand = 0.0;
      g.dlt_sw_parasite_demand = 0.0;
      fill_real_array (g.dm_green_demand, 0.0, max_part);
      fill_real_array (g.dm_dead, 0.0, max_part);
      fill_real_array (g.dm_green, 0.0, max_part);
      fill_real_array (g.dm_senesced, 0.0, max_part);
      g.radn_int = 0.0;
      g.radnIntGreenFruit = 0.0;
      g.transp_eff = 0.0;
      g.lai_canopy_green = 0.0;
      g.dlt_slai_detached = 0.0;
      g.dlt_slai_age = 0.0;
      g.dlt_slai_light = 0.0;
      g.dlt_slai_water = 0.0;
      g.dlt_slai_frost = 0.0;
      fill_real_array (g.leaf_no, 0.0, max_node);
      fill_real_array (g.leaf_no_dead, 0.0, max_node);
      g.dlt_leaf_no = 0.0;
      g.dlt_node_no = 0.0;
      g.dlt_leaf_no_pot = 0.0;
      g.dlt_node_no_pot = 0.0;
      g.dlt_leaf_no_dead = 0.0;
      g.leaf_no_final = 0.0;
      fill_real_array (g.leaf_area, 0.0, max_node);
      fill_real_array (g.lai_equilib_light, 0.0, 366+1);
      fill_real_array (g.lai_equilib_water, 0.0, 366+1);
      fill_real_array (g.n_demand , 0.0, max_part);
      fill_real_array (g.n_max , 0.0, max_part);
      fill_real_array (g.dlt_n_green, 0.0, max_part);
      fill_real_array (g.dlt_n_senesced, 0.0, max_part);
      fill_real_array (g.dlt_n_senesced_trans, 0.0, max_part);
      fill_real_array (g.dlt_n_senesced_retrans, 0.0, max_part);
      fill_real_array (g.dlt_n_detached, 0.0, max_part);
      fill_real_array (g.dlt_n_dead, 0.0, max_part);
      fill_real_array (g.dlt_n_dead_detached, 0.0, max_part);
      fill_real_array (g.n_dead, 0.0, max_part);
      fill_real_array (g.n_green, 0.0, max_part);
      fill_real_array (g.n_senesced, 0.0, max_part);
      fill_real_array (g.dlt_n_retrans, 0.0, max_part);
      fill_real_array (g.dlt_no3gsm, 0.0, max_layer);
      fill_real_array (g.dlt_nh4gsm, 0.0, max_layer);
      fill_real_array (g.no3gsm , 0.0, max_layer);
      fill_real_array (g.no3gsm_min, 0.0, max_layer);
      fill_real_array (g.nh4gsm , 0.0, max_layer);
      fill_real_array (g.nh4gsm_min, 0.0, max_layer);

      fill_real_array (g.no3gsm_diffn_pot, 0.0, max_layer);
      fill_real_array (g.no3gsm_mflow_avail, 0.0, max_layer);
      fill_real_array (g.soil_n_demand, 0.0, max_part);
      g.grain_n_supply = 0.0;
      g.n_fix_pot = 0.0;
      fill_real_array (g.no3gsm_uptake_pot, 0.0, max_layer);
      fill_real_array (g.nh4gsm_uptake_pot, 0.0, max_layer);
      g.n_fix_uptake = 0.0;
      g.n_fixed_tops = 0.0;
      fill_real_array (g.n_conc_crit, 0.0, max_part);
      fill_real_array (g.n_conc_max, 0.0, max_part);
      fill_real_array (g.n_conc_min, 0.0, max_part);
      fill_real_array (g.dm_plant_min, 0.0, max_part);
      fill_real_array (g.dlayer , 0.0, max_layer);
      fill_real_array (g.dlt_sw_dep, 0.0, max_layer);
      fill_real_array (g.ll15_dep, 0.0, max_layer);
      fill_real_array (g.dul_dep , 0.0, max_layer);
      fill_real_array (g.sat_dep, 0.0, max_layer);
      fill_real_array (g.bd, 0.0, max_layer);
      fill_real_array (g.sw_dep , 0.0, max_layer);
      g.sw_demand = 0.0;
      g.sw_demand_te = 0.0;
      g.swDemandTEFruit = 0.0;
      g.swSupplyFruit   = 0.0;
      g.swSupplyVeg   = 0.0;
      fill_real_array (g.sw_avail_pot, 0.0, max_layer);
      fill_real_array (g.sw_avail, 0.0, max_layer);
      fill_real_array (g.sw_supply , 0.0, max_layer);

      g.num_layers = 0;
      g.transpiration_tot = 0.0;
      g.n_uptake_tot = 0.0;
      g.n_demand_tot = 0.0;
      g.n_conc_act_stover_tot = 0.0;
      g.n_conc_crit_stover_tot = 0.0;
      g.n_uptake_stover_tot = 0.0;
      g.lai_max = 0.0;
      fill_real_array (g.root_length, 0.0, max_layer);
      fill_real_array (g.root_length_dead, 0.0, max_layer);
      fill_real_array (g.dlt_root_length_dead, 0.0, max_layer);
      fill_real_array (g.dlt_root_length, 0.0, max_layer);
      fill_real_array (g.dlt_root_length_senesced, 0.0, max_layer);
      g.ext_n_demand = 0.0;
      g.ext_sw_demand = 0.0;
      g.leaves_per_node = 0;

      fill_real_array (p.kl, 0.0, max_layer);
      fill_real_array (p.ll_dep, 0.0, max_layer);

      fill_real_array (p.xf, 0.0, max_layer);
      p.uptake_source = "";
      p.eo_crop_factor = 0.0;

      //       plant Constants
      c.n_uptake_option = 0;
      c.leaf_no_pot_option = 0;
      c.partition_option = 0;

      c.sen_start_stage = 0;

      c.no3_uptake_max = 0.0;
      c.no3_conc_half_max = 0.0;

      c.crop_type = "";
      c.default_crop_class = "";
      c.remove_biomass_report = "off";

      c.n_supply_preference = "";
      fill_real_array (c.x_sw_ratio , 0.0, max_table);
      fill_real_array (c.y_sw_fac_root , 0.0, max_table);
      fill_real_array (c.x_ws_root , 0.0, max_table);
      fill_real_array (c.y_ws_root_fac , 0.0, max_table);
      fill_real_array (c.x_sw_demand_ratio , 0.0, max_table);
      fill_real_array (c.y_swdef_leaf , 0.0, max_table);
      fill_real_array (c.x_sw_avail_ratio , 0.0, max_table);
      fill_real_array (c.y_swdef_pheno , 0.0, max_table);
      fill_real_array (c.x_sw_avail_fix , 0.0, max_table);
      fill_real_array (c.y_swdef_fix , 0.0, max_table);
      fill_real_array (c.oxdef_photo , 0.0, max_table);
      fill_real_array (c.oxdef_photo_rtfr, 0.0, max_table);
      c.num_oxdef_photo = 0;
      c.num_sw_ratio = 0;
      c.num_ws_root = 0;
      c.num_sw_demand_ratio = 0;
      c.num_sw_avail_ratio = 0;
      c.num_sw_avail_fix = 0;


      fill_real_array (c.x_lai_ratio, 0.0, max_table);

      fill_real_array (c.y_leaf_no_frac, 0.0, max_table);
      c.num_lai_ratio = 0;
      c.n_conc_crit_root = 0.0;
      c.n_conc_max_root = 0.0;
      c.n_conc_min_root = 0.0;
      fill_real_array (c.x_stage_code, 0.0, max_table);
      fill_real_array (c.y_n_conc_crit_leaf, 0.0, max_table);
      fill_real_array (c.y_n_conc_max_leaf, 0.0, max_table);
      fill_real_array (c.y_n_conc_min_leaf, 0.0, max_table);
      c.n_fact_photo = 0.0;
      c.n_fact_pheno = 0.0;
      c.n_fact_expansion = 0.0;
      fill_real_array (c.n_init_conc, 0.0, max_part);
      fill_real_array (c.n_sen_conc, 0.0, max_part);
      c.num_n_conc_stage = 0;
      fill_real_array (c.x_row_spacing, 0.0, max_table);
      fill_real_array (c.y_extinct_coef, 0.0, max_table);
      fill_real_array (c.y_extinct_coef_dead, 0.0, max_table);
      fill_real_array (c.root_depth_rate, 0.0, max_table);
      c.num_row_spacing = 0;
      c.leaf_no_crit = 0.0;
      c.tt_emerg_limit = 0.0;
      c.days_germ_limit = 0;
      c.swdf_pheno_limit = 0.0;
      c.swdf_photo_limit = 0.0;
      c.swdf_photo_rate = 0.0;
      c.initial_root_depth = 0.0;
      fill_real_array (c.x_lai , 0.0, max_table);
      fill_real_array (c.y_sla_max, 0.0, max_table);
      c.sla_min = 0.0;
      c.initial_tpla = 0.0;
      c.min_tpla = 0.0;
      c.svp_fract = 0.0;
      fill_real_array (c.transp_eff_cf, 0.0, max_table);
      c.num_lai = 0;
      c.grain_n_conc_min = 0.0;
      c.seed_wt_min = 0.0;
      c.leaf_no_at_emerg = 0.0;
      c.no3_diffn_const = 0.0;
      fill_real_array (c.n_fix_rate, 0.0,max_table);
      fill_real_array (c.x_node_no_app, 0.0, max_table);
      fill_real_array (c.y_node_app_rate, 0.0, max_table);
      fill_real_array (c.x_node_no_leaf, 0.0, max_table);
      fill_real_array (c.y_leaves_per_node, 0.0, max_table);
      fill_real_array (c.dm_init, 0.0, max_part);
      c.leaf_init_rate = 0.0;
      c.leaf_no_seed = 0.0;
      fill_real_array (c.dead_detach_frac,0.0,max_part);
      fill_real_array (c.sen_detach_frac,0.0,max_part);
      c.num_node_no_app = 0;
      c.num_node_no_leaf = 0;
      c.swdf_grain_min = 0.0;
      c.hi_min = 0.0;
      c.sfac_slope = 0.0;
      c.tfac_slope = 0.0;
      c.lai_sen_light = 0.0;
      c.sw_fac_max = 0.0;
      fill_real_array (c.x_temp_senescence, 0.0, max_table);
      fill_real_array (c.y_senescence_fac, 0.0, max_table);
      c.temp_fac_min = 0.0;
      c.spla_slope = 0.0;
      c.sen_threshold = 0.0;
      c.sen_rate_water = 0.0;
      c.sen_light_slope = 0.0;
      c.num_temp_senescence = 0;
      c.grn_water_cont = 0.0;
      c.partition_rate_leaf = 0.0;
      fill_real_array (c.frac_leaf,0.0,max_table);
      fill_real_array (c.ratio_root_shoot, 0.0, max_table);
      fill_real_array (c.x_stage_no_partition, 0.0, max_table);
      fill_real_array (c.y_frac_leaf, 0.0, max_table);
      fill_real_array (c.y_ratio_root_shoot, 0.0, max_table);
      c.num_stage_no_partition = 0;
      c.leaf_trans_frac = 0.0;
      c.htstress_coeff = 0.0;
      c.temp_grain_crit_stress = 0.0;
      c.node_sen_rate = 0.0;
      c.fr_lf_sen_rate = 0.0;
      c.n_fact_lf_sen_rate = 0.0;
      c.node_no_correction = 0.0;
      fill_real_array (c.x_node_no, 0.0, max_table);
      fill_real_array (c.y_leaf_size, 0.0, max_table);
      c.num_node_no = 0;
      fill_real_array (c.x_ave_temp, 0.0, max_table);
      fill_real_array (c.y_stress_photo, 0.0, max_table);
      fill_real_array (c.x_weighted_temp, 0.0, max_table);
      fill_real_array (c.y_plant_death, 0.0, max_table);
      c.num_temp = 0;
      c.num_ave_temp = 0;
      c.num_temp_grain = 0;
      c.num_factors = 0;
      c.num_temp_other = 0;
      c.num_weighted_temp = 0;
      c.kl_ub = 0.0;
      c.sw_dep_ub = 0.0;
      c.sw_dep_lb = 0.0;
      c.sw_ub = 0.0;
      c.sw_lb = 0.0;
      c.no3_ub = 0.0;
      c.no3_lb = 0.0;
      c.no3_min_ub = 0.0;
      c.no3_min_lb = 0.0;
      c.nh4_ub = 0.0;
      c.nh4_lb = 0.0;
      c.nh4_min_ub = 0.0;
      c.nh4_min_lb = 0.0;
      c.leaf_no_min = 0.0;
      c.leaf_no_max = 0.0;
      c.latitude_ub = 0.0;
      c.latitude_lb = 0.0;
      c.maxt_ub = 0.0;
      c.maxt_lb = 0.0;
      c.mint_ub = 0.0;
      c.mint_lb = 0.0;
      c.radn_ub = 0.0;
      c.radn_lb = 0.0;
      c.dlayer_ub = 0.0;
      c.dlayer_lb = 0.0;
      c.row_spacing_default = 0.0;
      c.skip_row_default = 0.0;
      c.skip_plant_default = 0.0;
      c.specific_root_length = 0.0;
      c.root_die_back_fr = 0.0;
      fill_real_array (c.x_plant_rld , 0.0, max_table);
      fill_real_array (c.y_rel_root_rate , 0.0, max_table);
      c.num_plant_rld = 0;
      c.class_action.clear();
      c.class_change.clear();
      fill_real_array (c.x_temp_root_advance, 0.0, max_table);
      fill_real_array (c.y_rel_root_advance, 0.0, max_table);
      c.num_temp_root_advance = 0;
      c.eo_crop_factor_default = 0.0;

      // parasite
      g.dlt_dm_parasite_demand =  0.0;
      g.dlt_sw_parasite_demand = 0.0;
      g.dm_parasite_retranslocate = 0.0;
      g.dlt_dm_parasite = 0.0;

#ifdef FRUIT_COHORTS
      // fruit cohorts
      g.setting_fruit = false;
      g.num_fruit_cohorts = 0;
      fill_real_array (g.previous_fruit_stage, 0.0, max_fruit_cohorts);
      fill_real_array (g.current_fruit_stage, 0.0, max_fruit_cohorts);
      fill_real_array (g.dlt_dm_fruit_grain_demand, 0.0, max_fruit_cohorts);
      fill_real_array (g.fruit_no, 0.0, max_fruit_cohorts);
      g.node_no_first_flower= 0.0;
      fill_real_array (g.dlt_dm_daily, 0.0, 366);
      g.fruit_site_no = 0.0;
      g.dlt_fruit_flower_no =  0.0;
      g.dlt_fruit_site_no = 0.0;
      g.swdef_pheno_flower = 0.0;
      g.swdef_pheno_grainfill = 0.0;
      for (int i = 0; i < max_fruit_cohorts; i++) {
         for (int j = 0; j < max_fruit_stage; j++) {
           g.fruit_days_tot[i][j] =  0.0;
           g.fruit_phase_tt[i][j] =  0.0;
           g.fruit_tt_tot[i][j] =  0.0;
         }
         for (int j = 0; j < max_part; j++) {
           g.dm_fruit_green[i][j] =  0.0;
           g.dlt_dm_fruit_green[i][j] =  0.0;
           g.dlt_dm_fruit_senesced[i][j]  =  0.0;
           g.dlt_dm_fruit_abort[i][j]  =  0.0;
           g.dm_fruit_dead[i][j] = 0.0;
           g.dm_fruit_senesced[i][j] = 0.0;
           g.dlt_dm_fruit_green_retrans[i][j] =  0.0;
         }
         for (int j = 0; j < 366; j++) {
           g.fruit_sdr_daily[i][j] = 0.0;
         }
      }

      fill_real_array(g.dlt_fruit_tt, 0.0, max_fruit_cohorts);
      fill_real_array(g.dlt_fruit_no, 0.0, max_fruit_cohorts);
      fill_real_array(g.dlt_fruit_stage, 0.0, max_fruit_cohorts);
      fill_real_array (g.fruit_phase_devel, 0.0, max_fruit_cohorts);

      fill_real_array (g.fruit_sdr, 0.0, max_fruit_cohorts);
      fill_real_array (g.dlt_fruit_no_abort, 0.0, max_fruit_cohorts);
      fill_real_array (g.dlt_dm_green_abort, 0.0, max_part);

       // fruit cohorts
      fill_real_array (p.x_node_no_fruit_sites, 0.0,max_table);
      fill_real_array (p.y_fruit_sites_per_node, 0.0,max_table);
      p.dm_fruit_set_min =  0.0;
      p.num_node_no_fruit_sites  =  0;
      fill_real_array (p.x_pp_fruit_start_to_end_grain, 0.0 ,max_table);
      fill_real_array (p.y_tt_fruit_start_to_end_grain, 0.0 ,max_table);
      fill_real_array (p.x_stage_sdr_min, 0.0 ,max_table);
      fill_real_array (p.y_sdr_min, 0.0 ,max_table);
      p.num_sdr_min = 0;

      p.num_pp_fruit_start_to_end_grain = 0;
      fill_integer_array (p.fruit_stage_no_partition, 0,max_table);

      p.num_fruit_stage_no_partition =  0;
      p.dm_fruit_max =  0.0;
      p.dm_fruit_set_crit = 0.0;
      p.potential_fruit_filling_rate =  0.0;
      p.cutout_fract = 0.0;

      fill_real_array (c.x_temp_fruit_site,0.0, max_table);
      fill_real_array (c.y_rel_fruit_site,0.0, max_table);
      c.num_temp_fruit_site  = 0;
      c.fruit_no_option = 0;
      fill_real_array (c.x_sw_avail_ratio_flower, 0.0, max_table);
      fill_real_array (c.y_swdef_pheno_flower, 0.0, max_table);
      c.num_sw_avail_ratio_flower = 0;

      fill_real_array (c.x_sw_avail_ratio_grainfill, 0.0, max_table);
      fill_real_array (c.y_swdef_pheno_grainfill, 0.0, max_table);
      c.num_sw_avail_ratio_grainfill = 0;
      c.days_assimilate_ave = 0.0;
      c.dm_abort_fract = 0.0;
      c.fruit_phen_end = 0.0;
      c.fract_dm_fruit_abort_crit = 0.0;
      c.swdef_pheno_flower = 0.0;
      c.swdef_pheno_grainfill = 0.0;
#endif

      fill_real_array (c.x_sw_avail_ratio_flower, 0.0, max_table);
      fill_real_array (c.y_swdef_pheno_flower, 0.0, max_table);
      c.num_sw_avail_ratio_flower = 0;

      fill_real_array (c.x_sw_avail_ratio_grainfill, 0.0, max_table);
      fill_real_array (c.y_swdef_pheno_grainfill, 0.0, max_table);
      c.num_sw_avail_ratio_grainfill = 0;

      p.root_distribution_pattern = 0.0;
      c.root_growth_option =  0;


      fill_real_array (c.x_co2_te_modifier, 0.0, max_table);
      fill_real_array (c.y_co2_te_modifier, 0.0, max_table);
      c.num_co2_te_modifier = 0;

      fill_real_array (c.x_co2_nconc_modifier, 0.0, max_table);
      fill_real_array (c.y_co2_nconc_modifier, 0.0, max_table);
      c.num_co2_nconc_modifier = 0;

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

    g.plant_status_out_today = false;

   for (vector<plantThing *>::iterator t = myThings.begin();
        t != myThings.end();
        t++)
       (*t)->zeroAllGlobals();

    fill_real_array (g.leaf_no , 0.0, max_node);
    g.node_no = 0.0;
    fill_real_array (g.leaf_no_dead , 0.0, max_node);

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
    fill_real_array (g.p_green , 0.0, max_part);
    fill_real_array (g.p_dead , 0.0, max_part);
    fill_real_array (g.p_sen , 0.0, max_part);
    fill_real_array (g.dlt_n_senesced_trans , 0.0, max_part);
    fill_real_array (g.dlt_n_senesced_retrans , 0.0, max_part);

    fill_real_array (g.leaf_area , 0.0, max_node);
//    fill_real_array (p.ll_dep , 0.0, max_layer);
    fill_real_array (p.xf , 0.0, max_layer);
    fill_real_array (g.root_length , 0.0, max_layer);
    fill_real_array (g.root_length_dead, 0.0, max_layer);
    fill_real_array (g.dlt_root_length_dead, 0.0, max_layer);

    g.leaves_per_node = 0.0;

    g.dlt_plants_death_external     = 0.0;
    g.lai_max               = 0.0;

    g.plants                = 0.0;
    g.root_depth            = 0.0;
    g.canopy_width         = 0.0;
    g.leaf_no_final         = 0.0;
    g.n_conc_act_stover_tot = 0.0;
    g.n_conc_crit_stover_tot = 0.0;
    g.n_demand_tot          = 0.0;
    g.n_uptake_stover_tot   = 0.0;
    g.n_uptake_tot          = 0.0;
    g.transpiration_tot     = 0.0;

    g.skip_row              = 0.0;
    g.skip_row_fac          = 0.0;
    g.skip_plant            = 0.0;
    g.skip_plant_fac        = 0.0;
    g.row_spacing           = 0.0;

    g.cover_green           = 0.0;
    g.cover_sen             = 0.0;
    g.cover_dead            = 0.0;

    g.swdef_pheno = 1.0;
    g.swdef_photo = 1.0;
    g.swdef_expansion = 1.0;
    g.swdef_fixation = 1.0;
    g.nfact_pheno = 1.0;
    g.nfact_photo = 1.0;
    g.nfact_grain_conc = 1.0;

//    g.remove_biom_pheno = 1.0;

    g.n_fix_pot = 0.0;
    g.n_fix_uptake = 0.0;
    g.n_fixed_tops = 0.0;

    g.dm_parasite_retranslocate   = 0.0;

    g.pfact_photo        = 1.0;
    g.pfact_expansion    = 1.0;
    g.pfact_pheno        = 1.0;
    g.pfact_grain        = 1.0;

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

   for (vector<plantThing *>::iterator t = myThings.begin();
        t != myThings.end();
        t++)
       (*t)->zeroDeltas();


    fill_real_array (g.dlt_dm_green , 0.0, max_part);
    fill_real_array (g.dlt_dm_green_retrans, 0.0, max_part);
    fill_real_array (g.dlt_n_green , 0.0, max_part);
    fill_real_array (g.dlt_n_retrans , 0.0, max_part);
    fill_real_array (g.dlt_no3gsm , 0.0, max_layer);
    fill_real_array (g.dlt_nh4gsm , 0.0, max_layer);
    fill_real_array (g.dlt_sw_dep , 0.0, max_layer);
    fill_real_array (g.dm_green_demand , 0.0, max_part);
    fill_real_array (g.n_demand , 0.0, max_part);
    g.grain_n_supply = 0.0;
    fill_real_array (g.soil_n_demand , 0.0, max_part);

    fill_real_array (g.dlt_dm_dead_detached, 0.0, max_part);
    fill_real_array (g.dlt_dm_detached , 0.0, max_part);
    fill_real_array (g.dlt_dm_senesced , 0.0, max_part);
    fill_real_array (g.dlt_dm_green_dead , 0.0, max_part);
    fill_real_array (g.dlt_dm_senesced_dead , 0.0, max_part);
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

    g.swSupplyFruit   = 0.0;
    g.swSupplyVeg   = 0.0;
    g.dlt_slai_detached        = 0.0;
    g.dlt_dm                   = 0.0;
    g.dlt_leaf_no              = 0.0;
//    g.dlt_node_no              = 0.0;JNGH - need to carry this through for site no next day.
    g.dlt_leaf_no_pot          = 0.0;
    g.dlt_node_no_pot          = 0.0;
    g.dlt_leaf_no_dead         = 0.0;
    g.dlt_plants               = 0.0;
    g.dlt_root_depth           = 0.0;
    g.sw_demand                = 0.0;
    g.sw_demand_te             = 0.0;
    g.swDemandTEFruit             = 0.0;
    g.ext_n_demand             = 0.0;

//      g%dlt_plants_death_barrenness     = 0.0
    g.dlt_plants_death_seedling       = 0.0;
    g.dlt_plants_death_drought        = 0.0;
    g.dlt_plants_failure_phen_delay   = 0.0;
    g.dlt_plants_failure_leaf_sen     = 0.0;
    g.dlt_plants_failure_emergence    = 0.0;
    g.dlt_plants_failure_germ         = 0.0;
//      g%dlt_plants_death_external       = 0.0

//      g.dlt_dm_pot_rue = 0.0;
//      g.dlt_dm_pot_te  = 0.0;
//      g%radn_int = 0.0

//      g.dlt_slai_age    = 0.0;
//      g.dlt_slai_light  = 0.0;
//      g.dlt_slai_water  = 0.0;
//      g.dlt_slai_frost  = 0.0;
    //g.pfact_photo        = 1.0;  NO!! needed during prepare()
    //g.pfact_expansion    = 1.0;
    //g.pfact_pheno        = 1.0;
    //g.pfact_grain        = 1.0;

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

    // initialize crop variables
    plant_get_site_characteristics();

    g.plant_status = out;
    g.module_name = parent->getName();

    vector<float> ll;
    if (parent->readParameter ("parameters"
                              , "ll"//, "()"
                              , ll, 0.0, c.sw_ub, true))
       {
       for (unsigned int layer = 0; layer != ll.size(); layer++)
          p.ll_dep[layer] = ll[layer]*g.dlayer[layer];

       if ((int)ll.size() != g.num_layers)
          parent->warningError ("LL parameter doesn't match soil profile?");
       }
    else
       {
       unsigned int id = parent->addRegistration(RegistrationType::get,
                                                 "ll15", floatArrayType,
                                                 "", "");
       parent->getVariable(id, ll, 0.0, c.sw_ub, true);
       if (ll.size() == 0)
          throw std::runtime_error("No Crop Lower Limit found");

       for (unsigned int i=0; i< ll.size(); i++) p.ll_dep[i] = ll[i]*g.dlayer[i];
       parent->writeString ("   Using externally supplied Lower Limit (ll15)");
       }

    doPInit(parent);
    fruitPart->doInit(parent, phenology);

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
    char  msg[200];                               // output string
    FString  dummy;                               // dummy variable
    float  sowing_depth;                          // sowing depth for reporting

//- Implementation Section ----------------------------------

    push_routine (my_name);

    if (g.plant_status == out)
    {
         if (g.plant_status_out_today == false)
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

           if (incomingApsimVariant.get("sowing_depth", protocol::DTsingle, false, sowing_depth) == false)
               {
               throw std::invalid_argument("sowing_depth not specified");
               }
           bound_check_real_var(parent,sowing_depth, 10.0, 200.0, "sowing_depth");

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
           g.plant_status = alive;
           sendStageMessage("sowing");
           for (vector<plantThing *>::iterator t = myThings.begin(); t != myThings.end(); t++)
             (*t)->onPlantEvent("sowing");

           parent->writeString ("");
           parent->writeString ("                 Crop Sowing Data");
           parent->writeString ("    ------------------------------------------------");
           parent->writeString ("    Sowing  Depth Plants Spacing Skip  Skip  Cultivar");
           parent->writeString ("    Day no   mm     m^2     mm   row   plant name");
           parent->writeString ("    ------------------------------------------------");

           sprintf(msg, "   %7d%7.1f%7.1f%7.1f%6.1f%6.1f %s"
                  , g.day_of_year, sowing_depth
                  , g.plants, g.row_spacing
                  , g.skip_row, g.skip_plant, g.cultivar.c_str());
           parent->writeString (msg);

           parent->writeString ("    ------------------------------------------------\n");

         }

         else
         {
            ostrstream msg;
            msg << g.module_name << " was taken out today by \"end_crop\" action -" << endl;
            msg << " Unable to accept \"sow\" action until the next day." << endl << ends;
            throw std::runtime_error (msg.str());
         }
    }
    else
    {
         ostrstream msg;
         msg << g.module_name << " is still in the ground -" << endl;
         msg << " Unable to sow until it is taken out by \"end_crop\" action." << endl << ends;
         throw std::runtime_error (msg.str());

//        string m = string(g.module_name + " is still in the ground -\n unable to sow until it is\n taken out by \"end_crop\" action.");
//        throw std::runtime_error (m.c_str());
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

    //  plant thing initialisations
    for (vector<plantThing *>::iterator t = myThings.begin();
         t != myThings.end();
         t++)
       {
       (*t)->readCultivarParameters(parent, g.cultivar);
       }

    // report
    parent->writeString ("    ------------------------------------------------");

    sprintf (msg, "   %s%s",  "cultivar                   = ", g.cultivar.c_str());
    parent->writeString (msg);

    phenology->writeCultivarInfo(parent);
    fruitPart->writeCultivarInfo(parent);


#if 0
XXX
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
#endif

    parent->writeString ("    ------------------------------------------------\n\n");

    pop_routine (my_name);
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
    vector<float> ll ;                           // lower limit of plant-extractable
                                                 // soil water for soil layer l
                                                 // (mm water/mm soil)
    float dep_tot, esw_tot;                      // total depth of soil & ll
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

//       plant_sw_supply
    p.uptake_source = parent->readParameter (section_name, "uptake_source");
    if (p.uptake_source == "")
        {
        p.uptake_source = "calc";
        }

    if (parent->readParameter (section_name
                              , "ll"//, "()"
                              , ll, 0.0, c.sw_ub, true))
       {
       for (unsigned int layer = 0; layer != ll.size(); layer++)
          p.ll_dep[layer] = ll[layer]*g.dlayer[layer];

       if ((int)ll.size() != g.num_layers)
          parent->warningError ("LL parameter doesn't match soil profile?");
       }
    else
       {
       unsigned int id = parent->addRegistration(RegistrationType::get,
                                                 "ll15", floatArrayType,
                                                 "", "");
       parent->getVariable(id, ll, 0.0, c.sw_ub, true);
       if (ll.size() == 0)
          throw std::runtime_error("No Crop Lower Limit found");

       for (unsigned int i=0; i< ll.size(); i++) p.ll_dep[i] = ll[i]*g.dlayer[i];
       parent->writeString ("   Using externally supplied Lower Limit (ll15)");
       }

    int num_kls = 0;
    parent->readParameter (section_name
               , "kl"//, "()"
               , p.kl, num_kls
               , 0.0, c.kl_ub);
    if (num_kls != g.num_layers)
       parent->warningError ("KL parameter doesn't match soil profile?");

    int num_xfs = 0;
    parent->readParameter (section_name
              , "xf"//, "()"
              , p.xf, num_xfs
              , 0.0, 1.0);
    if (num_xfs != g.num_layers)
       parent->warningError ("XF parameter doesn't match soil profile?");

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
    for (int layer = 0; layer < g.num_layers; layer++)
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
          , fract2pcnt * divide(esw_tot, dep_tot, 0.0));
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
    float p_residue;                              // phosphorus added to residue (g/m^2)
    float dm_root;                                // dry matter added to soil (g/m^2)
    float n_root;                                 // nitrogen added to soil (g/m^2)
    float p_root;                                 // phosphorus added to soil (g/m^2)
    char  msg[400];
    float yield;                                  // grain wt (kg/ha)
    int part;                                     // part
    float  incorp_fr;
    float  P_tops;                // Phosphorus added to residue (g/m^2)

    push_routine (my_name);

    if (g.plant_status != out)
        {
        g.plant_status_out_today = true;
        g.plant_status = out;

        stageObservers.reset();
        otherObservers.reset();

        // report
        yield = fruitPart->dmGrainTotal() * gm2kg / sm2ha;
        sprintf (msg, "Crop ended. Yield (dw) = %7.1f  (kg/ha)", yield);
        parent->writeString (msg);

        // now do post harvest processes
        dm_root = g.dm_green[root] + g.dm_senesced[root];
        n_root  = g.n_green[root] + g.n_senesced[root];
        p_root  = g.p_green[root] + g.p_sen[root];

        plant_root_incorp (dm_root, n_root, p_root, g.root_length);
        plant_root_incorp (g.dm_dead[root], g.n_dead[root], g.p_dead[root], g.root_length_dead);

        // put stover and any remaining grain into surface residue
        dm_residue =topsTot();
        n_residue =topsNTot();
        p_residue =topsPTot();

        dm_root = g.dm_green[root] + g.dm_dead[root] + g.dm_senesced[root];
        n_root  = g.n_green[root] + g.n_dead[root] + g.n_senesced[root];
        p_root  = g.p_green[root] + g.p_dead[root] + g.p_sen[root];

       if (dm_residue > 0.0)
          {
          // Build surface residues by part
          vector<string> part_name;
          vector<float> fraction_to_residue;           // fraction sent to residue (0-1)
          vector<float> dlt_dm_crop;                   // change in dry matter of crop (kg/ha)
          vector<float> dlt_dm_n;                      // N content of changeed dry matter (kg/ha)
          vector<float> dlt_dm_p;                      // P content of changeed dry matter (kg/ha)

//          char *names[] = {"root", "pod","meal", "oil"};
//          float fracts[] = {0.0, 1.0, 1.0, 1.0};
          char *names[] = {"root"};
          float fracts[] = {0.0};
          for (part = 0; part < 1; part++)
              {
              part_name.push_back(names[part]);
              dlt_dm_crop.push_back((g.dm_green[part]
                                    + g.dm_senesced[part]
                                    + g.dm_dead[part]) * gm2kg/sm2ha);
              dlt_dm_n.push_back((g.n_green[part]
                                 + g.n_senesced[part]
                                 + g.n_dead[part]) * gm2kg/sm2ha);
              dlt_dm_p.push_back((g.p_green[part]
                                 + g.p_sen[part]
                                 + g.p_dead[part]) * gm2kg/sm2ha);

              fraction_to_residue.push_back(fracts[part]);
              }

          for (vector<plantPart *>::iterator t = myParts.begin();
             t != myParts.end();
             t++)
           (*t)->onEndCrop(part_name,
                             dlt_dm_crop,
                             dlt_dm_n,
                             dlt_dm_p,
                             fraction_to_residue);

          plant_send_crop_chopped_event ( c.crop_type
                                         , part_name
                                         , dlt_dm_crop
                                         , dlt_dm_n
                                         , dlt_dm_p
                                         , fraction_to_residue);
          }

        parent->writeString ("    Organic matter from crop:-      Tops to surface residue      Roots to soil FOM");

        sprintf (msg, "%48s%7.2f%24.2f"
                           , "DM (kg/ha) =               ", dm_residue * gm2kg /sm2ha, dm_root * gm2kg /sm2ha);
        parent->writeString (msg);

        sprintf (msg, "%48s%7.2f%24.2f"
                           , "N  (kg/ha) =               ", n_residue * gm2kg /sm2ha, n_root * gm2kg /sm2ha);
        parent->writeString (msg);

        if (g.phosphorus_aware)
           {
           sprintf (msg, "%48s%7.2f%24.2f"
                           , "P  (kg/ha) =               ", p_residue * gm2kg /sm2ha, p_root * gm2kg /sm2ha);
           parent->writeString (msg);
           }

        parent->writeString (" ");

        for (vector<plantThing *>::iterator t = myThings.begin(); t != myThings.end(); t++)
           (*t)->onPlantEvent("end_crop");

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
       soil_temp = 0.0;
       if (parent->getVariable(id.maxt_soil_surface, soil_temp, 0.0, 80.0, true));
          {
          plant_store_value (g.day_of_year
                           , g.year
                           , g.soil_temp
                           , soil_temp);
          }
       }

    parent->getVariable(id.fr_intc_radn, g.fr_intc_radn, 0.0, 1.0, true);

    // Soilwat2
    parent->getVariable(id.eo, g.eo, 0.0, 20.0);

    values.clear();
    parent->getVariable(id.sw_dep, values, c.sw_dep_lb, c.sw_dep_ub);
    for (unsigned int i=0; i< values.size(); i++) {
    	g.sw_dep[i] = values[i];
    }
    //assert (values.size() == g.num_layers);

    values.clear();
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

    values.clear();
    parent->getVariable(id.no3_min, values, c.no3_min_lb, c.no3_min_ub, true);
    for (int i = 0; i < g.num_layers; i++)
       {
       g.no3gsm_min[i] = values[i] * kg2gm /ha2sm;
       }

    values.clear();
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

    values.clear();
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
                           float *uptake_array)              //(OUTPUT) crop uptake array

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
    plantPart *part;
    vector<plantPart*> parts;

    vector<string> part_name;
    vector<float> fraction_to_residue;           // fraction sent to residue (0-1)
    vector<float> dm_residue;                   // change in dry matter of crop (kg/ha)
    vector<float> dm_n;                      // N content of changeed dry matter (kg/ha)
    vector<float> dm_p;                      // P content of changeed dry matter (kg/ha)

    float P_tops;                // Phosphorus added to residue (g/m^2)
    int   layer;
    float root_length[max_layer];
    float incorp_fr[max_part];

//- Implementation Section ----------------------------------
    push_routine (my_name);

    // DANGER WILL ROBINSON make sure this is right
    //char *names[] = {"root", "leaf", "stem", "pod","meal", "oil"};
    //    float fracts[] = {0.0, 1.0, 1.0, 1.0, 1.0, 1.0};
    setupHacks(parts);

    // dispose of detached material from senesced parts in the live population
    for (unsigned int ipart = 0; ipart != parts.size(); ipart++)
       {
       part = parts[ipart];
       part->collectDetachedForResidue(part_name
                                      , dm_residue
                                      , dm_n
                                      , dm_p
                                      , fraction_to_residue);
       }
       fraction_to_residue[root] = 0.0;  //reset root fraction to residue;

    if (sum(dm_residue) > 0.0)
          {
          plant_send_crop_chopped_event ( c.crop_type
                                         , part_name
                                         , dm_residue
                                         , dm_n
                                         , dm_p
                                         , fraction_to_residue);
          }

    // put live population roots into root residue
    // correct root length for change in root length in update
    for (layer = 0; layer < max_layer; layer++)
        {
        root_length[layer] = g.root_length[layer] + g.dlt_root_length_dead[layer];
        }

    plant_root_incorp (g.dlt_dm_detached[root]
                       , g.dlt_n_detached[root]
                       , g.dlt_p_det[root]
                       , root_length);

    // now dispose of dead population detachments
    dm_residue.clear();
    dm_n.clear();
    dm_p.clear();
    fraction_to_residue.clear();

    for (unsigned int ipart = 0; ipart != parts.size(); ipart++)
       {
       part = parts[ipart];
       part->collectDeadDetachedForResidue(part_name
                                         , dm_residue
                                         , dm_n
                                         , dm_p
                                         , fraction_to_residue);
       }
       fraction_to_residue[root] = 0.0;  //reset root fraction to residue;

    if (sum(dm_residue) > 0.0)
       {
       plant_send_crop_chopped_event ( c.crop_type
                                      , part_name
                                      , dm_residue
                                      , dm_n
                                      , dm_p
                                      , fraction_to_residue);
       }



    // put dead population roots into root residue

    // correct root length for change in root length in update
    for (layer = 0; layer < max_layer; layer++)
        {
        root_length[layer] = g.root_length_dead[layer] - g.dlt_root_length_dead[layer];
        }

    plant_root_incorp (g.dlt_dm_dead_detached[root]
                       , g.dlt_n_dead_detached[root]
                       , g.dlt_p_dead_det[root]
                       , root_length);

    deleteHacks(parts);
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

    //string scratch = parent->readParameter (section_name, "part_names");
    //Split_string(scratch, " ", c.part_names);

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
    , -60.0, 60.0);

    parent->readParameter (section_name
    , "maxt_lb"//, "(oc)"
    , c.maxt_lb
    , -60.0, 60.0);

    parent->readParameter (section_name
    , "mint_ub"//, "(oc)"
    , c.mint_ub
    , -60.0, 40.0);

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

    for (vector<plantThing *>::iterator t = myThings.begin();
         t != myThings.end();
         t++)
      (*t)->readConstants(parent, section_name);

    if (g.phosphorus_aware)
       read_p_constants(parent);

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

    plant_co2_modifier_rue ();
    plant_co2_modifier_te ();
    plant_co2_modifier_n_conc ();
    plant_vpd (c.svp_fract, g.maxt, g.mint);

    for (vector<plantPart *>::iterator t = myParts.begin();
         t != myParts.end();
         t++)
       (*t)->prepare();

    plant_nit_stress (c.n_stress_option);
    plant_temp_stress (1);
    plant_light_supply_partition (1);
    plant_bio_rue (1);
    plant_transpiration_eff (1);
    plant_water_demand (1);
    plant_nit_demand_est(1);

    // Note actually should send total plant
    // potential growth rather than just tops - NIH
    prepare_p();

    pop_routine (myname);
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

    scratch = parent->readParameter (search_order, "class_action");
    Split_string(scratch, " ", c.class_action);
    registerClassActions();

    scratch = parent->readParameter (search_order, "class_change");
    Split_string(scratch, " ", c.class_change);

    parent->readParameter (search_order, "root_growth_option"//, "()"
                      , c.root_growth_option
                      , 1, 2);

   for (vector<plantThing *>::iterator t = myThings.begin();
        t != myThings.end();
        t++)
      (*t)->readSpeciesParameters(parent, search_order);



    c.rue.search(parent, search_order,
                 "x_stage_rue", "()", 0.0, 1000.0,
                 "y_rue", "(g dm/mj)", 0.0, 1000.0);

    parent->readParameter (search_order,
                      "root_depth_rate"//, "(mm)"
                     , c.root_depth_rate, numvals
                     , 0.0, 1000.0);

    parent->readParameter (search_order,
                      "n_fix_rate"//, "()"
                     , c.n_fix_rate, numvals
                     , 0.0, 1.0);

    parent->readParameter (search_order,
                      "transp_eff_cf"//, "(kpa)"
                     , c.transp_eff_cf, numvals
                     , 0.0, 1.0);

    parent->readParameter (search_order,
                       "partition_option"//, "()"
                      , c.partition_option
                      , 1, 3);

    if (c.partition_option==1 )
        {
        parent->readParameter (search_order
                         ,"frac_leaf"//,  "()"
                         , c.frac_leaf, numvals
                         , 0.0, 1.0);

        parent->readParameter (search_order
                         ,"ratio_root_shoot"//, "()"
                         , c.ratio_root_shoot, numvals
                         , 0.0, 1000.0);

        }
    else if (c.partition_option==2)
        {
        parent->readParameter (search_order
                         ,"x_stage_no_partition"//, "()"
                         , c.x_stage_no_partition
                         , c.num_stage_no_partition
                         , 0.0, 20.0);

        parent->readParameter (search_order
                         ,"y_frac_leaf"//,  "()"
                         , c.y_frac_leaf, numvals
                         , 0.0, 1.0);
        parent->readParameter (search_order
                         ,"y_ratio_root_shoot"//, "()"
                         , c.y_ratio_root_shoot, numvals
                         , 0.0, 1000.0);
        }
    else if (c.partition_option==3)
        {
        parent->readParameter (search_order
                         ,"x_stage_no_partition"//,  "()"
                         , c.x_stage_no_partition
                         , c.num_stage_no_partition
                         , 0.0, 20.0);

        parent->readParameter (search_order
                         ,"y_frac_leaf"//, "()"
                         , c.y_frac_leaf, numvals
                         , 0.0, 1.0);
        parent->readParameter (search_order
                         ,"y_ratio_root_shoot"//, "()"
                         , c.y_ratio_root_shoot, numvals
                         , 0.0, 1000.0);

        }

    parent->readParameter (search_order
                   ,"row_spacing_default"//, "(mm)"
                   , c.row_spacing_default
                   , 0.0, 2000.);

    parent->readParameter (search_order
                   ,"skiprow_default"//, "()"
                   , c.skip_row_default
                   , 0.0, 2.0);

    parent->readParameter (search_order
                     ,"x_row_spacing"//,  "(mm)"
                     , c.x_row_spacing, c.num_row_spacing
                     , 0.0, 2000.);

    parent->readParameter (search_order
                     ,"y_extinct_coef"//, "()"
                     , c.y_extinct_coef, c.num_row_spacing
                     , 0.0, 1.0);

    parent->readParameter (search_order
                     ,"y_extinct_coef_dead"//, "()"
                     , c.y_extinct_coef_dead, c.num_row_spacing
                     , 0.0, 1.0);

// crop failure

    parent->readParameter (search_order
                   ,"leaf_no_crit"//, "()"
                   , c.leaf_no_crit
                   , 0.0, 100.0);

    parent->readParameter (search_order
                   ,"tt_emerg_limit"//, "(oc)"
                   , c.tt_emerg_limit
                   , 0.0, 1000.0);

    parent->readParameter (search_order
                   ,"days_germ_limit"//, "(days)"
                   , c.days_germ_limit
                   , 0.0, 365.0);

    parent->readParameter (search_order
                   ,"swdf_pheno_limit"//, "()"
                   , c.swdf_pheno_limit
                   , 0.0, 1000.0);

    parent->readParameter (search_order
                   ,"swdf_photo_limit"//, "()"
                   , c.swdf_photo_limit
                   , 0.0, 1000.0);

    parent->readParameter (search_order
                   ,"swdf_photo_rate"//, "()"
                   , c.swdf_photo_rate
                   , 0.0, 1.0);

//    plant_root_depth

    parent->readParameter (search_order
                   ,"initial_root_depth"//, "(mm)"
                   , c.initial_root_depth
                   , 0.0, 1000.0);

//    plant_root_length

    parent->readParameter (search_order
                   ,"specific_root_length"//, "(mm/g)"
                   , c.specific_root_length
                   , 0.0, 1.0e6);

    parent->readParameter (search_order
                   ,"root_die_back_fr"//, "(0-1)"
                   , c.root_die_back_fr
                   , 0.0, 0.99);

    parent->readParameter (search_order
                     ,"x_plant_rld"//,  "()"
                     , c.x_plant_rld, c.num_plant_rld
                     , 0.0, 0.1);

    parent->readParameter (search_order
                     ,"y_rel_root_rate"//, "()"
                     , c.y_rel_root_rate, c.num_plant_rld
                     , 0.001, 1.0);

//    plant_leaf_area_init
    parent->readParameter (search_order
                   ,"initial_tpla"//, "(mm^2)"
                   , c.initial_tpla
                   , 0.0, 100000.0);

    parent->readParameter (search_order
                   ,"min_tpla"//, "(mm^2)"
                   , c.min_tpla
                   , 0.0, 100000.0);

// TEMPLATE OPTION
//    plant_leaf_area

    parent->readParameter (search_order
                     ,"x_lai"//, "(mm2/mm2)"
                     , c.x_lai, c.num_lai
                     , 0.0, 15.0);

    parent->readParameter (search_order
                     ,"y_sla_max"//, "(mm2/g)"
                     , c.y_sla_max, c.num_lai
                     , 0.0, 2.e5);

    parent->readParameter (search_order
                     ,"x_lai_ratio"//, "()"
                     , c.x_lai_ratio, c.num_lai_ratio
                     , 0.0, 1.0);

    parent->readParameter (search_order
                     ,"y_leaf_no_frac"//, "()"
                     , c.y_leaf_no_frac, c.num_lai_ratio
                     , 0.0, 1.0);

//    plant_transp_eff

    parent->readParameter (search_order
                   ,"svp_fract"//, "()"
                   , c.svp_fract
                   , 0.0, 1.0);

//    cproc_sw_demand_bound

    parent->readParameter (search_order
                   ,"eo_crop_factor_default"//, "()"
                   , c.eo_crop_factor_default
                   , 0.0, 100.);

//    plant_leaf_appearance

    parent->readParameter (search_order
                   ,"leaf_no_at_emerg"//, "()"
                   , c.leaf_no_at_emerg
                   , 0.0, 100.0);

//    plant_n_uptake

    parent->readParameter (search_order
                      ,"n_uptake_option"//, "()"
                      , c.n_uptake_option
                      , 1, 3);

    if (c.n_uptake_option==1)
        {
        parent->readParameter (search_order
                       ,"no3_diffn_const"//, "(days)"
                       , c.no3_diffn_const
                       , 0.0, 100.0);
        }
    else if (c.n_uptake_option==2)
        {
        parent->readParameter (search_order
                       ,"no3_uptake_max"//, "(g/mm)"
                       , c.no3_uptake_max
                       , 0.0, 1.0);

        parent->readParameter (search_order
                       ,"no3_conc_half_max"//, "(ppm)"
                       , c.no3_conc_half_max
                       , 0.0, 100.0);

        parent->readParameter (search_order
                       , "total_n_uptake_max"//, "(g/m2)"
                       , c.total_n_uptake_max
                       , 0.0, 100.0);
        }
     else if (c.n_uptake_option==3)
         {
         parent->readParameter (search_order
                       , "kno3"//, "(/day)"
                       , c.kno3
                       , 0.0, 1.0);

         parent->readParameter (search_order
                       , "no3ppm_min"//, "(ppm)"
                       , c.no3ppm_min
                       , 0.0, 10.0);

         parent->readParameter (search_order
                       , "knh4"//, "(/day)"
                       , c.knh4
                       , 0.0, 1.0);

         parent->readParameter (search_order
                       , "nh4ppm_min"//, "(ppm)"
                       , c.nh4ppm_min
                       , 0.0, 10.0);

         parent->readParameter (search_order
                        , "total_n_uptake_max"//, "(g/m2)"
                        , c.total_n_uptake_max
                        , 0.0, 100.0);
         }
     else
         {
         	// Unknown N uptake option
         }
    c.n_supply_preference = parent->readParameter (search_order, "n_supply_preference");

    //    plant_phenology_init
    parent->readParameter (search_order,
                       "leaf_no_pot_option"//, "()"
                      , c.leaf_no_pot_option
                      , 1, 2);

    parent->readParameter (search_order,
                      "x_node_no_app"//, "()"
                     , c.x_node_no_app, c.num_node_no_app
                     , 0.0, 200.);
    parent->readParameter (search_order,
                      "y_node_app_rate"//,  "()"
                     , c.y_node_app_rate, c.num_node_no_app
                     , 0.0, 400.);

    parent->readParameter (search_order,
                      "x_node_no_leaf"//,  "()"
                     , c.x_node_no_leaf, c.num_node_no_leaf
                     , 0.0, 200.);
    parent->readParameter (search_order,
                      "y_leaves_per_node"//,  "()"
                     , c.y_leaves_per_node, c.num_node_no_leaf
                     , 0.0, 50.);

    //    plant_dm_init
    parent->readParameter (search_order
                     ,"dm_init"//,  "(g/plant)"
                     , c.dm_init, numvals
                     , 0.0, 1.0);

     //    plant_get_root_params
    parent->readParameter (search_order
                   ,"kl_ub"//, "()"
                   , c.kl_ub
                   , 0.0, 1000.0);

     //    plant_n_dlt_grain_conc
     parent->readParameter (search_order
                        , "grain_n_option"//, "()"
                        , c.grain_n_option
                        , 1, 2);

     if (c.grain_n_option==1)
         {
         parent->readParameter (search_order
                        ,"sw_fac_max"//, "()"
                        , c.sw_fac_max
                        , 0.0, 100.0);

         parent->readParameter (search_order
                        ,"temp_fac_min"//, "()"
                        , c.temp_fac_min
                        , 0.0, 100.0);

         parent->readParameter (search_order
                        ,"sfac_slope"//, "()"
                        , c.sfac_slope
                        , -10.0, 0.0);

         parent->readParameter (search_order
                        ,"tfac_slope"//, "()"
                        , c.tfac_slope
                        , 0.0, 100.0);
         }
     else
         { // do nothing - read in fruitPart
         }

     parent->readParameter (search_order
                        , "n_retrans_option"//, "()"
                        , c.n_retrans_option
                        , 1, 2);
     if (c.n_retrans_option==1)
         {
         //Nothing..
         }
     else
         {
         parent->readParameter (search_order
                        , "n_retrans_fraction"//, "()"
                        , c.n_retrans_fraction
                        , 0.0, 1.0);

         parent->readParameter (search_order
                        , "n_deficit_uptake_fraction"//, "()"
                        , c.n_deficit_uptake_fraction
                        , 0.0, 1.0);
         }

     parent->readParameter (search_order
                     ,"sen_start_stage"//, "()"
                     , c.sen_start_stage
                     , 0.0, (float)max_table);

     parent->readParameter (search_order
                    ,"fr_lf_sen_rate"//, "(/degday)"
                    , c.fr_lf_sen_rate
                    , 0.0, 1.0);

     parent->readParameter (search_order
                    ,"node_sen_rate"//, "(degday)"
                    , c.node_sen_rate
                    , 0.0, 1000.0);

     parent->readParameter (search_order
                   , "n_fact_lf_sen_rate"//, "(/degday)"
                   , c.n_fact_lf_sen_rate
                   , 0.0, 5.0);

    //    plant_event
    parent->readParameter (search_order
                   ,"grn_water_cont"//, "(g/g)"
                   , c.grn_water_cont
                   , 0.0, 1.0);

    //    plant_dm_partition
    parent->readParameter (search_order
                   ,"sla_min"//, "(mm^2/g)"
                   , c.sla_min
                   , 0.0, 100000.0);

    //    plant_dm_senescence
    parent->readParameter (search_order
                      , "dm_senescence_option"//, "()"
                      , c.dm_senescence_option
                      , 1, 3);

    for (part=0; part<max_part;part++)
       {
       sprintf(name, "x_dm_sen_frac_%s", c.part_names[part].c_str());
       parent->readParameter (search_order
                        , name
                        //, "()"
                        , c.x_dm_sen_frac[part]
                        , c.num_dm_sen_frac[part]
                        , 0.0
                        , 100.0);

       sprintf(name, "y_dm_sen_frac_%s", c.part_names[part].c_str());
       parent->readParameter (search_order
                        , name
                        //, "()"
                        , c.y_dm_sen_frac[part]
                        , c.num_dm_sen_frac[part]
                        , 0.0
                        , 1.0);
       }

    //    plant_dm_dead_detachment
    parent->readParameter (search_order
                     , "dead_detach_frac"//, "()"
                     , c.dead_detach_frac, numvals
                     , 0.0, 1.0);

    parent->readParameter (search_order
                     , "sen_detach_frac"//, "()"
                     , c.sen_detach_frac, numvals
                     , 0.0, 1.0);

    //    plant_leaf_area_devel
    parent->readParameter (search_order
                   , "node_no_correction"//, "()"
                   , c.node_no_correction
                   , 0.0, 10.0);

    parent->readParameter (search_order
                     , "x_node_no"//, "()"
                     , c.x_node_no, c.num_node_no
                     , 0.0, 100.0);

    parent->readParameter (search_order
                     , "y_leaf_size"//, "(mm2)"
                     , c.y_leaf_size, c.num_node_no
                     , 0.0, 60000.0);

    //    plant_leaf_area_sen_light
    parent->readParameter (search_order
                   , "lai_sen_light"//, "(m^2/m^2)"
                   , c.lai_sen_light
                   , 3.0, 20.0);

    parent->readParameter (search_order
                   , "sen_light_slope"//, "()"
                   , c.sen_light_slope
                   , 0.0, 100.0);

    // TEMPLATE OPTION
    //    plant_leaf_area_sen_frost
    parent->readParameter (search_order
                     , "x_temp_senescence"//, "(oc)"
                     , c.x_temp_senescence, c.num_temp_senescence
                     , -20.0, 20.0);

    parent->readParameter (search_order
                     , "y_senescence_fac"//, "()"
                     , c.y_senescence_fac, c.num_temp_senescence
                     , 0.0, 1.0);

    // TEMPLATE OPTION
    //    plant_leaf_area_sen_water
    parent->readParameter (search_order
                   , "sen_rate_water"//, "()"
                   , c.sen_rate_water
                   , 0.0, 100.0);

    //    plant_n_conc_limits
    parent->readParameter (search_order
                     , "x_stage_code"//, "()"
                     , c.x_stage_code, c.num_n_conc_stage
                     , 0.0, 100.0);

    parent->readParameter (search_order
                   , "n_conc_crit_root"//, "()"
                   , c.n_conc_crit_root
                   , 0.0, 100.0);

    parent->readParameter (search_order
                   , "n_conc_max_root"//, "()"
                   , c.n_conc_max_root
                   , 0.0, 100.0);

    parent->readParameter (search_order
                   , "n_conc_min_root"//, "()"
                   , c.n_conc_min_root
                   , 0.0, 100.0);

    //    plant_n_init
    parent->readParameter (search_order
                     , "n_init_conc"//,  "(g/g)"
                     , c.n_init_conc, numvals
                     , 0.0, 1.0);

    //    plant_n_senescence
    parent->readParameter (search_order
                     , "n_senescence_option"//, "()"
                     , c.n_senescence_option
                     , 1, 2);
    parent->readParameter (search_order
                     , "n_sen_conc"//, "()"
                     , c.n_sen_conc, numvals
                     , 0.0, 1.0);

    //    plant_nfact
    parent->readParameter (search_order
                      , "n_stress_option"//, "()"
                      , c.n_stress_option
                      , 1, 2);

    parent->readParameter (search_order
                  , "N_stress_start_stage"//, "()"
                  , c.n_stress_start_stage
                  , 0.0, 100.0);

    parent->readParameter (search_order
                   , "n_fact_photo"//, "()"
                   , c.n_fact_photo
                   , 0.0, 100.0);

    parent->readParameter (search_order
                   , "n_fact_pheno"//, "()"
                   , c.n_fact_pheno
                   , 0.0, 100.0);

    parent->readParameter (search_order
                   , "n_fact_expansion"//, "()"
                   , c.n_fact_expansion
                   , 0.0, 100.0);

    //    plant_rue_reduction
    parent->readParameter (search_order
                     , "x_ave_temp"//,  "(oc)"
                     , c.x_ave_temp, c.num_ave_temp
                     , 0.0, 100.0);

    parent->readParameter (search_order
                     , "y_stress_photo"//,  "()"
                     , c.y_stress_photo, c.num_factors
                     , 0.0, 1.0);

    //    plant_thermal_time
    parent->readParameter (search_order
                     , "x_temp_root_advance"//, "(oc)"
                     , c.x_temp_root_advance
                     , c.num_temp_root_advance
                     , -10., 60.0);

    parent->readParameter (search_order
                     , "y_rel_root_advance"//, "(0-1)"
                     , c.y_rel_root_advance
                     , c.num_temp_root_advance
                     , 0.0, 1.0);

    parent->readParameter (search_order
                     , "x_weighted_temp"//, "(oc)"
                     , c.x_weighted_temp, c.num_weighted_temp
                     , 0.0, 100.0);

    parent->readParameter (search_order
                     , "y_plant_death"//, "(oc)"
                     , c.y_plant_death, c.num_weighted_temp
                     , 0.0, 100.0);

    //    plant_swdef
    parent->readParameter (search_order
                     , "x_sw_demand_ratio"//, "()"
                     , c.x_sw_demand_ratio, c.num_sw_demand_ratio
                     , 0.0, 100.0);

    parent->readParameter (search_order
                     , "y_swdef_leaf"//, "()"
                     , c.y_swdef_leaf, c.num_sw_demand_ratio
                     , 0.0, 100.0);

    parent->readParameter (search_order
                     , "x_sw_avail_ratio"//, "()"
                     , c.x_sw_avail_ratio, c.num_sw_avail_ratio
                     , 0.0, 100.0);

    parent->readParameter (search_order
                     , "y_swdef_pheno"//, "()"
                     , c.y_swdef_pheno, c.num_sw_avail_ratio
                     , 0.0, 100.0);

    parent->readParameter (search_order
                     , "x_sw_ratio"//, "()"
                     , c.x_sw_ratio, c.num_sw_ratio
                     , 0.0, 100.0);

    parent->readParameter (search_order
                     , "y_sw_fac_root"//, "()"
                     , c.y_sw_fac_root, c.num_sw_ratio
                     , 0.0, 100.0);

    parent->readParameter (search_order
                     , "x_ws_root"//,  "()"
                     , c.x_ws_root, c.num_ws_root
                     , 0.0, 1.0);

    parent->readParameter (search_order
                     , "y_ws_root_fac"//, "()"
                     , c.y_ws_root_fac, c.num_ws_root
                     , 0.0, 1.0);

    parent->readParameter (search_order
                     , "x_sw_avail_fix"//,  "()"
                     , c.x_sw_avail_fix, c.num_sw_avail_fix
                     , 0.0, 100.0);

    parent->readParameter (search_order
                     , "y_swdef_fix"//, "()"
                     , c.y_swdef_fix, c.num_sw_avail_fix
                     , 0.0, 100.0);

    parent->readParameter (search_order
                     , "oxdef_photo_rtfr"//, "()"
                     , c.oxdef_photo_rtfr, c.num_oxdef_photo
                     , 0.0, 1.0);

    parent->readParameter (search_order
                     , "oxdef_photo"//, "()"
                     , c.oxdef_photo, c.num_oxdef_photo
                     , 0.0, 1.0);

    parent->readParameter (search_order
                     , "x_sw_avail_ratio_flower"//, "()"
                     , c.x_sw_avail_ratio_flower, c.num_sw_avail_ratio_flower
                     , 0.0, 1.0);

    parent->readParameter (search_order
                     , "y_swdef_pheno_flower"//, "()"
                     , c.y_swdef_pheno_flower, c.num_sw_avail_ratio_flower
                     , 0.0, 5.0);

    parent->readParameter (search_order
                     , "x_sw_avail_ratio_grainfill"//, "()"
                     , c.x_sw_avail_ratio_grainfill, c.num_sw_avail_ratio_grainfill
                     , 0.0, 1.0);

    parent->readParameter (search_order
                     , "y_swdef_pheno_grainfill"//, "()"
                     , c.y_swdef_pheno_grainfill, c.num_sw_avail_ratio_grainfill
                     , 0.0, 5.0);

    parent->readParameter (search_order
                     , "co2_default"//, "()"
                     , c.co2_default
                     , 0.0, 1000.0);

    parent->readParameter (search_order
                     , "x_co2_te_modifier"//, "()"
                     , c.x_co2_te_modifier, c.num_co2_te_modifier
                     , 0.0, 1000.0);
    parent->readParameter (search_order
                     , "y_co2_te_modifier"//, "()"
                     , c.y_co2_te_modifier, c.num_co2_te_modifier
                     , 0.0, 10.0);

    parent->readParameter (search_order
                     , "x_co2_nconc_modifier"//, "()"
                     , c.x_co2_nconc_modifier, c.num_co2_nconc_modifier
                     , 0.0, 1000.0);
    parent->readParameter (search_order
                     , "y_co2_nconc_modifier"//, "()"
                     , c.y_co2_nconc_modifier, c.num_co2_nconc_modifier
                     , 0.0, 10.0);

    string pathway = parent->readParameter (search_order, "photosynthetic_pathway");
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
    char  msg[200];                               // message
    float yield;                                  // grain yield dry wt (kg/ha)
    float yield_wet;                              // grain yield including moisture (kg/ha)

    //- Implementation Section ----------------------------------
    push_routine (my_name);

    // crop harvested. Report status

    yield = fruitPart->dmGrainTotal() * gm2kg / sm2ha;

    // include the grain water content
    yield_wet = yield / (1.0 - c.grn_water_cont);

    grain_wt = fruitPart->grainWt();

    plant_grain_no = divide (fruitPart->grainNo(), g.plants, 0.0);

    float dmRoot = (g.dm_green[root] + g.dm_dead[root] + g.dm_senesced[root]) * gm2kg / sm2ha;
    float nRoot = (g.n_green[root] + g.n_dead[root] + g.n_senesced[root]) * gm2kg / sm2ha;

    leaf_no = sum_real_array (g.leaf_no, max_node);

    n_grain_conc_percent = fruitPart->grainNConcPercent();

    n_grain = fruitPart->nGrainTotal() * gm2kg/sm2ha;
    n_green = stoverNGreen() * gm2kg / sm2ha;
    n_senesced = stoverNSenesced() * gm2kg / sm2ha;
    n_dead = stoverNDead() * gm2kg / sm2ha;

    n_stover = n_green + n_senesced + n_dead;
    n_total = n_grain + n_stover;

    float DMRrootShootRatio = divide(dmRoot, topsTot()* gm2kg / sm2ha, 0.0);
    float HarvestIndex      = divide(yield, topsTot()* gm2kg / sm2ha, 0.0);
    float StoverCNRatio     = divide(stoverTot()* gm2kg / sm2ha*plant_c_frac, n_stover, 0.0);
    float RootCNRatio       = divide(dmRoot*plant_c_frac, nRoot, 0.0);

    parent->writeString ("");

    sprintf (msg, "%s%4d%26s%s%10.1f"
             , " flowering day          = ",floweringEventObserver->getDoy(), " "
             , " stover (kg/ha)         = ",stoverTot()* gm2kg / sm2ha);
    parent->writeString (msg);

    sprintf (msg, "%s%4d%26s%s%10.1f"
             , " maturity day           = ", maturityEventObserver->getDoy(), " "
             , " grain yield (kg/ha)    = ", yield);
    parent->writeString (msg);

    sprintf (msg, "%s%6.1f%24s%s%10.1f"
             , " grain % water content  = ", c.grn_water_cont * fract2pcnt, " "
             , " grain yield wet (kg/ha)= ", yield_wet);
    parent->writeString (msg);

    sprintf (msg, "%s%8.3f%22s%s%10.3f"
             , " grain wt (g)           = ", grain_wt, " "
             , " grains/m^2             = ", fruitPart->grainNo());
    parent->writeString (msg);

    sprintf (msg, "%s%6.1f%24s%s%10.3f"
             , " grains/plant           = ", plant_grain_no, " "
             , " maximum lai            = ", g.lai_max);
    parent->writeString (msg);

    sprintf (msg, "%s%10.1f"
             , " total above ground biomass (kg/ha)    = ", topsTot()* gm2kg / sm2ha);
    parent->writeString (msg);

    sprintf (msg, "%s%10.1f",
               " live above ground biomass (kg/ha)     = "
              , (topsGreen() + topsSenesced())* gm2kg / sm2ha);
    parent->writeString (msg);

    sprintf (msg, "%s%10.1f"
             , " green above ground biomass (kg/ha)    = ", topsGreen()* gm2kg / sm2ha);
    parent->writeString (msg);

    sprintf (msg, "%s%10.1f"
             , " senesced above ground biomass (kg/ha) = ", topsSenesced()* gm2kg / sm2ha);
    parent->writeString (msg);

    sprintf (msg, "%s%10.1f"
             , " dead above ground biomass (kg/ha)     = ", topsDead()* gm2kg / sm2ha);
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

    summary_p ();

    parent->writeString ("");

    sprintf (msg,"%s", " Average Stress Indices:                          Water Photo  Water Expan  N Photo      N grain conc");
    parent->writeString (msg);

    parent->writeString (g.averageStressMessage.c_str());
    g.averageStressMessage = "";

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
void Plant::plant_send_crop_chopped_event (const string&  crop_type             // (INPUT) crop type
                                           ,vector<string> &dm_type             // (INPUT) residue type
                                           ,vector<float>  &dlt_crop_dm         // (INPUT) residue weight (kg/ha)
                                           ,vector<float>  &dlt_dm_n            // (INPUT) residue N weight (kg/ha)
                                           ,vector<float>  &dlt_dm_p            // (INPUT) residue P weight (kg/ha)
                                           ,vector<float>  &fraction_to_residue)// (INPUT) residue fraction to residue (0-1)
{

//+  Constant Values
    const char*  myname = "plant_send_crop_chopped_event" ;
//- Implementation Section ----------------------------------
    push_routine (myname);

    if (dm_type.size() != dlt_crop_dm.size() ||
        dm_type.size() != dlt_dm_n.size() ||
        dm_type.size() != dlt_dm_p.size() ||
        dm_type.size() != fraction_to_residue.size())
       throw std::runtime_error("Vector size mismatch in plant_send_crop_chopped_event");

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

    // Make an FStrings string array and store it..
    unsigned int maxlen = 0;
    for (unsigned int i=0; i <  dm_type.size();i++)
        {
        maxlen = max(maxlen, dm_type[i].size());
        }
    char *buf = new char [maxlen*dm_type.size()];
    memset(buf, 0,maxlen*dm_type.size());
    for (unsigned int i=0; i <  dm_type.size();i++)
        {
        strncpy(buf+i*maxlen, dm_type[i].c_str(), maxlen);
        }
    outgoingApsimVariant.store("dm_type", protocol::DTstring, true,
              FStrings(buf, maxlen, dm_type.size(), dm_type.size()));
    delete [] buf;

    outgoingApsimVariant.store("dlt_crop_dm", protocol::DTsingle, true, dlt_crop_dm);
    outgoingApsimVariant.store("dlt_dm_n", protocol::DTsingle, true, dlt_dm_n);
    outgoingApsimVariant.store("dlt_dm_p", protocol::DTsingle, true, dlt_dm_p);
    outgoingApsimVariant.store("fraction_to_residue", protocol::DTsingle, true, fraction_to_residue);
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
void Plant::get_plant_status(protocol::Component *system, protocol::QueryValueData &qd) const
{
    switch (g.plant_status) {
    	case out: system->sendVariable(qd, FString("out")); break;
    	case dead: system->sendVariable(qd, FString("dead")); break;
    	case alive: system->sendVariable(qd, FString("alive")); break;
    }
}



void Plant::get_crop_type(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, FString(c.crop_type.c_str()));
}

void Plant::get_leaf_no(protocol::Component *system, protocol::QueryValueData &qd)
{
    float temp = sum_real_array(g.leaf_no, max_node);
    system->sendVariable(qd, temp);
}

float Plant::getLeafNo(void) const
{
   float sum = 0.0;
	for (int i = 0; i < max_node; i++) sum += g.leaf_no[i];
   return sum;
}

void Plant::get_leaf_no_dead(protocol::Component *system, protocol::QueryValueData &qd)
{
    float temp = sum_real_array(g.leaf_no_dead, max_node);
    system->sendVariable(qd, temp);
}


void Plant::get_leaf_area(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, protocol::vector<float>(g.leaf_area, g.leaf_area+20/*max_node*/)); // XX system can't handle big arrays..
}


void Plant::get_height(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, stemPart->g.height);
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

//NIH up to here
void Plant::get_biomass(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, topsTot() * gm2kg / sm2ha);
}


void Plant::get_green_biomass(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, topsGreen() * gm2kg / sm2ha);
}


void Plant::get_biomass_wt(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, topsTot());
}


void Plant::get_green_biomass_wt(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, topsGreen());
}

void Plant::get_stover_biomass_wt(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, stoverTot());
}

void Plant::get_dm_plant_min(protocol::Component *system, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<plantPart*> parts;
   vector<float>  dm_min;
   setupHacks(parts);

   for (part = parts.begin();
        part != parts.end();
        part++)
      (*part)->get_dm_plant_min(dm_min);

   system->sendVariable(qd, dm_min);
   deleteHacks(parts);
}


void Plant::get_biomass_n(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, topsNTot());
}


void Plant::get_n_uptake(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, topsNTot());
}


void Plant::get_green_biomass_n(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, topsNGreen());
}



void Plant::get_ep(protocol::Component *system, protocol::QueryValueData &qd)
{
    float sum = 0.0;
    int num_layers = 1+count_of_real_vals (g.dlayer, max_layer);
    for (int layer = 0; layer < num_layers; layer++)
        {
        sum = sum + fabs(g.dlt_sw_dep[layer]);
        }
    system->sendVariable(qd, sum);
}

void Plant::get_sw_uptake(protocol::Component *system, protocol::QueryValueData &qd)
{
    float rwu[max_layer];
    int num_layers = 1+count_of_real_vals (g.dlayer, max_layer);
    for (int layer = 0; layer < num_layers; layer++)
        {
        rwu[layer] = fabs(g.dlt_sw_dep[layer]);
        }
    system->sendVariable(qd, protocol::vector<float>(rwu, rwu+num_layers));
}



void Plant::get_cep(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, (float)fabs(g.transpiration_tot));
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
    float n_conc = divide (stoverNGreen(), stoverGreen(), 0.0) * fract2pcnt;
    system->sendVariable(qd, n_conc);
}


void Plant::get_n_conc_root(protocol::Component *system, protocol::QueryValueData &qd)
{
    float n_conc = divide (g.n_green[root]
                 , g.dm_green[root]
                 , 0.0) * fract2pcnt;

    system->sendVariable(qd, n_conc);
}

void Plant::get_n_conc_crit(protocol::Component *system, protocol::QueryValueData &qd)
{
    float n_conc = divide ((leafPart->g.n_conc_crit*leafPart->g.dm_green
                           + stemPart->g.n_conc_crit*stemPart->g.dm_green)
                          , (leafPart->g.dm_green + stemPart->g.dm_green)
                          , 0.0) * fract2pcnt;
    system->sendVariable(qd, n_conc);
}

void Plant::get_n_conc_min(protocol::Component *system, protocol::QueryValueData &qd)
{
    float n_conc = divide ((leafPart->g.n_conc_min * leafPart->g.dm_green
                            + stemPart->g.n_conc_min * stemPart->g.dm_green)
                          , (leafPart->g.dm_green + stemPart->g.dm_green)
                          , 0.0) * fract2pcnt;
    system->sendVariable(qd, n_conc);
}


void Plant::get_n_uptake_stover(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, stoverNGreen());
}


void Plant::get_no3_tot(protocol::Component *system, protocol::QueryValueData &qd)
{
    int deepest_layer = find_layer_no (g.root_depth, g.dlayer, max_layer);
    float no3gsm_tot = sum_real_array (g.no3gsm, deepest_layer+1);
    system->sendVariable(qd, no3gsm_tot);
}


void Plant::get_n_demand(protocol::Component *system, protocol::QueryValueData &qd)
{
    float n_demand = sumNDemand();
    system->sendVariable(qd, n_demand);
}

void Plant::get_n_demanded(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<plantPart*> parts;
   vector<float>  n_demanded;
   setupHacks(parts);

   for (part = parts.begin(); part != parts.end(); part++)
      (*part)->get_n_demanded(n_demanded);

   systemInterface->sendVariable(qd, n_demanded);
   deleteHacks(parts);
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
	float sum = g.cnd_grain_conc.getSum();
   system->sendVariable(qd, sum);
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


void Plant::get_dm_parasite_retranslocate(protocol::Component *system, protocol::QueryValueData &qd)
{
  system->sendVariable(qd, g.dm_parasite_retranslocate);
}

void Plant::get_pfact_grain(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    systemInterface->sendVariable(qd, g.pfact_grain);  //()
}

void Plant::get_pstress_photo(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    float pstress_photo;
    if (g.pfact_photo > 0.0)
       pstress_photo = 1.0 - g.pfact_photo;
    else
       pstress_photo = 0.0;
    systemInterface->sendVariable(qd, pstress_photo);  //()
}

void Plant::get_pstress_pheno(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    float pstress_pheno;
    if (g.pfact_pheno > 0.0)
       pstress_pheno = 1.0 - g.pfact_pheno;
    else
       pstress_pheno = 0.0;
    systemInterface->sendVariable(qd, pstress_pheno);  //()
}

void Plant::get_pstress_expansion(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    float pstress_expansion;
    if (g.pfact_expansion > 0.0)
       pstress_expansion = 1.0 - g.pfact_expansion;
    else
       pstress_expansion = 0.0;
    systemInterface->sendVariable(qd, pstress_expansion);  //()
}

void Plant::get_pstress_grain(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    float pstress_grain;
    if (g.pfact_grain > 0.0)
       pstress_grain = 1.0 - g.pfact_grain;
    else
       pstress_grain = 0.0;
    systemInterface->sendVariable(qd, pstress_grain);  //()
}

void Plant::get_biomass_p(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    systemInterface->sendVariable(qd, topsPTot());  //()
}

void Plant::get_green_biomass_p(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    systemInterface->sendVariable(qd, topsPGreen());  //()
}
//NIH up to here
void Plant::get_p_conc_stover(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    float p_conc_stover = divide (stoverPGreen(), stoverGreen(), 0.0) * fract2pcnt ;
    systemInterface->sendVariable(qd, p_conc_stover);  //()
}

void Plant::get_p_uptake_stover(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    systemInterface->sendVariable(qd, stoverPGreen());  //()
}

void Plant::get_dm_green(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<plantPart*> parts;
   vector<float>  dm_green;
   setupHacks(parts);

   for (part = parts.begin();
        part != parts.end();
        part++)
      (*part)->get_dm_green(dm_green);

   systemInterface->sendVariable(qd, dm_green);
   deleteHacks(parts);
}

void Plant::get_dm_dead(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<plantPart*> parts;
   vector<float>  dm_dead;
   setupHacks(parts);

   for (part = parts.begin();
        part != parts.end();
        part++)
      (*part)->get_dm_dead(dm_dead);

   systemInterface->sendVariable(qd, dm_dead);
   deleteHacks(parts);
}

void Plant::get_dm_senesced(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<plantPart*> parts;
   vector<float>  dm_senesced;
   setupHacks(parts);

   for (part = parts.begin();
        part != parts.end();
        part++)
      (*part)->get_dm_senesced(dm_senesced);

   systemInterface->sendVariable(qd, dm_senesced);
   deleteHacks(parts);
}

void Plant::get_dlt_dm_green(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<plantPart*> parts;
   vector<float>  dlt_dm_green;
   setupHacks(parts);

   for (part = parts.begin();
        part != parts.end();
        part++)
      (*part)->get_dlt_dm_green(dlt_dm_green);

   systemInterface->sendVariable(qd, dlt_dm_green);
   deleteHacks(parts);
}
void Plant::get_dlt_dm_green_retrans(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<plantPart*> parts;
   vector<float>  dlt_dm_green_retrans;
   setupHacks(parts);

   for (part = parts.begin();
        part != parts.end();
        part++)
      (*part)->get_dlt_dm_green_retrans(dlt_dm_green_retrans);

   systemInterface->sendVariable(qd, dlt_dm_green_retrans);
   deleteHacks(parts);
}
void Plant::get_dlt_dm_detached(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<plantPart*> parts;
   vector<float>  dlt_dm_detached;
   setupHacks(parts);

   for (part = parts.begin();
        part != parts.end();
        part++)
      (*part)->get_dlt_dm_detached(dlt_dm_detached);

   systemInterface->sendVariable(qd, dlt_dm_detached);
   deleteHacks(parts);
}
void Plant::get_dlt_dm_senesced(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<plantPart*> parts;
   vector<float>  dlt_dm_senesced;
   setupHacks(parts);

   for (part = parts.begin();
        part != parts.end();
        part++)
      (*part)->get_dlt_dm_senesced(dlt_dm_senesced);

   systemInterface->sendVariable(qd, dlt_dm_senesced);
   deleteHacks(parts);
}
void Plant::get_dlt_dm_dead_detached(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<plantPart*> parts;
   vector<float>  dlt_dm_dead_detached;
   setupHacks(parts);

   for (part = parts.begin();
        part != parts.end();
        part++)
      (*part)->get_dlt_dm_dead_detached(dlt_dm_dead_detached);

   systemInterface->sendVariable(qd, dlt_dm_dead_detached);
   deleteHacks(parts);
}
void Plant::get_dlt_dm_green_dead(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<plantPart*> parts;
   vector<float>  dlt_dm_green_dead;
   setupHacks(parts);

   for (part = parts.begin();
        part != parts.end();
        part++)
      (*part)->get_dlt_dm_green_dead(dlt_dm_green_dead);

   systemInterface->sendVariable(qd, dlt_dm_green_dead);
   deleteHacks(parts);
}
void Plant::get_dlt_dm_senesced_dead(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<plantPart*> parts;
   vector<float>  dlt_dm_senesced_dead;
   setupHacks(parts);

   for (part = parts.begin();
        part != parts.end();
        part++)
      (*part)->get_dlt_dm_senesced_dead(dlt_dm_senesced_dead);

   systemInterface->sendVariable(qd, dlt_dm_senesced_dead);
   deleteHacks(parts);
}
void Plant::get_n_green(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<plantPart*> parts;
   vector<float>  n_green;
   setupHacks(parts);

   for (part = parts.begin(); part != parts.end(); part++)
      (*part)->get_n_green(n_green);

   systemInterface->sendVariable(qd, n_green);
   deleteHacks(parts);
}
void Plant::get_n_senesced(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<plantPart*> parts;
   vector<float>  n_senesced;
   setupHacks(parts);

   for (part = parts.begin(); part != parts.end(); part++)
      (*part)->get_n_senesced(n_senesced);

   systemInterface->sendVariable(qd, n_senesced);
   deleteHacks(parts);
}
void Plant::get_n_dead(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<plantPart*> parts;
   vector<float>  n_dead;
   setupHacks(parts);

   for (part = parts.begin(); part != parts.end(); part++)
      (*part)->get_n_dead(n_dead);

   systemInterface->sendVariable(qd, n_dead);
   deleteHacks(parts);
}
void Plant::get_dlt_n_green(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<plantPart*> parts;
   vector<float>  n_green;
   setupHacks(parts);

   for (part = parts.begin(); part != parts.end(); part++)
      (*part)->get_dlt_n_green(n_green);

   systemInterface->sendVariable(qd, n_green);
   deleteHacks(parts);
}
void Plant::get_dlt_n_dead(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<plantPart*> parts;
   vector<float>  n_dead;
   setupHacks(parts);

   for (part = parts.begin(); part != parts.end(); part++)
      (*part)->get_dlt_n_dead(n_dead);

   systemInterface->sendVariable(qd, n_dead);
   deleteHacks(parts);
}
void Plant::get_dlt_n_retrans(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<plantPart*> parts;
   vector<float>  n_retrans;
   setupHacks(parts);

   for (part = parts.begin(); part != parts.end(); part++)
      (*part)->get_dlt_n_retrans(n_retrans);

   systemInterface->sendVariable(qd, n_retrans);
   deleteHacks(parts);
}
void Plant::get_dlt_n_senesced(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<plantPart*> parts;
   vector<float>  n_senesced;
   setupHacks(parts);

   for (part = parts.begin(); part != parts.end(); part++)
      (*part)->get_dlt_n_senesced(n_senesced);

   systemInterface->sendVariable(qd, n_senesced);
   deleteHacks(parts);
}

void Plant::get_dlt_n_senesced_dead(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<plantPart*> parts;
   vector<float>  dlt_n_senesced_dead;
   setupHacks(parts);

      (*part)->get_dlt_n_senesced_dead(dlt_n_senesced_dead);
      dlt_n_senesced_dead.push_back((*part)->dlt.n_senesced_dead);

   systemInterface->sendVariable(qd, dlt_n_senesced_dead);
   deleteHacks(parts);
}

void Plant::get_dlt_n_senesced_retrans(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<plantPart*> parts;
   vector<float>  n_senesced_retrans;
   setupHacks(parts);

   for (part = parts.begin(); part != parts.end(); part++)
      (*part)->get_dlt_n_senesced_retrans(n_senesced_retrans);

   systemInterface->sendVariable(qd, n_senesced_retrans);
   deleteHacks(parts);
}
void Plant::get_dlt_n_senesced_trans(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<plantPart*> parts;
   vector<float>  n_senesced_trans;
   setupHacks(parts);

   for (part = parts.begin(); part != parts.end(); part++)
      (*part)->get_dlt_n_senesced_trans(n_senesced_trans);

   systemInterface->sendVariable(qd, n_senesced_trans);
   deleteHacks(parts);
}
void Plant::get_dlt_n_detached(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<plantPart*> parts;
   vector<float>  n_detached;
   setupHacks(parts);

   for (part = parts.begin(); part != parts.end(); part++)
      (*part)->get_dlt_n_detached(n_detached);

   systemInterface->sendVariable(qd, n_detached);
   deleteHacks(parts);
}
void Plant::get_dlt_n_dead_detached(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<plantPart*> parts;
   vector<float>  n_dead_detached;
   setupHacks(parts);

   for (part = parts.begin(); part != parts.end(); part++)
      (*part)->get_dlt_n_dead_detached(n_dead_detached);

   systemInterface->sendVariable(qd, n_dead_detached);
   deleteHacks(parts);
}

void Plant::get_p_demand(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<plantPart*> parts;
   vector<float>  p_green;
   setupHacks(parts);

   float p_demand = 0.0;
   for (part = parts.begin(); part != parts.end(); part++)
      p_demand += (*part)->pDemand();

   deleteHacks(parts);
   systemInterface->sendVariable(qd, p_demand);   //(g/m^2
}

void Plant::get_p_demand_parts(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<plantPart*> parts;
   vector<float>  p_demand;
   setupHacks(parts);

   for (part = parts.begin(); part != parts.end(); part++)
      (*part)->get_p_demand(p_demand);

   deleteHacks(parts);
   systemInterface->sendVariable(qd, p_demand);   //(g/m^2
}

void Plant::get_p_green(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<plantPart*> parts;
   vector<float>  p_green;
   setupHacks(parts);

   for (part = parts.begin(); part != parts.end(); part++)
      (*part)->get_p_green(p_green);

   systemInterface->sendVariable(qd, p_green);
   deleteHacks(parts);
}
void Plant::get_p_dead(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<plantPart*> parts;
   vector<float>  p_dead;
   setupHacks(parts);

   for (part = parts.begin(); part != parts.end(); part++)
      (*part)->get_p_dead(p_dead);

   systemInterface->sendVariable(qd, p_dead);
   deleteHacks(parts);
}
void Plant::get_p_sen(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<plantPart*> parts;
   vector<float>  p_sen;
   setupHacks(parts);

   for (part = parts.begin(); part != parts.end(); part++)
      (*part)->get_p_sen(p_sen);

   systemInterface->sendVariable(qd, p_sen);
   deleteHacks(parts);
}

void Plant::get_dlt_p_green(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<plantPart*> parts;
   vector<float>  dlt_p_green;
   setupHacks(parts);

   for (part = parts.begin(); part != parts.end(); part++)
      (*part)->get_dlt_p_green(dlt_p_green);

   systemInterface->sendVariable(qd, dlt_p_green);
   deleteHacks(parts);
}
void Plant::get_dlt_p_retrans(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<plantPart*> parts;
   vector<float>  dlt_p_retrans;
   setupHacks(parts);

   for (part = parts.begin(); part != parts.end(); part++)
      (*part)->get_dlt_p_retrans(dlt_p_retrans);

   systemInterface->sendVariable(qd, dlt_p_retrans);
   deleteHacks(parts);
}
void Plant::get_dlt_p_detached(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<plantPart*> parts;
   vector<float>  dlt_p_detached;
   setupHacks(parts);

   for (part = parts.begin(); part != parts.end(); part++)
      (*part)->get_dlt_p_detached(dlt_p_detached);

   systemInterface->sendVariable(qd, dlt_p_detached);
   deleteHacks(parts);
}
void Plant::get_dlt_p_dead(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<plantPart*> parts;
   vector<float>  dlt_p_dead;
   setupHacks(parts);

   for (part = parts.begin(); part != parts.end(); part++)
      (*part)->get_dlt_p_dead(dlt_p_dead);

   systemInterface->sendVariable(qd, dlt_p_dead);
   deleteHacks(parts);
}

void Plant::get_dlt_p_sen(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<plantPart*> parts;
   vector<float>  dlt_p_sen;
   setupHacks(parts);

   for (part = parts.begin(); part != parts.end(); part++)
      (*part)->get_dlt_p_sen(dlt_p_sen);

   systemInterface->sendVariable(qd, dlt_p_sen);
   deleteHacks(parts);
}

void Plant::get_ll(protocol::Component *systemInterface, protocol::QueryValueData &qd)
   {
   vector<float> ll_dep;
   int num_layers = count_of_real_vals (p.ll_dep, max_layer);
   for(int layer = 0; layer <= num_layers; layer++)
      ll_dep.push_back(p.ll_dep[layer]);
   systemInterface->sendVariable(qd, ll_dep);
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
                               float dlt_P_root,         //(INPUT) new root residue P (g/m^2)
                               float *g_dlayer,           //(INPUT) layer thicknesses (mm)
                               float *g_root_length,      //(INPUT) layered root length (mm)
                               float g_root_depth,       //(INPUT) root depth (mm)
                               const char *c_crop_type)   //(INPUT) crop type
   {
   int deepest_layer;         // deepest layer in which the roots are growing
   float *dlt_dm_incorp = new float[max_layer]; // root residue (kg/ha)
   float *dlt_N_incorp = new float[max_layer];  // root residue N (kg/ha)
   float *dlt_P_incorp = new float[max_layer];  // root residue P (kg/ha)
   if (dlt_dm_root > 0.0)
         {
         // DM
         crop_root_dist(g_dlayer, g_root_length, g_root_depth, dlt_dm_incorp,
                        dlt_dm_root * gm2kg /sm2ha);

         bound_check_real_array(parent,dlt_dm_incorp, max_layer, 0.0, dlt_dm_root * gm2kg / sm2ha,
                                "dlt_dm_incorp");
         // Nitrogen
         crop_root_dist(g_dlayer, g_root_length, g_root_depth, dlt_N_incorp,
                        dlt_N_root * gm2kg /sm2ha);

         bound_check_real_array(parent,dlt_N_incorp,  max_layer, 0.0, dlt_N_root * gm2kg / sm2ha,
                                "dlt_N_incorp");

         // Phosporous
         crop_root_dist(g_dlayer, g_root_length, g_root_depth, dlt_P_incorp,
                        dlt_P_root * gm2kg /sm2ha);

         bound_check_real_array(parent,dlt_P_incorp,  max_layer, 0.0, dlt_P_root * gm2kg / sm2ha,
                                "dlt_P_incorp");

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
         outgoingApsimVariant.store("dlt_fom_p", protocol::DTsingle, true,
                                    protocol::vector<float>(dlt_P_incorp, dlt_P_incorp+deepest_layer+1));
         parent->publish (id.incorp_fom, outgoingApsimVariant);
#endif
         }
    delete []  dlt_dm_incorp;
    delete []  dlt_N_incorp;
    delete []  dlt_P_incorp;
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

         // need to calculate dm using potential rue not affected by
         // n and temperature

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
            // g_dm_green[part] =0.0;
            n_demand[part] = 0.0;
            n_max[part]    = 0.0;
            }
      } // for
      pop_routine (my_name);
   }


   float Plant::getStageCode(void) const {return phenology->stageCode();}
   float Plant::getStageNumber(void) const {return phenology->stageNumber();}
   float Plant::getPlants(void) const {return g.plants;}
   float Plant::getCo2(void) const {return g.co2;}
   photosynthetic_pathway_t Plant::getPhotosynthetic_pathway(void) const {return c.photosynthetic_pathway;}
   //float Plant::getRadnInterceptedPod(void) const {return g.radn_int_pod;}
   float Plant::getDltDMPotRueVeg(void) const {return g.dlt_dm_pot_rue - fruitPart->dltDmPotRuePod();}
   float Plant::getDmGreenVeg(void) const {return leafPart->g.dm_green + stemPart->g.dm_green;}
   //float Plant::getDltDmVeg(void) const {return leafPart->dltDmTotal() + stemPart->dltDmTotal();}
   float Plant::getWaterSupplyPod(void) const {return g.swSupplyFruit;}
   float Plant::getDmTops(void) const{ return topsGreen()+topsSenesced();}
   float Plant::getDltDm(void) const{ return g.dlt_dm;}
   float Plant::getDmVeg(void) const {return leafPart->dmTotal() + stemPart->dmTotal();}
   float Plant::getDmGreenStem(void) const {return stemPart->g.dm_green;}
   float Plant::getDmGreenTot(void) const {return plantGreen();}
   float Plant::getRelativeGrowthRate(void) {return divide(g.dlt_dm_pot_rue, getDmGreenTot(), 0.0);}
   float Plant::getDyingFractionPlants(void)
      {
          float dying_fract_plants = divide (-g.dlt_plants, g.plants, 0.0);
          dying_fract_plants = bound (dying_fract_plants, 0.0, 1.0);
          return dying_fract_plants;
      }

  float Plant::getCo2ModifierRue(void) const {return g.co2_modifier_rue;}
  float Plant::getCo2ModifierTe(void) const {return g.co2_modifier_te;}
  float Plant::getCo2ModifierNConc(void) const {return g.co2_modifier_n_conc;}
  float Plant::getVpd(void) const {return g.vpd;}
  float Plant::getTempStressPhoto(void) const {return g.temp_stress_photo;}
  float Plant::getNfactPhoto(void) const {return g.nfact_photo;}
  float Plant::getOxdefPhoto(void) const {return g.oxdef_photo;}
  float Plant::getPfactPhoto(void) const {return g.pfact_photo;}
  float Plant::getSwdefPhoto(void) const {return g.swdef_photo;}

float Plant::plantGreen(void) const
   {
     return  g.dm_green[root] + fruitPart->dmGreen() + leafPart->g.dm_green + stemPart->g.dm_green;
                    //     + reproStruct->g.dm_green;
   }
float Plant::plantSenesced(void) const
   {
      return  g.dm_senesced[root] + fruitPart->dmSenesced() + leafPart->g.dm_senesced + stemPart->g.dm_senesced;
                   //      + reproStruct->g.dm_senesced;
   }
float Plant::plantDead(void) const
   {
      return  g.dm_dead[root] + fruitPart->dmDead() + leafPart->g.dm_dead + stemPart->g.dm_dead;
                 //        + reproStruct->g.dm_dead;
   }
float Plant::plantDltDmGreen(void) const
   {
      return  g.dlt_dm_green[root] + fruitPart->dltDmGreen() + leafPart->dlt.dm_green + stemPart->dlt.dm_green;
                       //  + reproStruct->dlt.dm_green;
   }
float Plant::plantTot(void) const
   {
      return  plantGreen() + plantSenesced() + plantDead();
    }

float Plant::topsGreen(void) const
   {
     return  fruitPart->dmGreen() + leafPart->g.dm_green + stemPart->g.dm_green;
                    //     + reproStruct->g.dm_green;
   }
float Plant::topsSenesced(void) const
   {
      return  fruitPart->dmSenesced() + leafPart->g.dm_senesced + stemPart->g.dm_senesced;
                   //      + reproStruct->g.dm_senesced;
   }
float Plant::topsDead(void) const
   {
      return  fruitPart->dmDead() + leafPart->g.dm_dead + stemPart->g.dm_dead;
                 //        + reproStruct->g.dm_dead;
   }
float Plant::topsDltDmGreen(void) const
   {
      return  fruitPart->dltDmGreen() + leafPart->dlt.dm_green + stemPart->dlt.dm_green;
                       //  + reproStruct->dlt.dm_green;
   }
float Plant::topsTot(void) const
   {
      return  topsGreen() + topsSenesced() + topsDead();
    }

float Plant::stoverGreen(void) const
   {
      return  leafPart->g.dm_green + fruitPart->dmGreenVegTotal() + stemPart->g.dm_green;
              //           + reproStruct->g.dm_green;
    }
float Plant::stoverSenesced(void) const
   {
      return  leafPart->g.dm_senesced + fruitPart->dmSenescedVegTotal() +  stemPart->g.dm_senesced;
                    //     + reproStruct->g.dm_senesced;
    }
float Plant::stoverDead(void) const
   {
      return  leafPart->g.dm_dead + fruitPart->dmDeadVegTotal() +  stemPart->g.dm_dead;
                  //       + reproStruct->g.dm_dead;
    }

float Plant::stoverTot(void) const
   {
      return  stoverGreen() + stoverSenesced() + stoverDead();
    }

float Plant::plantNGreen(void) const
   {
      return  g.n_green[root] + fruitPart->nGreen() + leafPart->g.n_green + stemPart->g.n_green;
                 //        + reproStruct->g.n_green;
   }
float Plant::plantNSenesced(void) const
   {
      return  g.n_senesced[root] + fruitPart->nSenesced() + leafPart->g.n_senesced + stemPart->g.n_senesced;
               //          + reproStruct->g.n_senesced;
   }
float Plant::plantNDead(void) const
   {
      return  g.n_dead[root] + fruitPart->nDead() + leafPart->g.n_dead + stemPart->g.n_dead;
                       //  + reproStruct->g.n_dead;
   }
float Plant::plantNTot(void) const
   {
      return  plantNGreen() + plantNSenesced() + plantNDead();
    }

float Plant::plantDltNGreen(void) const
   {
      return  g.dlt_n_green[root] + fruitPart->dltNGreen() + leafPart->dlt.n_green + stemPart->dlt.n_green;
                       //  + reproStruct->g.n_dead;
   }
float Plant::plantDltNRetrans(void) const
   {
      return  g.dlt_n_retrans[root] + fruitPart->dltNRetransOut() + leafPart->dlt.n_retrans + stemPart->dlt.n_retrans;
                       //  + reproStruct->g.n_dead;
   }
float Plant::topsNGreen(void) const
   {
      return  fruitPart->nGreen() + leafPart->g.n_green + stemPart->g.n_green;
                 //        + reproStruct->g.n_green;
   }
float Plant::topsNSenesced(void) const
   {
      return  fruitPart->nSenesced() + leafPart->g.n_senesced + stemPart->g.n_senesced;
               //          + reproStruct->g.n_senesced;
   }
float Plant::topsNDead(void) const
   {
      return  fruitPart->nDead() + leafPart->g.n_dead + stemPart->g.n_dead;
                       //  + reproStruct->g.n_dead;
   }
float Plant::topsNTot(void) const
   {
      return  topsNGreen() + topsNSenesced() + topsNDead();
    }

float Plant::stoverNGreen(void) const
   {
      return  leafPart->g.n_green + fruitPart->nGreenVegTotal() + stemPart->g.n_green;
                     //    + reproStruct->g.n_green;
    }
float Plant::stoverNSenesced(void) const
   {
      return  leafPart->g.n_senesced + fruitPart->nSenescedVegTotal() + stemPart->g.n_senesced;
                   //      + reproStruct->g.n_senesced;
    }
float Plant::stoverNDead(void) const
   {
      return  leafPart->g.n_dead + fruitPart->nDeadVegTotal() + stemPart->g.n_dead;
                 //        + reproStruct->g.n_dead;
    }
float Plant::stoverNTot(void) const
   {
      return  stoverNGreen() + stoverNSenesced() + stoverNDead();
    }

float Plant::plantPGreen(void) const
   {
      return  g.p_green[root] + leafPart->g.p_green + fruitPart->pGreenVegTotal() + stemPart->g.p_green;
                       //  + reproStruct->g.p_green;
    }
float Plant::plantPSenesced(void) const
   {
      return  g.p_sen[root] + leafPart->g.p_sen + fruitPart->pSenescedVegTotal() + stemPart->g.p_sen;
                         //+ reproStruct->g.p_sen;
    }
float Plant::plantPDead(void) const
   {
      return  g.p_dead[root] + leafPart->g.p_dead + fruitPart->pDeadVegTotal()+ stemPart->g.p_dead;
                        // + reproStruct->g.p_dead;
    }
float Plant::plantPTot(void) const
   {
      return  plantPGreen() + plantPSenesced() + plantPDead();
   }

float Plant::topsPGreen(void) const
   {
      return  fruitPart->pGreen() + leafPart->g.p_green + stemPart->g.p_green;
               //          + reproStruct->g.p_green;
   }
float Plant::topsPSenesced(void) const
   {
      return fruitPart->pSenesced() + leafPart->g.p_sen + stemPart->g.p_sen;
                       //  + reproStruct->g.p_sen;
   }
float Plant::topsPDead(void) const
   {
      return fruitPart->pDead() + leafPart->g.p_dead + stemPart->g.p_dead;
                     //    + reproStruct->g.p_dead;
   }


float Plant::stoverPGreen(void) const
   {
      return  leafPart->g.p_green + fruitPart->pGreenVegTotal() + stemPart->g.p_green;
                       //  + reproStruct->g.p_green;
    }
float Plant::stoverPSenesced(void) const
   {
      return  leafPart->g.p_sen + fruitPart->pSenescedVegTotal() + stemPart->g.p_sen;
                         //+ reproStruct->g.p_sen;
    }
float Plant::stoverPDead(void) const
   {
      return  leafPart->g.p_dead + fruitPart->pDeadVegTotal()+ stemPart->g.p_dead;
                        // + reproStruct->g.p_dead;
    }
float Plant::stoverPTot(void) const
   {
      return  stoverPGreen() + stoverPSenesced() + stoverPDead();
   }

float Plant::grainPGreen(void) const
   {
      return  fruitPart->pGreenGrainTotal();
                       //  + reproStruct->g.p_green;
    }
float Plant::grainPSenesced(void) const
   {
      return  fruitPart->pSenescedGrainTotal();
                         //+ reproStruct->g.p_sen;
    }
float Plant::grainPDead(void) const
   {
      return  fruitPart->pDeadGrainTotal();
                        // + reproStruct->g.p_dead;
    }
float Plant::grainPTot(void) const
   {
      return  grainPGreen() + grainPSenesced() + grainPDead();
   }

float Plant::grainPConcTot(void) const
   {
      return  fruitPart->pConcGrainTotal();
                       //  + reproStruct->g.p_green;
    }
float Plant::topsPTot(void) const
   {
      return  topsPGreen() + topsPSenesced() + topsPDead();
   }
float Plant::sumNDemand(void)
   {
      float n_demand = 0.0;
      for (vector<plantPart *>::iterator t = myParts.begin(); t != myParts.end(); t++)
      n_demand += (*t)->nDemand();
      n_demand += g.n_demand[root];
      return n_demand;
   }
float Plant::sumSoilNDemand(void)
   {
      float soil_n_demand = 0.0;
      for (vector<plantPart *>::iterator t = myParts.begin(); t != myParts.end(); t++)
      soil_n_demand += (*t)->soilNDemand();
      soil_n_demand += g.soil_n_demand[root];
      return soil_n_demand;
   }
float Plant::sumNMax(void)
   {
      float n_max = 0.0;
      for (vector<plantPart *>::iterator t = myParts.begin(); t != myParts.end(); t++)
      n_max += (*t)->nMax();
      n_max += g.n_max[root];
      return n_max;
   }


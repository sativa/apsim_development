#include <stdio.h>
#include <math.h>
#include <string>

#include <map>
#include <list>
#include <vector>
#include <stdexcept>
#include <sstream>

#include <boost/function.hpp>
#include <boost/bind.hpp>

#include <ComponentInterface/Component.h>
#include <ComponentInterface/Type.h>

#include "PlantComponent.h"
#include "PlantInterface.h"
#include "PlantLibrary.h"
#include "Phenology/PlantPhenology.h"
#include "Phenology/WheatPhenology.h"
#include "Phenology/TTTPhenology.h"
#include "Phenology/TTTRatePhenology.h"
#include "Plant.h"
#include "PlantPart.h"
#include "CompositePart.h"
#include "LeafPart.h"
#include "FruitCohort.h"
#include "StemPart.h"
#include "LeafPart.h"
#include "PodPart.h"
#include "MealPart.h"
#include "OilPart.h"
#include "RootPart.h"
#include "Observers.h"
#include "ReproStruct.h"
#include "Phenology/GenericPhenology.h"
#include "Phenology/BroccoliPhenology.h"
#include "arbitrator.h"
#include "PlantUtility.h"

using namespace std;

Plant *currentInstance = NULL;

static const char* nullType =         "<type/>";
static const char* integerType =      "<type kind=\"integer4\"/>";
static const char* integerArrayType = "<type kind=\"integer4\" array=\"T\"/>";
static const char* floatType =        "<type kind=\"single\"/>";
static const char* floatArrayType =   "<type kind=\"single\" array=\"T\"/>";
static const char* doubleType =       "<type kind=\"double\"/>";
static const char* doubleArrayType =  "<type kind=\"double\" array=\"T\"/>";
static const char* stringType =       "<type kind=\"string\"/>";
static const char* stringArrayType =  "<type kind=\"string\" array=\"T\"/>";
static const char* logicalType =      "<type kind=\"boolean\"/>";
static const char* sowDDML =          "<type name = \"sow\">" \
                                      "   <field name=\"crop_class_name\" kind=\"string\"/>" \
                                      "   <field name=\"crop_class_numbytes\" kind=\"integer4\"/>" \
                                      "   <field name=\"crop_class_code\" kind=\"integer4\"/>" \
                                      "   <field name=\"crop_class_isarray\" kind=\"boolean\"/>" \
                                      "   <field name=\"crop_class_value\" kind=\"string\"/>" \

                                      "   <field name=\"cultivar_name\" kind=\"string\"/>" \
                                      "   <field name=\"cultivar_numbytes\" kind=\"integer4\"/>" \
                                      "   <field name=\"cultivar_code\" kind=\"integer4\"/>" \
                                      "   <field name=\"cultivar_isarray\" kind=\"boolean\"/>" \
                                      "   <field name=\"cultivar_value\" kind=\"string\"/>" \

                                      "   <field name=\"plants_name\" kind=\"string\"/>" \
                                      "   <field name=\"plants_numbytes\" kind=\"integer4\"/>" \
                                      "   <field name=\"plants_code\" kind=\"integer4\"/>" \
                                      "   <field name=\"plants_isarray\" kind=\"boolean\"/>" \
                                      "   <field name=\"plants_value\" kind=\"single\"/>" \

                                      "   <field name=\"sowing_depth_name\" kind=\"string\"/>" \
                                      "   <field name=\"sowing_depth_numbytes\" kind=\"integer4\"/>" \
                                      "   <field name=\"sowing_depth_code\" kind=\"integer4\"/>" \
                                      "   <field name=\"sowing_depth_isarray\" kind=\"boolean\"/>" \
                                      "   <field name=\"sowing_depth_value\" kind=\"single\"/>" \

                                      "   <field name=\"row_spacing_name\" kind=\"string\"/>" \
                                      "   <field name=\"row_spacing_numbytes\" kind=\"integer4\"/>" \
                                      "   <field name=\"row_spacing_code\" kind=\"integer4\"/>" \
                                      "   <field name=\"row_spacing_isarray\" kind=\"boolean\"/>" \
                                      "   <field name=\"row_spacing_value\" kind=\"single\"/>" \
                                      "</type>";

static const char* killStemDDML =     "<type name = \"KillStem\">" \
                                      "   <field name=\"plants_name\" kind=\"string\"/>" \
                                      "   <field name=\"plants_numbytes\" kind=\"integer4\"/>" \
                                      "   <field name=\"plants_code\" kind=\"integer4\"/>" \
                                      "   <field name=\"plants_isarray\" kind=\"boolean\"/>" \
                                      "   <field name=\"plants_value\" kind=\"single\"/>" \
                                      "</type>";

static const char* new_profileDDML =  "<type name = \"NewProfile\">" \
                                      "   <field name=\"dlayer_name\" kind=\"string\"/>" \
                                      "   <field name=\"dlayer_numbytes\" kind=\"integer4\"/>" \
                                      "   <field name=\"dlayer_code\" kind=\"integer4\"/>" \
                                      "   <field name=\"dlayer_isarray\" kind=\"boolean\"/>" \
                                      "   <field name=\"dlayer_value\" kind=\"single\" array=\"T\"/>" \

                                      "   <field name=\"ll15_dep_name\" kind=\"string\"/>" \
                                      "   <field name=\"ll15_dep_numbytes\" kind=\"integer4\"/>" \
                                      "   <field name=\"ll15_dep_code\" kind=\"integer4\"/>" \
                                      "   <field name=\"ll15_dep_isarray\" kind=\"boolean\"/>" \
                                      "   <field name=\"ll15_dep_value\" kind=\"single\" array=\"T\"/>" \

                                      "   <field name=\"dul_dep_name\" kind=\"string\"/>" \
                                      "   <field name=\"dul_dep_numbytes\" kind=\"integer4\"/>" \
                                      "   <field name=\"dul_dep_code\" kind=\"integer4\"/>" \
                                      "   <field name=\"dul_dep_isarray\" kind=\"boolean\"/>" \
                                      "   <field name=\"dul_dep_value\" kind=\"single\" array=\"T\"/>" \

                                      "   <field name=\"sat_dep_name\" kind=\"string\"/>" \
                                      "   <field name=\"sat_dep_numbytes\" kind=\"integer4\"/>" \
                                      "   <field name=\"sat_dep_code\" kind=\"integer4\"/>" \
                                      "   <field name=\"sat_dep_isarray\" kind=\"boolean\"/>" \
                                      "   <field name=\"sat_dep_value\" kind=\"single\" array=\"T\"/>" \

                                      "   <field name=\"sw_dep_name\" kind=\"string\"/>" \
                                      "   <field name=\"sw_dep_numbytes\" kind=\"integer4\"/>" \
                                      "   <field name=\"sw_dep_code\" kind=\"integer4\"/>" \
                                      "   <field name=\"sw_dep_isarray\" kind=\"boolean\"/>" \
                                      "   <field name=\"sw_dep_value\" kind=\"single\" array=\"T\"/>" \

                                      "   <field name=\"bd_name\" kind=\"string\"/>" \
                                      "   <field name=\"bd_numbytes\" kind=\"integer4\"/>" \
                                      "   <field name=\"bd_code\" kind=\"integer4\"/>" \
                                      "   <field name=\"bd_isarray\" kind=\"boolean\"/>" \
                                      "   <field name=\"bd_value\" kind=\"single\" array=\"T\"/>" \
                                      "</type>";

static const char* cropChoppedDDML =  "<type name = \"CropChopped\">" \
                                      "   <field name=\"croptype_name\" kind=\"string\"/>" \
                                      "   <field name=\"croptype_numbytes\" kind=\"integer4\"/>" \
                                      "   <field name=\"croptype_code\" kind=\"integer4\"/>" \
                                      "   <field name=\"croptype_isarray\" kind=\"boolean\"/>" \
                                      "   <field name=\"croptype_value\" kind=\"string\"/>" \

                                      "   <field name=\"dm_type_name\" kind=\"string\"/>" \
                                      "   <field name=\"dm_type_numbytes\" kind=\"integer4\"/>" \
                                      "   <field name=\"dm_type_code\" kind=\"integer4\"/>" \
                                      "   <field name=\"dm_type_isarray\" kind=\"boolean\"/>" \
                                      "   <field name=\"dm_type_value\" kind=\"string\" array=\"T\"/>" \

                                      "   <field name=\"dlt_crop_dm_name\" kind=\"string\"/>" \
                                      "   <field name=\"dlt_crop_dm_numbytes\" kind=\"integer4\"/>" \
                                      "   <field name=\"dlt_crop_dm_code\" kind=\"integer4\"/>" \
                                      "   <field name=\"dlt_crop_dm_isarray\" kind=\"boolean\"/>" \
                                      "   <field name=\"dlt_crop_dm_value\" kind=\"single\" array=\"T\"/>" \

                                      "   <field name=\"dlt_dm_n_name\" kind=\"string\"/>" \
                                      "   <field name=\"dlt_dm_n_numbytes\" kind=\"integer4\"/>" \
                                      "   <field name=\"dlt_dm_n_code\" kind=\"integer4\"/>" \
                                      "   <field name=\"dlt_dm_n_isarray\" kind=\"boolean\"/>" \
                                      "   <field name=\"dlt_dm_n_value\" kind=\"single\" array=\"T\"/>" \

                                      "   <field name=\"dlt_dm_p_name\" kind=\"string\"/>" \
                                      "   <field name=\"dlt_dm_p_numbytes\" kind=\"integer4\"/>" \
                                      "   <field name=\"dlt_dm_p_code\" kind=\"integer4\"/>" \
                                      "   <field name=\"dlt_dm_p_isarray\" kind=\"boolean\"/>" \
                                      "   <field name=\"dlt_dm_p_value\" kind=\"single\" array=\"T\"/>" \

                                      "   <field name=\"fraction_to_residue_name\" kind=\"string\"/>" \
                                      "   <field name=\"fraction_to_residue_numbytes\" kind=\"integer4\"/>" \
                                      "   <field name=\"fraction_to_residue_code\" kind=\"integer4\"/>" \
                                      "   <field name=\"fraction_to_residue_isarray\" kind=\"boolean\"/>" \
                                      "   <field name=\"fraction_to_residue_value\" kind=\"single\" array=\"T\"/>" \
                                      "</type>";


/////////////These might be redundancies??//////////
void push_routine (const char *) {};
void pop_routine (const char *) {};

Plant::Plant(PlantComponent *P)
    {
    parent = P;

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
    for (vector<plantThing *>::iterator t = myThings.begin();
         t != myThings.end();
         t++)
       delete (*t);
    }


// Init1. Set up plant structure
void Plant::doInit1(protocol::Component *s)
    {
   plant_zero_variables (); // Zero global states

    string scratch = s->readParameter ("constants", "phenology_model");
    if (scratch == "")
       throw std::invalid_argument("The parameter 'phenology_model'\nisn't in your ini file.\n\nGet one.\n");
    else if (scratch == "legume")
       phenology = new TTTPhenology(parent, this);
    else if (scratch == "tttrate")
       phenology = new TTTRatePhenology(parent, this);
    else if (scratch == "generic")
       phenology = new GenericPhenology(parent, this);
    else if (scratch == "wheat")
       phenology = new WheatPhenology(parent, this);
    else if (scratch == "broccoli")
       phenology = new BroccoliPhenology(parent, this);
    else
       throw std::invalid_argument("Unknown phenology model '" + scratch + "'");
    myThings.push_back(phenology);

    rootPart = constructRootPart(this, s->readParameter ("constants", "root_part"), "root");
    myThings.push_back(rootPart);
    myParts.push_back(rootPart);

    leafPart = constructLeafPart(this, s->readParameter ("constants", "leaf_part"), "leaf");
    myThings.push_back(leafPart);
    myParts.push_back(leafPart);
    myStoverParts.push_back(leafPart);

    stemPart = new plantStemPart(this, "stem");
    myThings.push_back(stemPart);
    myParts.push_back(stemPart);
    myStoverParts.push_back(stemPart);

//    fruitPart = new PlantFruit(this, "fruit");      //FIXME reinstate this when Plant fruit fixed for cohorting
    fruitPart = new FruitCohort(this, "fruit");       //FIXME remove this when Plant fruit fixed for cohorting
    myThings.push_back(fruitPart);
    myParts.push_back(fruitPart);
    myStoverParts.push_back(fruitPart);
    fruitPart->doInit1();

    reproStruct = new ReproStruct(this, "bruce");
    //myThings.push_back(reproStruct);
    //myParts.push_back(reproStruct);

    arbitrator = constructArbitrator(this, "");       // Make a null arbitrator until we call readSpecies...
    myThings.push_back(arbitrator);

    sowingEventObserver = new eventObserver("sowing", this);
    myThings.push_back(sowingEventObserver);

    emergenceEventObserver = new eventObserver("emergence", this);
    myThings.push_back(emergenceEventObserver);

    FIEventObserver = new eventObserver("floral_initiation", this);
    myThings.push_back(FIEventObserver);

    floweringEventObserver = new eventObserver("flowering", this);
    myThings.push_back(floweringEventObserver);

    maturityEventObserver = new eventObserver("maturity", this);
    myThings.push_back(maturityEventObserver);

    plant_zero_all_globals();
    zero_p_variables();
    doRegistrations(s);
    doIDs();                 // Gather IDs for getVariable requests
   }

// Init2. The rest of the system is here now..
void Plant::doInit2(protocol::Component *)
   {
   PlantP_set_phosphorus_aware(parent); // See whether a P module is plugged in
   plant_read_constants (); // Read constants
   plant_init ();           // Site specific init
   plant_get_other_variables (); // sw etc..
   }



void Plant::doIDs(void)
   {
   // gets
   rootPart->DoIDs(parent);

   id.eo = parent->addRegistration(RegistrationType::get,
                                   "eo", addUnitsToDDML(floatType, "mm").c_str(),
                                   "", "");

   id.no3 = parent->addRegistration(RegistrationType::get,
                                   "no3", addUnitsToDDML(floatArrayType, "kg/ha").c_str(),
                                   "", "");
   id.nh4 = parent->addRegistration(RegistrationType::get,
                                   "nh4", addUnitsToDDML(floatArrayType, "kg/ha").c_str(),
                                   "", "");
   id.latitude = parent->addRegistration(RegistrationType::get,
                                   "latitude", addUnitsToDDML(floatType, "oC").c_str(),
                                   "", "");
   id.parasite_c_demand = parent->addRegistration(RegistrationType::get,
                                   "parasite_dm_demand", addUnitsToDDML(floatType, "g/m2").c_str(),
                                   "", "");
   id.parasite_sw_demand = parent->addRegistration(RegistrationType::get,
                                   "parasite_sw_demand", addUnitsToDDML(floatType, "mm").c_str(),
                                   "", "");
   id.maxt_soil_surface = parent->addRegistration(RegistrationType::get,
                                   "maxt_soil_surface", addUnitsToDDML(floatType, "degree Celsius").c_str(),
                                   "", "");
   id.co2 = parent->addRegistration(RegistrationType::get,
                                    "co2", addUnitsToDDML(floatType, "ppm").c_str(),
                                    "", "");

   string canopyName = string("fr_intc_radn_") + string(parent->getName());
   id.fr_intc_radn = parent->addRegistration(RegistrationType::get,
                                   canopyName.c_str(),
                                   addUnitsToDDML(floatType, "").c_str(),
                                   "", "");
   // sets
   id.dlt_no3 = parent->addRegistration(RegistrationType::set,
                                   "dlt_no3", addUnitsToDDML(floatArrayType, "kg/ha").c_str(),
                                   "", "");
   id.dlt_nh4 = parent->addRegistration(RegistrationType::set,
                                   "dlt_nh4", addUnitsToDDML(floatArrayType, "kg/ha").c_str(),
                                   "", "");


   // events.
   id.crop_chopped = parent->addRegistration(RegistrationType::event,
                                   "crop_chopped", cropChoppedDDML,
                                   "", "");
   }

// Register Methods, Events,
void Plant::doRegistrations(protocol::Component *system)
   {
   // Events
   setupEvent(parent, "prepare",     RegistrationType::respondToEvent, &Plant::doPrepare, nullTypeDDML);
   setupEvent(parent, "process",     RegistrationType::respondToEvent, &Plant::doProcess, nullTypeDDML);
   setupEvent(parent, "tick",        RegistrationType::respondToEvent, &Plant::doTick, DDML(protocol::timeType()).c_str());
   setupEvent(parent, "newmet",      RegistrationType::respondToEvent, &Plant::doNewMet, DDML(protocol::newmetType()).c_str());
   setupEvent(parent, "new_profile", RegistrationType::respondToEvent, &Plant::doNewProfile, new_profileDDML);
   setupEvent(parent, "sow",         RegistrationType::respondToEvent, &Plant::doSow, sowDDML);
   setupEvent(parent, "harvest",     RegistrationType::respondToEvent, &Plant::doHarvest, nullTypeDDML);
   setupEvent(parent, "end_crop",    RegistrationType::respondToEvent, &Plant::doEndCrop, nullTypeDDML);
   setupEvent(parent, "kill_crop",   RegistrationType::respondToEvent, &Plant::doKillCrop, nullTypeDDML);
   setupEvent(parent, "end_run",     RegistrationType::respondToEvent, &Plant::doEndRun, nullTypeDDML);
   setupEvent(parent, "kill_stem",   RegistrationType::respondToEvent, &Plant::doKillStem, killStemDDML);
   setupEvent(parent, "remove_crop_biomass",   RegistrationType::respondToEvent, &Plant::doRemoveCropBiomass, DDML(protocol::removeCropDmType()).c_str());


   // Send My Variable

   setupGetFunction(parent, "plant_status", protocol::DTstring, false,
                     &Plant::get_plant_status, "", "Plant Status");

// XXXX UGLY HACK workaround for broken coordinator in 3.4
//   id = parent->addGettableVar("crop_type", protocol::DTstring, false, "");
//   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_crop_type));

   parent->addGettableVar("crop_class",
               g.crop_class, "", "Plant crop class");

//   id = parent->addGettableVar("height", protocol::DTsingle, false, "mm");
//   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_height));

   parent->addGettableVar("width",
               g.canopy_width, "mm", "canopy row width");

   parent->addGettableVar("plants",
               g.plants, "plants/m^2", "Plant desnity");

//   id = parent->addGettableVar("cover_green", protocol::DTsingle, false, "");
//   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_cover_green));

//   id = parent->addGettableVar("cover_tot", protocol::DTsingle, false, "");
//   IDtoGetFn.insert(UInt2GetFnMap::value_type(id,&Plant::get_cover_tot));

   parent->addGettableVar("lai_canopy_green",
               g.lai_canopy_green, "m^2/m^2", "Green lai");

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

   setupGetFunction(parent, "cep", protocol::DTsingle, false,
                    &Plant::get_cep,
                    "mm", "Cumulative plant water uptake");


   setupGetFunction(parent, "n_conc_stover", protocol::DTsingle, false,
                    &Plant::get_n_conc_stover, "%", "N concentration in stover");

   setupGetFunction(parent, "n_conc_crit", protocol::DTsingle, false,
                    &Plant::get_n_conc_crit, "%", "critical N content");

   setupGetFunction(parent, "n_conc_min", protocol::DTsingle, false,
                    &Plant::get_n_conc_min, "%", "minimum N content");

   setupGetFunction(parent, "n_uptake_stover", protocol::DTsingle, false,
                    &Plant::get_n_uptake_stover,
                    "g/m^2", "N taken up by agp");

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

   setupGetFunction(parent, "no3_demand", protocol::DTsingle, false,
                    &Plant::get_no3_demand,
                    "kg/ha", "Demand for NO3");

   parent->addGettableVar("sw_demand",
               g.sw_demand, "mm", "Demand for sw");

   parent->addGettableVar("sw_demand_te",
               g.sw_demand_te, "mm", "Demand for sw");

   setupGetFunction(parent, "no3gsm_uptake_pot", protocol::DTsingle, true,
                    &Plant::get_no3gsm_uptake_pot,
                    "g/m2", "Pot NO3 uptake");

   setupGetFunction(parent, "nh4gsm_uptake_pot", protocol::DTsingle, true,
                    &Plant::get_nh4gsm_uptake_pot,
                    "g/m2", "Pot NH4 uptake");

   setupGetFunction(parent, "no3_swfac", protocol::DTsingle, true,
                    &Plant::get_no3_swfac,
                    "???", "Work this out...>>");



   setupGetFunction(parent, "parasite_dm_supply", protocol::DTsingle, false,
                     &Plant::get_parasite_c_gain,
                     "g/m^2", "Assimilate to parasite");

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

#undef setupGetVar
#undef setupGetFunction

   unsigned int id;
   // Set My Variable
   id = system->addRegistration(RegistrationType::respondToSet, "crop_class", stringType);
   IDtoSetFn.insert(UInt2SetFnMap::value_type(id,&Plant::set_plant_crop_class));

   system->addRegistration(RegistrationType::event, "sowing", nullTypeDDML, "", "");
   system->addRegistration(RegistrationType::event, "harvesting", nullTypeDDML, "", "");

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
bool Plant::respondToSet(unsigned int &id, protocol::QuerySetValueData& qd)
  {
    ptr2setFn pf = IDtoSetFn[id];
    if (pf) {return((this->*pf)(qd));}
    return false;
  }
void Plant::sendStageMessage(const char *what)
  {
  unsigned int id = parent->addRegistration(RegistrationType::event,
                                            what, "<type/>",
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
void Plant::doEndCrop(unsigned &, unsigned &, protocol::Variant &)
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
void Plant::doAutoClassChange(unsigned &/*fromId*/, unsigned &eventId, protocol::Variant &)
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
  jday_to_day_of_year(&sd, &Environment.day_of_year, &Environment.year);
  fruitPart->doTick(tick);
  }

// Field a NewMet event
void Plant::doNewMet(unsigned &, unsigned &, protocol::Variant &v)
  {
  if (g.hasreadconstants)
     {
     struct protocol::newmetType newmet;
     v.unpack(newmet);
     Environment.radn = newmet.radn;
     Environment.maxt = newmet.maxt;
     Environment.mint = newmet.mint;
     fruitPart->doNewMet(newmet);

     }
  }


void Plant::plant_bio_actual (int option /* (INPUT) option number*/)
//=======================================================================================
//       Takes the minimum of biomass production limited by radiation and
//       biomass production limited by water.
    {
        g.dlt_dm = min (g.dlt_dm_pot_rue, g.dlt_dm_pot_te);
    }




void Plant::plant_bio_retrans (void)
//=======================================================================================
//       Retranslocate biomass.
   {
   vector<plantPart *> supply_pools_by_veg;
   supply_pools_by_veg.push_back(stemPart);
   supply_pools_by_veg.push_back(leafPart);

   vector<plantPart *> allParts;
   allParts.push_back(rootPart);
   allParts.push_back(leafPart);
   allParts.push_back(stemPart);
   allParts.push_back(fruitPart);

   float dm_demand_differential = fruitPart->dmGreenDemand ()
                                - fruitPart->dlt.dm_green;

   legnew_dm_retranslocate(allParts
                           , supply_pools_by_veg
                           , dm_demand_differential
                           , g.plants
                           , &g.dlt_dm_retrans_to_fruit);
   }



void Plant::plant_bio_distribute (void)
//     ===========================================================
//       distribute biomass to fruit parts.
   {
   fruitPart->doDmPartition (fruitPart->dlt.dm_green, fruitPart->dmGreenDemand ());

   float dm_demand_differential = fruitPart->dmGreenDemand ()
                                - fruitPart->dlt.dm_green;
   fruitPart->doDmRetranslocate (g.dlt_dm_retrans_to_fruit, dm_demand_differential);
   }





void Plant::plant_water_stress (void)
//     ===========================================================
//         Get current water stress factors (0-1)
    {
    rootPart->plant_water_stress (g.sw_demand,
                                  g.swdef_photo,
                                  g.swdef_pheno,
                                  g.swdef_pheno_flower,
                                  g.swdef_pheno_grainfill,
                                  g.swdef_expansion,
                                  g.swdef_fixation );
    }


void Plant::plant_temp_stress (void)
//     ===========================================================
//         Get current temperature stress factors (0-1)
    {
        crop_temperature_stress_photo(c.num_ave_temp,
                                      c.x_ave_temp, c.y_stress_photo,
                                      Environment.maxt, Environment.mint, &g.temp_stress_photo);
    }


void Plant::plant_bio_water (void)
//     ===========================================================
//     Calculate biomass transpiration efficiency
    {
    float dltDmPotTeVeg = 0.0;
    fruitPart->doDmPotTE ();
    plant_bio_water1 (g.swSupplyVeg, g.transp_eff, &dltDmPotTeVeg);
    g.dlt_dm_pot_te = dltDmPotTeVeg + fruitPart->dltDmPotTe();
    }


void Plant::plant_detachment (void)
//     ===========================================================
//       Simulate plant detachment.
    {
        leafPart->detachment();

        for (vector<plantPart *>::iterator t = myParts.begin();
             t != myParts.end();
             t++)
           {
           (*t)->doDmDetachment();
           (*t)->doNDetachment();
           if (g.phosphorus_aware == true) (*t)->doPDetachment();
           }
    }

void Plant::plant_plant_death (int option /* (INPUT) option number*/)
//      Determine plant death in crop

    {
//+  Constant Values
    const char*  my_name = "plant_plant_death" ;

//- Implementation Section ----------------------------------

    push_routine (my_name);

    if (option == 1)
        {
        if (phenology->inPhase("sowing"))
           g.dlt_plants_failure_germ =
                  crop_failure_germination (this,
                                            c.days_germ_limit,
                                            phenology->daysInCurrentPhase(),
                                            g.plants);
        else
           g.dlt_plants_failure_germ = 0.0;

        if (phenology->inPhase("germination"))
           g.dlt_plants_failure_emergence =
                  crop_failure_emergence (this,
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
                                     , Environment.day_of_year
                                     , g.soil_temp
                                     , Environment.year
                                     , g.plants);
              }
           }
        /*XXXX this needs tou be coupled with dlt_leaf_area_sen, c_sen_start_stage  FIXME*/
//        if (phenology->inPhase("leaf_senescence"))
        if (phenology->inPhase("above_ground"))
           g.dlt_plants_failure_leaf_sen =
                  crop_failure_leaf_sen(this, leafPart->getLAI(), g.plants);
        else
           g.dlt_plants_failure_leaf_sen = 0.0;

        if (phenology->inPhase("preflowering"))
           g.dlt_plants_failure_phen_delay =
                  crop_failure_phen_delay(this
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
            plant_kill_crop(&g.plant_status);
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


float Plant::plant_death_drought
    (
     float  c_leaf_no_crit              // (INPUT)  critical number of leaves belo
    ,float  c_swdf_photo_limit          // (INPUT)  critical cumulative photosynth
    ,float  c_swdf_photo_rate           // (INPUT)  rate of plant reduction with p
    ,float  cswd_photo                 // (INPUT)  cumulative water stress type 1
    ,float  g_plants                    // (INPUT)  Plant density (plants/m^2)
    ,float  g_swdef_photo               // (INPUT)
    ) {
    float killfr;                                 // fraction of crop population to kill
    float dlt_plants = 0.0;                       // population to kill


    if (getLeafNo() < c_leaf_no_crit
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
       bound_check_real_var(this, killfr, 0.0, 1.0, "killfr");
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
void Plant::plant_kill_crop (status_t *g_plant_status)
    {
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
        float no3gsm_min[max_layer];   // minimum allowable NO3 in soil (g/m^2)
        fill_real_array (no3gsm_min, 0.0, max_layer);

        cproc_n_supply1 (rootPart->dlayer
                         , rootPart->dlt_sw_dep
                         , rootPart->no3gsm
                         , no3gsm_min
                         , rootPart->root_depth
                         , rootPart->sw_dep
                         , g.no3gsm_mflow_avail
                         , rootPart->sw_avail
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
        float no3gsm_min[max_layer];   // minimum allowable NO3 in soil (g/m^2)
        fill_real_array (no3gsm_min, 0.0, max_layer);

        cproc_n_supply3 (rootPart->dlayer
                         , rootPart->no3gsm
                         , no3gsm_min
                         , g.no3gsm_uptake_pot
                         , rootPart->root_depth
                         , rootPart->root_length
                         , rootPart->bd
                         , c.n_stress_start_stage
                         , c.total_n_uptake_max
                         , c.no3_uptake_max
                         , c.no3_conc_half_max
                         , rootPart->sw_avail_pot
                         , rootPart->sw_avail
                         , phenology->stageNumber()
                         , c.n_fix_rate
                         , biomass
                         , g.swdef_fixation
                         , &g.n_fix_pot);
        }
     else if (option == 3)
        {
        biomass = topsGreen()  + g.dlt_dm;
        float no3gsm_min[max_layer];   // minimum allowable NO3 in soil (g/m^2)
        fill_real_array (no3gsm_min, 0.0, max_layer);
        float nh4gsm_min[max_layer];   // minimum allowable NH4 in soil (g/m^2)
        fill_real_array (nh4gsm_min, 0.0, max_layer);

        cproc_n_supply4 (rootPart->dlayer
                             , rootPart->bd
                             , rootPart->no3gsm
                             , no3gsm_min
                             , g.no3gsm_uptake_pot
                             , rootPart->nh4gsm
                             , nh4gsm_min
                             , g.nh4gsm_uptake_pot
                             , rootPart->root_depth
                             , c.n_stress_start_stage
                             , c.kno3
                             , c.no3ppm_min
                             , c.knh4
                             , c.nh4ppm_min
                             , c.total_n_uptake_max
                             , rootPart->sw_avail_pot
                             , rootPart->sw_avail
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
        legnew_n_retranslocate(fruitPart->nDemandGrain());
        }
    else if (option == 2)
        {
        legnew_n_retranslocate(fruitPart->nDemandGrain2());  //FIXME
        }
    else
        {
        throw std::invalid_argument ("invalid n retrans option");
        }

    pop_routine (my_name);
    return;
    }

void Plant::doNDemandGrain (void)
//=======================================================================================
//      Find grain nitrogen demand.
{
   fruitPart->doNDemandGrain(g.nfact_grain_conc, g.swdef_expansion);
}

void Plant::plant_nit_demand (int option /* (INPUT) option number*/)
//=======================================================================================
//       Find nitrogen demand.
    {
    if (option == 1)
        {
        for (vector<plantPart *>::iterator t = myParts.begin();
             t != myParts.end();
             t++)
           (*t)->doNDemand1(g.dlt_dm, g.dlt_dm_pot_rue);
        }
     else if (option == 2)
         {
         for (vector<plantPart *>::iterator t = myParts.begin();
              t != myParts.end();
              t++)
            (*t)->doNDemand2(g.dlt_dm, g.dlt_dm_pot_rue);
        }
    else
        {
        throw std::invalid_argument ("invalid n demand option");
        }
    }

void Plant::plant_soil_nit_demand()
//=======================================================================================
//      Find soil nitrogen demand.
   {
      for (vector<plantPart *>::iterator t = myParts.begin();
           t != myParts.end();
           t++)
         (*t)->doSoilNDemand();
   }

void Plant::plant_nit_uptake (int option/* (INPUT) option number*/)
//=======================================================================================
//       Find nitrogen uptake.
    {
    if (Str_i_Eq(rootPart->uptake_source, "apsim"))
        {
        // NIH - note that I use a -ve conversion
        // factor FOR NOW to make it a delta.
        plant_get_ext_uptakes(rootPart->uptake_source.c_str()
                             ,c.crop_type.c_str()
                             ,"no3"
                             ,-kg2gm/ha2sm
                             ,0.0
                             ,100.0
                             ,rootPart->dlt_no3gsm);


        }
    else if (option == 1)
        {
        cproc_n_uptake1(c.no3_diffn_const
                       , rootPart->dlayer
                       , g.no3gsm_diffn_pot
                       , g.no3gsm_mflow_avail
                       , g.n_fix_pot
                       , c.n_supply_preference.c_str()
                       , nDemand()
                       , sumNMax()
                       , rootPart->root_depth
                       , rootPart->dlt_no3gsm);
        }
    else if ((option == 2) || (option == 3))
        {
        cproc_n_uptake3(rootPart->dlayer
                        , g.no3gsm_uptake_pot
                        , g.nh4gsm_uptake_pot
                        , g.n_fix_pot
                        , c.n_supply_preference.c_str()
                        , sumSoilNDemand()
                        , sumNMax()
                        , rootPart->root_depth
                        , rootPart->dlt_no3gsm
                        , rootPart->dlt_nh4gsm);
        }
    else
        {
        throw std::invalid_argument ("invalid template option");
        }
    }


void Plant::plant_nit_partition ()
//=======================================================================================
//     Calculate the nitrogen and phosporous partitioning in the plant
    {
    legnew_n_partition(rootPart->dlayer
                      , rootPart->dlt_no3gsm
                      , rootPart->dlt_nh4gsm
                      , g.n_fix_pot
                      , rootPart->root_depth
                      , &g.n_fix_uptake
                      , myParts);

    if (g.phosphorus_aware)
       {
       PlantP_partition(myParts);
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

void Plant::plant_nit_demand_est (int option)
//=======================================================================================
//      Calculate an approximate nitrogen demand for today's growth.
//      The estimate basically = n to fill the plant up to maximum
//      nitrogen concentration.
    {
    if (option == 1)
        {
        // Option 1 is to assume that the distribution of plant
        // C will be similar after today and so N demand is that
        // required to raise all plant parts to max N conc.

        for (vector<plantPart *>::iterator t = myParts.begin();
             t != myParts.end();
             t++)
           {
           (*t)->doNDemand1Pot(g.dlt_dm_pot_rue, g.dlt_dm_pot_rue);
           }

        g.ext_n_demand = nDemand();

        //nh  use zero growth value here so that estimated n fix is always <= actual;
        float n_fix_pot;
        crop_n_fixation_pot1(phenology->stageNumber()
                             , c.n_fix_rate
                             , topsGreen()
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
    }

void Plant::plant_sen_bio (int dm_senescence_option)
//=======================================================================================
//       Simulate plant senescence.
    {
    if (dm_senescence_option == 1)
        {
        for (vector<plantPart *>::iterator t = myParts.begin();
             t != myParts.end();
             t++)
           (*t)->doSenescence1(leafPart->senFract());
         }
    else if (dm_senescence_option == 2)
         {
         //XX this arm is redundant - not used by any ini files..????
        for (vector<plantPart *>::iterator t = myParts.begin();
             t != myParts.end();
             t++)
           (*t)->doSenescence2(leafPart->senFract());
        }
    else
        {
        throw std::invalid_argument ("invalid template option in plant_sen_bio");
        }
    }

void Plant::plant_sen_nit (int   option/*(INPUT) option number*/)
//=======================================================================================
//       Simulate plant nitrogen senescence.
    {
    if (option == 1)
        {
        for (vector<plantPart *>::iterator t = myParts.begin();
             t != myParts.end();
             t++)
           (*t)->doNSenescence();
        }
    else if (option == 2)
        {
        plant_N_senescence();
        }
    else
        {
        throw std::invalid_argument ("invalid sen nit option");
        }

    if (g.phosphorus_aware == true)
       {
       PlantP_senescence(myParts);
       }
    }

void Plant::plant_cleanup ()
//=======================================================================================
//       cleanup after crop processes
    {
    const char*  my_name = "plant_cleanup" ;

    push_routine (my_name);
    g.remove_biom_pheno = 1.0;

    plant_update( g.row_spacing
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
                , g.dlt_plants
                , &g.plants);

    plant_check_bounds(g.cover_dead
                       , g.cover_green
                       , g.cover_sen
                       , rootPart->dlayer
                       , g.plants
                       , rootPart->root_depth);

    plant_totals( rootPart->dlayer
                , rootPart->dlt_sw_dep
                , &g.lai_max
                , &g.n_conc_act_stover_tot
                , &g.n_conc_crit_stover_tot
                , &g.n_demand_tot
                , &g.n_uptake_stover_tot
                , &g.n_uptake_tot
                , &g.n_fix_uptake
                , &g.n_fixed_tops
                , &rootPart->root_depth
                , &g.transpiration_tot );

    phenology->update();

    if (g.plant_status == alive &&
        phenology->previousStageName() != phenology->stageName())
        {
        plant_event ();
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

    pop_routine (my_name);
    }



//+  Purpose
//       Update states

//+  Mission Statement
//     Update states of variables

//+  Changes
//      250894 jngh specified and programmed
void Plant::plant_update(float  g_row_spacing                          // (INPUT)  row spacing (m) [optional]
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
    ,float  g_dlt_plants                                       // (INPUT)  change in Plant density (plant
    ,float *g_plants)                                           // (out/INPUT)  Plant density (plants/m^2)
{

//+  Constant Values
    const char*  my_name = "plant_update" ;

//+  Local Variables
    float canopy_fac;

//- Implementation Section ----------------------------------
    push_routine (my_name);

    for (vector<plantPart *>::iterator part = myParts.begin();
         part != myParts.end();
         part++)
       (*part)->update();

    // Let me register my surprise at how this is done on the next few lines
    // - why intrinsically limit processes to leaf etc right here!!! - NIH
    leafPart->NGreen -= leafPart->dlt.n_senesced_trans;
    stemPart->NGreen += leafPart->dlt.n_senesced_trans;

    float s = 0.0;
    for (vector<plantPart *>::iterator part = myParts.begin(); part != myParts.end(); part++)
       s += (*part)->dlt.n_senesced_retrans;

    // xx what does this mean??
    leafPart->NGreen -= s;
    leafPart->NGreen = l_bound(leafPart->NGreen, 0.0);   // Can occur at total leaf senescence. FIXME! XXXX

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
                          ,leafPart->getLAI()
                          ,&cover_green_leaf);

    float cover_pod = fruitPart->calcCover(canopy_fac);
    *g_cover_green = add_covers (cover_green_leaf, cover_pod);

    legnew_cover(g_row_spacing
                ,c_x_row_spacing
                ,c_y_extinct_coef_dead
                ,c_num_row_spacing
                , canopy_fac
                ,leafPart->getSLAI()
                ,g_cover_sen);

    legnew_cover(g_row_spacing
                 ,c_x_row_spacing
                 ,c_y_extinct_coef_dead
                 ,c_num_row_spacing
                 , canopy_fac
                 ,leafPart->getTLAI_dead()
                 ,g_cover_dead);

    // plant stress observers
    stageObservers.update();
    if (phenology->inPhase("preflowering")) g.cswd_pheno.update();

    // other plant states
    *g_plants = *g_plants + g_dlt_plants;

    plant_n_conc_limits( g.co2_modifier_n_conc);

    pop_routine (my_name);
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
    ,float  g_plants                            // (INPUT)  Plant density (plants/m^2)
    ,float  g_root_depth                        // (INPUT)  depth of roots (mm)
    ) {

//+  Constant Values
    const char*  my_name = "plant_check_bounds" ;

//+  Local Variables

//- Implementation Section ----------------------------------

    push_routine (my_name);

    bound_check_real_var(this,g_root_depth
                         , 0.0
                         , sum_real_array (g_dlayer, max_layer)
                         , "root_depth");

    bound_check_real_var(this,g_plants
                         , 0.0
                         , 10000.0
                         , "plants");

    bound_check_real_var(this,g_cover_green
                         , 0.0
                         , 1.0
                         , "cover_green");

    bound_check_real_var(this,g_cover_sen
                         , 0.0
                         , 1.0
                         , "cover_sen");

    bound_check_real_var(this,g_cover_dead
                         , 0.0
                         , 1.0
                         , "cover_dead");

    for (vector<plantPart *>::iterator t = myParts.begin(); t != myParts.end(); t++)
       {
       (*t)->checkBounds();
       }

    pop_routine (my_name);
    }


//+  Purpose
//         Collect totals of crop variables for output

//+  Mission Statement
//     Collect totals of crop variables for output

//+  Changes
//     010994 jngh specified and programmed
void Plant::plant_totals
    (float *g_dlayer                     // (INPUT)  thickness of soil layer I (mm)
    ,float *g_dlt_sw_dep                 // (INPUT)  water uptake in each layer (mm water)
    ,float *g_lai_max                    // (INPUT)  maximum lai - occurs at flowering
    ,float  *g_n_conc_act_stover_tot           // (INPUT)  sum of tops actual N concentration (g N/g biomass)
    ,float  *g_n_conc_crit_stover_tot          // (INPUT)  sum of tops critical N concentration (g N/g biomass)
    ,float  *g_n_demand_tot                    // (out/INPUT)  sum of N demand since last output (g/m^2)
    ,float  *g_n_uptake_stover_tot             // (out/INPUT)  sum of tops N uptake (g N/m^2)
    ,float  *g_n_uptake_tot                    // (out/INPUT)  cumulative total N uptake (g/m^2)
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
    n_green_demand = nDemand();

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
        n_uptake_soil_tops = n_uptake_soil - rootPart->dlt.n_green;
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
        n_uptake_soil_tops = n_uptake_soil - rootPart->dlt.n_green;
        *g_n_fixed_tops = *g_n_fixed_tops + n_uptake_soil_tops * divide (*g_n_fix_uptake ,n_uptake_soil ,0.0);

        }

    *g_lai_max = max (*g_lai_max, leafPart->getLAI());
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
void Plant::plant_event()
    {
//+  Local Variables
    float biomass;                                // total above ground plant wt (g/m^2)
    int   deepest_layer;                          // deepest layer in which the roots are growing
    int   layer;                                  // profile layer number
    float pesw_tot;                               // total plant extractable sw (mm)
    float pesw[max_layer];                        // plant extractable soil water (mm)
    float n_green;                                // plant nitrogen of tops (g/m^2) less pod
    float dm_green;                               // plant wt of tops (g/m^2) less pod
    float n_green_conc_percent;                   // n% of tops less pod (incl grain)

    // Tell the system (ie everything outside of plant) about this event.
    // NB. Don't send an "end_crop" to the system - otherwise all the other crops will stop too!
    if (phenology->stageName() != "end_crop")
       {
       sendStageMessage(phenology->stageName().c_str());
       }

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

    deepest_layer = find_layer_no (rootPart->root_depth, rootPart->dlayer, max_layer);
    for (layer = 0; layer <= deepest_layer; layer++)
       {
       pesw[layer] = rootPart->sw_dep[layer] - rootPart->ll_dep[layer];
       pesw[layer] = l_bound (pesw[layer], 0.0);
       }
    pesw_tot = sum_real_array (pesw, deepest_layer+1);

    if (phenology->inPhase ("above_ground"))
            {
            char msg[256];
            sprintf(msg,
"                biomass =       %8.2f (g/m^2)   lai          = %7.3f (m^2/m^2)\n"
"                stover N conc = %8.2f (%%)    extractable sw = %7.2f (mm)",
                biomass, leafPart->getLAI(), n_green_conc_percent, pesw_tot);
            parent->writeString (msg);
            }
    }




void Plant::plant_water_supply (int option /* (INPUT) option number*/)
//       Plant water supply
    {
     rootPart->CalcWaterSupply();

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

        g.swDemandTEFruit = fruitPart->SWDemand ();

        float swDemandTEVeg = 0.0;                                        //
        float dltDmPotRueveg = g.dlt_dm_pot_rue - fruitPart->dltDmPotRue();     // FIXME when fruit is proper class
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

    if (Str_i_Eq(rootPart->uptake_source,"apsim"))
        {
        plant_get_ext_uptakes(rootPart->uptake_source.c_str()
                             ,c.crop_type.c_str()
                             ,"water"
                             ,1.0
                             ,0.0
                             ,100.0
                             ,ext_sw_supply);

        for (layer = 0; layer < rootPart->num_layers; layer++)
           {
           rootPart->dlt_sw_dep[layer] = -ext_sw_supply[layer];
           }
        }
    else if (option == 1)
        {
        rootPart->doWaterUptake(g.sw_demand);
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
      float swSupply = - sum_real_array(rootPart->dlt_sw_dep, max_layer);

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
                       , Environment.radn
                       , &g.radnIntGreenFruit);

             // calc the total fruit interception - what is left is transmitted to the vegetative parts)
             // fruit is considered to be at top of canopy
          float radnIntTotFruit = 0.0;
          float frIntcRadnTotFruit = g.fr_intc_radn * divide (fruitPart->calcCover(canopy_fac), g.cover_green, 0.0);
          crop_radn_int0(fruitPart->calcCover(canopy_fac)
                       , frIntcRadnTotFruit
                       , Environment.radn
                       , &radnIntTotFruit);

             // calc the total interception
          float radnIntTot = 0.0;
          crop_radn_int0(g.cover_green
                       , g.fr_intc_radn
                       , Environment.radn
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

        fruitPart->doDmPotRUE( g.radnIntGreenFruit);


        float radnIntGreenVeg = g.radn_int - g.radnIntGreenFruit;  //  FIXME temporary until proper fruit class

        float dlt_dm_pot_rue_veg = 0.0;
        plant_dm_pot_rue_veg(&c.rue
                           , radnIntGreenVeg
                           , min(min(min(g.temp_stress_photo, g.nfact_photo),
                               g.oxdef_photo), g.pfact_photo)
                           , &dlt_dm_pot_rue_veg);

        g.dlt_dm_pot_rue = dlt_dm_pot_rue_veg + fruitPart->dltDmPotRue();  // FIXME when fruit is made proper class
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
                         Environment.maxt,
                         Environment.mint,
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

        fruitPart->doTECO2();

        cproc_transp_eff_co2(c.svp_fract, te_coeff,
                             Environment.maxt, Environment.mint, g.co2,
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



void Plant::plant_dm_init (void)
//=======================================================================================
// Set minimum part weights
   {

// Should plant event be called for all parts?
//    doPlantEvent(phenology->stageName());
   if(phenology->on_day_of(phenology->stageName()))
       fruitPart->onDayOf(phenology->stageName());

    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->doDmMin();
   }

void Plant::plant_n_conc_limits(float  g_co2_modifier_n_conc)
//=======================================================================================
//+  Purpose
//       Calculate the critical N concentration below which plant growth
//       is affected.  Also minimum and maximum N concentrations below
//       and above which it is not allowed to fall or rise.
//       These are analogous to the water concentrations
//       of sat, dul and ll.
   {
   // the tops critical N percentage concentration is the stover
   // (non-grain shoot) concentration below which N concentration
   // begins to affect plant growth.
   for (vector<plantPart *>::iterator t = myParts.begin();
        t != myParts.end();
        t++)
      {
      (*t)->doNConccentrationLimits();
      }

   leafPart->g.n_conc_crit *= g_co2_modifier_n_conc;

   if (leafPart->g.n_conc_crit <= leafPart->g.n_conc_min)
      throw std::runtime_error("Aiieeee nconc_crit < nconc_min!");
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
    ,float  g_n_fix_pot         // (INPUT)  N fixation potential (g/m^2)
    ,float  g_root_depth        // (INPUT)  depth of roots (mm)
    ,float  *n_fix_uptake        // (OUTPUT) actual N fixation (g/m^2)
    ,vector<plantPart *> &allParts        // (INPUT) vector of plant parts
    ) {

//+  Local Variables
    int   deepest_layer;                          // deepest layer in which the roots are growing
    float plant_part_fract;                       // fraction of nitrogen to use (0-1) for plant part
    vector<plantPart *>::iterator part;           // iterator
    float n_uptake_sum;                           // total plant N uptake (g/m^2)
    float n_excess;                               // N uptake above N crit (g/m^2)
    vector<float> n_capacity(allParts.size());    // amount of N that can be stored in plant part above Ncrit (g/m^2)
    float n_capacity_sum;                         // total excess N storage (g/m^2)
    float n_demand_sum;                               // total nitrogen demand (g/m^2)
    float n_fix_demand_tot;                       // total demand for N fixation (g/m^2)
    float fix_demand;                             // demand for fixed N per plant part (g/m^
    float fix_part_fract;                         // fraction of fixed N per plant part (g/m
    float dlt_n_green_part;

    // find the proportion of uptake to be distributed to
    // each plant part and distribute it.
    deepest_layer = find_layer_no (g_root_depth, g_dlayer, max_layer);
    n_uptake_sum = - sum_real_array (g_dlt_no3gsm, deepest_layer+1)
                   - sum_real_array (g_dlt_nh4gsm, deepest_layer+1);

    n_demand_sum = nDemand();

    n_capacity_sum = 0.0;
    for (part = allParts.begin(); part != allParts.end(); part++)
       n_capacity_sum += (*part)->nCapacity();

   for (part = allParts.begin(); part != allParts.end(); part++)
      (*part)->doNPartition(n_uptake_sum, n_demand_sum, n_capacity_sum);

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
         (*part)->doNFixRetranslocate (*n_fix_uptake, n_fix_demand_tot);
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
    float dm_part_avail;                          // carbohydrate avail from part(g/m^2)
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
           dm_part_avail = (*part)->DMGreen - (*part)->DMPlantMin * g_plants;
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
        bound_check_real_var (this,fabs((*part)->dltNRetransOut())
                              , 0.0, (*part)->availableRetranslocateN() + tolerence
                              , (string("dlt_N_retrans(") + (*part)->name() + string(")")).c_str() );
        }

    }




void Plant::plant_N_senescence (void)
//=====================================================================
//      Derives seneseced plant nitrogen (g N/m^2)
   {
   float    green_n_conc;  //! N conc of green material (g/g)
   float    sen_n_conc;    //! N conc of senescing material (g/g)
   float    dlt_n_in_senescing_leaf;
   float    navail;
   float    n_demand_tot;

   // Find N in senesced material
   for (vector<plantPart *>::iterator t = myParts.begin();
        t != myParts.end();
        t++)
      (*t)->doNSenescence();

   //! now get N to retranslocate out of senescing leaves
   for (vector<plantPart *>::iterator t = myParts.begin();
        t != myParts.end();
        t++)
      (*t)->zeroDltNSenescedTrans();

   green_n_conc = divide (leafPart->NGreen, leafPart->DMGreen, 0.0);
   dlt_n_in_senescing_leaf = leafPart->dlt.dm_senesced * green_n_conc;

   navail = dlt_n_in_senescing_leaf - leafPart->dlt.n_senesced;
   navail = l_bound(navail, 0.0);

   n_demand_tot = nDemand();
   for (vector<plantPart *>::iterator t = myParts.begin();
        t != myParts.end();
        t++)
      (*t)->doNSenescedRetrans(navail, n_demand_tot);
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
    plant_vpd (c.svp_fract, Environment.maxt, Environment.mint);

    rootPart->plant_root_depth ();

    plant_water_supply (1);

    if (g.plant_status == alive)
        {
        plant_water_uptake (1);
        plant_water_stress ();
        g.oxdef_photo = rootPart->oxdef_stress ();

        phenology->prepare (Environment);

        pheno_stress_t ps;
        ps.swdef = g.swdef_pheno;
        ps.nfact = min(g.nfact_pheno, g.pfact_pheno);
        ps.swdef_flower = g.swdef_pheno_flower;
        ps.swdef_grainfill = g.swdef_pheno_grainfill;
        ps.remove_biom_pheno = g.remove_biom_pheno;

        int layer_no_seed = rootPart->find_layer_no (g.sowing_depth);
        float fasw_seed = divide (rootPart->sw_dep[layer_no_seed] - rootPart->ll_dep[layer_no_seed],
                                rootPart->dul_dep[layer_no_seed] - rootPart->ll_dep[layer_no_seed], 0.0);
         fasw_seed = bound (fasw_seed, 0.0, 1.0);
        float pesw_seed = divide (rootPart->sw_dep[layer_no_seed] - rootPart->ll_dep[layer_no_seed],
                       rootPart->dlayer[layer_no_seed], 0.0);

        phenology->process (Environment, ps, fasw_seed, pesw_seed);

        for (vector<plantPart *>::iterator t = myParts.begin();
             t != myParts.end();
             t++)
           (*t)->morphology();

        leafPart->potential(c.leaf_no_pot_option,
                            min(pow(min(g.nfact_expansion, g.pfact_expansion),2),g.swdef_expansion),
                            phenology->get_dlt_tt() );

        leafPart->leaf_area_stressed (min(g.swdef_expansion, min(g.nfact_expansion, g.pfact_expansion)));

        plant_water_distribute (1);
        plant_bio_water ();
        plant_bio_rue (1);

        plant_dm_init();

        // Calculate DM supply (dlt_dm)
        plant_bio_actual (1);

        // Now calculate DM demands
        for (vector<plantPart *>::iterator t = myParts.begin();
             t != myParts.end();
             t++)
            (*t)->doDmDemand (g.dlt_dm);

        arbitrator->partitionDM(g.dlt_dm, rootPart, leafPart, stemPart, fruitPart);

        plant_bio_retrans ();
        plant_bio_distribute ();  // for fruit class - process bio distribute

        leafPart->actual ();
        fruitPart->calcDlt_pod_area ();

        rootPart->root_length_growth();

        leafPart->leaf_death( min(g.nfact_expansion, g.pfact_expansion), phenology->get_dlt_tt());
        leafPart->leaf_area_sen( g.swdef_photo , Environment.mint);

        plant_sen_bio (c.dm_senescence_option);
        rootPart->sen_length();

        fruitPart->doNInit();
        doNDemandGrain();

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
        plant_soil_nit_demand ();
        plant_nit_uptake (c.n_uptake_option);     // allows preference of N source
        plant_nit_partition ();                  // allows output of n fixed
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

    plant_detachment ();

    plant_cleanup();

    plant_water_stress ();
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
    float dm_chopped;                             // dry matter added to chopped pool (kg/ha)
    float n_chopped;                              // nitrogen added to chopped pool(kg/ha)
    float p_chopped;                              // phosphorus added to chopped pool(g/m^2)
    float dm_root_chopped;                             // dry matter added to chopped pool(kg/ha)
    float n_root_chopped;                              // nitrogen added to chopped pool(kg/ha)
    float p_root_chopped;                              // phosp added to chopped pool(kg/ha)
    float dm_tops_chopped;                             // dry matter added to chopped pool(kg/ha)
    float n_tops_chopped;                              // nitrogen added to chopped pool(kg/ha)
    float p_tops_chopped;                              // phosp added to chopped pool(kg/ha)

    float dm_residue;                             // dry matter added to residue (kg/ha)
    float n_residue;                              // nitrogen added to residue (kg/ha)
    float p_residue;                              // phosphorus added to residue (g/m^2)
    float dm_root_residue;                             // dry matter added to residue (kg/ha)
    float n_root_residue;                              // nitrogen added to residue (kg/ha)
    float p_root_residue;                              // phosp added to residue (kg/ha)
    float dm_tops_residue;                             // dry matter added to residue (kg/ha)
    float n_tops_residue;                              // nitrogen added to residue (kg/ha)
    float p_tops_residue;                              // phosp added to residue (kg/ha)

    float remove_fr;
    float height;                                 // cutting height
    float canopy_fac;
    float temp;
    float dlt_dm_harvest;                         // dry matter harvested (g/m^2)
    float dlt_n_harvest;                          // N content of dm harvested (g/m^2)
    float dlt_p_harvest;                          // N content of dm harvested (kg/ha)
    float dlt_dm_die;                             // dry matter in dieback of roots (g/m^2)
    float dlt_n_die;                              // N content of drymatter in dieback (g/m^2)
    float dlt_p_die;                              // P content of drymatter in dieback (g/m^2)
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
        bound_check_real_var(this,temp, 0.0, 10000.0, "plants");
        g.plants = temp;
        }

    if (incomingApsimVariant.get("remove", protocol::DTsingle, false, remove_fr) == false)
        {
        remove_fr = 0.0;
        }
    bound_check_real_var(this,remove_fr, 0.0, 1.0, "remove");

    // determine the cutting height
    if (incomingApsimVariant.get("height", protocol::DTsingle, false, height) == false)
       {
       height = 0.0;
       }
    bound_check_real_var(this,height, 0.0, 1000.0, "height");

    vector<string> dm_type;
    vector<float>  fraction_to_residue;
    vector<float>  dlt_crop_dm;
    vector<float>  dlt_dm_n;
    vector<float>  dlt_dm_p;

    // Update biomass and N pools.
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
        plant_send_crop_chopped_event (c.crop_type
                                     , dm_type
                                     , dlt_crop_dm
                                     , dlt_dm_n
                                     , dlt_dm_p
                                     , fraction_to_residue);

    dm_residue = 0.0; dm_root_residue = 0.0;
    n_residue = 0.0; n_root_residue = 0.0;
    p_residue = 0.0; p_root_residue = 0.0;
    dm_chopped = 0.0; dm_root_chopped = 0.0;
    n_chopped = 0.0; n_root_chopped = 0.0;
    p_chopped = 0.0; p_root_chopped = 0.0;

    for (unsigned int part=0; part < dm_type.size(); part++)
       {
       dm_chopped += dlt_crop_dm[part];
       n_chopped += dlt_dm_n[part];
       p_chopped += dlt_dm_p[part];
       dm_residue += dlt_crop_dm[part] * fraction_to_residue[part];
       n_residue += dlt_dm_n[part] * fraction_to_residue[part];
       p_residue += dlt_dm_p[part] * fraction_to_residue[part];
       if (dm_type[part] == "root")
          {
          dm_root_residue += dlt_crop_dm[part] * fraction_to_residue[part];
          n_root_residue += dlt_dm_n[part] * fraction_to_residue[part];
          p_root_residue += dlt_dm_p[part] * fraction_to_residue[part];
          dm_root_chopped += dlt_crop_dm[part];
          n_root_chopped += dlt_dm_n[part];
          p_root_chopped += dlt_dm_p[part];
          }
       }

    dm_tops_chopped = dm_chopped - dm_root_chopped;
    n_tops_chopped = n_chopped - n_root_chopped;
    p_tops_chopped = p_chopped - p_root_chopped;

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

    float dm_removed_tops = dm_tops_chopped - dm_tops_residue;
    float dm_removed_root = dm_root_chopped - dm_root_residue;
    float n_removed_tops = n_tops_chopped - n_tops_residue;
    float n_removed_root = n_root_chopped - n_root_residue;
    float p_removed_tops = p_tops_chopped - p_tops_residue;
    float p_removed_root = p_root_chopped - p_root_residue;

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
                          ,leafPart->getLAI()
                          ,&cover_green_leaf);

    cover_pod = fruitPart->calcCover(canopy_fac);
    g.cover_green = add_covers (cover_green_leaf, cover_pod);


    legnew_cover (g.row_spacing
                  ,c.x_row_spacing
                  ,c.y_extinct_coef_dead
                  ,c.num_row_spacing
                  , canopy_fac
                  ,leafPart->getSLAI()
                  ,&g.cover_sen);
    legnew_cover (g.row_spacing
                  ,c.x_row_spacing
                  ,c.y_extinct_coef_dead
                  ,c.num_row_spacing
                  , canopy_fac
                  ,leafPart->getTLAI_dead()
                  ,&g.cover_dead);

// other plant states
    plant_n_conc_limits (g.co2_modifier_n_conc);

    if (g.plant_status == alive &&
        phenology->previousStageName() != phenology->stageName())
        {
        for (vector<plantPart *>::iterator part = myParts.begin(); part != myParts.end(); part++)
           (*part)->onPlantEvent(phenology->stageName());
        plant_event ();
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
    float temp;
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
        bound_check_real_var(this,temp, 0.0, 10000.0, "plants");
        g.plants = temp;
        }

    // Update biomass and N pools.
    for (vector<plantPart *>::iterator part = myParts.begin(); part != myParts.end(); part++)
       (*part)->onKillStem();

    // JNGH need to account for dead pai
    g.pai = 0.0;

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
                          ,leafPart->getLAI()
                          ,&cover_green_leaf);

    cover_pod = fruitPart->calcCover(canopy_fac);
    g.cover_green = add_covers (cover_green_leaf, cover_pod);


    legnew_cover (g.row_spacing
                  , c.x_row_spacing
                  , c.y_extinct_coef_dead
                  , c.num_row_spacing
                  , canopy_fac
                  , leafPart->getSLAI()
                  , &g.cover_sen);
    legnew_cover (g.row_spacing
                  , c.x_row_spacing
                  , c.y_extinct_coef_dead
                  , c.num_row_spacing
                  , canopy_fac
                  , leafPart->getTLAI_dead()
                  , &g.cover_dead);

    plant_n_conc_limits ( g.co2_modifier_n_conc )  ;                  // plant N concentr

    if (g.plant_status == alive &&
        phenology->previousStageName() != phenology->stageName())
        {
        for (vector<plantPart *>::iterator part = myParts.begin(); part != myParts.end(); part++)
           (*part)->onPlantEvent(phenology->stageName());
        plant_event ();
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
    vector<plantPart *>::iterator part;

    float cover_pod;

    float dm_init;
    float n_init;
    float canopy_fac;
//    float fraction_to_residue[max_part];          // fraction sent to residue (0-1)

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
       ostringstream msg;
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

       parent->writeString (msg.str().c_str());
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
    float biomassGreen =  leafPart->DMGreen + stemPart->DMGreen;
    g.remove_biom_pheno = divide (dltBiomassGreen, biomassGreen, 0.0);

    if (c.remove_biomass_report == "on")
    {
       ostringstream msg1;
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

       parent->writeString (msg1.str().c_str());

       ostringstream msg2;
       msg2 << "Crop Biomass Available:-" << endl;
       float dmTotal2 = 0.0;

       msg2 << "   dm green leaf = " << leafPart->DMGreen << " (g/m2)" << endl;
       msg2 << "   dm green stem = " << stemPart->DMGreen << " (g/m2)" << endl;
       dmTotal2 +=  leafPart->DMGreen + stemPart->DMGreen;

       msg2 << "   dm senesced leaf = " << leafPart->DMSenesced << " (g/m2)" << endl;
       msg2 << "   dm senesced stem = " << stemPart->DMSenesced << " (g/m2)" << endl;
       dmTotal2 +=  leafPart->DMSenesced + stemPart->DMSenesced;

       msg2 << "   dm dead leaf = " << leafPart->DMDead << " (g/m2)" << endl;
       msg2 << "   dm dead stem = " << stemPart->DMDead << " (g/m2)" << endl;
       dmTotal2 +=  leafPart->DMDead + stemPart->DMDead;

       msg2 << endl << "   dm total = " << dmTotal2 << " (g/m2)" << endl << ends;

       parent->writeString (msg2.str().c_str());
    }

    // Check sensibility of part deltas
    for (part = allParts.begin(); part != allParts.end(); part++)
    {
      if ((*part)->name() == "leaf" || (*part)->name() == "stem")
      {
        if ((*part)->dlt.dm_green > (*part)->DMGreen + error_margin)
        {
             ostringstream msg;
             msg << "Attempting to remove more green " << (*part)->name() << " biomass than available:-" << endl;
             msg << "Removing " << (*part)->dlt.dm_green << " (g/m2) from " << (*part)->DMGreen << " (g/m2) available." << ends;
             throw std::runtime_error (msg.str().c_str());
        }
        else if ((*part)->dlt.dm_senesced > (*part)->DMSenesced + error_margin)
        {
             ostringstream msg;
             msg << "Attempting to remove more senesced " << (*part)->name() << " biomass than available:-" << endl;
             msg << "Removing " << (*part)->dlt.dm_senesced << " (g/m2) from " << (*part)->DMSenesced << " (g/m2) available." << ends;
             throw std::runtime_error (msg.str().c_str());
        }
        else if ((*part)->dlt.dm_dead > (*part)->DMDead + error_margin)
        {
             ostringstream msg;
             msg << "Attempting to remove more dead " << (*part)->name() << " biomass than available:-" << endl;
             msg << "Removing " << (*part)->dlt.dm_dead << " (g/m2) from " <<(*part)->DMDead << " (g/m2) available." << ends;
             throw std::runtime_error (msg.str().c_str());
        }
        else
        { // no more checks
        }
      }
    }

    // Update biomass and N pools.  Different types of plant pools are affected in different ways.
    // Calculate Root Die Back
    float dm_removed_root = 0.0, n_removed_root = 0.0;
    float chop_fr_green_leaf = divide(leafPart->dlt.dm_green, leafPart->DMGreen, 0.0);
    float dlt_dm_die = rootPart->DMGreen * c.root_die_back_fr * chop_fr_green_leaf;
    rootPart->DMSenesced = rootPart->DMSenesced + dlt_dm_die;
    rootPart->DMGreen = rootPart->DMGreen - dlt_dm_die;
    dm_removed_root = 0.0 /*dlt_dm_die??xx*/;

    float dlt_n_die = dlt_dm_die * rootPart->c.n_sen_conc;
    rootPart->NSenesced = rootPart->NSenesced + dlt_n_die;
    rootPart->NGreen= rootPart->NGreen - dlt_n_die;
    n_removed_root = 0.0 /*dlt_n_die??xx*/;

    float dm_removed_tops = 0.0;
    float n_removed_tops = 0.0;
    for (part = topsParts.begin(); part != topsParts.end(); part++)
        {
        float chop_fr_green = divide((*part)->dlt.dm_green, (*part)->DMGreen, 0.0);
        float chop_fr_sen   = divide((*part)->dlt.dm_senesced, (*part)->DMSenesced, 0.0);
        float chop_fr_dead  = divide((*part)->dlt.dm_dead, (*part)->DMDead, 0.0);

        dm_removed_tops += ((*part)->dlt.dm_green + (*part)->dlt.dm_senesced + (*part)->dlt.dm_dead) * gm2kg/sm2ha;
        n_removed_tops += ((*part)->NGreen*chop_fr_green +
                      (*part)->NSenesced*chop_fr_sen +
                      (*part)->NDead*chop_fr_dead) * gm2kg/sm2ha;

        (*part)->DMGreen -= (*part)->dlt.dm_green;
        (*part)->DMSenesced -= (*part)->dlt.dm_senesced;
        (*part)->DMDead -= (*part)->dlt.dm_dead;

        (*part)->NGreen -= (*part)->NGreen * chop_fr_green;
        (*part)->NSenesced -= (*part)->NSenesced * chop_fr_sen;
        (*part)->NDead -= (*part)->NDead * chop_fr_dead;

        (*part)->PGreen -= (*part)->PGreen * chop_fr_green;
        (*part)->PSen -= (*part)->PSen * chop_fr_sen;
        (*part)->PDead -= (*part)->PDead * chop_fr_dead;
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

    leafPart->remove_biomass_update();

    stemPart->Width *= (1.0 - divide(stemPart->dlt.dm_green, stemPart->DMGreen, 0.0));
    reproStruct->Width *= (1.0 - divide(reproStruct->dlt.dm_green, reproStruct->DMGreen, 0.0));

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
                          ,leafPart->getLAI()
                          ,&cover_green_leaf);

    cover_pod = fruitPart->calcCover(canopy_fac);
    g.cover_green = add_covers (cover_green_leaf, cover_pod);


    legnew_cover (g.row_spacing
                  ,c.x_row_spacing
                  ,c.y_extinct_coef_dead
                  ,c.num_row_spacing
                  , canopy_fac
                  ,leafPart->getSLAI()
                  ,&g.cover_sen);
    legnew_cover (g.row_spacing
                  ,c.x_row_spacing
                  ,c.y_extinct_coef_dead
                  ,c.num_row_spacing
                  , canopy_fac
                  ,leafPart->getTLAI_dead()
                  ,&g.cover_dead);

    phenology->onRemoveBiomass(g.remove_biom_pheno);

    // other plant states
    stemPart->Height *= (1.0 - divide(stemPart->dlt.dm_green, stemPart->DMGreen, 0.0));
    reproStruct->Height *= (1.0 - divide(reproStruct->dlt.dm_green, reproStruct->DMGreen, 0.0));

    plant_n_conc_limits ( g.co2_modifier_n_conc );

    if (g.plant_status == alive &&
        phenology->previousStageName() != phenology->stageName())
        {
        for (vector<plantPart *>::iterator part = myParts.begin(); part != myParts.end(); part++)
           (*part)->onPlantEvent(phenology->stageName());
        plant_event ();
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
      g.fr_intc_radn = 0.0;
      Environment.year = 0.0;
      Environment.day_of_year = 0.0;
      Environment.latitude = 0.0;
      Environment.mint = 0.0;
      Environment.maxt = 0.0;
      Environment.radn = 0.0;
      fill_real_array (g.soil_temp, 0.0, 366+1);
      g.eo = 0.0;
      g.canopy_width = 0.0;
      g.plants = 0.0;
      g.dlt_plants = 0.0;
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
      g.dlt_dm_retrans_to_fruit = 0.0;
      g.dlt_dm_parasite  =  0.0;
      g.dlt_dm_parasite_demand = 0.0;
      g.dlt_sw_parasite_demand = 0.0;
      g.radn_int = 0.0;
      g.radnIntGreenFruit = 0.0;
      g.transp_eff = 0.0;
      g.lai_canopy_green = 0.0;
      g.leaf_no_final = 0.0;


      g.grain_n_supply = 0.0;
      g.n_fix_pot = 0.0;
      fill_real_array (g.no3gsm_uptake_pot, 0.0, max_layer);
      fill_real_array (g.nh4gsm_uptake_pot, 0.0, max_layer);
      g.n_fix_uptake = 0.0;
      g.n_fixed_tops = 0.0;


      g.sw_demand = 0.0;
      g.sw_demand_te = 0.0;
      g.swDemandTEFruit = 0.0;
      g.swSupplyFruit   = 0.0;
      g.swSupplyVeg   = 0.0;


      g.transpiration_tot = 0.0;
      g.n_uptake_tot = 0.0;
      g.n_demand_tot = 0.0;
      g.n_conc_act_stover_tot = 0.0;
      g.n_conc_crit_stover_tot = 0.0;
      g.n_uptake_stover_tot = 0.0;
      g.lai_max = 0.0;
      g.ext_n_demand = 0.0;
      g.ext_sw_demand = 0.0;



      p.eo_crop_factor = 0.0;

      //       plant Constants
      c.n_uptake_option = 0;
      c.leaf_no_pot_option = 0;

      c.no3_uptake_max = 0.0;
      c.no3_conc_half_max = 0.0;

      c.crop_type = "";
      c.default_crop_class = "";
      c.remove_biomass_report = "off";

      c.n_supply_preference = "";
      fill_real_array (c.x_ws_root , 0.0, max_table);
      fill_real_array (c.y_ws_root_fac , 0.0, max_table);
      c.num_ws_root = 0;


      c.n_fact_photo = 0.0;
      c.n_fact_pheno = 0.0;
      c.n_fact_expansion = 0.0;
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
      c.svp_fract = 0.0;
      fill_real_array (c.transp_eff_cf, 0.0, max_table);
      c.grain_n_conc_min = 0.0;
      c.seed_wt_min = 0.0;
      c.no3_diffn_const = 0.0;
      fill_real_array (c.n_fix_rate, 0.0,max_table);
      c.leaf_init_rate = 0.0;
      c.leaf_no_seed = 0.0;
      c.swdf_grain_min = 0.0;
      c.hi_min = 0.0;
      c.sfac_slope = 0.0;
      c.tfac_slope = 0.0;
      c.sw_fac_max = 0.0;
      c.temp_fac_min = 0.0;
      c.spla_slope = 0.0;
      c.sen_threshold = 0.0;
      c.grn_water_cont = 0.0;
      c.leaf_trans_frac = 0.0;
      c.htstress_coeff = 0.0;
      c.temp_grain_crit_stress = 0.0;
      c.n_fact_lf_sen_rate = 0.0;
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

      c.no3_ub = 0.0;
      c.no3_lb = 0.0;
      c.nh4_ub = 0.0;
      c.nh4_lb = 0.0;
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
      c.root_die_back_fr = 0.0;
      c.class_action.clear();
      c.class_change.clear();
      c.eo_crop_factor_default = 0.0;

      // parasite
      g.dlt_dm_parasite_demand =  0.0;
      g.dlt_sw_parasite_demand = 0.0;
      g.dm_parasite_retranslocate = 0.0;
      g.dlt_dm_parasite = 0.0;

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

    fill_real_array (g.soil_temp , 0.0, 366);

//    fill_real_array (p.ll_dep , 0.0, max_layer);

    g.dlt_plants_death_external     = 0.0;
    g.lai_max               = 0.0;

    g.plants                = 0.0;
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



    g.grain_n_supply = 0.0;



    fill_real_array (g.no3gsm_uptake_pot, 0.0, max_layer);
    fill_real_array (g.nh4gsm_uptake_pot, 0.0, max_layer);

    g.swSupplyFruit   = 0.0;
    g.swSupplyVeg   = 0.0;
    g.dlt_dm                   = 0.0;

    g.dlt_plants               = 0.0;
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
                              , ll, 0.0, rootPart->sw_ub, true))
       {
       for (unsigned int layer = 0; layer != ll.size(); layer++)
          rootPart->ll_dep[layer] = ll[layer]*rootPart->dlayer[layer];

       if ((int)ll.size() != rootPart->num_layers)
          {
          char  msg[200];                               // output string
          sprintf (msg, "%7d %7d", rootPart->num_layers, ll.size());
          parent->writeString (msg);
          parent->warningError ("LL parameter doesn't match soil profile?");
          }
       }
    else
       {
       unsigned int id = parent->addRegistration(RegistrationType::get,
                                                 "ll15", floatArrayType,
                                                 "", "");
       parent->getVariable(id, ll, 0.0, rootPart->sw_ub, true);
       if (ll.size() == 0)
          throw std::runtime_error("No Crop Lower Limit found");

       for (unsigned int i=0; i< ll.size(); i++) rootPart->ll_dep[i] = ll[i]*rootPart->dlayer[i];
       parent->writeString ("   Using externally supplied Lower Limit (ll15)");
       }

//    rootPart->readRootParameters(parent, "parameters");

    doPInit(parent);
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
    char  msg[200];                               // output string
    FString  dummy;                               // dummy variable

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
           bound_check_real_var(this,g.plants, 0.0, 1000.0, "plants");

           if (incomingApsimVariant.get("sowing_depth", protocol::DTsingle, false, g.sowing_depth) == false)
               {
               throw std::invalid_argument("sowing_depth not specified");
               }
           bound_check_real_var(this,g.sowing_depth, 10.0, 200.0, "sowing_depth");

           if (incomingApsimVariant.get("row_spacing", protocol::DTsingle, false, g.row_spacing) == false)
               {
               g.row_spacing = c.row_spacing_default;
               }
           bound_check_real_var(this,g.row_spacing, 0.0, 2000.0, "row_spacing");


           if (incomingApsimVariant.get("skipplant", protocol::DTsingle, false, g.skip_plant) == false)
               {
               g.skip_plant = c.skip_plant_default;
               }
           bound_check_real_var(this,g.skip_plant, 0.0, 2.0, "skipplant");
           g.skip_plant_fac = (2.0 + g.skip_plant)/2.0;

           if (incomingApsimVariant.get("skiprow", protocol::DTsingle, false, g.skip_row) == false)
               {
               g.skip_row = c.skip_row_default;
               }
           bound_check_real_var(this,g.skip_row, 0.0, 2.0, "skiprow");
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
                  , Environment.day_of_year, g.sowing_depth
                  , g.plants, g.row_spacing
                  , g.skip_row, g.skip_plant, g.cultivar.c_str());
           parent->writeString (msg);

           parent->writeString ("    ------------------------------------------------\n");

         }

         else
         {
            ostringstream msg;
            msg << g.module_name << " was taken out today by \"end_crop\" action -" << endl;
            msg << " Unable to accept \"sow\" action until the next day." << endl << ends;
            throw std::runtime_error (msg.str());
         }
    }
    else
    {
         ostringstream msg;
         msg << g.module_name << " is still in the ground -" << endl;
         msg << " Unable to sow until it is taken out by \"end_crop\" action." << endl << ends;
         throw std::runtime_error (msg.str().c_str());

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

void Plant::plant_read_root_params ()
//============================================================================
//  Get root profile parameters
    {
    const char*  section_name = "parameters" ;
    char  msg[200];

    //       cproc_sw_demand_bound
    if (parent->readParameter (section_name
                , "eo_crop_factor"//, "()"
                , p.eo_crop_factor
                , 0.0, 100., true) == false)
        {
        p.eo_crop_factor = c.eo_crop_factor_default;
        }

    rootPart->readRootParameters(parent, section_name);


    sprintf (msg, "%s%5.1f%s"
        ,"    Crop factor for bounding water use is set to "
        , p.eo_crop_factor
          , " times eo.");
    parent->writeString (msg);

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
        // put stover and any remaining grain into surface residue,
        //     any roots to soil FOM pool
        dm_residue =topsTot();
        n_residue =topsNTot();
        p_residue =topsPTot();

        dm_root = rootPart->dmGreen()+ rootPart->dmDead() + rootPart->dmSenesced();
        n_root  = rootPart->nGreen() + rootPart->nDead() + rootPart->nSenesced();
        p_root  = rootPart->pGreen() + rootPart->pDead() + rootPart->pSenesced();

       if (dm_residue + dm_root > 0.0)
          {
          // Build surface residues by part
          vector<string> part_name;
          vector<float> fraction_to_residue;           // fraction sent to residue (0-1)
          vector<float> dlt_dm_crop;                   // change in dry matter of crop (kg/ha)
          vector<float> dlt_dm_n;                      // N content of changeed dry matter (kg/ha)
          vector<float> dlt_dm_p;                      // P content of changeed dry matter (kg/ha)

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
//            call plant_kill_crop (g%plant_status)
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
          plant_store_value (Environment.day_of_year
                           , Environment.year
                           , g.soil_temp
                           , soil_temp);
          }
       }

    parent->getVariable(id.fr_intc_radn, g.fr_intc_radn, 0.0, 1.0, true);

    // Soilwat2
    parent->getVariable(id.eo, g.eo, 0.0, 20.0);

    rootPart->getOtherVariables(parent);

    //assert (values.size() == Environment.num_layers);

    values.clear();
    if (!parent->getVariable(id.no3, values, c.no3_lb, c.no3_ub, true))
        {
        // we have no N supply - make non-limiting.
        for (int i = 0; i < rootPart->num_layers; i++)
           values.push_back(10000.0);
        }
    for (int i = 0; i < rootPart->num_layers; i++)
       {
       rootPart->no3gsm[i] = values[i] * kg2gm /ha2sm;
       }

    values.clear();
    if (!parent->getVariable(id.nh4, values, c.nh4_lb, c.nh4_ub, true))
        {
        // we have no N supply - make non-limiting.
        for (int i = 0; i < rootPart->num_layers; i++)
           values.push_back(10000.0);
        }
    for (int i = 0; i < rootPart->num_layers; i++)
       {
       rootPart->nh4gsm[i] = values[i] * kg2gm /ha2sm;
       }

    if (!parent->getVariable(id.co2, g.co2, 0.0, 1500.0, true))
       {
       g.co2 = c.co2_default;
       }

    //Environment.num_layers = count_of_real_vals(g.dlayer, max_layer);
    //Environment.dlayer = vector<float>(g.dlayer, g.dlayer + Environment.num_layers);

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

    if (Str_i_Eq(rootPart->uptake_source, "calc"))
        {
        //!!! perhaps we should get number of layers at init and keep it
        num_layers = rootPart->num_layers;

        for (layer = 0; layer< num_layers;layer++) {scratch[layer] = rootPart->dlt_no3gsm[layer] * gm2kg /sm2ha;}
        protocol::vector<float> dlt_no3_values(scratch, scratch+num_layers);
        parent->setVariable(id.dlt_no3, dlt_no3_values);

        for (layer = 0; layer< num_layers;layer++) {scratch[layer] = rootPart->dlt_nh4gsm[layer] * gm2kg /sm2ha;}
        protocol::vector<float> dlt_nh4_values(scratch, scratch+num_layers);
        parent->setVariable(id.dlt_nh4, dlt_nh4_values);

        rootPart->UpdateOtherVariables(parent);

        }
    else
        {
        // no need to send updates
        }

    pop_routine (my_name);
    return;
    }


void Plant::plant_update_other_variables (void)
//=======================================================================================
//  Update other modules states
    {
    vector<string> part_name;
    vector<float> dm_residue;                   // change in dry matter of crop (kg/ha)
    vector<float> dm_n;                      // N content of changeed dry matter (kg/ha)
    vector<float> dm_p;                      // P content of changeed dry matter (kg/ha)
    vector<float> fraction_to_residue;       // fraction of DM sent to surface residues

    // dispose of detached material from senesced parts in the live population
    for (vector<plantPart *>::iterator t = myParts.begin();
         t != myParts.end();
         t++)
       (*t)->collectDetachedForResidue(part_name,
                                       dm_residue,
                                       dm_n,
                                       dm_p,
                                       fraction_to_residue);

    if (sum(dm_residue) > 0.0)
       plant_send_crop_chopped_event (c.crop_type,
                                      part_name,
                                      dm_residue,
                                      dm_n,
                                      dm_p,
                                      fraction_to_residue);

    // now dispose of dead population detachments
    dm_residue.clear();
    dm_n.clear();
    dm_p.clear();
    fraction_to_residue.clear();

    for (vector<plantPart *>::iterator t = myParts.begin();
         t != myParts.end();
         t++)
       (*t)->collectDeadDetachedForResidue(part_name,
                                           dm_residue,
                                           dm_n,
                                           dm_p,
                                           fraction_to_residue);

    if (sum(dm_residue) > 0.0)
       {
       plant_send_crop_chopped_event(c.crop_type,
                                     part_name,
                                     dm_residue,
                                     dm_n,
                                     dm_p,
                                     fraction_to_residue);
       }

    rootPart->updateOthers();
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


//- Implementation Section ----------------------------------

    push_routine (my_name);

    // call write_string (new_line            //"    - reading constants");

    c.crop_type = parent->readParameter (section_name, "crop_type");

    c.default_crop_class = parent->readParameter (section_name, "default_crop_class");

    //string scratch = parent->readParameter (section_name, "part_names");
    //Split_string(scratch, " ", c.part_names);



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
    , "no3_ub"//, "(kg/ha)"
    , c.no3_ub
    , 0.0, 100000.0);

    parent->readParameter (section_name
    , "no3_lb"//, "(kg/ha)"
    , c.no3_lb
    , 0.0, 100000.0);

    parent->readParameter (section_name
    , "nh4_ub"//, "(kg/ha)"
    , c.nh4_ub
    , 0.0, 100000.0);

    parent->readParameter (section_name
    , "nh4_lb"//, "(kg/ha)"
    , c.nh4_lb
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
    plant_vpd (c.svp_fract, Environment.maxt, Environment.mint);

    for (vector<plantPart *>::iterator t = myParts.begin(); t != myParts.end(); t++)
       (*t)->prepare();

    plant_nit_stress (c.n_stress_option);
    plant_temp_stress ();
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
      id = parent->addEvent(i->c_str(), RegistrationType::respondToEvent, fn, "");

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

    // Kill off last arbitrator, and get a new one
    arbitrator->undoRegistrations(parent);
    myThings.erase(remove(myThings.begin(), myThings.end(), arbitrator),myThings.end());
    delete arbitrator;
    arbitrator = constructArbitrator(this, parent->readParameter (search_order, "partition_option"));
    arbitrator->doRegistrations(parent);
    myThings.push_back(arbitrator);

    for (vector<plantThing *>::iterator t = myThings.begin();
        t != myThings.end();
        t++)
      (*t)->readSpeciesParameters(parent, search_order);

    c.rue.search(parent, search_order,
                 "x_stage_rue", "()", 0.0, 1000.0,
                 "y_rue", "(g dm/mj)", 0.0, 1000.0);

    parent->readParameter (search_order,
                      "n_fix_rate"//, "()"
                     , c.n_fix_rate, numvals
                     , 0.0, 1.0);

    parent->readParameter (search_order,
                      "transp_eff_cf"//, "(kpa)"
                     , c.transp_eff_cf, numvals
                     , 0.0, 1.0);

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

    parent->readParameter (search_order
                   ,"root_die_back_fr"//, "(0-1)"
                   , c.root_die_back_fr
                   , 0.0, 0.99);



// TEMPLATE OPTION
//    plant_leaf_area

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



     parent->readParameter (search_order
                        , "n_retrans_option"//, "()"
                        , c.n_retrans_option
                        , 1, 2);

    //    plant_event
    parent->readParameter (search_order
                   ,"grn_water_cont"//, "(g/g)"
                   , c.grn_water_cont
                   , 0.0, 1.0);

    //    plant_dm_senescence
    parent->readParameter (search_order
                      , "dm_senescence_option"//, "()"
                      , c.dm_senescence_option
                      , 1, 3);

    //    plant_phenology_init
    parent->readParameter (search_order
                   , "twilight"//, "(o)"
                   , c.twilight
                   , -90.0, 90.0);

    //    plant_n_senescence
    parent->readParameter (search_order
                     , "n_senescence_option"//, "()"
                     , c.n_senescence_option
                     , 1, 2);

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
                     , "x_ws_root"//,  "()"
                     , c.x_ws_root, c.num_ws_root
                     , 0.0, 1.0);

    parent->readParameter (search_order
                     , "y_ws_root_fac"//, "()"
                     , c.y_ws_root_fac, c.num_ws_root
                     , 0.0, 1.0);


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
    float n_grain;                                // total grain N uptake (kg/ha)
    float n_dead;                                 // above ground dead plant N (kg/ha)
    float n_green;                                // above ground green plant N (kg/ha)
    float n_senesced;                             // above ground senesced plant N (kg/ha)
    float n_stover;                               // nitrogen content of stover (kg\ha)
    float n_total;                                // total gross nitrogen content (kg/ha)
    float n_grain_conc_percent;                   // grain nitrogen %
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

    float dmRoot = (rootPart->DMGreen + rootPart->DMDead + rootPart->DMSenesced) * gm2kg / sm2ha;
    float nRoot = (rootPart->NGreen + rootPart->NDead + rootPart->NSenesced) * gm2kg / sm2ha;

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
             , " number of leaves       = ", leafPart->getLeafNo());
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
                                           ,vector<float>  &fraction_to_residue) // (INPUT) fraction going to residue
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
    float profile_depth;                          // depth of soil profile (mm)

//+  Constant Values
    const char*  myname = "doNewProfile" ;

//- Implementation Section ----------------------------------
    push_routine (myname);


rootPart->doNewProfile(v);


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

//- Implementation Section ----------------------------------

    push_routine (my_name);

    parent->getVariable(id.latitude, Environment.latitude, c.latitude_lb, c.latitude_ub);

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

float Plant::getLeafNo(void) const
{
   return leafPart->getLeafNo();
}

void Plant::get_height(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, stemPart->Height);
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
   vector<float>  dm_min;

   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dm_plant_min(dm_min);

   system->sendVariable(qd, dm_min);
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



void Plant::get_cep(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, (float)fabs(g.transpiration_tot));
}





// plant nitrogen
void Plant::get_n_conc_stover(protocol::Component *system, protocol::QueryValueData &qd)
{
    float n_conc = divide (stoverNGreen(), stoverGreen(), 0.0) * fract2pcnt;
    system->sendVariable(qd, n_conc);
}

void Plant::get_n_conc_crit(protocol::Component *system, protocol::QueryValueData &qd)
{
    float n_conc = divide ((leafPart->g.n_conc_crit*leafPart->DMGreen
                           + stemPart->g.n_conc_crit*stemPart->DMGreen)
                          , (leafPart->DMGreen + stemPart->DMGreen)
                          , 0.0) * fract2pcnt;
    system->sendVariable(qd, n_conc);
}

void Plant::get_n_conc_min(protocol::Component *system, protocol::QueryValueData &qd)
{
    float n_conc = divide ((leafPart->g.n_conc_min * leafPart->DMGreen
                            + stemPart->g.n_conc_min * stemPart->DMGreen)
                          , (leafPart->DMGreen + stemPart->DMGreen)
                          , 0.0) * fract2pcnt;
    system->sendVariable(qd, n_conc);
}


void Plant::get_n_uptake_stover(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, stoverNGreen());
}


void Plant::get_n_demand(protocol::Component *system, protocol::QueryValueData &qd)
{
    float n_demand = nDemand();
    system->sendVariable(qd, n_demand);
}

void Plant::get_n_demanded(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<float>  n_demanded;

   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_n_demanded(n_demanded);

   systemInterface->sendVariable(qd, n_demanded);
}

void Plant::get_n_supply_soil(protocol::Component *system, protocol::QueryValueData &qd)
{
    int deepest_layer = rootPart->find_layer_no (rootPart->root_depth);
    float n_uptake_sum = sum_real_array (rootPart->dlt_no3gsm, deepest_layer+1)
                       +  sum_real_array (rootPart->dlt_nh4gsm, deepest_layer+1);
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



void Plant::get_no3_demand(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, (float)g.ext_n_demand*gm2kg/sm2ha);
}

void Plant::get_sw_demand_te(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, g.sw_demand_te);
}

void Plant::get_no3gsm_uptake_pot(protocol::Component *system, protocol::QueryValueData &qd)
{
    int num_layers = rootPart->num_layers;
    system->sendVariable(qd, protocol::vector<float>(g.no3gsm_uptake_pot, g.no3gsm_uptake_pot+num_layers));
}

void Plant::get_nh4gsm_uptake_pot(protocol::Component *system, protocol::QueryValueData &qd)
{
    int num_layers = rootPart->num_layers;
    system->sendVariable(qd, protocol::vector<float>(g.nh4gsm_uptake_pot, g.nh4gsm_uptake_pot+num_layers));
}

void Plant::get_no3_swfac(protocol::Component *system, protocol::QueryValueData &qd)
{
    float swfac[max_layer];
    int num_layers = rootPart->num_layers;
    for (int layer=0; layer < num_layers; layer++)
        {
        swfac[layer] = pow(divide(rootPart->sw_avail[layer],rootPart->sw_avail_pot[layer],0.0), 2);
        swfac[layer] = bound(swfac[layer],0.0,1.0);
        }

    system->sendVariable(qd, protocol::vector<float>(swfac, swfac+num_layers));
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
   vector<float>  dm_green;

   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dm_green(dm_green);

   systemInterface->sendVariable(qd, dm_green);
}

void Plant::get_dm_dead(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<float>  dm_dead;

   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dm_dead(dm_dead);

   systemInterface->sendVariable(qd, dm_dead);
}

void Plant::get_dm_senesced(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<float>  dm_senesced;

   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dm_senesced(dm_senesced);

   systemInterface->sendVariable(qd, dm_senesced);
}

void Plant::get_dlt_dm_green(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<float>  dlt_dm_green;

   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dlt_dm_green(dlt_dm_green);

   systemInterface->sendVariable(qd, dlt_dm_green);
}
void Plant::get_dlt_dm_green_retrans(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<float>  dlt_dm_green_retrans;

   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dlt_dm_green_retrans(dlt_dm_green_retrans);

   systemInterface->sendVariable(qd, dlt_dm_green_retrans);
}
void Plant::get_dlt_dm_detached(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<float>  dlt_dm_detached;

   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dlt_dm_detached(dlt_dm_detached);

   systemInterface->sendVariable(qd, dlt_dm_detached);
}
void Plant::get_dlt_dm_senesced(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<float>  dlt_dm_senesced;

   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dlt_dm_senesced(dlt_dm_senesced);

   systemInterface->sendVariable(qd, dlt_dm_senesced);
}
void Plant::get_dlt_dm_dead_detached(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<float>  dlt_dm_dead_detached;

   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dlt_dm_dead_detached(dlt_dm_dead_detached);

   systemInterface->sendVariable(qd, dlt_dm_dead_detached);
}
void Plant::get_dlt_dm_green_dead(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<float>  dlt_dm_green_dead;

   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dlt_dm_green_dead(dlt_dm_green_dead);

   systemInterface->sendVariable(qd, dlt_dm_green_dead);
}
void Plant::get_dlt_dm_senesced_dead(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<float>  dlt_dm_senesced_dead;

   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dlt_dm_senesced_dead(dlt_dm_senesced_dead);

   systemInterface->sendVariable(qd, dlt_dm_senesced_dead);
}
void Plant::get_n_green(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<float>  n_green;

   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_n_green(n_green);

   systemInterface->sendVariable(qd, n_green);
}
void Plant::get_n_senesced(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<float>  n_senesced;

   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_n_senesced(n_senesced);

   systemInterface->sendVariable(qd, n_senesced);
}
void Plant::get_n_dead(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<float>  n_dead;

   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_n_dead(n_dead);

   systemInterface->sendVariable(qd, n_dead);
}
void Plant::get_dlt_n_green(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<float>  n_green;

   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dlt_n_green(n_green);

   systemInterface->sendVariable(qd, n_green);
}
void Plant::get_dlt_n_dead(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<float>  n_dead;

   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dlt_n_dead(n_dead);

   systemInterface->sendVariable(qd, n_dead);
}
void Plant::get_dlt_n_retrans(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<float>  n_retrans;

   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dlt_n_retrans(n_retrans);

   systemInterface->sendVariable(qd, n_retrans);
}
void Plant::get_dlt_n_senesced(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<float>  n_senesced;

   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dlt_n_senesced(n_senesced);

   systemInterface->sendVariable(qd, n_senesced);
}

void Plant::get_dlt_n_senesced_dead(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<float>  dlt_n_senesced_dead;

   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dlt_n_senesced_dead(dlt_n_senesced_dead);

   systemInterface->sendVariable(qd, dlt_n_senesced_dead);
}

void Plant::get_dlt_n_senesced_retrans(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<float>  n_senesced_retrans;

   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dlt_n_senesced_retrans(n_senesced_retrans);

   systemInterface->sendVariable(qd, n_senesced_retrans);
}
void Plant::get_dlt_n_senesced_trans(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<float>  n_senesced_trans;

   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dlt_n_senesced_trans(n_senesced_trans);

   systemInterface->sendVariable(qd, n_senesced_trans);
}
void Plant::get_dlt_n_detached(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<float>  n_detached;

   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dlt_n_detached(n_detached);

   systemInterface->sendVariable(qd, n_detached);
}
void Plant::get_dlt_n_dead_detached(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<float>  n_dead_detached;

   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dlt_n_dead_detached(n_dead_detached);

   systemInterface->sendVariable(qd, n_dead_detached);
}

void Plant::get_p_demand(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<float>  p_green;

   float p_demand = 0.0;
   for (part = myParts.begin(); part != myParts.end(); part++)
      p_demand += (*part)->pDemand();

   systemInterface->sendVariable(qd, p_demand);   //(g/m^2
}

void Plant::get_p_demand_parts(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<float>  p_demand;

   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_p_demand(p_demand);

   systemInterface->sendVariable(qd, p_demand);   //(g/m^2
}

void Plant::get_p_green(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<float>  p_green;

   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_p_green(p_green);

   systemInterface->sendVariable(qd, p_green);
}
void Plant::get_p_dead(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<float>  p_dead;

   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_p_dead(p_dead);

   systemInterface->sendVariable(qd, p_dead);
}
void Plant::get_p_sen(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<float>  p_sen;

   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_p_sen(p_sen);

   systemInterface->sendVariable(qd, p_sen);
}

void Plant::get_dlt_p_green(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<float>  dlt_p_green;

   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dlt_p_green(dlt_p_green);

   systemInterface->sendVariable(qd, dlt_p_green);
}
void Plant::get_dlt_p_retrans(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<float>  dlt_p_retrans;

   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dlt_p_retrans(dlt_p_retrans);

   systemInterface->sendVariable(qd, dlt_p_retrans);
}
void Plant::get_dlt_p_detached(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<float>  dlt_p_detached;

   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dlt_p_detached(dlt_p_detached);

   systemInterface->sendVariable(qd, dlt_p_detached);
}
void Plant::get_dlt_p_dead(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<float>  dlt_p_dead;

   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dlt_p_dead(dlt_p_dead);

   systemInterface->sendVariable(qd, dlt_p_dead);
}

void Plant::get_dlt_p_sen(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<float>  dlt_p_sen;

   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dlt_p_sen(dlt_p_sen);

   systemInterface->sendVariable(qd, dlt_p_sen);
}




float Plant::getStageCode(void) const {return phenology->stageCode();}
float Plant::getStageNumber(void) const {return phenology->stageNumber();}
float Plant::getPlants(void) const {return g.plants;}
float Plant::getCo2(void) const {return g.co2;}
photosynthetic_pathway_t Plant::getPhotosynthetic_pathway(void) const {return c.photosynthetic_pathway;}
//float Plant::getRadnInterceptedPod(void) const {return g.radn_int_pod;}
float Plant::getDltDMPotRueVeg(void) const {return g.dlt_dm_pot_rue - fruitPart->dltDmPotRue();}
float Plant::getDmGreenVeg(void) const {return leafPart->DMGreen + stemPart->DMGreen;}
//float Plant::getDltDmVeg(void) const {return leafPart->dltDmTotal() + stemPart->dltDmTotal();}
float Plant::getWaterSupplyPod(void) const {return g.swSupplyFruit;}
float Plant::getDmTops(void) const{ return topsGreen()+topsSenesced();}
float Plant::getDltDm(void) const{ return g.dlt_dm;}
float Plant::getDmVeg(void) const {return leafPart->dmTotal() + stemPart->dmTotal();}
float Plant::getDmGreenStem(void) const {return stemPart->DMGreen;}
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
     return  rootPart->DMGreen + fruitPart->dmGreen() + leafPart->DMGreen + stemPart->DMGreen;
                    //     + reproStruct->DMGreen;
   }
float Plant::plantSenesced(void) const
   {
      return  rootPart->DMSenesced + fruitPart->dmSenesced() + leafPart->DMSenesced + stemPart->DMSenesced;
                   //      + reproStruct->DMSenesced;
   }
float Plant::plantDead(void) const
   {
      return  rootPart->DMDead + fruitPart->dmDead() + leafPart->DMDead + stemPart->DMDead;
                 //        + reproStruct->DMDead;
   }
float Plant::plantDltDmGreen(void) const
   {
      return  rootPart->dlt.dm_green + fruitPart->dltDmGreen() + leafPart->dlt.dm_green + stemPart->dlt.dm_green;
                       //  + reproStruct->dlt.dm_green;
   }
float Plant::plantTot(void) const
   {
      return  plantGreen() + plantSenesced() + plantDead();
    }

float Plant::topsGreen(void) const
   {
     return  fruitPart->dmGreen() + leafPart->DMGreen + stemPart->DMGreen;
                    //     + reproStruct->DMGreen;
   }
float Plant::topsSenesced(void) const
   {
      return  fruitPart->dmSenesced() + leafPart->DMSenesced + stemPart->DMSenesced;
                   //      + reproStruct->DMSenesced;
   }
float Plant::topsDead(void) const
   {
      return  fruitPart->dmDead() + leafPart->DMDead + stemPart->DMDead;
                 //        + reproStruct->DMDead;
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
      return  leafPart->DMGreen + fruitPart->dmGreenVegTotal() + stemPart->DMGreen;
              //           + reproStruct->DMGreen;
    }
float Plant::stoverSenesced(void) const
   {
      return  leafPart->DMSenesced + fruitPart->dmSenescedVegTotal() +  stemPart->DMSenesced;
                    //     + reproStruct->DMSenesced;
    }
float Plant::stoverDead(void) const
   {
      return  leafPart->DMDead + fruitPart->dmDeadVegTotal() +  stemPart->DMDead;
                  //       + reproStruct->DMDead;
    }

float Plant::stoverTot(void) const
   {
      return  stoverGreen() + stoverSenesced() + stoverDead();
    }

float Plant::plantNGreen(void) const
   {
      return  rootPart->NGreen + fruitPart->nGreen() + leafPart->NGreen + stemPart->NGreen;
                 //        + reproStruct->NGreen;
   }
float Plant::plantNSenesced(void) const
   {
      return  rootPart->NSenesced + fruitPart->nSenesced() + leafPart->NSenesced + stemPart->NSenesced;
               //          + reproStruct->NSenesced;
   }
float Plant::plantNDead(void) const
   {
      return  rootPart->NDead + fruitPart->nDead() + leafPart->NDead + stemPart->NDead;
                       //  + reproStruct->NDead;
   }
float Plant::plantNTot(void) const
   {
      return  plantNGreen() + plantNSenesced() + plantNDead();
    }

float Plant::plantDltNGreen(void) const
   {
      return  rootPart->dlt.n_green + fruitPart->dltNGreen() + leafPart->dlt.n_green + stemPart->dlt.n_green;
                       //  + reproStruct->NDead;
   }
float Plant::plantDltNRetrans(void) const
   {
      return  rootPart->dlt.n_retrans + fruitPart->dltNRetransOut() + leafPart->dlt.n_retrans + stemPart->dlt.n_retrans;
                       //  + reproStruct->NDead;
   }
float Plant::topsNGreen(void) const
   {
      return  fruitPart->nGreen() + leafPart->NGreen + stemPart->NGreen;
                 //        + reproStruct->NGreen;
   }
float Plant::topsNSenesced(void) const
   {
      return  fruitPart->nSenesced() + leafPart->NSenesced + stemPart->NSenesced;
               //          + reproStruct->NSenesced;
   }
float Plant::topsNDead(void) const
   {
      return  fruitPart->nDead() + leafPart->NDead + stemPart->NDead;
                       //  + reproStruct->NDead;
   }
float Plant::topsNTot(void) const
   {
      return  topsNGreen() + topsNSenesced() + topsNDead();
    }

float Plant::stoverNGreen(void) const
   {
      return  leafPart->NGreen + fruitPart->nGreenVegTotal() + stemPart->NGreen;
                     //    + reproStruct->NGreen;
    }
float Plant::stoverNSenesced(void) const
   {
      return  leafPart->NSenesced + fruitPart->nSenescedVegTotal() + stemPart->NSenesced;
                   //      + reproStruct->NSenesced;
    }
float Plant::stoverNDead(void) const
   {
      return  leafPart->NDead + fruitPart->nDeadVegTotal() + stemPart->NDead;
                 //        + reproStruct->NDead;
    }
float Plant::stoverNTot(void) const
   {
      return  stoverNGreen() + stoverNSenesced() + stoverNDead();
    }

float Plant::plantPGreen(void) const
   {
      return  rootPart->PGreen + leafPart->PGreen + fruitPart->pGreenVegTotal() + stemPart->PGreen;
                       //  + reproStruct->PGreen;
    }
float Plant::plantPSenesced(void) const
   {
      return  rootPart->PSen + leafPart->PSen + fruitPart->pSenescedVegTotal() + stemPart->PSen;
                         //+ reproStruct->PSen;
    }
float Plant::plantPDead(void) const
   {
      return  rootPart->PDead + leafPart->PDead + fruitPart->pDeadVegTotal()+ stemPart->PDead;
                        // + reproStruct->PDead;
    }
float Plant::plantPTot(void) const
   {
      return  plantPGreen() + plantPSenesced() + plantPDead();
   }

float Plant::topsPGreen(void) const
   {
      return  fruitPart->pGreen() + leafPart->PGreen + stemPart->PGreen;
               //          + reproStruct->PGreen;
   }
float Plant::topsPSenesced(void) const
   {
      return fruitPart->pSenesced() + leafPart->PSen + stemPart->PSen;
                       //  + reproStruct->PSen;
   }
float Plant::topsPDead(void) const
   {
      return fruitPart->pDead() + leafPart->PDead + stemPart->PDead;
                     //    + reproStruct->PDead;
   }


float Plant::stoverPGreen(void) const
   {
      return  leafPart->PGreen + fruitPart->pGreenVegTotal() + stemPart->PGreen;
                       //  + reproStruct->PGreen;
    }
float Plant::stoverPSenesced(void) const
   {
      return  leafPart->PSen + fruitPart->pSenescedVegTotal() + stemPart->PSen;
                         //+ reproStruct->PSen;
    }
float Plant::stoverPDead(void) const
   {
      return  leafPart->PDead + fruitPart->pDeadVegTotal()+ stemPart->PDead;
                        // + reproStruct->PDead;
    }
float Plant::stoverPTot(void) const
   {
      return  stoverPGreen() + stoverPSenesced() + stoverPDead();
   }

float Plant::grainPGreen(void) const
   {
      return  fruitPart->pGreenGrainTotal();
                       //  + reproStruct->PGreen;
    }
float Plant::grainPSenesced(void) const
   {
      return  fruitPart->pSenescedGrainTotal();
                         //+ reproStruct->PSen;
    }
float Plant::grainPDead(void) const
   {
      return  fruitPart->pDeadGrainTotal();
                        // + reproStruct->PDead;
    }
float Plant::grainPTot(void) const
   {
      return  grainPGreen() + grainPSenesced() + grainPDead();
   }

float Plant::grainPConcTot(void) const
   {
      return  fruitPart->pConcGrainTotal();
                       //  + reproStruct->PGreen;
    }
float Plant::topsPTot(void) const
   {
      return  topsPGreen() + topsPSenesced() + topsPDead();
   }
float Plant::nCapacity(void)
   {
   float n_capacity_sum = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      n_capacity_sum += (*part)->nCapacity();
   return n_capacity_sum;
   }
float Plant::nDemand(void)
   {
      float n_demand = 0.0;
      for (vector<plantPart *>::iterator t = myParts.begin(); t != myParts.end(); t++)
         n_demand += (*t)->nDemand();
      return n_demand;
   }
float Plant::sumSoilNDemand(void)
   {
      float soil_n_demand = 0.0;
      for (vector<plantPart *>::iterator t = myParts.begin(); t != myParts.end(); t++)
         soil_n_demand += (*t)->soilNDemand();
      return soil_n_demand;
   }
float Plant::sumNMax(void)
   {
      float n_max = 0.0;
      for (vector<plantPart *>::iterator t = myParts.begin(); t != myParts.end(); t++)
         n_max += (*t)->nMax();
      return n_max;
   }

bool  Plant::on_day_of(const string &what) {return (phenology->on_day_of(what));};
bool  Plant::inPhase(const string &what) {return (phenology->inPhase(what));};

void Plant::writeString (const char *line) {parent->writeString(line);};
void Plant::warningError (const char *msg) {parent->warningError(msg);};


const std::string & Plant::getCropType(void) {return c.crop_type;};
protocol::Component *Plant::getComponent(void) {return parent;};

#include <stdio.h>
#include <math.h>
#include <stdexcept>
#include <string>
#include "PlantPart.h"

#include "Plant.h"
#include "CompositePart.h"
#include "Phenology/PlantPhenology.h"
#include "Leaf/LeafPart.h"
#include "Reproductive/PlantFruit.h"
#include "StemPart.h"
#include "Leaf/LeafPart.h"
#include "Reproductive/PodPart.h"
#include "Reproductive/MealPart.h"
#include "Reproductive/OilPart.h"
#include "Root/RootPart.h"
#include "Storage/StoragePart.h"
#include "Utility/Observers.h"
#include "Arbitrators/arbitrator.h"
#include "Utility/PlantUtility.h"

using namespace std;

Plant *currentInstance = NULL;

static const char* floatType =        "<type kind=\"single\"/>";
static const char* doubleType =       "<type kind=\"double\"/>";
static const char* stringType =       "<type kind=\"string\"/>";
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


Plant::Plant(PlantComponent *P, ScienceAPI& api)
//=======================================================================================
   : scienceAPI(api),Environment(api),
     plant(scienceAPI, this, ""),
     tops(scienceAPI, this, "Tops"),
     population(scienceAPI, *this)
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

    plantSpatial.init(this);
    }

Plant::~Plant()
//=======================================================================================
    {
    for (vector<plantThing *>::iterator t = myThings.begin();
         t != myThings.end();
         t++)
       delete (*t);
    }


void Plant::onInit1()
//=======================================================================================
// Init1. Set up plant structure
    {
    plant_zero_variables (); // Zero global states

   // Read the default crop class and cultivar and setup the
   // scienceAPI.
   scienceAPI.read("default_crop_class", c.default_crop_class);
   string defaultCultivar;
   scienceAPI.read("default_cultivar", defaultCultivar);
   scienceAPI.setClass1(defaultCultivar);
   scienceAPI.setClass2(c.default_crop_class);

   population.Initialise();

    string phenologyModel;
    scienceAPI.readOptional("phenology_model", phenologyModel);
    phenology = constructPhenology(scienceAPI, this, phenologyModel);
    myThings.push_back(phenology);

    string rootModel;
    scienceAPI.readOptional("root_part", rootModel);
    rootPart = RootBase::construct(scienceAPI, this, rootModel, "Root");
    myThings.push_back(rootPart);
    myParts.push_back(rootPart);
    plant.add(rootPart);

    string leafModel;
    scienceAPI.readOptional("leaf_part", leafModel);
    leafPart = constructLeafPart(scienceAPI, this, leafModel, "Leaf");
    myThings.push_back(leafPart);
    myParts.push_back(leafPart);
    myTopsParts.push_back(leafPart);
    plant.add(leafPart);
    tops.add(leafPart);

    stemPart = new plantStemPart(scienceAPI, this, "Stem");
    myThings.push_back(stemPart);
    myParts.push_back(stemPart);
    myTopsParts.push_back(stemPart);
    plant.add(stemPart);
    tops.add(stemPart);

    fruitPart = PlantFruit::construct(scienceAPI, this, "Fruit");
    myThings.push_back(fruitPart);
    myParts.push_back(fruitPart);
    myTopsParts.push_back(fruitPart);
    plant.add(fruitPart);
    tops.add(fruitPart);

    StoragePart* storagePart = StoragePart::construct(scienceAPI, this, "Tuber");
    if (storagePart != NULL)
       {
       myThings.push_back(storagePart);
       myParts.push_back(storagePart);
       plant.add(storagePart);
       }

    arbitrator = constructArbitrator(scienceAPI, this, "");       // Make a null arbitrator until we call readSpecies...
    myThings.push_back(arbitrator);

    sowingEventObserver = new eventObserver(scienceAPI, "sowing", this);
    myThings.push_back(sowingEventObserver);

    emergenceEventObserver = new eventObserver(scienceAPI, "emergence", this);
    myThings.push_back(emergenceEventObserver);

    FIEventObserver = new eventObserver(scienceAPI, "floral_initiation", this);
    myThings.push_back(FIEventObserver);

    floweringEventObserver = new eventObserver(scienceAPI, "flowering", this);
    myThings.push_back(floweringEventObserver);

    maturityEventObserver = new eventObserver(scienceAPI, "maturity", this);
    myThings.push_back(maturityEventObserver);

    plant_zero_all_globals();
    zero_p_variables();

   id.eo = parent->addRegistration(RegistrationType::get,
                                   "eo", addUnitsToDDML(floatType, "mm").c_str(),
                                   "", "");

   id.parasite_c_demand = parent->addRegistration(RegistrationType::get,
                                   "parasite_dm_demand", addUnitsToDDML(floatType, "g/m2").c_str(),
                                   "", "");
   id.parasite_sw_demand = parent->addRegistration(RegistrationType::get,
                                   "parasite_sw_demand", addUnitsToDDML(floatType, "mm").c_str(),
                                   "", "");

   string canopyName = string("fr_intc_radn_") + string(parent->getName());
   id.fr_intc_radn = parent->addRegistration(RegistrationType::get,
                                   canopyName.c_str(),
                                   addUnitsToDDML(floatType, "").c_str(),
                                   "", "");
   // events.
   id.crop_chopped = parent->addRegistration(RegistrationType::event,
                                   "crop_chopped", cropChoppedDDML,
                                   "", "");
   setupEvent(parent, "prepare",     RegistrationType::respondToEvent, &Plant::onPrepare, nullTypeDDML);
   setupEvent(parent, "process",     RegistrationType::respondToEvent, &Plant::onProcess, nullTypeDDML);
   setupEvent(parent, "sow",         RegistrationType::respondToEvent, &Plant::onSow, sowDDML);
   setupEvent(parent, "harvest",     RegistrationType::respondToEvent, &Plant::onHarvest, nullTypeDDML);
   setupEvent(parent, "end_crop",    RegistrationType::respondToEvent, &Plant::onEndCrop, nullTypeDDML);
   setupEvent(parent, "end_run",     RegistrationType::respondToEvent, &Plant::onEndRun, nullTypeDDML);
   setupEvent(parent, "kill_stem",   RegistrationType::respondToEvent, &Plant::onKillStem, killStemDDML);
   setupEvent(parent, "remove_crop_biomass",   RegistrationType::respondToEvent, &Plant::onRemoveCropBiomass, DDML(protocol::RemoveCropDmType()).c_str());
   setupEvent(parent, "detach_crop_biomass_rate",   RegistrationType::respondToEvent, &Plant::onDetachCropBiomass, doubleType);


   // Send My Variable

   setupGetFunction(parent, "plant_status", protocol::DTstring, false,
                     &Plant::get_plant_status, "", "Plant Status");

   scienceAPI.expose("crop_type", "", "Crop Type", c.crop_type);

   parent->addGettableVar("crop_class",
                          g.crop_class, "", "Plant crop class");

   setupGetFunction(parent, "height", protocol::DTsingle, false,
                     &Plant::get_height, "mm", "Height of crop");

   setupGetFunction(parent, "width", protocol::DTsingle, false,
                     &Plant::get_width, "mm", "canopy row width");

   setupGetFunction(parent, "cover_tot", protocol::DTsingle, false,
                     &Plant::get_cover_tot, "", "Total cover");


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

   setupGetFunction(parent, "dlt_dm_green_retrans", protocol::DTsingle, true,
                    &Plant::get_dlt_dm_green_retrans, "g/m^2", "change in green pool from retranslocation");

   setupGetFunction(parent, "biomass_n", protocol::DTsingle, false,
                    &Plant::get_biomass_n,  "g/m^2", "N in total biomass");

   setupGetFunction(parent, "n_uptake", protocol::DTsingle, false,
                    &Plant::get_n_uptake, "g/m^2", "N uptake");

   setupGetFunction(parent, "green_biomass_n", protocol::DTsingle, false,
                    &Plant::get_green_biomass_n, "g/m^2", "N in green biomass");




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

   setupGetFunction(parent, "transp_eff", protocol::DTsingle, false,
                    &Plant::get_transp_eff,
                    "g/m2/mm", "Transpiration Efficiency");

   setupGetFunction(parent, "n_conc_stover", protocol::DTsingle, false,
                    &Plant::get_n_conc_stover, "%", "N concentration in stover");

   setupGetFunction(parent, "n_conc_crit", protocol::DTsingle, false,
                    &Plant::get_n_conc_crit, "%", "critical N content");

   setupGetFunction(parent, "n_conc_min", protocol::DTsingle, false,
                    &Plant::get_n_conc_min, "%", "minimum N content");

   setupGetFunction(parent, "n_uptake_stover", protocol::DTsingle, false,
                    &Plant::get_n_uptake_stover,
                    "g/m^2", "N taken up by agp");


   setupGetFunction(parent, "n_demanded", protocol::DTsingle, true,
                    &Plant::get_n_demanded,
                    "g/m^2", "N demanded");

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

   setupGetFunction(parent, "parasite_dm_supply", protocol::DTsingle, false,
                     &Plant::get_parasite_c_gain,
                     "g/m^2", "Assimilate to parasite");


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


   setupGetFunction(parent, "dlt_p_retrans", protocol::DTsingle, true,
                    &Plant::get_dlt_p_retrans,
                    "g/m^2","dlt P parts");


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
   id = parent->addRegistration(RegistrationType::respondToSet, "crop_class", stringType);
   IDtoSetFn.insert(UInt2SetFnMap::value_type(id,&Plant::set_plant_crop_class));

   id = parent->addRegistration(RegistrationType::respondToSet, "phase", stringType);
   IDtoSetFn.insert(UInt2SetFnMap::value_type(id,&Plant::onSetPhase));

   parent->addRegistration(RegistrationType::event, "sowing", nullTypeDDML, "", "");
   parent->addRegistration(RegistrationType::event, "harvesting", nullTypeDDML, "", "");

   Environment.onInit1(parent);

   plant.tempFlagToShortCircuitInit1 = true;
   plant.onInit1(parent);

   // now go and call onInit1 for all non part things.
   for (vector<plantThing *>::iterator t = myThings.begin();
        t != myThings.end();
        t++)
      {
      if (dynamic_cast<plantPart*>(*t) == NULL)
         (*t)->onInit1(parent);
      }
   }

void Plant::onInit2()
//=======================================================================================
// Init2. The rest of the system is here now..
   {
   PlantP_set_phosphorus_aware(parent); // See whether a P module is plugged in

   // Read the default crop class and cultivar and setup the
   // scienceAPI.
   scienceAPI.read("default_crop_class", c.default_crop_class);
   string defaultCultivar;
   scienceAPI.read("default_cultivar", defaultCultivar);
   scienceAPI.setClass1(defaultCultivar);
   scienceAPI.setClass2(c.default_crop_class);

   plant_read_constants ();

   read();
   plant_zero_variables (); // Zero global states

   scienceAPI.setClass2(c.default_crop_class);

   g.plant_status = out;
   g.module_name = parent->getName();
   doPInit(parent);
   plant_get_other_variables (); // sw etc..
   }



void Plant::doPlantEvent(const string &e)
//=======================================================================================
   {
   for (vector<plantThing *>::iterator t = myThings.begin();
        t != myThings.end();
        t++)
      (*t)->onPlantEvent(e);
   }

bool Plant::respondToSet(unsigned int &id, protocol::QuerySetValueData& qd)
//=======================================================================================
// Set a variable from the system.
  {
    ptr2setFn pf = IDtoSetFn[id];
    if (pf) {return((this->*pf)(qd));}
    return false;
  }

void Plant::sendStageMessage(const char *what)
//=======================================================================================
  {
  unsigned int id = parent->addRegistration(RegistrationType::event,
                                            what, "<type/>",
                                            "", "");
  protocol::ApsimVariant outgoingApsimVariant(parent);
  parent->publish (id, outgoingApsimVariant);
  }
/////////////////////////These routines are portions of the fortran "main" routine.

// Field a Prepare message
void Plant::onPrepare(unsigned &, unsigned &, protocol::Variant &)
//=======================================================================================
// Event Handler for Prepare Event
  {
  plant_zero_daily_variables ();

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

void Plant::onProcess(unsigned &, unsigned &, protocol::Variant &)
//=======================================================================================
// Event Handler for Process Event
  {
  if (g.plant_status != out)
     {
     plant_get_other_variables ();   // request and receive variables from owner-modules
     plant_process ();               // do crop processes
     plant_update_other_variables ();
     rootPart->UpdateOtherVariables();
     }
  else
     {} // plant is out
  }

void Plant::onSow(unsigned &, unsigned &, protocol::Variant &v)
//=======================================================================================
// Event Handler for Sowing Event
  {
  plant_get_other_variables (); // request and receive variables from owner-modules
  plant_start_crop (v);          // start crop and do  more initialisations
  }


void Plant::onHarvest(unsigned &, unsigned &, protocol::Variant &v)
//=======================================================================================
// Event Handler for a Harvest Event
  {
  plant_harvest (v);             // harvest crop - turn into residue
  }

void Plant::onEndCrop(unsigned &, unsigned &, protocol::Variant &)
//=======================================================================================
// Event Handler for End of Crop Event
  {
  plant_end_crop ();            //end crop - turn into residue
  }


void Plant::onKillStem(unsigned &, unsigned &, protocol::Variant &v)
//=======================================================================================
// Event Handler for a Kill Stem Event
   {
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
   }

void Plant::onEndRun(unsigned &, unsigned &,protocol::Variant &/*v*/)
//=======================================================================================
// Event Handler for the end of run event
   {
   plant_zero_variables ();
   }

void Plant::onRemoveCropBiomass(unsigned &, unsigned &, protocol::Variant &v)
//=======================================================================================
// Event Handler for a RemoveCropBiomass Event
   {
    protocol::RemoveCropDmType dmRemoved;
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
       msg << endl << "   dm total = " << dmTotal << " (g/m2)" << endl << ends;

       parent->writeString (msg.str().c_str());
       }

    plant_remove_biomass_update(dmRemoved);
   }

void Plant::onDetachCropBiomass(unsigned &, unsigned &, protocol::Variant &v)
//=======================================================================================
// Event Handler for a DetachCropBiomass Event
   {
   plant_detach_crop_biomass (v);
   }

void Plant::doAutoClassChange(unsigned &/*fromId*/, unsigned &eventId, protocol::Variant &)
//=======================================================================================
// Change crop class due to given event
  {
  string ps = IDtoAction[eventId];
  plant_auto_class_change(ps.c_str());
  }

void Plant::plant_bio_retrans (void)
//=======================================================================================
//       Retranslocate biomass.
   {
   vector<plantPart *> supply_pools_by_veg;
   supply_pools_by_veg.push_back(stemPart);
   supply_pools_by_veg.push_back(leafPart);

   float dm_demand_differential = fruitPart->dmGreenDemand ()   //FIXME - should be returned from a fruitPart method
                                - fruitPart->dltDmGreen();

   float dlt_dm_retrans_to_fruit = 0.0;                    // dry matter retranslocated to fruit (g/m^2)
   legnew_dm_retranslocate(myParts
                           , supply_pools_by_veg
                           , dm_demand_differential
                           , getPlants()
                           , &dlt_dm_retrans_to_fruit);

   fruitPart->doDmRetranslocate (dlt_dm_retrans_to_fruit, dm_demand_differential);

   // Finally, a mass balance check
//   float mbSum = 0.0;                                                    //FIXME - need to reinstate this check
//   for (vector<plantPart *>::iterator part = myParts.begin();
//        part != myParts.end();
//        part++)
//       mbSum += (*part)->dlt_dm_green_retrans();
//
//   if (fabs(mbSum) > 0.001)
//      {
//      string msg ="Crop dm retranslocate mass balance is off: error="
//              + ftoa(mbSum, ".6")
//              + "\n";
//      string msg;
//      for (vector<plantPart *>::iterator part = myParts.begin();
//           part != myParts.end();
//           part++)
//         msg += (*part)->name() + "=" +
//                  ftoa((*part)->dltDmGreenRetransUptake(), ".6") +"\n";
//
//      msg += "dlt_dm_retrans_to_fruit = " + ftoa(dlt_dm_retrans_to_fruit, ".6") + "\n";
//            fprintf(stdout,"%s",msg.c_str()) ;
//
//      parent->warningError(msg.c_str());
//      }
   }

void Plant::plant_temp_stress (void)
//     ===========================================================
//         Get current temperature stress factors (0-1)
   {
   float ave_temp = (Environment.maxt + Environment.mint) / 2.0;
   g.temp_stress_photo = linear_interp_real (ave_temp, c.x_ave_temp, c.y_stress_photo, c.num_ave_temp);
   g.temp_stress_photo = bound (g.temp_stress_photo, 0.0, 1.0);
   }


void Plant::plant_bio_water (void)
//     ===========================================================
//     Calculate biomass transpiration efficiency
    {
    float swSupply = rootPart->waterUptake();
    float swSupplyVeg = swSupply * divide (leafPart->SWDemand(), tops.SWDemand(), 0.0);
    float swSupplyFruit = swSupply - swSupplyVeg;

    fruitPart->doDmPotTE (swSupplyFruit);
    leafPart->doDmPotTE (swSupplyVeg);
    }





void Plant::plant_nit_retrans (int option/* (INPUT) option number*/)
//=======================================================================================
// Do Plant Nitrogen Retranslocation
    {
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
    }

void Plant::plant_nit_demand (int option /* (INPUT) option number*/)
//=======================================================================================
//       Find nitrogen demand.
    {
    float dlt_dm = tops.dltDm();
    float dlt_dm_pot_rue = tops.dltDmPotRue();
    if (option == 1)
        {
        for (vector<plantPart *>::iterator t = myParts.begin();
             t != myParts.end();
             t++)
           (*t)->doNDemand1(dlt_dm, dlt_dm_pot_rue);        //FIXME Should be able to do away with the arguments someday
        }
     else if (option == 2)
         {
         for (vector<plantPart *>::iterator t = myParts.begin();
              t != myParts.end();
              t++)
            (*t)->doNDemand2(dlt_dm, dlt_dm_pot_rue);      //FIXME Should be able to do away with the arguments someday
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

void Plant::plant_nit_uptake ()
//=======================================================================================
//       Find nitrogen uptake.
   {
   float soil_n_demand = 0.0;
   float n_max = 0.0;
   for (vector<plantPart *>::iterator t = myParts.begin(); t != myParts.end(); t++)
      {
      soil_n_demand += (*t)->soilNDemand();
      n_max += (*t)->nMax();
      }

    rootPart->plant_nit_uptake(n_max, soil_n_demand, plant.nDemand());
    }


void Plant::plant_nit_partition ()                                     //FIXME - another candidate for rootPart??
//=======================================================================================
//     Calculate the nitrogen and phosporous partitioning in the plant
    {
    legnew_n_partition(g.n_fix_pot
                      , &g.n_fix_uptake
                      , myParts);

    if (g.phosphorus_aware)
       {
       PlantP_partition(myParts);
       }
    }

void Plant::plant_nit_stress (int option /* (INPUT) option number*/)
//=======================================================================================
// Calculate Plant Nitrogen Stress Factors
    {
    if (option == 1)
        {
        vector<plantPart *> parts;

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
        vector< plantPart *> parts;

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

        float dlt_dm_pot_rue = plant.dltDmPotRue();
        for (vector<plantPart *>::iterator t = myParts.begin();
             t != myParts.end();
             t++)
           {
           (*t)->doNDemand1Pot(dlt_dm_pot_rue, dlt_dm_pot_rue);
           }

        g.ext_n_demand = plant.nDemand();

        //nh  use zero growth value here so that estimated n fix is always <= actual;
        float n_fix_pot;
        crop_n_fixation_pot1(phenology->stageNumber()
                             , c.n_fix_rate
                             , tops.Green.DM()
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
       plant.doPSenescence();
    }

void Plant::plant_cleanup ()
//=======================================================================================
//       cleanup after crop processes
    {

    g.remove_biom_pheno = 1.0;

    plant_update();

    plant_check_bounds(plant.coverGreen()
                       , plant.coverSen());

    plant_totals(&g.lai_max
                , &g.n_fix_uptake
                , &g.n_fixed_tops);

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

    }



void Plant::plant_update()
   {
   // Update states

    // Let me register my surprise at how this is done on the next few lines
    // - why intrinsically limit processes to leaf etc right here!!! - NIH
    float n_senesced_trans = leafPart->dltNSenescedTrans();
    leafPart->giveNGreen(-1.0*n_senesced_trans);
    stemPart->giveNGreen(n_senesced_trans);

    float n_senesced_retrans = 0.0;
    for (vector<plantPart *>::iterator part = myParts.begin(); part != myParts.end(); part++)
       n_senesced_retrans += (*part)->dltNSenescedRetrans();

    leafPart->giveNGreen(-1.0*n_senesced_retrans);
    rootPart->updateOthers();    // send off detached roots before root structure is updated by plant death

    plant.update();

    // now update new canopy covers
    plantSpatial.setPlants(getPlants());
    plantSpatial.setCanopyWidth(leafPart->width());

    for (vector<plantPart *>::iterator t = myParts.begin(); t != myParts.end(); t++)
       (*t)->doCover(plantSpatial);

    // plant stress observers
    stageObservers.update();
    if (phenology->inPhase("preflowering")) g.cswd_pheno.update();

    population.Update();

    plant.doNConccentrationLimits(g.co2_modifier_n_conc);

    }

void Plant::plant_check_bounds
    (float  g_cover_green                       // (INPUT)  fraction of radiation reaching
    ,float  g_cover_sen                         // (INPUT)  fraction of radiation reaching
    )
//=======================================================================================
// Check bounds of internal plant data

    {
    bound_check_real_var(this,g_cover_green
                         , 0.0
                         , 1.0
                         , "cover_green");

    bound_check_real_var(this,g_cover_sen
                         , 0.0
                         , 1.0
                         , "cover_sen");

    for (vector<plantPart *>::iterator t = myParts.begin(); t != myParts.end(); t++)
       {
       (*t)->checkBounds();
       }

    }


//+  Purpose
//         Collect totals of crop variables for output
void Plant::plant_totals
    (float *g_lai_max                    // (INPUT)  maximum lai - occurs at flowering
    ,float  *g_n_fix_uptake                    // (INPUT)  daily fixed N (g/m^2)
    ,float  *g_n_fixed_tops                    // (out/INPUT)  fixed N in tops (g/m2)
    )  {

    if (phenology->on_day_of ("sowing"))
        {
        *g_n_fixed_tops = tops.dltNGreen()
                              * divide (*g_n_fix_uptake
                                        ,plant.dltNGreen()
                                        ,0.0);
        }
    else
        {
        *g_n_fixed_tops = *g_n_fixed_tops + tops.dltNGreen() * divide (*g_n_fix_uptake ,plant.dltNGreen() ,0.0);
        }

    *g_lai_max = max (*g_lai_max, leafPart->getLAI());             //FIXME - should be returned from leafPart method
    }

//+  Purpose
//       Report occurence of event and the current status of specific
//       variables.
//       Called when a new phase has begun.
void Plant::plant_event()
    {
//+  Local Variables
    float biomass;                                // total above ground plant wt (g/m^2)
    float pesw_tot;                               // total plant extractable sw (mm)
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
    sprintf(msg, " stage %.1f %s"                                    // BANNISH sprintf
              , phenology->stageNumber()
              , phenology->stageName().c_str());
    parent->writeString(msg);

    biomass = tops.Total.DM();

    // note - oil has no N, thus is not included in calculations
    dm_green = tops.Vegetative.DM();
    n_green = tops.Vegetative.N();

    n_green_conc_percent = divide (n_green, dm_green, 0.0) * fract2pcnt;

    pesw_tot = rootPart->peswTotal();

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


//===========================================================================
void Plant::plant_light_supply_partition (int option /*(INPUT) option number*/)
//===========================================================================
{
//+  Purpose
//       light supply

    if (option == 1)
    {
            // back calculate transmitted solar radiation to canopy
          float fractIncidentRadn;
          if (g.fr_intc_radn <= 0.0)
          {
            fractIncidentRadn = 1.0;
          }
          else
          {
            fractIncidentRadn = divide (g.fr_intc_radn, plant.coverGreen(), 0.0);
          }
          float incomingSolarRadiation = Environment.radn * fractIncidentRadn;
          fruitPart->interceptRadiationGreen (incomingSolarRadiation);

             // calc the total fruit interception - what is left is transmitted to the vegetative parts)
             // fruit is considered to be at top of canopy
          float radnIntTotFruit = fruitPart->interceptRadiationTotal (incomingSolarRadiation);

          incomingSolarRadiation -= radnIntTotFruit;
          leafPart->interceptRadiationGreen (incomingSolarRadiation);
    }
    else
    {
        throw std::invalid_argument ("invalid template option");
    }

    }




//==========================================================================
void Plant::plant_rue_co2_modifier(float co2,                 //!CO2 level (ppm)
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
   if (c.photosynthetic_pathway == photosynthetic_pathway_C3)
      {
      temp = 0.5*( maxt + mint);
      TT  = divide(163.0 - temp, 5.0 - 0.1 * temp, 0.0);

      first = (co2 - TT) * (350.0 + 2.0 * TT);
      second = (co2 + 2.0 * TT)*(350.0 - TT);
      *modifier = divide( first, second, 1.0);
      }
    else if (c.photosynthetic_pathway == photosynthetic_pathway_C4)
      {
      *modifier = 0.000143 * co2 + 0.95; //Mark Howden, personal communication
      }
    else
      throw std::invalid_argument ("Unknown photosynthetic pathway in cproc_rue_co2_modifier()");
   }



//+  Purpose
//       Return actual plant nitrogen uptake to each plant part.

void Plant::legnew_n_partition
    (float  g_n_fix_pot         // (INPUT)  N fixation potential (g/m^2)
    ,float  *n_fix_uptake        // (OUTPUT) actual N fixation (g/m^2)
    ,vector<plantPart *> &allParts        // (INPUT) vector of plant parts
    ) {

//+  Local Variables

    vector<plantPart *>::iterator part;           // iterator
    vector<float> n_capacity(allParts.size());    // amount of N that can be stored in plant part above Ncrit (g/m^2)
    float n_capacity_sum;                         // total excess N storage (g/m^2)
    float n_demand_sum;                               // total nitrogen demand (g/m^2)
    float n_fix_demand_tot;                       // total demand for N fixation (g/m^2)

    // find the proportion of uptake to be distributed to
    // each plant part and distribute it.
    float n_uptake_sum = rootPart->nUptake();     // total plant N uptake (g/m^2)
    n_demand_sum = plant.nDemand();
    n_capacity_sum = plant.nCapacity();

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
    ,float // g_plants                      // (INPUT)  Plant density (plants/m^2)
    ,float  *dlt_dm_retrans_to_fruit)      // (OUTPUT) dm retranslocated to fruit (g/m^2)
{

//+  Purpose
//     Calculate plant dry matter delta's due to retranslocation
//     to grain, pod and energy (g/m^2)

    vector<plantPart *>::iterator part;

    float dlt_dm_retrans_part;                    // carbohydrate removed from part (g/m^2)
    float demand_differential;                    // demand in excess of available supply (g/m^2)
    float dm_part_avail;                          // carbohydrate avail from part(g/m^2)
    float dm_retranslocate = 0.0;

//- Implementation Section ----------------------------------

// now translocate carbohydrate between plant components
// this is different for each stage

    for (part = allParts.begin(); part != allParts.end(); part++)
        (*part)->dlt_dm_green_retrans_hack( 0.0 );

    demand_differential = g_dm_demand_differential;

    // get available carbohydrate from supply pools
    for (part = supply_pools.begin(); part != supply_pools.end(); part++)
        {
           dm_part_avail = (*part)->dmRetransSupply();

           dlt_dm_retrans_part = min (demand_differential, dm_part_avail);

           //assign and accumulate

           dm_retranslocate += (*part)->dlt_dm_green_retrans_hack( - dlt_dm_retrans_part);

           demand_differential = demand_differential - dlt_dm_retrans_part;
        }

    *dlt_dm_retrans_to_fruit = - dm_retranslocate;
}


//+  Purpose
//     Calculate the nitrogen retranslocation from the various plant parts
//     to the grain.

void Plant::legnew_n_retranslocate (float g_grain_n_demand)
{
//+  Constant Values
    const float  tolerence = 0.001 ;


    vector<plantPart*>::iterator part;            // plant part

//- Implementation Section ----------------------------------

          //! available N does not include roots or grain
          //! this should not presume roots and grain are 0.
          // grain N potential (supply)
    float n_avail_stover = 0.0;
    for (part = myTopsParts.begin(); part != myTopsParts.end(); part++)
        n_avail_stover += (*part)->availableRetranslocateN();


    for (part = myTopsParts.begin(); part != myTopsParts.end(); part++)
        (*part)->doNRetranslocate(n_avail_stover, g_grain_n_demand);

          // check that we got (some of) the maths right.
    for (part = myParts.begin(); part != myParts.end(); part++)
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

   dlt_n_in_senescing_leaf = leafPart->dltDmSenesced() * leafPart->Green.Nconc();

   n_demand_tot = plant.nDemand();

   navail = dlt_n_in_senescing_leaf - leafPart->dltNSenesced();
   navail = bound(navail, 0.0, n_demand_tot);

   for (vector<plantPart *>::iterator t = myParts.begin();
        t != myParts.end();
        t++)
      (*t)->doNSenescedRetrans(navail, n_demand_tot);
   }

//+  Purpose
//       Simulate crop processes.  These include biomass production,
//       phenological stages, plant component development,
//       water uptake and nitrogen uptake, and plant senescense.
void Plant::plant_process ( void )
    {

//- Implementation Section ----------------------------------
    //!!!!!!!! check order dependency of deltas

    rootPart->plant_root_depth ();
    rootPart->waterSupply();

    if (g.plant_status == alive)
        {
         // these next three executable lines are copied from the "prepare"
         // method. They need to be calculated in the case where a crop is
         // sown during a process method. In this situation, the co2 modifiers
         // have a value of zero.
         plant_rue_co2_modifier(Environment.co2,
                               Environment.maxt,
                               Environment.mint,
                               &g.co2_modifier_rue);

         g.co2_modifier_te = linear_interp_real (Environment.co2
                                               , c.x_co2_te_modifier
                                               , c.y_co2_te_modifier
                                               , c.num_co2_te_modifier);
         g.co2_modifier_n_conc = linear_interp_real (Environment.co2
                                               , c.x_co2_nconc_modifier
                                               , c.y_co2_nconc_modifier
                                               , c.num_co2_nconc_modifier);


        rootPart->plant_water_uptake(1, tops.SWDemand());
       rootPart->plant_water_stress (tops.SWDemand(),
                                     g.swdef_photo,
                                     g.swdef_pheno,
                                     g.swdef_pheno_flower,
                                     g.swdef_pheno_grainfill,
                                     g.swdef_expansion,
                                     g.swdef_fixation );

        g.oxdef_photo = rootPart->oxdef_stress ();

        phenology->prepare (Environment);

        pheno_stress_t ps;
        ps.swdef = g.swdef_pheno;
        ps.nfact = min(g.nfact_pheno, g.pfact_pheno);
        ps.swdef_flower = g.swdef_pheno_flower;
        ps.swdef_grainfill = g.swdef_pheno_grainfill;
        ps.remove_biom_pheno = g.remove_biom_pheno;

        float fasw_seed = rootPart->fasw(plantSpatial.sowing_depth);
        float pesw_seed = rootPart->pesw(plantSpatial.sowing_depth);

        phenology->process (Environment, ps, fasw_seed, pesw_seed);

        for (vector<plantPart *>::iterator t = myParts.begin();
             t != myParts.end();
             t++)
           (*t)->morphology();

        leafPart->potential(c.leaf_no_pot_option,
                            min(pow(min(g.nfact_expansion, g.pfact_expansion),2),g.swdef_expansion),
                            phenology->get_dlt_tt() );

        leafPart->leaf_area_stressed (min(g.swdef_expansion, min(g.nfact_expansion, g.pfact_expansion)));

        plant_bio_water ();
        // Calculate Potential Photosynthesis
        for (vector<plantPart *>::const_iterator part = myParts.begin(); part != myParts.end(); part++)
           (*part)->doDmPotRUE();

        if(phenology->on_day_of(phenology->stageName()))
           fruitPart->onDayOf(phenology->stageName());
        plant.doDmMin();

        // Calculate Actual DM increase from photosynthesis
        for (vector<plantPart *>::iterator t = myParts.begin(); t != myParts.end(); t++)
           (*t)->doBioActual();

        // Now calculate DM demands
        float dlt_dm = plant.dltDm();
        for (vector<plantPart *>::iterator t = myParts.begin();
             t != myParts.end();
             t++)
            (*t)->doDmDemand (dlt_dm);

        arbitrator->partitionDM(dlt_dm, myParts, fruitPart->name());

        plant_bio_retrans ();

        leafPart->actual ();
        fruitPart->calcDlt_pod_area ();

        rootPart->root_length_growth();

        leafPart->leaf_death( min(g.nfact_expansion, g.pfact_expansion), phenology->get_dlt_tt());
        leafPart->leaf_area_sen( g.swdef_photo , Environment.mint);

        plant.doSenescence(leafPart->senFract());
        rootPart->sen_length();

        for (vector<plantPart *>::iterator t = myParts.begin(); t != myParts.end(); t++)
           {
           (*t)->doNInit();
           (*t)->doNDemandGrain(g.nfact_grain_conc, g.swdef_expansion);
           }

        float biomass = tops.Green.DM() + plant.dltDm();
        g.n_fix_pot = rootPart->plant_nit_supply(biomass, phenology->stageNumber(), g.swdef_fixation);

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
        plant_nit_uptake ();     // allows preference of N source
        plant_nit_partition ();                  // allows output of n fixed
        if (c.n_retrans_option==2)
           {
           // this option requires soil uptake to satisfy grain n before
           // retranslocation
           plant_nit_retrans (c.n_retrans_option);
           }

        plant_p_retrans();

        population.PlantDeath();
        }
    else
        {
        // crop is dead
        }


    plant.Detachment();

    plant_cleanup();

    rootPart->plant_water_stress (tops.SWDemand(),
                                  g.swdef_photo,
                                  g.swdef_pheno,
                                  g.swdef_pheno_flower,
                                  g.swdef_pheno_grainfill,
                                  g.swdef_expansion,
                                  g.swdef_fixation );
    plant_nit_stress (c.n_stress_option);

    }

void Plant::plant_harvest (protocol::Variant &v/*(INPUT) message variant*/)
//=======================================================================================
// Report Harvest info and current status of specific variables
    {
    FString  report_flag;

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
    }

//       Detach crop biomass.
void Plant::plant_detach_crop_biomass (protocol::Variant &v/*(INPUT) incoming message variant*/)
    {
    float detachRate;
    v.unpack(detachRate);

    protocol::RemoveCropDmType dmRemoved;

      protocol::dmType dm;

      dm.pool = "green";
      vector<plantPart*>::iterator part;

      vector<float>  dmParts;
      for (part = myTopsParts.begin(); part != myTopsParts.end(); part++)
      {
         (*part)->get_name(dm.part);
         (*part)->get_dm_green(dmParts);
      }

      for (unsigned int pool=0; pool < dmParts.size(); pool++)
         dm.dlt.push_back(double(dmParts[pool] * 0.0));

      dmRemoved.dm.push_back(dm);

      dm.dlt.erase(dm.dlt.begin(), dm.dlt.end());
      dm.part.erase(dm.part.begin(), dm.part.end());
      dmParts.clear();

      dm.pool = "senesced";

      for (part = myTopsParts.begin(); part != myTopsParts.end(); part++)
      {
         (*part)->get_name(dm.part);
         (*part)->get_dm_senesced(dmParts);
      }

      for (unsigned int pool=0; pool < dmParts.size(); pool++)
         dm.dlt.push_back(double(dmParts[pool] * detachRate));

      dmRemoved.dm.push_back(dm);

      dm.dlt.erase(dm.dlt.begin(), dm.dlt.end());
      dm.part.erase(dm.part.begin(), dm.part.end());
      dmParts.clear();

    if (c.remove_biomass_report == "on")
    {
       ostringstream msg;
       msg << "Detach Crop Biomass:-" << endl;
       float dmTotal = 0.0;

       for (unsigned int pool=0; pool < dmRemoved.dm.size(); pool++)
       {
          for (unsigned int part = 0; part < dmRemoved.dm[pool].part.size(); part++)
          {
             msg << "   dm " << dmRemoved.dm[pool].pool << " " << dmRemoved.dm[pool].part[part] << " = " << dmRemoved.dm[pool].dlt[part] << " (g/m2)" << endl;
             dmTotal +=  dmRemoved.dm[pool].dlt[part];
          }
       }
       msg << endl << "   dm total = " << dmTotal << " (g/m2)" << endl << ends;

       parent->writeString (msg.str().c_str());
    }

    plant_remove_biomass_update(dmRemoved);


    }


//+  Purpose
//       Report occurence of harvest and the current status of specific
//       variables.
void Plant::plant_dormancy (protocol::Variant &v/*(INPUT) incoming message variant*/)
    {

//+  Local Variables
    FString  dormancy_flag;
    FString  previous_class;

//- Implementation Section ----------------------------------


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
        read();
        }
    else
        {
        // unknown dormancy_flag
        throw std::invalid_argument ("dormancy state is unknown - neither on nor off");
        }


    }


//+  Purpose
//       Update states after a harvest

void Plant::plant_harvest_update (protocol::Variant &v/*(INPUT)message arguments*/)
    {

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
    float temp;

//- Implementation Section ----------------------------------

    // Tell the rest of the system we are about to harvest
    sendStageMessage("harvesting");

    protocol::ApsimVariant incomingApsimVariant(parent);
    incomingApsimVariant.aliasTo(v.getMessageData());

    unsigned int junk = 0L;
    phenology->onHarvest(junk,junk,v);

    // determine the new stem density
    // ==============================
    if (incomingApsimVariant.get("plants", protocol::DTsingle, false, temp) == true)
        population.SetPlants(temp);

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

    sprintf (msg, "%48s%7.1f%24.1f", "DM (kg/ha) =               ", dm_tops_residue, dm_root_residue);
    parent->writeString (msg);

    sprintf (msg, "%48s%8.2f%24.2f", "N  (kg/ha) =               ", n_tops_residue, n_root_residue);
    parent->writeString (msg);
    if (g.phosphorus_aware)
       {
       sprintf (msg, "%48s%7.1f%24.2f",
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

    sprintf (msg, "%48s%7.1f%24.1f", "DM (kg/ha) =               ", dm_removed_tops, dm_removed_root);
    parent->writeString (msg);

    sprintf (msg, "%48s%8.2f%24.2f", "N  (kg/ha) =               ", n_removed_tops, n_removed_root);
    parent->writeString (msg);
    if (g.phosphorus_aware)
       {
       sprintf (msg, "%48s%7.2f%24.2f",
                                     "P  (kg/ha) =               ", p_removed_tops, p_removed_root);
       parent->writeString (msg);
       }
    parent->writeString (" ");

// now update new canopy covers

    plantSpatial.setPlants(getPlants());
    plantSpatial.setCanopyWidth(leafPart->width());

   for (vector<plantPart *>::iterator t = myParts.begin(); t != myParts.end(); t++)
       (*t)->doCover(plantSpatial);

// other plant states
    plant.doNConccentrationLimits(g.co2_modifier_n_conc);

    if (g.plant_status == alive &&
        phenology->previousStageName() != phenology->stageName())
        {
        for (vector<plantPart *>::iterator part = myParts.begin(); part != myParts.end(); part++)
           (*part)->onPlantEvent(phenology->stageName());
        plant_event ();
        stageObservers.reset();
        otherObservers.reset();
        }

    }


//+  Purpose
//       Update states after a kill stem event
void Plant::plant_kill_stem_update (protocol::Variant &v/*(INPUT) message arguments*/)
    {

//+  Local Variables
    float temp;

//- Implementation Section ----------------------------------

    protocol::ApsimVariant aV(parent);
    aV.aliasTo(v.getMessageData());

    unsigned int junk = 0L;
    phenology->onKillStem(junk,junk,v);

    // determine the new stem density
    // ==============================
    if (aV.get("plants", protocol::DTsingle, false, temp) == true)
        population.SetPlants(temp);

    // Update biomass and N pools.
    for (vector<plantPart *>::iterator part = myParts.begin(); part != myParts.end(); part++)
       (*part)->onKillStem();

    // now update new canopy covers

    plantSpatial.setPlants(getPlants());
    plantSpatial.setCanopyWidth(leafPart->width());

   for (vector<plantPart *>::iterator t = myParts.begin(); t != myParts.end(); t++)
       (*t)->doCover(plantSpatial);

    plant.doNConccentrationLimits( g.co2_modifier_n_conc )  ;                  // plant N concentr

    if (g.plant_status == alive &&
        phenology->previousStageName() != phenology->stageName())
        {
        for (vector<plantPart *>::iterator part = myParts.begin(); part != myParts.end(); part++)
           (*part)->onPlantEvent(phenology->stageName());
        plant_event ();
        stageObservers.reset();
        otherObservers.reset();
        }


    }

bool Plant::onSetPhase (protocol::QuerySetValueData &v/*(INPUT) message arguments*/)
    {
       // FIXME - hack to workaround bug in variant type conversion
     float phase = 0.0;
     protocol::TypeConverter* converter = NULL;
     if (getTypeConverter("phase",
                                 v.variant.getType().getCode(),
                                 protocol::DTsingle,
                                 v.variant.getType().isArray(),
                                 false,
                                 converter))
        {
        v.variant.setTypeConverter(converter);
        }
     v.variant.unpack(phase);

     if (converter) delete converter;
        // end of FIXME

        bound_check_real_var(this,phase, 1.0, 11.0, "phase");
        if (g.plant_status == alive)
           phenology->onSetPhase(phase);

    return true;
    }

//NIH up to here

//+  Purpose
//       Zero crop variables & arrays

void Plant::plant_remove_biomass_update (protocol::RemoveCropDmType dmRemoved)
    {

//+  Local Variables
    vector<plantPart *>::iterator part;

//- Implementation Section ----------------------------------


    // Unpack the DmRemoved structure
     for (vector<plantPart *>::iterator part = myTopsParts.begin(); part != myTopsParts.end(); part++)
        (*part)->doRemoveBiomass(dmRemoved, c.remove_biomass_report);

    // Update biomass and N pools.  Different types of plant pools are affected in different ways.
    // Calculate Root Die Back
    float chop_fr_green_leaf = divide(leafPart->dltDmGreenRemoved(), leafPart->Green.DM(), 0.0);

    rootPart->removeBiomass2(chop_fr_green_leaf);

    float biomassGreenTops =  0.0;
    float dmRemovedGreenTops = 0.0;
    float dmRemovedTops = 0.0;
    float nRemovedTops = 0.0;

    for (part = myTopsParts.begin(); part != myTopsParts.end(); part++)
        {
        biomassGreenTops +=  (*part)->Green.DM();
        dmRemovedGreenTops += (*part)->dltDmGreenRemoved();
        dmRemovedTops += ((*part)->dltDmRemoved()) * gm2kg/sm2ha;
        nRemovedTops += ((*part)->dltNRemoved()) * gm2kg/sm2ha;

        (*part)->removeBiomass();
        }
    g.remove_biom_pheno = divide (dmRemovedGreenTops, biomassGreenTops, 0.0);

    if (c.remove_biomass_report == "on")
    {
       parent->writeString ("\nCrop biomass removed.");
       char  msgrmv[400];

       parent->writeString ("    Organic matter removed from system:-      From Tops               From Roots");

       sprintf (msgrmv, "%48s%7.2f%24.2f", "DM (kg/ha) =               ", dmRemovedTops, 0.0);
       parent->writeString (msgrmv);

       sprintf (msgrmv, "%48s%7.2f%24.2f", "N  (kg/ha) =               ", nRemovedTops, 0.0);
       parent->writeString (msgrmv);

       sprintf (msgrmv, "%30s%7.2f", "Remove biomass phenology factor = ", g.remove_biom_pheno);
       parent->writeString (msgrmv);

       parent->writeString (" ");
    }

    stemPart->removeBiomass2(-1.0); // the values calculated here are overwritten in plantPart::morphology(void)

    // now update new canopy covers
    plantSpatial.setPlants(getPlants());
    plantSpatial.setCanopyWidth(leafPart->width());

    for (vector<plantPart *>::iterator t = myParts.begin(); t != myParts.end(); t++)
       (*t)->doCover(plantSpatial);

    phenology->onRemoveBiomass(g.remove_biom_pheno);

    plant.doNConccentrationLimits(g.co2_modifier_n_conc );

    if (g.plant_status == alive &&
        phenology->previousStageName() != phenology->stageName())
        {
        for (vector<plantPart *>::iterator part = myParts.begin(); part != myParts.end(); part++)
           (*part)->onPlantEvent(phenology->stageName());
        plant_event ();
        stageObservers.reset();
        otherObservers.reset();
        }

    }


//+  Purpose
//       Zero crop variables & arrays
void Plant::plant_zero_all_globals (void)
    {

//- Implementation Section ----------------------------------

          g.co2_modifier_te = 0.0;
      g.co2_modifier_n_conc = 0.0;
      g.co2_modifier_rue = 0.0;
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
      g.nfact_expansion = 1.0;
      g.nfact_photo = 1.0;
      g.nfact_grain_conc = 1.0;
      g.nfact_pheno = 1.0;
      g.remove_biom_pheno = 1.0;
      g.temp_stress_photo = 1.0;
      g.oxdef_photo = 1.0;
      g.fr_intc_radn = 0.0;

      g.eo = 0.0;
      g.dlt_dm_parasite  =  0.0;
      g.dlt_dm_parasite_demand = 0.0;
      g.dlt_sw_parasite_demand = 0.0;

      g.n_fix_pot = 0.0;
      g.n_fix_uptake = 0.0;
      g.n_fixed_tops = 0.0;

      g.lai_max = 0.0;
      g.ext_n_demand = 0.0;

      p.eo_crop_factor = 0.0;

      //       plant Constants
      c.leaf_no_pot_option = 0;


      c.crop_type = "";
      c.default_crop_class = "";
      c.remove_biomass_report = "off";

      c.n_supply_preference = "";

      c.n_fact_photo = 0.0;
      c.n_fact_pheno = 0.0;
      c.n_fact_expansion = 0.0;
      fill_real_array (c.n_fix_rate, 0.0,max_table);
      fill_real_array (c.x_ave_temp, 0.0, max_table);
      fill_real_array (c.y_stress_photo, 0.0, max_table);
      c.num_ave_temp = 0;
      c.num_factors = 0;

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

      c.photosynthetic_pathway = photosynthetic_pathway_UNDEF;


    }


//+  Purpose
//       Zero crop variables & arrays
void Plant::plant_zero_variables (void)
    {


// zero pools etc.

    plant_zero_daily_variables ();

    g.plant_status_out_today = false;

   for (vector<plantThing *>::iterator t = myThings.begin();
        t != myThings.end();
        t++)
       (*t)->zeroAllGlobals();

    Environment.zeroAllGlobals();
    plantSpatial.zeroAllGlobals();

    g.lai_max               = 0.0;


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

    }


//+  Purpose
//       Zero crop daily variables & arrays
void Plant::plant_zero_daily_variables ()
    {

//- Implementation Section ----------------------------------


// zero pools etc.

   for (vector<plantThing *>::iterator t = myThings.begin();
        t != myThings.end();
        t++)
       (*t)->zeroDeltas();


    g.ext_n_demand             = 0.0;
    }

//+  Purpose
//       Start crop using parameters specified in passed record
void Plant::plant_start_crop (protocol::Variant &v/*(INPUT) message arguments*/)
    {

//+  Local Variables
    char  msg[200];                               // output string
    FString  dummy;                               // dummy variable

//- Implementation Section ----------------------------------

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
               scienceAPI.setClass2(c.default_crop_class);
               }
           else
               {
               g.crop_class = dummy.f_str();
               g.crop_class = g.crop_class.substr(0,dummy.length());
               scienceAPI.setClass2(asString(dummy));
               }

           // get cultivar parameters
           if (incomingApsimVariant.get("cultivar", protocol::DTstring, false, dummy) == false)
               {
               throw std::invalid_argument("Cultivar not specified");
               }
           else
               {
               g.cultivar = dummy.substr(0,dummy.length()).f_str();
               g.cultivar = g.cultivar.substr(0,dummy.length());
               scienceAPI.setClass1(g.cultivar);
               }

           read();

           // get other sowing criteria
           float temp;
           if (incomingApsimVariant.get("plants", protocol::DTsingle, false, temp) == false)
               {
               throw std::invalid_argument("plant density ('plants') not specified");
               }
           population.SetPlants(temp);

           parent->writeString ("    ------------------------------------------------");
           sprintf (msg, "   %s%s",  "cultivar                   = ", g.cultivar.c_str());
           parent->writeString (msg);
           phenology->writeCultivarInfo(parent);
           for (vector<plantPart *>::iterator t = myParts.begin(); t != myParts.end(); t++)
              (*t)->writeCultivarInfo(parent);
           parent->writeString ("    ------------------------------------------------\n\n");

           rootPart->write();

           sprintf (msg, "%s%5.1f%s"
              ,"    Crop factor for bounding water use is set to "
              , p.eo_crop_factor
                , " times eo.");
           parent->writeString (msg);

           plantSpatial.startCrop (parent, v);

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
                  , Environment.day_of_year, plantSpatial.sowing_depth
                  , getPlants(), plantSpatial.row_spacing
                  , plantSpatial.skip_row, plantSpatial.skip_plant, g.cultivar.c_str());
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

    }

void Plant::read()
   {
   plant_read_species_const ();
    for (vector<plantThing *>::iterator t = myThings.begin();
         t != myThings.end();
         t++)
       {
       (*t)->readCultivarParameters(parent, g.cultivar);
       }

   if (!scienceAPI.readOptional("eo_crop_factor", p.eo_crop_factor, 0.0f, 100.0f))
      p.eo_crop_factor = c.eo_crop_factor_default;
   scienceAPI.readOptional("remove_biomass_report", c.remove_biomass_report);

   rootPart->read();
   }

//+  Purpose
//       End crop

void Plant::plant_end_crop ()
    {

    float dm_residue;                             // dry matter added to residue (g/m^2)
    float n_residue;                              // nitrogen added to residue (g/m^2)
    float p_residue;                              // phosphorus added to residue (g/m^2)
    float dm_root;                                // dry matter added to soil (g/m^2)
    float n_root;                                 // nitrogen added to soil (g/m^2)
    float p_root;                                 // phosphorus added to soil (g/m^2)
    char  msg[400];
    float yield;                                  // grain wt (kg/ha)

    if (g.plant_status != out)
        {
        g.plant_status_out_today = true;
        g.plant_status = out;

        stageObservers.reset();
        otherObservers.reset();

        // report
        yield = 0.0;
        for (vector<plantPart *>::iterator t = myParts.begin(); t != myParts.end(); t++)
            yield=yield+(*t)->GrainTotal.DM() * gm2kg / sm2ha;

        sprintf (msg, "Crop ended. Yield (dw) = %7.1f  (kg/ha)", yield);
        parent->writeString (msg);

        // now do post harvest processes
        // put stover and any remaining grain into surface residue,
        //     any roots to soil FOM pool
        dm_residue =tops.Total.DM();
        n_residue =tops.Total.N();
        p_residue =tops.Total.P();

        dm_root = rootPart->Total.DM();     //FIXME - should be returned from a rootPart method
        n_root  = rootPart->Total.N();       //FIXME - should be returned from a rootPart method
        p_root  = rootPart->Total.P();       //FIXME - should be returned from a rootPart method

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

          if (sum(dlt_dm_crop) > 0.0)
              plant_send_crop_chopped_event ( c.crop_type
                                         , part_name
                                         , dlt_dm_crop
                                         , dlt_dm_n
                                         , dlt_dm_p
                                         , fraction_to_residue);
          }

        parent->writeString ("    Organic matter from crop:-      Tops to surface residue      Roots to soil FOM");

        sprintf (msg, "%48s%7.1f%24.1f"
                           , "DM (kg/ha) =               ", dm_residue * gm2kg /sm2ha, dm_root * gm2kg /sm2ha);
        parent->writeString (msg);

        sprintf (msg, "%48s%8.2f%24.2f"
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

    }
//+  Purpose
//      Get the values of variables/arrays from other modules.
void Plant::plant_get_other_variables ()
    {
    std::vector<float> values;               // Scratch area

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


    parent->getVariable(id.fr_intc_radn, g.fr_intc_radn, 0.0, 1.0, true);

    // Soilwat2
    parent->getVariable(id.eo, g.eo, 0.0, 20.0);
    rootPart->getOtherVariables();
    Environment.getOtherVariables(parent);
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

    }

void Plant::plant_read_constants ( void )
//=======================================================================================
// Crop initialisation - reads constants from constants file
    {
    const char*  section_name = "constants" ;

    scienceAPI.readOptional("crop_type", c.crop_type);

    for (vector<plantThing *>::iterator t = myThings.begin();
         t != myThings.end();
         t++)
      (*t)->readConstants(parent, section_name);

    if (g.phosphorus_aware)
       read_p_constants(parent);

    }


void Plant::plant_prepare (void)
//=======================================================================================
// Event Handler for the Prepare Event
   {
   //     APSim allows modules to perform calculations in preparation for
   //     the standard APSim timestep.  This model uses this opportunity
   //     to calculate potential growth variables for the coming day
   //     and phenological development.

   plant_rue_co2_modifier(Environment.co2,
                         Environment.maxt,
                         Environment.mint,
                         &g.co2_modifier_rue);

   g.co2_modifier_te = linear_interp_real (Environment.co2
                                         , c.x_co2_te_modifier
                                         , c.y_co2_te_modifier
                                         , c.num_co2_te_modifier);
   g.co2_modifier_n_conc = linear_interp_real (Environment.co2
                                         , c.x_co2_nconc_modifier
                                         , c.y_co2_nconc_modifier
                                         , c.num_co2_nconc_modifier);

   for (vector<plantPart *>::iterator t = myParts.begin(); t != myParts.end(); t++)
      (*t)->prepare();

   plant_nit_stress (c.n_stress_option);
   plant_temp_stress ();
   plant_light_supply_partition (1);

   // Calculate Potential Photosynthesis
   for (vector<plantPart *>::const_iterator part = myParts.begin(); part != myParts.end(); part++)
       (*part)->doDmPotRUE();
   // Calculate Transpiration Efficiency
   for (vector<plantPart *>::const_iterator part = myParts.begin(); part != myParts.end(); part++)
       (*part)->doTECO2();
   // Calculate Plant Water Demand
   float SWDemandMaxFactor = p.eo_crop_factor * g.eo ;
   for (vector<plantPart *>::const_iterator part = myParts.begin(); part != myParts.end(); part++)
       (*part)->doSWDemand(SWDemandMaxFactor);
   plant_nit_demand_est(1);

   // Note actually should send total plant
   // potential growth rather than just tops - NIH
   prepare_p();
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
void Plant::plant_read_species_const ()
    {

//+  Local Variables
    int   numvals;                                // number of values returned
    vector<string> search_order;                  // sections to search
//- Implementation Section ----------------------------------

    string scratch = parent->readParameter (c.crop_type.c_str(), g.crop_class.c_str());

    Split_string(scratch, " ", search_order);

    scienceAPI.read("class_action", scratch);
    Split_string(scratch, " ", c.class_action);
    registerClassActions();

    scienceAPI.read("class_change", scratch);
    Split_string(scratch, " ", c.class_change);

    // Kill off last arbitrator, and get a new one
    arbitrator->undoRegistrations(parent);
    myThings.erase(remove(myThings.begin(), myThings.end(), arbitrator),myThings.end());
    delete arbitrator;

    string partitionOption;
    scienceAPI.read("partition_option", partitionOption);
    arbitrator = constructArbitrator(scienceAPI, this, partitionOption);
    arbitrator->onInit1(parent);
    myThings.push_back(arbitrator);

    for (vector<plantThing *>::iterator t = myThings.begin();
        t != myThings.end();
        t++)
      (*t)->readSpeciesParameters(parent, search_order);

    Environment.read();
    plantSpatial.read(scienceAPI);

    scienceAPI.read("n_fix_rate", c.n_fix_rate, numvals, 0.0f, 1.0f);
    scienceAPI.read("eo_crop_factor_default", c.eo_crop_factor_default, 0.0f, 100.0f);

    scienceAPI.read("n_supply_preference", c.n_supply_preference);

    //    plant_phenology_init                           //FIXME - should be in leafPart
    scienceAPI.read("leaf_no_pot_option", c.leaf_no_pot_option, 1, 2);
    scienceAPI.read("n_retrans_option", c.n_retrans_option, 1, 2);

    //    plant_n_senescence
    scienceAPI.read("n_senescence_option", c.n_senescence_option, 1, 2);

    //    plant_nfact
    scienceAPI.read("n_stress_option", c.n_stress_option, 1, 2);

    scienceAPI.read("N_stress_start_stage", c.n_stress_start_stage, 0.0f, 100.0f);
    scienceAPI.read("n_fact_photo", c.n_fact_photo, 0.0f, 100.0f);
    scienceAPI.read("n_fact_pheno", c.n_fact_pheno, 0.0f, 100.0f);
    scienceAPI.read("n_fact_expansion", c.n_fact_expansion, 0.0f, 100.0f);

    //    plant_rue_reduction
    scienceAPI.read("x_ave_temp", c.x_ave_temp, c.num_ave_temp, 0.0f, 100.0f);
    scienceAPI.read("y_stress_photo", c.y_stress_photo, c.num_factors, 0.0f, 1.0f);
    scienceAPI.read("x_co2_te_modifier", c.x_co2_te_modifier, c.num_co2_te_modifier, 0.0f, 1000.0f);
    scienceAPI.read("y_co2_te_modifier", c.y_co2_te_modifier, c.num_co2_te_modifier, 0.0f, 10.0f);
    scienceAPI.read("x_co2_nconc_modifier", c.x_co2_nconc_modifier, c.num_co2_nconc_modifier, 0.0f, 1000.0f);
    scienceAPI.read("y_co2_nconc_modifier", c.y_co2_nconc_modifier, c.num_co2_nconc_modifier, 0.0f, 10.0f);

    string pathway;
    scienceAPI.read("photosynthetic_pathway", pathway);
    if (Str_i_Eq(pathway.c_str(), "C3")) {
      c.photosynthetic_pathway = photosynthetic_pathway_C3;
    } else if(Str_i_Eq(pathway.c_str(), "C4")) {
      c.photosynthetic_pathway = photosynthetic_pathway_C4;
    } else {
      c.photosynthetic_pathway = photosynthetic_pathway_UNDEF;
      printf("undefined photosynthetic_pathway read!!!!\n");
    }

    }

void Plant::plant_harvest_report ()
//=======================================================================================
// Report the state of the crop at harvest time
    {
    //+  Constant Values
    const float  plant_c_frac = 0.4;    // fraction of c in resiudes


    //+  Local Variables
    float grain_wt;                               // grain dry weight (g/kernel)
    float plant_grain_no;                          // final grains /head
    float n_grain;                                // total grain N uptake (kg/ha)
    float n_green;                                // above ground green plant N (kg/ha)
    float n_stover;                               // nitrogen content of stover (kg\ha)
    float n_total;                                // total gross nitrogen content (kg/ha)
    float n_grain_conc_percent;                   // grain nitrogen %
    char  msg[200];                               // message
    float yield;                                  // grain yield dry wt (kg/ha)
    float yield_wet;                              // grain yield including moisture (kg/ha)

    //- Implementation Section ----------------------------------


    // crop harvested. Report status
    yield = 0.0;
    yield_wet = 0.0;
    grain_wt = 0.0;
    plant_grain_no = 0.0;
    n_grain = 0.0;
    for (vector<plantPart *>::iterator t = myParts.begin(); t != myParts.end(); t++)
       {
       yield = yield+(*t)->GrainTotal.DM() * gm2kg / sm2ha;
       yield_wet = yield_wet+(*t)->dmGrainWetTotal() * gm2kg / sm2ha;
       grain_wt = grain_wt + (*t)->grainWt();
       plant_grain_no = plant_grain_no+divide ((*t)->grainNo(), getPlants(), 0.0);
       n_grain = n_grain + (*t)->GrainTotal.N() * gm2kg/sm2ha;
       }


    float dmRoot = rootPart->Total.DM() * gm2kg / sm2ha;
    float nRoot = rootPart->Total.N() * gm2kg / sm2ha;

    n_grain_conc_percent = fruitPart->GrainTotal.NconcPercent();

    n_stover = tops.VegetativeTotal.N() * gm2kg / sm2ha;
    n_green = tops.Vegetative.N() * gm2kg / sm2ha;
    n_total = n_grain + n_stover;

    float stoverTot = tops.VegetativeTotal.DM();
    float DMRrootShootRatio = divide(dmRoot, tops.Total.DM() * gm2kg / sm2ha, 0.0);
    float HarvestIndex      = divide(yield, tops.Total.DM() * gm2kg / sm2ha, 0.0);
    float StoverCNRatio     = divide(stoverTot* gm2kg / sm2ha*plant_c_frac, n_stover, 0.0);
    float RootCNRatio       = divide(dmRoot*plant_c_frac, nRoot, 0.0);

    parent->writeString ("");

    sprintf (msg, "%s%4d%26s%s%10.1f"
             , " flowering day          = ",floweringEventObserver->getDoy(), " "
             , " stover (kg/ha)         = ",stoverTot* gm2kg / sm2ha);
    parent->writeString (msg);

    sprintf (msg, "%s%4d%26s%s%10.1f"
             , " maturity day           = ", maturityEventObserver->getDoy(), " "
             , " grain yield (kg/ha)    = ", yield);
    parent->writeString (msg);

    sprintf (msg, "%s%6.1f%24s%s%10.1f"
             , " grain % water content  = ", fruitPart->grainWaterContent() * fract2pcnt, " "
             , " grain yield wet (kg/ha)= ", yield_wet);
    parent->writeString (msg);

    sprintf (msg, "%s%8.3f%22s%s%10.1f"
             , " grain wt (g)           = ", grain_wt, " "
             , " grains/m^2             = ", fruitPart->grainNo());
    parent->writeString (msg);

    sprintf (msg, "%s%6.1f%24s%s%10.3f"
             , " grains/plant           = ", plant_grain_no, " "
             , " maximum lai            = ", g.lai_max);
    parent->writeString (msg);

    sprintf (msg, "%s%10.1f"
             , " total above ground biomass (kg/ha)    = ", tops.Total.DM() * gm2kg / sm2ha);
    parent->writeString (msg);

    sprintf (msg, "%s%10.1f",
               " live above ground biomass (kg/ha)     = "
              , (tops.Total.DM())* gm2kg / sm2ha);
    parent->writeString (msg);

    sprintf (msg, "%s%10.1f"
             , " green above ground biomass (kg/ha)    = ", tops.Green.DM()* gm2kg / sm2ha);
    parent->writeString (msg);

    sprintf (msg, "%s%10.1f"
             , " senesced above ground biomass (kg/ha) = ", tops.Senesced.DM()* gm2kg / sm2ha);
    parent->writeString (msg);

    sprintf (msg, "%s%8.1f"
             , " number of leaves       = ", leafPart->getLeafNo());
    parent->writeString (msg);

    sprintf (msg, "%s%8.2f%22s%s%10.2f"
             , " DM Root:Shoot ratio    = ", DMRrootShootRatio, " "
             , " Harvest Index          = ", HarvestIndex);
    parent->writeString (msg);

    sprintf (msg, "%s%8.2f%22s%s%10.2f"
             , " Stover C:N ratio       = ", StoverCNRatio, " "
             , " Root C:N ratio         = ", RootCNRatio);
    parent->writeString (msg);

    sprintf (msg, "%s%8.2f%22s%s%10.2f"
             , " grain N percent        = ", n_grain_conc_percent, " "
             , " total N content (kg/ha)= ", n_total);
    parent->writeString (msg);

    sprintf (msg, "%s%8.2f%22s%s%8.2f"
             , " grain N uptake (kg/ha) = ", n_grain, " "
             , " senesced N content (kg/ha)=", (tops.VegetativeTotal.N() - tops.Vegetative.N())* gm2kg / sm2ha);
    parent->writeString (msg);

    sprintf (msg, "%s%8.2f"
             , " green N content (kg/ha)= ", n_green);
    parent->writeString (msg);

    summary_p ();

    parent->writeString ("");

    sprintf (msg,"%s", " Average Stress Indices:                          Water Photo  Water Expan  N Photo      N grain conc");
    parent->writeString (msg);

    parent->writeString (g.averageStressMessage.c_str());
    g.averageStressMessage = "";

    }

bool  Plant::plant_auto_class_change (const char *action)
//=======================================================================================
// Change the Crop Class in response to a given action
   {
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
        scienceAPI.setClass2(c.class_change[i-c.class_action.begin()]);
        read();
        return true;
        }
    }

void Plant::plant_send_crop_chopped_event (const string&  crop_type             // (INPUT) crop type
                                           ,vector<string> &dm_type             // (INPUT) residue type
                                           ,vector<float>  &dlt_crop_dm         // (INPUT) residue weight (kg/ha)
                                           ,vector<float>  &dlt_dm_n            // (INPUT) residue N weight (kg/ha)
                                           ,vector<float>  &dlt_dm_p            // (INPUT) residue P weight (kg/ha)
                                           ,vector<float>  &fraction_to_residue) // (INPUT) fraction going to residue
//=======================================================================================
// Send a CropChoppedEvent via the communications system
   {
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
    }

/////////////////////////////Get&Set Interface code
bool Plant::set_plant_crop_class(protocol::QuerySetValueData&v)
    {
    FString crop_class;
    v.variant.unpack(crop_class);
    g.crop_class = crop_class.f_str();
    scienceAPI.setClass2(asString(crop_class));
    read();
    return true;
    }

void Plant::get_plant_status(protocol::Component *system, protocol::QueryValueData &qd)
{
    switch (g.plant_status) {
        case out: system->sendVariable(qd, FString("out")); break;
        case dead: system->sendVariable(qd, FString("dead")); break;
        case alive: system->sendVariable(qd, FString("alive")); break;
    }
}



float Plant::getLeafNo(void)
{
   return leafPart->getLeafNo();
}

void Plant::get_height(protocol::Component *system, protocol::QueryValueData &qd)
{
   system->sendVariable(qd, stemPart->height());
}

void Plant::get_width(protocol::Component *system, protocol::QueryValueData &qd)
{
   system->sendVariable(qd, leafPart->width());
}


void Plant::get_cover_tot(protocol::Component *system, protocol::QueryValueData &qd)
{
    float cover_tot = 1.0
        - (1.0 - plant.coverGreen())
        * (1.0 - plant.coverSen());

    system->sendVariable(qd, cover_tot);
}

//NIH up to here
void Plant::get_biomass(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, tops.Total.DM() * gm2kg / sm2ha);
}


void Plant::get_green_biomass(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, tops.Green.DM() * gm2kg / sm2ha);
}


void Plant::get_biomass_wt(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, tops.Total.DM());
}


void Plant::get_green_biomass_wt(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, tops.Green.DM());
}

void Plant::get_stover_biomass_wt(protocol::Component *system, protocol::QueryValueData &qd)
{
    float stoverTot = tops.VegetativeTotal.DM();
    system->sendVariable(qd, stoverTot);
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
    system->sendVariable(qd, tops.Total.N());
}


void Plant::get_n_uptake(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, tops.Total.N());
}


void Plant::get_green_biomass_n(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, tops.Green.N());
}

// plant nitrogen
void Plant::get_n_conc_stover(protocol::Component *system, protocol::QueryValueData &qd)
{
    float n_conc = divide (tops.Vegetative.N(), tops.Vegetative.DM(), 0.0) * fract2pcnt;
    system->sendVariable(qd, n_conc);
}

void Plant::get_n_conc_crit(protocol::Component *system, protocol::QueryValueData &qd)
{
    float n_conc = divide ((leafPart->n_conc_crit()*leafPart->Green.DM()
                           + stemPart->n_conc_crit()*stemPart->Green.DM())
                          , (leafPart->Green.DM() + stemPart->Green.DM())
                          , 0.0) * fract2pcnt;
    system->sendVariable(qd, n_conc);
}

void Plant::get_n_conc_min(protocol::Component *system, protocol::QueryValueData &qd)
{
    float n_conc = divide ((leafPart->n_conc_min() * leafPart->Green.DM()
                            + stemPart->n_conc_min() * stemPart->Green.DM())
                          , (leafPart->Green.DM() + stemPart->Green.DM())
                          , 0.0) * fract2pcnt;
    system->sendVariable(qd, n_conc);
}


void Plant::get_n_uptake_stover(protocol::Component *system, protocol::QueryValueData &qd)
{
    system->sendVariable(qd, tops.Vegetative.N());
}


void Plant::get_n_demanded(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<float>  n_demanded;

   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_n_demanded(n_demanded);

   systemInterface->sendVariable(qd, n_demanded);
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

void Plant::get_transp_eff(protocol::Component *system, protocol::QueryValueData &qd)
{
    float transp_eff = leafPart->transpirationEfficiency(); //FIXME - ?? how to handle fruitPart->transpirationEfficiency();
    system->sendVariable(qd, transp_eff);
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
    systemInterface->sendVariable(qd, tops.Total.P());  //()
}

void Plant::get_green_biomass_p(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    systemInterface->sendVariable(qd, tops.Green.P());  //()
}
//NIH up to here
void Plant::get_p_conc_stover(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    float p_conc_stover = divide (tops.Vegetative.P(), tops.Vegetative.DM(), 0.0) * fract2pcnt ;
    systemInterface->sendVariable(qd, p_conc_stover);  //()
}

void Plant::get_p_uptake_stover(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
    systemInterface->sendVariable(qd, tops.Vegetative.P());  //()
}

void Plant::get_dlt_dm_green_retrans(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<float>  dlt_dm_green_retrans;

   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dlt_dm_green_retrans(dlt_dm_green_retrans);

   systemInterface->sendVariable(qd, dlt_dm_green_retrans);
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

void Plant::get_dlt_p_retrans(protocol::Component *systemInterface, protocol::QueryValueData &qd)
{
   vector<plantPart*>::iterator part;
   vector<float>  dlt_p_retrans;

   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->get_dlt_p_retrans(dlt_p_retrans);

   systemInterface->sendVariable(qd, dlt_p_retrans);
}


float Plant::GreenDM(void) {return plant.Green.DM();}
float Plant::GreenN(void) {return plant.Green.N();}
float Plant::GreenP(void) {return plant.Green.P();}
float Plant::SenescedDM(void) {return plant.Senesced.DM();}
float Plant::SenescedN(void) {return plant.Senesced.N();}
float Plant::SenescedP(void) {return plant.Senesced.P();}

float Plant::getStageCode(void)  {return phenology->stageCode();}
float Plant::getStageNumber(void)  {return phenology->stageNumber();}
float Plant::getPlants(void)  {return population.Density();}
float Plant::getCo2(void)  {return Environment.co2;}
//float Plant::getRadnInterceptedPod(void)  {return g.radn_int_pod;}
float Plant::getDltDMPotRueVeg(void)  {return leafPart->dltDmPotRue();}
//float Plant::getDltDmVeg(void)  {return leafPart->dltDmTotal() + stemPart->dltDmTotal();}
////float Plant::getWaterSupplyPod(void)  {return g.swSupplyFruit;}
////float Plant::getWaterSupplyLeaf(void)  {return g.swSupplyVeg;}
float Plant::getDmTops(void) { return tops.Total.DM();}
float Plant::getDltDm(void) { return plant.dltDm();}
float Plant::getDltDmGreen(void) { return plant.dltDmGreen();}
float Plant::getDmVeg(void)  {return leafPart->Total.DM() + stemPart->Total.DM();}
float Plant::getDmGreenStem(void)  {return stemPart->Green.DM();}
float Plant::getDmGreenTot(void)  {return plant.Green.DM();}
// FIXME - remove next line when P demand corrections activated
float Plant::getRelativeGrowthRate(void) {return divide(arbitrator->dltDMWhole(plant.dltDmPotRue()), plant.Green.DM(), 0.0);} // the dlt_dm_pot_rue is only tops, thus either adjust it for roots or leave roots out of the divisor.
float Plant::getTotalPotentialGrowthRate(void) {return arbitrator->dltDMWhole(plant.dltDmPotRue());} // the dlt_dm_pot_rue is only tops, thus adjust it for roots.
float Plant::getDyingFractionPlants(void) {return population.DyingFractionPlants();}
float Plant::getLAI() {return leafPart->getLAI();}
float Plant::getCumSwdefPheno() {return g.cswd_pheno.getSum();}
float Plant::getCumSwdefPhoto() {return g.cswd_photo.getSum();}
float Plant::getCo2ModifierRue(void)  {return g.co2_modifier_rue;}
float Plant::getCo2ModifierTe(void)  {return g.co2_modifier_te;}
float Plant::getCo2ModifierNConc(void)  {return g.co2_modifier_n_conc;}
float Plant::getVpd(void)  {return Environment.vpdEstimate();}
float Plant::getTempStressPhoto(void)  {return g.temp_stress_photo;}
float Plant::getNfactPhoto(void)  {return g.nfact_photo;}
float Plant::getNfactGrainConc(void)  {return g.nfact_grain_conc;}
float Plant::getOxdefPhoto(void)  {return g.oxdef_photo;}
float Plant::getPfactPhoto(void)  {return g.pfact_photo;}
float Plant::getSwdefPhoto(void)  {return g.swdef_photo;}
bool  Plant::on_day_of(const string &what) {return (phenology->on_day_of(what));};
bool  Plant::inPhase(const string &what) {return (phenology->inPhase(what));};
void Plant::writeString (const char *line) {parent->writeString(line);};
void Plant::warningError (const char *msg) {parent->warningError(msg);};
const std::string & Plant::getCropType(void) {return c.crop_type;};
protocol::Component *Plant::getComponent(void) {return parent;};
int Plant::daysInCurrentPhase() {return phenology->daysInCurrentPhase();}
float Plant::ttInCurrentPhase() {return phenology->ttInCurrentPhase();}

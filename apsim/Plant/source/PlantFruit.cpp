
// Modification log
// 6 Aug 97 J. Hargreaves  Implementation

#define YES 1
#define NO 0
#define TEST_PlantFruit NO					// build unit test?
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

#include "PlantLibrary.h"
#include "PlantComponent.h"
#include "PlantPhenology.h"
#include "Plant.h"
#include "PlantParts.h"
#include "PlantInterface.h"

#include "PlantFruit.h"

using namespace std;

static const char* floatType =        "<type kind=\"single\"/>";

void push_routine (const char *) {};
void pop_routine (const char *) {};

      inline bool floatsAreEqual(float A, float B, float C) {return(fabs(A-B)<C);}

// default constructor
// 	initialise data members.
PlantFruit::PlantFruit(plantInterface *p, const string &name) : plantPart(p, name)
{
//    zeroAllGlobals();
    gDm_stress_max.setup(&gDlt_dm_stress_max);
    otherObservers.addObserver(&gDm_stress_max);
}

//PlantFruit::PlantFruit(Plant *P)  			 // member initialisation
//{
//   plant = P;
//   zeroVariables();
//}

//PlantFruit::PlantFruit(float greenShell, float greenMeal, float senescedShell, float senescedMeal, float deadShell, float deadMeal)
//{
//
//	green.shell = greenShell;
//	green.meal = greenMeal;
//	senesced.shell = senescedShell;
//	senesced.meal = senescedMeal;
//	dead.shell = deadShell;
//	dead.meal = deadMeal;
//}

// destructor
PlantFruit::~PlantFruit()
{
    if (podPart) delete podPart;
    if (oilPart) delete oilPart;
    if (mealPart) delete mealPart;

}

ostream &operator<<(ostream &output, const PlantFruit &pool)
{
	output << "PlantFruit:" << endl;
	output << "   Green cover:    " << pool.coverPod.green << endl;
	output << "   Senesced cover: " << pool.coverPod.sen << endl;
	output << "   Dead cover:     " << pool.coverPod.dead << endl;
	output << endl;
//	output << "   Green shell:    " << pool.green.shell << endl;
//	output << "   Green meal:    " << pool.green.meal << endl;
//	output << "   Senesced shell: " << pool.senesced.shell << endl;
//	output << "   Senesced meal: " << pool.senesced.meal << endl;
//	output << "   Dead shell:     " << pool.dead.shell << endl;
//	output << "   Dead meal:     " << pool.dead.meal << endl << endl;
	output << endl;
      return output;
}

// copy constructor
//	copy data members of object
//===========================================================================
//PlantFruit::PlantFruit(const PlantFruit &PlantFruit)
////===========================================================================
//{
//	throw std::invalid_argument("Copy constructor NI for plantFruit");
//}


// Assigment operator
//	assign data members of object
//===========================================================================
const PlantFruit &PlantFruit::operator=(const PlantFruit &other)
//===========================================================================
{
	throw std::invalid_argument("Assignment operator NI for plantFruit");
}

//===========================================================================
void PlantFruit::doRegistrations(protocol::Component *system)
//===========================================================================
{
   system->addGettableVar("pai", gPai, "m^2/m^2", "Pod area index");
   system->addGettableVar("dlt_pai", gDlt_pai, "m^2/m^2", "Delta Pod area index");
   system->addGettableVar("dlt_dm_grain_demand",gDlt_dm_grain_demand, "g/m^2", "??");
   system->addGettableVar("dlt_dm_fruit", gDlt_dm, "g/m^2", "Change in dry matter");
   system->addGettableVar("dlt_dm_pot_rue_pod", gDlt_dm_pot_rue_pod, "g/m^2", "Potential dry matter production via photosynthesis");
   system->addGettableVar("dlt_dm_pot_te_pod", gDlt_dm_pot_te, "g/m^2", "Potential dry matter production via transpiration");
   system->addGettableVar("dlt_dm_oil_conv",gDlt_dm_oil_conv,"g/m^2", "change in oil via ??");
   system->addGettableVar("grain_no",gGrain_no, "/m^2", "Grain number");
   setupGetFunction(system, "grain_size", protocol::DTsingle, false, &PlantFruit::get_grain_size, "g", "Size of each grain");
   system->addGettableVar("pod_wt", podPart->g.dm_green,"g/m^2", "Weight of pods");
   setupGetFunction(system, "grain_wt", protocol::DTsingle, false, &PlantFruit::get_grain_wt, "g/m^2", "Weight of grain");
   system->addGettableVar("meal_wt", mealPart->g.dm_green, "g/m^2", "Weight of meal");
   system->addGettableVar("oil_wt", oilPart->g.dm_green, "g/m^2", "Weight of oil");
   setupGetFunction(system, "head_wt", protocol::DTsingle, false,&PlantFruit::get_head_wt, "g/m^2", "Weight of heads");
   setupGetFunction(system, "yield", protocol::DTsingle, false,&PlantFruit::get_yield,  "kg/ha", "Yield");




   idLatitude = system->addRegistration (RegistrationType::get, "latitude", floatType, "", "");


   for (vector<plantPart *>::iterator t = myParts.begin();
        t != myParts.end();
        t++)
       (*t)->doRegistrations(system);

   plantPart::doRegistrations(system);
}

//===========================================================================
float PlantFruit::dmTotal(void)
//===========================================================================
{
    float dmTotal = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myParts.begin(); part != myParts.end(); part++)
      dmTotal += (*part)->dmTotal();
   return dmTotal;
}

//===========================================================================
float PlantFruit::dmGrainTotal(void)
//===========================================================================
{
    float dmTotal = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myGrainParts.begin(); part != myGrainParts.end(); part++)
      dmTotal += (*part)->dmTotal();
   return dmTotal;
}

//===========================================================================
float PlantFruit::dmGreenGrainTotal(void)
//===========================================================================
{
    float dmTotal = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myGrainParts.begin(); part != myParts.end(); part++)
      dmTotal += (*part)->dmGreen();
   return dmTotal;
}

//===========================================================================
void PlantFruit::get_grain_size(protocol::Component *system, protocol::QueryValueData &qd)
//===========================================================================
{
    float grain_size = divide (dmGrainTotal()
                            , gGrain_no, 0.0);

    system->sendVariable(qd, grain_size);
}

//===========================================================================
void PlantFruit::get_grain_wt(protocol::Component *system, protocol::QueryValueData &qd)
//===========================================================================
{
    system->sendVariable(qd, oilPart->g.dm_green + mealPart->g.dm_green);
}

//===========================================================================
void PlantFruit::get_head_wt(protocol::Component *system, protocol::QueryValueData &qd)
//===========================================================================
{
    float headWt = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myParts.begin(); part != myParts.end(); part++)
      headWt += (*part)->g.dm_green;

    system->sendVariable(qd, headWt);
}


//===========================================================================
void PlantFruit::get_yield(protocol::Component *system, protocol::QueryValueData &qd)
//===========================================================================
{
    system->sendVariable(qd, dmGrainTotal() * gm2kg / sm2ha);
}

//===========================================================================
void PlantFruit::grain_number (void)
//===========================================================================
//+  Purpose
//       Calculate Grain Numer

//+  Mission Statement
//       Calculate Grain Numer

//+  Changes

{
    if (cGrain_no_option == 1)
        {
        // do not use grain number
        gGrain_no = 0;
        }
    else if (cGrain_no_option == 2)
        {
        grain_number (plant->getDmGreenStem()
                    , pGrains_per_gram_stem
                    , &gGrain_no);
        }
    else
        {
        throw std::invalid_argument ("invalid template option");
        }

    return;
}

//===========================================================================
void PlantFruit::grain_number (float stem_dm_green
                             , float p_grains_per_gram_stem
                             , float *g_grain_no)    // OUTPUT
//===========================================================================
//+  Purpose
//       Perform grain number calculations
{
  if (phenology->on_day_of ("emergence"))
        {
        // seedling has just emerged.
        *g_grain_no = 0.0;
        }
  else if (phenology->on_day_of ("flowering"))
      {
      // we are at first day of grainfill.
      *g_grain_no = p_grains_per_gram_stem * stem_dm_green;
      }
  else
      {
      // no changes
      }
}

//===========================================================================
void PlantFruit::doTick(protocol::timeType &tick)
//===========================================================================
{
  double sd = (double)tick.startday;
  jday_to_day_of_year(&sd, &gDay_of_year, &gYear);
}

// Field a NewMet event
//===========================================================================
void PlantFruit::doNewMet(protocol::newmetType &newmet)
//===========================================================================
{
  if (gHasreadconstants)
  {
     gMaxt = newmet.maxt;
     gMint = newmet.mint;
  }
}

//===========================================================================
void PlantFruit::readCultivarParameters (protocol::Component *system, const string &cultivar)
//===========================================================================
{

//+  Local Variables
    string s;
    char  msg[200];                             // output string
    int   numvals;                                // number of values read

//- Implementation Section ----------------------------------

    system->writeString (" - reading fruit cultivar parameters");

    //  plant_dm_grain_hi
    system->readParameter (cultivar.c_str()
    , "x_pp_hi_incr"/*,  "(h)"*/
    , pX_pp_hi_incr
    , pNum_pp_hi_incr
    , 0.0, 24.0);

    system->readParameter (cultivar.c_str()
    , "y_hi_incr"/*,  "()"*/
    , pY_hi_incr
    , pNum_pp_hi_incr
    , 0.0, 1.0);

    system->readParameter (cultivar.c_str()
    , "x_hi_max_pot_stress"/*,  "(0-1)"*/
    , pX_hi_max_pot_stress, pNum_hi_max_pot
    , 0.0, 1.0);

    system->readParameter (cultivar.c_str()
    , "y_hi_max_pot"//, "(0-1)"
    , pY_hi_max_pot, pNum_hi_max_pot
    , 0.0, 1.0);

    if (system->readParameter (cultivar.c_str()
                , "min_temp_grnfill"//, "()"
                , pMinTempGrnFill
                , 0.0, 20.0, true) == false)
    {
        pMinTempGrnFill = -100.0;
        pDaysDelayGrnFill = 0;
    }
    else
    {
       system->readParameter (cultivar.c_str()
               , "days_delay_grnfill"//, "()"
               , pDaysDelayGrnFill
               , 0, 10);
    }

    if (cGrain_no_option==2)
        {
        system->readParameter (cultivar.c_str()
        , "grains_per_gram_stem"//, "(/g)"
        , pGrains_per_gram_stem
        , 0.0, 10000.0);
        }
    if (cGrain_fill_option==2)
        {
        system->readParameter (cultivar.c_str()
        , "potential_grain_filling_rate"//, "(g/grain/day)"
        , pPotential_grain_filling_rate
        , 0.0, 1.0);
        }

    // report

    // TEMPLATE OPTION
    s = string("   x_pp_hi_incr               = ");
    for (int i = 0; i < pNum_pp_hi_incr; i++)
       s = s + ftoa(pX_pp_hi_incr[i], "10.2") + " ";
    system->writeString (s.c_str());

    s = string("   y_hi_incr                  = ");
    for (int i = 0; i < pNum_pp_hi_incr; i++)
       s = s + ftoa(pY_hi_incr[i], "10.4") + " ";
    system->writeString (s.c_str());

    s = string("   x_hi_max_pot_stress        = ");
    for (int i = 0; i < pNum_hi_max_pot; i++)
       s = s + ftoa(pX_hi_max_pot_stress[i], "10.2") + " ";
    system->writeString (s.c_str());

    s = string("   y_hi_max_pot               = ");
    for (int i = 0; i < pNum_hi_max_pot; i++)
       s = s + ftoa(pY_hi_max_pot[i], "10.2") + " ";
    system->writeString (s.c_str());

   for (vector<plantPart *>::iterator t = myParts.begin();
        t != myParts.end();
        t++)
       (*t)->readCultivarParameters(system, cultivar);
}

void PlantFruit::onPlantEvent(const string &event)
{
   for (vector<plantPart *>::iterator t = myParts.begin();
        t != myParts.end();
        t++)
       (*t)->onPlantEvent(event);
}

void PlantFruit::zeroAllGlobals(void)
{

   coverPod.green = 0.0;
   coverPod.sen   = 0.0;
   coverPod.dead  = 0.0;

   gHasreadconstants = false;
   gLatitude = 0.0;
   gMaxt = 0.0;
   gMint = 0.0;

   cExtinctionCoeffPod = 0.0;
   cSpec_pod_area  = 0.0;
   cRue_pod  = 0.0;
   fill_real_array (cTransp_eff_cf, 0.0, max_table);

   gPai  = 0.0;
   gDelayGrnFill  = 0.0;
   gDaysDelayedGrnFill  = 0.0;
   gTranspEff = 0.0;

   cSvp_fract  = 0.0;
//   cFrac_pod  = 0.0;
   cGrain_no_option  = 0.0;
   cGrain_fill_option  = 0.0;
   cNum_temp_grainfill;
   cGrain_n_option  = 0.0;
   cSw_fac_max  = 0.0;
   cTemp_fac_min  = 0.0;
   cSfac_slope  = 0.0;
   cTfac_slope  = 0.0;
   cPotential_grain_n_filling_rate  = 0.0;
   cCrit_grainfill_rate  = 0.0;
   cNum_temp_grain_n_fill;
   cGrn_water_cont  = 0.0;
   cCarbo_oil_conv_ratio  = 0.0;
   cGrain_oil_conc  = 0.0;
   cNum_n_conc_stage;
   cN_conc_crit_grain  = 0.0;
   cN_conc_max_grain  = 0.0;
   cN_conc_min_grain  = 0.0;
   cCarbo_oil_conv_ratio = 0.0;
   cPod_trans_frac = 0.0;
   fill_real_array (cX_co2_te_modifier, 0.0, max_table);
   fill_real_array (cY_co2_te_modifier, 0.0, max_table);
   cNum_co2_te_modifier = 0;

//   cX_temp_grainfill[max_table];
//   cY_rel_grainfill[max_table];
//   cX_temp_grain_n_fill[max_table];
//   cY_rel_grain_n_fill[max_table];
//   cX_stage_code[max_table];
//   cY_n_conc_crit_pod[max_table];
//   cY_n_conc_max_pod[max_table];
//   cY_n_conc_min_pod[max_table];

   cTwilight = 0.0;
   fill_real_array (pX_pp_hi_incr, 0.0, max_table);
   pGrains_per_gram_stem = 0.0;
   pPotential_grain_filling_rate = 0.0;

   fill_real_array (pX_pp_hi_incr, 0.0, max_table);
   fill_real_array (pY_hi_incr, 0.0, max_table);
   pNum_pp_hi_incr = 0;
   pNum_hi_max_pot = 0;
   fill_real_array (pX_hi_max_pot_stress, 0.0, max_table);
   fill_real_array (pY_hi_max_pot, 0.0, max_table);

   pMinTempGrnFill = 0.0;
   pDaysDelayGrnFill = 0;

   gGrain_energy = 0.0;
   gGrain_no = 0.0;
   dmOil_conv_retranslocate = 0.0;

   gDlt_dm_stress_max        = 0.0;


   for (vector<plantPart *>::iterator t = myParts.begin();
        t != myParts.end();
        t++)
       (*t)->zeroAllGlobals();

}

void PlantFruit::zeroDeltas(void)
{
   gDlt_pai = 0.0;
   gDlt_dm_pot_rue_pod = 0.0;
   gDlt_dm_pot_te = 0.0;
   gDlt_dm_grain_demand = 0.0;
   gDlt_dm_oil_conv = 0.0;
   gDlt_dm = 0.0;

   gN_grain_demand = 0.0;

   for (vector<plantPart *>::iterator t = myParts.begin();
        t != myParts.end();
        t++)
       (*t)->zeroDeltas();

}

// ====================================================================
void PlantFruit::onHarvest(float cutting_height, float remove_fr,
                              vector<string> &dm_type,
                              vector<float> &dlt_crop_dm,
                              vector<float> &dlt_dm_n,
                              vector<float> &dlt_dm_p,
                              vector<float> &fraction_to_residue)
// ====================================================================
{
    for (vector<plantPart *>::iterator part = myParts.begin();
         part != myParts.end();
         part++)
       (*part)->onHarvest(cutting_height, remove_fr,
                        dm_type,
                        dlt_crop_dm,
                        dlt_dm_n,
                        dlt_dm_p,
                        fraction_to_residue);
}

// ====================================================================
void PlantFruit::onKillStem(float cutting_height, float remove_fr,
                              vector<string> &dm_type,
                              vector<float> &dlt_crop_dm,
                              vector<float> &dlt_dm_n,
                              vector<float> &dlt_dm_p,
                              vector<float> &fraction_to_residue)
// ====================================================================
{
    for (vector<plantPart *>::iterator part = myParts.begin();
         part != myParts.end();
         part++)
    {

        float dm_init = (*part)->c.dm_init * plant->getPlants();
        float n_init = dm_init * (*part)->c.n_init_conc;
        float p_init = dm_init * (*part)->c.p_init_conc;

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
}

// ====================================================================
void PlantFruit::doInit (PlantComponent *systemInterface, PlantPhenology *plantPhenology)
// ====================================================================
{
   parentPlant = systemInterface;
   phenology = plantPhenology;
}

// ====================================================================
void PlantFruit::doInit1 ()
// ====================================================================
{
    podPart = new fruitPodPart(plant, "pod");
//    myThings.push_back(podPart);
    myParts.push_back(podPart);
    supplyPools.push_back(podPart);

    mealPart = new fruitMealPart(plant, "meal");
//    myThings.push_back(mealPart);
    myParts.push_back(mealPart);
    myGrainParts.push_back(mealPart);

    oilPart = new fruitOilPart(plant, "oil");
//    myThings.push_back(oilPart);
    myParts.push_back(oilPart);
    myGrainParts.push_back(oilPart);

}


//===========================================================================
void PlantFruit::readConstants(protocol::Component *system, const string &section)
//===========================================================================
{
      system->getVariable(idLatitude, gLatitude, -90.0, 90.0);
      for (vector<plantPart *>::iterator t = myParts.begin();
           t != myParts.end();
           t++)
          (*t)->readConstants(system, section);

}

//===========================================================================
void PlantFruit::readSpeciesParameters(protocol::Component *system, vector<string> &sections)
//===========================================================================
{

    int   numvals;                                // number of values returned

    system->readParameter (sections,
                      "transp_eff_cf"//, "(kpa)"
                     , cTransp_eff_cf, numvals
                     , 0.0, 1.0);

    system->readParameter (sections
                     , "x_co2_te_modifier"//, "()"
                     , cX_co2_te_modifier, cNum_co2_te_modifier
                     , 0.0, 1000.0);
    system->readParameter (sections
                     , "y_co2_te_modifier"//, "()"
                     , cY_co2_te_modifier, cNum_co2_te_modifier
                     , 0.0, 10.0);

    //    plant_retranslocate
    system->readParameter (sections
                   ,"pod_trans_frac"//, "()"
                   , cPod_trans_frac
                   , 0.0, 1.0);

    //    plant_phenology_init
    system->readParameter (sections
                   , "twilight"//, "(o)"
                   , cTwilight
                   , -90.0, 90.0);

    system->readParameter (sections
                   ,"extinct_coef_pod"//, "()"
                   , cExtinctionCoeffPod
                   , 0.0, 1.0);

    system->readParameter (sections
                   ,"spec_pod_area"//, "()"
                   , cSpec_pod_area
                   , 0.0, 100000.0);

    system->readParameter (sections
                   ,"rue_pod"//, "()"
                   , cRue_pod
                   , 0.0, 3.0);

//    plant_transp_eff

    system->readParameter (sections
                   ,"svp_fract"//, "()"
                   , cSvp_fract
                   , 0.0, 1.0);

    int cPartition_option = 0;
    system->readParameter (sections,
                       "partition_option"//, "()"
                      , cPartition_option
                      , 1, 3);

    if (cPartition_option==1 )
         system->readParameter (sections
                         ,"frac_pod"//, "()"
                         , cFrac_pod, numvals
                         , 0.0, 2.0);

    else if (cPartition_option==2)
        {
        system->readParameter (sections
                         ,"x_stage_no_partition"//, "()"
                         , cX_stage_no_partition
                         , cNum_stage_no_partition
                         , 0.0, 20.0);

        system->readParameter (sections
                         ,"y_frac_pod"//, "()"
                         , cY_frac_pod, numvals
                         , 0.0, 2.0);

        }
    else if (cPartition_option==3)
        {
           // do nothing
        }
    //    grain number
    system->readParameter (sections
                      ,"grain_no_option"//, "()"
                      , cGrain_no_option
                      , 1, 2);

    //    legume grain filling
    system->readParameter (sections
                      ,"grain_fill_option"//,/ "()"
                      , cGrain_fill_option
                      , 1, 3);

    if (cGrain_fill_option==2 || cGrain_fill_option==3)
        {
        system->readParameter (sections
                         , "x_temp_grainfill"
                         //, "(oc)"
                         , cX_temp_grainfill
                         , cNum_temp_grainfill
                         , 0.0
                         , 40.0);

        system->readParameter (sections
                         ,"y_rel_grainfill"
                         //, "(-)"
                         , cY_rel_grainfill
                         , cNum_temp_grainfill
                         , 0.0
                         , 1.0);
        }

     //    plant_n_dlt_grain_conc
     system->readParameter (sections
                        , "grain_n_option"//, "()"
                        , cGrain_n_option
                        , 1, 2);

     if (cGrain_n_option==1)
         {
         system->readParameter (sections
                        ,"sw_fac_max"//, "()"
                        , cSw_fac_max
                        , 0.0, 100.0);

         system->readParameter (sections
                        ,"temp_fac_min"//, "()"
                        , cTemp_fac_min
                        , 0.0, 100.0);

         system->readParameter (sections
                        ,"sfac_slope"//, "()"
                        , cSfac_slope
                        , -10.0, 0.0);

         system->readParameter (sections
                        ,"tfac_slope"//, "()"
                        , cTfac_slope
                        , 0.0, 100.0);
         }
     else
         {
         system->readParameter (sections
                        , "potential_grain_n_filling_rate"//, "()"
                        , cPotential_grain_n_filling_rate
                        , 0.0, 1.0);

         system->readParameter (sections
                        , "crit_grainfill_rate"//, "(mg/grain/d)"
                        , cCrit_grainfill_rate
                        , 0.0, 1.0);

         system->readParameter (sections
                          , "x_temp_grain_n_fill"//,  "(oC)"
                          , cX_temp_grain_n_fill
                          , cNum_temp_grain_n_fill
                          , 0.0
                          , 40.0);

         system->readParameter (sections
                          , "y_rel_grain_n_fill"
                          //, "(-)"
                          , cY_rel_grain_n_fill
                          , cNum_temp_grain_n_fill
                          , 0.0
                          , 1.0);
         }

    //    plant_event
    system->readParameter (sections
                   ,"grn_water_cont"//, "(g/g)"
                   , cGrn_water_cont
                   , 0.0, 1.0);

    //    plant_dm_partition
    system->readParameter (sections
                   ,"carbo_oil_conv_ratio"//, "()"
                   , cCarbo_oil_conv_ratio
                   , 0.0, 20.0);

    system->readParameter (sections
                   ,"grain_oil_conc"//, "()"
                   , cGrain_oil_conc
                   , 0.0, 1.0);

    system->readParameter (sections
                     , "x_stage_code"//, "()"
                     , cX_stage_code, cNum_n_conc_stage
                     , 0.0, 100.0);

    system->readParameter (sections
                     , "y_n_conc_crit_pod"//, "()"
                     , cY_n_conc_crit_pod, cNum_n_conc_stage
                     , 0.0, 100.0);

    system->readParameter (sections
                     , "y_n_conc_max_pod"//, "()"
                     , cY_n_conc_max_pod, cNum_n_conc_stage
                     , 0.0, 100.0);

    system->readParameter (sections
                     , "y_n_conc_min_pod"//, "()"
                     , cY_n_conc_min_pod, cNum_n_conc_stage
                     , 0.0, 100.0);

    system->readParameter (sections
                   , "n_conc_crit_grain"//, "()"
                   , cN_conc_crit_grain
                   , 0.0, 100.0);

    system->readParameter (sections
                   , "n_conc_max_grain"//, "()"
                   , cN_conc_max_grain
                   , 0.0, 100.0);

    system->readParameter (sections
                   , "n_conc_min_grain"//, "()"
                   , cN_conc_min_grain
                   , 0.0, 100.0);

    if (cGrain_fill_option == 3)
        {
        system->readParameter (sections
                          , "x_temp_grainfill"
                          //, "(oC)"
                          , cX_temp_grainfill
                          , cNum_temp_grainfill
                          , 0.0
                          , 40.0);

        system->readParameter (sections
                          , "y_rel_grainfill"
                          //, "(-)"
                          , cY_rel_grainfill
                          , cNum_temp_grainfill
                          , 0.0
                          , 1.0);
        }
      gHasreadconstants = true;

        for (vector<plantPart *>::iterator part = myParts.begin();
             part != myParts.end();
             part++)
           (*part)->readSpeciesParameters(system, sections);

}

// Command
//===========================================================================
float PlantFruit::divide (float dividend, float divisor, float default_value) const
//===========================================================================

/*Definition
 *   Returns (dividend / divisor) if the division can be done
 *   without overflow or underflow.  If divisor is zero or
 *   overflow would have occurred, a specified default is returned.
 *   If underflow would have occurred, zero is returned.
 *Assumptions
 *   largest/smallest real number is 1.0e+/-30
 *Parameters
 *   dividend:     dividend
 *   divisor:      divisor
 *   defaultValue: default value to return if overflow
 *Calls
 *   reals_are_equal
 */

   {
   //Constant Values
   const float LARGEST = 1.0e30;    //largest acceptable no. for quotient
   const float SMALLEST = 1.0e-30;  //smallest acceptable no. for quotient
   const float nought = 0.0;
   const float one = 1.0;
   const float granularity = 1.0e-6;

   //Local Varialbes
   float quotient;

   //Implementation
   if(floatsAreEqual(dividend, nought, granularity))      //multiplying by 0
      {
      quotient = nought;
      }
   else if(floatsAreEqual(divisor, nought, granularity))  //dividing by 0
      {
      quotient = default_value;
      }
   else if(fabs(divisor) < one)            //possible overflow
      {
      if(fabs(dividend) > fabs(LARGEST * divisor)) //overflow
         {
         quotient = default_value;
         }
      else
         {
         quotient = dividend / divisor;          //ok
         }
      }
   else if(fabs(divisor) > one)             //possible underflow
      {
      if(fabs(dividend) < fabs(SMALLEST * divisor))    //underflow
         {
         quotient = nought;
         }
      else
         {
         quotient = dividend / divisor;                //ok
         }
      }
   else
      {
      quotient = dividend / divisor;                   //ok
      }
   return quotient;
   }


//===========================================================================
//void PlantFruit::setValue(float greenShell, float greenMeal, float senescedShell, float senescedMeal, float deadShell, float deadMeal)
//===========================================================================
//{
//
//	green.shell = greenShell;
//	green.meal = greenMeal;
//	senesced.shell = senescedShell;
//	senesced.meal = senescedMeal;
//	dead.shell = deadShell;
//	dead.meal = deadMeal;
//}

//===========================================================================
void PlantFruit::putStates(vector<plantPart *> fruitParts)
//===========================================================================
{
    vector<plantPart *>::iterator myPart =myParts.begin();

    vector<plantPart *>::iterator part;
    for (part = fruitParts.begin(); part != fruitParts.end(); part++)
    {
      (*part)->g.dm_dead = (*myPart)->g.dm_dead;
      (*part)->g.dm_senesced = (*myPart)->g.dm_senesced;
      (*part)->g.dm_green = (*myPart)->g.dm_green;
      (*part)->g.n_dead = (*myPart)->g.n_dead;
      (*part)->g.n_senesced = (*myPart)->g.n_senesced;
      (*part)->g.n_green = (*myPart)->g.n_green;

       if (plant->phosphorusAware())
       {
         (*part)->g.p_dead = (*myPart)->g.p_dead;
         (*part)->g.p_sen = (*myPart)->g.p_sen;
         (*part)->g.p_green = (*myPart)->g.p_green;
      }
      myPart++;
    }

}
//===========================================================================
void PlantFruit::getDltNGreen(vector<plantPart *> fruitParts)
//===========================================================================
{
    dlt.n_green = 0.0;
    vector<plantPart *>::iterator myPart =myParts.begin();

    vector<plantPart *>::iterator part;
    for (part = fruitParts.begin(); part != fruitParts.end(); part++)
    {
      (*myPart)->dlt.n_green = (*part)->dlt.n_green;
      dlt.n_green +=(*myPart)->dlt.n_green;
      myPart++;
    }

}
//===========================================================================
void PlantFruit::getDltDmGreen(vector<plantPart *> fruitParts)
//===========================================================================
{
    dlt.dm_green = 0.0;
    vector<plantPart *>::iterator myPart =myParts.begin();

    vector<plantPart *>::iterator part;
    for (part = fruitParts.begin(); part != fruitParts.end(); part++)
    {
      (*myPart)->dlt.dm_green = (*part)->dlt.dm_green;
      dlt.dm_green +=(*myPart)->dlt.dm_green;
      myPart++;
    }

}
//===========================================================================
void PlantFruit::getDltNRetrans(vector<plantPart *> fruitParts)
//===========================================================================
{
    dlt.n_retrans = 0.0;
    vector<plantPart *>::iterator myPart =myParts.begin();

    vector<plantPart *>::iterator part;
    for (part = fruitParts.begin(); part != fruitParts.end(); part++)
    {
      (*myPart)->dlt.n_retrans = (*part)->dlt.n_retrans;
      dlt.n_retrans +=(*myPart)->dlt.n_retrans;
      myPart++;
    }

}
//===========================================================================
void PlantFruit::getDltNSenescedRetrans(float navail, float n_demand_tot)
//===========================================================================
{
    dlt.n_senesced_retrans = 0.0;
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
    {
      (*myPart)->dlt.n_senesced_retrans = navail * divide ((*myPart)->v.n_demand, n_demand_tot, 0.0);
      dlt.n_senesced_retrans +=(*myPart)->dlt.n_senesced_retrans;
    }
}
//===========================================================================
void PlantFruit::update(float dying_fract_plants, vector<plantPart *> fruitParts)
//===========================================================================
{

// Get deltas from plant for the time being

// Get DM


    vector<plantPart *>::iterator fPart =fruitParts.begin();

    vector<plantPart *>::iterator part;
    for (part = myParts.begin(); part != myParts.end(); part++)
    {

      (*part)->dlt.n_dead_detached = (*fPart)->dlt.n_dead_detached;
      (*part)->dlt.n_green = (*fPart)->dlt.n_green;
//      (*part)->dlt.n_retrans = (*fPart)->dlt.n_retrans;
      (*part)->dlt.n_senesced = (*fPart)->dlt.n_senesced;
      (*part)->dlt.n_green_dead = (*fPart)->dlt.n_green_dead;
      (*part)->dlt.n_senesced_dead = (*fPart)->dlt.n_senesced_dead;
      (*part)->dlt.n_dead_detached = (*fPart)->dlt.n_dead_detached;

      (*part)->dlt.dm_dead_detached = (*fPart)->dlt.dm_dead_detached;
//
//      (*part)->dlt.dm_green = (*fPart)->dlt.dm_green;
      (*part)->dlt.dm_green_retrans = (*fPart)->dlt.dm_green_retrans;
//      (*part)->dlt.dm_senesced = (*fPart)->dlt.dm_senesced;
//
      (*part)->dlt.dm_detached = (*fPart)->dlt.dm_detached;
      (*part)->dlt.dm_green_dead = (*fPart)->dlt.dm_green_dead;
      (*part)->dlt.dm_senesced_dead = (*fPart)->dlt.dm_senesced_dead;

       if (plant->phosphorusAware())
       {
         (*part)->dlt.p_green = (*fPart)->dlt.p_green;
         (*part)->dlt.p_retrans = (*fPart)->dlt.p_retrans;
         (*part)->dlt.p_sen = (*fPart)->dlt.p_sen;
//         (*part)->dlt.p_senesced_dead = (*fPart)->dlt.p_senesced_dead;
//         (*part)->dlt.p_green_dead = (*fPart)->dlt.p_green_dead;
//         (*part)->dlt.p_dead_detached = (*fPart)->dlt.p_dead_detached;
      }
      fPart++;
    }

// Update N
    for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->updateN(dying_fract_plants);

        g.n_green = 0.0;
        g.n_senesced = 0.0;
        g.n_dead = 0.0;
        dlt.n_senesced_dead = 0.0;
        dlt.n_green_dead = 0.0;

        for (part = myParts.begin();
             part != myParts.end();
             part++)
        {
           g.n_green += (*part)->g.n_green;
           g.n_senesced += (*part)->g.n_senesced;
           g.n_dead += (*part)->g.n_dead;
           dlt.n_senesced_dead += (*part)->dlt.n_senesced_dead;
           dlt.n_green_dead += (*part)->dlt.n_green_dead;
        }

// Update DM
    for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->updateDm(dying_fract_plants);

        g.dm_green = 0.0;
        g.dm_senesced = 0.0;
        g.dm_dead = 0.0;
        dlt.dm_senesced_dead = 0.0;
        dlt.dm_green_dead = 0.0;
        for (part = myParts.begin();
             part != myParts.end();
             part++)
        {
           g.dm_green += (*part)->g.dm_green;
           g.dm_senesced += (*part)->g.dm_senesced;
           g.dm_dead += (*part)->g.dm_dead;
           dlt.dm_senesced_dead += (*part)->dlt.dm_senesced_dead;
           dlt.dm_green_dead += (*part)->dlt.dm_green_dead;
        }


// Update P
    if (plant->phosphorusAware())
    {
    for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->updateP(dying_fract_plants);
    }

        g.p_green = 0.0;
        g.p_sen = 0.0;
        g.p_dead = 0.0;
//        dlt.p_senesced_dead = 0.0;
//        dlt.p_green_dead = 0.0;
        for (part = myParts.begin();
             part != myParts.end();
             part++)
        {
           g.p_green += (*part)->g.p_green;
           g.p_sen += (*part)->g.p_sen;
           g.p_dead += (*part)->g.p_dead;
//           dlt.p_senesced_dead += (*part)->dlt.p_senesced_dead;
//           dlt.p_green_dead += (*part)->dlt.p_green_dead;
        }

    // transfer plant grain no.
    float dlt_grain_no_lost  = gGrain_no * dying_fract_plants;
    gGrain_no -= dlt_grain_no_lost;


    gPai += gDlt_pai;
    if (phenology->inPhase("hi_stress_sensitive")) gDm_stress_max.update();
}

//===========================================================================
void PlantFruit::n_conc_limits(void)
//===========================================================================
{
    vector<plantPart *>::iterator part;
    for (part = myParts.begin(); part != myParts.end(); part++)
    {
      (*part)->n_conc_limits();
    }
}



// Query
float PlantFruit::coverTotal() const
{
   return 1.0 - (1.0 - coverPod.green) * (1.0 - coverPod.sen) * (1.0 - coverPod.dead);
}

float PlantFruit::coverGreen() const
{
   return coverPod.green;
}

float PlantFruit::coverDead() const
{
   return coverPod.dead;
}

float PlantFruit::coverSen() const
{
   return coverPod.sen;
}

//float PlantFruit::total() const
//{
//
//	return green.shell + green.meal + senesced.shell + senesced.meal + dead.shell + dead.meal;
//}

void PlantFruit::display(ostream &os) const
{
	os << "PlantFruit:" << endl;
	os << "Green cover:    " << coverPod.green << endl;
	os << "Senesced cover: " << coverPod.sen << endl;
	os << "Dead cover:     " << coverPod.dead << endl;
//	os << "Green shell: " << green.shell << endl;
//	os << "Green meal: " << green.meal << endl;
//	os << "Senesced shell: " << senesced.shell << endl;
//	os << "Senesced meal: " << senesced.meal << endl;
//	os << "Dead shell: " << dead.shell << endl;
//	os << "Dead meal: " << dead.meal << endl << endl;
	os << endl;
}


//===========================================================================
float PlantFruit::calcCover (float canopy_fac)
//===========================================================================
{

//+  Purpose
//     Calculate pod cover

//+  Changes
//     02 Feb 2005 JNGH - Programmed and Specified

//+  Local Variables
    float cover;                // pod cover in canopy
    float coverA;

//- Implementation Section ----------------------------------
    if (gPai > 0.0)
    {
        coverA = 1.0 - exp(-cExtinctionCoeffPod * gPai*canopy_fac);
        cover = divide (coverA, canopy_fac, 0.0);
    }
    else
        cover = 0.0;

    coverPod.green = cover;
    return cover;
}

//===========================================================================
void PlantFruit::processBioDemand(void)
//===========================================================================
{

    bio_water1 ();
    yieldpart_demand_stress1 ();
    bio_grain_oil ();
    bio_grain_demand ();

    return;
}

float PlantFruit::grainEnergy(void) const {return gGrain_energy;}
float PlantFruit::dltDmGrainDemand(void) const {return gDlt_dm_grain_demand;}

//===========================================================================
void PlantFruit::calcDlt_pod_area (void)
//===========================================================================
{
        gDlt_pai = podPart->dlt.dm_green * cSpec_pod_area * smm2sm;  //FIXME when these data members are put in
}

//===========================================================================
float PlantFruit::dltDmRetranslocate(void)
//===========================================================================
{
  dlt.dm_green_retrans = 0.0;
  for (vector<plantPart *>::iterator t = myParts.begin();
       t != myParts.end();
       t++)
     dlt.dm_green_retrans += (*t)->dlt.dm_green_retrans;
  return dlt.dm_green_retrans;
}

//===========================================================================
float PlantFruit::interceptRadiation (float radiation)    // incident radiation on pods
//===========================================================================
{

//+  Purpose
//     Calculate pod total radiation interception and return transmitted radiation

//+  Changes
//     02 Feb 2005 JNGH - Programmed and Specified


//- Implementation Section ----------------------------------

   float radiationIntercepted = coverTotal() * radiation;
   return radiation - radiationIntercepted;
}

//===========================================================================
void PlantFruit::dm_pot_rue (double  radn_int_pod
                           , photosynthetic_pathway_t c_photosynthetic_pathway)                    // (OUTPUT) potential dry matter (carbohydrate) production (g/m^2)
//===========================================================================
{
//+  Purpose
//       Potential biomass (carbohydrate) production from
//       photosynthesis (g/m^2).  The effect of factors such
//       temperature and nutritional status of the plant are
//       taken into account in the radiation use efficiency.

//+  Mission Statement
//     Get the potential biomass production - limited by stress factors

//+  Changes
//       181197 nih specified and programmed
//+  Local Variables
  double podfr;                                  // fraction of intercepted light intercepted by pods
  double rue_leaf;
  float co2_modifier = 0.0;

//- Implementation Section ----------------------------------

  float meanT = 0.5*(gMaxt + gMint);
  rue_co2_modifier(plant->getPhotosynthetic_pathway()
                 , plant->getCo2()
                 , meanT
                 , &co2_modifier);
  double stress_factor = min(min(min(plant->getTempStressPhoto(), plant->getNfactPhoto())
                                  , plant->getOxdefPhoto()), plant->getPfactPhoto());

  gDlt_dm_pot_rue_pod = (radn_int_pod * cRue_pod) * stress_factor * co2_modifier;
  }


//==========================================================================
void PlantFruit::rue_co2_modifier(photosynthetic_pathway_t photosyntheticType // Photosynthetic pathway
                                , float co2                                   // CO2 level (ppm)
                                , float meanT                                 // daily mean temp (oC)
                                , float *modifier)                            // modifier (-)
//==========================================================================
{
//  Purpose
//     Calculation of the CO2 modification on rue

//     References
//     Reyenga, Howden, Meinke, Mckeon (1999), Modelling global change impact on wheat cropping in
//              south-east Queensland, Australia. Enivironmental Modelling & Software 14:297-306


//  Purpose
//     Calculation of the CO2 modification on rue

//  Changes
//     20000717   ew programmed

//  Local Variables
      float TT;               // co2 compensation point (ppm)
      float dividend;         // Temp vars for passing composite arg to a func
      float divisor;          // expecting a pointer

// Implementation Section ----------------------------------

   switch (photosyntheticType)
      {
      pw_C3:
        {
        TT  = divide(163.0 - meanT, 5.0 - 0.1 * meanT, 0.0);

        dividend = (co2 - TT) * (350.0 + 2.0 * TT);
        divisor = (co2 + 2.0 * TT)*(350.0 - TT);
        *modifier = divide( dividend, divisor, 1.0);
        break;
        }
      pw_C4:
        {
        *modifier = 0.000143 * co2 + 0.95; //Mark Howden, personal communication
        break;
        }
      default:
        {
        throw std::invalid_argument ("Unknown photosynthetic pathway in fruit rue_co2_modifier()");
        }
      }
}

//==========================================================================
void PlantFruit::transp_eff_co2()          // (OUTPUT) transpiration coefficient
//==========================================================================
{
   cproc_transp_eff_co2(cSvp_fract
                      , cTransp_eff_cf[(int)phenology->stageNumber()-1]
                      , gMaxt
                      , gMint
                      , plant->getCo2()
                      , cX_co2_te_modifier
                      , cY_co2_te_modifier
                      , cNum_co2_te_modifier
                      , &gTranspEff);
}

//===========================================================================
void PlantFruit::sw_demand1(float *sw_demand)         //(OUTPUT) crop water demand (mm)
//===========================================================================
/*  Purpose
*       Return crop water demand from soil by the crop (mm) calculated by
*       dividing biomass production limited by radiation by transpiration efficiency.
*
*  Mission Statement
*   Calculate the crop demand for soil water (based upon transpiration efficiency)
*
*  Changes
*       21/5/2003 ad converted to BC++
*       010994 jngh specified and programmed
*       970216 slw generalised
*
*/
   {
   // get potential transpiration from potential
   // carbohydrate production and transpiration efficiency
        cproc_sw_demand1 (gDlt_dm_pot_rue_pod
                         , gTranspEff
                         , sw_demand);
   }

//===========================================================================
void PlantFruit::bio_water1 (void)  //(OUTPUT) potential dry matter production
                                                    //         by transpiration (g/m^2)
//===========================================================================
//  Purpose
//   Calculate the potential biomass production based upon today's water supply.

//  Mission Statement
//   Calculate the potential biomass production based upon today's water supply.

//  Changes
//       090994 jngh specified and programmed

{
   // Implementation Section ----------------------------------

   // potential (supply) by transpiration

   gDlt_dm_pot_te= plant->getWaterSupplyPod() * gTranspEff;

       // Capping of sw demand will create an effective TE- recalculate it here       //FIXME
       // In an ideal world this should NOT be changed here - NIH
//       g.transp_eff = g.transp_eff * divide(g.sw_demand_te,g.sw_demand, 1.0);
//       g.swDemandTEFruit = g.swDemandTEFruit * divide(g.sw_demand,g.sw_demand_te, 1.0);  // Hack to correct TE for fruit
}

//===========================================================================
void PlantFruit::bio_grain_oil (void)                                // for seed energy content (>= 1.0)
//===========================================================================
{
   //+  Purpose
//       Calculate grain oil factors

//+  Changes
//      141100 jngh specified and programmed

//- Implementation Section ----------------------------------

    gGrain_energy = 1.0 + cGrain_oil_conc * (cCarbo_oil_conv_ratio - 1.0);
    bound_check_real_var (parentPlant, gGrain_energy, 1.0, 2.0, "grain_energy");
}

//===========================================================================
void PlantFruit::bio_grain_demand (void)
//===========================================================================
{
//+  Purpose
//       Simulate crop grain biomass demand.

//+  Mission Statement
//     Calculate grain biomass demand

//+  Changes
//      250894 jngh specified and programmed

//- Implementation Section ----------------------------------

    if (cGrain_fill_option == 1)
       {
        bio_yieldpart_demand1();
        }
    else if (cGrain_fill_option == 2)
        {
        if (phenology->inPhase("grainfill"))
           legnew_bio_yieldpart_demand2(gGrain_no,
                                        pPotential_grain_filling_rate,
                                        gMaxt,
                                        gMint,
                                        cX_temp_grainfill,
                                        cY_rel_grainfill,
                                        cNum_temp_grainfill,
                                        &gDlt_dm_grain_demand);
        else
           gDlt_dm_grain_demand = 0.0;
        }
    else
        {
        throw std::invalid_argument("invalid template option in plant_bio_grain_demand");
        }

    return;
}

//===========================================================================
void PlantFruit::bio_actual (void)
//===========================================================================
{
//+  Purpose
//       Takes the minimum of biomass production limited by radiation and
//       biomass production limited by water.

//+  Mission Statement
//     Takes the minimum of biomass production limited by radiation and
//     biomass production limited by water.

//+  Changes
//      250894 jngh specified and programmed

//- Implementation Section ----------------------------------

        // use whichever is limiting
        gDlt_dm = min (gDlt_dm_pot_rue_pod, gDlt_dm_pot_te);   //FIXME when these data members are put in

}

//===========================================================================
void PlantFruit::bio_yieldpart_demand1(void)
//===========================================================================
{
//+  Purpose
//        Find grain demand for carbohydrate using harvest index (g/m^2)

//+  Mission Statement
//   Calculate yield component biomass demand using harvest index increments

//+  Changes
//     010994 jngh specified and programmed

//+  Local Variables
//    float ave_stress;                             // average dm_stress from flowering to gra
    float dlt_dm_yield;                           // grain demand for carbohydrate (g/m^2)
    float dlt_dm_yield_unadj;                     // grain demand for carbohydrate, unadjusted
// for energy conversion (g/m^2)
    float harvest_index;                          // last harvest index (g grain/g biomass)
    float hi_max_pot;                             // max potential HI due to stress
    float dm_tops_new;                            // new drymatter  tops (g/m^2)
    float harvest_index_new;                      // next harvest index (g grain/g biomass)
    float dm_grain_new;                           // new drymatter grain (g/m^2)
    float energy_adjust;                          // adjustment for energy used in oil conversion.
    int   indx;                                   // loop index
    float hi_incr;                                // harvest index increment per day
    float photoperiod;                            // hours of photosynthetic light (hours)

//- Implementation Section ----------------------------------

    if (phenology->inPhase("grainfill"))
        {

//        ave_stress = divide ((*g_dm_stress_max).getSum(),
//                             (*g_dm_stress_max).getN(),
//                             1.0);
        hi_max_pot = linear_interp_real(gDm_stress_max.getAverage()
                                        ,pX_hi_max_pot_stress
                                        ,pY_hi_max_pot
                                        ,pNum_hi_max_pot);

        photoperiod = day_length (gDay_of_year, gLatitude, cTwilight);

        hi_incr = linear_interp_real(photoperiod
                                    ,pX_pp_hi_incr
                                    ,pY_hi_incr
                                    ,pNum_pp_hi_incr);

        // effective grain filling period
        float dm_green_yield_parts = mealPart->g.dm_green + oilPart->g.dm_green;

        harvest_index = divide (dm_green_yield_parts, plant->getDmTops(), 0.0);
        dm_tops_new = plant->getDmTops() + plant->getDltDm();

        harvest_index_new = u_bound (harvest_index + hi_incr, hi_max_pot);

        dm_grain_new = dm_tops_new * harvest_index_new;
        dlt_dm_yield_unadj = dm_grain_new - dm_green_yield_parts;

    // adjust for grain energy

        dlt_dm_yield_unadj = bound (dlt_dm_yield_unadj, 0.0, dm_grain_new);

        energy_adjust = divide (gGrain_energy
                                , 1.0 + harvest_index_new*(gGrain_energy - 1.0)
                                , 0.0);

        dlt_dm_yield = dlt_dm_yield_unadj * energy_adjust;
    //jh         dlt_dm_yield = dlt_dm_yield_unadj

        if (gMint <= pMinTempGrnFill)
        {
            gDelayGrnFill = true;
        }
        if (gDelayGrnFill)
        {
            dlt_dm_yield = 0.0;
            gDaysDelayedGrnFill = gDaysDelayedGrnFill + 1;
            if (gDaysDelayedGrnFill == pDaysDelayGrnFill)
            {
                  gDelayGrnFill = false ;
                  gDaysDelayedGrnFill = 0;
            }
        }
        }
    else
        {
        // we are out of grain fill period
        dlt_dm_yield = 0.0;
        }
//        ostrstream msg;
//       msg << g_mint << gDelayGrnFill << gDaysDelayedGrnFill << endl;
//       parent->writeString (msg.str());


//    *dlt_dm_grain_demand = dlt_dm_yield;
    gDlt_dm_grain_demand = dlt_dm_yield;

    return;
    }

//===========================================================================
void PlantFruit::grain_n_demand1(float g_nfact_grain_conc      //   (INPUT)
                               , float *g_n_conc_crit          //   (INPUT)  critical N concentration (g N/
                               , float g_swdef_expansion       //   (INPUT)
                               , float *g_n_conc_min           //   (INPUT)  minimum N concentration (g N/g
                               , float *g_n_conc_max           //   (INPUT)  maximum N concentration (g N/g
                               , float *g_n_green              //   (INPUT)  plant nitrogen content (g N/m^
                               , float *grain_n_demand)        //   grain N demand (g/m^2)
//===========================================================================
  {
//  Purpose
//    Calculate plant n demand

      float   n_avail[max_part];     // N available for transfer to grain
                                     // (g/m^2)
      float   n_avail_stover;        // total N available in stover
                                     // (g/m^2)
      float   n_potential;           // maximum grain N demand (g/m^2)
      int     part;                  // plant part number

//- Implementation Section ----------------------------------

      gN_grain_demand = (mealPart->dlt.dm_green + mealPart->dlt.dm_green_retrans)
             * crop_n_dlt_grain_conc(meal,
                            cSfac_slope
                          , cSw_fac_max
                          , cTemp_fac_min
                          , cTfac_slope
                          , gMaxt
                          , gMint
                          , g_nfact_grain_conc
                          , g_n_conc_crit
                          , g_n_conc_min
                          , g_swdef_expansion);


      n_potential  = (mealPart->g.dm_green
                     + mealPart->dlt.dm_green
                     + mealPart->dlt.dm_green_retrans)
               * g_n_conc_max[meal];


      gN_grain_demand = u_bound (gN_grain_demand
                                , n_potential - mealPart->g.n_green);
      *grain_n_demand = gN_grain_demand;

   }

//===========================================================================
float PlantFruit::dm_yield_demand ( float  g_dlt_dm_veg_supply           // (INPUT)  the daily vegetative biomass production (g/m^2)
                                  , double g_dlt_dm_supply_pod           // (INPUT)  the daily biomass production of pod (g/m^2)
                                  )
//===========================================================================
{

//+  Purpose
//       Calculate grain dm yield demand (g/m^2)
//       (OUTPUT) assimilate demand for reproductive part (g/m^2)
//+  Changes
//      170703 jngh specified and programmed

//+  Local Variables
      float      dm_grain_demand;       // assimilate demand for grain (g/m^2)
      float      dm_pod_demand;         // assimilate demand for pod (g/m^2)
      float      dm_yield_demand;       // (OUTPUT) assimilate demand for reproductive part (g/m^2)

//- Implementation Section ----------------------------------

         // calculate demands of reproductive parts

      float cFracPod = cFrac_pod[(int)phenology->stageNumber()-1];

      dm_grain_demand = divide (gDlt_dm_grain_demand, gGrain_energy, 0.0);

      if (dm_grain_demand > 0.0)
      {
         dm_pod_demand = dm_grain_demand * cFracPod;
      }
      else
      {
         dm_pod_demand = g_dlt_dm_veg_supply * cFracPod;        // fix
      }

      dm_yield_demand = dm_pod_demand
                      + gDlt_dm_grain_demand
                      - g_dlt_dm_supply_pod;

      return dm_yield_demand;
}

//===========================================================================
float PlantFruit::dm_yield_demand2 ( float  g_dlt_dm_veg_supply           // (INPUT)  the daily vegetative biomass production (g/m^2)
                                  , double g_dlt_dm_supply_pod           // (INPUT)  the daily biomass production of pod (g/m^2)
                                  )
//===========================================================================
{

//+  Purpose
//       Calculate grain dm yield demand (g/m^2)
//       (OUTPUT) assimilate demand for reproductive part (g/m^2)
//+  Changes
//      170703 jngh specified and programmed

//+  Local Variables
      float      dm_grain_demand;       // assimilate demand for grain (g/m^2)
      float      dm_pod_demand;         // assimilate demand for pod (g/m^2)
      float      dm_yield_demand;       // (OUTPUT) assimilate demand for reproductive part (g/m^2)

//- Implementation Section ----------------------------------

         // calculate demands of reproductive parts

      int g_current_stage = (int)phenology->stageNumber();
      float fracPod = linear_interp_real(g_current_stage
                                  ,cX_stage_no_partition
                                  ,cY_frac_pod
                                  ,cNum_stage_no_partition);

      dm_grain_demand = divide (gDlt_dm_grain_demand, gGrain_energy, 0.0);

      if (dm_grain_demand > 0.0)
      {
         dm_pod_demand = dm_grain_demand * fracPod;
      }
      else
      {
         dm_pod_demand = g_dlt_dm_veg_supply * fracPod;        // fix
      }

      dm_yield_demand = dm_pod_demand
                      + gDlt_dm_grain_demand
                      - g_dlt_dm_supply_pod;

      return dm_yield_demand;
}

//     ===========================================================
void PlantFruit::dm_partition1 (double g_dlt_dm                      // (INPUT)  the daily biomass production (
                               ,float  *dlt_dm_oil_conv               // (OUTPUT) actual biomass used in conversion to oil (g/m2)
                               ,float  *dlt_dm_green                  // (OUTPUT) actual biomass partitioned to plant parts (g/m^2)
                               )
//     ===========================================================
{

//+  Purpose
//       Partitions new dm (assimilate) between plant components (g/m^2)

//+  Changes
//      170703 jngh specified and programmed

//+  Local Variables
    double dlt_dm_green_tot;                       // total of partitioned dm (g/m^2)
    double dm_remaining;                           // interim dm pool for partitioning
    double yield_demand;                           // sum of grain, energy & pod
    double dm_grain_demand;                        // assimilate demand for grain (g/m^2)
    double dm_meal_demand;                         // assimilate demand for meal (g/m^2)
    double dm_oil_demand;                          // assimilate demand for oil (g/m^2)
    double dm_oil_conv_demand;                     // assimilate demand for conversion to oil (g/m^2)
    double dm_pod_demand;                          // assimilate demand for pod (g/m^2)

//- Implementation Section ----------------------------------

    float fracPod = cFrac_pod[(int)phenology->stageNumber()-1];
         // first we zero all plant component deltas
    fill_real_array (dlt_dm_green, 0.0, max_part);

    // calculate demands of reproductive parts
    dm_grain_demand = divide (gDlt_dm_grain_demand, gGrain_energy, 0.0);

    dm_meal_demand = dm_grain_demand * (1.0 - cGrain_oil_conc);
    dm_oil_demand = dm_grain_demand - dm_meal_demand;
    dm_oil_conv_demand = gDlt_dm_grain_demand - dm_grain_demand;

    if (dm_grain_demand > 0.0)
    {
        dm_pod_demand = dm_grain_demand * fracPod;
    }
    else
    {
        dm_pod_demand = g_dlt_dm;
    }
    yield_demand = dm_pod_demand
                 + dm_meal_demand
                 + dm_oil_demand
                 + dm_oil_conv_demand;

         // now distribute the assimilate to plant parts
    if (yield_demand >= g_dlt_dm)
            // reproductive demand exceeds supply - distribute assimilate to those parts only
    {
            // reproductive demand exceeds supply - distribute assimilate to those parts only
        mealPart->dlt.dm_green = g_dlt_dm * divide (dm_meal_demand, yield_demand, 0.0);
        oilPart->dlt.dm_green  = g_dlt_dm * divide (dm_oil_demand, yield_demand, 0.0);
        gDlt_dm_oil_conv   = g_dlt_dm * divide (dm_oil_conv_demand, yield_demand, 0.0);
        podPart->dlt.dm_green = g_dlt_dm
                             - mealPart->dlt.dm_green
                             - oilPart->dlt.dm_green
                             - gDlt_dm_oil_conv;
    }
    else
    {
        // more assimilate than needed for reproductive parts
        // distribute to all parts

        // satisfy reproductive demands
        mealPart->dlt.dm_green   = dm_meal_demand;
        oilPart->dlt.dm_green    = dm_oil_demand;
        gDlt_dm_oil_conv     = dm_oil_conv_demand;
        podPart->dlt.dm_green    = dm_pod_demand;

//        // distribute remainder to vegetative parts
//        dm_remaining = g_dlt_dm - yield_demand;
    }

     dlt_dm_green[meal]   = mealPart->dlt.dm_green;
     dlt_dm_green[oil]    = oilPart->dlt.dm_green;
     *dlt_dm_oil_conv     = gDlt_dm_oil_conv ;
     dlt_dm_green[pod]    = podPart->dlt.dm_green;

    // do mass balance check - roots are not included
    dlt_dm_green_tot = sum_real_array (dlt_dm_green, max_part)
                     + gDlt_dm_oil_conv;

    if (!reals_are_equal(dlt_dm_green_tot, g_dlt_dm, 1.0E-4))  // XX this is probably too much slop - try doubles XX
    {
         string msg = "Fruit dlt_dm_green_tot mass balance is off: "
                    + ftoa(dlt_dm_green_tot, ".6")
                    + " vs "
                    + ftoa(g_dlt_dm, ".6");
         parentPlant->warningError(msg.c_str());
    }

      // check that deltas are in legal range
    bound_check_real_array (parentPlant, dlt_dm_green, max_part, 0.0, g_dlt_dm, "Fruit dlt_dm_green");

}

//     ===========================================================
void PlantFruit::yieldpart_demand_stress1 (void)
//     ===========================================================
{
//+  Purpose
//       Simulate crop grain biomass demand stress factor

//- Implementation Section ----------------------------------

   cproc_yieldpart_demand_stress1 (min(plant->getNfactPhoto(), plant->getPfactPhoto())
                                 , plant->getSwdefPhoto()
                                 , plant->getTempStressPhoto()
                                 , &gDlt_dm_stress_max);
//   gDlt_dm_stress_max  = dlt_dm_stress_max;
}


//     ===========================================================
void PlantFruit::dm_retranslocate1( float  g_dlt_dm_retrans_to_fruit     // (INPUT)
                                  , float  *g_dm_plant_min               // (INPUT)  minimum weight of each plant p
                                  , float  *dm_oil_conv_retranslocate    // (OUTPUT) assimilate used for oil conversion - energy (g/m^2)
                                  , float  *dm_retranslocate             // (OUTPUT) actual change in plant part weights due to translocation (g/m^2)
                                  )
//     ===========================================================
{

//+  Purpose
//     Calculate plant dry matter delta's due to retranslocation
//     to grain, pod and energy (g/m^2)

//+  Mission Statement
//   Calculate biomass retranslocation to the yield component

//+  Changes
//       150900 jngh specified and programmed

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

// now translocate carbohydrate between plant components
// this is different for each stage

//+  Constant Values
    const int  num_supply_pools_by_fruit = 1 ;
    int   supply_pools[num_supply_pools_by_fruit] = {pod};

    float fracPod = cFrac_pod[(int) phenology->stageNumber()-1];

     for (vector<plantPart *>::iterator t = myParts.begin();      //FIXME later
          t != myParts.end();
          t++)
        (*t)->dlt.dm_green_retrans = 0.0;

    dlt_dm_grain = mealPart->dlt.dm_green
                 + oilPart->dlt.dm_green
                 + gDlt_dm_oil_conv;

    if (gDlt_dm_grain_demand > dlt_dm_grain)
    {
            // we can translocate source carbohydrate
            // to reproductive parts if needed

            // calculate demands for each reproductive part

        dm_demand_differential          = gDlt_dm_grain_demand - dlt_dm_grain;
        dm_grain_demand_differential    = divide (dm_demand_differential, gGrain_energy, 0.0);
        dm_meal_demand_differential     = dm_grain_demand_differential * (1.0 - cGrain_oil_conc);
        dm_oil_demand_differential      = dm_grain_demand_differential - dm_meal_demand_differential;
        dm_oil_conv_demand_differential = dm_demand_differential - dm_grain_demand_differential;
        dm_pod_demand_differential      = dm_grain_demand_differential * fracPod;

        yield_demand_differential  = dm_pod_demand_differential
                                   + dm_meal_demand_differential
                                   + dm_oil_demand_differential
                                   + dm_oil_conv_demand_differential;

        demand_differential = yield_demand_differential - g_dlt_dm_retrans_to_fruit;

            // get available carbohydrate from supply pools
     counter = 0;
     for (vector<plantPart *>::iterator fPart = supplyPools.begin();      //FIXME later
          fPart != supplyPools.end();
          fPart++)
        {
           part = supply_pools[counter];
           dm_part_pot = (*fPart)->g.dm_green + (*fPart)->dlt.dm_green_retrans;
           dm_part_avail = dm_part_pot
                         - g_dm_plant_min[part] * plant->getPlants();
//           dm_part_avail = dm_part_pot
//                         - (*fPart)->g.dm_plant_min * plant->getPlants();       //FIXME something wrong with dm_plant_min
           dm_part_avail = l_bound (dm_part_avail, 0.0);

           dlt_dm_retrans_part = min (demand_differential, dm_part_avail);
           (*fPart)->dlt.dm_green_retrans = - dlt_dm_retrans_part;

           demand_differential = demand_differential - dlt_dm_retrans_part;
           counter++;
        }

        dlt_dm_retrans_total = -1.0 * dltDmRetranslocate() + g_dlt_dm_retrans_to_fruit;

            // now distribute retranslocate to demand sinks.

        if (yield_demand_differential > dlt_dm_retrans_total)
        {
            mealPart->dlt.dm_green_retrans = dlt_dm_retrans_total
                                   * divide (dm_meal_demand_differential, yield_demand_differential, 0.0);
            oilPart->dlt.dm_green_retrans = dlt_dm_retrans_total
                                  * divide (dm_oil_demand_differential, yield_demand_differential, 0.0);
            dmOil_conv_retranslocate = dlt_dm_retrans_total
                                       * divide (dm_oil_conv_demand_differential, yield_demand_differential, 0.0);
            podPart->dlt.dm_green_retrans = dlt_dm_retrans_total
                                  * divide (dm_pod_demand_differential, yield_demand_differential, 0.0)
                                  + podPart->dlt.dm_green_retrans;
        }
        else
        {

            mealPart->dlt.dm_green_retrans     = dm_meal_demand_differential;
            oilPart->dlt.dm_green_retrans      = dm_oil_demand_differential;
            dmOil_conv_retranslocate           = dm_oil_conv_demand_differential;
            podPart->dlt.dm_green_retrans      = dm_pod_demand_differential
                                  + podPart->dlt.dm_green_retrans;
        }

            // ??? check that stem and leaf are >= min wts
    }
    else
    {
            // we have no retranslocation
     for (vector<plantPart *>::iterator t = myParts.begin();      //FIXME later
          t != myParts.end();
          t++)
        (*t)->dlt.dm_green_retrans = 0.0;
        dmOil_conv_retranslocate = 0.0;
    }

    *dm_oil_conv_retranslocate = dmOil_conv_retranslocate;
     part = pod;
     for (vector<plantPart *>::iterator t = myParts.begin();      //FIXME later
          t != myParts.end();
          t++)
     {
        dm_retranslocate[part] = (*t)->dlt.dm_green_retrans;
        part++;
     }

    // now check that we have mass balance
    if (!reals_are_equal(-1.0 * (dltDmRetranslocate() - g_dlt_dm_retrans_to_fruit), dmOil_conv_retranslocate))
    {
      string msg = "dm_retranslocate mass balance of fruit is off: "
                 + ftoa(dltDmRetranslocate() - g_dlt_dm_retrans_to_fruit, ".6")
                 + " vs "
                 + ftoa(dmOil_conv_retranslocate, ".6");

      dmOil_conv_retranslocate = *dm_oil_conv_retranslocate;

      parentPlant->warningError(msg.c_str());
    }
}

//============================================================================
void PlantFruit::doSenescence1 (float sen_fr)       // (OUTPUT) actual biomass senesced from plant parts (g/m^2)
//============================================================================
{
// Purpose
//   Derives seneseced plant dry matter (g/m^2) for the day

// Implementation Section ----------------------------------

   dlt.dm_senesced = 0.0;
   for (vector<plantPart *>::iterator t = myParts.begin();
       t != myParts.end();
       t++)
   {
     (*t)->doSenescence1(sen_fr);
      dlt.dm_senesced += (*t)->dlt.dm_senesced;
   }
}

//============================================================================
void PlantFruit::doSenescence2 (float sen_fr)       // (OUTPUT) actual biomass senesced from plant parts (g/m^2)
//============================================================================
{
// Purpose
//   Derives seneseced plant dry matter (g/m^2) for the day

// Implementation Section ----------------------------------

   dlt.dm_senesced = 0.0;
   for (vector<plantPart *>::iterator t = myParts.begin();
       t != myParts.end();
       t++)
   {
     (*t)->doSenescence2(sen_fr);
      dlt.dm_senesced += (*t)->dlt.dm_senesced;
   }
}

//============================================================================
void PlantFruit::retrans_init (float *dm_plant_min                          // (OUTPUT) minimum weight of each plant part (g/plant)
                             )
//============================================================================
{
//+  Purpose
//       Initialise pod weight minimum
//       at required instances.

//+  Mission Statement
//     Initialise pod weight minimums at required instances.

//+  Changes
//     010994 jngh specified and programmed

//+  Local Variables
    float dm_plant_pod;                           // dry matter in pods (g/plant)

//- Implementation Section ----------------------------------

    // initialise pod weight minimum
    dm_plant_pod = divide (podPart->g.dm_green, plant->getPlants(), 0.0);
    podPart->g.dm_plant_min = max (dm_plant_pod * (1.0 - cPod_trans_frac), podPart->g.dm_plant_min);
    dm_plant_min[pod] = podPart->g.dm_plant_min;

}
  // FIXME not used yet
//============================================================================
void PlantFruit::n_senescence1 (float  *dlt_n_senesced_trans  // (OUTPUT)  plant N senescence (g/m^2)
                             , float  *dlt_n_senesced        // (OUTPUT) actual nitrogen senesced from plant parts (g/m^2)
                             )
//============================================================================
{

//+  Purpose
//       Derives seneseced plant nitrogen (g N/m^2)

//+  Mission Statement
//   Calculate change in senesced plant Nitrogen

//+  Changes
//       121297 nih specified and programmed

//- Implementation Section ----------------------------------

     int part = pod;
     for (vector<plantPart *>::iterator t = myParts.begin();
          t != myParts.end();
          t++)
    {
       float green_n_conc = divide ((*t)->g.n_green, (*t)->g.dm_green, 0.0);  // N conc of green material (g/g)
       float sen_n_conc = min ((*t)->c.n_sen_conc, green_n_conc);             // N conc of senescing material (g/g)

       (*t)->dlt.n_senesced = (*t)->dlt.dm_senesced * sen_n_conc;
       (*t)->dlt.n_senesced = u_bound ((*t)->dlt.n_senesced, (*t)->g.n_green);
       dlt_n_senesced[part] = (*t)->dlt.n_senesced;

       float dlt_n_in_senescing_part = (*t)->dlt.dm_senesced * green_n_conc;
       (*t)->dlt.n_senesced_trans = dlt_n_in_senescing_part - (*t)->dlt.n_senesced;
       (*t)->dlt.n_senesced_trans = l_bound((*t)->dlt.n_senesced_trans, 0.0);
       dlt_n_senesced_trans[part] = (*t)->dlt.n_senesced_trans;
       part++;
    }
}

//============================================================================
void PlantFruit::nit_init (void)
//============================================================================
{
//+  Purpose
//       Initialise plant nitrogen.

//+  Mission Statement
//     Initialise plant nitrogen

//+  Changes
//      250894 jngh specified and programmed

//- Implementation Section ----------------------------------


//   if (phenology->inPhase("grainfill"))            //FIXME when these data members are put in
//      n_conc_grain_limits(gN_conc_crit
//                        , gN_conc_max
//                        , gN_conc_min);

}

//============================================================================
void PlantFruit::n_conc_grain_limits ( float  *n_conc_crit                    // (OUTPUT) critical N concentration (g N/g part)
                                    , float  *n_conc_max                     // (OUTPUT) maximum N concentration (g N/g part)
                                    , float  *n_conc_min                     // (OUTPUT) minimum N concentration (g N/g part)
                                          )
//============================================================================
{
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

//+  Local Variables
    float dm_oil;                                 // oil mass (g/m2)
    float dm_meal;                                // meal mass (g/m2)
    float dm_grain;                               // grain mass (g/m2)
    float n_crit_grain;                           // critial mass of grain N (g/m2)
    float n_max_grain;                            // maximum mass of grain N (g/m2)
    float n_min_grain;                            // minimum mass of grain N (g/m2)

//- Implementation Section ----------------------------------
    if (phenology->inPhase ("grainfill"))
        {
        dm_oil = oilPart->g.dm_green
               + oilPart->dlt.dm_green
               + oilPart->dlt.dm_green_retrans;
        dm_meal = mealPart->g.dm_green
               + mealPart->dlt.dm_green
               + mealPart->dlt.dm_green_retrans;
        dm_grain = dm_oil + dm_meal;

        n_crit_grain = cN_conc_crit_grain * dm_grain;
        n_max_grain = cN_conc_max_grain * dm_grain;
        n_min_grain = cN_conc_min_grain * dm_grain;

        n_conc_crit[meal] = divide (n_crit_grain, dm_meal, 0.0);
        n_conc_max[meal] = divide (n_max_grain, dm_meal, 0.0);
        n_conc_min[meal] = divide (n_min_grain, dm_meal, 0.0);
        }
    }

//============================================================================
void PlantFruit::n_retranslocate( float  *G_N_conc_min               // (INPUT)  minimum N concentration (g N/g
                                , float  *G_dm_green                 // (INPUT)  live plant dry weight (biomass
                                , float  *G_N_green                  // (INPUT)  plant nitrogen content (g N/m^
                                , float  G_grain_N_demand            //  INPUT
                                , float  *dlt_N_retrans              // (OUTPUT) plant N taken out from plant parts (g N/m^2)
                                )
//============================================================================
{
//+  Purpose
//     Calculate the nitrogen retranslocation from the various plant parts
//     to the grain.

//+  Mission Statement
//     Calculate N retranslocation from various plant parts to grain

//+  Changes
//      170703 jngh specified and programmed

//+  Local Variables
      float N_avail[max_part];     // N available for transfer to grain (g/m^2)
      float N_avail_rep;           // total N available in reproductive parts (g/m^2)

//- Implementation Section ----------------------------------
    float N_avail_stover = 0.0;
         // Get Grain N supply in this cohort
    vector<plantPart *>::iterator part;
    for (part = supplyPools.begin(); part != supplyPools.end(); part++)
        N_avail_stover += (*part)->availableRetranslocateN();  // grain N potential (supply) from pod

//      N_retrans_avail (meal
//                     , G_N_conc_min
//                     , G_dm_green
//                     , G_N_green
//                     , N_avail
//                     ) ;  // grain N potential (supply)

            // available N does not include grain
            // this should not presume grain is 0.

      N_avail_rep  =  sum_real_array (N_avail, max_part);

          // get actual grain N uptake by retransolcation
          // limit retranslocation to total available N

      fill_real_array (dlt_N_retrans, 0.0, max_part);

      if (gN_grain_demand >= N_avail_rep)
      {

             // demand greater than or equal to supply
             // retranslocate all available N

         podPart->dlt.n_retrans = - N_avail[pod];
         mealPart->dlt.n_retrans = N_avail_rep;
      }
      else
      {
             // supply greater than demand.
             // Retranslocate what is needed

         podPart->dlt.n_retrans = - gN_grain_demand
                                     * divide (N_avail[pod], N_avail_rep, 0.0);

         mealPart->dlt.n_retrans = gN_grain_demand;

      }
      dlt_N_retrans[pod] = podPart->dlt.n_retrans;
      dlt_N_retrans[meal] = mealPart->dlt.n_retrans;
}

//===========================================================================
void PlantFruit::N_retrans_avail(const int meal
                               , float *g_N_conc_min
                               , float *g_dm_green
                               , float *g_N_green
                               , float *N_avail)
//===========================================================================

//  Purpose
//    Calculate N available for transfer to grain (g/m^2)
//    from each fruit part.  By definition, available grain N
//    is set to 0.

// Mission Statement
//  Calculate the Nitrogen available for retranslocation to grain

// Notes
//    N available for translocation to the grain is the sum of
//    N available in the stover.
//    N available in stover is the difference of its N content and the minimum it's allowed to fall to.
//    NB. No translocation from roots.

// Changes
//      080994 jngh specified and programmed


{
   float N_min;                  // nitrogen minimum level (g/m^2)

      // get grain N potential (supply) -----------
      // now find the available N of each part.
   fill_real_array (N_avail, 0.0, max_part);
//   for(int part = 0; part < max_part; part++)      // FIXME when fruit becomes proper class
   for(int part = pod; part <= pod; part++)
   {
      N_min = g_N_conc_min[part] * g_dm_green[part];
      N_avail[part] = l_bound (g_N_green[part] - N_min, 0.0);
   }
   N_avail[meal]  = 0.0;
}

//============================================================================
void PlantFruit::doNDemand1(float dlt_dm             // (INPUT)  Whole plant the daily biomass production (g/m^2)
                          , float dlt_dm_pot_rue)     // (INPUT)  Whole plant potential dry matter production (g/m^2)
//============================================================================
// Purpose
//     Return plant nitrogen demand for each plant component

//  Mission Statement
//     Calculate the Nitrogen demand and maximum uptake for each plant pool

{
    v.n_demand = 0.0;
    v.n_max = 0.0;
    vector<plantPart *>::iterator part;
    for (part = supplyPools.begin(); part != supplyPools.end(); part++)
    {
           (*part)->doNDemand1(dlt_dm, dlt_dm_pot_rue);
           v.n_demand += (*part)->nDemand();
           v.n_max += (*part)->nMax();
    }
}

//============================================================================
void PlantFruit::doNDemand2(float dlt_dm             // (INPUT)  Whole plant the daily biomass production (g/m^2)
                          , float dlt_dm_pot_rue)     // (INPUT)  Whole plant potential dry matter production (g/m^2)
//============================================================================
// Purpose
//     Return plant nitrogen demand for each plant component

//  Mission Statement
//     Calculate the Nitrogen demand and maximum uptake for each plant pool

{
    v.n_demand = 0.0;
    v.n_max = 0.0;
    vector<plantPart *>::iterator part;
    for (part = supplyPools.begin(); part != supplyPools.end(); part++)
    {
           (*part)->doNDemand2(dlt_dm, dlt_dm_pot_rue);
           v.n_demand += (*part)->nDemand();
           v.n_max += (*part)->nMax();
    }
    mealPart->v.n_demand = gN_grain_demand;
}


//============================================================================
void PlantFruit::n_demand(const int max_part,           // (INPUT)
                        int   *demand_parts,          // (INPUT)
                        const int num_demand_parts,   // (INPUT)
                        float G_dlt_dm,           // (INPUT)  the daily biomass production (g/m^2)
                        float *G_dlt_dm_green,        // (INPUT)  plant biomass growth (g/m^2)
                        float G_dlt_dm_pot_rue,       // (INPUT)  potential dry matter production from pods (g/m^2)
                        float *G_dlt_n_retrans,       // (INPUT)  nitrogen retranslocated out from plant parts (g/m^2)
                        float *G_dm_green,            // (INPUT)  live plant dry weight (biomass g/m^2)
                        float *G_n_conc_crit,         // (INPUT)  critical N concentration (g N/g dm)
                        float *G_n_conc_max,          // (INPUT)  maximum N concentration (g N/g dm)
                        float *G_n_green,             // (INPUT)  plant nitrogen content (g N/m^2)
                        float *N_demand,              // (OUTPUT) critical plant nitrogen demand (g/m^2)
                        float *N_max)                 // (OUTPUT) max plant nitrogen demand (g/m^2)
//============================================================================
{

//  Purpose
//      Return plant nitrogen demand for each plant component

// Mission Statement
//  Calculate the Nitrogen demand and maximum uptake for each plant pool

// Notes
//          Nitrogen required for grain growth has already been removed
//          from the stover.  Thus the total N demand is the sum of the
//          demands of the stover and roots.  Stover N demand consists of
//          two components:
//          Firstly, the demand for nitrogen by the potential new growth.
//          Secondly, the demand due to the difference between
//          the actual N concentration and the critical N concentration
//          of the tops (stover), which can be positive or negative

//          NOTE that this routine will not work if the root:shoot ratio
//          is broken. - NIH

// Changes
//    170703 jngh specified and programmed


//  Local Variables
   int counter;
   float N_crit;                 // critical N amount (g/m^2)
   float N_demand_new ;          // demand for N by new growth (g/m^2)
   float N_demand_old;           // demand for N by old biomass (g/m^2)
   float N_potential;            // maximum N uptake potential (g/m^2)
   float N_max_new;              // N required by new growth to reach N_conc_max  (g/m^2)
   float N_max_old;              // N required by old biomass to reach N_conc_max  (g/m^2)
   int part;                     // plant part
   float dlt_dm_pot;             // potential dry weight increase (g/m^2)
   float part_fract;             // plant part fraction of dm  (0-1)

   //- Implementation Section ----------------------------------


   fill_real_array (N_demand, 0.0, max_part);
   fill_real_array (N_max, 0.0, max_part);
   for(counter = 0; counter < num_demand_parts; counter++)
   {
      part = demand_parts[counter];

            // need to calculate dm using potential rue not affected by
            // N and temperature

      part_fract = divide (G_dlt_dm_green[part], G_dlt_dm, 0.0);
      dlt_dm_pot = G_dlt_dm_pot_rue * part_fract;
      dlt_dm_pot = bound (dlt_dm_pot, 0.0, G_dlt_dm_pot_rue);

      if (G_dm_green[part] > 0.0)
      {
            // get N demands due to difference between actual N concentrations
            // and critical N concentrations of tops (stover) and roots.
         N_crit       = G_dm_green[part] * G_n_conc_crit[part];
         N_potential  = G_dm_green[part] * G_n_conc_max[part];

            // retranslocation is -ve for outflows
         N_demand_old = N_crit - (G_n_green[part] + G_dlt_n_retrans[part]);
         N_max_old    = N_potential - (G_n_green[part] + G_dlt_n_retrans[part]);

            // get potential N demand (critical N) of potential growth
         N_demand_new = dlt_dm_pot * G_n_conc_crit[part];
         N_max_new    = dlt_dm_pot * G_n_conc_max[part];

         N_demand[part] = N_demand_old + N_demand_new;
         N_max[part]    = N_max_old    + N_max_new ;

         N_demand[part] = l_bound (N_demand[part], 0.0);
         N_max[part]    = l_bound (N_max[part], 0.0);
      }
      else
      {
         N_demand[part] = 0.0;
         N_max[part]    = 0.0;
      }
   }
      // this routine does not allow excess N in one component to move
      // to another component deficient in N
}

//============================================================================
void PlantFruit::dm_detachment1(void)
//============================================================================
{
    dlt.dm_detached = 0.0;
    dlt.dm_dead_detached = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myParts.begin(); part != myParts.end(); part++)
    {
           (*part)->dm_detachment1();
           dlt.dm_detached += (*part)->dlt.dm_detached;
           dlt.dm_dead_detached += (*part)->dlt.dm_dead_detached;
    }
}

//============================================================================
void PlantFruit::n_detachment1(void)
//============================================================================
{
    dlt.n_detached = 0.0;
    dlt.n_dead_detached = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myParts.begin(); part != myParts.end(); part++)
    {
           (*part)->n_detachment1();
           dlt.n_detached += (*part)->dlt.n_detached;
           dlt.n_dead_detached += (*part)->dlt.n_dead_detached;
    }
}



//============================================================================

#if TEST_PlantFruit							// build unit test?


// PlantFruit class test harness

// Tests default constructor, copy constructor, assignment operator and
// each of the get and set functions.  Does not test the destructor.

// Modification log
// 6 Aug 97  J. Hargreaves    Initial implementation


#ifndef PlantFruit_H
#include "PlantFruit.h"
#endif

int main()
{
	cout << "PlantFruit test started" << endl;

	PlantFruit p, *aPtr = &p;

//	cout << endl << "Test set and get functions:" << endl;
//	p.setValue(10.0, 2.0, 20.0, 3.0, 30.0, 4.0);
//	if (p.total() != 69.0)
//		cout << "setValue(10.0, 2.0, 20.0, 3.0, 30.0, 4.0) / Total() test FAILED"
//		<< endl;
//
//	cout << endl << "Test default constructor:" << endl;
//	PlantFruit q;                           						// run default constructor
//	if (q.total() != 0.0)
//		cout << "default constructor test FAILED" << endl;
//
//	cout << endl << "Test constructor:" << endl;
//	PlantFruit a(1.0, 2.0, 3.0, 4.0, 5.0, 6.0);                           						// run default constructor
//	if (a.total() != 21.0)
//		cout << "constructor test FAILED" << endl;
//
//	cout << endl << "Test copy constructor:" << endl;
//	PlantFruit s = p;                       // run copy constructor
//	if (s.total() != p.total())
//      cout << "copy constructor test FAILED" << endl;
//
//	cout << endl << "Test assignment operator:" << endl;
//	s.setValue(50.0, 5.0, 60.0, 6.0, 70.0, 7.0); // change object
//
//	if (s.total() != p.total())
//	{
//		s = p;                          // run operator=
//		if (s.total() != p.total())
//			cout << "assignment operator test FAILED" << endl;
//	}
//	else
//		cout << "assignment operator test FAILED DIFFERENCE TEST" << endl;
//
//	cout << endl << "Test multiply operator:" << endl;
//	s.setValue(50.0, 5.0, 60.0, 6.0, 70.0, 7.0); // change object
//	PlantFruit k = p * s;
//	if (k.total() != 3856.0)
//		cout << "multiply operator test FAILED" << endl;
//
//	cout << endl << "Test simple multiply operator:" << endl;
//	s.setValue(50.0, 5.0, 60.0, 6.0, 70.0, 7.0); // change object
//	 k = s * 2.0;
//	if (k.total() != 396.0)
//		cout << "simple multiply operator test FAILED" << endl;
//
//	cout << endl << "Test divide operator:" << endl;
//	s.setValue(50.0, 5.0, 60.0, 6.0, 70.0, 7.0); // change object
//	 k = s/p;
//	if (k.total() < 16.58332 || k.total() > 16.58334)
//		cout << "divide operator test FAILED" << endl;
//
//	cout << endl << "Test simple divide operator:" << endl;
//	s.setValue(50.0, 5.0, 60.0, 6.0, 70.0, 7.0); // change object
//	 k = s / 2.0;
//	if (k.total() != 99.0)
//		cout << "simple divide operator test FAILED" << endl;
//
//	PlantFruit t;
//	t.setValue(50.0, 5.0, 60.0, 6.0, 70.0, 7.0); // change object
//	cout << endl << "Display PlantFruit t" << endl;
//	t.display();
//
//	PlantFruit x;
//	x.setValue(50.0, 5.0, 60.0, 6.0, 70.0, 7.0); // change object
//
//	cout << endl << "Display PlantFruit x - static binding" << endl;
//	x.display();
//
//	cout << endl << "Display PlantFruit x - dynamic binding" << endl;
//	PlantFruit *PlantFruitPtr = &x;
//	PlantFruitPtr->display();

	cout << endl << "PlantFruit test finished" << endl;
	return 0;
}

#endif



// Modification log
// 6 Aug 97 J. Hargreaves  Implementation

#include "GrainPart.h"

using namespace std;

static const char* floatType =        "<type kind=\"single\"/>";

inline bool floatsAreEqual(float A, float B, float C) {return(fabs(A-B)<C);}

// default constructor
fruitGrainPart::fruitGrainPart()
{}

//  initialise data members.
fruitGrainPart::fruitGrainPart(plantInterface *p, const string &name) : CompositePart(p, name)
{
   //    zeroAllGlobals();
   gDm_stress_max.setup(&gDlt_dm_stress_max);
   otherObservers.addObserver(&gDm_stress_max);
}

// destructor
fruitGrainPart::~fruitGrainPart()
{
   if (oilPart) delete oilPart;
   if (mealPart) delete mealPart;

}


void fruitGrainPart::doRegistrations(protocol::Component *system)
   //===========================================================================
{
   CompositePart::doRegistrations(system);

   //   setupEvent(system, "tick",        RegistrationType::respondToEvent, &fruitGrainPart::doTick);
   //   setupEvent(system, "newmet",      RegistrationType::respondToEvent, &fruitGrainPart::doNewMet);

   system->addGettableVar("dlt_dm_grain_demand",gDlt_dm_grain_demand, "g/m^2", "??");
   system->addGettableVar("dlt_dm_fruit", gDlt_dm, "g/m^2", "Change in dry matter");
   setupGetFunction(system, "grain_wt", protocol::DTsingle, false, &fruitGrainPart::get_grain_wt, "g/m^2", "Weight of grain");
   setupGetFunction(system, "yield", protocol::DTsingle, false,&fruitGrainPart::get_yield,  "kg/ha", "Yield");
   system->addGettableVar("grain_n_demand", gN_grain_demand, "g/m^2", "N demand of grain");

   setupGetFunction(system, "n_grain_pcnt", protocol::DTsingle, false, &fruitGrainPart::get_n_conc_grain, "%", "N concentration in grain");
   setupGetFunction(system, "n_conc_grain", protocol::DTsingle, false, &fruitGrainPart::get_n_conc_grain, "%", "N concentration in grain");
   setupGetFunction(system, "grain_protein", protocol::DTsingle, false, &fruitGrainPart::get_grain_protein, "%", "grain protein content");
   setupGetFunction(system, "n_conc_meal", protocol::DTsingle, false, &fruitGrainPart::get_n_conc_meal, "%", "meal N content");
   setupGetFunction(system, "grain_n", protocol::DTsingle, false, &fruitGrainPart::get_grain_n, "g/m^2", "N in grain");

   setupGetFunction(system, "grain_p", protocol::DTsingle, false, &fruitGrainPart::get_grain_p, "g/m^2","P in grain");

   setupGetFunction(system, "p_conc_grain", protocol::DTsingle, false, &fruitGrainPart::get_p_conc_grain, "%","P in grain");

   setupGetFunction(system, "p_grain_pcnt", protocol::DTsingle, false, &fruitGrainPart::get_p_conc_grain, "%","P in grain");

   system->addGettableVar("grain_p_demand",  gP_grain_demand, "g/m^2","P demand of grain");



   //   unsigned int id;
   // Set My Variable
   //   id = system->addRegistration(RegistrationType::respondToSet, "grain_oil_conc", floatType);
   //   IDtoSetFn.insert(UInt2SetFnMap::value_type(id,&Plant::set_plant_grain_oil_conc));




   idLatitude = system->addRegistration (RegistrationType::get, "latitude", floatType, "", "");


   for (vector<plantPart *>::iterator part = myParts.begin(); part != myParts.end(); part++)
      (*part)->doRegistrations(system);
}

float fruitGrainPart::nDemand2(void)
   //===========================================================================
{
   return mealPart->nDemand2();
}

float fruitGrainPart::pConcGrain(void) const
   //===========================================================================
{
   return pConcPercent();
}

float fruitGrainPart::pConcGrainTotal(void)  const
   //===========================================================================
{
   float p_conc = divide (pTotal() , dmTotal() , 0.0) * fract2pcnt;
   return p_conc;
}

float fruitGrainPart::pSenescedGrainTotal(void) const
   //===========================================================================
{
   return pSenesced();
}


void fruitGrainPart::get_grain_wt(protocol::Component *system, protocol::QueryValueData &qd)
   //===========================================================================
{
   system->sendVariable(qd, dmGreen());
}

void fruitGrainPart::get_grain_n(protocol::Component *system, protocol::QueryValueData &qd)
   //===========================================================================
{
   system->sendVariable(qd, nGreen());
}

void fruitGrainPart::get_grain_n_demand(protocol::Component *system, protocol::QueryValueData &qd)
   //===========================================================================
{
   system->sendVariable(qd, gN_grain_demand);
}

void fruitGrainPart::get_n_conc_grain(protocol::Component *system, protocol::QueryValueData &qd)
   //===========================================================================
{
   float n_conc = nConcPercent();
   system->sendVariable(qd, n_conc);
}

void fruitGrainPart::get_grain_protein(protocol::Component *system, protocol::QueryValueData &qd)
   //===========================================================================
{
   float gp = nConcPercent() * 5.71;
   system->sendVariable(qd, gp);
}

void fruitGrainPart::get_n_conc_meal(protocol::Component *system, protocol::QueryValueData &qd)
   //===========================================================================
{
   float n_conc = mealPart->nConcPercent();
   system->sendVariable(qd, n_conc);
}

void fruitGrainPart::get_yield(protocol::Component *system, protocol::QueryValueData &qd)
   //===========================================================================
{
   system->sendVariable(qd, dmTotal() * gm2kg / sm2ha);
}

void fruitGrainPart::get_yield_wet(protocol::Component *system, protocol::QueryValueData &qd)
   //===========================================================================
{
   system->sendVariable(qd, dmTotal() * gm2kg / sm2ha / (1.0 - cGrn_water_cont));
}

void fruitGrainPart::get_grain_p(protocol::Component *systemInterface, protocol::QueryValueData &qd)
   //===========================================================================
{
   float grain_p = pGreen();
   systemInterface->sendVariable(qd, grain_p);  //()
}

void fruitGrainPart::get_p_conc_grain(protocol::Component *systemInterface, protocol::QueryValueData &qd)
   //===========================================================================
{
   float p_conc_grain = pConcPercent();
   systemInterface->sendVariable(qd, p_conc_grain);  //()
}


void fruitGrainPart::doTick(protocol::TimeType &tick)
   //===========================================================================
{
   double sd = (double)tick.startday;
   jday_to_day_of_year(&sd, &gDay_of_year, &gYear);
}

// Field a NewMet event
void fruitGrainPart::doNewMet(protocol::NewMetType &newmet)
   //===========================================================================
{
   if (gHasreadconstants)
      {
      gMaxt = newmet.maxt;
      gMint = newmet.mint;
      }
}

void fruitGrainPart::readCultivarParameters (protocol::Component *system, const string &cultivar)
   //===========================================================================
{
   string s;

   system->writeString (" - reading grain cultivar parameters");

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

   for (vector<plantPart *>::iterator part = myParts.begin(); part != myParts.end(); part++)
      (*part)->readCultivarParameters(system, cultivar);
}

void fruitGrainPart::writeCultivarInfo (protocol::Component */* system*/)
   //===========================================================================
{
}

void fruitGrainPart::morphology(void)
{
}

void fruitGrainPart::zeroAllGlobals(void)
{
   plantPart::zeroAllGlobals();

   gHasreadconstants = false;
   gLatitude = 0.0;
   gMaxt = 0.0;
   gMint = 0.0;

   gDelayGrnFill  = 0.0;
   gDaysDelayedGrnFill  = 0;
   cNum_temp_grainfill = 0;
   cSw_fac_max  = 0.0;
   cTemp_fac_min  = 0.0;
   cSfac_slope  = 0.0;
   cTfac_slope  = 0.0;
   cGrn_water_cont  = 0.0;
   cNum_n_conc_stage;
   cN_conc_crit_grain  = 0.0;
   cN_conc_max_grain  = 0.0;
   cN_conc_min_grain  = 0.0;

   cTwilight = 0.0;

   pMinTempGrnFill = 0.0;
   pDaysDelayGrnFill = 0;

   gDlt_dm_stress_max        = 0.0;


   for (vector<plantPart *>::iterator part = myParts.begin(); part != myParts.end(); part++)
      (*part)->zeroAllGlobals();

}

void fruitGrainPart::zeroDeltas(void)
{
   plantPart::zeroDeltas();

   gDlt_dm_grain_demand = 0.0;
   gDlt_dm = 0.0;

   gN_grain_demand = 0.0;
   gP_grain_demand = 0.0;

   for (vector<plantPart *>::iterator part = myParts.begin(); part != myParts.end(); part++)
      (*part)->zeroDeltas();

}

void fruitGrainPart::onKillStem(void)
   // ====================================================================
{
   for (vector<plantPart *>::iterator part = myParts.begin(); part != myParts.end(); part++)
      (*part)->onKillStem();
}

void fruitGrainPart::doInit1(protocol::Component */* system*/)
   // ====================================================================
{
   mealPart = new fruitMealPart(plant, "meal");
   myParts.push_back(mealPart);

   oilPart = new fruitOilPart(plant, "oil");
   myParts.push_back(oilPart);

}


void fruitGrainPart::readConstants(protocol::Component *system, const string &section)
   //===========================================================================
{
   system->getVariable(idLatitude, gLatitude, -90.0, 90.0);
   for (vector<plantPart *>::iterator part = myParts.begin(); part != myParts.end(); part++)
      (*part)->readConstants(system, section);

}

void fruitGrainPart::readSpeciesParameters(protocol::Component *system, vector<string> &sections)
   //===========================================================================
{


   //    plant_phenology_init

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
   //    plant_event
   system->readParameter (sections
                          ,"grn_water_cont"//, "(g/g)"
                          , cGrn_water_cont
                          , 0.0, 1.0);

   system->readParameter (sections
                          , "x_stage_code"//, "()"
                          , cX_stage_code, cNum_n_conc_stage
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

   gHasreadconstants = true;

   for (vector<plantPart *>::iterator part = myParts.begin(); part != myParts.end(); part++)
      (*part)->readSpeciesParameters(system, sections);

}

void fruitGrainPart::update(void)
   //===========================================================================
{

   vector<plantPart *>::iterator part;

   // Update
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->update();

   if (plant->inPhase("hi_stress_sensitive")) gDm_stress_max.update();
}

void fruitGrainPart::display(ostream &os) const
{
   //   os << "fruitGrainPart:" << endl;
   //   os << "Green meal: " << green.meal << endl;
   //   os << "Senesced meal: " << senesced.meal << endl;
   //   os << "Dead meal: " << dead.meal << endl << endl;
   os << endl;
}


void fruitGrainPart::doProcessBioDemand(void)
   //===========================================================================
{

   doDMDemandStress ();
   oilPart->doBioGrainOil ();
   doDMDemandGrain ();
}

float fruitGrainPart::nDemandGrain(void) const {return gN_grain_demand;}
float fruitGrainPart::nDemandGrain2(void){return nDemand2();}
float fruitGrainPart::nConcPercent(void) const {return divide (nTotal(), dmTotal(), 0.0) * fract2pcnt;}   //remove
float fruitGrainPart::grainNConcPercent(void) const {return divide (nTotal(), dmTotal(), 0.0) * fract2pcnt;}
float fruitGrainPart::dltDmDemand(void) {return gDlt_dm_grain_demand;}                               //remove
float fruitGrainPart::dltDmGrainDemand(void) const {return gDlt_dm_grain_demand;}

float fruitGrainPart::meanT (void) {return 0.5 * (gMaxt + gMint);}

void fruitGrainPart::doDMDemandGrain(void)
   //===========================================================================
{
}

void fruitGrainPart::doNDemandGrain(float nfact_grain_conc      //   (INPUT)
                                     , float swdef_expansion)    //   grain N demand (g/m^2)
   //===========================================================================
{
   //    Calculate plant n demand

//   float   n_potential;           // maximum grain N demand (g/m^2)

   if (plant->inPhase("grainfill"))
   {
      gN_grain_demand = mealPart->dltDmGreenNew()
                      * dltNGrainConc(cSfac_slope
                                     , cSw_fac_max
                                     , cTemp_fac_min
                                     , cTfac_slope
                                     , meanT()
                                     , nfact_grain_conc
                                     , swdef_expansion);

      gN_grain_demand = u_bound (gN_grain_demand, mealPart->nCapacity2());
   }
}

float fruitGrainPart::dltNGrainConc(float sfac_slope      //(INPUT)  soil water stress factor slope
                                    , float sw_fac_max      //(INPUT)  soil water stress factor maximum
                                    , float temp_fac_min    //(INPUT)  temperature stress factor minimum optimum temp
                                    , float tfac_slope      //(INPUT)  temperature stress factor slope
                                    , float ave_temp        //(INPUT)  mean air temperature (oC)
                                    , float nfact_grain_conc// (INPUT)
                                    , float swdef_expansion) // (INPUT)
   //==========================================================================

   /*  Purpose
      *     Calculate the nitrogen concentration required to meet the increase
      *     from daily grain growth (0-1) as affected by temperature and water stress.
      *
      *
      *  Notes
      *     First, two factors are calculated and used to estimate the
      *     effects of mean temperature and drought stress on the N
      *     concentration in grain growth for the day.  High temperature
      *     or drought stress can cause the factors to exceed 1.
      *     N deficiency can cause nfac < 1.  The net effect of these
      *     equations is to allow grain nitrogen concentration to range
      *     from less than .01 when N deficiency is severe to about .018
      *     stress limit grain growth.
      *     Here, optimum N concentration = 1.7%
      *
      */
{
   //  Local Variables
   float N_conc_pot;                   // potential grain N concentration (0-1) (g N/g part)
   float N_grain_sw_fac;               // soil water stress factor for N uptake
   float N_grain_temp_fac;             // temperature stress factor for N uptake

   //!!!!!!!!!! return to orig cm
   N_grain_temp_fac = temp_fac_min + tfac_slope * ave_temp;
   N_grain_sw_fac = sw_fac_max - sfac_slope * swdef_expansion ;

   // N stress reduces grain N concentration below critical
   N_conc_pot = mealPart->N_conc_pot(nfact_grain_conc);

   // Temperature and water stresses can decrease/increase grain
   // N concentration

   // when there is no N stress, the following can be a higher N conc than
   // the crit and thus the N conc of the grain can exceed N critical.

   return  (N_conc_pot * max (N_grain_temp_fac, N_grain_sw_fac));
}

float fruitGrainPart::calcDmDemand (void)
   //===========================================================================
{
   //       Calculate grain dm yield demand (g/m^2)
   //       (OUTPUT) assimilate demand for reproductive part (g/m^2)

   return oilPart->calcDmDemand (gDlt_dm_grain_demand);
}

void fruitGrainPart::doDMDemandStress (void)
   //     ===========================================================
{
   //       Simulate crop grain biomass demand stress factor

   cproc_yieldpart_demand_stress1 (min(plant->getNfactPhoto(), plant->getPfactPhoto())
                                   , plant->getSwdefPhoto()
                                   , plant->getTempStressPhoto()
                                   , &gDlt_dm_stress_max);
}

void fruitGrainPart::doNInit (void)
   //============================================================================
{
   //       Initialise plant nitrogen.

   if (plant->inPhase("grainfill"))
      doNConcGrainLimits();
}

void fruitGrainPart::doNConcGrainLimits (void)
   //============================================================================
{
   //       Calculate the critical N concentration for grain below which plant growth
   //       is affected.  Also minimum and maximum N concentrations below
   //       and above which it is not allowed to fall or rise.
   //       These are analogous to the water concentrations
   //       of sat, dul and ll.

   //+  Local Variables
   float dm_grain;                               // grain mass (g/m2)
   float n_crit_grain;                           // critial mass of grain N (g/m2)
   float n_max_grain;                            // maximum mass of grain N (g/m2)
   float n_min_grain;                            // minimum mass of grain N (g/m2)

   //- Implementation Section ----------------------------------
   if (plant->inPhase ("grainfill"))
      {
      dm_grain = 0.0;
      vector<plantPart *>::iterator part;
      for (part = myParts.begin(); part != myParts.end(); part++)
         dm_grain += (*part)->dmGreenNew();

      n_crit_grain = cN_conc_crit_grain * dm_grain;
      n_max_grain = cN_conc_max_grain * dm_grain;
      n_min_grain = cN_conc_min_grain * dm_grain;

      mealPart->doNConcGrainLimits(n_min_grain, n_crit_grain, n_max_grain);
      }
}
void fruitGrainPart::doNRetranslocate( float N_supply, float grain_n_demand)
   //============================================================================
{
   //     Calculate the nitrogen retranslocation from the various plant parts to the grain.

   // available N does not include grain
   // this should not presume grain is 0.

   // get actual grain N uptake by retransolcation
   // limit retranslocation to total available N

   if (grain_n_demand >= N_supply)
      {
      // demand greater than or equal to supply
      // retranslocate all available N

      mealPart->doNRetranslocate(N_supply, grain_n_demand);
      }
   else
      {
      // supply greater than demand.
      // Retranslocate what is needed

      mealPart->doNRetranslocate(grain_n_demand, grain_n_demand);

      }
}

void fruitGrainPart::doNDemand1(float /*dlt_dm*/             // (INPUT)  Whole plant the daily biomass production (g/m^2)
                                , float /*dlt_dm_pot_rue*/)  // (INPUT)  Whole plant potential dry matter production (g/m^2)
   //============================================================================
   //     Return plant nitrogen demand for each plant component
{
   NDemand = 0.0;
   NMax = 0.0;
}

void fruitGrainPart::doNDemand1Pot(float /*dlt_dm*/             // (INPUT)  Whole plant the daily biomass production (g/m^2)
                                   , float /*dlt_dm_pot_rue*/)  // (INPUT)  Whole plant potential dry matter production (g/m^2)
   //============================================================================
   //     Return plant nitrogen demand for each plant component
{
   NDemand = 0.0;
   NMax = 0.0;
}

void fruitGrainPart::doNDemand2(float /*dlt_dm*/             // (INPUT)  Whole plant the daily biomass production (g/m^2)
                                , float /*dlt_dm_pot_rue*/)  // (INPUT)  Whole plant potential dry matter production (g/m^2)
   //============================================================================
   //     Return plant nitrogen demand for each plant component
{
   NDemand = 0.0;
   NMax = 0.0;
   NDemand -= mealPart->nDemand();           //FIXME can do this stuff better in mealpart?
   mealPart->doNDemand(gN_grain_demand);
   NDemand += mealPart->nDemand();
}

float fruitGrainPart::dmGrainWetTotal(void) const
//=======================================================================================
   {
   return (divide(dmTotal(), (1.0 - cGrn_water_cont), 0.0));
   }

float fruitGrainPart::grainWaterContent(void) const{return cGrn_water_cont;}

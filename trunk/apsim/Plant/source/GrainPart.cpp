
// Modification log
// 6 Aug 97 J. Hargreaves  Implementation

#define YES 1
#define NO 0
#define TEST_fruitGrainPart NO					// build unit test?

#include "GrainPart.h"

using namespace std;

static const char* floatType =        "<type kind=\"single\"/>";

void push_routine (const char *) {};
void pop_routine (const char *) {};

inline bool floatsAreEqual(float A, float B, float C) {return(fabs(A-B)<C);}

// default constructor
// 	initialise data members.
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

ostream &operator<<(ostream &output, const fruitGrainPart /*&pool*/)
{
   //	output << "fruitGrainPart:" << endl;
   //	output << "   Green meal:    " << pool.green.meal << endl;
   //	output << "   Senesced meal: " << pool.senesced.meal << endl;
   //	output << "   Dead meal:     " << pool.dead.meal << endl << endl;
   output << endl;
   return output;
}

// copy constructor
//	copy data members of object
//===========================================================================
//fruitGrainPart::fruitGrainPart(const fruitGrainPart &fruitGrainPart)
////===========================================================================
//{
//	throw std::invalid_argument("Copy constructor NI for fruitGrainPart");
//}


// Assigment operator
//	assign data members of object

const fruitGrainPart &fruitGrainPart::operator=(const fruitGrainPart &/*other*/)
   //===========================================================================
{
   throw std::invalid_argument("Assignment operator NI for fruitGrainPart");
}

void fruitGrainPart::doRegistrations(protocol::Component *system)
   //===========================================================================
{
   CompositePart::doRegistrations(system);

   //   setupEvent(system, "tick",        RegistrationType::respondToEvent, &fruitGrainPart::doTick);
   //   setupEvent(system, "newmet",      RegistrationType::respondToEvent, &fruitGrainPart::doNewMet);

   system->addGettableVar("dlt_dm_grain_demand",gDlt_dm_grain_demand, "g/m^2", "??");
   system->addGettableVar("dlt_dm_fruit", gDlt_dm, "g/m^2", "Change in dry matter");
   system->addGettableVar("grain_no",gGrain_no, "/m^2", "Grain number");
   setupGetFunction(system, "grain_size", protocol::DTsingle, false, &fruitGrainPart::get_grain_size, "g", "Size of each grain");
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

float fruitGrainPart::grainWt(void)
   //===========================================================================
{
   return divide (dmTotal(), gGrain_no, 0.0);
}

float fruitGrainPart::nDemand2(void)
   //===========================================================================
{
   return mealPart->nDemand2();
}

float fruitGrainPart::pConcGrainTotal(void)
   //===========================================================================
{
   float p_conc = divide (pTotal() , dmTotal() , 0.0) * fract2pcnt;
   return p_conc;
}


void fruitGrainPart::get_grain_wt(protocol::Component *system, protocol::QueryValueData &qd)
   //===========================================================================
{
   system->sendVariable(qd, dmGreen());
}

void fruitGrainPart::get_grain_size(protocol::Component *system, protocol::QueryValueData &qd)
   //===========================================================================
{
   float grain_size = divide (dmTotal(), gGrain_no, 0.0);

   system->sendVariable(qd, grain_size);
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



void fruitGrainPart::doGrainNumber (void)
   //===========================================================================
   //       Calculate Grain Number
{
   if (cGrain_no_option == 1)
      {
      // do not use grain number
      gGrain_no = 0;
      }
   else if (cGrain_no_option == 2)
      {
      gGrain_no = grainNumber (plant->getDmGreenStem()
                              , pGrains_per_gram_stem);
      }
   else
      {
      throw std::invalid_argument ("invalid template option");
      }

   return;
}

float fruitGrainPart::grainNumber (float stem_dm_green
                                      , float p_grains_per_gram_stem)    // OUTPUT
   //===========================================================================
   //       Perform grain number calculations
{
   float grain_no;
   if (plant->on_day_of ("emergence"))
      {
      // seedling has just emerged.
      grain_no = 0.0;
      }
   else if (plant->on_day_of ("flowering"))
      {
      // we are at first day of grainfill.
      grain_no = p_grains_per_gram_stem * stem_dm_green;
      }
   else
      {
      grain_no = gGrain_no;   // no changes
      }
   return grain_no;
}

void fruitGrainPart::doTick(protocol::timeType &tick)
   //===========================================================================
{
   double sd = (double)tick.startday;
   jday_to_day_of_year(&sd, &gDay_of_year, &gYear);
}

// Field a NewMet event
void fruitGrainPart::doNewMet(protocol::newmetType &newmet)
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

   for (vector<plantPart *>::iterator part = myParts.begin(); part != myParts.end(); part++)
      (*part)->readCultivarParameters(system, cultivar);
}

void fruitGrainPart::writeCultivarInfo (protocol::Component *system)
   //===========================================================================
{

   // report
   string s;

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
   gDaysDelayedGrnFill  = 0.0;
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
   cNum_n_conc_stage;
   cN_conc_crit_grain  = 0.0;
   cN_conc_max_grain  = 0.0;
   cN_conc_min_grain  = 0.0;

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

   gGrain_no = 0.0;

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

   gGrain_no = 0.0;
}

void fruitGrainPart::doInit1 ()
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
                          , "twilight"//, "(o)"
                          , cTwilight
                          , -90.0, 90.0);

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

   //    Grain _n_dlt_grain_conc
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

   // transfer plant grain no.
   float dlt_grain_no_lost  = gGrain_no * plant->getDyingFractionPlants();
   gGrain_no -= dlt_grain_no_lost;

   if (plant->inPhase("hi_stress_sensitive")) gDm_stress_max.update();
}

void fruitGrainPart::display(ostream &os) const
{
   //	os << "fruitGrainPart:" << endl;
   //	os << "Green meal: " << green.meal << endl;
   //	os << "Senesced meal: " << senesced.meal << endl;
   //	os << "Dead meal: " << dead.meal << endl << endl;
   os << endl;
}


void fruitGrainPart::doProcessBioDemand(void)
   //===========================================================================
{

   doDMDemandStress ();
   doGrainNumber();
   oilPart->doBioGrainOil ();
   doDMDemandGrain ();

   return;
}

float fruitGrainPart::grainNo(void) {return gGrain_no;}
float fruitGrainPart::nDemandGrain(void) {return gN_grain_demand;}
float fruitGrainPart::nConcPercent(void) {return divide (nTotal(), dmTotal(), 0.0) * fract2pcnt;}   //redmove
float fruitGrainPart::grainNConcPercent(void) {return divide (nTotal(), dmTotal(), 0.0) * fract2pcnt;}
float fruitGrainPart::dltDmDemand(void) {return gDlt_dm_grain_demand;}                               //remove
float fruitGrainPart::dltDmGrainDemand(void) const {return gDlt_dm_grain_demand;}

float fruitGrainPart::meanT (void) {return 0.5 * (gMaxt + gMint);}

void fruitGrainPart::doDMDemandGrain (void)
   //===========================================================================
{
   //       Simulate crop grain biomass demand.

   if (cGrain_fill_option == 1)
      {
      doDMDemandGrain1();
      }
   else if (cGrain_fill_option == 2)
      {
      if (plant->inPhase("grainfill"))
         doDMDemandGrain2();
      else
         gDlt_dm_grain_demand = 0.0;
      }
   else
      {
      throw std::invalid_argument("invalid template option in doDMDemandGrain");
      }

   return;
}

void fruitGrainPart::doDMDemandGrain2(void)
   //===========================================================================
{
   //       Perform grain filling calculations

   float tav;

   // we are in grain filling stage
   tav = meanT();

   gDlt_dm_grain_demand = gGrain_no
                        * pPotential_grain_filling_rate
                        * linear_interp_real(tav
                                             ,cX_temp_grainfill
                                             ,cY_rel_grainfill
                                             ,cNum_temp_grainfill);

   mealPart->doDMDemandGrain(gDlt_dm_grain_demand);
}

void fruitGrainPart::doDMDemandGrain1(void)
   //===========================================================================
{
   //        Find grain demand for carbohydrate using harvest index (g/m^2)


   //+  Local Variables
   float dlt_dm_yield;                           // grain demand for carbohydrate (g/m^2)
   float dlt_dm_yield_unadj;                     // grain demand for carbohydrate, unadjusted
   float harvest_index;                          // last harvest index (g grain/g biomass)
   float hi_max_pot;                             // max potential HI due to stress
   float dm_tops_new;                            // new drymatter  tops (g/m^2)
   float harvest_index_new;                      // next harvest index (g grain/g biomass)
   float dm_grain_new;                           // new drymatter grain (g/m^2)
   float hi_incr;                                // harvest index increment per day
   float photoperiod;                            // hours of photosynthetic light (hours)
   float dlt_dm_grain_demand;                    // total dm demand of grain (g/m^2)

   //- Implementation Section ----------------------------------

   if (plant->inPhase("grainfill"))
      {

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
      float dm_green_yield_parts = mealPart->dmGreen() + oilPart->dmGreen();

      harvest_index = divide (dm_green_yield_parts, plant->getDmTops(), 0.0);
      dm_tops_new = plant->getDmTops() + plant->getDltDm();

      harvest_index_new = u_bound (harvest_index + hi_incr, hi_max_pot);

      dm_grain_new = dm_tops_new * harvest_index_new;
      dlt_dm_yield_unadj = dm_grain_new - dm_green_yield_parts;

      // adjust for grain energy

      dlt_dm_yield_unadj = bound (dlt_dm_yield_unadj, 0.0, dm_grain_new);

      dlt_dm_yield = dlt_dm_yield_unadj * oilPart->energyAdjustHI(harvest_index_new);

      dlt_dm_grain_demand = oilPart->energyAdjustDM(dlt_dm_yield);


      // delay grainfill after cold snap
      if (gMint <= pMinTempGrnFill)
         gDelayGrnFill = true;
      if (gDelayGrnFill)
         {
         dlt_dm_grain_demand = 0.0;
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
         dlt_dm_grain_demand = 0.0;
      }
   gDlt_dm_grain_demand = dlt_dm_grain_demand;

      oilPart->doDMDemandGrain(dlt_dm_grain_demand);
      mealPart->doDMDemandGrain(dlt_dm_grain_demand - oilPart->dmGreenDemand());

   return;
}

//    Get the grain nitrogen demand
void fruitGrainPart::doNDemandGrain (float nfact_grain_conc      //   (INPUT)
                                     , float swdef_expansion)    //   grain N demand (g/m^2)
   //===========================================================================

   {
   if (cGrain_n_option == 1)
      {
      if (plant->inPhase("grainfill"))
         doNDemandGrain1(nfact_grain_conc, swdef_expansion);
      }

   else if (cGrain_n_option == 2)
      doNDemandGrain2();   // start grain n filling immediately after flowering

   else
      throw std::invalid_argument ("Invalid n demand option");
}

void fruitGrainPart::doNDemandGrain1(float nfact_grain_conc      //   (INPUT)
                                     , float swdef_expansion)    //   grain N demand (g/m^2)
   //===========================================================================
{
   //    Calculate plant n demand

   float   n_potential;           // maximum grain N demand (g/m^2)

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

void fruitGrainPart::doNDemandGrain2 (void)
   //===========================================================================
{
   const char *my_name = "doNDemandGrain2";

   float Tav ;
   float grain_growth;

   push_routine (my_name);

   // default case
   gN_grain_demand = 0.0;

   if (plant->inPhase("reproductive"))
      {
      // we are in grain filling stage
      Tav = meanT();

      gN_grain_demand = gGrain_no
                     * cPotential_grain_n_filling_rate
                     * linear_interp_real (Tav, cX_temp_grain_n_fill, cY_rel_grain_n_fill, cNum_temp_grain_n_fill);
      }

   if (plant->inPhase("grainfill"))
      {
      // during grain C filling period so make sure that C filling is still
      // going on otherwise stop putting N in now

      grain_growth = divide(mealPart->dltDmGreenNew()
                            , gGrain_no
                            , 0.0);
      if (grain_growth < cCrit_grainfill_rate)
         {
         //! grain filling has stopped - stop n flow as well
         gN_grain_demand = 0.0;
         }
      }

   pop_routine (my_name);
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
   NDemand -= mealPart->nDemand();           //fixme can do this stuff better in mealpart?
   mealPart->doNDemand(gN_grain_demand);
   NDemand += mealPart->nDemand();
}

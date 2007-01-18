
// Modification log
// 6 Aug 97 J. Hargreaves  Implementation

#include "GrainPartGN.h"

using namespace std;

static const char* floatType =        "<type kind=\"single\"/>";

inline bool floatsAreEqual(float A, float B, float C) {return(fabs(A-B)<C);}

// default constructor
fruitGrainPartGN::fruitGrainPartGN()
{}

//  initialise data members.
fruitGrainPartGN::fruitGrainPartGN(plantInterface *p, const string &name) : fruitGrainPart(p, name)
{
}

// destructor
fruitGrainPartGN::~fruitGrainPartGN()
{
}

ostream &operator<<(ostream &output, const fruitGrainPartGN /*&pool*/)
{
   //   output << "fruitGrainPartGN:" << endl;
   //   output << "   Green meal:    " << pool.green.meal << endl;
   //   output << "   Senesced meal: " << pool.senesced.meal << endl;
   //   output << "   Dead meal:     " << pool.dead.meal << endl << endl;
   output << endl;
   return output;
}

// copy constructor
//  copy data members of object
//===========================================================================
fruitGrainPartGN::fruitGrainPartGN(const fruitGrainPartGN &/* fruitGrainPartGN*/)
//===========================================================================
{
  throw std::invalid_argument("Copy constructor NI for fruitGrainPartGN");
}


// Assigment operator
//  assign data members of object

const fruitGrainPartGN &fruitGrainPartGN::operator=(const fruitGrainPartGN &/*other*/)
   //===========================================================================
{
   throw std::invalid_argument("Assignment operator NI for fruitGrainPartGN");
}

void fruitGrainPartGN::doRegistrations(protocol::Component *system)
   //===========================================================================
{
   fruitGrainPart::doRegistrations(system);

//   //   setupEvent(system, "tick",        RegistrationType::respondToEvent, &fruitGrainPartGN::doTick);
//   //   setupEvent(system, "newmet",      RegistrationType::respondToEvent, &fruitGrainPartGN::doNewMet);
//
   system->addGettableVar("grain_no",gGrain_no, "/m^2", "Grain number");
   setupGetFunction(system, "grain_size", protocol::DTsingle, false, &fruitGrainPartGN::get_grain_size, "g", "Size of each grain");
}

float fruitGrainPartGN::grainWt(void) const
   //===========================================================================
{
   return divide (dmTotal(), gGrain_no, 0.0);
}

void fruitGrainPartGN::get_grain_size(protocol::Component *system, protocol::QueryValueData &qd)
   //===========================================================================
{
   float grain_size = divide (dmTotal(), gGrain_no, 0.0);

   system->sendVariable(qd, grain_size);
}

void fruitGrainPartGN::doGrainNumber (void)
   //===========================================================================
   //       Calculate Grain Number
{
      gGrain_no = grainNumber (plant->getDmGreenStem()
                              , pGrains_per_gram_stem);
   return;
}

float fruitGrainPartGN::grainNumber (float stem_dm_green
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

void fruitGrainPartGN::readCultivarParameters (protocol::Component *system, const string &cultivar)
   //===========================================================================
{
   fruitGrainPart::readCultivarParameters (system, cultivar);
      system->readParameter (cultivar.c_str()
                             , "grains_per_gram_stem"//, "(/g)"
                             , pGrains_per_gram_stem
                             , 0.0, 10000.0);

      system->readParameter (cultivar.c_str()
                             , "potential_grain_filling_rate"//, "(g/grain/day)"
                             , pPotential_grain_filling_rate
                             , 0.0, 1.0);

      system->readParameter (cultivar.c_str()
                             , "potential_grain_growth_rate"//, "(g/grain/day)"
                             , pPotential_grain_growth_rate
                             , 0.0, 1.0);

      system->readParameter (cultivar.c_str()
                             , "max_grain_size"//, "(g)"
                             , pMaxGrainSize
                             , 0.0, 1.0);
}

void fruitGrainPartGN::writeCultivarInfo (protocol::Component *system)
   //===========================================================================
{

   // report
     ostringstream msg;
     msg.flags ( ios::right | ios::fixed);
     msg.precision(1);
     msg << "   grains_per_gram_stem           = " << setw(10) << pGrains_per_gram_stem << " (/g)" << endl;
     msg.precision(4);
     msg << "   potential_grain_filling_rate   = " << setw(10) << pPotential_grain_filling_rate << " (g/grain/day)" << endl;
     msg << "   potential_grain_growth_rate    = " << setw(10) << pPotential_grain_growth_rate << " (g/grain/day)" << endl;
     msg << "   max_grain_size                 = " << setw(10) << pMaxGrainSize << " (g)" << ends;
     system->writeString (msg.str().c_str());

}

void fruitGrainPartGN::zeroAllGlobals(void)
{
   fruitGrainPart::zeroAllGlobals();

   cPotential_grain_n_filling_rate  = 0.0;
   cMinimum_grain_n_filling_rate  = 0.0;
   cCrit_grainfill_rate  = 0.0;
   cNum_temp_grain_n_fill = 0;
   pGrains_per_gram_stem = 0.0;
   pPotential_grain_growth_rate = 0.0;
   pMaxGrainSize = 0.0;
   gGrain_no = 0.0;

}

void fruitGrainPartGN::onKillStem(void)
   // ====================================================================
{
   fruitGrainPart::onKillStem();
   gGrain_no = 0.0;
}

void fruitGrainPartGN::readSpeciesParameters(protocol::Component *system, vector<string> &sections)
   //===========================================================================
{
     fruitGrainPart::readSpeciesParameters(system, sections);

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
      system->readParameter (sections
                             , "potential_grain_n_filling_rate"//, "()"
                             , cPotential_grain_n_filling_rate
                             , 0.0, 1.0);
      system->readParameter (sections
                             , "minimum_grain_n_filling_rate"//, "()"
                             , cMinimum_grain_n_filling_rate
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

void fruitGrainPartGN::update(void)
   //===========================================================================
{
   fruitGrainPart::update();
   // transfer plant grain no.
   float dlt_grain_no_lost  = gGrain_no * plant->getDyingFractionPlants();
   gGrain_no -= dlt_grain_no_lost;

}

//void fruitGrainPartGN::display(ostream &os) const
//{
//   //   os << "fruitGrainPartGN:" << endl;
//   //   os << "Green meal: " << green.meal << endl;
//   //   os << "Senesced meal: " << senesced.meal << endl;
//   //   os << "Dead meal: " << dead.meal << endl << endl;
//   os << endl;
//}
//

void fruitGrainPartGN::doProcessBioDemand(void)
   //===========================================================================
{

   doDMDemandStress ();
   doGrainNumber();
   oilPart->doBioGrainOil ();
   doDMDemandGrain ();

   return;
}

float fruitGrainPartGN::grainNo(void) const {return gGrain_no;}

void fruitGrainPartGN::doDMDemandGrain(void)
   //===========================================================================
   {
   if (plant->inPhase("postflowering"))
      {
      //       Perform grain filling calculations
      float tav;
      tav = meanT();

      if (plant->inPhase("grainfill"))
         gDlt_dm_grain_demand = gGrain_no
                              * pPotential_grain_filling_rate
                              * linear_interp_real(tav
                                                   ,cX_temp_grainfill
                                                   ,cY_rel_grainfill
                                                  ,cNum_temp_grainfill);
      else
         {
         // we are in the flowering to grainfill phase
         gDlt_dm_grain_demand = gGrain_no
                              * pPotential_grain_growth_rate
                              * linear_interp_real(tav
                                                   ,cX_temp_grainfill
                                                   ,cY_rel_grainfill
                                                  ,cNum_temp_grainfill);
          }
       // check that grain growth will not result in daily n conc below minimum conc
       // for daily grain growth
      float nfact_grain_conc = plant->getNfactGrainConc();
      float nfact_grain_fill = min(1.0, nfact_grain_conc*cPotential_grain_n_filling_rate/cMinimum_grain_n_filling_rate);
      gDlt_dm_grain_demand = gDlt_dm_grain_demand * nfact_grain_fill;

      // Check that growth does not exceed maximum grain size
      float max_grain = gGrain_no * pMaxGrainSize;
      float max_dlt = max (max_grain - mealPart->dmGreen(), 0.0);
      gDlt_dm_grain_demand = min (gDlt_dm_grain_demand, max_dlt);

      mealPart->doDMDemandGrain(gDlt_dm_grain_demand);
      }
   else
      gDlt_dm_grain_demand = 0.0;

   }

void fruitGrainPartGN::doNDemandGrain (float nfact_grain_conc      //   (INPUT)
                                     , float /* swdef_expansion*/)    //   grain N demand (g/m^2)
   //===========================================================================
{
   float Tav ;
   float grain_growth;

   // default case
   gN_grain_demand = 0.0;

   if (plant->inPhase("reproductive"))
      {
      // we are in grain filling stage
      Tav = meanT();

      gN_grain_demand = gGrain_no
                     * cPotential_grain_n_filling_rate * nfact_grain_conc
                     * linear_interp_real (Tav, cX_temp_grain_n_fill, cY_rel_grain_n_fill, cNum_temp_grain_n_fill);
      }

   if (plant->inPhase("postflowering"))
   //if (plant->inPhase("grainfill"))
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
      float dailyNconc = divide(gN_grain_demand,mealPart->dltDmGreenNew(),1.0);
      if (dailyNconc > 0.03) gN_grain_demand = mealPart->dltDmGreenNew()*0.03;
      }

}



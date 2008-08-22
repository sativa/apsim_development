#include "StdPlant.h"

#include "FruitCohort.h"

#include "GrainPart.h"
#include "GrainPartGN.h"
#include "GrainPartHI.h"
#include "PodPart.h"

using namespace std;

// ##############################################################
// DPH: Most of this file can be removed by relying on
// composite part to do the looping over parts for dmGreenVeg,
// dmSenescedVeg etc. We would need to put in lines in the grain
// parts to return 0.0 for the dmGreenVeg, dmSenescedVeg etc.
// ##############################################################

//  initialise data members.
FruitCohort::FruitCohort(ScienceAPI& scienceAPI, plantInterface *p, const string &name)
   : CompositePart(scienceAPI, p, name)
{
}

// destructor
FruitCohort::~FruitCohort()
   // ====================================================================
   {
   }

void FruitCohort::onInit1(protocol::Component *system)
   //===========================================================================
   {
   zeroAllGlobals(); zeroDeltas();


   string grainType;
   scienceAPI.read("grain_part_type", grainType);
   if (grainType == "harvest_index")
      grainPart = new fruitGrainPartHI(scienceAPI, plant, "grain");
   else if (grainType == "grain_no")
      grainPart = new fruitGrainPartGN(scienceAPI, plant, "grain");
   else
     throw std::runtime_error("Unknown grain_part_type '" + grainType + "'");

   podPart = new fruitPodPart(scienceAPI, plant, this, "pod");

   add(podPart);
   myVegParts.push_back(podPart);
   supplyPools.push_back(podPart);

   add(grainPart);
   myGrainParts.push_back(grainPart);

   // call into base class.
   CompositePart::onInit1(system);

   // register some other things.
   system->addGettableVar("dlt_dm_fruit", gDlt_dm, "g/m^2", "Change in dry matter");
   setupGetFunction(system, "head_wt", protocol::DTsingle, false,&FruitCohort::get_head_wt, "g/m^2", "Weight of heads");
   setupGetFunction(system, "head_n", protocol::DTsingle, false,&FruitCohort::get_head_n, "g/m^2", "N in heads");
   setupGetFunction(system, "head_p", protocol::DTsingle, false, &FruitCohort::get_head_p, "g/m^2","P in head");
   }

void FruitCohort::checkBounds (void)
{
   for (vector<plantPart *>::iterator part = myParts.begin();
        part != myParts.end();
        part++)
      (*part)->checkBounds();
}

ostream &operator<<(ostream &output, const FruitCohort /*&pool*/)
{
   //   output << "FruitCohort:" << endl;
   //   output << "   Green cover:    " << pool.coverPod.green << endl;
   //   output << "   Senesced cover: " << pool.coverPod.sen << endl;
   //   output << "   Dead cover:     " << pool.coverPod.dead << endl;
   //   output << endl;
   //   output << "   Green shell:    " << pool.green.shell << endl;
   //   output << "   Green meal:    " << pool.green.meal << endl;
   //   output << "   Senesced shell: " << pool.senesced.shell << endl;
   //   output << "   Senesced meal: " << pool.senesced.meal << endl;
   //   output << "   Dead shell:     " << pool.dead.shell << endl;
   //   output << "   Dead meal:     " << pool.dead.meal << endl << endl;
   output << endl;
   return output;
}


// Assigment operator
//  assign data members of object
const FruitCohort &FruitCohort::operator=(const FruitCohort &/*other*/)
   //===========================================================================
{
   throw std::invalid_argument("Assignment operator NI for FruitCohort");
}

float FruitCohort::dltDmPotentialGrain(void)
   //===========================================================================
{
   return grainPart->calcDltDmPotentialGrain();
}

float FruitCohort::nMax(void)
   //===========================================================================
{
   float nMax = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      nMax += (*part)->nMax();
   return nMax;
}

float FruitCohort::pMaxPot(void)
   //===========================================================================
{
   float pMaxPot = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      pMaxPot += (*part)->pMaxPot();
   return pMaxPot;
}

float FruitCohort::pMinPot(void)
   //===========================================================================
{
   float pMinPot = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      pMinPot += (*part)->pMinPot();
   return pMinPot;
}

void FruitCohort::get_head_wt(protocol::Component *system, protocol::QueryValueData &qd)
   //===========================================================================
{
   float headWt = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      headWt += (*part)->Green.DM();

   system->sendVariable(qd, headWt);
}

void FruitCohort::get_head_n(protocol::Component *system, protocol::QueryValueData &qd)
   //===========================================================================
{
   system->sendVariable(qd, Grain.N() + Vegetative.N());  // Why not VegetativeTotal ????
}

void FruitCohort::get_head_p(protocol::Component *systemInterface, protocol::QueryValueData &qd)
   //===========================================================================
{
   systemInterface->sendVariable(qd, Grain.P() + Vegetative.P());  // Why not VegetativeTotal?????
}


void FruitCohort::display(ostream &os)
{
   //   os << "FruitCohort:" << endl;
   //   os << "Green cover:    " << coverPod.green << endl;
   //   os << "Senesced cover: " << coverPod.sen << endl;
   //   os << "Dead cover:     " << coverPod.dead << endl;
   //   os << "Green shell: " << green.shell << endl;
   //   os << "Green meal: " << green.meal << endl;
   //   os << "Senesced shell: " << senesced.shell << endl;
   //   os << "Senesced meal: " << senesced.meal << endl;
   //   os << "Dead shell: " << dead.shell << endl;
   //   os << "Dead meal: " << dead.meal << endl << endl;
   os << endl;
}

void FruitCohort::doDmDemand ( float dlt_dm_veg_supply)
   //===========================================================================
{
   doProcessBioDemand();
   //       (OUTPUT) assimilate demand for reproductive part (g/m^2)
   // calculate demands of reproductive parts
   podPart->doDmDemand(dlt_dm_veg_supply);
}


void FruitCohort::doNDemand1(float dlt_dm             // (INPUT)  Whole plant the daily biomass production (g/m^2)
                            , float dlt_dm_pot_rue)  // (INPUT)  Whole plant potential dry matter production (g/m^2)
   //============================================================================
   //     Return plant nitrogen demand for each plant component
{
   vector<plantPart *>::iterator part;
   for (part = supplyPools.begin(); part != supplyPools.end(); part++)
      (*part)->doNDemand1(dlt_dm, dlt_dm_pot_rue);
}

void FruitCohort::doNDemand1Pot(float dlt_dm             // (INPUT)  Whole plant the daily biomass production (g/m^2)
                               , float dlt_dm_pot_rue)  // (INPUT)  Whole plant potential dry matter production (g/m^2)
   //============================================================================
   //     Return plant nitrogen demand for each plant component
{
   vector<plantPart *>::iterator part;
   for (part = supplyPools.begin(); part != supplyPools.end(); part++)
      (*part)->doNDemand1Pot(dlt_dm, dlt_dm_pot_rue);
}



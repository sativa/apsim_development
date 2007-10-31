#include <stdio.h>
#include <math.h>
#include <stdexcept>
#include <string>
#include "PlantPart.h"

#include "FruitCohort.h"

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
   for (vector<plantPart *>::iterator part = myParts.begin();
        part != myParts.end();
        part++)
      delete (*part);
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

   podPart = new fruitPodPart(scienceAPI, plant, grainPart, "pod");

   myParts.push_back(podPart);
   myVegParts.push_back(podPart);
   supplyPools.push_back(podPart);

   myParts.push_back(grainPart);
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


//float FruitCohort::grainWaterContent(void)
//   //===========================================================================
//{
//   float total = 0.0;
//   float count = 0.0;
//   vector<plantPart *>::const_iterator part;
//   for (part = myGrainParts.begin(); part != myGrainParts.end(); part++)
//   {
//      total += (*part)->grainWaterContent();
//      count +=1.0;
//   }
//   return divide (total, count, 0.0);
//}

float FruitCohort::dmGrainTotal(void)
   //===========================================================================
{
   float dmTotal = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myGrainParts.begin(); part != myGrainParts.end(); part++)
      dmTotal += (*part)->dmTotal();
   return dmTotal;
}

float FruitCohort::dmGrainWetTotal(void)
   //===========================================================================
{
   float dmTotal = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myGrainParts.begin(); part != myGrainParts.end(); part++)
      dmTotal += (*part)->dmGrainWetTotal();
   return dmTotal;
}

float FruitCohort::dmTotalVeg(void)
   //===========================================================================
{
   float dmTotal = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      dmTotal += (*part)->dmTotal();
   return dmTotal;
}

float FruitCohort::dmGreenGrainTotal(void)
   //===========================================================================
{
   float dmTotal = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myGrainParts.begin(); part != myGrainParts.end(); part++)
      dmTotal += (*part)->dmGreen();
   return dmTotal;
}

float FruitCohort::dmGreenVeg(void)
   //===========================================================================
{
   float dmTotal = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      dmTotal += (*part)->dmGreen();
   return dmTotal;
}

float FruitCohort::dmSenescedVeg(void)
   //===========================================================================
{
   float dmTotal = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      dmTotal += (*part)->dmSenesced();
   return dmTotal;
}


float FruitCohort::nGrainTotal(void)
   //===========================================================================
{
   float nTotal = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myGrainParts.begin(); part != myGrainParts.end(); part++)
      nTotal += (*part)->nTotal();
   return nTotal;
}

float FruitCohort::nTotalVeg(void)
   //===========================================================================
{
   float nTotal = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      nTotal += (*part)->nTotal();
   return nTotal;
}

float FruitCohort::nGreenGrainTotal(void)
   //===========================================================================
{
   float nTotal = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myGrainParts.begin(); part != myGrainParts.end(); part++)
      nTotal += (*part)->nGreen();
   return nTotal;
}

float FruitCohort::nGreenVeg(void)
   //===========================================================================
{
   float nTotal = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      nTotal += (*part)->nGreen();
   return nTotal;
}


float FruitCohort::nSenescedVeg(void)
   //===========================================================================
{
   float nTotal = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      nTotal += (*part)->nSenesced();
   return nTotal;
}


float FruitCohort::nMaxPot(void)
   //===========================================================================
{
   float nMaxPot = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      nMaxPot += (*part)->nMaxPot();
   return nMaxPot;
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

float FruitCohort::nMinPot(void)
   //===========================================================================
{
   float nMinPot = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      nMinPot += (*part)->nMinPot();
   return nMinPot;
}

float FruitCohort::nCapacity(void)
   //============================================================================
{
   NCapacity = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      {
      NCapacity += (*part)->nCapacity();
      }
   return NCapacity;
}

float FruitCohort::pGrainTotal(void)
   //===========================================================================
{
   float pTotal = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myGrainParts.begin(); part != myGrainParts.end(); part++)
      pTotal += (*part)->pTotal();
   return pTotal;
}

float FruitCohort::pTotalVeg(void)
   //===========================================================================
{
   float pTotal = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      pTotal += (*part)->pTotal();
   return pTotal;
}

float FruitCohort::pGreenGrainTotal(void)
   //===========================================================================
{
   float pTotal = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myGrainParts.begin(); part != myGrainParts.end(); part++)
      pTotal += (*part)->pGreen();
   return pTotal;
}

float FruitCohort::pGreenVeg(void)
   //===========================================================================
{
   float pTotal = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      pTotal += (*part)->pGreen();
   return pTotal;
}

float FruitCohort::pSenescedVeg(void)
   //===========================================================================
{
   float pTotal = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      pTotal += (*part)->pSenesced();
   return pTotal;
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
      headWt += (*part)->dmGreen();

   system->sendVariable(qd, headWt);
}

void FruitCohort::get_head_n(protocol::Component *system, protocol::QueryValueData &qd)
   //===========================================================================
{
   system->sendVariable(qd, nGreenGrainTotal() + nGreenVeg());
}

void FruitCohort::get_pod_n(protocol::Component *systemInterface, protocol::QueryValueData &qd)     //put in pod
   //===========================================================================                      //put in pod
{                                                                                                  //put in pod
   systemInterface->sendVariable(qd, podPart->nGreen());   //()                                    //put in pod
}                                                                                                  //put in pod
                                                                                                   //put in pod
void FruitCohort::get_pod_p(protocol::Component *systemInterface, protocol::QueryValueData &qd)     //put in pod
   //===========================================================================                      //put in pod
{                                                                                                  //put in pod
   systemInterface->sendVariable(qd, podPart->pGreen());   //()                                    //put in pod
}                                                                                                  //put in pod

void FruitCohort::get_head_p(protocol::Component *systemInterface, protocol::QueryValueData &qd)
   //===========================================================================
{
   systemInterface->sendVariable(qd, pGreenGrainTotal() + pGreenVeg());  //()
}


//float FruitCohort::total()
//{
//
//  return green.shell + green.meal + senesced.shell + senesced.meal + dead.shell + dead.meal;
//}

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

float FruitCohort::availableRetranslocateN(void)
   //============================================================================
{
   float nAvail = 0.0;
   for (vector<plantPart *>::iterator part = supplyPools.begin(); part != supplyPools.end(); part++)
      nAvail += (*part)->availableRetranslocateN();

   return nAvail;
}

void FruitCohort::doNDemand1(float dlt_dm             // (INPUT)  Whole plant the daily biomass production (g/m^2)
                            , float dlt_dm_pot_rue)  // (INPUT)  Whole plant potential dry matter production (g/m^2)
   //============================================================================
   //     Return plant nitrogen demand for each plant component
{
   NDemand = 0.0;
   NMax = 0.0;
   vector<plantPart *>::iterator part;
   for (part = supplyPools.begin(); part != supplyPools.end(); part++)
      {
      (*part)->doNDemand1(dlt_dm, dlt_dm_pot_rue);
      NDemand += (*part)->nDemand();
      NMax += (*part)->nMax();
      }
}

void FruitCohort::doNDemand1Pot(float dlt_dm             // (INPUT)  Whole plant the daily biomass production (g/m^2)
                               , float dlt_dm_pot_rue)  // (INPUT)  Whole plant potential dry matter production (g/m^2)
   //============================================================================
   //     Return plant nitrogen demand for each plant component
{
   NDemand = 0.0;
   NMax = 0.0;
   vector<plantPart *>::iterator part;
   for (part = supplyPools.begin(); part != supplyPools.end(); part++)
      {
      (*part)->doNDemand1Pot(dlt_dm, dlt_dm_pot_rue);
      NDemand += (*part)->nDemand();
      NMax += (*part)->nMax();
      }
}


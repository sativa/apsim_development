// Modification log
// 6 Aug 97 J. Hargreaves  Implementation

#include "FruitCohort.h"

using namespace std;

// default constructor
//  initialise data members.
FruitCohort::FruitCohort(plantInterface *p, const string &name) : CompositePart(p, name)
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

void FruitCohort::doInit1(protocol::Component *system)
   // ====================================================================
{
   zeroAllGlobals(); zeroDeltas();

   string grainType = system->readParameter ("constants", "grain_part_type");
   if (grainType == "harvest_index")
      grainPart = new fruitGrainPartHI(plant, "grain");
   else if (grainType == "grain_no")
      grainPart = new fruitGrainPartGN(plant, "grain");
   else
     throw std::runtime_error("Unknown grain_part_type '" + grainType + "'");

   podPart = new fruitPodPart(plant, grainPart, "pod");
////   fruitPhenology = constructPhenology(plant, system->readParameter ("constants", "phenology_model"));

   myParts.push_back(podPart);
   myVegParts.push_back(podPart);
   supplyPools.push_back(podPart);
   podPart->doInit1(system);

   myParts.push_back(grainPart);
   myGrainParts.push_back(grainPart);
   grainPart->doInit1(system);
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

void FruitCohort::doRegistrations(protocol::Component *system)
   //===========================================================================
{
   CompositePart::doRegistrations(system);

   system->addGettableVar("dlt_dm_fruit", gDlt_dm, "g/m^2", "Change in dry matter");
   setupGetFunction(system, "head_wt", protocol::DTsingle, false,&FruitCohort::get_head_wt, "g/m^2", "Weight of heads");
   setupGetFunction(system, "head_n", protocol::DTsingle, false,&FruitCohort::get_head_n, "g/m^2", "N in heads");
   setupGetFunction(system, "head_p", protocol::DTsingle, false, &FruitCohort::get_head_p, "g/m^2","P in head");

   for (vector<plantPart *>::iterator part = myParts.begin(); part != myParts.end(); part++)
      (*part)->doRegistrations(system);
}



//float FruitCohort::grainWaterContent(void) const
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

float FruitCohort::dmGrainTotal(void) const
   //===========================================================================
{
   float dmTotal = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myGrainParts.begin(); part != myGrainParts.end(); part++)
      dmTotal += (*part)->dmTotal();
   return dmTotal;
}

float FruitCohort::dmGrainWetTotal(void) const
   //===========================================================================
{
   float dmTotal = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myGrainParts.begin(); part != myGrainParts.end(); part++)
      dmTotal += (*part)->dmGrainWetTotal();
   return dmTotal;
}

float FruitCohort::dmVegTotal(void) const
   //===========================================================================
{
   float dmTotal = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      dmTotal += (*part)->dmTotal();
   return dmTotal;
}

float FruitCohort::dmGreenGrainTotal(void) const
   //===========================================================================
{
   float dmTotal = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myGrainParts.begin(); part != myGrainParts.end(); part++)
      dmTotal += (*part)->dmGreen();
   return dmTotal;
}

float FruitCohort::dmGreenVegTotal(void) const
   //===========================================================================
{
   float dmTotal = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      dmTotal += (*part)->dmGreen();
   return dmTotal;
}

float FruitCohort::dmSenescedVegTotal(void) const
   //===========================================================================
{
   float dmTotal = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      dmTotal += (*part)->dmSenesced();
   return dmTotal;
}


float FruitCohort::dmDeadVegTotal(void) const
   //===========================================================================
{
   float dmTotal = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      dmTotal += (*part)->dmDead();
   return dmTotal;
}


float FruitCohort::nGrainTotal(void) const
   //===========================================================================
{
   float nTotal = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myGrainParts.begin(); part != myGrainParts.end(); part++)
      nTotal += (*part)->nTotal();
   return nTotal;
}

float FruitCohort::nVegTotal(void) const
   //===========================================================================
{
   float nTotal = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      nTotal += (*part)->nTotal();
   return nTotal;
}

float FruitCohort::nGreenGrainTotal(void) const
   //===========================================================================
{
   float nTotal = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myGrainParts.begin(); part != myGrainParts.end(); part++)
      nTotal += (*part)->nGreen();
   return nTotal;
}

float FruitCohort::nGreenVegTotal(void) const
   //===========================================================================
{
   float nTotal = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      nTotal += (*part)->nGreen();
   return nTotal;
}


float FruitCohort::nSenescedVegTotal(void) const
   //===========================================================================
{
   float nTotal = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      nTotal += (*part)->nSenesced();
   return nTotal;
}


float FruitCohort::nDeadVegTotal(void) const
   //===========================================================================
{
   float nTotal = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      nTotal += (*part)->nDead();
   return nTotal;
}


float FruitCohort::nMaxPot(void) const
   //===========================================================================
{
   float nMaxPot = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      nMaxPot += (*part)->nMaxPot();
   return nMaxPot;
}

float FruitCohort::nMax(void)const
   //===========================================================================
{
   float nMax = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      nMax += (*part)->nMax();
   return nMax;
}

float FruitCohort::nMinPot(void)const
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

float FruitCohort::pGrainTotal(void)  const
   //===========================================================================
{
   float pTotal = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myGrainParts.begin(); part != myGrainParts.end(); part++)
      pTotal += (*part)->pTotal();
   return pTotal;
}

float FruitCohort::pVegTotal(void)const
   //===========================================================================
{
   float pTotal = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      pTotal += (*part)->pTotal();
   return pTotal;
}

float FruitCohort::pGreenGrainTotal(void)const
   //===========================================================================
{
   float pTotal = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myGrainParts.begin(); part != myGrainParts.end(); part++)
      pTotal += (*part)->pGreen();
   return pTotal;
}

float FruitCohort::pDeadGrainTotal(void)const
   //===========================================================================
{
   float pTotal = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myGrainParts.begin(); part != myGrainParts.end(); part++)
      pTotal += (*part)->pDead();
   return pTotal;
}

float FruitCohort::pGreenVegTotal(void) const
   //===========================================================================
{
   float pTotal = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      pTotal += (*part)->pGreen();
   return pTotal;
}

float FruitCohort::pSenescedVegTotal(void)const
   //===========================================================================
{
   float pTotal = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      pTotal += (*part)->pSenesced();
   return pTotal;
}


float FruitCohort::pDeadVegTotal(void) const
   //===========================================================================
{
   float pTotal = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      pTotal += (*part)->pDead();
   return pTotal;
}

float FruitCohort::pMaxPot(void) const
   //===========================================================================
{
   float pMaxPot = 0.0;
   vector<plantPart *>::const_iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      pMaxPot += (*part)->pMaxPot();
   return pMaxPot;
}

float FruitCohort::pMinPot(void) const
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
   system->sendVariable(qd, nGreenGrainTotal() + nGreenVegTotal());
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
   systemInterface->sendVariable(qd, pGreenGrainTotal() + pGreenVegTotal());  //()
}


//float FruitCohort::total()
//{
//
//  return green.shell + green.meal + senesced.shell + senesced.meal + dead.shell + dead.meal;
//}

void FruitCohort::display(ostream &os) const
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


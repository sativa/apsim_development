// Modification log
// 6 Aug 97 J. Hargreaves  Implementation

#include "FruitCohort.h"

using namespace std;

// default constructor
// 	initialise data members.
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

void FruitCohort::doInit1 ()
   // ====================================================================
{
   zeroAllGlobals(); zeroDeltas();
   grainPart = new fruitGrainPart(plant, "grain");
   podPart = new fruitPodPart(plant, grainPart, "pod");

   myParts.push_back(podPart);
   myVegParts.push_back(podPart);
   supplyPools.push_back(podPart);
   podPart->doInit1();

   myParts.push_back(grainPart);
   myGrainParts.push_back(grainPart);
   grainPart->doInit1();
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
   //	output << "FruitCohort:" << endl;
   //	output << "   Green cover:    " << pool.coverPod.green << endl;
   //	output << "   Senesced cover: " << pool.coverPod.sen << endl;
   //	output << "   Dead cover:     " << pool.coverPod.dead << endl;
   //	output << endl;
   //	output << "   Green shell:    " << pool.green.shell << endl;
   //	output << "   Green meal:    " << pool.green.meal << endl;
   //	output << "   Senesced shell: " << pool.senesced.shell << endl;
   //	output << "   Senesced meal: " << pool.senesced.meal << endl;
   //	output << "   Dead shell:     " << pool.dead.shell << endl;
   //	output << "   Dead meal:     " << pool.dead.meal << endl << endl;
   output << endl;
   return output;
}


// Assigment operator
//	assign data members of object
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



float FruitCohort::dmGrainTotal(void)
   //===========================================================================
{
   float dmTotal = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myGrainParts.begin(); part != myGrainParts.end(); part++)
      dmTotal += (*part)->dmTotal();
   return dmTotal;
}

float FruitCohort::dmVegTotal(void)
   //===========================================================================
{
   float dmTotal = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      dmTotal += (*part)->dmTotal();
   return dmTotal;
}

float FruitCohort::dmGreenGrainTotal(void)
   //===========================================================================
{
   float dmTotal = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myGrainParts.begin(); part != myGrainParts.end(); part++)
      dmTotal += (*part)->dmGreen();
   return dmTotal;
}

float FruitCohort::dmGreenVegTotal(void)
   //===========================================================================
{
   float dmTotal = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      dmTotal += (*part)->dmGreen();
   return dmTotal;
}

float FruitCohort::dmSenescedVegTotal(void)
   //===========================================================================
{
   float dmTotal = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      dmTotal += (*part)->dmSenesced();
   return dmTotal;
}


float FruitCohort::dmDeadVegTotal(void)
   //===========================================================================
{
   float dmTotal = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      dmTotal += (*part)->dmDead();
   return dmTotal;
}


float FruitCohort::nGrainTotal(void)
   //===========================================================================
{
   float nTotal = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myGrainParts.begin(); part != myGrainParts.end(); part++)
      nTotal += (*part)->nTotal();
   return nTotal;
}

float FruitCohort::nVegTotal(void)
   //===========================================================================
{
   float nTotal = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      nTotal += (*part)->nTotal();
   return nTotal;
}

float FruitCohort::nGreenGrainTotal(void)
   //===========================================================================
{
   float nTotal = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myGrainParts.begin(); part != myGrainParts.end(); part++)
      nTotal += (*part)->nGreen();
   return nTotal;
}

float FruitCohort::nGreenVegTotal(void)
   //===========================================================================
{
   float nTotal = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      nTotal += (*part)->nGreen();
   return nTotal;
}


float FruitCohort::nSenescedVegTotal(void)
   //===========================================================================
{
   float nTotal = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      nTotal += (*part)->nSenesced();
   return nTotal;
}


float FruitCohort::nDeadVegTotal(void)
   //===========================================================================
{
   float nTotal = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      nTotal += (*part)->nDead();
   return nTotal;
}


float FruitCohort::nMaxPot(void)
   //===========================================================================
{
   float nMaxPot = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      nMaxPot += (*part)->nMaxPot();
   return nMaxPot;
}

float FruitCohort::nMax(void)
   //===========================================================================
{
   NMax = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      NMax += (*part)->nMax();
   return NMax;
}

float FruitCohort::nMinPot(void)
   //===========================================================================
{
   float nMinPot = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      nMinPot += (*part)->nMinPot();
   return nMinPot;
}

float FruitCohort::nDemandGrain2(void)                                             //remove  problem
   //===========================================================================  //remove
{                                                                                 //remove
   return grainPart->nDemand2();                                                  //remove
}                                                                                 //remove


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

////void FruitCohort::doNPartition(float nSupply, float n_demand_sum, float n_capacity_sum)
////   //============================================================================
////{
////   plantPart::doNPartition(nSupply, n_demand_sum, n_capacity_sum);             //FIXME need to do this differently
////
////   n_demand_sum = nDemand();
////   n_capacity_sum = nCapacity();
////
////   vector <plantPart *>::iterator part;
////   for (part = myParts.begin(); part != myParts.end(); part++)
////      (*part)->doNPartition(dlt.n_green, n_demand_sum, n_capacity_sum);
////
////   float dlt_n_green_sum = dltNGreen();
////   if (!reals_are_equal(dlt_n_green_sum - dlt.n_green, 0.0))
////      {
////      string msg = c.name + " dlt_n_green mass balance is off: dlt_n_green_sum ="
////                  + ftoa(dlt_n_green_sum, ".6")
////                  + " vs nSupply ="
////                  + ftoa(nSupply, ".6");
////      plant->warningError(msg.c_str());
////      }
////}


float FruitCohort::pGrainTotal(void)
   //===========================================================================
{
   float pTotal = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myGrainParts.begin(); part != myGrainParts.end(); part++)
      pTotal += (*part)->pTotal();
   return pTotal;
}

float FruitCohort::pVegTotal(void)
   //===========================================================================
{
   float pTotal = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      pTotal += (*part)->pTotal();
   return pTotal;
}

float FruitCohort::pGreenGrainTotal(void)
   //===========================================================================
{
   float pTotal = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myGrainParts.begin(); part != myGrainParts.end(); part++)
      pTotal += (*part)->pGreen();
   return pTotal;
}

float FruitCohort::pDeadGrainTotal(void)
   //===========================================================================
{
   float pTotal = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myGrainParts.begin(); part != myGrainParts.end(); part++)
      pTotal += (*part)->pDead();
   return pTotal;
}

float FruitCohort::pGreenVegTotal(void)
   //===========================================================================
{
   float pTotal = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      pTotal += (*part)->pGreen();
   return pTotal;
}


float FruitCohort::pSenescedGrainTotal(void)                                      //remove
   //=========================================================================== //remove
{                                                                                //remove
   float pTotal = 0.0;                                                           //remove
   vector<plantPart *>::iterator part;                                           //remove
   for (part = myGrainParts.begin(); part != myGrainParts.end(); part++)         //remove
      pTotal += (*part)->pSenesced();                                            //remove
   return pTotal;                                                                //remove
}                                                                                //remove

float FruitCohort::pSenescedVegTotal(void)
   //===========================================================================
{
   float pTotal = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      pTotal += (*part)->pSenesced();
   return pTotal;
}


float FruitCohort::pDeadVegTotal(void)
   //===========================================================================
{
   float pTotal = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      pTotal += (*part)->pDead();
   return pTotal;
}


float FruitCohort::pConcGrain(void)                                               //remove
   //=========================================================================== //remove
{                                                                                //remove
   return grainPart->pConcPercent();                                                    //remove
}                                                                                //remove

float FruitCohort::pConcGrainTotal(void)                                           //remove
   //===========================================================================  //remove
{                                                                                 //remove
   return grainPart->pConcGrainTotal();                                           //remove
}                                                                                 //remove

float FruitCohort::pMaxPot(void)
   //===========================================================================
{
   float pMaxPot = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      pMaxPot += (*part)->pMaxPot();
   return pMaxPot;
}

float FruitCohort::pMinPot(void)
   //===========================================================================
{
   float pMinPot = 0.0;
   vector<plantPart *>::iterator part;
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




void FruitCohort::doNSenescedRetrans(float navail, float n_demand_tot)              //remove  problem
   //===========================================================================   //remove
{                                                                                  //remove
   dlt.n_senesced_retrans = 0.0;                                                   //remove
   vector<plantPart *>::iterator myPart;                                           //remove
   for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)               //remove
      {                                                                            //remove
      (*myPart)->doNSenescedRetrans(navail, n_demand_tot);                         //remove
      dlt.n_senesced_retrans +=(*myPart)->dltNSenescedRetrans();                   //remove
      }                                                                            //remove
}                                                                                  //remove


//float FruitCohort::total()
//{
//
//	return green.shell + green.meal + senesced.shell + senesced.meal + dead.shell + dead.meal;
//}

void FruitCohort::display(ostream &os) const
{
   //	os << "FruitCohort:" << endl;
   //	os << "Green cover:    " << coverPod.green << endl;
   //	os << "Senesced cover: " << coverPod.sen << endl;
   //	os << "Dead cover:     " << coverPod.dead << endl;
   //	os << "Green shell: " << green.shell << endl;
   //	os << "Green meal: " << green.meal << endl;
   //	os << "Senesced shell: " << senesced.shell << endl;
   //	os << "Senesced meal: " << senesced.meal << endl;
   //	os << "Dead shell: " << dead.shell << endl;
   //	os << "Dead meal: " << dead.meal << endl << endl;
   os << endl;
}


float FruitCohort::calcCover (float canopy_fac) {return  podPart->calcCover(canopy_fac);}    //remove problem

float FruitCohort::grainNConcPercent(void) {return grainPart->nConcPercent();}     //remove problem

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


void FruitCohort::doNFixRetranslocate(float NFix, float NDemandDifferentialTotal)
//=======================================================================================
{
    plantPart::doNFixRetranslocate(NFix, NDemandDifferentialTotal);                        // FIXME - this needs to be done another way

    float n_demand_differential = nDemandDifferential ();

        // now distribute the n fixed to plant parts

    NFix = NFix * divide (nDemandDifferential(), NDemandDifferentialTotal, 0.0);
    for (vector<plantPart *>::iterator part = myParts.begin(); part != myParts.end(); part++)      //FIXME later
       (*part)->doNFixRetranslocate (NFix, n_demand_differential);
}

////void FruitCohort::doNRetranslocate( float N_supply, float g_grain_n_demand)
////   //============================================================================
////   //     Calculate the nitrogen retranslocation from the various plant parts to the grain.
////{
////
////   // available N does not include grain
////   // this should not presume grain is 0.
////
////   // get actual grain N uptake by retransolcation
////   // limit retranslocation to total available N
////
////   vector<plantPart *>::iterator part;
////   for (part = myParts.begin(); part != myParts.end(); part++)
////      (*part)->doNRetranslocate(N_supply, g_grain_n_demand);
////
////////   dlt.n_retrans = 0.0;
////////   for (part = supplyPools.begin(); part != supplyPools.end(); part++)
////////      dlt.n_retrans += (*part)->dlt.n_retrans;
////}

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

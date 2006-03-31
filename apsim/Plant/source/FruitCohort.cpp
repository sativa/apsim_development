
// Modification log
// 6 Aug 97 J. Hargreaves  Implementation

#define YES 1
#define NO 0
#define TEST_FruitCohort NO					// build unit test?
#include "FruitCohort.h"


using namespace std;

void push_routine (const char *) {};
void pop_routine (const char *) {};

inline bool floatsAreEqual(float A, float B, float C) {return(fabs(A-B)<C);}

// default constructor
// 	initialise data members.
FruitCohort::FruitCohort(plantInterface *p, const string &name) : CompositePart(p, name)
{
   //    zeroAllGlobals();
}

// destructor
FruitCohort::~FruitCohort()
{
   if (podPart) delete podPart;
   if (grainPart) delete grainPart;
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

// copy constructor
//	copy data members of object
//FruitCohort::FruitCohort(const FruitCohort &FruitCohort)
////===========================================================================
//{
//	throw std::invalid_argument("Copy constructor NI for FruitCohort");
//}


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
      headWt += (*part)->DMGreen;

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




void FruitCohort::doInit1 ()
   // ====================================================================
{
   podPart = new fruitPodPart(plant, "pod");
   myParts.push_back(podPart);
   myVegParts.push_back(podPart);
   supplyPools.push_back(podPart);

   grainPart = new fruitGrainPart(plant, "grain");
   myParts.push_back(grainPart);
   myGrainParts.push_back(grainPart);

   grainPart->doInit1();

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
   //       (OUTPUT) assimilate demand for reproductive part (g/m^2)
   // calculate demands of reproductive parts

   float dm_grain_demand = grainPart->calcDmDemand();            //FIXME throughout - dm_grain_demand should be gDlt_dm_grain_demand. Leave asis for compatability
   podPart->doDmDemand(dm_grain_demand, dlt_dm_veg_supply);      //FIXME this function needs to be handled in composite
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



//============================================================================

#if TEST_FruitCohort							// build unit test?


// FruitCohort class test harness

// Tests default constructor, copy constructor, assignment operator and
// each of the get and set functions.  Does not test the destructor.

// Modification log
// 6 Aug 97  J. Hargreaves    Initial implementation


#ifndef FruitCohort_H
#include "FruitCohort.h"
#endif

int main()
{
   cout << "FruitCohort test started" << endl;

   FruitCohort p, *aPtr = &p;

   //	cout << endl << "Test set and get functions:" << endl;
   //	p.setValue(10.0, 2.0, 20.0, 3.0, 30.0, 4.0);
   //	if (p.total() != 69.0)
   //		cout << "setValue(10.0, 2.0, 20.0, 3.0, 30.0, 4.0) / Total() test FAILED"
   //		<< endl;
   //
   //	cout << endl << "Test default constructor:" << endl;
   //	FruitCohort q;                           						// run default constructor
   //	if (q.total() != 0.0)
   //		cout << "default constructor test FAILED" << endl;
   //
   //	cout << endl << "Test constructor:" << endl;
   //	FruitCohort a(1.0, 2.0, 3.0, 4.0, 5.0, 6.0);                           						// run default constructor
   //	if (a.total() != 21.0)
   //		cout << "constructor test FAILED" << endl;
   //
   //	cout << endl << "Test copy constructor:" << endl;
   //	FruitCohort s = p;                       // run copy constructor
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
   //	FruitCohort k = p * s;
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
   //	FruitCohort t;
   //	t.setValue(50.0, 5.0, 60.0, 6.0, 70.0, 7.0); // change object
   //	cout << endl << "Display FruitCohort t" << endl;
   //	t.display();
   //
   //	FruitCohort x;
   //	x.setValue(50.0, 5.0, 60.0, 6.0, 70.0, 7.0); // change object
   //
   //	cout << endl << "Display FruitCohort x - static binding" << endl;
   //	x.display();
   //
   //	cout << endl << "Display FruitCohort x - dynamic binding" << endl;
   //	FruitCohort *FruitCohortPtr = &x;
   //	FruitCohortPtr->display();

   cout << endl << "FruitCohort test finished" << endl;
   return 0;
}

#endif



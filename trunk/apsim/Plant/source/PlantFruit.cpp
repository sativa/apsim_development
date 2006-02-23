
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
}

// destructor
PlantFruit::~PlantFruit()
{
   if (podPart) delete podPart;
   if (grainPart) delete grainPart;
}

ostream &operator<<(ostream &output, const PlantFruit &pool)
{
   //	output << "PlantFruit:" << endl;
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
//PlantFruit::PlantFruit(const PlantFruit &PlantFruit)
////===========================================================================
//{
//	throw std::invalid_argument("Copy constructor NI for plantFruit");
//}


// Assigment operator
//	assign data members of object
const PlantFruit &PlantFruit::operator=(const PlantFruit &other)
   //===========================================================================
{
   throw std::invalid_argument("Assignment operator NI for plantFruit");
}

void PlantFruit::doRegistrations(protocol::Component *system)
   //===========================================================================
{
   plantPart::doRegistrations(system);

   system->addGettableVar("dlt_dm_fruit", gDlt_dm, "g/m^2", "Change in dry matter");
   setupGetFunction(system, "head_wt", protocol::DTsingle, false,&PlantFruit::get_head_wt, "g/m^2", "Weight of heads");
   setupGetFunction(system, "head_n", protocol::DTsingle, false,&PlantFruit::get_head_n, "g/m^2", "N in heads");
   setupGetFunction(system, "head_p", protocol::DTsingle, false, &PlantFruit::get_head_p, "g/m^2","P in head");

   for (vector<plantPart *>::iterator t = myParts.begin();
        t != myParts.end();
        t++)
      (*t)->doRegistrations(system);
}

float PlantFruit::dmTotal(void)
   //===========================================================================
{
   float dmTotal = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      dmTotal += (*part)->dmTotal();
   return dmTotal;
}

float PlantFruit::dmGreenDemand(void)
   //===========================================================================
{
   float dmGreenDemand = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      dmGreenDemand += (*part)->dmGreenDemand();
   return dmGreenDemand;
}

float PlantFruit::grainWt(void)
   //===========================================================================
{
   return grainPart->grainWt();
}

float PlantFruit::dmGrainTotal(void)
   //===========================================================================
{
   float dmTotal = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myGrainParts.begin(); part != myGrainParts.end(); part++)
      dmTotal += (*part)->dmTotal();
   return dmTotal;
}

float PlantFruit::dmVegTotal(void)
   //===========================================================================
{
   float dmTotal = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      dmTotal += (*part)->dmTotal();
   return dmTotal;
}

float PlantFruit::dmGreenGrainTotal(void)
   //===========================================================================
{
   float dmTotal = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myGrainParts.begin(); part != myGrainParts.end(); part++)
      dmTotal += (*part)->dmGreen();
   return dmTotal;
}

float PlantFruit::dmGreenVegTotal(void)
   //===========================================================================
{
   float dmTotal = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      dmTotal += (*part)->dmGreen();
   return dmTotal;
}

float PlantFruit::dmSenescedVegTotal(void)
   //===========================================================================
{
   float dmTotal = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      dmTotal += (*part)->dmSenesced();
   return dmTotal;
}

float PlantFruit::dmSenesced(void)
   //===========================================================================
{
   DMSenesced = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      DMSenesced += (*part)->dmSenesced();
   return DMSenesced;
}

float PlantFruit::dmDeadVegTotal(void)
   //===========================================================================
{
   float dmTotal = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      dmTotal += (*part)->dmDead();
   return dmTotal;
}

float PlantFruit::dmDead(void)
   //===========================================================================
{
   DMDead = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      DMDead += (*part)->dmDead();
   return DMDead;
}

float PlantFruit::nTotal(void)
   //===========================================================================
{
   float nTotal = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      nTotal += (*part)->nTotal();
   return nTotal;
}

float PlantFruit::nGrainTotal(void)
   //===========================================================================
{
   float nTotal = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myGrainParts.begin(); part != myGrainParts.end(); part++)
      nTotal += (*part)->nTotal();
   return nTotal;
}

float PlantFruit::nVegTotal(void)
   //===========================================================================
{
   float nTotal = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      nTotal += (*part)->nTotal();
   return nTotal;
}

float PlantFruit::nGreenGrainTotal(void)
   //===========================================================================
{
   float nTotal = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myGrainParts.begin(); part != myGrainParts.end(); part++)
      nTotal += (*part)->nGreen();
   return nTotal;
}

float PlantFruit::nGreenVegTotal(void)
   //===========================================================================
{
   float nTotal = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      nTotal += (*part)->nGreen();
   return nTotal;
}

float PlantFruit::nGreen(void)
   //===========================================================================
{
   NGreen = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      NGreen += (*part)->nGreen();
   return NGreen;
}

float PlantFruit::nSenescedVegTotal(void)
   //===========================================================================
{
   float nTotal = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      nTotal += (*part)->nSenesced();
   return nTotal;
}

float PlantFruit::nSenesced(void)
   //===========================================================================
{
   NSenesced = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      NSenesced += (*part)->nSenesced();
   return NSenesced;
}

float PlantFruit::nDeadVegTotal(void)
   //===========================================================================
{
   float nTotal = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      nTotal += (*part)->nDead();
   return nTotal;
}

float PlantFruit::nDead(void)
   //===========================================================================
{
   NDead = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      NDead += (*part)->nDead();
   return NDead;
}

float PlantFruit::nMaxPot(void)
   //===========================================================================
{
   float nMaxPot = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      nMaxPot += (*part)->nMaxPot();
   return nMaxPot;
}

float PlantFruit::nMax(void)
   //===========================================================================
{
   NMax = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      NMax += (*part)->nMax();
   return NMax;
}

float PlantFruit::nMinPot(void)
   //===========================================================================
{
   float nMinPot = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      nMinPot += (*part)->nMinPot();
   return nMinPot;
}

float PlantFruit::nConcGrain(void)
   //===========================================================================
{
   return grainPart->nConc();
}


float PlantFruit::nGrainDemand2(void)
   //===========================================================================
{
   return grainPart->nDemand2();
}

float PlantFruit::soilNDemand(void)
   //============================================================================
{
   SoilNDemand = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      {
      SoilNDemand += (*part)->soilNDemand();
      }
   return SoilNDemand;
}

float PlantFruit::nDemand(void)
   //============================================================================
{
   float n_demand = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      {
      n_demand += (*part)->nDemand();
      }
   return n_demand;
}

float PlantFruit::nCapacity(void)
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

void PlantFruit::nPartition(float nSupply)
   //============================================================================
{
   float dlt_n_green;
   vector<plantPart *>::iterator part;

   float n_demand_sum = 0.0;
   for (part = myParts.begin(); part != myParts.end(); part++)
      n_demand_sum += (*part)->nDemand();

   float n_excess = nSupply - n_demand_sum;
   n_excess = l_bound (n_excess, 0.0);

   float n_capacity_sum = 0.0;
   for (part = myParts.begin(); part != myParts.end(); part++)
      n_capacity_sum += (*part)->nCapacity();

   for (part = myParts.begin(); part != myParts.end(); part++)
      {
      if (n_excess>0.0)
         {
         float plant_part_fract = divide ((*part)->nCapacity(), n_capacity_sum, 0.0);
         dlt_n_green = (*part)->nDemand() + n_excess * plant_part_fract;
         }
      else
         {
         float plant_part_fract = divide ((*part)->nDemand(), n_demand_sum, 0.0);
         dlt_n_green = nSupply * plant_part_fract;
         }
      (*part)->nPartition(dlt_n_green);
      }

   float dlt_n_green_sum = 0.0;
   for (part = myParts.begin(); part != myParts.end(); part++)
      dlt_n_green_sum += (*part)->dltNGreen();

   dlt.n_green = dlt_n_green_sum;

   if (!reals_are_equal(dlt_n_green_sum - nSupply, 0.0))
      {
      string msg ="Fruit dlt_n_green mass balance is off: dlt_n_green_sum ="
         + ftoa(dlt_n_green_sum, ".6")
         + " vs nSupply ="
         + ftoa(nSupply, ".6");
      parentPlant->warningError(msg.c_str());
      }

}
void PlantFruit::nFix(float nSupply)
   //============================================================================
{
   float n_demand_sum = 0.0;
   float dlt_n_green_sum = 0.0;
   vector<plantPart *>::iterator part;

   for (part = myParts.begin(); part != myParts.end(); part++)
      {
      n_demand_sum += (*part)->nDemand();
      dlt_n_green_sum += (*part)->dltNGreen();
      }

   float n_fix_demand_tot = l_bound (n_demand_sum - dlt_n_green_sum, 0.0);

   for (part = myParts.begin(); part != myParts.end(); part++)
      {
      float fix_demand = l_bound ((*part)->nDemand() - (*part)->dltNGreen(), 0.0);
      float fix_part_fract = divide (fix_demand, n_fix_demand_tot, 0.0);
      float dlt_n_green = fix_part_fract * nSupply;
      (*part)->nFix(dlt_n_green);
      }
}

float PlantFruit::pTotal(void)
   //===========================================================================
{
   float pTotal = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      pTotal += (*part)->pTotal();
   return pTotal;
}

float PlantFruit::pGrainTotal(void)
   //===========================================================================
{
   float pTotal = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myGrainParts.begin(); part != myGrainParts.end(); part++)
      pTotal += (*part)->pTotal();
   return pTotal;
}

float PlantFruit::pVegTotal(void)
   //===========================================================================
{
   float pTotal = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      pTotal += (*part)->pTotal();
   return pTotal;
}

float PlantFruit::pGreenGrainTotal(void)
   //===========================================================================
{
   float pTotal = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myGrainParts.begin(); part != myGrainParts.end(); part++)
      pTotal += (*part)->pGreen();
   return pTotal;
}

float PlantFruit::pDeadGrainTotal(void)
   //===========================================================================
{
   float pTotal = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myGrainParts.begin(); part != myGrainParts.end(); part++)
      pTotal += (*part)->pDead();
   return pTotal;
}

float PlantFruit::pGreenVegTotal(void)
   //===========================================================================
{
   float pTotal = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      pTotal += (*part)->pGreen();
   return pTotal;
}

float PlantFruit::pGreen(void)
   //===========================================================================
{
   float pTotal = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      pTotal += (*part)->pGreen();
   return pTotal;
}

float PlantFruit::pSenescedGrainTotal(void)
   //===========================================================================
{
   float pTotal = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myGrainParts.begin(); part != myGrainParts.end(); part++)
      pTotal += (*part)->pSenesced();
   return pTotal;
}

float PlantFruit::pSenescedVegTotal(void)
   //===========================================================================
{
   float pTotal = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      pTotal += (*part)->pSenesced();
   return pTotal;
}

float PlantFruit::pSenesced(void)
   //===========================================================================
{
   float pTotal = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      pTotal += (*part)->pSenesced();
   return pTotal;
}

float PlantFruit::pDeadVegTotal(void)
   //===========================================================================
{
   float pTotal = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      pTotal += (*part)->pDead();
   return pTotal;
}

float PlantFruit::pDead(void)
   //===========================================================================
{
   float pTotal = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      pTotal += (*part)->pDead();
   return pTotal;
}

float PlantFruit::pConcGrain(void)
   //===========================================================================
{
   return grainPart->pConc();
}

float PlantFruit::pConcGrainTotal(void)
   //===========================================================================
{
   return grainPart->pConcTotal();
}

float PlantFruit::pMaxPot(void)
   //===========================================================================
{
   float pMaxPot = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      pMaxPot += (*part)->pMaxPot();
   return pMaxPot;
}

float PlantFruit::pMinPot(void)
   //===========================================================================
{
   float pMinPot = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      pMinPot += (*part)->pMinPot();
   return pMinPot;
}

void PlantFruit::get_head_wt(protocol::Component *system, protocol::QueryValueData &qd)
   //===========================================================================
{
   float headWt = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      headWt += (*part)->DMGreen;

   system->sendVariable(qd, headWt);
}

void PlantFruit::get_head_n(protocol::Component *system, protocol::QueryValueData &qd)
   //===========================================================================
{
   system->sendVariable(qd, nGreenGrainTotal() + nGreenVegTotal());
}

//===========================================================================                      //put in pod
void PlantFruit::get_pod_n(protocol::Component *systemInterface, protocol::QueryValueData &qd)     //put in pod
   //===========================================================================                      //put in pod
{                                                                                                  //put in pod
   systemInterface->sendVariable(qd, podPart->nGreen());   //()                                    //put in pod
}                                                                                                  //put in pod
                                                                                                   //put in pod
//===========================================================================                      //put in pod
void PlantFruit::get_pod_p(protocol::Component *systemInterface, protocol::QueryValueData &qd)     //put in pod
   //===========================================================================                      //put in pod
{                                                                                                  //put in pod
   systemInterface->sendVariable(qd, podPart->pGreen());   //()                                    //put in pod
}                                                                                                  //put in pod

void PlantFruit::get_head_p(protocol::Component *systemInterface, protocol::QueryValueData &qd)
   //===========================================================================
{
   systemInterface->sendVariable(qd, pGreenGrainTotal() + pGreenVegTotal());  //()
}

void PlantFruit::get_p_demand(vector<float> &p_demand)
   //===========================================================================
{
   vector<plantPart *>::iterator myPart;
   for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
      (*myPart)->get_p_demand(p_demand);
}

void PlantFruit::get_dlt_p_green(vector<float> &dlt_p_green)
   //===========================================================================
{
   vector<plantPart *>::iterator myPart;
   for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
      (*myPart)->get_dlt_p_green(dlt_p_green);
}

void PlantFruit::get_p_green(vector<float> &p_green)
   //===========================================================================
{
   vector<plantPart *>::iterator myPart;
   for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
      (*myPart)->get_p_green(p_green);
}

void PlantFruit::get_dlt_p_retrans(vector<float> &dlt_p_retrans)
   //===========================================================================
{
   vector<plantPart *>::iterator myPart;
   for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
      (*myPart)->get_dlt_p_retrans(dlt_p_retrans);
}

void PlantFruit::get_dm_plant_min(vector<float> &dm_min)
{
   vector<plantPart *>::iterator myPart;
   for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
      (*myPart)->get_dm_plant_min(dm_min);
}

void PlantFruit::get_dm_green(vector<float> &dm_green)
{
   vector<plantPart *>::iterator myPart;
   for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
      (*myPart)->get_dm_green(dm_green);
}

void PlantFruit::get_dm_dead(vector<float> &dm_dead)
{
   vector<plantPart *>::iterator myPart;
   for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
      (*myPart)->get_dm_dead(dm_dead);
}

void PlantFruit::get_dm_senesced(vector<float> &dm_senesced)
{
   vector<plantPart *>::iterator myPart;
   for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
      (*myPart)->get_dm_senesced(dm_senesced);
}

void PlantFruit::get_dlt_dm_green(vector<float> &dlt_dm_green)
{
   vector<plantPart *>::iterator myPart;
   for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
      (*myPart)->get_dlt_dm_green(dlt_dm_green);
}

void PlantFruit::get_dlt_dm_green_retrans(vector<float> &dlt_dm_green_retrans)
{
   vector<plantPart *>::iterator myPart;
   for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
      (*myPart)->get_dlt_dm_green_retrans(dlt_dm_green_retrans);
}

void PlantFruit::get_dlt_dm_detached(vector<float> &dlt_dm_detached)
{
   vector<plantPart *>::iterator myPart;
   for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
      (*myPart)->get_dlt_dm_detached(dlt_dm_detached);
}

void PlantFruit::get_dlt_dm_senesced(vector<float> &dlt_dm_senesced)
{
   vector<plantPart *>::iterator myPart;
   for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
      (*myPart)->get_dlt_dm_senesced(dlt_dm_senesced);
}

void PlantFruit::get_dlt_dm_dead_detached(vector<float> &dlt_dm_dead_detached)
{
   vector<plantPart *>::iterator myPart;
   for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
      (*myPart)->get_dlt_dm_dead_detached(dlt_dm_dead_detached);
}

void PlantFruit::get_dlt_dm_green_dead(vector<float> &dlt_dm_green_dead)
{
   vector<plantPart *>::iterator myPart;
   for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
      (*myPart)->get_dlt_dm_green_dead(dlt_dm_green_dead);
}

void PlantFruit::get_dlt_dm_senesced_dead(vector<float> &dlt_dm_senesced_dead)
{
   vector<plantPart *>::iterator myPart;
   for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
      (*myPart)->get_dlt_dm_senesced_dead(dlt_dm_senesced_dead);
}

void PlantFruit::get_n_demanded(vector<float> &n_demand)
{
   vector<plantPart *>::iterator myPart;
   for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
      (*myPart)->get_n_demanded(n_demand);
}

void PlantFruit::get_n_green(vector<float> &n_green)
{
   vector<plantPart *>::iterator myPart;
   for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
      (*myPart)->get_n_green(n_green);
}

void PlantFruit::get_n_senesced(vector<float> &n_senesced)
{
   vector<plantPart *>::iterator myPart;
   for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
      (*myPart)->get_n_senesced(n_senesced);
}

void PlantFruit::get_n_dead(vector<float> &n_dead)
{
   vector<plantPart *>::iterator myPart;
   for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
      (*myPart)->get_n_dead(n_dead);
}

void PlantFruit::get_dlt_n_green(vector<float> &n_green)
{
   vector<plantPart *>::iterator myPart;
   for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
      (*myPart)->get_dlt_n_green(n_green);
}

void PlantFruit::get_dlt_n_dead(vector<float> &n_dead)
{
   vector<plantPart *>::iterator myPart;
   for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
      (*myPart)->get_dlt_n_dead(n_dead);
}

void PlantFruit::get_dlt_n_retrans(vector<float> &n_retrans)
{
   vector<plantPart *>::iterator myPart;
   for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
      (*myPart)->get_dlt_n_retrans(n_retrans);
}

void PlantFruit::get_dlt_n_senesced(vector<float> &n_senesced)
{
   vector<plantPart *>::iterator myPart;
   for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
      (*myPart)->get_dlt_n_senesced(n_senesced);
}

void PlantFruit::get_dlt_n_senesced_dead(vector<float> &dlt_n_senesced_dead)
{
   vector<plantPart *>::iterator myPart;
   for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
      (*myPart)->get_dlt_n_senesced_dead(dlt_n_senesced_dead);
}

void PlantFruit::get_dlt_n_senesced_retrans(vector<float> &n_senesced_retrans)
{
   vector<plantPart *>::iterator myPart;
   for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
      (*myPart)->get_dlt_n_senesced_retrans(n_senesced_retrans);
}

void PlantFruit::get_dlt_n_senesced_trans(vector<float> &n_senesced_trans)
{
   vector<plantPart *>::iterator myPart;
   for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
      (*myPart)->get_dlt_n_senesced_trans(n_senesced_trans);
}

void PlantFruit::get_dlt_n_detached(vector<float> &n_detached)
{
   vector<plantPart *>::iterator myPart;
   for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
      (*myPart)->get_dlt_n_detached(n_detached);
}

void PlantFruit::get_dlt_n_dead_detached(vector<float> &n_dead_detached)
{
   vector<plantPart *>::iterator myPart;
   for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
      (*myPart)->get_dlt_n_dead_detached(n_dead_detached);
}

void PlantFruit::get_p_dead(vector<float> &p_dead)
{
   vector<plantPart *>::iterator myPart;
   for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
      (*myPart)->get_p_dead(p_dead);
}

void PlantFruit::get_p_sen(vector<float> &p_sen)
{
   vector<plantPart *>::iterator myPart;
   for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
      (*myPart)->get_p_sen(p_sen);
}

void PlantFruit::get_dlt_p_detached(vector<float> &dlt_p_detached)
{
   vector<plantPart *>::iterator myPart;
   for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
      (*myPart)->get_dlt_p_detached(dlt_p_detached);
}

void PlantFruit::get_dlt_p_dead(vector<float> &dlt_p_dead)
{
   vector<plantPart *>::iterator myPart;
   for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
      (*myPart)->get_dlt_p_dead(dlt_p_dead);
}

void PlantFruit::get_dlt_p_sen(vector<float> &dlt_p_sen)
{
   vector<plantPart *>::iterator myPart;
   for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
      (*myPart)->get_dlt_p_sen(dlt_p_sen);
}


void PlantFruit::grain_number (void)
   //===========================================================================
   //       Calculate Grain Numer
{
   grainPart->grain_number();    // Calculate grain no
   return;
}

void PlantFruit::doTick(protocol::timeType &tick)
   //===========================================================================
{
   grainPart->doTick(tick);
}

// Field a NewMet event
void PlantFruit::doNewMet(protocol::newmetType &newmet)
   //===========================================================================
{
   grainPart->doNewMet(newmet);
}

void PlantFruit::readCultivarParameters (protocol::Component *system, const string &cultivar)
   //===========================================================================
{
   system->writeString (" - reading fruit cultivar parameters");

   for (vector<plantPart *>::iterator t = myParts.begin();
        t != myParts.end();
        t++)
      (*t)->readCultivarParameters(system, cultivar);
}

void PlantFruit::writeCultivarInfo (protocol::Component *system)
   //===========================================================================
{
   // report
   grainPart->writeCultivarInfo(system);
}

void PlantFruit::onDayOf(const string &stage)
{
   for (vector<plantPart *>::iterator t = myParts.begin();
        t != myParts.end();
        t++)
      (*t)->onDayOf(stage);

   refreshStates();
}

void PlantFruit::morphology(void)
{
}

void PlantFruit::zeroAllGlobals(void)
{
   plantPart::zeroAllGlobals();
   gHasreadconstants = false;

   for (vector<plantPart *>::iterator t = myParts.begin();
        t != myParts.end();
        t++)
      (*t)->zeroAllGlobals();
}

void PlantFruit::zeroDeltas(void)
{
   plantPart::zeroDeltas();
   gDlt_dm = 0.0;

   for (vector<plantPart *>::iterator t = myParts.begin();
        t != myParts.end();
        t++)
      (*t)->zeroDeltas();
}

void PlantFruit::zeroDltDmGreen(void)
{
   dlt.dm_green = 0.0;

   for (vector<plantPart *>::iterator t = myParts.begin();
        t != myParts.end();
        t++)
      (*t)->zeroDltDmGreen();
}

void PlantFruit::zeroDltNSenescedTrans(void)
{
   dlt.n_senesced_trans = 0.0;

   for (vector<plantPart *>::iterator t = myParts.begin();
        t != myParts.end();
        t++)
      (*t)->zeroDltNSenescedTrans();
}

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

   refreshStates();
}

void PlantFruit::onKillStem(void)
   // ====================================================================
{
   for (vector<plantPart *>::iterator part = myParts.begin();
        part != myParts.end();
        part++)
      (*part)->onKillStem();

   refreshStates();
}

void PlantFruit::onEndCrop(vector<string> &dm_type,
                           vector<float> &dlt_crop_dm,
                           vector<float> &dlt_dm_n,
                           vector<float> &dlt_dm_p,
                           vector<float> &fraction_to_residue)
   // ====================================================================
{
   for (vector<plantPart *>::iterator part = myParts.begin();
        part != myParts.end();
        part++)
      (*part)->onEndCrop(dm_type,
                         dlt_crop_dm,
                         dlt_dm_n,
                         dlt_dm_p,
                         fraction_to_residue);
   refreshStates();
}

void PlantFruit::refreshStates(void)
   // ====================================================================
{
   DMDead     = 0.0;
   DMSenesced = 0.0;
   DMGreen    = 0.0;

   NDead     = 0.0;
   NSenesced = 0.0;
   NGreen    = 0.0;

   PDead  = 0.0;
   PSen   = 0.0;
   PGreen = 0.0;

   for (vector<plantPart *>::iterator part = myParts.begin();
        part != myParts.end();
        part++)
      {
      DMDead += (*part)->DMDead;
      DMGreen += (*part)->DMGreen;
      DMSenesced += (*part)->DMSenesced;

      NDead += (*part)->NDead;
      NGreen += (*part)->NGreen;
      NSenesced += (*part)->NSenesced;

      PDead += (*part)->PDead;
      PGreen += (*part)->PGreen;
      PSen += (*part)->PSen;
      }
}

void PlantFruit::doInit (PlantComponent *systemInterface, PlantPhenology *plantPhenology)
   // ====================================================================
{
   parentPlant = systemInterface;
   phenology = plantPhenology;

   for (vector<plantPart *>::iterator part = myParts.begin();
        part != myParts.end();
        part++)
      (*part)->doInit (systemInterface, plantPhenology);

}

void PlantFruit::doInit1 ()
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


void PlantFruit::readConstants(protocol::Component *system, const string &section)
   //===========================================================================
{
   for (vector<plantPart *>::iterator t = myParts.begin();
        t != myParts.end();
        t++)
      (*t)->readConstants(system, section);

}

void PlantFruit::readSpeciesParameters(protocol::Component *system, vector<string> &sections)
   //===========================================================================
{
   gHasreadconstants = true;

   for (vector<plantPart *>::iterator part = myParts.begin();
        part != myParts.end();
        part++)
      (*part)->readSpeciesParameters(system, sections);

}


float PlantFruit::dmGreen(void)
   //===========================================================================
{
   DMGreen = 0.0;

   vector<plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      DMGreen +=(*part)->DMGreen;

   return DMGreen;
}

float PlantFruit::dltDmGreen(void)
   //===========================================================================
{
   dlt.dm_green = 0.0;

   vector<plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      dlt.dm_green +=(*part)->dlt.dm_green;

   return dlt.dm_green;
}

void PlantFruit::doNSenescedRetrans(float navail, float n_demand_tot)
   //===========================================================================
{
   dlt.n_senesced_retrans = 0.0;
   vector<plantPart *>::iterator myPart;
   for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
      {
      (*myPart)->doNSenescedRetrans(navail, n_demand_tot);
      dlt.n_senesced_retrans +=(*myPart)->dlt.n_senesced_retrans;
      }
}

void PlantFruit::collectDetachedForResidue(vector<string> &part_name
                                           , vector<float> &dm_residue
                                           , vector<float> &dm_n
                                           , vector<float> &dm_p
                                           , vector<float> &fraction_to_residue)
   //===========================================================================
{
   vector<plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      {
      (*part)->collectDetachedForResidue(part_name
                                         , dm_residue
                                         , dm_n
                                         , dm_p
                                         , fraction_to_residue);
      }
}

void PlantFruit::collectDeadDetachedForResidue(vector<string> &part_name
                                               , vector<float> &dm_dead_detached
                                               , vector<float> &n_dead_detached
                                               , vector<float> &p_dead_detached
                                               , vector<float> &fraction_to_residue)
   //===========================================================================
{
   vector<plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      {
      (*part)->collectDetachedForResidue(part_name
                                         , dm_dead_detached
                                         , n_dead_detached
                                         , p_dead_detached
                                         , fraction_to_residue);
      }
}

void PlantFruit::update(void)
   //===========================================================================
{

   vector<plantPart *>::iterator part;

   // Update
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->update();

   // Update N

   NGreen = 0.0;
   NSenesced = 0.0;
   NDead = 0.0;
   dlt.n_senesced_dead = 0.0;
   dlt.n_green_dead = 0.0;

   for (part = myParts.begin();
        part != myParts.end();
        part++)
      {
      NGreen += (*part)->nGreen();
      NSenesced += (*part)->nSenesced();
      NDead += (*part)->nDead();
      dlt.n_senesced_dead += (*part)->dlt.n_senesced_dead;
      dlt.n_green_dead += (*part)->dlt.n_green_dead;
      }

   // Update DM

   DMGreen = 0.0;
   DMSenesced = 0.0;
   DMDead = 0.0;
   dlt.dm_senesced_dead = 0.0;
   dlt.dm_green_dead = 0.0;
   for (part = myParts.begin();
        part != myParts.end();
        part++)
      {
      DMGreen += (*part)->dmGreen();
      DMSenesced += (*part)->dmSenesced();
      DMDead += (*part)->dmDead();
      dlt.dm_senesced_dead += (*part)->dlt.dm_senesced_dead;
      dlt.dm_green_dead += (*part)->dlt.dm_green_dead;
      }


   // Update P

   PGreen = 0.0;
   PSen = 0.0;
   PDead = 0.0;
   //        dlt.p_senesced_dead = 0.0;
   //        dlt.p_green_dead = 0.0;
   for (part = myParts.begin();
        part != myParts.end();
        part++)
      {
      PGreen += (*part)->pGreen();
      PSen += (*part)->pSenesced();
      PDead += (*part)->pDead();
      //           dlt.p_senesced_dead += (*part)->dlt.p_senesced_dead;
      //           dlt.p_green_dead += (*part)->dlt.p_green_dead;
      }
}

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
float PlantFruit::coverTotal(void) const {return podPart->coverTotal();}
float PlantFruit::coverGreen(void) const {return podPart->coverGreen();}
float PlantFruit::coverDead(void) const {return podPart->coverDead();}
float PlantFruit::coverSen(void) const {return podPart->coverSen();}

//float PlantFruit::total() const
//{
//
//	return green.shell + green.meal + senesced.shell + senesced.meal + dead.shell + dead.meal;
//}

void PlantFruit::display(ostream &os) const
{
   //	os << "PlantFruit:" << endl;
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


float PlantFruit::calcCover (float canopy_fac) {return  podPart->calcCover(canopy_fac);}

void PlantFruit::processBioDemand(void)
   //===========================================================================
{

   for (vector<plantPart *>::iterator t = myParts.begin();
        t != myParts.end();
        t++)
      (*t)->processBioDemand();
   return;
}

float PlantFruit::grainNo(void) const {return grainPart->grainNo();}
float PlantFruit::grainNDemand(void) const {return grainPart->nGrainDemand();}
float PlantFruit::dltDmPotTe(void) {return podPart->dltDmPotTe();}
float PlantFruit::dltDmPotRuePod(void) {return podPart->dltDmPotRuePod();}
float PlantFruit::grainNConcPercent(void) {return grainPart->nConcPercent();}
float PlantFruit::dltDmGrainDemand(void) const {return grainPart->dltDmDemand();}
void PlantFruit::calcDlt_pod_area (void)  {podPart->calcDlt_pod_area();}

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

float PlantFruit::interceptRadiation (float radiation) {return podPart->interceptRadiation(radiation);}
void PlantFruit::dm_pot_rue (double  radn_int_pod ) {podPart->dm_pot_rue(radn_int_pod);}
void PlantFruit::transp_eff_co2() {podPart->transp_eff_co2();}
void PlantFruit::sw_demand1(float *sw_demand) {podPart->sw_demand1(sw_demand);}
void PlantFruit::bio_water1 (void) {podPart->bio_water1();}                          //remove
void PlantFruit::bio_actual (void) {podPart->bio_actual();}

void PlantFruit::grain_n_demand1(float g_nfact_grain_conc      //   (INPUT)
                                 , float g_swdef_expansion)    //   grain N demand (g/m^2)
   //===========================================================================
{
   //    Calculate plant n demand

   grainPart->grain_n_demand1(g_nfact_grain_conc
                              , g_swdef_expansion);
}

void PlantFruit::grain_n_demand2 (void)
   //===========================================================================
{
   grainPart->grain_n_demand2();

}

float PlantFruit::dm_yield_demand ( float dlt_dm_veg_supply)
   //===========================================================================
{
   //       Calculate grain dm yield demand (g/m^2)
   //       (OUTPUT) assimilate demand for reproductive part (g/m^2)

   // calculate demands of reproductive parts

   float dm_grain_demand = grainPart->dm_yield_demand();            //FIXME throughout - dm_grain_demand should be gDlt_dm_grain_demand. Leave asis for compatability
   podPart->doDmDemand1(dm_grain_demand, dlt_dm_veg_supply);
   float dm_yield_demand = podPart->dmGreenDemand()
                         + grainPart->dmGreenDemand();

   return dm_yield_demand;
}

float PlantFruit::dm_yield_demand2 ( float  dlt_dm_veg_supply)
   //===========================================================================
{
   //       Calculate grain dm yield demand (g/m^2)
   //       (OUTPUT) assimilate demand for reproductive part (g/m^2)

   // calculate demands of reproductive parts
   float dm_grain_demand = grainPart->dm_yield_demand();            //FIXME throughout - dm_grain_demand should be gDlt_dm_grain_demand. Leave asis for compatability
   podPart->doDmDemand2(dm_grain_demand, dlt_dm_veg_supply);
   float dm_yield_demand = podPart->dmGreenDemand()
                         + grainPart->dmGreenDemand();

   return dm_yield_demand;
}

void PlantFruit::dm_partition1 (double g_dlt_dm)
   //     ===========================================================
{
   //       Partitions new dm (assimilate) between plant components (g/m^2)

   //+  Changes
   //      170703 jngh specified and programmed

   //+  Local Variables
   double yield_demand;                           // sum of grain, energy & pod
   double dm_grain_demand;                        // assimilate demand for grain (g/m^2)
   double dm_pod_demand;                          // assimilate demand for pod (g/m^2)
   float  dlt_dm_grain;
   float  dlt_dm_pod;

   //- Implementation Section ----------------------------------

   //JNGH to replace old code    float dm_demand = 0.0;
   //JNGH to replace old code
   //JNGH to replace old code     for (vector<plantPart *>::iterator t = myParts.begin();      //FIXME later
   //JNGH to replace old code          t != myParts.end();
   //JNGH to replace old code          t++)
   //JNGH to replace old code        dm_demand += (*t)->dmGreenDemand ();
   //JNGH to replace old code
   //JNGH to replace old code         // now distribute the assimilate to plant parts
   //JNGH to replace old code
   //JNGH to replace old code     for (vector<plantPart *>::iterator t = myParts.begin();      //FIXME later
   //JNGH to replace old code          t != myParts.end();
   //JNGH to replace old code          t++)
   //JNGH to replace old code        (*t)->doDmPartition (g_dlt_dm, dm_demand);
   //JNGH to replace old code
   //JNGH to replace old code    // do mass balance check
   //JNGH to replace old code    float dlt_dm_green_tot = dltDmGreen ();
   //JNGH to replace old code
   //JNGH to replace old code    if (!reals_are_equal(dlt_dm_green_tot, g_dlt_dm, 1.0E-4))  // XX this is probably too much slop - try doubles XX
   //JNGH to replace old code    {
   //JNGH to replace old code         string msg = "Fruit dlt_dm_green_tot mass balance is off: "
   //JNGH to replace old code                    + ftoa(dlt_dm_green_tot, ".6")
   //JNGH to replace old code                    + " vs "
   //JNGH to replace old code                    + ftoa(g_dlt_dm, ".6");
   //JNGH to replace old code         parentPlant->warningError(msg.c_str());
   //JNGH to replace old code    }
   //JNGH to replace old code


   for (vector<plantPart *>::iterator t = myParts.begin();
        t != myParts.end();
        t++)
      (*t)->zeroDltDmGreen();

   // calculate demands of reproductive parts
   yield_demand = podPart->dmGreenDemand()
                  + grainPart->dmGreenDemand();

   // now distribute the assimilate to plant parts
   if (yield_demand >= g_dlt_dm)
      // reproductive demand exceeds supply - distribute assimilate to those parts only
      {
      // reproductive demand exceeds supply - distribute assimilate to those parts only
      dlt_dm_grain = g_dlt_dm * divide (grainPart->dmGreenDemand(), yield_demand, 0.0);
      dlt_dm_pod = g_dlt_dm - dlt_dm_grain;
      }
   else
      {
      // more assimilate than needed for reproductive parts
      // distribute to all parts

      // satisfy reproductive demands
      dlt_dm_grain   = grainPart->dmGreenDemand();
      dlt_dm_pod    = podPart->dmGreenDemand();

      }

   podPart->dm_partition1 (dlt_dm_pod);
   grainPart->dm_partition1 (dlt_dm_grain);

   dltDmGreen();      // update fruit dlt.dm_green

   // do mass balance check
   //JNGH need to fix    dlt_dm_green_tot = dlt.dm_green
   //JNGH need to fix                     + gDlt_dm_oil_conv;      //FIXME
   //JNGH need to fix
   //JNGH need to fix    if (!reals_are_equal(dlt_dm_green_tot, g_dlt_dm, 1.0E-4))  // XX this is probably too much slop - try doubles XX
   //JNGH need to fix    {
   //JNGH need to fix         string msg = "Fruit dlt_dm_green_tot mass balance is off: "
   //JNGH need to fix                    + ftoa(dlt_dm_green_tot, ".6")
   //JNGH need to fix                    + " vs "
   //JNGH need to fix                    + ftoa(g_dlt_dm, ".6");
   //JNGH need to fix         parentPlant->warningError(msg.c_str());
   //JNGH need to fix    }

   // check that deltas are in legal range       //FIXME need to do something about this when array is removed
   //    bound_check_real_array (parentPlant, dlt_dm_green, max_part, 0.0, g_dlt_dm, "Fruit dlt.dm_green");

}


//     ===========================================================                        //remove
void PlantFruit::yieldpart_demand_stress1 (void)                                          //remove
   //     ===========================================================                        //remove
{                                                                                         //remove
   //+  Purpose                                                                              //remove
   //       Simulate crop grain biomass demand stress factor                                 //remove
                                                                                          //remove
                                                                                          //remove
   cproc_yieldpart_demand_stress1 (min(plant->getNfactPhoto(), plant->getPfactPhoto())    //remove
                                   , plant->getSwdefPhoto()                               //remove
                                   , plant->getTempStressPhoto()                          //remove
                                   , &gDlt_dm_stress_max);                                //remove
  //   gDlt_dm_stress_max  = dlt_dm_stress_max;                                             //remove
}


void PlantFruit::dm_retranslocate1( float  g_dlt_dm_retrans_to_fruit )       //FIXME this code should collapse to something similar to partition1
   //     ===========================================================
{
   //     Calculate plant dry matter delta's due to retranslocation
   //     to grain, pod and energy (g/m^2)

   //+  Changes
   //       150900 jngh specified and programmed

   //+  Local Variables
   float dlt_dm_retrans_part;                    // carbohydrate removed from part (g/m^2)
   float dlt_dm_retrans_total;                   // total carbohydrate removed from parts (g/m^2)
   float yield_demand_differential;              // demand in excess of available supply (g/m^2)
   float demand_differential;                    // demand in excess of available supply (g/m^2)
   float dm_demand_differential;                 // assimilate demand by grain - meal + oil + energy (g/m^2)
   float dm_pod_demand_differential;             // assimilate demand for pod (g/m^2)
   float dlt_dm_retrans_grain;
   float dlt_dm_retrans_pod;

   // now translocate carbohydrate between plant components
   // this is different for each stage

   for (vector<plantPart *>::iterator t = myParts.begin();      //FIXME later
        t != myParts.end();
        t++)
      (*t)->zeroDltDmGreenRetrans();

   if (grainPart->dmDemandDifferential() > 0.0)
      {
      // we can translocate source carbohydrate
      // to reproductive parts if needed

      // calculate demands for each reproductive part

      dm_demand_differential          = grainPart->dmDemandDifferential();                      //FIXME
      dm_pod_demand_differential      = podPart->dmDemandDifferential();                 //FIXME

      yield_demand_differential  = dm_pod_demand_differential
                                 + dm_demand_differential;

      demand_differential = yield_demand_differential - g_dlt_dm_retrans_to_fruit;

      // get available carbohydrate from fruit supply pools
      for (vector<plantPart *>::iterator fPart = supplyPools.begin();      //FIXME later
           fPart != supplyPools.end();
           fPart++)
         {
         dlt_dm_retrans_part = (*fPart)->dltDmRetranslocateSupply(demand_differential);
         demand_differential = demand_differential - dlt_dm_retrans_part;
         }

      dlt_dm_retrans_total = g_dlt_dm_retrans_to_fruit + (-dltDmRetranslocate());

      // now distribute retranslocate to demand sinks.

      if (yield_demand_differential > dlt_dm_retrans_total)
         {
         dlt_dm_retrans_grain = dlt_dm_retrans_total
                              * divide (dm_demand_differential, yield_demand_differential, 0.0);
         dlt_dm_retrans_pod = dlt_dm_retrans_total
                              * divide (dm_pod_demand_differential, yield_demand_differential, 0.0)
                              + podPart->dlt.dm_green_retrans;
         }
      else
         {

         dlt_dm_retrans_grain     = dm_demand_differential;
         dlt_dm_retrans_pod      = dm_pod_demand_differential
                                 + podPart->dlt.dm_green_retrans;
         }

      // ??? check that stem and leaf are >= min wts
      podPart->dm_retranslocate1 (dlt_dm_retrans_pod);
      grainPart->dm_retranslocate1 (dlt_dm_retrans_grain);
      }
   else
      {
      // we have no retranslocation
      for (vector<plantPart *>::iterator t = myParts.begin();      //FIXME later
           t != myParts.end();
           t++)
         (*t)->zeroDltDmGreenRetrans();
      }

   dltDmRetranslocate();

   // now check that we have mass balance
   //JNGH need to fix    if (!reals_are_equal(-1.0 * (dltDmRetranslocate() - g_dlt_dm_retrans_to_fruit), dmOil_conv_retranslocate, 1.0E-4))
   //JNGH need to fix    {
   //JNGH need to fix      string msg = "dm_retranslocate mass balance of fruit is off: "
   //JNGH need to fix                 + ftoa(dltDmRetranslocate() - g_dlt_dm_retrans_to_fruit, ".6")
   //JNGH need to fix                 + " vs "
   //JNGH need to fix                 + ftoa(dmOil_conv_retranslocate, ".6");
   //JNGH need to fix
   //JNGH need to fix
   //JNGH need to fix      parentPlant->warningError(msg.c_str());
   //JNGH need to fix    }
}

void PlantFruit::doSenescence1 (float sen_fr)       // (OUTPUT) actual biomass senesced from plant parts (g/m^2)
   //============================================================================
{
   //   Derives seneseced plant dry matter (g/m^2) for the day

   dlt.dm_senesced = 0.0;
   for (vector<plantPart *>::iterator t = myParts.begin();
        t != myParts.end();
        t++)
      {
      (*t)->doSenescence1(sen_fr);
      dlt.dm_senesced += (*t)->dlt.dm_senesced;
      }
}

void PlantFruit::doSenescence2 (float sen_fr)       // (OUTPUT) actual biomass senesced from plant parts (g/m^2)
   //============================================================================
{
   //   Derives seneseced plant dry matter (g/m^2) for the day

   dlt.dm_senesced = 0.0;
   for (vector<plantPart *>::iterator t = myParts.begin();
        t != myParts.end();
        t++)
      {
      (*t)->doSenescence2(sen_fr);
      dlt.dm_senesced += (*t)->dlt.dm_senesced;
      }
}

void PlantFruit::doDmMin (void)       // (OUTPUT) actual biomass senesced from plant parts (g/m^2)
   //============================================================================
{
   DMPlantMin = 0.0;
   for (vector<plantPart *>::iterator t = myParts.begin();
        t != myParts.end();
        t++)
      {
      (*t)->doDmMin();
      DMPlantMin += (*t)->DMPlantMin;
      }
}

void PlantFruit::nit_init (void)
   //============================================================================
{
   //       Initialise plant nitrogen.
   grainPart->nit_init();
}

float PlantFruit::availableRetranslocateN(void)
   //============================================================================
{
   float nAvail = 0.0;
   for (vector<plantPart *>::iterator t = supplyPools.begin();
        t != supplyPools.end();
        t++)
      nAvail += (*t)->availableRetranslocateN();

   return nAvail;
}

void PlantFruit::n_conc_grain_limits (void)
   //============================================================================
   //       Calculate the critical N concentration for grain below which plant growth
   //       is affected.  Also minimum and maximum N concentrations below
   //       and above which it is not allowed to fall or rise.
   //       These are analogous to the water concentrations
   //       of sat, dul and ll.
{

   //   for (vector<plantPart *>::iterator t = myGrainParts.begin();
   //       t != myGrainParts.end();
   //       t++)
   //     (*t)->n_conc_grain_limits();
   grainPart->n_conc_grain_limits();
}

//FIXME not called yet
//void PlantFruit::n_retranslocate( void)
////============================================================================
////     Calculate the nitrogen retranslocation from the various fruit parts to the grain.
//{
//
//    float N_avail_rep = 0.0;
//         // Get Grain N supply in this cohort
//
//    vector<plantPart *>::iterator part;
//    for (part = supplyPools.begin(); part != supplyPools.end(); part++)
//        N_avail_rep += (*part)->availableRetranslocateN();  // grain N potential (supply) from pod
//
//            // available N does not include grain
//            // this should not presume grain is 0.
//
//          // get actual grain N uptake by retransolcation
//          // limit retranslocation to total available N
//
//    for (part = myParts.begin(); part != myParts.end(); part++)
//         (*part)->dlt.n_retrans = 0.0;
//
//      if (gN_grain_demand >= N_avail_rep)
//      {
//             // demand greater than or equal to supply
//             // retranslocate all available N
//
//         for (part = supplyPools.begin(); part != supplyPools.end(); part++)
//              (*part)->dlt.n_retrans = - (*part)->availableRetranslocateN();
//         mealPart->dlt.n_retrans = N_avail_rep;
//      }
//      else
//      {
//             // supply greater than demand.
//             // Retranslocate what is needed
//
//         for (part = supplyPools.begin(); part != supplyPools.end(); part++)
//               (*part)->dlt.n_retrans = - gN_grain_demand
//                                       * divide ((*part)->availableRetranslocateN(), N_avail_rep, 0.0);
//
//         mealPart->dlt.n_retrans = gN_grain_demand;
//
//      }
//}

void PlantFruit::doNRetranslocate( float N_supply, float g_grain_n_demand)
   //============================================================================
   //     Calculate the nitrogen retranslocation from the various plant parts to the grain.
{

   // available N does not include grain
   // this should not presume grain is 0.

   // get actual grain N uptake by retransolcation
   // limit retranslocation to total available N

   vector<plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->dlt.n_retrans = 0.0;


   for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->doNRetranslocate(N_supply, g_grain_n_demand);

   dlt.n_retrans = 0.0;
   for (part = supplyPools.begin(); part != supplyPools.end(); part++)
      dlt.n_retrans += (*part)->dlt.n_retrans;
}

void PlantFruit::doNDemand1(float dlt_dm             // (INPUT)  Whole plant the daily biomass production (g/m^2)
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

void PlantFruit::doNDemand1Pot(float dlt_dm             // (INPUT)  Whole plant the daily biomass production (g/m^2)
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

void PlantFruit::doNDemand2(float dlt_dm             // (INPUT)  Whole plant the daily biomass production (g/m^2)
                            , float dlt_dm_pot_rue)  // (INPUT)  Whole plant potential dry matter production (g/m^2)
   //============================================================================
   //     Return plant nitrogen demand for each plant component
{
   NDemand = 0.0;
   NMax = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      {
      (*part)->doNDemand2(dlt_dm, dlt_dm_pot_rue);
      NDemand += (*part)->nDemand();
      NMax += (*part)->nMax();
      }
}


void PlantFruit::doSoilNDemand(void)
   //============================================================================
{
   SoilNDemand = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      {
      (*part)->doSoilNDemand();
      SoilNDemand += (*part)->SoilNDemand;
      }
}


void PlantFruit::doNSenescence(void)
   //============================================================================
{
   dlt.n_senesced = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      {
      (*part)->doNSenescence();
      dlt.n_senesced += (*part)->dlt.n_senesced;
      }
}

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

void PlantFruit::doPDemand(void)     // (INPUT)  Whole plant potential dry matter production (g/m^2)
   //============================================================================
   //     Return plant P demand for each plant component
{
   PDemand = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      {
      (*part)->doPDemand();
      PDemand += (*part)->pDemand();
      }
}

void PlantFruit::doPSenescence(void)
   //============================================================================
{
   dlt.p_sen = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      {
      (*part)->doPSenescence();
      dlt.p_sen += (*part)->dlt.p_sen;
      }
}

float PlantFruit::pDemand(void)
   //============================================================================
{
   float p_demand = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      {
      p_demand += (*part)->pDemand();
      }
   return p_demand;
}

float PlantFruit::pRetransSupply(void)
   //============================================================================
{
   float p_retrans_supply = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      {
      p_retrans_supply += (*part)->pRetransSupply();
      }
   return p_retrans_supply;
}

float PlantFruit::pRetransDemand(void)
   //============================================================================
{
   float p_retrans_demand = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      {
      p_retrans_demand += (*part)->pRetransDemand();
      }
   return p_retrans_demand;
}

float PlantFruit::dmRetransSupply(void)
   //============================================================================
{
   float dm_retrans_supply = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      {
      dm_retrans_supply += (*part)->dmRetransSupply();
      }
   return dm_retrans_supply;
}

float PlantFruit::dmRetransDemand(void)
   //============================================================================
{
   float dm_retrans_demand = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      {
      dm_retrans_demand += (*part)->dmRetransDemand();
      }
   return dm_retrans_demand;
}

float PlantFruit::nRetransSupply(void)
   //============================================================================
{
   float n_retrans_supply = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      {
      n_retrans_supply += (*part)->nRetransSupply();
      }
   return n_retrans_supply;
}

float PlantFruit::dltNRetransOut(void)
   //============================================================================
{
   float dlt_n_retrans = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      {
      dlt_n_retrans += (*part)->dltNRetransOut();
      }
   return dlt_n_retrans;
}

float PlantFruit::dltNGreen(void)
   //============================================================================
{
   float dlt_n_green = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      {
      dlt_n_green += (*part)->dltNGreen();
      }
   return dlt_n_green;
}

float PlantFruit::nRetransDemand(void)
   //============================================================================
{
   float n_retrans_demand = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      {
      n_retrans_demand += (*part)->nRetransDemand();
      }
   return n_retrans_demand;
}

void PlantFruit::distributeDltPGreen(float p_uptake, float total_p_demand)
   //============================================================================
{
   dlt.p_green = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      {
      (*part)->distributeDltPGreen(p_uptake, total_p_demand);
      dlt.p_green += (*part)->dlt.p_green;
      }
}

void PlantFruit::distributeDltPRetrans(float total_p_supply, float total_p_demand)
   //============================================================================
{
   dlt.p_retrans = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      {
      (*part)->distributeDltPRetrans(total_p_supply, total_p_demand);
      dlt.p_retrans += (*part)->dlt.p_retrans;
      }
}

void PlantFruit::p_detachment1(void)
   //============================================================================
{
   dlt.p_det = 0.0;
   dlt.p_dead_det = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      {
      (*part)->p_detachment1();
      dlt.p_det += (*part)->dlt.p_det;
      dlt.p_dead_det += (*part)->dlt.p_dead_det;
      }
}

void PlantFruit::updatePDet(void)
   //============================================================================
{
   vector<plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      {
      (*part)->updatePDet();
      PSen +=  (*part)->dlt.p_sen;
      PSen -= (*part)->dlt.p_det;
      PDead -= (*part)->dlt.p_dead_det;
      }
}

void PlantFruit::pInit(void)
   //============================================================================
{
   PGreen = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      {
      (*part)->pInit();
      PGreen +=  (*part)->PGreen;
      }
}

float PlantFruit::dmGreenStressDeterminant(void)
   //============================================================================
{
   float dm_green = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      {
      dm_green +=  (*part)->dmGreenStressDeterminant();
      }
   return dm_green;
}

float PlantFruit::pGreenStressDeterminant(void)
   //============================================================================
{
   float p_green = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      {
      p_green +=  (*part)->pGreenStressDeterminant();
      }
   return p_green;
}

float PlantFruit::pMaxPotStressDeterminant(void)
   //============================================================================
{
   float p_max_pot = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      {
      p_max_pot +=  (*part)->pMaxPotStressDeterminant();
      }
   return p_max_pot;
}

float PlantFruit::pMinPotStressDeterminant(void)
   //============================================================================
{
   float p_min_pot = 0.0;
   vector<plantPart *>::iterator part;
   for (part = myParts.begin(); part != myParts.end(); part++)
      {
      p_min_pot +=  (*part)->pMinPotStressDeterminant();
      }
   return p_min_pot;
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


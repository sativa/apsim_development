
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
    gDm_stress_max.setup(&gDlt_dm_stress_max);     //remove
    otherObservers.addObserver(&gDm_stress_max);   //remove
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
    if (oilPart) delete oilPart;               //remove
    if (mealPart) delete mealPart;             //remove
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
   plantPart::doRegistrations(system);

   system->addGettableVar("dlt_dm_grain_demand",gDlt_dm_grain_demand, "g/m^2", "??");       //remove
   system->addGettableVar("dlt_dm_fruit", gDlt_dm, "g/m^2", "Change in dry matter");
   system->addGettableVar("dlt_dm_oil_conv",gDlt_dm_oil_conv,"g/m^2", "change in oil via ??");   //remove
   system->addGettableVar("grain_no",gGrain_no, "/m^2", "Grain number");                         //remove
   setupGetFunction(system, "grain_size", protocol::DTsingle, false, &PlantFruit::get_grain_size, "g", "Size of each grain");    //remove
   setupGetFunction(system, "grain_wt", protocol::DTsingle, false, &PlantFruit::get_grain_wt, "g/m^2", "Weight of grain");        //remove
   setupGetFunction(system, "head_wt", protocol::DTsingle, false,&PlantFruit::get_head_wt, "g/m^2", "Weight of heads");
   setupGetFunction(system, "yield", protocol::DTsingle, false,&PlantFruit::get_yield,  "kg/ha", "Yield");                        //remove
   system->addGettableVar("grain_n_demand", gN_grain_demand, "g/m^2", "N demand of grain");                                       //remove

   setupGetFunction(system, "n_grain_pcnt", protocol::DTsingle, false, &PlantFruit::get_n_conc_grain, "%", "N concentration in grain");   //remove
   setupGetFunction(system, "n_conc_grain", protocol::DTsingle, false, &PlantFruit::get_n_conc_grain, "%", "N concentration in grain");   //remove
   setupGetFunction(system, "grain_protein", protocol::DTsingle, false, &PlantFruit::get_grain_protein, "%", "grain protein content");    //remove
   setupGetFunction(system, "n_conc_meal", protocol::DTsingle, false, &PlantFruit::get_n_conc_meal, "%", "meal N content");               //remove
   setupGetFunction(system, "head_n", protocol::DTsingle, false,&PlantFruit::get_head_n, "g/m^2", "N in heads");
   setupGetFunction(system, "head_p", protocol::DTsingle, false, &PlantFruit::get_head_p, "g/m^2","P in head");
   setupGetFunction(system, "grain_n", protocol::DTsingle, false, &PlantFruit::get_grain_n, "g/m^2", "N in grain");                       //remove

   setupGetFunction(system, "grain_p", protocol::DTsingle, false, &PlantFruit::get_grain_p, "g/m^2","P in grain");                        //remove

   setupGetFunction(system, "p_conc_grain", protocol::DTsingle, false, &PlantFruit::get_p_conc_grain, "%","P in grain");                  //remove

   setupGetFunction(system, "p_grain_pcnt", protocol::DTsingle, false, &PlantFruit::get_p_conc_grain, "%","P in grain");                  //remove

   system->addGettableVar("grain_p_demand",  gP_grain_demand, "g/m^2","P demand of grain");                                               //remove
   system->addGettableVar("grain_oil_conc", cGrain_oil_conc, "%", "??");                                                                  //remove
   system->addGettableVar("dlt_dm_oil_conv_retrans", dmOil_conv_retranslocate, "g/m^2", "change in oil via retranslocation");             //remove



   unsigned int id;
   // Set My Variable
//   id = system->addRegistration(RegistrationType::respondToSet, "grain_oil_conc", floatType);
//   IDtoSetFn.insert(UInt2SetFnMap::value_type(id,&Plant::set_plant_grain_oil_conc));




   idLatitude = system->addRegistration (RegistrationType::get, "latitude", floatType, "", "");


   for (vector<plantPart *>::iterator t = myParts.begin();
        t != myParts.end();
        t++)
       (*t)->doRegistrations(system);
}

// Set a variable from the system.
////===========================================================================
//bool PlantFruit::setVariable(unsigned id, protocol::QuerySetValueData& qd)
////===========================================================================
//  {
//    ptr2setFn pf = IDtoSetFn[id];
//    if (pf) {return((this->*pf)(qd));}
//    return false;
//  }
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
float PlantFruit::grainWt(void)
//===========================================================================
{
    float grainWt = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myGrainParts.begin(); part != myGrainParts.end(); part++)
      grainWt += (*part)->dmTotal();
   return divide (grainWt, gGrain_no, 0.0);
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
float PlantFruit::dmVegTotal(void)
//===========================================================================
{
    float dmTotal = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      dmTotal += (*part)->dmTotal();
   return dmTotal;
}

//===========================================================================
float PlantFruit::dmGreenGrainTotal(void)
//===========================================================================
{
    float dmTotal = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myGrainParts.begin(); part != myGrainParts.end(); part++)
      dmTotal += (*part)->dmGreen();
   return dmTotal;
}

//===========================================================================
float PlantFruit::dmGreenVegTotal(void)
//===========================================================================
{
    float dmTotal = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      dmTotal += (*part)->dmGreen();
   return dmTotal;
}

//===========================================================================
float PlantFruit::dmSenescedVegTotal(void)
//===========================================================================
{
    float dmTotal = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      dmTotal += (*part)->dmSenesced();
   return dmTotal;
}

//===========================================================================
float PlantFruit::dmSenesced(void)
//===========================================================================
{
    g.dm_senesced = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myParts.begin(); part != myParts.end(); part++)
      g.dm_senesced += (*part)->dmSenesced();
   return g.dm_senesced;
}

//===========================================================================
float PlantFruit::dmDeadVegTotal(void)
//===========================================================================
{
    float dmTotal = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      dmTotal += (*part)->dmDead();
   return dmTotal;
}

//===========================================================================
float PlantFruit::dmDead(void)
//===========================================================================
{
    g.dm_dead = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myParts.begin(); part != myParts.end(); part++)
      g.dm_dead += (*part)->dmDead();
   return g.dm_dead;
}

//===========================================================================
float PlantFruit::nTotal(void)
//===========================================================================
{
    float nTotal = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myParts.begin(); part != myParts.end(); part++)
      nTotal += (*part)->nTotal();
   return nTotal;
}

//===========================================================================
float PlantFruit::nGrainTotal(void)
//===========================================================================
{
    float nTotal = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myGrainParts.begin(); part != myGrainParts.end(); part++)
      nTotal += (*part)->nTotal();
   return nTotal;
}

//===========================================================================
float PlantFruit::nVegTotal(void)
//===========================================================================
{
    float nTotal = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      nTotal += (*part)->nTotal();
   return nTotal;
}

//===========================================================================
float PlantFruit::nGreenGrainTotal(void)
//===========================================================================
{
    float nTotal = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myGrainParts.begin(); part != myGrainParts.end(); part++)
      nTotal += (*part)->nGreen();
   return nTotal;
}

//===========================================================================
float PlantFruit::nGreenVegTotal(void)
//===========================================================================
{
    float nTotal = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      nTotal += (*part)->nGreen();
   return nTotal;
}

//===========================================================================
float PlantFruit::nGreen(void)
//===========================================================================
{
    g.n_green = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myParts.begin(); part != myParts.end(); part++)
      g.n_green += (*part)->nGreen();
   return g.n_green;
}

//===========================================================================
float PlantFruit::nSenescedVegTotal(void)
//===========================================================================
{
    float nTotal = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      nTotal += (*part)->nSenesced();
   return nTotal;
}

//===========================================================================
float PlantFruit::nSenesced(void)
//===========================================================================
{
    g.n_senesced = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myParts.begin(); part != myParts.end(); part++)
      g.n_senesced += (*part)->nSenesced();
   return g.n_senesced;
}

//===========================================================================
float PlantFruit::nDeadVegTotal(void)
//===========================================================================
{
    float nTotal = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      nTotal += (*part)->nDead();
   return nTotal;
}

//===========================================================================
float PlantFruit::nDead(void)
//===========================================================================
{
    g.n_dead = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myParts.begin(); part != myParts.end(); part++)
      g.n_dead += (*part)->nDead();
   return g.n_dead;
}

//===========================================================================
float PlantFruit::nMaxPot(void)
//===========================================================================
{
    float nMaxPot = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      nMaxPot += (*part)->nMaxPot();
   return nMaxPot;
}

//===========================================================================
float PlantFruit::nMax(void)
//===========================================================================
{
    v.n_max = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      v.n_max += (*part)->nMax();
   return v.n_max;
}

//===========================================================================
float PlantFruit::nMinPot(void)
//===========================================================================
{
    float nMinPot = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      nMinPot += (*part)->nMinPot();
   return nMinPot;
}

//===========================================================================
float PlantFruit::nConcGrain(void)
//===========================================================================
{
//    return grainPart->nConcGrain();
    float n_conc = divide (nGreenGrainTotal() , dmGreenGrainTotal() , 0.0) * fract2pcnt;
    return n_conc;
}


//===========================================================================
float PlantFruit::nGrainDemand2(void)
//===========================================================================
{
//    return grainPart->nGrainDemand2();
    return l_bound(mealPart->v.soil_n_demand - mealPart->dlt.n_green, 0.0);
}


//============================================================================
float PlantFruit::soilNDemand(void)
//============================================================================
{
    v.soil_n_demand = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myParts.begin(); part != myParts.end(); part++)
    {
            v.soil_n_demand += (*part)->soilNDemand();
    }
    return v.soil_n_demand;
}

//============================================================================
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

//============================================================================
float PlantFruit::nCapacity(void)
//============================================================================
{
    v.n_capacity = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myVegParts.begin(); part != myVegParts.end(); part++)
    {
            v.n_capacity += (*part)->nCapacity();
    }
    return v.n_capacity;
}

//============================================================================
void PlantFruit::nPartition(float nSupply)
//============================================================================
{
    float dlt_n_green = 0.0;
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
    //cnh mealPart->dlt.n_green = 0.0;
    oilPart->dlt.n_green = 0.0;      //remove

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
//============================================================================
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

//===========================================================================
float PlantFruit::pTotal(void)
//===========================================================================
{
    float pTotal = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myParts.begin(); part != myParts.end(); part++)
      pTotal += (*part)->pTotal();
   return pTotal;
}

//===========================================================================
float PlantFruit::pGrainTotal(void)
//===========================================================================
{
    float pTotal = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myGrainParts.begin(); part != myGrainParts.end(); part++)
      pTotal += (*part)->pTotal();
   return pTotal;
}

//===========================================================================
float PlantFruit::pVegTotal(void)
//===========================================================================
{
    float pTotal = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      pTotal += (*part)->pTotal();
   return pTotal;
}

//===========================================================================
float PlantFruit::pGreenGrainTotal(void)
//===========================================================================
{
    float pTotal = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myGrainParts.begin(); part != myGrainParts.end(); part++)
      pTotal += (*part)->pGreen();
   return pTotal;
}

//===========================================================================
float PlantFruit::pDeadGrainTotal(void)
//===========================================================================
{
    float pTotal = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myGrainParts.begin(); part != myGrainParts.end(); part++)
      pTotal += (*part)->pDead();
   return pTotal;
}

//===========================================================================
float PlantFruit::pGreenVegTotal(void)
//===========================================================================
{
    float pTotal = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      pTotal += (*part)->pGreen();
   return pTotal;
}

//===========================================================================
float PlantFruit::pGreen(void)
//===========================================================================
{
    float pTotal = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myParts.begin(); part != myParts.end(); part++)
      pTotal += (*part)->pGreen();
   return pTotal;
}

//===========================================================================
float PlantFruit::pSenescedGrainTotal(void)
//===========================================================================
{
    float pTotal = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myGrainParts.begin(); part != myGrainParts.end(); part++)
      pTotal += (*part)->pSenesced();
   return pTotal;
}

//===========================================================================
float PlantFruit::pSenescedVegTotal(void)
//===========================================================================
{
    float pTotal = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      pTotal += (*part)->pSenesced();
   return pTotal;
}

//===========================================================================
float PlantFruit::pSenesced(void)
//===========================================================================
{
    float pTotal = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myParts.begin(); part != myParts.end(); part++)
      pTotal += (*part)->pSenesced();
   return pTotal;
}

//===========================================================================
float PlantFruit::pDeadVegTotal(void)
//===========================================================================
{
    float pTotal = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      pTotal += (*part)->pDead();
   return pTotal;
}

//===========================================================================
float PlantFruit::pDead(void)
//===========================================================================
{
    float pTotal = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myParts.begin(); part != myParts.end(); part++)
      pTotal += (*part)->pDead();
   return pTotal;
}

//===========================================================================
float PlantFruit::pConcGrain(void)
//===========================================================================
{
    float p_conc = divide (pGreenGrainTotal() , dmGreenGrainTotal() , 0.0) * fract2pcnt;
    return p_conc;
}


//===========================================================================
float PlantFruit::pConcGrainTotal(void)
//===========================================================================
{
    float p_conc = divide (pGrainTotal() , dmGrainTotal() , 0.0) * fract2pcnt;
    return p_conc;
}


//===========================================================================
float PlantFruit::pMaxPot(void)
//===========================================================================
{
    float pMaxPot = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      pMaxPot += (*part)->pMaxPot();
   return pMaxPot;
}

//===========================================================================
float PlantFruit::pMinPot(void)
//===========================================================================
{
    float pMinPot = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myVegParts.begin(); part != myVegParts.end(); part++)
      pMinPot += (*part)->pMinPot();
   return pMinPot;
}

//===========================================================================                 //remove
void PlantFruit::get_grain_wt(protocol::Component *system, protocol::QueryValueData &qd)      //remove
//===========================================================================                 //remove
{                                                                                             //remove
    system->sendVariable(qd, dmGreenGrainTotal());                                            //remove
}                                                                                             //remove
                                                                                              //remove
//===========================================================================                 //remove
void PlantFruit::get_grain_size(protocol::Component *system, protocol::QueryValueData &qd)    //remove
//===========================================================================                 //remove
{                                                                                             //remove
    float grain_size = divide (dmGrainTotal()                                                 //remove
                            , gGrain_no, 0.0);                                                //remove
                                                                                              //remove
    system->sendVariable(qd, grain_size);                                                     //remove
}                                                                                             //remove

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
void PlantFruit::get_head_n(protocol::Component *system, protocol::QueryValueData &qd)
//===========================================================================
{
    system->sendVariable(qd, nGreenGrainTotal() + nGreenVegTotal());
}

//===========================================================================                         //remove
void PlantFruit::get_grain_n(protocol::Component *system, protocol::QueryValueData &qd)               //remove
//===========================================================================                         //remove
{                                                                                                     //remove
    system->sendVariable(qd, nGreenGrainTotal());                                                     //remove
}                                                                                                     //remove
                                                                                                      //remove
//===========================================================================                         //remove
void PlantFruit::get_grain_n_demand(protocol::Component *system, protocol::QueryValueData &qd)        //remove
//===========================================================================                         //remove
{                                                                                                     //remove
    system->sendVariable(qd, gN_grain_demand);                                                        //remove
}                                                                                                     //remove
                                                                                                      //remove
//===========================================================================                         //remove
void PlantFruit::get_n_conc_grain(protocol::Component *system, protocol::QueryValueData &qd)          //remove
//===========================================================================                         //remove
{                                                                                                     //remove
    float n_conc = nConcGrain();                                                                      //remove
    system->sendVariable(qd, n_conc);                                                                 //remove
}                                                                                                     //remove
                                                                                                      //remove
//===========================================================================                         //remove
void PlantFruit::get_grain_protein(protocol::Component *system, protocol::QueryValueData &qd)         //remove
//===========================================================================                         //remove
{                                                                                                     //remove
    float gp = nConcGrain() * fract2pcnt * 5.71;                                                      //remove
    system->sendVariable(qd, gp);                                                                     //remove
}                                                                                                     //remove
                                                                                                      //remove
//===========================================================================                         //remove
void PlantFruit::get_n_conc_meal(protocol::Component *system, protocol::QueryValueData &qd)           //remove
//===========================================================================                         //remove
{                                                                                                     //remove
    float n_conc = mealPart->nConc();                                                                 //remove
    system->sendVariable(qd, n_conc);                                                                 //remove
}                                                                                                     //remove
                                                                                                      //remove
//===========================================================================                         //remove
void PlantFruit::get_yield(protocol::Component *system, protocol::QueryValueData &qd)                 //remove
//===========================================================================                         //remove
{                                                                                                     //remove
    system->sendVariable(qd, dmGrainTotal() * gm2kg / sm2ha);                                         //remove
}                                                                                                     //remove
                                                                                                      //remove
//===========================================================================                         //remove
void PlantFruit::get_grain_p(protocol::Component *systemInterface, protocol::QueryValueData &qd)      //remove
//===========================================================================                         //remove
{                                                                                                     //remove
    float grain_p = pGreenGrainTotal();                                                               //remove
    systemInterface->sendVariable(qd, grain_p);  //()                                                 //remove
}                                                                                                     //remove
                                                                                                      //remove
//===========================================================================                      //put in pod
void PlantFruit::get_pod_n(protocol::Component *systemInterface, protocol::QueryValueData &qd)     //put in pod
//===========================================================================                      //put in pod
{                                                                                                  //put in pod
    systemInterface->sendVariable(qd, podPart->nGreen());  //()                                    //put in pod
}                                                                                                  //put in pod
                                                                                                   //put in pod
//===========================================================================                      //put in pod
void PlantFruit::get_pod_p(protocol::Component *systemInterface, protocol::QueryValueData &qd)     //put in pod
//===========================================================================                      //put in pod
{                                                                                                  //put in pod
    systemInterface->sendVariable(qd, podPart->pGreen());  //()                                    //put in pod
}                                                                                                  //put in pod

//===========================================================================
void PlantFruit::get_head_p(protocol::Component *systemInterface, protocol::QueryValueData &qd)
//===========================================================================
{
    systemInterface->sendVariable(qd, pGreenGrainTotal() + pGreenVegTotal());  //()
}

//===========================================================================                           //remove
void PlantFruit::get_p_conc_grain(protocol::Component *systemInterface, protocol::QueryValueData &qd)   //remove
//===========================================================================                           //remove
{                                                                                                       //remove
    float p_conc_grain = pConcGrain();                                                                  //remove
    systemInterface->sendVariable(qd, p_conc_grain);  //()                                              //remove
}                                                                                                       //remove

//===========================================================================
void PlantFruit::get_p_demand(vector<float> &p_demand)
//===========================================================================
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_p_demand(p_demand);
}

//===========================================================================
void PlantFruit::get_dlt_p_green(vector<float> &dlt_p_green)
//===========================================================================
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dlt_p_green(dlt_p_green);
}

//===========================================================================
void PlantFruit::get_p_green(vector<float> &p_green)
//===========================================================================
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_p_green(p_green);
}

//===========================================================================
void PlantFruit::get_dlt_p_retrans(vector<float> &dlt_p_retrans)
//===========================================================================
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dlt_p_retrans(dlt_p_retrans);
}

//===========================================================================
void PlantFruit::get_dm_plant_min(vector<float> &dm_min)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dm_plant_min(dm_min);
}

//===========================================================================
void PlantFruit::get_dm_green(vector<float> &dm_green)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dm_green(dm_green);
}

//===========================================================================
void PlantFruit::get_dm_dead(vector<float> &dm_dead)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dm_dead(dm_dead);
}

//===========================================================================
void PlantFruit::get_dm_senesced(vector<float> &dm_senesced)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dm_senesced(dm_senesced);
}

//===========================================================================
void PlantFruit::get_dlt_dm_green(vector<float> &dlt_dm_green)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dlt_dm_green(dlt_dm_green);
}

//===========================================================================
void PlantFruit::get_dlt_dm_green_retrans(vector<float> &dlt_dm_green_retrans)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dlt_dm_green_retrans(dlt_dm_green_retrans);
}

//===========================================================================
void PlantFruit::get_dlt_dm_detached(vector<float> &dlt_dm_detached)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dlt_dm_detached(dlt_dm_detached);
}

//===========================================================================
void PlantFruit::get_dlt_dm_senesced(vector<float> &dlt_dm_senesced)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dlt_dm_senesced(dlt_dm_senesced);
}

//===========================================================================
void PlantFruit::get_dlt_dm_dead_detached(vector<float> &dlt_dm_dead_detached)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dlt_dm_dead_detached(dlt_dm_dead_detached);
}

//===========================================================================
void PlantFruit::get_dlt_dm_green_dead(vector<float> &dlt_dm_green_dead)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dlt_dm_green_dead(dlt_dm_green_dead);
}

//===========================================================================
void PlantFruit::get_dlt_dm_senesced_dead(vector<float> &dlt_dm_senesced_dead)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dlt_dm_senesced_dead(dlt_dm_senesced_dead);
}

//===========================================================================
void PlantFruit::get_n_demanded(vector<float> &n_demand)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_n_demanded(n_demand);
}

//===========================================================================
void PlantFruit::get_n_green(vector<float> &n_green)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_n_green(n_green);
}

//===========================================================================
void PlantFruit::get_n_senesced(vector<float> &n_senesced)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_n_senesced(n_senesced);
}

//===========================================================================
void PlantFruit::get_n_dead(vector<float> &n_dead)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_n_dead(n_dead);
}

//===========================================================================
void PlantFruit::get_dlt_n_green(vector<float> &n_green)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dlt_n_green(n_green);
}

//===========================================================================
void PlantFruit::get_dlt_n_dead(vector<float> &n_dead)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dlt_n_dead(n_dead);
}

//===========================================================================
void PlantFruit::get_dlt_n_retrans(vector<float> &n_retrans)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dlt_n_retrans(n_retrans);
}

//===========================================================================
void PlantFruit::get_dlt_n_senesced(vector<float> &n_senesced)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dlt_n_senesced(n_senesced);
}

//===========================================================================
void PlantFruit::get_dlt_n_senesced_dead(vector<float> &dlt_n_senesced_dead)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dlt_n_senesced_dead(dlt_n_senesced_dead);
}

//===========================================================================
void PlantFruit::get_dlt_n_senesced_retrans(vector<float> &n_senesced_retrans)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dlt_n_senesced_retrans(n_senesced_retrans);
}

//===========================================================================
void PlantFruit::get_dlt_n_senesced_trans(vector<float> &n_senesced_trans)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dlt_n_senesced_trans(n_senesced_trans);
}

//===========================================================================
void PlantFruit::get_dlt_n_detached(vector<float> &n_detached)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dlt_n_detached(n_detached);
}

//===========================================================================
void PlantFruit::get_dlt_n_dead_detached(vector<float> &n_dead_detached)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dlt_n_dead_detached(n_dead_detached);
}

//===========================================================================
void PlantFruit::get_p_dead(vector<float> &p_dead)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_p_dead(p_dead);
}

//===========================================================================
void PlantFruit::get_p_sen(vector<float> &p_sen)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_p_sen(p_sen);
}

//===========================================================================
void PlantFruit::get_dlt_p_detached(vector<float> &dlt_p_detached)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dlt_p_detached(dlt_p_detached);
}

//===========================================================================
void PlantFruit::get_dlt_p_dead(vector<float> &dlt_p_dead)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dlt_p_dead(dlt_p_dead);
}

//===========================================================================
void PlantFruit::get_dlt_p_sen(vector<float> &dlt_p_sen)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dlt_p_sen(dlt_p_sen);
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
//    grainPart->grain_number();    // Calculate grain no
    if (cGrain_no_option == 1)                                        //remove
        {                                                             //remove
        // do not use grain number                                    //remove
        gGrain_no = 0;                                                //remove
        }                                                             //remove
    else if (cGrain_no_option == 2)                                   //remove
        {                                                             //remove
        grain_number (plant->getDmGreenStem()                         //remove
                    , pGrains_per_gram_stem                           //remove
                    , &gGrain_no);                                    //remove
        }                                                             //remove
    else                                                              //remove
        {                                                             //remove
        throw std::invalid_argument ("invalid template option");      //remove
        }                                                             //remove
                                                                      //remove
    return;
}

//===========================================================================      //remove
void PlantFruit::grain_number (float stem_dm_green                                 //remove
                             , float p_grains_per_gram_stem                        //remove
                             , float *g_grain_no)    // OUTPUT                     //remove
//===========================================================================      //remove
//+  Purpose                                                                       //remove
//       Perform grain number calculations                                         //remove
{                                                                                  //remove
  if (plant->on_day_of ("emergence"))                                              //remove
        {                                                                          //remove
        // seedling has just emerged.                                              //remove
        *g_grain_no = 0.0;                                                         //remove
        }                                                                          //remove
  else if (plant->on_day_of ("flowering"))                                         //remove
      {                                                                            //remove
      // we are at first day of grainfill.                                         //remove
      *g_grain_no = p_grains_per_gram_stem * stem_dm_green;                        //remove
      }                                                                            //remove
  else                                                                             //remove
      {                                                                            //remove
      // no changes                                                                //remove
      }                                                                            //remove
}                                                                                  //remove

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

//- Implementation Section ----------------------------------

    system->writeString (" - reading fruit cultivar parameters");

    //  plant_dm_grain_hi                                    //remove
    system->readParameter (cultivar.c_str()                  //remove
    , "x_pp_hi_incr"/*,  "(h)"*/                             //remove
    , pX_pp_hi_incr                                          //remove
    , pNum_pp_hi_incr                                        //remove
    , 0.0, 24.0);                                            //remove
                                                             //remove
    system->readParameter (cultivar.c_str()                  //remove
    , "y_hi_incr"/*,  "()"*/                                 //remove
    , pY_hi_incr                                             //remove
    , pNum_pp_hi_incr                                        //remove
    , 0.0, 1.0);                                             //remove
                                                             //remove
    system->readParameter (cultivar.c_str()                  //remove
    , "x_hi_max_pot_stress"/*,  "(0-1)"*/                    //remove
    , pX_hi_max_pot_stress, pNum_hi_max_pot                  //remove
    , 0.0, 1.0);                                             //remove
                                                             //remove
    system->readParameter (cultivar.c_str()                  //remove
    , "y_hi_max_pot"//, "(0-1)"                              //remove
    , pY_hi_max_pot, pNum_hi_max_pot                         //remove
    , 0.0, 1.0);                                             //remove
                                                             //remove
    if (system->readParameter (cultivar.c_str()              //remove
                , "min_temp_grnfill"//, "()"                 //remove
                , pMinTempGrnFill                            //remove
                , 0.0, 20.0, true) == false)                 //remove
    {                                                        //remove
        pMinTempGrnFill = -100.0;                            //remove
        pDaysDelayGrnFill = 0;                               //remove
    }                                                        //remove
    else                                                     //remove
    {                                                        //remove
       system->readParameter (cultivar.c_str()               //remove
               , "days_delay_grnfill"//, "()"                //remove
               , pDaysDelayGrnFill                           //remove
               , 0, 10);                                     //remove
    }                                                        //remove
                                                             //remove
    if (cGrain_no_option==2)                                 //remove
        {                                                    //remove
        system->readParameter (cultivar.c_str()              //remove
        , "grains_per_gram_stem"//, "(/g)"                   //remove
        , pGrains_per_gram_stem                              //remove
        , 0.0, 10000.0);                                     //remove
        }                                                    //remove
    if (cGrain_fill_option==2)                               //remove
        {                                                    //remove
        system->readParameter (cultivar.c_str()              //remove
        , "potential_grain_filling_rate"//, "(g/grain/day)"  //remove
        , pPotential_grain_filling_rate                      //remove
        , 0.0, 1.0);                                         //remove
        }                                                    //remove

   for (vector<plantPart *>::iterator t = myParts.begin();
        t != myParts.end();
        t++)
       (*t)->readCultivarParameters(system, cultivar);
}

//===========================================================================
void PlantFruit::writeCultivarInfo (protocol::Component *system)
//===========================================================================
{

    // report
//    grainPart->writeCultivarInfo(system);

    string s;                                                      //remove
                                                                   //remove
    // TEMPLATE OPTION                                             //remove
    s = string("   x_pp_hi_incr               = ");                //remove
    for (int i = 0; i < pNum_pp_hi_incr; i++)                      //remove
       s = s + ftoa(pX_pp_hi_incr[i], "10.2") + " ";               //remove
    system->writeString (s.c_str());                               //remove
                                                                   //remove
    s = string("   y_hi_incr                  = ");                //remove
    for (int i = 0; i < pNum_pp_hi_incr; i++)                      //remove
       s = s + ftoa(pY_hi_incr[i], "10.4") + " ";                  //remove
    system->writeString (s.c_str());                               //remove
                                                                   //remove
    s = string("   x_hi_max_pot_stress        = ");                //remove
    for (int i = 0; i < pNum_hi_max_pot; i++)                      //remove
       s = s + ftoa(pX_hi_max_pot_stress[i], "10.2") + " ";        //remove
    system->writeString (s.c_str());                               //remove
                                                                   //remove
    s = string("   y_hi_max_pot               = ");                //remove
    for (int i = 0; i < pNum_hi_max_pot; i++)                      //remove
       s = s + ftoa(pY_hi_max_pot[i], "10.2") + " ";               //remove
    system->writeString (s.c_str());                               //remove

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
   gLatitude = 0.0;                   //remove
   gMaxt = 0.0;                                 //remove
   gMint = 0.0;                                 //remove

   gDelayGrnFill  = 0.0;                        //remove
   gDaysDelayedGrnFill  = 0.0;                  //remove
   cGrain_no_option  = 0.0;                     //remove
   cGrain_fill_option  = 0.0;                   //remove
   cNum_temp_grainfill;                         //remove
   cGrain_n_option  = 0.0;                      //remove
   cSw_fac_max  = 0.0;                          //remove
   cTemp_fac_min  = 0.0;                        //remove
   cSfac_slope  = 0.0;                          //remove
   cTfac_slope  = 0.0;                          //remove
   cPotential_grain_n_filling_rate  = 0.0;      //remove
   cCrit_grainfill_rate  = 0.0;                 //remove
   cNum_temp_grain_n_fill;                      //remove
   cGrn_water_cont  = 0.0;                      //remove
   cCarbo_oil_conv_ratio  = 0.0;                //remove
   cGrain_oil_conc  = 0.0;                      //remove
   cNum_n_conc_stage;                           //remove
   cN_conc_crit_grain  = 0.0;                   //remove
   cN_conc_max_grain  = 0.0;                    //remove
   cN_conc_min_grain  = 0.0;                    //remove
   cCarbo_oil_conv_ratio = 0.0;                 //remove

//   cX_temp_grainfill[max_table];
//   cY_rel_grainfill[max_table];
//   cX_temp_grain_n_fill[max_table];
//   cY_rel_grain_n_fill[max_table];
//   cX_stage_code[max_table];
//   cY_n_conc_crit_pod[max_table];
//   cY_n_conc_max_pod[max_table];
//   cY_n_conc_min_pod[max_table];

   cTwilight = 0.0;                                         //remove
   fill_real_array (pX_pp_hi_incr, 0.0, max_table);         //remove
   pGrains_per_gram_stem = 0.0;                             //remove
   pPotential_grain_filling_rate = 0.0;                     //remove
                                                            //remove
   fill_real_array (pX_pp_hi_incr, 0.0, max_table);         //remove
   fill_real_array (pY_hi_incr, 0.0, max_table);            //remove
   pNum_pp_hi_incr = 0;                                     //remove
   pNum_hi_max_pot = 0;                                     //remove
   fill_real_array (pX_hi_max_pot_stress, 0.0, max_table);  //remove
   fill_real_array (pY_hi_max_pot, 0.0, max_table);         //remove
                                                            //remove
   pMinTempGrnFill = 0.0;                                   //remove
   pDaysDelayGrnFill = 0;                                   //remove
                                                            //remove
   gGrain_energy = 0.0;                                     //remove
   gGrain_no = 0.0;                                         //remove
   dmOil_conv_retranslocate = 0.0;                          //remove
                                                            //remove
   gDlt_dm_stress_max        = 0.0;                         //remove


   for (vector<plantPart *>::iterator t = myParts.begin();
        t != myParts.end();
        t++)
       (*t)->zeroAllGlobals();

}

void PlantFruit::zeroDeltas(void)
{
   plantPart::zeroDeltas();

   gDlt_dm_grain_demand = 0.0;                                    //remove
   gDlt_dm_oil_conv = 0.0;                                        //remove
   gDlt_dm = 0.0;

   gN_grain_demand = 0.0;                                          //remove
   gP_grain_demand = 0.0;                                          //remove

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

    refreshStates();
}

// ====================================================================
void PlantFruit::onKillStem(void)
// ====================================================================
{
    for (vector<plantPart *>::iterator part = myParts.begin();
         part != myParts.end();
         part++)
       (*part)->onKillStem();

    refreshStates();
    gGrain_no = 0.0;
}

// ====================================================================
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

// ====================================================================
void PlantFruit::refreshStates(void)
// ====================================================================
{
     g.dm_dead     = 0.0;
     g.dm_senesced = 0.0;
     g.dm_green    = 0.0;

     g.n_dead     = 0.0;
     g.n_senesced = 0.0;
     g.n_green    = 0.0;

     g.p_dead  = 0.0;
     g.p_sen   = 0.0;
     g.p_green = 0.0;

    for (vector<plantPart *>::iterator part = myParts.begin();
         part != myParts.end();
         part++)
    {
       g.dm_dead += (*part)->g.dm_dead;
       g.dm_green += (*part)->g.dm_green;
       g.dm_senesced += (*part)->g.dm_senesced;

       g.n_dead += (*part)->g.n_dead;
       g.n_green += (*part)->g.n_green;
       g.n_senesced += (*part)->g.n_senesced;

       g.p_dead += (*part)->g.p_dead;
       g.p_green += (*part)->g.p_green;
       g.p_sen += (*part)->g.p_sen;
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
    myVegParts.push_back(podPart);
    supplyPools.push_back(podPart);
                                                              //remove
    mealPart = new fruitMealPart(plant, "meal");              //remove
//    myThings.push_back(mealPart);                           //remove
    myParts.push_back(mealPart);                              //remove
    myGrainParts.push_back(mealPart);                         //remove
                                                              //remove
    oilPart = new fruitOilPart(plant, "oil");                 //remove
//    myThings.push_back(oilPart);                            //remove
    myParts.push_back(oilPart);                               //remove
    myGrainParts.push_back(oilPart);                          //remove

    grainPart = new fruitGrainPart(plant, "grain");
//    myParts.push_back(grainPart);
//    myGrainParts.push_back(grainPart);

    grainPart->doInit1();

}


//===========================================================================
void PlantFruit::readConstants(protocol::Component *system, const string &section)
//===========================================================================
{
      system->getVariable(idLatitude, gLatitude, -90.0, 90.0);                  //remove
      for (vector<plantPart *>::iterator t = myParts.begin();
           t != myParts.end();
           t++)
          (*t)->readConstants(system, section);

}

//===========================================================================
void PlantFruit::readSpeciesParameters(protocol::Component *system, vector<string> &sections)
//===========================================================================
{


    //    plant_phenology_init                             //remove
    system->readParameter (sections                        //remove
                   , "twilight"//, "(o)"                   //remove
                   , cTwilight                             //remove
                   , -90.0, 90.0);                         //remove

    int cPartition_option = 0;                             //remove
    system->readParameter (sections,                       //remove
                       "partition_option"//, "()"          //remove
                      , cPartition_option                  //remove
                      , 1, 3);                             //remove
                                                           //remove
    if (cPartition_option==1 )                             //remove
    {                                                      //remove
    }                                                      //remove
    else if (cPartition_option==3)                         //remove
        {                                                  //remove
           // do nothing                                   //remove
        }                                                  //remove
    //    grain number                                     //remove
    system->readParameter (sections                        //remove
                      ,"grain_no_option"//, "()"           //remove
                      , cGrain_no_option                   //remove
                      , 1, 2);                             //remove
                                                           //remove
    //    legume grain filling                             //remove
    system->readParameter (sections                        //remove
                      ,"grain_fill_option"//,/ "()"        //remove
                      , cGrain_fill_option                 //remove
                      , 1, 3);                             //remove
                                                           //remove
    if (cGrain_fill_option==2 || cGrain_fill_option==3)    //remove
        {                                                  //remove
        system->readParameter (sections                    //remove
                         , "x_temp_grainfill"              //remove
                         //, "(oc)"                        //remove
                         , cX_temp_grainfill               //remove
                         , cNum_temp_grainfill             //remove
                         , 0.0                             //remove
                         , 40.0);                          //remove
                                                           //remove
        system->readParameter (sections                    //remove
                         ,"y_rel_grainfill"                //remove
                         //, "(-)"                         //remove
                         , cY_rel_grainfill                //remove
                         , cNum_temp_grainfill             //remove
                         , 0.0                             //remove
                         , 1.0);                           //remove
        }                                                  //remove
                                                           //remove
     //    plant_n_dlt_grain_conc                          //remove
     system->readParameter (sections                       //remove
                        , "grain_n_option"//, "()"         //remove
                        , cGrain_n_option                  //remove
                        , 1, 2);                           //remove
                                                           //remove
     if (cGrain_n_option==1)                               //remove
         {                                                 //remove
         system->readParameter (sections                   //remove
                        ,"sw_fac_max"//, "()"              //remove
                        , cSw_fac_max                      //remove
                        , 0.0, 100.0);                     //remove
                                                           //remove
         system->readParameter (sections                   //remove
                        ,"temp_fac_min"//, "()"            //remove
                        , cTemp_fac_min                    //remove
                        , 0.0, 100.0);                     //remove
                                                           //remove
         system->readParameter (sections                   //remove
                        ,"sfac_slope"//, "()"              //remove
                        , cSfac_slope                      //remove
                        , -10.0, 0.0);                     //remove
                                                           //remove
         system->readParameter (sections                   //remove
                        ,"tfac_slope"//, "()"              //remove
                        , cTfac_slope                      //remove
                        , 0.0, 100.0);                     //remove
         }                                                 //remove
     else                                                              //remove
         {                                                             //remove
         system->readParameter (sections                               //remove
                        , "potential_grain_n_filling_rate"//, "()"     //remove
                        , cPotential_grain_n_filling_rate              //remove
                        , 0.0, 1.0);                                   //remove
                                                                       //remove
         system->readParameter (sections                               //remove
                        , "crit_grainfill_rate"//, "(mg/grain/d)"      //remove
                        , cCrit_grainfill_rate                         //remove
                        , 0.0, 1.0);                                   //remove
                                                                       //remove
         system->readParameter (sections                               //remove
                          , "x_temp_grain_n_fill"//,  "(oC)"           //remove
                          , cX_temp_grain_n_fill                       //remove
                          , cNum_temp_grain_n_fill                     //remove
                          , 0.0                                        //remove
                          , 40.0);                                     //remove
                                                                       //remove
         system->readParameter (sections                               //remove
                          , "y_rel_grain_n_fill"                       //remove
                          //, "(-)"                                    //remove
                          , cY_rel_grain_n_fill                        //remove
                          , cNum_temp_grain_n_fill                     //remove
                          , 0.0                                        //remove
                          , 1.0);                                      //remove
         }                                                             //remove
                                                                       //remove
    //    plant_event                                                  //remove
    system->readParameter (sections                                    //remove
                   ,"grn_water_cont"//, "(g/g)"                        //remove
                   , cGrn_water_cont                                   //remove
                   , 0.0, 1.0);                                        //remove
                                                                       //remove
    //    plant_dm_partition                                           //remove
    system->readParameter (sections                                    //remove
                   ,"carbo_oil_conv_ratio"//, "()"                     //remove
                   , cCarbo_oil_conv_ratio                             //remove
                   , 0.0, 20.0);                                       //remove
                                                                       //remove
    system->readParameter (sections                                    //remove
                   ,"grain_oil_conc"//, "()"                           //remove
                   , cGrain_oil_conc                                   //remove
                   , 0.0, 1.0);                                        //remove
                                                                       //remove
    system->readParameter (sections                                    //remove
                     , "x_stage_code"//, "()"                          //remove
                     , cX_stage_code, cNum_n_conc_stage                //remove
                     , 0.0, 100.0);                                    //remove
                                                                       //remove
    system->readParameter (sections                                    //remove
                   , "n_conc_crit_grain"//, "()"                       //remove
                   , cN_conc_crit_grain                                //remove
                   , 0.0, 100.0);                                      //remove
                                                                       //remove
    system->readParameter (sections                                    //remove
                   , "n_conc_max_grain"//, "()"                        //remove
                   , cN_conc_max_grain                                 //remove
                   , 0.0, 100.0);                                      //remove
                                                                       //remove
    system->readParameter (sections                                    //remove
                   , "n_conc_min_grain"//, "()"                        //remove
                   , cN_conc_min_grain                                 //remove
                   , 0.0, 100.0);                                      //remove
                                                                       //remove
    if (cGrain_fill_option == 3)                                       //remove
        {                                                              //remove
        system->readParameter (sections                                //remove
                          , "x_temp_grainfill"                         //remove
                          //, "(oC)"                                   //remove
                          , cX_temp_grainfill                          //remove
                          , cNum_temp_grainfill                        //remove
                          , 0.0                                        //remove
                          , 40.0);                                     //remove
                                                                       //remove
        system->readParameter (sections                                //remove
                          , "y_rel_grainfill"                          //remove
                          //, "(-)"                                    //remove
                          , cY_rel_grainfill                           //remove
                          , cNum_temp_grainfill                        //remove
                          , 0.0                                        //remove
                          , 1.0);                                      //remove
        }                                                              //remove
      gHasreadconstants = true;

        for (vector<plantPart *>::iterator part = myParts.begin();
             part != myParts.end();
             part++)
           (*part)->readSpeciesParameters(system, sections);

}


//===========================================================================
float PlantFruit::dmGreen(void)
//===========================================================================
{
    g.dm_green = 0.0;

    vector<plantPart *>::iterator part;
    for (part = myParts.begin(); part != myParts.end(); part++)
      g.dm_green +=(*part)->g.dm_green;

    return g.dm_green;
}

//===========================================================================
float PlantFruit::dltDmGreen(void)
//===========================================================================
{
    dlt.dm_green = 0.0;

    vector<plantPart *>::iterator part;
    for (part = myParts.begin(); part != myParts.end(); part++)
      dlt.dm_green +=(*part)->dlt.dm_green;

    return dlt.dm_green;
}

//===========================================================================
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

//===========================================================================
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

//===========================================================================
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

//===========================================================================
void PlantFruit::update(void)
//===========================================================================
{

    vector<plantPart *>::iterator part;

// Update
    for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->update();

// Update N
    for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->updateN();

        g.n_green = 0.0;
        g.n_senesced = 0.0;
        g.n_dead = 0.0;
        dlt.n_senesced_dead = 0.0;
        dlt.n_green_dead = 0.0;

        for (part = myParts.begin();
             part != myParts.end();
             part++)
        {
           g.n_green += (*part)->nGreen();
           g.n_senesced += (*part)->nSenesced();
           g.n_dead += (*part)->nDead();
           dlt.n_senesced_dead += (*part)->dlt.n_senesced_dead;
           dlt.n_green_dead += (*part)->dlt.n_green_dead;
        }

// Update DM
    for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->updateDm();

        g.dm_green = 0.0;
        g.dm_senesced = 0.0;
        g.dm_dead = 0.0;
        dlt.dm_senesced_dead = 0.0;
        dlt.dm_green_dead = 0.0;
        for (part = myParts.begin();
             part != myParts.end();
             part++)
        {
           g.dm_green += (*part)->dmGreen();
           g.dm_senesced += (*part)->dmSenesced();
           g.dm_dead += (*part)->dmDead();
           dlt.dm_senesced_dead += (*part)->dlt.dm_senesced_dead;
           dlt.dm_green_dead += (*part)->dlt.dm_green_dead;
        }


// Update P
    if (plant->phosphorusAware())
    {
    for (part = myParts.begin(); part != myParts.end(); part++)
      (*part)->updateP();
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
           g.p_green += (*part)->pGreen();
           g.p_sen += (*part)->pSenesced();
           g.p_dead += (*part)->pDead();
//           dlt.p_senesced_dead += (*part)->dlt.p_senesced_dead;
//           dlt.p_green_dead += (*part)->dlt.p_green_dead;
        }

    // transfer plant grain no.
    float dlt_grain_no_lost  = gGrain_no * plant->getDyingFractionPlants();
    gGrain_no -= dlt_grain_no_lost;


    if (plant->inPhase("hi_stress_sensitive")) gDm_stress_max.update();
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

//===========================================================================
void PlantFruit::processBioDemand(void)
//===========================================================================
{

//  for (vector<plantPart *>::iterator t = myParts.begin();
//       t != myParts.end();
//       t++)
//     (*t)->processBioDemand();

    bio_water1 ();                //remove to pod
    yieldpart_demand_stress1 (); //remove to grain
    bio_grain_oil ();            //remove to grain
    bio_grain_demand ();         //remove to grain

    return;
}

float PlantFruit::grainNo(void) const {return gGrain_no;}                                                     //remove
float PlantFruit::grainEnergy(void) const {return gGrain_energy;}                                             //remove
float PlantFruit::grainNDemand(void) const {return gN_grain_demand;}                                          //remove
float PlantFruit::dltDmPotTe(void) {return podPart->dltDmPotTe();}                                            //remove
float PlantFruit::dltDmPotRuePod(void) {return podPart->dltDmPotRuePod();}                                    //remove
float PlantFruit::grainNConcPercent(void) {return divide (nGrainTotal(), dmGrainTotal(), 0.0) * fract2pcnt;}  //remove
float PlantFruit::dltDmGrainDemand(void) const {return gDlt_dm_grain_demand;}                                 //remove

//float PlantFruit::grainNo(void) const {return grainPart->grainNo();}
//float PlantFruit::grainEnergy(void) const {return grainPart->grainEnergy();}
//float PlantFruit::grainNDemand(void) const {return grainPart->grainNDemand();}
//float PlantFruit::dltDmPotTe(void) {return podPart->dltDmPotTe();}
//float PlantFruit::dltDmPotRuePod(void) {return podPart->dltDmPotRuePod();}
//float PlantFruit::grainNConcPercent(void) {return grainPart->grainNConcPercent();}
//float PlantFruit::dltDmGrainDemand(void) const {return grainPart->dltDmGrainDemand();}

void PlantFruit::calcDlt_pod_area (void)  {podPart->calcDlt_pod_area();}
float PlantFruit::meanT (void) {return 0.5 * (gMaxt + gMint);}                  //remove

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

float PlantFruit::interceptRadiation (float radiation) {return podPart->interceptRadiation(radiation);}
void PlantFruit::dm_pot_rue (double  radn_int_pod ) {podPart->dm_pot_rue(radn_int_pod);}
void PlantFruit::transp_eff_co2() {podPart->transp_eff_co2();}
void PlantFruit::sw_demand1(float *sw_demand) {podPart->sw_demand1(sw_demand);}
void PlantFruit::bio_water1 (void) {podPart->bio_water1();}                          //remove

//===========================================================================                             //remove
void PlantFruit::bio_grain_oil (void)                                // for seed energy content (>= 1.0)  //remove
//===========================================================================                             //remove
{                                                                                                         //remove
   //+  Purpose                                                                                           //remove
//       Calculate grain oil factors                                                                      //remove
                                                                                                          //remove
//+  Changes                                                                                              //remove
//      141100 jngh specified and programmed                                                              //remove
                                                                                                          //remove
//- Implementation Section ----------------------------------                                             //remove
                                                                                                          //remove
    gGrain_energy = 1.0 + cGrain_oil_conc * (cCarbo_oil_conv_ratio - 1.0);                                //remove
    bound_check_real_var (parentPlant, gGrain_energy, 1.0, 2.0, "grain_energy");                          //remove
}                                                                                                         //remove

//===========================================================================                   //remove
void PlantFruit::bio_grain_demand (void)                                                        //remove
//===========================================================================                   //remove
{                                                                                               //remove
//+  Purpose                                                                                    //remove
//       Simulate crop grain biomass demand.                                                    //remove
                                                                                                //remove
//+  Mission Statement                                                                          //remove
//     Calculate grain biomass demand                                                           //remove
                                                                                                //remove
//+  Changes                                                                                    //remove
//      250894 jngh specified and programmed                                                    //remove
                                                                                                //remove
//- Implementation Section ----------------------------------                                   //remove
                                                                                                //remove
    if (cGrain_fill_option == 1)                                                                //remove
       {                                                                                        //remove
        bio_yieldpart_demand1();                                                                //remove
        }                                                                                       //remove
    else if (cGrain_fill_option == 2)                                                           //remove
        {                                                                                       //remove
        if (plant->inPhase("grainfill"))                                                        //remove
           bio_yieldpart_demand2();                                                             //remove
        else                                                                                    //remove
           gDlt_dm_grain_demand = 0.0;                                                          //remove
        }                                                                                       //remove
    else                                                                                        //remove
        {                                                                                       //remove
        throw std::invalid_argument("invalid template option in plant_bio_grain_demand");       //remove
        }                                                                                       //remove
                                                                                                //remove
    return;                                                                                     //remove
}                                                                                               //remove

//===========================================================================               //remove
void PlantFruit::bio_yieldpart_demand2(void)                                                //remove
//===========================================================================               //remove
{                                                                                           //remove
//+  Purpose                                                                                //remove
//       Perform grain filling calculations                                                 //remove
                                                                                            //remove
//+  Changes                                                                                //remove
    //+  Local Variables                                                                    //remove
    float tav;                                                                              //remove
                                                                                            //remove
        // we are in grain filling stage                                                    //remove
        tav = meanT();                                                                      //remove
                                                                                            //remove
        gDlt_dm_grain_demand = gGrain_no                                                    //remove
                             * pPotential_grain_filling_rate                                //remove
                             * linear_interp_real(tav                                       //remove
                                                  ,cX_temp_grainfill                        //remove
                                                  ,cY_rel_grainfill                         //remove
                                                  ,cNum_temp_grainfill);                    //remove
                                                                                            //remove
}                                                                                           //remove

void PlantFruit::bio_actual (void) {podPart->bio_actual();}

//===========================================================================          //remove
void PlantFruit::bio_yieldpart_demand1(void)                                           //remove
//===========================================================================          //remove
{                                                                                      //remove
//+  Purpose                                                                           //remove
//        Find grain demand for carbohydrate using harvest index (g/m^2)               //remove
                                                                                       //remove
//+  Mission Statement                                                                 //remove
//   Calculate yield component biomass demand using harvest index increments           //remove
                                                                                       //remove
//+  Changes                                                                           //remove
//     010994 jngh specified and programmed                                            //remove
                                                                                       //remove
//+  Local Variables                                                                   //remove
//    float ave_stress;                             // average dm_stress from flowering//remove to gra
    float dlt_dm_yield;                           // grain demand for carbohydrate (g/m//remove^2)
    float dlt_dm_yield_unadj;                     // grain demand for carbohydrate, una//removedjusted
// for energy conversion (g/m^2)                                                       //remove
    float harvest_index;                          // last harvest index (g grain/g biom//removeass)
    float hi_max_pot;                             // max potential HI due to stress    //remove
    float dm_tops_new;                            // new drymatter  tops (g/m^2)       //remove
    float harvest_index_new;                      // next harvest index (g grain/g biom//removeass)
    float dm_grain_new;                           // new drymatter grain (g/m^2)       //remove
    float energy_adjust;                          // adjustment for energy used in oil //removeconversion.
    float hi_incr;                                // harvest index increment per day   //remove
    float photoperiod;                            // hours of photosynthetic light (hou//removers)
                                                                                       //remove
//- Implementation Section ----------------------------------                          //remove
                                                                                       //remove
    if (plant->inPhase("grainfill"))                                                   //remove
        {                                                                              //remove
                                                                                       //remove
//        ave_stress = divide ((*g_dm_stress_max).getSum(),                            //remove
//                             (*g_dm_stress_max).getN(),                              //remove
//                             1.0);                                                   //remove
        hi_max_pot = linear_interp_real(gDm_stress_max.getAverage()                    //remove
                                        ,pX_hi_max_pot_stress                          //remove
                                        ,pY_hi_max_pot                                 //remove
                                        ,pNum_hi_max_pot);                             //remove
                                                                                       //remove
        photoperiod = day_length (gDay_of_year, gLatitude, cTwilight);                 //remove
                                                                                       //remove
        hi_incr = linear_interp_real(photoperiod                                       //remove
                                    ,pX_pp_hi_incr                                     //remove
                                    ,pY_hi_incr                                        //remove
                                    ,pNum_pp_hi_incr);                                 //remove
                                                                                       //remove
        // effective grain filling period                                              //remove
        float dm_green_yield_parts = grainPart->dmGreen();                             //remove
         dm_green_yield_parts = mealPart->dmGreen() + oilPart->dmGreen();   //remove   //remove
                                                                                       //remove
        harvest_index = divide (dm_green_yield_parts, plant->getDmTops(), 0.0);        //remove
        dm_tops_new = plant->getDmTops() + plant->getDltDm();                          //remove
                                                                                       //remove
        harvest_index_new = u_bound (harvest_index + hi_incr, hi_max_pot);             //remove
                                                                                       //remove
        dm_grain_new = dm_tops_new * harvest_index_new;                                //remove
        dlt_dm_yield_unadj = dm_grain_new - dm_green_yield_parts;                      //remove
                                                                                       //remove
    // adjust for grain energy                                                         //remove
                                                                                       //remove
        dlt_dm_yield_unadj = bound (dlt_dm_yield_unadj, 0.0, dm_grain_new);            //remove
                                                                                       //remove
        energy_adjust = divide (gGrain_energy                                          //remove
                                , 1.0 + harvest_index_new*(gGrain_energy - 1.0)        //remove
                                , 0.0);                                                //remove
                                                                                       //remove
        dlt_dm_yield = dlt_dm_yield_unadj * energy_adjust;                             //remove
    //jh         dlt_dm_yield = dlt_dm_yield_unadj                                     //remove
                                                                                       //remove
        if (gMint <= pMinTempGrnFill)                                                  //remove
        {                                                                              //remove
            gDelayGrnFill = true;                                                      //remove
        }                                                                              //remove
        if (gDelayGrnFill)                                                             //remove
        {                                                                              //remove
            dlt_dm_yield = 0.0;                                                        //remove
            gDaysDelayedGrnFill = gDaysDelayedGrnFill + 1;                             //remove
            if (gDaysDelayedGrnFill == pDaysDelayGrnFill)                              //remove
            {                                                                          //remove
                  gDelayGrnFill = false ;                                              //remove
                  gDaysDelayedGrnFill = 0;                                             //remove
            }                                                                          //remove
        }                                                                              //remove
        }                                                                              //remove
    else                                                                               //remove
        {                                                                              //remove
        // we are out of grain fill period                                             //remove
        dlt_dm_yield = 0.0;                                                            //remove
        }                                                                              //remove
//        ostrstream msg;                                                              //remove
//       msg << g_mint << gDelayGrnFill << gDaysDelayedGrnFill << endl;                //remove
//       parent->writeString (msg.str());                                              //remove
                                                                                       //remove
                                                                                       //remove
//    *dlt_dm_grain_demand = dlt_dm_yield;                                             //remove
    gDlt_dm_grain_demand = dlt_dm_yield;                                               //remove
                                                                                       //remove
    return;                                                                            //remove
    }                                                                                  //remove

//===========================================================================
void PlantFruit::grain_n_demand1(float g_nfact_grain_conc      //   (INPUT)
                               , float g_swdef_expansion)        //   grain N demand (g/m^2)
//===========================================================================
{
//  Purpose
//    Calculate plant n demand

      float   n_potential;           // maximum grain N demand (g/m^2)

//- Implementation Section ----------------------------------

//      grainPart->grain_n_demand1(g.nfact_grain_conc
//                                 , g.swdef_expansion);

      gN_grain_demand = (mealPart->dlt.dm_green + mealPart->dlt.dm_green_retrans)    //remove
                      * n_dlt_grain_conc(mealPart                                    //remove
                          , cSfac_slope                                              //remove
                          , cSw_fac_max                                              //remove
                          , cTemp_fac_min                                            //remove
                          , cTfac_slope                                              //remove
                          , meanT()                                                  //remove
                          , g_nfact_grain_conc                                       //remove
                          , g_swdef_expansion);                                      //remove
                                                                                     //remove
                                                                                     //remove
      n_potential  = (mealPart->g.dm_green                                           //remove
                     + mealPart->dlt.dm_green                                        //remove
                     + mealPart->dlt.dm_green_retrans)                               //remove
                   * mealPart->g.n_conc_max;                                         //remove
                                                                                     //remove
      gN_grain_demand = u_bound (gN_grain_demand                                     //remove
                                , n_potential - mealPart->g.n_green);                //remove

   }

//===========================================================================
void PlantFruit::grain_n_demand2 (void)
//===========================================================================
{
//      fruitPart->grain_n_demand2();

      const char *my_name = "plant_grain_n_demand2";                                               //remove
                                                                                                   //remove
      float Tav ;                                                                                  //remove
      float grain_growth;                                                                          //remove
                                                                                                   //remove
      push_routine (my_name);                                                                      //remove
                                                                                                   //remove
      // default case                                                                              //remove
      gN_grain_demand = 0.0;                                                                       //remove
                                                                                                   //remove
      if (plant->inPhase("reproductive"))                                                          //remove
         {                                                                                         //remove
         // we are in grain filling stage                                                          //remove
         Tav = meanT();                                                                            //remove
                                                                                                   //remove
         gN_grain_demand = gGrain_no                                                               //remove
                               * cPotential_grain_n_filling_rate                                   //remove
                               * linear_interp_real (Tav, cX_temp_grain_n_fill                     //remove
                                                    , cY_rel_grain_n_fill                          //remove
                                                    , cNum_temp_grain_n_fill);                     //remove
         }                                                                                         //remove
                                                                                                   //remove
      if (plant->inPhase("grainfill"))                                                             //remove
         {                                                                                         //remove
         // during grain C filling period so make sure that C filling is still                     //remove
         // going on otherwise stop putting N in now                                               //remove
                                                                                                   //remove
         grain_growth = divide(mealPart->dlt.dm_green + mealPart->dlt.dm_green_retrans             //remove
                              , gGrain_no                                                          //remove
                              , 0.0);                                                              //remove
         if (grain_growth < cCrit_grainfill_rate)                                                  //remove
            {                                                                                      //remove
            //! grain filling has stopped - stop n flow as well                                    //remove
            gN_grain_demand = 0.0;                                                                 //remove
            }                                                                                      //remove
         }                                                                                         //remove
                                                                                                   //remove
      pop_routine (my_name);                                                                       //remove
   }
//==========================================================================                                          //remove
float PlantFruit::n_dlt_grain_conc(plantPart *grainPart                                                               //remove
                           , float sfac_slope      //(INPUT)  soil water stress factor slope                          //remove
                           , float sw_fac_max      //(INPUT)  soil water stress factor maximum                        //remove
                           , float temp_fac_min    //(INPUT)  temperature stress factor minimum optimum temp          //remove
                           , float tfac_slope      //(INPUT)  temperature stress factor slope                         //remove
                           , float ave_temp         //(INPUT)  mean air temperature (oC)                              //remove
                           , float nfact_grain_conc// (INPUT)                                                         //remove
                           , float swdef_expansion) // (INPUT)                                                        //remove
//==========================================================================                                          //remove
                                                                                                                      //remove
/*  Purpose                                                                                                           //remove
*     Calculate the nitrogen concentration required to meet the increase                                              //remove
*     from daily grain growth (0-1) as affected by temperature and water stress.                                      //remove
*                                                                                                                     //remove
*  Mission Statement                                                                                                  //remove
*   Calculate the nitrogen concentration required for grain growth.                                                   //remove
*                                                                                                                     //remove
*  Notes                                                                                                              //remove
*     First, two factors are calculated and used to estimate the                                                      //remove
*     effects of mean temperature and drought stress on the N                                                         //remove
*     concentration in grain growth for the day.  High temperature                                                    //remove
*     or drought stress can cause the factors to exceed 1.                                                            //remove
*     N deficiency can cause nfac < 1.  The net effect of these                                                       //remove
*     equations is to allow grain nitrogen concentration to range                                                     //remove
*     from less than .01 when N deficiency is severe to about .018                                                    //remove
*     stress limit grain growth.                                                                                      //remove
*     Here, optimum N concentration = 1.7%                                                                            //remove
*                                                                                                                     //remove
*/                                                                                                                    //remove
   {                                                                                                                  //remove
   //  Local Variables                                                                                                //remove
   float N_conc_pot;                   // potential grain N concentration (0-1) (g N/g part)                          //remove
   float N_grain_sw_fac;               // soil water stress factor for N uptake                                       //remove
   float N_grain_temp_fac;             // temperature stress factor for N uptake                                      //remove
                                                                                                                      //remove
   //!!!!!!!!!! return to orig cm                                                                                     //remove
   N_grain_temp_fac = temp_fac_min + tfac_slope * ave_temp;                                                           //remove
   N_grain_sw_fac = sw_fac_max - sfac_slope * swdef_expansion ;                                                       //remove
                                                                                                                      //remove
   // N stress reduces grain N concentration below critical                                                           //remove
   N_conc_pot = grainPart->g.n_conc_min + (grainPart->g.n_conc_crit - grainPart->g.n_conc_min) * nfact_grain_conc;    //remove
                                                                                                                      //remove
            // Temperature and water stresses can decrease/increase grain                                             //remove
            // N concentration                                                                                        //remove
                                                                                                                      //remove
            // when there is no N stress, the following can be a higher N conc than                                   //remove
            // the crit and thus the N conc of the grain can exceed N critical.                                       //remove
                                                                                                                      //remove
   return  (N_conc_pot * max (N_grain_temp_fac, N_grain_sw_fac));                                                     //remove
   }                                                                                                                  //remove

//===========================================================================
float PlantFruit::dm_yield_demand ( float  g_dlt_dm_veg_supply)
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
      float dlt_dm_supply_by_pod = 0.0;  // FIXME
      g_dlt_dm_veg_supply += dlt_dm_supply_by_pod;

      float cFracPod = podPart->fracPod1();                               //belongs in podPart FIXME

      dm_grain_demand = divide (gDlt_dm_grain_demand, gGrain_energy, 0.0);    //belongs in grainpart

      if (dm_grain_demand > 0.0)                                        //belongs in podPart FIXME
      {                                                                 //belongs in podPart FIXME
         dm_pod_demand = dm_grain_demand * cFracPod;                    //belongs in podPart FIXME
      }                                                                 //belongs in podPart FIXME
      else                                                              //belongs in podPart FIXME
      {                                                                 //belongs in podPart FIXME
         dm_pod_demand = g_dlt_dm_veg_supply * cFracPod;        // fix  //belongs in podPart FIXME
      }                                                                 //belongs in podPart FIXME

      dm_yield_demand = dm_pod_demand
                      + gDlt_dm_grain_demand
                      - dlt_dm_supply_by_pod;

      return dm_yield_demand;
}

//===========================================================================
float PlantFruit::dm_yield_demand2 ( float  g_dlt_dm_veg_supply)
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
      float dlt_dm_supply_by_pod = 0.0;  // FIXME
      g_dlt_dm_veg_supply += dlt_dm_supply_by_pod;


      float fracPod = podPart->fracPod();                                   //belongs in podPart FIXME

      dm_grain_demand = divide (gDlt_dm_grain_demand, gGrain_energy, 0.0);    //belongs in grainpart

      if (dm_grain_demand > 0.0)                                         //belongs in podPart FIXME
      {                                                                  //belongs in podPart FIXME
         dm_pod_demand = dm_grain_demand * fracPod;                      //belongs in podPart FIXME
      }                                                                  //belongs in podPart FIXME
      else                                                               //belongs in podPart FIXME
      {                                                                  //belongs in podPart FIXME
         dm_pod_demand = g_dlt_dm_veg_supply * fracPod;        // fix    //belongs in podPart FIXME
      }                                                                  //belongs in podPart FIXME

      dm_yield_demand = dm_pod_demand
                      + gDlt_dm_grain_demand
                      - dlt_dm_supply_by_pod;

      return dm_yield_demand;
}

//     ===========================================================
void PlantFruit::dm_partition1 (double g_dlt_dm)
//     ===========================================================
{

//+  Purpose
//       Partitions new dm (assimilate) between plant components (g/m^2)

//+  Changes
//      170703 jngh specified and programmed

//+  Local Variables
    double dlt_dm_green_tot;                       // total of partitioned dm (g/m^2)
    double yield_demand;                           // sum of grain, energy & pod
    double dm_grain_demand;                        // assimilate demand for grain (g/m^2)
    double dm_meal_demand;                         // assimilate demand for meal (g/m^2)
    double dm_oil_demand;                          // assimilate demand for oil (g/m^2)
    double dm_oil_conv_demand;                     // assimilate demand for conversion to oil (g/m^2)
    double dm_pod_demand;                          // assimilate demand for pod (g/m^2)

//- Implementation Section ----------------------------------

      float dlt_dm_supply_by_pod = 0.0;  // FIXME
      g_dlt_dm += dlt_dm_supply_by_pod;

    float fracPod = podPart->fracPod1();

     for (vector<plantPart *>::iterator t = myParts.begin();      //FIXME later
          t != myParts.end();
          t++)
        (*t)->dlt.dm_green = 0.0;

     gDlt_dm_oil_conv = 0.0;

    // calculate demands of reproductive parts
    dm_grain_demand = grainPart->dm_yield_demand();
    dm_grain_demand = divide (gDlt_dm_grain_demand, gGrain_energy, 0.0);   //remove

    dm_meal_demand = dm_grain_demand * (1.0 - cGrain_oil_conc);
    dm_oil_demand = dm_grain_demand - dm_meal_demand;
    dm_oil_conv_demand = gDlt_dm_grain_demand - dm_grain_demand;

    if (dm_grain_demand > 0.0)                          //belongs in podPart FIXME
    {                                                   //belongs in podPart FIXME
        dm_pod_demand = dm_grain_demand * fracPod;      //belongs in podPart FIXME
    }                                                   //belongs in podPart FIXME
    else                                                //belongs in podPart FIXME
    {                                                   //belongs in podPart FIXME
        dm_pod_demand = g_dlt_dm;                       //belongs in podPart FIXME
    }                                                   //belongs in podPart FIXME
    yield_demand = dm_pod_demand
                 + dm_grain_demand;

    yield_demand = dm_pod_demand            //remove
                 + dm_meal_demand           //remove
                 + dm_oil_demand            //remove
                 + dm_oil_conv_demand;      //remove

         // now distribute the assimilate to plant parts
    if (yield_demand >= g_dlt_dm)
            // reproductive demand exceeds supply - distribute assimilate to those parts only
    {
            // reproductive demand exceeds supply - distribute assimilate to those parts only
        grainPart->dlt.dm_green = g_dlt_dm * divide (dm_grain_demand    , yield_demand, 0.0);
        mealPart->dlt.dm_green = g_dlt_dm * divide (dm_meal_demand    , yield_demand, 0.0);     //remove
        oilPart->dlt.dm_green  = g_dlt_dm * divide (dm_oil_demand     , yield_demand, 0.0);     //remove
        gDlt_dm_oil_conv       = g_dlt_dm * divide (dm_oil_conv_demand, yield_demand, 0.0);     //remove

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
        grainPart->dlt.dm_green   = dm_grain_demand;
        mealPart->dlt.dm_green   = dm_meal_demand;        //remove
        oilPart->dlt.dm_green    = dm_oil_demand;         //remove
        gDlt_dm_oil_conv         = dm_oil_conv_demand;    //remove
        podPart->dlt.dm_green    = dm_pod_demand;

    }

////     for (vector<plantPart *>::iterator t = myParts.begin();      //FIXME later
////          t != myParts.end();
////          t++)
////        (*t)->dm_partition1 (g_dlt_dm);

     dltDmGreen();      // update fruit dlt.dm_green

    // do mass balance check
    dlt_dm_green_tot = dlt.dm_green
                     + gDlt_dm_oil_conv;      //FIXME

    if (!reals_are_equal(dlt_dm_green_tot, g_dlt_dm, 1.0E-4))  // XX this is probably too much slop - try doubles XX
    {
         string msg = "Fruit dlt_dm_green_tot mass balance is off: "
                    + ftoa(dlt_dm_green_tot, ".6")
                    + " vs "
                    + ftoa(g_dlt_dm, ".6");
         parentPlant->warningError(msg.c_str());
    }

      // check that deltas are in legal range       //FIXME need to do something about this when array is removed
//    bound_check_real_array (parentPlant, dlt_dm_green, max_part, 0.0, g_dlt_dm, "Fruit dlt.dm_green");

}

//     ===========================================================
void PlantFruit::dm_partition2 (double g_dlt_dm)
//     ===========================================================
{

//+  Purpose
//       Partitions new dm (assimilate) between plant components (g/m^2)

//+  Changes
//      170703 jngh specified and programmed

//+  Local Variables
    double dlt_dm_green_tot;                       // total of partitioned dm (g/m^2)
    double yield_demand;                           // sum of grain, energy & pod
    double dm_grain_demand;                        // assimilate demand for grain (g/m^2)
    double dm_meal_demand;                         // assimilate demand for meal (g/m^2)
    double dm_oil_demand;                          // assimilate demand for oil (g/m^2)
    double dm_oil_conv_demand;                     // assimilate demand for conversion to oil (g/m^2)
    double dm_pod_demand;                          // assimilate demand for pod (g/m^2)

//- Implementation Section ----------------------------------

      float dlt_dm_supply_by_pod = 0.0;  // FIXME
      g_dlt_dm += dlt_dm_supply_by_pod;

      float fracPod = podPart->fracPod();

         // first we zero all plant component deltas

     for (vector<plantPart *>::iterator t = myParts.begin();
          t != myParts.end();
          t++)
        (*t)->zeroDltDmGreen();

     gDlt_dm_oil_conv = 0.0;

    // calculate demands of reproductive parts
    dm_grain_demand = grainPart->dm_yield_demand();
    dm_grain_demand = divide (gDlt_dm_grain_demand, gGrain_energy, 0.0);   //remove

    dm_meal_demand = dm_grain_demand * (1.0 - cGrain_oil_conc);
    dm_oil_demand = dm_grain_demand - dm_meal_demand;
    dm_oil_conv_demand = gDlt_dm_grain_demand - dm_grain_demand;

    if (dm_grain_demand > 0.0)                        //belongs in podPart FIXME
    {                                                 //belongs in podPart FIXME
        dm_pod_demand = dm_grain_demand * fracPod;    //belongs in podPart FIXME
    }                                                 //belongs in podPart FIXME
    else                                              //belongs in podPart FIXME
    {                                                 //belongs in podPart FIXME
        dm_pod_demand = g_dlt_dm;                     //belongs in podPart FIXME
    }                                                 //belongs in podPart FIXME
    yield_demand = dm_pod_demand
                 + dm_grain_demand;

    yield_demand = dm_pod_demand          //remove
                 + dm_meal_demand         //remove
                 + dm_oil_demand          //remove
                 + dm_oil_conv_demand;    //remove

         // now distribute the assimilate to plant parts
    if (yield_demand >= g_dlt_dm)
            // reproductive demand exceeds supply - distribute assimilate to those parts only
    {
            // reproductive demand exceeds supply - distribute assimilate to those parts only
        grainPart->dlt.dm_green = g_dlt_dm * divide (dm_grain_demand    , yield_demand, 0.0);
        mealPart->dlt.dm_green = g_dlt_dm * divide (dm_meal_demand    , yield_demand, 0.0);    //remove
        oilPart->dlt.dm_green  = g_dlt_dm * divide (dm_oil_demand     , yield_demand, 0.0);    //remove
        gDlt_dm_oil_conv       = g_dlt_dm * divide (dm_oil_conv_demand, yield_demand, 0.0);    //remove

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
        grainPart->dlt.dm_green   = dm_grain_demand;
        mealPart->dlt.dm_green   = dm_meal_demand;         //remove
        oilPart->dlt.dm_green    = dm_oil_demand;          //remove
        gDlt_dm_oil_conv         = dm_oil_conv_demand;     //remove
        podPart->dlt.dm_green    = dm_pod_demand;

    }

     dltDmGreen();   // update fruit dlt.dm_green

    // do mass balance check
    dlt_dm_green_tot = dlt.dm_green
                     + gDlt_dm_oil_conv;    //FIXME

    if (!reals_are_equal(dlt_dm_green_tot, g_dlt_dm, 1.0E-4))  // XX this is probably too much slop - try doubles XX
    {
         string msg = "Fruit dlt_dm_green_tot mass balance is off: "
                    + ftoa(dlt_dm_green_tot, ".6")
                    + " vs "
                    + ftoa(g_dlt_dm, ".6");
         parentPlant->warningError(msg.c_str());
    }

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
//- Implementation Section ----------------------------------                             //remove
                                                                                          //remove
   cproc_yieldpart_demand_stress1 (min(plant->getNfactPhoto(), plant->getPfactPhoto())    //remove
                                 , plant->getSwdefPhoto()                                 //remove
                                 , plant->getTempStressPhoto()                            //remove
                                 , &gDlt_dm_stress_max);                                  //remove
//   gDlt_dm_stress_max  = dlt_dm_stress_max;                                             //remove
}


//     ===========================================================
void PlantFruit::dm_retranslocate1( float  g_dlt_dm_retrans_to_fruit )
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
    float dlt_dm_retrans_part;                    // carbohydrate removed from part (g/m^2)
    float dlt_dm_retrans_total;                   // total carbohydrate removed from parts (g/m^2)
    float yield_demand_differential;              // demand in excess of available supply (g/m^2)
    float demand_differential;                    // demand in excess of available supply (g/m^2)
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

    float fracPod = podPart->fracPod1();

     for (vector<plantPart *>::iterator t = myParts.begin();      //FIXME later
          t != myParts.end();
          t++)
        (*t)->dlt.dm_green_retrans = 0.0;
     dmOil_conv_retranslocate = 0.0;

    dlt_dm_grain = grainPart->dlt.dm_green;
    dlt_dm_grain = mealPart->dlt.dm_green   //remove
                 + oilPart->dlt.dm_green    //remove
                 + gDlt_dm_oil_conv;        //remove

    if (gDlt_dm_grain_demand > dlt_dm_grain)
    {
            // we can translocate source carbohydrate
            // to reproductive parts if needed

            // calculate demands for each reproductive part

        dm_demand_differential          = gDlt_dm_grain_demand - dlt_dm_grain;                      //FIXME
        dm_grain_demand_differential    = divide (dm_demand_differential, gGrain_energy, 0.0);      //FIXME
        dm_meal_demand_differential     = dm_grain_demand_differential * (1.0 - cGrain_oil_conc);     //FIXME
        dm_oil_demand_differential      = dm_grain_demand_differential - dm_meal_demand_differential;  //FIXME
        dm_oil_conv_demand_differential = dm_demand_differential - dm_grain_demand_differential;       //FIXME
        dm_pod_demand_differential      = dm_grain_demand_differential * fracPod;                  //FIXME

//        yield_demand_differential  = dm_pod_demand_differential
//                                   + dm_grain_demand_differential
//                                   + dm_oil_demand_differential
//                                   + dm_oil_conv_demand_differential;

        yield_demand_differential  = dm_pod_demand_differential
                                   + dm_meal_demand_differential
                                   + dm_oil_demand_differential
                                   + dm_oil_conv_demand_differential;

        demand_differential = yield_demand_differential - g_dlt_dm_retrans_to_fruit;

            // get available carbohydrate from supply pools
     for (vector<plantPart *>::iterator fPart = supplyPools.begin();      //FIXME later
          fPart != supplyPools.end();
          fPart++)
        {
           dm_part_pot = (*fPart)->g.dm_green + (*fPart)->dlt.dm_green_retrans;
           dm_part_avail = dm_part_pot
                         - (*fPart)->g.dm_plant_min * plant->getPlants();
           dm_part_avail = l_bound (dm_part_avail, 0.0);

           dlt_dm_retrans_part = min (demand_differential, dm_part_avail);
           (*fPart)->dlt.dm_green_retrans = - dlt_dm_retrans_part;

           demand_differential = demand_differential - dlt_dm_retrans_part;
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

    dltDmRetranslocate();

    // now check that we have mass balance
    if (!reals_are_equal(-1.0 * (dltDmRetranslocate() - g_dlt_dm_retrans_to_fruit), dmOil_conv_retranslocate, 1.0E-4))
    {
      string msg = "dm_retranslocate mass balance of fruit is off: "
                 + ftoa(dltDmRetranslocate() - g_dlt_dm_retrans_to_fruit, ".6")
                 + " vs "
                 + ftoa(dmOil_conv_retranslocate, ".6");


      parentPlant->warningError(msg.c_str());
    }
}

//     ===========================================================
void PlantFruit::dm_retranslocate2( float  g_dlt_dm_retrans_to_fruit)
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
    float dlt_dm_retrans_part;                    // carbohydrate removed from part (g/m^2)
    float dlt_dm_retrans_total;                   // total carbohydrate removed from parts (g/m^2)
    float yield_demand_differential;              // demand in excess of available supply (g/m^2)
    float demand_differential;                    // demand in excess of available supply (g/m^2)
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

      float fracPod = podPart->fracPod();


     for (vector<plantPart *>::iterator t = myParts.begin();      //FIXME later
          t != myParts.end();
          t++)
        (*t)->dlt.dm_green_retrans = 0.0;
     dmOil_conv_retranslocate = 0.0;

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
        dm_pod_demand_differential      = dm_grain_demand_differential * fracPod;    //FIXME

        yield_demand_differential  = dm_pod_demand_differential
                                   + dm_meal_demand_differential
                                   + dm_oil_demand_differential
                                   + dm_oil_conv_demand_differential;

        demand_differential = yield_demand_differential - g_dlt_dm_retrans_to_fruit;

            // get available carbohydrate from supply pools
     for (vector<plantPart *>::iterator fPart = supplyPools.begin();      //FIXME later
          fPart != supplyPools.end();
          fPart++)
        {
           dm_part_pot = (*fPart)->g.dm_green + (*fPart)->dlt.dm_green_retrans;
           dm_part_avail = dm_part_pot
                         - (*fPart)->g.dm_plant_min * plant->getPlants();
           dm_part_avail = l_bound (dm_part_avail, 0.0);

           dlt_dm_retrans_part = min (demand_differential, dm_part_avail);
           (*fPart)->dlt.dm_green_retrans = - dlt_dm_retrans_part;

           demand_differential = demand_differential - dlt_dm_retrans_part;
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
     for (vector<plantPart *>::iterator t = myParts.begin();
          t != myParts.end();
          t++)
        (*t)->dlt.dm_green_retrans = 0.0;     //FIXME later
        dmOil_conv_retranslocate = 0.0;
    }
    dltDmRetranslocate();   // update fruit dm_retranslocate

    // now check that we have mass balance
    if (!reals_are_equal(-1.0 * (dltDmRetranslocate() - g_dlt_dm_retrans_to_fruit), dmOil_conv_retranslocate, 1.0E-4))
    {
      string msg = "dm_retranslocate mass balance of fruit is off: "
                 + ftoa(dltDmRetranslocate() - g_dlt_dm_retrans_to_fruit, ".6")
                 + " vs "
                 + ftoa(dmOil_conv_retranslocate, ".6");


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
void PlantFruit::doDmMin (void)       // (OUTPUT) actual biomass senesced from plant parts (g/m^2)
//============================================================================
{
   g.dm_plant_min = 0.0;
   for (vector<plantPart *>::iterator t = myParts.begin();
       t != myParts.end();
       t++)
   {
     (*t)->doDmMin();
      g.dm_plant_min += (*t)->g.dm_plant_min;
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

   grainPart->nit_init();

}

//============================================================================
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

//============================================================================
void PlantFruit::n_conc_grain_limits (void)
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
    if (plant->inPhase ("grainfill"))
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

        mealPart->g.n_conc_crit = divide (n_crit_grain, dm_meal, 0.0);
        mealPart->g.n_conc_max = divide (n_max_grain, dm_meal, 0.0);
        mealPart->g.n_conc_min = divide (n_min_grain, dm_meal, 0.0);
        }
    }
//FIXME not called yet
//============================================================================
void PlantFruit::n_retranslocate( void)
//============================================================================
{
//+  Purpose
//     Calculate the nitrogen retranslocation from the various fruit parts
//     to the grain.

//+  Mission Statement
//     Calculate N retranslocation from various fruit parts to grain

//+  Changes
//      170703 jngh specified and programmed

//+  Local Variables

//- Implementation Section ----------------------------------
    float N_avail_rep = 0.0;
         // Get Grain N supply in this cohort

    vector<plantPart *>::iterator part;
    for (part = supplyPools.begin(); part != supplyPools.end(); part++)
        N_avail_rep += (*part)->availableRetranslocateN();  // grain N potential (supply) from pod

            // available N does not include grain
            // this should not presume grain is 0.

          // get actual grain N uptake by retransolcation
          // limit retranslocation to total available N

    for (part = myParts.begin(); part != myParts.end(); part++)
         (*part)->dlt.n_retrans = 0.0;

      if (gN_grain_demand >= N_avail_rep)
      {
             // demand greater than or equal to supply
             // retranslocate all available N

         for (part = supplyPools.begin(); part != supplyPools.end(); part++)
              (*part)->dlt.n_retrans = - (*part)->availableRetranslocateN();
         mealPart->dlt.n_retrans = N_avail_rep;
      }
      else
      {
             // supply greater than demand.
             // Retranslocate what is needed

         for (part = supplyPools.begin(); part != supplyPools.end(); part++)
               (*part)->dlt.n_retrans = - gN_grain_demand
                                       * divide ((*part)->availableRetranslocateN(), N_avail_rep, 0.0);

         mealPart->dlt.n_retrans = gN_grain_demand;

      }
}

//============================================================================
void PlantFruit::doNRetranslocate( float N_supply, float g_grain_n_demand)
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

//- Implementation Section ----------------------------------

            // available N does not include grain
            // this should not presume grain is 0.

          // get actual grain N uptake by retransolcation
          // limit retranslocation to total available N

    vector<plantPart *>::iterator part;
    for (part = myParts.begin(); part != myParts.end(); part++)
         (*part)->dlt.n_retrans = 0.0;


     for (part = supplyPools.begin(); part != supplyPools.end(); part++)
        (*part)->doNRetranslocate(N_supply, g_grain_n_demand);
      if (g_grain_n_demand >= N_supply)
      {
             // demand greater than or equal to supply
             // retranslocate all available N

//         for (part = supplyPools.begin(); part != supplyPools.end(); part++)
//              (*part)->dlt.n_retrans = - (*part)->availableRetranslocateN();
         mealPart->dlt.n_retrans = N_supply;
      }
      else
      {
             // supply greater than demand.
             // Retranslocate what is needed

//         for (part = supplyPools.begin(); part != supplyPools.end(); part++)
//               (*part)->dlt.n_retrans = - g_grain_n_demand
//                                       * divide ((*part)->availableRetranslocateN(), N_supply, 0.0);

         mealPart->dlt.n_retrans = g_grain_n_demand;

      }
    dlt.n_retrans = 0.0;
    for (part = supplyPools.begin(); part != supplyPools.end(); part++)
         dlt.n_retrans += (*part)->dlt.n_retrans;
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
void PlantFruit::doNDemand1Pot(float dlt_dm             // (INPUT)  Whole plant the daily biomass production (g/m^2)
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
           (*part)->doNDemand1Pot(dlt_dm, dlt_dm_pot_rue);
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
    v.n_demand -= mealPart->nDemand();           //remove
    mealPart->v.n_demand = gN_grain_demand;      //remove
    v.n_demand += mealPart->nDemand();           //remove
}


//============================================================================
void PlantFruit::doSoilNDemand(void)
//============================================================================
{
    v.soil_n_demand = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myParts.begin(); part != myParts.end(); part++)
    {
           (*part)->doSoilNDemand();
           v.soil_n_demand += (*part)->v.soil_n_demand;
    }
}


//============================================================================
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
void PlantFruit::doPDemand(void)     // (INPUT)  Whole plant potential dry matter production (g/m^2)
//============================================================================
// Purpose
//     Return plant P demand for each plant component

//  Mission Statement
//     Calculate the P demand for each plant pool

{
    v.p_demand = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myParts.begin(); part != myParts.end(); part++)
    {
           (*part)->doPDemand();
           v.p_demand += (*part)->pDemand();
    }
}

//============================================================================
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

//============================================================================
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

//============================================================================
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

//============================================================================
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

//============================================================================
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

//============================================================================
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

//============================================================================
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

//============================================================================
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

//============================================================================
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

//============================================================================
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

//============================================================================
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

//============================================================================
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

//============================================================================
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

//============================================================================
void PlantFruit::updatePDet(void)
//============================================================================
{
    vector<plantPart *>::iterator part;
    for (part = myParts.begin(); part != myParts.end(); part++)
    {
           (*part)->updatePDet();
            g.p_sen +=  (*part)->dlt.p_sen;
            g.p_sen -= (*part)->dlt.p_det;
            g.p_dead -= (*part)->dlt.p_dead_det;
    }
}

//============================================================================
void PlantFruit::pInit(void)
//============================================================================
{
    g.p_green = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myParts.begin(); part != myParts.end(); part++)
    {
           (*part)->pInit();
            g.p_green +=  (*part)->g.p_green;
    }
}

//============================================================================
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

//============================================================================
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

//============================================================================
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

//============================================================================
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


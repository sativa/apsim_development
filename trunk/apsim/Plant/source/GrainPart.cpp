
// Modification log
// 6 Aug 97 J. Hargreaves  Implementation

#define YES 1
#define NO 0
#define TEST_fruitGrainPart NO					// build unit test?
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

#include "GrainPart.h"

using namespace std;

static const char* floatType =        "<type kind=\"single\"/>";

void push_routine (const char *) {};
void pop_routine (const char *) {};

      inline bool floatsAreEqual(float A, float B, float C) {return(fabs(A-B)<C);}

// default constructor
// 	initialise data members.
fruitGrainPart::fruitGrainPart(plantInterface *p, const string &name) : plantPart(p, name)
{
//    zeroAllGlobals();
    gDm_stress_max.setup(&gDlt_dm_stress_max);
    otherObservers.addObserver(&gDm_stress_max);
}

//fruitGrainPart::fruitGrainPart(Plant *P)  			 // member initialisation
//{
//   plant = P;
//   zeroVariables();
//}

//fruitGrainPart::fruitGrainPart(float greenShell, float greenMeal, float senescedMeal, float deadMeal)
//{
//
//	green.meal = greenMeal;
//	senesced.meal = senescedMeal;
//	dead.meal = deadMeal;
//}

// destructor
fruitGrainPart::~fruitGrainPart()
{
    if (oilPart) delete oilPart;
    if (mealPart) delete mealPart;

}

ostream &operator<<(ostream &output, const fruitGrainPart &pool)
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
//===========================================================================
const fruitGrainPart &fruitGrainPart::operator=(const fruitGrainPart &other)
//===========================================================================
{
	throw std::invalid_argument("Assignment operator NI for fruitGrainPart");
}

//===========================================================================
void fruitGrainPart::doRegistrations(protocol::Component *system)
//===========================================================================
{
   plantPart::doRegistrations(system);

   system->addGettableVar("dlt_dm_grain_demand",gDlt_dm_grain_demand, "g/m^2", "??");
   system->addGettableVar("dlt_dm_fruit", gDlt_dm, "g/m^2", "Change in dry matter");
   system->addGettableVar("dlt_dm_oil_conv",gDlt_dm_oil_conv,"g/m^2", "change in oil via ??");
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
   system->addGettableVar("grain_oil_conc", cGrain_oil_conc, "%", "??");
   system->addGettableVar("dlt_dm_oil_conv_retrans", dmOil_conv_retranslocate, "g/m^2", "change in oil via retranslocation");



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
//bool fruitGrainPart::setVariable(unsigned id, protocol::QuerySetValueData& qd)
////===========================================================================
//  {
//    ptr2setFn pf = IDtoSetFn[id];
//    if (pf) {return((this->*pf)(qd));}
//    return false;
//  }
//===========================================================================
float fruitGrainPart::dmTotal(void)
//===========================================================================
{
    float dmTotal = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myParts.begin(); part != myParts.end(); part++)
      dmTotal += (*part)->dmTotal();
   return dmTotal;
}

//===========================================================================
float fruitGrainPart::dmSenesced(void)
//===========================================================================
{
    g.dm_senesced = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myParts.begin(); part != myParts.end(); part++)
      g.dm_senesced += (*part)->dmSenesced();
   return g.dm_senesced;
}

//===========================================================================
float fruitGrainPart::dmDead(void)
//===========================================================================
{
    g.dm_dead = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myParts.begin(); part != myParts.end(); part++)
      g.dm_dead += (*part)->dmDead();
   return g.dm_dead;
}

//===========================================================================
float fruitGrainPart::nTotal(void)
//===========================================================================
{
    float nTotal = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myParts.begin(); part != myParts.end(); part++)
      nTotal += (*part)->nTotal();
   return nTotal;
}

//===========================================================================
float fruitGrainPart::nGreen(void)
//===========================================================================
{
    g.n_green = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myParts.begin(); part != myParts.end(); part++)
      g.n_green += (*part)->nGreen();
   return g.n_green;
}

//===========================================================================
float fruitGrainPart::nSenesced(void)
//===========================================================================
{
    g.n_senesced = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myParts.begin(); part != myParts.end(); part++)
      g.n_senesced += (*part)->nSenesced();
   return g.n_senesced;
}

//===========================================================================
float fruitGrainPart::nDead(void)
//===========================================================================
{
    g.n_dead = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myParts.begin(); part != myParts.end(); part++)
      g.n_dead += (*part)->nDead();
   return g.n_dead;
}

//===========================================================================
float fruitGrainPart::nConc(void)
//===========================================================================
{
    float n_conc = divide (nGreen() , dmGreen() , 0.0) * fract2pcnt;
    return n_conc;
}


//===========================================================================
float fruitGrainPart::nGrainDemand2(void)
//===========================================================================
{
    return l_bound(mealPart->v.soil_n_demand - mealPart->dlt.n_green, 0.0);
}


//============================================================================
float fruitGrainPart::soilNDemand(void)
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
float fruitGrainPart::nDemand(void)
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
void fruitGrainPart::nPartition(float nSupply)
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
    oilPart->dlt.n_green = 0.0;

    float dlt_n_green_sum = 0.0;
    for (part = myParts.begin(); part != myParts.end(); part++)
         dlt_n_green_sum += (*part)->dltNGreen();

    dlt.n_green = dlt_n_green_sum;

    if (!reals_are_equal(dlt_n_green_sum - nSupply, 0.0))
        {
        string msg ="Grain dlt_n_green mass balance is off: dlt_n_green_sum ="
              + ftoa(dlt_n_green_sum, ".6")
              + " vs nSupply ="
              + ftoa(nSupply, ".6");
        parentPlant->warningError(msg.c_str());
        }

}
//============================================================================
void fruitGrainPart::nFix(float nSupply)
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
float fruitGrainPart::pTotal(void)
//===========================================================================
{
    float pTotal = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myParts.begin(); part != myParts.end(); part++)
      pTotal += (*part)->pTotal();
   return pTotal;
}

//===========================================================================
float fruitGrainPart::pGreen(void)
//===========================================================================
{
    float pTotal = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myParts.begin(); part != myParts.end(); part++)
      pTotal += (*part)->pGreen();
   return pTotal;
}

//===========================================================================
float fruitGrainPart::pSenesced(void)
//===========================================================================
{
    float pTotal = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myParts.begin(); part != myParts.end(); part++)
      pTotal += (*part)->pSenesced();
   return pTotal;
}

//===========================================================================
float fruitGrainPart::pDead(void)
//===========================================================================
{
    float pTotal = 0.0;
    vector<plantPart *>::iterator part;
    for (part = myParts.begin(); part != myParts.end(); part++)
      pTotal += (*part)->pDead();
   return pTotal;
}

//===========================================================================
float fruitGrainPart::pConc(void)
//===========================================================================
{
    float p_conc = divide (pGreen() , dmGreen() , 0.0) * fract2pcnt;
    return p_conc;
}


//===========================================================================
float fruitGrainPart::pConcTotal(void)
//===========================================================================
{
    float p_conc = divide (pTotal() , dmTotal() , 0.0) * fract2pcnt;
    return p_conc;
}


//===========================================================================
void fruitGrainPart::get_grain_wt(protocol::Component *system, protocol::QueryValueData &qd)
//===========================================================================
{
    system->sendVariable(qd, dmGreen());
}

//===========================================================================
void fruitGrainPart::get_grain_size(protocol::Component *system, protocol::QueryValueData &qd)
//===========================================================================
{
    float grain_size = divide (dmTotal()
                            , gGrain_no, 0.0);

    system->sendVariable(qd, grain_size);
}

//===========================================================================
void fruitGrainPart::get_grain_n(protocol::Component *system, protocol::QueryValueData &qd)
//===========================================================================
{
    system->sendVariable(qd, nGreen());
}

//===========================================================================
void fruitGrainPart::get_grain_n_demand(protocol::Component *system, protocol::QueryValueData &qd)
//===========================================================================
{
    system->sendVariable(qd, gN_grain_demand);
}

//===========================================================================
void fruitGrainPart::get_n_conc_grain(protocol::Component *system, protocol::QueryValueData &qd)
//===========================================================================
{
    float n_conc = nConc();
    system->sendVariable(qd, n_conc);
}

//===========================================================================
void fruitGrainPart::get_grain_protein(protocol::Component *system, protocol::QueryValueData &qd)
//===========================================================================
{
    float gp = nConc() * fract2pcnt * 5.71;
    system->sendVariable(qd, gp);
}

//===========================================================================
void fruitGrainPart::get_n_conc_meal(protocol::Component *system, protocol::QueryValueData &qd)
//===========================================================================
{
    float n_conc = mealPart->nConc();
    system->sendVariable(qd, n_conc);
}

//===========================================================================
void fruitGrainPart::get_yield(protocol::Component *system, protocol::QueryValueData &qd)
//===========================================================================
{
    system->sendVariable(qd, dmTotal() * gm2kg / sm2ha);
}

//===========================================================================
void fruitGrainPart::get_grain_p(protocol::Component *systemInterface, protocol::QueryValueData &qd)
//===========================================================================
{
    float grain_p = pGreen();
    systemInterface->sendVariable(qd, grain_p);  //()
}

//===========================================================================
void fruitGrainPart::get_p_conc_grain(protocol::Component *systemInterface, protocol::QueryValueData &qd)
//===========================================================================
{
    float p_conc_grain = pConc();
    systemInterface->sendVariable(qd, p_conc_grain);  //()
}

//===========================================================================
void fruitGrainPart::get_p_demand(vector<float> &p_demand)
//===========================================================================
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_p_demand(p_demand);
}

//===========================================================================
void fruitGrainPart::get_dlt_p_green(vector<float> &dlt_p_green)
//===========================================================================
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dlt_p_green(dlt_p_green);
}

//===========================================================================
void fruitGrainPart::get_p_green(vector<float> &p_green)
//===========================================================================
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_p_green(p_green);
}

//===========================================================================
void fruitGrainPart::get_dlt_p_retrans(vector<float> &dlt_p_retrans)
//===========================================================================
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dlt_p_retrans(dlt_p_retrans);
}

//===========================================================================
void fruitGrainPart::get_dm_plant_min(vector<float> &dm_min)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dm_plant_min(dm_min);
}

//===========================================================================
void fruitGrainPart::get_dm_green(vector<float> &dm_green)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dm_green(dm_green);
}

//===========================================================================
void fruitGrainPart::get_dm_dead(vector<float> &dm_dead)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dm_dead(dm_dead);
}

//===========================================================================
void fruitGrainPart::get_dm_senesced(vector<float> &dm_senesced)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dm_senesced(dm_senesced);
}

//===========================================================================
void fruitGrainPart::get_dlt_dm_green(vector<float> &dlt_dm_green)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dlt_dm_green(dlt_dm_green);
}

//===========================================================================
void fruitGrainPart::get_dlt_dm_green_retrans(vector<float> &dlt_dm_green_retrans)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dlt_dm_green_retrans(dlt_dm_green_retrans);
}

//===========================================================================
void fruitGrainPart::get_dlt_dm_detached(vector<float> &dlt_dm_detached)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dlt_dm_detached(dlt_dm_detached);
}

//===========================================================================
void fruitGrainPart::get_dlt_dm_senesced(vector<float> &dlt_dm_senesced)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dlt_dm_senesced(dlt_dm_senesced);
}

//===========================================================================
void fruitGrainPart::get_dlt_dm_dead_detached(vector<float> &dlt_dm_dead_detached)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dlt_dm_dead_detached(dlt_dm_dead_detached);
}

//===========================================================================
void fruitGrainPart::get_dlt_dm_green_dead(vector<float> &dlt_dm_green_dead)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dlt_dm_green_dead(dlt_dm_green_dead);
}

//===========================================================================
void fruitGrainPart::get_dlt_dm_senesced_dead(vector<float> &dlt_dm_senesced_dead)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dlt_dm_senesced_dead(dlt_dm_senesced_dead);
}

//===========================================================================
void fruitGrainPart::get_n_demanded(vector<float> &n_demand)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_n_demanded(n_demand);
}

//===========================================================================
void fruitGrainPart::get_n_green(vector<float> &n_green)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_n_green(n_green);
}

//===========================================================================
void fruitGrainPart::get_n_senesced(vector<float> &n_senesced)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_n_senesced(n_senesced);
}

//===========================================================================
void fruitGrainPart::get_n_dead(vector<float> &n_dead)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_n_dead(n_dead);
}

//===========================================================================
void fruitGrainPart::get_dlt_n_green(vector<float> &n_green)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dlt_n_green(n_green);
}

//===========================================================================
void fruitGrainPart::get_dlt_n_dead(vector<float> &n_dead)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dlt_n_dead(n_dead);
}

//===========================================================================
void fruitGrainPart::get_dlt_n_retrans(vector<float> &n_retrans)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dlt_n_retrans(n_retrans);
}

//===========================================================================
void fruitGrainPart::get_dlt_n_senesced(vector<float> &n_senesced)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dlt_n_senesced(n_senesced);
}

//===========================================================================
void fruitGrainPart::get_dlt_n_senesced_dead(vector<float> &dlt_n_senesced_dead)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dlt_n_senesced_dead(dlt_n_senesced_dead);
}

//===========================================================================
void fruitGrainPart::get_dlt_n_senesced_retrans(vector<float> &n_senesced_retrans)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dlt_n_senesced_retrans(n_senesced_retrans);
}

//===========================================================================
void fruitGrainPart::get_dlt_n_senesced_trans(vector<float> &n_senesced_trans)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dlt_n_senesced_trans(n_senesced_trans);
}

//===========================================================================
void fruitGrainPart::get_dlt_n_detached(vector<float> &n_detached)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dlt_n_detached(n_detached);
}

//===========================================================================
void fruitGrainPart::get_dlt_n_dead_detached(vector<float> &n_dead_detached)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dlt_n_dead_detached(n_dead_detached);
}

//===========================================================================
void fruitGrainPart::get_p_dead(vector<float> &p_dead)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_p_dead(p_dead);
}

//===========================================================================
void fruitGrainPart::get_p_sen(vector<float> &p_sen)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_p_sen(p_sen);
}

//===========================================================================
void fruitGrainPart::get_dlt_p_detached(vector<float> &dlt_p_detached)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dlt_p_detached(dlt_p_detached);
}

//===========================================================================
void fruitGrainPart::get_dlt_p_dead(vector<float> &dlt_p_dead)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dlt_p_dead(dlt_p_dead);
}

//===========================================================================
void fruitGrainPart::get_dlt_p_sen(vector<float> &dlt_p_sen)
{
    vector<plantPart *>::iterator myPart;
    for (myPart = myParts.begin(); myPart != myParts.end(); myPart++)
       (*myPart)->get_dlt_p_sen(dlt_p_sen);
}


//===========================================================================
void fruitGrainPart::grain_number (void)
//===========================================================================
//+  Purpose
//       Calculate Grain Numer

//+  Mission Statement
//       Calculate Grain Numer

//+  Changes

{
    if (cGrain_no_option == 1)
        {
        // do not use grain number
        gGrain_no = 0;
        }
    else if (cGrain_no_option == 2)
        {
        grain_number (plant->getDmGreenStem()
                    , pGrains_per_gram_stem
                    , &gGrain_no);
        }
    else
        {
        throw std::invalid_argument ("invalid template option");
        }

    return;
}

//===========================================================================
void fruitGrainPart::grain_number (float stem_dm_green
                             , float p_grains_per_gram_stem
                             , float *g_grain_no)    // OUTPUT
//===========================================================================
//+  Purpose
//       Perform grain number calculations
{
  if (plant->on_day_of ("emergence"))
        {
        // seedling has just emerged.
        *g_grain_no = 0.0;
        }
  else if (plant->on_day_of ("flowering"))
      {
      // we are at first day of grainfill.
      *g_grain_no = p_grains_per_gram_stem * stem_dm_green;
      }
  else
      {
      // no changes
      }
}

//===========================================================================
void fruitGrainPart::doTick(protocol::timeType &tick)
//===========================================================================
{
  double sd = (double)tick.startday;
  jday_to_day_of_year(&sd, &gDay_of_year, &gYear);
}

// Field a NewMet event
//===========================================================================
void fruitGrainPart::doNewMet(protocol::newmetType &newmet)
//===========================================================================
{
  if (gHasreadconstants)
  {
     gMaxt = newmet.maxt;
     gMint = newmet.mint;
  }
}

//===========================================================================
void fruitGrainPart::readCultivarParameters (protocol::Component *system, const string &cultivar)
//===========================================================================
{

//+  Local Variables
    string s;
    char  msg[200];                             // output string
    int   numvals;                                // number of values read

//- Implementation Section ----------------------------------

    system->writeString (" - reading fruit cultivar parameters");

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

   for (vector<plantPart *>::iterator t = myParts.begin();
        t != myParts.end();
        t++)
       (*t)->readCultivarParameters(system, cultivar);
}

//===========================================================================
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

void fruitGrainPart::onDayOf(const string &stage)
{
      for (vector<plantPart *>::iterator t = myParts.begin();
           t != myParts.end();
           t++)
          (*t)->onDayOf(stage);

    refreshStates();
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
   cCarbo_oil_conv_ratio  = 0.0;
   cGrain_oil_conc  = 0.0;
   cNum_n_conc_stage;
   cN_conc_crit_grain  = 0.0;
   cN_conc_max_grain  = 0.0;
   cN_conc_min_grain  = 0.0;
   cCarbo_oil_conv_ratio = 0.0;

//   cX_temp_grainfill[max_table];
//   cY_rel_grainfill[max_table];
//   cX_temp_grain_n_fill[max_table];
//   cY_rel_grain_n_fill[max_table];
//   cX_stage_code[max_table];

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

   gGrain_energy = 0.0;
   gGrain_no = 0.0;
   dmOil_conv_retranslocate = 0.0;

   gDlt_dm_stress_max        = 0.0;


   for (vector<plantPart *>::iterator t = myParts.begin();
        t != myParts.end();
        t++)
       (*t)->zeroAllGlobals();

}

void fruitGrainPart::zeroDeltas(void)
{
   plantPart::zeroDeltas();

   gDlt_dm_grain_demand = 0.0;
   gDlt_dm_oil_conv = 0.0;
   gDlt_dm = 0.0;

   gN_grain_demand = 0.0;
   gP_grain_demand = 0.0;

   for (vector<plantPart *>::iterator t = myParts.begin();
        t != myParts.end();
        t++)
       (*t)->zeroDeltas();

}

void fruitGrainPart::zeroDltDmGreen(void)
{
      dlt.dm_green = 0.0;

   for (vector<plantPart *>::iterator t = myParts.begin();
        t != myParts.end();
        t++)
       (*t)->zeroDltDmGreen();
}

void fruitGrainPart::zeroDltNSenescedTrans(void)
{
      dlt.n_senesced_trans = 0.0;

   for (vector<plantPart *>::iterator t = myParts.begin();
        t != myParts.end();
        t++)
       (*t)->zeroDltNSenescedTrans();
}

// ====================================================================
void fruitGrainPart::onHarvest(float cutting_height, float remove_fr,
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
void fruitGrainPart::onKillStem(void)
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
void fruitGrainPart::onEndCrop(vector<string> &dm_type,
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
void fruitGrainPart::refreshStates(void)
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
void fruitGrainPart::doInit (PlantComponent *systemInterface, PlantPhenology *plantPhenology)
// ====================================================================
{
   parentPlant = systemInterface;
   phenology = plantPhenology;
}

// ====================================================================
void fruitGrainPart::doInit1 ()
// ====================================================================
{
    mealPart = new fruitMealPart(plant, "meal");
    myParts.push_back(mealPart);

    oilPart = new fruitOilPart(plant, "oil");
    myParts.push_back(oilPart);

}


//===========================================================================
void fruitGrainPart::readConstants(protocol::Component *system, const string &section)
//===========================================================================
{
      system->getVariable(idLatitude, gLatitude, -90.0, 90.0);
      for (vector<plantPart *>::iterator t = myParts.begin();
           t != myParts.end();
           t++)
          (*t)->readConstants(system, section);

}

//===========================================================================
void fruitGrainPart::readSpeciesParameters(protocol::Component *system, vector<string> &sections)
//===========================================================================
{

    int   numvals;                                // number of valureturned

    //    plant_phenology_init
    system->readParameter (sections
                   , "twilight"//, "(o)"
                   , cTwilight
                   , -90.0, 90.0);

    int cPartition_option = 0;
    system->readParameter (sections,
                       "partition_option"//, "()"
                      , cPartition_option
                      , 1, 3);

    if (cPartition_option==1 )
    {
    }
    else if (cPartition_option==3)
        {
           // do nothing
        }
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

     //    plant_n_dlt_grain_conc
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

    //    plant_dm_partition
    system->readParameter (sections
                   ,"carbo_oil_conv_ratio"//, "()"
                   , cCarbo_oil_conv_ratio
                   , 0.0, 20.0);

    system->readParameter (sections
                   ,"grain_oil_conc"//, "()"
                   , cGrain_oil_conc
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

        for (vector<plantPart *>::iterator part = myParts.begin();
             part != myParts.end();
             part++)
           (*part)->readSpeciesParameters(system, sections);

}


//===========================================================================
float fruitGrainPart::dmGreen(void)
//===========================================================================
{
    g.dm_green = 0.0;

    vector<plantPart *>::iterator part;
    for (part = myParts.begin(); part != myParts.end(); part++)
      g.dm_green +=(*part)->g.dm_green;

    return g.dm_green;
}

//===========================================================================
float fruitGrainPart::dltDmGreen(void)
//===========================================================================
{
    dlt.dm_green = 0.0;

    vector<plantPart *>::iterator part;
    for (part = myParts.begin(); part != myParts.end(); part++)
      dlt.dm_green +=(*part)->dlt.dm_green;

    return dlt.dm_green;
}

//===========================================================================
void fruitGrainPart::doNSenescedRetrans(float navail, float n_demand_tot)
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
void fruitGrainPart::collectDetachedForResidue(vector<string> &part_name
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
void fruitGrainPart::collectDeadDetachedForResidue(vector<string> &part_name
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
void fruitGrainPart::update(void)
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
void fruitGrainPart::n_conc_limits(void)
//===========================================================================
{
    vector<plantPart *>::iterator part;
    for (part = myParts.begin(); part != myParts.end(); part++)
    {
      (*part)->n_conc_limits();
    }
}


//float fruitGrainPart::total() const
//{
//
//	return green.shell + green.meal + senesced.shell + senesced.meal + dead.shell + dead.meal;
//}

void fruitGrainPart::display(ostream &os) const
{
//	os << "fruitGrainPart:" << endl;
//	os << "Green meal: " << green.meal << endl;
//	os << "Senesced meal: " << senesced.meal << endl;
//	os << "Dead meal: " << dead.meal << endl << endl;
	os << endl;
}


//===========================================================================
void fruitGrainPart::processBioDemand(void)
//===========================================================================
{

    bio_water1 ();
    yieldpart_demand_stress1 ();
    bio_grain_oil ();
    bio_grain_demand ();

    return;
}

float fruitGrainPart::grainNo(void) const {return gGrain_no;}
float fruitGrainPart::grainEnergy(void) const {return gGrain_energy;}
float fruitGrainPart::nDemand(void) const {return gN_grain_demand;}
float fruitGrainPart::nConcPercent(void) {return divide (nTotal(), dmTotal(), 0.0) * fract2pcnt;}
float fruitGrainPart::dltDmDemand(void) const {return gDlt_dm_grain_demand;}

float fruitGrainPart::meanT (void) {return 0.5 * (gMaxt + gMint);}

//===========================================================================
float fruitGrainPart::dltDmRetranslocate(void)
//===========================================================================
{
  dlt.dm_green_retrans = 0.0;
  for (vector<plantPart *>::iterator t = myParts.begin();
       t != myParts.end();
       t++)
     dlt.dm_green_retrans += (*t)->dlt.dm_green_retrans;
  return dlt.dm_green_retrans;
}


//===========================================================================
void fruitGrainPart::bio_grain_oil (void)                                // for seed energy content (>= 1.0)
//===========================================================================
{
   //+  Purpose
//       Calculate grain oil factors

//+  Changes
//      141100 jngh specified and programmed

//- Implementation Section ----------------------------------

    gGrain_energy = 1.0 + cGrain_oil_conc * (cCarbo_oil_conv_ratio - 1.0);
    bound_check_real_var (parentPlant, gGrain_energy, 1.0, 2.0, "grain_energy");
}

//===========================================================================
void fruitGrainPart::bio_grain_demand (void)
//===========================================================================
{
//+  Purpose
//       Simulate crop grain biomass demand.

//+  Mission Statement
//     Calculate grain biomass demand

//+  Changes
//      250894 jngh specified and programmed

//- Implementation Section ----------------------------------

    if (cGrain_fill_option == 1)
       {
        bio_yieldpart_demand1();
        }
    else if (cGrain_fill_option == 2)
        {
        if (plant->inPhase("grainfill"))
           bio_yieldpart_demand2();
        else
           gDlt_dm_grain_demand = 0.0;
        }
    else
        {
        throw std::invalid_argument("invalid template option in plant_bio_grain_demand");
        }

    return;
}

//===========================================================================
void fruitGrainPart::bio_yieldpart_demand2(void)
//===========================================================================
{
//+  Purpose
//       Perform grain filling calculations

//+  Changes
    //+  Local Variables
    float tav;

        // we are in grain filling stage
        tav = meanT();

        gDlt_dm_grain_demand = gGrain_no
                             * pPotential_grain_filling_rate
                             * linear_interp_real(tav
                                                  ,cX_temp_grainfill
                                                  ,cY_rel_grainfill
                                                  ,cNum_temp_grainfill);

}

//===========================================================================
void fruitGrainPart::bio_yieldpart_demand1(void)
//===========================================================================
{
//+  Purpose
//        Find grain demand for carbohydrate using harvest index (g/m^2)

//+  Mission Statement
//   Calculate yield component biomass demand using harvest index increments

//+  Changes
//     010994 jngh specified and programmed

//+  Local Variables
//    float ave_stress;                             // average dm_stress from flowering to gra
    float dlt_dm_yield;                           // grain demand for carbohydrate (g/m^2)
    float dlt_dm_yield_unadj;                     // grain demand for carbohydrate, unadjusted
// for energy conversion (g/m^2)
    float harvest_index;                          // last harvest index (g grain/g biomass)
    float hi_max_pot;                             // max potential HI due to stress
    float dm_tops_new;                            // new drymatter  tops (g/m^2)
    float harvest_index_new;                      // next harvest index (g grain/g biomass)
    float dm_grain_new;                           // new drymatter grain (g/m^2)
    float energy_adjust;                          // adjustment for energy used in oil conversion.
    int   indx;                                   // loop index
    float hi_incr;                                // harvest index increment per day
    float photoperiod;                            // hours of photosynthetic light (hours)

//- Implementation Section ----------------------------------

    if (plant->inPhase("grainfill"))
        {

//        ave_stress = divide ((*g_dm_stress_max).getSum(),
//                             (*g_dm_stress_max).getN(),
//                             1.0);
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
        float dm_green_yield_parts = mealPart->g.dm_green + oilPart->g.dm_green;

        harvest_index = divide (dm_green_yield_parts, plant->getDmTops(), 0.0);
        dm_tops_new = plant->getDmTops() + plant->getDltDm();

        harvest_index_new = u_bound (harvest_index + hi_incr, hi_max_pot);

        dm_grain_new = dm_tops_new * harvest_index_new;
        dlt_dm_yield_unadj = dm_grain_new - dm_green_yield_parts;

    // adjust for grain energy

        dlt_dm_yield_unadj = bound (dlt_dm_yield_unadj, 0.0, dm_grain_new);

        energy_adjust = divide (gGrain_energy
                                , 1.0 + harvest_index_new*(gGrain_energy - 1.0)
                                , 0.0);

        dlt_dm_yield = dlt_dm_yield_unadj * energy_adjust;
    //jh         dlt_dm_yield = dlt_dm_yield_unadj

        if (gMint <= pMinTempGrnFill)
        {
            gDelayGrnFill = true;
        }
        if (gDelayGrnFill)
        {
            dlt_dm_yield = 0.0;
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
        dlt_dm_yield = 0.0;
        }
//        ostrstream msg;
//       msg << g_mint << gDelayGrnFill << gDaysDelayedGrnFill << endl;
//       parent->writeString (msg.str());


//    *dlt_dm_grain_demand = dlt_dm_yield;
    gDlt_dm_grain_demand = dlt_dm_yield;

    return;
    }

//===========================================================================
void fruitGrainPart::grain_n_demand1(float g_nfact_grain_conc      //   (INPUT)
                               , float g_swdef_expansion)        //   grain N demand (g/m^2)
//===========================================================================
{
//  Purpose
//    Calculate plant n demand

      float   n_potential;           // maximum grain N demand (g/m^2)

//- Implementation Section ----------------------------------

      gN_grain_demand = (mealPart->dlt.dm_green + mealPart->dlt.dm_green_retrans)
                      * n_dlt_grain_conc(mealPart
                          , cSfac_slope
                          , cSw_fac_max
                          , cTemp_fac_min
                          , cTfac_slope
                          , meanT()
                          , g_nfact_grain_conc
                          , g_swdef_expansion);


      n_potential  = (mealPart->g.dm_green
                     + mealPart->dlt.dm_green
                     + mealPart->dlt.dm_green_retrans)
                   * mealPart->g.n_conc_max;

      gN_grain_demand = u_bound (gN_grain_demand
                                , n_potential - mealPart->g.n_green);

   }

//===========================================================================
void fruitGrainPart::grain_n_demand2 (void)
//===========================================================================
{
      const char *my_name = "plant_grain_n_demand2";

      float Tav ;
      float N_potential;
      float grain_growth;
      float max_grain_n;

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

         grain_growth = divide(mealPart->dlt.dm_green + mealPart->dlt.dm_green_retrans
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
//==========================================================================
float fruitGrainPart::n_dlt_grain_conc(plantPart *grainPart
                           , float sfac_slope      //(INPUT)  soil water stress factor slope
                           , float sw_fac_max      //(INPUT)  soil water stress factor maximum
                           , float temp_fac_min    //(INPUT)  temperature stress factor minimum optimum temp
                           , float tfac_slope      //(INPUT)  temperature stress factor slope
                           , float ave_temp         //(INPUT)  mean air temperature (oC)
                           , float nfact_grain_conc// (INPUT)
                           , float swdef_expansion) // (INPUT)
//==========================================================================

/*  Purpose
*     Calculate the nitrogen concentration required to meet the increase
*     from daily grain growth (0-1) as affected by temperature and water stress.
*
*  Mission Statement
*   Calculate the nitrogen concentration required for grain growth.
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
   N_conc_pot = grainPart->g.n_conc_min + (grainPart->g.n_conc_crit - grainPart->g.n_conc_min) * nfact_grain_conc;

            // Temperature and water stresses can decrease/increase grain
            // N concentration

            // when there is no N stress, the following can be a higher N conc than
            // the crit and thus the N conc of the grain can exceed N critical.

   return  (N_conc_pot * max (N_grain_temp_fac, N_grain_sw_fac));
   }

//===========================================================================
float fruitGrainPart::dm_yield_demand (void)
//===========================================================================
{

//+  Purpose
//       Calculate grain dm yield demand (g/m^2)
//       (OUTPUT) assimilate demand for reproductive part (g/m^2)
//+  Changes
//      170703 jngh specified and programmed

      return divide (gDlt_dm_grain_demand, gGrain_energy, 0.0);
}

//     ===========================================================
void fruitGrainPart::dm_partition1 (double g_dlt_dm)
//     ===========================================================
{

//+  Purpose
//       Partitions new dm (assimilate) between plant components (g/m^2)

//+  Changes
//      170703 jngh specified and programmed

//+  Local Variables
    double dlt_dm_green_tot;                       // total of partitioned dm (g/m^2)
    double dm_remaining;                           // interim dm pool for partitioning
    double yield_demand;                           // sum of grain, energy & pod
    double dm_grain_demand;                        // assimilate demand for grain (g/m^2)
    double dm_meal_demand;                         // assimilate demand for meal (g/m^2)
    double dm_oil_demand;                          // assimilate demand for oil (g/m^2)
    double dm_oil_conv_demand;                     // assimilate demand for conversion to oil (g/m^2)

//- Implementation Section ----------------------------------

     for (vector<plantPart *>::iterator t = myParts.begin();      //FIXME later
          t != myParts.end();
          t++)
        (*t)->dlt.dm_green = 0.0;

     gDlt_dm_oil_conv = 0.0;

    // calculate demands of reproductive parts
    dm_grain_demand = divide (gDlt_dm_grain_demand, gGrain_energy, 0.0);

    dm_meal_demand = dm_grain_demand * (1.0 - cGrain_oil_conc);
    dm_oil_demand = dm_grain_demand - dm_meal_demand;
    dm_oil_conv_demand = gDlt_dm_grain_demand - dm_grain_demand;

    yield_demand = dm_meal_demand
                 + dm_oil_demand
                 + dm_oil_conv_demand;

         // now distribute the assimilate to plant parts
    if (yield_demand >= g_dlt_dm)
            // reproductive demand exceeds supply - distribute assimilate to those parts only
    {
            // reproductive demand exceeds supply - distribute assimilate to those parts only
        mealPart->dlt.dm_green = g_dlt_dm * divide (dm_meal_demand    , yield_demand, 0.0);
        oilPart->dlt.dm_green  = g_dlt_dm * divide (dm_oil_demand     , yield_demand, 0.0);
        gDlt_dm_oil_conv       = g_dlt_dm * divide (dm_oil_conv_demand, yield_demand, 0.0);

    }
    else
    {
        // more assimilate than needed for reproductive parts
        // distribute to all parts

        // satisfy reproductive demands
        mealPart->dlt.dm_green   = dm_meal_demand;
        oilPart->dlt.dm_green    = dm_oil_demand;
        gDlt_dm_oil_conv         = dm_oil_conv_demand;

    }

     dltDmGreen();      // update fruit dlt.dm_green

    // do mass balance check
    dlt_dm_green_tot = dlt.dm_green
                     + gDlt_dm_oil_conv;

    if (!reals_are_equal(dlt_dm_green_tot, g_dlt_dm, 1.0E-4))  // XX this is probably too much slop - try doubles XX
    {
         string msg = "Grain dlt_dm_green_tot mass balance is off: "
                    + ftoa(dlt_dm_green_tot, ".6")
                    + " vs "
                    + ftoa(g_dlt_dm, ".6");
         parentPlant->warningError(msg.c_str());
    }

      // check that deltas are in legal range       //FIXME need to do something about this when array is removed
//    bound_check_real_array (parentPlant, dlt_dm_green, max_part, 0.0, g_dlt_dm, "Fruit dlt.dm_green");

}

//     ===========================================================
void fruitGrainPart::dm_partition2 (double g_dlt_dm)
//     ===========================================================
{

//+  Purpose
//       Partitions new dm (assimilate) between plant components (g/m^2)

//+  Changes
//      170703 jngh specified and programmed

//+  Local Variables
    double dlt_dm_green_tot;                       // total of partitioned dm (g/m^2)
    double dm_remaining;                           // interim dm pool for partitioning
    double yield_demand;                           // sum of grain, energy & pod
    double dm_grain_demand;                        // assimilate demand for grain (g/m^2)
    double dm_meal_demand;                         // assimilate demand for meal (g/m^2)
    double dm_oil_demand;                          // assimilate demand for oil (g/m^2)
    double dm_oil_conv_demand;                     // assimilate demand for conversion to oil (g/m^2)

//- Implementation Section ----------------------------------

         // first we zero all plant component deltas

     for (vector<plantPart *>::iterator t = myParts.begin();
          t != myParts.end();
          t++)
        (*t)->zeroDltDmGreen();

     gDlt_dm_oil_conv = 0.0;

    // calculate demands of reproductive parts
    dm_grain_demand = divide (gDlt_dm_grain_demand, gGrain_energy, 0.0);

    dm_meal_demand = dm_grain_demand * (1.0 - cGrain_oil_conc);
    dm_oil_demand = dm_grain_demand - dm_meal_demand;
    dm_oil_conv_demand = gDlt_dm_grain_demand - dm_grain_demand;

    yield_demand = dm_meal_demand
                 + dm_oil_demand
                 + dm_oil_conv_demand;

         // now distribute the assimilate to plant parts
    if (yield_demand >= g_dlt_dm)
            // reproductive demand exceeds supply - distribute assimilate to those parts only
    {
            // reproductive demand exceeds supply - distribute assimilate to those parts only
        mealPart->dlt.dm_green = g_dlt_dm * divide (dm_meal_demand    , yield_demand, 0.0);
        oilPart->dlt.dm_green  = g_dlt_dm * divide (dm_oil_demand     , yield_demand, 0.0);
        gDlt_dm_oil_conv       = g_dlt_dm * divide (dm_oil_conv_demand, yield_demand, 0.0);

    }
    else
    {
        // more assimilate than needed for reproductive parts
        // distribute to all parts

        // satisfy reproductive demands
        mealPart->dlt.dm_green   = dm_meal_demand;
        oilPart->dlt.dm_green    = dm_oil_demand;
        gDlt_dm_oil_conv         = dm_oil_conv_demand;

    }

     dltDmGreen();   // update fruit dlt.dm_green

    // do mass balance check
    dlt_dm_green_tot = dlt.dm_green
                     + gDlt_dm_oil_conv;

    if (!reals_are_equal(dlt_dm_green_tot, g_dlt_dm, 1.0E-4))  // XX this is probably too much slop - try doubles XX
    {
         string msg = "Grain dlt_dm_green_tot mass balance is off: "
                    + ftoa(dlt_dm_green_tot, ".6")
                    + " vs "
                    + ftoa(g_dlt_dm, ".6");
         parentPlant->warningError(msg.c_str());
    }

      // check that deltas are in legal range       //FIXME need to do something about this when array is removed
//    bound_check_real_array (parentPlant, dlt_dm_green, max_part, 0.0, g_dlt_dm, "Fruit dlt.dm_green");

}

//     ===========================================================
void fruitGrainPart::yieldpart_demand_stress1 (void)
//     ===========================================================
{
//+  Purpose
//       Simulate crop grain biomass demand stress factor

//- Implementation Section ----------------------------------

   cproc_yieldpart_demand_stress1 (min(plant->getNfactPhoto(), plant->getPfactPhoto())
                                 , plant->getSwdefPhoto()
                                 , plant->getTempStressPhoto()
                                 , &gDlt_dm_stress_max);
//   gDlt_dm_stress_max  = dlt_dm_stress_max;
}


//     ===========================================================
void fruitGrainPart::dm_retranslocate1( float  g_dlt_dm_retrans_to_fruit )
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
    int   part;                                   // plant part no.
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
    float dm_oil_conv_demand_differential;        // assimilate demand for oil conversion - energy (g/m^2)
    float dlt_dm_grain;                           // assimilate used to produce grain and oil in partitioning (g/m^2)

//- Implementation Section ----------------------------------

// now translocate carbohydrate between plant components
// this is different for each stage

//+  Constant Values

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

        yield_demand_differential  = dm_meal_demand_differential
                                   + dm_oil_demand_differential
                                   + dm_oil_conv_demand_differential;

        demand_differential = yield_demand_differential - g_dlt_dm_retrans_to_fruit;

            // get available carbohydrate from supply pools
////     for (vector<plantPart *>::iterator fPart = supplyPools.begin();      //FIXME later
////          fPart != supplyPools.end();
////          fPart++)
////        {
////           dm_part_pot = (*fPart)->g.dm_green + (*fPart)->dlt.dm_green_retrans;
////           dm_part_avail = dm_part_pot
////                         - (*fPart)->g.dm_plant_min * plant->getPlants();
////           dm_part_avail = l_bound (dm_part_avail, 0.0);
////
////           dlt_dm_retrans_part = min (demand_differential, dm_part_avail);
////           (*fPart)->dlt.dm_green_retrans = - dlt_dm_retrans_part;
////
////           demand_differential = demand_differential - dlt_dm_retrans_part;
////        }

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
        }
        else
        {

            mealPart->dlt.dm_green_retrans     = dm_meal_demand_differential;
            oilPart->dlt.dm_green_retrans      = dm_oil_demand_differential;
            dmOil_conv_retranslocate           = dm_oil_conv_demand_differential;
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
      string msg = "dm_retranslocate mass balance of grain is off: "
                 + ftoa(dltDmRetranslocate() - g_dlt_dm_retrans_to_fruit, ".6")
                 + " vs "
                 + ftoa(dmOil_conv_retranslocate, ".6");


      parentPlant->warningError(msg.c_str());
    }
}

//     ===========================================================
void fruitGrainPart::dm_retranslocate2( float  g_dlt_dm_retrans_to_fruit)
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
    int   part;                                   // plant part no.
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
    float dm_oil_conv_demand_differential;        // assimilate demand for oil conversion - energy (g/m^2)
    float dlt_dm_grain;                           // assimilate used to produce grain and oil in partitioning (g/m^2)

//- Implementation Section ----------------------------------

// now translocate carbohydrate between plant components
// this is different for each stage

//+  Constant Values


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

        yield_demand_differential  = dm_meal_demand_differential
                                   + dm_oil_demand_differential
                                   + dm_oil_conv_demand_differential;

        demand_differential = yield_demand_differential - g_dlt_dm_retrans_to_fruit;

            // get available carbohydrate from supply pools
////     for (vector<plantPart *>::iterator fPart = supplyPools.begin();      //FIXME later
////          fPart != supplyPools.end();
////          fPart++)
////        {
////           dm_part_pot = (*fPart)->g.dm_green + (*fPart)->dlt.dm_green_retrans;
////           dm_part_avail = dm_part_pot
////                         - (*fPart)->g.dm_plant_min * plant->getPlants();
////           dm_part_avail = l_bound (dm_part_avail, 0.0);
////
////           dlt_dm_retrans_part = min (demand_differential, dm_part_avail);
////           (*fPart)->dlt.dm_green_retrans = - dlt_dm_retrans_part;
////
////           demand_differential = demand_differential - dlt_dm_retrans_part;
////        }

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
        }
        else
        {
            mealPart->dlt.dm_green_retrans     = dm_meal_demand_differential;
            oilPart->dlt.dm_green_retrans      = dm_oil_demand_differential;
            dmOil_conv_retranslocate           = dm_oil_conv_demand_differential;
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
      string msg = "dm_retranslocate mass balance of grain is off: "
                 + ftoa(dltDmRetranslocate() - g_dlt_dm_retrans_to_fruit, ".6")
                 + " vs "
                 + ftoa(dmOil_conv_retranslocate, ".6");


      parentPlant->warningError(msg.c_str());
    }
}

//============================================================================
void fruitGrainPart::doSenescence1 (float sen_fr)       // (OUTPUT) actual biomass senesced from plant parts (g/m^2)
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
void fruitGrainPart::doSenescence2 (float sen_fr)       // (OUTPUT) actual biomass senesced from plant parts (g/m^2)
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
void fruitGrainPart::doDmMin (void)       // (OUTPUT) actual biomass senesced from plant parts (g/m^2)
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
void fruitGrainPart::nit_init (void)
//============================================================================
{
//+  Purpose
//       Initialise plant nitrogen.

//+  Mission Statement
//     Initialise plant nitrogen

//+  Changes
//      250894 jngh specified and programmed

//- Implementation Section ----------------------------------


//   if (plant->inPhase("grainfill"))            //FIXME when these data members are put in
//      n_conc_grain_limits(gN_conc_crit
//                        , gN_conc_max
//                        , gN_conc_min);

}

//============================================================================
void fruitGrainPart::n_conc_grain_limits (void)
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
void fruitGrainPart::n_retranslocate( void)
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
////    for (part = supplyPools.begin(); part != supplyPools.end(); part++)
////        N_avail_rep += (*part)->availableRetranslocateN();  // grain N potential (supply) from pod

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

         mealPart->dlt.n_retrans = N_avail_rep;
      }
      else
      {
             // supply greater than demand.
             // Retranslocate what is needed

         mealPart->dlt.n_retrans = gN_grain_demand;

      }
}

//============================================================================
void fruitGrainPart::doNRetranslocate( float N_supply, float g_grain_n_demand)
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


      if (g_grain_n_demand >= N_supply)
      {
             // demand greater than or equal to supply
             // retranslocate all available N

         mealPart->dlt.n_retrans = N_supply;
      }
      else
      {
             // supply greater than demand.
             // Retranslocate what is needed

         mealPart->dlt.n_retrans = g_grain_n_demand;

      }
    dlt.n_retrans = 0.0;
}

//============================================================================
void fruitGrainPart::doNDemand1(float dlt_dm             // (INPUT)  Whole plant the daily biomass production (g/m^2)
                          , float dlt_dm_pot_rue)     // (INPUT)  Whole plant potential dry matter production (g/m^2)
//============================================================================
// Purpose
//     Return plant nitrogen demand for each plant component

//  Mission Statement
//     Calculate the Nitrogen demand and maximum uptake for each plant pool

{
    v.n_demand = 0.0;
    v.n_max = 0.0;
}

//============================================================================
void fruitGrainPart::doNDemand1Pot(float dlt_dm             // (INPUT)  Whole plant the daily biomass production (g/m^2)
                          , float dlt_dm_pot_rue)     // (INPUT)  Whole plant potential dry matter production (g/m^2)
//============================================================================
// Purpose
//     Return plant nitrogen demand for each plant component

//  Mission Statement
//     Calculate the Nitrogen demand and maximum uptake for each plant pool

{
    v.n_demand = 0.0;
    v.n_max = 0.0;
}

//============================================================================
void fruitGrainPart::doNDemand2(float dlt_dm             // (INPUT)  Whole plant the daily biomass production (g/m^2)
                          , float dlt_dm_pot_rue)     // (INPUT)  Whole plant potential dry matter production (g/m^2)
//============================================================================
// Purpose
//     Return plant nitrogen demand for each plant component

//  Mission Statement
//     Calculate the Nitrogen demand and maximum uptake for each plant pool

{
    v.n_demand = 0.0;
    v.n_max = 0.0;
    v.n_demand -= mealPart->nDemand();
    mealPart->v.n_demand = gN_grain_demand;
    v.n_demand += mealPart->nDemand();
}


//============================================================================
void fruitGrainPart::doSoilNDemand(void)
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
void fruitGrainPart::doNSenescence(void)
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
void fruitGrainPart::dm_detachment1(void)
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
void fruitGrainPart::n_detachment1(void)
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
void fruitGrainPart::doPDemand(void)     // (INPUT)  Whole plant potential dry matter production (g/m^2)
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
void fruitGrainPart::doPSenescence(void)
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
float fruitGrainPart::pDemand(void)
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
float fruitGrainPart::pRetransSupply(void)
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
float fruitGrainPart::pRetransDemand(void)
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
float fruitGrainPart::dmRetransSupply(void)
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
float fruitGrainPart::dmRetransDemand(void)
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
float fruitGrainPart::nRetransSupply(void)
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
float fruitGrainPart::dltNRetransOut(void)
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
float fruitGrainPart::dltNGreen(void)
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
float fruitGrainPart::nRetransDemand(void)
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
void fruitGrainPart::distributeDltPGreen(float p_uptake, float total_p_demand)
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
void fruitGrainPart::distributeDltPRetrans(float total_p_supply, float total_p_demand)
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
void fruitGrainPart::p_detachment1(void)
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
void fruitGrainPart::updatePDet(void)
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
void fruitGrainPart::pInit(void)
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
float fruitGrainPart::dmGreenStressDeterminant(void)
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
float fruitGrainPart::pGreenStressDeterminant(void)
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
float fruitGrainPart::pMaxPotStressDeterminant(void)
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
float fruitGrainPart::pMinPotStressDeterminant(void)
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

// Command
//===========================================================================
float fruitGrainPart::divide (float dividend, float divisor, float default_value) const
//===========================================================================

/*Definition
 *   Returns (dividend / divisor) if the division can be done
 *   without overflow or underflow.  If divisor is zero or
 *   overflow would have occurred, a specified default is returned.
 *   If underflow would have occurred, zero is returned.
 *Assumptions
 *   largest/smallest real number is 1.0e+/-30
 *Parameters
 *   dividend:     dividend
 *   divisor:      divisor
 *   defaultValue: default value to return if overflow
 *Calls
 *   reals_are_equal
 */

   {
   //Constant Values
   const float LARGEST = 1.0e30;    //largest acceptable no. for quotient
   const float SMALLEST = 1.0e-30;  //smallest acceptable no. for quotient
   const float nought = 0.0;
   const float one = 1.0;
   const float granularity = 1.0e-6;

   //Local Varialbes
   float quotient;

   //Implementation
   if(floatsAreEqual(dividend, nought, granularity))      //multiplying by 0
      {
      quotient = nought;
      }
   else if(floatsAreEqual(divisor, nought, granularity))  //dividing by 0
      {
      quotient = default_value;
      }
   else if(fabs(divisor) < one)            //possible overflow
      {
      if(fabs(dividend) > fabs(LARGEST * divisor)) //overflow
         {
         quotient = default_value;
         }
      else
         {
         quotient = dividend / divisor;          //ok
         }
      }
   else if(fabs(divisor) > one)             //possible underflow
      {
      if(fabs(dividend) < fabs(SMALLEST * divisor))    //underflow
         {
         quotient = nought;
         }
      else
         {
         quotient = dividend / divisor;                //ok
         }
      }
   else
      {
      quotient = dividend / divisor;                   //ok
      }
   return quotient;
   }



//============================================================================

#if TEST_fruitGrainPart							// build unit test?


// fruitGrainPart class test harness

// Tests default constructor, copy constructor, assignment operator and
// each of the get and set functions.  Does not test the destructor.

// Modification log
// 6 Aug 97  J. Hargreaves    Initial implementation


#ifndef fruitGrainPart_H
#include "fruitGrainPart.h"
#endif

int main()
{
	cout << "fruitGrainPart test started" << endl;

	fruitGrainPart p, *aPtr = &p;

//	cout << endl << "Test set and get functions:" << endl;
//	p.setValue(10.0, 2.0, 20.0, 3.0, 30.0, 4.0);
//	if (p.total() != 69.0)
//		cout << "setValue(10.0, 2.0, 20.0, 3.0, 30.0, 4.0) / Total() test FAILED"
//		<< endl;
//
//	cout << endl << "Test default constructor:" << endl;
//	fruitGrainPart q;                           						// run default constructor
//	if (q.total() != 0.0)
//		cout << "default constructor test FAILED" << endl;
//
//	cout << endl << "Test constructor:" << endl;
//	fruitGrainPart a(1.0, 2.0, 3.0, 4.0, 5.0, 6.0);                           						// run default constructor
//	if (a.total() != 21.0)
//		cout << "constructor test FAILED" << endl;
//
//	cout << endl << "Test copy constructor:" << endl;
//	fruitGrainPart s = p;                       // run copy constructor
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
//	fruitGrainPart k = p * s;
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
//	fruitGrainPart t;
//	t.setValue(50.0, 5.0, 60.0, 6.0, 70.0, 7.0); // change object
//	cout << endl << "Display fruitGrainPart t" << endl;
//	t.display();
//
//	fruitGrainPart x;
//	x.setValue(50.0, 5.0, 60.0, 6.0, 70.0, 7.0); // change object
//
//	cout << endl << "Display fruitGrainPart x - static binding" << endl;
//	x.display();
//
//	cout << endl << "Display fruitGrainPart x - dynamic binding" << endl;
//	fruitGrainPart *fruitGrainPartPtr = &x;
//	fruitGrainPartPtr->display();

	cout << endl << "fruitGrainPart test finished" << endl;
	return 0;
}

#endif


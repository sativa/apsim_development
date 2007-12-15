#include <stdio.h>
#include <math.h>
#include <stdexcept>
#include <string>
#include "PlantPart.h"

using namespace std;

plantPart::plantPart(ScienceAPI& api, plantInterface *p, const string &name)
//=======================================================================================
     : plantThing(api),
       // deltas
       Senescing(api, "Senescing", name),
       Growth(api, "Growth", name),
       Detaching(api, "Detaching", name),
       Retranslocation (api, "Retranslocation", name),

       // pools
       Green(*new Pool(*p, api, "Green", name)),
       Senesced(*new Pool(*p, api, "Senesced", name)),

       // summary pools
       Total(*p, api, "Total", name),
       Grain(*p, api, "Grain", name),
       GrainTotal(*p, api, "GrainTotal", name),
       Vegetative(*p, api, "Vegetative", name),
       VegetativeTotal(*p, api, "VegetativeTotal", name)
     {
     plant = p;
     myName = name;

     Initialise();
     }

plantPart::plantPart(ScienceAPI& api, plantInterface *p, const string &name,
                     Pool& green, Pool& senesced)
//=======================================================================================
     : plantThing(api),
       // deltas
       Senescing(api, "Senescing", name),
       Growth(api, "Growth", name),
       Detaching(api, "Detaching", name),
       Retranslocation (api, "Retranslocation", name),

       // pools
       Green(green),
       Senesced(senesced),

       // summary pools
       Total(*p, api, "Total", name),
       Grain(*p, api, "Grain", name),
       GrainTotal(*p, api, "GrainTotal", name),
       Vegetative(*p, api, "Vegetative", name),
       VegetativeTotal(*p, api, "VegetativeTotal", name)
   {
   plant = p;
   myName = name;
   Initialise();
   }

void plantPart::Initialise()
   {
   // setup summary pools
   Vegetative.AddPool(Green);

   Total.AddPool(Green);
   Total.AddPool(Senesced);
   VegetativeTotal.AddPool(Green);
   VegetativeTotal.AddPool(Senesced);

     zeroAllGlobals();
     }


string plantPart::addPartToVar(const string& variableName)
   {
   // --------------------------------------------------------------------------
   // add the part name, if it isn't blank, to the specified variable name.
   // --------------------------------------------------------------------------
   string LcaseName = myName;
   To_lower(LcaseName);
   if (myName != "")
      return variableName + "_" + LcaseName;
   else
      return variableName;
   }

string plantPart::addPartToDesc(const string& description)
   {
   // --------------------------------------------------------------------------
   // add the part name, if it isn't blank, to the specified description
   // --------------------------------------------------------------------------
   if (myName != "")
      return description + myName;
   else
      return description + " plant";
   }

void plantPart::onInit1(protocol::Component*)
//=======================================================================================
   {
   scienceAPI.exposeFunction(addPartToVar("dlt_dm_green"), "g/m^2", addPartToDesc("Delta Weight of "), FloatFunction(&plantPart::dltDmGreen));
   scienceAPI.exposeFunction(addPartToVar("dlt_dm_detached"), "g/m^2", addPartToDesc("Delta Weight of detached "), FloatFunction(&plantPart::dltDmDetached));
   scienceAPI.exposeFunction(addPartToVar("dlt_dm_senesced"), "g/m^2", addPartToDesc("Delta Weight of senesced "), FloatFunction(&plantPart::dltDmSenesced));

   scienceAPI.exposeFunction(addPartToVar("dlt_n_green"), "g/m^2", addPartToDesc("Delta N in "), FloatFunction(&plantPart::dltNGreen));
   scienceAPI.exposeFunction(addPartToVar("dlt_n_retrans"), "g/m^2", addPartToDesc("N retranslocated to/from "), FloatFunction(&plantPart::dltNRetrans));
   scienceAPI.exposeFunction(addPartToVar("dlt_n_detached"), "g/m^2", addPartToDesc("Delta N in detached "), FloatFunction(&plantPart::dltNDetached));
   scienceAPI.exposeFunction(addPartToVar("dlt_n_senesced"), "g/m^2", addPartToDesc("Delta N in senesced "), FloatFunction(&plantPart::dltNSenesced));
   scienceAPI.exposeFunction(addPartToVar("dlt_n_senesced_trans"), "g/m^2", addPartToDesc("N translocated to/from senesced "), FloatFunction(&plantPart::dltNSenescedTrans));
   scienceAPI.exposeFunction(addPartToVar("dlt_n_senesced_retrans"), "g/m^2", addPartToDesc("N retranslocated to/from senesced "), FloatFunction(&plantPart::dltNSenescedRetrans));
   scienceAPI.exposeFunction(addPartToVar("n_demand"), "g/m^2", addPartToDesc("N demand of "), FloatFunction(&plantPart::nDemand));

   scienceAPI.exposeFunction(addPartToVar("dlt_p_green"), "g/m^2", addPartToDesc("Delta P in "), FloatFunction(&plantPart::dltPGreen));
   scienceAPI.exposeFunction(addPartToVar("dlt_p_senesced"), "g/m^2", addPartToDesc("Delta P in senesced "), FloatFunction(&plantPart::dltPSenesced));
   scienceAPI.exposeFunction(addPartToVar("dlt_p_detached"), "g/m^2", addPartToDesc("Delta P in detached "), FloatFunction(&plantPart::dltPDetached));

   if (tempFlagToShortCircuitInit1) return;


   scienceAPI.exposeFunction(addPartToVar("n_conc_crit"), "%", addPartToDesc("Critical N content in "), FloatFunction(&plantPart::nConcCrit));
   scienceAPI.exposeFunction(addPartToVar("n_conc_min"), "%", addPartToDesc("Minimum N content in "), FloatFunction(&plantPart::nConcMin));

   scienceAPI.exposeFunction(addPartToVar("dlt_dm_retrans"), "g/m^2", addPartToDesc("DM retranslocated to/from "), FloatFunction(&plantPart::dltDmGreenRetrans));
   scienceAPI.exposeFunction(addPartToVar("dm_demand"), "g/m^2", addPartToDesc("DM demand of "), FloatFunction(&plantPart::dmGreenDemand));
   }

void plantPart::get_dm_green_demand(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
   {
   system->sendVariable(qd, dmGreenDemand());
   }


float plantPart::dltNGreen(void)
   //===========================================================================
{
   return Growth.N();
}


float plantPart::dltPGreen(void)
   //===========================================================================
{
   return Growth.P();
}


float plantPart::dltDmSenesced(void)
   //===========================================================================
{
   return Senescing.DM();
}

void plantPart::zeroAllGlobals(void)
//=======================================================================================
   {
   Green.Clear();
   Senesced.Clear();
   g.n_conc_crit=0.0;
   g.n_conc_min=0.0;

   zeroDeltas();
   }

void plantPart::zeroDeltas(void)
//=======================================================================================
   {
   Growth.Clear();
   Senescing.Clear();
   Detaching.Clear();
   Retranslocation.Clear();
   }

void plantPart::prepare(void)
//=======================================================================================
   {
   zeroDeltas();
   }

void plantPart::doNFixRetranslocate(float NFix, float NDemandDifferentialTotal)
//=======================================================================================
   {
   Growth = Growth + Biomass(0, NFix * divide (nDemandDifferential(), NDemandDifferentialTotal, 0.0),
                                 0);
   }

void plantPart::doNPartition(float nSupply, float n_demand_sum, float n_capacity_sum)
   //============================================================================
{
   float n_excess = nSupply - n_demand_sum;
   n_excess = l_bound (n_excess, 0.0);

   if (n_excess>0.0)
      {
      float plant_part_fract = divide (nCapacity(), n_capacity_sum, 0.0);
      Growth = Biomass(Growth.DM(),
                         nDemand() + n_excess * plant_part_fract,
                         Growth.P());
      }
   else
      {
      float plant_part_fract = divide (nDemand(), n_demand_sum, 0.0);
      Growth = Biomass(Growth.DM(),
                         nSupply * plant_part_fract,
                         Growth.P());
      }
}


float plantPart::dlt_dm_green_retrans_hack(float delta)
   {
   Retranslocation = Biomass(delta, Retranslocation.N(), Retranslocation.P());
   return delta;
   }

float plantPart::dmGreenNew(void)
//=======================================================================================
   {
   return (Green.DM() + Growth.DM() + Retranslocation.DM());
   }

void plantPart::doInit1(protocol::Component *system){this->onInit1(system) ;}

float plantPart::nMin(void)  {return g.n_conc_min * Green.DM();}
float plantPart::nCrit(void)  {return g.n_conc_crit * Green.DM();}
void plantPart::checkBounds(void) {}
const string &plantPart::name(void) {return myName ;}


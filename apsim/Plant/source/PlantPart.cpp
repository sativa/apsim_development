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
     c.name = name;

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
   c.name = name;
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
     c.dm_init = 0;
     c.n_init_conc = 0;
     c.p_init_conc = 0;
     c.trans_frac = 1;
     c.trans_frac_option = false;
     }


string plantPart::addPartToVar(const string& variableName)
   {
   // --------------------------------------------------------------------------
   // add the part name, if it isn't blank, to the specified variable name.
   // --------------------------------------------------------------------------
   string LcaseName = c.name;
   To_lower(LcaseName);
   if (c.name != "")
      return variableName + "_" + LcaseName;
   else
      return variableName;
   }

string plantPart::addPartToDesc(const string& description)
   {
   // --------------------------------------------------------------------------
   // add the part name, if it isn't blank, to the specified description
   // --------------------------------------------------------------------------
   if (c.name != "")
      return description + c.name;
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

void plantPart::zeroDltDmGreenRetrans(void)
//=======================================================================================
   {
   Retranslocation = Biomass(0.0, Retranslocation.N(), Retranslocation.P());
   }

void plantPart::zeroAllGlobals(void)
//=======================================================================================
   {
   Green.Clear();
   Senesced.Clear();
   Height=0.0;
   Width=0.0;
   g.n_conc_crit=0.0;
   g.n_conc_min=0.0;
   DMPlantMin=0.0;

   zeroDeltas();
   }

void plantPart::zeroDeltas(void)
//=======================================================================================
   {
   Growth.Clear();
   Senescing.Clear();
   Detaching.Clear();
   Retranslocation.Clear();
   dlt.dm_green_removed = 0.0;
   dlt.dm_senesced_removed = 0.0;

   NCapacity = 0.0;
   NDemand = 0.0 ;
   SoilNDemand = 0.0;
   NMax = 0.0 ;
   }

void plantPart::onFlowering(void)
//=======================================================================================
   {
   float dm_plant = divide (Green.DM(), plant->getPlants(), 0.0);
   if (c.trans_frac_option==1)
      DMPlantMin = dm_plant;
   else
      DMPlantMin = dm_plant * (1.0 - c.trans_frac);
   }

void plantPart::onStartGrainFill(void)
//=======================================================================================
// set the minimum weight of part; used for retranslocation to grain
   {
   float dm_plant = divide (Green.DM(), plant->getPlants(), 0.0);
   if (c.trans_frac_option==1)
      DMPlantMin = dm_plant * (1.0 - c.trans_frac);
   //else
      //DMPlantMin = DMPlantMin + (dm_plant - DMPlantMin) * (1.0 - c.trans_frac);
      //DMPlantMin = DMPlantMin + (dm_plant - DMPlantMin);
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
void plantPart::onHarvest_GenericAboveGroundPart( float remove_fr,
                             vector<string> &dm_type,
                             vector<float> &dlt_crop_dm,
                             vector<float> &dlt_dm_n,
                             vector<float> &dlt_dm_p,
                             vector<float> &fraction_to_residue)
//=======================================================================================
// Generic harvest method for above ground parts that lose all dm to residue (eg leaf & stem, not grain..)
{
   float fractToResidue = 1.0 - remove_fr;

   float dm_init = u_bound (c.dm_init * plant->getPlants(), Green.DM());
   float n_init  = u_bound (  dm_init * plantPart::c.n_init_conc, Green.N());
   float p_init  = u_bound (  dm_init * plantPart::c.p_init_conc, Green.P());

   float retain_fr_green = divide(dm_init, Green.DM(), 0.0);
   float retain_fr_sen   = 0.0;

   float dlt_dm_harvest = Green.DM() + Senesced.DM() - dm_init;
   float dlt_n_harvest  = Green.N()  + Senesced.N()  - n_init;
   float dlt_p_harvest  = Green.P()  + Senesced.P() - p_init;

   Senesced = Senesced * retain_fr_sen;
   Green = Biomass(Green.DM() * retain_fr_green, n_init, p_init);

   dm_type.push_back(c.name);
   fraction_to_residue.push_back(fractToResidue);
   dlt_crop_dm.push_back (dlt_dm_harvest * gm2kg/sm2ha);
   dlt_dm_n.push_back    (dlt_n_harvest  * gm2kg/sm2ha);
   dlt_dm_p.push_back    (dlt_p_harvest  * gm2kg/sm2ha);
}


float plantPart::dmGreenNew(void)
//=======================================================================================
   {
   return (Green.DM() + Growth.DM() + Retranslocation.DM());
   }
float plantPart::dltDmGreenNew(void)
//=======================================================================================
   {
   return (Growth.DM() + Retranslocation.DM());
   }

float plantPart::dltNGreenRemoved(void)
//=======================================================================================
   {
   return (Green.N() * divide(dlt.dm_green_removed, Green.DM(), 0.0));
   }


float plantPart::dltNSenescedRemoved(void)
//=======================================================================================
   {
   return (Senesced.N() * divide(dlt.dm_senesced_removed, Senesced.DM(), 0.0));
   }
float plantPart::dltPGreenRemoved(void)
//=======================================================================================
   {
   return (Green.P() * divide(dlt.dm_green_removed, Green.DM(), 0.0));
   }

float plantPart::dltPSenescedRemoved(void)
//=======================================================================================
   {
   return (Senesced.P() * divide(dlt.dm_senesced_removed, Senesced.DM(), 0.0));
   }

float plantPart::dltPRemoved(void)
//=======================================================================================
   {
   return (dltPGreenRemoved() + dltPSenescedRemoved());
   }

float plantPart::height(void) {return Height;}
float plantPart::width(void) {return Width;}

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

void plantPart::onPlantEvent(const string &event)
//=======================================================================================
   {
   if (event == "sowing") onSowing();
   else if (event == "germination") onGermination();
   else if (event == "emergence") onEmergence();
   else if (event == "transplanting")onTransplanting();
   else if (event == "flowering") onFlowering();
   else if (event == "start_grain_fill") onStartGrainFill();
   }

   //needed to standardise interface for composite subclass

float plantPart::giveNGreen(float delta)
//=======================================================================================
   {
   Growth = Growth + Biomass(0, delta, 0);
   return delta;
   }

float plantPart::giveDmSenesced (float delta)
//=======================================================================================
   {
   Senescing = Senescing + Biomass(delta, 0, 0);
   return delta;
   }

float plantPart::dlt_dm_green_retrans_hack(float delta)
   {
   Retranslocation = Biomass(delta, Retranslocation.N(), Retranslocation.P());
   return delta;
   }

float plantPart::giveDmGreenRemoved (float delta)
//=======================================================================================
// addXXX: something is removing some XXX. return the delta.
   {
   dlt.dm_green_removed = delta;
   float error_margin = 1.0e-6 ;
   if (delta > Green.DM() + error_margin)
   {
       ostringstream msg;
       msg << "Attempting to remove more green " << name() << " biomass than available:-" << endl;
       msg << "Removing " << -delta << " (g/m2) from " << Green.DM() << " (g/m2) available." << ends;
       throw std::runtime_error (msg.str().c_str());
   }
   return delta;
   }

float plantPart::giveDmSenescedRemoved (float delta)
//=======================================================================================
   {
   dlt.dm_senesced_removed = delta;
   float error_margin = 1.0e-6 ;
   if (delta > Senesced.DM() + error_margin)
   {
       ostringstream msg;
       msg << "Attempting to remove more Senesced " << name() << " biomass than available:-" << endl;
       msg << "Removing " << -delta << " (g/m2) from " << Senesced.DM() << " (g/m2) available." << ends;
       throw std::runtime_error (msg.str().c_str());
   }
   return delta;
   }

float critNFactor(vector< plantPart *> &parts, float multiplier)
//=======================================================================================
//   Calculate Nitrogen stress factor from a bunch of parts
/*  Purpose
*   The concentration of Nitrogen in plant parts is used to derive a Nitrogen stress index
*   for many processes. This stress index is calculated from today's relative nutitional
*   status between a critical and minimum Nitrogen concentration.
*/
   {
   vector< plantPart *>::iterator part;

   float dm = 0.0, N = 0.0;
   for (part = parts.begin(); part != parts.end(); part++)
      {
      dm += (*part)->Green.DM();
      N += (*part)->Green.N();
      }

   if (dm > 0.0)
      {
      float N_conc = divide (N, dm, 0.0);

      // calculate critical N concentrations
      float N_crit = 0.0;
      for (part = parts.begin(); part != parts.end(); part++)
          N_crit += (*part)->nCrit();

      float N_conc_crit = divide (N_crit, dm, 0.0);

      // calculate minimum N concentrations
      float N_min = 0.0;
      for (part = parts.begin(); part != parts.end(); part++)
         N_min += (*part)->nMin();

      float N_conc_min = divide (N_min, dm, 0.0);

      //calculate shortfall in N concentrations
      float dividend =  N_conc - N_conc_min;
      float divisor =   N_conc_crit - N_conc_min;
      float result = multiplier * divide (dividend, divisor, 1.0);
      result = bound (result, 0.0, 1.0);
      return (result);
      }
   else
      return (1.0);
   }

void plantPart::doInit1(protocol::Component *system){this->onInit1(system) ;}

void plantPart::onSowing(void){}
void plantPart::onGermination(void){}

void plantPart::onTransplanting(void) {}
float plantPart::dltLeafAreaPot(void) {throw std::runtime_error("plantPart::dltLeafAreaPot() called") ;}
float plantPart::nMin(void)  {return g.n_conc_min * Green.DM();}
float plantPart::nCrit(void)  {return g.n_conc_crit * Green.DM();}
void plantPart::onRemoveBiomass(float) {}
void plantPart::write() {}
void plantPart::checkBounds(void) {}
const string &plantPart::name(void) {return c.name ;}



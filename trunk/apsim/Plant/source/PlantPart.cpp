#include <stdio.h>
#include <math.h>
#include <stdexcept>
#include <string>
#include "PlantPart.h"

using namespace std;

plantPart::plantPart(ScienceAPI& api, plantInterface *p, const string &name)
//=======================================================================================
     : plantThing(api),
       PrivateSenesced(api, "Senesced", name),
       Senescing(api, "Senescing", name),
       Detaching(api, "Detaching", name),
       privateGrowth(api, "Growth", name),
       PrivateGreen(api, "Green", name),
       Retranslocation (api, "Retranslocation", name)
     {
     zeroAllGlobals();
     plant = p;
     c.name = name;
     c.dm_init = 0;
     c.n_init_conc = 0;
     c.p_init_conc = 0;
     c.n_sen_conc = 0;
     c.trans_frac = 1;
     c.trans_frac_option = false;
     c.n_retrans_fraction = 1.0;
     c.sen_detach_frac = 0;
     c.p_stress_determinant = false;
     c.p_yield_part = false;
     c.p_retrans_part = false;
     c.stress_determinant = false;
     c.yield_part = false;
     c.retrans_part = false;
     c.n_deficit_uptake_fraction = 0;
     tempFlagToShortCircuitInit1 = false;
     };


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

   scienceAPI.exposeFunction(addPartToVar("digestibility_max_dm_green"), "0-1", addPartToDesc("Maximum Digestibility of green dry matter of "), FloatFunction(&plantPart::digestibilityMaxDmGreen));
   scienceAPI.exposeFunction(addPartToVar("digestibility_avg_dm_green"), "0-1", addPartToDesc("Average Digestibility of green dry matter of "), FloatFunction(&plantPart::digestibilityAvgDmGreen));
   scienceAPI.exposeFunction(addPartToVar("digestibility_min_dm_green"), "0-1", addPartToDesc("Minimum Digestibility of green dry matter of "), FloatFunction(&plantPart::digestibilityMinDmGreen));

   scienceAPI.exposeFunction(addPartToVar("digestibility_max_dm_senesced"), "0-1", addPartToDesc("Maximum Digestibility of senesced dry matter of "), FloatFunction(&plantPart::digestibilityMaxDmSenesced));
   scienceAPI.exposeFunction(addPartToVar("digestibility_avg_dm_senesced"), "0-1", addPartToDesc("Average Digestibility of senesced dry matter of "), FloatFunction(&plantPart::digestibilityAvgDmSenesced));
   scienceAPI.exposeFunction(addPartToVar("digestibility_min_dm_senesced"), "0-1", addPartToDesc("Minimum Digestibility of senesced dry matter of "), FloatFunction(&plantPart::digestibilityMinDmSenesced));



   scienceAPI.exposeFunction(addPartToVar("dlt_dm_retrans"), "g/m^2", addPartToDesc("DM retranslocated to/from "), FloatFunction(&plantPart::dltDmGreenRetrans));
   scienceAPI.exposeFunction(addPartToVar("dm_demand"), "g/m^2", addPartToDesc("DM demand of "), FloatFunction(&plantPart::dmGreenDemand));

   // These next 6 variables are the same as the ones above.
   string LcaseName = c.name;
   To_lower(LcaseName);

   scienceAPI.expose(LcaseName + "_wt", "g/m^2", addPartToDesc("Weight of "), Green().DM);
   scienceAPI.expose(LcaseName + "_n", "g/m^2", addPartToDesc("N in "), Green().N);
   scienceAPI.expose(LcaseName + "_p", "g/m^2", addPartToDesc("P in "), Green().P);
   }

float plantPart::nConcCrit()
//=======================================================================================
   {
   return g.n_conc_crit * fract2pcnt;
   }

float plantPart::nConcMin()
//=======================================================================================
   {
   return g.n_conc_min * fract2pcnt;
   }

void plantPart::get_dm_green_demand(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
   {
   system->sendVariable(qd, dmGreenDemand());
   }


float plantPart::dltNGreen(void)
   //===========================================================================
{
   return Growth().N;
}


float plantPart::dltPGreen(void)
   //===========================================================================
{
   return Growth().P;
}


float plantPart::dltDmSenesced(void)
   //===========================================================================
{
   return Senescing.DM;
}


float plantPart::dltNSenesced(void)
   //===========================================================================
{
   return Senescing.N;
}


float plantPart::dltPSenesced(void)
   //===========================================================================
{
   return Senescing.P;
}

float plantPart::dltNDetached(void)
   //===========================================================================
{
   return Detaching.N;
}


float plantPart::dltPDetached(void)
   //===========================================================================
{
   return Detaching.P;
}


float plantPart::n_conc_crit(void)
   //===========================================================================
{
   return g.n_conc_crit;
}


float plantPart::n_conc_min(void)
   //===========================================================================
{
   return g.n_conc_min;
}


float plantPart::dltNRetrans(void)
   //===========================================================================
{
   return Retranslocation.N;
}


float plantPart::dltNSenescedRetrans(void)
   //===========================================================================
{
   return dlt.n_senesced_retrans;
}


float plantPart::dltNSenescedTrans(void)
   //===========================================================================
{
   return dlt.n_senesced_trans;
}


float plantPart::dltDmGreenRetrans(void)
   //===========================================================================
{
   return Retranslocation.DM;
}


void plantPart::zeroDltDmGreen(void)
//=======================================================================================
   {
   Growth().DM = 0.0;
   }

void plantPart::zeroDltDmGreenRetrans(void)
//=======================================================================================
   {
   Retranslocation.DM = 0.0;
   }

float plantPart::digestibilityMaxDmGreen(void)
   //===========================================================================
{
   return Green().DigestibilityMax.value(plant->getStageCode());
}

float plantPart::digestibilityAvgDmGreen(void)
   //===========================================================================
{
   return Green().DigestibilityAvg.value(plant->getStageCode());
}

float plantPart::digestibilityMinDmGreen(void)
   //===========================================================================
{
   return Green().DigestibilityMin.value(plant->getStageCode());
}

float plantPart::digestibilityMaxDmSenesced(void)
   //===========================================================================
{
   return Senesced().DigestibilityMax.value(plant->getStageCode());
}

float plantPart::digestibilityAvgDmSenesced(void)
   //===========================================================================
{
   return Senesced().DigestibilityAvg.value(plant->getStageCode());
}

float plantPart::digestibilityMinDmSenesced(void)
   //===========================================================================
{
   return Senesced().DigestibilityMin.value(plant->getStageCode());
}

void plantPart::zeroAllGlobals(void)
//=======================================================================================
   {
   Green().Clear();
   Senesced().Clear();
   Height=0.0;
   Width=0.0;
   g.n_conc_crit=0.0;
   g.n_conc_max=0.0;
   g.n_conc_min=0.0;
   g.p_conc_sen=0.0;
   g.p_conc_max=0.0;
   g.p_conc_min=0.0;
   DMPlantMin=0.0;

   relativeGrowthRate = 0.0;
   radiationInterceptedGreen = 0.0;
   radiationInterceptedTotal = 0.0;
   transpEff = 0.0;

   zeroDeltas();
   }

void plantPart::zeroDeltas(void)
//=======================================================================================
   {
   dlt.dm_pot_te = 0.0;
   dlt.dm_pot_rue = 0.0;
   dlt.dm = 0.0;
   Growth().Clear();
   Senescing.Clear();
   Detaching.Clear();
   Retranslocation.Clear();
   dlt.dm_green_removed = 0.0;
   dlt.dm_senesced_removed = 0.0;


   dlt.n_senesced_retrans = 0.0;
   dlt.n_senesced_trans = 0.0;

   dlt.height = 0.0;
   dlt.width = 0.0;

   DMGreenDemand = 0.0;
   NCapacity = 0.0;
   NDemand = 0.0 ;
   SoilNDemand = 0.0;
   NMax = 0.0 ;
   PDemand = 0.0;
   sw_demand_te = 0.0;
   sw_demand = 0.0;
   }

void plantPart::zeroDltNSenescedTrans(void)
//=======================================================================================
   {
   dlt.n_senesced_trans = 0.0;
   }

void plantPart::checkBounds(void)
//=======================================================================================
   {
   // Use a small comparison tolerance here.
   const float ctz = -0.00001;
   if (Green().DM < ctz) throw std::runtime_error(c.name + " dm_green pool is negative! " + ftoa(Green().DM,".6"));
   if (Green().N < ctz) throw std::runtime_error(c.name + " n_green pool is negative!" + ftoa(Green().N,".6"));
   if (Green().P < ctz) throw std::runtime_error(c.name + " p_green pool is negative!" + ftoa(Green().P,".6"));
   if (Senesced().DM < ctz) throw std::runtime_error(c.name + " dm_sen pool is negative!" + ftoa(Senesced().DM,".6"));
   if (Senesced().N < ctz) throw std::runtime_error(c.name + " n_sen pool is negative!" + ftoa(Senesced().N,".6"));
   if (Senesced().P < ctz) throw std::runtime_error(c.name + " p_sen pool is negative!" + ftoa(Senesced().P,".6"));
   }

void plantPart::readConstants(protocol::Component *, const string &)
//=======================================================================================
    {
    vector<string> parts;
    scienceAPI.readOptional("stress_determinants", parts);
    if (find_if(parts.begin(), parts.end(), CaseInsensitiveStringComparison(c.name)) != parts.end())
       {
       c.p_stress_determinant = true;
       c.stress_determinant = true;
       }
    else
       {
       c.p_stress_determinant = false;
       c.stress_determinant = false;
       }

    scienceAPI.readOptional("yield_parts", parts);
    if (find_if(parts.begin(),parts.end(), CaseInsensitiveStringComparison(c.name)) != parts.end())
       {
       c.p_yield_part = true;
       c.yield_part = true;
       }
    else
       {
       c.p_yield_part = false;
       c.yield_part = false;
       }

    scienceAPI.readOptional("retrans_parts", parts);
    if (find_if(parts.begin(),parts.end(), CaseInsensitiveStringComparison(c.name)) != parts.end())
       {
       c.p_retrans_part = true;
       c.retrans_part = true;
       }
    else
       {
       c.p_retrans_part = false;
       c.retrans_part = false;
       }

    if (plant->phosphorusAware())
       {
       scienceAPI.read("x_p_stage_code", c.x_p_stage_code, c.num_x_p_stage_code, 0.0, 12.0);
       scienceAPI.read("y_p_conc_max_" + c.name, c.y_p_conc_max, c.num_x_p_stage_code, 0.0, 1.0);
       scienceAPI.read("y_p_conc_sen_" + c.name, c.y_p_conc_sen, c.num_x_p_stage_code, 0.0, 1.0);
       scienceAPI.read("y_p_conc_min_" + c.name, c.y_p_conc_min, c.num_x_p_stage_code, 0.0, 1.0);
       scienceAPI.read(c.name + "_p_conc_init", c.p_init_conc, 0.0f, 1.0f);
       }
     else
       {
       c.num_x_p_stage_code = 0;
       c.p_init_conc = 0.0;
       }
    }

void plantPart::readSpeciesParameters (protocol::Component *, vector<string> &)
//=======================================================================================
    {
    scienceAPI.read(c.name + "_trans_frac", c.trans_frac, 0.0f, 1.0f);
    if (!scienceAPI.readOptional(c.name + "_trans_frac_option", c.trans_frac_option, 1, 2))
      c.trans_frac_option=1;

    scienceAPI.read(c.name + "_sen_detach_frac", c.sen_detach_frac, 0.0f, 1.0f);
    scienceAPI.read(c.name + "_dm_init", c.dm_init, 0.0f, 1.0f);
    scienceAPI.read(c.name + "_n_init_conc", c.n_init_conc, 0.0f, 1.0f);

    c.n_conc_crit.read(scienceAPI
                        , "x_stage_code" , "()", 1.0, 100.0
                        , ("y_n_conc_crit_" + c.name).c_str(), "()", 0.0, 100.0);

    c.n_conc_min.read(scienceAPI
                        , "x_stage_code" , "()", 1.0, 100.0
                        , ("y_n_conc_min_" + c.name).c_str(), "()", 0.0, 100.0);

    c.n_conc_max.read(scienceAPI
                        , "x_stage_code" , "()", 1.0, 100.0
                        , ("y_n_conc_max_" + c.name).c_str(), "()", 0.0, 100.0);

    c.dm_sen_frac.read(scienceAPI
                        , ("x_dm_sen_frac_" + c.name).c_str(), "()", 0.0, 100.0
                        , ("y_dm_sen_frac_" + c.name).c_str(), "()", 0.0, 1.0);

    scienceAPI.read(c.name + "_n_sen_conc", c.n_sen_conc, 0.0f, 1.0f);

    c.fr_remain.read(scienceAPI
                     , "fr_height_cut",  "(0-1)", 0.0, 1.0
                     , ("fr_"+c.name+"_remain").c_str(), "(0-1)", 0.0, 1.0);

    if (!scienceAPI.readOptional(c.name + "_n_retrans_fraction", c.n_retrans_fraction, 0.0f, 1.0f))
        c.n_retrans_fraction = 1.0;

    if (!scienceAPI.readOptional("n_deficit_uptake_fraction", c.n_deficit_uptake_fraction, 0.0f, 1.0f))
        c.n_deficit_uptake_fraction = 0.0;

    }

void plantPart::readCultivarParameters (protocol::Component*, const string&)
//=======================================================================================
   {
   c.height.read(scienceAPI
                , ("x_" + c.name + "_wt").c_str() , "(g/plant)", 0.0, 1000.0
                , "y_height", "(mm)", 0.0, 5000.0);
   c.width.read(scienceAPI
                , ("x_" + c.name + "_wt").c_str() , "(g/plant)", 0.0, 1000.0
                , "y_width", "(mm)", 0.0, 5000.0);
   }

void plantPart::onEmergence()
//=======================================================================================
   {
   Green().Init(plant->getPlants());
   }

void plantPart::onFlowering(void)
//=======================================================================================
   {
   float dm_plant = divide (Green().DM, plant->getPlants(), 0.0);
   if (c.trans_frac_option==1)
      DMPlantMin = dm_plant;
   else
      DMPlantMin = dm_plant * (1.0 - c.trans_frac);
   }

void plantPart::onStartGrainFill(void)
//=======================================================================================
// set the minimum weight of part; used for retranslocation to grain
   {
   float dm_plant = divide (Green().DM, plant->getPlants(), 0.0);
   if (c.trans_frac_option==1)
      DMPlantMin = dm_plant * (1.0 - c.trans_frac);
   //else
      //DMPlantMin = DMPlantMin + (dm_plant - DMPlantMin) * (1.0 - c.trans_frac);
      //DMPlantMin = DMPlantMin + (dm_plant - DMPlantMin);
   }

void plantPart::onKillStem(void)
//=======================================================================================
   {
   float dm_init = u_bound(plantPart::c.dm_init * plant->getPlants(), plantPart::Green().DM);
   float n_init = u_bound(dm_init * plantPart::c.n_init_conc, plantPart::Green().N);
   float p_init = u_bound(dm_init * plantPart::c.p_init_conc, plantPart::Green().P);

   Senesced().DM += Green().DM - dm_init;
   Senesced().DM = l_bound (Senesced().DM, 0.0);
   Green().DM = dm_init;

   Senesced().N += Green().N - n_init;
   Senesced().N = l_bound (Senesced().N, 0.0);
   Green().N = n_init;

   Senesced().P += Green().P - p_init;
   Senesced().P = l_bound (Senesced().P, 0.0);
   Green().P = p_init;


   }


void plantPart::doNConccentrationLimits(float)
//=======================================================================================
   {
   g.n_conc_crit = c.n_conc_crit.value(plant->getStageCode());
   g.n_conc_min = c.n_conc_min.value(plant->getStageCode());
   g.n_conc_max = c.n_conc_max.value(plant->getStageCode());
   }

void plantPart::morphology(void)
//=======================================================================================
   {
   float dm_plant;               // dry matter of part (g/plant)
   dm_plant = divide (Green().DM, plant->getPlants(), 0.0);

   if (c.height.isInitialised())
      {
      float new_height = c.height.value(dm_plant);       // new plant height (mm)
      dlt.height = l_bound(new_height - Height, 0.0);
      }
   else
      {
      dlt.height = 0.0;
      }

   if (c.width.isInitialised())
      {
      float new_width = c.width.value(dm_plant);
      dlt.width = l_bound(new_width - Width, 0.0);
      }
   else
      {
      dlt.width = 0.0;
      }
   }

void plantPart::prepare(void)
//=======================================================================================
   {
   zeroDeltas();
   }

// Note.
// The following table describes the transfer of material that should
// take place
//                        POOLS
//                 green senesced  dead
// dlt_green         +                     (incoming only)
// dlt_retrans       +-
// dlt_senesced      -      +
// dlt_dead          -      -       +
// dlt_detached             -       -      (outgoing only)
//
// Each pool is treated in the same manner.
void plantPart::update(void)
//=======================================================================================
   {
   Green() = Green() + Growth();
   Senescing.Move (Green(), Senesced());
   Senesced() = Senesced() - Detaching;
   Green() = Green() + Retranslocation;

   Green().N += dlt.n_senesced_retrans;
   relativeGrowthRate = divide (Growth().DM, plant->getDltDmGreen(), 0.0);


   float dying_fract_plants = plant->getDyingFractionPlants();

   float n_green_dead = Green().N * dying_fract_plants;
   Green().N -= n_green_dead;
   Senesced().N += n_green_dead;
   Senescing.N +=n_green_dead;
   Green().N = l_bound(Green().N, 0.0);   // Can occur at total leaf senescence.

   float dm_green_dead = Green().DM * dying_fract_plants;
   Green().DM -=  dm_green_dead;
   Senesced().DM += dm_green_dead;
   Senescing.DM += dm_green_dead;

   if (plant->phosphorusAware())
      {
      float p_green_dead = Green().P * dying_fract_plants;
      Green().P -= p_green_dead;
      Senesced().P += p_green_dead;
      Senescing.P += p_green_dead;
      }

   Height += dlt.height;
   Width += dlt.width;
   }

void plantPart::removeBiomass(void)
//=======================================================================================
// deltas have been given from an external module; update states.
   {
//    update();
   Green().N -= dltNGreenRemoved();
   Senesced().N -= dltNSenescedRemoved();


   Green().P -= dltPGreenRemoved();
   Senesced().P -= dltPSenescedRemoved();


   Green().DM -= dltDmGreenRemoved();
   Senesced().DM -= dltDmSenescedRemoved();

   }

void plantPart::doRemoveBiomass(protocol::RemoveCropDmType dmRemoved, string &c_remove_biomass_report)
//=======================================================================================
// deltas have been given from an external module; update states.
{
    float error_margin = 1.0e-6 ;

    for (unsigned int pool = 0; pool < dmRemoved.dm.size(); pool++)
    {
       for (unsigned int part = 0; part < dmRemoved.dm[pool].part.size(); part++)
       {
          if (Str_i_Eq(dmRemoved.dm[pool].pool, "green"))
          {
             if (Str_i_Eq(dmRemoved.dm[pool].part[part], c.name))       {giveDmGreenRemoved(dmRemoved.dm[pool].dlt[part]); }
             else {  /* not my part */ }
          }

          else if (Str_i_Eq(dmRemoved.dm[pool].pool, "senesced"))
          {
             if (Str_i_Eq(dmRemoved.dm[pool].part[part], c.name))       {giveDmSenescedRemoved(dmRemoved.dm[pool].dlt[part]); }
             else { /* not my part */ }
          }

          else if (Str_i_Eq(dmRemoved.dm[pool].pool, "dead"))
          {
             if (Str_i_Eq(dmRemoved.dm[pool].part[part], c.name) && dmRemoved.dm[pool].dlt[part] != 0.0)       {throw std::runtime_error(c.name + " cannot have dead dm removed "); }
             else { /* not my part */ }
          }
          else { /* unknown type */ }
       }
    }

    if (c_remove_biomass_report == "on")
    {
       ostringstream msg1;
       msg1 << "Remove Crop Biomass 2:-" << endl;
       float dmTotal1 = 0.0;

       msg1 << ("   dm green "+c.name+" = ") << dltDmGreenRemoved() << " (g/m2)" << endl;
       dmTotal1 += dltDmGreenRemoved();

       msg1 << ("   dm senesced "+c.name+" = ") << dltDmSenescedRemoved() << " (g/m2)" << endl;
       dmTotal1 +=  dltDmSenescedRemoved();

       msg1 << endl << ("   dm total "+c.name+" = ") << dmTotal1 << " (g/m2)" << endl << ends;

       plant->writeString (msg1.str().c_str());

       ostringstream msg2;
       msg2 << "Crop Biomass Available:-" << endl;
       float dmTotal2 = 0.0;

       msg2 << ("   dm green "+c.name+" = ") << Green().DM << " (g/m2)" << endl;
       dmTotal2 +=  Green().DM;

       msg2 << ("   dm senesced "+c.name+" = ") << Senesced().DM << " (g/m2)" << endl;
       dmTotal2 +=  Senesced().DM;

       msg2 << endl << ("   dm total "+c.name+" = ") << dmTotal2 << " (g/m2)" << endl << ends;

       plant->writeString (msg2.str().c_str());
    }

    // Check sensibility of part deltas
     if (dltDmGreenRemoved() > (Green().DM + error_margin))
     {
          ostringstream msg;
          msg << "Attempting to remove more green " << name() << " biomass than available:-" << endl;
          msg << "Removing " << -dltDmGreenRemoved() << " (g/m2) from " << Green().DM << " (g/m2) available." << ends;
          throw std::runtime_error (msg.str().c_str());
     }
     else if (dltDmSenescedRemoved() > (Senesced().DM + error_margin))
     {
          ostringstream msg;
          msg << "Attempting to remove more senesced " << name() << " biomass than available:-" << endl;
          msg << "Removing " << -dltDmSenescedRemoved() << " (g/m2) from " << Senesced().DM << " (g/m2) available." << ends;
          throw std::runtime_error (msg.str().c_str());
     }
     else
     { // no more checks
     }
}

void plantPart::removeBiomass2(float)
   {
   }
void plantPart::doNDemand1Pot(float dlt_dm             //  Whole plant the daily biomass production (g/m^2)
                            , float dlt_dm_pot_rue)    //  Whole plant potential dry matter production (g/m^2)
//=======================================================================================
   {
   // Estimate of dlt dm green
   Growth().DM = dlt_dm_pot_rue * divide (Green().DM, plant->getDmGreenTot(), 0.0);

   doNDemand1(dlt_dm, dlt_dm_pot_rue);
   Growth().DM = 0.0;
   }

void plantPart::doNDemand1(float dlt_dm               //   Whole plant the daily biomass production (g/m^2)
                          , float dlt_dm_pot_rue)     //  Whole plant potential dry matter production (g/m^2)
//=======================================================================================
//     Return plant nitrogen demand for this plant component
   {
   float part_fract = divide (Growth().DM, dlt_dm, 0.0);
   float dlt_dm_pot = dlt_dm_pot_rue * part_fract;         // potential dry weight increase (g/m^2)
   dlt_dm_pot = bound(dlt_dm_pot, 0.0, dlt_dm_pot_rue);

   if (Green().DM > 0.0)
     {
      // get N demands due to difference between
      // actual N concentrations and critical N concentrations
      float N_crit       = Green().DM * g.n_conc_crit;    // critical N amount (g/m^2)
      float N_potential  = Green().DM * g.n_conc_max;     // maximum N uptake potential (g/m^2)

      // retranslocation is -ve for outflows
      float N_demand_old = N_crit                       // demand for N by old biomass (g/m^2)
                         - (Green().N + Retranslocation.N);
      float N_max_old    = N_potential                  // N required by old biomass to reach  N_conc_max  (g/m^2)
                         - (Green().N + Retranslocation.N);

      // get potential N demand (critical N) of potential growth
      float N_demand_new = dlt_dm_pot * g.n_conc_crit;     // demand for N by new growth (g/m^2)
      float N_max_new    = dlt_dm_pot * g.n_conc_max;      // N required by new growth to reach N_conc_max  (g/m^2)

      NDemand = N_demand_old + N_demand_new;
      NMax    = N_max_old    + N_max_new ;

      NDemand = l_bound (NDemand, 0.0);
      NMax    = l_bound (NMax, 0.0);
      }
   else
      {
      NDemand = NMax = 0.0;
      }
   }

void plantPart::doNDemand2(float dlt_dm               // (INPUT)  Whole plant the daily biomass production (g/m^2)
                          , float dlt_dm_pot_rue)     // (INPUT)  Whole plant potential dry matter production (g/m^2)
//=======================================================================================
//       Return plant nitrogen demand for each plant component
//+  Notes
//           Nitrogen required for grain growth has already been removed
//           from the stover.  Thus the total N demand is the sum of the
//           demands of the stover and roots.  Stover N demand consists of
//           two components:
//           Firstly, the demand for nitrogen by the potential new Growth().
//           Secondly, the demand due to the difference between
//           the actual N concentration and the critical N concentration
//           of the tops (stover), which can be positive or negative
//
//           NOTE that this routine will not work if the root:shoot ratio
//           is broken. - NIH

   {
   float part_fract = divide (Growth().DM, dlt_dm, 0.0);
   float dlt_dm_pot = dlt_dm_pot_rue * part_fract;         // potential dry weight increase (g/m^2)
   dlt_dm_pot = bound(dlt_dm_pot, 0.0, dlt_dm_pot_rue);

   if (Green().DM > 0.0)
      {
      // get N demands due to difference between
      // actual N concentrations and critical N concentrations
      float N_crit       = Green().DM * g.n_conc_crit;    // critical N amount (g/m^2)
      float N_potential  = Green().DM * g.n_conc_max;     // maximum N uptake potential (g/m^2)

      // retranslocation is -ve for outflows
      float N_demand_old = N_crit - Green().N;            // demand for N by old biomass (g/m^2)
      if (N_demand_old > 0.0)                             // Don't allow demand to satisfy all deficit
         N_demand_old *= c.n_deficit_uptake_fraction;

      float N_max_old    = N_potential - Green().N;       // N required by old biomass to reach N_conc_max  (g/m^2)

      if (N_max_old>0.0)
         N_max_old *= c.n_deficit_uptake_fraction;        // Don't allow demand to satisfy all deficit


      // get potential N demand (critical N) of potential growth
      float N_demand_new = dlt_dm_pot * g.n_conc_crit;     // demand for N by new growth (g/m^2)
      float N_max_new    = dlt_dm_pot * g.n_conc_max;      // N required by new growth to reach N_conc_max  (g/m^2)

      NDemand = N_demand_old + N_demand_new;
      NMax    = N_max_old    + N_max_new ;

      NDemand = l_bound (NDemand, 0.0);
      NMax    = l_bound (NMax, 0.0);
      }
   else
      {
      NDemand = 0.0;
      NMax = 0.0;
      }
   }

void plantPart::doPDemand(void)
//=======================================================================================
   {
   float    deficit;
   float    pConcMax;
   float    totalPotentialGrowthRate;

   PDemand = 0.0;
   totalPotentialGrowthRate = plant->getTotalPotentialGrowthRate();

   if (c.p_yield_part)
      {
      // A yield part - does not contribute to soil demand
      PDemand = 0.0;
      }
   else
      {
      // Not a yield part - therefore it contributes to demand
      pConcMax = linear_interp_real (plant->getStageCode()
                                     , c.x_p_stage_code
                                     , c.y_p_conc_max
                                     , c.num_x_p_stage_code);

   // scale up to include potential new growth
   // assuming partitioning today similar to current
   // plant form - a rough approximation

      float dltDMPot = totalPotentialGrowthRate * relativeGrowthRate;
      float PDemandNew = dltDMPot * pConcMax;
      float PDemandOld = (Green().DM * pConcMax) - Green().P;
      PDemandOld = l_bound (PDemandOld, 0.0);

      deficit = PDemandOld + PDemandNew;
      deficit = l_bound (deficit, 0.0);

      PDemand = deficit;
   // float pDemandMax = pDemandNew * pUptakeFactor;
   // PDemand = u_bound (deficit, pDemandMax);
      }
 // FIXME - remove following 4 lines after P demand corrections above are activated
   float rel_growth_rate = plant->getRelativeGrowthRate();
   float p_conc_max = linear_interp_real (plant->getStageCode()
                                     , c.x_p_stage_code
                                     , c.y_p_conc_max
                                     , c.num_x_p_stage_code);
   deficit = p_conc_max * Green().DM * (1.0 + rel_growth_rate) - Green().P;
   PDemand = l_bound(deficit, 0.0);
   }

void plantPart::doSoilNDemand(void)
//=======================================================================================
   {
   SoilNDemand = NDemand - dlt.n_senesced_retrans;
   SoilNDemand = l_bound(SoilNDemand,0.0);
   }

void plantPart::doSenescence(float sen_fr)
//=======================================================================================
   {
   float fraction_senescing = c.dm_sen_frac.value(sen_fr);
   fraction_senescing = bound (fraction_senescing, 0.0, 1.0);
   Senescing.DM = (Green().DM + Growth().DM + Retranslocation.DM)
                   * fraction_senescing;
   }

//void plantPart::doDmPartition(float DMAvail, float DMDemandTotal)
//=======================================================================================
//   {
//   dlt.dm_green = DMAvail * divide (DMGreenDemand, DMDemandTotal, 0.0);
//   }

void plantPart::doDmRetranslocate(float DMAvail, float DMDemandDifferentialTotal)
//=======================================================================================
   {
   Retranslocation.DM = DMAvail * divide (dmDemandDifferential(), DMDemandDifferentialTotal, 0.0);
   }

float plantPart::dmDemandDifferential(void)
//=======================================================================================
   {
   return l_bound(dmGreenDemand() - dltDmGreen(), 0.0);
   }

float plantPart::dltDmRetranslocateSupply(float /* DemandDifferential*/)
//=======================================================================================
   {
//   float DMPartPot = DMGreen + dlt.dm_green_retrans;
//   float DMPartAvail = DMPartPot - DMPlantMin * plant->getPlants();
//   DMPartAvail = l_bound (DMPartAvail, 0.0);
//   float DltDmRetransPart = min (DemandDifferential, DMPartAvail);
//   dlt.dm_green_retrans = - DltDmRetransPart;
//   return DltDmRetransPart;
   return 0.0;
   }

float plantPart::nDemandDifferential(void)
//=======================================================================================
   {
   return l_bound(nDemand() - dltNGreen(), 0.0);
   }

void plantPart::doNSenescence(void)
//=======================================================================================
   {
   float green_n_conc = divide (Green().N, Green().DM, 0.0);

   float dlt_n_in_senescing_part = Senescing.DM * green_n_conc;

   float sen_n_conc = min (c.n_sen_conc, green_n_conc);

   Senescing.N = Senescing.DM * sen_n_conc;
   Senescing.N = u_bound (Senescing.N, Green().N);

   dlt.n_senesced_trans = dlt_n_in_senescing_part - Senescing.N;
   dlt.n_senesced_trans = l_bound(dlt.n_senesced_trans, 0.0);
   }

void plantPart::doNSenescedRetrans(float navail, float n_demand_tot)
//=======================================================================================
   {
   dlt.n_senesced_retrans = navail * divide (NDemand, n_demand_tot, 0.0);
   }

void plantPart::doNFixRetranslocate(float NFix, float NDemandDifferentialTotal)
//=======================================================================================
   {
   Growth().N += NFix * divide (nDemandDifferential(), NDemandDifferentialTotal, 0.0);
   }

void plantPart::doNRetranslocate( float N_supply, float g_grain_n_demand)
//=======================================================================================
   {
   if (g_grain_n_demand >= N_supply)
      {
      // demand greater than or equal to supply
      // retranslocate all available N
      Retranslocation.N = - availableRetranslocateN();
      }
   else
      {
      // supply greater than demand.
      // Retranslocate what is needed
      Retranslocation.N = - g_grain_n_demand * divide (availableRetranslocateN(), N_supply, 0.0);
      }
// need to do bound check here  FIXME
   }

void plantPart::Detachment(void)
//=======================================================================================
   {
   Detaching = Senesced() * c.sen_detach_frac;
   }

void plantPart::doPSenescence(void)
//=======================================================================================
   {
   float sen_p_conc = linear_interp_real (plant->getStageCode()
                                        , c.x_p_stage_code
                                        , c.y_p_conc_sen
                                        , c.num_x_p_stage_code);

   Senescing.P = u_bound(sen_p_conc, Green().Pconc()) * Senescing.DM;
   Senescing.P = u_bound (Senescing.P, Green().P);
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
      dm += (*part)->Green().DM;
      N += (*part)->Green().N;
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



void plantPart::onEndCrop(vector<string> &dm_type,
                          vector<float> &dlt_crop_dm,
                          vector<float> &dlt_dm_n,
                          vector<float> &dlt_dm_p,
                          vector<float> &fraction_to_residue)
//=======================================================================================
   {
   dm_type.push_back(c.name);
   dlt_crop_dm.push_back ((Green().DM + Senesced().DM) * gm2kg/sm2ha);
   dlt_dm_n.push_back    ((Green().N  + Senesced().N)  * gm2kg/sm2ha);
   dlt_dm_p.push_back    ((Green().P  + Senesced().P)       * gm2kg/sm2ha);
   fraction_to_residue.push_back(1.0);

   Senesced().Clear();
   Green().Clear();

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

   float dm_init = u_bound (c.dm_init * plant->getPlants(), Green().DM);
   float n_init  = u_bound (  dm_init * plantPart::c.n_init_conc, Green().N);
   float p_init  = u_bound (  dm_init * plantPart::c.p_init_conc, Green().P);

   float retain_fr_green = divide(dm_init, Green().DM, 0.0);
   float retain_fr_sen   = 0.0;

   float dlt_dm_harvest = Green().DM + Senesced().DM - dm_init;
   float dlt_n_harvest  = Green().N  + Senesced().N  - n_init;
   float dlt_p_harvest  = Green().P  + Senesced().P - p_init;

   Senesced().DM *= retain_fr_sen;
   Green().DM    *= retain_fr_green;

   Senesced().N *= retain_fr_sen;
   Green().N    = n_init;

   Senesced().P   *= retain_fr_sen;
   Green().P  = p_init;

   dm_type.push_back(c.name);
   fraction_to_residue.push_back(fractToResidue);
   dlt_crop_dm.push_back (dlt_dm_harvest * gm2kg/sm2ha);
   dlt_dm_n.push_back    (dlt_n_harvest  * gm2kg/sm2ha);
   dlt_dm_p.push_back    (dlt_p_harvest  * gm2kg/sm2ha);
}

float plantPart::availableRetranslocateN(void)
//=======================================================================================
//    Calculate N available for transfer to grain (g/m^2)
//    from each plant part.
   {
   float N_min = g.n_conc_min * Green().DM;
   float N_avail = l_bound (Green().N - N_min, 0.0);
   return (N_avail * c.n_retrans_fraction);
   }

void plantPart::collectDetachedForResidue(vector<string> &part_name
                              , vector<float> &dm_residue
                              , vector<float> &dm_n
                              , vector<float> &dm_p
                              , vector<float> &fraction_to_residue)
//=======================================================================================
   {
   part_name.push_back(c.name);
   dm_residue.push_back(Detaching.DM * gm2kg/sm2ha);
   dm_n.push_back(Detaching.N * gm2kg/sm2ha);
   dm_p.push_back(Detaching.P * gm2kg/sm2ha);
   fraction_to_residue.push_back(1.0);
   }


float plantPart::dmGreenDemand(void)
//=======================================================================================
   {
   return (DMGreenDemand);
   }

float plantPart::dltDmGreen(void)
//=======================================================================================
   {
   return (Growth().DM);
   }

float plantPart::transpirationEfficiency(void)
//=======================================================================================
   {
   return (transpEff);
   }

float plantPart::dltDmPotRue(void)
//=======================================================================================
   {
   return (dlt.dm_pot_rue);
   }

float plantPart::dltDmPotTe(void)
//=======================================================================================
   {
   return (dlt.dm_pot_te);
   }

float plantPart::dltDm(void)
//=======================================================================================
   {
   return (dlt.dm);
   }

float plantPart::dltDmRetranslocate(void)
//=======================================================================================
   {
   return (Retranslocation.DM);
   }

float plantPart::dltDmGreenRetransUptake(void)
//=======================================================================================
   {
   return (Retranslocation.DM);
   }

float plantPart::dmGreenNew(void)
//=======================================================================================
   {
   return (Green().DM + Growth().DM + Retranslocation.DM);
   }

float plantPart::dltDmGreenNew(void)
//=======================================================================================
   {
   return (Growth().DM + Retranslocation.DM);
   }

float plantPart::dltDmDetached(void)
//=======================================================================================
   {
   return (Detaching.DM);
   }

float plantPart::dltDmGreenRemoved(void)
//=======================================================================================
   {
   return (dlt.dm_green_removed);
   }

float plantPart::dltDmSenescedRemoved(void)
//=======================================================================================
   {
   return (dlt.dm_senesced_removed);
   }


float plantPart::dltNGreenRemoved(void)
//=======================================================================================
   {
   return (Green().N * divide(dlt.dm_green_removed, Green().DM, 0.0));
   }

float plantPart::dltNSenescedRemoved(void)
//=======================================================================================
   {
   return (Senesced().N * divide(dlt.dm_senesced_removed, Senesced().DM, 0.0));
   }


float plantPart::dltPGreenRemoved(void)
//=======================================================================================
   {
   return (Green().P * divide(dlt.dm_green_removed, Green().DM, 0.0));
   }

float plantPart::dltPSenescedRemoved(void)
//=======================================================================================
   {
   return (Senesced().P * divide(dlt.dm_senesced_removed, Senesced().DM, 0.0));
   }

float plantPart::dltDmRemoved(void)
//=======================================================================================
   {
   return (dltDmGreenRemoved() + dltDmSenescedRemoved());
   }

float plantPart::dltNRemoved(void)
//=======================================================================================
   {
   return (dltNGreenRemoved() + dltNSenescedRemoved());
   }

float plantPart::dltPRemoved(void)
//=======================================================================================
   {
   return (dltPGreenRemoved() + dltPSenescedRemoved());
   }

float plantPart::dmSenescedVeg(void)
//=======================================================================================
   {
   return (Senesced().DM);
   }

float plantPart::dmGreenStressDeterminant(void)
//=======================================================================================
   {
   if (c.p_stress_determinant)
      return Green().DM;
   else
      return 0.0;
   }

float plantPart::pGreenStressDeterminant(void)
//=======================================================================================
   {
   if (c.p_stress_determinant)
      return Green().P;
   else
      return 0.0;
   }

float plantPart::pMaxPotStressDeterminant(void)
//=======================================================================================
   {
   if (c.p_stress_determinant)
      return pMaxPot();
   else
      return 0.0;
 }

float plantPart::pMinPotStressDeterminant(void)
//=======================================================================================
   {
   if (c.p_stress_determinant)
      return pMinPot();
   else
      return 0.0;
   }

float plantPart::height(void) {return Height;}
float plantPart::width(void) {return Width;}

float plantPart::soilNDemand(void) {return (SoilNDemand);}
float plantPart::nDemand(void)  {return (NDemand);}
float plantPart::nMax(void) {return (NMax);}
float plantPart::nCapacity(void)
//=======================================================================================
   {
   NCapacity = l_bound(NMax - NDemand, 0.0);
   return (NCapacity);
   }

void plantPart::doNPartition(float nSupply, float n_demand_sum, float n_capacity_sum)
   //============================================================================
{
   Growth().N = 0.0;

   float n_excess = nSupply - n_demand_sum;
   n_excess = l_bound (n_excess, 0.0);

   if (n_excess>0.0)
      {
      float plant_part_fract = divide (nCapacity(), n_capacity_sum, 0.0);
      Growth().N = nDemand() + n_excess * plant_part_fract;
      }
   else
      {
      float plant_part_fract = divide (nDemand(), n_demand_sum, 0.0);
      Growth().N = nSupply * plant_part_fract;
      }
}

float plantPart::pDemand(void) {return (PDemand);}

float plantPart::dltNRetransOut(void)
//=======================================================================================
   {
   if(Retranslocation.N < 0.0)
      return (Retranslocation.N);
   else
      return 0.0;
   }

float plantPart::nMaxPot(void)
//=======================================================================================
   {
   float n_conc_max = c.n_conc_max.value(plant->getStageCode());
   return n_conc_max * Green().DM;
   }

float plantPart::nMinPot(void)
//=======================================================================================
   {
   float n_conc_min = c.n_conc_min.value(plant->getStageCode());
   return n_conc_min * Green().DM;
   }


float plantPart::pRetransSupply(void)
//=======================================================================================
   {
   if (c.p_retrans_part)
      return l_bound(Green().P - pMinPot(), 0.0);
   else
      return 0.0;
   }

float plantPart::nRetransSupply(void)
//=======================================================================================
   {
//   if (c.retrans_part)
//       return l_bound(Green().N - nMinPot(), 0.0);
   return 0.0;
   }

float plantPart::dmRetransSupply(void)
//=======================================================================================
   {
   if (c.retrans_part)
      return l_bound(Green().DM - (DMPlantMin*plant->getPlants()), 0.0);
   return 0.0;
   }

float plantPart::pRetransDemand(void)
//=======================================================================================
   {
   if (c.p_yield_part)
      return l_bound(pMaxPot() - Green().P, 0.0);
   else
      return 0.0;
   }


void plantPart::doPPartition(float p_uptake, float total_p_demand)
//=======================================================================================
   {
   Growth().P = p_uptake * divide(pDemand(), total_p_demand, 0.0);
   }

void plantPart::doPRetranslocate(float total_p_supply, float total_p_demand)
//=======================================================================================
   {
   double p_supply = pRetransSupply();
   double p_demand = pRetransDemand();
   if (p_supply > 0.0)
      {
      double fraction = divide(total_p_demand, total_p_supply, 0.0);
      fraction = bound(fraction, 0.0, 1.0);
      Retranslocation.P = - p_supply * fraction;
      }
   else if (p_demand > 0.0)
      {
      double fraction = divide(total_p_supply, total_p_demand, 0.0);
      fraction = bound(fraction, 0.0, 1.0);
      Retranslocation.P = p_demand * fraction;
      }
   else
      {
      Retranslocation.P = 0.0;// this part is not involved
      }
   }


float plantPart::pMaxPot(void)
//=======================================================================================
   {
   float p_conc_max = linear_interp_real (plant->getStageCode()
                                         , c.x_p_stage_code
                                         , c.y_p_conc_max
                                         , c.num_x_p_stage_code);
   return p_conc_max * Green().DM;
   }

float plantPart::pMinPot(void)
//=======================================================================================
   {
   float p_conc_min = linear_interp_real (plant->getStageCode()
                                         , c.x_p_stage_code
                                         , c.y_p_conc_min
                                         , c.num_x_p_stage_code);
   return p_conc_min * Green().DM;
   }

void plantPart::onDayOf(const string &stage)
//=======================================================================================
   {
   if (stage == "emergence") onEmergence();
   else if (stage == "flowering") onFlowering();
   else if (stage == "start_grain_fill") onStartGrainFill();
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

void plantPart::get_name(vector<string> &names) {names.push_back(c.name);}
void plantPart::get_p_demand(vector<float> &demands) {demands.push_back(PDemand);}
void plantPart::get_dlt_p_retrans(vector<float> &dlt_p_retrans) {dlt_p_retrans.push_back(Retranslocation.P);}
void plantPart::get_dm_plant_min(vector<float> &dm_min) {dm_min.push_back(DMPlantMin);}
void plantPart::get_dm_green(vector<float> &dm_green) {dm_green.push_back(Green().DM);}
void plantPart::get_dm_senesced(vector<float> &dm_senesced) {dm_senesced.push_back(Senesced().DM);}
void plantPart::get_dlt_dm_green(vector<float> &dlt_dm_green) {dlt_dm_green.push_back(Growth().DM);}
void plantPart::get_dlt_dm_green_retrans(vector<float> &dlt_dm_green_retrans) {dlt_dm_green_retrans.push_back(Retranslocation.DM);}
void plantPart::get_dlt_dm_detached(vector<float> &dlt_dm_detached) {dlt_dm_detached.push_back(Detaching.DM);}
void plantPart::get_dlt_dm_senesced(vector<float> &dlt_dm_senesced) {dlt_dm_senesced.push_back(Senescing.DM);}
void plantPart::get_n_demanded(vector<float> &demands) {demands.push_back(NDemand);}



   //needed to standardise interface for composite subclass

void plantPart::doCover (PlantSpatial &/*spatial*/){}
float plantPart::coverGreen(void) {return 0;}
float plantPart::coverSen(void) {return 0;}
float plantPart::coverTotal(void) {return 0;}
float plantPart::dltDmGrainDemand(void)  {return 0;}
////float plantPart::dltDmRetranslocate(void){return 0;}
float plantPart::dmGrainWetTotal(void) {return 0;}
float plantPart::grainWaterContent(void) {return 0;}
float plantPart::grainNo(void)  {return 0;}
float plantPart::grainWt(void)  {return 0;}
float plantPart::interceptRadiationGreen(float /* radiation*/){return 0;}
float plantPart::interceptRadiationTotal(float /* radiation*/){return 0;}
float plantPart::nDemandGrain(void)  {return 0;}
float plantPart::nDemandGrain2(void){return 0;}
void plantPart::doSWDemand(float /*SWDemandMaxFactor*/){}
float plantPart::SWDemand(void){return sw_demand;}
float plantPart::SWDemandTE(void){return sw_demand_te;}
void plantPart::calcDlt_pod_area (void){}   //FIXME
void plantPart::doBioActual (void){}
void plantPart::doDmDemand (float /* dlt_dm_supply_by_veg*/){}
void plantPart::doDmPotRUE (void){}                         // (OUTPUT) potential dry matter (carbohydrate) production (g/m^2)
void plantPart::doDmPotTE(float /*swSupply*/){}                           //(OUTPUT) potential dry matter production by transpiration (g/m^2)
void plantPart::doGrainNumber (void){}
void plantPart::doNDemandGrain(float /* nfact_grain_conc*/, float /* swdef_expansion*/){}
void plantPart::doNewMet(protocol::NewMetType &/* newmet*/) {}
void plantPart::doNInit (void){}
void plantPart::doTECO2(void){}                              // (OUTPUT) transpiration coefficient                         //FIXME
void plantPart::writeCultivarInfo (protocol::Component *){}

float plantPart::giveDmGreen(float delta)
//=======================================================================================
// giveXXX: something is giving us some XXX. return the amount we actually take.
   {
   Growth().DM += delta;
   return delta;
   }

float plantPart::giveNGreen(float delta)
//=======================================================================================
   {
   Growth().N += delta;
   return delta;
   }

float plantPart::giveDmSenesced (float delta)
//=======================================================================================
   {
   Senescing.DM += delta;
   return delta;
   }

float plantPart::dlt_dm_green_retrans_hack(float delta)
   {
   Retranslocation.DM = delta;
   return delta;
   }

float plantPart::giveDmGreenRemoved (float delta)
//=======================================================================================
// addXXX: something is removing some XXX. return the delta.
   {
   dlt.dm_green_removed = delta;
   float error_margin = 1.0e-6 ;
   if (delta > Green().DM + error_margin)
   {
       ostringstream msg;
       msg << "Attempting to remove more green " << name() << " biomass than available:-" << endl;
       msg << "Removing " << -delta << " (g/m2) from " << Green().DM << " (g/m2) available." << ends;
       throw std::runtime_error (msg.str().c_str());
   }
   return delta;
   }

float plantPart::giveDmSenescedRemoved (float delta)
//=======================================================================================
   {
   dlt.dm_senesced_removed = delta;
   float error_margin = 1.0e-6 ;
   if (delta > Senesced().DM + error_margin)
   {
       ostringstream msg;
       msg << "Attempting to remove more Senesced " << name() << " biomass than available:-" << endl;
       msg << "Removing " << -delta << " (g/m2) from " << Senesced().DM << " (g/m2) available." << ends;
       throw std::runtime_error (msg.str().c_str());
   }
   return delta;
   }


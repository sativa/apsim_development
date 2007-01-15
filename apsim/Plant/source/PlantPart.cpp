
#include "PlantPart.h"
using namespace std;


void plantPart::doRegistrations(protocol::Component *system)
//=======================================================================================
   {
   string varName1, varName2, varName3, varName4, varName5, varName6;
   string varName7, varName8, varName9;
   string desc1, desc2, desc3, desc4, desc5, desc6, desc7, desc8, desc9;


   varName1 = "dm_green_" + c.name;
   desc1 = "Weight of " + c.name;
   system->addGettableVar(varName1.c_str(), DMGreen, "g/m^2", desc1.c_str());

   varName2 = "n_green_" + c.name;
   desc2 = "N in " + c.name;
   system->addGettableVar(varName2.c_str(),  NGreen, "g/m^2", desc2.c_str());

   varName3 = "p_green_" + c.name;
   desc3 = "P in " + c.name;
   system->addGettableVar(varName3.c_str(),  PGreen, "g/m^2", desc3.c_str());

   varName1 = c.name + "_wt";
   desc1 = "Weight of " + c.name;
   system->addGettableVar(varName1.c_str(), DMGreen, "g/m^2", desc1.c_str());

   varName2 = c.name + "_n";
   desc2 = "N in " + c.name;
   system->addGettableVar(varName2.c_str(),  NGreen, "g/m^2", desc2.c_str());

   varName3 = c.name + "_p";
   desc3 = "P in " + c.name;
   system->addGettableVar(varName3.c_str(),  PGreen, "g/m^2", desc3.c_str());

   varName1 = "dm_dead_" + c.name;
   desc1 = "Weight of dead " + c.name;
   system->addGettableVar(varName1.c_str(), DMDead, "g/m^2", desc1.c_str());

   varName2 = "n_dead_" + c.name;
   desc2 = "N in dead " + c.name;
   system->addGettableVar(varName2.c_str(),  NDead, "g/m^2", desc2.c_str());

   varName3 = "p_dead_" + c.name;
   desc3 = "P in dead " + c.name;
   system->addGettableVar(varName3.c_str(),  PDead, "g/m^2", desc3.c_str());

   varName1 = "dead" + c.name + "_wt";
   desc1 = "Weight of dead " + c.name;
   system->addGettableVar(varName1.c_str(), DMDead, "g/m^2", desc1.c_str());

   varName2 = "dead" + c.name + "_n";
   desc2 = "N in dead " + c.name;
   system->addGettableVar(varName2.c_str(),  NDead, "g/m^2", desc2.c_str());

   varName3 = "dead" + c.name + "_p";
   desc3 = "P in dead " + c.name;
   system->addGettableVar(varName3.c_str(),  PDead, "g/m^2", desc3.c_str());

   varName1 = "dm_senesced_" + c.name;
   desc1 = "Weight of senesced " + c.name;
   system->addGettableVar(varName1.c_str(), DMSenesced, "g/m^2", desc1.c_str());

   varName2 = "n_senesced_" + c.name;
   desc2 = "N in senesced " + c.name;
   system->addGettableVar(varName2.c_str(),  NSenesced, "g/m^2", desc2.c_str());

   varName3 = "p_senesced_" + c.name;
   desc3 = "P in senesced " + c.name;
   system->addGettableVar(varName3.c_str(),  PSen, "g/m^2", desc3.c_str());

   varName1 = "dlt_dm_green_" + c.name;
   desc1 = "Delta Weight of " + c.name;
   system->addGettableVar(varName1.c_str(), dlt.dm_green, "g/m^2", desc1.c_str());

   varName2 = "dlt_n_green_" + c.name;
   desc2 = "Delta N in " + c.name;
   system->addGettableVar(varName2.c_str(),  dlt.n_green, "g/m^2", desc2.c_str());

   varName3 = "dlt_p_green_" + c.name;
   desc3 = "Delta P in " + c.name;
   system->addGettableVar(varName3.c_str(),  dlt.p_green, "g/m^2", desc3.c_str());

   varName1 = "dlt_dm_dead_" + c.name;
   desc1 = "Delta Weight of dead " + c.name;
   system->addGettableVar(varName1.c_str(), dlt.dm_dead, "g/m^2", desc1.c_str());

   varName2 = "dlt_n_dead_" + c.name;
   desc2 = "Delta N in dead " + c.name;
   system->addGettableVar(varName2.c_str(),  dlt.n_dead, "g/m^2", desc2.c_str());

   varName3 = "dlt_p_dead_" + c.name;
   desc3 = "Delta P in dead " + c.name;
   system->addGettableVar(varName3.c_str(),  dlt.p_dead, "g/m^2", desc3.c_str());

   varName1 = "dlt_dm_senesced_" + c.name;
   desc1 = "Delta Weight of senesced " + c.name;
   system->addGettableVar(varName1.c_str(), dlt.dm_senesced, "g/m^2", desc1.c_str());

   varName2 = "dlt_n_senesced_" + c.name;
   desc2 = "Delta N in senesced " + c.name;
   system->addGettableVar(varName2.c_str(),  dlt.n_senesced, "g/m^2", desc2.c_str());

   varName3 = "dlt_p_senesced_" + c.name;
   desc3 = "Delta P in senesced " + c.name;
   system->addGettableVar(varName3.c_str(),  dlt.p_sen, "g/m^2", desc3.c_str());

   varName1 = "dlt_dm_detached_" + c.name;
   desc1 = "Delta Weight of detached " + c.name;
   system->addGettableVar(varName1.c_str(), dlt.dm_detached, "g/m^2", desc1.c_str());

   varName2 = "dlt_n_detached_" + c.name;
   desc2 = "Delta N in detached " + c.name;
   system->addGettableVar(varName2.c_str(),  dlt.n_detached, "g/m^2", desc2.c_str());

   varName3 = "dlt_p_detached_" + c.name;
   desc3 = "Delta P in detached " + c.name;
   system->addGettableVar(varName3.c_str(),  dlt.p_det, "g/m^2", desc3.c_str());

   varName4 =  "n_conc_" + c.name;
   desc4 = "N concentration in " + c.name;
   setupGetFunction(system, varName4.c_str(), protocol::DTsingle, false,
                    &plantPart::get_n_conc, "%", desc4.c_str());

   varName5 = "p_conc_" + c.name;
   desc5 = "P concentration in " + c.name;
   setupGetFunction(system, varName5.c_str(), protocol::DTsingle, false,
                    &plantPart::get_p_conc, "%", desc5.c_str());

   varName6 = "n_conc_crit_" + c.name;
   desc6 = "critical N content in " + c.name;
   setupGetFunction(system, varName6.c_str(), protocol::DTsingle, false,
                    &plantPart::get_n_conc_crit,
                    "%", desc6.c_str());

   varName7 = "n_conc_min_" + c.name;
   desc7 = "minimum N content in " + c.name;
   setupGetFunction(system, varName7.c_str(), protocol::DTsingle, false,
                    &plantPart::get_n_conc_min,
                    "%", desc7.c_str());

   varName8 = "n_demand_" + c.name;
   desc8 = "N demand of " + c.name;
   system->addGettableVar(varName8.c_str(),
               NDemand, "g/m^2", desc8.c_str());

   varName9 = "dlt_n_retrans_" + c.name;
   desc9 = "N retranslocated to/from " + c.name;
   system->addGettableVar(varName9.c_str(),
               dlt.n_retrans, "g/m^2", desc9.c_str());

   varName9 = "dlt_n_senesced_retrans_" + c.name;
   desc9 = "N retranslocated to/from senesced " + c.name;
   system->addGettableVar(varName9.c_str(),
               dlt.n_senesced_retrans, "g/m^2", desc9.c_str());

   varName9 = "dlt_n_senesced_trans_" + c.name;
   desc9 = "N translocated to/from senesced " + c.name;
   system->addGettableVar(varName9.c_str(),
               dlt.n_senesced_trans, "g/m^2", desc9.c_str());

   varName9 = "dlt_dm_retrans_" + c.name;
   desc9 = "DM retranslocated to/from " + c.name;
   system->addGettableVar(varName9.c_str(),
               dlt.dm_green_retrans, "g/m^2", desc9.c_str());

   varName9 = "dm_demand_" + c.name;
   desc9 = "DM demand of " + c.name;
   setupGetFunction(system, varName9.c_str(), protocol::DTsingle, false,
                    &plantPart::get_dm_green_demand, "g/m^2", desc9.c_str());
   }

void plantPart::get_n_conc(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
   {
   float n_conc = divide (NGreen, DMGreen, 0.0) * fract2pcnt;
   system->sendVariable(qd, n_conc);
   }

void plantPart::get_n_conc_crit(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
   {
   system->sendVariable(qd, (float) (g.n_conc_crit * fract2pcnt));
   }

void plantPart::get_n_conc_min(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
   {
   system->sendVariable(qd, (float) (g.n_conc_min * fract2pcnt));
   }

void plantPart::get_p_conc(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
   {
   float p_conc = divide (PGreen, DMGreen, 0.0) * fract2pcnt;
   system->sendVariable(qd, p_conc);
   }

void plantPart::get_dm_green_demand(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
   {
   system->sendVariable(qd, dmGreenDemand());
   }


float plantPart::dltNGreen(void) const
   //===========================================================================
{
   return dlt.n_green;
}


float plantPart::dltPGreen(void) const
   //===========================================================================
{
   return dlt.p_green;
}


float plantPart::dltDmDead(void) const
   //===========================================================================
{
   return dlt.dm_dead;
}


float plantPart::dltNDead(void) const
   //===========================================================================
{
   return dlt.n_dead;
}


float plantPart::dltPDead(void) const
   //===========================================================================
{
   return dlt.p_dead;
}


float plantPart::dltDmSenesced(void) const
   //===========================================================================
{
   return dlt.dm_senesced;
}


float plantPart::dltNSenesced(void) const
   //===========================================================================
{
   return dlt.n_senesced;
}


float plantPart::dltPSenesced(void) const
   //===========================================================================
{
   return dlt.p_sen;
}

float plantPart::dltNDetached(void) const
   //===========================================================================
{
   return dlt.n_detached;
}


float plantPart::dltPDetached(void) const
   //===========================================================================
{
   return dlt.p_det;
}


float plantPart::n_conc_crit(void) const
   //===========================================================================
{
   return g.n_conc_crit;
}


float plantPart::n_conc_min(void) const
   //===========================================================================
{
   return g.n_conc_min;
}


float plantPart::dltNRetrans(void) const
   //===========================================================================
{
   return dlt.n_retrans;
}


float plantPart::dltNSenescedRetrans(void) const
   //===========================================================================
{
   return dlt.n_senesced_retrans;
}


float plantPart::dltNSenescedTrans(void) const
   //===========================================================================
{
   return dlt.n_senesced_trans;
}


float plantPart::dltDmGreenRetrans(void) const
   //===========================================================================
{
   return dlt.dm_green_retrans;
}


void plantPart::zeroDltDmGreen(void)
//=======================================================================================
   {
   dlt.dm_green = 0.0;
   }

void plantPart::zeroDltDmGreenRetrans(void)
//=======================================================================================
   {
   dlt.dm_green_retrans = 0.0;
   }

void plantPart::zeroAllGlobals(void)
//=======================================================================================
   {
   DMDead=0.0;
   DMGreen=0.0;
   DMSenesced=0.0;
   NDead=0.0;
   NGreen=0.0;
   NSenesced=0.0;
   Height=0.0;
   Width=0.0;
   g.n_conc_crit=0.0;
   g.n_conc_max=0.0;
   g.n_conc_min=0.0;
   g.p_conc_sen=0.0;
   g.p_conc_max=0.0;
   g.p_conc_min=0.0;
   DMPlantMin=0.0;

   PGreen=0.0;
   PSen=0.0;
   PDead=0.0;
   relativeGrowthRate = 0.0;

   zeroDeltas();
   }

void plantPart::zeroDeltas(void)
//=======================================================================================
   {
   dlt.dm_green = 0.0;
   dlt.dm_senesced = 0.0;
   dlt.dm_detached = 0.0;
   dlt.dm_dead = 0.0;
   dlt.dm_dead_detached = 0.0;
   dlt.dm_green_retrans = 0.0;
   dlt.dm_green_dead = 0.0;
   dlt.dm_senesced_dead = 0.0;
   dlt.dm_green_removed = 0.0;
   dlt.dm_senesced_removed = 0.0;
   dlt.dm_dead_removed = 0.0;

   dlt.n_green = 0.0;
   dlt.n_senesced = 0.0;
   dlt.n_senesced_retrans = 0.0;
   dlt.n_senesced_trans = 0.0;
   dlt.n_detached = 0.0;
   dlt.n_dead = 0.0;
   dlt.n_dead_detached = 0.0;
   dlt.n_retrans = 0.0;
   dlt.n_green_dead = 0.0;
   dlt.n_senesced_dead = 0.0;

   dlt.p_green = 0.0;
   dlt.p_sen = 0.0;
   dlt.p_det = 0.0;
   dlt.p_dead_det = 0.0;
   dlt.p_retrans = 0.0;
   dlt.p_dead = 0.0;
   dlt.p_green_dead = 0.0;
   dlt.p_senesced_dead = 0.0;

   dlt.height = 0.0;
   dlt.width = 0.0;

   DMGreenDemand = 0.0;
   NCapacity = 0.0;
   NDemand = 0.0 ;
   SoilNDemand = 0.0;
   NMax = 0.0 ;
   PDemand = 0.0;
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
   if (DMGreen < ctz) throw std::runtime_error(c.name + " dm_green pool is negative! " + ftoa(DMGreen,".6"));
   if (NGreen < ctz) throw std::runtime_error(c.name + " n_green pool is negative!" + ftoa(NGreen,".6"));
   if (PGreen < ctz) throw std::runtime_error(c.name + " p_green pool is negative!" + ftoa(PGreen,".6"));
   if (DMDead < ctz) throw std::runtime_error(c.name + " dm_dead pool is negative!" + ftoa(DMDead,".6"));
   if (NDead < ctz) throw std::runtime_error(c.name + " n_dead pool is negative!" + ftoa(NDead,".6"));
   if (PDead < ctz) throw std::runtime_error(c.name + " p_dead pool is negative!" + ftoa(PDead,".6"));
   if (DMSenesced < ctz) throw std::runtime_error(c.name + " dm_sen pool is negative!" + ftoa(DMSenesced,".6"));
   if (NSenesced < ctz) throw std::runtime_error(c.name + " n_sen pool is negative!" + ftoa(NSenesced,".6"));
   if (PSen < ctz) throw std::runtime_error(c.name + " p_sen pool is negative!" + ftoa(PSen,".6"));
   }

void plantPart::readConstants(protocol::Component *system, const string &section)
//=======================================================================================
    {
    vector<string> parts;
    Split_string(system->readParameter (section, "stress_determinants"), " ", parts);
    if (find(parts.begin(), parts.end(), c.name) != parts.end())
       {
       c.p_stress_determinant = true;
       c.stress_determinant = true;
       }
    else
       {
       c.p_stress_determinant = false;
       c.stress_determinant = false;
       }

    Split_string(system->readParameter (section, "yield_parts"), " ", parts);
    if (find(parts.begin(),parts.end(), c.name) != parts.end())
       {
       c.p_yield_part = true;
       c.yield_part = true;
       }
    else
       {
       c.p_yield_part = false;
       c.yield_part = false;
       }

    Split_string(system->readParameter (section, "retrans_parts"), " ", parts);
    if (find(parts.begin(),parts.end(), c.name) != parts.end())
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
       system->readParameter (section, "x_p_stage_code", c.x_p_stage_code, c.num_x_p_stage_code, 0.0, 12.0);

       system->readParameter (section, ("y_p_conc_max_" + c.name).c_str(),
                              c.y_p_conc_max, c.num_x_p_stage_code, 0.0, 1.0);
       system->readParameter (section, ("y_p_conc_sen_" + c.name).c_str(),
                              c.y_p_conc_sen, c.num_x_p_stage_code, 0.0, 1.0);
       system->readParameter (section, ("y_p_conc_min_" + c.name).c_str(),
                              c.y_p_conc_min, c.num_x_p_stage_code, 0.0, 1.0);
       system->readParameter (section, c.name + "_p_conc_init",
                              c.p_init_conc, 0.0, 1.0);
       }
     else
       {
       c.num_x_p_stage_code = 0;
       c.p_init_conc = 0.0;
       }
    }

void plantPart::readSpeciesParameters(protocol::Component *system, vector<string> &sections)
//=======================================================================================
    {
    system->readParameter (sections
                            , c.name + "_trans_frac"
                            , c.trans_frac
                            , 0.0, 1.0);
    if(system->readParameter (sections
                            , c.name + "_trans_frac_option"
                            , c.trans_frac_option
                            , 1, 2,true)==false)
      c.trans_frac_option=1;

    system->readParameter (sections
                            , c.name + "_sen_detach_frac"
                            , c.sen_detach_frac
                            , 0.0, 1.0);

    system->readParameter (sections
                            , c.name + "_dead_detach_frac"
                            , c.dead_detach_frac
                            , 0.0, 1.0);

    system->readParameter (sections
                            , c.name + "_dm_init"
                            , c.dm_init
                            , 0.0, 1.0);

    system->readParameter (sections
                            , c.name + "_n_init_conc"
                            , c.n_init_conc
                            , 0.0, 1.0);

    c.n_conc_crit.search(system, sections
                        , "x_stage_code" , "()", 1.0, 100.0
                        , ("y_n_conc_crit_" + c.name).c_str(), "()", 0.0, 100.0);

    c.n_conc_min.search(system, sections
                        , "x_stage_code" , "()", 1.0, 100.0
                        , ("y_n_conc_min_" + c.name).c_str(), "()", 0.0, 100.0);

    c.n_conc_max.search(system, sections
                        , "x_stage_code" , "()", 1.0, 100.0
                        , ("y_n_conc_max_" + c.name).c_str(), "()", 0.0, 100.0);

    c.dm_sen_frac.search(system, sections
                        , ("x_dm_sen_frac_" + c.name).c_str(), "()", 0.0, 100.0
                        , ("y_dm_sen_frac_" + c.name).c_str(), "()", 0.0, 1.0);

    system->readParameter (sections
                        , (c.name + "_n_sen_conc").c_str() //, "()"
                        , c.n_sen_conc
                        , 0.0, 1.0);

    c.fr_remain.search(system, sections
                     , "fr_height_cut",  "(0-1)", 0.0, 1.0
                     , ("fr_"+c.name+"_remain").c_str(), "(0-1)", 0.0, 1.0);

    if (system->readParameter (sections
                             , (c.name + "_n_retrans_fraction").c_str()
                             , c.n_retrans_fraction
                             , 0.0, 1.0, true) == false)
        c.n_retrans_fraction = 1.0;

    if (system->readParameter (sections
                            , "n_deficit_uptake_fraction"
                            , c.n_deficit_uptake_fraction
                            , 0.0, 1.0, true) == false)
        c.n_deficit_uptake_fraction = 0.0;

    }

void plantPart::readCultivarParameters (protocol::Component *system, const string &cultivar)
//=======================================================================================
   {
   c.height.read(system, cultivar
                , ("x_" + c.name + "_wt").c_str() , "(g/plant)", 0.0, 1000.0
                , "y_height", "(mm)", 0.0, 5000.0);
   c.width.read(system, cultivar
                , ("x_" + c.name + "_wt").c_str() , "(g/plant)", 0.0, 1000.0
                , "y_width", "(mm)", 0.0, 5000.0);
   }
void plantPart::onSowing()
//=======================================================================================
   {
   }
void plantPart::onGermination()
//=======================================================================================
   {
   }
void plantPart::onEmergence()
//=======================================================================================
   {
   DMGreen = c.dm_init * plant->getPlants();
   NGreen = c.n_init_conc * DMGreen;
   PGreen = c.p_init_conc * DMGreen;
   }

void plantPart::onFlowering(void)
//=======================================================================================
   {
   float dm_plant = divide (DMGreen, plant->getPlants(), 0.0);
   if (c.trans_frac_option==1)
      DMPlantMin = dm_plant;
   else
      DMPlantMin = dm_plant * (1.0 - c.trans_frac);
   }

void plantPart::onStartGrainFill(void)
//=======================================================================================
// set the minimum weight of part; used for retranslocation to grain
   {
   float dm_plant = divide (DMGreen, plant->getPlants(), 0.0);
   if (c.trans_frac_option==1)
      DMPlantMin = dm_plant * (1.0 - c.trans_frac);
   //else
      //DMPlantMin = DMPlantMin + (dm_plant - DMPlantMin) * (1.0 - c.trans_frac);
      //DMPlantMin = DMPlantMin + (dm_plant - DMPlantMin);
   }

void plantPart::onKillStem(void)
//=======================================================================================
   {
   float dm_init = u_bound(plantPart::c.dm_init * plant->getPlants(), plantPart::DMGreen);
   float n_init = u_bound(dm_init * plantPart::c.n_init_conc, plantPart::NGreen);
   float p_init = u_bound(dm_init * plantPart::c.p_init_conc, plantPart::PGreen);

   DMDead += DMGreen + DMSenesced - dm_init;
   DMDead = l_bound (DMDead, 0.0);
   DMGreen = dm_init;
   DMSenesced = 0.0;

   NDead += NGreen + NSenesced - n_init;
   NDead = l_bound (NDead, 0.0);
   NGreen = n_init;
   NSenesced = 0.0;

   PDead += PGreen + PSen - p_init;
   PDead = l_bound (PDead, 0.0);
   PGreen = p_init;
   PSen = 0.0;

   }


void plantPart::doProcessBioDemand(void)
//===========================================================================
{
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
   dm_plant = divide (DMGreen, plant->getPlants(), 0.0);

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
   updateDm();
   updateN();
   updateP();

   Height += dlt.height;
   Width += dlt.width;
   }

void plantPart::updateN(void)
//=======================================================================================
   {
   // transfer N
   NDead -= dlt.n_dead_detached;

   NGreen += dlt.n_green;
   NGreen += dlt.n_retrans;
   NGreen -= dlt.n_senesced;

   NSenesced += dlt.n_senesced;
   NGreen += dlt.n_senesced_retrans;
   NSenesced -= dlt.n_detached;

   float dying_fract_plants = plant->getDyingFractionPlants();
   dlt.n_green_dead = NGreen * dying_fract_plants;
   NGreen -= dlt.n_green_dead;
   NDead += dlt.n_green_dead;

   dlt.n_senesced_dead = NSenesced * dying_fract_plants;
   NSenesced -= dlt.n_senesced_dead;
   NDead += dlt.n_senesced_dead;

   NGreen = l_bound(NGreen, 0.0);   // Can occur at total leaf senescence.
   }

void plantPart::updateDm(void)
//=======================================================================================
   {
   // Update DM
   DMDead -= dlt.dm_dead_detached;

   DMGreen += dlt.dm_green;
   DMGreen += dlt.dm_green_retrans;
   DMGreen -= dlt.dm_senesced;
   relativeGrowthRate = divide (dlt.dm_green, plant->getDltDmGreen(), 0.0);

   DMSenesced += dlt.dm_senesced;
   DMSenesced -= dlt.dm_detached;

   float dying_fract_plants = plant->getDyingFractionPlants();
   dlt.dm_green_dead = DMGreen * dying_fract_plants;
   DMGreen -=  dlt.dm_green_dead;
   DMDead += dlt.dm_green_dead;

   dlt.dm_senesced_dead = DMSenesced * dying_fract_plants;
   DMSenesced -= dlt.dm_senesced_dead;
   DMDead += dlt.dm_senesced_dead;
   }

void plantPart::updateP(void)
//=======================================================================================
   {
   if (plant->phosphorusAware())
      {
      // Update P
      PDead -= dlt.p_dead_det;
      PGreen += dlt.p_green;
      PGreen += dlt.p_retrans;
      PGreen -= dlt.p_sen;

      PSen += dlt.p_sen;
      PSen -= dlt.p_det;

      float dying_fract_plants = plant->getDyingFractionPlants();
      dlt.p_green_dead = PGreen * dying_fract_plants;
      PGreen -= dlt.p_green_dead;
      PDead += dlt.p_green_dead;

      dlt.p_senesced_dead = PSen * dying_fract_plants;
      PSen  -= dlt.p_senesced_dead;
      PDead += dlt.p_senesced_dead;
      PGreen = l_bound(PGreen, 0.0);  // Can occur at total leaf senescence.
      }
   }

void plantPart::removeBiomass(void)
//=======================================================================================
// deltas have been given from an external module; update states.
   {
//    update();
   DMGreen -= dltDmGreenRemoved();
   DMSenesced -= dltDmSenescedRemoved();
   DMDead -= dltDmDeadRemoved();

   NGreen -= dltNGreenRemoved();
   NSenesced -= dltNSenescedRemoved();
   NDead -= dltNDeadRemoved();

   PGreen -= dltPGreenRemoved();
   PSen -= dltPSenescedRemoved();
   PDead -= dltNDeadRemoved();
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
          if (dmRemoved.dm[pool].pool == "green")
          {
             if (dmRemoved.dm[pool].part[part] == c.name)       {giveDmGreenRemoved(dmRemoved.dm[pool].dlt[part]); }
             else {  /* not my part */ }
          }

          else if (dmRemoved.dm[pool].pool == "senesced")
          {
             if (dmRemoved.dm[pool].part[part] == c.name)       {giveDmSenescedRemoved(dmRemoved.dm[pool].dlt[part]); }
             else { /* not my part */ }
          }

          else if (dmRemoved.dm[pool].pool == "dead")
          {
             if (dmRemoved.dm[pool].part[part] == c.name)       {giveDmDeadRemoved(dmRemoved.dm[pool].dlt[part]); }
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

       msg1 << "   dm green "+c.name+" = " << dltDmGreenRemoved() << " (g/m2)" << endl;
       dmTotal1 += dltDmGreenRemoved();

       msg1 << "   dm senesced "+c.name+" = " << dltDmSenescedRemoved() << " (g/m2)" << endl;
       dmTotal1 +=  dltDmSenescedRemoved();

       msg1 << "   dm dead "+c.name+" = " << dltDmDeadRemoved() << " (g/m2)" << endl;
       dmTotal1 +=  dltDmDeadRemoved();

       msg1 << endl << "   dm total "+c.name+" = " << dmTotal1 << " (g/m2)" << endl << ends;

       plant->writeString (msg1.str().c_str());

       ostringstream msg2;
       msg2 << "Crop Biomass Available:-" << endl;
       float dmTotal2 = 0.0;

       msg2 << "   dm green "+c.name+" = " << dmGreen() << " (g/m2)" << endl;
       dmTotal2 +=  dmGreen();

       msg2 << "   dm senesced "+c.name+" = " << dmSenesced() << " (g/m2)" << endl;
       dmTotal2 +=  dmSenesced();

       msg2 << "   dm dead "+c.name+" = " << dmDead() << " (g/m2)" << endl;
       dmTotal2 +=  dmDead();

       msg2 << endl << "   dm total "+c.name+" = " << dmTotal2 << " (g/m2)" << endl << ends;

       plant->writeString (msg2.str().c_str());
    }

    // Check sensibility of part deltas
     if (dltDmGreenRemoved() > (dmGreen() + error_margin))
     {
          ostringstream msg;
          msg << "Attempting to remove more green " << name() << " biomass than available:-" << endl;
          msg << "Removing " << -dltDmGreenRemoved() << " (g/m2) from " << dmGreen() << " (g/m2) available." << ends;
          throw std::runtime_error (msg.str().c_str());
     }
     else if (dltDmSenescedRemoved() > (dmSenesced() + error_margin))
     {
          ostringstream msg;
          msg << "Attempting to remove more senesced " << name() << " biomass than available:-" << endl;
          msg << "Removing " << -dltDmSenescedRemoved() << " (g/m2) from " << dmSenesced() << " (g/m2) available." << ends;
          throw std::runtime_error (msg.str().c_str());
     }
     else if (dltDmDeadRemoved() > (dmDead() + error_margin))
     {
          ostringstream msg;
          msg << "Attempting to remove more dead " << name() << " biomass than available:-" << endl;
          msg << "Removing " << -dltDmDeadRemoved() << " (g/m2) from " << dmDead() << " (g/m2) available." << ends;
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
   dlt.dm_green = dlt_dm_pot_rue * divide (DMGreen, plant->getDmGreenTot(), 0.0);

   doNDemand1(dlt_dm, dlt_dm_pot_rue);
   dlt.dm_green = 0.0;
   }

void plantPart::doNDemand1(float dlt_dm               //   Whole plant the daily biomass production (g/m^2)
                          , float dlt_dm_pot_rue)     //  Whole plant potential dry matter production (g/m^2)
//=======================================================================================
//     Return plant nitrogen demand for this plant component
   {
   float part_fract = divide (dlt.dm_green, dlt_dm, 0.0);
   float dlt_dm_pot = dlt_dm_pot_rue * part_fract;         // potential dry weight increase (g/m^2)
   dlt_dm_pot = bound(dlt_dm_pot, 0.0, dlt_dm_pot_rue);

   if (DMGreen > 0.0)
     {
      // get N demands due to difference between
      // actual N concentrations and critical N concentrations
      float N_crit       = DMGreen * g.n_conc_crit;    // critical N amount (g/m^2)
      float N_potential  = DMGreen * g.n_conc_max;     // maximum N uptake potential (g/m^2)

      // retranslocation is -ve for outflows
      float N_demand_old = N_crit                       // demand for N by old biomass (g/m^2)
                         - (NGreen + dlt.n_retrans);
      float N_max_old    = N_potential                  // N required by old biomass to reach  N_conc_max  (g/m^2)
                         - (NGreen + dlt.n_retrans);

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
//           Firstly, the demand for nitrogen by the potential new growth.
//           Secondly, the demand due to the difference between
//           the actual N concentration and the critical N concentration
//           of the tops (stover), which can be positive or negative
//
//           NOTE that this routine will not work if the root:shoot ratio
//           is broken. - NIH

   {
   float part_fract = divide (dlt.dm_green, dlt_dm, 0.0);
   float dlt_dm_pot = dlt_dm_pot_rue * part_fract;         // potential dry weight increase (g/m^2)
   dlt_dm_pot = bound(dlt_dm_pot, 0.0, dlt_dm_pot_rue);

   if (DMGreen > 0.0)
      {
      // get N demands due to difference between
      // actual N concentrations and critical N concentrations
      float N_crit       = DMGreen * g.n_conc_crit;    // critical N amount (g/m^2)
      float N_potential  = DMGreen * g.n_conc_max;     // maximum N uptake potential (g/m^2)

      // retranslocation is -ve for outflows
      float N_demand_old = N_crit - NGreen;            // demand for N by old biomass (g/m^2)
      if (N_demand_old > 0.0)                             // Don't allow demand to satisfy all deficit
         N_demand_old *= c.n_deficit_uptake_fraction;

      float N_max_old    = N_potential - NGreen;       // N required by old biomass to reach N_conc_max  (g/m^2)

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
      float PDemandOld = (DMGreen * pConcMax) - PGreen;
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
   deficit = p_conc_max * DMGreen * (1.0 + rel_growth_rate) - PGreen;
   PDemand = l_bound(deficit, 0.0);
   }

void plantPart::doSoilNDemand(void)
//=======================================================================================
   {
   SoilNDemand = NDemand - dlt.n_senesced_retrans;
   SoilNDemand = l_bound(SoilNDemand,0.0);
   }

void plantPart::doSenescence1(float sen_fr)
//=======================================================================================
   {
   float fraction_senescing = c.dm_sen_frac.value(sen_fr);
   fraction_senescing = bound (fraction_senescing, 0.0, 1.0);
   dlt.dm_senesced = (DMGreen + dlt.dm_green + dlt.dm_green_retrans)
                   * fraction_senescing;
   }

void plantPart::doSenescence2(float sen_fr)
//=======================================================================================
   {
   float fraction_senescing = c.dm_sen_frac.value(sen_fr);
   fraction_senescing = bound (fraction_senescing, 0.0, 1.0);
   dlt.dm_senesced = DMGreen * fraction_senescing;
   }

//void plantPart::doDmPartition(float DMAvail, float DMDemandTotal)
//=======================================================================================
//   {
//   dlt.dm_green = DMAvail * divide (DMGreenDemand, DMDemandTotal, 0.0);
//   }

void plantPart::doDmRetranslocate(float DMAvail, float DMDemandDifferentialTotal)
//=======================================================================================
   {
   dlt.dm_green_retrans = DMAvail * divide (dmDemandDifferential(), DMDemandDifferentialTotal, 0.0);
   }

float plantPart::dmDemandDifferential(void) const
//=======================================================================================
   {
   return l_bound(dmGreenDemand() - dltDmGreen(), 0.0);
   }

float plantPart::dltDmRetranslocateSupply(float DemandDifferential)
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

void plantPart::doDmMin(void)
//=======================================================================================
   {   // do nothing - set on events
   }

float plantPart::nDemandDifferential(void)
//=======================================================================================
   {
   return l_bound(nDemand() - dltNGreen(), 0.0);
   }

void plantPart::doNSenescence(void)
//=======================================================================================
   {
   float green_n_conc = divide (NGreen, DMGreen, 0.0);

   float dlt_n_in_senescing_part = dlt.dm_senesced * green_n_conc;

   float sen_n_conc = min (c.n_sen_conc, green_n_conc);

   dlt.n_senesced = dlt.dm_senesced * sen_n_conc;
   dlt.n_senesced = u_bound (dlt.n_senesced, NGreen);

   dlt.n_senesced_trans = dlt_n_in_senescing_part - dlt.n_senesced;
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
   dlt.n_green += NFix * divide (nDemandDifferential(), NDemandDifferentialTotal, 0.0);
   }

void plantPart::doNRetranslocate( float N_supply, float g_grain_n_demand)
//=======================================================================================
   {
   if (g_grain_n_demand >= N_supply)
      {
      // demand greater than or equal to supply
      // retranslocate all available N
      dlt.n_retrans = - availableRetranslocateN();
      }
   else
      {
      // supply greater than demand.
      // Retranslocate what is needed
      dlt.n_retrans = - g_grain_n_demand * divide (availableRetranslocateN(), N_supply, 0.0);
      }
// need to do bound check here  FIXME
   }

void plantPart::doDmDetachment(void)
//=======================================================================================
   {
   dlt.dm_detached = DMSenesced * c.sen_detach_frac;
   dlt.dm_dead_detached = DMDead * c.dead_detach_frac;
   }

void plantPart::doNDetachment(void)
//=======================================================================================
   {
   dlt.n_detached = NSenesced * c.sen_detach_frac;
   dlt.n_dead_detached = NDead * c.dead_detach_frac;
   }

void plantPart::doPSenescence(void)
//=======================================================================================
   {
   float green_p_conc = divide (PGreen, DMGreen, 0.0);

   float sen_p_conc = linear_interp_real (plant->getStageCode()
                                        , c.x_p_stage_code
                                        , c.y_p_conc_sen
                                        , c.num_x_p_stage_code);

   dlt.p_sen = u_bound(sen_p_conc, green_p_conc) * dlt.dm_senesced;
   dlt.p_sen = u_bound (dlt.p_sen, PGreen);
   }

void plantPart::doPDetachment(void)
//=======================================================================================
   {
   float sen_detach_frac = divide(dlt.dm_detached, DMSenesced, 0.0);

   dlt.p_det = PSen * sen_detach_frac;
   float dead_detach_frac = divide(dlt.dm_dead_detached, DMDead, 0.0);

   dlt.p_dead_det = PDead * dead_detach_frac;
   }


float critNFactor(vector<const plantPart *> &parts, float multiplier)
//=======================================================================================
//   Calculate Nitrogen stress factor from a bunch of parts
/*  Purpose
*   The concentration of Nitrogen in plant parts is used to derive a Nitrogen stress index
*   for many processes. This stress index is calculated from today's relative nutitional
*   status between a critical and minimum Nitrogen concentration.
*/
   {
   vector<const plantPart *>::iterator part;

   float dm = 0.0, N = 0.0;
   for (part = parts.begin(); part != parts.end(); part++)
      {
      dm += (*part)->dmGreen();
      N += (*part)->nGreen();
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
   dlt_crop_dm.push_back ((DMDead + DMGreen + DMSenesced) * gm2kg/sm2ha);
   dlt_dm_n.push_back    ((NDead  + NGreen  + NSenesced)  * gm2kg/sm2ha);
   dlt_dm_p.push_back    ((PDead  + PGreen  + PSen)       * gm2kg/sm2ha);
   fraction_to_residue.push_back(1.0);
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

   float dm_init = u_bound (c.dm_init * plant->getPlants(), dmGreen());
   float n_init  = u_bound (  dm_init * plantPart::c.n_init_conc, nGreen());
   float p_init  = u_bound (  dm_init * plantPart::c.p_init_conc, pGreen());

   float retain_fr_green = divide(dm_init, dmGreen(), 0.0);
   float retain_fr_dead  = 0.0;
   float retain_fr_sen   = 0.0;

   float dlt_dm_harvest = dmDead() + dmGreen() + dmSenesced() - dm_init;
   float dlt_n_harvest  = nDead()  + nGreen()  + nSenesced()  - n_init;
   float dlt_p_harvest  = pDead()  + pGreen()  + pSenesced() - p_init;

   DMDead     *= retain_fr_dead;
   DMSenesced *= retain_fr_sen;
   DMGreen    *= retain_fr_green;

   NDead     *= retain_fr_dead;
   NSenesced *= retain_fr_sen;
   NGreen    = n_init;

   PDead  *= retain_fr_dead;
   PSen   *= retain_fr_sen;
   PGreen  = p_init;

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
   float N_min = g.n_conc_min * DMGreen;
   float N_avail = l_bound (NGreen - N_min, 0.0);
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
   dm_residue.push_back(dlt.dm_detached * gm2kg/sm2ha);
   dm_n.push_back(dlt.n_detached * gm2kg/sm2ha);
   dm_p.push_back(dlt.p_det * gm2kg/sm2ha);
   fraction_to_residue.push_back(1.0);
   }

void plantPart::collectDeadDetachedForResidue(vector<string> &part_name
                                 , vector<float> &dm_dead_detached
                                 , vector<float> &n_dead_detached
                                 , vector<float> &p_dead_detached
                                 , vector<float> &fraction_to_residue)
//=======================================================================================
   {
   part_name.push_back(c.name);
   dm_dead_detached.push_back(dlt.dm_dead_detached * gm2kg/sm2ha);
   n_dead_detached.push_back(dlt.n_dead_detached * gm2kg/sm2ha);
   p_dead_detached.push_back(dlt.p_dead_det * gm2kg/sm2ha);
   fraction_to_residue.push_back(1.0);
   }

float plantPart::dmTotal(void) const
//=======================================================================================
   {
   return (dmGreen() + dmSenesced() + dmDead());
   }

float plantPart::dmGreenDemand(void) const
//=======================================================================================
   {
   return (DMGreenDemand);
   }

float plantPart::dmGreen(void) const
//=======================================================================================
   {
   return (DMGreen);
   }

float plantPart::dltDmGreen(void) const
//=======================================================================================
   {
   return (dlt.dm_green);
   }

float plantPart::dltDmRetranslocate(void) const
//=======================================================================================
   {
   return (dlt.dm_green_retrans);
   }

float plantPart::dltDmGreenRetransUptake(void) const
//=======================================================================================
   {
   return (dlt.dm_green_retrans);
   }

float plantPart::dmGreenNew(void) const
//=======================================================================================
   {
   return (DMGreen + dlt.dm_green + dlt.dm_green_retrans);
   }

float plantPart::dltDmGreenNew(void) const
//=======================================================================================
   {
   return (dlt.dm_green + dlt.dm_green_retrans);
   }

float plantPart::dltDmDetached(void) const
//=======================================================================================
   {
   return (dlt.dm_detached);
   }

float plantPart::dltDmGreenRemoved(void) const
//=======================================================================================
   {
   return (dlt.dm_green_removed);
   }

float plantPart::dltDmSenescedRemoved(void) const
//=======================================================================================
   {
   return (dlt.dm_senesced_removed);
   }

float plantPart::dltDmDeadRemoved(void) const
//=======================================================================================
   {
   return (dlt.dm_dead_removed);
   }

float plantPart::dltNGreenRemoved(void) const
//=======================================================================================
   {
   return (NGreen * divide(dlt.dm_green_removed, DMGreen, 0.0));
   }

float plantPart::dltNSenescedRemoved(void) const
//=======================================================================================
   {
   return (NSenesced * divide(dlt.dm_senesced_removed, DMSenesced, 0.0));
   }

float plantPart::dltNDeadRemoved(void) const
//=======================================================================================
   {
   return (NDead * divide(dlt.dm_dead_removed, DMDead, 0.0));
   }

float plantPart::dltPGreenRemoved(void) const
//=======================================================================================
   {
   return (PGreen * divide(dlt.dm_green_removed, DMGreen, 0.0));
   }

float plantPart::dltPSenescedRemoved(void) const
//=======================================================================================
   {
   return (PSen * divide(dlt.dm_senesced_removed, DMSenesced, 0.0));
   }

float plantPart::dltPDeadRemoved(void) const
//=======================================================================================
   {
   return (PDead * divide(dlt.dm_dead_removed, DMDead, 0.0));
   }

float plantPart::dltDmRemoved(void) const
//=======================================================================================
   {
   return (dltDmGreenRemoved() + dltDmSenescedRemoved() + dltDmDeadRemoved());
   }

float plantPart::dltNRemoved(void) const
//=======================================================================================
   {
   return (dltNGreenRemoved() + dltNSenescedRemoved() + dltNDeadRemoved());
   }

float plantPart::dltPRemoved(void) const
//=======================================================================================
   {
   return (dltPGreenRemoved() + dltPSenescedRemoved() + dltPDeadRemoved());
   }

float plantPart::dmSenesced(void) const
//=======================================================================================
   {
   return (DMSenesced);
   }

float plantPart::dmDead(void) const
//=======================================================================================
   {
   return (DMDead);
   }

float plantPart::dmGreenStressDeterminant(void)
//=======================================================================================
   {
   if (c.p_stress_determinant)
      return DMGreen;
   else
      return 0.0;
   }

float plantPart::pGreenStressDeterminant(void)
//=======================================================================================
   {
   if (c.p_stress_determinant)
      return PGreen;
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

float plantPart::soilNDemand(void) {return (SoilNDemand);}
float plantPart::nDemand(void) const {return (NDemand);}
float plantPart::nMax(void)const{return (NMax);}
float plantPart::nCapacity(void)
//=======================================================================================
   {
   NCapacity = l_bound(NMax - NDemand, 0.0);
   return (NCapacity);
   }

void plantPart::doNPartition(float nSupply, float n_demand_sum, float n_capacity_sum)
   //============================================================================
{
   dlt.n_green = 0.0;

   float n_excess = nSupply - n_demand_sum;
   n_excess = l_bound (n_excess, 0.0);

   if (n_excess>0.0)
      {
      float plant_part_fract = divide (nCapacity(), n_capacity_sum, 0.0);
      dlt.n_green = nDemand() + n_excess * plant_part_fract;
      }
   else
      {
      float plant_part_fract = divide (nDemand(), n_demand_sum, 0.0);
      dlt.n_green = nSupply * plant_part_fract;
      }
}

float plantPart::pDemand(void) {return (PDemand);}
float plantPart::nTotal(void) const {return (nGreen() + nSenesced() + nDead());}
float plantPart::nGreen(void) const {return (NGreen);}
float plantPart::nSenesced(void) const{return (NSenesced);}
float plantPart::nDead(void) const{return (NDead);}

float plantPart::nConc(void) const
//=======================================================================================
   {
   float n_conc = divide (NGreen, DMGreen, 0.0);
   return n_conc;
   }

float plantPart::nConcPercent(void)const
//=======================================================================================
   {
   return nConc() * fract2pcnt;
   }

float plantPart::dltNRetransOut(void)
//=======================================================================================
   {
   if(dlt.n_retrans < 0.0)
      return (dlt.n_retrans);
   else
      return 0.0;
   }

float plantPart::nMaxPot(void) const
//=======================================================================================
   {
   float n_conc_max = c.n_conc_max.value(plant->getStageCode());
   return n_conc_max * DMGreen;
   }

float plantPart::nMinPot(void) const
//=======================================================================================
   {
   float n_conc_min = c.n_conc_min.value(plant->getStageCode());
   return n_conc_min * DMGreen;
   }
float plantPart::pTotal(void) const {return (pGreen() + pSenesced() + pDead());}
float plantPart::pGreen(void) const {return (PGreen);}
float plantPart::pSenesced(void) const{return (PSen);}
float plantPart::pDead(void) const{return (PDead);}

float plantPart::pConc(void) const
//=======================================================================================
   {
   float p_conc = divide (PGreen, DMGreen, 0.0);
   return p_conc;
   }

float plantPart::pConcPercent(void) const
//=======================================================================================
   {
   return pConc() * fract2pcnt;
   }

float plantPart::pRetransSupply(void)
//=======================================================================================
   {
   if (c.p_retrans_part)
      return l_bound(PGreen - pMinPot(), 0.0);
   else
      return 0.0;
   }

float plantPart::nRetransSupply(void)
//=======================================================================================
   {
//   if (c.retrans_part)
//       return l_bound(NGreen - nMinPot(), 0.0);
   return 0.0;
   }

float plantPart::dmRetransSupply(void) const
//=======================================================================================
   {
   if (c.retrans_part)
      return l_bound(DMGreen - (DMPlantMin*plant->getPlants()), 0.0);
   return 0.0;
   }

float plantPart::pRetransDemand(void)
//=======================================================================================
   {
   if (c.p_yield_part)
      return l_bound(pMaxPot() - PGreen, 0.0);
   else
      return 0.0;
   }

float plantPart::nRetransDemand(void)
//=======================================================================================
   {
//    if (c.yield_part)
//       return l_bound(nMaxPot() - NGreen, 0.0);
//    else
   return 0.0;
   }

float plantPart::dmRetransDemand(void)
//=======================================================================================
   {
//    if (c.yield_part)
//       return l_bound(dmMaxPot() - DMGreen, 0.0);
//    else
   return 0.0;
   }


void plantPart::doPPartition(float p_uptake, float total_p_demand)
//=======================================================================================
   {
   dlt.p_green = p_uptake * divide(pDemand(), total_p_demand, 0.0);
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
      dlt.p_retrans = - p_supply * fraction;
      }
   else if (p_demand > 0.0)
      {
      double fraction = divide(total_p_supply, total_p_demand, 0.0);
      fraction = bound(fraction, 0.0, 1.0);
      dlt.p_retrans = p_demand * fraction;
      }
   else
      {
      dlt.p_retrans = 0.0;// this part is not involved
      }
   }

void plantPart::doPInit()
//=======================================================================================
   {
   PGreen = c.p_init_conc * DMGreen;
   }

float plantPart::pMaxPot(void) const
//=======================================================================================
   {
   float p_conc_max = linear_interp_real (plant->getStageCode()
                                         , c.x_p_stage_code
                                         , c.y_p_conc_max
                                         , c.num_x_p_stage_code);
   return p_conc_max * DMGreen;
   }

float plantPart::pMinPot(void) const
//=======================================================================================
   {
   float p_conc_min = linear_interp_real (plant->getStageCode()
                                         , c.x_p_stage_code
                                         , c.y_p_conc_min
                                         , c.num_x_p_stage_code);
   return p_conc_min * DMGreen;
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
   else if (event == "flowering") onFlowering();
   else if (event == "start_grain_fill") onStartGrainFill();
   }

void plantPart::get_name(vector<string> &names) {names.push_back(c.name);}
void plantPart::get_p_demand(vector<float> &demands) {demands.push_back(PDemand);}
void plantPart::get_dlt_p_green(vector<float> &dlt_p_green) {dlt_p_green.push_back(dlt.p_green);}
void plantPart::get_p_green(vector<float> &p_green) {p_green.push_back(PGreen);}
void plantPart::get_dlt_p_retrans(vector<float> &dlt_p_retrans) {dlt_p_retrans.push_back(dlt.p_retrans);}
void plantPart::get_dm_plant_min(vector<float> &dm_min) {dm_min.push_back(DMPlantMin);}
void plantPart::get_dm_green(vector<float> &dm_green) {dm_green.push_back(DMGreen);}
void plantPart::get_dm_dead(vector<float> &dm_dead) {dm_dead.push_back(DMDead);}
void plantPart::get_dm_senesced(vector<float> &dm_senesced) {dm_senesced.push_back(DMSenesced);}
void plantPart::get_dlt_dm_green(vector<float> &dlt_dm_green) {dlt_dm_green.push_back(dlt.dm_green);}
void plantPart::get_dlt_dm_green_retrans(vector<float> &dlt_dm_green_retrans) {dlt_dm_green_retrans.push_back(dlt.dm_green_retrans);}
void plantPart::get_dlt_dm_detached(vector<float> &dlt_dm_detached) {dlt_dm_detached.push_back(dlt.dm_detached);}
void plantPart::get_dlt_dm_senesced(vector<float> &dlt_dm_senesced) {dlt_dm_senesced.push_back(dlt.dm_senesced);}
void plantPart::get_dlt_dm_dead_detached(vector<float> &dlt_dm_dead_detached) {dlt_dm_dead_detached.push_back(dlt.dm_dead_detached);}
void plantPart::get_dlt_dm_green_dead(vector<float> &dlt_dm_green_dead) {dlt_dm_green_dead.push_back(dlt.dm_green_dead);}
void plantPart::get_dlt_dm_senesced_dead(vector<float> &dlt_dm_senesced_dead) {dlt_dm_senesced_dead.push_back(dlt.dm_senesced_dead);}
void plantPart::get_n_green(vector<float> &n_green) {n_green.push_back(NGreen);}
void plantPart::get_n_senesced(vector<float> &n_senesced) {n_senesced.push_back(NSenesced);}
void plantPart::get_n_dead(vector<float> &n_dead) {n_dead.push_back(NDead);}
void plantPart::get_n_demanded(vector<float> &demands) {demands.push_back(NDemand);}
void plantPart::get_dlt_n_green(vector<float> &n_green) {n_green.push_back(dlt.n_green);}
void plantPart::get_dlt_n_dead(vector<float> &n_dead) {n_dead.push_back(dlt.n_dead);}
void plantPart::get_dlt_n_retrans(vector<float> &n_retrans) {n_retrans.push_back(dlt.n_retrans);}
void plantPart::get_dlt_n_senesced(vector<float> &n_senesced) {n_senesced.push_back(dlt.n_senesced);}
void plantPart::get_dlt_n_senesced_dead(vector<float> &dlt_n_senesced_dead) {dlt_n_senesced_dead.push_back(dlt.n_senesced_dead);}
void plantPart::get_dlt_n_senesced_retrans(vector<float> &n_senesced_retrans) {n_senesced_retrans.push_back(dlt.n_senesced_retrans);}
void plantPart::get_dlt_n_senesced_trans(vector<float> &n_senesced_trans) {n_senesced_trans.push_back(dlt.n_senesced_trans);}
void plantPart::get_dlt_n_detached(vector<float> &n_detached) {n_detached.push_back(dlt.n_detached);}
void plantPart::get_dlt_n_dead_detached(vector<float> &n_dead_detached) {n_dead_detached.push_back(dlt.n_dead_detached);}
void plantPart::get_p_dead(vector<float> &p_dead) {p_dead.push_back(PDead);}
void plantPart::get_p_sen(vector<float> &p_sen) {p_sen.push_back(PSen);}
void plantPart::get_dlt_p_detached(vector<float> &dlt_p_detached) {dlt_p_detached.push_back(dlt.p_det);}
void plantPart::get_dlt_p_dead(vector<float> &dlt_p_dead) {dlt_p_dead.push_back(dlt.p_dead);}
void plantPart::get_dlt_p_sen(vector<float> &dlt_p_sen) {dlt_p_sen.push_back(dlt.p_sen);}

   //needed to standardise interface for composite subclass

float plantPart::calcCover (float canopy_fac){return 0;}                  // return pod cover   //FIXME
float plantPart::coverDead(void) {return 0;}
float plantPart::coverGreen(void) {return 0;}
float plantPart::coverSen(void) {return 0;}
float plantPart::coverTotal(void) {return 0;}
float plantPart::dltDmGrainDemand(void) const {return 0;}
float plantPart::dltDmPotRue(void){return 0;}        //FIXME
float plantPart::dltDmPotTe(void){return 0;}            //FIXME
////float plantPart::dltDmRetranslocate(void){return 0;}
float plantPart::dmDeadVegTotal(void)const{return 0;}
float plantPart::dmGrainTotal(void) const{return 0;}
float plantPart::dmGreenGrainTotal(void) const{return 0;}
float plantPart::dmGreenVegTotal(void)const{return 0;}
float plantPart::dmSenescedVegTotal(void)const{return 0;}
float plantPart::dmVegTotal(void) const {return 0;}
float plantPart::grainNConcPercent(void) const {return 0;}
float plantPart::grainNo(void) const {return 0;}
float plantPart::grainWt(void) const {return 0;}
float plantPart::interceptRadiation(float radiation){return 0;}        //FIXME
float plantPart::nConcGrain(void)const{return 0;}
float plantPart::nDeadVegTotal(void) const{return 0;}
float plantPart::nDemandGrain(void) const {return 0;}
float plantPart::nDemandGrain2(void){return 0;}
float plantPart::nGrainTotal(void) const {return 0;}
float plantPart::nGreenGrainTotal(void)const{return 0;}
float plantPart::nGreenVegTotal(void)const{return 0;}
float plantPart::nSenescedVegTotal(void)const{return 0;}
float plantPart::nVegTotal(void)const{return 0;}
float plantPart::pConcGrain(void)const{return 0;}
float plantPart::pConcGrainTotal(void)const{return 0;}
float plantPart::pDeadGrainTotal(void)const{return 0;}
float plantPart::pDeadVegTotal(void)const{return 0;}
float plantPart::pGrainTotal(void)const{return 0;}
float plantPart::pGreenGrainTotal(void)const{return 0;}
float plantPart::pGreenVegTotal(void)const{return 0;}
float plantPart::pSenescedGrainTotal(void)const{return 0;}
float plantPart::pSenescedVegTotal(void)const{return 0;}
float plantPart::pVegTotal(void)const{return 0;}
float plantPart::SWDemand(void){return 0;}                           //(OUTPUT) crop water demand (mm)               //FIXME
void plantPart::calcDlt_pod_area (void){}   //FIXME
void plantPart::doBioActual (void){}
void plantPart::doDmDemand (float dlt_dm_supply_by_veg){}
void plantPart::doDmPotRUE (double  radn_int_pod){}                      //FIXME   // (OUTPUT) potential dry matter (carbohydrate) production (g/m^2)
void plantPart::doDmPotTE(void){}                                       //(OUTPUT) potential dry matter production by transpiration (g/m^2)//FIXME
void plantPart::doGrainNumber (void){}
void plantPart::doInit1(protocol::Component *system){}
void plantPart::doNDemandGrain(float nfact_grain_conc, float swdef_expansion){}
void plantPart::doNewMet(protocol::NewMetType &newmet) {}
void plantPart::doNInit (void){}
void plantPart::doTECO2(void){}                                       // (OUTPUT) transpiration coefficient                         //FIXME
void plantPart::doTick(protocol::TimeType &tick) {}
void plantPart::writeCultivarInfo (protocol::Component *){}

float plantPart::giveDmGreen(float delta)
//=======================================================================================
// giveXXX: something is giving us some XXX. return the amount we actually take.
   {
   dlt.dm_green += delta;
   return delta;
   }

float plantPart::giveNGreen(float delta)
//=======================================================================================
   {
   dlt.n_green += delta;
   return delta;
   }

float plantPart::giveDmSenesced (float delta)
//=======================================================================================
   {
   dlt.dm_senesced += delta;
   return delta;
   }

float plantPart::giveDmDead (float delta)
//=======================================================================================
   {
   dlt.dm_dead += delta;
   return delta;
   }

float plantPart::dlt_dm_green_retrans_hack(float delta)
   {
   dlt.dm_green_retrans = delta;
   return delta;
   }

float plantPart::giveDmGreenRemoved (float delta)
//=======================================================================================
// addXXX: something is removing some XXX. return the delta.
   {
   dlt.dm_green_removed = delta;
   return delta;
   }

float plantPart::giveDmSenescedRemoved (float delta)
//=======================================================================================
   {
   dlt.dm_senesced_removed = delta;
   return delta;
   }

float plantPart::giveDmDeadRemoved (float delta)
//=======================================================================================
   {
   dlt.dm_dead_removed = delta;
   return delta;
   }

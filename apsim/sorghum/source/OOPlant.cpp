
#pragma hdrstop
#include <math.h>

#include <vector>
#include <general/date_class.h>
#include <ComponentInterface2/ScienceAPI.h>
#include <ComponentInterface2/DataTypes.h>
#include <ComponentInterface2/Variant.h>

#include "OOPlantComponents.h"
#include "OOPlant.h"
#include "OOPlantInterface.h"

using namespace std;

//------------------------------------------------------------------------------------------------
//------------- Plant Constructor
//------------------------------------------------------------------------------------------------
OOPlant::OOPlant(ScienceAPI &api) : scienceAPI(api)
   {
   initialize();
   }
//------------------------------------------------------------------------------------------------
void OOPlant::initialize(void)
   {
   dltPlants = 0.0;
   dltDeadPlants = 0.0;
   frIntcRadn = 0.0;
   eo = 0.0;
   stage = 0.0;
   das = 0;
   tempStress = 0.0;
   }
//------------------------------------------------------------------------------------------------
//------------ read the crop and cultivar parameters
//------------------------------------------------------------------------------------------------
void OOPlant::readParams(void)
   {
   scienceAPI.write(string(" - reading constants for " +
                           cropClass + "(" + cropType +") - " + cultivar + "\n"));

   tempStressTable.read(scienceAPI, "x_ave_temp","y_stress_photo");


   scienceAPI.read("rue",   "",   0, rue,  0.0f,   10.0f);
   rue.insert(rue.begin(),0);  // for compatibility with fortran

   scienceAPI.read("transp_eff_cf", "", 0, transpEffCf, 0.0f, 0.1f);
   transpEffCf.insert(transpEffCf.begin(),0);  // for compatibility with fortran
   
   scienceAPI.read("svp_fract","", 0, svpFract, 0.0f, 1.0f);

   scienceAPI.read("tt_emerg_limit", "", 0, ttEmergeLimit);

   //Read arrays for detachment
   if(!scienceAPI.read("sen_detach_frac", "", true, senDetachFrac))
      {
      for(int i = 0; i < 5; i++)
         {
         senDetachFrac.push_back(0.0);
         }
      }

   scienceAPI.read("dead_detach_frac", "", 0, deadDetachFrac);
   }

//------------------------------------------------------------------------------------------------
//------------- Plant Destructor
//------------------------------------------------------------------------------------------------
OOPlant::~OOPlant()
   {
   delete roots;
   delete leaf;
   delete stem;
   delete rachis;
   delete grain;
   
   delete phenology;
   delete nitrogen;
   delete phosphorus;
   delete water;
   delete biomass;
   }
//------------------------------------------------------------------------------------------------
//------------- Initialise plant
//------------------------------------------------------------------------------------------------
void OOPlant::plantInit1(void)
  {
   // parameters
   // initialise any variable that is needed before the crop is planted
   vector<string> sections;
   sections.push_back("genetics");
   scienceAPI.setSearchOrder(sections);

   scienceAPI.read ("crop_type", "",0, cropType);
   scienceAPI.read ("default_crop_class", "", 0, defaultCropClass);

   roots     = new Roots(scienceAPI, this);   PlantComponents.push_back(roots); PlantParts.push_back(roots);
   leaf      = new Leaf(scienceAPI, this);    PlantComponents.push_back(leaf);  PlantParts.push_back(leaf);
   stem      = new Stem(scienceAPI, this);    PlantComponents.push_back(stem);  PlantParts.push_back(stem);
   rachis    = new Rachis(scienceAPI, this);  PlantComponents.push_back(rachis);PlantParts.push_back(rachis);
   grain     = new Grain(scienceAPI, this);   PlantComponents.push_back(grain); PlantParts.push_back(grain);

   phenology = new Phenology(scienceAPI, this); PlantComponents.push_back(phenology);
                                    PlantProcesses.push_back(phenology);
   nitrogen  = new Nitrogen(scienceAPI, this);  PlantComponents.push_back(nitrogen);
                                    PlantProcesses.push_back(nitrogen);
   phosphorus  = new Phosphorus(scienceAPI, this);  PlantComponents.push_back(phosphorus);
                                    PlantProcesses.push_back(phosphorus);
   water     = new Water(scienceAPI, this);     PlantComponents.push_back(water);
                                    PlantProcesses.push_back(water);
   biomass   = new Biomass(scienceAPI, this);   PlantComponents.push_back(biomass);
                                    PlantProcesses.push_back(biomass);

   doRegistrations();

   setStatus(out);

   scienceAPI.read("row_spacing_default", "", 0, rowSpacingDefault);
   
   // CO2 stuff
   co2_te_modifier.read(scienceAPI, "x_co2_te_modifier", "y_co2_te_modifier");
  }
//------------------------------------------------------------------------------------------------
void OOPlant::plantInit2(void)
  {
  }

//------------------------------------------------------------------------------------------------
void OOPlant::setStatus(Status status)
   {
   plantStatus = status;
   char statusStrings[3][6] = {"out", "dead", "alive"};
   statusString = statusStrings[status];
   }
//------------------------------------------------------------------------------------------------
//------------------- Field a Sow event
//------------------------------------------------------------------------------------------------
void OOPlant::onSowCrop(Variant &sowLine)
   {
   if(plantStatus != out)
      throw std::runtime_error("Crop is still in the ground -\n unable to sow until it is\n taken out by \"end_crop\" action.");

   scienceAPI.write("Sowing initiate\n");

   string temp;
   if (get(sowLine,"crop_class", temp) == false)
      cropClass = defaultCropClass;
   else
      cropClass = temp;

   if (get(sowLine, "cultivar", temp) == false)
      throw std::runtime_error("Cultivar not specified");
   else
      cultivar = temp;

   if (get(sowLine, "plants", plantDensity) == false)
      throw std::runtime_error("plant density ('plants') not specified");

   checkRange(scienceAPI, plantDensity, 0.0, 1000.0, "plants");

   if (get(sowLine, "sowing_depth", sowingDepth) == false)
      throw std::runtime_error("sowing depth not specified");

   checkRange(scienceAPI,sowingDepth, 0.0, 100.0, "sowing_depth");

   if (get(sowLine, "row_spacing", rowSpacing) == false)
      rowSpacing = rowSpacingDefault; // metres!!!!!!
   checkRange(scienceAPI,rowSpacing, 0.0, 100.0, "row_spacing");

   if (get(sowLine, "tiller_no_fertile", ftn) == false)
      throw std::runtime_error("tiller_no_fertile");

   checkRange(scienceAPI,ftn, 0.0, 10.0, "tiller_no_fertile");

   skipRow = 1.0;
   if (get(sowLine, "skip", temp) )
      {
      if (temp == "single")skipRow = 1.5;
      else if (temp == "double")skipRow = 2.0;
      else if (temp == "solid")skipRow = 1.0;
      else
        throw std::runtime_error("Unknown skip row configuration '" + temp + "'");
      }

   checkRange(scienceAPI,skipRow, 0.0, 2.0, "skiprow");

   phenology->setStage(sowing);
   setStatus(alive);
   
   char msg[120];
   sprintf(msg, "\n");   scienceAPI.write(msg);
   sprintf(msg, "                 Crop Sowing Data\n");   scienceAPI.write(msg);
   sprintf(msg, "    -------------------------------------------------------\n");   scienceAPI.write(msg);
   sprintf(msg, "    Sowing   Depth  Plants Spacing Skiprow Cultivar    FTN\n");   scienceAPI.write(msg);
   sprintf(msg, "    Day no    mm      m^2    m      code     name       no\n");   scienceAPI.write(msg);
   sprintf(msg, "    -------------------------------------------------------\n");   scienceAPI.write(msg);

   sprintf(msg, "   %7d%7.1f%7.1f%7.1f%9.1f     %s%8.2f\n",
               today.doy, sowingDepth, plantDensity, rowSpacing,
               skipRow, cultivar.c_str(), ftn);   scienceAPI.write(msg);

   scienceAPI.write("    -------------------------------------------------------\n");
   scienceAPI.write("\n");

   // Set up which sections to look for parameters
   vector<string> sections;
   sections.push_back(cultivar);
   scienceAPI.setSearchOrder(sections);

   // now we have the cultivar, get all 'constants' and cultivar parameters.
   readParams(); 

   for(unsigned i=0;i < PlantComponents.size();i++) 
     PlantComponents[i]->readParams (cultivar);

   scienceAPI.publish("sowing");
   }
//------------------------------------------------------------------------------------------------
//------------------- Field a Prepare event
//------------------------------------------------------------------------------------------------
void OOPlant::prepare (void)
   {
   if (!scienceAPI.get("co2", "mg/kg", true, co2, 300.0f, 1000.0f))
       co2 = 350.0;

   tempStress = tempStressTable.value(today.avgT);

   radnIntercepted = radnInt();

   float rueToday = rue[(int) stage] * rue_co2_modifier();

   biomass->calcBiomassRUE(rueToday,radnIntercepted);
   transpEff = transpEfficiency();
   water->calcDemand();

      // at beginning of day, reset the daily dlt variables
   for(unsigned i=0;i < PlantParts.size();i++)
      {
      PlantParts[i]->resetDailyVars();
      }

   if(phosphorus->Active())
       phosphorus->prepare();


   }
//------------------------------------------------------------------------------------------------
//------------------- Field a process event
//------------------------------------------------------------------------------------------------
void OOPlant::process (void)                 // do crop preparation
   {

   stage = phenology->currentStage();

   water->getOtherVariables();
   water->calcDailySupply();
   water->calcStresses();
   water->calcUptake();

   stem->calcCanopyHeight();

   leaf->calcLeafNo();

   phenology->development();

   leaf->calcPotentialArea();

    biomass->calcBiomassTE();

   biomass->calcDltBiomass();

   // calculate grain biomass demand
   if(stage >= startGrainFill && stage <= endGrainFill)
      {
      grain->calcDemandStress();
      grain->calcBiomassDemand();
      }

   // biomass partitioning
   biomass->calcPartitioning();

   // biomass retranslocation
   if(stage >= startGrainFill && stage <= endGrainFill)
      biomass->calcRetranslocation();

   // root length
   if(stage > germination)
      {
      roots->calcRootDistribution();
      }
   // actual dltLai C limited
   leaf->areaActual();

   // scenescence
   leaf->senesceArea();
   biomass->dmScenescence();
   if(stage > germination)
      {
      roots->calcSenLength();
      }
   // nitrogen
   nitrogen->process();

   if(phosphorus->Active())
       phosphorus->process();

   death();

   //Calculate detachment
   detachment();
   //Cleanup plant process
   cleanup();


   // at end of day, update class state variables
   for(unsigned i=0;i < PlantComponents.size();i++)
      {
      PlantComponents[i]->updateVars();
      }
//   phenology->development();
   updateVars();
   }
//------------------------------------------------------------------------------------------------
void OOPlant::updateVars(void)
   {
   das++;

   stage = phenology->currentStage();
   }
//------------------------------------------------------------------------------------------------
void OOPlant::death(void)
   {
   // Emergence
   float ttEmergeLimit;
   scienceAPI.read("tt_emerg_limit", "", 0, ttEmergeLimit);


   if(phenology->currentStage() >= germination && 
      phenology->currentStage() < emergence &&
      phenology->sumDaysTotal(germination,harvest) > ttEmergeLimit)
      {
      scienceAPI.write(" failed emergence due to deep planting\n");
      dltDeadPlants = -plantDensity;
      return;
      }


   //If leaves are killed from frost, leaf->dltSlai is set to leaf->lai
   //need to kill plant if lai = 0
   //gmc & rlv

   if (phenology->currentStage() >= flag && phenology->currentStage() < maturity)
      {
      if ((leaf->getDltSlai() >= leaf->getLAI()) ||
               ((leaf->getLAI() - leaf->getDltSlai()) < 0.1))
         {
         dltDeadPlants = -plantDensity;
         //Maybe should write a message ??
         }
      }

   //Check to see if plant death should terminate crop
   if(plantDensity + dltDeadPlants < 0.01)
      {
      killCrop();
      }
   }
//------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------
//- Science --------------------------------------------------------------------------------------
//----------  Radiation intercepted by leaves (mj/m^2)
//------------------------------------------------------------------------------------------------
float OOPlant::radnInt(void)
   {
   if (isEqual(frIntcRadn,0.0))return leaf->getCoverGreen() * today.radn;
   else
      // interception has already been calculated for us
      return frIntcRadn * today.radn;
   }
////------------------------------------------------------------------------------------------------
////------------------------------------------------------------------------------------------------
//---   Calculate today's transpiration efficiency from the transpiration efficiency coefficient
//---   and vapour pressure deficit, which is calculated from min and max temperatures.
//------------------------------------------------------------------------------------------------
float OOPlant::transpEfficiency(void)
   {
   // get vapour pressure deficit when net radiation is positive.
   vpd = Max(svpFract * (svp(today.maxT) - svp(today.minT)), 0.01);

   return divide (transpEffCf[int (stage)], vpd, 0.0) / g2mm;
   }
//------------------------------------------------------------------------------------------------
//-------- function to get saturation vapour pressure for a given temperature in oC (kpa)
//------------------------------------------------------------------------------------------------
float OOPlant::svp(float temp)
   {
   return 6.1078 * exp(17.269 * temp / (237.3 + temp)) * mb2kpa;
   }
//------------------------------------------------------------------------------------------------
//-------- function to calculate detachment
//------------------------------------------------------------------------------------------------
void OOPlant::detachment(void)
   {
   biomass->detachment(senDetachFrac, deadDetachFrac);
   leaf->laiDetachment(senDetachFrac, deadDetachFrac);
   nitrogen->detachment(senDetachFrac, deadDetachFrac);
   }
//------------------------------------------------------------------------------------------------
//-------- Cleanup Plant process
//------------------------------------------------------------------------------------------------
void OOPlant::cleanup(void)
   {
   //Could not find a definition
   }
//------------------------------------------------------------------------------------------------
//-------- Kill the crop
//------------------------------------------------------------------------------------------------

void OOPlant::killCrop(void)
   {
   if (plantStatus == alive)
      {
      setStatus(dead);
      }

   //Report
   char msg[120];
   sprintf(msg, "Plant Death. Standing above-ground dm = %.2f (kg/ha)\n", 
           biomass->getAboveGroundBiomass());
   scienceAPI.write(msg);
   }
//------------------------------------------------------------------------------------------------
void OOPlant::phenologyEvent(int iStage)
   {
   char msg[120];   
   sprintf(msg,"\t\tbiomass = %6.2f \t\t lai = %6.2f\n",
      biomass->getAboveGroundBiomass() / 10.0, leaf->getLAI());
   scienceAPI.write(msg);
   sprintf(msg,"\t\tstover N conc = %6.2f \t\t extractable sw = %6.2f\n",
      nitrogen->getNStover(),water->getESWAvail());
   scienceAPI.write(msg);

   // output the current stage
   // Don't send an end crop to the system - otherwise all the other crops will stop too!
   string stage = phenology->returnStageName();
   scienceAPI.publish(stage);
   }

void OOPlant::get_cover_green(float &result)
   {
   result = leaf->getCoverGreen();
   }
void OOPlant::get_cover_tot(float &result)
   {
   result = leaf->getCoverTot();
   }
void OOPlant::get_height(float &result)
   {
   result = stem->getCanopyHeight();
   }


float OOPlant::rue_co2_modifier(void)                 //!CO2 level (ppm)
/*  Purpose
*     Calculation of the CO2 modification on rue
*/
   {
   const float scale = 1.0 / 350.0 * 0.05;
   return (scale * this->co2 + 0.95); //Mark Howden, personal communication
   }



//#include <general/pch.h>                                                

#pragma hdrstop
#include <math.h >

#include "OOPlantComponents.h"
#include "OOPlant.h"
#include "OOPlantInterface.h"
#include "TypeKind.h"
#include <vector>
#include <ComponentInterface/dataTypes.h>

using namespace std;

//------------------------------------------------------------------------------------------------
//------------- Plant Constructor
//------------------------------------------------------------------------------------------------
OOPlant::OOPlant(PlantInterface *P)
   {
   plantInterface = P;
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

   vector<string> sections;                  // sections to look for parameters
   sections.push_back("constants");
   sections.push_back(cultivar);

   plantInterface->writeString (string(" - reading constants for " +
                  cropClass + "(" + cropType +") - " + cultivar).c_str());

   tempStressTable.read(plantInterface,sections,"x_ave_temp","y_stress_photo");


   readArray(plantInterface,sections,"rue",rue);
   rue.insert(rue.begin(),0);  // for compatibility with fortran

   readArray(plantInterface,sections,"transp_eff_cf",transpEffCf);
   transpEffCf.insert(transpEffCf.begin(),0);  // for compatibility with fortran
   
   readVar(plantInterface,sections,"svp_fract",svpFract);

   ttEmergeLimit = readVar(plantInterface,sections,"tt_emerg_limit");

   //Read arrays for detachment
   if(!readArray(plantInterface, sections, "sen_detach_frac", senDetachFrac))
      {
      for(int i = 0; i < 5; i++)
         {
         senDetachFrac.push_back(0.0);
         }
      }
//   readArray(plantInterface, sections, "sen_detach_frac", senDetachFrac);

   readArray(plantInterface, sections, "dead_detach_frac", deadDetachFrac);

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


 //  for (VariableMap::iterator m = vMap.begin();m != vMap.end();m++)
 //     {
 //     delete (*m).second;
 //     }
   }
//------------------------------------------------------------------------------------------------
//------------- Initialise plant
//------------------------------------------------------------------------------------------------
void OOPlant::plantInit(void)
  {
   // parameters
   // initialise any variable that is needed before the crop is planted
   vector<string> sections;
   sections.push_back("constants");
   sections.push_back("genetics");

   readVar (plantInterface,sections[0],"crop_type", "()",cropType);
   readVar (plantInterface,sections[0], "default_crop_class", "()",defaultCropClass);



   roots     = new Roots(this);   PlantComponents.push_back(roots); PlantParts.push_back(roots);
   leaf      = new Leaf(this);    PlantComponents.push_back(leaf);  PlantParts.push_back(leaf);
   stem      = new Stem(this);    PlantComponents.push_back(stem);  PlantParts.push_back(stem);
   rachis    = new Rachis(this);  PlantComponents.push_back(rachis);PlantParts.push_back(rachis);
   grain     = new Grain(this);   PlantComponents.push_back(grain); PlantParts.push_back(grain);

   phenology = new Phenology(this); PlantComponents.push_back(phenology);
                                    PlantProcesses.push_back(phenology);
   nitrogen  = new Nitrogen(this);  PlantComponents.push_back(nitrogen);
                                    PlantProcesses.push_back(nitrogen);
   phosphorus  = new Phosphorus(this);  PlantComponents.push_back(phosphorus);
                                    PlantProcesses.push_back(phosphorus);
   water     = new Water(this);     PlantComponents.push_back(water);
                                    PlantProcesses.push_back(water);
   biomass   = new Biomass(this);   PlantComponents.push_back(biomass);
                                    PlantProcesses.push_back(biomass);

   doRegistrations();

   setStatus(out);

   rowSpacingDefault = readVar (plantInterface,sections,"row_spacing_default" );

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
void OOPlant::sowCrop(unsigned &, unsigned &, protocol::Variant &v)
   {
   if(plantStatus != out)
      {
      string msg = string(
         "Crop is still in the ground -\n unable to sow until it is\n taken out by \"end_crop\" action.");
      //plantInterface->error(msg.c_str(),fatal);
      throw std::runtime_error(msg.c_str());
      }

   protocol::ApsimVariant sowLine(plantInterface);
   sowLine.aliasTo(v.getMessageData());
   plantInterface->writeString ("Sowing initiate");
   FString temp;                                   // ?????

   if (sowLine.get("crop_class", protocol::DTstring, false, temp) == false)
      cropClass = defaultCropClass;
   else
      {
      cropClass = temp.f_str();
      cropClass = cropClass.substr(0,temp.length());
/* TODO : Find out what all this shit is about */
      }

   if (sowLine.get("cultivar", protocol::DTstring, false, temp) == false)
      //plantInterface->error("Cultivar not specified",fatal);
      throw std::runtime_error("Cultivar not specified");
   else
      {
      cultivar = temp.substr(0,temp.length()).f_str();
      cultivar = cultivar.substr(0,temp.length());
      }

   if (sowLine.get("plants", protocol::DTsingle, false, plantDensity) == false)
      {
      //plantInterface->error("plant density ('plants') not specified",fatal);
      throw std::runtime_error("plant density ('plants') not specified");
      }
   checkRange(plantInterface,plantDensity, 0.0, 1000.0, "plants");

   if (sowLine.get("sowing_depth", protocol::DTsingle, false, sowingDepth) == false)
      {
      //plantInterface->error("sowing depth not specified",fatal);
      throw std::runtime_error("sowing depth not specified");
      }
   checkRange(plantInterface,sowingDepth, 0.0, 100.0, "sowing_depth");

   if (sowLine.get("row_spacing", protocol::DTsingle, false, rowSpacing) == false)
      {
      rowSpacing = rowSpacingDefault; // metres!!!!!!
      }
   checkRange(plantInterface,rowSpacing, 0.0, 100.0, "row_spacing");

   if (sowLine.get("tiller_no_fertile", protocol::DTsingle, false, ftn) == false)
      {
      //plantInterface->error("tiller_no_fertile",fatal);
      throw std::runtime_error("tiller_no_fertile");
      }
   checkRange(plantInterface,ftn, 0.0, 10.0, "tiller_no_fertile");

   skipRow = 1.0;
   if (sowLine.get("skip", protocol::DTstring, false, temp) )
      {
      string skip = temp.substr(0,temp.length()).f_str();
      skip = skip.substr(0,temp.length());
      if (skip == "single")skipRow = 1.5;
      else if (skip == "double")skipRow = 2.0;
      else if (skip == "solid")skipRow = 1.0;
      else
        throw std::runtime_error("Unknown skip row configuration '" + skip + "'");
      }
   checkRange(plantInterface,skipRow, 0.0, 2.0, "skiprow");

   phenology->setStage(sowing);
   setStatus(alive);


   plantInterface->writeString ("");
   plantInterface->writeString ("                 Crop Sowing Data");
   plantInterface->writeString ("    -------------------------------------------------------");
   plantInterface->writeString ("    Sowing   Depth  Plants Spacing Skiprow Cultivar    FTN");
   plantInterface->writeString ("    Day no    mm      m^2    m      code     name       no");
   plantInterface->writeString ("    -------------------------------------------------------");

   char msg[100];
   sprintf(msg, "   %7d%7.1f%7.1f%7.1f%9.1f     %s%8.2f",
               today.todayDate.doy, sowingDepth, plantDensity, rowSpacing,
               skipRow, cultivar.c_str(),ftn);
   plantInterface->writeString (msg);

   plantInterface->writeString ("    -------------------------------------------------------\n");


   readParams(); // now we have the cultivar, get all 'constants' and cultivar parameters.

   for(unsigned i=0;i < PlantComponents.size();i++) PlantComponents[i]->readParams (cultivar);

   unsigned int id = plantInterface->addRegistration(RegistrationType::event,"sowing", "", "", "");
   protocol::ApsimVariant outgoingApsimVariant(plantInterface);
   plantInterface->publish (id, outgoingApsimVariant);

   }
//------------------------------------------------------------------------------------------------
//------------------- Field a Prepare event
//------------------------------------------------------------------------------------------------
void OOPlant::prepare (void)
   {
   tempStress = tempStressTable.value(today.avgT);

   radnIntercepted = radnInt();

   float rueToday = rue[(int) stage];

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
   vector<string> sections;
   sections.push_back("constants");
   sections.push_back(cultivar);
   float killfr = 0.0;

   // Emergence
   float ttEmergeLimit = readVar(plantInterface,sections,"tt_emerg_limit");


   if(phenology->currentStage() >= germination && phenology->currentStage() < emergence &&
         phenology->sumDaysTotal(germination,harvest) > ttEmergeLimit)
      {
      plantInterface->writeString (" failed emergence due to deep planting");
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
   float AGBiomass;
   if (plantStatus == alive)
      {
      setStatus(dead);
      }

   AGBiomass = biomass->getAboveGroundBiomass();

   //Report
   string msg= "Plant Death. Standing above-ground dm = ";
      msg = msg + ftoa(AGBiomass, 2).c_str();
      msg = msg + " (kg/ha)";
   plantInterface->writeString (msg.c_str());
//   for(unsigned i=0;i < PlantComponents.size();i++) PlantComponents[i]->initialize ();

   }
//------------------------------------------------------------------------------------------------
void OOPlant::phenologyEvent(int iStage)
   {
   //  report

   summaryLine(plantInterface,"\t\tbiomass = %6.2f \t\t lai = %6.2f",
      biomass->getAboveGroundBiomass() / 10.0, leaf->getLAI());
   summaryLine(plantInterface,"\t\tstover N conc = %6.2f \t\t extractable sw = %6.2f",
      nitrogen->getNStover(),water->getESWAvail());

   // output the current stage
   // Don't send an end crop to the system - otherwise all the other crops will stop too!
   string stage = phenology->returnStageName();
   unsigned int id = plantInterface->addRegistration(RegistrationType::event,stage.c_str(), "", "", "");
   protocol::ApsimVariant outgoingApsimVariant(plantInterface);
   plantInterface->publish (id, outgoingApsimVariant);

   }
//------------------------------------------------------------------------------------------------

void OOPlant::onApsimGetQuery(protocol::ApsimGetQueryData& apsimQueryData)
   {
   string name = string(apsimQueryData.name.f_str(),apsimQueryData.name.length());
   boost::function2<void, protocol::Component *, protocol::QueryValueData &> fn;
   if (name == string("crop_type"))
      {

      fn = boost::bind(&OOPlant::get_crop_type, this, _1, _2);
      plantInterface->addGettableVar("crop_type", protocol::DTstring, false,fn, "",  "");
      }
   else if (name == string("cover_green"))
      {
      fn = boost::bind(&OOPlant::get_cover_green, this, _1, _2);
      plantInterface->addGettableVar("cover_green", protocol::DTsingle, false,fn, "",  "");
      }
   else if (name == string("cover_tot"))
      {
      fn = boost::bind(&OOPlant::get_cover_tot, this, _1, _2);
      plantInterface->addGettableVar("cover_tot", protocol::DTsingle, false,fn, "",  "");
      }
   else if (name == string("height"))
      {
      fn = boost::bind(&OOPlant::get_height, this, _1, _2);
      plantInterface->addGettableVar("height", protocol::DTsingle, false,fn, "mm",  "");
      }
   }


void OOPlant::get_crop_type(protocol::Component *system, protocol::QueryValueData &qd)
   {
   system->sendVariable(qd, FString(cropType.c_str()));
   }
void OOPlant::get_cover_green(protocol::Component *system, protocol::QueryValueData &qd)
   {
   float coverGreen = leaf->getCoverGreen();
   system->sendVariable(qd, coverGreen);
   }
void OOPlant::get_cover_tot(protocol::Component *system, protocol::QueryValueData &qd)
   {
   float cover_tot = leaf->getCoverTot();
   system->sendVariable(qd, cover_tot);
   }
void OOPlant::get_height(protocol::Component *system, protocol::QueryValueData &qd)
   {
   float height = stem->getCanopyHeight();
   system->sendVariable(qd, height);
   }



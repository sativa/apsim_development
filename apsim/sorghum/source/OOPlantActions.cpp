//------------------------------------------------------------------------------------------------

#pragma hdrstop

#include <stdio.h>
#include <math.h>
#include <map>
#include <string>
#include <stdexcept>
#include <strstream>

#include "OOPlant.h"
#include "OOPlantComponents.h"
#include "OOPlantActions.h"
#include "OOPhosphorus.h"

//------------------------------------------------------------------------------------------------
// Register Methods, Events,
//------------------------------------------------------------------------------------------------
void OOPlant::doRegistrations(void)
   {
   scienceAPI.subscribe("prepare",     nullFunction(&OOPlant::onPrepare));
   scienceAPI.subscribe("process",     nullFunction(&OOPlant::onProcess));
   scienceAPI.subscribe("tick",        TimeFunction(&OOPlant::onTick));
   scienceAPI.subscribe("newmet",      NewMetFunction(&OOPlant::onNewMet));
   scienceAPI.subscribe("new_profile", NewProfileFunction(&OOPlant::onNewProfile));
   scienceAPI.subscribe("sow",         VariantFunction(&OOPlant::onSowCrop)); 
   scienceAPI.subscribe("harvest",     nullFunction(&OOPlant::onHarvest));
   scienceAPI.subscribe("end_crop",    nullFunction(&OOPlant::onEndCrop));
   scienceAPI.subscribe("kill_crop",   nullFunction(&OOPlant::onKillCrop));
   scienceAPI.subscribe("end_run",     nullFunction(&OOPlant::onEndRun));

   // --------------------------------------------------------------------------------------------
   // Variables available to other modules on request (e.g. report)
   scienceAPI.expose("crop_type",         "",             "Crop species",                     0, cropType         );
   scienceAPI.expose("crop_class",        "",             "Crop class",                       0, cropClass        );
   scienceAPI.expose("das",               "days",         "Days after sowing",                0, das              );
   scienceAPI.expose("radn_int",          "",             "",                                 0, radnIntercepted  );
   scienceAPI.expose("temp_stress",       "",             "",                                 0, tempStress       ); 
   scienceAPI.expose("plants",            "plants/m2",    "Plant density",                    0, plantDensity     );
   scienceAPI.expose("tiller_no",         "tillers/plant","No of tillers on main stem",       0, ftn              );
   scienceAPI.expose("tiller_no_fertile", "tillers/plant","No of tillers that produce a head",0, ftn              );
   scienceAPI.expose("vpd",               "",             "Vapour pressure deficit",          0, vpd              );
   scienceAPI.expose("transp_eff",        "g/m2",         "Transpiration efficiency",         0, transpEff        );

   scienceAPI.exposeFunction("plant_status", "",   "Status of crop", StringFunction(&OOPlant::getPlantStatus));
   scienceAPI.exposeFunction("height",       "mm", "Height of crop", FloatFunction(&OOPlant::get_height));
   scienceAPI.exposeFunction("cover_green",  "",   "Green cover",    FloatFunction(&OOPlant::get_cover_green));
   scienceAPI.exposeFunction("cover_tot",    "",   "Total cover",    FloatFunction(&OOPlant::get_cover_tot));

   }
void OOPlant::onPrepare(void)
   {
   if (plantStatus == out)
      {
      // reset variables
      initialize();
      for(unsigned i=0;i < PlantComponents.size();i++)
         {
         PlantComponents[i]->initialize ();
         }
      }
   else if (plantStatus == alive)
      {
      getOtherVariables (); // sw etc..
      prepare ();                 // do crop preparation
      }
   }
//------------------------------------------------------------------------------------------------
//-------- Field a Process message
//------------------------------------------------------------------------------------------------
void OOPlant::onProcess(void)
   {
   if (plantStatus == alive)
      {
      getOtherVariables (); // sw etc..
/* TODO : Is this needed? called in prepare */
      process ();               // do crop processes
      }
   }
//------------------------------------------------------------------------------------------------
//-----------------   Field a Tick event
//------------------------------------------------------------------------------------------------
void OOPlant::onTick(TimeType &tick)
   {
   JulianToCalendar((float)tick.startday,today.day,today.month,today.year);
   today.doy = (int) (tick.startday - CalendarToJulian(1,1,today.year) + 1);
   }
//------------------------------------------------------------------------------------------------
//-----------------   Field a Kill event
//------------------------------------------------------------------------------------------------
void OOPlant::onKillCrop(void)
   {
   scienceAPI.write("Kill Crop\n");

   if(plantStatus == alive)
      {
      setStatus(dead);
      char msg[120];
      sprintf(msg,"Crop kill. Standing above-ground dm = %7.1f kg/ha\n",
              biomass->getAboveGroundBiomass());
      scienceAPI.write(msg);
      }
   }
//------------------------------------------------------------------------------------------------
//-----------------   Field a NewMet event
//------------------------------------------------------------------------------------------------
void OOPlant::onNewMet(NewMetType &newmet)
   {
   today.radn = newmet.radn;
   today.maxT = newmet.maxt;
   today.minT = newmet.mint;
   today.avgT = (today.maxT + today.minT) / 2.0;
   today.rain = newmet.rain;
   today.vp   = newmet.vp;
   }
//------------------------------------------------------------------------------------------------
//-----------------  Field a NewProfile event -------
//------------------------------------------------------------------------------------------------
void OOPlant::onNewProfile(NewProfileType &v)
   {
   roots->onNewProfile(v);
   water->onNewProfile(v);
   nitrogen->onNewProfile(v);
   }
//------------------------------------------------------------------------------------------------
//-----------------   respondToMethodCall
//-----------------   Harvest
//------------------------------------------------------------------------------------------------
void OOPlant::onHarvest(void)     // Field a Harvest event
   {
   scienceAPI.write("\n");
   scienceAPI.write("Harvest\n");

   phenology->Summary();
   leaf->Summary();
   biomass->Summary();
   grain->Summary();
   nitrogen->Summary();
   if(phosphorus->Active())
      phosphorus->Summary();

   scienceAPI.write("\n\n");
   
   // stress - not done yet
   char msg[120];
   sprintf(msg,"Average Stress Indices:                          Water Photo  Water Expan  N Photo      N grain conc\n"); scienceAPI.write(msg);
   sprintf(msg,"   emergence           to end_of_juvenile           N/A          N/A        N/A          N/A        \n"); scienceAPI.write(msg);
   sprintf(msg,"   end_of_juvenile     to floral_initiation         N/A          N/A        N/A          N/A        \n"); scienceAPI.write(msg);
   sprintf(msg,"   floral_initiation   to flag_leaf                 N/A          N/A        N/A          N/A        \n"); scienceAPI.write(msg);
   sprintf(msg,"   flag_leaf           to flowering                 N/A          N/A        N/A          N/A        \n"); scienceAPI.write(msg);
   sprintf(msg,"   flowering           to start_grain_fill          N/A          N/A        N/A          N/A        \n"); scienceAPI.write(msg);
   sprintf(msg,"   start_grain_fill    to end_grain_fill            N/A          N/A        N/A          N/A        \n"); scienceAPI.write(msg);

   sprintf(msg,"\n"); scienceAPI.write(msg);
   sprintf(msg,"Crop harvested.\n"); scienceAPI.write(msg);
   sprintf(msg,"   Organic matter removed from system:-      From Tops\t\tFrom Roots\n"); scienceAPI.write(msg);
   sprintf(msg,"                    DM (kg/ha) =              %8.2f\t\t%8.2f\n",
                 grain->getDmGreen() * 10,0); scienceAPI.write(msg);
   sprintf(msg,"                    N  (kg/ha) =              %8.2f\t\t%8.2f\n",
                 grain->getNGreen() * 10,0); scienceAPI.write(msg);
   
   scienceAPI.publish("harvesting");

   grain->Harvest();
   biomass->Update();
   }
//------------------------------------------------------------------------------------------------
//-----------------   end run
//------------------------------------------------------------------------------------------------
void OOPlant::onEndRun(void)  // Field a end run event
  {
  scienceAPI.write("End Run\n");
  }

//------------------------------------------------------------------------------------------------
//--------------------------  getOtherVariables   from other modules
//------------------------------------------------------------------------------------------------
void OOPlant::getOtherVariables (void)
   {
   // Canopy
//   plantInterface->getVariable(frIntcRadnID, frIntcRadn, 0.0, 1.0, true);
   }
//------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------
//-----------------   end crop
//------------------------------------------------------------------------------------------------
void OOPlant::onEndCrop(void)     // Field a End crop event
   {
   if (plantStatus == out)
      {
      string message = cropType + " is not in the ground - unable to end crop.";
      return;
      //plantInterface->error(message.c_str(), fatal);
      //throw std::runtime_error(message.c_str());
      }

   setStatus(out);
   phenology->setStage(endCrop);

   //Report the crop yield
   char msg[120];
   float yield = grain->getDmGreen() + grain->getDmDead() * gm2kg /sm2ha;

   sprintf(msg, "Crop ended. Yield (dw) = %7.1f kg/ha\n",yield * 10);
   scienceAPI.write(msg);

   sprintf(msg, "Organic matter from crop:-      Tops to surface residue\t Roots to soil FOM\n");
   scienceAPI.write(msg);
   sprintf(msg, "                    DM (kg/ha) =              %8.2f\t\t%8.2f\n",
           biomass->getAboveGroundBiomass() - grain->getDmGreen() * 10.0,roots->getDmGreen() * 10.0);
   scienceAPI.write(msg);
   sprintf(msg, "                    N  (kg/ha) =              %8.2f\t\t%8.2f\n",
           (leaf->getNGreen() + stem->getNGreen()) * 10.0,roots->getNGreen() * 10);
   scienceAPI.write(msg);
   if(phosphorus->Active())
      {
      sprintf(msg, "                    P  (kg/ha) =              %8.2f\t\t%8.2f\n",
              (leaf->getPGreen() + stem->getPGreen()) * 10.0,roots->getPGreen() * 10);
      scienceAPI.write(msg);
      }
   else
      {
      sprintf(msg,"                    P  (kg/ha) =              %8.2f\t\t%8.2f\n",0,0);
      scienceAPI.write(msg);
      }

   roots->incorporateResidue();
   biomass->incorporateResidue();
   for(unsigned i=0;i < PlantParts.size();i++) PlantParts[i]->initialize ();
   biomass->Harvest();
   biomass->Update();
   }
//------------------------------------------------------------------------------------------------
void OOPlant::getPlantStatus(string &result)
   {
   result = statusString;
   }
//------------------------------------------------------------------------------------------------


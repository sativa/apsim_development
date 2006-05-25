//------------------------------------------------------------------------------------------------

#pragma hdrstop

#include <stdio.h>
#include <math.h>
#include <map>
#include <string>
#include <stdexcept>
#include <strstream>

#include <boost/function.hpp>
#include <boost/bind.hpp>

#include <ComponentInterface/Type.h>
#include <ComponentInterface/ApsimVariant.h>
#include <ComponentInterface/Component.h>
#include <ComponentInterface/dataTypes.h>
#include <ComponentInterface/Messages.h>
#include <ComponentInterface/MessageDataExt.h>
#include <ApsimShared/ApsimComponentData.h>
#include <ApsimShared/FStringExt.h>
#include <general/string_functions.h>

#include "OOPlantActions.h"
#include "OOPlant.h"
#include "TypeKind.h"

//------------------------------------------------------------------------------------------------

#pragma package(smart_init)

//------------------------------------------------------------------------------------------------
// Register Methods, Events,
//------------------------------------------------------------------------------------------------
void OOPlant::doRegistrations(void)
   {
   unsigned id;
      // Events

   setupEvent(plantInterface,"prepare",     RegistrationType::respondToEvent, &OOPlant::doPrepare, "");
   setupEvent(plantInterface,"process",     RegistrationType::respondToEvent, &OOPlant::doProcess, "");
   setupEvent(plantInterface,"tick",        RegistrationType::respondToEvent, &OOPlant::doTick, "");
   setupEvent(plantInterface,"newmet",      RegistrationType::respondToEvent, &OOPlant::doNewMet, "");
   setupEvent(plantInterface,"new_profile", RegistrationType::respondToEvent, &OOPlant::doNewProfile, "");
   setupEvent(plantInterface,"sow",         RegistrationType::respondToEvent, &OOPlant::sowCrop, "");
   setupEvent(plantInterface,"harvest",     RegistrationType::respondToEvent, &OOPlant::doHarvest, "");
   setupEvent(plantInterface,"end_crop",    RegistrationType::respondToEvent, &OOPlant::doEndCrop, "");
   setupEvent(plantInterface,"kill_crop",   RegistrationType::respondToEvent, &OOPlant::doKillCrop, "");
   setupEvent(plantInterface,"end_run",     RegistrationType::respondToEvent, &OOPlant::doEndRun, "");


   // --------------------------------------------------------------------------------------------
   // Variables available to other modules on request (e.g. report)


#define setupGetVar plantInterface->addGettableVar
//   setupGetVar("crop_type", cropType, "", "Crop species");
   setupGetVar("crop_class",cropClass, "", "");
//   setupGetVar("plant_status", statusString, "", "Status of crop");
   setupGetVar("das", das, "days", "Days after sowing");
   setupGetVar("radn_int", radnIntercepted, "", "");
   setupGetVar("temp_stress", tempStress, "", "");
   setupGetVar("plants", plantDensity, "plants/m2", "Plant density");
   setupGetVar("tiller_no", ftn, "tillers/plant", "No of tillers on main stem");
   setupGetVar("tiller_no_fertile", ftn, "tillers/plant", "No of tillers that produce a head");
   setupGetVar("vpd", vpd, "", "Vapour pressure deficit");
   setupGetVar("transp_eff", transpEff, "g/m2", "Transpiration efficiency");
#undef setupGetVar

   setupGetFunction(plantInterface,"plant_status", protocol::DTstring, false,
                    &OOPlant::getPlantStatus, "", "Status of crop");



   // --------------------------------------------------------------------------------------------
   // gets
   //frIntcRadnID  = plantInterface->addRegistration(RegistrationType::get,
   //                                                         "fr_intc_radn", floatType,"", "");

  // no3MinID      = plantInterface->addRegistration(RegistrationType::get,
   //                                                         "no3_min", floatArrayType,"", "");

   // sets

   // events.
//   cropChoppedID = plantInterface->addRegistration(RegistrationType::event,
//                                                            "crop_chopped", "","", "");
//   incorpFomID   = plantInterface->addRegistration(RegistrationType::event,
//                                                            "incorp_fom", "","", "");

   }
/*//------------------------------------------------------------------------------------------------
//--------  add an entry to the variable map
//------------------------------------------------------------------------------------------------
void OOPlant::mapVar(unsigned id,string name, void *ptr,int dType)
   {
   VarInfo *v = new VarInfo;
   v->name = name;
   v->ptr = ptr;
   v->type = dType;
   vMap.insert(VariableMap::value_type(id,v));
   }  */
//------------------------------------------------------------------------------------------------
//-----------------    Call events
//------------------------------------------------------------------------------------------------
/*void OOPlant::doEvent(unsigned int &id, protocol::Variant &v)
   {
   ptr2EventFn pf = IDtoEventFn[id];
   if (pf) {(this->*pf)(id,v);}
   }     */
//------------------------------------------------------------------------------------------------
//---------   respondToEvent
//---------   Field a Prepare message
//------------------------------------------------------------------------------------------------
void OOPlant::doPrepare(unsigned &, unsigned &, protocol::Variant &)
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
void OOPlant::doProcess(unsigned &, unsigned &, protocol::Variant &)
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
void OOPlant::doTick(unsigned &, unsigned &, protocol::Variant &v)
   {
   struct protocol::timeType tick;
   v.unpack(tick);
   today.todayDate.convertJulian((float)tick.startday);
   }
//------------------------------------------------------------------------------------------------
//-----------------   Field a Kill event
//------------------------------------------------------------------------------------------------
void OOPlant::doKillCrop(unsigned &, unsigned &, protocol::Variant &v)
   {
   plantInterface->writeString("Kill Crop");

   if(plantStatus == alive)
      {
      setStatus(dead);
      char line[80];
      sprintf(line,"Crop kill. Standing above-ground dm = %7.1f kg/ha",
         biomass->getAboveGroundBiomass());
      plantInterface->writeString(line);
      }
   }
//------------------------------------------------------------------------------------------------
//-----------------   Field a NewMet event
//------------------------------------------------------------------------------------------------
void OOPlant::doNewMet(unsigned &, unsigned &, protocol::Variant &v)
   {
   struct protocol::newmetType newmet;
   v.unpack(newmet);
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
void OOPlant::doNewProfile(unsigned &, unsigned &, protocol::Variant &v /* (INPUT) message arguments*/)
   {
   roots->doNewProfile(v);
   water->doNewProfile(v);
   nitrogen->doNewProfile(v);
   }
//------------------------------------------------------------------------------------------------
//-----------------   respondToMethodCall
//-----------------   Harvest
//------------------------------------------------------------------------------------------------
void OOPlant::doHarvest(unsigned &, unsigned &, protocol::Variant &v)     // Field a Harvest event
   {
   plantInterface->writeString("");
   plantInterface->writeString("Harvest");
   char line[200];

   phenology->Summary();
   leaf->Summary();
   biomass->Summary();
   grain->Summary();
   nitrogen->Summary();
   if(phosphorus->Active())
      phosphorus->Summary();

   plantInterface->writeString("");
   plantInterface->writeString("");

   // stress - not done yet
   plantInterface->writeString("Average Stress Indices:                          Water Photo  Water Expan  N Photo      N grain conc");
   plantInterface->writeString("   emergence           to end_of_juvenile           N/A          N/A        N/A          N/A        ");
   plantInterface->writeString("   end_of_juvenile     to floral_initiation         N/A          N/A        N/A          N/A        ");
   plantInterface->writeString("   floral_initiation   to flag_leaf                 N/A          N/A        N/A          N/A        ");
   plantInterface->writeString("   flag_leaf           to flowering                 N/A          N/A        N/A          N/A        ");
   plantInterface->writeString("   flowering           to start_grain_fill          N/A          N/A        N/A          N/A        ");
   plantInterface->writeString("   start_grain_fill    to end_grain_fill            N/A          N/A        N/A          N/A        ");

   plantInterface->writeString("");
   plantInterface->writeString("Crop harvested.");
   plantInterface->writeString("   Organic matter removed from system:-      From Tops\t\tFrom Roots");
   summaryLine(plantInterface,"                    DM (kg/ha) =              %8.2f\t\t%8.2f",
      grain->getDmGreen() * 10,0);
   summaryLine(plantInterface,"                    N  (kg/ha) =              %8.2f\t\t%8.2f",
      grain->getNGreen() * 10,0);

   unsigned int id = plantInterface->addRegistration(RegistrationType::event,"harvesting", "", "", "");
   protocol::ApsimVariant outgoingApsimVariant(plantInterface);
   plantInterface->publish (id, outgoingApsimVariant);

   grain->Harvest();
   biomass->Update();


   }
//------------------------------------------------------------------------------------------------
//-----------------   end run
//------------------------------------------------------------------------------------------------
void OOPlant::doEndRun(unsigned &, unsigned &, protocol::Variant &/*v*/)  // Field a end run event
  {
  plantInterface->writeString("End Run");
  }
//------------------------------------------------------------------------------------------------
/*
//------------------------------------------------------------------------------------------------
//-----------------   respondToGet -  Return a variable to the system.
//------------------------------------------------------------------------------------------------
void OOPlant::getVariable(protocol::QueryValueData& qd)
   {
   VarInfo *v = vMap[qd.ID];
   switch (v->type)
      {
      case protocol::DTfloat :
         plantInterface->sendVariable(qd, *(float *)v->ptr);
         break;
      case protocol::DTint4 :
         plantInterface->sendVariable(qd, *(int *)v->ptr);
         break;
      case protocol::DTstring :
         plantInterface->sendVariable(qd,FString((*(string *)v->ptr).c_str()));
         break;
      }
   }    */
//------------------------------------------------------------------------------------------------
// ---------respondToSet   -  Set a variable from the system.
//------------------------------------------------------------------------------------------------
bool OOPlant::setVariable(unsigned id, protocol::QuerySetValueData& qd)
   {
   ptr2setFn pf = IDtoSetFn[id];
   if (pf) {return((this->*pf)(qd));}
   return false;
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
void OOPlant::doEndCrop(unsigned &, unsigned &,protocol::Variant &v)     // Field a End crop event
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
   float yield = grain->getDmGreen() + grain->getDmDead() * gm2kg /sm2ha;

   char line[80];
   sprintf(line,"Crop ended. Yield (dw) = %7.1f kg/ha",yield * 10);
   plantInterface->writeString(line);

   plantInterface->writeString("Organic matter from crop:-      Tops to surface residue\t Roots to soil FOM");
   summaryLine(plantInterface,"                    DM (kg/ha) =              %8.2f\t\t%8.2f",
      biomass->getAboveGroundBiomass() - grain->getDmGreen() * 10.0,roots->getDmGreen() * 10.0);
   summaryLine(plantInterface,"                    N  (kg/ha) =              %8.2f\t\t%8.2f",
      (leaf->getNGreen() + stem->getNGreen()) * 10.0,roots->getNGreen() * 10);
   if(phosphorus->Active())
      summaryLine(plantInterface,"                    P  (kg/ha) =              %8.2f\t\t%8.2f",
      (leaf->getPGreen() + stem->getPGreen()) * 10.0,roots->getPGreen() * 10);
   else
      summaryLine(plantInterface,"                    P  (kg/ha) =              %8.2f\t\t%8.2f",0,0);


   roots->incorporateResidue();
   biomass->incorporateResidue();
   for(unsigned i=0;i < PlantParts.size();i++) PlantParts[i]->initialize ();
   biomass->Harvest();
   biomass->Update();

   }
//------------------------------------------------------------------------------------------------
void OOPlant::getPlantStatus(protocol::Component *system, protocol::QueryValueData &qd)
   {
   system->sendVariable(qd, FString(statusString.c_str()));
   }
//------------------------------------------------------------------------------------------------


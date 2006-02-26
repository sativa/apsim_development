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

   setupEvent(plantInterface,"prepare",     RegistrationType::respondToEvent, &OOPlant::doPrepare);
   setupEvent(plantInterface,"process",     RegistrationType::respondToEvent, &OOPlant::doProcess);
   setupEvent(plantInterface,"tick",        RegistrationType::respondToEvent, &OOPlant::doTick);
   setupEvent(plantInterface,"newmet",      RegistrationType::respondToEvent, &OOPlant::doNewMet);
   setupEvent(plantInterface,"new_profile", RegistrationType::respondToEvent, &OOPlant::doNewProfile);
   setupEvent(plantInterface,"sow",         RegistrationType::respondToEvent, &OOPlant::sowCrop);
   setupEvent(plantInterface,"harvest",     RegistrationType::respondToEvent, &OOPlant::doHarvest);
   setupEvent(plantInterface,"end_crop",    RegistrationType::respondToEvent, &OOPlant::doEndCrop);
   setupEvent(plantInterface,"kill_crop",   RegistrationType::respondToEvent, &OOPlant::doKillCrop);
   setupEvent(plantInterface,"end_run",     RegistrationType::respondToEvent, &OOPlant::doEndRun);


   // --------------------------------------------------------------------------------------------
   // Variables available to other modules on request (e.g. report)


#define setupGetVar plantInterface->addGettableVar
   setupGetVar("crop_type", cropType, "", "Crop species");
   setupGetVar("crop_class",cropClass, "", "");
//   setupGetVar("plant_status", statusString, "", "Status of crop");
   setupGetVar("das", das, "days", "Days after sowing");
   setupGetVar("radn_int", radnIntercepted, "", "");
   setupGetVar("temp_stress", tempStress, "", "");
   setupGetVar("plants", plantDensity, "plants/m2", "Plant density");
   setupGetVar("tiller_no", ftn, "tillers/plant", "No of tillers on main stem");
   setupGetVar("tiller_no_fertile", ftn, "tillers/plant", "No of tillers that produce a head");
   setupGetVar("vpd", vpd, "", "Vapour pressure density");
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
   if (plantStatus == alive)
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
         biomass->getAboveGroundBiomass() * gm2kg /sm2ha);
      plantInterface->writeString(line);
      }
      /* TODO : deand and senesced bio to be added here */
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
   plantInterface->writeString("   emergence           to end_of_juvenile         0.000        0.000        0.000        0.000");
   plantInterface->writeString("   end_of_juvenile     to floral_initiation       0.000        0.000        0.000        0.000");
   plantInterface->writeString("   floral_initiation   to flag_leaf               0.000        0.000        0.000        0.000");
   plantInterface->writeString("   flag_leaf           to flowering               0.000        0.000        0.000        0.000");
   plantInterface->writeString("   flowering           to start_grain_fill        0.000        0.000        0.000        0.000");
   plantInterface->writeString("   start_grain_fill    to end_grain_fill          0.000        0.000        0.006        0.000");

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

   //plant_harvest (v);             // harvest crop - turn into residue
   }
//------------------------------------------------------------------------------------------------
//-----------------   end run
//------------------------------------------------------------------------------------------------
void OOPlant::doEndRun(unsigned &, unsigned &, protocol::Variant &/*v*/)  // Field a end run event
  {
  plantInterface->writeString("End Run");
  //plant_zero_variables ();
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
   sprintf(line,"Crop ended. Yield (dw) = %7.1f kg/ha",yield);
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


   // reset variables
   initialize();
   for(unsigned i=0;i < PlantComponents.size();i++)
      {
      PlantComponents[i]->initialize ();
      }
   }
   /*
       !Root residue incorporation

         dm_root = g%dm_green(root)
     :           + g%dm_dead(root)
     :           + g%dm_senesced(root)

         N_root  = g%N_green(root)
     :           + g%N_dead(root)
     :           + g%N_senesced(root)


      if (dm_root.gt.0.0) then

         call crop_root_incorp (
     :          dm_root
     :         ,N_root
     :         ,g%dlayer
     :         ,g%root_length
     :         ,g%root_depth
     :         ,c%crop_type
     :         ,max_layer
     :         ,EventInterface)

      endif


      !Top residue - put stover into surface residue
         dm_residue = (sum_real_array (g%dm_green, max_part)
     :              - g%dm_green(root))
c    :              - g%dm_green(root) - g%dm_green(grain))
 
     :              + (sum_real_array (g%dm_senesced, max_part)
     :              - g%dm_senesced(root))
C    :              - g%dm_senesced(root) - g%dm_senesced(grain))
 
     :              + (sum_real_array (g%dm_dead, max_part)
     :              - g%dm_dead(root))
c    :              - g%dm_dead(root) - g%dm_dead(grain))

         N_residue = (sum_real_array (g%N_green, max_part)
     :             - g%N_green(root) )
c    :             - g%N_green(root) - g%N_green(grain))
 
     :             + (sum_real_array (g%N_senesced, max_part)
     :             - g%N_senesced(root))
c    :             - g%N_senesced(root) - g%N_senesced(grain))
 
     :             + (sum_real_array (g%N_dead, max_part)
     :             - g%N_dead(root))
c    :             - g%N_dead(root) - g%N_dead(grain))

         dlt_dm_crop(:) = (g%dm_green(:)
     :                  + g%dm_senesced(:)
     :                  + g%dm_dead(:))
     :                  * gm2kg/sm2ha

         dlt_dm_N   (:) = (g%N_green(:)
     :                  + g%N_senesced(:)
     :                  + g%N_dead(:))
     :                  * gm2kg/sm2ha
                    

         fraction_to_residue(:)    = 1.0
         fraction_to_Residue(root) = 0.0

         if (sum(dlt_dm_crop) .gt. 0.0) then

            call Send_Crop_Chopped_Event
     :                (c%crop_type
     :               , part_name
     :               , dlt_dm_crop
     :               , dlt_dm_N
     :               , fraction_to_Residue
     :               , max_part)

         else
            ! no surface residue
         endif


         write (string, '(40x, a, f7.1, a, 3(a, 40x, a, f6.1, a))')
     :                  '  straw residue ='
     :                  , dm_residue * gm2kg /sm2ha, ' kg/ha'
     :                  , new_line
     :                  , '  straw N = '
     :                  , N_residue * gm2kg /sm2ha, ' kg/ha'

     :                  , new_line
     :                  , '  root residue = '
     :                  , dm_root * gm2kg /sm2ha, ' kg/ha'
     :                  , new_line
     :                  , '  root N = '
     :                  , N_root * gm2kg /sm2ha, ' kg/ha'
 
         call write_string ( string)



    float dm_residue;                             // dry matter added to residue (g/m^2)
    float n_residue;                              // nitrogen added to residue (g/m^2)
    float dm_root;                                // dry matter added to soil (g/m^2)
    float n_root;                                 // nitrogen added to soil (g/m^2)
    char  msg[400];
    float yield;                                  // grain wt (kg/ha)
    float fraction_to_residue[6];          // fraction sent to residue (0-1)
    float dlt_dm_crop[max_part];                  // change in dry matter of crop (kg/ha)
    float dlt_dm_n[max_part];                     // N content of changeed dry matter (kg/ha)
    int part;                                     // part


      plantStatus = out;

        // report
        yield = (g.dm_green[meal] + g.dm_dead[meal]
          + g.dm_green[oil] + g.dm_dead[oil] )
          * gm2kg /sm2ha;
        sprintf (msg, "   crop ended. yield (dw) = %7.1f  (kg/ha)", yield);
        parent->writeString (msg);

        // now do post harvest processes
        dm_root = g.dm_green[root] + g.dm_senesced[root];

        n_root  = g.n_green[root] + g.n_senesced[root];

        plant_root_incorp (dm_root, n_root, g.root_length);

        plant_root_incorp (g.dm_dead[root], g.n_dead[root], g.root_length_dead);

        // put stover and any remaining grain into surface residue
        dm_residue =
             sum_real_array (g.dm_green, max_part) - g.dm_green[root]
           + sum_real_array (g.dm_senesced, max_part) - g.dm_senesced[root]
           + sum_real_array (g.dm_dead, max_part) - g.dm_dead[root];

        for ( part = 0; part < max_part; part++)
           {
           dlt_dm_crop[part] = (g.dm_green[part]
               + g.dm_senesced[part]
               + g.dm_dead[part])
               * gm2kg/sm2ha;
           }
        n_residue =
             sum_real_array (g.n_green, max_part) - g.n_green[root]
           + sum_real_array (g.n_senesced, max_part) - g.n_senesced[root]
           + sum_real_array (g.n_dead, max_part) - g.n_dead[root];

        for ( part = 0; part < max_part; part++)
           {
           dlt_dm_n[part] = (g.n_green[part]
              + g.n_senesced[part]
              + g.n_dead[part])
              * gm2kg/sm2ha;
           }

        // call crop_top_residue (c%crop_type, dm_residue, N_residue)

        for ( part = 0; part < max_part; part++) { fraction_to_residue[part] = 1.0; }
        fraction_to_residue[root] = 0.0;

        if (sum_real_array(dlt_dm_crop, max_part) > 0.0)
            {
            plant_send_crop_chopped_event ( c.crop_type
                  , part_name
                  , dlt_dm_crop
                  , dlt_dm_n
                  , fraction_to_residue
                  , max_part);
            }
        else
            {
            // no surface residue
            }

        dm_root = g.dm_green[root] + g.dm_dead[root] + g.dm_senesced[root];
        n_root  = g.n_green[root] + g.n_dead[root] + g.n_senesced[root];

        sprintf (msg, "%s%7.1f%s"
        , "  straw residue = "
        , dm_residue * gm2kg /sm2ha, " kg/ha");
        parent->writeString (msg);

        sprintf (msg, "%s%7.1f%s"
        , "  straw n =       "
        , n_residue * gm2kg /sm2ha, " kg/ha");
        parent->writeString (msg);

        sprintf (msg, "%s%7.1f%s"
        , "  root residue =  "
        , dm_root * gm2kg /sm2ha, " kg/ha");
        parent->writeString (msg);

        sprintf (msg, "%s%7.1f%s"
        , "  root n =        "
        , n_root * gm2kg /sm2ha, " kg/ha");
        parent->writeString (msg);

        }
    else
        {
        sprintf(msg, "%s%s%s"
         ,g.module_name.c_str()
         , " is not in the ground -"
         , " unable to end crop.");

        warning_error (&err_user, msg);
        }


    }

     */
//------------------------------------------------------------------------------------------------
void OOPlant::getPlantStatus(protocol::Component *system, protocol::QueryValueData &qd)
   {
   system->sendVariable(qd, FString(statusString.c_str()));
   }






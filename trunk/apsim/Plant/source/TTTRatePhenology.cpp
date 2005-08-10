#include "TTTRatephenology.h"
#include <ComponentInterface/Component.h>
#include <ComponentInterface/dataTypes.h>
#include <ComponentInterface/ApsimVariant.h>
#include <ComponentInterface/MessageDataExt.h>
#include "PlantComponent.h"
#include "PlantLibrary.h"
#include "PlantPhenology.h"
#include "Environment.h"



// dynamic TT targets
void TTTRatePhenology::updateTTTargets(const environment_t &e)
   {
   dlt_cumvd = vernal_days.value((e.maxt + e.mint)*0.5);

   if (inPhase("germination"))
      {
      pPhase *emerg_to_endjuv = getStage("emergence");
      emerg_to_endjuv->setTarget(tt_emerg_to_endjuv[cumvd + dlt_cumvd]);
      }
   else if (inPhase("emergence"))
      {
      if (on_day_of("emergence"))
         {
         int est_day_of_floral_init = e.day_of_year + est_days_emerg_to_init % 366;
         float est_photoperiod = e.daylength (est_day_of_floral_init, twilight);

         pPhase *endjuv_to_init = getStage("end_of_juvenile");
         endjuv_to_init->setTarget(tt_endjuv_to_init[est_photoperiod]);
         }

      pPhase *emerg_to_endjuv = getStage("emergence");
      emerg_to_endjuv->setTarget(tt_emerg_to_endjuv[cumvd + dlt_cumvd]);

      pPhase *endjuv_to_init = getStage("end_of_juvenile");
      endjuv_to_init->setTarget(tt_endjuv_to_init[photoperiod]);
      }
   else if (inPhase("end_of_juvenile"))
      {

      pPhase *endjuv_to_init = getStage("end_of_juvenile");
      float fraction = 1.0 - endjuv_to_init->getTT()/endjuv_to_init->getTTTarget();
      float photoperiodYesterday = e.daylength (max(e.day_of_year - 1,1),twilight);
      float newTarget = endjuv_to_init->getTTTarget() + fraction*(tt_endjuv_to_init[photoperiod] - tt_endjuv_to_init[photoperiodYesterday]);
      endjuv_to_init->setTarget(newTarget);

   string s;
   s =  ftoa(fraction, "10.4") + " " + ftoa(photoperiod, "10.4") + " " + ftoa(photoperiodYesterday, "10.4")+ ftoa(endjuv_to_init->getTTTarget(), "10.2") + " " + ftoa(tt_endjuv_to_init[photoperiod], "10.2");
   parentPlant->writeString (s.c_str());
   s =  ftoa(tt_endjuv_to_init[photoperiodYesterday], "10.2") + " " + ftoa(tt_endjuv_to_init[photoperiod], "10.2");
   parentPlant->writeString (s.c_str());

      pPhase *init_to_flower = getStage("floral_initiation");
      init_to_flower->setTarget(tt_init_to_flower[photoperiod]);
      }
   else if (inPhase("floral_initiation"))
      {
      pPhase *init_to_flower = getStage("floral_initiation");
      float fraction = 1.0 - init_to_flower->getTT()/init_to_flower->getTTTarget();
      float newTarget = init_to_flower->getTTTarget() + fraction*(tt_init_to_flower[photoperiod] - init_to_flower->getTTTarget());
      init_to_flower->setTarget(newTarget);

      pPhase *flower_to_start_grain = getStage("flowering");
      flower_to_start_grain->setTarget(tt_flower_to_start_grain[photoperiod]);
      }
   else if (inPhase("flowering"))
      {
      pPhase *flower_to_start_grain = getStage("flowering");
      float fraction = 1.0 - flower_to_start_grain->getTT()/flower_to_start_grain->getTTTarget();
      float newTarget = flower_to_start_grain->getTTTarget() + fraction*(tt_flower_to_start_grain[photoperiod] - flower_to_start_grain->getTTTarget());
      flower_to_start_grain->setTarget(newTarget);

      pPhase *start_to_end_grain = getStage("start_grain_fill");
      start_to_end_grain->setTarget(tt_start_to_end_grain[photoperiod]);
      }
   else if (inPhase("start_grain_fill"))
      {
      pPhase *start_to_end_grain = getStage("start_grain_fill");
      float fraction = 1.0 - start_to_end_grain->getTT()/start_to_end_grain->getTTTarget();
      float newTarget = start_to_end_grain->getTTTarget() + fraction*(tt_start_to_end_grain[photoperiod] - start_to_end_grain->getTTTarget());
      start_to_end_grain->setTarget(newTarget);
      }
   else
      {
      //??
      }
   }


void TTTRatePhenology::get_zadok_stage(protocol::Component *system, protocol::QueryValueData &qd)
{
    float sowing = 1.0, emerg = 3.0;
    float zadok_stage = 0.0;
    if (currentStage >= sowing &&
        currentStage <= emerg)
       {
       zadok_stage = 5.0 * (currentStage - sowing);
       }
    else if (currentStage > emerg &&
             currentStage <= 4.9)
       {
       float leaf_no_now = max(0.0, plant->getLeafNo() - 2.0);

       static float tillerno_y[] =   // tiller no.
           {0, 0, 5};
       static float tillerno_x[] =   // lf. no.
           {0, 5, 8};
       float tiller_no_now = linear_interp_real (leaf_no_now
                                               , tillerno_x
                                               , tillerno_y
                                               , sizeof(tillerno_x)/sizeof(float));
       if (tiller_no_now <= 0.0)
           {
           zadok_stage = 10.0 + leaf_no_now;
           }
        else
           {
           zadok_stage = 20.0 + tiller_no_now;
           }
       }
    else if (currentStage > 4.9 &&
             currentStage < 11 )
       {

       static float zadok_code_y[] =
           {30,   40, 65, 71, 87, 90, 100};
       static float zadok_code_x[] =
           {4.9, 5.7,  6,  7,  8,  9,  10};

      zadok_stage = linear_interp_real (currentStage
                                       , zadok_code_x
                                       , zadok_code_y
                                       , sizeof(zadok_code_x)/sizeof(float));
       }
    system->sendVariable(qd, zadok_stage);
}

void TTTRatePhenology::doRegistrations (protocol::Component *s)
   {
   TTTPhenology::doRegistrations(s);

   setupGetFunction(parentPlant, "zadok_stage", protocol::DTsingle, false,
                    &TTTRatePhenology::get_zadok_stage,
                    "0-100", "Zadok's growth developmental stage");

   }   
#include "wheatphenology.h"
#include <ComponentInterface/Component.h>
#include <ComponentInterface/dataTypes.h>
#include <ComponentInterface/ApsimVariant.h>
#include <ComponentInterface/MessageDataExt.h>
#include "PlantComponent.h"
#include "PlantLibrary.h"
#include "PlantPhenology.h"
#include "Environment.h"
///////////////////////////WHEAT///////////////////////////////////
void WheatPhenology::zeroAllGlobals(void)
   {
   CropPhenology::zeroAllGlobals();
   vern_eff = photop_eff = 0.0;
   }

void WheatPhenology::readConstants (protocol::Component *s, const string &section)
   {
   CropPhenology::readConstants(s, section);
   s->writeString("phenology model: Wheat");
   }



void WheatPhenology::setupTTTargets(void)
   {
   pPhase *sow_to_germ = getStage("sowing");
   sow_to_germ->setTarget(0.0);

   // On the germination day, calculate the tt for emergence
   pPhase *germ_to_emerg = getStage("germination");
   germ_to_emerg->setTarget(shoot_lag + sowing_depth * shoot_rate);

   //This is to avoid a warning in leaf number final
   pPhase *emerg_to_endjuv = getStage("emergence");
   emerg_to_endjuv->setTarget(1.0);

   pPhase *endjuv_to_init = getStage("end_of_juvenile");
   endjuv_to_init->setTarget(400.0);

   pPhase *init_to_flower = getStage("floral_initiation");
   init_to_flower->setTarget(5.0 * phyllochron + 80.0);

   pPhase *flower_to_start_grain = getStage("flowering");
   flower_to_start_grain->setTarget( 200.0 - 80.0);

   pPhase *end_grain_to_maturity = getStage("end_grain_fill");
   end_grain_to_maturity->setTarget( 0.05 *
           (flower_to_start_grain->getTTTarget() + startgf_to_mat));

   pPhase *start_to_end_grain = getStage("start_grain_fill");
   start_to_end_grain->setTarget( startgf_to_mat - end_grain_to_maturity->getTTTarget());

   pPhase *maturity_to_ripe = getStage("maturity");
   maturity_to_ripe->setTarget(1.0);

   pPhase *ripe_to_harvest = getStage("harvest_ripe");
   ripe_to_harvest->setTarget(1000.0) ;       // keep it from dying????
   }



void WheatPhenology::readCultivarParameters(protocol::Component *s, const string & cultivar)
   {
   CropPhenology::readCultivarParameters(s, cultivar);
   s->readParameter (cultivar
                   , "phyllochron"//, "()"
                   , phyllochron
                   , 0.0, 300.);

   s->readParameter (cultivar
                   , "startgf_to_mat"//, "()"
                   , startgf_to_mat
                   , 0.0, 3000.);

   s->readParameter (cultivar
                   , "vern_sens"//, "()"
                   , vern_sens
                   , 0.0, 10.0);

   s->readParameter (cultivar
                   , "photop_sens"//, "()"
                   , photop_sens
                   , 0.0, 10.0);
   }






// vernalisation & photoperiod stresses
void WheatPhenology::vernalisation (const environment_t &t)
   {
   float tempcr = crown_temp_nwheat (t.maxt, t.mint, 0.0);

   dlt_cumvd = wheat_vernaliz_days(t.maxt ,t.mint ,tempcr, 0.0 , cumvd);

   //maximum vernalisation requirement is 50 days
   vern_eff = wheat_vernaliz_effect(vern_sens, cumvd, dlt_cumvd, 50.0);

   float photoperiod = t.daylength(twilight);
   photop_eff = wheat_photoperiod_effect(photoperiod, photop_sens);

   // Thermal time is calculated from crown temperature
   dlt_tt = y_tt[tempcr];
   }

// Crown temperature from nwheat
float WheatPhenology::crown_temp_nwheat (float maxt, float mint, float snow)
   {
   // Calculate max crown temperature
   float cx;
   if (maxt < 0.)
        cx = 2.0 + maxt * (0.4 + 0.0018 * pow(snow - 15., 2));
   else
        cx = maxt;

   // Calculate min crown temperature
   float cn;
   if (mint < 0.)
        cn = 2.0 + mint * (0.4 + 0.0018 * pow(snow - 15., 2));
   else
        cn = mint;

   return ((cn+cx)/2.0);
   }


void WheatPhenology::writeCultivarInfo (PlantComponent *systemInterface)
   {
   string s;
   s += "   pesw germination           = ";
   s += ftoa(pesw_germ, "10.2") + " (0-1)\n";

   s += "   vernalisation sensitivity  = ";
   s += ftoa(vern_sens, "10.2") + " ()\n";

   s += "   photoperiod sensitivity    = ";
   s += ftoa(photop_sens, "10.2") + " ()\n";

   s += "   phyllochron                = ";
   s += ftoa(phyllochron, "10.0") + " ()\n";

   s += "   tt start gf to maturity    = ";
   s += ftoa(startgf_to_mat, "10.0") + " (dd)";

   systemInterface->writeString (s.c_str());
   }


// Soil water stresses
//+  Purpose
//     Use temperature, photoperiod and genetic characteristics
//     to determine when the crop begins a new growth phase.
//     The initial daily thermal time and height are also set.
// NB. There are 2 ways to advance from one stage to the next:
// via the dltStage calculation in stage_devel(), or
// via thermal time accumulation

void WheatPhenology::process (const environment_t &sw, const pheno_stress_t &ps)
   {
   float phase_devel, new_stage;

   vernalisation(sw);


   if (inPhase("sowing"))
      {
      dlt_tt_phenol = dlt_tt;
      // can't germinate on same day as sowing, because we would miss out on
      // day of sowing elsewhere.
      if (das == 0)
         {
         phase_devel = 0.999;
         }
      else if ( plant_germination(pesw_germ, sowing_depth, sw) )
         {
         phase_devel = 1.999;
         }
      else
         {
         phase_devel = 0.0;
         }
      new_stage = floor(currentStage) + phase_devel;
      }
   else if (inPhase("germination"))
      {
      int layer_no_seed = sw.find_layer_no (sowing_depth);
      float fasw_seed = divide (sw.sw_dep[layer_no_seed] - sw.ll_dep[layer_no_seed],
                                sw.dul_dep[layer_no_seed] - sw.ll_dep[layer_no_seed], 0.0);
      fasw_seed = bound (fasw_seed, 0.0, 1.0);

      dlt_tt_phenol = dlt_tt *
                       min(vern_eff, photop_eff) *
                       rel_emerg_rate[fasw_seed];

      const pPhase &current = phases[currentStage];
      phase_devel = divide(current.getTT() + dlt_tt_phenol, current.getTTTarget(), 1.0);
      new_stage = floor(currentStage) + phase_devel;
      }
   else if (inPhase("above_ground"))
      {
      float fstress = min (ps.swdef, ps.nfact);  //eme2fi: (g.swdef_pheno, g.nfact_pheno);
      if (inPhase("flowering"))
         fstress = ps.swdef_flower;              //g.swdef_pheno_flower;
      else if (inPhase("start_grain_fill"))
         fstress = ps.swdef_grainfill;           //g.swdef_pheno_grainfill;
      else if (inPhase("end_grain_fill") || inPhase("maturity"))
         fstress = 1.0;                          //no stress - not really a stage..
      else if (inPhase("harvest_ripe"))
         fstress = 0.0;                          //stop development
      dlt_tt_phenol = dlt_tt *
                      fstress *
                      min(vern_eff, photop_eff);

      const pPhase &current = phases[currentStage];
      phase_devel = divide(current.getTT() + dlt_tt_phenol, current.getTTTarget(), 1.0);
      new_stage = floor(currentStage) + phase_devel;
      }
   else
      {
      // ??Hmmm. should probably stop dead here??
      dlt_tt_phenol = dlt_tt;
      phase_devel = 0.0;
      new_stage = floor(currentStage) + phase_devel;
      }
   dltStage = new_stage - currentStage;


   /// accumulate() to objects
   float value = dlt_tt_phenol;             //  (INPUT) value to add to array
   float p_index = currentStage;           //  (INPUT) current p_index no
   float dlt_index = dltStage;       //  (INPUT) increment in p_index no

   {
   int current_index;           // current index number ()
   float fract_in_old;           // fraction of value in last index
   float index_devel;            // fraction_of of current index elapsed ()
   int new_index;                // number of index just starting ()
   float portion_in_new;         // portion of value in next index
   float portion_in_old;         // portion of value in last index

   // (implicit) assert(dlt_index <= 1.0);
   current_index = int(p_index);

   // make sure the index is something we can work with
   if(current_index >= 0)
      {
      index_devel = p_index - floor(p_index) + dlt_index;
      if (index_devel >= 1.0)
         {
         // now we need to divvy
         new_index = (int) (p_index + min (1.0, dlt_index));
         if (reals_are_equal(fmod(p_index,1.0),0.0))
            {
            fract_in_old = 1.0 - divide(index_devel - 1.0, dlt_index, 0.0);
            portion_in_old = fract_in_old * (value + phases[current_index].getTT())-
                                 phases[current_index].getTT();
            }
         else
            {
            fract_in_old = 1.0 - divide(index_devel - 1.0, dlt_index, 0.0);
            portion_in_old = fract_in_old * value;
            }
         portion_in_new = value - portion_in_old;
         phases[current_index].add(fract_in_old, portion_in_old);
         phases[new_index].add(1.0-fract_in_old, portion_in_new);
         }
      else
         {
         phases[current_index].add(1.0, value);
         }
      }
   }

   if (phase_devel >= 1.0)
      currentStage = floor(currentStage + 1.0);
   else
      currentStage = new_stage;

   if ((unsigned int)currentStage >= phases.size() || currentStage < 0.0)
     throw std::runtime_error("stage has gone wild in WheatPhenology::process()..");

   if ((int)currentStage != (int)previousStage) plant->doPlantEvent(phases[(int)currentStage].name());
   cumvd += dlt_cumvd;
   das++;
   }


//+  Mission Statement
//     Photoperiod factor
float WheatPhenology::wheat_photoperiod_effect(float photoperiod, float p_photop_sen)
    {
    float photop_eff = 1.0;

    if (inPhase("eme2ej"))
        {
        float  photop_sen_factor = p_photop_sen * 0.002;
        photop_eff = 1. - photop_sen_factor * pow(20. - photoperiod, 2);
        photop_eff = bound (photop_eff, 0.0, 1.0);
        }
    return photop_eff;
    }


//+  Purpose
//     Calculate daily vernalisation and accumulate to g_cumvd

//+  Mission Statement
//     Calculate todays vernalization (used to affect phenology)

//+  Notes
//   Nwheat originally had the following if logic for determining whether
//   vernalisation is calculated for today
//     if     (          cumvd .lt. reqvd
//     :                        .and.
//     :       (istage .eq.emerg .or. istage .eq. germ)   )
//     :then
//
//   In order to remove the explicit value 'reqvd' and make the stages
//   more flexibile this logic was replaced. - NIH 14/07/98
float WheatPhenology::wheat_vernaliz_days(float g_maxt    //Daily maximum Temperature
                                         ,float g_mint    //Daily minimum temperature
                                         ,float tempcr    //Crown temperature
                                         ,float g_snow    //Snow depth of the day (mm)
                                         ,float g_cumvd)  //cumulative vernalisation days till yesterday
   {
   float dlt_cumvd = 0.0;

   if (inPhase("vernalisation"))
      {
      if (g_mint < 15.0 && g_maxt > 0.0)
          {
          // Cold
          float vd,vd1,vd2;
          vd1 = 1.4 - 0.0778 * tempcr;
          vd2 = 0.5 + 13.44 / pow(g_maxt-g_mint + 3., 2) * tempcr;
          vd = min (vd1, vd2);
          dlt_cumvd = l_bound (vd, 0.0);
          }
      if (g_maxt > 30. && g_cumvd + dlt_cumvd < 10.)
          {
          // high temperature will reduce vernalization
          dlt_cumvd = - 0.5*(g_maxt - 30.);
          dlt_cumvd = - min(-(dlt_cumvd), g_cumvd);
          }
      }
   return dlt_cumvd;
   }

//+  Purpose
//     Vernalisation factor
float WheatPhenology::wheat_vernaliz_effect(float p_vern_sens
                                           ,float cumvd
                                           ,float dlt_cumvd
                                           ,float reqvd) {
    float vfac;                // vernalization factor
    float vern_sens_fac;
    float vern_effect = 1.0;

    if (inPhase("eme2ej"))
        {
        if (reqvd < 0.0) { reqvd = 50.0; }
        vern_sens_fac =  p_vern_sens* 0.0054545 + 0.0003;
        vfac = 1. - vern_sens_fac * (reqvd - (cumvd+dlt_cumvd));
        vern_effect = bound (vfac, 0.0, 1.0);
        }
    return vern_effect;
    }




// XX This is heavily tied to current ini file. !!Watch Out!!
// NB. "5.2" is half way between FI and flag leaf in wheat
void WheatPhenology::get_zadok_stage(protocol::Component *system, protocol::QueryValueData &qd)
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
// from senthold's archive:
//1    2    3         4.5     5       6
//eme  ej   eveg(fl)  anth    sgf   mat
//10   30   43 	     65      70     9

// from CropMod
//                 sow ger eme  juv    fi   flag    fl st_gf end_gf mat hv_rpe end_crop
//stage_code       = 1   2   3    4      5     6     7    8    9    10   11   12
//Zadok_stage      = 0   5  10   10     15    40    60   71   87    90   93  100

// Plant:
//                 sow    ger   eme  juv    fi       fl   st_gf end_gf  mat hv_rpe  end
//stage_code      = 1      2    3     4     5   4.9 5.4   6     7      8     9    10     11  ()     ! numeric code for phenological stages
//                  na     na   na    na    na   30  43   65    71     87    90    100

       static float zadok_code_y[] =
           {30,   40, 65, 71, 87, 90, 100};
       static float zadok_code_x[] =
           {4.9, 5.4,  6,  7,  8,  9,  10};

      zadok_stage = linear_interp_real (currentStage
                                       , zadok_code_x
                                       , zadok_code_y
                                       , sizeof(zadok_code_x)/sizeof(float));
       }
    system->sendVariable(qd, zadok_stage);
}

void WheatPhenology::doRegistrations (protocol::Component *s)
   {
   CropPhenology::doRegistrations(s);

   parentPlant->addGettableVar("cum_vernal_days", cumvd, "vd", "Cumulative vernalisation");
   parentPlant->addGettableVar("vern_eff", vern_eff,     "", "Vernalisation effect");
   parentPlant->addGettableVar("photop_eff", photop_eff, "", "Photoperiod effect");


   setupGetFunction(parentPlant, "zadok_stage", protocol::DTsingle, false,
                    &WheatPhenology::get_zadok_stage,
                    "0-100", "Zadok's growth developmental stage");

   }
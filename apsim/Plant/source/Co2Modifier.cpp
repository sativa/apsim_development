
#include <stdio.h>
#include <math.h>
#include <stdexcept>
#include <string>
#include "Co2Modifier.h"

using namespace std;

static const char* floatArrayType =   "<type kind=\"single\" array=\"T\"/>";


//#####################################################################################
// Co2Modifier implementation
//######################################################################################

Co2Modifier::Co2Modifier(ScienceAPI& scienceAPI, protocol::Component *p)
   : scienceAPI(scienceAPI)
   , parent(p)
{
}

// destructor
Co2Modifier::~Co2Modifier()
{
}

void Co2Modifier::init(void)
   {
   zero_co2_variables();
   parent->addGettableVar("temp_stress_photo",
               tFact.photo, "", "Temperature Stress in photosynthesis");

//   parent->addGettableVar("temp_fact_photo",
//               tFact.photo, "", "Temperature Stress in photosynthesis");
//
//   setupGetFunction(parent, "temp_stress_photo", protocol::DTsingle, false,
//                    &Co2Modifier::get_tstress_photo,
//                    "","Temperature stress for photosynthesis");

      c.photosynthetic_pathway = photosynthetic_pathway_UNDEF;


   }

void Co2Modifier::zero_co2_variables (void)
// =======================================
//     Set all variables in this module to zero.
{
      co2_modifier_te = 0.0;
      co2_modifier_n_conc = 0.0;
      co2_modifier_rue = 0.0;

 }

//     ===========================================================
void Co2Modifier::read_co2_constants (void)
{
    cTE.read(scienceAPI, "x_co2_te_modifier", "()", 0.0, 1000.0,
                         "y_co2_te_modifier", "()", 0.0, 10.0);

    cNConc.read(scienceAPI, "x_co2_nconc_modifier", "()", 0.0, 1000.0,
                            "y_co2_nconc_modifier", "()", 0.0, 10.0);

    string pathway;
    scienceAPI.read("photosynthetic_pathway", pathway);

    if (Str_i_Eq(pathway.c_str(), "C3")) {
      c.photosynthetic_pathway = photosynthetic_pathway_C3;

    } else if(Str_i_Eq(pathway.c_str(), "C4")) {
      c.photosynthetic_pathway = photosynthetic_pathway_C4;

    } else {
      c.photosynthetic_pathway = photosynthetic_pathway_UNDEF;
      printf("undefined photosynthetic_pathway read!!!!\n");
    }
}

float Co2Modifier::rue (void)
   {
      return co2_modifier_rue;
   }

float Co2Modifier::te (void)
   {
      return co2_modifier_te;
   }

float Co2Modifier::n_conc (void)
   {
      return co2_modifier_n_conc;
   }

void Co2Modifier::doPlant_Co2Modifier (environment_t& Environment)
//     ===========================================================
//         Get current temperature stress factors (0-1)
   {
         co2_modifier_rue = plant_rue_co2_modifier(Environment.co2,
                               Environment.meanT);

         co2_modifier_te = cTE.value(Environment.co2);
         co2_modifier_n_conc = cNConc.value(Environment.co2);

   }

//==========================================================================
float Co2Modifier::plant_rue_co2_modifier(float co2,                 //!CO2 level (ppm)
                                   float temp)                //!daily average temp (C)
//==========================================================================
/*  Purpose
*     Calculation of the CO2 modification on rue
*
*     References
*     Reyenga, Howden, Meinke, Mckeon (1999), Modelling global change impact on wheat cropping in
*              south-east Queensland, Australia. Enivironmental Modelling & Software 14:297-306
*
*
*  Purpose
*     Calculation of the CO2 modification on rue
*
*  Changes
*     20000717   ew programmed
*/
   {
   //  Local Variables
      float TT;    //co2 compensation point (ppm)
      float first;            // Temp vars for passing composite arg to a func
      float second;           // expecting a pointer

   // Implementation Section ----------------------------------
   if (c.photosynthetic_pathway == photosynthetic_pathway_C3)
      {
      TT  = divide(163.0 - temp, 5.0 - 0.1 * temp, 0.0);

      first = (co2 - TT) * (350.0 + 2.0 * TT);
      second = (co2 + 2.0 * TT)*(350.0 - TT);
      return divide( first, second, 1.0);
      }
    else if (c.photosynthetic_pathway == photosynthetic_pathway_C4)
      {
      return 0.000143 * co2 + 0.95; //Mark Howden, personal communication
      }
    else
      throw std::invalid_argument ("Unknown photosynthetic pathway in cproc_rue_co2_modifier()");
      return 0;
   }






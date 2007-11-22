#include <stdio.h>
#include <math.h>
#include <stdexcept>
#include <string>
#include "PlantPart.h"

#include "Pool.h"
#include "Delta.h"
using namespace std;

Pool::Pool(ScienceAPI& API, const std::string& Name, const std::string& PartName)
   : scienceAPI(API)
   {
   this->Name = Name;
   this->PartName = PartName;
   Clear();

   scienceAPI.exposeFunction(PartName+Name+"Wt", "g/m^2", Name + " " + PartName + " dry matter", FloatFunction(&Biomass::DM));
   scienceAPI.exposeFunction(PartName+Name+"N",  "g/m^2", Name + " " + PartName + " nitrogen", FloatFunction(&Biomass::N));
   scienceAPI.exposeFunction(PartName+Name+"P",  "g/m^2", Name + " " + PartName + " phosphorus", FloatFunction(&Biomass::P));

   scienceAPI.exposeFunction(PartName+Name+"nconc", "%", "N concentration in "+Name+" "+PartName, FloatFunction(&Pool::NconcPercent));
   scienceAPI.exposeFunction(PartName+Name+"pconc", "%", "P concentration in "+Name+" "+PartName, FloatFunction(&Pool::PconcPercent));

   DigestibilityMax.read(scienceAPI
                        , "x_dmd_stage_code" , "()", 1.0, 12.0
                        , ("y_dmd_max_"+Name+"_" + PartName).c_str(), "()", 0.0, 1.0);

   DigestibilityAvg.read(scienceAPI
                        , "x_dmd_stage_code" , "()", 1.0, 12.0
                        , ("y_dmd_avg_"+Name+"_" + PartName).c_str(), "()", 0.0, 1.0);

   DigestibilityMin.read(scienceAPI
                        , "x_dmd_stage_code" , "()", 1.0, 12.0
                        , ("y_dmd_min_"+Name+"_" + PartName).c_str(), "()", 0.0, 1.0);
   }

void Pool::Init(float Plants)
   {
   float dm_init;
   float n_init_conc;
   float p_init_conc;
   scienceAPI.read(PartName + "_dm_init", dm_init, 0.0f, 1.0f);
   scienceAPI.read(PartName + "_n_init_conc", n_init_conc, 0.0f, 1.0f);
   p_init_conc = 0.0; //default value
   scienceAPI.readOptional(PartName + "_p_conc_init", p_init_conc, 0.0f, 1.0f);

   *this = Biomass(dm_init * Plants,
                   dm_init * Plants * n_init_conc,
                   dm_init * Plants * p_init_conc);
   }


Biomass& Pool::operator = (const Biomass& Pool2)
   {
   return Biomass::operator=(Pool2);
   }


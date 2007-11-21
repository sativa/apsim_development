#include <stdio.h>
#include <math.h>
#include <stdexcept>
#include <string>
#include "PlantPart.h"

#include "CompositePool.h"
#include "Delta.h"
using namespace std;

CompositePool::CompositePool(ScienceAPI& API, const std::string& Name, const std::string& PartName)
   {
   this->Name = Name;
   this->PartName = PartName;
   this->scienceAPI = &API;
   Clear();

   scienceAPI->expose(PartName+Name+"Wt", "g/m^2", Name + " " + PartName + " dry matter", DM);
   scienceAPI->expose(PartName+Name+"N",  "g/m^2", Name + " " + PartName + " nitrogen", N);
   scienceAPI->expose(PartName+Name+"P",  "g/m^2", Name + " " + PartName + " phosphorus", P);

   scienceAPI->exposeFunction(PartName+Name+"nconc", "%", "N concentration in "+Name+" "+PartName, FloatFunction(&CompositePool::NconcPercent));
   scienceAPI->exposeFunction(PartName+Name+"pconc", "%", "P concentration in "+Name+" "+PartName, FloatFunction(&CompositePool::PconcPercent));

   DigestibilityMax.read(*scienceAPI
                        , "x_dmd_stage_code" , "()", 1.0, 12.0
                        , ("y_dmd_max_"+Name+"_" + PartName).c_str(), "()", 0.0, 1.0);

   DigestibilityAvg.read(*scienceAPI
                        , "x_dmd_stage_code" , "()", 1.0, 12.0
                        , ("y_dmd_avg_"+Name+"_" + PartName).c_str(), "()", 0.0, 1.0);

   DigestibilityMin.read(*scienceAPI
                        , "x_dmd_stage_code" , "()", 1.0, 12.0
                        , ("y_dmd_min_"+Name+"_" + PartName).c_str(), "()", 0.0, 1.0);
   }

void CompositePool::Clear (void)
   {
   DM = 0.0;
   N = 0.0;
   P = 0.0;
   }

void CompositePool::Init(float Plants)
   {
   if (scienceAPI == NULL)
      throw runtime_error ("Cannot initialise CompositePool. SciencAPI is NULL");

   float dm_init;
   float n_init_conc;
   float p_init_conc;
   scienceAPI->read(PartName + "_dm_init", dm_init, 0.0f, 1.0f);
   scienceAPI->read(PartName + "_n_init_conc", n_init_conc, 0.0f, 1.0f);
   p_init_conc = 0.0; //default value
   scienceAPI->readOptional(PartName + "_p_conc_init", p_init_conc, 0.0f, 1.0f);

   DM = dm_init * Plants;
   N = DM * n_init_conc;
   P = DM * p_init_conc;
   }



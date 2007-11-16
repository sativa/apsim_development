#include <stdio.h>
#include <math.h>
#include <stdexcept>
#include <string>
#include "PlantPart.h"

#include "Pool.h"
#include "Delta.h"
using namespace std;

Pool::Pool(plantInterface& p, ScienceAPI& API, const std::string& Name, const std::string& PartName)
   {
   this->Name = Name;
   this->PartName = PartName;
   this->scienceAPI = &API;
   this->plant = &p;
   Clear();

   scienceAPI->expose(PartName+Name+"Wt", "g/m^2", Name + " " + PartName + " dry matter", DM);
   scienceAPI->expose(PartName+Name+"N",  "g/m^2", Name + " " + PartName + " nitrogen", N);
   scienceAPI->expose(PartName+Name+"P",  "g/m^2", Name + " " + PartName + " phosphorus", P);

   scienceAPI->exposeFunction(PartName+Name+"nconc", "%", "N concentration in "+Name+" "+PartName, FloatFunction(&Pool::NconcPercent));
   scienceAPI->exposeFunction(PartName+Name+"pconc", "%", "P concentration in "+Name+" "+PartName, FloatFunction(&Pool::PconcPercent));


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

Pool::Pool()
   {
   Clear();
   }

void Pool::Clear (void)
   {
   DM = 0.0;
   N = 0.0;
   P = 0.0;
   }

void Pool::Init()
   {
   if (scienceAPI == NULL)
      throw runtime_error ("Cannot initialise pool. SciencAPI is NULL");
   float Plants = plant->getPlants();
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

Pool Pool::operator + (const Pool& Pool2)
   {
   Pool Temp;
   Temp.DM = DM + Pool2.DM;
   Temp.N = N + Pool2.N;
   Temp.P = P + Pool2.P;
   Temp.PartName = PartName;
   Temp.Name = Name;
   return Temp;
   }
Pool Pool::operator + (const Pool Pool2)
   {
   Pool Temp;
   Temp.DM = DM + Pool2.DM;
   Temp.N = N + Pool2.N;
   Temp.P = P + Pool2.P;
   Temp.PartName = PartName;
   Temp.Name = Name;
   return Temp;
   }

Pool Pool::operator + (const Delta& Dlt)
   {
   Pool Temp;
   Temp.DM = DM + Dlt.DM;
   Temp.N = N + Dlt.N;
   Temp.P = P + Dlt.P;
   Temp.PartName = PartName;
   Temp.Name = Name;
   return Temp;
   }

Pool Pool::operator - (const Delta& Dlt)
   {
   Pool Temp;
   Temp.DM = DM - Dlt.DM;
   Temp.N = N - Dlt.N;
   Temp.P = P - Dlt.P;
   Temp.PartName = PartName;
   Temp.Name = Name;
   return Temp;
   }

Pool Pool::operator * (float Fraction)
   {
   Pool Temp;
   Temp.DM = DM * Fraction;
   Temp.N = N * Fraction;
   Temp.P = P * Fraction;
   Temp.PartName = PartName;
   Temp.Name = Name;
   return Temp;
   }

Pool Pool::operator = (const Pool& Pool2)
   {
   DM = Pool2.DM;
   N = Pool2.N;
   P = Pool2.P;
   PartName = Pool2.PartName;
   Name = Pool2.Name;
   // Now check nothing is negative
   const float ctz = -0.00001;
   if (DM < ctz) cerr << endl << "     *** " << PartName << " " << Name << " DM pool is negative! " << ftoa(DM,6) << endl << endl;
   if (N < ctz)  cerr << endl << "     *** " << PartName << " "<< Name << " N pool is negative! " << ftoa(N,6) << endl<< endl;
   if (P < ctz)  cerr << endl << "     *** " << PartName << " " << Name << " P pool is negative! " << ftoa(P,6) << endl<< endl;
   return *this;
   }   

#include "Pool.h"
#include "Delta.h"
#include <ComponentInterface/ScienceAPI.h>
using namespace std;

Pool::Pool(ScienceAPI& scienceAPI, const std::string& Name, const std::string& PartName)
   {
   scienceAPI.expose(Name+PartName+"Wt", "g/m^2", Name + " " + PartName + " dry matter", DM);
   scienceAPI.expose(Name+PartName+"N",  "g/m^2", Name + " " + PartName + " nitrogen", N);
   scienceAPI.expose(Name+PartName+"P",  "g/m^2", Name + " " + PartName + " phosphorus", P);
   }
void Pool::Add (Delta& Dlt)
   {
   DM += Dlt.DM;
   N += Dlt.N;
   P += Dlt.P;
   }
void Pool::Remove (Delta& Dlt)
   {
   DM -= Dlt.DM;
   N -= Dlt.N;
   P -= Dlt.P;
   }

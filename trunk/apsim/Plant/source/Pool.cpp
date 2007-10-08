
#include "Pool.h"
#include "Delta.h"
#include <ComponentInterface/ScienceAPI.h>
using namespace std;

Pool::Pool(ScienceAPI& scienceAPI, const std::string& Name, const std::string& PartName)
   {
   scienceAPI.expose(PartName+Name+"Wt", "g/m^2", Name + " " + PartName + " dry matter", DM);
   scienceAPI.expose(PartName+Name+"N",  "g/m^2", Name + " " + PartName + " nitrogen", N);
   scienceAPI.expose(PartName+Name+"P",  "g/m^2", Name + " " + PartName + " phosphorus", P);
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
void Pool::Clear (void)
   {
   DM = 0.0;
   N = 0.0;
   P = 0.0;
   }

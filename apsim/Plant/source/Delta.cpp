
#include "Delta.h"
#include "Pool.h"
#include <ComponentInterface/ScienceAPI.h>
using namespace std;

Delta::Delta(ScienceAPI& scienceAPI, const std::string& Name, const std::string& PartName)
   {
   scienceAPI.expose("Delta"+Name+PartName+"Wt", "g/m^2", "Change in " + Name + " " + PartName + " dry matter", DM);
   scienceAPI.expose("Delta"+Name+PartName+"N",  "g/m^2", "Change in " + Name + " " + PartName + " nitrogen", N);
   scienceAPI.expose("Delta"+Name+PartName+"P",  "g/m^2", "Change in " + Name + " " + PartName + " phosphorus", P);
   }

void Delta::Move (Pool& From, Pool& To)
   {
   From.Remove(*this);
   To.Add(*this);
   }


#include "Delta.h"
#include "Pool.h"
#include <ComponentInterface/ScienceAPI.h>
using namespace std;

Delta::Delta()
   {
   Clear();
   }
Delta::Delta(ScienceAPI& scienceAPI, const std::string& Name, const std::string& PartName)
   {
   scienceAPI.expose(PartName+"Delta"+Name+"Wt", "g/m^2", "Change in " + Name + " " + PartName + " dry matter", DM);
   scienceAPI.expose(PartName+"Delta"+Name+"N",  "g/m^2", "Change in " + Name + " " + PartName + " nitrogen", N);
   scienceAPI.expose(PartName+"Delta"+Name+"P",  "g/m^2", "Change in " + Name + " " + PartName + " phosphorus", P);
   }

void Delta::Move (Pool& From, Pool& To)
   {
   From = From - *this;
   To = To + *this;
   }
void Delta::Clear (void)
   {
   DM = 0.0;
   N = 0.0;
   P = 0.0;
   }

Delta Delta::operator + (const Delta& Delta2) const
   {
   Delta Temp;
   Temp.DM = DM + Delta2.DM;
   Temp.N = N + Delta2.N;
   Temp.P = P + Delta2.P;
   return Temp;
   }
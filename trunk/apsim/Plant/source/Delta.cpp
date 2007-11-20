#include <stdio.h>
#include <math.h>
#include <stdexcept>
#include <string>
#include "PlantPart.h"

#include "Delta.h"
#include "Pool.h"

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

Delta Delta::operator + (const Delta& Delta2)
   {
   Delta Temp;
   Temp.DM = DM + Delta2.DM;
   Temp.N = N + Delta2.N;
   Temp.P = P + Delta2.P;
   return Temp;
   }

Delta Delta::operator * (float value)
   {
   Delta Temp;
   Temp.DM = DM * value;
   Temp.N = N * value;
   Temp.P = P * value;
   return Temp;
   }
Delta Delta::operator = (const Pool& Pool2)
   {
   DM = Pool2.DM;
   N = Pool2.N;
   P = Pool2.P;
//   PartName = Pool2.PartName;
 //  Name = Pool2.Name;
   // Now check nothing is negative
//   const float ctz = -0.00001;
//   if (DM < ctz) throw std::runtime_error(PartName + " " + Name + " DM pool is negative! " + ftoa(DM,6));
//   if (N < ctz) throw std::runtime_error(PartName + " " + Name + " N pool is negative! " + ftoa(N,6));
//   if (P < ctz) throw std::runtime_error(PartName + " " + Name + " P pool is negative! " + ftoa(P,6));
   return *this;
   }      
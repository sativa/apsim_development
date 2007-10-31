#include <stdio.h>
#include <math.h>
#include <stdexcept>
#include <string>
#include "PlantPart.h"

#include "Pool.h"
#include "Delta.h"
using namespace std;

Pool::Pool(ScienceAPI& scienceAPI, const std::string& Name, const std::string& PartName)
   {
   scienceAPI.expose(PartName+Name+"Wt", "g/m^2", Name + " " + PartName + " dry matter", DM);
   scienceAPI.expose(PartName+Name+"N",  "g/m^2", Name + " " + PartName + " nitrogen", N);
   scienceAPI.expose(PartName+Name+"P",  "g/m^2", Name + " " + PartName + " phosphorus", P);
   Clear();
   this->Name = Name;
   this->PartName = PartName;
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
   if (DM < ctz) throw std::runtime_error(PartName + " " + Name + " DM pool is negative! " + ftoa(DM,6));
   if (N < ctz) throw std::runtime_error(PartName + " " + Name + " N pool is negative! " + ftoa(N,6));
   if (P < ctz) throw std::runtime_error(PartName + " " + Name + " P pool is negative! " + ftoa(P,6));
   return *this;
   }   
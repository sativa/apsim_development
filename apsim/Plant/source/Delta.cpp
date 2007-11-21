#include <stdio.h>
#include <math.h>
#include <stdexcept>
#include <string>
#include "PlantPart.h"

#include "Delta.h"
#include "Pool.h"

using namespace std;

Delta::Delta(ScienceAPI& scienceAPI, const std::string& Name, const std::string& PartName)
   {
   scienceAPI.expose(PartName+"Delta"+Name+"Wt", "g/m^2", "Change in " + Name + " " + PartName + " dry matter", privateDM);
   scienceAPI.expose(PartName+"Delta"+Name+"N",  "g/m^2", "Change in " + Name + " " + PartName + " nitrogen", privateN);
   scienceAPI.expose(PartName+"Delta"+Name+"P",  "g/m^2", "Change in " + Name + " " + PartName + " phosphorus", privateP);
   }

void Delta::Move (Pool& From, Pool& To)
   {
   From = From - *this;
   To = To + *this;
   }

Delta Delta::operator = (const Biomass& Pool2)
   {
   privateDM = Pool2.DM();
   privateN = Pool2.N();
   privateP = Pool2.P();
//   *(Biomass*)this = Pool2;
   return *this;
   }

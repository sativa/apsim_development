#include <stdio.h>
#include <math.h>
#include <stdexcept>
#include <string>
#include "plantPart.h"

#include "Delta.h"
#include "Pool.h"

using namespace std;

Delta::Delta(ScienceAPI& scienceAPI, const std::string& Name, const std::string& PartName)
   {
   scienceAPI.exposeFunction(PartName+"Delta"+Name+"Wt", "g/m^2", "Change in " + Name + " " + PartName + " dry matter", FloatFunction(&Biomass::DM));
   scienceAPI.exposeFunction(PartName+"Delta"+Name+"N",  "g/m^2", "Change in " + Name + " " + PartName + " nitrogen", FloatFunction(&Biomass::N));
   scienceAPI.exposeFunction(PartName+"Delta"+Name+"P",  "g/m^2", "Change in " + Name + " " + PartName + " phosphorus", FloatFunction(&Biomass::P));
   }

void Delta::Move (Pool& From, Pool& To)
   {
   From = From - *this;
   To = To + *this;
   }

Biomass& Delta::operator = (const Biomass& Biomass2)
   {
   return Biomass::operator=(Biomass2);
   }


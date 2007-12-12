#include <stdio.h>
#include <math.h>
#include <stdexcept>
#include <string>
#include "plantPart.h"

#include "CompositePool.h"
#include "Delta.h"
using namespace std;

CompositePool::CompositePool(plantInterface& plant, ScienceAPI& scienceAPI, const std::string& Name, const std::string& PartName)
   : Pool(plant, scienceAPI, Name, PartName)
   {
   }

void CompositePool::Clear()
   {
   for (unsigned i = 0; i != Pools.size(); i++)
      Pools[i]->Clear();
   }
void CompositePool::ClearPools()
   {
   Pools.erase(Pools.begin(), Pools.end());
   }
void CompositePool::AddPool(Biomass& pool)
   {
   Pools.push_back(&pool);
   }

float CompositePool::DM() const
   {
   float wt = 0.0;
   for (unsigned i = 0; i != Pools.size(); i++)
      wt += Pools[i]->DM();
   return wt;
   }
float CompositePool::N()  const
   {
   float n = 0.0;
   for (unsigned i = 0; i != Pools.size(); i++)
      n += Pools[i]->N();
   return n;
   }
float CompositePool::P()  const
   {
   float p = 0.0;
   for (unsigned i = 0; i != Pools.size(); i++)
      p += Pools[i]->P();
   return p;
   }

Biomass& CompositePool::operator = (const Biomass&)
   {
   throw runtime_error("Cannot set biomass for a composite pool" 
                       ". Part name = " + PartName +
                       ". Name = " + Name);
   }


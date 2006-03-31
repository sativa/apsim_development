
// Modification log
// 2 Feb 05 J. Hargreaves  Implementation

#ifndef PLANTFRUIT_H
#define PLANTFRUIT_H

#ifndef __CSTRING_H
#include <cstring.h>
#endif

#ifndef __IOSTREAM_H
#include <iostream.h>
#endif

#include "CompositePart.h"
#include "FruitCohort.h"



class PlantFruit : public CompositePart
{
   friend ostream &operator<<(ostream &, const PlantFruit &);
public:												// member functions
   PlantFruit(plantInterface *p, const string &name);
   ~PlantFruit();
   void display(ostream &os) const;

   //		PlantFruit(const PlantFruit &PlantFruit); 			// copy constructor
   const PlantFruit &operator=(const PlantFruit &other);		// Assigment operator

   void doInit1();

   void doRegistrations(protocol::Component *);
   void  doNPartition(float nSupply, float n_demand_sum, float n_capacity_sum);
   void doNFixRetranslocate(float NFix, float NDemandDifferentialTotal);
   void doNRetranslocate( float N_avail_rep, float grain_n_demand);
   void doNSenescedRetrans(float navail, float n_demand_tot);
};

#endif


// Modification log
// 2 Feb 05 J. Hargreaves  Implementation

#ifndef PLANTFRUIT_H
#define PLANTFRUIT_H

#include "CompositePart.h"
#include "FruitCohort.h"




class PlantFruit : public CompositePart
{
   friend ostream &operator<<(ostream &, const PlantFruit &);
public:												// member functions
   static plantPart* construct(ScienceAPI& scienceAPI, plantInterface *p, const string &name);
   PlantFruit(ScienceAPI& scienceAPI, plantInterface *p, const string &name);
   ~PlantFruit();
   void display(ostream &os) const;

   PlantFruit(const PlantFruit &PlantFruit); 			// copy constructor
   const PlantFruit &operator=(const PlantFruit &other);		// Assigment operator

   void onInit1(protocol::Component *system);
   void doNewCohort(protocol::Component *system);

   void addNewCohort (protocol::Component *system);

private:
   int cohortNum;
};

#endif


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
   PlantFruit(plantInterface *p, const string &name);
   ~PlantFruit();
   void display(ostream &os) const;

   //		PlantFruit(const PlantFruit &PlantFruit); 			// copy constructor
   const PlantFruit &operator=(const PlantFruit &other);		// Assigment operator

   void doInit1(void);

   void doRegistrations(protocol::Component *);
   void addNewCohort (void);

private:
   int cohortNum;
};

#endif

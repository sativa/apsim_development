
// Modification log
// 2 Feb 05 J. Hargreaves  Implementation

#ifndef PLANTFRUIT_H
#define PLANTFRUIT_H

#include "CompositePart.h"
#include "FruitCohort.h"
#include "FruitCohortFN.h"




class PlantFruit : public CompositePart
{
   friend ostream &operator<<(ostream &, const PlantFruit &);
public:                                             // member functions
   static plantPart* construct(ScienceAPI& scienceAPI, plantInterface *p, const string &name);
   PlantFruit(ScienceAPI& scienceAPI, plantInterface *p, const string &name);
   ~PlantFruit();
   void display(ostream &os);

   const PlantFruit &operator=(const PlantFruit &other);        // Assigment operator

   void prepare(void);
   void process(void);
   void onInit1(protocol::Component *system);
   void doNewCohort(protocol::Component *system);
   void addNewCohort (protocol::Component *system);

private:
   int cohortNum;

   // Fruit Cohort specific
      bool  setting_fruit;
      int  num_fruit_cohorts;
      float     Node_no_first_flower;
      float     dlt_dm_daily[366];
      float     veg_ratio[366];

      float     fruit_site_no;
      float     dlt_fruit_site_no;
      float     dlt_fruit_flower_no;

      float     fruit_no_potential;

};

#endif

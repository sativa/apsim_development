#ifndef NULLARBITRATOR_H
#define NULLARBITRATOR_H

#include "PlantInterface.h"

// A null arbitrator used to plug an empty hole...
class nullArbitrator : public Arbitrator
   {
  public:
   nullArbitrator(ScienceAPI& scienceAPI, plantInterface *p)
      : Arbitrator(scienceAPI, p) {};
   ~nullArbitrator(void) {};
   virtual void partitionDM(float,vector <plantPart *>& Parts, string FruitName)
      {
      throw std::runtime_error("Aieee! Null arbitrator called!!");
      };
   };
#endif


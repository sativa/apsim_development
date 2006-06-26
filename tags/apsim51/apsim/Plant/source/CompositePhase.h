#ifndef PLANTPHENOLOGYCOMPOSITEPHASE_H
#define PLANTPHENOLOGYCOMPOSITEPHASE_H

#include "phase.h"
#include <vector>

class compositePhase
// A collection of phases (eg leaf growth phases, grain filling phases)
   {
   private:
      std::vector<pPhase *> phases;
   public:
      compositePhase()  {};
      void add(pPhase *p) {phases.push_back(p);}
      bool contains(const pPhase &p) const;
      bool isEmpty(void) const {return phases.size() == 0;};
      float getTT(void) const;
      float getTTTarget(void) const;
      float getDays(void) const;
   };

#endif
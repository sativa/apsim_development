#ifndef CompositePhaseH
#define CompositePhaseH

#include "Phase.h"
#include <vector>

class compositePhase
// A collection of phases (eg leaf growth phases, grain filling phases)
   {
   private:
      std::vector<pPhase *> phases;
   public:
      compositePhase()  {};
      void add(pPhase *p) {phases.push_back(p);}
      bool contains(const pPhase &p);
      bool isEmpty(void)  {return phases.size() == 0;};
      float getTT(void);
      float getTTTarget(void);
   };

#endif

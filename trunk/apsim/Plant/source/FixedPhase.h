#ifndef FIXEDPHASE_H
#define FIXEDPHASE_H

#include "phase.h"

// A phenological phase.
class FixedPhase : public pPhase
   {
       protected:

      public:
      void readCultivarParameters(protocol::Component *s, const string & cultivar);
      FixedPhase(const string& stage_name) : pPhase (stage_name){};
      virtual string description() const;
   };


#endif


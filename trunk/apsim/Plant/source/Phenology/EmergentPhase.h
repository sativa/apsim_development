#ifndef EmergentPhase_H
#define EmergentPhase_H

#include "phase.h"
#include "environment.h"

class EmergentPhase : public pPhase
   // A phenological phase.
   {
      protected:
      float emergent_tt;       // Growing degree days to complete phase as a function of cum vd
      float shoot_lag;
      float shoot_rate;
      float sowing_depth;

      public:
      void readCultivarParameters(protocol::Component *s, const string & cultivar);
      void readSpeciesParameters (protocol::Component *, std::vector<string> &);
      void onSow(protocol::ApsimVariant incomingApsimVariant);
      EmergentPhase(const string& stage_name) : pPhase (stage_name){};
      //void updateTTTargets(const environment_t &e);
      void setupTTTarget(void);
      virtual string description() const;
   };


#endif


#ifndef VernalPhase_H
#define VernalPhase_H

#include "phase.h"
#include "environment.h"

class VernalPhase : public pPhase
   // A phenological phase.
   {
      protected:
      interpolationFunction vernal_days;     // relate temperature to vernalisation
      interpolationFunction vernal_tt;       // Growing degree days to complete phase as a function of cum vd
      float dlt_cumvd;
      float cumvd;

      public:
      void readCultivarParameters(protocol::Component *s, const string & cultivar);
      void readSpeciesParameters (protocol::Component *, std::vector<string> &);
      VernalPhase(const string& stage_name) : pPhase (stage_name){};
      void updateTTTargets(const environment_t &e);
      virtual string description() const;
   };


#endif


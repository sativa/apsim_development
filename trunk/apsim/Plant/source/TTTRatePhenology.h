#ifndef TTTRATEPHENOLOGY_H
#define TTTRATEPHENOLOGY_H

#include "tttphenology.h"

class TTTRatePhenology : public TTTPhenology {
 private:

   void get_zadok_stage(protocol::Component *system, protocol::QueryValueData &qd);

 public:
   TTTRatePhenology(PlantComponent *s, plantInterface *p) : TTTPhenology(s, p) {};

   //void process(const environment_t &e, const pheno_stress_t &ps);
   //void setupTTTargets(void);
   void updateTTTargets(const environment_t &e);
   void doRegistrations (protocol::Component *);

};

#endif